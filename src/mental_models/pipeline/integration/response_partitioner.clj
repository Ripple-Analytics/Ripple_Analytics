(ns mental-models.pipeline.integration.response-partitioner
  "Response partitioner for mental model analysis system.
   
   Features:
   - Response partitioning
   - Data chunking
   - Partition strategies
   - Parallel processing
   - Partition merging
   - Partition routing
   - Partition metrics
   - Load distribution"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:strategies {}       ;; strategy-id -> partition strategy
         :partitions {}       ;; partition-id -> partition data
         :config {:default-strategy :size
                  :default-partition-size 1000
                  :max-partitions 100
                  :parallel-processing? true}
         :stats {:partitions-created (AtomicLong. 0)
                 :items-partitioned (AtomicLong. 0)
                 :partitions-merged (AtomicLong. 0)
                 :bytes-partitioned (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Built-in Partition Strategies
;; ============================================================================

(def built-in-strategies
  {:size
   (fn [data opts]
     (let [size (get opts :size (get-in @state [:config :default-partition-size]))]
       (partition-all size data)))
   
   :count
   (fn [data opts]
     (let [n (get opts :count 4)
           total (count data)
           size (max 1 (int (Math/ceil (/ total n))))]
       (partition-all size data)))
   
   :hash
   (fn [data opts]
     (let [n (get opts :partitions 4)
           key-fn (get opts :key-fn identity)]
       (vals (group-by #(mod (hash (key-fn %)) n) data))))
   
   :range
   (fn [data opts]
     (let [key-fn (get opts :key-fn identity)
           ranges (get opts :ranges [])]
       (if (empty? ranges)
         [data]
         (let [sorted (sort-by key-fn data)]
           (reduce (fn [partitions [start end]]
                     (conj partitions
                           (filter #(let [k (key-fn %)]
                                      (and (>= k start) (< k end)))
                                   sorted)))
                   []
                   ranges)))))
   
   :round-robin
   (fn [data opts]
     (let [n (get opts :partitions 4)]
       (map vec (apply map vector (partition-all n data)))))
   
   :key
   (fn [data opts]
     (let [key-fn (get opts :key-fn identity)]
       (vals (group-by key-fn data))))
   
   :weighted
   (fn [data opts]
     (let [weights (get opts :weights [1 1 1 1])
           total-weight (reduce + weights)
           total-items (count data)]
       (loop [remaining data
              weights weights
              partitions []]
         (if (or (empty? remaining) (empty? weights))
           partitions
           (let [weight (first weights)
                 size (int (* total-items (/ weight total-weight)))]
             (recur (drop size remaining)
                    (rest weights)
                    (conj partitions (take size remaining))))))))})

;; ============================================================================
;; Strategy Management
;; ============================================================================

(defn register-strategy!
  "Register a custom partition strategy."
  [strategy-id partition-fn]
  (swap! state assoc-in [:strategies strategy-id]
         {:id strategy-id
          :partition-fn partition-fn
          :created-at (System/currentTimeMillis)}))

(defn get-strategy
  "Get a partition strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id :partition-fn])
      (get built-in-strategies strategy-id)))

(defn list-strategies
  "List all strategies."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-strategies)
   (mapv (fn [[id s]] {:id id :type :custom :created-at (:created-at s)})
         (:strategies @state))))

;; ============================================================================
;; Partitioning
;; ============================================================================

(defn partition-data
  "Partition data using a strategy."
  [data & {:keys [strategy opts]
           :or {strategy :size opts {}}}]
  (let [strategy-fn (get-strategy strategy)
        partitions (strategy-fn data opts)]
    
    (.incrementAndGet (:partitions-created (:stats @state)))
    (.addAndGet (:items-partitioned (:stats @state)) (count data))
    
    (mapv vec partitions)))

(defn partition-response
  "Partition a response body."
  [response & {:keys [strategy opts]
               :or {strategy :size opts {}}}]
  (let [body (:body response)]
    (cond
      (sequential? body)
      (let [partitions (partition-data body :strategy strategy :opts opts)]
        (assoc response :partitions partitions))
      
      (string? body)
      (let [size (get opts :size 1024)
            partitions (partition-all size body)]
        (assoc response :partitions (mapv #(apply str %) partitions)))
      
      :else response)))

;; ============================================================================
;; Partition Storage
;; ============================================================================

(defn store-partition!
  "Store a partition."
  [partition-id data]
  (let [partition {:id partition-id
                   :data data
                   :size (count data)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:partitions partition-id] partition)
    partition-id))

(defn get-partition
  "Get a partition."
  [partition-id]
  (get-in @state [:partitions partition-id]))

(defn list-partitions
  "List all partitions."
  []
  (mapv (fn [[id p]]
          {:id id
           :size (:size p)
           :created-at (:created-at p)})
        (:partitions @state)))

(defn delete-partition!
  "Delete a partition."
  [partition-id]
  (swap! state update :partitions dissoc partition-id))

(defn clear-partitions!
  "Clear all partitions."
  []
  (swap! state assoc :partitions {}))

;; ============================================================================
;; Partition Merging
;; ============================================================================

(defn merge-partitions
  "Merge partitions back together."
  [partitions & {:keys [merge-fn]
                 :or {merge-fn concat}}]
  (.incrementAndGet (:partitions-merged (:stats @state)))
  (reduce merge-fn partitions))

(defn merge-partition-ids
  "Merge partitions by their IDs."
  [partition-ids & {:keys [merge-fn]
                    :or {merge-fn concat}}]
  (let [partitions (map #(:data (get-partition %)) partition-ids)]
    (merge-partitions partitions :merge-fn merge-fn)))

;; ============================================================================
;; Parallel Processing
;; ============================================================================

(defn process-partitions
  "Process partitions in parallel."
  [partitions process-fn & {:keys [parallel?]
                            :or {parallel? (get-in @state [:config :parallel-processing?])}}]
  (if parallel?
    (let [results (pmap process-fn partitions)]
      (vec results))
    (mapv process-fn partitions)))

(defn process-partitions-async
  "Process partitions asynchronously."
  [partitions process-fn]
  (let [result-chan (chan (count partitions))]
    (doseq [partition partitions]
      (go
        (let [result (process-fn partition)]
          (>! result-chan result))))
    
    (go-loop [results []
              remaining (count partitions)]
      (if (zero? remaining)
        results
        (let [result (<! result-chan)]
          (recur (conj results result) (dec remaining)))))))

;; ============================================================================
;; Partition Routing
;; ============================================================================

(defn create-router
  "Create a partition router."
  [route-fn]
  (fn [item]
    (route-fn item)))

(defn route-to-partitions
  "Route items to partitions."
  [items router num-partitions]
  (let [partitions (vec (repeat num-partitions []))]
    (reduce (fn [parts item]
              (let [partition-idx (mod (router item) num-partitions)]
                (update parts partition-idx conj item)))
            partitions
            items)))

;; ============================================================================
;; Partition Balancing
;; ============================================================================

(defn balance-partitions
  "Balance partitions to have similar sizes."
  [partitions]
  (let [total-items (reduce + (map count partitions))
        num-partitions (count partitions)
        target-size (int (Math/ceil (/ total-items num-partitions)))
        all-items (apply concat partitions)]
    (partition-all target-size all-items)))

(defn get-partition-stats
  "Get statistics about partitions."
  [partitions]
  (let [sizes (map count partitions)]
    {:count (count partitions)
     :total-items (reduce + sizes)
     :min-size (apply min sizes)
     :max-size (apply max sizes)
     :avg-size (/ (reduce + sizes) (count sizes))
     :std-dev (let [avg (/ (reduce + sizes) (count sizes))]
                (Math/sqrt (/ (reduce + (map #(Math/pow (- % avg) 2) sizes))
                              (count sizes))))}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-partition
  "Ring middleware to partition responses."
  [handler & {:keys [strategy opts]
              :or {strategy :size opts {}}}]
  (fn [request]
    (let [response (handler request)]
      (partition-response response :strategy strategy :opts opts))))

(defn wrap-partition-process
  "Ring middleware to partition and process responses."
  [handler process-fn & {:keys [strategy opts]
                         :or {strategy :size opts {}}}]
  (fn [request]
    (let [response (handler request)
          partitioned (partition-response response :strategy strategy :opts opts)
          processed (process-partitions (:partitions partitioned) process-fn)]
      (assoc response :body (merge-partitions processed)))))

(defn wrap-partition-header
  "Ring middleware to add partition info header."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if-let [partitions (:partitions response)]
        (-> response
            (assoc-in [:headers "X-Partition-Count"] (str (count partitions)))
            (assoc-in [:headers "X-Partition-Sizes"]
                      (str/join "," (map count partitions))))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set default partition strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-default-partition-size!
  "Set default partition size."
  [size]
  (swap! state assoc-in [:config :default-partition-size] size))

(defn set-max-partitions!
  "Set maximum partitions."
  [max-partitions]
  (swap! state assoc-in [:config :max-partitions] max-partitions))

(defn set-parallel-processing!
  "Enable/disable parallel processing."
  [parallel?]
  (swap! state assoc-in [:config :parallel-processing?] parallel?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-partitioner-metrics
  "Get partitioner metrics."
  []
  (let [stats (:stats @state)]
    {:partitions-created (.get (:partitions-created stats))
     :items-partitioned (.get (:items-partitioned stats))
     :partitions-merged (.get (:partitions-merged stats))
     :bytes-partitioned (.get (:bytes-partitioned stats))
     :stored-partitions (count (:partitions @state))
     :strategies-count (+ (count built-in-strategies) (count (:strategies @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-partitioner-stats
  "Get partitioner statistics."
  []
  (merge (get-partitioner-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :default-partition-size (get-in @state [:config :default-partition-size])
          :max-partitions (get-in @state [:config :max-partitions])
          :parallel-processing? (get-in @state [:config :parallel-processing?])}))

(defn reset-stats!
  "Reset partitioner statistics."
  []
  (.set (:partitions-created (:stats @state)) 0)
  (.set (:items-partitioned (:stats @state)) 0)
  (.set (:partitions-merged (:stats @state)) 0)
  (.set (:bytes-partitioned (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-partitioner!
  "Initialize the response partitioner."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response partitioner initialized")
    (events/emit! :response-partitioner-initialized {})
    true))
