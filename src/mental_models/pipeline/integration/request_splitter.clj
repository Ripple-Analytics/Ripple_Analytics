(ns mental-models.pipeline.integration.request-splitter
  "Request splitter for mental model analysis system.
   
   Features:
   - Request splitting
   - Batch splitting
   - Parallel processing
   - Split strategies
   - Result aggregation
   - Split tracking
   - Error handling
   - Splitting metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent Executors ExecutorService]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:strategies {}       ;; strategy-id -> strategy
         :splits []           ;; active splits
         :config {:max-split-size 100
                  :max-parallel 10
                  :timeout-ms 30000
                  :default-strategy :even}
         :stats {:requests-split 0
                 :chunks-created 0
                 :chunks-processed 0
                 :aggregations 0
                 :errors 0}
         :executor nil
         :initialized? false}))

;; ============================================================================
;; Split Strategies
;; ============================================================================

(def built-in-strategies
  {:even (fn [items chunk-size]
           (partition-all chunk-size items))
   
   :by-key (fn [items key-fn]
             (vals (group-by key-fn items)))
   
   :weighted (fn [items weight-fn max-weight]
               (loop [remaining items
                      current-chunk []
                      current-weight 0
                      chunks []]
                 (if (empty? remaining)
                   (if (empty? current-chunk)
                     chunks
                     (conj chunks current-chunk))
                   (let [item (first remaining)
                         item-weight (weight-fn item)]
                     (if (> (+ current-weight item-weight) max-weight)
                       (recur remaining [] 0 (conj chunks current-chunk))
                       (recur (rest remaining)
                              (conj current-chunk item)
                              (+ current-weight item-weight)
                              chunks))))))
   
   :round-robin (fn [items num-chunks]
                  (let [chunks (vec (repeat num-chunks []))]
                    (reduce-kv (fn [c idx item]
                                 (update c (mod idx num-chunks) conj item))
                               chunks
                               (vec items))))
   
   :random (fn [items num-chunks]
             (let [chunks (vec (repeat num-chunks []))]
               (reduce (fn [c item]
                         (update c (rand-int num-chunks) conj item))
                       chunks
                       items)))})

;; ============================================================================
;; Custom Strategies
;; ============================================================================

(defn register-strategy!
  "Register a custom split strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :split-fn (get config :split-fn)
                  :enabled? (atom true)
                  :metrics {:invocations (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered split strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get a split strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id])
      (when-let [built-in (get built-in-strategies strategy-id)]
        {:id strategy-id :split-fn built-in})))

(defn list-strategies
  "List all strategies."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-strategies)
   (mapv (fn [[id s]]
           {:id id
            :name (:name s)
            :type :custom
            :enabled? @(:enabled? s)})
         (:strategies @state))))

;; ============================================================================
;; Request Splitting
;; ============================================================================

(defn split-request
  "Split a request into multiple chunks."
  [request & {:keys [strategy chunk-size key-fn]
              :or {strategy :even
                   chunk-size (get-in @state [:config :max-split-size])}}]
  (swap! state update-in [:stats :requests-split] inc)
  
  (let [body (:body request)
        items (cond
                (sequential? body) body
                (map? body) (or (:items body) (:data body) [body])
                :else [body])
        strategy-fn (or (:split-fn (get-strategy strategy))
                        (get built-in-strategies strategy)
                        (get built-in-strategies :even))
        chunks (case strategy
                 :even (strategy-fn items chunk-size)
                 :by-key (strategy-fn items key-fn)
                 :round-robin (strategy-fn items (get-in @state [:config :max-parallel]))
                 :random (strategy-fn items (get-in @state [:config :max-parallel]))
                 (strategy-fn items chunk-size))]
    
    (swap! state update-in [:stats :chunks-created] + (count chunks))
    
    (mapv (fn [chunk idx]
            (-> request
                (assoc :body {:items (vec chunk)})
                (assoc-in [:headers "X-Split-Index"] (str idx))
                (assoc-in [:headers "X-Split-Total"] (str (count chunks)))
                (assoc :split-index idx)
                (assoc :split-total (count chunks))))
          chunks
          (range))))

(defn split-batch
  "Split a batch of items."
  [items & {:keys [chunk-size strategy]
            :or {chunk-size (get-in @state [:config :max-split-size])
                 strategy :even}}]
  (let [strategy-fn (or (:split-fn (get-strategy strategy))
                        (get built-in-strategies strategy)
                        (get built-in-strategies :even))]
    (vec (strategy-fn items chunk-size))))

;; ============================================================================
;; Parallel Processing
;; ============================================================================

(defn process-splits-parallel
  "Process split requests in parallel."
  [split-requests handler & {:keys [max-parallel timeout-ms]
                              :or {max-parallel (get-in @state [:config :max-parallel])
                                   timeout-ms (get-in @state [:config :timeout-ms])}}]
  (let [result-chan (chan (count split-requests))
        executor (or (:executor @state)
                     (Executors/newFixedThreadPool max-parallel))]
    
    ;; Submit all tasks
    (doseq [req split-requests]
      (.submit executor
               (fn []
                 (try
                   (let [result (handler req)]
                     (swap! state update-in [:stats :chunks-processed] inc)
                     (async/put! result-chan {:success true
                                              :index (:split-index req)
                                              :result result}))
                   (catch Exception e
                     (swap! state update-in [:stats :errors] inc)
                     (async/put! result-chan {:success false
                                              :index (:split-index req)
                                              :error (.getMessage e)}))))))
    
    ;; Collect results with timeout
    (let [results (atom [])
          deadline (+ (System/currentTimeMillis) timeout-ms)]
      (loop [remaining (count split-requests)]
        (when (and (pos? remaining)
                   (< (System/currentTimeMillis) deadline))
          (when-let [result (async/poll! result-chan)]
            (swap! results conj result))
          (recur (- remaining (count @results)))))
      
      (close! result-chan)
      (sort-by :index @results))))

(defn process-splits-sequential
  "Process split requests sequentially."
  [split-requests handler]
  (mapv (fn [req]
          (try
            (let [result (handler req)]
              (swap! state update-in [:stats :chunks-processed] inc)
              {:success true
               :index (:split-index req)
               :result result})
            (catch Exception e
              (swap! state update-in [:stats :errors] inc)
              {:success false
               :index (:split-index req)
               :error (.getMessage e)})))
        split-requests))

;; ============================================================================
;; Result Aggregation
;; ============================================================================

(defn aggregate-results
  "Aggregate results from split processing."
  [results & {:keys [merge-fn error-strategy]
              :or {merge-fn concat
                   error-strategy :skip}}]
  (swap! state update-in [:stats :aggregations] inc)
  
  (let [successful (filter :success results)
        failed (filter (complement :success) results)]
    
    (case error-strategy
      :skip
      {:success (empty? failed)
       :data (vec (apply merge-fn (map #(get-in % [:result :body :items]
                                                 (get-in % [:result :body]))
                                       successful)))
       :errors (mapv :error failed)
       :processed (count successful)
       :failed (count failed)}
      
      :fail-fast
      (if (seq failed)
        {:success false
         :error (:error (first failed))
         :processed (count successful)
         :failed (count failed)}
        {:success true
         :data (vec (apply merge-fn (map #(get-in % [:result :body :items]
                                                   (get-in % [:result :body]))
                                         successful)))
         :processed (count successful)})
      
      :partial
      {:success true
       :data (vec (apply merge-fn (map #(get-in % [:result :body :items]
                                                 (get-in % [:result :body]))
                                       successful)))
       :partial? (boolean (seq failed))
       :errors (mapv :error failed)
       :processed (count successful)
       :failed (count failed)})))

(defn aggregate-responses
  "Aggregate HTTP responses."
  [responses]
  (let [bodies (map :body responses)
        all-items (mapcat (fn [b]
                            (cond
                              (sequential? b) b
                              (map? b) (or (:items b) (:data b) [b])
                              :else [b]))
                          bodies)]
    {:status (if (every? #(< (:status %) 400) responses) 200 207)
     :headers {"Content-Type" "application/json"}
     :body {:items (vec all-items)
            :count (count all-items)}}))

;; ============================================================================
;; Split and Process
;; ============================================================================

(defn split-and-process
  "Split a request and process all chunks."
  [request handler & {:keys [strategy chunk-size parallel? max-parallel timeout-ms error-strategy]
                      :or {strategy :even
                           parallel? true
                           error-strategy :skip}}]
  (let [split-requests (split-request request
                                       :strategy strategy
                                       :chunk-size chunk-size)
        results (if parallel?
                  (process-splits-parallel split-requests handler
                                           :max-parallel max-parallel
                                           :timeout-ms timeout-ms)
                  (process-splits-sequential split-requests handler))]
    (aggregate-results results :error-strategy error-strategy)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-split-request
  "Ring middleware to split large requests."
  [handler & {:keys [threshold strategy]
              :or {threshold 100
                   strategy :even}}]
  (fn [request]
    (let [body (:body request)
          items (cond
                  (sequential? body) body
                  (map? body) (or (:items body) (:data body))
                  :else nil)]
      (if (and items (> (count items) threshold))
        (let [result (split-and-process request handler :strategy strategy)]
          {:status (if (:success result) 200 207)
           :headers {"Content-Type" "application/json"}
           :body result})
        (handler request)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-split-size!
  "Set maximum split size."
  [size]
  (swap! state assoc-in [:config :max-split-size] size))

(defn set-max-parallel!
  "Set maximum parallel processing."
  [max-parallel]
  (swap! state assoc-in [:config :max-parallel] max-parallel))

(defn set-timeout!
  "Set processing timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :timeout-ms] timeout-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-splitter-metrics
  "Get splitter metrics."
  []
  (let [stats (:stats @state)]
    {:requests-split (:requests-split stats)
     :chunks-created (:chunks-created stats)
     :chunks-processed (:chunks-processed stats)
     :aggregations (:aggregations stats)
     :errors (:errors stats)
     :strategies-count (+ (count built-in-strategies)
                          (count (:strategies @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-splitter-stats
  "Get splitter statistics."
  []
  (merge (get-splitter-metrics)
         {:max-split-size (get-in @state [:config :max-split-size])
          :max-parallel (get-in @state [:config :max-parallel])
          :timeout-ms (get-in @state [:config :timeout-ms])}))

(defn reset-stats!
  "Reset splitter statistics."
  []
  (swap! state assoc :stats {:requests-split 0
                             :chunks-created 0
                             :chunks-processed 0
                             :aggregations 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-splitter!
  "Initialize the request splitter."
  []
  (when-not (:initialized? @state)
    (let [executor (Executors/newFixedThreadPool
                    (get-in @state [:config :max-parallel]))]
      (swap! state assoc :executor executor))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request splitter initialized")
    (events/emit! :request-splitter-initialized {})
    true))

(defn shutdown!
  "Shutdown the request splitter."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor))
  (swap! state assoc :initialized? false))
