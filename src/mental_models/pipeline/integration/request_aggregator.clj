(ns mental-models.pipeline.integration.request-aggregator
  "Request aggregator for mental model analysis system.
   
   Features:
   - Request aggregation
   - Batch processing
   - Time-based aggregation
   - Count-based aggregation
   - Aggregation strategies
   - Result merging
   - Aggregation metrics
   - Aggregation timeouts"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! put! take! alt!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:aggregators {}      ;; aggregator-id -> aggregator
         :config {:default-batch-size 10
                  :default-timeout-ms 5000
                  :max-batch-size 100}
         :stats {:requests-aggregated 0
                 :batches-processed 0
                 :timeouts 0
                 :merges 0}
         :initialized? false}))

;; ============================================================================
;; Aggregator Creation
;; ============================================================================

(defn create-aggregator
  "Create a new request aggregator."
  [aggregator-id config]
  (let [batch-size (get config :batch-size (get-in @state [:config :default-batch-size]))
        timeout-ms (get config :timeout-ms (get-in @state [:config :default-timeout-ms]))
        key-fn (get config :key-fn identity)
        merge-fn (get config :merge-fn (fn [reqs] reqs))
        handler-fn (get config :handler-fn)
        input-chan (chan batch-size)
        aggregator {:id aggregator-id
                    :name (get config :name (name aggregator-id))
                    :batch-size batch-size
                    :timeout-ms timeout-ms
                    :key-fn key-fn
                    :merge-fn merge-fn
                    :handler-fn handler-fn
                    :input-chan input-chan
                    :pending (atom {})  ;; key -> [requests]
                    :status (atom :active)
                    :metrics {:requests (atom 0)
                              :batches (atom 0)
                              :timeouts (atom 0)}
                    :created-at (System/currentTimeMillis)}]
    
    ;; Start aggregation loop
    (start-aggregation-loop! aggregator)
    
    (swap! state assoc-in [:aggregators aggregator-id] aggregator)
    (logging/log :info "Created aggregator" {:aggregator-id aggregator-id})
    aggregator-id))

(defn get-aggregator
  "Get an aggregator by ID."
  [aggregator-id]
  (get-in @state [:aggregators aggregator-id]))

(defn list-aggregators
  "List all aggregators."
  []
  (mapv (fn [[id a]]
          {:id id
           :name (:name a)
           :batch-size (:batch-size a)
           :timeout-ms (:timeout-ms a)
           :status @(:status a)
           :pending-count (reduce + (map count (vals @(:pending a))))})
        (:aggregators @state)))

(defn delete-aggregator!
  "Delete an aggregator."
  [aggregator-id]
  (when-let [aggregator (get-aggregator aggregator-id)]
    (reset! (:status aggregator) :stopped)
    (close! (:input-chan aggregator))
    (swap! state update :aggregators dissoc aggregator-id)
    (logging/log :info "Deleted aggregator" {:aggregator-id aggregator-id})))

;; ============================================================================
;; Aggregation Loop
;; ============================================================================

(defn- process-batch!
  "Process a batch of requests."
  [aggregator key requests]
  (try
    (let [merged ((:merge-fn aggregator) requests)
          result ((:handler-fn aggregator) merged)]
      (swap! (get-in aggregator [:metrics :batches]) inc)
      (swap! state update-in [:stats :batches-processed] inc)
      {:success? true :result result :count (count requests)})
    (catch Exception e
      (logging/log :error "Batch processing failed" {:aggregator-id (:id aggregator)
                                                      :key key
                                                      :error (.getMessage e)})
      {:success? false :error (.getMessage e) :count (count requests)})))

(defn- flush-pending!
  "Flush all pending requests for a key."
  [aggregator key]
  (let [pending @(:pending aggregator)
        requests (get pending key)]
    (when (seq requests)
      (swap! (:pending aggregator) dissoc key)
      (process-batch! aggregator key requests))))

(defn- flush-all-pending!
  "Flush all pending requests."
  [aggregator]
  (let [pending @(:pending aggregator)]
    (doseq [[key _] pending]
      (flush-pending! aggregator key))))

(defn- start-aggregation-loop!
  "Start the aggregation loop for an aggregator."
  [aggregator]
  (let [input-chan (:input-chan aggregator)
        timeout-ms (:timeout-ms aggregator)
        batch-size (:batch-size aggregator)]
    
    (go-loop [last-flush (System/currentTimeMillis)]
      (when (= @(:status aggregator) :active)
        (alt!
          input-chan
          ([request]
           (when request
             (let [key ((:key-fn aggregator) request)]
               (swap! (:pending aggregator) update key (fnil conj []) request)
               (swap! (get-in aggregator [:metrics :requests]) inc)
               (swap! state update-in [:stats :requests-aggregated] inc)
               
               ;; Check if batch is full
               (when (>= (count (get @(:pending aggregator) key)) batch-size)
                 (flush-pending! aggregator key)))
             (recur (System/currentTimeMillis))))
          
          (timeout timeout-ms)
          ([_]
           ;; Timeout - flush all pending
           (when (seq @(:pending aggregator))
             (swap! (get-in aggregator [:metrics :timeouts]) inc)
             (swap! state update-in [:stats :timeouts] inc)
             (flush-all-pending! aggregator))
           (recur (System/currentTimeMillis))))))))

;; ============================================================================
;; Request Submission
;; ============================================================================

(defn submit-request!
  "Submit a request to an aggregator."
  [aggregator-id request]
  (if-let [aggregator (get-aggregator aggregator-id)]
    (if (= @(:status aggregator) :active)
      (do
        (put! (:input-chan aggregator) request)
        {:submitted? true :aggregator-id aggregator-id})
      {:submitted? false :reason :aggregator-stopped})
    {:submitted? false :reason :aggregator-not-found}))

(defn submit-and-wait!
  "Submit a request and wait for the result."
  [aggregator-id request timeout-ms]
  (let [result-chan (chan 1)
        request-with-callback (assoc request :result-chan result-chan)]
    (if-let [aggregator (get-aggregator aggregator-id)]
      (do
        (put! (:input-chan aggregator) request-with-callback)
        (let [[result _] (alt!
                           result-chan ([v] [v :result])
                           (timeout timeout-ms) ([_] [nil :timeout]))]
          (if result
            {:success? true :result result}
            {:success? false :reason :timeout})))
      {:success? false :reason :aggregator-not-found})))

;; ============================================================================
;; Aggregation Strategies
;; ============================================================================

(defn create-time-based-aggregator
  "Create a time-based aggregator."
  [aggregator-id config]
  (create-aggregator aggregator-id
                     (assoc config
                            :batch-size Integer/MAX_VALUE
                            :timeout-ms (get config :window-ms 1000))))

(defn create-count-based-aggregator
  "Create a count-based aggregator."
  [aggregator-id config]
  (create-aggregator aggregator-id
                     (assoc config
                            :batch-size (get config :count 10)
                            :timeout-ms Integer/MAX_VALUE)))

(defn create-hybrid-aggregator
  "Create a hybrid aggregator (time or count, whichever comes first)."
  [aggregator-id config]
  (create-aggregator aggregator-id config))

;; ============================================================================
;; Merge Functions
;; ============================================================================

(defn merge-by-concat
  "Merge requests by concatenating bodies."
  [requests]
  {:method :post
   :uri (get (first requests) :uri)
   :body (mapv :body requests)
   :original-requests requests})

(defn merge-by-union
  "Merge requests by taking union of parameters."
  [requests]
  (let [all-params (reduce merge {} (map :params requests))]
    {:method :get
     :uri (get (first requests) :uri)
     :params all-params
     :original-requests requests}))

(defn merge-by-latest
  "Merge requests by taking the latest one."
  [requests]
  (last requests))

(defn merge-by-first
  "Merge requests by taking the first one."
  [requests]
  (first requests))

;; ============================================================================
;; Result Distribution
;; ============================================================================

(defn distribute-result
  "Distribute a batch result to individual request callbacks."
  [requests result]
  (doseq [request requests]
    (when-let [result-chan (:result-chan request)]
      (put! result-chan result)
      (close! result-chan))))

(defn distribute-results
  "Distribute individual results to request callbacks."
  [requests results]
  (doseq [[request result] (map vector requests results)]
    (when-let [result-chan (:result-chan request)]
      (put! result-chan result)
      (close! result-chan))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-aggregator
  "Ring middleware for request aggregation."
  [handler aggregator-id & {:keys [key-fn] :or {key-fn :uri}}]
  (fn [request]
    (let [result (submit-and-wait! aggregator-id
                                   (assoc request :key (key-fn request))
                                   10000)]
      (if (:success? result)
        (:result result)
        (handler request)))))

;; ============================================================================
;; Aggregator Control
;; ============================================================================

(defn pause-aggregator!
  "Pause an aggregator."
  [aggregator-id]
  (when-let [aggregator (get-aggregator aggregator-id)]
    (reset! (:status aggregator) :paused)
    (logging/log :info "Paused aggregator" {:aggregator-id aggregator-id})))

(defn resume-aggregator!
  "Resume an aggregator."
  [aggregator-id]
  (when-let [aggregator (get-aggregator aggregator-id)]
    (reset! (:status aggregator) :active)
    (start-aggregation-loop! aggregator)
    (logging/log :info "Resumed aggregator" {:aggregator-id aggregator-id})))

(defn flush-aggregator!
  "Flush all pending requests in an aggregator."
  [aggregator-id]
  (when-let [aggregator (get-aggregator aggregator-id)]
    (flush-all-pending! aggregator)
    (logging/log :info "Flushed aggregator" {:aggregator-id aggregator-id})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-aggregator-metrics
  "Get metrics for an aggregator."
  [aggregator-id]
  (when-let [aggregator (get-aggregator aggregator-id)]
    {:aggregator-id aggregator-id
     :name (:name aggregator)
     :requests @(get-in aggregator [:metrics :requests])
     :batches @(get-in aggregator [:metrics :batches])
     :timeouts @(get-in aggregator [:metrics :timeouts])
     :pending-count (reduce + (map count (vals @(:pending aggregator))))
     :status @(:status aggregator)}))

(defn get-all-aggregator-metrics
  "Get metrics for all aggregators."
  []
  (mapv (fn [[id _]] (get-aggregator-metrics id)) (:aggregators @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-aggregator-stats
  "Get aggregator statistics."
  []
  (let [stats (:stats @state)]
    {:aggregators-count (count (:aggregators @state))
     :requests-aggregated (:requests-aggregated stats)
     :batches-processed (:batches-processed stats)
     :timeouts (:timeouts stats)
     :merges (:merges stats)}))

(defn reset-stats!
  "Reset aggregator statistics."
  []
  (swap! state assoc :stats {:requests-aggregated 0
                             :batches-processed 0
                             :timeouts 0
                             :merges 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-aggregator!
  "Initialize the request aggregator system."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request aggregator initialized")
    (events/emit! :request-aggregator-initialized {})
    true))
