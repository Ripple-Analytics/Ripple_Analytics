(ns mental-models.pipeline.integration.request-batcher
  "Request batcher for mental model analysis system.
   
   Features:
   - Automatic request batching
   - Batch size optimization
   - Batch timeout
   - Batch splitting
   - Result mapping
   - Error handling per item
   - Batch metrics
   - Adaptive batching"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:batchers {}         ;; batcher-id -> batcher
         :config {:default-batch-size 50
                  :default-batch-timeout-ms 100
                  :default-max-wait-ms 5000}
         :stats {:requests-received 0
                 :batches-executed 0
                 :items-processed 0
                 :batch-sizes []}
         :initialized? false}))

;; ============================================================================
;; Batch Request
;; ============================================================================

(defrecord BatchRequest [id request result-promise created-at])

(defn- create-batch-request
  "Create a batch request."
  [request]
  (->BatchRequest
   (str (UUID/randomUUID))
   request
   (promise)
   (System/currentTimeMillis)))

;; ============================================================================
;; Batcher Creation
;; ============================================================================

(defn create-batcher!
  "Create a request batcher."
  [batcher-id config]
  (let [batch-size (get config :batch-size
                        (get-in @state [:config :default-batch-size]))
        batch-timeout-ms (get config :batch-timeout-ms
                              (get-in @state [:config :default-batch-timeout-ms]))
        max-wait-ms (get config :max-wait-ms
                         (get-in @state [:config :default-max-wait-ms]))
        
        batcher {:id batcher-id
                 :name (get config :name (name batcher-id))
                 :batch-size batch-size
                 :batch-timeout-ms batch-timeout-ms
                 :max-wait-ms max-wait-ms
                 :batch-fn (get config :batch-fn)
                 :error-handler (get config :error-handler)
                 :queue (LinkedBlockingQueue.)
                 :enabled? (atom true)
                 :processing? (atom false)
                 :metrics {:received (atom 0)
                           :batches (atom 0)
                           :processed (atom 0)
                           :errors (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:batchers batcher-id] batcher)
    (logging/log :info "Created batcher" {:batcher-id batcher-id :batch-size batch-size})
    (events/emit! :batcher-created {:batcher-id batcher-id})
    
    ;; Start batch processor
    (start-batch-processor! batcher-id)
    
    batcher-id))

(defn get-batcher
  "Get a batcher."
  [batcher-id]
  (get-in @state [:batchers batcher-id]))

(defn list-batchers
  "List all batchers."
  []
  (mapv (fn [[id b]]
          {:id id
           :name (:name b)
           :batch-size (:batch-size b)
           :queue-size (.size (:queue b))
           :processing? @(:processing? b)
           :enabled? @(:enabled? b)})
        (:batchers @state)))

(defn delete-batcher!
  "Delete a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (reset! (:enabled? batcher) false)
    (.clear (:queue batcher))
    (swap! state update :batchers dissoc batcher-id)
    (logging/log :info "Deleted batcher" {:batcher-id batcher-id})))

;; ============================================================================
;; Batch Processing
;; ============================================================================

(defn- collect-batch
  "Collect a batch of requests."
  [batcher]
  (let [queue (:queue batcher)
        batch-size (:batch-size batcher)
        timeout-ms (:batch-timeout-ms batcher)
        batch (atom [])]
    
    ;; Collect up to batch-size items
    (loop [count 0]
      (when (< count batch-size)
        (if-let [item (if (zero? count)
                        ;; First item - wait with timeout
                        (.poll queue timeout-ms TimeUnit/MILLISECONDS)
                        ;; Subsequent items - poll immediately
                        (.poll queue))]
          (do
            (swap! batch conj item)
            (recur (inc count)))
          ;; No more items available
          nil)))
    
    @batch))

(defn- execute-batch!
  "Execute a batch of requests."
  [batcher batch]
  (when (seq batch)
    (reset! (:processing? batcher) true)
    (swap! (get-in batcher [:metrics :batches]) inc)
    (swap! state update-in [:stats :batches-executed] inc)
    (swap! state update-in [:stats :batch-sizes] conj (count batch))
    
    (logging/log :debug "Executing batch" {:batcher-id (:id batcher)
                                            :batch-size (count batch)})
    
    (try
      (let [requests (mapv :request batch)
            batch-fn (:batch-fn batcher)
            results (if batch-fn
                      (batch-fn requests)
                      requests)]
        
        ;; Distribute results
        (doseq [[req result] (map vector batch results)]
          (deliver (:result-promise req) {:status :success :result result})
          (swap! (get-in batcher [:metrics :processed]) inc)
          (swap! state update-in [:stats :items-processed] inc)))
      
      (catch Exception e
        (logging/log :error "Batch execution failed" {:batcher-id (:id batcher)
                                                       :error (.getMessage e)})
        ;; Handle error for each item
        (let [error-handler (:error-handler batcher)]
          (doseq [req batch]
            (if error-handler
              (try
                (let [result (error-handler (:request req) e)]
                  (deliver (:result-promise req) {:status :error-handled :result result}))
                (catch Exception e2
                  (deliver (:result-promise req) {:status :error :error (.getMessage e2)})))
              (deliver (:result-promise req) {:status :error :error (.getMessage e)}))
            (swap! (get-in batcher [:metrics :errors]) inc))))
      
      (finally
        (reset! (:processing? batcher) false)))))

(defn- start-batch-processor!
  "Start the batch processor for a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (go-loop []
      (when @(:enabled? batcher)
        (let [batch (collect-batch batcher)]
          (when (seq batch)
            (execute-batch! batcher batch)))
        (recur)))))

;; ============================================================================
;; Request Submission
;; ============================================================================

(defn submit
  "Submit a request for batching."
  [batcher-id request]
  (swap! state update-in [:stats :requests-received] inc)
  
  (if-let [batcher (get-batcher batcher-id)]
    (if @(:enabled? batcher)
      (let [batch-request (create-batch-request request)
            max-wait-ms (:max-wait-ms batcher)]
        
        (swap! (get-in batcher [:metrics :received]) inc)
        (.offer (:queue batcher) batch-request)
        
        ;; Wait for result with timeout
        (let [result (deref (:result-promise batch-request) max-wait-ms {:status :timeout})]
          (case (:status result)
            :success (:result result)
            :error-handled (:result result)
            :error (throw (ex-info "Batch processing failed" {:error (:error result)}))
            :timeout (throw (ex-info "Batch processing timeout" {:batcher-id batcher-id})))))
      (throw (ex-info "Batcher disabled" {:batcher-id batcher-id})))
    (throw (ex-info "Batcher not found" {:batcher-id batcher-id}))))

(defn submit-async
  "Submit a request for batching asynchronously."
  [batcher-id request callback]
  (go
    (try
      (let [result (submit batcher-id request)]
        (callback {:status :success :result result}))
      (catch Exception e
        (callback {:status :error :error (.getMessage e)})))))

(defn submit-many
  "Submit multiple requests for batching."
  [batcher-id requests]
  (mapv #(submit batcher-id %) requests))

;; ============================================================================
;; Batcher Control
;; ============================================================================

(defn enable-batcher!
  "Enable a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (reset! (:enabled? batcher) true)
    (start-batch-processor! batcher-id)
    (logging/log :info "Enabled batcher" {:batcher-id batcher-id})))

(defn disable-batcher!
  "Disable a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (reset! (:enabled? batcher) false)
    (logging/log :info "Disabled batcher" {:batcher-id batcher-id})))

(defn flush-batcher!
  "Flush pending requests for a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (let [queue (:queue batcher)
          batch (atom [])]
      (loop []
        (when-let [item (.poll queue)]
          (swap! batch conj item)
          (recur)))
      (when (seq @batch)
        (execute-batch! batcher @batch))
      (logging/log :info "Flushed batcher" {:batcher-id batcher-id :items-flushed (count @batch)}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-batch-size!
  "Set the batch size for a batcher."
  [batcher-id batch-size]
  (swap! state assoc-in [:batchers batcher-id :batch-size] batch-size))

(defn set-batch-timeout!
  "Set the batch timeout for a batcher."
  [batcher-id timeout-ms]
  (swap! state assoc-in [:batchers batcher-id :batch-timeout-ms] timeout-ms))

;; ============================================================================
;; Adaptive Batching
;; ============================================================================

(defn- calculate-optimal-batch-size
  "Calculate optimal batch size based on metrics."
  [batcher]
  (let [batch-sizes (:batch-sizes (:stats @state))
        recent-sizes (take-last 100 batch-sizes)]
    (if (seq recent-sizes)
      (let [avg-size (/ (reduce + recent-sizes) (count recent-sizes))
            current-size (:batch-size batcher)]
        (cond
          ;; Batches are consistently full - increase size
          (> avg-size (* 0.9 current-size))
          (min 200 (int (* current-size 1.2)))
          
          ;; Batches are consistently small - decrease size
          (< avg-size (* 0.5 current-size))
          (max 10 (int (* current-size 0.8)))
          
          :else current-size))
      (:batch-size batcher))))

(defn optimize-batch-size!
  "Optimize batch size based on recent performance."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    (let [optimal-size (calculate-optimal-batch-size batcher)
          current-size (:batch-size batcher)]
      (when (not= optimal-size current-size)
        (set-batch-size! batcher-id optimal-size)
        (logging/log :info "Optimized batch size" {:batcher-id batcher-id
                                                    :old-size current-size
                                                    :new-size optimal-size})))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-batcher-metrics
  "Get metrics for a batcher."
  [batcher-id]
  (when-let [batcher (get-batcher batcher-id)]
    {:batcher-id batcher-id
     :name (:name batcher)
     :batch-size (:batch-size batcher)
     :queue-size (.size (:queue batcher))
     :received @(get-in batcher [:metrics :received])
     :batches @(get-in batcher [:metrics :batches])
     :processed @(get-in batcher [:metrics :processed])
     :errors @(get-in batcher [:metrics :errors])
     :processing? @(:processing? batcher)
     :enabled? @(:enabled? batcher)}))

(defn get-all-batcher-metrics
  "Get metrics for all batchers."
  []
  (mapv (fn [[id _]] (get-batcher-metrics id)) (:batchers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-batcher-stats
  "Get batcher statistics."
  []
  (let [stats (:stats @state)
        batch-sizes (:batch-sizes stats)]
    {:batchers-count (count (:batchers @state))
     :requests-received (:requests-received stats)
     :batches-executed (:batches-executed stats)
     :items-processed (:items-processed stats)
     :avg-batch-size (if (seq batch-sizes)
                       (/ (reduce + batch-sizes) (count batch-sizes))
                       0)}))

(defn reset-stats!
  "Reset batcher statistics."
  []
  (swap! state assoc :stats {:requests-received 0
                             :batches-executed 0
                             :items-processed 0
                             :batch-sizes []}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-batcher!
  "Initialize the request batcher system."
  []
  (when-not (:initialized? @state)
    ;; Create default batchers
    (create-batcher! :analysis
                     {:name "Analysis"
                      :batch-size 50
                      :batch-timeout-ms 100
                      :batch-fn (fn [requests]
                                  (logging/log :debug "Processing analysis batch"
                                               {:count (count requests)})
                                  requests)})
    
    (create-batcher! :lm-studio
                     {:name "LM Studio"
                      :batch-size 10
                      :batch-timeout-ms 50
                      :batch-fn (fn [requests]
                                  (logging/log :debug "Processing LM Studio batch"
                                               {:count (count requests)})
                                  requests)})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request batcher initialized")
    (events/emit! :request-batcher-initialized {})
    true))
