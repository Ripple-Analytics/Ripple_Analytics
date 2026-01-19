(ns mental-models.pipeline.integration.database
  "Database Integration for Pipeline
   
   Connects the pipeline to database storage:
   - Persists analysis results
   - Stores Lollapalooza alerts
   - Tracks model detection history
   - Provides query interface for results"
  (:require
   [mental-models.db.pipeline-storage :as storage]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.retry.queue :as retry]))

;; =============================================================================
;; DATABASE OPERATIONS WITH RESILIENCE
;; =============================================================================

(defn save-analysis-result!
  "Save analysis result to database with circuit breaker protection."
  [result]
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/save-pipeline-result! result))]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/writes)
        (audit/log-operation! {:operation :save-analysis
                               :result-id (:id result)})
        (events/publish! :database/analysis-saved {:id (:id result)})
        (:result cb-result))
      (do
        (log/error "Failed to save analysis result" {:status (:status cb-result)})
        (metrics/inc-counter! :database/write-failures)
        ;; Queue for retry
                (retry/enqueue! :save-analysis {:data result
                                                 :error (:error cb-result)})
        nil))))

(defn save-batch-results!
  "Save batch of analysis results to database."
  [results]
  (let [start-time (System/currentTimeMillis)
        cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/save-batch-results! results))
        duration (- (System/currentTimeMillis) start-time)]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/batch-writes)
        (metrics/observe-histogram! :database/batch-write-latency duration)
        (audit/log-operation! {:operation :save-batch
                               :count (count results)
                               :duration-ms duration})
        (events/publish! :database/batch-saved {:count (count results)})
        (:result cb-result))
      (do
        (log/error "Failed to save batch results" {:status (:status cb-result)})
        (metrics/inc-counter! :database/batch-write-failures)
        ;; Queue individual items for retry
                (doseq [result results]
                  (retry/enqueue! :save-analysis {:data result
                                                  :error (:error cb-result)}))
        nil))))

(defn save-lollapalooza-alert!
  "Save Lollapalooza alert to database."
  [alert]
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/save-lollapalooza-alert! alert))]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/lollapalooza-saves)
        (audit/log-operation! {:operation :save-lollapalooza
                               :model-count (count (:models alert))
                               :confidence (:avg-confidence alert)})
        (events/publish! :database/lollapalooza-saved alert)
        (:result cb-result))
      (do
        (log/error "Failed to save Lollapalooza alert" {:status (:status cb-result)})
                (retry/enqueue! :save-lollapalooza {:data alert
                                                     :error (:error cb-result)})
        nil))))

;; =============================================================================
;; QUERY OPERATIONS
;; =============================================================================

(defn get-recent-analyses
  "Get recent analysis results."
  [& {:keys [limit] :or {limit 100}}]
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/get-recent-results limit))]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/reads)
        (:result cb-result))
      (do
        (log/warn "Failed to get recent analyses" {:status (:status cb-result)})
        []))))

(defn get-analyses-by-model
  "Get analyses that detected a specific model."
  [model-id & {:keys [limit] :or {limit 100}}]
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/get-results-by-model model-id limit))]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/reads)
        (:result cb-result))
      [])))

(defn get-lollapalooza-history
  "Get Lollapalooza alert history."
  [& {:keys [limit] :or {limit 50}}]
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/get-lollapalooza-history limit))]
    (if (= :success (:status cb-result))
      (do
        (metrics/inc-counter! :database/reads)
        (:result cb-result))
      [])))

(defn get-model-statistics
  "Get detection statistics for all models."
  []
  (let [cb-result (cb/execute (cb/get-circuit-breaker "database")
                              #(storage/get-model-statistics))]
    (if (= :success (:status cb-result))
      (:result cb-result)
      {})))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-event-handlers!
  "Set up event handlers for automatic database persistence."
  []
  ;; Save analysis results when completed
  (events/subscribe! :analysis/completed
                     (fn [event]
                       (when-let [result (:result event)]
                         (save-analysis-result! result))))
  ;; Save Lollapalooza alerts when detected
  (events/subscribe! :lollapalooza/detected
                     (fn [alert]
                       (save-lollapalooza-alert! alert)))
  ;; Save batch results when completed
  (events/subscribe! :batch/completed
                     (fn [event]
                       (when-let [results (:results event)]
                         (save-batch-results! results))))
  (log/info "Database event handlers initialized"))

;; =============================================================================
;; HEALTH CHECK
;; =============================================================================

(defn check-database-health []
  (let [cb (cb/get-circuit-breaker "database")
        db-healthy (try
                     (storage/health-check)
                     (catch Exception _ false))]
    {:circuit-breaker-state (cb/get-state cb)
     :failure-rate (cb/get-failure-rate cb)
     :database-connected db-healthy
     :healthy (and (cb/closed? cb) db-healthy)}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-database-integration! []
  (log/info "Initializing database integration")
  ;; Create metrics
  (metrics/create-counter! :database/writes "Total database writes")
  (metrics/create-counter! :database/reads "Total database reads")
  (metrics/create-counter! :database/write-failures "Database write failures")
  (metrics/create-counter! :database/batch-writes "Batch database writes")
  (metrics/create-counter! :database/batch-write-failures "Batch write failures")
  (metrics/create-counter! :database/lollapalooza-saves "Lollapalooza saves")
  (metrics/create-histogram! :database/batch-write-latency "Batch write latency" [10 50 100 250 500 1000])
  ;; Set up event handlers
  (setup-event-handlers!)
  (log/info "Database integration initialized"))
