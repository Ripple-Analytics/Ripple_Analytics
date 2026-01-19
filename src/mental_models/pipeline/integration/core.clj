(ns mental-models.pipeline.integration.core
  "Pipeline Integration Module
   
   Wires all infrastructure modules into the main pipeline:
   - Feature flags for runtime control
   - Circuit breaker for fault tolerance
   - Rate limiting for resource protection
   - Caching for performance
   - Audit logging for compliance
   - Metrics for monitoring
   - Event bus for decoupled communication
   - Validation for data integrity
   - Retry queue for resilience"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.middleware.rate-limit :as rate-limit]
   [mental-models.cache.core :as cache]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.validation.core :as validation]
   [mental-models.retry.queue :as retry]
   [mental-models.logging.structured :as log]
   [mental-models.registry.models :as registry]
   [mental-models.config.pipeline-config :as config]))

;; =============================================================================
;; PIPELINE STATE
;; =============================================================================

(defonce pipeline-state (atom {:initialized false
                               :components {}
                               :start-time nil}))

;; =============================================================================
;; COMPONENT INITIALIZATION
;; =============================================================================

(defn init-feature-flags! []
  (log/info "Initializing feature flags")
  (flags/init-default-flags!)
  (swap! pipeline-state assoc-in [:components :feature-flags] :initialized))

(defn init-circuit-breakers! []
  (log/info "Initializing circuit breakers")
  (cb/create-lm-studio-circuit-breaker)
  (cb/create-database-circuit-breaker)
  (cb/create-notification-circuit-breaker)
  (swap! pipeline-state assoc-in [:components :circuit-breakers] :initialized))

(defn init-cache! []
  (log/info "Initializing cache layer")
  (cache/create-cache! "analysis" {:max-size 1000 :ttl-ms 3600000})
  (cache/create-cache! "models" {:max-size 100 :ttl-ms 86400000})
  (swap! pipeline-state assoc-in [:components :cache] :initialized))

(defn init-metrics! []
  (log/info "Initializing metrics")
  (metrics/create-counter! :pipeline/documents-processed "Total documents processed")
  (metrics/create-counter! :pipeline/analysis-requests "Total analysis requests")
  (metrics/create-counter! :pipeline/lollapalooza-alerts "Total Lollapalooza alerts")
  (metrics/create-histogram! :pipeline/analysis-latency "Analysis latency in ms" [10 50 100 250 500 1000 2500 5000])
  (metrics/create-gauge! :pipeline/active-workers "Number of active workers")
  (swap! pipeline-state assoc-in [:components :metrics] :initialized))

(defn init-event-bus! []
  (log/info "Initializing event bus")
  (events/start-event-bus!)
  (swap! pipeline-state assoc-in [:components :event-bus] :initialized))

(defn init-retry-queue! []
  (log/info "Initializing retry queue")
  (retry/start-retry-worker!)
  (swap! pipeline-state assoc-in [:components :retry-queue] :initialized))

(defn init-model-registry! []
  (log/info "Initializing model registry with Munger tendencies")
  (registry/init-core-models!)
  (swap! pipeline-state assoc-in [:components :model-registry] :initialized))

(defn init-audit! []
  (log/info "Initializing audit logging")
  (audit/start-audit-logger!)
  (swap! pipeline-state assoc-in [:components :audit] :initialized))

;; =============================================================================
;; PIPELINE INITIALIZATION
;; =============================================================================

(defn init-pipeline!
  "Initialize all pipeline components."
  []
  (log/info "Starting pipeline initialization")
  (let [start-time (System/currentTimeMillis)]
    (init-feature-flags!)
    (init-circuit-breakers!)
    (init-cache!)
    (init-metrics!)
    (init-event-bus!)
    (init-retry-queue!)
    (init-model-registry!)
    (init-audit!)
    (swap! pipeline-state assoc
           :initialized true
           :start-time start-time)
    (log/info "Pipeline initialized" {:duration-ms (- (System/currentTimeMillis) start-time)})
    @pipeline-state))

(defn shutdown-pipeline!
  "Shutdown all pipeline components."
  []
  (log/info "Shutting down pipeline")
  (events/stop-event-bus!)
  (retry/stop-retry-worker!)
  (audit/stop-audit-logger!)
  (reset! pipeline-state {:initialized false :components {} :start-time nil})
  (log/info "Pipeline shutdown complete"))

;; =============================================================================
;; INTEGRATED ANALYSIS FLOW
;; =============================================================================

(defn with-pipeline-middleware
  "Wrap a function with all pipeline middleware."
  [f]
  (fn [& args]
    ;; Check feature flag
    (if (flags/is-enabled? "streaming-analysis")
      ;; Check rate limit
      (if (rate-limit/allow-request? "analysis" (first args))
        ;; Execute with circuit breaker
        (let [result (cb/execute (cb/get-circuit-breaker "lm-studio")
                                 #(apply f args))]
          (if (= :success (:status result))
            ;; Record metrics
            (do
              (metrics/inc-counter! :pipeline/analysis-requests)
              (:result result))
            ;; Handle failure
            (do
              (log/warn "Analysis failed" {:status (:status result)})
                            (when (= :failure (:status result))
                              (retry/enqueue! :analysis {:args args
                                                         :error (:error result)}))
              nil)))
        (do
          (log/warn "Rate limit exceeded")
          nil))
      (do
        (log/info "Streaming analysis disabled by feature flag")
        nil))))

(defn analyze-with-pipeline
  "Analyze text using the integrated pipeline."
  [text & {:keys [cache-key skip-cache] :or {skip-cache false}}]
  (let [cache-key (or cache-key (str (hash text)))]
    ;; Check cache first
    (if (and (not skip-cache)
             (flags/is-enabled? "cache-enabled")
             (cache/get-value "analysis" cache-key))
      (do
        (log/debug "Cache hit" {:key cache-key})
        (cache/get-value "analysis" cache-key))
      ;; Perform analysis
      (let [start-time (System/currentTimeMillis)
            ;; Validate input
            _ (when-not (validation/validate-string text {:min-length 1 :max-length 100000})
                (throw (ex-info "Invalid input text" {:text-length (count text)})))
            ;; Audit the request
            _ (audit/log-operation! {:operation :analyze
                                     :input-length (count text)
                                     :timestamp (System/currentTimeMillis)})
            ;; Perform analysis (placeholder - would call LM Studio)
            result {:text text
                    :models-detected []
                    :confidence 0.0
                    :timestamp (System/currentTimeMillis)}
            duration (- (System/currentTimeMillis) start-time)]
        ;; Record metrics
        (metrics/observe-histogram! :pipeline/analysis-latency duration)
        (metrics/inc-counter! :pipeline/documents-processed)
        ;; Cache result
        (when (flags/is-enabled? "cache-enabled")
          (cache/put! "analysis" cache-key result))
        ;; Publish event
        (events/publish! :analysis/completed {:result result :duration duration})
        result))))

;; =============================================================================
;; LOLLAPALOOZA DETECTION WITH PIPELINE
;; =============================================================================

(defn detect-lollapalooza
  "Detect Lollapalooza effect with pipeline integration."
  [analysis-result]
  (let [models (:models-detected analysis-result)
        high-confidence (filter #(>= (:confidence %) 0.7) models)]
    (when (>= (count high-confidence) 3)
      (let [alert {:type :lollapalooza
                   :models (map :model-id high-confidence)
                   :confidence (/ (reduce + (map :confidence high-confidence))
                                  (count high-confidence))
                   :timestamp (System/currentTimeMillis)}]
        ;; Record metric
        (metrics/inc-counter! :pipeline/lollapalooza-alerts)
        ;; Audit the alert
        (audit/log-operation! {:operation :lollapalooza-alert
                               :models (:models alert)
                               :confidence (:confidence alert)})
        ;; Publish event
        (events/publish! :lollapalooza/detected alert)
        ;; Log alert
        (log/warn "Lollapalooza detected!" alert)
        alert))))

;; =============================================================================
;; BATCH PROCESSING WITH PIPELINE
;; =============================================================================

(defn process-batch-with-pipeline
  "Process a batch of documents with full pipeline integration."
  [documents & {:keys [concurrency] :or {concurrency 4}}]
  (log/info "Starting batch processing" {:count (count documents) :concurrency concurrency})
  (let [start-time (System/currentTimeMillis)
        ;; Set active workers gauge
        _ (metrics/set-gauge! :pipeline/active-workers concurrency)
        ;; Process documents
        results (doall
                 (pmap (fn [doc]
                         (try
                           (analyze-with-pipeline (:text doc))
                           (catch Exception e
                             (log/error "Batch item failed" {:error (.getMessage e)})
                                                          (retry/enqueue! :batch-item {:document doc
                                                                             :error e})
                             nil)))
                       documents))
        ;; Reset workers gauge
        _ (metrics/set-gauge! :pipeline/active-workers 0)
        duration (- (System/currentTimeMillis) start-time)
        successful (count (filter some? results))]
    ;; Audit batch completion
    (audit/log-operation! {:operation :batch-complete
                           :total (count documents)
                           :successful successful
                           :duration-ms duration})
    ;; Publish event
    (events/publish! :batch/completed {:total (count documents)
                                       :successful successful
                                       :duration duration})
    {:total (count documents)
     :successful successful
     :failed (- (count documents) successful)
     :duration-ms duration
     :results (filter some? results)}))

;; =============================================================================
;; PIPELINE STATUS
;; =============================================================================

(defn get-pipeline-status []
  (let [state @pipeline-state]
    {:initialized (:initialized state)
     :components (:components state)
     :uptime-ms (when (:start-time state)
                  (- (System/currentTimeMillis) (:start-time state)))
     :feature-flags (flags/get-stats)
     :circuit-breakers (cb/get-all-stats)
     :cache-stats (cache/get-all-stats)
     :metrics (metrics/get-all-metrics)
     :retry-queue-size (retry/get-queue-size)
     :model-count (registry/get-model-count)}))

(defn health-check []
  (let [status (get-pipeline-status)]
    {:healthy (:initialized status)
     :components (into {}
                       (map (fn [[k v]] [k (= v :initialized)])
                            (:components status)))
     :uptime-ms (:uptime-ms status)}))
