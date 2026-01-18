(ns mental-models.pipeline.orchestrator
  "Pipeline Orchestrator
   
   Unified orchestration layer that ties all pipeline components together:
   - Initializes all integration modules in correct order
   - Provides high-level API for pipeline operations
   - Manages pipeline lifecycle
   - Coordinates between components"
  (:require
   [mental-models.pipeline.integration.core :as core]
   [mental-models.pipeline.integration.lm-studio :as lm]
   [mental-models.pipeline.integration.database :as db]
   [mental-models.pipeline.integration.notifications :as notif]
   [mental-models.pipeline.integration.file-watcher :as watcher]
   [mental-models.features.flags :as flags]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.registry.models :as registry]
   [mental-models.config.pipeline-config :as config]
   [mental-models.health.checks :as health]
   [mental-models.logging.structured :as log]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.audit.core :as audit]))

;; =============================================================================
;; ORCHESTRATOR STATE
;; =============================================================================

(defonce orchestrator-state (atom {:initialized false
                                   :start-time nil
                                   :components {}
                                   :config {}}))

;; =============================================================================
;; COMPONENT INITIALIZATION ORDER
;; =============================================================================

(def initialization-order
  [:config
   :logging
   :metrics
   :events
   :audit
   :feature-flags
   :circuit-breakers
   :model-registry
   :database
   :notifications
   :file-watcher
   :health-checks])

;; =============================================================================
;; COMPONENT INITIALIZERS
;; =============================================================================

(defn init-component! [component-id]
  (log/info "Initializing component" {:component component-id})
  (try
    (case component-id
      :config (config/load-config!)
      :logging (log/init-logging!)
      :metrics (metrics/init-metrics!)
      :events (events/start-event-bus!)
      :audit (audit/start-audit-logger!)
      :feature-flags (flags/init-default-flags!)
      :circuit-breakers (do
                          (cb/create-lm-studio-circuit-breaker)
                          (cb/create-database-circuit-breaker)
                          (cb/create-notification-circuit-breaker))
      :model-registry (registry/init-core-models!)
      :database (db/init-database-integration!)
      :notifications (notif/init-notifications!)
      :file-watcher (watcher/init-file-watcher!)
      :health-checks (health/start-health-monitoring!))
    (swap! orchestrator-state assoc-in [:components component-id] :initialized)
    (log/info "Component initialized" {:component component-id})
    true
    (catch Exception e
      (log/error "Failed to initialize component" {:component component-id :error (.getMessage e)})
      (swap! orchestrator-state assoc-in [:components component-id] :failed)
      false)))

;; =============================================================================
;; PIPELINE LIFECYCLE
;; =============================================================================

(defn start-pipeline!
  "Start the entire pipeline with all components."
  [& {:keys [config] :or {config {}}}]
  (log/info "Starting Mental Models Pipeline")
  (let [start-time (System/currentTimeMillis)]
    (swap! orchestrator-state assoc :config config :start-time start-time)
    ;; Initialize all components in order
    (doseq [component initialization-order]
      (init-component! component))
    ;; Check if all components initialized
    (let [failed (filter #(= :failed (get-in @orchestrator-state [:components %])) initialization-order)]
      (if (empty? failed)
        (do
          (swap! orchestrator-state assoc :initialized true)
          (log/info "Pipeline started successfully" {:duration-ms (- (System/currentTimeMillis) start-time)})
          (events/publish! :pipeline/started {:timestamp (System/currentTimeMillis)})
          true)
        (do
          (log/error "Pipeline started with failures" {:failed-components failed})
          false)))))

(defn stop-pipeline!
  "Stop the entire pipeline gracefully."
  []
  (log/info "Stopping Mental Models Pipeline")
  ;; Stop components in reverse order
  (watcher/stop-watch-service!)
  (health/stop-health-monitoring!)
  (events/stop-event-bus!)
  (audit/stop-audit-logger!)
  (swap! orchestrator-state assoc :initialized false :components {})
  (log/info "Pipeline stopped")
  (events/publish! :pipeline/stopped {:timestamp (System/currentTimeMillis)}))

(defn restart-pipeline!
  "Restart the pipeline."
  []
  (log/info "Restarting pipeline")
  (stop-pipeline!)
  (Thread/sleep 1000)
  (start-pipeline!))

;; =============================================================================
;; HIGH-LEVEL API
;; =============================================================================

(defn analyze-text
  "Analyze text for mental models."
  [text & {:keys [threshold] :or {threshold 0.7}}]
  (when (:initialized @orchestrator-state)
    (lm/detect-models text :threshold threshold)))

(defn analyze-file
  "Analyze a file for mental models."
  [file-path]
  (when (:initialized @orchestrator-state)
    (watcher/process-file! (clojure.java.io/file file-path))))

(defn analyze-directory
  "Analyze all files in a directory."
  [dir-path & {:keys [recursive] :or {recursive false}}]
  (when (:initialized @orchestrator-state)
    (watcher/process-directory! dir-path :recursive recursive)))

(defn watch-directory
  "Start watching a directory for new files."
  [dir-path]
  (when (:initialized @orchestrator-state)
    (watcher/watch-directory! dir-path)))

(defn get-recent-analyses
  "Get recent analysis results."
  [& {:keys [limit] :or {limit 100}}]
  (when (:initialized @orchestrator-state)
    (db/get-recent-analyses :limit limit)))

(defn get-lollapalooza-history
  "Get Lollapalooza alert history."
  [& {:keys [limit] :or {limit 50}}]
  (when (:initialized @orchestrator-state)
    (db/get-lollapalooza-history :limit limit)))

(defn get-model-statistics
  "Get detection statistics for all models."
  []
  (when (:initialized @orchestrator-state)
    (db/get-model-statistics)))

;; =============================================================================
;; STATUS AND HEALTH
;; =============================================================================

(defn get-pipeline-status
  "Get comprehensive pipeline status."
  []
  {:initialized (:initialized @orchestrator-state)
   :uptime-ms (when (:start-time @orchestrator-state)
                (- (System/currentTimeMillis) (:start-time @orchestrator-state)))
   :components (:components @orchestrator-state)
   :config (:config @orchestrator-state)
   :health (health/get-health-status)
   :circuit-breakers (cb/get-all-stats)
   :feature-flags (flags/get-stats)
   :file-watcher (watcher/get-file-watcher-status)
   :notifications (notif/get-notification-status)
   :model-count (registry/get-model-count)})

(defn is-healthy?
  "Check if pipeline is healthy."
  []
  (and (:initialized @orchestrator-state)
       (every? #(= :initialized (get-in @orchestrator-state [:components %])) initialization-order)))

;; =============================================================================
;; CONVENIENCE MACROS
;; =============================================================================

(defmacro with-pipeline
  "Execute body with pipeline started, stopping when done."
  [& body]
  `(try
     (start-pipeline!)
     ~@body
     (finally
       (stop-pipeline!))))

;; =============================================================================
;; CLI ENTRY POINT
;; =============================================================================

(defn -main
  "Main entry point for pipeline."
  [& args]
  (let [command (first args)]
    (case command
      "start" (do
                (start-pipeline!)
                (println "Pipeline started. Press Ctrl+C to stop.")
                (.addShutdownHook (Runtime/getRuntime)
                                  (Thread. #(stop-pipeline!)))
                (while true (Thread/sleep 1000)))
      "analyze" (with-pipeline
                  (let [result (analyze-text (second args))]
                    (println "Analysis result:" result)))
      "watch" (with-pipeline
                (watch-directory (second args))
                (println "Watching directory:" (second args))
                (while true (Thread/sleep 1000)))
      "status" (println "Pipeline status:" (get-pipeline-status))
      (println "Usage: pipeline [start|analyze|watch|status] [args]"))))
