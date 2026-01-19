(ns mental-models.pipeline.integration.system-initializer
  "System Initializer Module
   
   Unified system initialization:
   - Component startup ordering
   - Dependency resolution
   - Health verification
   - Graceful shutdown
   - Configuration loading"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.pipeline.integration.config-loader :as config]
   [mental-models.pipeline.integration.resource-pool :as pool]
   [mental-models.pipeline.integration.telemetry :as telemetry]
   [mental-models.pipeline.integration.task-queue :as queue]
   [mental-models.pipeline.integration.session-manager :as session]
   [mental-models.pipeline.integration.localization :as l10n]
   [mental-models.pipeline.integration.template-engine :as template]))

;; =============================================================================
;; SYSTEM STATE
;; =============================================================================

(defonce system-state (atom {:status :stopped
                             :components {}
                             :start-time nil
                             :shutdown-hooks []
                             :health-checks {}}))

;; =============================================================================
;; COMPONENT REGISTRY
;; =============================================================================

(def component-order
  "Components in startup order (dependencies first)."
  [:logging
   :config
   :metrics
   :events
   :flags
   :telemetry
   :resource-pool
   :task-queue
   :session
   :localization
   :template
   :api
   :pipeline])

(defn register-component!
  "Register a component with its lifecycle functions."
  [component-id {:keys [start stop health depends-on]}]
  (log/info "Registering component" {:id component-id})
  (swap! system-state assoc-in [:components component-id]
         {:id component-id
          :start start
          :stop stop
          :health health
          :depends-on (or depends-on [])
          :status :stopped
          :started-at nil}))

(defn get-component
  "Get a component by ID."
  [component-id]
  (get-in @system-state [:components component-id]))

(defn list-components
  "List all registered components."
  []
  (keys (:components @system-state)))

;; =============================================================================
;; DEPENDENCY RESOLUTION
;; =============================================================================

(defn resolve-start-order
  "Resolve component start order based on dependencies."
  []
  (let [components (:components @system-state)
        registered (set (keys components))]
    (filter registered component-order)))

(defn resolve-stop-order
  "Resolve component stop order (reverse of start)."
  []
  (reverse (resolve-start-order)))

;; =============================================================================
;; COMPONENT LIFECYCLE
;; =============================================================================

(defn start-component!
  "Start a single component."
  [component-id]
  (let [component (get-component component-id)]
    (when (and component (= :stopped (:status component)))
      (log/info "Starting component" {:id component-id})
      (try
        (when-let [start-fn (:start component)]
          (start-fn))
        (swap! system-state update-in [:components component-id]
               assoc :status :running :started-at (System/currentTimeMillis))
        (metrics/inc-counter! :system/components-started)
        (events/publish! :system/component-started {:component component-id})
        true
        (catch Exception e
          (log/error "Failed to start component" {:id component-id :error (.getMessage e)})
          (swap! system-state update-in [:components component-id]
                 assoc :status :failed :error (.getMessage e))
          false)))))

(defn stop-component!
  "Stop a single component."
  [component-id]
  (let [component (get-component component-id)]
    (when (and component (= :running (:status component)))
      (log/info "Stopping component" {:id component-id})
      (try
        (when-let [stop-fn (:stop component)]
          (stop-fn))
        (swap! system-state update-in [:components component-id]
               assoc :status :stopped :started-at nil)
        (metrics/inc-counter! :system/components-stopped)
        (events/publish! :system/component-stopped {:component component-id})
        true
        (catch Exception e
          (log/error "Failed to stop component" {:id component-id :error (.getMessage e)})
          false)))))

;; =============================================================================
;; HEALTH CHECKS
;; =============================================================================

(defn register-health-check!
  "Register a health check."
  [check-id check-fn]
  (swap! system-state assoc-in [:health-checks check-id] check-fn))

(defn run-health-check
  "Run a single health check."
  [check-id]
  (when-let [check-fn (get-in @system-state [:health-checks check-id])]
    (try
      {:id check-id
       :status (if (check-fn) :healthy :unhealthy)
       :checked-at (System/currentTimeMillis)}
      (catch Exception e
        {:id check-id
         :status :error
         :error (.getMessage e)
         :checked-at (System/currentTimeMillis)}))))

(defn run-all-health-checks
  "Run all health checks."
  []
  (let [checks (:health-checks @system-state)]
    (into {} (map (fn [[id _]] [id (run-health-check id)]) checks))))

(defn is-healthy?
  "Check if the system is healthy."
  []
  (let [results (run-all-health-checks)]
    (every? #(= :healthy (:status %)) (vals results))))

;; =============================================================================
;; SHUTDOWN HOOKS
;; =============================================================================

(defn register-shutdown-hook!
  "Register a shutdown hook."
  [hook-id hook-fn]
  (swap! system-state update :shutdown-hooks conj {:id hook-id :fn hook-fn}))

(defn run-shutdown-hooks!
  "Run all shutdown hooks."
  []
  (doseq [{:keys [id fn]} (:shutdown-hooks @system-state)]
    (log/info "Running shutdown hook" {:id id})
    (try
      (fn)
      (catch Exception e
        (log/error "Shutdown hook failed" {:id id :error (.getMessage e)})))))

;; =============================================================================
;; SYSTEM LIFECYCLE
;; =============================================================================

(defn start-system!
  "Start the entire system."
  []
  (log/info "Starting Mental Models System")
  (swap! system-state assoc :status :starting :start-time (System/currentTimeMillis))
  (events/publish! :system/starting {})
  (let [order (resolve-start-order)
        results (doall (map (fn [id]
                              [id (start-component! id)])
                            order))
        all-started (every? second results)]
    (if all-started
      (do
        (swap! system-state assoc :status :running)
        (events/publish! :system/started {:components (count order)})
        (log/info "System started successfully" {:components (count order)})
        true)
      (do
        (swap! system-state assoc :status :degraded)
        (log/warn "System started with failures" {:results results})
        false))))

(defn stop-system!
  "Stop the entire system."
  []
  (log/info "Stopping Mental Models System")
  (swap! system-state assoc :status :stopping)
  (events/publish! :system/stopping {})
  ;; Run shutdown hooks first
  (run-shutdown-hooks!)
  ;; Stop components in reverse order
  (let [order (resolve-stop-order)]
    (doseq [id order]
      (stop-component! id)))
  (swap! system-state assoc :status :stopped :start-time nil)
  (events/publish! :system/stopped {})
  (log/info "System stopped"))

(defn restart-system!
  "Restart the entire system."
  []
  (log/info "Restarting system")
  (stop-system!)
  (Thread/sleep 1000)
  (start-system!))

;; =============================================================================
;; DEFAULT COMPONENTS
;; =============================================================================

(defn register-default-components!
  "Register default system components."
  []
  ;; Logging
  (register-component! :logging
                       {:start #(log/info "Logging initialized")
                        :stop #(log/info "Logging stopped")
                        :health (constantly true)})
  ;; Config
  (register-component! :config
                       {:start config/init-config-loader!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging]})
  ;; Metrics
  (register-component! :metrics
                       {:start metrics/init-metrics!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging]})
  ;; Events
  (register-component! :events
                       {:start events/init-event-bus!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging]})
  ;; Flags
  (register-component! :flags
                       {:start flags/init-flags!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging :config]})
  ;; Telemetry
  (register-component! :telemetry
                       {:start telemetry/init-telemetry!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging :metrics]})
  ;; Resource Pool
  (register-component! :resource-pool
                       {:start (constantly nil)
                        :stop pool/destroy-all-pools!
                        :health (constantly true)
                        :depends-on [:logging :metrics]})
  ;; Task Queue
  (register-component! :task-queue
                       {:start (constantly nil)
                        :stop queue/stop-all-workers!
                        :health (constantly true)
                        :depends-on [:logging :metrics :events]})
  ;; Session
  (register-component! :session
                       {:start session/start-cleanup-scheduler!
                        :stop session/stop-cleanup-scheduler!
                        :health (constantly true)
                        :depends-on [:logging :metrics]})
  ;; Localization
  (register-component! :localization
                       {:start l10n/init-localization!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging :flags]})
  ;; Template
  (register-component! :template
                       {:start template/init-template-engine!
                        :stop (constantly nil)
                        :health (constantly true)
                        :depends-on [:logging :flags :metrics]}))

;; =============================================================================
;; DEFAULT HEALTH CHECKS
;; =============================================================================

(defn register-default-health-checks!
  "Register default health checks."
  []
  (register-health-check! :system
                          #(= :running (:status @system-state)))
  (register-health-check! :memory
                          #(let [runtime (Runtime/getRuntime)
                                 used (- (.totalMemory runtime) (.freeMemory runtime))
                                 max (.maxMemory runtime)
                                 usage (/ (double used) max)]
                             (< usage 0.9)))
  (register-health-check! :components
                          #(every? (fn [[_ c]] (not= :failed (:status c)))
                                   (:components @system-state))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-system!
  "Initialize the system (register components but don't start)."
  []
  (log/info "Initializing Mental Models System")
  ;; Create metrics
  (metrics/create-counter! :system/components-started "Components started")
  (metrics/create-counter! :system/components-stopped "Components stopped")
  (metrics/create-gauge! :system/uptime "System uptime"
                         #(if-let [start (:start-time @system-state)]
                            (- (System/currentTimeMillis) start)
                            0))
  ;; Register defaults
  (register-default-components!)
  (register-default-health-checks!)
  ;; Register JVM shutdown hook
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable stop-system!))
  (log/info "System initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-system-status
  "Get comprehensive system status."
  []
  {:status (:status @system-state)
   :uptime (when-let [start (:start-time @system-state)]
             (- (System/currentTimeMillis) start))
   :components (into {} (map (fn [[id c]]
                               [id {:status (:status c)
                                    :started-at (:started-at c)}])
                             (:components @system-state)))
   :health (run-all-health-checks)
   :memory (let [runtime (Runtime/getRuntime)]
             {:used (- (.totalMemory runtime) (.freeMemory runtime))
              :free (.freeMemory runtime)
              :total (.totalMemory runtime)
              :max (.maxMemory runtime)})})
