(ns mental-models.pipeline.integration.system-orchestrator
  "System Orchestrator Module
   
   Central orchestration for the Mental Models System:
   - Component lifecycle management
   - Pipeline coordination
   - Health monitoring
   - Graceful shutdown
   - System-wide configuration"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; SYSTEM ORCHESTRATOR STATE
;; =============================================================================

(defonce orchestrator-state (atom {:components (ConcurrentHashMap.)
                                   :pipelines (ConcurrentHashMap.)
                                   :health-checks (ConcurrentHashMap.)
                                   :shutdown-hooks (ConcurrentHashMap.)
                                   :system-status (AtomicReference. :stopped)
                                   :start-time (AtomicLong. 0)
                                   :scheduler nil
                                   :config {:health-check-interval-ms 30000
                                            :shutdown-timeout-ms 30000
                                            :max-startup-retries 3}}))

;; =============================================================================
;; COMPONENT MANAGEMENT
;; =============================================================================

(defn register-component!
  "Register a system component."
  [component-id {:keys [name description init-fn start-fn stop-fn health-fn dependencies priority]}]
  (log/info "Registering component" {:id component-id :name name})
  (.put ^ConcurrentHashMap (:components @orchestrator-state) component-id
        {:id component-id
         :name name
         :description description
         :init-fn init-fn
         :start-fn start-fn
         :stop-fn stop-fn
         :health-fn health-fn
         :dependencies (or dependencies [])
         :priority (or priority 100)
         :status :registered
         :last-health-check nil
         :health-status :unknown
         :error nil
         :registered-at (System/currentTimeMillis)}))

(defn get-component
  "Get a component by ID."
  [component-id]
  (.get ^ConcurrentHashMap (:components @orchestrator-state) component-id))

(defn list-components
  "List all components."
  [& {:keys [status]}]
  (let [components (vals (:components @orchestrator-state))]
    (if status
      (filter #(= (:status %) status) components)
      components)))

(defn update-component-status!
  "Update component status."
  [component-id status & {:keys [error]}]
  (when-let [component (get-component component-id)]
    (let [updated (assoc component
                         :status status
                         :error error
                         :updated-at (System/currentTimeMillis))]
      (.put ^ConcurrentHashMap (:components @orchestrator-state) component-id updated)
      (events/publish! :component/status-changed {:component-id component-id :status status})
      updated)))

;; =============================================================================
;; DEPENDENCY RESOLUTION
;; =============================================================================

(defn resolve-dependencies
  "Resolve component dependencies and return startup order."
  []
  (let [components (list-components)
        ;; Build dependency graph
        dep-graph (into {} (map (fn [c] [(:id c) (set (:dependencies c))]) components))
        ;; Topological sort
        sorted (loop [remaining (set (keys dep-graph))
                      result []
                      iterations 0]
                 (if (or (empty? remaining) (> iterations 100))
                   result
                   (let [ready (filter (fn [id]
                                         (empty? (set/intersection (get dep-graph id #{})
                                                                   remaining)))
                                       remaining)]
                     (if (empty? ready)
                       (do (log/warn "Circular dependency detected")
                           result)
                       (recur (set/difference remaining (set ready))
                              (concat result ready)
                              (inc iterations))))))]
    ;; Sort by priority within dependency order
    (sort-by (fn [id] (:priority (get-component id) 100)) sorted)))

;; =============================================================================
;; COMPONENT LIFECYCLE
;; =============================================================================

(defn init-component!
  "Initialize a single component."
  [component-id]
  (when-let [component (get-component component-id)]
    (log/info "Initializing component" {:id component-id})
    (try
      (when-let [init-fn (:init-fn component)]
        (init-fn))
      (update-component-status! component-id :initialized)
      true
      (catch Exception e
        (log/error "Component initialization failed" {:id component-id :error (.getMessage e)})
        (update-component-status! component-id :failed :error (.getMessage e))
        false))))

(defn start-component!
  "Start a single component."
  [component-id]
  (when-let [component (get-component component-id)]
    (when (= (:status component) :initialized)
      (log/info "Starting component" {:id component-id})
      (try
        (when-let [start-fn (:start-fn component)]
          (start-fn))
        (update-component-status! component-id :running)
        true
        (catch Exception e
          (log/error "Component start failed" {:id component-id :error (.getMessage e)})
          (update-component-status! component-id :failed :error (.getMessage e))
          false)))))

(defn stop-component!
  "Stop a single component."
  [component-id]
  (when-let [component (get-component component-id)]
    (when (= (:status component) :running)
      (log/info "Stopping component" {:id component-id})
      (try
        (when-let [stop-fn (:stop-fn component)]
          (stop-fn))
        (update-component-status! component-id :stopped)
        true
        (catch Exception e
          (log/error "Component stop failed" {:id component-id :error (.getMessage e)})
          (update-component-status! component-id :failed :error (.getMessage e))
          false)))))

;; =============================================================================
;; HEALTH CHECKS
;; =============================================================================

(defn check-component-health
  "Check health of a single component."
  [component-id]
  (when-let [component (get-component component-id)]
    (let [health-fn (:health-fn component)
          health (if health-fn
                   (try
                     (health-fn)
                     (catch Exception e
                       {:status :unhealthy :error (.getMessage e)}))
                   {:status :healthy})]
      (.put ^ConcurrentHashMap (:components @orchestrator-state) component-id
            (assoc component
                   :health-status (:status health)
                   :last-health-check (System/currentTimeMillis)
                   :health-details health))
      health)))

(defn check-all-health
  "Check health of all running components."
  []
  (let [running (list-components :status :running)
        results (map (fn [c]
                       {:component-id (:id c)
                        :health (check-component-health (:id c))})
                     running)
        all-healthy (every? #(= (get-in % [:health :status]) :healthy) results)]
    {:overall-status (if all-healthy :healthy :degraded)
     :components results
     :checked-at (System/currentTimeMillis)}))

(defn register-health-check!
  "Register a custom health check."
  [check-id {:keys [name check-fn interval-ms critical]}]
  (log/debug "Registering health check" {:id check-id :name name})
  (.put ^ConcurrentHashMap (:health-checks @orchestrator-state) check-id
        {:id check-id
         :name name
         :check-fn check-fn
         :interval-ms (or interval-ms 30000)
         :critical (if (nil? critical) true critical)
         :last-result nil
         :last-check nil}))

(defn run-health-checks
  "Run all registered health checks."
  []
  (let [checks (vals (:health-checks @orchestrator-state))
        results (map (fn [check]
                       (let [result (try
                                      ((:check-fn check))
                                      (catch Exception e
                                        {:status :unhealthy :error (.getMessage e)}))]
                         (.put ^ConcurrentHashMap (:health-checks @orchestrator-state) (:id check)
                               (assoc check
                                      :last-result result
                                      :last-check (System/currentTimeMillis)))
                         {:check-id (:id check)
                          :name (:name check)
                          :critical (:critical check)
                          :result result}))
                     checks)
        critical-failures (filter #(and (:critical %)
                                        (not= (get-in % [:result :status]) :healthy))
                                  results)]
    {:overall-status (if (empty? critical-failures) :healthy :unhealthy)
     :checks results
     :critical-failures (count critical-failures)
     :checked-at (System/currentTimeMillis)}))

;; =============================================================================
;; PIPELINE COORDINATION
;; =============================================================================

(defn register-pipeline!
  "Register a processing pipeline."
  [pipeline-id {:keys [name description stages enabled]}]
  (log/info "Registering pipeline" {:id pipeline-id :name name})
  (.put ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id
        {:id pipeline-id
         :name name
         :description description
         :stages (or stages [])
         :enabled (if (nil? enabled) true enabled)
         :execution-count (AtomicLong. 0)
         :last-execution nil
         :status :idle}))

(defn get-pipeline
  "Get a pipeline by ID."
  [pipeline-id]
  (.get ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id))

(defn list-pipelines
  "List all pipelines."
  [& {:keys [enabled-only]}]
  (let [pipelines (vals (:pipelines @orchestrator-state))]
    (if enabled-only
      (filter :enabled pipelines)
      pipelines)))

(defn execute-pipeline!
  "Execute a pipeline."
  [pipeline-id input]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (when (:enabled pipeline)
      (.incrementAndGet ^AtomicLong (:execution-count pipeline))
      (log/info "Executing pipeline" {:id pipeline-id})
      (let [start-time (System/currentTimeMillis)
            result (reduce (fn [data stage]
                             (try
                               ((:fn stage) data)
                               (catch Exception e
                                 (log/error "Pipeline stage failed" {:pipeline pipeline-id
                                                                     :stage (:name stage)
                                                                     :error (.getMessage e)})
                                 (reduced {:error (.getMessage e) :stage (:name stage)}))))
                           input
                           (:stages pipeline))
            duration (- (System/currentTimeMillis) start-time)]
        (.put ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id
              (assoc pipeline
                     :last-execution {:input input
                                      :result result
                                      :duration-ms duration
                                      :executed-at (System/currentTimeMillis)}))
        (events/publish! :pipeline/executed {:pipeline-id pipeline-id :duration-ms duration})
        result))))

;; =============================================================================
;; SHUTDOWN MANAGEMENT
;; =============================================================================

(defn register-shutdown-hook!
  "Register a shutdown hook."
  [hook-id {:keys [name fn priority]}]
  (log/debug "Registering shutdown hook" {:id hook-id :name name})
  (.put ^ConcurrentHashMap (:shutdown-hooks @orchestrator-state) hook-id
        {:id hook-id
         :name name
         :fn fn
         :priority (or priority 100)}))

(defn run-shutdown-hooks
  "Run all shutdown hooks in priority order."
  []
  (let [hooks (sort-by :priority (vals (:shutdown-hooks @orchestrator-state)))]
    (doseq [hook hooks]
      (log/info "Running shutdown hook" {:id (:id hook) :name (:name hook)})
      (try
        ((:fn hook))
        (catch Exception e
          (log/error "Shutdown hook failed" {:id (:id hook) :error (.getMessage e)}))))))

;; =============================================================================
;; SYSTEM LIFECYCLE
;; =============================================================================

(defn start-system!
  "Start the entire system."
  []
  (log/info "Starting Mental Models System")
  (.set ^AtomicReference (:system-status @orchestrator-state) :starting)
  (.set ^AtomicLong (:start-time @orchestrator-state) (System/currentTimeMillis))
  (let [startup-order (resolve-dependencies)
        max-retries (get-in @orchestrator-state [:config :max-startup-retries])]
    ;; Initialize all components
    (doseq [component-id startup-order]
      (loop [retries 0]
        (when (and (< retries max-retries)
                   (not (init-component! component-id)))
          (log/warn "Retrying component initialization" {:id component-id :attempt (inc retries)})
          (Thread/sleep 1000)
          (recur (inc retries)))))
    ;; Start all initialized components
    (doseq [component-id startup-order]
      (start-component! component-id))
    ;; Start health check scheduler
    (let [scheduler (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @orchestrator-state [:config :health-check-interval-ms])]
      (.scheduleAtFixedRate scheduler
                            #(try (run-health-checks) (catch Exception _))
                            interval interval TimeUnit/MILLISECONDS)
      (swap! orchestrator-state assoc :scheduler scheduler))
    (.set ^AtomicReference (:system-status @orchestrator-state) :running)
    (log/info "Mental Models System started" {:components (count startup-order)})
    (events/publish! :system/started {:components (count startup-order)})
    true))

(defn stop-system!
  "Stop the entire system gracefully."
  []
  (log/info "Stopping Mental Models System")
  (.set ^AtomicReference (:system-status @orchestrator-state) :stopping)
  ;; Stop scheduler
  (when-let [scheduler (:scheduler @orchestrator-state)]
    (.shutdown ^ScheduledExecutorService scheduler)
    (.awaitTermination ^ScheduledExecutorService scheduler
                       (get-in @orchestrator-state [:config :shutdown-timeout-ms])
                       TimeUnit/MILLISECONDS))
  ;; Run shutdown hooks
  (run-shutdown-hooks)
  ;; Stop components in reverse order
  (let [startup-order (resolve-dependencies)
        shutdown-order (reverse startup-order)]
    (doseq [component-id shutdown-order]
      (stop-component! component-id)))
  (.set ^AtomicReference (:system-status @orchestrator-state) :stopped)
  (log/info "Mental Models System stopped")
  (events/publish! :system/stopped {})
  true)

(defn restart-system!
  "Restart the entire system."
  []
  (log/info "Restarting Mental Models System")
  (stop-system!)
  (Thread/sleep 1000)
  (start-system!))

;; =============================================================================
;; SYSTEM STATUS
;; =============================================================================

(defn get-system-status
  "Get comprehensive system status."
  []
  (let [status (.get ^AtomicReference (:system-status @orchestrator-state))
        start-time (.get ^AtomicLong (:start-time @orchestrator-state))
        components (list-components)
        running-count (count (filter #(= (:status %) :running) components))
        health (when (= status :running) (check-all-health))]
    {:status status
     :uptime-ms (if (pos? start-time)
                  (- (System/currentTimeMillis) start-time)
                  0)
     :components {:total (count components)
                  :running running-count
                  :failed (count (filter #(= (:status %) :failed) components))}
     :pipelines {:total (.size ^ConcurrentHashMap (:pipelines @orchestrator-state))
                 :enabled (count (list-pipelines :enabled-only true))}
     :health health
     :config (:config @orchestrator-state)}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-orchestrator-stats
  "Get orchestrator statistics."
  []
  {:components (.size ^ConcurrentHashMap (:components @orchestrator-state))
   :pipelines (.size ^ConcurrentHashMap (:pipelines @orchestrator-state))
   :health-checks (.size ^ConcurrentHashMap (:health-checks @orchestrator-state))
   :shutdown-hooks (.size ^ConcurrentHashMap (:shutdown-hooks @orchestrator-state))
   :system-status (.get ^AtomicReference (:system-status @orchestrator-state))
   :uptime-ms (let [start (.get ^AtomicLong (:start-time @orchestrator-state))]
                (if (pos? start) (- (System/currentTimeMillis) start) 0))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-system-orchestrator!
  "Initialize system orchestrator."
  []
  (log/info "Initializing system orchestrator")
  ;; Register feature flag
  (flags/register-flag! "system-orchestrator" "Enable system orchestrator" true)
  ;; Create metrics
  (metrics/create-gauge! :systemorchestrator/components "Total components"
                         #(.size ^ConcurrentHashMap (:components @orchestrator-state)))
  (metrics/create-gauge! :systemorchestrator/pipelines "Total pipelines"
                         #(.size ^ConcurrentHashMap (:pipelines @orchestrator-state)))
  ;; Register JVM shutdown hook
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable (fn []
                                         (when (= (.get ^AtomicReference (:system-status @orchestrator-state)) :running)
                                           (stop-system!)))))
  (log/info "System orchestrator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-system-orchestrator-status []
  {:enabled (flags/is-enabled? "system-orchestrator")
   :system-status (.get ^AtomicReference (:system-status @orchestrator-state))
   :components (.size ^ConcurrentHashMap (:components @orchestrator-state))
   :pipelines (.size ^ConcurrentHashMap (:pipelines @orchestrator-state))
   :stats (get-orchestrator-stats)
   :config (:config @orchestrator-state)})
