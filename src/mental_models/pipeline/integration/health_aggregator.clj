(ns mental-models.pipeline.integration.health-aggregator
  "Health Aggregator Module
   
   System health monitoring:
   - Health check aggregation
   - Component health tracking
   - Dependency health
   - Health history
   - Alert thresholds"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; HEALTH STATE
;; =============================================================================

(defonce health-state (atom {:components {}
                             :dependencies {}
                             :history []
                             :alerts {}
                             :scheduler nil
                             :config {:check-interval-ms 30000
                                      :history-size 100
                                      :unhealthy-threshold 3
                                      :degraded-threshold 1}}))

;; =============================================================================
;; HEALTH STATUS
;; =============================================================================

(def health-statuses #{:healthy :degraded :unhealthy :unknown})

(defn status-priority
  "Get priority of a health status (lower is worse)."
  [status]
  (case status
    :healthy 3
    :degraded 2
    :unhealthy 1
    :unknown 0
    0))

(defn worst-status
  "Get the worst status from a collection."
  [statuses]
  (first (sort-by status-priority statuses)))

;; =============================================================================
;; COMPONENT REGISTRATION
;; =============================================================================

(defn register-component!
  "Register a health check component."
  [component-id {:keys [name check-fn dependencies critical? timeout-ms]}]
  (log/info "Registering health component" {:id component-id :name name})
  (swap! health-state assoc-in [:components component-id]
         {:id component-id
          :name (or name (str component-id))
          :check-fn check-fn
          :dependencies (or dependencies [])
          :critical? (if (nil? critical?) false critical?)
          :timeout-ms (or timeout-ms 5000)
          :status :unknown
          :last-check nil
          :consecutive-failures 0
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :health/components-registered)
  component-id)

(defn unregister-component!
  "Unregister a health check component."
  [component-id]
  (log/info "Unregistering health component" {:id component-id})
  (swap! health-state update :components dissoc component-id))

(defn get-component
  "Get a health check component."
  [component-id]
  (get-in @health-state [:components component-id]))

(defn list-components
  "List all health check components."
  []
  (keys (:components @health-state)))

;; =============================================================================
;; DEPENDENCY REGISTRATION
;; =============================================================================

(defn register-dependency!
  "Register an external dependency."
  [dependency-id {:keys [name check-fn critical? timeout-ms]}]
  (log/info "Registering health dependency" {:id dependency-id :name name})
  (swap! health-state assoc-in [:dependencies dependency-id]
         {:id dependency-id
          :name (or name (str dependency-id))
          :check-fn check-fn
          :critical? (if (nil? critical?) true critical?)
          :timeout-ms (or timeout-ms 5000)
          :status :unknown
          :last-check nil
          :consecutive-failures 0
          :created-at (System/currentTimeMillis)})
  dependency-id)

(defn unregister-dependency!
  "Unregister an external dependency."
  [dependency-id]
  (log/info "Unregistering health dependency" {:id dependency-id})
  (swap! health-state update :dependencies dissoc dependency-id))

(defn get-dependency
  "Get an external dependency."
  [dependency-id]
  (get-in @health-state [:dependencies dependency-id]))

(defn list-dependencies
  "List all external dependencies."
  []
  (keys (:dependencies @health-state)))

;; =============================================================================
;; HEALTH CHECKS
;; =============================================================================

(defn check-health
  "Check health of a component or dependency."
  [item]
  (try
    (let [check-fn (:check-fn item)
          result (if check-fn
                   (check-fn)
                   {:status :healthy})]
      (if (map? result)
        result
        {:status (if result :healthy :unhealthy)}))
    (catch Exception e
      (log/error "Health check failed" {:id (:id item) :error (.getMessage e)})
      {:status :unhealthy :error (.getMessage e)})))

(defn update-component-health!
  "Update health status of a component."
  [component-id]
  (when-let [component (get-component component-id)]
    (let [result (check-health component)
          status (:status result)
          prev-status (:status component)
          failures (if (= status :healthy)
                     0
                     (inc (:consecutive-failures component)))]
      (swap! health-state update-in [:components component-id]
             merge {:status status
                    :last-check (System/currentTimeMillis)
                    :last-result result
                    :consecutive-failures failures})
      ;; Check for status change
      (when (not= status prev-status)
        (events/publish! :health/status-changed {:component component-id
                                                  :from prev-status
                                                  :to status}))
      ;; Check for alert threshold
      (when (>= failures (get-in @health-state [:config :unhealthy-threshold]))
        (events/publish! :health/alert {:component component-id :status status}))
      result)))

(defn update-dependency-health!
  "Update health status of a dependency."
  [dependency-id]
  (when-let [dependency (get-dependency dependency-id)]
    (let [result (check-health dependency)
          status (:status result)
          prev-status (:status dependency)
          failures (if (= status :healthy)
                     0
                     (inc (:consecutive-failures dependency)))]
      (swap! health-state update-in [:dependencies dependency-id]
             merge {:status status
                    :last-check (System/currentTimeMillis)
                    :last-result result
                    :consecutive-failures failures})
      (when (not= status prev-status)
        (events/publish! :health/dependency-changed {:dependency dependency-id
                                                      :from prev-status
                                                      :to status}))
      result)))

(defn check-all-components!
  "Check health of all components."
  []
  (doseq [component-id (list-components)]
    (update-component-health! component-id)))

(defn check-all-dependencies!
  "Check health of all dependencies."
  []
  (doseq [dependency-id (list-dependencies)]
    (update-dependency-health! dependency-id)))

(defn check-all!
  "Check health of all components and dependencies."
  []
  (check-all-components!)
  (check-all-dependencies!))

;; =============================================================================
;; HEALTH AGGREGATION
;; =============================================================================

(defn get-component-health
  "Get health status of a specific component."
  [component-id]
  (when-let [component (get-component component-id)]
    {:id component-id
     :name (:name component)
     :status (:status component)
     :last-check (:last-check component)
     :critical? (:critical? component)
     :consecutive-failures (:consecutive-failures component)}))

(defn get-dependency-health
  "Get health status of a specific dependency."
  [dependency-id]
  (when-let [dependency (get-dependency dependency-id)]
    {:id dependency-id
     :name (:name dependency)
     :status (:status dependency)
     :last-check (:last-check dependency)
     :critical? (:critical? dependency)
     :consecutive-failures (:consecutive-failures dependency)}))

(defn aggregate-health
  "Aggregate health status from all components and dependencies."
  []
  (let [components (vals (:components @health-state))
        dependencies (vals (:dependencies @health-state))
        all-items (concat components dependencies)
        critical-items (filter :critical? all-items)
        statuses (map :status all-items)
        critical-statuses (map :status critical-items)
        overall-status (cond
                         (some #(= % :unhealthy) critical-statuses) :unhealthy
                         (some #(= % :unhealthy) statuses) :degraded
                         (some #(= % :degraded) statuses) :degraded
                         (every? #(= % :healthy) statuses) :healthy
                         :else :unknown)]
    {:status overall-status
     :timestamp (System/currentTimeMillis)
     :components (into {} (for [c components] [(:id c) (:status c)]))
     :dependencies (into {} (for [d dependencies] [(:id d) (:status d)]))
     :healthy-count (count (filter #(= (:status %) :healthy) all-items))
     :unhealthy-count (count (filter #(= (:status %) :unhealthy) all-items))
     :degraded-count (count (filter #(= (:status %) :degraded) all-items))
     :total-count (count all-items)}))

;; =============================================================================
;; HEALTH HISTORY
;; =============================================================================

(defn record-health-snapshot!
  "Record a health snapshot to history."
  []
  (let [snapshot (aggregate-health)
        history-size (get-in @health-state [:config :history-size])]
    (swap! health-state update :history
           (fn [history]
             (take history-size (conj history snapshot))))))

(defn get-health-history
  "Get health history."
  [& {:keys [limit] :or {limit 10}}]
  (take limit (:history @health-state)))

(defn get-health-trend
  "Get health trend over time."
  []
  (let [history (get-health-history :limit 10)]
    {:samples (count history)
     :healthy-ratio (if (seq history)
                      (/ (count (filter #(= (:status %) :healthy) history))
                         (count history))
                      0)
     :trend (if (>= (count history) 2)
              (let [recent (first history)
                    older (last history)]
                (cond
                  (> (status-priority (:status recent))
                     (status-priority (:status older))) :improving
                  (< (status-priority (:status recent))
                     (status-priority (:status older))) :degrading
                  :else :stable))
              :unknown)}))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-health-scheduler!
  "Start the health check scheduler."
  []
  (when (and (flags/is-enabled? "health-aggregator")
             (nil? (:scheduler @health-state)))
    (log/info "Starting health check scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @health-state [:config :check-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (check-all!)
                               (record-health-snapshot!)
                               (catch Exception e
                                 (log/error "Health check error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! health-state assoc :scheduler executor))))

(defn stop-health-scheduler!
  "Stop the health check scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @health-state)]
    (log/info "Stopping health check scheduler")
    (.shutdown executor)
    (swap! health-state assoc :scheduler nil)))

;; =============================================================================
;; LIVENESS/READINESS PROBES
;; =============================================================================

(defn liveness-check
  "Kubernetes-style liveness check."
  []
  (let [health (aggregate-health)]
    {:alive (not= (:status health) :unhealthy)
     :status (:status health)}))

(defn readiness-check
  "Kubernetes-style readiness check."
  []
  (let [health (aggregate-health)
        dependencies (vals (:dependencies @health-state))
        all-deps-healthy? (every? #(= (:status %) :healthy) dependencies)]
    {:ready (and (not= (:status health) :unhealthy)
                 all-deps-healthy?)
     :status (:status health)
     :dependencies-healthy all-deps-healthy?}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-health-aggregator!
  "Initialize health aggregator."
  []
  (log/info "Initializing health aggregator")
  ;; Register feature flag
  (flags/register-flag! "health-aggregator" "Enable health aggregator" true)
  ;; Create metrics
  (metrics/create-counter! :health/components-registered "Components registered")
  (metrics/create-gauge! :health/healthy-components "Healthy components"
                         #(count (filter #(= (:status %) :healthy) (vals (:components @health-state)))))
  (metrics/create-gauge! :health/unhealthy-components "Unhealthy components"
                         #(count (filter #(= (:status %) :unhealthy) (vals (:components @health-state)))))
  (log/info "Health aggregator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-health-status []
  (let [health (aggregate-health)]
    {:enabled (flags/is-enabled? "health-aggregator")
     :overall-status (:status health)
     :components (count (:components @health-state))
     :dependencies (count (:dependencies @health-state))
     :healthy (:healthy-count health)
     :unhealthy (:unhealthy-count health)
     :degraded (:degraded-count health)
     :scheduler-running (some? (:scheduler @health-state))
     :trend (get-health-trend)
     :config (:config @health-state)}))
