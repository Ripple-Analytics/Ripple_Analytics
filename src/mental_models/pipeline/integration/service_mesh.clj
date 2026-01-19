(ns mental-models.pipeline.integration.service-mesh
  "Service Mesh Module
   
   Microservice communication:
   - Service discovery
   - Load balancing
   - Health checking
   - Circuit breaking
   - Request routing"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]
   [java.util.concurrent.atomic AtomicInteger]))

;; =============================================================================
;; SERVICE MESH STATE
;; =============================================================================

(defonce mesh-state (atom {:services {}
                           :instances {}
                           :routes {}
                           :health-checks {}
                           :circuit-breakers {}
                           :load-balancers {}
                           :config {:health-check-interval-ms 10000
                                    :circuit-breaker-threshold 5
                                    :circuit-breaker-timeout-ms 30000
                                    :default-timeout-ms 5000}}))

;; =============================================================================
;; SERVICE REGISTRATION
;; =============================================================================

(defn register-service!
  "Register a service."
  [service-id {:keys [name version endpoints metadata]}]
  (log/info "Registering service" {:id service-id :name name})
  (swap! mesh-state assoc-in [:services service-id]
         {:id service-id
          :name name
          :version (or version "1.0.0")
          :endpoints (or endpoints [])
          :metadata (or metadata {})
          :status :active
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :mesh/services-registered)
  service-id)

(defn unregister-service!
  "Unregister a service."
  [service-id]
  (log/info "Unregistering service" {:id service-id})
  (swap! mesh-state update :services dissoc service-id)
  ;; Remove all instances
  (swap! mesh-state update :instances
         (fn [instances]
           (into {} (remove (fn [[_ inst]] (= (:service-id inst) service-id)) instances)))))

(defn get-service
  "Get a service by ID."
  [service-id]
  (get-in @mesh-state [:services service-id]))

(defn list-services
  "List all registered services."
  []
  (vals (:services @mesh-state)))

;; =============================================================================
;; INSTANCE MANAGEMENT
;; =============================================================================

(defn register-instance!
  "Register a service instance."
  [instance-id {:keys [service-id host port weight]}]
  (log/info "Registering instance" {:id instance-id :service service-id})
  (swap! mesh-state assoc-in [:instances instance-id]
         {:id instance-id
          :service-id service-id
          :host host
          :port port
          :weight (or weight 1)
          :status :healthy
          :last-health-check nil
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :mesh/instances-registered)
  instance-id)

(defn unregister-instance!
  "Unregister a service instance."
  [instance-id]
  (log/info "Unregistering instance" {:id instance-id})
  (swap! mesh-state update :instances dissoc instance-id))

(defn get-instance
  "Get an instance by ID."
  [instance-id]
  (get-in @mesh-state [:instances instance-id]))

(defn list-instances
  "List instances for a service."
  [service-id]
  (filter #(= (:service-id %) service-id) (vals (:instances @mesh-state))))

(defn healthy-instances
  "Get healthy instances for a service."
  [service-id]
  (filter #(= (:status %) :healthy) (list-instances service-id)))

;; =============================================================================
;; LOAD BALANCING
;; =============================================================================

(defonce round-robin-counters (atom {}))

(defn round-robin-select
  "Select an instance using round-robin."
  [service-id instances]
  (when (seq instances)
    (let [counter (or (get @round-robin-counters service-id) (AtomicInteger. 0))
          idx (mod (.getAndIncrement counter) (count instances))]
      (swap! round-robin-counters assoc service-id counter)
      (nth (vec instances) idx))))

(defn weighted-select
  "Select an instance using weighted random."
  [instances]
  (when (seq instances)
    (let [total-weight (reduce + (map :weight instances))
          rand-val (rand total-weight)]
      (loop [remaining instances
             cumulative 0]
        (when (seq remaining)
          (let [inst (first remaining)
                new-cumulative (+ cumulative (:weight inst))]
            (if (< rand-val new-cumulative)
              inst
              (recur (rest remaining) new-cumulative))))))))

(defn least-connections-select
  "Select instance with least connections."
  [instances]
  (when (seq instances)
    (first (sort-by #(get-in @mesh-state [:load-balancers (:id %) :connections] 0) instances))))

(defn select-instance
  "Select an instance for a service."
  [service-id & {:keys [strategy] :or {strategy :round-robin}}]
  (let [instances (healthy-instances service-id)]
    (case strategy
      :round-robin (round-robin-select service-id instances)
      :weighted (weighted-select instances)
      :least-connections (least-connections-select instances)
      :random (rand-nth (vec instances))
      (round-robin-select service-id instances))))

;; =============================================================================
;; HEALTH CHECKING
;; =============================================================================

(defn check-instance-health!
  "Check health of an instance."
  [instance-id health-fn]
  (try
    (let [healthy? (health-fn)]
      (swap! mesh-state update-in [:instances instance-id]
             merge {:status (if healthy? :healthy :unhealthy)
                    :last-health-check (System/currentTimeMillis)})
      (when (not healthy?)
        (metrics/inc-counter! :mesh/health-check-failures))
      healthy?)
    (catch Exception e
      (log/error "Health check failed" {:instance instance-id :error (.getMessage e)})
      (swap! mesh-state update-in [:instances instance-id]
             merge {:status :unhealthy
                    :last-health-check (System/currentTimeMillis)
                    :last-error (.getMessage e)})
      (metrics/inc-counter! :mesh/health-check-failures)
      false)))

(defn register-health-check!
  "Register a health check for an instance."
  [instance-id health-fn]
  (log/info "Registering health check" {:instance instance-id})
  (swap! mesh-state assoc-in [:health-checks instance-id] health-fn))

(defn run-health-checks!
  "Run all health checks."
  []
  (doseq [[instance-id health-fn] (:health-checks @mesh-state)]
    (check-instance-health! instance-id health-fn)))

;; =============================================================================
;; CIRCUIT BREAKER
;; =============================================================================

(defn get-circuit-state
  "Get circuit breaker state for an instance."
  [instance-id]
  (get-in @mesh-state [:circuit-breakers instance-id]
          {:state :closed :failures 0 :last-failure nil}))

(defn record-success!
  "Record a successful call."
  [instance-id]
  (swap! mesh-state update-in [:circuit-breakers instance-id]
         (fn [cb]
           (-> (or cb {:state :closed :failures 0})
               (assoc :failures 0)
               (assoc :state :closed)))))

(defn record-failure!
  "Record a failed call."
  [instance-id]
  (let [threshold (get-in @mesh-state [:config :circuit-breaker-threshold])]
    (swap! mesh-state update-in [:circuit-breakers instance-id]
           (fn [cb]
             (let [cb (or cb {:state :closed :failures 0})
                   failures (inc (:failures cb))]
               (if (>= failures threshold)
                 (do
                   (metrics/inc-counter! :mesh/circuit-opened)
                   (assoc cb :state :open :failures failures :opened-at (System/currentTimeMillis)))
                 (assoc cb :failures failures :last-failure (System/currentTimeMillis))))))))

(defn circuit-open?
  "Check if circuit is open."
  [instance-id]
  (let [cb (get-circuit-state instance-id)
        timeout (get-in @mesh-state [:config :circuit-breaker-timeout-ms])]
    (and (= (:state cb) :open)
         (< (- (System/currentTimeMillis) (:opened-at cb)) timeout))))

;; =============================================================================
;; REQUEST ROUTING
;; =============================================================================

(defn register-route!
  "Register a route."
  [route-id {:keys [path service-id methods middleware]}]
  (log/info "Registering route" {:id route-id :path path :service service-id})
  (swap! mesh-state assoc-in [:routes route-id]
         {:id route-id
          :path path
          :service-id service-id
          :methods (or methods [:get :post :put :delete])
          :middleware (or middleware [])
          :registered-at (System/currentTimeMillis)}))

(defn unregister-route!
  "Unregister a route."
  [route-id]
  (swap! mesh-state update :routes dissoc route-id))

(defn find-route
  "Find a route for a path."
  [path method]
  (first (filter (fn [[_ route]]
                   (and (str/starts-with? path (:path route))
                        (contains? (set (:methods route)) method)))
                 (:routes @mesh-state))))

(defn route-request
  "Route a request to a service."
  [path method & {:keys [strategy]}]
  (when-let [[_ route] (find-route path method)]
    (let [service-id (:service-id route)
          instance (select-instance service-id :strategy strategy)]
      (when instance
        {:instance instance
         :route route
         :url (str "http://" (:host instance) ":" (:port instance) path)}))))

;; =============================================================================
;; SERVICE DISCOVERY
;; =============================================================================

(defn discover-service
  "Discover a service by name."
  [service-name]
  (first (filter #(= (:name %) service-name) (list-services))))

(defn discover-instances
  "Discover instances for a service name."
  [service-name]
  (when-let [service (discover-service service-name)]
    (healthy-instances (:id service))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defonce health-check-executor (atom nil))

(defn start-health-checker!
  "Start the health check scheduler."
  []
  (when (and (flags/is-enabled? "service-mesh")
             (nil? @health-check-executor))
    (log/info "Starting health checker")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @mesh-state [:config :health-check-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try (run-health-checks!)
                                  (catch Exception e
                                    (log/error "Health checker error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (reset! health-check-executor executor))))

(defn stop-health-checker!
  "Stop the health check scheduler."
  []
  (when-let [^ScheduledExecutorService executor @health-check-executor]
    (log/info "Stopping health checker")
    (.shutdown executor)
    (reset! health-check-executor nil)))

(defn init-service-mesh!
  "Initialize service mesh."
  []
  (log/info "Initializing service mesh")
  ;; Register feature flag
  (flags/register-flag! "service-mesh" "Enable service mesh" true)
  ;; Create metrics
  (metrics/create-counter! :mesh/services-registered "Services registered")
  (metrics/create-counter! :mesh/instances-registered "Instances registered")
  (metrics/create-counter! :mesh/health-check-failures "Health check failures")
  (metrics/create-counter! :mesh/circuit-opened "Circuit breakers opened")
  (metrics/create-gauge! :mesh/healthy-instances "Healthy instances"
                         #(count (filter #(= (:status %) :healthy) (vals (:instances @mesh-state)))))
  (log/info "Service mesh initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-mesh-status []
  {:enabled (flags/is-enabled? "service-mesh")
   :services (count (:services @mesh-state))
   :instances (count (:instances @mesh-state))
   :healthy-instances (count (filter #(= (:status %) :healthy) (vals (:instances @mesh-state))))
   :routes (count (:routes @mesh-state))
   :health-checks (count (:health-checks @mesh-state))
   :circuit-breakers (count (:circuit-breakers @mesh-state))
   :health-checker-running (some? @health-check-executor)
   :config (:config @mesh-state)})
