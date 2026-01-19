(ns mental-models.pipeline.integration.dependency-injection
  "Dependency Injection Module
   
   Component dependency management:
   - Service registration
   - Dependency resolution
   - Lifecycle management
   - Scoped instances
   - Lazy initialization"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; CONTAINER STATE
;; =============================================================================

(defonce container (atom {:services {}
                          :instances {}
                          :scopes {}
                          :lifecycle-order []}))

;; =============================================================================
;; SERVICE REGISTRATION
;; =============================================================================

(defn register-service!
  "Register a service with the container."
  [service-id & {:keys [factory dependencies scope lifecycle]
                 :or {dependencies [] scope :singleton lifecycle {}}}]
  (log/info "Registering service" {:id service-id :scope scope :deps dependencies})
  (swap! container update :services assoc service-id
         {:id service-id
          :factory factory
          :dependencies dependencies
          :scope scope
          :lifecycle lifecycle
          :registered-at (System/currentTimeMillis)})
  ;; Add to lifecycle order if has start/stop
  (when (or (:start lifecycle) (:stop lifecycle))
    (swap! container update :lifecycle-order conj service-id))
  service-id)

(defn register-value!
  "Register a constant value as a service."
  [service-id value]
  (log/info "Registering value" {:id service-id})
  (swap! container update :services assoc service-id
         {:id service-id
          :factory (constantly value)
          :dependencies []
          :scope :singleton
          :lifecycle {}
          :registered-at (System/currentTimeMillis)})
  (swap! container assoc-in [:instances service-id] value)
  service-id)

(defn unregister-service!
  "Unregister a service from the container."
  [service-id]
  (log/info "Unregistering service" {:id service-id})
  (swap! container update :services dissoc service-id)
  (swap! container update :instances dissoc service-id)
  (swap! container update :lifecycle-order #(remove #{service-id} %)))

(defn get-service-definition
  "Get a service definition."
  [service-id]
  (get-in @container [:services service-id]))

;; =============================================================================
;; DEPENDENCY RESOLUTION
;; =============================================================================

(defn resolve-dependencies
  "Resolve dependencies for a service."
  [service-id visited]
  (when (contains? visited service-id)
    (throw (ex-info "Circular dependency detected" {:service service-id :visited visited})))
  (let [service (get-service-definition service-id)]
    (when-not service
      (throw (ex-info "Service not found" {:service service-id})))
    (let [deps (:dependencies service)
          visited' (conj visited service-id)]
      (mapv #(resolve-service! % visited') deps))))

(defn create-instance
  "Create an instance of a service."
  [service-id visited]
  (let [service (get-service-definition service-id)
        deps (resolve-dependencies service-id visited)
        factory (:factory service)]
    (log/debug "Creating instance" {:service service-id})
    (apply factory deps)))

(defn resolve-service!
  "Resolve a service, creating it if necessary."
  [service-id & [visited]]
  (let [visited (or visited #{})
        service (get-service-definition service-id)]
    (when-not service
      (throw (ex-info "Service not found" {:service service-id})))
    (case (:scope service)
      :singleton
      (if-let [instance (get-in @container [:instances service-id])]
        instance
        (let [instance (create-instance service-id visited)]
          (swap! container assoc-in [:instances service-id] instance)
          (metrics/inc-counter! :di/instances-created)
          instance))
      
      :transient
      (create-instance service-id visited)
      
      :scoped
      (if-let [scope-id (get-in @container [:current-scope])]
        (if-let [instance (get-in @container [:scopes scope-id service-id])]
          instance
          (let [instance (create-instance service-id visited)]
            (swap! container assoc-in [:scopes scope-id service-id] instance)
            instance))
        (throw (ex-info "No active scope for scoped service" {:service service-id}))))))

(defn inject
  "Inject dependencies into a function."
  [f & service-ids]
  (let [deps (mapv resolve-service! service-ids)]
    (apply f deps)))

;; =============================================================================
;; SCOPED INSTANCES
;; =============================================================================

(defn begin-scope!
  "Begin a new scope."
  [scope-id]
  (log/debug "Beginning scope" {:scope-id scope-id})
  (swap! container assoc :current-scope scope-id)
  (swap! container assoc-in [:scopes scope-id] {})
  scope-id)

(defn end-scope!
  "End the current scope and clean up instances."
  []
  (when-let [scope-id (get-in @container [:current-scope])]
    (log/debug "Ending scope" {:scope-id scope-id})
    (swap! container update :scopes dissoc scope-id)
    (swap! container dissoc :current-scope)))

(defmacro with-scope
  "Execute body within a scope."
  [scope-id & body]
  `(do
     (begin-scope! ~scope-id)
     (try
       ~@body
       (finally
         (end-scope!)))))

;; =============================================================================
;; LIFECYCLE MANAGEMENT
;; =============================================================================

(defn start-service!
  "Start a service if it has a start lifecycle hook."
  [service-id]
  (let [service (get-service-definition service-id)
        start-fn (get-in service [:lifecycle :start])]
    (when start-fn
      (log/info "Starting service" {:id service-id})
      (let [instance (resolve-service! service-id)]
        (start-fn instance)
        (events/publish! :di/service-started {:service service-id})))))

(defn stop-service!
  "Stop a service if it has a stop lifecycle hook."
  [service-id]
  (let [service (get-service-definition service-id)
        stop-fn (get-in service [:lifecycle :stop])
        instance (get-in @container [:instances service-id])]
    (when (and stop-fn instance)
      (log/info "Stopping service" {:id service-id})
      (stop-fn instance)
      (events/publish! :di/service-stopped {:service service-id}))))

(defn start-all!
  "Start all services in lifecycle order."
  []
  (log/info "Starting all services")
  (doseq [service-id (:lifecycle-order @container)]
    (start-service! service-id)))

(defn stop-all!
  "Stop all services in reverse lifecycle order."
  []
  (log/info "Stopping all services")
  (doseq [service-id (reverse (:lifecycle-order @container))]
    (stop-service! service-id)))

;; =============================================================================
;; LAZY INITIALIZATION
;; =============================================================================

(defn lazy-service
  "Create a lazy reference to a service."
  [service-id]
  (delay (resolve-service! service-id)))

(defn force-service
  "Force resolution of a lazy service."
  [lazy-ref]
  @lazy-ref)

;; =============================================================================
;; DECORATORS
;; =============================================================================

(defn decorate-service!
  "Decorate a service with additional behavior."
  [service-id decorator-fn]
  (let [service (get-service-definition service-id)
        original-factory (:factory service)
        decorated-factory (fn [& deps]
                            (decorator-fn (apply original-factory deps)))]
    (swap! container assoc-in [:services service-id :factory] decorated-factory)
    ;; Clear cached instance
    (swap! container update :instances dissoc service-id)))

;; =============================================================================
;; QUERY FUNCTIONS
;; =============================================================================

(defn list-services
  "List all registered services."
  []
  (keys (:services @container)))

(defn list-instances
  "List all created instances."
  []
  (keys (:instances @container)))

(defn get-dependency-graph
  "Get the dependency graph for all services."
  []
  (into {} (map (fn [[id service]]
                  [id (:dependencies service)])
                (:services @container))))

(defn validate-dependencies
  "Validate that all dependencies can be resolved."
  []
  (let [errors (atom [])]
    (doseq [[service-id service] (:services @container)]
      (doseq [dep (:dependencies service)]
        (when-not (get-service-definition dep)
          (swap! errors conj {:service service-id :missing-dep dep}))))
    (if (empty? @errors)
      {:valid true}
      {:valid false :errors @errors})))

;; =============================================================================
;; RESET
;; =============================================================================

(defn reset-container!
  "Reset the container to initial state."
  []
  (log/warn "Resetting DI container")
  (stop-all!)
  (reset! container {:services {}
                     :instances {}
                     :scopes {}
                     :lifecycle-order []}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-dependency-injection!
  "Initialize dependency injection system."
  []
  (log/info "Initializing dependency injection")
  ;; Register feature flag
  (flags/register-flag! "dependency-injection" "Enable dependency injection" true)
  ;; Create metrics
  (metrics/create-counter! :di/instances-created "Instances created")
  (metrics/create-gauge! :di/registered-services "Registered services"
                         #(count (:services @container)))
  (metrics/create-gauge! :di/active-instances "Active instances"
                         #(count (:instances @container)))
  (log/info "Dependency injection initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-di-status []
  {:enabled (flags/is-enabled? "dependency-injection")
   :registered-services (count (:services @container))
   :active-instances (count (:instances @container))
   :lifecycle-services (count (:lifecycle-order @container))
   :validation (validate-dependencies)})
