(ns mental-models.pipeline.integration.dependency-injector
  "Dependency injector for mental model analysis system.
   
   Features:
   - Service registration
   - Dependency resolution
   - Lifecycle management
   - Scoped instances
   - Lazy initialization
   - Circular dependency detection
   - Factory support
   - Configuration injection"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
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
  (atom {:services {}         ;; service-id -> service-definition
         :instances {}        ;; service-id -> instance
         :scoped-instances {} ;; scope-id -> {service-id -> instance}
         :factories {}        ;; factory-id -> factory-fn
         :config {}           ;; configuration values
         :resolution-stack [] ;; For circular dependency detection
         :stats {:services-registered 0
                 :instances-created 0
                 :resolutions 0}
         :initialized? false}))

;; ============================================================================
;; Service Scopes
;; ============================================================================

(def scopes
  {:singleton :singleton   ;; Single instance for entire application
   :transient :transient   ;; New instance every time
   :scoped :scoped})       ;; Instance per scope (e.g., per request)

;; ============================================================================
;; Service Registration
;; ============================================================================

(defn register-service!
  "Register a service with the container."
  [service-id config]
  (let [service {:id service-id
                 :name (get config :name (name service-id))
                 :scope (get config :scope :singleton)
                 :factory (get config :factory)
                 :class (get config :class)
                 :dependencies (get config :dependencies [])
                 :init-fn (get config :init-fn)
                 :destroy-fn (get config :destroy-fn)
                 :lazy? (get config :lazy? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:services service-id] service)
    (swap! state update-in [:stats :services-registered] inc)
    (logging/log :debug "Registered service" {:service-id service-id :scope (:scope service)})
    
    ;; Eagerly initialize if not lazy
    (when (and (not (:lazy? service)) (= (:scope service) :singleton))
      (resolve-service service-id))
    
    service-id))

(defn register-factory!
  "Register a factory function."
  [factory-id factory-fn]
  (swap! state assoc-in [:factories factory-id] factory-fn)
  (logging/log :debug "Registered factory" {:factory-id factory-id})
  factory-id)

(defn register-instance!
  "Register an existing instance."
  [service-id instance]
  (register-service! service-id {:scope :singleton :lazy? false})
  (swap! state assoc-in [:instances service-id] instance)
  service-id)

(defn register-config!
  "Register configuration values."
  [config-map]
  (swap! state update :config merge config-map)
  (logging/log :debug "Registered config" {:keys (keys config-map)}))

(defn get-service-definition
  "Get a service definition."
  [service-id]
  (get-in @state [:services service-id]))

(defn list-services
  "List all registered services."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :scope (:scope s)
           :dependencies (:dependencies s)
           :instantiated? (contains? (:instances @state) id)})
        (:services @state)))

;; ============================================================================
;; Dependency Resolution
;; ============================================================================

(declare resolve-service)

(defn- check-circular-dependency!
  "Check for circular dependencies."
  [service-id]
  (let [stack (:resolution-stack @state)]
    (when (some #{service-id} stack)
      (throw (ex-info "Circular dependency detected"
                      {:service-id service-id
                       :resolution-stack stack})))))

(defn- resolve-dependencies
  "Resolve all dependencies for a service."
  [service]
  (mapv resolve-service (:dependencies service)))

(defn- create-instance
  "Create a new instance of a service."
  [service]
  (let [dependencies (resolve-dependencies service)
        instance (cond
                   ;; Use factory function
                   (:factory service)
                   (apply (:factory service) dependencies)
                   
                   ;; Use registered factory
                   (keyword? (:factory service))
                   (let [factory-fn (get-in @state [:factories (:factory service)])]
                     (apply factory-fn dependencies))
                   
                   ;; Use class constructor
                   (:class service)
                   (let [ctor (.getConstructor (:class service)
                                               (into-array Class (repeat (count dependencies) Object)))]
                     (.newInstance ctor (into-array Object dependencies)))
                   
                   :else
                   (throw (ex-info "No factory or class specified" {:service-id (:id service)})))]
    
    ;; Call init function if provided
    (when-let [init-fn (:init-fn service)]
      (init-fn instance))
    
    (swap! state update-in [:stats :instances-created] inc)
    (logging/log :debug "Created instance" {:service-id (:id service)})
    instance))

(defn resolve-service
  "Resolve a service by ID."
  [service-id]
  (swap! state update-in [:stats :resolutions] inc)
  
  (if-let [service (get-service-definition service-id)]
    (do
      (check-circular-dependency! service-id)
      (swap! state update :resolution-stack conj service-id)
      
      (try
        (let [instance
              (case (:scope service)
                :singleton
                (or (get-in @state [:instances service-id])
                    (let [inst (create-instance service)]
                      (swap! state assoc-in [:instances service-id] inst)
                      inst))
                
                :transient
                (create-instance service)
                
                :scoped
                (throw (ex-info "Scoped resolution requires scope context"
                                {:service-id service-id})))]
          instance)
        (finally
          (swap! state update :resolution-stack pop))))
    
    ;; Check if it's a config value
    (if-let [config-value (get-in @state [:config service-id])]
      config-value
      (throw (ex-info "Service not found" {:service-id service-id})))))

(defn resolve-scoped
  "Resolve a service within a scope."
  [service-id scope-id]
  (if-let [service (get-service-definition service-id)]
    (if (= (:scope service) :scoped)
      (or (get-in @state [:scoped-instances scope-id service-id])
          (let [inst (create-instance service)]
            (swap! state assoc-in [:scoped-instances scope-id service-id] inst)
            inst))
      (resolve-service service-id))
    (throw (ex-info "Service not found" {:service-id service-id}))))

;; ============================================================================
;; Scope Management
;; ============================================================================

(defn create-scope
  "Create a new scope."
  []
  (let [scope-id (str (UUID/randomUUID))]
    (swap! state assoc-in [:scoped-instances scope-id] {})
    (logging/log :debug "Created scope" {:scope-id scope-id})
    scope-id))

(defn destroy-scope!
  "Destroy a scope and all its instances."
  [scope-id]
  (when-let [instances (get-in @state [:scoped-instances scope-id])]
    (doseq [[service-id instance] instances]
      (when-let [service (get-service-definition service-id)]
        (when-let [destroy-fn (:destroy-fn service)]
          (try
            (destroy-fn instance)
            (catch Exception e
              (logging/log :warn "Error destroying scoped instance"
                           {:service-id service-id :error (.getMessage e)}))))))
    (swap! state update :scoped-instances dissoc scope-id)
    (logging/log :debug "Destroyed scope" {:scope-id scope-id})))

(defmacro with-scope
  "Execute body within a scope."
  [scope-binding & body]
  `(let [~scope-binding (create-scope)]
     (try
       ~@body
       (finally
         (destroy-scope! ~scope-binding)))))

;; ============================================================================
;; Lifecycle Management
;; ============================================================================

(defn start-service!
  "Start a service (call init-fn if not already started)."
  [service-id]
  (resolve-service service-id))

(defn stop-service!
  "Stop a service (call destroy-fn)."
  [service-id]
  (when-let [instance (get-in @state [:instances service-id])]
    (when-let [service (get-service-definition service-id)]
      (when-let [destroy-fn (:destroy-fn service)]
        (try
          (destroy-fn instance)
          (catch Exception e
            (logging/log :warn "Error stopping service"
                         {:service-id service-id :error (.getMessage e)})))))
    (swap! state update :instances dissoc service-id)
    (logging/log :debug "Stopped service" {:service-id service-id})))

(defn restart-service!
  "Restart a service."
  [service-id]
  (stop-service! service-id)
  (start-service! service-id))

(defn start-all!
  "Start all registered services."
  []
  (doseq [[service-id service] (:services @state)]
    (when (and (= (:scope service) :singleton) (not (:lazy? service)))
      (start-service! service-id))))

(defn stop-all!
  "Stop all running services."
  []
  (doseq [[service-id _] (:instances @state)]
    (stop-service! service-id)))

;; ============================================================================
;; Injection Helpers
;; ============================================================================

(defn inject
  "Inject dependencies into a function."
  [f & service-ids]
  (let [dependencies (mapv resolve-service service-ids)]
    (apply f dependencies)))

(defmacro definjected
  "Define a function with injected dependencies."
  [name deps & body]
  `(defn ~name []
     (let [~@(mapcat (fn [dep]
                       [dep `(resolve-service ~(keyword dep))])
                     deps)]
       ~@body)))

;; ============================================================================
;; Auto-wiring
;; ============================================================================

(defn- get-constructor-params
  "Get constructor parameter types."
  [clazz]
  (when-let [ctor (first (.getConstructors clazz))]
    (.getParameterTypes ctor)))

(defn auto-wire!
  "Auto-wire a class based on constructor parameters."
  [service-id clazz]
  (let [param-types (get-constructor-params clazz)
        dependencies (mapv (fn [param-type]
                             (keyword (.getSimpleName param-type)))
                           param-types)]
    (register-service! service-id
                       {:class clazz
                        :dependencies dependencies})))

;; ============================================================================
;; Service Replacement
;; ============================================================================

(defn replace-service!
  "Replace a service (useful for testing)."
  [service-id new-config]
  (stop-service! service-id)
  (swap! state update :services dissoc service-id)
  (register-service! service-id new-config))

(defn mock-service!
  "Replace a service with a mock."
  [service-id mock-instance]
  (stop-service! service-id)
  (swap! state assoc-in [:instances service-id] mock-instance))

;; ============================================================================
;; Validation
;; ============================================================================

(defn validate-dependencies
  "Validate that all dependencies can be resolved."
  []
  (let [errors (atom [])]
    (doseq [[service-id service] (:services @state)]
      (doseq [dep-id (:dependencies service)]
        (when-not (or (get-in @state [:services dep-id])
                      (get-in @state [:config dep-id]))
          (swap! errors conj {:service-id service-id
                              :missing-dependency dep-id}))))
    (if (empty? @errors)
      {:valid? true}
      {:valid? false :errors @errors})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-injector-stats
  "Get dependency injector statistics."
  []
  (let [stats (:stats @state)]
    {:services-registered (:services-registered stats)
     :instances-created (:instances-created stats)
     :resolutions (:resolutions stats)
     :active-instances (count (:instances @state))
     :active-scopes (count (:scoped-instances @state))
     :factories-count (count (:factories @state))
     :config-keys (count (:config @state))}))

(defn reset-stats!
  "Reset injector statistics."
  []
  (swap! state assoc :stats {:services-registered 0
                             :instances-created 0
                             :resolutions 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-dependency-injector!
  "Initialize the dependency injector."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Dependency injector initialized")
    (events/emit! :dependency-injector-initialized {})
    true))
