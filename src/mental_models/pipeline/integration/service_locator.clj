(ns mental-models.pipeline.integration.service-locator
  "Service locator for mental model analysis system.
   
   Features:
   - Service registration
   - Service lookup
   - Service versioning
   - Service aliases
   - Health-aware routing
   - Load balancing
   - Service groups
   - Dynamic discovery"
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
  (atom {:services {}         ;; service-id -> service-info
         :aliases {}          ;; alias -> service-id
         :groups {}           ;; group-id -> [service-ids]
         :versions {}         ;; service-id -> {version -> service-info}
         :health-status {}    ;; service-id -> health-status
         :load-balancers {}   ;; group-id -> load-balancer-state
         :config {:health-check-interval-ms 30000
                  :unhealthy-threshold 3
                  :healthy-threshold 2}
         :stats {:lookups 0
                 :registrations 0
                 :health-checks 0}
         :initialized? false}))

;; ============================================================================
;; Service Registration
;; ============================================================================

(defn register!
  "Register a service."
  [service-id config]
  (let [service {:id service-id
                 :name (get config :name (name service-id))
                 :version (get config :version "1.0.0")
                 :endpoint (get config :endpoint)
                 :protocol (get config :protocol :http)
                 :metadata (get config :metadata {})
                 :tags (get config :tags #{})
                 :weight (get config :weight 1)
                 :health-check-fn (get config :health-check-fn)
                 :enabled? (atom true)
                 :registered-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:services service-id] service)
    (swap! state assoc-in [:health-status service-id]
           {:status :unknown :last-check nil :consecutive-failures 0})
    (swap! state update-in [:stats :registrations] inc)
    
    ;; Register version
    (swap! state assoc-in [:versions service-id (:version service)] service)
    
    (logging/log :info "Registered service" {:service-id service-id :version (:version service)})
    (events/emit! :service-registered {:service-id service-id})
    service-id))

(defn unregister!
  "Unregister a service."
  [service-id]
  (swap! state update :services dissoc service-id)
  (swap! state update :health-status dissoc service-id)
  (logging/log :info "Unregistered service" {:service-id service-id})
  (events/emit! :service-unregistered {:service-id service-id}))

(defn get-service
  "Get a service by ID."
  [service-id]
  (get-in @state [:services service-id]))

(defn list-services
  "List all registered services."
  [& {:keys [tag enabled-only?]}]
  (let [services (vals (:services @state))
        filtered (cond->> services
                   tag (filter #(contains? (:tags %) tag))
                   enabled-only? (filter #(deref (:enabled? %))))]
    (mapv #(select-keys % [:id :name :version :endpoint :tags]) filtered)))

;; ============================================================================
;; Service Aliases
;; ============================================================================

(defn register-alias!
  "Register an alias for a service."
  [alias service-id]
  (swap! state assoc-in [:aliases alias] service-id)
  (logging/log :debug "Registered alias" {:alias alias :service-id service-id})
  alias)

(defn resolve-alias
  "Resolve an alias to a service ID."
  [alias]
  (get-in @state [:aliases alias] alias))

(defn unregister-alias!
  "Unregister an alias."
  [alias]
  (swap! state update :aliases dissoc alias))

;; ============================================================================
;; Service Groups
;; ============================================================================

(defn create-group!
  "Create a service group."
  [group-id config]
  (let [group {:id group-id
               :name (get config :name (name group-id))
               :services (atom (get config :services []))
               :load-balancer (get config :load-balancer :round-robin)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:groups group-id] group)
    (swap! state assoc-in [:load-balancers group-id] {:index (atom 0)})
    (logging/log :info "Created service group" {:group-id group-id})
    group-id))

(defn add-to-group!
  "Add a service to a group."
  [group-id service-id]
  (when-let [group (get-in @state [:groups group-id])]
    (swap! (:services group) conj service-id)
    (logging/log :debug "Added service to group" {:group-id group-id :service-id service-id})))

(defn remove-from-group!
  "Remove a service from a group."
  [group-id service-id]
  (when-let [group (get-in @state [:groups group-id])]
    (swap! (:services group) #(remove #{service-id} %))
    (logging/log :debug "Removed service from group" {:group-id group-id :service-id service-id})))

(defn get-group-services
  "Get all services in a group."
  [group-id]
  (when-let [group (get-in @state [:groups group-id])]
    @(:services group)))

;; ============================================================================
;; Service Versioning
;; ============================================================================

(defn get-service-version
  "Get a specific version of a service."
  [service-id version]
  (get-in @state [:versions service-id version]))

(defn list-versions
  "List all versions of a service."
  [service-id]
  (keys (get-in @state [:versions service-id])))

(defn get-latest-version
  "Get the latest version of a service."
  [service-id]
  (when-let [versions (get-in @state [:versions service-id])]
    (let [sorted-versions (sort (comparator (fn [a b]
                                              (neg? (compare a b))))
                                (keys versions))]
      (get versions (first sorted-versions)))))

;; ============================================================================
;; Health Checking
;; ============================================================================

(defn- check-service-health
  "Check health of a single service."
  [service]
  (swap! state update-in [:stats :health-checks] inc)
  (if-let [health-check-fn (:health-check-fn service)]
    (try
      (let [healthy? (health-check-fn)]
        {:status (if healthy? :healthy :unhealthy)
         :last-check (System/currentTimeMillis)})
      (catch Exception e
        {:status :unhealthy
         :last-check (System/currentTimeMillis)
         :error (.getMessage e)}))
    {:status :unknown
     :last-check (System/currentTimeMillis)}))

(defn update-health-status!
  "Update health status for a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (let [result (check-service-health service)
          current-status (get-in @state [:health-status service-id])
          new-failures (if (= :unhealthy (:status result))
                         (inc (:consecutive-failures current-status 0))
                         0)
          new-successes (if (= :healthy (:status result))
                          (inc (:consecutive-successes current-status 0))
                          0)]
      (swap! state assoc-in [:health-status service-id]
             (assoc result
                    :consecutive-failures new-failures
                    :consecutive-successes new-successes))
      
      ;; Emit events on status changes
      (when (and (= :unhealthy (:status result))
                 (>= new-failures (get-in @state [:config :unhealthy-threshold])))
        (events/emit! :service-unhealthy {:service-id service-id}))
      
      result)))

(defn get-health-status
  "Get health status for a service."
  [service-id]
  (get-in @state [:health-status service-id]))

(defn is-healthy?
  "Check if a service is healthy."
  [service-id]
  (= :healthy (:status (get-health-status service-id))))

(defn- start-health-checker!
  "Start the background health checker."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :health-check-interval-ms])))
    (doseq [[service-id _] (:services @state)]
      (update-health-status! service-id))
    (recur)))

;; ============================================================================
;; Load Balancing
;; ============================================================================

(defn- round-robin-select
  "Select a service using round-robin."
  [services lb-state]
  (let [idx @(:index lb-state)
        selected (nth services (mod idx (count services)))]
    (swap! (:index lb-state) inc)
    selected))

(defn- weighted-select
  "Select a service using weighted random."
  [services]
  (let [total-weight (reduce + (map #(:weight (get-service %) 1) services))
        r (rand total-weight)]
    (loop [remaining services
           cumulative 0]
      (if-let [service-id (first remaining)]
        (let [weight (:weight (get-service service-id) 1)
              new-cumulative (+ cumulative weight)]
          (if (< r new-cumulative)
            service-id
            (recur (rest remaining) new-cumulative)))
        (first services)))))

(defn- least-connections-select
  "Select a service with least connections (placeholder)."
  [services]
  ;; In a real implementation, this would track active connections
  (first services))

(defn select-from-group
  "Select a service from a group using load balancing."
  [group-id & {:keys [healthy-only?] :or {healthy-only? true}}]
  (when-let [group (get-in @state [:groups group-id])]
    (let [services @(:services group)
          available (if healthy-only?
                      (filter is-healthy? services)
                      services)
          lb-state (get-in @state [:load-balancers group-id])]
      (when (seq available)
        (case (:load-balancer group)
          :round-robin (round-robin-select available lb-state)
          :weighted (weighted-select available)
          :least-connections (least-connections-select available)
          :random (rand-nth available)
          (first available))))))

;; ============================================================================
;; Service Lookup
;; ============================================================================

(defn lookup
  "Look up a service by ID, alias, or from a group."
  [identifier & {:keys [version healthy-only?] :or {healthy-only? true}}]
  (swap! state update-in [:stats :lookups] inc)
  
  (let [resolved-id (resolve-alias identifier)]
    (cond
      ;; Check if it's a group
      (get-in @state [:groups resolved-id])
      (when-let [service-id (select-from-group resolved-id :healthy-only? healthy-only?)]
        (get-service service-id))
      
      ;; Check if it's a direct service
      (get-in @state [:services resolved-id])
      (let [service (if version
                      (get-service-version resolved-id version)
                      (get-service resolved-id))]
        (when (or (not healthy-only?) (is-healthy? resolved-id))
          service))
      
      :else nil)))

(defn lookup-all
  "Look up all services matching criteria."
  [& {:keys [tag version healthy-only?] :or {healthy-only? true}}]
  (let [services (vals (:services @state))
        filtered (cond->> services
                   tag (filter #(contains? (:tags %) tag))
                   version (filter #(= (:version %) version))
                   healthy-only? (filter #(is-healthy? (:id %))))]
    (vec filtered)))

;; ============================================================================
;; Service Enable/Disable
;; ============================================================================

(defn enable-service!
  "Enable a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (reset! (:enabled? service) true)
    (logging/log :info "Enabled service" {:service-id service-id})))

(defn disable-service!
  "Disable a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (reset! (:enabled? service) false)
    (logging/log :info "Disabled service" {:service-id service-id})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-locator-stats
  "Get service locator statistics."
  []
  (let [stats (:stats @state)
        services (vals (:services @state))
        healthy-count (count (filter #(is-healthy? (:id %)) services))]
    {:lookups (:lookups stats)
     :registrations (:registrations stats)
     :health-checks (:health-checks stats)
     :services-count (count services)
     :healthy-count healthy-count
     :unhealthy-count (- (count services) healthy-count)
     :groups-count (count (:groups @state))
     :aliases-count (count (:aliases @state))}))

(defn reset-stats!
  "Reset locator statistics."
  []
  (swap! state assoc :stats {:lookups 0
                             :registrations 0
                             :health-checks 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-service-locator!
  "Initialize the service locator."
  []
  (when-not (:initialized? @state)
    ;; Start health checker
    (start-health-checker!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Service locator initialized")
    (events/emit! :service-locator-initialized {})
    true))
