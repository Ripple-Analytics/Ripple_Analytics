(ns mental-models.pipeline.integration.request-gateway
  "Request gateway for mental model analysis system.
   
   Features:
   - API gateway functionality
   - Route management
   - Service discovery
   - Load balancing
   - Request routing
   - Gateway policies
   - Gateway authentication
   - Gateway metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:routes {}           ;; route-id -> route config
         :services {}         ;; service-id -> service config
         :policies {}         ;; policy-id -> policy
         :config {:enabled? true
                  :default-timeout-ms 30000
                  :load-balancing :round-robin
                  :health-check-interval-ms 30000}
         :stats {:requests-routed 0
                 :requests-failed 0
                 :services-healthy 0
                 :services-unhealthy 0}
         :initialized? false}))

;; ============================================================================
;; Service Registry
;; ============================================================================

(defn register-service!
  "Register a service with the gateway."
  [service-id config]
  (let [service {:id service-id
                 :name (get config :name (name service-id))
                 :url (get config :url)
                 :handler (get config :handler)
                 :instances (atom (get config :instances []))
                 :health-check-fn (get config :health-check-fn)
                 :healthy? (atom true)
                 :weight (get config :weight 1)
                 :metadata (get config :metadata {})
                 :request-count (AtomicLong. 0)
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:services service-id] service)
    (logging/log :info "Registered service" {:service-id service-id})
    service-id))

(defn get-service
  "Get a service."
  [service-id]
  (get-in @state [:services service-id]))

(defn list-services
  "List all services."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :url (:url s)
           :healthy? @(:healthy? s)
           :enabled? @(:enabled? s)
           :request-count (.get (:request-count s))
           :instances-count (count @(:instances s))})
        (:services @state)))

(defn enable-service!
  "Enable a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (reset! (:enabled? service) true)))

(defn disable-service!
  "Disable a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (reset! (:enabled? service) false)))

(defn deregister-service!
  "Deregister a service."
  [service-id]
  (swap! state update :services dissoc service-id))

;; ============================================================================
;; Service Instances
;; ============================================================================

(defn add-instance!
  "Add an instance to a service."
  [service-id instance]
  (when-let [service (get-service service-id)]
    (swap! (:instances service) conj
           (merge {:id (str (UUID/randomUUID))
                   :healthy? true
                   :weight 1
                   :added-at (System/currentTimeMillis)}
                  instance))))

(defn remove-instance!
  "Remove an instance from a service."
  [service-id instance-id]
  (when-let [service (get-service service-id)]
    (swap! (:instances service)
           (fn [instances]
             (vec (remove #(= (:id %) instance-id) instances))))))

(defn get-healthy-instances
  "Get healthy instances for a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (filter :healthy? @(:instances service))))

;; ============================================================================
;; Load Balancing
;; ============================================================================

(defonce ^:private round-robin-counters (atom {}))

(defn- select-instance-round-robin
  "Select an instance using round-robin."
  [service-id instances]
  (let [counter (get (swap! round-robin-counters update service-id
                            (fnil inc -1))
                     service-id)
        idx (mod counter (count instances))]
    (nth instances idx)))

(defn- select-instance-random
  "Select an instance randomly."
  [instances]
  (rand-nth instances))

(defn- select-instance-weighted
  "Select an instance based on weights."
  [instances]
  (let [total-weight (reduce + (map :weight instances))
        r (rand total-weight)]
    (loop [remaining instances
           cumulative 0]
      (if (empty? remaining)
        (last instances)
        (let [instance (first remaining)
              new-cumulative (+ cumulative (:weight instance))]
          (if (< r new-cumulative)
            instance
            (recur (rest remaining) new-cumulative)))))))

(defn- select-instance-least-connections
  "Select instance with least connections."
  [instances]
  (first (sort-by :active-connections instances)))

(defn select-instance
  "Select an instance for a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (let [instances (get-healthy-instances service-id)
          strategy (get-in @state [:config :load-balancing])]
      (when (seq instances)
        (case strategy
          :round-robin (select-instance-round-robin service-id instances)
          :random (select-instance-random instances)
          :weighted (select-instance-weighted instances)
          :least-connections (select-instance-least-connections instances)
          (select-instance-round-robin service-id instances))))))

;; ============================================================================
;; Route Management
;; ============================================================================

(defn register-route!
  "Register a route."
  [route-id config]
  (let [route {:id route-id
               :path (get config :path)
               :method (get config :method :any)
               :service-id (get config :service-id)
               :handler (get config :handler)
               :transform-fn (get config :transform-fn identity)
               :policies (get config :policies [])
               :priority (get config :priority 100)
               :enabled? (atom true)
               :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:routes route-id] route)
    (logging/log :info "Registered route" {:route-id route-id :path (:path route)})
    route-id))

(defn get-route
  "Get a route."
  [route-id]
  (get-in @state [:routes route-id]))

(defn list-routes
  "List all routes."
  []
  (mapv (fn [[id r]]
          {:id id
           :path (:path r)
           :method (:method r)
           :service-id (:service-id r)
           :enabled? @(:enabled? r)
           :priority (:priority r)})
        (:routes @state)))

(defn delete-route!
  "Delete a route."
  [route-id]
  (swap! state update :routes dissoc route-id))

(defn- match-route
  "Match a request to a route."
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (->> (vals (:routes @state))
         (filter #@(:enabled? %))
         (filter (fn [route]
                   (and (or (= (:method route) :any)
                            (= (:method route) method))
                        (or (= (:path route) uri)
                            (and (string? (:path route))
                                 (str/starts-with? uri (:path route)))
                            (and (instance? java.util.regex.Pattern (:path route))
                                 (re-matches (:path route) uri))))))
         (sort-by :priority)
         first)))

;; ============================================================================
;; Gateway Policies
;; ============================================================================

(defn register-policy!
  "Register a gateway policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :type (get config :type :filter)
                :apply-fn (get config :apply-fn)
                :condition-fn (get config :condition-fn (constantly true))
                :priority (get config :priority 100)
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:policies policy-id] policy)
    policy-id))

(defn get-policy
  "Get a policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn apply-policies
  "Apply policies to a request."
  [request policies]
  (reduce (fn [req policy-id]
            (if-let [policy (get-policy policy-id)]
              (if (and @(:enabled? policy)
                       ((:condition-fn policy) req))
                ((:apply-fn policy) req)
                req)
              req))
          request
          policies))

;; ============================================================================
;; Request Routing
;; ============================================================================

(defn route-request
  "Route a request through the gateway."
  [request]
  (swap! state update-in [:stats :requests-routed] inc)
  
  (if-let [route (match-route request)]
    (let [;; Apply route policies
          processed-request (apply-policies request (:policies route))
          ;; Transform request
          transformed-request ((:transform-fn route) processed-request)]
      
      (cond
        ;; Direct handler
        (:handler route)
        ((:handler route) transformed-request)
        
        ;; Service routing
        (:service-id route)
        (if-let [service (get-service (:service-id route))]
          (if @(:enabled? service)
            (do
              (.incrementAndGet (:request-count service))
              (if-let [handler (:handler service)]
                (handler transformed-request)
                ;; Would make HTTP call to service URL here
                {:status 502
                 :body {:error "Service handler not configured"}}))
            {:status 503
             :body {:error "Service disabled"}})
          {:status 502
           :body {:error "Service not found"}})
        
        :else
        {:status 500
         :body {:error "Route misconfigured"}}))
    
    (do
      (swap! state update-in [:stats :requests-failed] inc)
      {:status 404
       :body {:error "No route found"}})))

;; ============================================================================
;; Health Checks
;; ============================================================================

(defn check-service-health
  "Check health of a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (if-let [health-fn (:health-check-fn service)]
      (try
        (let [healthy? (health-fn)]
          (reset! (:healthy? service) healthy?)
          (if healthy?
            (swap! state update-in [:stats :services-healthy] inc)
            (swap! state update-in [:stats :services-unhealthy] inc))
          healthy?)
        (catch Exception _
          (reset! (:healthy? service) false)
          (swap! state update-in [:stats :services-unhealthy] inc)
          false))
      true)))

(defn check-all-services-health
  "Check health of all services."
  []
  (doseq [[service-id _] (:services @state)]
    (check-service-health service-id)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-gateway
  "Ring middleware for gateway routing."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [response (route-request request)]
        (if (= (:status response) 404)
          (handler request)  ;; Fall through to default handler
          response))
      (handler request))))

(defn wrap-gateway-only
  "Ring middleware that only uses gateway routing."
  []
  (fn [request]
    (route-request request)))

;; ============================================================================
;; Service Discovery
;; ============================================================================

(defn discover-services
  "Discover services (placeholder for service discovery integration)."
  []
  ;; This would integrate with Consul, etcd, Kubernetes, etc.
  (list-services))

(defn watch-services
  "Watch for service changes."
  [callback]
  ;; This would set up a watch on service registry
  (let [watch-chan (chan)]
    (go-loop []
      (when-let [event (<! watch-chan)]
        (callback event)
        (recur)))
    watch-chan))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable the gateway."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-load-balancing!
  "Set load balancing strategy."
  [strategy]
  (swap! state assoc-in [:config :load-balancing] strategy))

(defn set-default-timeout!
  "Set default timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :default-timeout-ms] timeout-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-gateway-metrics
  "Get gateway metrics."
  []
  (let [stats (:stats @state)]
    {:requests-routed (:requests-routed stats)
     :requests-failed (:requests-failed stats)
     :services-healthy (:services-healthy stats)
     :services-unhealthy (:services-unhealthy stats)
     :routes-count (count (:routes @state))
     :services-count (count (:services @state))
     :policies-count (count (:policies @state))
     :success-rate (if (pos? (:requests-routed stats))
                     (/ (- (:requests-routed stats) (:requests-failed stats))
                        (:requests-routed stats))
                     1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-gateway-stats
  "Get gateway statistics."
  []
  (merge (get-gateway-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :load-balancing (get-in @state [:config :load-balancing])
          :default-timeout-ms (get-in @state [:config :default-timeout-ms])}))

(defn reset-stats!
  "Reset gateway statistics."
  []
  (swap! state assoc :stats {:requests-routed 0
                             :requests-failed 0
                             :services-healthy 0
                             :services-unhealthy 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-gateway!
  "Initialize the request gateway."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request gateway initialized")
    (events/emit! :request-gateway-initialized {})
    true))
