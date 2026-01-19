(ns mental-models.pipeline.integration.request-balancer
  "Request balancer for mental model analysis system.
   
   Features:
   - Load balancing
   - Multiple algorithms
   - Health-aware routing
   - Weighted distribution
   - Sticky sessions
   - Failover handling
   - Balancer metrics
   - Backend management"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.util.concurrent.atomic AtomicLong AtomicInteger AtomicBoolean]
           [java.util.concurrent ConcurrentHashMap]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:backends {}         ;; backend-id -> backend config
         :pools {}            ;; pool-id -> pool config
         :sessions (ConcurrentHashMap.)  ;; session-id -> backend-id
         :config {:default-algorithm :round-robin
                  :health-check-interval-ms 30000
                  :session-timeout-ms 3600000
                  :max-retries 3}
         :stats {:requests-balanced (AtomicLong. 0)
                 :backend-failures (AtomicLong. 0)
                 :failovers (AtomicLong. 0)
                 :session-hits (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Backend Management
;; ============================================================================

(defn register-backend!
  "Register a backend server."
  [backend-id config]
  (let [backend {:id backend-id
                 :name (get config :name (name backend-id))
                 :url (get config :url)
                 :weight (get config :weight 1)
                 :max-connections (get config :max-connections 100)
                 :active-connections (AtomicInteger. 0)
                 :total-requests (AtomicLong. 0)
                 :failed-requests (AtomicLong. 0)
                 :response-time-sum (AtomicLong. 0)
                 :healthy? (AtomicBoolean. true)
                 :enabled? (AtomicBoolean. true)
                 :last-health-check (atom nil)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:backends backend-id] backend)
    (logging/log :info "Registered backend" {:backend-id backend-id :url (:url backend)})
    backend-id))

(defn get-backend
  "Get a backend."
  [backend-id]
  (get-in @state [:backends backend-id]))

(defn list-backends
  "List all backends."
  []
  (mapv (fn [[id b]]
          {:id id
           :name (:name b)
           :url (:url b)
           :weight (:weight b)
           :healthy? (.get (:healthy? b))
           :enabled? (.get (:enabled? b))
           :active-connections (.get (:active-connections b))
           :total-requests (.get (:total-requests b))
           :avg-response-time (let [total (.get (:total-requests b))]
                                (if (pos? total)
                                  (/ (.get (:response-time-sum b)) total)
                                  0))})
        (:backends @state)))

(defn enable-backend!
  "Enable a backend."
  [backend-id]
  (when-let [backend (get-backend backend-id)]
    (.set (:enabled? backend) true)))

(defn disable-backend!
  "Disable a backend."
  [backend-id]
  (when-let [backend (get-backend backend-id)]
    (.set (:enabled? backend) false)))

(defn mark-healthy!
  "Mark a backend as healthy."
  [backend-id]
  (when-let [backend (get-backend backend-id)]
    (.set (:healthy? backend) true)
    (reset! (:last-health-check backend) (System/currentTimeMillis))))

(defn mark-unhealthy!
  "Mark a backend as unhealthy."
  [backend-id]
  (when-let [backend (get-backend backend-id)]
    (.set (:healthy? backend) false)
    (reset! (:last-health-check backend) (System/currentTimeMillis))
    (logging/log :warn "Backend marked unhealthy" {:backend-id backend-id})))

(defn delete-backend!
  "Delete a backend."
  [backend-id]
  (swap! state update :backends dissoc backend-id))

;; ============================================================================
;; Backend Selection Algorithms
;; ============================================================================

(defn- get-healthy-backends
  "Get list of healthy and enabled backends."
  []
  (filter (fn [[_ b]]
            (and (.get (:healthy? b))
                 (.get (:enabled? b))))
          (:backends @state)))

(defonce ^:private round-robin-counter (AtomicInteger. 0))

(defn select-round-robin
  "Select backend using round-robin."
  []
  (let [backends (vec (get-healthy-backends))]
    (when (seq backends)
      (let [idx (mod (.getAndIncrement round-robin-counter) (count backends))]
        (first (nth backends idx))))))

(defn select-random
  "Select backend randomly."
  []
  (let [backends (vec (get-healthy-backends))]
    (when (seq backends)
      (first (rand-nth backends)))))

(defn select-least-connections
  "Select backend with least active connections."
  []
  (let [backends (get-healthy-backends)]
    (when (seq backends)
      (first (apply min-key
                    (fn [[_ b]] (.get (:active-connections b)))
                    backends)))))

(defn select-weighted
  "Select backend using weighted distribution."
  []
  (let [backends (vec (get-healthy-backends))
        total-weight (reduce + (map (fn [[_ b]] (:weight b)) backends))]
    (when (and (seq backends) (pos? total-weight))
      (let [r (rand total-weight)]
        (loop [remaining backends
               cumulative 0]
          (when (seq remaining)
            (let [[id b] (first remaining)
                  new-cumulative (+ cumulative (:weight b))]
              (if (< r new-cumulative)
                id
                (recur (rest remaining) new-cumulative)))))))))

(defn select-least-response-time
  "Select backend with lowest average response time."
  []
  (let [backends (get-healthy-backends)]
    (when (seq backends)
      (first (apply min-key
                    (fn [[_ b]]
                      (let [total (.get (:total-requests b))]
                        (if (pos? total)
                          (/ (.get (:response-time-sum b)) total)
                          0)))
                    backends)))))

(defn select-ip-hash
  "Select backend based on IP hash."
  [ip]
  (let [backends (vec (get-healthy-backends))]
    (when (seq backends)
      (let [idx (mod (hash ip) (count backends))]
        (first (nth backends idx))))))

;; ============================================================================
;; Backend Selection
;; ============================================================================

(defn select-backend
  "Select a backend using the specified algorithm."
  [& {:keys [algorithm ip session-id]
      :or {algorithm (get-in @state [:config :default-algorithm])}}]
  ;; Check for sticky session first
  (if-let [sticky-backend (when session-id
                            (.get (:sessions @state) session-id))]
    (if (and (get-backend sticky-backend)
             (.get (:healthy? (get-backend sticky-backend))))
      (do
        (.incrementAndGet (:session-hits (:stats @state)))
        sticky-backend)
      (do
        (.remove (:sessions @state) session-id)
        (select-backend :algorithm algorithm :ip ip)))
    
    ;; Select using algorithm
    (let [backend-id (case algorithm
                       :round-robin (select-round-robin)
                       :random (select-random)
                       :least-connections (select-least-connections)
                       :weighted (select-weighted)
                       :least-response-time (select-least-response-time)
                       :ip-hash (select-ip-hash ip)
                       (select-round-robin))]
      
      ;; Store session if provided
      (when (and session-id backend-id)
        (.put (:sessions @state) session-id backend-id))
      
      backend-id)))

;; ============================================================================
;; Request Execution
;; ============================================================================

(defn record-request-start!
  "Record the start of a request to a backend."
  [backend-id]
  (when-let [backend (get-backend backend-id)]
    (.incrementAndGet (:active-connections backend))))

(defn record-request-end!
  "Record the end of a request to a backend."
  [backend-id response-time-ms success?]
  (when-let [backend (get-backend backend-id)]
    (.decrementAndGet (:active-connections backend))
    (.incrementAndGet (:total-requests backend))
    (.addAndGet (:response-time-sum backend) response-time-ms)
    (when-not success?
      (.incrementAndGet (:failed-requests backend))
      (.incrementAndGet (:backend-failures (:stats @state))))))

(defn execute-with-backend
  "Execute a function with a selected backend."
  [f & {:keys [algorithm ip session-id max-retries]
        :or {max-retries (get-in @state [:config :max-retries])}}]
  (.incrementAndGet (:requests-balanced (:stats @state)))
  
  (loop [retries 0
         tried-backends #{}]
    (let [backend-id (select-backend :algorithm algorithm :ip ip :session-id session-id)]
      (if (or (nil? backend-id) (contains? tried-backends backend-id))
        {:error :no-healthy-backends}
        (let [backend (get-backend backend-id)
              start-time (System/currentTimeMillis)]
          (record-request-start! backend-id)
          (try
            (let [result (f (:url backend))]
              (record-request-end! backend-id (- (System/currentTimeMillis) start-time) true)
              result)
            (catch Exception e
              (record-request-end! backend-id (- (System/currentTimeMillis) start-time) false)
              (if (< retries max-retries)
                (do
                  (.incrementAndGet (:failovers (:stats @state)))
                  (recur (inc retries) (conj tried-backends backend-id)))
                {:error :all-backends-failed
                 :exception e}))))))))

;; ============================================================================
;; Pool Management
;; ============================================================================

(defn create-pool!
  "Create a backend pool."
  [pool-id config]
  (let [pool {:id pool-id
              :name (get config :name (name pool-id))
              :backends (atom (get config :backends []))
              :algorithm (get config :algorithm :round-robin)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:pools pool-id] pool)
    pool-id))

(defn add-to-pool!
  "Add a backend to a pool."
  [pool-id backend-id]
  (when-let [pool (get-in @state [:pools pool-id])]
    (swap! (:backends pool) conj backend-id)))

(defn remove-from-pool!
  "Remove a backend from a pool."
  [pool-id backend-id]
  (when-let [pool (get-in @state [:pools pool-id])]
    (swap! (:backends pool) #(vec (remove #{backend-id} %)))))

(defn get-pool-backends
  "Get backends in a pool."
  [pool-id]
  (when-let [pool (get-in @state [:pools pool-id])]
    @(:backends pool)))

;; ============================================================================
;; Health Checking
;; ============================================================================

(defn check-backend-health!
  "Check health of a backend."
  [backend-id health-check-fn]
  (when-let [backend (get-backend backend-id)]
    (try
      (let [healthy? (health-check-fn (:url backend))]
        (if healthy?
          (mark-healthy! backend-id)
          (mark-unhealthy! backend-id))
        healthy?)
      (catch Exception _
        (mark-unhealthy! backend-id)
        false))))

(defn start-health-checks!
  "Start periodic health checks for all backends."
  [health-check-fn]
  (go-loop []
    (<! (timeout (get-in @state [:config :health-check-interval-ms])))
    (doseq [[backend-id _] (:backends @state)]
      (check-backend-health! backend-id health-check-fn))
    (recur)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-load-balance
  "Ring middleware for load balancing."
  [handler & {:keys [algorithm]}]
  (fn [request]
    (let [ip (:remote-addr request)
          session-id (get-in request [:session :id])
          backend-id (select-backend :algorithm algorithm :ip ip :session-id session-id)]
      (if backend-id
        (let [backend (get-backend backend-id)]
          (handler (assoc request :backend-url (:url backend) :backend-id backend-id)))
        {:status 503
         :headers {"Content-Type" "application/json"}
         :body {:error "No healthy backends available"}}))))

(defn wrap-backend-header
  "Ring middleware to add backend header to response."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if-let [backend-id (:backend-id request)]
        (assoc-in response [:headers "X-Backend-Id"] (name backend-id))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-algorithm!
  "Set default load balancing algorithm."
  [algorithm]
  (swap! state assoc-in [:config :default-algorithm] algorithm))

(defn set-health-check-interval!
  "Set health check interval."
  [interval-ms]
  (swap! state assoc-in [:config :health-check-interval-ms] interval-ms))

(defn set-max-retries!
  "Set maximum retries."
  [max-retries]
  (swap! state assoc-in [:config :max-retries] max-retries))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-balancer-metrics
  "Get balancer metrics."
  []
  (let [stats (:stats @state)
        backends (list-backends)]
    {:requests-balanced (.get (:requests-balanced stats))
     :backend-failures (.get (:backend-failures stats))
     :failovers (.get (:failovers stats))
     :session-hits (.get (:session-hits stats))
     :backends-count (count backends)
     :healthy-backends (count (filter :healthy? backends))
     :active-sessions (.size (:sessions @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-balancer-stats
  "Get balancer statistics."
  []
  (merge (get-balancer-metrics)
         {:default-algorithm (get-in @state [:config :default-algorithm])
          :health-check-interval-ms (get-in @state [:config :health-check-interval-ms])
          :max-retries (get-in @state [:config :max-retries])}))

(defn reset-stats!
  "Reset balancer statistics."
  []
  (.set (:requests-balanced (:stats @state)) 0)
  (.set (:backend-failures (:stats @state)) 0)
  (.set (:failovers (:stats @state)) 0)
  (.set (:session-hits (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-balancer!
  "Initialize the request balancer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request balancer initialized")
    (events/emit! :request-balancer-initialized {})
    true))
