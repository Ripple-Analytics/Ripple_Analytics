(ns mental-models.pipeline.integration.request-circuit
  "Request circuit breaker for mental model analysis system.
   
   Features:
   - Circuit breaker pattern
   - Failure detection
   - State management
   - Half-open testing
   - Fallback handlers
   - Circuit metrics
   - Health monitoring
   - Recovery strategies"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.concurrent.atomic AtomicLong AtomicInteger AtomicReference]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:circuits {}         ;; circuit-id -> circuit config
         :config {:default-failure-threshold 5
                  :default-success-threshold 3
                  :default-timeout-ms 30000
                  :default-half-open-requests 3
                  :default-reset-timeout-ms 60000}
         :stats {:total-requests (AtomicLong. 0)
                 :successful-requests (AtomicLong. 0)
                 :failed-requests (AtomicLong. 0)
                 :rejected-requests (AtomicLong. 0)
                 :circuit-opens (AtomicLong. 0)
                 :circuit-closes (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Circuit States
;; ============================================================================

(def circuit-states #{:closed :open :half-open})

;; ============================================================================
;; Circuit Creation
;; ============================================================================

(defn create-circuit!
  "Create a new circuit breaker."
  [circuit-id config]
  (let [circuit {:id circuit-id
                 :name (get config :name (name circuit-id))
                 :state (AtomicReference. :closed)
                 :failure-count (AtomicInteger. 0)
                 :success-count (AtomicInteger. 0)
                 :half-open-requests (AtomicInteger. 0)
                 :last-failure-time (AtomicLong. 0)
                 :last-state-change (AtomicLong. (System/currentTimeMillis))
                 :failure-threshold (get config :failure-threshold
                                         (get-in @state [:config :default-failure-threshold]))
                 :success-threshold (get config :success-threshold
                                         (get-in @state [:config :default-success-threshold]))
                 :timeout-ms (get config :timeout-ms
                                  (get-in @state [:config :default-timeout-ms]))
                 :reset-timeout-ms (get config :reset-timeout-ms
                                        (get-in @state [:config :default-reset-timeout-ms]))
                 :max-half-open-requests (get config :max-half-open-requests
                                              (get-in @state [:config :default-half-open-requests]))
                 :fallback-fn (get config :fallback-fn)
                 :on-state-change (get config :on-state-change)
                 :failure-predicate (get config :failure-predicate
                                         (fn [response]
                                           (or (instance? Exception response)
                                               (and (map? response)
                                                    (>= (get response :status 200) 500)))))
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:circuits circuit-id] circuit)
    (logging/log :info "Created circuit breaker" {:circuit-id circuit-id})
    circuit-id))

(defn get-circuit
  "Get a circuit breaker."
  [circuit-id]
  (get-in @state [:circuits circuit-id]))

(defn list-circuits
  "List all circuit breakers."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :state (.get (:state c))
           :failure-count (.get (:failure-count c))
           :success-count (.get (:success-count c))})
        (:circuits @state)))

(defn delete-circuit!
  "Delete a circuit breaker."
  [circuit-id]
  (swap! state update :circuits dissoc circuit-id))

;; ============================================================================
;; State Transitions
;; ============================================================================

(defn- transition-to!
  "Transition circuit to a new state."
  [circuit new-state]
  (let [old-state (.getAndSet (:state circuit) new-state)]
    (when (not= old-state new-state)
      (.set (:last-state-change circuit) (System/currentTimeMillis))
      
      (case new-state
        :open (do
                (.incrementAndGet (:circuit-opens (:stats @state)))
                (logging/log :warn "Circuit opened" {:circuit-id (:id circuit)}))
        :closed (do
                  (.incrementAndGet (:circuit-closes (:stats @state)))
                  (logging/log :info "Circuit closed" {:circuit-id (:id circuit)}))
        :half-open (logging/log :info "Circuit half-open" {:circuit-id (:id circuit)}))
      
      (when-let [callback (:on-state-change circuit)]
        (callback {:circuit-id (:id circuit)
                   :old-state old-state
                   :new-state new-state})))))

(defn- should-attempt-reset?
  "Check if circuit should attempt reset."
  [circuit]
  (let [current-state (.get (:state circuit))
        last-failure (.get (:last-failure-time circuit))
        reset-timeout (:reset-timeout-ms circuit)]
    (and (= current-state :open)
         (> (- (System/currentTimeMillis) last-failure) reset-timeout))))

(defn- check-state-transition!
  "Check and perform state transitions."
  [circuit]
  (let [current-state (.get (:state circuit))]
    (case current-state
      :closed
      (when (>= (.get (:failure-count circuit)) (:failure-threshold circuit))
        (transition-to! circuit :open))
      
      :open
      (when (should-attempt-reset? circuit)
        (.set (:half-open-requests circuit) 0)
        (.set (:success-count circuit) 0)
        (transition-to! circuit :half-open))
      
      :half-open
      (cond
        (>= (.get (:success-count circuit)) (:success-threshold circuit))
        (do
          (.set (:failure-count circuit) 0)
          (transition-to! circuit :closed))
        
        (> (.get (:failure-count circuit)) 0)
        (transition-to! circuit :open))
      
      nil)))

;; ============================================================================
;; Circuit Operations
;; ============================================================================

(defn record-success!
  "Record a successful request."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (.incrementAndGet (:successful-requests (:stats @state)))
    (.incrementAndGet (:success-count circuit))
    (check-state-transition! circuit)))

(defn record-failure!
  "Record a failed request."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (.incrementAndGet (:failed-requests (:stats @state)))
    (.incrementAndGet (:failure-count circuit))
    (.set (:last-failure-time circuit) (System/currentTimeMillis))
    (check-state-transition! circuit)))

(defn allow-request?
  "Check if a request should be allowed."
  [circuit-id]
  (if-let [circuit (get-circuit circuit-id)]
    (let [current-state (.get (:state circuit))]
      (check-state-transition! circuit)
      (case current-state
        :closed true
        :open false
        :half-open (< (.getAndIncrement (:half-open-requests circuit))
                      (:max-half-open-requests circuit))))
    true))

(defn get-state
  "Get circuit state."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (.get (:state circuit))))

(defn force-open!
  "Force circuit to open state."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (transition-to! circuit :open)))

(defn force-close!
  "Force circuit to closed state."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (.set (:failure-count circuit) 0)
    (.set (:success-count circuit) 0)
    (transition-to! circuit :closed)))

(defn reset!
  "Reset circuit to initial state."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (.set (:failure-count circuit) 0)
    (.set (:success-count circuit) 0)
    (.set (:half-open-requests circuit) 0)
    (.set (:last-failure-time circuit) 0)
    (transition-to! circuit :closed)))

;; ============================================================================
;; Circuit Execution
;; ============================================================================

(defn execute
  "Execute a function through the circuit breaker."
  [circuit-id f & args]
  (.incrementAndGet (:total-requests (:stats @state)))
  
  (if-let [circuit (get-circuit circuit-id)]
    (if (allow-request? circuit-id)
      (try
        (let [result (apply f args)]
          (if ((:failure-predicate circuit) result)
            (do
              (record-failure! circuit-id)
              (if-let [fallback (:fallback-fn circuit)]
                (fallback {:circuit-id circuit-id
                           :reason :failure
                           :result result})
                result))
            (do
              (record-success! circuit-id)
              result)))
        (catch Exception e
          (record-failure! circuit-id)
          (if-let [fallback (:fallback-fn circuit)]
            (fallback {:circuit-id circuit-id
                       :reason :exception
                       :exception e})
            (throw e))))
      (do
        (.incrementAndGet (:rejected-requests (:stats @state)))
        (if-let [fallback (:fallback-fn circuit)]
          (fallback {:circuit-id circuit-id
                     :reason :circuit-open})
          {:status 503
           :body {:error "Service unavailable"
                  :reason :circuit-open}})))
    (apply f args)))

(defn execute-async
  "Execute a function through the circuit breaker asynchronously."
  [circuit-id f & args]
  (let [result-chan (chan)]
    (go
      (let [result (apply execute circuit-id f args)]
        (>! result-chan result)
        (async/close! result-chan)))
    result-chan))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-circuit-breaker
  "Ring middleware for circuit breaker."
  [handler circuit-id]
  (fn [request]
    (execute circuit-id handler request)))

(defn wrap-circuit-breaker-with-fallback
  "Ring middleware for circuit breaker with fallback."
  [handler circuit-id fallback-handler]
  (fn [request]
    (if (allow-request? circuit-id)
      (try
        (let [response (handler request)]
          (if-let [circuit (get-circuit circuit-id)]
            (if ((:failure-predicate circuit) response)
              (do
                (record-failure! circuit-id)
                (fallback-handler request))
              (do
                (record-success! circuit-id)
                response))
            response))
        (catch Exception e
          (record-failure! circuit-id)
          (fallback-handler request)))
      (fallback-handler request))))

(defn wrap-circuit-breaker-per-route
  "Ring middleware for per-route circuit breakers."
  [handler route-circuit-map]
  (fn [request]
    (let [route (:uri request)
          circuit-id (get route-circuit-map route)]
      (if circuit-id
        (execute circuit-id handler request)
        (handler request)))))

;; ============================================================================
;; Health Monitoring
;; ============================================================================

(defn get-circuit-health
  "Get health status of a circuit."
  [circuit-id]
  (when-let [circuit (get-circuit circuit-id)]
    (let [current-state (.get (:state circuit))
          failure-count (.get (:failure-count circuit))
          success-count (.get (:success-count circuit))]
      {:circuit-id circuit-id
       :state current-state
       :healthy? (= current-state :closed)
       :failure-count failure-count
       :success-count success-count
       :failure-rate (if (pos? (+ failure-count success-count))
                       (/ failure-count (+ failure-count success-count))
                       0.0)
       :last-state-change (.get (:last-state-change circuit))
       :last-failure (.get (:last-failure-time circuit))})))

(defn get-all-circuits-health
  "Get health status of all circuits."
  []
  (mapv (fn [[id _]] (get-circuit-health id)) (:circuits @state)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-failure-threshold!
  "Set default failure threshold."
  [threshold]
  (swap! state assoc-in [:config :default-failure-threshold] threshold))

(defn set-default-success-threshold!
  "Set default success threshold."
  [threshold]
  (swap! state assoc-in [:config :default-success-threshold] threshold))

(defn set-default-reset-timeout!
  "Set default reset timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :default-reset-timeout-ms] timeout-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-circuit-metrics
  "Get circuit breaker metrics."
  []
  (let [stats (:stats @state)]
    {:total-requests (.get (:total-requests stats))
     :successful-requests (.get (:successful-requests stats))
     :failed-requests (.get (:failed-requests stats))
     :rejected-requests (.get (:rejected-requests stats))
     :circuit-opens (.get (:circuit-opens stats))
     :circuit-closes (.get (:circuit-closes stats))
     :circuits-count (count (:circuits @state))
     :open-circuits (count (filter (fn [[_ c]] (= (.get (:state c)) :open))
                                   (:circuits @state)))
     :success-rate (let [total (.get (:total-requests stats))]
                     (if (pos? total)
                       (/ (.get (:successful-requests stats)) total)
                       1.0))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-circuit-stats
  "Get circuit breaker statistics."
  []
  (merge (get-circuit-metrics)
         {:default-failure-threshold (get-in @state [:config :default-failure-threshold])
          :default-success-threshold (get-in @state [:config :default-success-threshold])
          :default-reset-timeout-ms (get-in @state [:config :default-reset-timeout-ms])}))

(defn reset-stats!
  "Reset circuit breaker statistics."
  []
  (.set (:total-requests (:stats @state)) 0)
  (.set (:successful-requests (:stats @state)) 0)
  (.set (:failed-requests (:stats @state)) 0)
  (.set (:rejected-requests (:stats @state)) 0)
  (.set (:circuit-opens (:stats @state)) 0)
  (.set (:circuit-closes (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-circuit!
  "Initialize the request circuit breaker."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request circuit breaker initialized")
    (events/emit! :request-circuit-initialized {})
    true))
