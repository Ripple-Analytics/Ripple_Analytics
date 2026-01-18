(ns mental-models.resilience.circuit-breaker
  "Circuit Breaker Module for Mental Models Pipeline
   
   Provides fault tolerance with:
   - Circuit breaker pattern
   - Failure threshold tracking
   - Automatic recovery
   - Half-open state testing
   - Metrics and monitoring"
  (:require
   [clojure.string :as str])
  (:import
   [java.util.concurrent.atomic AtomicInteger AtomicLong AtomicReference]
   [java.time Instant Duration]))

;; =============================================================================
;; CIRCUIT BREAKER STATES
;; =============================================================================

(def states #{:closed :open :half-open})

;; =============================================================================
;; CIRCUIT BREAKER DEFINITION
;; =============================================================================

(defrecord CircuitBreaker [name
                           state
                           failure-count
                           success-count
                           last-failure-time
                           failure-threshold
                           success-threshold
                           timeout-ms
                           metrics])

(defn create-circuit-breaker
  "Create a new circuit breaker."
  [name & {:keys [failure-threshold success-threshold timeout-ms]
           :or {failure-threshold 5
                success-threshold 3
                timeout-ms 30000}}]
  (->CircuitBreaker name
                    (AtomicReference. :closed)
                    (AtomicInteger. 0)
                    (AtomicInteger. 0)
                    (AtomicLong. 0)
                    failure-threshold
                    success-threshold
                    timeout-ms
                    (atom {:total-calls 0
                           :successful-calls 0
                           :failed-calls 0
                           :rejected-calls 0
                           :state-changes []})))

;; =============================================================================
;; STATE MANAGEMENT
;; =============================================================================

(defn get-state [cb]
  (.get (:state cb)))

(defn set-state! [cb new-state]
  (let [old-state (.getAndSet (:state cb) new-state)]
    (when (not= old-state new-state)
      (swap! (:metrics cb) update :state-changes conj
             {:from old-state :to new-state :timestamp (Instant/now)}))
    new-state))

(defn closed? [cb]
  (= :closed (get-state cb)))

(defn open? [cb]
  (= :open (get-state cb)))

(defn half-open? [cb]
  (= :half-open (get-state cb)))

;; =============================================================================
;; FAILURE TRACKING
;; =============================================================================

(defn record-failure! [cb]
  (let [count (.incrementAndGet (:failure-count cb))]
    (.set (:last-failure-time cb) (.toEpochMilli (Instant/now)))
    (swap! (:metrics cb) update :failed-calls inc)
    (when (and (closed? cb) (>= count (:failure-threshold cb)))
      (set-state! cb :open)
      (.set (:failure-count cb) 0))
    count))

(defn record-success! [cb]
  (swap! (:metrics cb) update :successful-calls inc)
  (cond
    (half-open? cb)
    (let [count (.incrementAndGet (:success-count cb))]
      (when (>= count (:success-threshold cb))
        (set-state! cb :closed)
        (.set (:success-count cb) 0)
        (.set (:failure-count cb) 0))
      count)
    
    (closed? cb)
    (do
      (.set (:failure-count cb) 0)
      0)
    
    :else 0))

(defn reset-counts! [cb]
  (.set (:failure-count cb) 0)
  (.set (:success-count cb) 0))

;; =============================================================================
;; TIMEOUT CHECKING
;; =============================================================================

(defn timeout-elapsed? [cb]
  (let [last-failure (.get (:last-failure-time cb))
        now (.toEpochMilli (Instant/now))]
    (> (- now last-failure) (:timeout-ms cb))))

(defn check-timeout! [cb]
  (when (and (open? cb) (timeout-elapsed? cb))
    (set-state! cb :half-open)
    (.set (:success-count cb) 0)))

;; =============================================================================
;; EXECUTION
;; =============================================================================

(defn allow-request? [cb]
  (check-timeout! cb)
  (let [state (get-state cb)]
    (case state
      :closed true
      :half-open true
      :open false)))

(defn execute
  "Execute a function through the circuit breaker."
  [cb f]
  (swap! (:metrics cb) update :total-calls inc)
  (if (allow-request? cb)
    (try
      (let [result (f)]
        (record-success! cb)
        {:status :success :result result})
      (catch Exception e
        (record-failure! cb)
        {:status :failure :error e}))
    (do
      (swap! (:metrics cb) update :rejected-calls inc)
      {:status :rejected :reason :circuit-open})))

(defn execute-with-fallback
  "Execute a function with a fallback when circuit is open."
  [cb f fallback]
  (let [result (execute cb f)]
    (if (= :rejected (:status result))
      {:status :fallback :result (fallback)}
      result)))

;; =============================================================================
;; MANUAL CONTROLS
;; =============================================================================

(defn force-open! [cb]
  (set-state! cb :open))

(defn force-close! [cb]
  (set-state! cb :closed)
  (reset-counts! cb))

(defn force-half-open! [cb]
  (set-state! cb :half-open)
  (.set (:success-count cb) 0))

(defn reset! [cb]
  (force-close! cb)
  (reset! (:metrics cb) {:total-calls 0
                         :successful-calls 0
                         :failed-calls 0
                         :rejected-calls 0
                         :state-changes []}))

;; =============================================================================
;; METRICS
;; =============================================================================

(defn get-metrics [cb]
  (merge @(:metrics cb)
         {:current-state (get-state cb)
          :failure-count (.get (:failure-count cb))
          :success-count (.get (:success-count cb))
          :failure-threshold (:failure-threshold cb)
          :success-threshold (:success-threshold cb)
          :timeout-ms (:timeout-ms cb)}))

(defn get-failure-rate [cb]
  (let [metrics @(:metrics cb)
        total (:total-calls metrics)
        failed (:failed-calls metrics)]
    (if (zero? total)
      0.0
      (/ failed total))))

(defn get-success-rate [cb]
  (- 1.0 (get-failure-rate cb)))

;; =============================================================================
;; CIRCUIT BREAKER REGISTRY
;; =============================================================================

(def ^:private circuit-breakers (atom {}))

(defn register-circuit-breaker!
  "Register a circuit breaker in the global registry."
  [cb]
  (swap! circuit-breakers assoc (:name cb) cb)
  cb)

(defn get-circuit-breaker [name]
  (get @circuit-breakers name))

(defn get-all-circuit-breakers []
  (vals @circuit-breakers))

(defn unregister-circuit-breaker! [name]
  (swap! circuit-breakers dissoc name))

;; =============================================================================
;; BUILT-IN CIRCUIT BREAKERS
;; =============================================================================

(defn create-lm-studio-circuit-breaker []
  (register-circuit-breaker!
   (create-circuit-breaker "lm-studio"
                           :failure-threshold 5
                           :success-threshold 3
                           :timeout-ms 30000)))

(defn create-database-circuit-breaker []
  (register-circuit-breaker!
   (create-circuit-breaker "database"
                           :failure-threshold 3
                           :success-threshold 2
                           :timeout-ms 15000)))

(defn create-notification-circuit-breaker []
  (register-circuit-breaker!
   (create-circuit-breaker "notification"
                           :failure-threshold 5
                           :success-threshold 2
                           :timeout-ms 60000)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-all-stats []
  (into {}
        (map (fn [[name cb]]
               [name (get-metrics cb)])
             @circuit-breakers)))

(defn get-unhealthy-circuit-breakers []
  (filter #(open? (val %)) @circuit-breakers))

;; =============================================================================
;; MACROS
;; =============================================================================

(defmacro with-circuit-breaker
  "Execute body with circuit breaker protection."
  [cb-name & body]
  `(let [cb# (get-circuit-breaker ~cb-name)]
     (if cb#
       (execute cb# (fn [] ~@body))
       {:status :error :reason :circuit-breaker-not-found})))

(defmacro with-circuit-breaker-fallback
  "Execute body with circuit breaker and fallback."
  [cb-name fallback & body]
  `(let [cb# (get-circuit-breaker ~cb-name)]
     (if cb#
       (execute-with-fallback cb# (fn [] ~@body) (fn [] ~fallback))
       {:status :error :reason :circuit-breaker-not-found})))
