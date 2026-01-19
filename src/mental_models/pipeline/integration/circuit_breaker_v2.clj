(ns mental-models.pipeline.integration.circuit-breaker-v2
  "Circuit Breaker V2 Module
   
   Advanced circuit breaker with:
   - Sliding window metrics
   - Slow call detection
   - Automatic recovery
   - Health-based transitions
   - Event-driven state changes"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent.atomic AtomicLong AtomicReference]
   [java.util.concurrent ConcurrentLinkedDeque Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; CIRCUIT BREAKER STATE
;; =============================================================================

(defonce breaker-state (atom {:breakers {}
                              :scheduler nil
                              :config {:default-failure-threshold 5
                                       :default-success-threshold 3
                                       :default-timeout-ms 30000
                                       :default-slow-call-threshold-ms 5000
                                       :default-slow-call-rate-threshold 50
                                       :default-window-size 10
                                       :health-check-interval-ms 5000}}))

;; =============================================================================
;; CIRCUIT STATES
;; =============================================================================

(def circuit-states #{:closed :open :half-open})

;; =============================================================================
;; SLIDING WINDOW
;; =============================================================================

(defn create-sliding-window
  "Create a sliding window for metrics."
  [size]
  {:size size
   :calls (ConcurrentLinkedDeque.)
   :total-calls (AtomicLong. 0)
   :failed-calls (AtomicLong. 0)
   :slow-calls (AtomicLong. 0)
   :total-duration (AtomicLong. 0)})

(defn record-call!
  "Record a call in the sliding window."
  [window {:keys [success? duration-ms]}]
  (let [calls (:calls window)
        call-record {:success? success?
                     :duration-ms duration-ms
                     :timestamp (System/currentTimeMillis)}]
    ;; Add new call
    (.addLast calls call-record)
    (.incrementAndGet (:total-calls window))
    (.addAndGet (:total-duration window) duration-ms)
    (when-not success?
      (.incrementAndGet (:failed-calls window)))
    (when (> duration-ms 5000) ;; Default slow threshold
      (.incrementAndGet (:slow-calls window)))
    ;; Remove old calls if over size
    (while (> (.size calls) (:size window))
      (when-let [old (.pollFirst calls)]
        (.decrementAndGet (:total-calls window))
        (.addAndGet (:total-duration window) (- (:duration-ms old)))
        (when-not (:success? old)
          (.decrementAndGet (:failed-calls window)))
        (when (> (:duration-ms old) 5000)
          (.decrementAndGet (:slow-calls window)))))))

(defn get-window-metrics
  "Get metrics from the sliding window."
  [window]
  (let [total (.get (:total-calls window))
        failed (.get (:failed-calls window))
        slow (.get (:slow-calls window))
        duration (.get (:total-duration window))]
    {:total-calls total
     :failed-calls failed
     :slow-calls slow
     :failure-rate (if (pos? total) (* 100.0 (/ failed total)) 0.0)
     :slow-call-rate (if (pos? total) (* 100.0 (/ slow total)) 0.0)
     :avg-duration-ms (if (pos? total) (/ duration total) 0)}))

;; =============================================================================
;; CIRCUIT BREAKER CREATION
;; =============================================================================

(defn create-breaker
  "Create a new circuit breaker."
  [breaker-id {:keys [failure-threshold success-threshold timeout-ms
                      slow-call-threshold-ms slow-call-rate-threshold
                      window-size on-state-change on-success on-failure]}]
  (let [config (:config @breaker-state)]
    {:id breaker-id
     :state (AtomicReference. :closed)
     :failure-threshold (or failure-threshold (:default-failure-threshold config))
     :success-threshold (or success-threshold (:default-success-threshold config))
     :timeout-ms (or timeout-ms (:default-timeout-ms config))
     :slow-call-threshold-ms (or slow-call-threshold-ms (:default-slow-call-threshold-ms config))
     :slow-call-rate-threshold (or slow-call-rate-threshold (:default-slow-call-rate-threshold config))
     :window-size (or window-size (:default-window-size config))
     :window (create-sliding-window (or window-size (:default-window-size config)))
     :consecutive-successes (AtomicLong. 0)
     :consecutive-failures (AtomicLong. 0)
     :last-failure-time (AtomicLong. 0)
     :opened-at (AtomicLong. 0)
     :on-state-change on-state-change
     :on-success on-success
     :on-failure on-failure
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; BREAKER REGISTRATION
;; =============================================================================

(defn register-breaker!
  "Register a circuit breaker."
  [breaker-id opts]
  (log/info "Registering circuit breaker" {:id breaker-id})
  (let [breaker (create-breaker breaker-id opts)]
    (swap! breaker-state assoc-in [:breakers breaker-id] breaker)
    (metrics/inc-counter! :circuitbreakerv2/breakers-registered)
    breaker-id))

(defn unregister-breaker!
  "Unregister a circuit breaker."
  [breaker-id]
  (log/info "Unregistering circuit breaker" {:id breaker-id})
  (swap! breaker-state update :breakers dissoc breaker-id))

(defn get-breaker
  "Get a circuit breaker."
  [breaker-id]
  (get-in @breaker-state [:breakers breaker-id]))

(defn list-breakers
  "List all circuit breakers."
  []
  (keys (:breakers @breaker-state)))

;; =============================================================================
;; STATE TRANSITIONS
;; =============================================================================

(defn get-state
  "Get the current state of a circuit breaker."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (.get ^AtomicReference (:state breaker))))

(defn transition-to!
  "Transition a circuit breaker to a new state."
  [breaker-id new-state]
  (when-let [breaker (get-breaker breaker-id)]
    (let [^AtomicReference state-ref (:state breaker)
          old-state (.get state-ref)]
      (when (not= old-state new-state)
        (.set state-ref new-state)
        (log/info "Circuit breaker state transition" {:id breaker-id
                                                       :from old-state
                                                       :to new-state})
        (metrics/inc-counter! (keyword (str "circuitbreakerv2/transitions-to-" (name new-state))))
        (events/publish! :circuitbreakerv2/state-changed {:breaker-id breaker-id
                                                           :from old-state
                                                           :to new-state})
        (when-let [callback (:on-state-change breaker)]
          (try (callback {:breaker-id breaker-id :from old-state :to new-state})
               (catch Exception e
                 (log/error "State change callback failed" {:error (.getMessage e)}))))
        ;; Reset counters on transition
        (when (= new-state :open)
          (.set ^AtomicLong (:opened-at breaker) (System/currentTimeMillis)))
        (when (= new-state :half-open)
          (.set ^AtomicLong (:consecutive-successes breaker) 0))
        (when (= new-state :closed)
          (.set ^AtomicLong (:consecutive-failures breaker) 0)
          (.set ^AtomicLong (:consecutive-successes breaker) 0))))))

;; =============================================================================
;; CALL RECORDING
;; =============================================================================

(defn record-success!
  "Record a successful call."
  [breaker-id duration-ms]
  (when-let [breaker (get-breaker breaker-id)]
    (record-call! (:window breaker) {:success? true :duration-ms duration-ms})
    (.set ^AtomicLong (:consecutive-failures breaker) 0)
    (.incrementAndGet ^AtomicLong (:consecutive-successes breaker))
    (metrics/inc-counter! :circuitbreakerv2/successful-calls)
    ;; Check for half-open -> closed transition
    (when (= (get-state breaker-id) :half-open)
      (when (>= (.get ^AtomicLong (:consecutive-successes breaker))
                (:success-threshold breaker))
        (transition-to! breaker-id :closed)))
    ;; Call success callback
    (when-let [callback (:on-success breaker)]
      (try (callback {:breaker-id breaker-id :duration-ms duration-ms})
           (catch Exception _)))))

(defn record-failure!
  "Record a failed call."
  [breaker-id duration-ms & {:keys [exception]}]
  (when-let [breaker (get-breaker breaker-id)]
    (record-call! (:window breaker) {:success? false :duration-ms duration-ms})
    (.set ^AtomicLong (:consecutive-successes breaker) 0)
    (.incrementAndGet ^AtomicLong (:consecutive-failures breaker))
    (.set ^AtomicLong (:last-failure-time breaker) (System/currentTimeMillis))
    (metrics/inc-counter! :circuitbreakerv2/failed-calls)
    ;; Check for state transitions
    (let [state (get-state breaker-id)
          window-metrics (get-window-metrics (:window breaker))]
      (cond
        ;; Half-open -> open on failure
        (= state :half-open)
        (transition-to! breaker-id :open)
        ;; Closed -> open on threshold
        (and (= state :closed)
             (or (>= (:failure-rate window-metrics) 50.0)
                 (>= (.get ^AtomicLong (:consecutive-failures breaker))
                     (:failure-threshold breaker))))
        (transition-to! breaker-id :open)))
    ;; Call failure callback
    (when-let [callback (:on-failure breaker)]
      (try (callback {:breaker-id breaker-id :duration-ms duration-ms :exception exception})
           (catch Exception _)))))

;; =============================================================================
;; CIRCUIT BREAKER EXECUTION
;; =============================================================================

(defn allow-request?
  "Check if a request should be allowed."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [state (get-state breaker-id)]
      (case state
        :closed true
        :half-open true
        :open (let [opened-at (.get ^AtomicLong (:opened-at breaker))
                    timeout (:timeout-ms breaker)
                    now (System/currentTimeMillis)]
                (when (> (- now opened-at) timeout)
                  (transition-to! breaker-id :half-open)
                  true))))))

(defn execute
  "Execute a function with circuit breaker protection."
  [breaker-id f & {:keys [fallback timeout-ms]}]
  (if-not (allow-request? breaker-id)
    (do
      (metrics/inc-counter! :circuitbreakerv2/rejected-calls)
      (if fallback
        (fallback {:breaker-id breaker-id :reason :circuit-open})
        (throw (ex-info "Circuit breaker is open" {:breaker-id breaker-id}))))
    (let [start-time (System/currentTimeMillis)]
      (try
        (let [result (if timeout-ms
                       (deref (future (f)) timeout-ms ::timeout)
                       (f))]
          (if (= result ::timeout)
            (do
              (record-failure! breaker-id (- (System/currentTimeMillis) start-time))
              (if fallback
                (fallback {:breaker-id breaker-id :reason :timeout})
                (throw (ex-info "Circuit breaker call timed out" {:breaker-id breaker-id}))))
            (do
              (record-success! breaker-id (- (System/currentTimeMillis) start-time))
              result)))
        (catch Exception e
          (record-failure! breaker-id (- (System/currentTimeMillis) start-time) :exception e)
          (if fallback
            (fallback {:breaker-id breaker-id :reason :exception :exception e})
            (throw e)))))))

(defmacro with-breaker
  "Execute body with circuit breaker protection."
  [breaker-id & body]
  `(execute ~breaker-id (fn [] ~@body)))

(defmacro with-breaker-fallback
  "Execute body with circuit breaker and fallback."
  [breaker-id fallback-expr & body]
  `(execute ~breaker-id (fn [] ~@body) :fallback (fn [_#] ~fallback-expr)))

;; =============================================================================
;; HEALTH CHECKS
;; =============================================================================

(defn check-breaker-health
  "Check the health of a circuit breaker."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [state (get-state breaker-id)
          window-metrics (get-window-metrics (:window breaker))]
      {:id breaker-id
       :state state
       :healthy? (= state :closed)
       :metrics window-metrics
       :consecutive-failures (.get ^AtomicLong (:consecutive-failures breaker))
       :consecutive-successes (.get ^AtomicLong (:consecutive-successes breaker))})))

(defn check-all-breakers-health
  "Check the health of all circuit breakers."
  []
  (into {} (for [breaker-id (list-breakers)]
             [breaker-id (check-breaker-health breaker-id)])))

;; =============================================================================
;; AUTOMATIC RECOVERY
;; =============================================================================

(defn attempt-recovery!
  "Attempt to recover an open circuit breaker."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (when (= (get-state breaker-id) :open)
      (let [opened-at (.get ^AtomicLong (:opened-at breaker))
            timeout (:timeout-ms breaker)
            now (System/currentTimeMillis)]
        (when (> (- now opened-at) timeout)
          (log/info "Attempting circuit breaker recovery" {:id breaker-id})
          (transition-to! breaker-id :half-open))))))

(defn start-recovery-scheduler!
  "Start the automatic recovery scheduler."
  []
  (when (and (flags/is-enabled? "circuit-breaker-v2")
             (nil? (:scheduler @breaker-state)))
    (log/info "Starting circuit breaker recovery scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @breaker-state [:config :health-check-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (doseq [breaker-id (list-breakers)]
                                 (attempt-recovery! breaker-id))
                               (catch Exception e
                                 (log/error "Recovery check error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! breaker-state assoc :scheduler executor))))

(defn stop-recovery-scheduler!
  "Stop the automatic recovery scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @breaker-state)]
    (log/info "Stopping circuit breaker recovery scheduler")
    (.shutdown executor)
    (swap! breaker-state assoc :scheduler nil)))

;; =============================================================================
;; MANUAL CONTROLS
;; =============================================================================

(defn force-open!
  "Force a circuit breaker to open state."
  [breaker-id]
  (log/warn "Forcing circuit breaker open" {:id breaker-id})
  (transition-to! breaker-id :open))

(defn force-close!
  "Force a circuit breaker to closed state."
  [breaker-id]
  (log/info "Forcing circuit breaker closed" {:id breaker-id})
  (transition-to! breaker-id :closed))

(defn reset!
  "Reset a circuit breaker to initial state."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (log/info "Resetting circuit breaker" {:id breaker-id})
    (transition-to! breaker-id :closed)
    (.set ^AtomicLong (:consecutive-failures breaker) 0)
    (.set ^AtomicLong (:consecutive-successes breaker) 0)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-circuit-breaker-v2!
  "Initialize circuit breaker v2."
  []
  (log/info "Initializing circuit breaker v2")
  ;; Register feature flag
  (flags/register-flag! "circuit-breaker-v2" "Enable circuit breaker v2" true)
  ;; Create metrics
  (metrics/create-counter! :circuitbreakerv2/breakers-registered "Breakers registered")
  (metrics/create-counter! :circuitbreakerv2/successful-calls "Successful calls")
  (metrics/create-counter! :circuitbreakerv2/failed-calls "Failed calls")
  (metrics/create-counter! :circuitbreakerv2/rejected-calls "Rejected calls")
  (metrics/create-gauge! :circuitbreakerv2/open-breakers "Open breakers"
                         #(count (filter #(= (get-state %) :open) (list-breakers))))
  (log/info "Circuit breaker v2 initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-breaker-status []
  {:enabled (flags/is-enabled? "circuit-breaker-v2")
   :breakers (count (:breakers @breaker-state))
   :by-state (frequencies (map get-state (list-breakers)))
   :scheduler-active (some? (:scheduler @breaker-state))
   :config (:config @breaker-state)})
