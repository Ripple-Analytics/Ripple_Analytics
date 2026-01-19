(ns mental-models.pipeline.integration.retry-policy
  "Retry Policy Module
   
   Resilient operations:
   - Exponential backoff
   - Linear backoff
   - Constant backoff
   - Jitter
   - Retry conditions"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; RETRY POLICY STATE
;; =============================================================================

(defonce policy-state (atom {:policies {}
                             :config {:default-max-retries 3
                                      :default-initial-delay-ms 1000
                                      :default-max-delay-ms 30000
                                      :default-multiplier 2.0
                                      :default-jitter 0.1}}))

;; =============================================================================
;; BACKOFF STRATEGIES
;; =============================================================================

(defn exponential-backoff
  "Calculate exponential backoff delay."
  [attempt initial-delay-ms multiplier max-delay-ms]
  (min max-delay-ms
       (long (* initial-delay-ms (Math/pow multiplier attempt)))))

(defn linear-backoff
  "Calculate linear backoff delay."
  [attempt initial-delay-ms increment-ms max-delay-ms]
  (min max-delay-ms
       (+ initial-delay-ms (* attempt increment-ms))))

(defn constant-backoff
  "Calculate constant backoff delay."
  [delay-ms]
  delay-ms)

(defn fibonacci-backoff
  "Calculate Fibonacci backoff delay."
  [attempt initial-delay-ms max-delay-ms]
  (let [fib (fn fib [n]
              (cond
                (<= n 0) 0
                (= n 1) 1
                :else (+ (fib (- n 1)) (fib (- n 2)))))]
    (min max-delay-ms
         (* initial-delay-ms (fib (inc attempt))))))

(defn add-jitter
  "Add jitter to a delay."
  [delay-ms jitter-factor]
  (let [jitter (* delay-ms jitter-factor)
        random-jitter (* (- (rand) 0.5) 2 jitter)]
    (max 0 (long (+ delay-ms random-jitter)))))

;; =============================================================================
;; RETRY CONDITIONS
;; =============================================================================

(defn always-retry
  "Always retry on failure."
  [_exception _attempt]
  true)

(defn never-retry
  "Never retry."
  [_exception _attempt]
  false)

(defn retry-on-exception-types
  "Retry only on specific exception types."
  [exception-types]
  (fn [exception _attempt]
    (some #(instance? % exception) exception-types)))

(defn retry-on-exception-message
  "Retry if exception message matches pattern."
  [pattern]
  (fn [exception _attempt]
    (when-let [msg (.getMessage exception)]
      (re-find pattern msg))))

(defn max-attempts-condition
  "Retry up to max attempts."
  [max-attempts]
  (fn [_exception attempt]
    (< attempt max-attempts)))

(defn combine-conditions
  "Combine multiple retry conditions with AND."
  [& conditions]
  (fn [exception attempt]
    (every? #(% exception attempt) conditions)))

;; =============================================================================
;; POLICY CREATION
;; =============================================================================

(defn create-policy
  "Create a retry policy."
  [{:keys [max-retries backoff-type initial-delay-ms max-delay-ms
           multiplier increment-ms jitter retry-condition]}]
  {:max-retries (or max-retries (get-in @policy-state [:config :default-max-retries]))
   :backoff-type (or backoff-type :exponential)
   :initial-delay-ms (or initial-delay-ms (get-in @policy-state [:config :default-initial-delay-ms]))
   :max-delay-ms (or max-delay-ms (get-in @policy-state [:config :default-max-delay-ms]))
   :multiplier (or multiplier (get-in @policy-state [:config :default-multiplier]))
   :increment-ms (or increment-ms 1000)
   :jitter (or jitter (get-in @policy-state [:config :default-jitter]))
   :retry-condition (or retry-condition always-retry)})

(defn register-policy!
  "Register a retry policy."
  [policy-id policy-config]
  (log/info "Registering retry policy" {:id policy-id})
  (swap! policy-state assoc-in [:policies policy-id]
         (merge (create-policy policy-config)
                {:id policy-id
                 :created-at (System/currentTimeMillis)}))
  (metrics/inc-counter! :retry/policies-registered)
  policy-id)

(defn unregister-policy!
  "Unregister a retry policy."
  [policy-id]
  (log/info "Unregistering retry policy" {:id policy-id})
  (swap! policy-state update :policies dissoc policy-id))

(defn get-policy
  "Get a retry policy."
  [policy-id]
  (get-in @policy-state [:policies policy-id]))

(defn list-policies
  "List all retry policies."
  []
  (keys (:policies @policy-state)))

;; =============================================================================
;; DELAY CALCULATION
;; =============================================================================

(defn calculate-delay
  "Calculate delay for a retry attempt."
  [policy attempt]
  (let [base-delay (case (:backoff-type policy)
                     :exponential (exponential-backoff attempt
                                                       (:initial-delay-ms policy)
                                                       (:multiplier policy)
                                                       (:max-delay-ms policy))
                     :linear (linear-backoff attempt
                                             (:initial-delay-ms policy)
                                             (:increment-ms policy)
                                             (:max-delay-ms policy))
                     :constant (constant-backoff (:initial-delay-ms policy))
                     :fibonacci (fibonacci-backoff attempt
                                                   (:initial-delay-ms policy)
                                                   (:max-delay-ms policy))
                     (:initial-delay-ms policy))]
    (if (pos? (:jitter policy))
      (add-jitter base-delay (:jitter policy))
      base-delay)))

;; =============================================================================
;; RETRY EXECUTION
;; =============================================================================

(defn execute-with-retry
  "Execute a function with retry policy."
  [policy-id f]
  (when (flags/is-enabled? "retry-policy")
    (if-let [policy (get-policy policy-id)]
      (loop [attempt 0
             last-exception nil]
        (if (> attempt (:max-retries policy))
          ;; Max retries exceeded
          (do
            (metrics/inc-counter! :retry/max-retries-exceeded)
            (events/publish! :retry/exhausted {:policy policy-id :attempts attempt})
            {:success false
             :error (if last-exception (.getMessage last-exception) "Max retries exceeded")
             :attempts attempt
             :exception last-exception})
          ;; Try execution
          (try
            (let [result (f)]
              (when (pos? attempt)
                (metrics/inc-counter! :retry/successful-retries))
              {:success true :result result :attempts (inc attempt)})
            (catch Exception e
              (log/debug "Retry attempt failed" {:policy policy-id :attempt attempt :error (.getMessage e)})
              (metrics/inc-counter! :retry/attempts)
              ;; Check if should retry
              (if (and (< attempt (:max-retries policy))
                       ((:retry-condition policy) e attempt))
                (let [delay (calculate-delay policy attempt)]
                  (log/debug "Retrying after delay" {:policy policy-id :attempt attempt :delay delay})
                  (Thread/sleep delay)
                  (recur (inc attempt) e))
                ;; Don't retry
                (do
                  (metrics/inc-counter! :retry/failures)
                  (events/publish! :retry/failed {:policy policy-id :attempts (inc attempt)})
                  {:success false
                   :error (.getMessage e)
                   :attempts (inc attempt)
                   :exception e}))))))
      {:success false :error "Policy not found"})))

(defn retry
  "Execute with retry using inline policy config."
  [policy-config f]
  (let [policy (create-policy policy-config)]
    (loop [attempt 0
           last-exception nil]
      (if (> attempt (:max-retries policy))
        {:success false
         :error (if last-exception (.getMessage last-exception) "Max retries exceeded")
         :attempts attempt
         :exception last-exception}
        (try
          (let [result (f)]
            {:success true :result result :attempts (inc attempt)})
          (catch Exception e
            (if (and (< attempt (:max-retries policy))
                     ((:retry-condition policy) e attempt))
              (let [delay (calculate-delay policy attempt)]
                (Thread/sleep delay)
                (recur (inc attempt) e))
              {:success false
               :error (.getMessage e)
               :attempts (inc attempt)
               :exception e})))))))

;; =============================================================================
;; RETRY MACROS
;; =============================================================================

(defmacro with-retry
  "Execute body with retry policy."
  [policy-id & body]
  `(let [result# (execute-with-retry ~policy-id (fn [] ~@body))]
     (if (:success result#)
       (:result result#)
       (throw (or (:exception result#)
                  (ex-info "Retry failed" result#))))))

(defmacro try-with-retry
  "Try to execute body with retry, return nil on failure."
  [policy-id & body]
  `(let [result# (execute-with-retry ~policy-id (fn [] ~@body))]
     (when (:success result#)
       (:result result#))))

;; =============================================================================
;; PREDEFINED POLICIES
;; =============================================================================

(def aggressive-retry
  "Aggressive retry policy for critical operations."
  {:max-retries 10
   :backoff-type :exponential
   :initial-delay-ms 100
   :max-delay-ms 60000
   :multiplier 2.0
   :jitter 0.2})

(def conservative-retry
  "Conservative retry policy for non-critical operations."
  {:max-retries 3
   :backoff-type :exponential
   :initial-delay-ms 1000
   :max-delay-ms 10000
   :multiplier 2.0
   :jitter 0.1})

(def immediate-retry
  "Immediate retry with no delay."
  {:max-retries 3
   :backoff-type :constant
   :initial-delay-ms 0
   :jitter 0})

(def network-retry
  "Retry policy optimized for network operations."
  {:max-retries 5
   :backoff-type :exponential
   :initial-delay-ms 500
   :max-delay-ms 30000
   :multiplier 2.0
   :jitter 0.25
   :retry-condition (retry-on-exception-types [java.net.SocketException
                                                java.net.ConnectException
                                                java.io.IOException])})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-retry-policy!
  "Initialize retry policy."
  []
  (log/info "Initializing retry policy")
  ;; Register feature flag
  (flags/register-flag! "retry-policy" "Enable retry policy" true)
  ;; Create metrics
  (metrics/create-counter! :retry/policies-registered "Policies registered")
  (metrics/create-counter! :retry/attempts "Retry attempts")
  (metrics/create-counter! :retry/successful-retries "Successful retries")
  (metrics/create-counter! :retry/failures "Retry failures")
  (metrics/create-counter! :retry/max-retries-exceeded "Max retries exceeded")
  ;; Register predefined policies
  (register-policy! :aggressive aggressive-retry)
  (register-policy! :conservative conservative-retry)
  (register-policy! :immediate immediate-retry)
  (register-policy! :network network-retry)
  (log/info "Retry policy initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-retry-status []
  {:enabled (flags/is-enabled? "retry-policy")
   :policies (count (:policies @policy-state))
   :policy-ids (list-policies)
   :config (:config @policy-state)})
