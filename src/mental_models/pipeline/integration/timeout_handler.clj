(ns mental-models.pipeline.integration.timeout-handler
  "Timeout Handler Module
   
   Operation timeouts:
   - Configurable timeouts
   - Timeout callbacks
   - Cancellation support
   - Deadline propagation
   - Timeout metrics"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ExecutorService Future TimeUnit
    TimeoutException CancellationException]))

;; =============================================================================
;; TIMEOUT STATE
;; =============================================================================

(defonce timeout-state (atom {:timeouts {}
                              :active-operations {}
                              :executor nil
                              :config {:default-timeout-ms 30000
                                       :min-timeout-ms 100
                                       :max-timeout-ms 600000}}))

;; =============================================================================
;; EXECUTOR MANAGEMENT
;; =============================================================================

(defn get-executor
  "Get or create the timeout executor."
  []
  (or (:executor @timeout-state)
      (let [executor (Executors/newCachedThreadPool)]
        (swap! timeout-state assoc :executor executor)
        executor)))

(defn shutdown-executor!
  "Shutdown the timeout executor."
  []
  (when-let [^ExecutorService executor (:executor @timeout-state)]
    (.shutdown executor)
    (swap! timeout-state assoc :executor nil)))

;; =============================================================================
;; TIMEOUT CONFIGURATION
;; =============================================================================

(defn register-timeout!
  "Register a timeout configuration."
  [timeout-id {:keys [timeout-ms on-timeout on-success on-cancel]}]
  (log/info "Registering timeout" {:id timeout-id :timeout-ms timeout-ms})
  (let [timeout-ms (or timeout-ms (get-in @timeout-state [:config :default-timeout-ms]))
        timeout-ms (max (get-in @timeout-state [:config :min-timeout-ms])
                        (min timeout-ms (get-in @timeout-state [:config :max-timeout-ms])))]
    (swap! timeout-state assoc-in [:timeouts timeout-id]
           {:id timeout-id
            :timeout-ms timeout-ms
            :on-timeout on-timeout
            :on-success on-success
            :on-cancel on-cancel
            :created-at (System/currentTimeMillis)})
    (metrics/inc-counter! :timeout/timeouts-registered)
    timeout-id))

(defn unregister-timeout!
  "Unregister a timeout configuration."
  [timeout-id]
  (log/info "Unregistering timeout" {:id timeout-id})
  (swap! timeout-state update :timeouts dissoc timeout-id))

(defn get-timeout
  "Get a timeout configuration."
  [timeout-id]
  (get-in @timeout-state [:timeouts timeout-id]))

(defn list-timeouts
  "List all timeout configurations."
  []
  (keys (:timeouts @timeout-state)))

;; =============================================================================
;; OPERATION TRACKING
;; =============================================================================

(defn generate-operation-id
  "Generate a unique operation ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn register-operation!
  "Register an active operation."
  [operation-id {:keys [timeout-id future deadline]}]
  (swap! timeout-state assoc-in [:active-operations operation-id]
         {:id operation-id
          :timeout-id timeout-id
          :future future
          :deadline deadline
          :started-at (System/currentTimeMillis)
          :status :running}))

(defn unregister-operation!
  "Unregister an operation."
  [operation-id]
  (swap! timeout-state update :active-operations dissoc operation-id))

(defn get-operation
  "Get an active operation."
  [operation-id]
  (get-in @timeout-state [:active-operations operation-id]))

(defn list-operations
  "List all active operations."
  []
  (vals (:active-operations @timeout-state)))

;; =============================================================================
;; TIMEOUT EXECUTION
;; =============================================================================

(defn execute-with-timeout
  "Execute a function with a timeout."
  [timeout-id f]
  (when (flags/is-enabled? "timeout-handler")
    (let [timeout-config (or (get-timeout timeout-id)
                             {:timeout-ms (get-in @timeout-state [:config :default-timeout-ms])})
          timeout-ms (:timeout-ms timeout-config)
          operation-id (generate-operation-id)
          ^ExecutorService executor (get-executor)
          ^Future future (.submit executor ^Callable f)
          deadline (+ (System/currentTimeMillis) timeout-ms)]
      (register-operation! operation-id {:timeout-id timeout-id
                                         :future future
                                         :deadline deadline})
      (try
        (let [result (.get future timeout-ms TimeUnit/MILLISECONDS)]
          (unregister-operation! operation-id)
          (metrics/inc-counter! :timeout/operations-completed)
          (when-let [on-success (:on-success timeout-config)]
            (on-success result))
          {:success true :result result :operation-id operation-id})
        (catch TimeoutException _
          (log/warn "Operation timed out" {:timeout-id timeout-id :operation-id operation-id})
          (.cancel future true)
          (unregister-operation! operation-id)
          (metrics/inc-counter! :timeout/operations-timed-out)
          (events/publish! :timeout/timed-out {:timeout-id timeout-id :operation-id operation-id})
          (when-let [on-timeout (:on-timeout timeout-config)]
            (on-timeout))
          {:success false :error "Operation timed out" :operation-id operation-id :timed-out true})
        (catch CancellationException _
          (log/info "Operation cancelled" {:timeout-id timeout-id :operation-id operation-id})
          (unregister-operation! operation-id)
          (metrics/inc-counter! :timeout/operations-cancelled)
          (when-let [on-cancel (:on-cancel timeout-config)]
            (on-cancel))
          {:success false :error "Operation cancelled" :operation-id operation-id :cancelled true})
        (catch Exception e
          (unregister-operation! operation-id)
          (metrics/inc-counter! :timeout/operations-failed)
          {:success false :error (.getMessage e) :operation-id operation-id :exception e})))))

(defn execute-with-deadline
  "Execute a function with an absolute deadline."
  [deadline-ms f]
  (let [now (System/currentTimeMillis)
        timeout-ms (- deadline-ms now)]
    (if (pos? timeout-ms)
      (execute-with-timeout nil (fn []
                                  (let [timeout-config {:timeout-ms timeout-ms}]
                                    (swap! timeout-state assoc-in [:timeouts :temp-deadline] timeout-config)
                                    (try
                                      (f)
                                      (finally
                                        (swap! timeout-state update :timeouts dissoc :temp-deadline))))))
      {:success false :error "Deadline already passed"})))

;; =============================================================================
;; CANCELLATION
;; =============================================================================

(defn cancel-operation!
  "Cancel an active operation."
  [operation-id]
  (when-let [operation (get-operation operation-id)]
    (log/info "Cancelling operation" {:operation-id operation-id})
    (let [^Future future (:future operation)]
      (.cancel future true)
      (swap! timeout-state assoc-in [:active-operations operation-id :status] :cancelled)
      (metrics/inc-counter! :timeout/operations-cancelled)
      true)))

(defn cancel-all-operations!
  "Cancel all active operations."
  []
  (let [operations (list-operations)]
    (doseq [op operations]
      (cancel-operation! (:id op)))
    (count operations)))

;; =============================================================================
;; TIMEOUT MACROS
;; =============================================================================

(defmacro with-timeout
  "Execute body with a timeout."
  [timeout-id & body]
  `(let [result# (execute-with-timeout ~timeout-id (fn [] ~@body))]
     (if (:success result#)
       (:result result#)
       (throw (ex-info (or (:error result#) "Timeout") result#)))))

(defmacro try-with-timeout
  "Try to execute body with timeout, return nil on timeout."
  [timeout-id & body]
  `(let [result# (execute-with-timeout ~timeout-id (fn [] ~@body))]
     (when (:success result#)
       (:result result#))))

(defmacro with-timeout-ms
  "Execute body with a specific timeout in milliseconds."
  [timeout-ms & body]
  `(let [temp-id# (keyword (str "temp-" (System/currentTimeMillis)))]
     (register-timeout! temp-id# {:timeout-ms ~timeout-ms})
     (try
       (with-timeout temp-id# ~@body)
       (finally
         (unregister-timeout! temp-id#)))))

;; =============================================================================
;; DEADLINE CONTEXT
;; =============================================================================

(def ^:dynamic *deadline* nil)

(defmacro with-deadline
  "Execute body with a deadline context."
  [deadline-ms & body]
  `(binding [*deadline* ~deadline-ms]
     ~@body))

(defn remaining-time
  "Get remaining time until deadline."
  []
  (when *deadline*
    (max 0 (- *deadline* (System/currentTimeMillis)))))

(defn deadline-exceeded?
  "Check if deadline has been exceeded."
  []
  (when *deadline*
    (<= *deadline* (System/currentTimeMillis))))

(defn check-deadline!
  "Check deadline and throw if exceeded."
  []
  (when (deadline-exceeded?)
    (throw (ex-info "Deadline exceeded" {:deadline *deadline*}))))

;; =============================================================================
;; TIMEOUT STATISTICS
;; =============================================================================

(defn get-operation-stats
  "Get statistics for active operations."
  []
  (let [operations (list-operations)
        now (System/currentTimeMillis)]
    {:total (count operations)
     :running (count (filter #(= (:status %) :running) operations))
     :near-deadline (count (filter #(< (- (:deadline %) now) 5000) operations))
     :overdue (count (filter #(< (:deadline %) now) operations))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-timeout-handler!
  "Initialize timeout handler."
  []
  (log/info "Initializing timeout handler")
  ;; Register feature flag
  (flags/register-flag! "timeout-handler" "Enable timeout handler" true)
  ;; Create metrics
  (metrics/create-counter! :timeout/timeouts-registered "Timeouts registered")
  (metrics/create-counter! :timeout/operations-completed "Operations completed")
  (metrics/create-counter! :timeout/operations-timed-out "Operations timed out")
  (metrics/create-counter! :timeout/operations-cancelled "Operations cancelled")
  (metrics/create-counter! :timeout/operations-failed "Operations failed")
  (metrics/create-gauge! :timeout/active-operations "Active operations"
                         #(count (list-operations)))
  ;; Register default timeouts
  (register-timeout! :short {:timeout-ms 5000})
  (register-timeout! :medium {:timeout-ms 30000})
  (register-timeout! :long {:timeout-ms 120000})
  (register-timeout! :very-long {:timeout-ms 600000})
  (log/info "Timeout handler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-timeout-status []
  {:enabled (flags/is-enabled? "timeout-handler")
   :timeouts (count (:timeouts @timeout-state))
   :active-operations (count (:active-operations @timeout-state))
   :operation-stats (get-operation-stats)
   :executor-active (some? (:executor @timeout-state))
   :config (:config @timeout-state)})
