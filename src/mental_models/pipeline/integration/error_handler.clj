(ns mental-models.pipeline.integration.error-handler
  "Error Handler Module
   
   Centralized error handling and recovery:
   - Error classification and categorization
   - Error recovery strategies
   - Error reporting and notification
   - Error aggregation and analysis
   - Circuit breaker integration"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; ERROR HANDLER STATE
;; =============================================================================

(defonce error-state (atom {:handlers (ConcurrentHashMap.)
                           :recovery-strategies (ConcurrentHashMap.)
                           :error-log []
                           :error-counts (ConcurrentHashMap.)
                           :suppressed-errors #{}
                           :config {:max-log-entries 10000
                                    :error-rate-window-ms 60000
                                    :error-rate-threshold 100
                                    :enable-notifications true}}))

;; =============================================================================
;; ERROR CLASSIFICATION
;; =============================================================================

(def error-categories
  {:transient #{:timeout :connection-refused :service-unavailable :rate-limited}
   :permanent #{:not-found :unauthorized :forbidden :bad-request :validation-error}
   :system #{:out-of-memory :disk-full :internal-error :configuration-error}
   :external #{:third-party-error :api-error :network-error}})

(defn classify-error
  "Classify an error into a category."
  [error]
  (let [error-type (cond
                     (instance? java.net.SocketTimeoutException error) :timeout
                     (instance? java.net.ConnectException error) :connection-refused
                     (instance? java.io.IOException error) :network-error
                     (instance? OutOfMemoryError error) :out-of-memory
                     (instance? IllegalArgumentException error) :validation-error
                     (instance? SecurityException error) :unauthorized
                     :else :unknown)
        category (or (first (for [[cat types] error-categories
                                  :when (contains? types error-type)]
                              cat))
                     :unknown)]
    {:type error-type
     :category category
     :recoverable (contains? (:transient error-categories) error-type)}))

(defn create-error-info
  "Create detailed error information."
  [error & {:keys [context operation component]}]
  (let [classification (classify-error error)]
    {:id (str (java.util.UUID/randomUUID))
     :timestamp (System/currentTimeMillis)
     :error-type (:type classification)
     :category (:category classification)
     :recoverable (:recoverable classification)
     :message (if (instance? Throwable error) (.getMessage ^Throwable error) (str error))
     :class (when (instance? Throwable error) (.getName (class error)))
     :stack-trace (when (instance? Throwable error)
                    (mapv str (.getStackTrace ^Throwable error)))
     :cause (when (and (instance? Throwable error) (.getCause ^Throwable error))
              (.getMessage (.getCause ^Throwable error)))
     :context context
     :operation operation
     :component component}))

;; =============================================================================
;; ERROR HANDLERS
;; =============================================================================

(defn register-handler!
  "Register an error handler for a specific error type or category."
  [handler-id {:keys [error-type category handler-fn priority]}]
  (log/info "Registering error handler" {:id handler-id})
  (.put ^ConcurrentHashMap (:handlers @error-state) handler-id
        {:id handler-id
         :error-type error-type
         :category category
         :handler-fn handler-fn
         :priority (or priority 0)})
  (metrics/inc-counter! :errorhandler/handlers-registered))

(defn unregister-handler!
  "Unregister an error handler."
  [handler-id]
  (log/info "Unregistering error handler" {:id handler-id})
  (.remove ^ConcurrentHashMap (:handlers @error-state) handler-id))

(defn get-handler
  "Get an error handler by ID."
  [handler-id]
  (.get ^ConcurrentHashMap (:handlers @error-state) handler-id))

(defn list-handlers
  "List all registered handlers."
  []
  (vec (vals (:handlers @error-state))))

(defn find-handlers
  "Find handlers that match an error."
  [error-info]
  (let [handlers (vals (:handlers @error-state))]
    (->> handlers
         (filter (fn [h]
                   (or (= (:error-type h) (:error-type error-info))
                       (= (:category h) (:category error-info))
                       (and (nil? (:error-type h)) (nil? (:category h))))))
         (sort-by :priority >))))

;; =============================================================================
;; RECOVERY STRATEGIES
;; =============================================================================

(defn register-recovery-strategy!
  "Register a recovery strategy."
  [strategy-id {:keys [error-type category strategy-fn max-attempts delay-ms]}]
  (log/info "Registering recovery strategy" {:id strategy-id})
  (.put ^ConcurrentHashMap (:recovery-strategies @error-state) strategy-id
        {:id strategy-id
         :error-type error-type
         :category category
         :strategy-fn strategy-fn
         :max-attempts (or max-attempts 3)
         :delay-ms (or delay-ms 1000)}))

(defn unregister-recovery-strategy!
  "Unregister a recovery strategy."
  [strategy-id]
  (.remove ^ConcurrentHashMap (:recovery-strategies @error-state) strategy-id))

(defn get-recovery-strategy
  "Get a recovery strategy by ID."
  [strategy-id]
  (.get ^ConcurrentHashMap (:recovery-strategies @error-state) strategy-id))

(defn find-recovery-strategy
  "Find a recovery strategy for an error."
  [error-info]
  (let [strategies (vals (:recovery-strategies @error-state))]
    (first (filter (fn [s]
                     (or (= (:error-type s) (:error-type error-info))
                         (= (:category s) (:category error-info))))
                   strategies))))

(defn attempt-recovery
  "Attempt to recover from an error."
  [error-info retry-fn]
  (if-let [strategy (find-recovery-strategy error-info)]
    (let [{:keys [strategy-fn max-attempts delay-ms]} strategy]
      (loop [attempt 1]
        (if (> attempt max-attempts)
          {:recovered false
           :attempts attempt
           :error error-info}
          (do
            (when (> attempt 1)
              (Thread/sleep delay-ms))
            (let [result (try
                           {:success true :result (strategy-fn error-info retry-fn)}
                           (catch Exception e
                             {:success false :error e}))]
              (if (:success result)
                (do
                  (metrics/inc-counter! :errorhandler/recoveries-successful)
                  {:recovered true
                   :attempts attempt
                   :result (:result result)})
                (recur (inc attempt))))))))
    {:recovered false
     :reason :no-strategy
     :error error-info}))

;; =============================================================================
;; ERROR LOGGING
;; =============================================================================

(defn log-error!
  "Log an error."
  [error-info]
  (let [max-entries (get-in @error-state [:config :max-log-entries])]
    (swap! error-state update :error-log
           (fn [log]
             (let [new-log (conj log error-info)]
               (if (> (count new-log) max-entries)
                 (vec (drop (- (count new-log) max-entries) new-log))
                 new-log)))))
  ;; Update error counts
  (let [error-type (:error-type error-info)
        counts (:error-counts @error-state)]
    (if-let [counter (.get ^ConcurrentHashMap counts error-type)]
      (.incrementAndGet ^AtomicLong counter)
      (.put ^ConcurrentHashMap counts error-type (AtomicLong. 1)))))

(defn get-error-log
  "Get the error log."
  [& {:keys [error-type category component limit since]}]
  (let [log (:error-log @error-state)]
    (cond->> log
      error-type (filter #(= (:error-type %) error-type))
      category (filter #(= (:category %) category))
      component (filter #(= (:component %) component))
      since (filter #(>= (:timestamp %) since))
      limit (take-last limit))))

(defn clear-error-log!
  "Clear the error log."
  []
  (swap! error-state assoc :error-log []))

(defn get-error-counts
  "Get error counts by type."
  []
  (into {} (for [[k v] (:error-counts @error-state)]
             [k (.get ^AtomicLong v)])))

(defn reset-error-counts!
  "Reset error counts."
  []
  (doseq [[_ v] (:error-counts @error-state)]
    (.set ^AtomicLong v 0)))

;; =============================================================================
;; ERROR SUPPRESSION
;; =============================================================================

(defn suppress-error!
  "Suppress an error type from logging/notification."
  [error-type]
  (log/info "Suppressing error type" {:type error-type})
  (swap! error-state update :suppressed-errors conj error-type))

(defn unsuppress-error!
  "Unsuppress an error type."
  [error-type]
  (log/info "Unsuppressing error type" {:type error-type})
  (swap! error-state update :suppressed-errors disj error-type))

(defn is-suppressed?
  "Check if an error type is suppressed."
  [error-type]
  (contains? (:suppressed-errors @error-state) error-type))

;; =============================================================================
;; ERROR HANDLING
;; =============================================================================

(defn handle-error!
  "Handle an error through the error handling pipeline."
  [error & {:keys [context operation component notify recover retry-fn]}]
  (let [error-info (create-error-info error
                                      :context context
                                      :operation operation
                                      :component component)]
    ;; Log the error
    (when-not (is-suppressed? (:error-type error-info))
      (log-error! error-info)
      (log/error "Error occurred" {:error-type (:error-type error-info)
                                   :category (:category error-info)
                                   :message (:message error-info)
                                   :component component
                                   :operation operation}))
    ;; Update metrics
    (metrics/inc-counter! :errorhandler/errors-handled)
    (metrics/inc-counter! (keyword "errorhandler" (str "errors-" (name (:category error-info)))))
    ;; Publish event
    (events/publish! :errorhandler/error-occurred error-info)
    ;; Find and execute handlers
    (let [handlers (find-handlers error-info)]
      (doseq [handler handlers]
        (try
          ((:handler-fn handler) error-info)
          (catch Exception e
            (log/warn "Error handler failed" {:handler (:id handler) :error (.getMessage e)})))))
    ;; Attempt recovery if requested
    (let [recovery-result (when (and recover (:recoverable error-info) retry-fn)
                            (attempt-recovery error-info retry-fn))]
      ;; Notify if enabled and not recovered
      (when (and notify
                 (get-in @error-state [:config :enable-notifications])
                 (not (:recovered recovery-result)))
        (events/publish! :errorhandler/error-notification {:error error-info
                                                           :recovery-attempted (some? recovery-result)
                                                           :recovery-result recovery-result}))
      ;; Return result
      {:error-info error-info
       :handlers-executed (count handlers)
       :recovery-result recovery-result})))

(defmacro with-error-handling
  "Execute body with error handling."
  [opts & body]
  `(try
     ~@body
     (catch Exception e#
       (handle-error! e# ~@(mapcat identity opts)))))

(defmacro with-retry
  "Execute body with retry on error."
  [{:keys [max-attempts delay-ms on-error]} & body]
  `(loop [attempt# 1]
     (let [result# (try
                     {:success true :value (do ~@body)}
                     (catch Exception e#
                       {:success false :error e#}))]
       (if (:success result#)
         (:value result#)
         (if (>= attempt# ~(or max-attempts 3))
           (do
             (when ~on-error
               (~on-error (:error result#)))
             (throw (:error result#)))
           (do
             (Thread/sleep ~(or delay-ms 1000))
             (recur (inc attempt#))))))))

;; =============================================================================
;; ERROR RATE MONITORING
;; =============================================================================

(defn get-error-rate
  "Get the error rate within a time window."
  [& {:keys [window-ms error-type]}]
  (let [window (or window-ms (get-in @error-state [:config :error-rate-window-ms]))
        since (- (System/currentTimeMillis) window)
        errors (get-error-log :since since :error-type error-type)]
    {:count (count errors)
     :window-ms window
     :rate-per-second (double (/ (count errors) (/ window 1000)))}))

(defn check-error-rate!
  "Check if error rate exceeds threshold."
  []
  (let [threshold (get-in @error-state [:config :error-rate-threshold])
        rate-info (get-error-rate)]
    (when (> (:count rate-info) threshold)
      (log/warn "Error rate threshold exceeded" rate-info)
      (events/publish! :errorhandler/error-rate-exceeded rate-info)
      true)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-error-stats
  "Get error statistics."
  []
  (let [log (:error-log @error-state)
        by-type (group-by :error-type log)
        by-category (group-by :category log)]
    {:total-errors (count log)
     :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :by-category (into {} (map (fn [[k v]] [k (count v)]) by-category))
     :recoverable-count (count (filter :recoverable log))
     :handlers-registered (.size ^ConcurrentHashMap (:handlers @error-state))
     :recovery-strategies (.size ^ConcurrentHashMap (:recovery-strategies @error-state))
     :suppressed-types (count (:suppressed-errors @error-state))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-error-handler!
  "Initialize error handler."
  []
  (log/info "Initializing error handler")
  ;; Register feature flag
  (flags/register-flag! "error-handler" "Enable error handler" true)
  ;; Create metrics
  (metrics/create-counter! :errorhandler/errors-handled "Errors handled")
  (metrics/create-counter! :errorhandler/handlers-registered "Handlers registered")
  (metrics/create-counter! :errorhandler/recoveries-successful "Successful recoveries")
  (metrics/create-counter! :errorhandler/errors-transient "Transient errors")
  (metrics/create-counter! :errorhandler/errors-permanent "Permanent errors")
  (metrics/create-counter! :errorhandler/errors-system "System errors")
  (metrics/create-counter! :errorhandler/errors-external "External errors")
  (metrics/create-counter! :errorhandler/errors-unknown "Unknown errors")
  (metrics/create-gauge! :errorhandler/error-log-size "Error log size"
                         #(count (:error-log @error-state)))
  ;; Register default recovery strategies
  (register-recovery-strategy! :retry-transient
                               {:category :transient
                                :strategy-fn (fn [_ retry-fn] (retry-fn))
                                :max-attempts 3
                                :delay-ms 1000})
  (log/info "Error handler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-error-handler-status []
  {:enabled (flags/is-enabled? "error-handler")
   :handlers (.size ^ConcurrentHashMap (:handlers @error-state))
   :recovery-strategies (.size ^ConcurrentHashMap (:recovery-strategies @error-state))
   :error-log-size (count (:error-log @error-state))
   :suppressed-errors (count (:suppressed-errors @error-state))
   :error-counts (get-error-counts)
   :stats (get-error-stats)
   :config (:config @error-state)})
