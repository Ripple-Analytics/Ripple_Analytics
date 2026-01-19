(ns mental-models.pipeline.integration.timeout-manager
  "Timeout manager for mental model analysis system.
   
   Features:
   - Operation timeouts
   - Timeout policies
   - Deadline propagation
   - Timeout inheritance
   - Cancellation support
   - Timeout metrics
   - Async timeout handling
   - Timeout budgets"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent TimeoutException Future TimeUnit
            ScheduledThreadPoolExecutor ScheduledFuture]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:policies {}         ;; policy-id -> timeout-policy
         :active-timeouts {}  ;; timeout-id -> timeout-info
         :scheduler (ScheduledThreadPoolExecutor. 2)
         :config {:default-timeout-ms 30000
                  :max-timeout-ms 300000
                  :min-timeout-ms 100}
         :stats {:timeouts-started 0
                 :timeouts-completed 0
                 :timeouts-expired 0
                 :timeouts-cancelled 0}
         :initialized? false}))

;; ============================================================================
;; Thread-Local Deadline
;; ============================================================================

(def ^:dynamic *deadline* nil)
(def ^:dynamic *timeout-budget* nil)

(defn get-deadline
  "Get the current deadline."
  []
  *deadline*)

(defn get-remaining-time
  "Get remaining time until deadline."
  []
  (when *deadline*
    (max 0 (- *deadline* (System/currentTimeMillis)))))

(defn has-time-remaining?
  "Check if there's time remaining."
  []
  (or (nil? *deadline*)
      (> (System/currentTimeMillis) *deadline*)))

;; ============================================================================
;; Timeout Policies
;; ============================================================================

(defn create-policy!
  "Create a timeout policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :timeout-ms (get config :timeout-ms
                                 (get-in @state [:config :default-timeout-ms]))
                :on-timeout (get config :on-timeout)
                :fallback-fn (get config :fallback-fn)
                :propagate? (get config :propagate? true)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created timeout policy" {:policy-id policy-id :timeout-ms (:timeout-ms policy)})
    policy-id))

(defn get-policy
  "Get a timeout policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all timeout policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :timeout-ms (:timeout-ms p)})
        (:policies @state)))

(defn update-policy!
  "Update a timeout policy."
  [policy-id updates]
  (swap! state update-in [:policies policy-id] merge updates))

;; ============================================================================
;; Timeout Execution
;; ============================================================================

(defn- create-timeout-info
  "Create timeout tracking info."
  [timeout-id timeout-ms]
  {:id timeout-id
   :timeout-ms timeout-ms
   :started-at (System/currentTimeMillis)
   :deadline (+ (System/currentTimeMillis) timeout-ms)
   :status (atom :running) ;; :running, :completed, :expired, :cancelled
   :future (atom nil)})

(defn- schedule-timeout-check
  "Schedule a timeout check."
  [timeout-info on-timeout]
  (let [scheduler (:scheduler @state)
        task (fn []
               (when (= :running @(:status timeout-info))
                 (reset! (:status timeout-info) :expired)
                 (swap! state update-in [:stats :timeouts-expired] inc)
                 (swap! state update :active-timeouts dissoc (:id timeout-info))
                 (logging/log :warn "Timeout expired" {:timeout-id (:id timeout-info)})
                 (events/emit! :timeout-expired {:timeout-id (:id timeout-info)})
                 (when on-timeout
                   (try (on-timeout) (catch Exception _)))))
        future (.schedule scheduler task (:timeout-ms timeout-info) TimeUnit/MILLISECONDS)]
    (reset! (:future timeout-info) future)))

(defn- complete-timeout!
  "Mark timeout as completed."
  [timeout-info]
  (when (compare-and-set! (:status timeout-info) :running :completed)
    (when-let [future @(:future timeout-info)]
      (.cancel future false))
    (swap! state update-in [:stats :timeouts-completed] inc)
    (swap! state update :active-timeouts dissoc (:id timeout-info))))

(defn- cancel-timeout!
  "Cancel a timeout."
  [timeout-id]
  (when-let [timeout-info (get-in @state [:active-timeouts timeout-id])]
    (when (compare-and-set! (:status timeout-info) :running :cancelled)
      (when-let [future @(:future timeout-info)]
        (.cancel future false))
      (swap! state update-in [:stats :timeouts-cancelled] inc)
      (swap! state update :active-timeouts dissoc timeout-id)
      true)))

(defn execute-with-timeout
  "Execute a function with a timeout."
  [timeout-ms f & {:keys [on-timeout fallback-fn]}]
  (let [timeout-id (str (UUID/randomUUID))
        timeout-info (create-timeout-info timeout-id timeout-ms)
        deadline (+ (System/currentTimeMillis) timeout-ms)]
    
    (swap! state assoc-in [:active-timeouts timeout-id] timeout-info)
    (swap! state update-in [:stats :timeouts-started] inc)
    (schedule-timeout-check timeout-info on-timeout)
    
    (binding [*deadline* (if *deadline* (min *deadline* deadline) deadline)]
      (try
        (let [result-chan (async/thread (f))
              timeout-chan (timeout timeout-ms)
              [result ch] (alts! [result-chan timeout-chan])]
          (if (= ch timeout-chan)
            ;; Timeout occurred
            (do
              (reset! (:status timeout-info) :expired)
              (swap! state update-in [:stats :timeouts-expired] inc)
              (swap! state update :active-timeouts dissoc timeout-id)
              (if fallback-fn
                (fallback-fn)
                (throw (TimeoutException. (str "Operation timed out after " timeout-ms "ms")))))
            ;; Completed in time
            (do
              (complete-timeout! timeout-info)
              result)))
        (catch Exception e
          (complete-timeout! timeout-info)
          (throw e))))))

(defmacro with-timeout
  "Execute body with a timeout."
  [timeout-ms & body]
  `(execute-with-timeout ~timeout-ms (fn [] ~@body)))

(defmacro with-timeout-policy
  "Execute body with a timeout policy."
  [policy-id & body]
  `(let [policy# (get-policy ~policy-id)]
     (if policy#
       (execute-with-timeout (:timeout-ms policy#) (fn [] ~@body)
                             :on-timeout (:on-timeout policy#)
                             :fallback-fn (:fallback-fn policy#))
       (do ~@body))))

;; ============================================================================
;; Timeout Budget
;; ============================================================================

(defn create-budget
  "Create a timeout budget."
  [total-ms]
  {:total-ms total-ms
   :started-at (System/currentTimeMillis)
   :spent-ms (atom 0)})

(defn spend-budget!
  "Spend time from the budget."
  [budget ms]
  (swap! (:spent-ms budget) + ms))

(defn get-remaining-budget
  "Get remaining budget."
  [budget]
  (max 0 (- (:total-ms budget) @(:spent-ms budget))))

(defn budget-exhausted?
  "Check if budget is exhausted."
  [budget]
  (<= (get-remaining-budget budget) 0))

(defmacro with-budget
  "Execute body with a timeout budget."
  [budget & body]
  `(let [start# (System/currentTimeMillis)]
     (binding [*timeout-budget* ~budget]
       (try
         ~@body
         (finally
           (spend-budget! ~budget (- (System/currentTimeMillis) start#)))))))

(defn allocate-from-budget
  "Allocate time from the current budget."
  [requested-ms]
  (if *timeout-budget*
    (min requested-ms (get-remaining-budget *timeout-budget*))
    requested-ms))

;; ============================================================================
;; Deadline Propagation
;; ============================================================================

(defn propagate-deadline-headers
  "Get headers for deadline propagation."
  []
  (when *deadline*
    {"X-Deadline" (str *deadline*)
     "X-Remaining-Time" (str (get-remaining-time))}))

(defn extract-deadline-from-headers
  "Extract deadline from request headers."
  [headers]
  (when-let [deadline-str (get headers "x-deadline")]
    (try
      (Long/parseLong deadline-str)
      (catch Exception _ nil))))

(defn deadline-middleware
  "Ring middleware for deadline propagation."
  [handler]
  (fn [request]
    (let [incoming-deadline (extract-deadline-from-headers (:headers request))
          effective-deadline (when incoming-deadline
                               (min incoming-deadline
                                    (+ (System/currentTimeMillis)
                                       (get-in @state [:config :max-timeout-ms]))))]
      (binding [*deadline* effective-deadline]
        (let [response (handler request)]
          (if *deadline*
            (update response :headers merge (propagate-deadline-headers))
            response))))))

;; ============================================================================
;; Async Timeout
;; ============================================================================

(defn timeout-channel
  "Create a timeout channel."
  [timeout-ms]
  (timeout timeout-ms))

(defn with-timeout-async
  "Execute an async operation with timeout."
  [timeout-ms ch]
  (go
    (let [timeout-ch (timeout timeout-ms)
          [result source] (alts! [ch timeout-ch])]
      (if (= source timeout-ch)
        {:status :timeout :timeout-ms timeout-ms}
        {:status :success :result result}))))

(defn race-with-timeout
  "Race multiple channels with a timeout."
  [timeout-ms & channels]
  (go
    (let [timeout-ch (timeout timeout-ms)
          all-channels (conj (vec channels) timeout-ch)
          [result source] (alts! all-channels)]
      (if (= source timeout-ch)
        {:status :timeout}
        {:status :success :result result :channel source}))))

;; ============================================================================
;; Timeout Utilities
;; ============================================================================

(defn check-deadline!
  "Check if deadline has passed and throw if so."
  []
  (when (and *deadline* (<= *deadline* (System/currentTimeMillis)))
    (throw (TimeoutException. "Deadline exceeded"))))

(defn ensure-time-remaining
  "Ensure there's at least the specified time remaining."
  [required-ms]
  (let [remaining (get-remaining-time)]
    (when (and remaining (< remaining required-ms))
      (throw (TimeoutException.
              (str "Insufficient time remaining. Required: " required-ms "ms, Available: " remaining "ms"))))))

(defn adjust-timeout-for-deadline
  "Adjust a timeout based on the current deadline."
  [requested-timeout-ms]
  (if-let [remaining (get-remaining-time)]
    (min requested-timeout-ms remaining)
    requested-timeout-ms))

;; ============================================================================
;; Active Timeout Management
;; ============================================================================

(defn list-active-timeouts
  "List all active timeouts."
  []
  (mapv (fn [[id info]]
          {:id id
           :timeout-ms (:timeout-ms info)
           :started-at (:started-at info)
           :deadline (:deadline info)
           :status @(:status info)
           :elapsed-ms (- (System/currentTimeMillis) (:started-at info))})
        (:active-timeouts @state)))

(defn cancel-all-timeouts!
  "Cancel all active timeouts."
  []
  (doseq [[id _] (:active-timeouts @state)]
    (cancel-timeout! id)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-timeout-stats
  "Get timeout statistics."
  []
  (let [stats (:stats @state)]
    {:active-timeouts (count (:active-timeouts @state))
     :policies-count (count (:policies @state))
     :timeouts-started (:timeouts-started stats)
     :timeouts-completed (:timeouts-completed stats)
     :timeouts-expired (:timeouts-expired stats)
     :timeouts-cancelled (:timeouts-cancelled stats)
     :completion-rate (if (pos? (:timeouts-started stats))
                        (/ (:timeouts-completed stats) (:timeouts-started stats))
                        1.0)}))

(defn reset-stats!
  "Reset timeout statistics."
  []
  (swap! state assoc :stats {:timeouts-started 0
                             :timeouts-completed 0
                             :timeouts-expired 0
                             :timeouts-cancelled 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-timeout-manager!
  "Initialize the timeout manager."
  []
  (when-not (:initialized? @state)
    ;; Create default policies
    (create-policy! :short
                    {:name "Short"
                     :timeout-ms 5000})
    
    (create-policy! :medium
                    {:name "Medium"
                     :timeout-ms 30000})
    
    (create-policy! :long
                    {:name "Long"
                     :timeout-ms 120000})
    
    (create-policy! :analysis
                    {:name "Analysis"
                     :timeout-ms 60000
                     :on-timeout (fn []
                                   (logging/log :warn "Analysis timeout"))})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Timeout manager initialized")
    (events/emit! :timeout-manager-initialized {})
    true))
