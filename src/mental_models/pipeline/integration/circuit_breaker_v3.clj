(ns mental-models.pipeline.integration.circuit-breaker-v3
  "Advanced circuit breaker for mental model analysis system.
   
   Features:
   - Half-open state with gradual recovery
   - Sliding window failure tracking
   - Adaptive thresholds
   - Circuit breaker groups
   - Fallback strategies
   - Health-based opening
   - Circuit metrics
   - Event notifications"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent ConcurrentLinkedDeque]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:breakers {}         ;; breaker-id -> breaker
         :groups {}           ;; group-id -> [breaker-ids]
         :config {:default-failure-threshold 5
                  :default-success-threshold 3
                  :default-timeout-ms 30000
                  :default-window-size 10
                  :default-half-open-requests 3}
         :stats {:total-calls 0
                 :successful-calls 0
                 :failed-calls 0
                 :rejected-calls 0
                 :state-changes 0}
         :initialized? false}))

;; ============================================================================
;; Sliding Window
;; ============================================================================

(defn- create-sliding-window
  "Create a sliding window for tracking results."
  [size]
  {:window (ConcurrentLinkedDeque.)
   :size size})

(defn- add-to-window!
  "Add a result to the sliding window."
  [window result]
  (let [deque (:window window)]
    (.addLast deque {:result result :timestamp (System/currentTimeMillis)})
    (while (> (.size deque) (:size window))
      (.removeFirst deque))))

(defn- get-window-stats
  "Get statistics from the sliding window."
  [window]
  (let [results (vec (.toArray (:window window)))
        total (count results)
        failures (count (filter #(= :failure (:result %)) results))
        successes (count (filter #(= :success (:result %)) results))]
    {:total total
     :failures failures
     :successes successes
     :failure-rate (if (pos? total) (/ failures total) 0)}))

;; ============================================================================
;; Circuit Breaker Creation
;; ============================================================================

(defn create-breaker!
  "Create a circuit breaker."
  [breaker-id config]
  (let [breaker {:id breaker-id
                 :name (get config :name (name breaker-id))
                 :state (atom :closed)
                 :failure-threshold (get config :failure-threshold
                                         (get-in @state [:config :default-failure-threshold]))
                 :success-threshold (get config :success-threshold
                                         (get-in @state [:config :default-success-threshold]))
                 :timeout-ms (get config :timeout-ms
                                  (get-in @state [:config :default-timeout-ms]))
                 :window-size (get config :window-size
                                   (get-in @state [:config :default-window-size]))
                 :half-open-requests (get config :half-open-requests
                                          (get-in @state [:config :default-half-open-requests]))
                 :window (create-sliding-window (get config :window-size
                                                     (get-in @state [:config :default-window-size])))
                 :half-open-successes (atom 0)
                 :half-open-count (atom 0)
                 :last-failure-time (atom nil)
                 :last-state-change (atom (System/currentTimeMillis))
                 :fallback-fn (get config :fallback-fn)
                 :on-state-change (get config :on-state-change)
                 :metrics {:calls (atom 0)
                           :successes (atom 0)
                           :failures (atom 0)
                           :rejections (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:breakers breaker-id] breaker)
    (logging/log :info "Created circuit breaker" {:breaker-id breaker-id})
    (events/emit! :circuit-breaker-created {:breaker-id breaker-id})
    breaker-id))

(defn get-breaker
  "Get a circuit breaker."
  [breaker-id]
  (get-in @state [:breakers breaker-id]))

(defn list-breakers
  "List all circuit breakers."
  []
  (mapv (fn [[id b]]
          {:id id
           :name (:name b)
           :state @(:state b)
           :calls @(get-in b [:metrics :calls])
           :failures @(get-in b [:metrics :failures])})
        (:breakers @state)))

(defn delete-breaker!
  "Delete a circuit breaker."
  [breaker-id]
  (swap! state update :breakers dissoc breaker-id)
  (logging/log :info "Deleted circuit breaker" {:breaker-id breaker-id}))

;; ============================================================================
;; State Transitions
;; ============================================================================

(defn- change-state!
  "Change the circuit breaker state."
  [breaker new-state]
  (let [old-state @(:state breaker)]
    (when (not= old-state new-state)
      (reset! (:state breaker) new-state)
      (reset! (:last-state-change breaker) (System/currentTimeMillis))
      (swap! state update-in [:stats :state-changes] inc)
      
      ;; Reset half-open counters
      (when (= new-state :half-open)
        (reset! (:half-open-successes breaker) 0)
        (reset! (:half-open-count breaker) 0))
      
      (logging/log :info "Circuit breaker state changed" {:breaker-id (:id breaker)
                                                           :old-state old-state
                                                           :new-state new-state})
      (events/emit! :circuit-breaker-state-changed {:breaker-id (:id breaker)
                                                     :old-state old-state
                                                     :new-state new-state})
      
      ;; Call state change callback
      (when-let [callback (:on-state-change breaker)]
        (try (callback old-state new-state) (catch Exception _))))))

(defn- should-open?
  "Check if the circuit should open."
  [breaker]
  (let [stats (get-window-stats (:window breaker))]
    (and (>= (:total stats) (:window-size breaker))
         (>= (:failures stats) (:failure-threshold breaker)))))

(defn- should-close?
  "Check if the circuit should close."
  [breaker]
  (>= @(:half-open-successes breaker) (:success-threshold breaker)))

(defn- timeout-elapsed?
  "Check if the timeout has elapsed since last failure."
  [breaker]
  (when-let [last-failure @(:last-failure-time breaker)]
    (> (- (System/currentTimeMillis) last-failure) (:timeout-ms breaker))))

;; ============================================================================
;; Circuit Breaker Execution
;; ============================================================================

(defn- record-success!
  "Record a successful call."
  [breaker]
  (add-to-window! (:window breaker) :success)
  (swap! (get-in breaker [:metrics :successes]) inc)
  (swap! state update-in [:stats :successful-calls] inc)
  
  (case @(:state breaker)
    :half-open
    (do
      (swap! (:half-open-successes breaker) inc)
      (when (should-close? breaker)
        (change-state! breaker :closed)))
    nil))

(defn- record-failure!
  "Record a failed call."
  [breaker]
  (add-to-window! (:window breaker) :failure)
  (swap! (get-in breaker [:metrics :failures]) inc)
  (swap! state update-in [:stats :failed-calls] inc)
  (reset! (:last-failure-time breaker) (System/currentTimeMillis))
  
  (case @(:state breaker)
    :closed
    (when (should-open? breaker)
      (change-state! breaker :open))
    
    :half-open
    (change-state! breaker :open)
    
    nil))

(defn- can-execute?
  "Check if execution is allowed."
  [breaker]
  (case @(:state breaker)
    :closed true
    
    :open
    (if (timeout-elapsed? breaker)
      (do
        (change-state! breaker :half-open)
        true)
      false)
    
    :half-open
    (< @(:half-open-count breaker) (:half-open-requests breaker))))

(defn execute
  "Execute a function through the circuit breaker."
  [breaker-id f]
  (swap! state update-in [:stats :total-calls] inc)
  
  (if-let [breaker (get-breaker breaker-id)]
    (do
      (swap! (get-in breaker [:metrics :calls]) inc)
      
      (if (can-execute? breaker)
        (do
          (when (= :half-open @(:state breaker))
            (swap! (:half-open-count breaker) inc))
          
          (try
            (let [result (f)]
              (record-success! breaker)
              result)
            (catch Exception e
              (record-failure! breaker)
              (if-let [fallback (:fallback-fn breaker)]
                (fallback e)
                (throw e)))))
        
        (do
          (swap! (get-in breaker [:metrics :rejections]) inc)
          (swap! state update-in [:stats :rejected-calls] inc)
          (if-let [fallback (:fallback-fn breaker)]
            (fallback (ex-info "Circuit breaker open" {:breaker-id breaker-id}))
            (throw (ex-info "Circuit breaker open" {:breaker-id breaker-id
                                                     :state @(:state breaker)}))))))
    (throw (ex-info "Circuit breaker not found" {:breaker-id breaker-id}))))

(defmacro with-circuit-breaker
  "Execute body with circuit breaker protection."
  [breaker-id & body]
  `(execute ~breaker-id (fn [] ~@body)))

;; ============================================================================
;; Circuit Breaker Groups
;; ============================================================================

(defn create-group!
  "Create a circuit breaker group."
  [group-id breaker-ids]
  (swap! state assoc-in [:groups group-id] (vec breaker-ids))
  (logging/log :info "Created circuit breaker group" {:group-id group-id :breakers breaker-ids}))

(defn get-group-state
  "Get the state of a circuit breaker group."
  [group-id]
  (when-let [breaker-ids (get-in @state [:groups group-id])]
    (let [states (map (fn [id]
                        (when-let [b (get-breaker id)]
                          @(:state b)))
                      breaker-ids)]
      (cond
        (every? #(= :closed %) states) :closed
        (every? #(= :open %) states) :open
        :else :partial))))

(defn open-group!
  "Open all circuit breakers in a group."
  [group-id]
  (when-let [breaker-ids (get-in @state [:groups group-id])]
    (doseq [id breaker-ids]
      (when-let [breaker (get-breaker id)]
        (change-state! breaker :open)))))

(defn close-group!
  "Close all circuit breakers in a group."
  [group-id]
  (when-let [breaker-ids (get-in @state [:groups group-id])]
    (doseq [id breaker-ids]
      (when-let [breaker (get-breaker id)]
        (change-state! breaker :closed)))))

;; ============================================================================
;; Manual Control
;; ============================================================================

(defn force-open!
  "Force a circuit breaker to open."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (change-state! breaker :open)))

(defn force-close!
  "Force a circuit breaker to close."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (change-state! breaker :closed)))

(defn reset-breaker!
  "Reset a circuit breaker to initial state."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (.clear (:window (:window breaker)))
    (reset! (:half-open-successes breaker) 0)
    (reset! (:half-open-count breaker) 0)
    (reset! (:last-failure-time breaker) nil)
    (change-state! breaker :closed)
    (logging/log :info "Reset circuit breaker" {:breaker-id breaker-id})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-breaker-metrics
  "Get metrics for a circuit breaker."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [window-stats (get-window-stats (:window breaker))]
      {:breaker-id breaker-id
       :name (:name breaker)
       :state @(:state breaker)
       :calls @(get-in breaker [:metrics :calls])
       :successes @(get-in breaker [:metrics :successes])
       :failures @(get-in breaker [:metrics :failures])
       :rejections @(get-in breaker [:metrics :rejections])
       :window-stats window-stats
       :last-state-change @(:last-state-change breaker)})))

(defn get-all-breaker-metrics
  "Get metrics for all circuit breakers."
  []
  (mapv (fn [[id _]] (get-breaker-metrics id)) (:breakers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-circuit-breaker-stats
  "Get circuit breaker statistics."
  []
  (let [stats (:stats @state)
        breakers (vals (:breakers @state))]
    {:breakers-count (count breakers)
     :groups-count (count (:groups @state))
     :open-count (count (filter #(= :open @(:state %)) breakers))
     :closed-count (count (filter #(= :closed @(:state %)) breakers))
     :half-open-count (count (filter #(= :half-open @(:state %)) breakers))
     :total-calls (:total-calls stats)
     :successful-calls (:successful-calls stats)
     :failed-calls (:failed-calls stats)
     :rejected-calls (:rejected-calls stats)
     :state-changes (:state-changes stats)}))

(defn reset-stats!
  "Reset circuit breaker statistics."
  []
  (swap! state assoc :stats {:total-calls 0
                             :successful-calls 0
                             :failed-calls 0
                             :rejected-calls 0
                             :state-changes 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-circuit-breaker-v3!
  "Initialize the circuit breaker system."
  []
  (when-not (:initialized? @state)
    ;; Create default breakers
    (create-breaker! :lm-studio
                     {:name "LM Studio"
                      :failure-threshold 3
                      :success-threshold 2
                      :timeout-ms 30000
                      :window-size 10})
    
    (create-breaker! :database
                     {:name "Database"
                      :failure-threshold 5
                      :success-threshold 3
                      :timeout-ms 60000
                      :window-size 20})
    
    (create-breaker! :external-api
                     {:name "External API"
                      :failure-threshold 3
                      :success-threshold 2
                      :timeout-ms 15000
                      :window-size 10})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Circuit breaker v3 initialized")
    (events/emit! :circuit-breaker-v3-initialized {})
    true))
