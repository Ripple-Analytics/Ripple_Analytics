(ns mental-models.pipeline.integration.retry-handler
  "Retry handler for mental model analysis system.
   
   Features:
   - Configurable retry strategies
   - Exponential backoff
   - Jitter support
   - Retry conditions
   - Max attempts
   - Retry budgets
   - Retry metrics
   - Async retry support"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:policies {}         ;; policy-id -> policy
         :budgets {}          ;; budget-id -> budget
         :config {:default-max-attempts 3
                  :default-initial-delay-ms 1000
                  :default-max-delay-ms 30000
                  :default-multiplier 2.0
                  :default-jitter-factor 0.1}
         :stats {:total-attempts 0
                 :successful-attempts 0
                 :failed-attempts 0
                 :retries 0
                 :exhausted 0}
         :initialized? false}))

(def ^:private random (Random.))

;; ============================================================================
;; Retry Policies
;; ============================================================================

(defn create-policy!
  "Create a retry policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :max-attempts (get config :max-attempts
                                   (get-in @state [:config :default-max-attempts]))
                :initial-delay-ms (get config :initial-delay-ms
                                       (get-in @state [:config :default-initial-delay-ms]))
                :max-delay-ms (get config :max-delay-ms
                                   (get-in @state [:config :default-max-delay-ms]))
                :multiplier (get config :multiplier
                                 (get-in @state [:config :default-multiplier]))
                :jitter-factor (get config :jitter-factor
                                    (get-in @state [:config :default-jitter-factor]))
                :retry-on (get config :retry-on (constantly true))
                :on-retry (get config :on-retry)
                :on-exhausted (get config :on-exhausted)
                :metrics {:attempts (atom 0)
                          :successes (atom 0)
                          :failures (atom 0)
                          :retries (atom 0)}
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created retry policy" {:policy-id policy-id})
    (events/emit! :retry-policy-created {:policy-id policy-id})
    policy-id))

(defn get-policy
  "Get a retry policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all retry policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :max-attempts (:max-attempts p)
           :initial-delay-ms (:initial-delay-ms p)})
        (:policies @state)))

(defn delete-policy!
  "Delete a retry policy."
  [policy-id]
  (swap! state update :policies dissoc policy-id)
  (logging/log :info "Deleted retry policy" {:policy-id policy-id}))

;; ============================================================================
;; Delay Calculation
;; ============================================================================

(defn- calculate-delay
  "Calculate the delay for a retry attempt."
  [policy attempt]
  (let [base-delay (* (:initial-delay-ms policy)
                      (Math/pow (:multiplier policy) (dec attempt)))
        capped-delay (min base-delay (:max-delay-ms policy))
        jitter-factor (:jitter-factor policy)
        jitter (* capped-delay jitter-factor (.nextDouble random))]
    (long (+ capped-delay jitter))))

(defn- calculate-delay-linear
  "Calculate linear delay for a retry attempt."
  [policy attempt]
  (let [delay (* (:initial-delay-ms policy) attempt)]
    (min delay (:max-delay-ms policy))))

(defn- calculate-delay-fixed
  "Calculate fixed delay for a retry attempt."
  [policy _attempt]
  (:initial-delay-ms policy))

;; ============================================================================
;; Retry Execution
;; ============================================================================

(defn- should-retry?
  "Check if we should retry based on the exception."
  [policy exception]
  (let [retry-on (:retry-on policy)]
    (cond
      (fn? retry-on) (retry-on exception)
      (set? retry-on) (some #(instance? % exception) retry-on)
      :else true)))

(defn execute-with-retry
  "Execute a function with retry logic."
  [policy-id f]
  (if-let [policy (get-policy policy-id)]
    (loop [attempt 1]
      (swap! (get-in policy [:metrics :attempts]) inc)
      (swap! state update-in [:stats :total-attempts] inc)
      
      (let [result (try
                     {:success? true :value (f)}
                     (catch Exception e
                       {:success? false :exception e}))]
        
        (if (:success? result)
          (do
            (swap! (get-in policy [:metrics :successes]) inc)
            (swap! state update-in [:stats :successful-attempts] inc)
            (:value result))
          
          (let [exception (:exception result)
                can-retry? (and (< attempt (:max-attempts policy))
                                (should-retry? policy exception))]
            (if can-retry?
              (let [delay-ms (calculate-delay policy attempt)]
                (swap! (get-in policy [:metrics :retries]) inc)
                (swap! state update-in [:stats :retries] inc)
                
                (logging/log :debug "Retrying" {:policy-id policy-id
                                                 :attempt attempt
                                                 :delay-ms delay-ms
                                                 :error (.getMessage exception)})
                
                ;; Call on-retry callback
                (when-let [on-retry (:on-retry policy)]
                  (try (on-retry attempt exception delay-ms) (catch Exception _)))
                
                (Thread/sleep delay-ms)
                (recur (inc attempt)))
              
              (do
                (swap! (get-in policy [:metrics :failures]) inc)
                (swap! state update-in [:stats :failed-attempts] inc)
                (swap! state update-in [:stats :exhausted] inc)
                
                (logging/log :warn "Retry exhausted" {:policy-id policy-id
                                                       :attempts attempt
                                                       :error (.getMessage exception)})
                
                ;; Call on-exhausted callback
                (when-let [on-exhausted (:on-exhausted policy)]
                  (try (on-exhausted attempt exception) (catch Exception _)))
                
                (throw exception)))))))
    (throw (ex-info "Retry policy not found" {:policy-id policy-id}))))

(defmacro with-retry
  "Execute body with retry logic."
  [policy-id & body]
  `(execute-with-retry ~policy-id (fn [] ~@body)))

;; ============================================================================
;; Async Retry
;; ============================================================================

(defn execute-with-retry-async
  "Execute a function with retry logic asynchronously."
  [policy-id f callback]
  (go
    (try
      (let [result (execute-with-retry policy-id f)]
        (callback {:status :success :result result}))
      (catch Exception e
        (callback {:status :error :error (.getMessage e) :exception e})))))

;; ============================================================================
;; Retry Budgets
;; ============================================================================

(defn create-budget!
  "Create a retry budget."
  [budget-id config]
  (let [budget {:id budget-id
                :name (get config :name (name budget-id))
                :max-retries-per-window (get config :max-retries-per-window 100)
                :window-ms (get config :window-ms 60000)
                :retries (atom [])
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:budgets budget-id] budget)
    (logging/log :info "Created retry budget" {:budget-id budget-id})
    budget-id))

(defn get-budget
  "Get a retry budget."
  [budget-id]
  (get-in @state [:budgets budget-id]))

(defn- cleanup-old-retries!
  "Remove retries outside the window."
  [budget]
  (let [now (System/currentTimeMillis)
        window-start (- now (:window-ms budget))]
    (swap! (:retries budget) (fn [retries]
                               (vec (filter #(> % window-start) retries))))))

(defn can-retry?
  "Check if a retry is allowed within the budget."
  [budget-id]
  (if-let [budget (get-budget budget-id)]
    (do
      (cleanup-old-retries! budget)
      (and @(:enabled? budget)
           (< (count @(:retries budget)) (:max-retries-per-window budget))))
    true))

(defn record-retry!
  "Record a retry against the budget."
  [budget-id]
  (when-let [budget (get-budget budget-id)]
    (swap! (:retries budget) conj (System/currentTimeMillis))))

(defn get-budget-usage
  "Get the current budget usage."
  [budget-id]
  (when-let [budget (get-budget budget-id)]
    (cleanup-old-retries! budget)
    {:budget-id budget-id
     :used (count @(:retries budget))
     :max (:max-retries-per-window budget)
     :remaining (- (:max-retries-per-window budget) (count @(:retries budget)))}))

;; ============================================================================
;; Retry with Budget
;; ============================================================================

(defn execute-with-retry-budget
  "Execute with retry, respecting a budget."
  [policy-id budget-id f]
  (if-let [policy (get-policy policy-id)]
    (loop [attempt 1]
      (swap! (get-in policy [:metrics :attempts]) inc)
      
      (let [result (try
                     {:success? true :value (f)}
                     (catch Exception e
                       {:success? false :exception e}))]
        
        (if (:success? result)
          (do
            (swap! (get-in policy [:metrics :successes]) inc)
            (:value result))
          
          (let [exception (:exception result)
                can-retry? (and (< attempt (:max-attempts policy))
                                (should-retry? policy exception)
                                (can-retry? budget-id))]
            (if can-retry?
              (let [delay-ms (calculate-delay policy attempt)]
                (record-retry! budget-id)
                (swap! (get-in policy [:metrics :retries]) inc)
                (Thread/sleep delay-ms)
                (recur (inc attempt)))
              
              (do
                (swap! (get-in policy [:metrics :failures]) inc)
                (throw exception)))))))
    (throw (ex-info "Retry policy not found" {:policy-id policy-id}))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-policy-metrics
  "Get metrics for a retry policy."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    {:policy-id policy-id
     :name (:name policy)
     :attempts @(get-in policy [:metrics :attempts])
     :successes @(get-in policy [:metrics :successes])
     :failures @(get-in policy [:metrics :failures])
     :retries @(get-in policy [:metrics :retries])
     :success-rate (let [attempts @(get-in policy [:metrics :attempts])]
                     (if (pos? attempts)
                       (/ @(get-in policy [:metrics :successes]) attempts)
                       1.0))}))

(defn get-all-policy-metrics
  "Get metrics for all retry policies."
  []
  (mapv (fn [[id _]] (get-policy-metrics id)) (:policies @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-retry-stats
  "Get retry statistics."
  []
  (let [stats (:stats @state)]
    {:policies-count (count (:policies @state))
     :budgets-count (count (:budgets @state))
     :total-attempts (:total-attempts stats)
     :successful-attempts (:successful-attempts stats)
     :failed-attempts (:failed-attempts stats)
     :retries (:retries stats)
     :exhausted (:exhausted stats)
     :success-rate (if (pos? (:total-attempts stats))
                     (/ (:successful-attempts stats) (:total-attempts stats))
                     1.0)}))

(defn reset-stats!
  "Reset retry statistics."
  []
  (swap! state assoc :stats {:total-attempts 0
                             :successful-attempts 0
                             :failed-attempts 0
                             :retries 0
                             :exhausted 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-retry-handler!
  "Initialize the retry handler."
  []
  (when-not (:initialized? @state)
    ;; Create default policies
    (create-policy! :default
                    {:name "Default"
                     :max-attempts 3
                     :initial-delay-ms 1000
                     :multiplier 2.0})
    
    (create-policy! :aggressive
                    {:name "Aggressive"
                     :max-attempts 5
                     :initial-delay-ms 500
                     :multiplier 1.5
                     :max-delay-ms 10000})
    
    (create-policy! :conservative
                    {:name "Conservative"
                     :max-attempts 2
                     :initial-delay-ms 2000
                     :multiplier 3.0
                     :max-delay-ms 60000})
    
    ;; Create default budget
    (create-budget! :default
                    {:name "Default"
                     :max-retries-per-window 100
                     :window-ms 60000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Retry handler initialized")
    (events/emit! :retry-handler-initialized {})
    true))
