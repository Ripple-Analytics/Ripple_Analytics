(ns mental-models.pipeline.integration.circuit-breaker-registry
  "Circuit breaker registry for mental model analysis system.
   
   Features:
   - Centralized circuit breaker management
   - Named circuit breakers
   - Configuration profiles
   - State monitoring
   - Bulk operations
   - Event notifications
   - Dashboard integration
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
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:breakers {}         ;; breaker-id -> circuit-breaker
         :profiles {}         ;; profile-id -> profile-config
         :groups {}           ;; group-id -> #{breaker-ids}
         :config {:default-profile :standard
                  :monitor-interval-ms 5000}
         :stats {:total-calls 0 :failures 0 :opens 0 :half-opens 0}
         :initialized? false}))

;; ============================================================================
;; Configuration Profiles
;; ============================================================================

(defn create-profile!
  "Create a circuit breaker configuration profile."
  [profile-id config]
  (let [profile {:id profile-id
                 :name (get config :name (name profile-id))
                 :failure-threshold (get config :failure-threshold 5)
                 :success-threshold (get config :success-threshold 3)
                 :timeout-ms (get config :timeout-ms 30000)
                 :half-open-timeout-ms (get config :half-open-timeout-ms 60000)
                 :failure-rate-threshold (get config :failure-rate-threshold 0.5)
                 :slow-call-threshold-ms (get config :slow-call-threshold-ms 5000)
                 :slow-call-rate-threshold (get config :slow-call-rate-threshold 0.5)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:profiles profile-id] profile)
    (logging/log :info "Created circuit breaker profile" {:profile-id profile-id})
    profile-id))

(defn get-profile
  "Get a configuration profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn list-profiles
  "List all configuration profiles."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :failure-threshold (:failure-threshold p)})
        (:profiles @state)))

;; ============================================================================
;; Circuit Breaker State
;; ============================================================================

(defn- create-breaker-state
  "Create initial circuit breaker state."
  [profile]
  {:state :closed ;; :closed, :open, :half-open
   :failure-count (atom 0)
   :success-count (atom 0)
   :total-calls (atom 0)
   :slow-calls (atom 0)
   :last-failure-time nil
   :opened-at nil
   :half-opened-at nil
   :last-state-change (System/currentTimeMillis)})

;; ============================================================================
;; Circuit Breaker Registration
;; ============================================================================

(defn register-breaker!
  "Register a circuit breaker."
  [breaker-id config]
  (let [profile-id (get config :profile (get-in @state [:config :default-profile]))
        profile (or (get-profile profile-id) (get-profile :standard))
        breaker {:id breaker-id
                 :name (get config :name (name breaker-id))
                 :profile-id profile-id
                 :profile profile
                 :group (get config :group)
                 :fallback-fn (get config :fallback-fn)
                 :on-open (get config :on-open)
                 :on-close (get config :on-close)
                 :on-half-open (get config :on-half-open)
                 :state (create-breaker-state profile)
                 :enabled? (get config :enabled? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:breakers breaker-id] breaker)
    
    ;; Add to group if specified
    (when-let [group (:group breaker)]
      (swap! state update-in [:groups group] (fnil conj #{}) breaker-id))
    
    (logging/log :info "Registered circuit breaker" {:breaker-id breaker-id :profile-id profile-id})
    breaker-id))

(defn unregister-breaker!
  "Unregister a circuit breaker."
  [breaker-id]
  (when-let [breaker (get-in @state [:breakers breaker-id])]
    ;; Remove from group
    (when-let [group (:group breaker)]
      (swap! state update-in [:groups group] disj breaker-id))
    (swap! state update :breakers dissoc breaker-id)))

(defn get-breaker
  "Get a circuit breaker."
  [breaker-id]
  (get-in @state [:breakers breaker-id]))

(defn list-breakers
  "List all circuit breakers."
  [& {:keys [group state-filter]}]
  (let [breakers (vals (:breakers @state))
        filtered (cond->> breakers
                   group (filter #(= (:group %) group))
                   state-filter (filter #(= (get-in % [:state :state]) state-filter)))]
    (mapv (fn [b]
            {:id (:id b)
             :name (:name b)
             :group (:group b)
             :state (get-in b [:state :state])
             :enabled? (:enabled? b)})
          filtered)))

;; ============================================================================
;; State Transitions
;; ============================================================================

(defn- transition-to-open!
  "Transition circuit breaker to open state."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [now (System/currentTimeMillis)]
      (swap! state update-in [:breakers breaker-id :state]
             assoc :state :open :opened-at now :last-state-change now)
      (swap! state update-in [:stats :opens] inc)
      
      ;; Call on-open callback
      (when-let [on-open (:on-open breaker)]
        (try (on-open) (catch Exception _)))
      
      (logging/log :warn "Circuit breaker opened" {:breaker-id breaker-id})
      (events/emit! :circuit-breaker-opened {:breaker-id breaker-id}))))

(defn- transition-to-half-open!
  "Transition circuit breaker to half-open state."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [now (System/currentTimeMillis)]
      (swap! state update-in [:breakers breaker-id :state]
             assoc :state :half-open :half-opened-at now :last-state-change now)
      (reset! (get-in @state [:breakers breaker-id :state :success-count]) 0)
      (swap! state update-in [:stats :half-opens] inc)
      
      ;; Call on-half-open callback
      (when-let [on-half-open (:on-half-open breaker)]
        (try (on-half-open) (catch Exception _)))
      
      (logging/log :info "Circuit breaker half-opened" {:breaker-id breaker-id})
      (events/emit! :circuit-breaker-half-opened {:breaker-id breaker-id}))))

(defn- transition-to-closed!
  "Transition circuit breaker to closed state."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [now (System/currentTimeMillis)]
      (swap! state update-in [:breakers breaker-id :state]
             assoc :state :closed :opened-at nil :half-opened-at nil :last-state-change now)
      (reset! (get-in @state [:breakers breaker-id :state :failure-count]) 0)
      (reset! (get-in @state [:breakers breaker-id :state :success-count]) 0)
      
      ;; Call on-close callback
      (when-let [on-close (:on-close breaker)]
        (try (on-close) (catch Exception _)))
      
      (logging/log :info "Circuit breaker closed" {:breaker-id breaker-id})
      (events/emit! :circuit-breaker-closed {:breaker-id breaker-id}))))

;; ============================================================================
;; Circuit Breaker Logic
;; ============================================================================

(defn- should-open?
  "Check if circuit breaker should open."
  [breaker]
  (let [profile (:profile breaker)
        failure-count @(get-in breaker [:state :failure-count])
        total-calls @(get-in breaker [:state :total-calls])
        failure-rate (if (pos? total-calls) (/ failure-count total-calls) 0)]
    (or (>= failure-count (:failure-threshold profile))
        (and (> total-calls 10)
             (>= failure-rate (:failure-rate-threshold profile))))))

(defn- should-close?
  "Check if circuit breaker should close."
  [breaker]
  (let [profile (:profile breaker)
        success-count @(get-in breaker [:state :success-count])]
    (>= success-count (:success-threshold profile))))

(defn- should-try-half-open?
  "Check if circuit breaker should try half-open."
  [breaker]
  (let [profile (:profile breaker)
        opened-at (get-in breaker [:state :opened-at])
        now (System/currentTimeMillis)]
    (and opened-at
         (> (- now opened-at) (:half-open-timeout-ms profile)))))

(defn record-success!
  "Record a successful call."
  [breaker-id & {:keys [duration-ms]}]
  (when-let [breaker (get-breaker breaker-id)]
    (let [breaker-state (get-in breaker [:state :state])]
      (swap! (get-in breaker [:state :total-calls]) inc)
      (swap! state update-in [:stats :total-calls] inc)
      
      (case breaker-state
        :half-open
        (do
          (swap! (get-in breaker [:state :success-count]) inc)
          (when (should-close? (get-breaker breaker-id))
            (transition-to-closed! breaker-id)))
        
        :closed
        (when duration-ms
          (let [profile (:profile breaker)]
            (when (> duration-ms (:slow-call-threshold-ms profile))
              (swap! (get-in breaker [:state :slow-calls]) inc))))
        
        nil))))

(defn record-failure!
  "Record a failed call."
  [breaker-id & {:keys [error]}]
  (when-let [breaker (get-breaker breaker-id)]
    (let [breaker-state (get-in breaker [:state :state])
          now (System/currentTimeMillis)]
      (swap! (get-in breaker [:state :total-calls]) inc)
      (swap! (get-in breaker [:state :failure-count]) inc)
      (swap! state update-in [:stats :total-calls] inc)
      (swap! state update-in [:stats :failures] inc)
      (swap! state assoc-in [:breakers breaker-id :state :last-failure-time] now)
      
      (case breaker-state
        :closed
        (when (should-open? (get-breaker breaker-id))
          (transition-to-open! breaker-id))
        
        :half-open
        (transition-to-open! breaker-id)
        
        nil))))

(defn allow-request?
  "Check if a request should be allowed."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (when (:enabled? breaker)
      (let [breaker-state (get-in breaker [:state :state])]
        (case breaker-state
          :closed true
          :half-open true
          :open (if (should-try-half-open? breaker)
                  (do
                    (transition-to-half-open! breaker-id)
                    true)
                  false))))))

;; ============================================================================
;; Execution Wrapper
;; ============================================================================

(defn execute
  "Execute a function with circuit breaker protection."
  [breaker-id f & {:keys [fallback]}]
  (if-not (allow-request? breaker-id)
    ;; Circuit is open
    (if-let [fallback-fn (or fallback (get-in @state [:breakers breaker-id :fallback-fn]))]
      (fallback-fn)
      (throw (ex-info "Circuit breaker is open" {:breaker-id breaker-id})))
    
    ;; Circuit allows request
    (let [start-time (System/currentTimeMillis)]
      (try
        (let [result (f)]
          (record-success! breaker-id :duration-ms (- (System/currentTimeMillis) start-time))
          result)
        (catch Exception e
          (record-failure! breaker-id :error (.getMessage e))
          (throw e))))))

(defmacro with-circuit-breaker
  "Execute body with circuit breaker protection."
  [breaker-id & body]
  `(execute ~breaker-id (fn [] ~@body)))

;; ============================================================================
;; Bulk Operations
;; ============================================================================

(defn reset-breaker!
  "Reset a circuit breaker to closed state."
  [breaker-id]
  (when (get-breaker breaker-id)
    (transition-to-closed! breaker-id)
    (reset! (get-in @state [:breakers breaker-id :state :total-calls]) 0)
    (reset! (get-in @state [:breakers breaker-id :state :slow-calls]) 0)))

(defn reset-all-breakers!
  "Reset all circuit breakers."
  []
  (doseq [[breaker-id _] (:breakers @state)]
    (reset-breaker! breaker-id)))

(defn reset-group!
  "Reset all circuit breakers in a group."
  [group-id]
  (when-let [breaker-ids (get-in @state [:groups group-id])]
    (doseq [breaker-id breaker-ids]
      (reset-breaker! breaker-id))))

(defn force-open!
  "Force a circuit breaker to open state."
  [breaker-id]
  (transition-to-open! breaker-id))

(defn force-close!
  "Force a circuit breaker to closed state."
  [breaker-id]
  (transition-to-closed! breaker-id))

;; ============================================================================
;; Monitoring
;; ============================================================================

(defn get-breaker-status
  "Get detailed status of a circuit breaker."
  [breaker-id]
  (when-let [breaker (get-breaker breaker-id)]
    (let [breaker-state (:state breaker)]
      {:id breaker-id
       :name (:name breaker)
       :state (:state breaker-state)
       :failure-count @(:failure-count breaker-state)
       :success-count @(:success-count breaker-state)
       :total-calls @(:total-calls breaker-state)
       :slow-calls @(:slow-calls breaker-state)
       :last-failure-time (:last-failure-time breaker-state)
       :opened-at (:opened-at breaker-state)
       :last-state-change (:last-state-change breaker-state)
       :enabled? (:enabled? breaker)})))

(defn get-all-statuses
  "Get status of all circuit breakers."
  []
  (mapv (fn [[id _]] (get-breaker-status id)) (:breakers @state)))

(defn get-group-status
  "Get status of all circuit breakers in a group."
  [group-id]
  (when-let [breaker-ids (get-in @state [:groups group-id])]
    (mapv get-breaker-status breaker-ids)))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn get-dashboard-data
  "Get data for circuit breaker dashboard."
  []
  {:breakers (get-all-statuses)
   :groups (reduce (fn [m [group-id breaker-ids]]
                     (assoc m group-id {:count (count breaker-ids)
                                        :open (count (filter #(= :open (get-in (get-breaker %) [:state :state])) breaker-ids))}))
                   {}
                   (:groups @state))
   :profiles (list-profiles)
   :stats (:stats @state)})

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-registry-stats
  "Get circuit breaker registry statistics."
  []
  (let [stats (:stats @state)
        breakers (vals (:breakers @state))
        by-state (group-by #(get-in % [:state :state]) breakers)]
    {:total-breakers (count breakers)
     :total-groups (count (:groups @state))
     :total-profiles (count (:profiles @state))
     :breakers-by-state {:closed (count (get by-state :closed []))
                         :open (count (get by-state :open []))
                         :half-open (count (get by-state :half-open []))}
     :total-calls (:total-calls stats)
     :failures (:failures stats)
     :opens (:opens stats)
     :half-opens (:half-opens stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-circuit-breaker-registry!
  "Initialize the circuit breaker registry."
  []
  (when-not (:initialized? @state)
    ;; Create default profiles
    (create-profile! :standard
                     {:name "Standard"
                      :failure-threshold 5
                      :success-threshold 3
                      :timeout-ms 30000
                      :half-open-timeout-ms 60000})
    
    (create-profile! :aggressive
                     {:name "Aggressive"
                      :failure-threshold 3
                      :success-threshold 2
                      :timeout-ms 10000
                      :half-open-timeout-ms 30000})
    
    (create-profile! :lenient
                     {:name "Lenient"
                      :failure-threshold 10
                      :success-threshold 5
                      :timeout-ms 60000
                      :half-open-timeout-ms 120000})
    
    ;; Register default circuit breakers
    (register-breaker! :lm-studio
                       {:name "LM Studio"
                        :profile :standard
                        :group :external-services
                        :fallback-fn (fn [] {:error "LM Studio unavailable"})})
    
    (register-breaker! :database
                       {:name "Database"
                        :profile :aggressive
                        :group :infrastructure})
    
    (register-breaker! :analysis-pipeline
                       {:name "Analysis Pipeline"
                        :profile :standard
                        :group :core-services})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Circuit breaker registry initialized")
    (events/emit! :circuit-breaker-registry-initialized {})
    true))
