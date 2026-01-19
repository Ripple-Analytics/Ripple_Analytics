(ns mental-models.pipeline.integration.memory-manager
  "Memory manager for mental model analysis system.
   
   Features:
   - Memory usage monitoring
   - GC triggering
   - Memory pressure detection
   - Object pool management
   - Memory budgets
   - Soft reference caching
   - Memory metrics
   - Memory alerts"
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
           [java.lang.management ManagementFactory MemoryMXBean MemoryPoolMXBean]
           [java.lang.ref SoftReference WeakReference ReferenceQueue]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:budgets {}          ;; budget-id -> budget
         :pools {}            ;; pool-id -> object-pool
         :soft-cache {}       ;; cache-id -> soft-cache
         :config {:monitor-interval-ms 5000
                  :gc-threshold-percent 85
                  :pressure-threshold-percent 75
                  :critical-threshold-percent 90
                  :min-free-mb 100}
         :stats {:gc-triggered 0
                 :pressure-events 0
                 :critical-events 0
                 :objects-pooled 0
                 :objects-evicted 0}
         :pressure-level (atom :normal)  ;; :normal, :elevated, :critical
         :initialized? false}))

;; ============================================================================
;; Memory Monitoring
;; ============================================================================

(defn get-memory-usage
  "Get current memory usage."
  []
  (let [runtime (Runtime/getRuntime)
        max-memory (.maxMemory runtime)
        total-memory (.totalMemory runtime)
        free-memory (.freeMemory runtime)
        used-memory (- total-memory free-memory)]
    {:max-mb (/ max-memory 1048576.0)
     :total-mb (/ total-memory 1048576.0)
     :used-mb (/ used-memory 1048576.0)
     :free-mb (/ free-memory 1048576.0)
     :usage-percent (* 100.0 (/ used-memory max-memory))}))

(defn get-heap-usage
  "Get heap memory usage."
  []
  (let [memory-bean (ManagementFactory/getMemoryMXBean)
        heap-usage (.getHeapMemoryUsage memory-bean)]
    {:init-mb (/ (.getInit heap-usage) 1048576.0)
     :used-mb (/ (.getUsed heap-usage) 1048576.0)
     :committed-mb (/ (.getCommitted heap-usage) 1048576.0)
     :max-mb (/ (.getMax heap-usage) 1048576.0)
     :usage-percent (* 100.0 (/ (.getUsed heap-usage) (.getMax heap-usage)))}))

(defn get-non-heap-usage
  "Get non-heap memory usage."
  []
  (let [memory-bean (ManagementFactory/getMemoryMXBean)
        non-heap-usage (.getNonHeapMemoryUsage memory-bean)]
    {:init-mb (/ (.getInit non-heap-usage) 1048576.0)
     :used-mb (/ (.getUsed non-heap-usage) 1048576.0)
     :committed-mb (/ (.getCommitted non-heap-usage) 1048576.0)}))

(defn get-memory-pools
  "Get memory pool information."
  []
  (let [pool-beans (ManagementFactory/getMemoryPoolMXBeans)]
    (mapv (fn [pool]
            {:name (.getName pool)
             :type (str (.getType pool))
             :usage-mb (when-let [usage (.getUsage pool)]
                         (/ (.getUsed usage) 1048576.0))})
          pool-beans)))

;; ============================================================================
;; Memory Pressure Detection
;; ============================================================================

(defn- calculate-pressure-level
  "Calculate the current memory pressure level."
  []
  (let [usage (get-memory-usage)
        percent (:usage-percent usage)
        critical-threshold (get-in @state [:config :critical-threshold-percent])
        pressure-threshold (get-in @state [:config :pressure-threshold-percent])]
    (cond
      (>= percent critical-threshold) :critical
      (>= percent pressure-threshold) :elevated
      :else :normal)))

(defn- update-pressure-level!
  "Update the memory pressure level."
  []
  (let [new-level (calculate-pressure-level)
        old-level @(:pressure-level @state)]
    (when (not= new-level old-level)
      (reset! (:pressure-level @state) new-level)
      (logging/log :info "Memory pressure changed" {:old-level old-level
                                                     :new-level new-level})
      (events/emit! :memory-pressure-changed {:old-level old-level
                                               :new-level new-level})
      
      (case new-level
        :elevated (swap! state update-in [:stats :pressure-events] inc)
        :critical (swap! state update-in [:stats :critical-events] inc)
        nil))))

(defn get-pressure-level
  "Get the current memory pressure level."
  []
  @(:pressure-level @state))

(defn is-under-pressure?
  "Check if the system is under memory pressure."
  []
  (not= :normal (get-pressure-level)))

;; ============================================================================
;; Garbage Collection
;; ============================================================================

(defn trigger-gc!
  "Trigger garbage collection."
  []
  (System/gc)
  (swap! state update-in [:stats :gc-triggered] inc)
  (logging/log :info "Triggered garbage collection"))

(defn- maybe-trigger-gc!
  "Trigger GC if memory usage exceeds threshold."
  []
  (let [usage (get-memory-usage)
        threshold (get-in @state [:config :gc-threshold-percent])]
    (when (>= (:usage-percent usage) threshold)
      (trigger-gc!))))

;; ============================================================================
;; Memory Budgets
;; ============================================================================

(defn create-budget!
  "Create a memory budget."
  [budget-id config]
  (let [budget {:id budget-id
                :name (get config :name (name budget-id))
                :max-mb (get config :max-mb 100)
                :current-mb (atom 0)
                :allocations (atom {})
                :on-exceeded (get config :on-exceeded)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:budgets budget-id] budget)
    (logging/log :info "Created memory budget" {:budget-id budget-id :max-mb (:max-mb budget)})
    budget-id))

(defn get-budget
  "Get a memory budget."
  [budget-id]
  (get-in @state [:budgets budget-id]))

(defn allocate!
  "Allocate memory from a budget."
  [budget-id allocation-id size-mb]
  (when-let [budget (get-budget budget-id)]
    (let [current @(:current-mb budget)
          new-total (+ current size-mb)]
      (if (<= new-total (:max-mb budget))
        (do
          (swap! (:current-mb budget) + size-mb)
          (swap! (:allocations budget) assoc allocation-id size-mb)
          {:success? true :allocated size-mb})
        (do
          (when-let [on-exceeded (:on-exceeded budget)]
            (on-exceeded budget-id size-mb))
          {:success? false :reason :budget-exceeded})))))

(defn deallocate!
  "Deallocate memory from a budget."
  [budget-id allocation-id]
  (when-let [budget (get-budget budget-id)]
    (when-let [size-mb (get @(:allocations budget) allocation-id)]
      (swap! (:current-mb budget) - size-mb)
      (swap! (:allocations budget) dissoc allocation-id)
      {:success? true :deallocated size-mb})))

(defn get-budget-usage
  "Get budget usage."
  [budget-id]
  (when-let [budget (get-budget budget-id)]
    {:budget-id budget-id
     :max-mb (:max-mb budget)
     :current-mb @(:current-mb budget)
     :available-mb (- (:max-mb budget) @(:current-mb budget))
     :usage-percent (* 100.0 (/ @(:current-mb budget) (:max-mb budget)))
     :allocations-count (count @(:allocations budget))}))

;; ============================================================================
;; Object Pools
;; ============================================================================

(defn create-object-pool!
  "Create an object pool."
  [pool-id config]
  (let [pool {:id pool-id
              :name (get config :name (name pool-id))
              :factory-fn (get config :factory-fn)
              :reset-fn (get config :reset-fn identity)
              :max-size (get config :max-size 100)
              :objects (atom [])
              :in-use (atom #{})
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:pools pool-id] pool)
    (logging/log :info "Created object pool" {:pool-id pool-id :max-size (:max-size pool)})
    pool-id))

(defn get-object-pool
  "Get an object pool."
  [pool-id]
  (get-in @state [:pools pool-id]))

(defn borrow!
  "Borrow an object from the pool."
  [pool-id]
  (when-let [pool (get-object-pool pool-id)]
    (if-let [obj (first @(:objects pool))]
      (do
        (swap! (:objects pool) rest)
        (swap! (:in-use pool) conj obj)
        obj)
      (when-let [factory (:factory-fn pool)]
        (let [obj (factory)]
          (swap! (:in-use pool) conj obj)
          (swap! state update-in [:stats :objects-pooled] inc)
          obj)))))

(defn return!
  "Return an object to the pool."
  [pool-id obj]
  (when-let [pool (get-object-pool pool-id)]
    (swap! (:in-use pool) disj obj)
    (let [reset-fn (:reset-fn pool)
          reset-obj (reset-fn obj)]
      (if (< (count @(:objects pool)) (:max-size pool))
        (swap! (:objects pool) conj reset-obj)
        (swap! state update-in [:stats :objects-evicted] inc)))))

(defn get-pool-stats
  "Get object pool statistics."
  [pool-id]
  (when-let [pool (get-object-pool pool-id)]
    {:pool-id pool-id
     :available (count @(:objects pool))
     :in-use (count @(:in-use pool))
     :max-size (:max-size pool)}))

;; ============================================================================
;; Soft Reference Cache
;; ============================================================================

(defn create-soft-cache!
  "Create a soft reference cache."
  [cache-id config]
  (let [cache {:id cache-id
               :name (get config :name (name cache-id))
               :entries (atom {})
               :reference-queue (ReferenceQueue.)
               :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:soft-cache cache-id] cache)
    (logging/log :info "Created soft cache" {:cache-id cache-id})
    cache-id))

(defn soft-cache-put!
  "Put a value in the soft cache."
  [cache-id key value]
  (when-let [cache (get-in @state [:soft-cache cache-id])]
    (let [ref (SoftReference. value (:reference-queue cache))]
      (swap! (:entries cache) assoc key ref))))

(defn soft-cache-get
  "Get a value from the soft cache."
  [cache-id key]
  (when-let [cache (get-in @state [:soft-cache cache-id])]
    (when-let [ref (get @(:entries cache) key)]
      (.get ref))))

(defn soft-cache-remove!
  "Remove a value from the soft cache."
  [cache-id key]
  (when-let [cache (get-in @state [:soft-cache cache-id])]
    (swap! (:entries cache) dissoc key)))

(defn soft-cache-clear!
  "Clear the soft cache."
  [cache-id]
  (when-let [cache (get-in @state [:soft-cache cache-id])]
    (swap! (:entries cache) (constantly {}))))

;; ============================================================================
;; Memory Monitoring Loop
;; ============================================================================

(defn- start-monitor!
  "Start the memory monitor."
  []
  (go-loop []
    (when (:initialized? @state)
      (update-pressure-level!)
      (maybe-trigger-gc!)
      (<! (timeout (get-in @state [:config :monitor-interval-ms])))
      (recur))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-memory-stats
  "Get memory manager statistics."
  []
  (let [stats (:stats @state)
        usage (get-memory-usage)]
    {:memory-usage usage
     :pressure-level (get-pressure-level)
     :budgets-count (count (:budgets @state))
     :pools-count (count (:pools @state))
     :soft-caches-count (count (:soft-cache @state))
     :gc-triggered (:gc-triggered stats)
     :pressure-events (:pressure-events stats)
     :critical-events (:critical-events stats)
     :objects-pooled (:objects-pooled stats)
     :objects-evicted (:objects-evicted stats)}))

(defn reset-stats!
  "Reset memory manager statistics."
  []
  (swap! state assoc :stats {:gc-triggered 0
                             :pressure-events 0
                             :critical-events 0
                             :objects-pooled 0
                             :objects-evicted 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-memory-manager!
  "Initialize the memory manager."
  []
  (when-not (:initialized? @state)
    ;; Create default budget
    (create-budget! :default
                    {:name "Default"
                     :max-mb 500})
    
    ;; Start monitor
    (swap! state assoc :initialized? true)
    (start-monitor!)
    
    (logging/log :info "Memory manager initialized")
    (events/emit! :memory-manager-initialized {})
    true))

(defn shutdown-memory-manager!
  "Shutdown the memory manager."
  []
  (swap! state assoc :initialized? false)
  (logging/log :info "Memory manager shutdown"))
