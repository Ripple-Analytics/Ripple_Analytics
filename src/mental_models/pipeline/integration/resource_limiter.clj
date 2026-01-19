(ns mental-models.pipeline.integration.resource-limiter
  "Resource limiter for mental model analysis system.
   
   Features:
   - CPU usage limiting
   - Memory usage limiting
   - Concurrent request limiting
   - Bandwidth limiting
   - Resource quotas
   - Resource reservations
   - Limit enforcement
   - Resource metrics"
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
           [java.util.concurrent Semaphore ConcurrentHashMap]
           [java.lang.management ManagementFactory]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:limiters {}         ;; limiter-id -> limiter
         :quotas {}           ;; quota-id -> quota
         :reservations {}     ;; reservation-id -> reservation
         :config {:default-cpu-limit 80        ;; percent
                  :default-memory-limit 85     ;; percent
                  :default-concurrent-limit 100
                  :check-interval-ms 1000}
         :stats {:requests-allowed 0
                 :requests-denied 0
                 :quota-exceeded 0
                 :reservations-made 0}
         :initialized? false}))

;; ============================================================================
;; Resource Monitoring
;; ============================================================================

(defn get-cpu-usage
  "Get current CPU usage."
  []
  (let [os-bean (ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? com.sun.management.OperatingSystemMXBean os-bean)
      (* 100 (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean os-bean))
      -1)))

(defn get-memory-usage
  "Get current memory usage percentage."
  []
  (let [runtime (Runtime/getRuntime)
        max-memory (.maxMemory runtime)
        total-memory (.totalMemory runtime)
        free-memory (.freeMemory runtime)
        used-memory (- total-memory free-memory)]
    (* 100.0 (/ used-memory max-memory))))

(defn get-system-load
  "Get system load average."
  []
  (let [os-bean (ManagementFactory/getOperatingSystemMXBean)]
    (.getSystemLoadAverage os-bean)))

;; ============================================================================
;; Limiter Creation
;; ============================================================================

(defn create-limiter!
  "Create a resource limiter."
  [limiter-id config]
  (let [limiter {:id limiter-id
                 :name (get config :name (name limiter-id))
                 :type (get config :type :concurrent)  ;; :concurrent, :cpu, :memory, :bandwidth
                 :limit (get config :limit 100)
                 :semaphore (when (= :concurrent (get config :type :concurrent))
                              (Semaphore. (get config :limit 100)))
                 :current (atom 0)
                 :enabled? (atom true)
                 :metrics {:allowed (atom 0)
                           :denied (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:limiters limiter-id] limiter)
    (logging/log :info "Created limiter" {:limiter-id limiter-id :type (:type limiter)})
    (events/emit! :limiter-created {:limiter-id limiter-id})
    limiter-id))

(defn get-limiter
  "Get a limiter."
  [limiter-id]
  (get-in @state [:limiters limiter-id]))

(defn list-limiters
  "List all limiters."
  []
  (mapv (fn [[id l]]
          {:id id
           :name (:name l)
           :type (:type l)
           :limit (:limit l)
           :current @(:current l)
           :enabled? @(:enabled? l)})
        (:limiters @state)))

(defn delete-limiter!
  "Delete a limiter."
  [limiter-id]
  (swap! state update :limiters dissoc limiter-id)
  (logging/log :info "Deleted limiter" {:limiter-id limiter-id}))

;; ============================================================================
;; Limit Checking
;; ============================================================================

(defn- check-cpu-limit
  "Check if CPU limit is exceeded."
  [limiter]
  (let [current-cpu (get-cpu-usage)]
    (< current-cpu (:limit limiter))))

(defn- check-memory-limit
  "Check if memory limit is exceeded."
  [limiter]
  (let [current-memory (get-memory-usage)]
    (< current-memory (:limit limiter))))

(defn- check-concurrent-limit
  "Check if concurrent limit is exceeded."
  [limiter]
  (< @(:current limiter) (:limit limiter)))

(defn- check-bandwidth-limit
  "Check if bandwidth limit is exceeded."
  [limiter]
  ;; Simplified - would need actual bandwidth tracking
  (< @(:current limiter) (:limit limiter)))

(defn can-proceed?
  "Check if a request can proceed."
  [limiter-id]
  (if-let [limiter (get-limiter limiter-id)]
    (if @(:enabled? limiter)
      (case (:type limiter)
        :cpu (check-cpu-limit limiter)
        :memory (check-memory-limit limiter)
        :concurrent (check-concurrent-limit limiter)
        :bandwidth (check-bandwidth-limit limiter)
        true)
      true)
    true))

;; ============================================================================
;; Resource Acquisition
;; ============================================================================

(defn acquire!
  "Acquire a resource permit."
  [limiter-id & {:keys [timeout-ms] :or {timeout-ms 0}}]
  (if-let [limiter (get-limiter limiter-id)]
    (if @(:enabled? limiter)
      (case (:type limiter)
        :concurrent
        (let [semaphore (:semaphore limiter)]
          (if (pos? timeout-ms)
            (if (.tryAcquire semaphore timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
              (do
                (swap! (:current limiter) inc)
                (swap! (get-in limiter [:metrics :allowed]) inc)
                (swap! state update-in [:stats :requests-allowed] inc)
                {:acquired? true})
              (do
                (swap! (get-in limiter [:metrics :denied]) inc)
                (swap! state update-in [:stats :requests-denied] inc)
                {:acquired? false :reason :timeout}))
            (if (.tryAcquire semaphore)
              (do
                (swap! (:current limiter) inc)
                (swap! (get-in limiter [:metrics :allowed]) inc)
                (swap! state update-in [:stats :requests-allowed] inc)
                {:acquired? true})
              (do
                (swap! (get-in limiter [:metrics :denied]) inc)
                (swap! state update-in [:stats :requests-denied] inc)
                {:acquired? false :reason :limit-exceeded}))))
        
        ;; For CPU/memory limits, just check
        (if (can-proceed? limiter-id)
          (do
            (swap! (:current limiter) inc)
            (swap! (get-in limiter [:metrics :allowed]) inc)
            (swap! state update-in [:stats :requests-allowed] inc)
            {:acquired? true})
          (do
            (swap! (get-in limiter [:metrics :denied]) inc)
            (swap! state update-in [:stats :requests-denied] inc)
            {:acquired? false :reason :limit-exceeded})))
      {:acquired? true})
    {:acquired? true}))

(defn release!
  "Release a resource permit."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (case (:type limiter)
      :concurrent
      (do
        (.release (:semaphore limiter))
        (swap! (:current limiter) dec))
      
      (swap! (:current limiter) dec))))

(defmacro with-limit
  "Execute body with resource limiting."
  [limiter-id & body]
  `(let [result# (acquire! ~limiter-id)]
     (if (:acquired? result#)
       (try
         ~@body
         (finally
           (release! ~limiter-id)))
       (throw (ex-info "Resource limit exceeded" {:limiter-id ~limiter-id
                                                   :reason (:reason result#)})))))

;; ============================================================================
;; Resource Quotas
;; ============================================================================

(defn create-quota!
  "Create a resource quota."
  [quota-id config]
  (let [quota {:id quota-id
               :name (get config :name (name quota-id))
               :limit (get config :limit 1000)
               :period-ms (get config :period-ms 60000)  ;; 1 minute
               :usage (atom 0)
               :period-start (atom (System/currentTimeMillis))
               :on-exceeded (get config :on-exceeded)
               :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:quotas quota-id] quota)
    (logging/log :info "Created quota" {:quota-id quota-id :limit (:limit quota)})
    quota-id))

(defn get-quota
  "Get a quota."
  [quota-id]
  (get-in @state [:quotas quota-id]))

(defn- reset-quota-if-needed!
  "Reset quota if period has elapsed."
  [quota]
  (let [now (System/currentTimeMillis)
        period-start @(:period-start quota)]
    (when (> (- now period-start) (:period-ms quota))
      (reset! (:usage quota) 0)
      (reset! (:period-start quota) now))))

(defn use-quota!
  "Use quota units."
  [quota-id units]
  (when-let [quota (get-quota quota-id)]
    (reset-quota-if-needed! quota)
    (let [current @(:usage quota)
          new-usage (+ current units)]
      (if (<= new-usage (:limit quota))
        (do
          (swap! (:usage quota) + units)
          {:success? true :remaining (- (:limit quota) new-usage)})
        (do
          (swap! state update-in [:stats :quota-exceeded] inc)
          (when-let [on-exceeded (:on-exceeded quota)]
            (on-exceeded quota-id units))
          {:success? false :reason :quota-exceeded})))))

(defn get-quota-usage
  "Get quota usage."
  [quota-id]
  (when-let [quota (get-quota quota-id)]
    (reset-quota-if-needed! quota)
    {:quota-id quota-id
     :limit (:limit quota)
     :used @(:usage quota)
     :remaining (- (:limit quota) @(:usage quota))
     :period-ms (:period-ms quota)}))

;; ============================================================================
;; Resource Reservations
;; ============================================================================

(defn make-reservation!
  "Make a resource reservation."
  [reservation-id config]
  (let [reservation {:id reservation-id
                     :limiter-id (get config :limiter-id)
                     :amount (get config :amount 1)
                     :expires-at (+ (System/currentTimeMillis)
                                    (get config :duration-ms 60000))
                     :created-at (System/currentTimeMillis)}]
    
    ;; Acquire the resources
    (let [limiter-id (:limiter-id reservation)
          result (acquire! limiter-id)]
      (if (:acquired? result)
        (do
          (swap! state assoc-in [:reservations reservation-id] reservation)
          (swap! state update-in [:stats :reservations-made] inc)
          (logging/log :debug "Made reservation" {:reservation-id reservation-id})
          {:success? true :reservation-id reservation-id})
        {:success? false :reason (:reason result)}))))

(defn cancel-reservation!
  "Cancel a resource reservation."
  [reservation-id]
  (when-let [reservation (get-in @state [:reservations reservation-id])]
    (release! (:limiter-id reservation))
    (swap! state update :reservations dissoc reservation-id)
    (logging/log :debug "Cancelled reservation" {:reservation-id reservation-id})))

(defn- cleanup-expired-reservations!
  "Cleanup expired reservations."
  []
  (let [now (System/currentTimeMillis)]
    (doseq [[id r] (:reservations @state)]
      (when (> now (:expires-at r))
        (cancel-reservation! id)))))

;; ============================================================================
;; Limiter Control
;; ============================================================================

(defn enable-limiter!
  "Enable a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (reset! (:enabled? limiter) true)
    (logging/log :info "Enabled limiter" {:limiter-id limiter-id})))

(defn disable-limiter!
  "Disable a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (reset! (:enabled? limiter) false)
    (logging/log :info "Disabled limiter" {:limiter-id limiter-id})))

(defn set-limit!
  "Set the limit for a limiter."
  [limiter-id new-limit]
  (when-let [limiter (get-limiter limiter-id)]
    (swap! state assoc-in [:limiters limiter-id :limit] new-limit)
    (logging/log :info "Set limit" {:limiter-id limiter-id :limit new-limit})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-limiter-metrics
  "Get metrics for a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    {:limiter-id limiter-id
     :name (:name limiter)
     :type (:type limiter)
     :limit (:limit limiter)
     :current @(:current limiter)
     :utilization (/ @(:current limiter) (:limit limiter))
     :allowed @(get-in limiter [:metrics :allowed])
     :denied @(get-in limiter [:metrics :denied])
     :enabled? @(:enabled? limiter)}))

(defn get-all-limiter-metrics
  "Get metrics for all limiters."
  []
  (mapv (fn [[id _]] (get-limiter-metrics id)) (:limiters @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-resource-limiter-stats
  "Get resource limiter statistics."
  []
  (let [stats (:stats @state)]
    {:limiters-count (count (:limiters @state))
     :quotas-count (count (:quotas @state))
     :reservations-count (count (:reservations @state))
     :requests-allowed (:requests-allowed stats)
     :requests-denied (:requests-denied stats)
     :quota-exceeded (:quota-exceeded stats)
     :reservations-made (:reservations-made stats)
     :allow-rate (if (pos? (+ (:requests-allowed stats) (:requests-denied stats)))
                   (/ (:requests-allowed stats)
                      (+ (:requests-allowed stats) (:requests-denied stats)))
                   1.0)}))

(defn reset-stats!
  "Reset resource limiter statistics."
  []
  (swap! state assoc :stats {:requests-allowed 0
                             :requests-denied 0
                             :quota-exceeded 0
                             :reservations-made 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-resource-limiter!
  "Initialize the resource limiter."
  []
  (when-not (:initialized? @state)
    ;; Create default limiters
    (create-limiter! :concurrent
                     {:name "Concurrent"
                      :type :concurrent
                      :limit 100})
    
    (create-limiter! :cpu
                     {:name "CPU"
                      :type :cpu
                      :limit 80})
    
    (create-limiter! :memory
                     {:name "Memory"
                      :type :memory
                      :limit 85})
    
    ;; Create default quota
    (create-quota! :api
                   {:name "API"
                    :limit 1000
                    :period-ms 60000})
    
    ;; Start cleanup task
    (go-loop []
      (when (:initialized? @state)
        (<! (timeout 10000))
        (cleanup-expired-reservations!)
        (recur)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Resource limiter initialized")
    (events/emit! :resource-limiter-initialized {})
    true))
