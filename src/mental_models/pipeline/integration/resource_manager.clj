(ns mental-models.pipeline.integration.resource-manager
  "Resource management for mental model analysis system.
   
   Features:
   - Resource pooling
   - Connection management
   - Memory management
   - Thread pool management
   - Resource limits
   - Quota management
   - Resource scheduling
   - Cleanup and garbage collection"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.concurrent Executors ThreadPoolExecutor TimeUnit
            ArrayBlockingQueue LinkedBlockingQueue Semaphore]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:pools {}            ;; pool-id -> pool
         :connections {}      ;; connection-id -> connection
         :quotas {}           ;; quota-id -> quota-config
         :usage {}            ;; {quota-id user-id} -> usage
         :limits {}           ;; limit-id -> limit-config
         :schedulers {}       ;; scheduler-id -> scheduler
         :stats {:allocations 0 :releases 0 :quota-exceeded 0}
         :initialized? false}))

;; ============================================================================
;; Resource Pools
;; ============================================================================

(defn create-pool!
  "Create a resource pool."
  [pool-id config]
  (let [min-size (get config :min-size 1)
        max-size (get config :max-size 10)
        factory-fn (get config :factory-fn (fn [] {:id (str (UUID/randomUUID))}))
        validate-fn (get config :validate-fn (fn [_] true))
        destroy-fn (get config :destroy-fn (fn [_] nil))
        
        ;; Create initial resources
        initial-resources (vec (repeatedly min-size factory-fn))
        
        pool {:id pool-id
              :name (get config :name (name pool-id))
              :min-size min-size
              :max-size max-size
              :factory-fn factory-fn
              :validate-fn validate-fn
              :destroy-fn destroy-fn
              :available initial-resources
              :in-use {}
              :waiting-queue (LinkedBlockingQueue.)
              :semaphore (Semaphore. max-size)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:pools pool-id] pool)
    (logging/log :info "Created pool" {:pool-id pool-id :size (count initial-resources)})
    (events/emit! :pool-created {:pool-id pool-id})
    pool-id))

(defn acquire-resource!
  "Acquire a resource from a pool."
  [pool-id & {:keys [timeout-ms] :or {timeout-ms 30000}}]
  (when (flags/enabled? :resource-manager)
    (when-let [pool (get-in @state [:pools pool-id])]
      (let [semaphore (:semaphore pool)]
        (if (.tryAcquire semaphore timeout-ms TimeUnit/MILLISECONDS)
          (let [resource (or (first (:available pool))
                             ((:factory-fn pool)))
                resource-id (or (:id resource) (str (UUID/randomUUID)))]
            ;; Move from available to in-use
            (swap! state update-in [:pools pool-id :available] rest)
            (swap! state assoc-in [:pools pool-id :in-use resource-id]
                   {:resource resource
                    :acquired-at (System/currentTimeMillis)
                    :acquired-by (Thread/currentThread)})
            (swap! state update-in [:stats :allocations] inc)
            (metrics/increment :resource-acquired {:pool-id pool-id})
            (logging/log :debug "Acquired resource" {:pool-id pool-id :resource-id resource-id})
            resource)
          (do
            (logging/log :warn "Resource acquisition timeout" {:pool-id pool-id})
            nil))))))

(defn release-resource!
  "Release a resource back to the pool."
  [pool-id resource]
  (when-let [pool (get-in @state [:pools pool-id])]
    (let [resource-id (or (:id resource) (str (hash resource)))
          validate-fn (:validate-fn pool)
          destroy-fn (:destroy-fn pool)
          semaphore (:semaphore pool)]
      ;; Remove from in-use
      (swap! state update-in [:pools pool-id :in-use] dissoc resource-id)
      
      ;; Validate and return to pool or destroy
      (if (validate-fn resource)
        (swap! state update-in [:pools pool-id :available] conj resource)
        (do
          (destroy-fn resource)
          ;; Create replacement if below min size
          (when (< (count (:available (get-in @state [:pools pool-id])))
                   (:min-size pool))
            (swap! state update-in [:pools pool-id :available]
                   conj ((:factory-fn pool))))))
      
      (.release semaphore)
      (swap! state update-in [:stats :releases] inc)
      (metrics/increment :resource-released {:pool-id pool-id})
      (logging/log :debug "Released resource" {:pool-id pool-id :resource-id resource-id}))))

(defn get-pool-stats
  "Get statistics for a pool."
  [pool-id]
  (when-let [pool (get-in @state [:pools pool-id])]
    {:pool-id pool-id
     :available (count (:available pool))
     :in-use (count (:in-use pool))
     :total (+ (count (:available pool)) (count (:in-use pool)))
     :max-size (:max-size pool)
     :utilization (/ (count (:in-use pool)) (max 1 (:max-size pool)))}))

(defn resize-pool!
  "Resize a pool."
  [pool-id new-max-size]
  (when-let [pool (get-in @state [:pools pool-id])]
    (let [old-max (:max-size pool)
          diff (- new-max-size old-max)]
      (swap! state assoc-in [:pools pool-id :max-size] new-max-size)
      ;; Adjust semaphore
      (if (pos? diff)
        (.release (:semaphore pool) diff)
        (dotimes [_ (Math/abs diff)]
          (.tryAcquire (:semaphore pool))))
      (logging/log :info "Resized pool" {:pool-id pool-id :old-max old-max :new-max new-max-size}))))

;; ============================================================================
;; Connection Management
;; ============================================================================

(defn register-connection!
  "Register a connection."
  [connection-id config]
  (let [connection {:id connection-id
                    :type (get config :type :generic)
                    :host (get config :host "localhost")
                    :port (get config :port 80)
                    :status :connected
                    :created-at (System/currentTimeMillis)
                    :last-used (System/currentTimeMillis)
                    :use-count 0}]
    (swap! state assoc-in [:connections connection-id] connection)
    (logging/log :info "Registered connection" {:connection-id connection-id})
    connection-id))

(defn get-connection
  "Get a connection."
  [connection-id]
  (when-let [conn (get-in @state [:connections connection-id])]
    (swap! state update-in [:connections connection-id]
           (fn [c]
             (-> c
                 (assoc :last-used (System/currentTimeMillis))
                 (update :use-count inc))))
    conn))

(defn close-connection!
  "Close a connection."
  [connection-id]
  (swap! state update-in [:connections connection-id] assoc :status :closed)
  (logging/log :info "Closed connection" {:connection-id connection-id}))

(defn list-connections
  "List all connections."
  []
  (mapv (fn [[id c]]
          {:id id
           :type (:type c)
           :status (:status c)
           :use-count (:use-count c)})
        (:connections @state)))

(defn cleanup-idle-connections!
  "Cleanup idle connections."
  [& {:keys [idle-threshold-ms] :or {idle-threshold-ms 300000}}]
  (let [now (System/currentTimeMillis)
        cutoff (- now idle-threshold-ms)
        idle-connections (filter (fn [[_ c]]
                                   (and (= :connected (:status c))
                                        (< (:last-used c) cutoff)))
                                 (:connections @state))]
    (doseq [[id _] idle-connections]
      (close-connection! id))
    (count idle-connections)))

;; ============================================================================
;; Quota Management
;; ============================================================================

(defn create-quota!
  "Create a resource quota."
  [quota-id config]
  (let [quota {:id quota-id
               :name (get config :name (name quota-id))
               :resource-type (get config :resource-type :generic)
               :limit (get config :limit 100)
               :period-ms (get config :period-ms 3600000) ;; 1 hour
               :action (get config :action :reject) ;; :reject, :queue, :throttle
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:quotas quota-id] quota)
    (logging/log :info "Created quota" {:quota-id quota-id :limit (:limit quota)})
    quota-id))

(defn check-quota
  "Check if quota allows the request."
  [quota-id user-id & {:keys [amount] :or {amount 1}}]
  (when-let [quota (get-in @state [:quotas quota-id])]
    (let [now (System/currentTimeMillis)
          period-start (- now (:period-ms quota))
          usage-key [quota-id user-id]
          current-usage (get-in @state [:usage usage-key] {:count 0 :window-start now})
          
          ;; Reset if window expired
          effective-usage (if (< (:window-start current-usage) period-start)
                            {:count 0 :window-start now}
                            current-usage)
          
          allowed? (<= (+ (:count effective-usage) amount) (:limit quota))]
      {:allowed? allowed?
       :current-usage (:count effective-usage)
       :limit (:limit quota)
       :remaining (- (:limit quota) (:count effective-usage))})))

(defn consume-quota!
  "Consume quota."
  [quota-id user-id & {:keys [amount] :or {amount 1}}]
  (let [check-result (check-quota quota-id user-id :amount amount)]
    (if (:allowed? check-result)
      (do
        (swap! state update-in [:usage [quota-id user-id]]
               (fn [u]
                 (let [current (or u {:count 0 :window-start (System/currentTimeMillis)})]
                   (update current :count + amount))))
        (assoc check-result :consumed true))
      (do
        (swap! state update-in [:stats :quota-exceeded] inc)
        (logging/log :warn "Quota exceeded" {:quota-id quota-id :user-id user-id})
        (assoc check-result :consumed false)))))

(defn get-quota-usage
  "Get quota usage for a user."
  [quota-id user-id]
  (let [quota (get-in @state [:quotas quota-id])
        usage (get-in @state [:usage [quota-id user-id]] {:count 0})]
    {:quota-id quota-id
     :user-id user-id
     :limit (:limit quota)
     :used (:count usage)
     :remaining (- (:limit quota) (:count usage))
     :percentage (/ (:count usage) (max 1 (:limit quota)))}))

;; ============================================================================
;; Resource Limits
;; ============================================================================

(defn set-limit!
  "Set a resource limit."
  [limit-id config]
  (let [limit {:id limit-id
               :name (get config :name (name limit-id))
               :resource-type (get config :resource-type :memory)
               :soft-limit (get config :soft-limit)
               :hard-limit (get config :hard-limit)
               :action (get config :action :warn)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:limits limit-id] limit)
    limit-id))

(defn check-limit
  "Check if a limit is exceeded."
  [limit-id current-value]
  (when-let [limit (get-in @state [:limits limit-id])]
    (let [soft-exceeded? (and (:soft-limit limit) (> current-value (:soft-limit limit)))
          hard-exceeded? (and (:hard-limit limit) (> current-value (:hard-limit limit)))]
      {:limit-id limit-id
       :current-value current-value
       :soft-limit (:soft-limit limit)
       :hard-limit (:hard-limit limit)
       :soft-exceeded? soft-exceeded?
       :hard-exceeded? hard-exceeded?
       :status (cond
                 hard-exceeded? :critical
                 soft-exceeded? :warning
                 :else :ok)})))

;; ============================================================================
;; Thread Pool Management
;; ============================================================================

(defn create-thread-pool!
  "Create a managed thread pool."
  [pool-id config]
  (let [core-size (get config :core-size 4)
        max-size (get config :max-size 10)
        queue-size (get config :queue-size 100)
        keep-alive-ms (get config :keep-alive-ms 60000)
        
        executor (ThreadPoolExecutor.
                  core-size
                  max-size
                  keep-alive-ms
                  TimeUnit/MILLISECONDS
                  (ArrayBlockingQueue. queue-size))
        
        pool {:id pool-id
              :name (get config :name (name pool-id))
              :executor executor
              :core-size core-size
              :max-size max-size
              :queue-size queue-size
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:pools (keyword (str "thread-" (name pool-id)))] pool)
    (logging/log :info "Created thread pool" {:pool-id pool-id})
    pool-id))

(defn submit-task!
  "Submit a task to a thread pool."
  [pool-id task-fn]
  (when-let [pool (get-in @state [:pools (keyword (str "thread-" (name pool-id)))])]
    (let [executor (:executor pool)]
      (.submit executor ^Runnable task-fn))))

(defn get-thread-pool-stats
  "Get thread pool statistics."
  [pool-id]
  (when-let [pool (get-in @state [:pools (keyword (str "thread-" (name pool-id)))])]
    (let [executor (:executor pool)]
      {:pool-id pool-id
       :active-count (.getActiveCount executor)
       :pool-size (.getPoolSize executor)
       :core-pool-size (.getCorePoolSize executor)
       :maximum-pool-size (.getMaximumPoolSize executor)
       :task-count (.getTaskCount executor)
       :completed-task-count (.getCompletedTaskCount executor)
       :queue-size (.size (.getQueue executor))})))

(defn shutdown-thread-pool!
  "Shutdown a thread pool."
  [pool-id & {:keys [wait-ms] :or {wait-ms 30000}}]
  (when-let [pool (get-in @state [:pools (keyword (str "thread-" (name pool-id)))])]
    (let [executor (:executor pool)]
      (.shutdown executor)
      (.awaitTermination executor wait-ms TimeUnit/MILLISECONDS)
      (logging/log :info "Shutdown thread pool" {:pool-id pool-id}))))

;; ============================================================================
;; Garbage Collection
;; ============================================================================

(defn trigger-gc!
  "Trigger garbage collection."
  []
  (System/gc)
  (logging/log :info "Triggered garbage collection"))

(defn get-memory-stats
  "Get memory statistics."
  []
  (let [runtime (Runtime/getRuntime)
        total (.totalMemory runtime)
        free (.freeMemory runtime)
        max (.maxMemory runtime)
        used (- total free)]
    {:total-bytes total
     :free-bytes free
     :used-bytes used
     :max-bytes max
     :usage-percentage (/ used (double max))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-resource-stats
  "Get resource manager statistics."
  []
  (let [stats (:stats @state)
        memory (get-memory-stats)]
    {:total-pools (count (:pools @state))
     :total-connections (count (:connections @state))
     :total-quotas (count (:quotas @state))
     :allocations (:allocations stats)
     :releases (:releases stats)
     :quota-exceeded (:quota-exceeded stats)
     :memory memory}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-resource-manager!
  "Initialize the resource manager."
  []
  (when-not (:initialized? @state)
    ;; Create default connection pool
    (create-pool! :http-connections
                  {:name "HTTP Connections"
                   :min-size 5
                   :max-size 50
                   :factory-fn (fn [] {:id (str (UUID/randomUUID)) :type :http})})
    
    ;; Create default thread pool
    (create-thread-pool! :analysis
                         {:name "Analysis Workers"
                          :core-size 4
                          :max-size 16
                          :queue-size 1000})
    
    ;; Create default quotas
    (create-quota! :api-requests
                   {:name "API Requests"
                    :resource-type :requests
                    :limit 1000
                    :period-ms 3600000})
    
    (create-quota! :analysis-jobs
                   {:name "Analysis Jobs"
                    :resource-type :jobs
                    :limit 100
                    :period-ms 3600000})
    
    ;; Set memory limits
    (set-limit! :memory
                {:name "Memory Usage"
                 :resource-type :memory
                 :soft-limit (* 0.7 (.maxMemory (Runtime/getRuntime)))
                 :hard-limit (* 0.9 (.maxMemory (Runtime/getRuntime)))
                 :action :gc})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Resource manager initialized")
    (events/emit! :resource-manager-initialized {})
    true))
