(ns mental-models.pipeline.integration.thread-pool-manager
  "Thread pool manager for mental model analysis system.
   
   Features:
   - Named thread pools
   - Pool sizing strategies
   - Thread monitoring
   - Pool metrics
   - Graceful shutdown
   - Thread naming
   - Rejection handling
   - Pool health checks"
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
           [java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue
            ArrayBlockingQueue SynchronousQueue ThreadFactory
            RejectedExecutionHandler Executors ScheduledThreadPoolExecutor]
           [java.util.concurrent.atomic AtomicInteger]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:pools {}            ;; pool-id -> pool
         :config {:default-core-size 4
                  :default-max-size 16
                  :default-keep-alive-ms 60000
                  :default-queue-size 1000
                  :monitor-interval-ms 10000}
         :stats {:tasks-submitted 0
                 :tasks-completed 0
                 :tasks-rejected 0
                 :pools-created 0}
         :initialized? false}))

;; ============================================================================
;; Thread Factory
;; ============================================================================

(defn- create-thread-factory
  "Create a named thread factory."
  [pool-name]
  (let [counter (AtomicInteger. 0)]
    (reify ThreadFactory
      (newThread [_ runnable]
        (let [thread (Thread. runnable)]
          (.setName thread (str pool-name "-" (.incrementAndGet counter)))
          (.setDaemon thread true)
          thread)))))

;; ============================================================================
;; Rejection Handlers
;; ============================================================================

(defn- create-rejection-handler
  "Create a rejection handler."
  [pool-id strategy]
  (case strategy
    :abort
    (reify RejectedExecutionHandler
      (rejectedExecution [_ runnable executor]
        (swap! state update-in [:stats :tasks-rejected] inc)
        (logging/log :warn "Task rejected (abort)" {:pool-id pool-id})
        (throw (java.util.concurrent.RejectedExecutionException. "Task rejected"))))
    
    :caller-runs
    (reify RejectedExecutionHandler
      (rejectedExecution [_ runnable executor]
        (swap! state update-in [:stats :tasks-rejected] inc)
        (logging/log :warn "Task rejected (caller-runs)" {:pool-id pool-id})
        (when-not (.isShutdown executor)
          (.run runnable))))
    
    :discard
    (reify RejectedExecutionHandler
      (rejectedExecution [_ runnable executor]
        (swap! state update-in [:stats :tasks-rejected] inc)
        (logging/log :warn "Task rejected (discard)" {:pool-id pool-id})))
    
    :discard-oldest
    (reify RejectedExecutionHandler
      (rejectedExecution [_ runnable executor]
        (swap! state update-in [:stats :tasks-rejected] inc)
        (logging/log :warn "Task rejected (discard-oldest)" {:pool-id pool-id})
        (when-not (.isShutdown executor)
          (.poll (.getQueue executor))
          (.execute executor runnable))))
    
    ;; Default to abort
    (create-rejection-handler pool-id :abort)))

;; ============================================================================
;; Queue Creation
;; ============================================================================

(defn- create-queue
  "Create a work queue."
  [queue-type queue-size]
  (case queue-type
    :linked (LinkedBlockingQueue. (int queue-size))
    :array (ArrayBlockingQueue. (int queue-size))
    :synchronous (SynchronousQueue.)
    (LinkedBlockingQueue. (int queue-size))))

;; ============================================================================
;; Pool Creation
;; ============================================================================

(defn create-pool!
  "Create a thread pool."
  [pool-id config]
  (let [core-size (get config :core-size (get-in @state [:config :default-core-size]))
        max-size (get config :max-size (get-in @state [:config :default-max-size]))
        keep-alive-ms (get config :keep-alive-ms (get-in @state [:config :default-keep-alive-ms]))
        queue-size (get config :queue-size (get-in @state [:config :default-queue-size]))
        queue-type (get config :queue-type :linked)
        rejection-strategy (get config :rejection-strategy :abort)
        
        thread-factory (create-thread-factory (name pool-id))
        rejection-handler (create-rejection-handler pool-id rejection-strategy)
        work-queue (create-queue queue-type queue-size)
        
        executor (ThreadPoolExecutor.
                  core-size
                  max-size
                  keep-alive-ms
                  TimeUnit/MILLISECONDS
                  work-queue
                  thread-factory
                  rejection-handler)
        
        pool {:id pool-id
              :name (get config :name (name pool-id))
              :executor executor
              :core-size core-size
              :max-size max-size
              :keep-alive-ms keep-alive-ms
              :queue-size queue-size
              :rejection-strategy rejection-strategy
              :metrics {:submitted (atom 0)
                        :completed (atom 0)
                        :rejected (atom 0)}
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:pools pool-id] pool)
    (swap! state update-in [:stats :pools-created] inc)
    (logging/log :info "Created thread pool" {:pool-id pool-id
                                               :core-size core-size
                                               :max-size max-size})
    (events/emit! :thread-pool-created {:pool-id pool-id})
    pool-id))

(defn get-pool
  "Get a thread pool."
  [pool-id]
  (get-in @state [:pools pool-id]))

(defn list-pools
  "List all thread pools."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :core-size (:core-size p)
           :max-size (:max-size p)
           :active-count (.getActiveCount (:executor p))
           :pool-size (.getPoolSize (:executor p))
           :queue-size (.size (.getQueue (:executor p)))})
        (:pools @state)))

(defn delete-pool!
  "Delete a thread pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (shutdown-pool! pool-id)
    (swap! state update :pools dissoc pool-id)
    (logging/log :info "Deleted thread pool" {:pool-id pool-id})))

;; ============================================================================
;; Task Submission
;; ============================================================================

(defn submit!
  "Submit a task to a thread pool."
  [pool-id task-fn]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (swap! (get-in pool [:metrics :submitted]) inc)
      (swap! state update-in [:stats :tasks-submitted] inc)
      
      (.submit executor
               (fn []
                 (try
                   (task-fn)
                   (finally
                     (swap! (get-in pool [:metrics :completed]) inc)
                     (swap! state update-in [:stats :tasks-completed] inc))))))))

(defn execute!
  "Execute a task on a thread pool (fire and forget)."
  [pool-id task-fn]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (swap! (get-in pool [:metrics :submitted]) inc)
      (swap! state update-in [:stats :tasks-submitted] inc)
      
      (.execute executor
                (fn []
                  (try
                    (task-fn)
                    (finally
                      (swap! (get-in pool [:metrics :completed]) inc)
                      (swap! state update-in [:stats :tasks-completed] inc))))))))

(defn submit-callable!
  "Submit a callable task and return a Future."
  [pool-id callable-fn]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (swap! (get-in pool [:metrics :submitted]) inc)
      (swap! state update-in [:stats :tasks-submitted] inc)
      
      (.submit executor
               (reify java.util.concurrent.Callable
                 (call [_]
                   (try
                     (let [result (callable-fn)]
                       (swap! (get-in pool [:metrics :completed]) inc)
                       (swap! state update-in [:stats :tasks-completed] inc)
                       result)
                     (catch Exception e
                       (swap! (get-in pool [:metrics :completed]) inc)
                       (swap! state update-in [:stats :tasks-completed] inc)
                       (throw e)))))))))

;; ============================================================================
;; Pool Control
;; ============================================================================

(defn shutdown-pool!
  "Shutdown a thread pool gracefully."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (.shutdown executor)
      (logging/log :info "Initiated pool shutdown" {:pool-id pool-id}))))

(defn shutdown-pool-now!
  "Shutdown a thread pool immediately."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (.shutdownNow executor)
      (logging/log :info "Forced pool shutdown" {:pool-id pool-id}))))

(defn await-termination!
  "Wait for pool termination."
  [pool-id timeout-ms]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      (.awaitTermination executor timeout-ms TimeUnit/MILLISECONDS))))

(defn is-shutdown?
  "Check if a pool is shutdown."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (.isShutdown (:executor pool))))

(defn is-terminated?
  "Check if a pool is terminated."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (.isTerminated (:executor pool))))

;; ============================================================================
;; Pool Sizing
;; ============================================================================

(defn set-core-size!
  "Set the core pool size."
  [pool-id core-size]
  (when-let [pool (get-pool pool-id)]
    (.setCorePoolSize (:executor pool) core-size)
    (swap! state assoc-in [:pools pool-id :core-size] core-size)
    (logging/log :info "Set core pool size" {:pool-id pool-id :core-size core-size})))

(defn set-max-size!
  "Set the maximum pool size."
  [pool-id max-size]
  (when-let [pool (get-pool pool-id)]
    (.setMaximumPoolSize (:executor pool) max-size)
    (swap! state assoc-in [:pools pool-id :max-size] max-size)
    (logging/log :info "Set max pool size" {:pool-id pool-id :max-size max-size})))

(defn prestartAllCoreThreads!
  "Prestart all core threads."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (.prestartAllCoreThreads (:executor pool))))

;; ============================================================================
;; Pool Metrics
;; ============================================================================

(defn get-pool-metrics
  "Get metrics for a thread pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)]
      {:pool-id pool-id
       :name (:name pool)
       :core-size (.getCorePoolSize executor)
       :max-size (.getMaximumPoolSize executor)
       :pool-size (.getPoolSize executor)
       :active-count (.getActiveCount executor)
       :largest-pool-size (.getLargestPoolSize executor)
       :task-count (.getTaskCount executor)
       :completed-task-count (.getCompletedTaskCount executor)
       :queue-size (.size (.getQueue executor))
       :queue-remaining-capacity (.remainingCapacity (.getQueue executor))
       :is-shutdown (.isShutdown executor)
       :is-terminated (.isTerminated executor)
       :submitted @(get-in pool [:metrics :submitted])
       :completed @(get-in pool [:metrics :completed])
       :rejected @(get-in pool [:metrics :rejected])})))

(defn get-all-pool-metrics
  "Get metrics for all thread pools."
  []
  (mapv (fn [[id _]] (get-pool-metrics id)) (:pools @state)))

;; ============================================================================
;; Pool Health
;; ============================================================================

(defn is-pool-healthy?
  "Check if a pool is healthy."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)
          queue-size (.size (.getQueue executor))
          queue-capacity (:queue-size pool)
          active-count (.getActiveCount executor)
          max-size (.getMaximumPoolSize executor)]
      (and (not (.isShutdown executor))
           (< queue-size (* 0.9 queue-capacity))
           (< active-count max-size)))))

(defn get-pool-health
  "Get health status for a pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [executor (:executor pool)
          queue-size (.size (.getQueue executor))
          queue-capacity (:queue-size pool)
          active-count (.getActiveCount executor)
          max-size (.getMaximumPoolSize executor)]
      {:pool-id pool-id
       :healthy? (is-pool-healthy? pool-id)
       :queue-utilization (/ queue-size queue-capacity)
       :thread-utilization (/ active-count max-size)
       :is-shutdown (.isShutdown executor)})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-thread-pool-stats
  "Get thread pool manager statistics."
  []
  (let [stats (:stats @state)]
    {:pools-count (count (:pools @state))
     :pools-created (:pools-created stats)
     :tasks-submitted (:tasks-submitted stats)
     :tasks-completed (:tasks-completed stats)
     :tasks-rejected (:tasks-rejected stats)
     :completion-rate (if (pos? (:tasks-submitted stats))
                        (/ (:tasks-completed stats) (:tasks-submitted stats))
                        1.0)}))

(defn reset-stats!
  "Reset thread pool manager statistics."
  []
  (swap! state assoc :stats {:tasks-submitted 0
                             :tasks-completed 0
                             :tasks-rejected 0
                             :pools-created 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-thread-pool-manager!
  "Initialize the thread pool manager."
  []
  (when-not (:initialized? @state)
    ;; Create default pools
    (create-pool! :default
                  {:name "Default"
                   :core-size 4
                   :max-size 16
                   :queue-size 1000})
    
    (create-pool! :io
                  {:name "IO"
                   :core-size 8
                   :max-size 32
                   :queue-size 500
                   :rejection-strategy :caller-runs})
    
    (create-pool! :compute
                  {:name "Compute"
                   :core-size 4
                   :max-size 8
                   :queue-size 100
                   :rejection-strategy :abort})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Thread pool manager initialized")
    (events/emit! :thread-pool-manager-initialized {})
    true))

(defn shutdown-thread-pool-manager!
  "Shutdown all thread pools."
  []
  (doseq [[id _] (:pools @state)]
    (shutdown-pool! id))
  (swap! state assoc :initialized? false)
  (logging/log :info "Thread pool manager shutdown"))
