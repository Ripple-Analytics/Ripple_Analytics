(ns mental-models.pipeline.integration.bulkhead-pattern
  "Bulkhead Pattern Module
   
   Fault isolation:
   - Thread pool isolation
   - Semaphore isolation
   - Resource partitioning
   - Failure containment
   - Graceful degradation"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ExecutorService Semaphore TimeUnit ThreadPoolExecutor
    LinkedBlockingQueue RejectedExecutionException]))

;; =============================================================================
;; BULKHEAD STATE
;; =============================================================================

(defonce bulkhead-state (atom {:bulkheads {}
                               :config {:default-max-concurrent 10
                                        :default-max-wait-ms 1000
                                        :default-queue-size 100}}))

;; =============================================================================
;; SEMAPHORE BULKHEAD
;; =============================================================================

(defn create-semaphore-bulkhead
  "Create a semaphore-based bulkhead."
  [max-concurrent max-wait-ms]
  {:type :semaphore
   :semaphore (Semaphore. max-concurrent true)
   :max-concurrent max-concurrent
   :max-wait-ms max-wait-ms
   :active-count (atom 0)
   :rejected-count (atom 0)})

(defn try-acquire-semaphore!
  "Try to acquire a permit from semaphore bulkhead."
  [bulkhead]
  (let [^Semaphore sem (:semaphore bulkhead)
        acquired? (.tryAcquire sem (:max-wait-ms bulkhead) TimeUnit/MILLISECONDS)]
    (if acquired?
      (do
        (swap! (:active-count bulkhead) inc)
        true)
      (do
        (swap! (:rejected-count bulkhead) inc)
        false))))

(defn release-semaphore!
  "Release a permit to semaphore bulkhead."
  [bulkhead]
  (let [^Semaphore sem (:semaphore bulkhead)]
    (.release sem)
    (swap! (:active-count bulkhead) dec)))

;; =============================================================================
;; THREAD POOL BULKHEAD
;; =============================================================================

(defn create-thread-pool-bulkhead
  "Create a thread pool-based bulkhead."
  [core-size max-size queue-size]
  {:type :thread-pool
   :executor (ThreadPoolExecutor. core-size max-size
                                  60 TimeUnit/SECONDS
                                  (LinkedBlockingQueue. queue-size))
   :core-size core-size
   :max-size max-size
   :queue-size queue-size
   :rejected-count (atom 0)})

(defn submit-to-pool!
  "Submit a task to thread pool bulkhead."
  [bulkhead task-fn]
  (try
    (let [^ExecutorService executor (:executor bulkhead)
          future (.submit executor ^Callable task-fn)]
      {:success true :future future})
    (catch RejectedExecutionException _
      (swap! (:rejected-count bulkhead) inc)
      {:success false :error "Bulkhead full"})))

(defn shutdown-pool!
  "Shutdown thread pool bulkhead."
  [bulkhead]
  (let [^ExecutorService executor (:executor bulkhead)]
    (.shutdown executor)))

;; =============================================================================
;; BULKHEAD MANAGEMENT
;; =============================================================================

(defn register-bulkhead!
  "Register a bulkhead."
  [bulkhead-id {:keys [type max-concurrent max-wait-ms core-size max-size queue-size]}]
  (log/info "Registering bulkhead" {:id bulkhead-id :type type})
  (let [bulkhead (case type
                   :semaphore (create-semaphore-bulkhead
                               (or max-concurrent (get-in @bulkhead-state [:config :default-max-concurrent]))
                               (or max-wait-ms (get-in @bulkhead-state [:config :default-max-wait-ms])))
                   :thread-pool (create-thread-pool-bulkhead
                                 (or core-size (get-in @bulkhead-state [:config :default-max-concurrent]))
                                 (or max-size (* 2 (or core-size (get-in @bulkhead-state [:config :default-max-concurrent]))))
                                 (or queue-size (get-in @bulkhead-state [:config :default-queue-size])))
                   ;; Default to semaphore
                   (create-semaphore-bulkhead
                    (or max-concurrent (get-in @bulkhead-state [:config :default-max-concurrent]))
                    (or max-wait-ms (get-in @bulkhead-state [:config :default-max-wait-ms]))))]
    (swap! bulkhead-state assoc-in [:bulkheads bulkhead-id]
           (merge bulkhead
                  {:id bulkhead-id
                   :created-at (System/currentTimeMillis)}))
    (metrics/inc-counter! :bulkhead/bulkheads-registered)
    bulkhead-id))

(defn unregister-bulkhead!
  "Unregister a bulkhead."
  [bulkhead-id]
  (log/info "Unregistering bulkhead" {:id bulkhead-id})
  (when-let [bulkhead (get-in @bulkhead-state [:bulkheads bulkhead-id])]
    (when (= (:type bulkhead) :thread-pool)
      (shutdown-pool! bulkhead)))
  (swap! bulkhead-state update :bulkheads dissoc bulkhead-id))

(defn get-bulkhead
  "Get a bulkhead."
  [bulkhead-id]
  (get-in @bulkhead-state [:bulkheads bulkhead-id]))

(defn list-bulkheads
  "List all bulkheads."
  []
  (keys (:bulkheads @bulkhead-state)))

;; =============================================================================
;; BULKHEAD EXECUTION
;; =============================================================================

(defn execute-with-bulkhead
  "Execute a function with bulkhead protection."
  [bulkhead-id f]
  (when (flags/is-enabled? "bulkhead-pattern")
    (if-let [bulkhead (get-bulkhead bulkhead-id)]
      (case (:type bulkhead)
        :semaphore
        (if (try-acquire-semaphore! bulkhead)
          (try
            (metrics/inc-counter! :bulkhead/executions-allowed)
            {:success true :result (f)}
            (catch Exception e
              {:success false :error (.getMessage e)})
            (finally
              (release-semaphore! bulkhead)))
          (do
            (metrics/inc-counter! :bulkhead/executions-rejected)
            (events/publish! :bulkhead/rejected {:bulkhead bulkhead-id})
            {:success false :error "Bulkhead full"}))
        
        :thread-pool
        (let [result (submit-to-pool! bulkhead f)]
          (if (:success result)
            (do
              (metrics/inc-counter! :bulkhead/executions-allowed)
              (try
                {:success true :result (.get ^java.util.concurrent.Future (:future result))}
                (catch Exception e
                  {:success false :error (.getMessage e)})))
            (do
              (metrics/inc-counter! :bulkhead/executions-rejected)
              (events/publish! :bulkhead/rejected {:bulkhead bulkhead-id})
              result)))
        
        {:success false :error "Unknown bulkhead type"})
      {:success false :error "Bulkhead not found"})))

(defn try-execute
  "Try to execute with bulkhead, return nil if rejected."
  [bulkhead-id f]
  (let [result (execute-with-bulkhead bulkhead-id f)]
    (when (:success result)
      (:result result))))

;; =============================================================================
;; BULKHEAD MACROS
;; =============================================================================

(defmacro with-bulkhead
  "Execute body with bulkhead protection."
  [bulkhead-id & body]
  `(let [result# (execute-with-bulkhead ~bulkhead-id (fn [] ~@body))]
     (if (:success result#)
       (:result result#)
       (throw (ex-info "Bulkhead rejected" result#)))))

(defmacro try-with-bulkhead
  "Try to execute body with bulkhead, return nil if rejected."
  [bulkhead-id & body]
  `(try-execute ~bulkhead-id (fn [] ~@body)))

;; =============================================================================
;; BULKHEAD STATISTICS
;; =============================================================================

(defn get-bulkhead-stats
  "Get statistics for a bulkhead."
  [bulkhead-id]
  (when-let [bulkhead (get-bulkhead bulkhead-id)]
    (case (:type bulkhead)
      :semaphore
      {:id bulkhead-id
       :type :semaphore
       :max-concurrent (:max-concurrent bulkhead)
       :active @(:active-count bulkhead)
       :available (.availablePermits ^Semaphore (:semaphore bulkhead))
       :rejected @(:rejected-count bulkhead)}
      
      :thread-pool
      (let [^ThreadPoolExecutor executor (:executor bulkhead)]
        {:id bulkhead-id
         :type :thread-pool
         :core-size (:core-size bulkhead)
         :max-size (:max-size bulkhead)
         :queue-size (:queue-size bulkhead)
         :active (.getActiveCount executor)
         :pool-size (.getPoolSize executor)
         :queue-length (.size (.getQueue executor))
         :completed (.getCompletedTaskCount executor)
         :rejected @(:rejected-count bulkhead)})
      
      {:id bulkhead-id :type :unknown})))

(defn all-bulkhead-stats
  "Get statistics for all bulkheads."
  []
  (into {} (for [id (list-bulkheads)]
             [id (get-bulkhead-stats id)])))

;; =============================================================================
;; ADAPTIVE BULKHEAD
;; =============================================================================

(defn adjust-bulkhead-size!
  "Adjust bulkhead size based on metrics."
  [bulkhead-id new-size]
  (when-let [bulkhead (get-bulkhead bulkhead-id)]
    (log/info "Adjusting bulkhead size" {:id bulkhead-id :new-size new-size})
    (case (:type bulkhead)
      :semaphore
      ;; For semaphore, we need to recreate
      (let [current-max (:max-concurrent bulkhead)
            diff (- new-size current-max)
            ^Semaphore sem (:semaphore bulkhead)]
        (if (pos? diff)
          (.release sem diff)
          (dotimes [_ (Math/abs diff)]
            (.tryAcquire sem)))
        (swap! bulkhead-state assoc-in [:bulkheads bulkhead-id :max-concurrent] new-size))
      
      :thread-pool
      (let [^ThreadPoolExecutor executor (:executor bulkhead)]
        (.setCorePoolSize executor new-size)
        (.setMaximumPoolSize executor (* 2 new-size))
        (swap! bulkhead-state assoc-in [:bulkheads bulkhead-id :core-size] new-size)
        (swap! bulkhead-state assoc-in [:bulkheads bulkhead-id :max-size] (* 2 new-size)))
      
      nil)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-bulkhead-pattern!
  "Initialize bulkhead pattern."
  []
  (log/info "Initializing bulkhead pattern")
  ;; Register feature flag
  (flags/register-flag! "bulkhead-pattern" "Enable bulkhead pattern" true)
  ;; Create metrics
  (metrics/create-counter! :bulkhead/bulkheads-registered "Bulkheads registered")
  (metrics/create-counter! :bulkhead/executions-allowed "Executions allowed")
  (metrics/create-counter! :bulkhead/executions-rejected "Executions rejected")
  (metrics/create-gauge! :bulkhead/active-bulkheads "Active bulkheads"
                         #(count (:bulkheads @bulkhead-state)))
  (log/info "Bulkhead pattern initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-bulkhead-status []
  {:enabled (flags/is-enabled? "bulkhead-pattern")
   :bulkheads (count (:bulkheads @bulkhead-state))
   :stats (all-bulkhead-stats)
   :config (:config @bulkhead-state)})
