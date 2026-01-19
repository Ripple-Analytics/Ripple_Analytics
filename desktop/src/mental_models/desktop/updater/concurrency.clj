(ns mental-models.desktop.updater.concurrency
  "Concurrency Module - Handle concurrent update operations safely
   
   Covers Category 14: Concurrency
   - Simultaneous update checks prevention
   - UI responsiveness during updates
   - Update cancellation support
   - Multiple instance detection
   - Scheduler stacking prevention
   - Thread-safe state management"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout alts! put!]]
            [taoensso.timbre :as log])
  (:import [java.util.concurrent.atomic AtomicBoolean AtomicInteger AtomicReference]
           [java.util.concurrent Semaphore TimeUnit ConcurrentHashMap Executors ScheduledExecutorService]
           [java.time Instant]
           [java.io File RandomAccessFile]
           [java.nio.channels FileLock]
           [java.lang.management ManagementFactory]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def concurrency-config
  (atom {:max-concurrent-downloads 1
         :max-concurrent-checks 1
         :ui-update-interval-ms 100
         :cancellation-check-interval-ms 50
         :lock-file-path (str (System/getProperty "user.home") "/.mental-models/update.lock")
         :instance-check-interval-ms 5000}))

;; =============================================================================
;; Atomic State Management
;; =============================================================================

(defonce operation-state
  "Thread-safe operation state"
  {:check-in-progress (AtomicBoolean. false)
   :download-in-progress (AtomicBoolean. false)
   :install-in-progress (AtomicBoolean. false)
   :cancellation-requested (AtomicBoolean. false)
   :active-operations (AtomicInteger. 0)
   :current-operation (AtomicReference. nil)})

(defn is-check-in-progress? []
  (.get (:check-in-progress operation-state)))

(defn is-download-in-progress? []
  (.get (:download-in-progress operation-state)))

(defn is-install-in-progress? []
  (.get (:install-in-progress operation-state)))

(defn is-any-operation-in-progress? []
  (or (is-check-in-progress?)
      (is-download-in-progress?)
      (is-install-in-progress?)))

(defn is-cancellation-requested? []
  (.get (:cancellation-requested operation-state)))

(defn get-active-operation-count []
  (.get (:active-operations operation-state)))

(defn get-current-operation []
  (.get (:current-operation operation-state)))

;; =============================================================================
;; Operation Locking
;; =============================================================================

(defn try-start-operation!
  "Try to start an operation, returns true if successful"
  [operation-type]
  (let [flag (case operation-type
               :check (:check-in-progress operation-state)
               :download (:download-in-progress operation-state)
               :install (:install-in-progress operation-state)
               nil)]
    (when flag
      (if (.compareAndSet flag false true)
        (do
          (.incrementAndGet (:active-operations operation-state))
          (.set (:current-operation operation-state) operation-type)
          (.set (:cancellation-requested operation-state) false)
          (log/debug "Started operation:" operation-type)
          true)
        (do
          (log/warn "Operation already in progress:" operation-type)
          false)))))

(defn end-operation!
  "End an operation"
  [operation-type]
  (let [flag (case operation-type
               :check (:check-in-progress operation-state)
               :download (:download-in-progress operation-state)
               :install (:install-in-progress operation-state)
               nil)]
    (when flag
      (.set flag false)
      (.decrementAndGet (:active-operations operation-state))
      (.compareAndSet (:current-operation operation-state) operation-type nil)
      (log/debug "Ended operation:" operation-type))))

(defmacro with-operation
  "Execute body within an operation lock"
  [operation-type & body]
  `(if (try-start-operation! ~operation-type)
     (try
       ~@body
       (finally
         (end-operation! ~operation-type)))
     {:success false :error "Operation already in progress"}))

;; =============================================================================
;; Cancellation Support
;; =============================================================================

(defn request-cancellation!
  "Request cancellation of current operation"
  []
  (when (is-any-operation-in-progress?)
    (.set (:cancellation-requested operation-state) true)
    (log/info "Cancellation requested for:" (get-current-operation))
    true))

(defn clear-cancellation!
  "Clear cancellation request"
  []
  (.set (:cancellation-requested operation-state) false))

(defn check-cancellation!
  "Check if cancellation was requested, throws if so"
  []
  (when (is-cancellation-requested?)
    (throw (ex-info "Operation cancelled by user" {:type :cancelled}))))

(defn cancellable-loop
  "Execute a loop that can be cancelled"
  [items process-fn & {:keys [on-cancel on-progress]}]
  (let [total (count items)
        processed (atom 0)]
    (loop [remaining items]
      (if (empty? remaining)
        {:success true :processed @processed}
        (if (is-cancellation-requested?)
          (do
            (when on-cancel (on-cancel @processed total))
            {:success false :cancelled true :processed @processed})
          (do
            (process-fn (first remaining))
            (swap! processed inc)
            (when on-progress (on-progress @processed total))
            (recur (rest remaining))))))))

;; =============================================================================
;; Semaphore-based Rate Limiting
;; =============================================================================

(defonce download-semaphore 
  (Semaphore. 1 true))  ;; Fair semaphore for downloads

(defonce check-semaphore
  (Semaphore. 1 true))  ;; Fair semaphore for checks

(defn acquire-download-permit!
  "Acquire permit for download operation"
  [timeout-ms]
  (try
    (.tryAcquire download-semaphore timeout-ms TimeUnit/MILLISECONDS)
    (catch InterruptedException _
      false)))

(defn release-download-permit!
  "Release download permit"
  []
  (.release download-semaphore))

(defn acquire-check-permit!
  "Acquire permit for check operation"
  [timeout-ms]
  (try
    (.tryAcquire check-semaphore timeout-ms TimeUnit/MILLISECONDS)
    (catch InterruptedException _
      false)))

(defn release-check-permit!
  "Release check permit"
  []
  (.release check-semaphore))

(defmacro with-download-permit
  "Execute body with download permit"
  [timeout-ms & body]
  `(if (acquire-download-permit! ~timeout-ms)
     (try
       ~@body
       (finally
         (release-download-permit!)))
     {:success false :error "Could not acquire download permit"}))

;; =============================================================================
;; Multiple Instance Detection
;; =============================================================================

(defonce instance-lock (atom nil))
(defonce lock-file (atom nil))
(defonce lock-channel (atom nil))

(defn get-process-id []
  "Get current process ID"
  (let [runtime-name (.getName (ManagementFactory/getRuntimeMXBean))]
    (first (clojure.string/split runtime-name #"@"))))

(defn acquire-instance-lock!
  "Try to acquire exclusive instance lock"
  []
  (let [lock-path (:lock-file-path @concurrency-config)
        lock-file-obj (File. lock-path)]
    (try
      (io/make-parents lock-file-obj)
      (let [raf (RandomAccessFile. lock-file-obj "rw")
            channel (.getChannel raf)
            lock (.tryLock channel)]
        (if lock
          (do
            (reset! lock-file raf)
            (reset! lock-channel channel)
            (reset! instance-lock lock)
            ;; Write PID to lock file
            (.seek raf 0)
            (.writeBytes raf (str (get-process-id) "\n"))
            (log/info "Acquired instance lock")
            true)
          (do
            (.close channel)
            (.close raf)
            (log/warn "Another instance is running")
            false)))
      (catch Exception e
        (log/error e "Failed to acquire instance lock")
        false))))

(defn release-instance-lock!
  "Release instance lock"
  []
  (try
    (when @instance-lock
      (.release @instance-lock)
      (reset! instance-lock nil))
    (when @lock-channel
      (.close @lock-channel)
      (reset! lock-channel nil))
    (when @lock-file
      (.close @lock-file)
      (reset! lock-file nil))
    (log/info "Released instance lock")
    true
    (catch Exception e
      (log/error e "Error releasing instance lock")
      false)))

(defn is-another-instance-running?
  "Check if another instance is running"
  []
  (let [lock-path (:lock-file-path @concurrency-config)
        lock-file-obj (File. lock-path)]
    (when (.exists lock-file-obj)
      (try
        (let [raf (RandomAccessFile. lock-file-obj "rw")
              channel (.getChannel raf)
              lock (.tryLock channel)]
          (if lock
            (do
              (.release lock)
              (.close channel)
              (.close raf)
              false)
            (do
              (.close channel)
              (.close raf)
              true)))
        (catch Exception _
          true)))))

(defn get-other-instance-pid
  "Get PID of other running instance"
  []
  (let [lock-path (:lock-file-path @concurrency-config)
        lock-file-obj (File. lock-path)]
    (when (.exists lock-file-obj)
      (try
        (clojure.string/trim (slurp lock-file-obj))
        (catch Exception _ nil)))))

;; =============================================================================
;; Scheduler Stacking Prevention
;; =============================================================================

(defonce scheduled-tasks (ConcurrentHashMap.))
(defonce scheduler (atom nil))

(defn get-scheduler []
  "Get or create the scheduler"
  (when (nil? @scheduler)
    (reset! scheduler (Executors/newScheduledThreadPool 2)))
  @scheduler)

(defn schedule-task!
  "Schedule a task, preventing stacking"
  [task-id delay-ms task-fn]
  (let [existing (.get scheduled-tasks task-id)]
    ;; Cancel existing task if any
    (when existing
      (.cancel existing false)
      (log/debug "Cancelled existing scheduled task:" task-id))
    
    ;; Schedule new task
    (let [future (.schedule (get-scheduler)
                            (fn []
                              (.remove scheduled-tasks task-id)
                              (try
                                (task-fn)
                                (catch Exception e
                                  (log/error e "Scheduled task error:" task-id))))
                            delay-ms
                            TimeUnit/MILLISECONDS)]
      (.put scheduled-tasks task-id future)
      (log/debug "Scheduled task:" task-id "delay:" delay-ms "ms")
      future)))

(defn schedule-recurring-task!
  "Schedule a recurring task, preventing stacking"
  [task-id initial-delay-ms period-ms task-fn]
  (let [existing (.get scheduled-tasks task-id)]
    (when existing
      (.cancel existing false)
      (log/debug "Cancelled existing recurring task:" task-id))
    
    (let [future (.scheduleAtFixedRate 
                   (get-scheduler)
                   (fn []
                     (try
                       (task-fn)
                       (catch Exception e
                         (log/error e "Recurring task error:" task-id))))
                   initial-delay-ms
                   period-ms
                   TimeUnit/MILLISECONDS)]
      (.put scheduled-tasks task-id future)
      (log/debug "Scheduled recurring task:" task-id "period:" period-ms "ms")
      future)))

(defn cancel-scheduled-task!
  "Cancel a scheduled task"
  [task-id]
  (when-let [future (.remove scheduled-tasks task-id)]
    (.cancel future false)
    (log/debug "Cancelled task:" task-id)
    true))

(defn cancel-all-scheduled-tasks!
  "Cancel all scheduled tasks"
  []
  (doseq [task-id (keys scheduled-tasks)]
    (cancel-scheduled-task! task-id))
  (log/info "Cancelled all scheduled tasks"))

;; =============================================================================
;; UI Responsiveness
;; =============================================================================

(defn run-with-ui-updates
  "Run a long operation while keeping UI responsive"
  [operation-fn progress-callback & {:keys [update-interval-ms] 
                                      :or {update-interval-ms 100}}]
  (let [result-chan (chan 1)
        progress-chan (chan 100)
        done (AtomicBoolean. false)]
    
    ;; Progress reporter
    (go-loop []
      (when-not (.get done)
        (when-let [progress (<! progress-chan)]
          (try
            (progress-callback progress)
            (catch Exception e
              (log/warn "Progress callback error:" (.getMessage e))))
          (<! (timeout update-interval-ms))
          (recur))))
    
    ;; Main operation
    (go
      (try
        (let [result (operation-fn 
                       (fn [progress]
                         (put! progress-chan progress)))]
          (.set done true)
          (close! progress-chan)
          (>! result-chan result))
        (catch Exception e
          (.set done true)
          (close! progress-chan)
          (>! result-chan {:success false :error (.getMessage e)}))))
    
    result-chan))

(defn yield-to-ui
  "Yield execution to allow UI updates"
  []
  (Thread/sleep 1))

;; =============================================================================
;; Thread-Safe Update Queue
;; =============================================================================

(defonce update-queue (chan 100))
(defonce queue-processor-running (AtomicBoolean. false))

(defn enqueue-update!
  "Enqueue an update operation"
  [operation]
  (put! update-queue operation)
  (log/debug "Enqueued update operation:" (:type operation)))

(defn start-queue-processor!
  "Start the update queue processor"
  []
  (when (.compareAndSet queue-processor-running false true)
    (go-loop []
      (when (.get queue-processor-running)
        (when-let [operation (<! update-queue)]
          (log/debug "Processing queued operation:" (:type operation))
          (try
            (when-let [handler (:handler operation)]
              (handler (:data operation)))
            (catch Exception e
              (log/error e "Queue processor error")))
          (recur))))
    (log/info "Update queue processor started")))

(defn stop-queue-processor!
  "Stop the update queue processor"
  []
  (.set queue-processor-running false)
  (log/info "Update queue processor stopped"))

;; =============================================================================
;; Concurrent Check Prevention
;; =============================================================================

(defonce last-check-time (AtomicReference. nil))
(def min-check-interval-ms 60000)  ;; Minimum 1 minute between checks

(defn can-check-now?
  "Check if enough time has passed since last check"
  []
  (let [last-time (.get last-check-time)]
    (or (nil? last-time)
        (> (- (System/currentTimeMillis) last-time) min-check-interval-ms))))

(defn record-check-time!
  "Record the time of a check"
  []
  (.set last-check-time (System/currentTimeMillis)))

(defn time-until-next-check
  "Get milliseconds until next check is allowed"
  []
  (let [last-time (.get last-check-time)]
    (if (nil? last-time)
      0
      (max 0 (- min-check-interval-ms (- (System/currentTimeMillis) last-time))))))

;; =============================================================================
;; Safe Update Check
;; =============================================================================

(defn safe-check-for-updates!
  "Perform update check with all concurrency protections"
  [check-fn]
  (cond
    (not (can-check-now?))
    {:success false 
     :error "Too soon since last check" 
     :retry-after-ms (time-until-next-check)}
    
    (is-check-in-progress?)
    {:success false :error "Check already in progress"}
    
    (is-download-in-progress?)
    {:success false :error "Download in progress"}
    
    (is-install-in-progress?)
    {:success false :error "Installation in progress"}
    
    :else
    (with-operation :check
      (record-check-time!)
      (check-fn))))

;; =============================================================================
;; Safe Download
;; =============================================================================

(defn safe-download-update!
  "Perform download with all concurrency protections"
  [download-fn progress-callback]
  (cond
    (is-download-in-progress?)
    {:success false :error "Download already in progress"}
    
    (is-install-in-progress?)
    {:success false :error "Installation in progress"}
    
    :else
    (with-operation :download
      (with-download-permit 30000
        (run-with-ui-updates download-fn progress-callback)))))

;; =============================================================================
;; Status
;; =============================================================================

(defn get-concurrency-status
  "Get current concurrency status"
  []
  {:check-in-progress (is-check-in-progress?)
   :download-in-progress (is-download-in-progress?)
   :install-in-progress (is-install-in-progress?)
   :cancellation-requested (is-cancellation-requested?)
   :active-operations (get-active-operation-count)
   :current-operation (get-current-operation)
   :another-instance-running (is-another-instance-running?)
   :scheduled-tasks (count scheduled-tasks)
   :time-until-next-check (time-until-next-check)
   :queue-processor-running (.get queue-processor-running)})

;; =============================================================================
;; Initialization & Cleanup
;; =============================================================================

(defn init-concurrency!
  "Initialize concurrency management"
  []
  (log/info "Initializing concurrency management")
  (if (acquire-instance-lock!)
    (do
      (start-queue-processor!)
      {:success true :single-instance true})
    {:success false :error "Another instance is running"}))

(defn shutdown-concurrency!
  "Shutdown concurrency management"
  []
  (log/info "Shutting down concurrency management")
  (cancel-all-scheduled-tasks!)
  (stop-queue-processor!)
  (release-instance-lock!)
  (when @scheduler
    (.shutdown @scheduler)
    (reset! scheduler nil))
  {:success true})
