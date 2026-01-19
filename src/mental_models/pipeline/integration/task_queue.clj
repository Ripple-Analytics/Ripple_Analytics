(ns mental-models.pipeline.integration.task-queue
  "Task Queue Module
   
   Background job processing:
   - Priority-based task queuing
   - Worker pool management
   - Task scheduling
   - Dead letter queue
   - Task persistence"
  (:require
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent PriorityBlockingQueue Executors TimeUnit]))

;; =============================================================================
;; TASK QUEUE STATE
;; =============================================================================

(defonce queue-state (atom {:queues {}
                            :workers {}
                            :tasks {}
                            :dead-letter []
                            :stats {:enqueued 0
                                    :processed 0
                                    :failed 0}}))

;; =============================================================================
;; TASK DEFINITION
;; =============================================================================

(defn create-task
  "Create a new task."
  [task-type payload & {:keys [priority delay-ms max-retries]
                        :or {priority 5 delay-ms 0 max-retries 3}}]
  {:id (str (java.util.UUID/randomUUID))
   :type task-type
   :payload payload
   :priority priority
   :delay-ms delay-ms
   :max-retries max-retries
   :retries 0
   :status :pending
   :created-at (System/currentTimeMillis)
   :scheduled-at (+ (System/currentTimeMillis) delay-ms)})

;; =============================================================================
;; QUEUE MANAGEMENT
;; =============================================================================

(defn create-queue!
  "Create a new task queue."
  [queue-name & {:keys [max-size] :or {max-size 10000}}]
  (log/info "Creating queue" {:name queue-name :max-size max-size})
  (let [queue (chan max-size)]
    (swap! queue-state assoc-in [:queues queue-name] {:channel queue
                                                       :max-size max-size
                                                       :created-at (System/currentTimeMillis)})
    queue-name))

(defn get-queue
  "Get a queue by name."
  [queue-name]
  (get-in @queue-state [:queues queue-name :channel]))

(defn delete-queue!
  "Delete a queue."
  [queue-name]
  (when-let [queue (get-queue queue-name)]
    (log/info "Deleting queue" {:name queue-name})
    (close! queue)
    (swap! queue-state update :queues dissoc queue-name)))

;; =============================================================================
;; TASK OPERATIONS
;; =============================================================================

(defn enqueue!
  "Add a task to a queue."
  [queue-name task]
  (when (flags/is-enabled? "task-queue")
    (if-let [queue (get-queue queue-name)]
      (do
        (log/debug "Enqueueing task" {:queue queue-name :task-id (:id task)})
        ;; Store task
        (swap! queue-state assoc-in [:tasks (:id task)] task)
        ;; Update stats
        (swap! queue-state update-in [:stats :enqueued] inc)
        ;; Record metrics
        (metrics/inc-counter! :task-queue/enqueued)
        ;; Put on queue
        (go (>! queue task))
        ;; Publish event
        (events/publish! :task/enqueued {:queue queue-name :task-id (:id task)})
        (:id task))
      (do
        (log/warn "Queue not found" {:queue queue-name})
        nil))))

(defn enqueue-task!
  "Create and enqueue a task."
  [queue-name task-type payload & opts]
  (let [task (apply create-task task-type payload opts)]
    (enqueue! queue-name task)))

(defn update-task-status!
  "Update task status."
  [task-id status & {:keys [result error]}]
  (swap! queue-state update-in [:tasks task-id]
         merge {:status status
                :updated-at (System/currentTimeMillis)
                :result result
                :error error}))

(defn get-task
  "Get a task by ID."
  [task-id]
  (get-in @queue-state [:tasks task-id]))

;; =============================================================================
;; WORKER MANAGEMENT
;; =============================================================================

(defn create-worker!
  "Create a worker for a queue."
  [queue-name handler & {:keys [worker-id] :or {worker-id (str (java.util.UUID/randomUUID))}}]
  (log/info "Creating worker" {:queue queue-name :worker-id worker-id})
  (let [queue (get-queue queue-name)
        control-chan (chan)
        worker (go-loop []
                 (let [[v ch] (async/alts! [queue control-chan])]
                   (cond
                     (= ch control-chan)
                     (log/info "Worker stopping" {:worker-id worker-id})
                     
                     (nil? v)
                     (log/info "Queue closed, worker stopping" {:worker-id worker-id})
                     
                     :else
                     (do
                       (log/debug "Processing task" {:worker-id worker-id :task-id (:id v)})
                       (update-task-status! (:id v) :processing)
                       (try
                         (let [result (handler v)]
                           (update-task-status! (:id v) :completed :result result)
                           (swap! queue-state update-in [:stats :processed] inc)
                           (metrics/inc-counter! :task-queue/processed)
                           (events/publish! :task/completed {:task-id (:id v)}))
                         (catch Exception e
                           (log/error "Task failed" {:task-id (:id v) :error (.getMessage e)})
                           (handle-task-failure! queue-name v e)))
                       (recur)))))]
    (swap! queue-state assoc-in [:workers worker-id]
           {:id worker-id
            :queue queue-name
            :control-chan control-chan
            :started-at (System/currentTimeMillis)})
    worker-id))

(defn stop-worker!
  "Stop a worker."
  [worker-id]
  (when-let [worker (get-in @queue-state [:workers worker-id])]
    (log/info "Stopping worker" {:worker-id worker-id})
    (>!! (:control-chan worker) :stop)
    (close! (:control-chan worker))
    (swap! queue-state update :workers dissoc worker-id)))

(defn create-worker-pool!
  "Create a pool of workers for a queue."
  [queue-name handler & {:keys [size] :or {size 4}}]
  (log/info "Creating worker pool" {:queue queue-name :size size})
  (let [worker-ids (doall
                    (for [i (range size)]
                      (create-worker! queue-name handler
                                      :worker-id (str queue-name "-worker-" i))))]
    worker-ids))

;; =============================================================================
;; FAILURE HANDLING
;; =============================================================================

(defn handle-task-failure!
  "Handle a failed task."
  [queue-name task error]
  (let [retries (inc (:retries task))
        max-retries (:max-retries task)]
    (if (< retries max-retries)
      ;; Retry with exponential backoff
      (let [delay-ms (* 1000 (Math/pow 2 retries))
            retry-task (assoc task
                              :retries retries
                              :delay-ms delay-ms
                              :scheduled-at (+ (System/currentTimeMillis) delay-ms))]
        (log/warn "Retrying task" {:task-id (:id task) :retry retries :delay-ms delay-ms})
        (update-task-status! (:id task) :retrying)
        (go
          (<! (timeout delay-ms))
          (enqueue! queue-name retry-task)))
      ;; Move to dead letter queue
      (do
        (log/error "Task moved to dead letter queue" {:task-id (:id task)})
        (update-task-status! (:id task) :dead-letter :error (.getMessage error))
        (swap! queue-state update :dead-letter conj
               (assoc task :error (.getMessage error) :failed-at (System/currentTimeMillis)))
        (swap! queue-state update-in [:stats :failed] inc)
        (metrics/inc-counter! :task-queue/failed)
        (events/publish! :task/dead-letter {:task-id (:id task)})))))

(defn retry-dead-letter!
  "Retry a task from the dead letter queue."
  [task-id queue-name]
  (let [dead-tasks (:dead-letter @queue-state)
        task (first (filter #(= (:id %) task-id) dead-tasks))]
    (when task
      (log/info "Retrying dead letter task" {:task-id task-id})
      (swap! queue-state update :dead-letter
             (fn [tasks] (remove #(= (:id %) task-id) tasks)))
      (enqueue! queue-name (assoc task :retries 0 :status :pending)))))

(defn clear-dead-letter!
  "Clear the dead letter queue."
  []
  (log/warn "Clearing dead letter queue")
  (swap! queue-state assoc :dead-letter []))

;; =============================================================================
;; SCHEDULED TASKS
;; =============================================================================

(defonce scheduler (atom nil))

(defn schedule-task!
  "Schedule a task for future execution."
  [queue-name task-type payload delay-ms & opts]
  (let [task (apply create-task task-type payload :delay-ms delay-ms opts)]
    (log/info "Scheduling task" {:task-id (:id task) :delay-ms delay-ms})
    (go
      (<! (timeout delay-ms))
      (enqueue! queue-name task))
    (:id task)))

(defn schedule-recurring!
  "Schedule a recurring task."
  [queue-name task-type payload interval-ms & opts]
  (log/info "Scheduling recurring task" {:type task-type :interval-ms interval-ms})
  (let [control-chan (chan)]
    (go-loop []
      (let [[v ch] (async/alts! [(timeout interval-ms) control-chan])]
        (when-not (= ch control-chan)
          (apply enqueue-task! queue-name task-type payload opts)
          (recur))))
    control-chan))

;; =============================================================================
;; QUEUE STATS
;; =============================================================================

(defn get-queue-stats
  "Get statistics for a queue."
  [queue-name]
  (let [queue-info (get-in @queue-state [:queues queue-name])
        workers (filter #(= (:queue (val %)) queue-name) (:workers @queue-state))
        tasks (filter #(= (:queue (val %)) queue-name) (:tasks @queue-state))]
    {:queue-name queue-name
     :created-at (:created-at queue-info)
     :worker-count (count workers)
     :pending-tasks (count (filter #(= :pending (:status (val %))) tasks))
     :processing-tasks (count (filter #(= :processing (:status (val %))) tasks))}))

(defn get-all-stats
  "Get statistics for all queues."
  []
  {:queues (map get-queue-stats (keys (:queues @queue-state)))
   :total-workers (count (:workers @queue-state))
   :dead-letter-count (count (:dead-letter @queue-state))
   :stats (:stats @queue-state)})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-task-queue!
  "Initialize task queue system."
  []
  (log/info "Initializing task queue")
  ;; Register feature flag
  (flags/register-flag! "task-queue" "Enable task queue" true)
  ;; Create metrics
  (metrics/create-counter! :task-queue/enqueued "Tasks enqueued")
  (metrics/create-counter! :task-queue/processed "Tasks processed")
  (metrics/create-counter! :task-queue/failed "Tasks failed")
  (metrics/create-gauge! :task-queue/dead-letter "Dead letter queue size"
                         #(count (:dead-letter @queue-state)))
  ;; Create default queues
  (create-queue! :default)
  (create-queue! :high-priority :max-size 1000)
  (create-queue! :low-priority :max-size 50000)
  (log/info "Task queue initialized"))

;; =============================================================================
;; SHUTDOWN
;; =============================================================================

(defn shutdown!
  "Shutdown task queue system."
  []
  (log/info "Shutting down task queue")
  ;; Stop all workers
  (doseq [worker-id (keys (:workers @queue-state))]
    (stop-worker! worker-id))
  ;; Close all queues
  (doseq [queue-name (keys (:queues @queue-state))]
    (delete-queue! queue-name))
  (log/info "Task queue shutdown complete"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-queue-status []
  {:enabled (flags/is-enabled? "task-queue")
   :queues (count (:queues @queue-state))
   :workers (count (:workers @queue-state))
   :dead-letter (count (:dead-letter @queue-state))
   :stats (:stats @queue-state)})
