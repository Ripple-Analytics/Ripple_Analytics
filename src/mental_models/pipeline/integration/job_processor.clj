(ns mental-models.pipeline.integration.job-processor
  "Job processor for mental model analysis background tasks.
   
   Features:
   - Job queuing
   - Priority scheduling
   - Parallel execution
   - Job dependencies
   - Progress tracking
   - Retry handling
   - Job cancellation
   - Result storage"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.concurrent Executors ThreadPoolExecutor TimeUnit LinkedBlockingQueue]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:jobs {}             ;; job-id -> job
         :queues {}           ;; queue-name -> [job-ids]
         :handlers {}         ;; job-type -> handler-fn
         :workers {}          ;; worker-id -> worker-info
         :results {}          ;; job-id -> result
         :config {:max-workers 4
                  :default-timeout-ms 300000
                  :max-retries 3
                  :retry-delay-ms 5000}
         :stats {:jobs-submitted 0 :jobs-completed 0 :jobs-failed 0}
         :initialized? false}))

(defonce ^:private executor (atom nil))
(defonce ^:private running? (atom false))

;; ============================================================================
;; Job Handler Registration
;; ============================================================================

(defn register-handler!
  "Register a job handler."
  [job-type handler-fn]
  (swap! state assoc-in [:handlers job-type] handler-fn)
  (logging/log :info "Registered job handler" {:job-type job-type})
  job-type)

(defn get-handler
  "Get a job handler."
  [job-type]
  (get-in @state [:handlers job-type]))

(defn list-handlers
  "List all registered handlers."
  []
  (keys (:handlers @state)))

;; ============================================================================
;; Queue Management
;; ============================================================================

(defn create-queue!
  "Create a job queue."
  [queue-name & {:keys [priority] :or {priority 0}}]
  (swap! state assoc-in [:queues queue-name] {:jobs []
                                               :priority priority
                                               :created-at (System/currentTimeMillis)})
  (logging/log :info "Created queue" {:queue-name queue-name})
  queue-name)

(defn get-queue
  "Get a queue."
  [queue-name]
  (get-in @state [:queues queue-name]))

(defn list-queues
  "List all queues."
  []
  (mapv (fn [[name q]]
          {:name name
           :job-count (count (:jobs q))
           :priority (:priority q)})
        (:queues @state)))

(defn delete-queue!
  "Delete a queue."
  [queue-name]
  (swap! state update :queues dissoc queue-name))

;; ============================================================================
;; Job Operations
;; ============================================================================

(defn submit-job!
  "Submit a job for processing."
  [config]
  (when (flags/enabled? :job-processor)
    (let [job-id (str (UUID/randomUUID))
          queue-name (get config :queue :default)
          job {:id job-id
               :type (get config :type)
               :queue queue-name
               :payload (get config :payload {})
               :priority (get config :priority 0)
               :dependencies (get config :dependencies [])
               :timeout-ms (get config :timeout-ms (get-in @state [:config :default-timeout-ms]))
               :max-retries (get config :max-retries (get-in @state [:config :max-retries]))
               :retry-count 0
               :status :pending
               :progress 0
               :created-at (System/currentTimeMillis)
               :scheduled-at (get config :scheduled-at)
               :metadata (get config :metadata {})}]
      
      ;; Ensure queue exists
      (when-not (get-queue queue-name)
        (create-queue! queue-name))
      
      ;; Store job
      (swap! state assoc-in [:jobs job-id] job)
      
      ;; Add to queue
      (swap! state update-in [:queues queue-name :jobs] conj job-id)
      
      (swap! state update-in [:stats :jobs-submitted] inc)
      (logging/log :info "Submitted job" {:job-id job-id :type (:type job)})
      (events/emit! :job-submitted {:job-id job-id})
      
      job-id)))

(defn get-job
  "Get a job."
  [job-id]
  (get-in @state [:jobs job-id]))

(defn list-jobs
  "List jobs."
  [& {:keys [queue status type limit] :or {limit 100}}]
  (let [jobs (vals (:jobs @state))
        filtered (cond->> jobs
                   queue (filter #(= (:queue %) queue))
                   status (filter #(= (:status %) status))
                   type (filter #(= (:type %) type))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :type :queue :status :progress :created-at]) filtered)))

(defn update-job-progress!
  "Update job progress."
  [job-id progress & {:keys [message]}]
  (swap! state update-in [:jobs job-id]
         (fn [j]
           (cond-> j
             true (assoc :progress progress)
             message (assoc :progress-message message)))))

(defn cancel-job!
  "Cancel a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when (#{:pending :running} (:status job))
      (swap! state update-in [:jobs job-id]
             (fn [j]
               (assoc j
                      :status :cancelled
                      :cancelled-at (System/currentTimeMillis))))
      (logging/log :info "Cancelled job" {:job-id job-id})
      (events/emit! :job-cancelled {:job-id job-id}))))

(defn retry-job!
  "Retry a failed job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when (= :failed (:status job))
      (swap! state update-in [:jobs job-id]
             (fn [j]
               (assoc j
                      :status :pending
                      :retry-count (inc (:retry-count j))
                      :retried-at (System/currentTimeMillis))))
      (swap! state update-in [:queues (:queue job) :jobs] conj job-id)
      (logging/log :info "Retried job" {:job-id job-id}))))

;; ============================================================================
;; Job Execution
;; ============================================================================

(defn- check-dependencies
  "Check if job dependencies are satisfied."
  [job]
  (let [deps (:dependencies job)]
    (if (empty? deps)
      true
      (every? (fn [dep-id]
                (let [dep-job (get-job dep-id)]
                  (= :completed (:status dep-job))))
              deps))))

(defn- execute-job!
  "Execute a single job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when (and (= :pending (:status job))
               (check-dependencies job))
      (let [handler (get-handler (:type job))]
        (if handler
          (do
            ;; Mark as running
            (swap! state update-in [:jobs job-id]
                   (fn [j]
                     (assoc j
                            :status :running
                            :started-at (System/currentTimeMillis))))
            (events/emit! :job-started {:job-id job-id})
            
            (try
              ;; Execute handler
              (let [result (handler (:payload job)
                                    {:job-id job-id
                                     :update-progress! #(update-job-progress! job-id %1 :message %2)})]
                
                ;; Mark as completed
                (swap! state update-in [:jobs job-id]
                       (fn [j]
                         (assoc j
                                :status :completed
                                :progress 100
                                :completed-at (System/currentTimeMillis))))
                
                ;; Store result
                (swap! state assoc-in [:results job-id] result)
                
                (swap! state update-in [:stats :jobs-completed] inc)
                (logging/log :info "Job completed" {:job-id job-id})
                (events/emit! :job-completed {:job-id job-id}))
              
              (catch Exception e
                (let [retry-count (:retry-count job)
                      max-retries (:max-retries job)]
                  (if (< retry-count max-retries)
                    ;; Schedule retry
                    (do
                      (swap! state update-in [:jobs job-id]
                             (fn [j]
                               (assoc j
                                      :status :pending
                                      :retry-count (inc retry-count)
                                      :last-error (.getMessage e))))
                      (logging/log :warn "Job failed, will retry" {:job-id job-id :retry (inc retry-count)}))
                    ;; Mark as failed
                    (do
                      (swap! state update-in [:jobs job-id]
                             (fn [j]
                               (assoc j
                                      :status :failed
                                      :error (.getMessage e)
                                      :failed-at (System/currentTimeMillis))))
                      (swap! state update-in [:stats :jobs-failed] inc)
                      (logging/log :error "Job failed" {:job-id job-id :error (.getMessage e)})
                      (events/emit! :job-failed {:job-id job-id :error (.getMessage e)})))))))
          
          (do
            (swap! state update-in [:jobs job-id]
                   (fn [j]
                     (assoc j
                            :status :failed
                            :error "No handler registered"
                            :failed-at (System/currentTimeMillis))))
            (logging/log :error "No handler for job type" {:job-id job-id :type (:type job)})))))))

(defn- get-next-job
  "Get the next job to process."
  []
  (let [queues (sort-by (fn [[_ q]] (- (:priority q))) (:queues @state))]
    (loop [remaining queues]
      (when (seq remaining)
        (let [[queue-name queue] (first remaining)
              job-ids (:jobs queue)
              pending-jobs (filter (fn [job-id]
                                     (let [job (get-job job-id)]
                                       (and (= :pending (:status job))
                                            (check-dependencies job)
                                            (or (nil? (:scheduled-at job))
                                                (<= (:scheduled-at job) (System/currentTimeMillis))))))
                                   job-ids)]
          (if (seq pending-jobs)
            (let [job-id (first pending-jobs)]
              ;; Remove from queue
              (swap! state update-in [:queues queue-name :jobs]
                     (fn [jobs] (vec (remove #(= % job-id) jobs))))
              job-id)
            (recur (rest remaining))))))))

;; ============================================================================
;; Worker Management
;; ============================================================================

(defn- start-worker!
  "Start a worker."
  [worker-id]
  (let [worker {:id worker-id
                :status :idle
                :started-at (System/currentTimeMillis)
                :jobs-processed 0}]
    (swap! state assoc-in [:workers worker-id] worker)
    
    (go-loop []
      (when @running?
        (if-let [job-id (get-next-job)]
          (do
            (swap! state assoc-in [:workers worker-id :status] :busy)
            (swap! state assoc-in [:workers worker-id :current-job] job-id)
            (execute-job! job-id)
            (swap! state update-in [:workers worker-id :jobs-processed] inc)
            (swap! state assoc-in [:workers worker-id :status] :idle)
            (swap! state update-in [:workers worker-id] dissoc :current-job))
          (<! (timeout 100)))
        (recur)))
    
    worker-id))

(defn list-workers
  "List all workers."
  []
  (mapv (fn [[id w]]
          {:id id
           :status (:status w)
           :current-job (:current-job w)
           :jobs-processed (:jobs-processed w)})
        (:workers @state)))

;; ============================================================================
;; Processor Control
;; ============================================================================

(defn start-processor!
  "Start the job processor."
  []
  (when-not @running?
    (reset! running? true)
    (let [max-workers (get-in @state [:config :max-workers])]
      (doseq [i (range max-workers)]
        (start-worker! (str "worker-" i))))
    (logging/log :info "Job processor started")
    (events/emit! :job-processor-started {})))

(defn stop-processor!
  "Stop the job processor."
  []
  (when @running?
    (reset! running? false)
    (swap! state assoc :workers {})
    (logging/log :info "Job processor stopped")
    (events/emit! :job-processor-stopped {})))

(defn is-running?
  "Check if processor is running."
  []
  @running?)

;; ============================================================================
;; Results
;; ============================================================================

(defn get-result
  "Get job result."
  [job-id]
  (get-in @state [:results job-id]))

(defn clear-results!
  "Clear old results."
  [& {:keys [older-than-ms] :or {older-than-ms 86400000}}]
  (let [now (System/currentTimeMillis)
        to-clear (filter (fn [[job-id _]]
                           (let [job (get-job job-id)]
                             (and (:completed-at job)
                                  (> (- now (:completed-at job)) older-than-ms))))
                         (:results @state))]
    (doseq [[job-id _] to-clear]
      (swap! state update :results dissoc job-id))
    (count to-clear)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-processor-stats
  "Get processor statistics."
  []
  (let [stats (:stats @state)
        jobs (vals (:jobs @state))
        by-status (frequencies (map :status jobs))
        workers (vals (:workers @state))
        busy-workers (count (filter #(= :busy (:status %)) workers))]
    {:running? @running?
     :total-jobs (count jobs)
     :jobs-by-status by-status
     :total-queues (count (:queues @state))
     :total-workers (count workers)
     :busy-workers busy-workers
     :jobs-submitted (:jobs-submitted stats)
     :jobs-completed (:jobs-completed stats)
     :jobs-failed (:jobs-failed stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-job-processor!
  "Initialize the job processor."
  []
  (when-not (:initialized? @state)
    ;; Create default queue
    (create-queue! :default :priority 0)
    (create-queue! :high-priority :priority 10)
    (create-queue! :low-priority :priority -10)
    
    ;; Register built-in handlers
    (register-handler! :analyze-document
                       (fn [payload ctx]
                         (let [update-progress! (:update-progress! ctx)]
                           (update-progress! 10 "Starting analysis")
                           (Thread/sleep 100)
                           (update-progress! 50 "Processing document")
                           (Thread/sleep 100)
                           (update-progress! 90 "Finalizing")
                           {:status :success
                            :document-id (:document-id payload)
                            :models-detected [:confirmation-bias :anchoring]})))
    
    (register-handler! :batch-analysis
                       (fn [payload ctx]
                         (let [documents (:documents payload)
                               update-progress! (:update-progress! ctx)]
                           (doseq [[idx doc] (map-indexed vector documents)]
                             (update-progress! (* 100 (/ idx (count documents)))
                                               (str "Processing document " (inc idx))))
                           {:status :success
                            :processed (count documents)})))
    
    (register-handler! :export-report
                       (fn [payload _]
                         {:status :success
                          :report-url (str "/reports/" (:report-id payload))}))
    
    ;; Start processor
    (start-processor!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Job processor initialized")
    (events/emit! :job-processor-initialized {})
    true))
