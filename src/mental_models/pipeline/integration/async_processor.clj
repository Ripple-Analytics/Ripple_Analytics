(ns mental-models.pipeline.integration.async-processor
  "Async Processor Module
   
   Asynchronous job processing:
   - Job queuing and execution
   - Priority scheduling
   - Retry with backoff
   - Job chaining
   - Progress tracking"
  (:require
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; PROCESSOR STATE
;; =============================================================================

(defonce processor-state (atom {:jobs {}
                                :queues {:high (chan 1000)
                                         :normal (chan 5000)
                                         :low (chan 10000)}
                                :workers []
                                :handlers {}
                                :config {:max-workers 8
                                         :max-retries 3
                                         :base-delay-ms 1000
                                         :max-delay-ms 60000}}))

;; =============================================================================
;; JOB CREATION
;; =============================================================================

(defn generate-job-id
  "Generate a unique job ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-job
  "Create a new job."
  [job-type payload & {:keys [priority callback depends-on]}]
  {:id (generate-job-id)
   :type job-type
   :payload payload
   :priority (or priority :normal)
   :status :pending
   :callback callback
   :depends-on depends-on
   :created-at (System/currentTimeMillis)
   :started-at nil
   :completed-at nil
   :attempts 0
   :error nil
   :result nil
   :progress 0})

;; =============================================================================
;; JOB HANDLERS
;; =============================================================================

(defn register-handler!
  "Register a job handler."
  [job-type handler-fn]
  (log/info "Registering job handler" {:type job-type})
  (swap! processor-state assoc-in [:handlers job-type] handler-fn))

(defn get-handler
  "Get a job handler."
  [job-type]
  (get-in @processor-state [:handlers job-type]))

(defn unregister-handler!
  "Unregister a job handler."
  [job-type]
  (swap! processor-state update :handlers dissoc job-type))

;; =============================================================================
;; JOB SUBMISSION
;; =============================================================================

(defn submit-job!
  "Submit a job for processing."
  [job]
  (when (flags/is-enabled? "async-processor")
    (log/info "Submitting job" {:id (:id job) :type (:type job) :priority (:priority job)})
    (let [queue (get-in @processor-state [:queues (:priority job)])]
      (swap! processor-state assoc-in [:jobs (:id job)] job)
      (go (>! queue job))
      (metrics/inc-counter! :async/jobs-submitted)
      (events/publish! :async/job-submitted {:job-id (:id job) :type (:type job)})
      (:id job))))

(defn submit!
  "Convenience function to create and submit a job."
  [job-type payload & opts]
  (let [job (apply create-job job-type payload opts)]
    (submit-job! job)))

;; =============================================================================
;; JOB STATUS
;; =============================================================================

(defn get-job
  "Get a job by ID."
  [job-id]
  (get-in @processor-state [:jobs job-id]))

(defn update-job!
  "Update a job's state."
  [job-id updates]
  (swap! processor-state update-in [:jobs job-id] merge updates))

(defn update-progress!
  "Update a job's progress."
  [job-id progress]
  (update-job! job-id {:progress progress})
  (events/publish! :async/job-progress {:job-id job-id :progress progress}))

(defn get-job-status
  "Get a job's status."
  [job-id]
  (when-let [job (get-job job-id)]
    {:id (:id job)
     :type (:type job)
     :status (:status job)
     :progress (:progress job)
     :attempts (:attempts job)
     :error (:error job)
     :created-at (:created-at job)
     :started-at (:started-at job)
     :completed-at (:completed-at job)}))

;; =============================================================================
;; RETRY LOGIC
;; =============================================================================

(defn calculate-delay
  "Calculate retry delay with exponential backoff."
  [attempts]
  (let [{:keys [base-delay-ms max-delay-ms]} (:config @processor-state)
        delay (* base-delay-ms (Math/pow 2 attempts))]
    (min delay max-delay-ms)))

(defn should-retry?
  "Check if a job should be retried."
  [job]
  (let [max-retries (get-in @processor-state [:config :max-retries])]
    (< (:attempts job) max-retries)))

(defn retry-job!
  "Retry a failed job."
  [job error]
  (let [delay (calculate-delay (:attempts job))]
    (log/info "Retrying job" {:id (:id job) :attempt (inc (:attempts job)) :delay delay})
    (go
      (<! (timeout delay))
      (let [updated-job (-> job
                            (update :attempts inc)
                            (assoc :status :pending)
                            (assoc :error nil))]
        (swap! processor-state assoc-in [:jobs (:id job)] updated-job)
        (let [queue (get-in @processor-state [:queues (:priority job)])]
          (>! queue updated-job))))))

;; =============================================================================
;; JOB EXECUTION
;; =============================================================================

(defn execute-job!
  "Execute a single job."
  [job]
  (log/info "Executing job" {:id (:id job) :type (:type job)})
  (update-job! (:id job) {:status :running :started-at (System/currentTimeMillis)})
  (metrics/inc-counter! :async/jobs-started)
  (events/publish! :async/job-started {:job-id (:id job)})
  (if-let [handler (get-handler (:type job))]
    (try
      (let [result (handler job)]
        (update-job! (:id job) {:status :completed
                                :result result
                                :progress 100
                                :completed-at (System/currentTimeMillis)})
        (metrics/inc-counter! :async/jobs-completed)
        (events/publish! :async/job-completed {:job-id (:id job) :result result})
        ;; Execute callback if provided
        (when-let [callback (:callback job)]
          (callback result))
        result)
      (catch Exception e
        (let [error-msg (.getMessage e)]
          (log/error "Job execution failed" {:id (:id job) :error error-msg})
          (if (should-retry? job)
            (do
              (update-job! (:id job) {:status :retrying :error error-msg})
              (retry-job! job e))
            (do
              (update-job! (:id job) {:status :failed
                                      :error error-msg
                                      :completed-at (System/currentTimeMillis)})
              (metrics/inc-counter! :async/jobs-failed)
              (events/publish! :async/job-failed {:job-id (:id job) :error error-msg}))))))
    (do
      (log/error "No handler for job type" {:type (:type job)})
      (update-job! (:id job) {:status :failed
                              :error "No handler registered"
                              :completed-at (System/currentTimeMillis)})
      (metrics/inc-counter! :async/jobs-failed))))

;; =============================================================================
;; WORKERS
;; =============================================================================

(defn create-worker
  "Create a worker that processes jobs from queues."
  [worker-id]
  (log/info "Creating worker" {:id worker-id})
  (let [running (atom true)
        worker (go-loop []
                 (when @running
                   ;; Check queues in priority order
                   (let [[job _] (async/alts! [(get-in @processor-state [:queues :high])
                                               (get-in @processor-state [:queues :normal])
                                               (get-in @processor-state [:queues :low])
                                               (timeout 1000)]
                                              :priority true)]
                     (when job
                       (execute-job! job))
                     (recur))))]
    {:id worker-id
     :running running
     :channel worker}))

(defn stop-worker!
  "Stop a worker."
  [{:keys [id running]}]
  (log/info "Stopping worker" {:id id})
  (reset! running false))

(defn start-workers!
  "Start worker pool."
  [& {:keys [num-workers]}]
  (let [n (or num-workers (get-in @processor-state [:config :max-workers]))]
    (log/info "Starting worker pool" {:workers n})
    (let [workers (mapv #(create-worker (str "worker-" %)) (range n))]
      (swap! processor-state assoc :workers workers)
      (metrics/set-gauge! :async/active-workers n)
      workers)))

(defn stop-workers!
  "Stop all workers."
  []
  (log/info "Stopping all workers")
  (doseq [worker (:workers @processor-state)]
    (stop-worker! worker))
  (swap! processor-state assoc :workers [])
  (metrics/set-gauge! :async/active-workers 0))

;; =============================================================================
;; JOB CHAINING
;; =============================================================================

(defn chain-jobs!
  "Chain multiple jobs to execute in sequence."
  [jobs]
  (let [job-ids (atom [])]
    (reduce (fn [prev-id job]
              (let [chained-job (if prev-id
                                  (assoc job :depends-on prev-id)
                                  job)
                    job-id (submit-job! chained-job)]
                (swap! job-ids conj job-id)
                job-id))
            nil
            jobs)
    @job-ids))

(defn parallel-jobs!
  "Submit multiple jobs to run in parallel."
  [jobs]
  (mapv submit-job! jobs))

;; =============================================================================
;; JOB QUERIES
;; =============================================================================

(defn list-jobs
  "List jobs with optional filters."
  [& {:keys [status type limit]}]
  (let [jobs (vals (:jobs @processor-state))
        filtered (cond->> jobs
                   status (filter #(= (:status %) status))
                   type (filter #(= (:type %) type))
                   limit (take limit))]
    (sort-by :created-at > filtered)))

(defn pending-jobs
  "Get all pending jobs."
  []
  (list-jobs :status :pending))

(defn running-jobs
  "Get all running jobs."
  []
  (list-jobs :status :running))

(defn failed-jobs
  "Get all failed jobs."
  []
  (list-jobs :status :failed))

(defn cleanup-completed-jobs!
  "Remove completed jobs older than specified age."
  [max-age-ms]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        to-remove (for [[id job] (:jobs @processor-state)
                        :when (and (= (:status job) :completed)
                                   (< (:completed-at job) cutoff))]
                    id)]
    (doseq [id to-remove]
      (swap! processor-state update :jobs dissoc id))
    (count to-remove)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-async-processor!
  "Initialize async processor."
  []
  (log/info "Initializing async processor")
  ;; Register feature flag
  (flags/register-flag! "async-processor" "Enable async job processing" true)
  ;; Create metrics
  (metrics/create-counter! :async/jobs-submitted "Jobs submitted")
  (metrics/create-counter! :async/jobs-started "Jobs started")
  (metrics/create-counter! :async/jobs-completed "Jobs completed")
  (metrics/create-counter! :async/jobs-failed "Jobs failed")
  (metrics/create-gauge! :async/active-workers "Active workers" (constantly 0))
  (metrics/create-gauge! :async/pending-jobs "Pending jobs"
                         #(count (pending-jobs)))
  (metrics/create-gauge! :async/running-jobs "Running jobs"
                         #(count (running-jobs)))
  ;; Start workers
  (start-workers!)
  (log/info "Async processor initialized"))

(defn shutdown!
  "Shutdown async processor."
  []
  (log/info "Shutting down async processor")
  (stop-workers!)
  ;; Close queues
  (doseq [[_ queue] (:queues @processor-state)]
    (close! queue)))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-processor-status []
  {:enabled (flags/is-enabled? "async-processor")
   :workers (count (:workers @processor-state))
   :handlers (count (:handlers @processor-state))
   :jobs {:total (count (:jobs @processor-state))
          :pending (count (pending-jobs))
          :running (count (running-jobs))
          :failed (count (failed-jobs))}
   :config (:config @processor-state)})
