(ns mental-models.pipeline.integration.batch-processor
  "Batch Processor Module
   
   Batch processing and bulk operations:
   - Batch job definition and execution
   - Parallel batch processing
   - Progress tracking
   - Error handling and retry
   - Checkpoint and resume"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap Executors ExecutorService TimeUnit]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean AtomicInteger]))

;; =============================================================================
;; BATCH PROCESSOR STATE
;; =============================================================================

(defonce batch-state (atom {:jobs (ConcurrentHashMap.)
                            :executions (ConcurrentHashMap.)
                            :checkpoints (ConcurrentHashMap.)
                            :executor nil
                            :config {:default-batch-size 100
                                     :default-parallelism 4
                                     :max-retries 3
                                     :retry-delay-ms 1000
                                     :checkpoint-interval 100}}))

;; =============================================================================
;; BATCH JOB DEFINITION
;; =============================================================================

(defn create-batch-job
  "Create a batch job definition."
  [job-id {:keys [name description processor batch-size parallelism on-error on-complete]}]
  {:id job-id
   :name (or name (str job-id))
   :description description
   :processor processor
   :batch-size (or batch-size (get-in @batch-state [:config :default-batch-size]))
   :parallelism (or parallelism (get-in @batch-state [:config :default-parallelism]))
   :on-error on-error
   :on-complete on-complete
   :created-at (System/currentTimeMillis)})

(defn register-job!
  "Register a batch job."
  [job-id processor & {:keys [name description batch-size parallelism on-error on-complete]}]
  (log/info "Registering batch job" {:id job-id})
  (let [job (create-batch-job job-id {:name name
                                      :description description
                                      :processor processor
                                      :batch-size batch-size
                                      :parallelism parallelism
                                      :on-error on-error
                                      :on-complete on-complete})]
    (.put ^ConcurrentHashMap (:jobs @batch-state) job-id job)
    (metrics/inc-counter! :batchprocessor/jobs-registered)
    (events/publish! :batchprocessor/job-registered {:job-id job-id})
    job-id))

(defn unregister-job!
  "Unregister a batch job."
  [job-id]
  (log/info "Unregistering batch job" {:id job-id})
  (.remove ^ConcurrentHashMap (:jobs @batch-state) job-id))

(defn get-job
  "Get a batch job by ID."
  [job-id]
  (.get ^ConcurrentHashMap (:jobs @batch-state) job-id))

(defn list-jobs
  "List all registered batch jobs."
  []
  (vec (vals (:jobs @batch-state))))

;; =============================================================================
;; EXECUTION TRACKING
;; =============================================================================

(defn create-execution
  "Create an execution record."
  [execution-id job-id total-items]
  {:id execution-id
   :job-id job-id
   :status :pending
   :total-items total-items
   :processed-items (AtomicLong. 0)
   :successful-items (AtomicLong. 0)
   :failed-items (AtomicLong. 0)
   :skipped-items (AtomicLong. 0)
   :current-batch (AtomicInteger. 0)
   :total-batches 0
   :started-at nil
   :completed-at nil
   :errors []
   :cancelled (AtomicBoolean. false)
   :paused (AtomicBoolean. false)})

(defn get-execution
  "Get an execution by ID."
  [execution-id]
  (.get ^ConcurrentHashMap (:executions @batch-state) execution-id))

(defn list-executions
  "List all executions."
  [& {:keys [job-id status]}]
  (let [executions (vals (:executions @batch-state))]
    (cond->> executions
      job-id (filter #(= (:job-id %) job-id))
      status (filter #(= (:status %) status)))))

(defn update-execution-status!
  "Update execution status."
  [execution-id status]
  (when-let [execution (get-execution execution-id)]
    (.put ^ConcurrentHashMap (:executions @batch-state) execution-id
          (assoc execution :status status))))

(defn get-execution-progress
  "Get execution progress."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (let [total (:total-items execution)
          processed (.get ^AtomicLong (:processed-items execution))]
      {:execution-id execution-id
       :status (:status execution)
       :total-items total
       :processed-items processed
       :successful-items (.get ^AtomicLong (:successful-items execution))
       :failed-items (.get ^AtomicLong (:failed-items execution))
       :skipped-items (.get ^AtomicLong (:skipped-items execution))
       :current-batch (.get ^AtomicInteger (:current-batch execution))
       :total-batches (:total-batches execution)
       :progress-percent (if (pos? total) (double (* 100 (/ processed total))) 0.0)
       :started-at (:started-at execution)
       :completed-at (:completed-at execution)})))

;; =============================================================================
;; CHECKPOINTING
;; =============================================================================

(defn save-checkpoint!
  "Save a checkpoint for an execution."
  [execution-id checkpoint-data]
  (log/debug "Saving checkpoint" {:execution execution-id})
  (.put ^ConcurrentHashMap (:checkpoints @batch-state) execution-id
        {:execution-id execution-id
         :data checkpoint-data
         :saved-at (System/currentTimeMillis)})
  (metrics/inc-counter! :batchprocessor/checkpoints-saved))

(defn get-checkpoint
  "Get a checkpoint for an execution."
  [execution-id]
  (.get ^ConcurrentHashMap (:checkpoints @batch-state) execution-id))

(defn clear-checkpoint!
  "Clear a checkpoint."
  [execution-id]
  (.remove ^ConcurrentHashMap (:checkpoints @batch-state) execution-id))

(defn resume-from-checkpoint
  "Resume execution from a checkpoint."
  [execution-id]
  (when-let [checkpoint (get-checkpoint execution-id)]
    (log/info "Resuming from checkpoint" {:execution execution-id})
    (:data checkpoint)))

;; =============================================================================
;; BATCH PROCESSING
;; =============================================================================

(defn partition-items
  "Partition items into batches."
  [items batch-size]
  (partition-all batch-size items))

(defn process-item
  "Process a single item with error handling."
  [processor item execution max-retries retry-delay-ms]
  (loop [attempt 1]
    (let [result (try
                   {:success true :result (processor item)}
                   (catch Exception e
                     {:success false :error e :attempt attempt}))]
      (if (:success result)
        (do
          (.incrementAndGet ^AtomicLong (:successful-items execution))
          result)
        (if (< attempt max-retries)
          (do
            (Thread/sleep retry-delay-ms)
            (recur (inc attempt)))
          (do
            (.incrementAndGet ^AtomicLong (:failed-items execution))
            result))))))

(defn process-batch
  "Process a batch of items."
  [job batch execution]
  (let [processor (:processor job)
        max-retries (get-in @batch-state [:config :max-retries])
        retry-delay-ms (get-in @batch-state [:config :retry-delay-ms])
        results (atom [])]
    (doseq [item batch]
      (when-not (.get ^AtomicBoolean (:cancelled execution))
        ;; Wait if paused
        (while (.get ^AtomicBoolean (:paused execution))
          (Thread/sleep 100))
        (let [result (process-item processor item execution max-retries retry-delay-ms)]
          (swap! results conj result)
          (.incrementAndGet ^AtomicLong (:processed-items execution)))))
    @results))

(defn process-batch-parallel
  "Process a batch in parallel."
  [job batch execution parallelism]
  (let [processor (:processor job)
        max-retries (get-in @batch-state [:config :max-retries])
        retry-delay-ms (get-in @batch-state [:config :retry-delay-ms])
        result-chan (chan parallelism)
        item-chan (chan)]
    ;; Start workers
    (dotimes [_ parallelism]
      (go-loop []
        (when-let [item (<! item-chan)]
          (when-not (.get ^AtomicBoolean (:cancelled execution))
            (while (.get ^AtomicBoolean (:paused execution))
              (<! (timeout 100)))
            (let [result (process-item processor item execution max-retries retry-delay-ms)]
              (>! result-chan result)
              (.incrementAndGet ^AtomicLong (:processed-items execution))))
          (recur))))
    ;; Feed items
    (go
      (doseq [item batch]
        (>! item-chan item))
      (close! item-chan))
    ;; Collect results
    (let [results (atom [])]
      (dotimes [_ (count batch)]
        (when-let [result (async/<!! result-chan)]
          (swap! results conj result)))
      (close! result-chan)
      @results)))

;; =============================================================================
;; JOB EXECUTION
;; =============================================================================

(defn execute-job!
  "Execute a batch job."
  [job-id items & {:keys [parallel resume-from]}]
  (if-let [job (get-job job-id)]
    (let [execution-id (str (java.util.UUID/randomUUID))
          ;; Resume from checkpoint if available
          start-index (or resume-from
                          (when-let [checkpoint (get-checkpoint execution-id)]
                            (get-in checkpoint [:data :processed-count]))
                          0)
          remaining-items (drop start-index items)
          total-items (count items)
          batches (partition-items remaining-items (:batch-size job))
          total-batches (count batches)
          execution (create-execution execution-id job-id total-items)
          execution (assoc execution
                           :total-batches total-batches
                           :started-at (System/currentTimeMillis))]
      ;; Store execution
      (.put ^ConcurrentHashMap (:executions @batch-state) execution-id execution)
      ;; Update status
      (update-execution-status! execution-id :running)
      (metrics/inc-counter! :batchprocessor/jobs-started)
      (events/publish! :batchprocessor/job-started {:execution-id execution-id :job-id job-id})
      (log/info "Starting batch job" {:job job-id :execution execution-id :items total-items})
      ;; Process batches
      (let [checkpoint-interval (get-in @batch-state [:config :checkpoint-interval])
            all-results (atom [])]
        (doseq [[batch-idx batch] (map-indexed vector batches)]
          (when-not (.get ^AtomicBoolean (:cancelled execution))
            (.set ^AtomicInteger (:current-batch execution) (inc batch-idx))
            ;; Process batch
            (let [batch-results (if parallel
                                  (process-batch-parallel job batch execution (:parallelism job))
                                  (process-batch job batch execution))]
              (swap! all-results into batch-results))
            ;; Save checkpoint periodically
            (when (zero? (mod (inc batch-idx) checkpoint-interval))
              (save-checkpoint! execution-id {:processed-count (.get ^AtomicLong (:processed-items execution))
                                              :batch-index batch-idx}))
            ;; Publish progress
            (events/publish! :batchprocessor/batch-completed {:execution-id execution-id
                                                               :batch-index batch-idx
                                                               :progress (get-execution-progress execution-id)})))
        ;; Complete execution
        (let [final-status (if (.get ^AtomicBoolean (:cancelled execution)) :cancelled :completed)]
          (update-execution-status! execution-id final-status)
          (.put ^ConcurrentHashMap (:executions @batch-state) execution-id
                (assoc execution :completed-at (System/currentTimeMillis)))
          ;; Clear checkpoint on completion
          (when (= final-status :completed)
            (clear-checkpoint! execution-id))
          ;; Call completion callback
          (when-let [on-complete (:on-complete job)]
            (on-complete {:execution-id execution-id
                          :status final-status
                          :results @all-results
                          :progress (get-execution-progress execution-id)}))
          (metrics/inc-counter! :batchprocessor/jobs-completed)
          (events/publish! :batchprocessor/job-completed {:execution-id execution-id
                                                           :status final-status
                                                           :progress (get-execution-progress execution-id)})
          (log/info "Batch job completed" {:job job-id :execution execution-id :status final-status})
          {:execution-id execution-id
           :status final-status
           :progress (get-execution-progress execution-id)
           :results @all-results})))
    {:error :job-not-found :job-id job-id}))

(defn execute-job-async!
  "Execute a batch job asynchronously."
  [job-id items & opts]
  (let [result-chan (chan)]
    (go
      (let [result (apply execute-job! job-id items opts)]
        (>! result-chan result)))
    result-chan))

;; =============================================================================
;; JOB CONTROL
;; =============================================================================

(defn cancel-execution!
  "Cancel a running execution."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (log/info "Cancelling execution" {:id execution-id})
    (.set ^AtomicBoolean (:cancelled execution) true)
    (update-execution-status! execution-id :cancelling)
    (events/publish! :batchprocessor/execution-cancelled {:execution-id execution-id})))

(defn pause-execution!
  "Pause a running execution."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (log/info "Pausing execution" {:id execution-id})
    (.set ^AtomicBoolean (:paused execution) true)
    (update-execution-status! execution-id :paused)
    (events/publish! :batchprocessor/execution-paused {:execution-id execution-id})))

(defn resume-execution!
  "Resume a paused execution."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (log/info "Resuming execution" {:id execution-id})
    (.set ^AtomicBoolean (:paused execution) false)
    (update-execution-status! execution-id :running)
    (events/publish! :batchprocessor/execution-resumed {:execution-id execution-id})))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-job-stats
  "Get statistics for a job."
  [job-id]
  (let [executions (list-executions :job-id job-id)]
    {:job-id job-id
     :total-executions (count executions)
     :completed (count (filter #(= (:status %) :completed) executions))
     :failed (count (filter #(= (:status %) :failed) executions))
     :cancelled (count (filter #(= (:status %) :cancelled) executions))
     :running (count (filter #(= (:status %) :running) executions))}))

(defn get-all-job-stats
  "Get statistics for all jobs."
  []
  (into {} (for [job-id (keys (:jobs @batch-state))]
             [job-id (get-job-stats job-id)])))

(defn get-processor-stats
  "Get overall processor statistics."
  []
  {:total-jobs (.size ^ConcurrentHashMap (:jobs @batch-state))
   :total-executions (.size ^ConcurrentHashMap (:executions @batch-state))
   :active-checkpoints (.size ^ConcurrentHashMap (:checkpoints @batch-state))
   :jobs (get-all-job-stats)})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-batch-processor!
  "Initialize batch processor."
  []
  (log/info "Initializing batch processor")
  ;; Register feature flag
  (flags/register-flag! "batch-processor" "Enable batch processor" true)
  ;; Create metrics
  (metrics/create-counter! :batchprocessor/jobs-registered "Jobs registered")
  (metrics/create-counter! :batchprocessor/jobs-started "Jobs started")
  (metrics/create-counter! :batchprocessor/jobs-completed "Jobs completed")
  (metrics/create-counter! :batchprocessor/checkpoints-saved "Checkpoints saved")
  (metrics/create-gauge! :batchprocessor/total-jobs "Total jobs"
                         #(.size ^ConcurrentHashMap (:jobs @batch-state)))
  (metrics/create-gauge! :batchprocessor/active-executions "Active executions"
                         #(count (list-executions :status :running)))
  (log/info "Batch processor initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-batch-processor-status []
  {:enabled (flags/is-enabled? "batch-processor")
   :jobs (.size ^ConcurrentHashMap (:jobs @batch-state))
   :executions (.size ^ConcurrentHashMap (:executions @batch-state))
   :checkpoints (.size ^ConcurrentHashMap (:checkpoints @batch-state))
   :active-executions (count (list-executions :status :running))
   :stats (get-processor-stats)
   :config (:config @batch-state)})
