(ns mental-models.scheduler.core
  "Scheduler Module for Mental Models Pipeline
   
   Provides scheduling with:
   - Cron-style job scheduling
   - One-time delayed execution
   - Recurring jobs
   - Job priorities
   - Job dependencies
   - Job status tracking"
  (:require
   [clojure.string :as str])
  (:import
   [java.util UUID]
   [java.time Instant Duration]
   [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit Executors]))

;; =============================================================================
;; JOB DEFINITION
;; =============================================================================

(defrecord Job [id name fn schedule priority status created-at last-run next-run])

(defn create-job [name f schedule priority]
  (->Job (str (UUID/randomUUID))
         name
         f
         schedule
         priority
         :pending
         (Instant/now)
         nil
         nil))

;; =============================================================================
;; SCHEDULER STATE
;; =============================================================================

(def ^:private jobs (atom {}))
(def ^:private executor (atom nil))
(def ^:private pool-size (atom 4))

(defn get-executor []
  (or @executor
      (let [e (ScheduledThreadPoolExecutor. @pool-size)]
        (reset! executor e)
        e)))

(defn set-pool-size! [n]
  (reset! pool-size n)
  (when @executor
    (.setCorePoolSize @executor n)))

;; =============================================================================
;; SCHEDULE PARSING
;; =============================================================================

(defn parse-interval [schedule]
  (cond
    (number? schedule) schedule
    (string? schedule)
    (let [[_ num unit] (re-matches #"(\d+)\s*(s|m|h|d)" schedule)]
      (when (and num unit)
        (* (Long/parseLong num)
           (case unit
             "s" 1000
             "m" 60000
             "h" 3600000
             "d" 86400000))))
    :else nil))

(defn calculate-next-run [schedule]
  (when-let [interval (parse-interval schedule)]
    (.plusMillis (Instant/now) interval)))

;; =============================================================================
;; JOB MANAGEMENT
;; =============================================================================

(defn register-job
  "Register a new job with the scheduler."
  [name f schedule & {:keys [priority] :or {priority 5}}]
  (let [job (create-job name f schedule priority)
        job-with-next (assoc job :next-run (calculate-next-run schedule))]
    (swap! jobs assoc (:id job-with-next) job-with-next)
    job-with-next))

(defn unregister-job
  "Remove a job from the scheduler."
  [job-id]
  (swap! jobs dissoc job-id))

(defn get-job [job-id]
  (get @jobs job-id))

(defn get-all-jobs []
  (vals @jobs))

(defn get-jobs-by-status [status]
  (filter #(= (:status %) status) (get-all-jobs)))

(defn get-pending-jobs []
  (get-jobs-by-status :pending))

(defn get-running-jobs []
  (get-jobs-by-status :running))

;; =============================================================================
;; JOB EXECUTION
;; =============================================================================

(defn update-job-status [job-id status]
  (swap! jobs update job-id assoc :status status))

(defn update-job-last-run [job-id]
  (swap! jobs update job-id assoc :last-run (Instant/now)))

(defn update-job-next-run [job-id]
  (let [job (get-job job-id)
        next-run (calculate-next-run (:schedule job))]
    (swap! jobs update job-id assoc :next-run next-run)))

(defn execute-job [job-id]
  (when-let [job (get-job job-id)]
    (try
      (update-job-status job-id :running)
      ((:fn job))
      (update-job-status job-id :completed)
      (update-job-last-run job-id)
      (update-job-next-run job-id)
      {:status :success :job-id job-id}
      (catch Exception e
        (update-job-status job-id :failed)
        {:status :error :job-id job-id :error (str e)}))))

;; =============================================================================
;; SCHEDULING
;; =============================================================================

(defn schedule-once
  "Schedule a job to run once after a delay."
  [name f delay-ms]
  (let [job (register-job name f delay-ms)]
    (.schedule (get-executor)
               #(execute-job (:id job))
               delay-ms
               TimeUnit/MILLISECONDS)
    job))

(defn schedule-recurring
  "Schedule a job to run repeatedly at a fixed interval."
  [name f interval-ms]
  (let [job (register-job name f interval-ms)]
    (.scheduleAtFixedRate (get-executor)
                          #(execute-job (:id job))
                          0
                          interval-ms
                          TimeUnit/MILLISECONDS)
    job))

(defn schedule-with-delay
  "Schedule a job to run repeatedly with a fixed delay between runs."
  [name f delay-ms]
  (let [job (register-job name f delay-ms)]
    (.scheduleWithFixedDelay (get-executor)
                             #(execute-job (:id job))
                             0
                             delay-ms
                             TimeUnit/MILLISECONDS)
    job))

;; =============================================================================
;; BUILT-IN JOBS
;; =============================================================================

(defn schedule-analysis-batch
  "Schedule batch analysis job."
  [interval-ms batch-fn]
  (schedule-recurring "batch-analysis" batch-fn interval-ms))

(defn schedule-cache-cleanup
  "Schedule cache cleanup job."
  [interval-ms cleanup-fn]
  (schedule-recurring "cache-cleanup" cleanup-fn interval-ms))

(defn schedule-metrics-collection
  "Schedule metrics collection job."
  [interval-ms collect-fn]
  (schedule-recurring "metrics-collection" collect-fn interval-ms))

(defn schedule-health-check
  "Schedule health check job."
  [interval-ms check-fn]
  (schedule-recurring "health-check" check-fn interval-ms))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  (let [all-jobs (get-all-jobs)]
    {:total-jobs (count all-jobs)
     :by-status (frequencies (map :status all-jobs))
     :by-priority (frequencies (map :priority all-jobs))
     :pool-size @pool-size
     :active-count (when @executor (.getActiveCount @executor))
     :completed-count (when @executor (.getCompletedTaskCount @executor))}))

;; =============================================================================
;; LIFECYCLE
;; =============================================================================

(defn start-scheduler []
  (get-executor)
  {:status :started})

(defn stop-scheduler []
  (when @executor
    (.shutdown @executor)
    (reset! executor nil))
  {:status :stopped})

(defn restart-scheduler []
  (stop-scheduler)
  (start-scheduler))

(defn pause-job [job-id]
  (update-job-status job-id :paused))

(defn resume-job [job-id]
  (update-job-status job-id :pending))

;; =============================================================================
;; JOB DEPENDENCIES
;; =============================================================================

(def ^:private job-dependencies (atom {}))

(defn add-dependency [job-id depends-on-id]
  (swap! job-dependencies update job-id (fnil conj #{}) depends-on-id))

(defn remove-dependency [job-id depends-on-id]
  (swap! job-dependencies update job-id disj depends-on-id))

(defn get-dependencies [job-id]
  (get @job-dependencies job-id #{}))

(defn dependencies-satisfied? [job-id]
  (let [deps (get-dependencies job-id)]
    (every? #(= :completed (:status (get-job %))) deps)))

(defn execute-with-dependencies [job-id]
  (if (dependencies-satisfied? job-id)
    (execute-job job-id)
    {:status :waiting :job-id job-id :waiting-for (get-dependencies job-id)}))
