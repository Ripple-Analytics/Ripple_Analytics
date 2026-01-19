(ns mental-models.pipeline.integration.job-scheduler
  "Job Scheduler Module
   
   Scheduled task execution:
   - Cron-style scheduling
   - One-time and recurring jobs
   - Job dependencies
   - Retry on failure
   - Job history tracking"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService ScheduledFuture TimeUnit]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util UUID Calendar Date]))

;; =============================================================================
;; JOB SCHEDULER STATE
;; =============================================================================

(defonce scheduler-state (atom {:jobs {}
                                :executions {}
                                :history []
                                :executor nil
                                :config {:max-history 1000
                                         :default-max-retries 3
                                         :default-retry-delay-ms 5000
                                         :thread-pool-size 10}}))

;; =============================================================================
;; CRON PARSING
;; =============================================================================

(defn parse-cron-field
  "Parse a single cron field."
  [field min-val max-val]
  (cond
    (= field "*") (set (range min-val (inc max-val)))
    (str/includes? field "/") (let [[_ step] (str/split field #"/")
                                     step-val (Integer/parseInt step)]
                                 (set (range min-val (inc max-val) step-val)))
    (str/includes? field "-") (let [[start end] (str/split field #"-")]
                                 (set (range (Integer/parseInt start) (inc (Integer/parseInt end)))))
    (str/includes? field ",") (set (map #(Integer/parseInt %) (str/split field #",")))
    :else #{(Integer/parseInt field)}))

(defn parse-cron
  "Parse a cron expression (minute hour day-of-month month day-of-week)."
  [cron-expr]
  (let [fields (str/split cron-expr #"\s+")]
    (when (= (count fields) 5)
      {:minutes (parse-cron-field (nth fields 0) 0 59)
       :hours (parse-cron-field (nth fields 1) 0 23)
       :days-of-month (parse-cron-field (nth fields 2) 1 31)
       :months (parse-cron-field (nth fields 3) 1 12)
       :days-of-week (parse-cron-field (nth fields 4) 0 6)})))

(defn matches-cron?
  "Check if a time matches a cron expression."
  [cron-schedule ^Calendar cal]
  (and (contains? (:minutes cron-schedule) (.get cal Calendar/MINUTE))
       (contains? (:hours cron-schedule) (.get cal Calendar/HOUR_OF_DAY))
       (contains? (:days-of-month cron-schedule) (.get cal Calendar/DAY_OF_MONTH))
       (contains? (:months cron-schedule) (inc (.get cal Calendar/MONTH)))
       (contains? (:days-of-week cron-schedule) (.get cal Calendar/DAY_OF_WEEK))))

(defn next-cron-time
  "Calculate the next time a cron expression will match."
  [cron-schedule]
  (let [cal (Calendar/getInstance)]
    (.add cal Calendar/MINUTE 1)
    (.set cal Calendar/SECOND 0)
    (.set cal Calendar/MILLISECOND 0)
    (loop [iterations 0]
      (if (or (> iterations 525600) ;; Max 1 year of minutes
              (matches-cron? cron-schedule cal))
        (.getTimeInMillis cal)
        (do
          (.add cal Calendar/MINUTE 1)
          (recur (inc iterations)))))))

;; =============================================================================
;; JOB CREATION
;; =============================================================================

(defn create-job
  "Create a new job."
  [job-id {:keys [name handler schedule interval-ms delay-ms max-retries
                  retry-delay-ms dependencies enabled? tags]}]
  (let [config (:config @scheduler-state)]
    {:id job-id
     :name (or name (str job-id))
     :handler handler
     :schedule (when schedule (parse-cron schedule))
     :schedule-expr schedule
     :interval-ms interval-ms
     :delay-ms delay-ms
     :max-retries (or max-retries (:default-max-retries config))
     :retry-delay-ms (or retry-delay-ms (:default-retry-delay-ms config))
     :dependencies (or dependencies [])
     :enabled? (AtomicBoolean. (if (nil? enabled?) true enabled?))
     :tags (or tags #{})
     :last-run (AtomicLong. 0)
     :next-run (AtomicLong. 0)
     :run-count (AtomicLong. 0)
     :success-count (AtomicLong. 0)
     :failure-count (AtomicLong. 0)
     :scheduled-future (atom nil)
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; JOB REGISTRATION
;; =============================================================================

(defn register-job!
  "Register a job."
  [job-id opts]
  (log/info "Registering job" {:id job-id :name (:name opts)})
  (let [job (create-job job-id opts)]
    (swap! scheduler-state assoc-in [:jobs job-id] job)
    (metrics/inc-counter! :jobscheduler/jobs-registered)
    job-id))

(defn unregister-job!
  "Unregister a job."
  [job-id]
  (when-let [job (get-in @scheduler-state [:jobs job-id])]
    (log/info "Unregistering job" {:id job-id})
    ;; Cancel scheduled future
    (when-let [^ScheduledFuture future @(:scheduled-future job)]
      (.cancel future false))
    (swap! scheduler-state update :jobs dissoc job-id)))

(defn get-job
  "Get a job."
  [job-id]
  (get-in @scheduler-state [:jobs job-id]))

(defn list-jobs
  "List all jobs."
  [& {:keys [tag enabled?]}]
  (let [jobs (vals (:jobs @scheduler-state))]
    (cond->> jobs
      tag (filter #(contains? (:tags %) tag))
      (some? enabled?) (filter #(= (.get ^AtomicBoolean (:enabled? %)) enabled?)))))

;; =============================================================================
;; JOB EXECUTION
;; =============================================================================

(defn record-execution!
  "Record a job execution."
  [job-id {:keys [status duration-ms error started-at]}]
  (let [execution {:id (str (UUID/randomUUID))
                   :job-id job-id
                   :status status
                   :duration-ms duration-ms
                   :error error
                   :started-at started-at
                   :completed-at (System/currentTimeMillis)}]
    (swap! scheduler-state update :history
           (fn [h]
             (let [max-history (get-in @scheduler-state [:config :max-history])
                   new-history (conj h execution)]
               (if (> (count new-history) max-history)
                 (vec (drop (- (count new-history) max-history) new-history))
                 new-history))))
    execution))

(defn check-dependencies
  "Check if all job dependencies have completed successfully."
  [job]
  (every? (fn [dep-id]
            (when-let [dep-job (get-job dep-id)]
              (pos? (.get ^AtomicLong (:success-count dep-job)))))
          (:dependencies job)))

(defn execute-job!
  "Execute a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when (and (.get ^AtomicBoolean (:enabled? job))
               (check-dependencies job))
      (let [started-at (System/currentTimeMillis)]
        (.set ^AtomicLong (:last-run job) started-at)
        (.incrementAndGet ^AtomicLong (:run-count job))
        (metrics/inc-counter! :jobscheduler/jobs-executed)
        (events/publish! :jobscheduler/job-started {:job-id job-id})
        (try
          (let [result ((:handler job))]
            (.incrementAndGet ^AtomicLong (:success-count job))
            (record-execution! job-id {:status :success
                                       :duration-ms (- (System/currentTimeMillis) started-at)
                                       :started-at started-at})
            (events/publish! :jobscheduler/job-completed {:job-id job-id :result result})
            result)
          (catch Exception e
            (.incrementAndGet ^AtomicLong (:failure-count job))
            (log/error "Job execution failed" {:job job-id :error (.getMessage e)})
            (record-execution! job-id {:status :failure
                                       :duration-ms (- (System/currentTimeMillis) started-at)
                                       :error (.getMessage e)
                                       :started-at started-at})
            (events/publish! :jobscheduler/job-failed {:job-id job-id :error (.getMessage e)})
            nil))))))

(defn execute-with-retry!
  "Execute a job with retry logic."
  [job-id]
  (when-let [job (get-job job-id)]
    (loop [attempt 1]
      (let [result (execute-job! job-id)]
        (if (or result (>= attempt (:max-retries job)))
          result
          (do
            (Thread/sleep (:retry-delay-ms job))
            (recur (inc attempt))))))))

;; =============================================================================
;; SCHEDULING
;; =============================================================================

(defn schedule-job!
  "Schedule a job for execution."
  [job-id]
  (when-let [job (get-job job-id)]
    (when-let [^ScheduledExecutorService executor (:executor @scheduler-state)]
      ;; Cancel existing schedule
      (when-let [^ScheduledFuture future @(:scheduled-future job)]
        (.cancel future false))
      (cond
        ;; Cron schedule
        (:schedule job)
        (let [next-time (next-cron-time (:schedule job))
              delay (- next-time (System/currentTimeMillis))]
          (.set ^AtomicLong (:next-run job) next-time)
          (reset! (:scheduled-future job)
                  (.schedule executor
                             (fn []
                               (execute-with-retry! job-id)
                               (schedule-job! job-id))
                             delay
                             TimeUnit/MILLISECONDS)))
        ;; Fixed interval
        (:interval-ms job)
        (let [initial-delay (or (:delay-ms job) 0)]
          (reset! (:scheduled-future job)
                  (.scheduleAtFixedRate executor
                                        #(execute-with-retry! job-id)
                                        initial-delay
                                        (:interval-ms job)
                                        TimeUnit/MILLISECONDS)))
        ;; One-time delayed execution
        (:delay-ms job)
        (reset! (:scheduled-future job)
                (.schedule executor
                           #(execute-with-retry! job-id)
                           (:delay-ms job)
                           TimeUnit/MILLISECONDS))))))

(defn unschedule-job!
  "Unschedule a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when-let [^ScheduledFuture future @(:scheduled-future job)]
      (.cancel future false)
      (reset! (:scheduled-future job) nil))))

(defn reschedule-job!
  "Reschedule a job."
  [job-id]
  (unschedule-job! job-id)
  (schedule-job! job-id))

;; =============================================================================
;; JOB CONTROL
;; =============================================================================

(defn enable-job!
  "Enable a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (log/info "Enabling job" {:id job-id})
    (.set ^AtomicBoolean (:enabled? job) true)
    (schedule-job! job-id)))

(defn disable-job!
  "Disable a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (log/info "Disabling job" {:id job-id})
    (.set ^AtomicBoolean (:enabled? job) false)
    (unschedule-job! job-id)))

(defn trigger-job!
  "Trigger immediate job execution."
  [job-id]
  (log/info "Triggering job" {:id job-id})
  (future (execute-with-retry! job-id)))

;; =============================================================================
;; HISTORY
;; =============================================================================

(defn get-job-history
  "Get execution history for a job."
  [job-id & {:keys [limit]}]
  (let [history (filter #(= (:job-id %) job-id) (:history @scheduler-state))]
    (if limit
      (take-last limit history)
      history)))

(defn get-recent-executions
  "Get recent executions across all jobs."
  [& {:keys [limit status]}]
  (let [history (:history @scheduler-state)]
    (cond->> history
      status (filter #(= (:status %) status))
      limit (take-last limit))))

(defn clear-history!
  "Clear execution history."
  [& {:keys [job-id]}]
  (if job-id
    (swap! scheduler-state update :history
           (fn [h] (vec (remove #(= (:job-id %) job-id) h))))
    (swap! scheduler-state assoc :history [])))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-job-stats
  "Get statistics for a job."
  [job-id]
  (when-let [job (get-job job-id)]
    {:id job-id
     :name (:name job)
     :enabled? (.get ^AtomicBoolean (:enabled? job))
     :schedule (:schedule-expr job)
     :interval-ms (:interval-ms job)
     :run-count (.get ^AtomicLong (:run-count job))
     :success-count (.get ^AtomicLong (:success-count job))
     :failure-count (.get ^AtomicLong (:failure-count job))
     :success-rate (let [total (.get ^AtomicLong (:run-count job))]
                     (if (pos? total)
                       (* 100.0 (/ (.get ^AtomicLong (:success-count job)) total))
                       0.0))
     :last-run (.get ^AtomicLong (:last-run job))
     :next-run (.get ^AtomicLong (:next-run job))}))

(defn get-all-job-stats
  "Get statistics for all jobs."
  []
  (into {} (for [job (list-jobs)]
             [(:id job) (get-job-stats (:id job))])))

;; =============================================================================
;; SCHEDULER LIFECYCLE
;; =============================================================================

(defn start-scheduler!
  "Start the job scheduler."
  []
  (when (and (flags/is-enabled? "job-scheduler")
             (nil? (:executor @scheduler-state)))
    (log/info "Starting job scheduler")
    (let [pool-size (get-in @scheduler-state [:config :thread-pool-size])
          executor (Executors/newScheduledThreadPool pool-size)]
      (swap! scheduler-state assoc :executor executor)
      ;; Schedule all enabled jobs
      (doseq [job (list-jobs :enabled? true)]
        (schedule-job! (:id job))))))

(defn stop-scheduler!
  "Stop the job scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:executor @scheduler-state)]
    (log/info "Stopping job scheduler")
    ;; Unschedule all jobs
    (doseq [job (list-jobs)]
      (unschedule-job! (:id job)))
    (.shutdown executor)
    (swap! scheduler-state assoc :executor nil)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-job-scheduler!
  "Initialize job scheduler."
  []
  (log/info "Initializing job scheduler")
  ;; Register feature flag
  (flags/register-flag! "job-scheduler" "Enable job scheduler" true)
  ;; Create metrics
  (metrics/create-counter! :jobscheduler/jobs-registered "Jobs registered")
  (metrics/create-counter! :jobscheduler/jobs-executed "Jobs executed")
  (metrics/create-gauge! :jobscheduler/total-jobs "Total jobs"
                         #(count (:jobs @scheduler-state)))
  (metrics/create-gauge! :jobscheduler/enabled-jobs "Enabled jobs"
                         #(count (filter #(.get ^AtomicBoolean (:enabled? %)) (vals (:jobs @scheduler-state)))))
  (log/info "Job scheduler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-scheduler-status []
  {:enabled (flags/is-enabled? "job-scheduler")
   :running (some? (:executor @scheduler-state))
   :jobs (count (:jobs @scheduler-state))
   :enabled-jobs (count (list-jobs :enabled? true))
   :history-size (count (:history @scheduler-state))
   :stats (get-all-job-stats)
   :config (:config @scheduler-state)})
