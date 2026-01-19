(ns mental-models.pipeline.integration.request-scheduler
  "Request scheduler for mental model analysis system.
   
   Features:
   - Request scheduling
   - Delayed execution
   - Recurring requests
   - Cron expressions
   - Schedule management
   - Job queuing
   - Schedule metrics
   - Execution history"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDateTime ZoneId]
           [java.util.concurrent ScheduledExecutorService ScheduledThreadPoolExecutor
            TimeUnit Future Executors]
           [java.util.concurrent.atomic AtomicLong AtomicBoolean]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:jobs {}             ;; job-id -> job config
         :schedules {}        ;; schedule-id -> schedule config
         :history []          ;; execution history
         :config {:pool-size 4
                  :max-history 1000
                  :default-timezone "UTC"}
         :stats {:jobs-scheduled (AtomicLong. 0)
                 :jobs-executed (AtomicLong. 0)
                 :jobs-failed (AtomicLong. 0)
                 :jobs-cancelled (AtomicLong. 0)}
         :executor nil
         :initialized? false}))

;; ============================================================================
;; Executor Management
;; ============================================================================

(defn- get-executor
  "Get or create the scheduled executor."
  []
  (or (:executor @state)
      (let [executor (ScheduledThreadPoolExecutor.
                      (get-in @state [:config :pool-size]))]
        (swap! state assoc :executor executor)
        executor)))

;; ============================================================================
;; Cron Expression Parsing
;; ============================================================================

(defn parse-cron-field
  "Parse a single cron field."
  [field min-val max-val]
  (cond
    (= field "*") (set (range min-val (inc max-val)))
    (str/includes? field "/")
    (let [[_ step] (str/split field #"/")
          step-val (Integer/parseInt step)]
      (set (range min-val (inc max-val) step-val)))
    (str/includes? field "-")
    (let [[start end] (str/split field #"-")]
      (set (range (Integer/parseInt start) (inc (Integer/parseInt end)))))
    (str/includes? field ",")
    (set (map #(Integer/parseInt %) (str/split field #",")))
    :else #{(Integer/parseInt field)}))

(defn parse-cron
  "Parse a cron expression (minute hour day-of-month month day-of-week)."
  [expr]
  (let [parts (str/split expr #"\s+")]
    (when (= (count parts) 5)
      {:minute (parse-cron-field (nth parts 0) 0 59)
       :hour (parse-cron-field (nth parts 1) 0 23)
       :day-of-month (parse-cron-field (nth parts 2) 1 31)
       :month (parse-cron-field (nth parts 3) 1 12)
       :day-of-week (parse-cron-field (nth parts 4) 0 6)})))

(defn cron-matches?
  "Check if a datetime matches a cron expression."
  [cron-parsed datetime]
  (let [minute (.getMinute datetime)
        hour (.getHour datetime)
        day-of-month (.getDayOfMonth datetime)
        month (.getMonthValue datetime)
        day-of-week (mod (.getValue (.getDayOfWeek datetime)) 7)]
    (and (contains? (:minute cron-parsed) minute)
         (contains? (:hour cron-parsed) hour)
         (contains? (:day-of-month cron-parsed) day-of-month)
         (contains? (:month cron-parsed) month)
         (contains? (:day-of-week cron-parsed) day-of-week))))

(defn next-cron-time
  "Calculate next execution time for a cron expression."
  [cron-parsed from-time]
  (loop [dt (.plusMinutes from-time 1)
         iterations 0]
    (if (> iterations 525600) ;; Max 1 year of minutes
      nil
      (if (cron-matches? cron-parsed dt)
        dt
        (recur (.plusMinutes dt 1) (inc iterations))))))

;; ============================================================================
;; Job Management
;; ============================================================================

(defn create-job!
  "Create a scheduled job."
  [job-id config]
  (let [job {:id job-id
             :name (get config :name (name job-id))
             :handler (get config :handler)
             :args (get config :args [])
             :delay-ms (get config :delay-ms 0)
             :interval-ms (get config :interval-ms)
             :cron (get config :cron)
             :max-executions (get config :max-executions)
             :execution-count (AtomicLong. 0)
             :last-execution (atom nil)
             :next-execution (atom nil)
             :future (atom nil)
             :enabled? (AtomicBoolean. true)
             :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:jobs job-id] job)
    (.incrementAndGet (:jobs-scheduled (:stats @state)))
    (logging/log :info "Created scheduled job" {:job-id job-id})
    job-id))

(defn get-job
  "Get a job."
  [job-id]
  (get-in @state [:jobs job-id]))

(defn list-jobs
  "List all jobs."
  []
  (mapv (fn [[id j]]
          {:id id
           :name (:name j)
           :enabled? (.get (:enabled? j))
           :execution-count (.get (:execution-count j))
           :last-execution @(:last-execution j)
           :next-execution @(:next-execution j)})
        (:jobs @state)))

(defn delete-job!
  "Delete a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when-let [future @(:future job)]
      (.cancel future false))
    (swap! state update :jobs dissoc job-id)))

(defn enable-job!
  "Enable a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (.set (:enabled? job) true)))

(defn disable-job!
  "Disable a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (.set (:enabled? job) false)))

;; ============================================================================
;; Job Execution
;; ============================================================================

(defn- record-execution!
  "Record a job execution in history."
  [job-id result duration-ms success?]
  (let [entry {:job-id job-id
               :timestamp (System/currentTimeMillis)
               :duration-ms duration-ms
               :success? success?
               :result (when-not success? result)}
        max-history (get-in @state [:config :max-history])]
    (swap! state update :history
           (fn [h]
             (let [new-h (conj h entry)]
               (if (> (count new-h) max-history)
                 (vec (drop (- (count new-h) max-history) new-h))
                 new-h))))))

(defn- execute-job!
  "Execute a job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when (.get (:enabled? job))
      (let [start-time (System/currentTimeMillis)]
        (try
          (let [result (apply (:handler job) (:args job))]
            (.incrementAndGet (:jobs-executed (:stats @state)))
            (.incrementAndGet (:execution-count job))
            (reset! (:last-execution job) (System/currentTimeMillis))
            (record-execution! job-id result (- (System/currentTimeMillis) start-time) true)
            result)
          (catch Exception e
            (.incrementAndGet (:jobs-failed (:stats @state)))
            (record-execution! job-id (.getMessage e) (- (System/currentTimeMillis) start-time) false)
            (logging/log :error "Job execution failed" {:job-id job-id :error (.getMessage e)})))))))

;; ============================================================================
;; Scheduling
;; ============================================================================

(defn schedule-once!
  "Schedule a job for one-time execution."
  [job-id delay-ms]
  (when-let [job (get-job job-id)]
    (let [executor (get-executor)
          future (.schedule executor
                            (fn [] (execute-job! job-id))
                            delay-ms
                            TimeUnit/MILLISECONDS)]
      (reset! (:future job) future)
      (reset! (:next-execution job) (+ (System/currentTimeMillis) delay-ms))
      job-id)))

(defn schedule-fixed-rate!
  "Schedule a job at a fixed rate."
  [job-id initial-delay-ms interval-ms]
  (when-let [job (get-job job-id)]
    (let [executor (get-executor)
          future (.scheduleAtFixedRate executor
                                        (fn [] (execute-job! job-id))
                                        initial-delay-ms
                                        interval-ms
                                        TimeUnit/MILLISECONDS)]
      (reset! (:future job) future)
      (reset! (:next-execution job) (+ (System/currentTimeMillis) initial-delay-ms))
      job-id)))

(defn schedule-fixed-delay!
  "Schedule a job with a fixed delay between executions."
  [job-id initial-delay-ms delay-ms]
  (when-let [job (get-job job-id)]
    (let [executor (get-executor)
          future (.scheduleWithFixedDelay executor
                                           (fn [] (execute-job! job-id))
                                           initial-delay-ms
                                           delay-ms
                                           TimeUnit/MILLISECONDS)]
      (reset! (:future job) future)
      (reset! (:next-execution job) (+ (System/currentTimeMillis) initial-delay-ms))
      job-id)))

(defn schedule-cron!
  "Schedule a job using a cron expression."
  [job-id cron-expr]
  (when-let [job (get-job job-id)]
    (when-let [cron-parsed (parse-cron cron-expr)]
      (let [timezone (ZoneId/of (get-in @state [:config :default-timezone]))
            schedule-next (fn schedule-next []
                            (when (.get (:enabled? job))
                              (let [now (LocalDateTime/now timezone)
                                    next-time (next-cron-time cron-parsed now)]
                                (when next-time
                                  (let [delay-ms (.toMillis
                                                  (java.time.Duration/between now next-time))
                                        executor (get-executor)
                                        future (.schedule executor
                                                          (fn []
                                                            (execute-job! job-id)
                                                            (schedule-next))
                                                          delay-ms
                                                          TimeUnit/MILLISECONDS)]
                                    (reset! (:future job) future)
                                    (reset! (:next-execution job)
                                            (+ (System/currentTimeMillis) delay-ms)))))))]
        (schedule-next)
        job-id))))

(defn cancel-job!
  "Cancel a scheduled job."
  [job-id]
  (when-let [job (get-job job-id)]
    (when-let [future @(:future job)]
      (.cancel future false)
      (.incrementAndGet (:jobs-cancelled (:stats @state)))
      (reset! (:future job) nil)
      (reset! (:next-execution job) nil)
      true)))

;; ============================================================================
;; Schedule Management
;; ============================================================================

(defn create-schedule!
  "Create a named schedule."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :jobs (atom [])
                  :enabled? (AtomicBoolean. true)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schedules schedule-id] schedule)
    schedule-id))

(defn add-job-to-schedule!
  "Add a job to a schedule."
  [schedule-id job-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (swap! (:jobs schedule) conj job-id)))

(defn start-schedule!
  "Start all jobs in a schedule."
  [schedule-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (doseq [job-id @(:jobs schedule)]
      (when-let [job (get-job job-id)]
        (cond
          (:cron job) (schedule-cron! job-id (:cron job))
          (:interval-ms job) (schedule-fixed-rate! job-id
                                                    (or (:delay-ms job) 0)
                                                    (:interval-ms job))
          (:delay-ms job) (schedule-once! job-id (:delay-ms job)))))))

(defn stop-schedule!
  "Stop all jobs in a schedule."
  [schedule-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (doseq [job-id @(:jobs schedule)]
      (cancel-job! job-id))))

;; ============================================================================
;; Execution History
;; ============================================================================

(defn get-history
  "Get execution history."
  [& {:keys [job-id limit] :or {limit 100}}]
  (let [history (:history @state)]
    (cond->> history
      job-id (filter #(= (:job-id %) job-id))
      true (take-last limit)
      true vec)))

(defn clear-history!
  "Clear execution history."
  []
  (swap! state assoc :history []))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-schedule-request
  "Ring middleware to schedule a request for later execution."
  [handler & {:keys [delay-ms]}]
  (fn [request]
    (if delay-ms
      (let [job-id (keyword (str "request-" (UUID/randomUUID)))]
        (create-job! job-id {:handler (fn [] (handler request))})
        (schedule-once! job-id delay-ms)
        {:status 202
         :headers {"Content-Type" "application/json"}
         :body {:scheduled true
                :job-id job-id
                :execute-at (+ (System/currentTimeMillis) delay-ms)}})
      (handler request))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-pool-size!
  "Set executor pool size."
  [size]
  (swap! state assoc-in [:config :pool-size] size))

(defn set-max-history!
  "Set maximum history entries."
  [max-entries]
  (swap! state assoc-in [:config :max-history] max-entries))

(defn set-default-timezone!
  "Set default timezone."
  [timezone]
  (swap! state assoc-in [:config :default-timezone] timezone))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-scheduler-metrics
  "Get scheduler metrics."
  []
  (let [stats (:stats @state)]
    {:jobs-scheduled (.get (:jobs-scheduled stats))
     :jobs-executed (.get (:jobs-executed stats))
     :jobs-failed (.get (:jobs-failed stats))
     :jobs-cancelled (.get (:jobs-cancelled stats))
     :active-jobs (count (:jobs @state))
     :schedules-count (count (:schedules @state))
     :history-count (count (:history @state))
     :success-rate (let [executed (.get (:jobs-executed stats))]
                     (if (pos? executed)
                       (/ (- executed (.get (:jobs-failed stats))) executed)
                       1.0))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-scheduler-stats
  "Get scheduler statistics."
  []
  (merge (get-scheduler-metrics)
         {:pool-size (get-in @state [:config :pool-size])
          :max-history (get-in @state [:config :max-history])
          :default-timezone (get-in @state [:config :default-timezone])}))

(defn reset-stats!
  "Reset scheduler statistics."
  []
  (.set (:jobs-scheduled (:stats @state)) 0)
  (.set (:jobs-executed (:stats @state)) 0)
  (.set (:jobs-failed (:stats @state)) 0)
  (.set (:jobs-cancelled (:stats @state)) 0))

;; ============================================================================
;; Shutdown
;; ============================================================================

(defn shutdown!
  "Shutdown the scheduler."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor)
    (swap! state assoc :executor nil)))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-scheduler!
  "Initialize the request scheduler."
  []
  (when-not (:initialized? @state)
    (get-executor) ;; Initialize executor
    (swap! state assoc :initialized? true)
    (logging/log :info "Request scheduler initialized")
    (events/emit! :request-scheduler-initialized {})
    true))
