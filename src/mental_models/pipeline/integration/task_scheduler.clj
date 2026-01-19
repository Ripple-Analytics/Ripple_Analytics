(ns mental-models.pipeline.integration.task-scheduler
  "Task scheduler for mental model analysis system.
   
   Features:
   - Cron-style scheduling
   - One-time task scheduling
   - Recurring tasks
   - Task dependencies
   - Task priorities
   - Concurrent execution limits
   - Task history
   - Failure handling"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZoneId]
           [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit
            ScheduledFuture Executors]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:tasks {}            ;; task-id -> task
         :scheduled {}        ;; task-id -> ScheduledFuture
         :history []          ;; execution history
         :executor nil        ;; ScheduledThreadPoolExecutor
         :config {:pool-size 4
                  :max-history-size 1000
                  :default-retry-count 3
                  :default-retry-delay-ms 5000}
         :stats {:tasks-scheduled 0
                 :tasks-executed 0
                 :tasks-succeeded 0
                 :tasks-failed 0
                 :tasks-cancelled 0}
         :initialized? false}))

;; ============================================================================
;; Cron Expression Parsing
;; ============================================================================

(defn- parse-cron-field
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
  [cron-expr]
  (let [fields (str/split cron-expr #"\s+")]
    (when (= 5 (count fields))
      {:minute (parse-cron-field (nth fields 0) 0 59)
       :hour (parse-cron-field (nth fields 1) 0 23)
       :day-of-month (parse-cron-field (nth fields 2) 1 31)
       :month (parse-cron-field (nth fields 3) 1 12)
       :day-of-week (parse-cron-field (nth fields 4) 0 6)})))

(defn- matches-cron?
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

(defn- next-cron-time
  "Calculate the next execution time for a cron expression."
  [cron-parsed from-time]
  (loop [dt (.plusMinutes from-time 1)]
    (if (matches-cron? cron-parsed dt)
      dt
      (recur (.plusMinutes dt 1)))))

;; ============================================================================
;; Task Creation
;; ============================================================================

(defn create-task!
  "Create a scheduled task."
  [task-id config]
  (let [task {:id task-id
              :name (get config :name (name task-id))
              :type (get config :type :one-time) ;; :one-time, :recurring, :cron
              :task-fn (get config :task-fn)
              :cron-expr (get config :cron-expr)
              :cron-parsed (when-let [expr (get config :cron-expr)]
                             (parse-cron expr))
              :interval-ms (get config :interval-ms)
              :delay-ms (get config :delay-ms 0)
              :priority (get config :priority 5)
              :dependencies (get config :dependencies [])
              :retry-count (get config :retry-count
                                (get-in @state [:config :default-retry-count]))
              :retry-delay-ms (get config :retry-delay-ms
                                   (get-in @state [:config :default-retry-delay-ms]))
              :on-success (get config :on-success)
              :on-failure (get config :on-failure)
              :enabled? (atom true)
              :running? (atom false)
              :last-run nil
              :next-run nil
              :run-count (atom 0)
              :success-count (atom 0)
              :failure-count (atom 0)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:tasks task-id] task)
    (swap! state update-in [:stats :tasks-scheduled] inc)
    (logging/log :info "Created task" {:task-id task-id :type (:type task)})
    (events/emit! :task-created {:task-id task-id})
    task-id))

(defn get-task
  "Get a task by ID."
  [task-id]
  (get-in @state [:tasks task-id]))

(defn list-tasks
  "List all tasks."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :type (:type t)
           :enabled? @(:enabled? t)
           :running? @(:running? t)
           :run-count @(:run-count t)
           :last-run (:last-run t)
           :next-run (:next-run t)})
        (:tasks @state)))

(defn delete-task!
  "Delete a task."
  [task-id]
  (cancel-task! task-id)
  (swap! state update :tasks dissoc task-id)
  (logging/log :info "Deleted task" {:task-id task-id})
  (events/emit! :task-deleted {:task-id task-id}))

;; ============================================================================
;; Task Execution
;; ============================================================================

(defn- check-dependencies
  "Check if all task dependencies have completed."
  [task]
  (every? (fn [dep-id]
            (when-let [dep-task (get-task dep-id)]
              (pos? @(:success-count dep-task))))
          (:dependencies task)))

(defn- record-execution!
  "Record a task execution in history."
  [task-id status duration error]
  (let [execution {:task-id task-id
                   :status status
                   :duration-ms duration
                   :error error
                   :timestamp (System/currentTimeMillis)}
        max-size (get-in @state [:config :max-history-size])]
    (swap! state update :history
           (fn [history]
             (let [new-history (conj history execution)]
               (if (> (count new-history) max-size)
                 (vec (drop (- (count new-history) max-size) new-history))
                 new-history))))))

(defn- execute-task!
  "Execute a task."
  [task]
  (when (and @(:enabled? task) (not @(:running? task)))
    (if (check-dependencies task)
      (do
        (reset! (:running? task) true)
        (swap! (:run-count task) inc)
        (swap! state update-in [:stats :tasks-executed] inc)
        (swap! state assoc-in [:tasks (:id task) :last-run] (System/currentTimeMillis))
        
        (let [start-time (System/currentTimeMillis)]
          (try
            ((:task-fn task))
            (let [duration (- (System/currentTimeMillis) start-time)]
              (swap! (:success-count task) inc)
              (swap! state update-in [:stats :tasks-succeeded] inc)
              (record-execution! (:id task) :success duration nil)
              (logging/log :info "Task succeeded" {:task-id (:id task) :duration-ms duration})
              (when-let [on-success (:on-success task)]
                (try (on-success) (catch Exception _))))
            (catch Exception e
              (let [duration (- (System/currentTimeMillis) start-time)]
                (swap! (:failure-count task) inc)
                (swap! state update-in [:stats :tasks-failed] inc)
                (record-execution! (:id task) :failure duration (.getMessage e))
                (logging/log :error "Task failed" {:task-id (:id task) :error (.getMessage e)})
                (when-let [on-failure (:on-failure task)]
                  (try (on-failure e) (catch Exception _)))))
            (finally
              (reset! (:running? task) false)))))
      (logging/log :debug "Task dependencies not met" {:task-id (:id task)}))))

;; ============================================================================
;; Task Scheduling
;; ============================================================================

(defn schedule-task!
  "Schedule a task for execution."
  [task-id]
  (when-let [task (get-task task-id)]
    (let [executor (:executor @state)]
      (case (:type task)
        :one-time
        (let [future (.schedule executor
                                (fn [] (execute-task! task))
                                (:delay-ms task)
                                TimeUnit/MILLISECONDS)]
          (swap! state assoc-in [:scheduled task-id] future)
          (swap! state assoc-in [:tasks task-id :next-run]
                 (+ (System/currentTimeMillis) (:delay-ms task))))
        
        :recurring
        (let [future (.scheduleAtFixedRate executor
                                           (fn [] (execute-task! task))
                                           (:delay-ms task)
                                           (:interval-ms task)
                                           TimeUnit/MILLISECONDS)]
          (swap! state assoc-in [:scheduled task-id] future)
          (swap! state assoc-in [:tasks task-id :next-run]
                 (+ (System/currentTimeMillis) (:delay-ms task))))
        
        :cron
        (let [now (LocalDateTime/now)
              next-time (next-cron-time (:cron-parsed task) now)
              delay-ms (.toMillis (java.time.Duration/between now next-time))]
          (letfn [(schedule-next []
                    (let [future (.schedule executor
                                            (fn []
                                              (execute-task! task)
                                              (schedule-next))
                                            delay-ms
                                            TimeUnit/MILLISECONDS)]
                      (swap! state assoc-in [:scheduled task-id] future)
                      (swap! state assoc-in [:tasks task-id :next-run]
                             (.toEpochMilli (.atZone next-time (ZoneId/systemDefault)) ))))]
            (schedule-next))))
      
      (logging/log :info "Scheduled task" {:task-id task-id :type (:type task)}))))

(defn cancel-task!
  "Cancel a scheduled task."
  [task-id]
  (when-let [future (get-in @state [:scheduled task-id])]
    (.cancel future false)
    (swap! state update :scheduled dissoc task-id)
    (swap! state update-in [:stats :tasks-cancelled] inc)
    (logging/log :info "Cancelled task" {:task-id task-id})))

(defn reschedule-task!
  "Reschedule a task."
  [task-id]
  (cancel-task! task-id)
  (schedule-task! task-id))

;; ============================================================================
;; Task Control
;; ============================================================================

(defn enable-task!
  "Enable a task."
  [task-id]
  (when-let [task (get-task task-id)]
    (reset! (:enabled? task) true)
    (logging/log :info "Enabled task" {:task-id task-id})))

(defn disable-task!
  "Disable a task."
  [task-id]
  (when-let [task (get-task task-id)]
    (reset! (:enabled? task) false)
    (logging/log :info "Disabled task" {:task-id task-id})))

(defn run-task-now!
  "Run a task immediately."
  [task-id]
  (when-let [task (get-task task-id)]
    (go (execute-task! task))))

;; ============================================================================
;; Task History
;; ============================================================================

(defn get-task-history
  "Get execution history for a task."
  [task-id & {:keys [limit] :or {limit 100}}]
  (let [history (:history @state)
        filtered (filter #(= (:task-id %) task-id) history)]
    (vec (take-last limit filtered))))

(defn get-all-history
  "Get all execution history."
  [& {:keys [limit status] :or {limit 100}}]
  (let [history (:history @state)
        filtered (cond->> history
                   status (filter #(= (:status %) status))
                   limit (take-last limit))]
    (vec filtered)))

(defn clear-history!
  "Clear execution history."
  []
  (swap! state assoc :history [])
  (logging/log :info "Cleared task history"))

;; ============================================================================
;; Scheduler Control
;; ============================================================================

(defn start-scheduler!
  "Start the task scheduler."
  []
  (when-not (:executor @state)
    (let [pool-size (get-in @state [:config :pool-size])
          executor (ScheduledThreadPoolExecutor. pool-size)]
      (swap! state assoc :executor executor)
      (logging/log :info "Started scheduler" {:pool-size pool-size}))))

(defn stop-scheduler!
  "Stop the task scheduler."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor)
    (swap! state assoc :executor nil)
    (swap! state assoc :scheduled {})
    (logging/log :info "Stopped scheduler")))

(defn restart-scheduler!
  "Restart the task scheduler."
  []
  (stop-scheduler!)
  (start-scheduler!))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-scheduler-stats
  "Get scheduler statistics."
  []
  (let [stats (:stats @state)]
    {:tasks-count (count (:tasks @state))
     :scheduled-count (count (:scheduled @state))
     :history-size (count (:history @state))
     :tasks-scheduled (:tasks-scheduled stats)
     :tasks-executed (:tasks-executed stats)
     :tasks-succeeded (:tasks-succeeded stats)
     :tasks-failed (:tasks-failed stats)
     :tasks-cancelled (:tasks-cancelled stats)
     :success-rate (if (pos? (:tasks-executed stats))
                     (/ (:tasks-succeeded stats) (:tasks-executed stats))
                     1.0)}))

(defn reset-stats!
  "Reset scheduler statistics."
  []
  (swap! state assoc :stats {:tasks-scheduled 0
                             :tasks-executed 0
                             :tasks-succeeded 0
                             :tasks-failed 0
                             :tasks-cancelled 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-task-scheduler!
  "Initialize the task scheduler."
  []
  (when-not (:initialized? @state)
    (start-scheduler!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Task scheduler initialized")
    (events/emit! :task-scheduler-initialized {})
    true))
