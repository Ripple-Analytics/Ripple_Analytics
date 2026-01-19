(ns mental-models.pipeline.integration.batch-manager
  "Batch Job Manager
   
   Manages long-running batch analysis jobs:
   - Job creation and tracking
   - Progress monitoring
   - Job cancellation
   - Result aggregation
   - Checkpoint/resume support"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.pipeline.integration.notifications :as notif]
   [clojure.core.async :as async :refer [go go-loop chan <! >! close!]]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:import
   [java.util UUID]
   [java.util.concurrent Executors ExecutorService Future TimeUnit]))

;; =============================================================================
;; JOB STATE
;; =============================================================================

(defonce job-registry (atom {}))
(defonce executor (atom nil))

(def job-statuses #{:pending :running :paused :completed :failed :cancelled})

;; =============================================================================
;; JOB STRUCTURE
;; =============================================================================

(defn create-job
  "Create a new batch job."
  [job-type items & {:keys [priority] :or {priority :normal}}]
  {:id (str (UUID/randomUUID))
   :type job-type
   :status :pending
   :priority priority
   :items items
   :total-items (count items)
   :processed-items 0
   :failed-items 0
   :results []
   :errors []
   :created-at (System/currentTimeMillis)
   :started-at nil
   :completed-at nil
   :checkpoint nil
   :future nil})

;; =============================================================================
;; JOB REGISTRY
;; =============================================================================

(defn register-job! [job]
  (swap! job-registry assoc (:id job) job)
  (metrics/inc-counter! :batch-manager/jobs-created)
  (events/publish! :batch/job-created {:job-id (:id job) :type (:type job)})
  (log/info "Job registered" {:job-id (:id job) :type (:type job) :items (:total-items job)})
  job)

(defn get-job [job-id]
  (get @job-registry job-id))

(defn update-job! [job-id updates]
  (swap! job-registry update job-id merge updates)
  (get @job-registry job-id))

(defn remove-job! [job-id]
  (swap! job-registry dissoc job-id))

;; =============================================================================
;; CHECKPOINT SUPPORT
;; =============================================================================

(defn save-checkpoint!
  "Save job checkpoint to disk for resume support."
  [job-id]
  (when-let [job (get-job job-id)]
    (let [checkpoint-file (io/file (str ".checkpoints/" job-id ".edn"))]
      (io/make-parents checkpoint-file)
      (spit checkpoint-file (pr-str (dissoc job :future)))
      (update-job! job-id {:checkpoint (.getAbsolutePath checkpoint-file)})
      (log/debug "Checkpoint saved" {:job-id job-id}))))

(defn load-checkpoint
  "Load job from checkpoint file."
  [checkpoint-file]
  (when (.exists (io/file checkpoint-file))
    (edn/read-string (slurp checkpoint-file))))

(defn resume-from-checkpoint!
  "Resume a job from its checkpoint."
  [checkpoint-file process-fn]
  (when-let [job (load-checkpoint checkpoint-file)]
    (let [remaining-items (drop (:processed-items job) (:items job))
          resumed-job (assoc job
                             :items remaining-items
                             :status :pending)]
      (register-job! resumed-job)
      (start-job! (:id resumed-job) process-fn))))

;; =============================================================================
;; JOB EXECUTION
;; =============================================================================

(defn process-item!
  "Process a single item and update job state."
  [job-id item process-fn]
  (try
    (let [result (process-fn item)]
      (update-job! job-id
                   {:processed-items (inc (:processed-items (get-job job-id)))
                    :results (conj (:results (get-job job-id)) result)})
      result)
    (catch Exception e
      (update-job! job-id
                   {:processed-items (inc (:processed-items (get-job job-id)))
                    :failed-items (inc (:failed-items (get-job job-id)))
                    :errors (conj (:errors (get-job job-id))
                                  {:item item :error (.getMessage e)})})
      nil)))

(defn run-job!
  "Execute a batch job."
  [job-id process-fn]
  (let [job (get-job job-id)]
    (when (= (:status job) :pending)
      (update-job! job-id {:status :running :started-at (System/currentTimeMillis)})
      (events/publish! :batch/job-started {:job-id job-id})
      (notif/send-analysis-progress! {:job-id job-id :status :started})
      (doseq [[idx item] (map-indexed vector (:items job))]
        (let [current-job (get-job job-id)]
          (when (= (:status current-job) :running)
            (process-item! job-id item process-fn)
            ;; Save checkpoint every 10 items
            (when (zero? (mod (inc idx) 10))
              (save-checkpoint! job-id))
            ;; Publish progress
            (let [progress (/ (inc idx) (:total-items job))]
              (events/publish! :batch/job-progress
                               {:job-id job-id
                                :progress progress
                                :processed (inc idx)
                                :total (:total-items job)})))))
      ;; Complete job
      (let [final-job (get-job job-id)]
        (if (= (:status final-job) :running)
          (do
            (update-job! job-id {:status :completed :completed-at (System/currentTimeMillis)})
            (events/publish! :batch/job-completed {:job-id job-id :results (:results final-job)})
            (notif/send-batch-completion-notification!
             {:job-id job-id
              :total (:total-items final-job)
              :successful (:processed-items final-job)
              :failed (:failed-items final-job)})
            (metrics/inc-counter! :batch-manager/jobs-completed))
          (metrics/inc-counter! :batch-manager/jobs-cancelled))))))

(defn start-job!
  "Start a job asynchronously."
  [job-id process-fn]
  (when-let [exec @executor]
    (let [future (.submit exec (fn [] (run-job! job-id process-fn)))]
      (update-job! job-id {:future future})
      (log/info "Job started" {:job-id job-id}))))

;; =============================================================================
;; JOB CONTROL
;; =============================================================================

(defn pause-job! [job-id]
  (when-let [job (get-job job-id)]
    (when (= (:status job) :running)
      (update-job! job-id {:status :paused})
      (save-checkpoint! job-id)
      (events/publish! :batch/job-paused {:job-id job-id})
      (log/info "Job paused" {:job-id job-id}))))

(defn resume-job! [job-id process-fn]
  (when-let [job (get-job job-id)]
    (when (= (:status job) :paused)
      (update-job! job-id {:status :pending})
      (start-job! job-id process-fn)
      (events/publish! :batch/job-resumed {:job-id job-id})
      (log/info "Job resumed" {:job-id job-id}))))

(defn cancel-job! [job-id]
  (when-let [job (get-job job-id)]
    (when (#{:pending :running :paused} (:status job))
      (update-job! job-id {:status :cancelled :completed-at (System/currentTimeMillis)})
      (when-let [future (:future job)]
        (.cancel future true))
      (events/publish! :batch/job-cancelled {:job-id job-id})
      (log/info "Job cancelled" {:job-id job-id}))))

;; =============================================================================
;; JOB QUERIES
;; =============================================================================

(defn get-all-jobs []
  (vals @job-registry))

(defn get-jobs-by-status [status]
  (filter #(= (:status %) status) (get-all-jobs)))

(defn get-running-jobs []
  (get-jobs-by-status :running))

(defn get-job-progress [job-id]
  (when-let [job (get-job job-id)]
    {:job-id job-id
     :status (:status job)
     :progress (if (pos? (:total-items job))
                 (/ (:processed-items job) (:total-items job))
                 0)
     :processed (:processed-items job)
     :failed (:failed-items job)
     :total (:total-items job)
     :elapsed-ms (when (:started-at job)
                   (- (or (:completed-at job) (System/currentTimeMillis))
                      (:started-at job)))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-batch-manager!
  "Initialize batch job manager."
  [& {:keys [thread-pool-size] :or {thread-pool-size 4}}]
  (log/info "Initializing batch manager" {:thread-pool-size thread-pool-size})
  (reset! executor (Executors/newFixedThreadPool thread-pool-size))
  ;; Register feature flag
  (flags/register-flag! "batch-processing" "Enable batch processing" true)
  ;; Create metrics
  (metrics/create-counter! :batch-manager/jobs-created "Jobs created")
  (metrics/create-counter! :batch-manager/jobs-completed "Jobs completed")
  (metrics/create-counter! :batch-manager/jobs-cancelled "Jobs cancelled")
  (metrics/create-gauge! :batch-manager/running-jobs "Running jobs" #(count (get-running-jobs)))
  (log/info "Batch manager initialized"))

(defn shutdown-batch-manager! []
  (log/info "Shutting down batch manager")
  (when-let [exec @executor]
    (.shutdown exec)
    (.awaitTermination exec 30 TimeUnit/SECONDS))
  (reset! executor nil)
  (log/info "Batch manager shutdown complete"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-manager-status []
  {:enabled (flags/is-enabled? "batch-processing")
   :total-jobs (count @job-registry)
   :running-jobs (count (get-running-jobs))
   :pending-jobs (count (get-jobs-by-status :pending))
   :completed-jobs (count (get-jobs-by-status :completed))})
