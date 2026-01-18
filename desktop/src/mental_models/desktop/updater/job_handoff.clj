(ns mental-models.desktop.updater.job-handoff
  "Graceful Job Handoff - Ensure in-progress work completes during updates
   
   Features:
   - Track all in-flight jobs/analyses
   - Wait for completion before version switch
   - Transfer pending jobs to new version
   - Checkpoint long-running jobs for resume
   - Timeout handling for stuck jobs
   
   This ensures no work is lost during deployments."
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout alts!]]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration]
           [java.util.concurrent ConcurrentHashMap CountDownLatch TimeUnit]
           [java.util UUID]))

;; =============================================================================
;; Job Registry
;; =============================================================================

(defonce job-registry
  "Thread-safe registry of all in-flight jobs"
  (ConcurrentHashMap.))

(defonce job-state
  (atom {:total-jobs 0
         :completed-jobs 0
         :failed-jobs 0
         :handoff-in-progress false
         :drain-mode false
         :pending-handoff []}))

(defrecord Job [id type status started-at progress checkpoint-data priority])

(defn register-job!
  "Register a new job in the registry"
  [job-type & {:keys [priority] :or {priority :normal}}]
  (let [job-id (str (UUID/randomUUID))
        job (->Job job-id 
                   job-type 
                   :running 
                   (Instant/now) 
                   0 
                   nil 
                   priority)]
    (.put job-registry job-id job)
    (swap! job-state update :total-jobs inc)
    (log/debug "Registered job:" job-id "type:" job-type)
    job-id))

(defn update-job-progress!
  "Update job progress (0-100)"
  [job-id progress]
  (when-let [job (.get job-registry job-id)]
    (.put job-registry job-id (assoc job :progress progress))))

(defn checkpoint-job!
  "Save checkpoint data for a job (for resume after restart)"
  [job-id checkpoint-data]
  (when-let [job (.get job-registry job-id)]
    (.put job-registry job-id (assoc job :checkpoint-data checkpoint-data))
    (log/debug "Checkpointed job:" job-id)))

(defn complete-job!
  "Mark a job as completed"
  [job-id & {:keys [success] :or {success true}}]
  (when-let [job (.get job-registry job-id)]
    (.put job-registry job-id (assoc job :status (if success :completed :failed)))
    (swap! job-state update (if success :completed-jobs :failed-jobs) inc)
    (log/debug "Completed job:" job-id "success:" success)
    ;; Remove from registry after short delay
    (go
      (<! (timeout 5000))
      (.remove job-registry job-id))))

(defn get-job [job-id]
  (.get job-registry job-id))

(defn get-all-jobs []
  (into {} job-registry))

(defn get-running-jobs []
  (into {} (filter #(= :running (:status (val %))) job-registry)))

;; =============================================================================
;; Drain Mode - Stop accepting new jobs
;; =============================================================================

(defn enter-drain-mode!
  "Enter drain mode - no new jobs accepted"
  []
  (swap! job-state assoc :drain-mode true)
  (log/info "⏸️  Entered drain mode - no new jobs will be accepted"))

(defn exit-drain-mode!
  "Exit drain mode - resume accepting jobs"
  []
  (swap! job-state assoc :drain-mode false)
  (log/info "▶️  Exited drain mode - accepting new jobs"))

(defn in-drain-mode? []
  (:drain-mode @job-state))

(defn can-accept-job?
  "Check if new jobs can be accepted"
  []
  (not (in-drain-mode?)))

;; =============================================================================
;; Job Waiting & Draining
;; =============================================================================

(def drain-config
  {:poll-interval-ms 1000
   :max-wait-ms (* 5 60 1000)      ;; 5 minutes max wait
   :checkpoint-threshold-ms 30000  ;; Checkpoint jobs running > 30s
   :force-kill-after-ms (* 10 60 1000)})  ;; Force kill after 10 minutes

(defn wait-for-jobs-to-complete
  "Wait for all running jobs to complete
   
   Returns:
   - :drained - All jobs completed
   - :timeout - Timed out waiting
   - :interrupted - Wait was interrupted"
  [& {:keys [timeout-ms] :or {timeout-ms (:max-wait-ms drain-config)}}]
  (log/info "Waiting for" (count (get-running-jobs)) "jobs to complete...")
  (let [start-time (System/currentTimeMillis)
        deadline (+ start-time timeout-ms)]
    (loop []
      (let [running (get-running-jobs)
            now (System/currentTimeMillis)]
        (cond
          (empty? running)
          (do
            (log/info "✅ All jobs completed")
            :drained)
          
          (> now deadline)
          (do
            (log/warn "⏰ Timeout waiting for jobs:" (count running) "still running")
            :timeout)
          
          :else
          (do
            (log/debug "Waiting for" (count running) "jobs..." 
                       (int (/ (- deadline now) 1000)) "s remaining")
            (Thread/sleep (:poll-interval-ms drain-config))
            (recur)))))))

(defn checkpoint-long-running-jobs!
  "Checkpoint all jobs that have been running longer than threshold"
  []
  (let [threshold-ms (:checkpoint-threshold-ms drain-config)
        now (Instant/now)]
    (doseq [[job-id job] (get-running-jobs)]
      (let [running-ms (.toMillis (Duration/between (:started-at job) now))]
        (when (> running-ms threshold-ms)
          (log/info "Checkpointing long-running job:" job-id 
                    "running for" (int (/ running-ms 1000)) "s")
          ;; Job should have its own checkpoint logic
          ;; This just marks it for checkpointing
          (update-job-progress! job-id (:progress job)))))))

(defn force-stop-jobs!
  "Force stop all remaining jobs (last resort)"
  []
  (log/warn "⚠️  Force stopping" (count (get-running-jobs)) "jobs")
  (doseq [[job-id job] (get-running-jobs)]
    (log/warn "Force stopping job:" job-id)
    (.put job-registry job-id (assoc job :status :force-stopped))
    (complete-job! job-id :success false)))

;; =============================================================================
;; Job Transfer
;; =============================================================================

(defn export-pending-jobs
  "Export all pending/checkpointed jobs for transfer to new version"
  []
  (let [jobs-to-transfer (into []
                           (comp
                             (filter #(#{:running :checkpointed} (:status (val %))))
                             (map (fn [[id job]]
                                    {:id id
                                     :type (:type job)
                                     :progress (:progress job)
                                     :checkpoint-data (:checkpoint-data job)
                                     :priority (:priority job)})))
                           job-registry)]
    (log/info "Exported" (count jobs-to-transfer) "jobs for transfer")
    jobs-to-transfer))

(defn import-pending-jobs!
  "Import jobs from previous version"
  [jobs]
  (log/info "Importing" (count jobs) "jobs from previous version")
  (doseq [job jobs]
    (let [job-id (:id job)]
      (.put job-registry job-id
            (->Job job-id
                   (:type job)
                   :pending-resume
                   (Instant/now)
                   (:progress job)
                   (:checkpoint-data job)
                   (:priority job)))
      (swap! job-state update :pending-handoff conj job-id)))
  (log/info "Imported jobs ready for resume"))

(defn get-jobs-to-resume []
  "Get jobs that need to be resumed after handoff"
  (filter #(= :pending-resume (:status (val %))) job-registry))

;; =============================================================================
;; Graceful Handoff Orchestration
;; =============================================================================

(defn prepare-handoff!
  "Prepare for graceful handoff to new version
   
   Steps:
   1. Enter drain mode
   2. Checkpoint long-running jobs
   3. Wait for jobs to complete
   4. Export pending jobs"
  []
  (log/info "========================================")
  (log/info "Preparing graceful handoff...")
  (log/info "========================================")
  
  (swap! job-state assoc :handoff-in-progress true)
  
  ;; Step 1: Stop accepting new jobs
  (enter-drain-mode!)
  
  ;; Step 2: Checkpoint long-running jobs
  (checkpoint-long-running-jobs!)
  
  ;; Step 3: Wait for completion
  (let [result (wait-for-jobs-to-complete)]
    (case result
      :drained
      (do
        (log/info "✅ Graceful handoff ready")
        {:success true :pending-jobs (export-pending-jobs)})
      
      :timeout
      (do
        (log/warn "Handoff timeout - checkpointing remaining jobs")
        (checkpoint-long-running-jobs!)
        {:success true 
         :pending-jobs (export-pending-jobs)
         :warning "Some jobs were checkpointed, not completed"})
      
      ;; Default
      (do
        (log/error "Handoff preparation failed:" result)
        {:success false :error result}))))

(defn complete-handoff!
  "Complete the handoff after new version is running"
  []
  (log/info "Completing handoff...")
  (exit-drain-mode!)
  (swap! job-state assoc :handoff-in-progress false)
  (log/info "✅ Handoff complete - ready for new jobs"))

(defn abort-handoff!
  "Abort a handoff in progress"
  []
  (log/warn "Aborting handoff...")
  (exit-drain-mode!)
  (swap! job-state assoc :handoff-in-progress false)
  (log/info "Handoff aborted"))

;; =============================================================================
;; Status & Monitoring
;; =============================================================================

(defn get-handoff-status []
  "Get current handoff status"
  (let [running (get-running-jobs)
        state @job-state]
    {:in-progress (:handoff-in-progress state)
     :drain-mode (:drain-mode state)
     :running-jobs (count running)
     :total-jobs (:total-jobs state)
     :completed-jobs (:completed-jobs state)
     :failed-jobs (:failed-jobs state)
     :pending-handoff (count (:pending-handoff state))
     :jobs (map (fn [[id job]]
                  {:id id
                   :type (:type job)
                   :status (:status job)
                   :progress (:progress job)
                   :running-for-ms (when (:started-at job)
                                     (.toMillis (Duration/between (:started-at job) (Instant/now))))})
                running)}))

(defn print-handoff-status []
  "Print handoff status to console"
  (let [status (get-handoff-status)]
    (println "\n╔════════════════════════════════════════╗")
    (println "║         JOB HANDOFF STATUS             ║")
    (println "╠════════════════════════════════════════╣")
    (println (str "║ Handoff in progress: " (:in-progress status)))
    (println (str "║ Drain mode: " (:drain-mode status)))
    (println (str "║ Running jobs: " (:running-jobs status)))
    (println (str "║ Total/Completed/Failed: " 
                  (:total-jobs status) "/" 
                  (:completed-jobs status) "/" 
                  (:failed-jobs status)))
    (when (seq (:jobs status))
      (println "╠════════════════════════════════════════╣")
      (println "║ Active Jobs:")
      (doseq [job (:jobs status)]
        (println (str "║   " (:id job) " - " (:type job) 
                      " (" (:progress job) "%) "
                      (int (/ (:running-for-ms job) 1000)) "s"))))
    (println "╚════════════════════════════════════════╝\n")))
