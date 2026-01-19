(ns mental-models.desktop.updater.logging-reporter
  "Logging & Reporting Module - Comprehensive update logging and failure reporting
   
   Covers Category 15: Logging & Reporting
   - Failure reports with full context
   - Log capture and rotation
   - Sensitive data masking
   - User-friendly error messages
   - Structured logging for debugging"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.io File FileWriter BufferedWriter]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util.zip GZIPOutputStream]
           [java.nio.file Files Paths StandardCopyOption]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def log-config
  (atom {:log-dir (str (System/getProperty "user.home") "/.mental-models/logs")
         :max-log-size-mb 10
         :max-log-files 5
         :log-level :info
         :mask-patterns [#"(?i)(password|token|secret|key|auth)[=:]\s*\S+"
                         #"(?i)bearer\s+\S+"
                         #"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"
                         #"\b\d{4}[- ]?\d{4}[- ]?\d{4}[- ]?\d{4}\b"]
         :user-friendly-messages true}))

(defonce log-state
  (atom {:current-log-file nil
         :log-count 0
         :error-count 0
         :warning-count 0
         :last-error nil
         :session-id (str (java.util.UUID/randomUUID))
         :session-start (str (Instant/now))}))

;; =============================================================================
;; Sensitive Data Masking
;; =============================================================================

(defn mask-sensitive-data
  "Mask sensitive data in log messages"
  [message]
  (if (string? message)
    (reduce (fn [msg pattern]
              (str/replace msg pattern "[REDACTED]"))
            message
            (:mask-patterns @log-config))
    message))

(defn mask-map-values
  "Mask sensitive values in a map"
  [m sensitive-keys]
  (reduce (fn [acc k]
            (if (contains? acc k)
              (assoc acc k "[REDACTED]")
              acc))
          m
          sensitive-keys))

;; =============================================================================
;; Log File Management
;; =============================================================================

(defn get-log-dir []
  (let [dir (io/file (:log-dir @log-config))]
    (.mkdirs dir)
    dir))

(defn get-current-log-file []
  (let [date-str (.format (LocalDateTime/now (ZoneId/systemDefault))
                          (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
        log-file (io/file (get-log-dir) (str "update-" date-str ".log"))]
    log-file))

(defn get-log-files []
  "Get all log files sorted by modification time (newest first)"
  (->> (get-log-dir)
       (.listFiles)
       (filter #(.endsWith (.getName %) ".log"))
       (sort-by #(.lastModified %) >)))

(defn compress-log-file!
  "Compress a log file using gzip"
  [log-file]
  (let [gz-file (io/file (str (.getAbsolutePath log-file) ".gz"))]
    (with-open [in (io/input-stream log-file)
                out (GZIPOutputStream. (io/output-stream gz-file))]
      (io/copy in out))
    (.delete log-file)
    gz-file))

(defn rotate-logs!
  "Rotate log files, keeping only the configured number"
  []
  (let [log-files (get-log-files)
        max-files (:max-log-files @log-config)]
    (when (> (count log-files) max-files)
      (doseq [old-file (drop max-files log-files)]
        (log/debug "Deleting old log file:" (.getName old-file))
        (.delete old-file)))))

(defn check-log-size!
  "Check if current log file needs rotation"
  []
  (let [log-file (get-current-log-file)
        max-size (* (:max-log-size-mb @log-config) 1024 1024)]
    (when (and (.exists log-file) (> (.length log-file) max-size))
      (log/info "Log file exceeded max size, compressing...")
      (compress-log-file! log-file)
      (rotate-logs!))))

;; =============================================================================
;; Structured Logging
;; =============================================================================

(defn format-log-entry
  "Format a structured log entry"
  [level category message & {:keys [data error]}]
  (let [timestamp (str (Instant/now))
        session-id (:session-id @log-state)
        entry {:timestamp timestamp
               :level (name level)
               :category category
               :message (mask-sensitive-data message)
               :session-id session-id}
        entry (if data (assoc entry :data (mask-sensitive-data (pr-str data))) entry)
        entry (if error (assoc entry :error {:type (type error)
                                              :message (.getMessage error)
                                              :stack (take 10 (map str (.getStackTrace error)))})
                entry)]
    entry))

(defn write-log-entry!
  "Write a log entry to the log file"
  [entry]
  (check-log-size!)
  (let [log-file (get-current-log-file)
        line (str (pr-str entry) "\n")]
    (with-open [writer (BufferedWriter. (FileWriter. log-file true))]
      (.write writer line))
    (swap! log-state update :log-count inc)))

(defn log-update-event!
  "Log an update-related event"
  [level category message & opts]
  (let [entry (apply format-log-entry level category message opts)]
    (write-log-entry! entry)
    (case level
      :error (do
               (swap! log-state update :error-count inc)
               (swap! log-state assoc :last-error entry)
               (log/error message))
      :warn (do
              (swap! log-state update :warning-count inc)
              (log/warn message))
      :info (log/info message)
      :debug (log/debug message)
      (log/info message))))

;; =============================================================================
;; User-Friendly Error Messages
;; =============================================================================

(def error-messages
  "Map of error codes to user-friendly messages"
  {:network-error "Unable to connect to the update server. Please check your internet connection and try again."
   :download-failed "The update download was interrupted. Please try again."
   :disk-full "Not enough disk space to download the update. Please free up some space and try again."
   :checksum-mismatch "The downloaded update appears to be corrupted. Please try downloading again."
   :extraction-failed "Failed to extract the update package. The download may be corrupted."
   :permission-denied "Permission denied while installing the update. Please run as administrator."
   :version-conflict "A newer version is already installed."
   :rollback-failed "Failed to rollback to the previous version. Please reinstall the application."
   :timeout "The update operation timed out. Please try again."
   :unknown "An unexpected error occurred. Please try again or contact support."})

(defn get-user-friendly-message
  "Convert a technical error to a user-friendly message"
  [error-code & {:keys [details]}]
  (let [base-message (get error-messages error-code (:unknown error-messages))]
    (if details
      (str base-message " (" details ")")
      base-message)))

(defn classify-error
  "Classify an exception into an error code"
  [exception]
  (let [msg (str/lower-case (or (.getMessage exception) ""))]
    (cond
      (or (str/includes? msg "connection")
          (str/includes? msg "network")
          (str/includes? msg "unreachable")) :network-error
      (or (str/includes? msg "space")
          (str/includes? msg "disk full")) :disk-full
      (or (str/includes? msg "checksum")
          (str/includes? msg "hash")) :checksum-mismatch
      (or (str/includes? msg "permission")
          (str/includes? msg "access denied")) :permission-denied
      (str/includes? msg "timeout") :timeout
      (or (str/includes? msg "extract")
          (str/includes? msg "zip")) :extraction-failed
      :else :unknown)))

;; =============================================================================
;; Failure Reports
;; =============================================================================

(defn generate-failure-report
  "Generate a comprehensive failure report"
  [error context]
  (let [timestamp (str (Instant/now))
        report {:report-id (str (java.util.UUID/randomUUID))
                :timestamp timestamp
                :session-id (:session-id @log-state)
                :error {:type (str (type error))
                        :message (.getMessage error)
                        :stack-trace (mapv str (.getStackTrace error))}
                :context (mask-sensitive-data (pr-str context))
                :system {:os (System/getProperty "os.name")
                         :os-version (System/getProperty "os.version")
                         :java-version (System/getProperty "java.version")
                         :available-memory (/ (.freeMemory (Runtime/getRuntime)) 1024 1024)
                         :total-memory (/ (.totalMemory (Runtime/getRuntime)) 1024 1024)}
                :log-summary {:total-logs (:log-count @log-state)
                              :errors (:error-count @log-state)
                              :warnings (:warning-count @log-state)}}]
    report))

(defn save-failure-report!
  "Save a failure report to disk"
  [report]
  (let [report-dir (io/file (get-log-dir) "failure-reports")
        _ (.mkdirs report-dir)
        report-file (io/file report-dir (str "failure-" (:report-id report) ".edn"))]
    (spit report-file (pr-str report))
    (log/info "Failure report saved:" (.getName report-file))
    report-file))

(defn report-failure!
  "Report a failure with full context"
  [error context]
  (let [report (generate-failure-report error context)]
    (log-update-event! :error "failure" (.getMessage error) :error error :data context)
    (save-failure-report! report)
    {:report-id (:report-id report)
     :user-message (get-user-friendly-message (classify-error error))
     :report-file (str (io/file (get-log-dir) "failure-reports" (str "failure-" (:report-id report) ".edn")))}))

;; =============================================================================
;; Log Analysis
;; =============================================================================

(defn read-log-file
  "Read and parse a log file"
  [log-file]
  (when (.exists log-file)
    (with-open [rdr (io/reader log-file)]
      (doall
        (for [line (line-seq rdr)
              :when (not (str/blank? line))]
          (try
            (read-string line)
            (catch Exception _ {:raw line})))))))

(defn get-recent-errors
  "Get recent error entries from logs"
  [& {:keys [limit] :or {limit 10}}]
  (let [log-file (get-current-log-file)
        entries (read-log-file log-file)]
    (->> entries
         (filter #(= "error" (:level %)))
         (take-last limit)
         (reverse))))

(defn get-log-summary
  "Get a summary of log activity"
  []
  (let [log-file (get-current-log-file)
        entries (read-log-file log-file)
        by-level (group-by :level entries)
        by-category (group-by :category entries)]
    {:total-entries (count entries)
     :by-level (into {} (map (fn [[k v]] [k (count v)]) by-level))
     :by-category (into {} (map (fn [[k v]] [k (count v)]) by-category))
     :session-stats @log-state}))

;; =============================================================================
;; Log Export
;; =============================================================================

(defn export-logs-for-support
  "Export logs for support ticket"
  [& {:keys [include-reports] :or {include-reports true}}]
  (let [export-dir (io/file (get-log-dir) "export")
        _ (.mkdirs export-dir)
        timestamp (.format (LocalDateTime/now (ZoneId/systemDefault))
                           (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))
        export-file (io/file export-dir (str "support-logs-" timestamp ".zip"))]
    
    ;; Create zip with logs
    (with-open [zos (java.util.zip.ZipOutputStream. (io/output-stream export-file))]
      ;; Add log files
      (doseq [log-file (take 3 (get-log-files))]
        (let [entry (java.util.zip.ZipEntry. (.getName log-file))]
          (.putNextEntry zos entry)
          (io/copy (io/input-stream log-file) zos)
          (.closeEntry zos)))
      
      ;; Add failure reports if requested
      (when include-reports
        (let [reports-dir (io/file (get-log-dir) "failure-reports")]
          (when (.exists reports-dir)
            (doseq [report-file (.listFiles reports-dir)]
              (let [entry (java.util.zip.ZipEntry. (str "reports/" (.getName report-file)))]
                (.putNextEntry zos entry)
                (io/copy (io/input-stream report-file) zos)
                (.closeEntry zos))))))
      
      ;; Add summary
      (let [summary-entry (java.util.zip.ZipEntry. "summary.edn")]
        (.putNextEntry zos summary-entry)
        (.write zos (.getBytes (pr-str (get-log-summary))))
        (.closeEntry zos)))
    
    (log/info "Logs exported to:" (.getAbsolutePath export-file))
    {:file export-file
     :size (.length export-file)}))

;; =============================================================================
;; Convenience Logging Functions
;; =============================================================================

(defn log-download-start! [url size]
  (log-update-event! :info "download" (str "Starting download: " (mask-sensitive-data url))
                     :data {:url (mask-sensitive-data url) :size size}))

(defn log-download-progress! [url progress total]
  (log-update-event! :debug "download" (str "Download progress: " progress "/" total)
                     :data {:progress progress :total total}))

(defn log-download-complete! [url file-path]
  (log-update-event! :info "download" "Download complete"
                     :data {:file file-path}))

(defn log-extraction-start! [archive-path target-path]
  (log-update-event! :info "extraction" "Starting extraction"
                     :data {:archive archive-path :target target-path}))

(defn log-extraction-complete! [target-path file-count]
  (log-update-event! :info "extraction" (str "Extraction complete: " file-count " files")
                     :data {:target target-path :files file-count}))

(defn log-version-check! [current latest]
  (log-update-event! :info "version" (str "Version check: " current " -> " latest)
                     :data {:current current :latest latest}))

(defn log-update-available! [version]
  (log-update-event! :info "update" (str "Update available: " version)
                     :data {:version version}))

(defn log-update-applied! [old-version new-version]
  (log-update-event! :info "update" (str "Update applied: " old-version " -> " new-version)
                     :data {:old-version old-version :new-version new-version}))

(defn log-rollback! [from-version to-version reason]
  (log-update-event! :warn "rollback" (str "Rolling back: " from-version " -> " to-version)
                     :data {:from from-version :to to-version :reason reason}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-logging!
  "Initialize the logging system"
  []
  (let [log-dir (get-log-dir)]
    (log/info "Initializing update logging system")
    (log/info "Log directory:" (.getAbsolutePath log-dir))
    (rotate-logs!)
    (log-update-event! :info "system" "Logging system initialized"
                       :data {:session-id (:session-id @log-state)})
    {:log-dir log-dir
     :session-id (:session-id @log-state)}))

(defn get-logging-status []
  "Get current logging status"
  {:state @log-state
   :config (dissoc @log-config :mask-patterns)
   :log-files (count (get-log-files))
   :current-log-file (str (get-current-log-file))})
