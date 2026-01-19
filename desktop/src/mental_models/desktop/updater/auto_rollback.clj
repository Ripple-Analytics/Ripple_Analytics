(ns mental-models.desktop.updater.auto-rollback
  "Auto-Rollback System - Automatic crash detection and recovery
   
   Features:
   - Crash detection on startup
   - Automatic backup version restore
   - Crash marker file management
   - Safe mode after multiple crashes
   - User data preservation during rollback
   - Startup health verification
   - Rollback history tracking
   - Configurable crash thresholds"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.time Instant Duration]
           [java.nio.file Files Paths StandardCopyOption]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:base-dir (str (System/getProperty "user.home") "/.mental-models")
         :crash-marker-file "crash_marker.edn"
         :startup-marker-file "startup_marker.edn"
         :backup-dir "backups"
         :max-backups 3
         :crash-threshold 3           ;; Crashes before safe mode
         :crash-window-ms (* 60 60 1000)  ;; 1 hour window for counting crashes
         :startup-timeout-ms 30000    ;; 30 seconds to verify startup
         :safe-mode-cooldown-ms (* 24 60 60 1000)}))  ;; 24 hours in safe mode

(defonce rollback-state
  (atom {:crash-count 0
         :last-crash nil
         :in-safe-mode false
         :safe-mode-started nil
         :current-backup nil
         :rollback-history []
         :startup-verified false}))

;; =============================================================================
;; File Paths
;; =============================================================================

(defn get-crash-marker-path []
  (str (:base-dir @config) "/" (:crash-marker-file @config)))

(defn get-startup-marker-path []
  (str (:base-dir @config) "/" (:startup-marker-file @config)))

(defn get-backup-dir []
  (str (:base-dir @config) "/" (:backup-dir @config)))

(defn get-backup-path [version]
  (str (get-backup-dir) "/backup-" version))

;; =============================================================================
;; Crash Marker Management
;; =============================================================================

(defn read-crash-marker
  "Read the crash marker file."
  []
  (let [marker-file (io/file (get-crash-marker-path))]
    (when (.exists marker-file)
      (try
        (edn/read-string (slurp marker-file))
        (catch Exception e
          (log/warn "Failed to read crash marker:" (.getMessage e))
          nil)))))

(defn write-crash-marker!
  "Write a crash marker indicating the app started but didn't complete startup."
  [version]
  (let [marker-file (io/file (get-crash-marker-path))
        marker-data {:version version
                     :started-at (str (Instant/now))
                     :pid (-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
                              .getName
                              (str/split #"@")
                              first)}]
    (try
      (io/make-parents marker-file)
      (spit marker-file (pr-str marker-data))
      (log/debug "Crash marker written:" marker-data)
      true
      (catch Exception e
        (log/error "Failed to write crash marker:" (.getMessage e))
        false))))

(defn clear-crash-marker!
  "Clear the crash marker after successful startup."
  []
  (let [marker-file (io/file (get-crash-marker-path))]
    (when (.exists marker-file)
      (try
        (.delete marker-file)
        (log/debug "Crash marker cleared")
        true
        (catch Exception e
          (log/warn "Failed to clear crash marker:" (.getMessage e))
          false)))))

(defn crash-marker-exists?
  "Check if a crash marker exists (indicates previous crash)."
  []
  (.exists (io/file (get-crash-marker-path))))

;; =============================================================================
;; Startup Marker Management
;; =============================================================================

(defn write-startup-marker!
  "Write a startup marker indicating successful startup."
  [version]
  (let [marker-file (io/file (get-startup-marker-path))
        marker-data {:version version
                     :started-at (str (Instant/now))
                     :startup-verified true}]
    (try
      (io/make-parents marker-file)
      (spit marker-file (pr-str marker-data))
      (swap! rollback-state assoc :startup-verified true)
      (log/info "Startup verified successfully")
      true
      (catch Exception e
        (log/error "Failed to write startup marker:" (.getMessage e))
        false))))

(defn read-startup-marker
  "Read the startup marker."
  []
  (let [marker-file (io/file (get-startup-marker-path))]
    (when (.exists marker-file)
      (try
        (edn/read-string (slurp marker-file))
        (catch Exception _ nil)))))

;; =============================================================================
;; Crash Detection
;; =============================================================================

(defn detect-crash!
  "Detect if the previous run crashed.
   Returns crash info if a crash was detected."
  []
  (when-let [marker (read-crash-marker)]
    (log/warn "========================================")
    (log/warn "CRASH DETECTED!")
    (log/warn "Previous version:" (:version marker))
    (log/warn "Started at:" (:started-at marker))
    (log/warn "========================================")
    
    ;; Update crash count
    (let [now (System/currentTimeMillis)
          window (:crash-window-ms @config)
          last-crash (:last-crash @rollback-state)
          reset-count? (and last-crash
                            (> (- now last-crash) window))]
      (if reset-count?
        (swap! rollback-state assoc :crash-count 1 :last-crash now)
        (swap! rollback-state update :crash-count inc))
      (swap! rollback-state assoc :last-crash now))
    
    marker))

(defn should-enter-safe-mode?
  "Check if we should enter safe mode due to repeated crashes."
  []
  (>= (:crash-count @rollback-state) (:crash-threshold @config)))

(defn enter-safe-mode!
  "Enter safe mode after repeated crashes."
  []
  (log/warn "========================================")
  (log/warn "ENTERING SAFE MODE")
  (log/warn "Too many consecutive crashes detected")
  (log/warn "========================================")
  (swap! rollback-state assoc 
         :in-safe-mode true
         :safe-mode-started (System/currentTimeMillis))
  (clear-crash-marker!))

(defn exit-safe-mode!
  "Exit safe mode."
  []
  (log/info "Exiting safe mode")
  (swap! rollback-state assoc 
         :in-safe-mode false
         :safe-mode-started nil
         :crash-count 0))

(defn in-safe-mode?
  "Check if currently in safe mode."
  []
  (let [state @rollback-state]
    (and (:in-safe-mode state)
         (let [started (:safe-mode-started state)
               cooldown (:safe-mode-cooldown-ms @config)]
           (or (nil? started)
               (< (- (System/currentTimeMillis) started) cooldown))))))

;; =============================================================================
;; Backup Management
;; =============================================================================

(defn ensure-backup-dir!
  "Ensure the backup directory exists."
  []
  (let [dir (io/file (get-backup-dir))]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn list-backups
  "List all available backups, sorted by date (newest first)."
  []
  (ensure-backup-dir!)
  (let [backup-dir (io/file (get-backup-dir))]
    (->> (.listFiles backup-dir)
         (filter #(.isDirectory %))
         (filter #(str/starts-with? (.getName %) "backup-"))
         (sort-by #(.lastModified %) >)
         (map (fn [f]
                {:path (.getAbsolutePath f)
                 :version (str/replace (.getName f) "backup-" "")
                 :created-at (.lastModified f)
                 :size (reduce + (map #(.length %) (file-seq f)))})))))

(defn create-backup!
  "Create a backup of the current version."
  [version source-dir]
  (log/info "Creating backup of version:" version)
  (ensure-backup-dir!)
  
  (let [backup-path (get-backup-path version)
        backup-dir (io/file backup-path)]
    (try
      ;; Remove existing backup of same version
      (when (.exists backup-dir)
        (log/debug "Removing existing backup:" backup-path)
        (doseq [f (reverse (file-seq backup-dir))]
          (.delete f)))
      
      ;; Create backup directory
      (.mkdirs backup-dir)
      
      ;; Copy all files
      (let [source (io/file source-dir)]
        (doseq [f (file-seq source)
                :when (.isFile f)]
          (let [relative (.relativize (.toPath source) (.toPath f))
                target (io/file backup-dir (.toString relative))]
            (io/make-parents target)
            (io/copy f target))))
      
      ;; Clean up old backups
      (let [backups (list-backups)
            max-backups (:max-backups @config)]
        (when (> (count backups) max-backups)
          (doseq [old-backup (drop max-backups backups)]
            (log/info "Removing old backup:" (:version old-backup))
            (doseq [f (reverse (file-seq (io/file (:path old-backup))))]
              (.delete f)))))
      
      (log/info "Backup created successfully:" backup-path)
      {:success true :path backup-path}
      
      (catch Exception e
        (log/error e "Failed to create backup")
        {:success false :error (.getMessage e)}))))

(defn get-latest-backup
  "Get the most recent backup."
  []
  (first (list-backups)))

(defn get-backup-for-version
  "Get backup for a specific version."
  [version]
  (first (filter #(= (:version %) version) (list-backups))))

;; =============================================================================
;; Rollback Operations
;; =============================================================================

(defn restore-backup!
  "Restore a backup to the target directory."
  [backup-path target-dir]
  (log/info "========================================")
  (log/info "RESTORING BACKUP")
  (log/info "From:" backup-path)
  (log/info "To:" target-dir)
  (log/info "========================================")
  
  (try
    (let [backup (io/file backup-path)
          target (io/file target-dir)]
      
      ;; Verify backup exists
      (when-not (.exists backup)
        (throw (ex-info "Backup not found" {:path backup-path})))
      
      ;; Clear target directory (but preserve user data)
      (when (.exists target)
        (doseq [f (file-seq target)
                :when (and (.isFile f)
                           (not (str/includes? (.getPath f) "user_data"))
                           (not (str/includes? (.getPath f) "config"))
                           (not (str/includes? (.getPath f) "database")))]
          (.delete f)))
      
      ;; Copy backup files
      (doseq [f (file-seq backup)
              :when (.isFile f)]
        (let [relative (.relativize (.toPath backup) (.toPath f))
              target-file (io/file target (.toString relative))]
          (io/make-parents target-file)
          (io/copy f target-file)))
      
      ;; Record rollback
      (swap! rollback-state update :rollback-history conj
             {:from-backup backup-path
              :to-target target-dir
              :timestamp (str (Instant/now))})
      
      (log/info "Backup restored successfully")
      {:success true})
    
    (catch Exception e
      (log/error e "Failed to restore backup")
      {:success false :error (.getMessage e)})))

(defn auto-rollback!
  "Automatically rollback to the latest working backup."
  [target-dir]
  (log/warn "Initiating automatic rollback...")
  
  (if-let [backup (get-latest-backup)]
    (do
      (log/info "Found backup:" (:version backup))
      (let [result (restore-backup! (:path backup) target-dir)]
        (when (:success result)
          (swap! rollback-state assoc :current-backup backup))
        result))
    (do
      (log/error "No backup available for rollback!")
      {:success false :error :no-backup-available})))

;; =============================================================================
;; Startup Verification
;; =============================================================================

(defn verify-startup!
  "Verify that the application started successfully.
   Call this after all critical initialization is complete."
  [version]
  (log/info "Verifying startup for version:" version)
  
  ;; Clear crash marker
  (clear-crash-marker!)
  
  ;; Write startup marker
  (write-startup-marker! version)
  
  ;; Reset crash count on successful startup
  (swap! rollback-state assoc :crash-count 0)
  
  (log/info "Startup verification complete")
  {:success true})

(defn on-startup!
  "Called at application startup to handle crash detection and recovery.
   
   Returns:
   - :normal - Normal startup, proceed
   - :recovered - Recovered from crash, proceed with caution
   - :safe-mode - In safe mode, limited functionality
   - :rollback-failed - Rollback attempted but failed"
  [version target-dir]
  (log/info "========================================")
  (log/info "Startup check for version:" version)
  (log/info "========================================")
  
  ;; Check for previous crash
  (if-let [crash-info (detect-crash!)]
    (do
      (log/warn "Previous crash detected, checking recovery options...")
      
      (if (should-enter-safe-mode?)
        (do
          (enter-safe-mode!)
          :safe-mode)
        
        ;; Attempt auto-rollback
        (let [rollback-result (auto-rollback! target-dir)]
          (if (:success rollback-result)
            (do
              (log/info "Rollback successful, starting recovered version")
              :recovered)
            (do
              (log/error "Rollback failed!")
              :rollback-failed)))))
    
    ;; No crash detected, normal startup
    (do
      ;; Write crash marker (will be cleared on successful startup)
      (write-crash-marker! version)
      :normal)))

;; =============================================================================
;; State Preservation
;; =============================================================================

(def preserved-paths
  "Paths that should be preserved during rollback."
  ["user_data"
   "config"
   "database"
   "watched_folders.edn"
   "scan_history.edn"
   "decision_journal.edn"
   "settings.edn"])

(defn is-preserved-path?
  "Check if a path should be preserved during rollback."
  [path]
  (some #(str/includes? path %) preserved-paths))

(defn backup-user-data!
  "Backup user data before a risky operation."
  [source-dir]
  (let [user-data-backup (str (:base-dir @config) "/user_data_backup")]
    (try
      (doseq [preserved preserved-paths]
        (let [source-file (io/file source-dir preserved)]
          (when (.exists source-file)
            (let [target-file (io/file user-data-backup preserved)]
              (io/make-parents target-file)
              (if (.isDirectory source-file)
                (doseq [f (file-seq source-file)
                        :when (.isFile f)]
                  (let [relative (.relativize (.toPath source-file) (.toPath f))
                        target (io/file target-file (.toString relative))]
                    (io/make-parents target)
                    (io/copy f target)))
                (io/copy source-file target-file))))))
      {:success true :path user-data-backup}
      (catch Exception e
        (log/error e "Failed to backup user data")
        {:success false :error (.getMessage e)}))))

(defn restore-user-data!
  "Restore user data after rollback."
  [target-dir]
  (let [user-data-backup (str (:base-dir @config) "/user_data_backup")]
    (when (.exists (io/file user-data-backup))
      (try
        (doseq [preserved preserved-paths]
          (let [backup-file (io/file user-data-backup preserved)
                target-file (io/file target-dir preserved)]
            (when (.exists backup-file)
              (io/make-parents target-file)
              (if (.isDirectory backup-file)
                (doseq [f (file-seq backup-file)
                        :when (.isFile f)]
                  (let [relative (.relativize (.toPath backup-file) (.toPath f))
                        target (io/file target-file (.toString relative))]
                    (io/make-parents target)
                    (io/copy f target)))
                (io/copy backup-file target-file)))))
        {:success true}
        (catch Exception e
          (log/error e "Failed to restore user data")
          {:success false :error (.getMessage e)})))))

;; =============================================================================
;; Status & Monitoring
;; =============================================================================

(defn get-rollback-status
  "Get current rollback system status."
  []
  (let [state @rollback-state]
    {:crash-count (:crash-count state)
     :last-crash (:last-crash state)
     :in-safe-mode (in-safe-mode?)
     :safe-mode-started (:safe-mode-started state)
     :current-backup (:current-backup state)
     :startup-verified (:startup-verified state)
     :available-backups (count (list-backups))
     :rollback-history (take 10 (:rollback-history state))}))

(defn print-status
  "Print rollback system status."
  []
  (let [status (get-rollback-status)]
    (println "\n╔════════════════════════════════════════╗")
    (println "║       AUTO-ROLLBACK STATUS             ║")
    (println "╠════════════════════════════════════════╣")
    (println (str "║ Crash count: " (:crash-count status)))
    (println (str "║ Safe mode: " (:in-safe-mode status)))
    (println (str "║ Startup verified: " (:startup-verified status)))
    (println (str "║ Available backups: " (:available-backups status)))
    (println "╚════════════════════════════════════════╝\n")))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-auto-rollback!
  "Initialize the auto-rollback system."
  []
  (log/info "Initializing auto-rollback system...")
  (ensure-backup-dir!)
  {:success true})
