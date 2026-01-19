(ns mental-models.desktop.updater.state-persistence
  "State Persistence - Preserve user data across updates
   
   Features:
   - Watched folders persistence
   - Scan history preservation
   - Decision journal backup
   - Settings migration
   - Database integrity verification
   - In-progress job state preservation
   - Configuration versioning
   - Atomic state saves
   - Corruption recovery"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [taoensso.timbre :as log])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.time Instant]
           [java.nio.file Files Paths StandardCopyOption]
           [java.security MessageDigest]
           [java.util.zip GZIPInputStream GZIPOutputStream]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:base-dir (str (System/getProperty "user.home") "/.mental-models")
         :state-dir "state"
         :backup-dir "state_backups"
         :max-backups 5
         :auto-save-interval-ms 60000      ;; 1 minute
         :compression-enabled true
         :verify-on-load true}))

(defonce persistence-state
  (atom {:last-save nil
         :last-load nil
         :save-count 0
         :load-count 0
         :corruption-detected false
         :auto-save-running false}))

;; =============================================================================
;; File Paths
;; =============================================================================

(defn get-state-dir []
  (str (:base-dir @config) "/" (:state-dir @config)))

(defn get-backup-dir []
  (str (:base-dir @config) "/" (:backup-dir @config)))

(defn get-state-file [name]
  (str (get-state-dir) "/" name ".edn"))

(defn get-compressed-state-file [name]
  (str (get-state-dir) "/" name ".edn.gz"))

;; =============================================================================
;; State Files Registry
;; =============================================================================

(def state-files
  "Registry of all state files that need to be preserved."
  {:watched-folders {:file "watched_folders"
                     :description "User's watched folder configurations"
                     :critical true}
   :scan-history {:file "scan_history"
                  :description "History of file scans and analyses"
                  :critical false}
   :decision-journal {:file "decision_journal"
                      :description "User's decision journal entries"
                      :critical true}
   :settings {:file "settings"
              :description "Application settings and preferences"
              :critical true}
   :model-state {:file "model_state"
                 :description "Mental model analysis state"
                 :critical false}
   :job-queue {:file "job_queue"
               :description "Pending and in-progress jobs"
               :critical true}
   :cache {:file "cache"
           :description "Application cache data"
           :critical false}
   :user-preferences {:file "user_preferences"
                      :description "User UI preferences"
                      :critical true}
   :api-tokens {:file "api_tokens"
                :description "Stored API tokens (encrypted)"
                :critical true
                :sensitive true}
   :sync-state {:file "sync_state"
                :description "Synchronization state with web app"
                :critical false}})

;; =============================================================================
;; Directory Management
;; =============================================================================

(defn ensure-directories!
  "Ensure all required directories exist."
  []
  (doseq [dir [(get-state-dir) (get-backup-dir)]]
    (let [f (io/file dir)]
      (when-not (.exists f)
        (.mkdirs f)))))

;; =============================================================================
;; Checksum Utilities
;; =============================================================================

(defn calculate-checksum
  "Calculate SHA-256 checksum of data."
  [data]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (.getBytes (pr-str data) "UTF-8")]
    (apply str (map #(format "%02x" %) (.digest digest bytes)))))

(defn verify-checksum
  "Verify data against stored checksum."
  [data expected-checksum]
  (= (calculate-checksum data) expected-checksum))

;; =============================================================================
;; Compression
;; =============================================================================

(defn compress-data
  "Compress data using GZIP."
  [data]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [gzos (GZIPOutputStream. baos)]
      (.write gzos (.getBytes (pr-str data) "UTF-8")))
    (.toByteArray baos)))

(defn decompress-data
  "Decompress GZIP data."
  [bytes]
  (with-open [bais (java.io.ByteArrayInputStream. bytes)
              gzis (GZIPInputStream. bais)
              reader (java.io.InputStreamReader. gzis "UTF-8")]
    (edn/read (java.io.PushbackReader. reader))))

;; =============================================================================
;; Atomic File Operations
;; =============================================================================

(defn atomic-write!
  "Write data to a file atomically using a temp file and rename."
  [file data]
  (let [temp-file (io/file (str (.getAbsolutePath file) ".tmp"))
        parent (.getParentFile file)]
    (when parent
      (.mkdirs parent))
    (try
      ;; Write to temp file
      (spit temp-file (pr-str data))
      ;; Atomic rename
      (Files/move (.toPath temp-file)
                  (.toPath file)
                  (into-array [StandardCopyOption/REPLACE_EXISTING
                               StandardCopyOption/ATOMIC_MOVE]))
      true
      (catch Exception e
        (log/error "Atomic write failed:" (.getMessage e))
        ;; Clean up temp file if it exists
        (when (.exists temp-file)
          (.delete temp-file))
        false))))

(defn atomic-write-compressed!
  "Write compressed data to a file atomically."
  [file data]
  (let [temp-file (io/file (str (.getAbsolutePath file) ".tmp"))
        parent (.getParentFile file)]
    (when parent
      (.mkdirs parent))
    (try
      ;; Write compressed data to temp file
      (with-open [fos (FileOutputStream. temp-file)
                  gzos (GZIPOutputStream. fos)]
        (.write gzos (.getBytes (pr-str data) "UTF-8")))
      ;; Atomic rename
      (Files/move (.toPath temp-file)
                  (.toPath file)
                  (into-array [StandardCopyOption/REPLACE_EXISTING
                               StandardCopyOption/ATOMIC_MOVE]))
      true
      (catch Exception e
        (log/error "Atomic compressed write failed:" (.getMessage e))
        (when (.exists temp-file)
          (.delete temp-file))
        false))))

;; =============================================================================
;; State Save/Load
;; =============================================================================

(defn save-state!
  "Save a state to disk with optional compression and checksum."
  [state-key data & {:keys [compress] :or {compress (:compression-enabled @config)}}]
  (ensure-directories!)
  (let [state-info (get state-files state-key)
        file-name (:file state-info)
        file (io/file (if compress
                        (get-compressed-state-file file-name)
                        (get-state-file file-name)))
        checksum (calculate-checksum data)
        wrapped-data {:version 1
                      :state-key state-key
                      :timestamp (str (Instant/now))
                      :checksum checksum
                      :data data}]
    (log/debug "Saving state:" state-key "to" (.getName file))
    (let [success (if compress
                    (atomic-write-compressed! file wrapped-data)
                    (atomic-write! file wrapped-data))]
      (when success
        (swap! persistence-state assoc :last-save (str (Instant/now)))
        (swap! persistence-state update :save-count inc))
      {:success success :file (.getAbsolutePath file)})))

(defn load-state
  "Load a state from disk with verification."
  [state-key & {:keys [verify] :or {verify (:verify-on-load @config)}}]
  (let [state-info (get state-files state-key)
        file-name (:file state-info)
        compressed-file (io/file (get-compressed-state-file file-name))
        plain-file (io/file (get-state-file file-name))
        file (cond
               (.exists compressed-file) compressed-file
               (.exists plain-file) plain-file
               :else nil)]
    (if-not file
      (do
        (log/debug "State file not found:" state-key)
        {:success false :error :not-found})
      (try
        (log/debug "Loading state:" state-key "from" (.getName file))
        (let [wrapped-data (if (str/ends-with? (.getName file) ".gz")
                             (with-open [fis (FileInputStream. file)
                                         gzis (GZIPInputStream. fis)
                                         reader (java.io.InputStreamReader. gzis "UTF-8")]
                               (edn/read (java.io.PushbackReader. reader)))
                             (edn/read-string (slurp file)))
              data (:data wrapped-data)
              stored-checksum (:checksum wrapped-data)]
          
          ;; Verify checksum if requested
          (when (and verify stored-checksum)
            (when-not (verify-checksum data stored-checksum)
              (log/error "Checksum verification failed for:" state-key)
              (swap! persistence-state assoc :corruption-detected true)
              (throw (ex-info "Checksum verification failed" {:state-key state-key}))))
          
          (swap! persistence-state assoc :last-load (str (Instant/now)))
          (swap! persistence-state update :load-count inc)
          {:success true :data data :timestamp (:timestamp wrapped-data)})
        (catch Exception e
          (log/error "Failed to load state:" state-key (.getMessage e))
          {:success false :error :load-failed :message (.getMessage e)})))))

;; =============================================================================
;; Backup Management
;; =============================================================================

(defn create-backup!
  "Create a backup of a state file."
  [state-key]
  (let [state-info (get state-files state-key)
        file-name (:file state-info)
        source-file (let [compressed (io/file (get-compressed-state-file file-name))
                          plain (io/file (get-state-file file-name))]
                      (cond
                        (.exists compressed) compressed
                        (.exists plain) plain
                        :else nil))]
    (when source-file
      (let [backup-name (str file-name "-" (System/currentTimeMillis) 
                             (if (str/ends-with? (.getName source-file) ".gz") ".edn.gz" ".edn"))
            backup-file (io/file (get-backup-dir) backup-name)]
        (try
          (io/copy source-file backup-file)
          (log/debug "Created backup:" backup-name)
          {:success true :file (.getAbsolutePath backup-file)}
          (catch Exception e
            (log/error "Failed to create backup:" (.getMessage e))
            {:success false :error (.getMessage e)}))))))

(defn list-backups
  "List all backups for a state."
  [state-key]
  (let [state-info (get state-files state-key)
        file-name (:file state-info)
        backup-dir (io/file (get-backup-dir))]
    (when (.exists backup-dir)
      (->> (.listFiles backup-dir)
           (filter #(str/starts-with? (.getName %) file-name))
           (sort-by #(.lastModified %) >)
           (map (fn [f]
                  {:name (.getName f)
                   :path (.getAbsolutePath f)
                   :size (.length f)
                   :modified (.lastModified f)}))))))

(defn restore-from-backup!
  "Restore a state from a backup file."
  [backup-path]
  (let [backup-file (io/file backup-path)]
    (when (.exists backup-file)
      (try
        (let [data (if (str/ends-with? (.getName backup-file) ".gz")
                     (with-open [fis (FileInputStream. backup-file)
                                 gzis (GZIPInputStream. fis)
                                 reader (java.io.InputStreamReader. gzis "UTF-8")]
                       (edn/read (java.io.PushbackReader. reader)))
                     (edn/read-string (slurp backup-file)))
              state-key (:state-key data)]
          (save-state! state-key (:data data))
          (log/info "Restored state from backup:" backup-path)
          {:success true :state-key state-key})
        (catch Exception e
          (log/error "Failed to restore from backup:" (.getMessage e))
          {:success false :error (.getMessage e)})))))

(defn cleanup-old-backups!
  "Remove old backups, keeping only the most recent ones."
  [state-key]
  (let [backups (list-backups state-key)
        max-backups (:max-backups @config)]
    (when (> (count backups) max-backups)
      (doseq [old-backup (drop max-backups backups)]
        (try
          (.delete (io/file (:path old-backup)))
          (log/debug "Removed old backup:" (:name old-backup))
          (catch Exception e
            (log/warn "Failed to remove old backup:" (.getMessage e))))))))

;; =============================================================================
;; Bulk Operations
;; =============================================================================

(defn save-all-states!
  "Save all registered states."
  [states-map]
  (ensure-directories!)
  (let [results (atom {})]
    (doseq [[state-key data] states-map]
      (when (contains? state-files state-key)
        (let [result (save-state! state-key data)]
          (swap! results assoc state-key result))))
    @results))

(defn load-all-states
  "Load all registered states."
  []
  (let [results (atom {})]
    (doseq [[state-key _] state-files]
      (let [result (load-state state-key)]
        (when (:success result)
          (swap! results assoc state-key (:data result)))))
    @results))

(defn backup-all-states!
  "Create backups of all states."
  []
  (let [results (atom {})]
    (doseq [[state-key _] state-files]
      (let [result (create-backup! state-key)]
        (swap! results assoc state-key result)))
    @results))

;; =============================================================================
;; Migration Support
;; =============================================================================

(defn migrate-state
  "Migrate state data from one version to another."
  [state-key data from-version to-version]
  (log/info "Migrating" state-key "from version" from-version "to" to-version)
  ;; Add migration logic here as needed
  ;; For now, just return the data unchanged
  data)

(defn check-and-migrate!
  "Check state version and migrate if necessary."
  [state-key current-version]
  (let [result (load-state state-key :verify false)]
    (when (:success result)
      (let [stored-version (get-in result [:data :_version] 1)]
        (when (< stored-version current-version)
          (let [migrated-data (migrate-state state-key (:data result) 
                                             stored-version current-version)]
            (save-state! state-key (assoc migrated-data :_version current-version))))))))

;; =============================================================================
;; Database Integrity
;; =============================================================================

(defn verify-database-integrity
  "Verify the integrity of the SQLite database if present."
  []
  (let [db-file (io/file (str (:base-dir @config) "/database.sqlite"))]
    (if (.exists db-file)
      (try
        ;; Basic file integrity check
        (let [size (.length db-file)
              readable (.canRead db-file)]
          (if (and (pos? size) readable)
            {:success true :size size}
            {:success false :error "Database file is empty or unreadable"}))
        (catch Exception e
          {:success false :error (.getMessage e)}))
      {:success true :note "No database file found"})))

;; =============================================================================
;; Pre-Update State Export
;; =============================================================================

(defn export-state-for-update!
  "Export all critical state before an update."
  []
  (log/info "Exporting state for update...")
  (ensure-directories!)
  
  ;; Backup all critical states
  (let [critical-states (filter (fn [[_ info]] (:critical info)) state-files)
        export-dir (str (:base-dir @config) "/update_export")
        export-file (io/file export-dir "state_export.edn")]
    
    ;; Create export directory
    (.mkdirs (io/file export-dir))
    
    ;; Collect all critical state data
    (let [export-data (reduce
                        (fn [acc [state-key _]]
                          (let [result (load-state state-key)]
                            (if (:success result)
                              (assoc acc state-key (:data result))
                              acc)))
                        {}
                        critical-states)]
      
      ;; Write export file
      (let [wrapped {:version 1
                     :exported-at (str (Instant/now))
                     :states export-data}]
        (atomic-write! export-file wrapped)
        (log/info "State exported to:" (.getAbsolutePath export-file))
        {:success true :file (.getAbsolutePath export-file)}))))

(defn import-state-after-update!
  "Import state after an update."
  []
  (log/info "Importing state after update...")
  (let [export-file (io/file (str (:base-dir @config) "/update_export/state_export.edn"))]
    (if (.exists export-file)
      (try
        (let [wrapped (edn/read-string (slurp export-file))
              states (:states wrapped)]
          (doseq [[state-key data] states]
            (save-state! state-key data))
          (log/info "State imported successfully")
          ;; Clean up export file
          (.delete export-file)
          {:success true :imported (keys states)})
        (catch Exception e
          (log/error "Failed to import state:" (.getMessage e))
          {:success false :error (.getMessage e)}))
      {:success false :error "No export file found"})))

;; =============================================================================
;; Auto-Save
;; =============================================================================

(defonce auto-save-thread (atom nil))

(defn start-auto-save!
  "Start automatic state saving."
  [get-states-fn]
  (when-not (:auto-save-running @persistence-state)
    (swap! persistence-state assoc :auto-save-running true)
    (reset! auto-save-thread
            (Thread.
              (fn []
                (log/info "Auto-save started")
                (try
                  (while (:auto-save-running @persistence-state)
                    (Thread/sleep (:auto-save-interval-ms @config))
                    (when (:auto-save-running @persistence-state)
                      (try
                        (let [states (get-states-fn)]
                          (save-all-states! states))
                        (catch Exception e
                          (log/warn "Auto-save failed:" (.getMessage e))))))
                  (catch InterruptedException _
                    (log/info "Auto-save interrupted"))))))
    (.start @auto-save-thread)))

(defn stop-auto-save!
  "Stop automatic state saving."
  []
  (swap! persistence-state assoc :auto-save-running false)
  (when @auto-save-thread
    (.interrupt @auto-save-thread)
    (reset! auto-save-thread nil))
  (log/info "Auto-save stopped"))

;; =============================================================================
;; Status
;; =============================================================================

(defn get-persistence-status
  "Get current persistence status."
  []
  (let [state @persistence-state]
    {:last-save (:last-save state)
     :last-load (:last-load state)
     :save-count (:save-count state)
     :load-count (:load-count state)
     :corruption-detected (:corruption-detected state)
     :auto-save-running (:auto-save-running state)
     :state-files (count state-files)
     :database-integrity (verify-database-integrity)}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-persistence!
  "Initialize the persistence module."
  []
  (log/info "Initializing state persistence...")
  (ensure-directories!)
  {:success true})
