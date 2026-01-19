(ns mental-models.desktop.hotload.state
  "State Preservation System for Mental Models Desktop
   
   Ensures user state is never lost during hot updates.
   Based on Munger's principle of 'margin of safety' - 
   always have backups and never lose user work.
   
   Features:
   - Automatic state snapshots before updates
   - Multiple backup generations (time-travel)
   - Selective state restoration
   - State migration between versions
   - Crash recovery from last known good state"
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files Paths StandardCopyOption]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *state-config*
  {:backup-dir (str (System/getProperty "user.home") "/.mental-models/state-backups")
   :max-backups 10                    ;; Keep last 10 state backups
   :auto-save-interval-ms 60000       ;; Auto-save every 60 seconds
   :crash-recovery-file "crash-recovery.edn"
   :state-version 1})                 ;; For state migration

;; =============================================================================
;; State Schema
;; =============================================================================

;; The state we preserve includes:
;; - User settings (theme, preferences)
;; - Scan results and analysis history
;; - Decision journal entries
;; - Connection states
;; - UI state (selected tabs, scroll positions)
;; - Running jobs/tasks

(def preservable-keys
  "Keys from the app state that should be preserved across updates"
  [:settings
   :scan-results
   :decisions
   :mental-models-cache
   :stats
   :connections
   :logs
   :ui-state
   :running-jobs
   :user-data])

;; =============================================================================
;; Serialization
;; =============================================================================

(defn serialize-state [app-state]
  "Serialize application state for storage
   Filters out non-serializable values like functions and UI components"
  (let [state (if (instance? clojure.lang.Atom app-state)
                @app-state
                app-state)]
    {:version (:state-version *state-config*)
     :timestamp (System/currentTimeMillis)
     :app-version (get-in state [:config :version] "unknown")
     :data (select-keys state preservable-keys)}))

(defn migrate-state [data from-version to-version]
  "Migrate state from one version to another
   Add migration logic here as state schema evolves"
  (cond
    ;; Example migration: v0 -> v1
    (and (= from-version 0) (>= to-version 1))
    (-> data
        (assoc :migrated-from 0)
        (update :settings #(or % {})))
    
    ;; No migration needed or unknown version
    :else data))

(defn deserialize-state [serialized]
  "Deserialize stored state back to application format"
  (when serialized
    (let [{:keys [version data]} serialized]
      ;; Handle state migration if versions differ
      (if (= version (:state-version *state-config*))
        data
        (migrate-state data version (:state-version *state-config*))))))

;; =============================================================================
;; Backup Management
;; =============================================================================

(defn ensure-backup-dir! []
  "Ensure backup directory exists"
  (let [dir (io/file (:backup-dir *state-config*))]
    (.mkdirs dir)
    dir))

(defn generate-backup-name []
  "Generate a timestamped backup filename"
  (let [now (LocalDateTime/now)
        formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss")]
    (str "state-" (.format now formatter) ".edn")))

(defn list-backups []
  "List all available state backups, newest first"
  (let [dir (ensure-backup-dir!)]
    (->> (.listFiles dir)
         (filter #(str/ends-with? (.getName %) ".edn"))
         (filter #(not= (.getName %) (:crash-recovery-file *state-config*)))
         (sort-by #(.lastModified %) >)
         (map (fn [f]
                {:file f
                 :name (.getName f)
                 :timestamp (.lastModified f)
                 :size (.length f)})))))

(defn cleanup-old-backups! []
  "Remove old backups beyond the max limit"
  (let [backups (list-backups)
        to-remove (drop (:max-backups *state-config*) backups)]
    (doseq [{:keys [file]} to-remove]
      (.delete file)
      (println (str "[STATE] Removed old backup: " (.getName file))))
    (count to-remove)))

(defn save-backup! [app-state & {:keys [name reason]}]
  "Save a state backup
   
   Parameters:
   - app-state: The application state atom or map
   - name: (optional) Custom backup name
   - reason: (optional) Reason for backup (for logging)
   
   Returns:
   - {:success true/false :file File :size long}"
  (let [dir (ensure-backup-dir!)
        backup-name (or name (generate-backup-name))
        backup-file (io/file dir backup-name)
        serialized (serialize-state app-state)]
    
    (try
      (spit backup-file (pr-str serialized))
      (println (str "[STATE] Backup saved: " backup-name 
                   (when reason (str " (reason: " reason ")"))))
      (cleanup-old-backups!)
      {:success true 
       :file backup-file 
       :size (.length backup-file)
       :timestamp (:timestamp serialized)}
      (catch Exception e
        (println (str "[STATE] Backup failed: " (.getMessage e)))
        {:success false :error (.getMessage e)}))))

(defn load-backup [backup-name]
  "Load a specific backup by name"
  (let [backup-file (io/file (:backup-dir *state-config*) backup-name)]
    (when (.exists backup-file)
      (try
        (let [serialized (edn/read-string (slurp backup-file))]
          {:success true
           :data (deserialize-state serialized)
           :metadata (select-keys serialized [:version :timestamp :app-version])})
        (catch Exception e
          {:success false :error (.getMessage e)})))))

(defn load-latest-backup []
  "Load the most recent backup"
  (when-let [latest (first (list-backups))]
    (load-backup (:name latest))))

;; =============================================================================
;; Crash Recovery
;; =============================================================================

(defn save-crash-recovery! [app-state]
  "Save state for crash recovery (called periodically)"
  (let [dir (ensure-backup-dir!)
        recovery-file (io/file dir (:crash-recovery-file *state-config*))
        serialized (serialize-state app-state)]
    (try
      ;; Write to temp file first, then atomic rename
      (let [temp-file (io/file dir ".crash-recovery.tmp")]
        (spit temp-file (pr-str serialized))
        (Files/move (.toPath temp-file) (.toPath recovery-file)
                   (into-array [StandardCopyOption/REPLACE_EXISTING
                               StandardCopyOption/ATOMIC_MOVE])))
      {:success true}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn load-crash-recovery []
  "Load crash recovery state if available"
  (let [recovery-file (io/file (:backup-dir *state-config*) 
                               (:crash-recovery-file *state-config*))]
    (when (.exists recovery-file)
      (try
        (let [serialized (edn/read-string (slurp recovery-file))]
          {:success true
           :data (deserialize-state serialized)
           :timestamp (:timestamp serialized)})
        (catch Exception e
          {:success false :error (.getMessage e)})))))

(defn clear-crash-recovery! []
  "Clear crash recovery file (called after successful startup)"
  (let [recovery-file (io/file (:backup-dir *state-config*) 
                               (:crash-recovery-file *state-config*))]
    (when (.exists recovery-file)
      (.delete recovery-file))))

;; =============================================================================
;; State Restoration
;; =============================================================================

(defn restore-state! [app-state-atom backup-data]
  "Restore state from backup data to the application state atom
   
   Parameters:
   - app-state-atom: The application state atom
   - backup-data: The deserialized backup data
   
   Returns:
   - {:success true/false :restored-keys [...]}"
  (when backup-data
    (try
      (let [keys-to-restore (keys backup-data)]
        (swap! app-state-atom merge backup-data)
        (println (str "[STATE] Restored " (count keys-to-restore) " state keys"))
        {:success true :restored-keys keys-to-restore})
      (catch Exception e
        (println (str "[STATE] Restore failed: " (.getMessage e)))
        {:success false :error (.getMessage e)}))))

(defn selective-restore! [app-state-atom backup-data keys-to-restore]
  "Restore only specific keys from backup"
  (when backup-data
    (let [selected (select-keys backup-data keys-to-restore)]
      (swap! app-state-atom merge selected)
      {:success true :restored-keys (keys selected)})))

;; =============================================================================
;; Auto-Save System
;; =============================================================================

(def ^:private auto-save-timer (atom nil))

(defn start-auto-save! [app-state-atom]
  "Start automatic state saving"
  (when @auto-save-timer
    (.cancel @auto-save-timer))
  
  (let [timer (java.util.Timer. "state-auto-save" true)]
    (.scheduleAtFixedRate timer
      (proxy [java.util.TimerTask] []
        (run []
          (try
            (save-crash-recovery! app-state-atom)
            (catch Exception e
              (println (str "[STATE] Auto-save error: " (.getMessage e)))))))
      (:auto-save-interval-ms *state-config*)
      (:auto-save-interval-ms *state-config*))
    (reset! auto-save-timer timer)
    (println "[STATE] Auto-save started")))

(defn stop-auto-save! []
  "Stop automatic state saving"
  (when @auto-save-timer
    (.cancel @auto-save-timer)
    (reset! auto-save-timer nil)
    (println "[STATE] Auto-save stopped")))

;; =============================================================================
;; Pre-Update State Capture
;; =============================================================================

(defn capture-pre-update-state! [app-state-atom update-version]
  "Capture state before an update for rollback capability
   
   Parameters:
   - app-state-atom: The application state atom
   - update-version: Version being updated to (for naming)
   
   Returns:
   - {:success true/false :backup-file File}"
  (save-backup! app-state-atom 
               :name (str "pre-update-" update-version "-" (System/currentTimeMillis) ".edn")
               :reason (str "Pre-update backup for " update-version)))

(defn rollback-to-pre-update! [app-state-atom update-version]
  "Rollback to state before a specific update"
  (let [backups (list-backups)
        pre-update-backup (first (filter #(str/includes? (:name %) 
                                                        (str "pre-update-" update-version))
                                        backups))]
    (if pre-update-backup
      (let [loaded (load-backup (:name pre-update-backup))]
        (if (:success loaded)
          (restore-state! app-state-atom (:data loaded))
          loaded))
      {:success false :error (str "No pre-update backup found for " update-version)})))

;; =============================================================================
;; State Diff (for debugging)
;; =============================================================================

(defn state-diff [old-state new-state]
  "Calculate differences between two states (for debugging)"
  (let [old-keys (set (keys old-state))
        new-keys (set (keys new-state))
        added (clojure.set/difference new-keys old-keys)
        removed (clojure.set/difference old-keys new-keys)
        common (clojure.set/intersection old-keys new-keys)
        changed (filter #(not= (get old-state %) (get new-state %)) common)]
    {:added added
     :removed removed
     :changed (set changed)
     :unchanged (clojure.set/difference common (set changed))}))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-state-system! [app-state-atom]
  "Initialize the state preservation system
   
   Parameters:
   - app-state-atom: The application state atom
   
   This will:
   1. Check for crash recovery state
   2. Start auto-save
   3. Return any recovered state"
  (ensure-backup-dir!)
  
  ;; Check for crash recovery
  (let [recovery (load-crash-recovery)]
    (when (:success recovery)
      (println "[STATE] Found crash recovery state from " 
               (java.util.Date. (:timestamp recovery)))
      ;; Don't auto-restore - let the app decide
      )
    
    ;; Start auto-save
    (start-auto-save! app-state-atom)
    
    ;; Return recovery info
    {:initialized true
     :crash-recovery-available (:success recovery)
     :crash-recovery-data (when (:success recovery) (:data recovery))
     :backup-count (count (list-backups))}))

(defn shutdown-state-system! [app-state-atom]
  "Shutdown the state system gracefully"
  (stop-auto-save!)
  (save-backup! app-state-atom :reason "shutdown")
  (clear-crash-recovery!)
  (println "[STATE] State system shutdown complete"))
