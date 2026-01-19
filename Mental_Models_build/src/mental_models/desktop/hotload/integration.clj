(ns mental-models.desktop.hotload.integration
  "Hotload Integration Module for Mental Models Desktop
   
   This module integrates all hotload components and provides
   a simple API for the main application to use.
   
   Based on Munger's principles:
   - Checklist Approach: Systematic verification at each step
   - Redundancy: Multiple fallback mechanisms
   - Inversion: What could go wrong? Handle it.
   
   Usage:
   1. Call (init-hotload-system! app-state-atom config) at startup
   2. Call (check-and-apply-updates!) periodically or on user request
   3. Call (shutdown-hotload-system!) on app exit"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.io File]
           [java.net URL HttpURLConnection]
           [javax.swing SwingUtilities JOptionPane Timer]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *hotload-integration-config*
  {:enabled true
   :check-interval-ms 60000           ;; Check every 60 seconds
   :manifest-url nil                  ;; Set by app
   :download-base-url nil             ;; Set by app
   :github-raw-url "https://raw.githubusercontent.com/Ripple-Analytics/Ripple_Analytics/master"
   :gdrive-manifest-url nil           ;; Google Drive manifest URL
   :auto-update true                  ;; Auto-apply non-breaking updates
   :notify-user true                  ;; Show notification for updates
   :require-confirmation-for-major true  ;; Require user confirmation for major updates
   :staging-dir (str (System/getProperty "java.io.tmpdir") "/mm-hotload")
   :backup-dir (str (System/getProperty "user.home") "/.mental-models/backups")
   :max-retries 3
   :retry-delay-ms 5000})

;; =============================================================================
;; State
;; =============================================================================

(def *integration-state
  (atom {:initialized false
         :app-state-atom nil
         :current-version nil
         :last-check nil
         :last-update nil
         :pending-update nil
         :update-in-progress false
         :error nil
         :scheduler nil
         :ui-refresh-callback nil
         :status-callback nil}))

;; =============================================================================
;; Logging
;; =============================================================================

(defn log! [message]
  (let [timestamp (java.time.LocalDateTime/now)
        formatted (.format timestamp (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss.SSS"))
        entry (str "[" formatted "] [HOTLOAD-INT] " message)]
    (println entry)
    ;; Also log to app state if available
    (when-let [app-state (:app-state-atom @*integration-state)]
      (swap! app-state update :logs #(take 1000 (conj (or % []) entry))))))

;; =============================================================================
;; Manifest Fetching
;; =============================================================================

(defn fetch-manifest [url]
  "Fetch manifest from URL with error handling"
  (try
    (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 10000)
                 (.setRequestProperty "User-Agent" "MentalModelsDesktop/2.0"))
          response-code (.getResponseCode conn)]
      (if (= response-code 200)
        (let [content (slurp (.getInputStream conn))]
          (.disconnect conn)
          {:success true :manifest (edn/read-string content)})
        (do
          (.disconnect conn)
          {:success false :error (str "HTTP " response-code)})))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn fetch-manifest-with-fallback []
  "Try multiple sources to fetch manifest"
  (let [sources [(str (:github-raw-url *hotload-integration-config*) "/Mental_Models_build/manifest.edn")
                 (:gdrive-manifest-url *hotload-integration-config*)
                 (:manifest-url *hotload-integration-config*)]
        valid-sources (filter some? sources)]
    (loop [remaining valid-sources]
      (if (empty? remaining)
        {:success false :error "All manifest sources failed"}
        (let [result (fetch-manifest (first remaining))]
          (if (:success result)
            result
            (do
              (log! (str "Manifest fetch failed from " (first remaining) ": " (:error result)))
              (recur (rest remaining)))))))))

;; =============================================================================
;; Version Comparison
;; =============================================================================

(defn parse-version [version-str]
  "Parse version string like 'v2.1.0' into comparable parts"
  (when version-str
    (let [cleaned (str/replace version-str #"^v" "")
          parts (str/split cleaned #"\.")]
      (mapv #(try (Integer/parseInt %) (catch Exception _ 0)) parts))))

(defn version-compare [v1 v2]
  "Compare two version strings. Returns -1, 0, or 1"
  (let [p1 (parse-version v1)
        p2 (parse-version v2)]
    (compare p1 p2)))

(defn is-major-update? [old-version new-version]
  "Check if this is a major version update"
  (let [old-parts (parse-version old-version)
        new-parts (parse-version new-version)]
    (and (seq old-parts) (seq new-parts)
         (> (first new-parts) (first old-parts)))))

(defn is-minor-update? [old-version new-version]
  "Check if this is a minor version update"
  (let [old-parts (parse-version old-version)
        new-parts (parse-version new-version)]
    (and (seq old-parts) (seq new-parts)
         (= (first new-parts) (first old-parts))
         (> (second new-parts) (second old-parts)))))

;; =============================================================================
;; State Preservation (Simplified inline implementation)
;; =============================================================================

(def preservable-keys
  [:settings :scan-results :decisions :mental-models-cache 
   :stats :connections :ui-state :running-jobs])

(defn capture-state []
  "Capture current app state for preservation"
  (when-let [app-state (:app-state-atom @*integration-state)]
    (select-keys @app-state preservable-keys)))

(defn restore-state! [saved-state]
  "Restore saved state to app"
  (when-let [app-state (:app-state-atom @*integration-state)]
    (when saved-state
      (swap! app-state merge saved-state)
      (log! "State restored successfully"))))

(defn save-state-to-file! [state filename]
  "Save state to file for crash recovery"
  (let [backup-dir (io/file (:backup-dir *hotload-integration-config*))]
    (.mkdirs backup-dir)
    (spit (io/file backup-dir filename) (pr-str state))))

(defn load-state-from-file [filename]
  "Load state from file"
  (let [file (io/file (:backup-dir *hotload-integration-config*) filename)]
    (when (.exists file)
      (try
        (edn/read-string (slurp file))
        (catch Exception _ nil)))))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn download-file! [url dest-file progress-callback]
  "Download a file with progress reporting"
  (try
    (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000))
          total-size (.getContentLengthLong conn)
          buffer (byte-array 65536)]
      (.mkdirs (.getParentFile dest-file))
      (with-open [in (.getInputStream conn)
                  out (java.io.FileOutputStream. dest-file)]
        (loop [downloaded 0]
          (let [n (.read in buffer)]
            (when (pos? n)
              (.write out buffer 0 n)
              (let [new-downloaded (+ downloaded n)]
                (when (and progress-callback (pos? total-size))
                  (progress-callback (/ new-downloaded total-size)))
                (recur new-downloaded))))))
      (.disconnect conn)
      {:success true :size (.length dest-file)})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn copy-file! [src dest]
  "Copy a file"
  (let [src-file (io/file src)
        dest-file (io/file dest)]
    (.mkdirs (.getParentFile dest-file))
    (java.nio.file.Files/copy 
     (.toPath src-file) 
     (.toPath dest-file)
     (into-array [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))))

;; =============================================================================
;; Delta Update Logic
;; =============================================================================

(defn calculate-files-to-update [local-files remote-files]
  "Calculate which files need to be updated"
  (let [local-by-path (into {} (map (juxt :path identity) local-files))
        remote-by-path (into {} (map (juxt :path identity) remote-files))]
    {:new (filter #(not (contains? local-by-path (:path %))) remote-files)
     :updated (filter (fn [rf]
                        (when-let [lf (get local-by-path (:path rf))]
                          (not= (:hash lf) (:hash rf))))
                      remote-files)
     :unchanged (filter (fn [rf]
                          (when-let [lf (get local-by-path (:path rf))]
                            (= (:hash lf) (:hash rf))))
                        remote-files)}))

(defn perform-delta-download! [files-to-download base-url staging-dir progress-callback]
  "Download only changed files"
  (let [total (count files-to-download)
        staging (io/file staging-dir)]
    (.mkdirs staging)
    (doseq [[idx file-info] (map-indexed vector files-to-download)]
      (let [dest (io/file staging (:path file-info))
            url (str base-url "/" (:path file-info))]
        (download-file! url dest nil)
        (when progress-callback
          (progress-callback (/ (inc idx) total) (str "Downloaded: " (:path file-info))))))
    {:success true :count total}))

;; =============================================================================
;; Namespace Reloading
;; =============================================================================

(defn reload-app-namespaces! []
  "Reload application namespaces for hot code update"
  (let [namespaces ['mental-models.desktop.gui.swing-app]]
    (doseq [ns-sym namespaces]
      (try
        (require ns-sym :reload)
        (log! (str "Reloaded: " ns-sym))
        (catch Exception e
          (log! (str "Failed to reload " ns-sym ": " (.getMessage e))))))))

;; =============================================================================
;; Main Update Flow
;; =============================================================================

(defn check-for-updates []
  "Check if updates are available"
  (log! "Checking for updates...")
  (swap! *integration-state assoc :last-check (System/currentTimeMillis))
  
  (let [result (fetch-manifest-with-fallback)]
    (if (:success result)
      (let [remote-version (get-in result [:manifest :version])
            current-version (:current-version @*integration-state)]
        (log! (str "Remote version: " remote-version ", Current: " current-version))
        (if (pos? (version-compare remote-version current-version))
          {:update-available true
           :current-version current-version
           :new-version remote-version
           :is-major (is-major-update? current-version remote-version)
           :manifest (:manifest result)}
          {:update-available false}))
      {:update-available false :error (:error result)})))

(defn apply-hot-update! 
  "Apply a hot update without restart
   
   Parameters:
   - manifest: The update manifest
   - progress-callback: (fn [percent message])
   
   Returns:
   - {:success true/false :message string}"
  [manifest progress-callback]
  
  (if (:update-in-progress @*integration-state)
    {:success false :message "Update already in progress"}
    (do
      (swap! *integration-state assoc :update-in-progress true)
      (log! "=== STARTING HOT UPDATE ===")
      
      (try
        ;; Step 1: Save current state
        (when progress-callback (progress-callback 0.1 "Saving state..."))
        (let [saved-state (capture-state)]
          (save-state-to-file! saved-state "pre-update-state.edn")
          (log! "State saved for recovery")
          
          ;; Step 2: Download delta files
          (when progress-callback (progress-callback 0.2 "Downloading updates..."))
          (let [staging-dir (:staging-dir *hotload-integration-config*)
                base-url (:base-url manifest)
                files (:files manifest)]
            
            ;; For now, download all files (full update)
            (perform-delta-download! files base-url staging-dir
                                    (fn [pct msg]
                                      (when progress-callback
                                        (progress-callback (+ 0.2 (* 0.5 pct)) msg))))
            
            ;; Step 3: Apply updates
            (when progress-callback (progress-callback 0.75 "Installing updates..."))
            (let [app-dir (System/getProperty "user.dir")]
              ;; Copy files from staging to app directory
              (doseq [f (file-seq (io/file staging-dir))]
                (when (.isFile f)
                  (let [rel-path (.relativize (.toPath (io/file staging-dir)) (.toPath f))
                        dest (io/file app-dir (str rel-path))]
                    (copy-file! f dest))))
              
              ;; Step 4: Reload namespaces
              (when progress-callback (progress-callback 0.85 "Reloading code..."))
              (reload-app-namespaces!)
              
              ;; Step 5: Restore state
              (when progress-callback (progress-callback 0.95 "Restoring state..."))
              (restore-state! saved-state)
              
              ;; Step 6: Update version
              (swap! *integration-state assoc 
                     :current-version (:version manifest)
                     :last-update (System/currentTimeMillis)
                     :update-in-progress false)
              
              ;; Step 7: Refresh UI if callback provided
              (when-let [ui-callback (:ui-refresh-callback @*integration-state)]
                (SwingUtilities/invokeLater ui-callback))
              
              (when progress-callback (progress-callback 1.0 "Update complete!"))
              (log! "=== HOT UPDATE COMPLETE ===")
              
              {:success true 
               :message (str "Updated to " (:version manifest))
               :version (:version manifest)})))
        (catch Exception e
          (log! (str "Hot update failed: " (.getMessage e)))
          (swap! *integration-state assoc :update-in-progress false :error (.getMessage e))
          
          ;; Attempt state recovery
          (when-let [saved-state (load-state-from-file "pre-update-state.edn")]
            (restore-state! saved-state)
            (log! "State recovered from backup"))
          
          {:success false :message "Update failed" :error (.getMessage e)})))))

;; =============================================================================
;; Background Update Checker
;; =============================================================================

(defn start-background-checker! []
  "Start background update checking"
  (when-let [old-scheduler (:scheduler @*integration-state)]
    (.shutdown old-scheduler))
  
  (let [scheduler (Executors/newSingleThreadScheduledExecutor)]
    (swap! *integration-state assoc :scheduler scheduler)
    
    (.scheduleAtFixedRate scheduler
      (fn []
        (try
          (let [result (check-for-updates)]
            (when (:update-available result)
              (log! (str "Update available: " (:new-version result)))
              
              ;; Notify user or auto-update based on config
              (if (and (:auto-update *hotload-integration-config*)
                       (not (:is-major result)))
                (do
                  (log! "Auto-applying minor update...")
                  (apply-hot-update! (:manifest result) nil))
                (when-let [status-callback (:status-callback @*integration-state)]
                  (status-callback {:type :update-available
                                   :version (:new-version result)
                                   :is-major (:is-major result)})))))
          (catch Exception e
            (log! (str "Background check error: " (.getMessage e))))))
      0
      (:check-interval-ms *hotload-integration-config*)
      TimeUnit/MILLISECONDS)
    
    (log! "Background update checker started")))

(defn stop-background-checker! []
  "Stop background update checking"
  (when-let [scheduler (:scheduler @*integration-state)]
    (.shutdown scheduler)
    (swap! *integration-state assoc :scheduler nil)
    (log! "Background update checker stopped")))

;; =============================================================================
;; Public API
;; =============================================================================

(defn init-hotload-system! 
  "Initialize the hotload system
   
   Parameters:
   - app-state-atom: The main application state atom
   - config: Configuration map with:
     - :current-version - Current app version
     - :manifest-url - URL to fetch manifest
     - :ui-refresh-callback - (optional) Called after UI update
     - :status-callback - (optional) Called with status updates"
  [app-state-atom config]
  
  (alter-var-root #'*hotload-integration-config* merge config)
  
  (swap! *integration-state assoc
         :initialized true
         :app-state-atom app-state-atom
         :current-version (:current-version config)
         :ui-refresh-callback (:ui-refresh-callback config)
         :status-callback (:status-callback config))
  
  ;; Start background checker if enabled
  (when (:enabled *hotload-integration-config*)
    (start-background-checker!))
  
  (log! (str "Hotload system initialized. Version: " (:current-version config)))
  {:success true})

(defn shutdown-hotload-system! []
  "Shutdown the hotload system gracefully"
  (stop-background-checker!)
  
  ;; Save final state
  (when-let [state (capture-state)]
    (save-state-to-file! state "shutdown-state.edn"))
  
  (swap! *integration-state assoc :initialized false)
  (log! "Hotload system shutdown complete"))

(defn check-and-apply-updates! 
  "Manually check and apply updates
   
   Parameters:
   - progress-callback: (optional) (fn [percent message])
   - force: (optional) Force update even if not needed
   
   Returns:
   - {:success true/false :message string}"
  [& {:keys [progress-callback force]}]
  
  (let [check-result (check-for-updates)]
    (if (or (:update-available check-result) force)
      (if (and (:is-major check-result)
               (:require-confirmation-for-major *hotload-integration-config*))
        {:success false 
         :message "Major update requires confirmation"
         :version (:new-version check-result)
         :is-major true}
        (apply-hot-update! (:manifest check-result) progress-callback))
      {:success true :message "Already up to date"})))

(defn get-hotload-status []
  "Get current hotload system status"
  (let [state @*integration-state]
    {:initialized (:initialized state)
     :current-version (:current-version state)
     :last-check (:last-check state)
     :last-update (:last-update state)
     :update-in-progress (:update-in-progress state)
     :pending-update (:pending-update state)
     :error (:error state)}))

(defn force-rollback! []
  "Force rollback to last known good state"
  (log! "Forcing rollback...")
  (when-let [saved-state (or (load-state-from-file "pre-update-state.edn")
                             (load-state-from-file "shutdown-state.edn"))]
    (restore-state! saved-state)
    (log! "Rollback complete")
    {:success true :message "Rolled back to previous state"})
  {:success false :message "No backup state available"})
