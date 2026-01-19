(ns mental-models.desktop.updater.delta-updater
  "Delta Updates - Download only changed files
   
   Features:
   - Delta manifest parsing
   - Changed files detection
   - Single/multiple file updates
   - Mid-way failure recovery
   - File permission preservation
   - Corruption detection
   - Partial update rollback
   - Bandwidth optimization"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.time Instant]
           [java.nio.file Files Paths StandardCopyOption]
           [java.nio.file.attribute PosixFilePermission PosixFilePermissions]
           [java.security MessageDigest]
           [java.util.concurrent.atomic AtomicInteger]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:base-dir (str (System/getProperty "user.home") "/.mental-models")
         :delta-manifest-url nil
         :staging-dir "delta_staging"
         :backup-dir "delta_backup"
         :max-concurrent-downloads 4
         :chunk-size 65536
         :verify-checksums true
         :preserve-permissions true
         :max-retries 3}))

(defonce delta-state
  (atom {:current-version nil
         :target-version nil
         :files-to-update []
         :files-updated []
         :files-failed []
         :in-progress false
         :progress 0
         :last-update nil}))

;; =============================================================================
;; Checksum Utilities
;; =============================================================================

(defn calculate-sha256
  "Calculate SHA-256 checksum of a file."
  [file]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 8192)]
    (with-open [fis (FileInputStream. file)]
      (loop []
        (let [n (.read fis buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn verify-file-checksum
  "Verify a file's checksum."
  [file expected-checksum]
  (when (and file expected-checksum (.exists file))
    (let [actual (calculate-sha256 file)]
      (= (str/lower-case expected-checksum) (str/lower-case actual)))))

;; =============================================================================
;; Delta Manifest
;; =============================================================================

(defrecord DeltaFile [path checksum size url permissions modified])

(defn parse-delta-manifest
  "Parse a delta manifest from JSON or EDN."
  [manifest-data]
  (try
    (let [data (if (string? manifest-data)
                 (try
                   (json/parse-string manifest-data true)
                   (catch Exception _
                     (edn/read-string manifest-data)))
                 manifest-data)]
      {:success true
       :from-version (:fromVersion data)
       :to-version (:toVersion data)
       :files (map (fn [f]
                     (->DeltaFile
                       (:path f)
                       (:checksum f)
                       (:size f)
                       (:url f)
                       (:permissions f)
                       (:modified f)))
                   (:files data))
       :deleted-files (:deletedFiles data [])
       :total-size (:totalSize data)
       :created-at (:createdAt data)})
    (catch Exception e
      (log/error "Failed to parse delta manifest:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn fetch-delta-manifest
  "Fetch the delta manifest from the server."
  [from-version to-version]
  (when-let [base-url (:delta-manifest-url @config)]
    (let [url (str base-url "?from=" from-version "&to=" to-version)]
      (log/info "Fetching delta manifest:" url)
      (try
        (let [response (http/get url {:as :json
                                      :socket-timeout 30000
                                      :connection-timeout 10000})]
          (if (= 200 (:status response))
            (parse-delta-manifest (:body response))
            {:success false :error (str "HTTP " (:status response))}))
        (catch Exception e
          (log/error "Failed to fetch delta manifest:" (.getMessage e))
          {:success false :error (.getMessage e)})))))

;; =============================================================================
;; File Comparison
;; =============================================================================

(defn get-local-file-info
  "Get information about a local file."
  [base-dir path]
  (let [file (io/file base-dir path)]
    (when (.exists file)
      {:path path
       :checksum (calculate-sha256 file)
       :size (.length file)
       :modified (.lastModified file)})))

(defn compare-files
  "Compare local files with delta manifest to determine what needs updating."
  [base-dir manifest]
  (let [files-to-update (atom [])
        files-unchanged (atom [])
        files-new (atom [])]
    (doseq [delta-file (:files manifest)]
      (let [local-info (get-local-file-info base-dir (:path delta-file))]
        (cond
          (nil? local-info)
          (swap! files-new conj delta-file)
          
          (not= (:checksum local-info) (:checksum delta-file))
          (swap! files-to-update conj delta-file)
          
          :else
          (swap! files-unchanged conj delta-file))))
    {:to-update @files-to-update
     :new @files-new
     :unchanged @files-unchanged
     :to-delete (:deleted-files manifest)}))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn get-staging-dir []
  (str (:base-dir @config) "/" (:staging-dir @config)))

(defn get-backup-dir []
  (str (:base-dir @config) "/" (:backup-dir @config)))

(defn ensure-directories!
  "Ensure staging and backup directories exist."
  []
  (doseq [dir [(get-staging-dir) (get-backup-dir)]]
    (let [f (io/file dir)]
      (when-not (.exists f)
        (.mkdirs f)))))

(defn backup-file!
  "Backup a file before updating."
  [base-dir path]
  (let [source (io/file base-dir path)
        backup (io/file (get-backup-dir) path)]
    (when (.exists source)
      (try
        (io/make-parents backup)
        (io/copy source backup)
        (log/debug "Backed up:" path)
        {:success true :backup (.getAbsolutePath backup)}
        (catch Exception e
          (log/error "Failed to backup:" path (.getMessage e))
          {:success false :error (.getMessage e)})))))

(defn restore-file!
  "Restore a file from backup."
  [base-dir path]
  (let [backup (io/file (get-backup-dir) path)
        target (io/file base-dir path)]
    (when (.exists backup)
      (try
        (io/make-parents target)
        (io/copy backup target)
        (log/info "Restored from backup:" path)
        {:success true}
        (catch Exception e
          (log/error "Failed to restore:" path (.getMessage e))
          {:success false :error (.getMessage e)})))))

(defn download-file!
  "Download a single file from the delta update."
  [delta-file staging-dir & {:keys [progress-callback]}]
  (let [target-file (io/file staging-dir (:path delta-file))
        url (:url delta-file)]
    (log/debug "Downloading:" (:path delta-file))
    (try
      (io/make-parents target-file)
      (with-open [in (io/input-stream url)
                  out (io/output-stream target-file)]
        (let [buffer (byte-array (:chunk-size @config))
              downloaded (atom 0)]
          (loop []
            (let [n (.read in buffer)]
              (when (pos? n)
                (.write out buffer 0 n)
                (swap! downloaded + n)
                (when progress-callback
                  (progress-callback {:file (:path delta-file)
                                      :downloaded @downloaded
                                      :total (:size delta-file)}))
                (recur))))))
      
      ;; Verify checksum
      (when (:verify-checksums @config)
        (when-not (verify-file-checksum target-file (:checksum delta-file))
          (throw (ex-info "Checksum verification failed" {:file (:path delta-file)}))))
      
      {:success true :file target-file}
      (catch Exception e
        (log/error "Failed to download:" (:path delta-file) (.getMessage e))
        {:success false :error (.getMessage e) :file (:path delta-file)}))))

(defn apply-file!
  "Apply a downloaded file to the target directory."
  [staging-dir target-dir delta-file]
  (let [staged (io/file staging-dir (:path delta-file))
        target (io/file target-dir (:path delta-file))]
    (try
      (io/make-parents target)
      
      ;; Atomic move
      (Files/move (.toPath staged)
                  (.toPath target)
                  (into-array [StandardCopyOption/REPLACE_EXISTING
                               StandardCopyOption/ATOMIC_MOVE]))
      
      ;; Preserve permissions if specified
      (when (and (:preserve-permissions @config) (:permissions delta-file))
        (try
          (let [perms (PosixFilePermissions/fromString (:permissions delta-file))]
            (Files/setPosixFilePermissions (.toPath target) perms))
          (catch UnsupportedOperationException _
            ;; Windows doesn't support POSIX permissions
            nil)))
      
      (log/debug "Applied:" (:path delta-file))
      {:success true}
      (catch Exception e
        (log/error "Failed to apply:" (:path delta-file) (.getMessage e))
        {:success false :error (.getMessage e)}))))

(defn delete-file!
  "Delete a file that was removed in the update."
  [target-dir path]
  (let [file (io/file target-dir path)]
    (when (.exists file)
      (try
        (.delete file)
        (log/debug "Deleted:" path)
        {:success true}
        (catch Exception e
          (log/error "Failed to delete:" path (.getMessage e))
          {:success false :error (.getMessage e)})))))

;; =============================================================================
;; Delta Update Process
;; =============================================================================

(defn apply-delta-update!
  "Apply a delta update to the target directory.
   
   Options:
   - :progress-callback - Called with progress updates
   - :on-file-complete - Called when each file is complete"
  [manifest target-dir & {:keys [progress-callback on-file-complete]}]
  (log/info "========================================")
  (log/info "Starting delta update")
  (log/info "From:" (:from-version manifest) "To:" (:to-version manifest))
  (log/info "Files to update:" (count (:files manifest)))
  (log/info "========================================")
  
  (ensure-directories!)
  (swap! delta-state assoc
         :in-progress true
         :current-version (:from-version manifest)
         :target-version (:to-version manifest)
         :files-to-update (map :path (:files manifest))
         :files-updated []
         :files-failed []
         :progress 0)
  
  (let [staging-dir (get-staging-dir)
        files (:files manifest)
        total-files (count files)
        completed (AtomicInteger. 0)
        failed-files (atom [])
        success (atom true)]
    
    (try
      ;; Step 1: Backup existing files
      (log/info "Step 1: Backing up existing files...")
      (doseq [delta-file files]
        (backup-file! target-dir (:path delta-file)))
      
      ;; Step 2: Download all files to staging
      (log/info "Step 2: Downloading files to staging...")
      (doseq [delta-file files]
        (let [result (download-file! delta-file staging-dir
                                     :progress-callback progress-callback)]
          (if (:success result)
            (do
              (.incrementAndGet completed)
              (swap! delta-state update :files-updated conj (:path delta-file))
              (swap! delta-state assoc :progress 
                     (int (* 50 (/ (.get completed) total-files))))
              (when on-file-complete
                (on-file-complete {:file (:path delta-file) :status :downloaded})))
            (do
              (reset! success false)
              (swap! failed-files conj delta-file)
              (swap! delta-state update :files-failed conj (:path delta-file))))))
      
      ;; Check if all downloads succeeded
      (when-not @success
        (throw (ex-info "Some downloads failed" {:failed @failed-files})))
      
      ;; Step 3: Apply files atomically
      (log/info "Step 3: Applying files...")
      (doseq [delta-file files]
        (let [result (apply-file! staging-dir target-dir delta-file)]
          (if (:success result)
            (do
              (swap! delta-state assoc :progress 
                     (int (+ 50 (* 40 (/ (.indexOf files delta-file) total-files)))))
              (when on-file-complete
                (on-file-complete {:file (:path delta-file) :status :applied})))
            (do
              (reset! success false)
              (throw (ex-info "Failed to apply file" {:file (:path delta-file)}))))))
      
      ;; Step 4: Delete removed files
      (log/info "Step 4: Deleting removed files...")
      (doseq [path (:deleted-files manifest)]
        (delete-file! target-dir path))
      
      ;; Step 5: Cleanup staging
      (log/info "Step 5: Cleaning up...")
      (doseq [f (reverse (file-seq (io/file staging-dir)))]
        (.delete f))
      
      (swap! delta-state assoc 
             :in-progress false
             :progress 100
             :last-update (str (Instant/now)))
      
      (log/info "========================================")
      (log/info "Delta update complete!")
      (log/info "========================================")
      
      {:success true
       :updated-files (count files)
       :deleted-files (count (:deleted-files manifest))}
      
      (catch Exception e
        (log/error "Delta update failed:" (.getMessage e))
        (log/info "Rolling back changes...")
        
        ;; Rollback: restore backed up files
        (doseq [delta-file files]
          (restore-file! target-dir (:path delta-file)))
        
        (swap! delta-state assoc :in-progress false)
        
        {:success false
         :error (.getMessage e)
         :failed-files @failed-files}))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn check-for-delta-update
  "Check if a delta update is available."
  [current-version]
  (when-let [manifest-url (:delta-manifest-url @config)]
    (try
      (let [response (http/get (str manifest-url "/latest")
                               {:as :json
                                :socket-timeout 10000
                                :connection-timeout 5000})]
        (when (= 200 (:status response))
          (let [latest (:latestVersion (:body response))]
            (when (not= current-version latest)
              {:available true
               :current-version current-version
               :latest-version latest}))))
      (catch Exception e
        (log/warn "Failed to check for delta update:" (.getMessage e))
        nil))))

(defn perform-delta-update!
  "Perform a delta update from current to latest version."
  [current-version target-dir & opts]
  (log/info "Checking for delta update from version:" current-version)
  
  (if-let [check (check-for-delta-update current-version)]
    (if (:available check)
      (let [manifest (fetch-delta-manifest current-version (:latest-version check))]
        (if (:success manifest)
          (apply apply-delta-update! manifest target-dir opts)
          {:success false :error "Failed to fetch delta manifest"}))
      {:success true :message "Already up to date"})
    {:success false :error "Could not check for updates"}))

;; =============================================================================
;; Status
;; =============================================================================

(defn get-delta-status
  "Get current delta update status."
  []
  (let [state @delta-state]
    {:in-progress (:in-progress state)
     :current-version (:current-version state)
     :target-version (:target-version state)
     :progress (:progress state)
     :files-to-update (count (:files-to-update state))
     :files-updated (count (:files-updated state))
     :files-failed (count (:files-failed state))
     :last-update (:last-update state)}))

(defn configure!
  "Update delta updater configuration."
  [config-map]
  (swap! config merge config-map)
  (log/info "Delta updater configured:" (keys config-map)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-delta-updater!
  "Initialize the delta updater."
  []
  (log/info "Initializing delta updater...")
  (ensure-directories!)
  {:success true})
