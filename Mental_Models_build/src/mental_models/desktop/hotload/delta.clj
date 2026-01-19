(ns mental-models.desktop.hotload.delta
  "Delta Update System for Mental Models Desktop
   
   Implements efficient delta updates that only download changed files.
   Based on Munger's principle of 'avoiding stupidity' - don't download
   what you already have.
   
   Features:
   - Content-addressable storage using SHA-256 hashes
   - Manifest-based change detection
   - Incremental downloads with resume support
   - Bandwidth optimization through compression
   - Integrity verification before installation"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.io File FileInputStream FileOutputStream BufferedInputStream]
           [java.security MessageDigest]
           [java.nio.file Files Paths StandardCopyOption]
           [java.util.zip GZIPInputStream GZIPOutputStream]
           [java.net URL HttpURLConnection]))

;; =============================================================================
;; Manifest Schema
;; =============================================================================

;; A manifest looks like:
;; {:version "v2.1.0"
;;  :timestamp 1705632000000
;;  :base-url "https://example.com/releases/v2.1.0"
;;  :files [{:path "lib/app.jar"
;;           :hash "abc123..."
;;           :size 1234567
;;           :compressed-size 456789
;;           :compressed-hash "def456..."}
;;          ...]
;;  :dependencies {:java-version "17+"
;;                 :min-memory "256m"}
;;  :changelog ["Fixed bug X" "Added feature Y"]}

(defn hash-bytes [^bytes data]
  "Calculate SHA-256 hash of byte array"
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest data)
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn hash-file [^File file]
  "Calculate SHA-256 hash of a file"
  (when (.exists file)
    (hash-bytes (Files/readAllBytes (.toPath file)))))

(defn create-manifest [version base-dir]
  "Create a manifest for a release directory"
  (let [base-path (.toPath (io/file base-dir))
        files (->> (file-seq (io/file base-dir))
                   (filter #(.isFile %))
                   (filter #(not (str/includes? (.getName %) ".git")))
                   (map (fn [f]
                          (let [rel-path (str (.relativize base-path (.toPath f)))
                                content (Files/readAllBytes (.toPath f))
                                hash (hash-bytes content)]
                            {:path rel-path
                             :hash hash
                             :size (count content)
                             :modified (.lastModified f)})))
                   (into []))]
    {:version version
     :timestamp (System/currentTimeMillis)
     :file-count (count files)
     :total-size (reduce + (map :size files))
     :files files}))

;; =============================================================================
;; Delta Calculation
;; =============================================================================

(defn calculate-delta [local-manifest remote-manifest]
  "Calculate the delta between local and remote manifests
   Returns a map with files to add, update, and remove"
  (let [local-files (into {} (map (juxt :path identity) (:files local-manifest)))
        remote-files (into {} (map (juxt :path identity) (:files remote-manifest)))
        
        ;; Files in remote but not local = new files
        new-files (->> (:files remote-manifest)
                       (filter #(not (contains? local-files (:path %))))
                       (into []))
        
        ;; Files in local but not remote = deleted files
        deleted-files (->> (:files local-manifest)
                           (filter #(not (contains? remote-files (:path %))))
                           (map :path)
                           (into []))
        
        ;; Files in both but with different hashes = updated files
        updated-files (->> (:files remote-manifest)
                           (filter (fn [rf]
                                     (when-let [lf (get local-files (:path rf))]
                                       (not= (:hash lf) (:hash rf)))))
                           (into []))
        
        ;; Files unchanged
        unchanged-files (->> (:files remote-manifest)
                             (filter (fn [rf]
                                       (when-let [lf (get local-files (:path rf))]
                                         (= (:hash lf) (:hash rf)))))
                             (into []))]
    
    {:from-version (:version local-manifest)
     :to-version (:version remote-manifest)
     :new-files new-files
     :updated-files updated-files
     :deleted-files deleted-files
     :unchanged-files unchanged-files
     :download-count (+ (count new-files) (count updated-files))
     :download-size (reduce + (map :size (concat new-files updated-files)))
     :delete-count (count deleted-files)
     :unchanged-count (count unchanged-files)}))

;; =============================================================================
;; Download with Resume Support
;; =============================================================================

(def ^:private download-buffer-size 65536)  ;; 64KB buffer

(defn download-file-with-resume! 
  "Download a file with resume support if partially downloaded"
  [url dest-file progress-callback]
  (let [dest (io/file dest-file)
        temp-file (io/file (str dest-file ".part"))
        existing-size (if (.exists temp-file) (.length temp-file) 0)]
    
    (.mkdirs (.getParentFile dest))
    
    (let [conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000)
                 (.setRequestProperty "User-Agent" "MentalModelsDesktop/2.0"))
          
          ;; Request resume if we have partial file
          _ (when (pos? existing-size)
              (.setRequestProperty conn "Range" (str "bytes=" existing-size "-")))
          
          response-code (.getResponseCode conn)
          total-size (+ existing-size (.getContentLengthLong conn))
          append? (= response-code 206)]  ;; 206 = Partial Content
      
      (when (or (= response-code 200) (= response-code 206))
        (with-open [in (BufferedInputStream. (.getInputStream conn))
                    out (FileOutputStream. temp-file append?)]
          (let [buffer (byte-array download-buffer-size)]
            (loop [downloaded existing-size]
              (let [n (.read in buffer)]
                (when (pos? n)
                  (.write out buffer 0 n)
                  (let [new-downloaded (+ downloaded n)]
                    (when progress-callback
                      (progress-callback (/ new-downloaded total-size)))
                    (recur new-downloaded)))))))
        
        ;; Rename temp to final
        (Files/move (.toPath temp-file) (.toPath dest)
                   (into-array [StandardCopyOption/REPLACE_EXISTING]))
        
        (.disconnect conn)
        {:success true :size total-size :resumed? append?}))))

;; =============================================================================
;; Delta Download Manager
;; =============================================================================

(defn download-delta! 
  "Download only the files that changed (delta update)"
  [delta base-url staging-dir progress-callback]
  (let [files-to-download (concat (:new-files delta) (:updated-files delta))
        total-count (count files-to-download)
        total-size (:download-size delta)
        staging (io/file staging-dir)]
    
    (.mkdirs staging)
    
    (println (str "[DELTA] Downloading " total-count " files (" 
                  (format "%.2f MB" (/ total-size 1048576.0)) ")"))
    
    (let [downloaded-size (atom 0)
          results (doall
                   (for [[idx file-info] (map-indexed vector files-to-download)]
                     (let [rel-path (:path file-info)
                           dest-file (io/file staging rel-path)
                           url (str base-url "/" rel-path)]
                       (try
                         (download-file-with-resume! 
                          url dest-file
                          (fn [file-progress]
                            (let [file-downloaded (* file-progress (:size file-info))
                                  total-downloaded (+ @downloaded-size file-downloaded)
                                  overall-progress (/ total-downloaded total-size)]
                              (when progress-callback
                                (progress-callback overall-progress 
                                                  (str "Downloading: " rel-path))))))
                         (swap! downloaded-size + (:size file-info))
                         {:path rel-path :success true}
                         (catch Exception e
                           {:path rel-path :success false :error (.getMessage e)})))))]
      
      (let [successful (filter :success results)
            failed (filter (complement :success) results)]
        {:success (empty? failed)
         :downloaded (count successful)
         :failed failed
         :total-size @downloaded-size}))))

;; =============================================================================
;; Integrity Verification
;; =============================================================================

(defn verify-downloaded-files! [delta staging-dir]
  "Verify integrity of downloaded files by checking hashes"
  (let [files-to-verify (concat (:new-files delta) (:updated-files delta))
        results (for [file-info files-to-verify]
                  (let [file (io/file staging-dir (:path file-info))
                        actual-hash (when (.exists file) (hash-file file))
                        expected-hash (:hash file-info)]
                    {:path (:path file-info)
                     :valid (= actual-hash expected-hash)
                     :expected expected-hash
                     :actual actual-hash}))]
    
    (let [valid (filter :valid results)
          invalid (filter (complement :valid) results)]
      {:success (empty? invalid)
       :verified (count valid)
       :invalid invalid})))

;; =============================================================================
;; Apply Delta
;; =============================================================================

(defn apply-delta! 
  "Apply a delta update to the target directory"
  [delta staging-dir target-dir backup-dir]
  (let [target (io/file target-dir)
        backup (io/file backup-dir (str "backup-" (System/currentTimeMillis)))]
    
    ;; Create backup directory
    (.mkdirs backup)
    
    ;; Step 1: Backup files that will be updated or deleted
    (println "[DELTA] Backing up existing files...")
    (doseq [file-info (concat (:updated-files delta))]
      (let [src (io/file target (:path file-info))
            dest (io/file backup (:path file-info))]
        (when (.exists src)
          (.mkdirs (.getParentFile dest))
          (Files/copy (.toPath src) (.toPath dest)
                     (into-array [StandardCopyOption/REPLACE_EXISTING])))))
    
    ;; Step 2: Copy new and updated files from staging
    (println "[DELTA] Installing new files...")
    (doseq [file-info (concat (:new-files delta) (:updated-files delta))]
      (let [src (io/file staging-dir (:path file-info))
            dest (io/file target (:path file-info))]
        (when (.exists src)
          (.mkdirs (.getParentFile dest))
          (Files/copy (.toPath src) (.toPath dest)
                     (into-array [StandardCopyOption/REPLACE_EXISTING])))))
    
    ;; Step 3: Delete removed files (optional, controlled by flag)
    ;; We keep deleted files for safety - they can be cleaned up later
    
    {:success true
     :backup-dir (.getAbsolutePath backup)
     :files-installed (+ (count (:new-files delta)) (count (:updated-files delta)))}))

;; =============================================================================
;; Rollback from Backup
;; =============================================================================

(defn rollback-from-backup! [backup-dir target-dir]
  "Rollback to a previous backup"
  (let [backup (io/file backup-dir)
        target (io/file target-dir)]
    (when (.exists backup)
      (doseq [f (file-seq backup)]
        (when (.isFile f)
          (let [rel-path (.relativize (.toPath backup) (.toPath f))
                dest (io/file target (str rel-path))]
            (.mkdirs (.getParentFile dest))
            (Files/copy (.toPath f) (.toPath dest)
                       (into-array [StandardCopyOption/REPLACE_EXISTING])))))
      {:success true :restored-from (.getAbsolutePath backup)})))

;; =============================================================================
;; Full Delta Update Flow
;; =============================================================================

(defn perform-delta-update! 
  "Perform a complete delta update
   
   Parameters:
   - manifest-url: URL to fetch the remote manifest
   - local-dir: Current application directory
   - progress-callback: (fn [percent message]) for progress updates
   
   Returns:
   - {:success true/false :message string :details map}"
  [manifest-url local-dir progress-callback]
  
  (println "=== STARTING DELTA UPDATE ===")
  
  (try
    ;; Step 1: Fetch remote manifest
    (when progress-callback (progress-callback 0.05 "Fetching remote manifest..."))
    (let [remote-manifest (-> (slurp manifest-url) edn/read-string)]
      (println (str "[DELTA] Remote version: " (:version remote-manifest)))
      
      ;; Step 2: Build local manifest
      (when progress-callback (progress-callback 0.10 "Analyzing local files..."))
      (let [local-manifest (create-manifest "local" local-dir)]
        (println (str "[DELTA] Local files: " (:file-count local-manifest)))
        
        ;; Step 3: Calculate delta
        (when progress-callback (progress-callback 0.15 "Calculating delta..."))
        (let [delta (calculate-delta local-manifest remote-manifest)]
          (println (str "[DELTA] Files to download: " (:download-count delta)))
          (println (str "[DELTA] Download size: " (format "%.2f MB" (/ (:download-size delta) 1048576.0))))
          
          (if (zero? (:download-count delta))
            (do
              (when progress-callback (progress-callback 1.0 "Already up to date!"))
              {:success true :message "Already up to date" :delta delta})
            
            (let [staging-dir (str (System/getProperty "java.io.tmpdir") "/mm-delta-staging")
                  backup-dir (str (System/getProperty "user.home") "/.mental-models/backups")]
              
              ;; Step 4: Download delta files
              (when progress-callback (progress-callback 0.20 "Downloading updates..."))
              (let [download-result (download-delta! 
                                     delta 
                                     (:base-url remote-manifest)
                                     staging-dir
                                     (fn [pct msg]
                                       (when progress-callback
                                         (progress-callback (+ 0.20 (* 0.50 pct)) msg))))]
                
                (if-not (:success download-result)
                  {:success false :message "Download failed" :details download-result}
                  
                  (do
                    ;; Step 5: Verify integrity
                    (when progress-callback (progress-callback 0.75 "Verifying integrity..."))
                    (let [verify-result (verify-downloaded-files! delta staging-dir)]
                      
                      (if-not (:success verify-result)
                        {:success false :message "Integrity check failed" :details verify-result}
                        
                        (do
                          ;; Step 6: Apply delta
                          (when progress-callback (progress-callback 0.85 "Installing updates..."))
                          (let [apply-result (apply-delta! delta staging-dir local-dir backup-dir)]
                            
                            (when progress-callback (progress-callback 1.0 "Update complete!"))
                            (println "=== DELTA UPDATE COMPLETE ===")
                            {:success true
                             :message (str "Updated to " (:to-version delta))
                             :delta delta
                             :download download-result
                             :apply apply-result}))))))))))))
    (catch Exception e
      (println (str "[DELTA] Error: " (.getMessage e)))
      {:success false :message "Update failed" :error (.getMessage e)})))

;; =============================================================================
;; Manifest Server Helpers (for generating manifests)
;; =============================================================================

(defn save-manifest! [manifest file-path]
  "Save a manifest to a file"
  (spit file-path (pr-str manifest))
  (println (str "Manifest saved to: " file-path)))

(defn generate-release-manifest! [version release-dir output-file]
  "Generate a manifest for a release and save it"
  (let [manifest (create-manifest version release-dir)]
    (save-manifest! manifest output-file)
    manifest))
