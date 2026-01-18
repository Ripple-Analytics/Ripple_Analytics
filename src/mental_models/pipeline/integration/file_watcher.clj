(ns mental-models.pipeline.integration.file-watcher
  "File Watcher Integration for Pipeline
   
   Connects the pipeline to file system monitoring:
   - Watch directories for new files
   - Automatic file extraction and analysis
   - Progress tracking and notifications
   - Integration with batch processing"
  (:require
   [mental-models.scanner.auto-scanner :as scanner]
   [mental-models.pipeline.integration.lm-studio :as lm]
   [mental-models.pipeline.integration.database :as db]
   [mental-models.pipeline.integration.notifications :as notif]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.java.io :as io]
   [clojure.core.async :as async :refer [go go-loop chan <! >! close!]])
  (:import
   [java.nio.file FileSystems Paths StandardWatchEventKinds WatchService]
   [java.util.concurrent Executors TimeUnit]))

;; =============================================================================
;; WATCHER STATE
;; =============================================================================

(defonce watcher-state (atom {:running false
                              :watch-service nil
                              :watched-dirs #{}
                              :executor nil
                              :processed-files #{}}))

;; =============================================================================
;; FILE PROCESSING
;; =============================================================================

(defn supported-file? [file]
  (let [name (.getName file)
        extensions #{".txt" ".md" ".pdf" ".docx" ".html" ".json" ".csv" ".rtf"}]
    (some #(.endsWith (.toLowerCase name) %) extensions)))

(defn process-file!
  "Process a single file through the pipeline."
  [file]
  (when (and (supported-file? file)
             (not (contains? (:processed-files @watcher-state) (.getAbsolutePath file))))
    (log/info "Processing file" {:path (.getAbsolutePath file)})
    (try
      (let [start-time (System/currentTimeMillis)
            ;; Extract text from file
            text (slurp file)
            ;; Analyze with LM Studio
            detections (lm/detect-models text)
            ;; Check for Lollapalooza
            lollapalooza (lm/detect-lollapalooza text)
            ;; Create result
            result {:file-path (.getAbsolutePath file)
                    :file-name (.getName file)
                    :text-length (count text)
                    :detections detections
                    :lollapalooza lollapalooza
                    :timestamp (System/currentTimeMillis)
                    :duration-ms (- (System/currentTimeMillis) start-time)}]
        ;; Save to database
        (db/save-analysis-result! result)
        ;; Mark as processed
        (swap! watcher-state update :processed-files conj (.getAbsolutePath file))
        ;; Record metrics
        (metrics/inc-counter! :file-watcher/files-processed)
        (metrics/observe-histogram! :file-watcher/processing-time (:duration-ms result))
        ;; Publish event
        (events/publish! :file-watcher/file-processed result)
        ;; Audit
        (audit/log-operation! {:operation :file-processed
                               :file-path (.getAbsolutePath file)
                               :detections-count (count detections)
                               :has-lollapalooza (boolean lollapalooza)})
        result)
      (catch Exception e
        (log/error "Failed to process file" {:path (.getAbsolutePath file) :error (.getMessage e)})
        (metrics/inc-counter! :file-watcher/processing-failures)
        nil))))

;; =============================================================================
;; DIRECTORY WATCHING
;; =============================================================================

(defn watch-directory!
  "Start watching a directory for new files."
  [dir-path]
  (when (flags/is-enabled? "file-watcher")
    (let [dir (io/file dir-path)]
      (when (.isDirectory dir)
        (log/info "Watching directory" {:path dir-path})
        (swap! watcher-state update :watched-dirs conj dir-path)
        ;; Process existing files
        (doseq [file (.listFiles dir)]
          (when (.isFile file)
            (process-file! file)))
        ;; Register with watch service
        (when-let [ws (:watch-service @watcher-state)]
          (let [path (Paths/get dir-path (into-array String []))]
            (.register path ws (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                            StandardWatchEventKinds/ENTRY_MODIFY]))))
        (metrics/inc-counter! :file-watcher/directories-watched)
        true))))

(defn unwatch-directory!
  "Stop watching a directory."
  [dir-path]
  (swap! watcher-state update :watched-dirs disj dir-path)
  (log/info "Stopped watching directory" {:path dir-path}))

;; =============================================================================
;; WATCH SERVICE
;; =============================================================================

(defn start-watch-service! []
  (when-not (:running @watcher-state)
    (log/info "Starting file watch service")
    (let [ws (.newWatchService (FileSystems/getDefault))
          executor (Executors/newSingleThreadExecutor)]
      (swap! watcher-state assoc
             :running true
             :watch-service ws
             :executor executor)
      ;; Start watch loop
      (.submit executor
               (fn []
                 (while (:running @watcher-state)
                   (try
                     (when-let [key (.poll ws 1 TimeUnit/SECONDS)]
                       (doseq [event (.pollEvents key)]
                         (let [kind (.kind event)
                               context (.context event)
                               dir-path (str (.watchable key))
                               file-path (str dir-path "/" context)
                               file (io/file file-path)]
                           (when (and (or (= kind StandardWatchEventKinds/ENTRY_CREATE)
                                          (= kind StandardWatchEventKinds/ENTRY_MODIFY))
                                      (.isFile file))
                             (log/debug "File event" {:kind kind :path file-path})
                             (process-file! file))))
                       (.reset key))
                     (catch Exception e
                       (log/warn "Watch service error" {:error (.getMessage e)}))))))
      (log/info "File watch service started"))))

(defn stop-watch-service! []
  (when (:running @watcher-state)
    (log/info "Stopping file watch service")
    (swap! watcher-state assoc :running false)
    (when-let [ws (:watch-service @watcher-state)]
      (.close ws))
    (when-let [executor (:executor @watcher-state)]
      (.shutdown executor))
    (swap! watcher-state assoc
           :watch-service nil
           :executor nil
           :watched-dirs #{})
    (log/info "File watch service stopped")))

;; =============================================================================
;; BATCH PROCESSING
;; =============================================================================

(defn process-directory!
  "Process all files in a directory."
  [dir-path & {:keys [recursive] :or {recursive false}}]
  (log/info "Processing directory" {:path dir-path :recursive recursive})
  (let [dir (io/file dir-path)
        files (if recursive
                (file-seq dir)
                (.listFiles dir))
        processable (filter #(and (.isFile %) (supported-file? %)) files)
        start-time (System/currentTimeMillis)
        results (doall (map process-file! processable))
        successful (filter some? results)
        duration (- (System/currentTimeMillis) start-time)]
    (let [batch-result {:total (count processable)
                        :successful (count successful)
                        :failed (- (count processable) (count successful))
                        :lollapaloozas (count (filter :lollapalooza successful))
                        :duration-ms duration
                        :results successful}]
      ;; Send notification
      (notif/send-batch-completion-notification! batch-result)
      ;; Publish event
      (events/publish! :file-watcher/batch-completed batch-result)
      batch-result)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-file-watcher!
  "Initialize file watcher integration."
  []
  (log/info "Initializing file watcher integration")
  ;; Register feature flag
  (flags/register-flag! "file-watcher" "Enable file watcher" true)
  ;; Create metrics
  (metrics/create-counter! :file-watcher/files-processed "Files processed")
  (metrics/create-counter! :file-watcher/processing-failures "Processing failures")
  (metrics/create-counter! :file-watcher/directories-watched "Directories watched")
  (metrics/create-histogram! :file-watcher/processing-time "File processing time" [100 500 1000 5000 10000])
  ;; Start watch service
  (start-watch-service!)
  (log/info "File watcher integration initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-file-watcher-status []
  {:running (:running @watcher-state)
   :watched-dirs (:watched-dirs @watcher-state)
   :processed-files-count (count (:processed-files @watcher-state))
   :enabled (flags/is-enabled? "file-watcher")})
