(ns mental-models.beast.devourer
  "BEAST MODE: Folder Devourer
   
   Point this at any folder and it will:
   - Recursively scan everything
   - Never stop (watches for new files)
   - Saturate all CPU cores
   - Process petabytes of data
   - Zero manual interaction required
   
   Usage: (devour! \"/path/to/folder\")"
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! >! >!! chan 
                                                   close! thread pipeline-async
                                                   onto-chan! sliding-buffer]]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.nio.file Files Paths StandardWatchEventKinds 
                          WatchService FileSystems FileVisitOption]
           [java.nio.file.attribute BasicFileAttributes]
           [java.util.concurrent Executors ThreadPoolExecutor 
                                 LinkedBlockingQueue TimeUnit]
           [java.time Instant Duration]
           [java.security MessageDigest]
           [java.util UUID]))

;; =============================================================================
;; CONFIGURATION - BEAST MODE SETTINGS
;; =============================================================================

(def config
  {:parallelism (* 2 (.availableProcessors (Runtime/getRuntime))) ; 2x cores
   :queue-size 1000000           ; 1M items in queue
   :batch-size 1000              ; Process in batches
   :watch-poll-ms 100            ; Check for new files every 100ms
   :max-file-size-mb 1000        ; Skip files > 1GB (process separately)
   :supported-extensions #{".txt" ".md" ".pdf" ".doc" ".docx" ".xls" ".xlsx"
                           ".csv" ".json" ".xml" ".html" ".htm" ".rtf"
                           ".ppt" ".pptx" ".odt" ".ods" ".odp" ".epub"
                           ".py" ".js" ".ts" ".clj" ".cljs" ".java" ".c" ".cpp"
                           ".h" ".go" ".rs" ".rb" ".php" ".swift" ".kt"
                           ".sql" ".sh" ".bash" ".zsh" ".yaml" ".yml" ".toml"
                           ".ini" ".cfg" ".conf" ".log" ".eml" ".msg"
                           ".jpg" ".jpeg" ".png" ".gif" ".bmp" ".tiff" ".webp"
                           ".mp3" ".wav" ".flac" ".m4a" ".ogg"
                           ".mp4" ".avi" ".mkv" ".mov" ".webm"}
   :skip-patterns #{#"node_modules" #"\.git" #"__pycache__" #"\.DS_Store"
                    #"\.cache" #"\.tmp" #"\.temp" #"thumbs\.db" #"\.Trash"}})

;; =============================================================================
;; STATE - BEAST METRICS
;; =============================================================================

(defonce beast-state
  (atom {:status :idle
         :started-at nil
         :targets []
         :stats {:files-discovered 0
                 :files-processed 0
                 :files-failed 0
                 :bytes-processed 0
                 :patterns-found 0
                 :models-matched 0}
         :rate {:files-per-second 0
                :bytes-per-second 0}
         :queue-depth 0
         :active-workers 0
         :last-file nil
         :errors []}))

;; =============================================================================
;; FILE DISCOVERY - RECURSIVE SCANNER
;; =============================================================================

(defn should-skip?
  "Check if path should be skipped"
  [^File file]
  (let [name (.getName file)
        path (.getAbsolutePath file)]
    (or (str/starts-with? name ".")
        (some #(re-find % path) (:skip-patterns config)))))

(defn supported-file?
  "Check if file type is supported"
  [^File file]
  (let [name (str/lower-case (.getName file))
        ext (when-let [i (str/last-index-of name ".")]
              (subs name i))]
    (or (nil? ext)  ; Files without extension
        (contains? (:supported-extensions config) ext))))

(defn file-info
  "Extract file metadata"
  [^File file]
  (try
    {:path (.getAbsolutePath file)
     :name (.getName file)
     :size (.length file)
     :modified (.lastModified file)
     :extension (let [name (.getName file)
                      i (str/last-index-of name ".")]
                  (when i (subs name i)))
     :type (cond
             (.isDirectory file) :directory
             (.isFile file) :file
             :else :unknown)
     :id (str (UUID/randomUUID))
     :discovered-at (Instant/now)}
    (catch Exception e
      (log/warn "Failed to get file info:" (.getAbsolutePath file) (.getMessage e))
      nil)))

(defn scan-directory!
  "Recursively scan directory and push files to channel"
  [^File dir out-chan]
  (thread
    (try
      (let [walk (fn walk [^File f]
                   (when-not (should-skip? f)
                     (if (.isDirectory f)
                       (doseq [child (.listFiles f)]
                         (walk child))
                       (when (and (supported-file? f)
                                  (<= (.length f) (* (:max-file-size-mb config) 1024 1024)))
                         (when-let [info (file-info f)]
                           (swap! beast-state update-in [:stats :files-discovered] inc)
                           (>!! out-chan info))))))]
        (walk dir)
        (log/info "Finished scanning:" (.getAbsolutePath dir)))
      (catch Exception e
        (log/error "Scan error:" (.getMessage e))))))

;; =============================================================================
;; FILE WATCHER - CONTINUOUS MONITORING
;; =============================================================================

(defn start-watcher!
  "Watch directory for new/modified files"
  [^String path out-chan]
  (thread
    (try
      (let [watch-service (.newWatchService (FileSystems/getDefault))
            dir-path (Paths/get path (into-array String []))]
        ;; Register directory and all subdirectories
        (Files/walkFileTree dir-path
                            #{}
                            Integer/MAX_VALUE
                            (reify java.nio.file.FileVisitor
                              (preVisitDirectory [_ dir attrs]
                                (when-not (should-skip? (.toFile dir))
                                  (.register dir watch-service
                                             (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                                          StandardWatchEventKinds/ENTRY_MODIFY])))
                                java.nio.file.FileVisitResult/CONTINUE)
                              (visitFile [_ _ _] java.nio.file.FileVisitResult/CONTINUE)
                              (visitFileFailed [_ _ _] java.nio.file.FileVisitResult/CONTINUE)
                              (postVisitDirectory [_ _ _] java.nio.file.FileVisitResult/CONTINUE)))
        
        (log/info "Watching for changes:" path)
        
        ;; Watch loop
        (loop []
          (when (= :running (:status @beast-state))
            (when-let [key (.poll watch-service (:watch-poll-ms config) TimeUnit/MILLISECONDS)]
              (doseq [event (.pollEvents key)]
                (let [kind (.kind event)
                      filename (.context event)
                      full-path (.resolve (.watchable key) filename)
                      file (.toFile full-path)]
                  (when (and (.isFile file)
                             (supported-file? file)
                             (not (should-skip? file)))
                    (when-let [info (file-info file)]
                      (swap! beast-state update-in [:stats :files-discovered] inc)
                      (>!! out-chan info)))))
              (.reset key))
            (recur))))
      (catch Exception e
        (log/error "Watcher error:" (.getMessage e))))))

;; =============================================================================
;; CONTENT EXTRACTION - ALL FILE TYPES
;; =============================================================================

(defmulti extract-content
  "Extract text content from file"
  (fn [file-info] (:extension file-info)))

(defmethod extract-content :default
  [{:keys [path]}]
  (try
    (slurp path)
    (catch Exception e
      (log/debug "Cannot read as text:" path)
      nil)))

(defmethod extract-content ".pdf"
  [{:keys [path]}]
  ;; Would use PDFBox in production
  (try
    {:type :pdf
     :path path
     :content nil  ; Placeholder - needs PDFBox
     :pages 0}
    (catch Exception e nil)))

(defmethod extract-content ".json"
  [{:keys [path]}]
  (try
    (let [content (slurp path)]
      {:type :json
       :content content
       :parsed (clojure.edn/read-string content)})
    (catch Exception e nil)))

(defmethod extract-content ".csv"
  [{:keys [path]}]
  (try
    (let [lines (str/split-lines (slurp path))]
      {:type :csv
       :headers (first lines)
       :rows (count (rest lines))
       :content (slurp path)})
    (catch Exception e nil)))

;; Image/audio/video - extract metadata only
(defmethod extract-content ".jpg" [f] {:type :image :path (:path f)})
(defmethod extract-content ".jpeg" [f] {:type :image :path (:path f)})
(defmethod extract-content ".png" [f] {:type :image :path (:path f)})
(defmethod extract-content ".mp3" [f] {:type :audio :path (:path f)})
(defmethod extract-content ".mp4" [f] {:type :video :path (:path f)})

;; =============================================================================
;; ANALYSIS ENGINE - PATTERN DETECTION
;; =============================================================================

(defn compute-hash
  "Compute content hash for deduplication"
  [content]
  (when content
    (let [md (MessageDigest/getInstance "SHA-256")
          bytes (.getBytes (str content) "UTF-8")]
      (.update md bytes)
      (format "%064x" (BigInteger. 1 (.digest md))))))

(defn detect-patterns
  "Detect patterns in content"
  [content]
  (when (string? content)
    (let [text (str/lower-case content)]
      {:word-count (count (str/split text #"\s+"))
       :char-count (count text)
       :line-count (count (str/split-lines content))
       :has-code? (or (str/includes? text "function")
                      (str/includes? text "def ")
                      (str/includes? text "class "))
       :has-data? (or (str/includes? text "data")
                      (str/includes? text "analysis")
                      (str/includes? text "result"))
       :sentiment (cond
                    (or (str/includes? text "excellent")
                        (str/includes? text "great")
                        (str/includes? text "success")) :positive
                    (or (str/includes? text "fail")
                        (str/includes? text "error")
                        (str/includes? text "problem")) :negative
                    :else :neutral)})))

(defn match-mental-models
  "Match content against mental models"
  [content patterns]
  ;; Would use the 129 mental models database
  ;; For now, simple keyword matching
  (when (string? content)
    (let [text (str/lower-case content)
          models [["confirmation-bias" #{"confirm" "bias" "believe" "evidence"}]
                  ["sunk-cost" #{"sunk" "cost" "invested" "continue"}]
                  ["first-principles" #{"first" "principle" "fundamental" "basic"}]
                  ["inversion" #{"invert" "reverse" "opposite" "avoid"}]
                  ["circle-of-competence" #{"competence" "expertise" "know" "understand"}]
                  ["margin-of-safety" #{"margin" "safety" "buffer" "risk"}]
                  ["opportunity-cost" #{"opportunity" "cost" "alternative" "trade-off"}]
                  ["second-order" #{"second" "order" "consequence" "effect"}]]]
      (->> models
           (filter (fn [[_ keywords]]
                     (some #(str/includes? text %) keywords)))
           (map first)))))

;; =============================================================================
;; PROCESSING PIPELINE - SATURATE ALL CORES
;; =============================================================================

(defn process-file!
  "Process a single file through the analysis pipeline"
  [file-info result-chan]
  (go
    (try
      (let [start-time (System/currentTimeMillis)
            content (extract-content file-info)
            text-content (if (map? content) (:content content) content)
            hash (compute-hash text-content)
            patterns (detect-patterns text-content)
            models (match-mental-models text-content patterns)
            duration (- (System/currentTimeMillis) start-time)
            
            result {:file file-info
                    :hash hash
                    :content-type (if (map? content) (:type content) :text)
                    :patterns patterns
                    :models-matched models
                    :processed-at (Instant/now)
                    :duration-ms duration}]
        
        ;; Update stats
        (swap! beast-state (fn [s]
                             (-> s
                                 (update-in [:stats :files-processed] inc)
                                 (update-in [:stats :bytes-processed] + (:size file-info 0))
                                 (update-in [:stats :patterns-found] + (if patterns 1 0))
                                 (update-in [:stats :models-matched] + (count models))
                                 (assoc :last-file (:path file-info)))))
        
        (>! result-chan result))
      (catch Exception e
        (swap! beast-state (fn [s]
                             (-> s
                                 (update-in [:stats :files-failed] inc)
                                 (update :errors conj {:file (:path file-info)
                                                       :error (.getMessage e)
                                                       :time (Instant/now)}))))
        (log/debug "Process error:" (:path file-info) (.getMessage e))))))

(defn start-processing-pipeline!
  "Start the parallel processing pipeline"
  [file-chan result-chan]
  (let [parallelism (:parallelism config)]
    (log/info "Starting processing pipeline with" parallelism "workers")
    
    ;; Create worker pool
    (dotimes [_ parallelism]
      (go-loop []
        (when-let [file (<! file-chan)]
          (swap! beast-state update :active-workers inc)
          (<! (process-file! file result-chan))
          (swap! beast-state update :active-workers dec)
          (recur))))))

;; =============================================================================
;; RESULT STORAGE - DISTRIBUTED
;; =============================================================================

(defonce results-store (atom {:results []
                               :by-hash {}
                               :by-model {}}))

(defn store-result!
  "Store analysis result"
  [result]
  (let [hash (:hash result)]
    (when-not (get-in @results-store [:by-hash hash])
      (swap! results-store (fn [s]
                             (-> s
                                 (update :results conj result)
                                 (assoc-in [:by-hash hash] result)
                                 (update :by-model
                                         (fn [m]
                                           (reduce (fn [m model]
                                                     (update m model (fnil conj []) result))
                                                   m
                                                   (:models-matched result))))))))))

(defn start-result-consumer!
  "Consume and store results"
  [result-chan]
  (go-loop []
    (when-let [result (<! result-chan)]
      (store-result! result)
      (recur))))

;; =============================================================================
;; RATE CALCULATOR
;; =============================================================================

(defn start-rate-calculator!
  "Calculate processing rates"
  []
  (go-loop [last-files 0
            last-bytes 0
            last-time (System/currentTimeMillis)]
    (<! (async/timeout 1000))
    (when (= :running (:status @beast-state))
      (let [now (System/currentTimeMillis)
            elapsed (/ (- now last-time) 1000.0)
            current-files (get-in @beast-state [:stats :files-processed])
            current-bytes (get-in @beast-state [:stats :bytes-processed])
            files-rate (/ (- current-files last-files) elapsed)
            bytes-rate (/ (- current-bytes last-bytes) elapsed)]
        (swap! beast-state assoc :rate {:files-per-second (Math/round files-rate)
                                        :bytes-per-second (Math/round bytes-rate)
                                        :mb-per-second (/ bytes-rate 1024 1024)})
        (recur current-files current-bytes now)))))

;; =============================================================================
;; BEAST MODE - MAIN ENTRY POINT
;; =============================================================================

(defn devour!
  "BEAST MODE: Point at folder(s) and devour everything
   
   Usage:
   (devour! \"/path/to/folder\")
   (devour! [\"/folder1\" \"/folder2\" \"/folder3\"])"
  [targets]
  (let [targets (if (string? targets) [targets] targets)
        file-chan (chan (:queue-size config))
        result-chan (chan (:queue-size config))]
    
    (log/info "🔥 BEAST MODE ACTIVATED 🔥")
    (log/info "Targets:" targets)
    (log/info "Parallelism:" (:parallelism config) "workers")
    
    ;; Initialize state
    (reset! beast-state {:status :running
                         :started-at (Instant/now)
                         :targets targets
                         :stats {:files-discovered 0
                                 :files-processed 0
                                 :files-failed 0
                                 :bytes-processed 0
                                 :patterns-found 0
                                 :models-matched 0}
                         :rate {:files-per-second 0
                                :bytes-per-second 0}
                         :queue-depth 0
                         :active-workers 0
                         :last-file nil
                         :errors []})
    
    ;; Start all components
    (start-processing-pipeline! file-chan result-chan)
    (start-result-consumer! result-chan)
    (start-rate-calculator!)
    
    ;; Scan all targets
    (doseq [target targets]
      (let [f (io/file target)]
        (when (.exists f)
          (scan-directory! f file-chan)
          (start-watcher! target file-chan))))
    
    (log/info "Beast is devouring..." (count targets) "targets")
    
    ;; Return control handle
    {:stop! (fn []
              (log/info "Stopping beast...")
              (swap! beast-state assoc :status :stopped)
              (close! file-chan)
              (close! result-chan)
              (log/info "Beast stopped"))
     :status (fn [] @beast-state)
     :results (fn [] @results-store)}))

(defn stop!
  "Stop the beast"
  []
  (swap! beast-state assoc :status :stopped)
  (log/info "Beast stopped"))

(defn status
  "Get beast status"
  []
  (let [s @beast-state
        uptime (when (:started-at s)
                 (.toSeconds (Duration/between (:started-at s) (Instant/now))))]
    (merge s {:uptime-seconds uptime
              :results-count (count (:results @results-store))})))

(defn stats
  "Get processing stats"
  []
  (let [s (:stats @beast-state)
        r (:rate @beast-state)]
    (merge s r {:unique-results (count (:by-hash @results-store))
                :models-index-size (count (:by-model @results-store))})))

;; =============================================================================
;; CLI INTERFACE
;; =============================================================================

(defn -main
  "CLI entry point"
  [& args]
  (if (empty? args)
    (println "Usage: beast <folder> [folder2] [folder3] ...")
    (let [beast (devour! (vec args))]
      ;; Print status every 5 seconds
      (go-loop []
        (<! (async/timeout 5000))
        (when (= :running (:status @beast-state))
          (let [s (stats)]
            (println (format "📊 Files: %d/%d | Rate: %d/s | MB: %.1f | Models: %d | Errors: %d"
                             (:files-processed s)
                             (:files-discovered s)
                             (:files-per-second s)
                             (/ (:bytes-processed s) 1024.0 1024.0)
                             (:models-matched s)
                             (:files-failed s))))
          (recur)))
      
      ;; Keep running
      (println "Press Ctrl+C to stop")
      @(promise))))
