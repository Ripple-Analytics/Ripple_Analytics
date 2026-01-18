(ns mental-models.desktop.cli
  "Command-line interface for Mental Models Desktop App.
   Point at a folder and it automatically discovers mental models."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [taoensso.timbre :as log]
            [mental-models.services.lm-studio :as lm]
            [mental-models.desktop.db :as db]
            [mental-models.desktop.extractor :as extractor]
            [mental-models.desktop.watcher :as watcher])
  (:import [java.io File]
           [java.nio.file Files Paths]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter])
  (:gen-class))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:lm-studio-url "http://localhost:1234"
         :web-app-url "https://mental-models.manus.space"
         :watched-folders []
         :auto-sync true
         :scan-interval-ms 5000
         :batch-size 10
         :max-concurrent 4}))

;; =============================================================================
;; File Discovery
;; =============================================================================

(def supported-extensions
  #{".txt" ".md" ".pdf" ".docx" ".doc" ".rtf" ".html" ".htm"})

(defn supported-file? [^File f]
  (and (.isFile f)
       (some #(str/ends-with? (str/lower-case (.getName f)) %) supported-extensions)))

(defn discover-files [folder-path]
  (log/info "Discovering files in:" folder-path)
  (let [folder (io/file folder-path)]
    (if (.exists folder)
      (->> (file-seq folder)
           (filter supported-file?)
           (map (fn [^File f]
                  {:path (.getAbsolutePath f)
                   :name (.getName f)
                   :size (.length f)
                   :modified (.lastModified f)
                   :extension (last (str/split (.getName f) #"\."))}))
           (vec))
      (do
        (log/error "Folder does not exist:" folder-path)
        []))))

;; =============================================================================
;; Text Extraction
;; =============================================================================

(defn extract-text [file-info]
  (log/debug "Extracting text from:" (:name file-info))
  (try
    (let [path (:path file-info)
          ext (str/lower-case (or (:extension file-info) ""))]
      (cond
        (contains? #{".txt" ".md"} (str "." ext))
        (slurp path)
        
        (= ext "pdf")
        (extractor/extract-pdf-text path)
        
        (contains? #{"docx" "doc"} ext)
        (extractor/extract-docx-text path)
        
        :else
        (slurp path)))
    (catch Exception e
      (log/error e "Failed to extract text from:" (:path file-info))
      nil)))

;; =============================================================================
;; LM Studio Analysis
;; =============================================================================

(defn analyze-with-lm-studio [text file-info]
  (log/info "Analyzing:" (:name file-info) "(" (count text) "chars)")
  (try
    (let [truncated-text (subs text 0 (min (count text) 8000))
          result (lm/analyze-text truncated-text)]
      (assoc result
             :file-path (:path file-info)
             :file-name (:name file-info)
             :analyzed-at (str (LocalDateTime/now))))
    (catch Exception e
      (log/error e "LM Studio analysis failed for:" (:name file-info))
      {:error (str e)
       :file-path (:path file-info)
       :file-name (:name file-info)})))

;; =============================================================================
;; Processing Pipeline
;; =============================================================================

(defn process-file [file-info]
  (log/info "Processing:" (:name file-info))
  (let [text (extract-text file-info)]
    (if (and text (> (count text) 100))
      (let [result (analyze-with-lm-studio text file-info)]
        (db/save-analysis! result)
        (when-let [models (:mental-models result)]
          (when (>= (count models) 3)
            (log/warn "🎯 LOLLAPALOOZA DETECTED in" (:name file-info) 
                      "- Models:" (str/join ", " (map :name models)))))
        result)
      (do
        (log/warn "Skipping" (:name file-info) "- insufficient text")
        {:skipped true :reason "insufficient-text" :file-path (:path file-info)}))))

(defn process-folder [folder-path]
  (log/info "=" (apply str (repeat 60 "=")))
  (log/info "Starting folder scan:" folder-path)
  (log/info "=" (apply str (repeat 60 "=")))
  
  (let [files (discover-files folder-path)
        total (count files)]
    (log/info "Found" total "files to process")
    
    (doseq [[idx file-info] (map-indexed vector files)]
      (log/info (format "[%d/%d] Processing: %s" (inc idx) total (:name file-info)))
      (try
        (let [result (process-file file-info)]
          (if (:error result)
            (log/error "Failed:" (:name file-info) "-" (:error result))
            (log/info "Completed:" (:name file-info))))
        (catch Exception e
          (log/error e "Exception processing:" (:name file-info)))))
    
    (log/info "=" (apply str (repeat 60 "=")))
    (log/info "Scan complete. Processed" total "files")
    (log/info "=" (apply str (repeat 60 "=")))))

;; =============================================================================
;; Watch Mode
;; =============================================================================

(defn start-watching [folder-path]
  (log/info "Starting watch mode for:" folder-path)
  (watcher/start-watching folder-path
                          (fn [event]
                            (when (and (= (:kind event) :create)
                                       (supported-file? (io/file (:path event))))
                              (log/info "New file detected:" (:path event))
                              (process-file {:path (:path event)
                                             :name (.getName (io/file (:path event)))})))))

;; =============================================================================
;; CLI Interface
;; =============================================================================

(defn print-banner []
  (println)
  (println "╔════════════════════════════════════════════════════════════════╗")
  (println "║         🧠 MENTAL MODELS DESKTOP ANALYZER 🧠                   ║")
  (println "║                                                                ║")
  (println "║  Autonomous mental model discovery from your documents         ║")
  (println "║  Powered by LM Studio for local AI inference                   ║")
  (println "╚════════════════════════════════════════════════════════════════╝")
  (println))

(defn print-help []
  (println "Usage: clojure -M:run <command> [options]")
  (println)
  (println "Commands:")
  (println "  scan <folder>     Scan a folder for mental models")
  (println "  watch <folder>    Watch a folder for new files")
  (println "  status            Show analysis statistics")
  (println "  config            Show current configuration")
  (println "  help              Show this help message")
  (println)
  (println "Examples:")
  (println "  clojure -M:run scan ~/Documents/research")
  (println "  clojure -M:run watch ~/Documents/inbox")
  (println)
  (println "Requirements:")
  (println "  - LM Studio running at http://localhost:1234")
  (println "  - Supported file types: .txt, .md, .pdf, .docx"))

(defn print-status []
  (let [stats (db/get-stats)]
    (println)
    (println "📊 Analysis Statistics")
    (println "─────────────────────────────────────")
    (println "  Total files analyzed:" (:total stats 0))
    (println "  Successful analyses:" (:successful stats 0))
    (println "  Failed analyses:" (:failed stats 0))
    (println "  Lollapalooza events:" (:lollapaloozas stats 0))
    (println "  Mental models found:" (:models-found stats 0))
    (println)))

(defn print-config []
  (println)
  (println "⚙️  Current Configuration")
  (println "─────────────────────────────────────")
  (doseq [[k v] @config]
    (println (format "  %s: %s" (name k) v)))
  (println))

(defn check-lm-studio []
  (print "Checking LM Studio connection... ")
  (flush)
  (if (:healthy (lm/health-check (:lm-studio-url @config)))
    (do (println "✅ Connected")
        true)
    (do (println "❌ Not available")
        (println)
        (println "Please ensure LM Studio is running at" (:lm-studio-url @config))
        (println "1. Open LM Studio")
        (println "2. Load a model (e.g., Mistral, Llama)")
        (println "3. Start the local server (Server tab)")
        false)))

(defn -main [& args]
  (print-banner)
  (db/init!)
  
  (let [[command & rest-args] args]
    (case command
      "scan"
      (if-let [folder (first rest-args)]
        (when (check-lm-studio)
          (process-folder folder))
        (do
          (println "Error: Please specify a folder to scan")
          (println "Usage: clojure -M:run scan <folder>")))
      
      "watch"
      (if-let [folder (first rest-args)]
        (when (check-lm-studio)
          (start-watching folder)
          (println "Watching for new files. Press Ctrl+C to stop.")
          ;; Keep the process running
          @(promise))
        (do
          (println "Error: Please specify a folder to watch")
          (println "Usage: clojure -M:run watch <folder>")))
      
      "status"
      (print-status)
      
      "config"
      (print-config)
      
      "help"
      (print-help)
      
      nil
      (print-help)
      
      ;; Default: treat as folder path for backwards compatibility
      (when (check-lm-studio)
        (process-folder command)))))
