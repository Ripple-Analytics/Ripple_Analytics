(ns mental-models.services.scanner
  "Desktop Scanner - Local file system scanning and analysis
   Processes documents, code, and media for mental model insights"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [mental-models.services.llm :as llm])
  (:import [java.io File]
           [java.nio.file Files Paths]
           [java.security MessageDigest]))

;; -- File Type Detection -----------------------------------------------------

(def supported-extensions
  {:text #{".txt" ".md" ".markdown" ".rst" ".org"}
   :code #{".clj" ".cljs" ".cljc" ".py" ".js" ".ts" ".java" ".go" ".rs" ".rb" ".ex" ".erl"}
   :document #{".pdf" ".doc" ".docx" ".odt" ".rtf"}
   :data #{".json" ".yaml" ".yml" ".xml" ".csv" ".edn"}
   :image #{".jpg" ".jpeg" ".png" ".gif" ".svg" ".webp"}
   :audio #{".mp3" ".wav" ".ogg" ".m4a" ".flac"}
   :video #{".mp4" ".mkv" ".avi" ".mov" ".webm"}})

(defn get-file-type [filename]
  (let [ext (str/lower-case (or (re-find #"\.[^.]+$" filename) ""))]
    (some (fn [[type exts]] (when (contains? exts ext) type))
          supported-extensions)))

;; -- File Hashing ------------------------------------------------------------

(defn file-hash [^File file]
  (try
    (let [digest (MessageDigest/getInstance "SHA-256")
          bytes (Files/readAllBytes (.toPath file))]
      (.update digest bytes)
      (apply str (map #(format "%02x" %) (.digest digest))))
    (catch Exception e
      (log/warn "Failed to hash file:" (.getMessage e))
      nil)))

;; -- File Reading ------------------------------------------------------------

(defn read-text-file [^File file max-chars]
  (try
    (let [content (slurp file)]
      (if (> (count content) max-chars)
        (subs content 0 max-chars)
        content))
    (catch Exception e
      (log/warn "Failed to read file:" (.getMessage e))
      nil)))

(defn read-code-file [^File file]
  (try
    (let [content (slurp file)
          lines (str/split-lines content)]
      {:content content
       :lines (count lines)
       :chars (count content)
       :language (get-file-type (.getName file))})
    (catch Exception e
      (log/warn "Failed to read code file:" (.getMessage e))
      nil)))

;; -- File Analysis -----------------------------------------------------------

(defn analyze-text-content
  "Analyze text content for mental model relevance"
  [content filename]
  (when (and content (> (count content) 50))
    (let [analysis (llm/analyze-text-for-models content)]
      (merge analysis
             {:filename filename
              :analyzed-at (System/currentTimeMillis)
              :content-length (count content)}))))

(defn analyze-code-content
  "Analyze code for patterns and mental models"
  [code-info filename]
  (when code-info
    (let [;; Extract key patterns from code
          content (:content code-info)
          patterns {:has-recursion (boolean (re-find #"defn\s+\w+.*\n.*\(\1\s" content))
                    :has-higher-order (boolean (re-find #"(map|filter|reduce|comp|partial)" content))
                    :has-state (boolean (re-find #"(atom|ref|agent|volatile)" content))
                    :has-concurrency (boolean (re-find #"(future|promise|async|go-loop)" content))
                    :complexity-estimate (min 10 (/ (:lines code-info) 50))}]
      {:filename filename
       :type :code
       :language (:language code-info)
       :lines (:lines code-info)
       :patterns patterns
       :mental-models (cond-> []
                        (:has-recursion patterns) (conj "Recursion/Self-Reference")
                        (:has-higher-order patterns) (conj "Abstraction/Composition")
                        (:has-state patterns) (conj "State Management")
                        (:has-concurrency patterns) (conj "Parallel Processing"))
       :analyzed-at (System/currentTimeMillis)})))

;; -- Directory Scanning ------------------------------------------------------

(defn scan-directory
  "Recursively scan a directory for files"
  [dir-path & {:keys [max-depth max-files] :or {max-depth 5 max-files 1000}}]
  (let [dir (io/file dir-path)
        files (atom [])
        count (atom 0)]
    (when (.isDirectory dir)
      (letfn [(scan [^File f depth]
                (when (and (< @count max-files) (< depth max-depth))
                  (if (.isDirectory f)
                    (doseq [child (.listFiles f)]
                      (scan child (inc depth)))
                    (when-let [type (get-file-type (.getName f))]
                      (swap! files conj {:path (.getAbsolutePath f)
                                         :name (.getName f)
                                         :type type
                                         :size (.length f)
                                         :modified (.lastModified f)})
                      (swap! count inc)))))]
        (scan dir 0)))
    @files))

;; -- Batch Processing --------------------------------------------------------

(defn process-file
  "Process a single file based on its type"
  [{:keys [path name type size]}]
  (let [file (io/file path)]
    (case type
      :text (analyze-text-content (read-text-file file 10000) name)
      :code (analyze-code-content (read-code-file file) name)
      :data {:filename name :type :data :size size :analyzed-at (System/currentTimeMillis)}
      ;; Other types just get metadata
      {:filename name :type type :size size :analyzed-at (System/currentTimeMillis)})))

(defn process-batch
  "Process a batch of files with progress tracking"
  [files & {:keys [on-progress] :or {on-progress (fn [_])}}]
  (let [total (count files)
        results (atom [])]
    (doseq [[idx file] (map-indexed vector files)]
      (try
        (let [result (process-file file)]
          (swap! results conj result)
          (on-progress {:current (inc idx)
                        :total total
                        :file (:name file)
                        :status :success}))
        (catch Exception e
          (log/warn "Failed to process file:" (:name file) (.getMessage e))
          (on-progress {:current (inc idx)
                        :total total
                        :file (:name file)
                        :status :error
                        :error (.getMessage e)}))))
    @results))

;; -- Queue Management --------------------------------------------------------

(def processing-queue (atom []))
(def processing-status (atom {:running false :current nil :progress 0}))

(defn enqueue-files [files]
  (swap! processing-queue concat files)
  {:queued (count files)
   :total-in-queue (count @processing-queue)})

(defn start-processing []
  (when-not (:running @processing-status)
    (swap! processing-status assoc :running true)
    (future
      (try
        (while (seq @processing-queue)
          (let [file (first @processing-queue)]
            (swap! processing-queue rest)
            (swap! processing-status assoc :current (:name file))
            (process-file file)
            (swap! processing-status update :progress inc)))
        (finally
          (swap! processing-status assoc :running false :current nil))))))

(defn get-processing-status []
  @processing-status)

;; -- Summary Generation ------------------------------------------------------

(defn generate-scan-summary
  "Generate a summary of scanned files"
  [results]
  (let [by-type (group-by :type results)
        models-found (distinct (mapcat :mental-models results))]
    {:total-files (count results)
     :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :total-models-found (count models-found)
     :models models-found
     :generated-at (System/currentTimeMillis)}))
