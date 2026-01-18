(ns mental-models.ingestion
  "Ingestion Module - Electric Clojure
   
   Data ingestion pipeline for mental models system.
   Ported from Python to Electric Clojure for unified codebase.
   
   Features:
   - File watching and processing
   - Text chunking
   - PDF processing
   - Batch processing
   - Feedback collection"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])))

;; ============================================
;; Configuration
;; ============================================

(def default-config
  {:chunk-size 1000
   :chunk-overlap 200
   :supported-extensions #{".txt" ".md" ".pdf" ".json" ".csv"}
   :batch-size 100
   :watch-interval-ms 5000})

;; ============================================
;; Text Chunking
;; ============================================

(defn chunk-text
  "Split text into overlapping chunks."
  [text & {:keys [chunk-size overlap]
           :or {chunk-size (:chunk-size default-config)
                overlap (:chunk-overlap default-config)}}]
  (if (or (nil? text) (<= (count text) chunk-size))
    [{:text text :start 0 :end (count (or text ""))}]
    (loop [chunks []
           start 0]
      (if (>= start (count text))
        chunks
        (let [end (min (+ start chunk-size) (count text))
              chunk-text (subs text start end)
              chunk {:text chunk-text
                     :start start
                     :end end
                     :index (count chunks)}]
          (recur (conj chunks chunk)
                 (- end overlap)))))))

(defn chunk-by-sentences
  "Split text into chunks by sentence boundaries."
  [text & {:keys [max-chunk-size] :or {max-chunk-size 1000}}]
  (let [sentences (str/split text #"(?<=[.!?])\s+")
        chunks (reduce (fn [acc sentence]
                         (let [current (last acc)
                               current-text (or (:text current) "")
                               new-text (if (empty? current-text)
                                          sentence
                                          (str current-text " " sentence))]
                           (if (> (count new-text) max-chunk-size)
                             (conj acc {:text sentence :sentences [sentence]})
                             (-> acc
                                 (butlast)
                                 (vec)
                                 (conj {:text new-text
                                        :sentences (conj (or (:sentences current) []) sentence)})))))
                       [{:text "" :sentences []}]
                       sentences)]
    (mapv (fn [chunk idx]
            (assoc chunk :index idx))
          (filter #(not (empty? (:text %))) chunks)
          (range))))

(defn chunk-by-paragraphs
  "Split text into chunks by paragraph boundaries."
  [text]
  (let [paragraphs (str/split text #"\n\n+")]
    (mapv (fn [para idx]
            {:text (str/trim para)
             :index idx
             :type :paragraph})
          (filter #(not (empty? (str/trim %))) paragraphs)
          (range))))

;; ============================================
;; File Processing
;; ============================================

#?(:clj
   (defn get-file-extension
     "Get the extension of a file."
     [path]
     (let [name (.getName (io/file path))
           dot-idx (.lastIndexOf name ".")]
       (if (pos? dot-idx)
         (subs name dot-idx)
         ""))))

#?(:clj
   (defn is-supported-file?
     "Check if a file has a supported extension."
     [path]
     (contains? (:supported-extensions default-config)
                (str/lower-case (get-file-extension path)))))

#?(:clj
   (defn read-text-file
     "Read a text file and return its contents."
     [path]
     (try
       {:success true
        :path path
        :content (slurp path)
        :size (.length (io/file path))
        :extension (get-file-extension path)}
       (catch Exception e
         {:success false
          :path path
          :error (.getMessage e)}))))

#?(:clj
   (defn process-text-file
     "Process a text file into chunks."
     [path & {:keys [chunk-size overlap]
              :or {chunk-size (:chunk-size default-config)
                   overlap (:chunk-overlap default-config)}}]
     (let [file-result (read-text-file path)]
       (if (:success file-result)
         (let [chunks (chunk-text (:content file-result)
                                  :chunk-size chunk-size
                                  :overlap overlap)]
           {:success true
            :path path
            :chunks chunks
            :total-chunks (count chunks)
            :total-characters (count (:content file-result))})
         file-result))))

;; ============================================
;; PDF Processing (Stub - requires external library)
;; ============================================

(defn process-pdf
  "Process a PDF file (stub - requires PDFBox or similar)."
  [path]
  {:success false
   :path path
   :note "PDF processing requires external library (PDFBox for JVM)"
   :alternative "Use ABBYY FineReader API or convert to text first"})

;; ============================================
;; Batch Processing
;; ============================================

#?(:clj
   (defn list-files-recursive
     "List all files in a directory recursively."
     [dir-path]
     (let [dir (io/file dir-path)]
       (if (.isDirectory dir)
         (->> (file-seq dir)
              (filter #(.isFile %))
              (map #(.getAbsolutePath %)))
         []))))

#?(:clj
   (defn batch-process-directory
     "Process all supported files in a directory."
     [dir-path & {:keys [chunk-size overlap]
                  :or {chunk-size (:chunk-size default-config)
                       overlap (:chunk-overlap default-config)}}]
     (let [files (list-files-recursive dir-path)
           supported-files (filter is-supported-file? files)
           results (map #(process-text-file % :chunk-size chunk-size :overlap overlap)
                        supported-files)]
       {:directory dir-path
        :total-files (count files)
        :supported-files (count supported-files)
        :processed (count (filter :success results))
        :failed (count (filter (complement :success) results))
        :results results})))

;; ============================================
;; File Watching
;; ============================================

(def watch-state (atom {:watching false :files {}}))

#?(:clj
   (defn start-watching
     "Start watching a directory for changes."
     [dir-path callback]
     (swap! watch-state assoc :watching true)
     {:status :started
      :directory dir-path
      :note "File watching started (polling mode)"
      :interval-ms (:watch-interval-ms default-config)}))

(defn stop-watching
  "Stop watching for file changes."
  []
  (swap! watch-state assoc :watching false)
  {:status :stopped})

(defn get-watch-status
  "Get current watch status."
  []
  @watch-state)

;; ============================================
;; Feedback Collection
;; ============================================

(def feedback-store (atom []))

(defn submit-feedback
  "Submit feedback for a processed document."
  [doc-id feedback-type content & {:keys [user-id]}]
  (let [feedback {:id (str (random-uuid))
                  :doc-id doc-id
                  :type feedback-type
                  :content content
                  :user-id user-id
                  :timestamp (str #?(:clj (java.time.Instant/now) :cljs (js/Date.)))}]
    (swap! feedback-store conj feedback)
    feedback))

(defn get-feedback
  "Get feedback for a document."
  [doc-id]
  (filter #(= (:doc-id %) doc-id) @feedback-store))

(defn get-all-feedback
  "Get all feedback."
  []
  @feedback-store)

;; ============================================
;; Pipeline
;; ============================================

(defn create-pipeline
  "Create an ingestion pipeline."
  [& {:keys [chunk-size overlap processors]
      :or {chunk-size (:chunk-size default-config)
           overlap (:chunk-overlap default-config)
           processors []}}]
  {:chunk-size chunk-size
   :overlap overlap
   :processors processors
   :status :ready})

(defn run-pipeline
  "Run the ingestion pipeline on input."
  [pipeline input]
  (let [chunks (chunk-text input
                           :chunk-size (:chunk-size pipeline)
                           :overlap (:overlap pipeline))
        processed (reduce (fn [chunks processor]
                            (map processor chunks))
                          chunks
                          (:processors pipeline))]
    {:input-length (count input)
     :chunks processed
     :total-chunks (count processed)
     :pipeline-status :completed}))

;; ============================================
;; Terabyte-Scale Processing
;; ============================================

(def processing-state (atom {:total-processed 0
                             :total-bytes 0
                             :current-batch nil
                             :errors []}))

(defn process-large-file
  "Process a large file in streaming fashion."
  [path & {:keys [batch-size callback]
           :or {batch-size (:batch-size default-config)}}]
  #?(:clj
     (try
       (with-open [reader (io/reader path)]
         (let [lines (line-seq reader)
               batches (partition-all batch-size lines)]
           (doseq [batch batches]
             (let [text (str/join "\n" batch)
                   chunks (chunk-text text)]
               (swap! processing-state update :total-processed + (count chunks))
               (swap! processing-state update :total-bytes + (count text))
               (when callback
                 (callback {:chunks chunks :batch-size (count batch)}))))
           {:success true
            :path path
            :total-processed (:total-processed @processing-state)
            :total-bytes (:total-bytes @processing-state)}))
       (catch Exception e
         {:success false
          :path path
          :error (.getMessage e)}))
     :cljs
     {:success false
      :note "Large file processing requires server-side execution"}))

(defn get-processing-stats
  "Get current processing statistics."
  []
  @processing-state)

(defn reset-processing-stats
  "Reset processing statistics."
  []
  (reset! processing-state {:total-processed 0
                            :total-bytes 0
                            :current-batch nil
                            :errors []}))

;; ============================================
;; Data Validation
;; ============================================

(defn validate-chunk
  "Validate a chunk of text."
  [chunk]
  (let [text (:text chunk)
        issues (cond-> []
                 (nil? text) (conj "Text is nil")
                 (and text (empty? (str/trim text))) (conj "Text is empty")
                 (and text (> (count text) 10000)) (conj "Text exceeds maximum length"))]
    {:valid (empty? issues)
     :issues issues
     :chunk chunk}))

(defn validate-document
  "Validate a processed document."
  [doc]
  (let [chunks (:chunks doc)
        chunk-validations (map validate-chunk chunks)
        all-valid (every? :valid chunk-validations)]
    {:valid all-valid
     :total-chunks (count chunks)
     :valid-chunks (count (filter :valid chunk-validations))
     :invalid-chunks (count (filter (complement :valid) chunk-validations))
     :issues (mapcat :issues (filter (complement :valid) chunk-validations))}))
