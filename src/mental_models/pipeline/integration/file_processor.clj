(ns mental-models.pipeline.integration.file-processor
  "File Processor Module
   
   File processing and transformation:
   - Multi-format file reading
   - File transformation pipelines
   - Streaming file processing
   - File validation
   - Output generation"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.io File BufferedReader BufferedWriter FileInputStream FileOutputStream]
   [java.nio.file Files Paths StandardCopyOption]
   [java.security MessageDigest]
   [java.util Base64]))

;; =============================================================================
;; FILE PROCESSOR STATE
;; =============================================================================

(defonce processor-state (atom {:readers (ConcurrentHashMap.)
                                :writers (ConcurrentHashMap.)
                                :transformers (ConcurrentHashMap.)
                                :validators (ConcurrentHashMap.)
                                :processed-count (AtomicLong. 0)
                                :config {:default-encoding "UTF-8"
                                         :buffer-size 8192
                                         :max-file-size (* 100 1024 1024)}}))

;; =============================================================================
;; FILE READERS
;; =============================================================================

(defn register-reader!
  "Register a file reader for a format."
  [format-id read-fn & {:keys [description extensions]}]
  (log/info "Registering file reader" {:format format-id})
  (.put ^ConcurrentHashMap (:readers @processor-state) format-id
        {:fn read-fn
         :description description
         :extensions (or extensions [])}))

(defn unregister-reader!
  "Unregister a file reader."
  [format-id]
  (.remove ^ConcurrentHashMap (:readers @processor-state) format-id))

(defn get-reader
  "Get a reader for a format."
  [format-id]
  (.get ^ConcurrentHashMap (:readers @processor-state) format-id))

(defn detect-format
  "Detect file format from extension."
  [file-path]
  (let [ext (str/lower-case (or (last (str/split (str file-path) #"\.")) ""))]
    (case ext
      ("json") :json
      ("edn") :edn
      ("csv") :csv
      ("tsv") :tsv
      ("txt" "text") :text
      ("xml") :xml
      ("yaml" "yml") :yaml
      ("md" "markdown") :markdown
      ("html" "htm") :html
      :binary)))

(defn read-file
  "Read a file using the appropriate reader."
  [file-path & {:keys [format encoding]}]
  (let [format (or format (detect-format file-path))
        encoding (or encoding (get-in @processor-state [:config :default-encoding]))]
    (metrics/inc-counter! :fileprocessor/files-read)
    (if-let [reader (get-reader format)]
      ((:fn reader) file-path encoding)
      ;; Default readers
      (case format
        :json (with-open [r (io/reader file-path :encoding encoding)]
                (json/read r :key-fn keyword))
        :edn (edn/read-string (slurp file-path :encoding encoding))
        :text (slurp file-path :encoding encoding)
        :csv (with-open [r (io/reader file-path :encoding encoding)]
               (vec (line-seq r)))
        :tsv (with-open [r (io/reader file-path :encoding encoding)]
               (vec (line-seq r)))
        ;; Binary
        (Files/readAllBytes (Paths/get file-path (into-array String [])))))))

(defn read-lines
  "Read file lines lazily."
  [file-path & {:keys [encoding skip-empty]}]
  (let [encoding (or encoding (get-in @processor-state [:config :default-encoding]))]
    (with-open [r (io/reader file-path :encoding encoding)]
      (let [lines (line-seq r)]
        (vec (if skip-empty
               (remove str/blank? lines)
               lines))))))

(defn read-streaming
  "Read file with streaming callback."
  [file-path callback & {:keys [encoding buffer-size]}]
  (let [encoding (or encoding (get-in @processor-state [:config :default-encoding]))
        buffer-size (or buffer-size (get-in @processor-state [:config :buffer-size]))]
    (with-open [r (io/reader file-path :encoding encoding)]
      (let [buffer (char-array buffer-size)]
        (loop [total 0]
          (let [n (.read r buffer)]
            (when (pos? n)
              (callback (String. buffer 0 n) total)
              (recur (+ total n)))))))))

;; =============================================================================
;; FILE WRITERS
;; =============================================================================

(defn register-writer!
  "Register a file writer for a format."
  [format-id write-fn & {:keys [description extensions]}]
  (log/info "Registering file writer" {:format format-id})
  (.put ^ConcurrentHashMap (:writers @processor-state) format-id
        {:fn write-fn
         :description description
         :extensions (or extensions [])}))

(defn unregister-writer!
  "Unregister a file writer."
  [format-id]
  (.remove ^ConcurrentHashMap (:writers @processor-state) format-id))

(defn get-writer
  "Get a writer for a format."
  [format-id]
  (.get ^ConcurrentHashMap (:writers @processor-state) format-id))

(defn write-file
  "Write data to a file."
  [file-path data & {:keys [format encoding append]}]
  (let [format (or format (detect-format file-path))
        encoding (or encoding (get-in @processor-state [:config :default-encoding]))]
    (metrics/inc-counter! :fileprocessor/files-written)
    (if-let [writer (get-writer format)]
      ((:fn writer) file-path data encoding append)
      ;; Default writers
      (case format
        :json (spit file-path (json/write-str data) :encoding encoding :append append)
        :edn (spit file-path (pr-str data) :encoding encoding :append append)
        :text (spit file-path data :encoding encoding :append append)
        :csv (with-open [w (io/writer file-path :encoding encoding :append append)]
               (doseq [line data]
                 (.write w (str line "\n"))))
        ;; Binary
        (Files/write (Paths/get file-path (into-array String []))
                     (if (bytes? data) data (.getBytes (str data) encoding)))))))

(defn append-file
  "Append data to a file."
  [file-path data & opts]
  (apply write-file file-path data :append true opts))

(defn write-lines
  "Write lines to a file."
  [file-path lines & {:keys [encoding append]}]
  (let [encoding (or encoding (get-in @processor-state [:config :default-encoding]))]
    (with-open [w (io/writer file-path :encoding encoding :append append)]
      (doseq [line lines]
        (.write w (str line "\n"))))))

;; =============================================================================
;; FILE TRANSFORMERS
;; =============================================================================

(defn register-transformer!
  "Register a file transformer."
  [transformer-id transform-fn & {:keys [description]}]
  (log/info "Registering file transformer" {:id transformer-id})
  (.put ^ConcurrentHashMap (:transformers @processor-state) transformer-id
        {:fn transform-fn
         :description description}))

(defn unregister-transformer!
  "Unregister a transformer."
  [transformer-id]
  (.remove ^ConcurrentHashMap (:transformers @processor-state) transformer-id))

(defn get-transformer
  "Get a transformer by ID."
  [transformer-id]
  (.get ^ConcurrentHashMap (:transformers @processor-state) transformer-id))

(defn transform-file
  "Transform a file using a transformer."
  [input-path output-path transformer-id & {:keys [options]}]
  (if-let [transformer (get-transformer transformer-id)]
    (let [input-data (read-file input-path)
          output-data ((:fn transformer) input-data options)]
      (write-file output-path output-data)
      (metrics/inc-counter! :fileprocessor/files-transformed)
      {:success true :input input-path :output output-path})
    {:success false :error :transformer-not-found}))

(defn transform-content
  "Transform file content in memory."
  [content transformer-id & {:keys [options]}]
  (if-let [transformer (get-transformer transformer-id)]
    ((:fn transformer) content options)
    content))

;; =============================================================================
;; FILE VALIDATORS
;; =============================================================================

(defn register-validator!
  "Register a file validator."
  [validator-id validate-fn & {:keys [description]}]
  (log/info "Registering file validator" {:id validator-id})
  (.put ^ConcurrentHashMap (:validators @processor-state) validator-id
        {:fn validate-fn
         :description description}))

(defn unregister-validator!
  "Unregister a validator."
  [validator-id]
  (.remove ^ConcurrentHashMap (:validators @processor-state) validator-id))

(defn get-validator
  "Get a validator by ID."
  [validator-id]
  (.get ^ConcurrentHashMap (:validators @processor-state) validator-id))

(defn validate-file
  "Validate a file."
  [file-path validator-id & {:keys [options]}]
  (if-let [validator (get-validator validator-id)]
    (let [result ((:fn validator) file-path options)]
      (metrics/inc-counter! :fileprocessor/files-validated)
      result)
    {:valid false :error :validator-not-found}))

;; =============================================================================
;; FILE UTILITIES
;; =============================================================================

(defn file-exists?
  "Check if a file exists."
  [file-path]
  (.exists (io/file file-path)))

(defn file-size
  "Get file size in bytes."
  [file-path]
  (.length (io/file file-path)))

(defn file-info
  "Get file information."
  [file-path]
  (let [file (io/file file-path)]
    {:path (.getAbsolutePath file)
     :name (.getName file)
     :exists (.exists file)
     :size (.length file)
     :directory (.isDirectory file)
     :readable (.canRead file)
     :writable (.canWrite file)
     :last-modified (.lastModified file)
     :format (detect-format file-path)}))

(defn list-files
  "List files in a directory."
  [dir-path & {:keys [recursive pattern]}]
  (let [dir (io/file dir-path)
        files (if recursive
                (file-seq dir)
                (.listFiles dir))]
    (cond->> (filter #(.isFile %) files)
      pattern (filter #(re-matches (re-pattern pattern) (.getName %)))
      true (map #(.getAbsolutePath %)))))

(defn copy-file
  "Copy a file."
  [source-path dest-path & {:keys [replace]}]
  (let [options (if replace
                  (into-array [StandardCopyOption/REPLACE_EXISTING])
                  (into-array StandardCopyOption []))]
    (Files/copy (Paths/get source-path (into-array String []))
                (Paths/get dest-path (into-array String []))
                options)
    (metrics/inc-counter! :fileprocessor/files-copied)))

(defn move-file
  "Move a file."
  [source-path dest-path & {:keys [replace]}]
  (let [options (if replace
                  (into-array [StandardCopyOption/REPLACE_EXISTING])
                  (into-array StandardCopyOption []))]
    (Files/move (Paths/get source-path (into-array String []))
                (Paths/get dest-path (into-array String []))
                options)
    (metrics/inc-counter! :fileprocessor/files-moved)))

(defn delete-file
  "Delete a file."
  [file-path]
  (io/delete-file file-path true)
  (metrics/inc-counter! :fileprocessor/files-deleted))

(defn file-checksum
  "Calculate file checksum."
  [file-path & {:keys [algorithm]}]
  (let [algorithm (or algorithm "SHA-256")
        digest (MessageDigest/getInstance algorithm)]
    (with-open [is (FileInputStream. file-path)]
      (let [buffer (byte-array 8192)]
        (loop []
          (let [n (.read is buffer)]
            (when (pos? n)
              (.update digest buffer 0 n)
              (recur))))))
    (.encodeToString (Base64/getEncoder) (.digest digest))))

(defn compare-files
  "Compare two files."
  [file1 file2]
  (let [checksum1 (file-checksum file1)
        checksum2 (file-checksum file2)]
    {:identical (= checksum1 checksum2)
     :file1 {:path file1 :checksum checksum1 :size (file-size file1)}
     :file2 {:path file2 :checksum checksum2 :size (file-size file2)}}))

;; =============================================================================
;; BATCH FILE PROCESSING
;; =============================================================================

(defn process-files
  "Process multiple files."
  [file-paths processor-fn & {:keys [parallel on-error on-complete]}]
  (let [results (atom [])
        errors (atom [])]
    (doseq [file-path file-paths]
      (try
        (let [result (processor-fn file-path)]
          (swap! results conj {:file file-path :result result :success true})
          (.incrementAndGet ^AtomicLong (:processed-count @processor-state)))
        (catch Exception e
          (let [error {:file file-path :error (.getMessage e) :success false}]
            (swap! errors conj error)
            (when on-error (on-error error))))))
    (let [summary {:total (count file-paths)
                   :successful (count @results)
                   :failed (count @errors)
                   :results @results
                   :errors @errors}]
      (when on-complete (on-complete summary))
      summary)))

(defn process-directory
  "Process all files in a directory."
  [dir-path processor-fn & opts]
  (let [files (list-files dir-path :recursive true)]
    (apply process-files files processor-fn opts)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-processor-stats
  "Get file processor statistics."
  []
  {:readers (.size ^ConcurrentHashMap (:readers @processor-state))
   :writers (.size ^ConcurrentHashMap (:writers @processor-state))
   :transformers (.size ^ConcurrentHashMap (:transformers @processor-state))
   :validators (.size ^ConcurrentHashMap (:validators @processor-state))
   :processed-count (.get ^AtomicLong (:processed-count @processor-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-file-processor!
  "Initialize file processor."
  []
  (log/info "Initializing file processor")
  ;; Register feature flag
  (flags/register-flag! "file-processor" "Enable file processor" true)
  ;; Create metrics
  (metrics/create-counter! :fileprocessor/files-read "Files read")
  (metrics/create-counter! :fileprocessor/files-written "Files written")
  (metrics/create-counter! :fileprocessor/files-transformed "Files transformed")
  (metrics/create-counter! :fileprocessor/files-validated "Files validated")
  (metrics/create-counter! :fileprocessor/files-copied "Files copied")
  (metrics/create-counter! :fileprocessor/files-moved "Files moved")
  (metrics/create-counter! :fileprocessor/files-deleted "Files deleted")
  (metrics/create-gauge! :fileprocessor/total-processed "Total processed"
                         #(.get ^AtomicLong (:processed-count @processor-state)))
  ;; Register built-in validators
  (register-validator! :exists (fn [path _] {:valid (file-exists? path)})
                       :description "Check if file exists")
  (register-validator! :max-size (fn [path opts]
                                   (let [max-size (or (:max-size opts) (get-in @processor-state [:config :max-file-size]))]
                                     {:valid (<= (file-size path) max-size)
                                      :size (file-size path)
                                      :max-size max-size}))
                       :description "Check file size limit")
  (log/info "File processor initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-file-processor-status []
  {:enabled (flags/is-enabled? "file-processor")
   :readers (.size ^ConcurrentHashMap (:readers @processor-state))
   :writers (.size ^ConcurrentHashMap (:writers @processor-state))
   :transformers (.size ^ConcurrentHashMap (:transformers @processor-state))
   :validators (.size ^ConcurrentHashMap (:validators @processor-state))
   :processed-count (.get ^AtomicLong (:processed-count @processor-state))
   :stats (get-processor-stats)
   :config (:config @processor-state)})
