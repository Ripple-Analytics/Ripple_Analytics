(ns mental-models.pipeline.integration.document-processor
  "Document Processor Integration
   
   Multi-format document extraction and processing:
   - PDF extraction with text and metadata
   - DOCX/DOC processing
   - HTML parsing
   - JSON/CSV data extraction
   - RTF processing
   - Markdown parsing"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.data.csv :as csv])
  (:import
   [org.apache.pdfbox.pdmodel PDDocument]
   [org.apache.pdfbox.text PDFTextStripper]
   [org.apache.poi.xwpf.usermodel XWPFDocument]
   [org.jsoup Jsoup]
   [java.io FileInputStream]))

;; =============================================================================
;; SUPPORTED FORMATS
;; =============================================================================

(def supported-formats
  {:pdf {:extensions [".pdf"] :mime-types ["application/pdf"]}
   :docx {:extensions [".docx"] :mime-types ["application/vnd.openxmlformats-officedocument.wordprocessingml.document"]}
   :doc {:extensions [".doc"] :mime-types ["application/msword"]}
   :txt {:extensions [".txt"] :mime-types ["text/plain"]}
   :md {:extensions [".md" ".markdown"] :mime-types ["text/markdown"]}
   :html {:extensions [".html" ".htm"] :mime-types ["text/html"]}
   :json {:extensions [".json"] :mime-types ["application/json"]}
   :csv {:extensions [".csv"] :mime-types ["text/csv"]}
   :rtf {:extensions [".rtf"] :mime-types ["application/rtf"]}})

;; =============================================================================
;; FORMAT DETECTION
;; =============================================================================

(defn detect-format [file-path]
  (let [lower-path (str/lower-case file-path)]
    (some (fn [[format {:keys [extensions]}]]
            (when (some #(str/ends-with? lower-path %) extensions)
              format))
          supported-formats)))

(defn supported-file? [file-path]
  (boolean (detect-format file-path)))

;; =============================================================================
;; TEXT EXTRACTORS
;; =============================================================================

(defn extract-pdf-text [file-path]
  (log/debug "Extracting PDF text" {:path file-path})
  (try
    (with-open [doc (PDDocument/load (io/file file-path))]
      (let [stripper (PDFTextStripper.)
            text (.getText stripper doc)
            metadata {:page-count (.getNumberOfPages doc)
                      :title (-> doc .getDocumentInformation .getTitle)
                      :author (-> doc .getDocumentInformation .getAuthor)
                      :subject (-> doc .getDocumentInformation .getSubject)}]
        {:text text
         :metadata metadata
         :format :pdf
         :char-count (count text)}))
    (catch Exception e
      (log/error "Failed to extract PDF" {:path file-path :error (.getMessage e)})
      nil)))

(defn extract-docx-text [file-path]
  (log/debug "Extracting DOCX text" {:path file-path})
  (try
    (with-open [fis (FileInputStream. file-path)
                doc (XWPFDocument. fis)]
      (let [paragraphs (.getParagraphs doc)
            text (str/join "
" (map #(.getText %) paragraphs))
            metadata {:paragraph-count (count paragraphs)}]
        {:text text
         :metadata metadata
         :format :docx
         :char-count (count text)}))
    (catch Exception e
      (log/error "Failed to extract DOCX" {:path file-path :error (.getMessage e)})
      nil)))

(defn extract-html-text [file-path]
  (log/debug "Extracting HTML text" {:path file-path})
  (try
    (let [doc (Jsoup/parse (io/file file-path) "UTF-8")
          text (.text doc)
          title (.title doc)
          metadata {:title title
                    :links (count (.select doc "a"))
                    :images (count (.select doc "img"))}]
      {:text text
       :metadata metadata
       :format :html
       :char-count (count text)})
    (catch Exception e
      (log/error "Failed to extract HTML" {:path file-path :error (.getMessage e)})
      nil)))

(defn extract-json-text [file-path]
  (log/debug "Extracting JSON text" {:path file-path})
  (try
    (let [content (slurp file-path)
          data (json/read-str content :key-fn keyword)
          text (json/write-str data :indent true)
          metadata {:keys (if (map? data) (count (keys data)) 0)
                    :array-length (if (vector? data) (count data) 0)}]
      {:text text
       :metadata metadata
       :format :json
       :char-count (count text)
       :data data})
    (catch Exception e
      (log/error "Failed to extract JSON" {:path file-path :error (.getMessage e)})
      nil)))

(defn extract-csv-text [file-path]
  (log/debug "Extracting CSV text" {:path file-path})
  (try
    (with-open [reader (io/reader file-path)]
      (let [data (doall (csv/read-csv reader))
            headers (first data)
            rows (rest data)
            text (str/join "
" (map #(str/join ", " %) data))
            metadata {:columns (count headers)
                      :rows (count rows)
                      :headers headers}]
        {:text text
         :metadata metadata
         :format :csv
         :char-count (count text)
         :data {:headers headers :rows rows}}))
    (catch Exception e
      (log/error "Failed to extract CSV" {:path file-path :error (.getMessage e)})
      nil)))

(defn extract-plain-text [file-path]
  (log/debug "Extracting plain text" {:path file-path})
  (try
    (let [text (slurp file-path)
          lines (str/split-lines text)
          metadata {:line-count (count lines)
                    :word-count (count (str/split text #"\s+"))}]
      {:text text
       :metadata metadata
       :format (if (str/ends-with? (str/lower-case file-path) ".md") :md :txt)
       :char-count (count text)})
    (catch Exception e
      (log/error "Failed to extract text" {:path file-path :error (.getMessage e)})
      nil)))

;; =============================================================================
;; UNIFIED EXTRACTION
;; =============================================================================

(defn extract-document
  "Extract text and metadata from a document."
  [file-path]
  (when (flags/is-enabled? "document-processing")
    (let [format (detect-format file-path)
          start-time (System/currentTimeMillis)]
      (log/info "Extracting document" {:path file-path :format format})
      (let [result (case format
                     :pdf (extract-pdf-text file-path)
                     :docx (extract-docx-text file-path)
                     :doc (extract-docx-text file-path)
                     :html (extract-html-text file-path)
                     :json (extract-json-text file-path)
                     :csv (extract-csv-text file-path)
                     (:txt :md :rtf) (extract-plain-text file-path)
                     nil)]
        (when result
          (let [duration (- (System/currentTimeMillis) start-time)
                enriched (assoc result
                                :file-path file-path
                                :extraction-time-ms duration)]
            ;; Record metrics
            (metrics/inc-counter! :document-processor/extractions)
            (metrics/observe-histogram! :document-processor/extraction-time duration)
            ;; Publish event
            (events/publish! :document/extracted enriched)
            ;; Audit
            (audit/log-operation! {:operation :document-extracted
                                   :file-path file-path
                                   :format format
                                   :char-count (:char-count result)})
            enriched))))))

;; =============================================================================
;; BATCH PROCESSING
;; =============================================================================

(defn extract-documents
  "Extract text from multiple documents."
  [file-paths]
  (log/info "Batch extracting documents" {:count (count file-paths)})
  (let [start-time (System/currentTimeMillis)
        results (doall (map extract-document file-paths))
        successful (filter some? results)
        duration (- (System/currentTimeMillis) start-time)]
    {:total (count file-paths)
     :successful (count successful)
     :failed (- (count file-paths) (count successful))
     :duration-ms duration
     :results successful}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-document-processor!
  "Initialize document processor."
  []
  (log/info "Initializing document processor")
  ;; Register feature flag
  (flags/register-flag! "document-processing" "Enable document processing" true)
  ;; Create metrics
  (metrics/create-counter! :document-processor/extractions "Documents extracted")
  (metrics/create-histogram! :document-processor/extraction-time "Extraction time" [100 500 1000 5000 10000])
  (log/info "Document processor initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-processor-status []
  {:enabled (flags/is-enabled? "document-processing")
   :supported-formats (keys supported-formats)})
