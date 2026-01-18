(ns mental-models.scanner.file-extractors
  "Multi-format file text extraction
   
   Supports: PDF, DOCX, DOC, TXT, MD, HTML, JSON, CSV, RTF
   
   Uses:
   - Apache PDFBox for PDF
   - Apache POI for DOCX/DOC
   - Built-in for text formats"
  (:require
   [clojure.string :as str]
   #?(:clj [clojure.java.io :as io])))

;; =============================================================================
;; PDF EXTRACTION (Apache PDFBox)
;; =============================================================================

#?(:clj
   (import '[org.apache.pdfbox.pdmodel PDDocument]
           '[org.apache.pdfbox.text PDFTextStripper]))

#?(:clj
   (defn extract-pdf-text
     "Extract text from PDF file using PDFBox"
     [file-path]
     (try
       (with-open [doc (PDDocument/load (io/file file-path))]
         (let [stripper (PDFTextStripper.)]
           (.getText stripper doc)))
       (catch Exception e
         (println "[EXTRACTOR] PDF error:" (.getMessage e))
         nil))))

#?(:clj
   (defn extract-pdf-pages
     "Extract text from specific pages of PDF"
     [file-path start-page end-page]
     (try
       (with-open [doc (PDDocument/load (io/file file-path))]
         (let [stripper (doto (PDFTextStripper.)
                          (.setStartPage start-page)
                          (.setEndPage end-page))]
           (.getText stripper doc)))
       (catch Exception e nil))))

;; =============================================================================
;; DOCX EXTRACTION (Apache POI)
;; =============================================================================

#?(:clj
   (import '[org.apache.poi.xwpf.usermodel XWPFDocument]
           '[org.apache.poi.xwpf.extractor XWPFWordExtractor]))

#?(:clj
   (defn extract-docx-text
     "Extract text from DOCX file using Apache POI"
     [file-path]
     (try
       (with-open [fis (io/input-stream file-path)
                   doc (XWPFDocument. fis)]
         (let [extractor (XWPFWordExtractor. doc)]
           (.getText extractor)))
       (catch Exception e
         (println "[EXTRACTOR] DOCX error:" (.getMessage e))
         nil))))

;; =============================================================================
;; DOC EXTRACTION (Apache POI - older format)
;; =============================================================================

#?(:clj
   (import '[org.apache.poi.hwpf HWPFDocument]
           '[org.apache.poi.hwpf.extractor WordExtractor]))

#?(:clj
   (defn extract-doc-text
     "Extract text from DOC file (older Word format)"
     [file-path]
     (try
       (with-open [fis (io/input-stream file-path)
                   doc (HWPFDocument. fis)]
         (let [extractor (WordExtractor. doc)]
           (.getText extractor)))
       (catch Exception e
         (println "[EXTRACTOR] DOC error:" (.getMessage e))
         nil))))

;; =============================================================================
;; HTML EXTRACTION
;; =============================================================================

#?(:clj
   (defn extract-html-text
     "Extract text from HTML, removing tags and scripts"
     [file-path]
     (try
       (-> (slurp file-path)
           ;; Remove scripts
           (str/replace #"(?is)<script[^>]*>.*?</script>" "")
           ;; Remove styles
           (str/replace #"(?is)<style[^>]*>.*?</style>" "")
           ;; Remove comments
           (str/replace #"<!--.*?-->" "")
           ;; Remove tags
           (str/replace #"<[^>]+>" " ")
           ;; Decode common entities
           (str/replace "&nbsp;" " ")
           (str/replace "&amp;" "&")
           (str/replace "&lt;" "<")
           (str/replace "&gt;" ">")
           (str/replace "&quot;" "\"")
           ;; Normalize whitespace
           (str/replace #"\s+" " ")
           str/trim)
       (catch Exception e nil))))

;; =============================================================================
;; JSON EXTRACTION
;; =============================================================================

#?(:clj
   (defn extract-json-text
     "Extract text content from JSON file"
     [file-path]
     (try
       (let [content (slurp file-path)]
         ;; Extract string values from JSON
         (->> (re-seq #"\"([^\"]+)\"" content)
              (map second)
              (filter #(> (count %) 10))  ; Only meaningful strings
              (str/join " ")))
       (catch Exception e nil))))

;; =============================================================================
;; CSV EXTRACTION
;; =============================================================================

#?(:clj
   (defn extract-csv-text
     "Extract text from CSV file"
     [file-path]
     (try
       (-> (slurp file-path)
           (str/replace #"," " ")
           (str/replace #"\"" ""))
       (catch Exception e nil))))

;; =============================================================================
;; RTF EXTRACTION
;; =============================================================================

#?(:clj
   (defn extract-rtf-text
     "Extract text from RTF file"
     [file-path]
     (try
       (-> (slurp file-path)
           ;; Remove RTF control words
           (str/replace #"\\[a-z]+\d*\s?" "")
           ;; Remove braces
           (str/replace #"[{}]" "")
           str/trim)
       (catch Exception e nil))))

;; =============================================================================
;; PLAIN TEXT
;; =============================================================================

#?(:clj
   (defn extract-plain-text
     "Extract text from plain text files (TXT, MD)"
     [file-path]
     (try
       (slurp file-path)
       (catch Exception e nil))))

;; =============================================================================
;; UNIFIED EXTRACTOR
;; =============================================================================

#?(:clj
   (defn extract-text
     "Extract text from any supported file format"
     [file-path]
     (let [ext (str/lower-case (or (re-find #"\.[^.]+$" file-path) ""))]
       (case ext
         ".pdf" (extract-pdf-text file-path)
         ".docx" (extract-docx-text file-path)
         ".doc" (extract-doc-text file-path)
         ".html" (extract-html-text file-path)
         ".htm" (extract-html-text file-path)
         ".json" (extract-json-text file-path)
         ".csv" (extract-csv-text file-path)
         ".rtf" (extract-rtf-text file-path)
         ".txt" (extract-plain-text file-path)
         ".md" (extract-plain-text file-path)
         ".markdown" (extract-plain-text file-path)
         ;; Default: try plain text
         (extract-plain-text file-path)))))

;; =============================================================================
;; METADATA EXTRACTION
;; =============================================================================

#?(:clj
   (defn get-file-metadata
     "Get metadata about a file"
     [file-path]
     (try
       (let [f (io/file file-path)]
         {:path file-path
          :name (.getName f)
          :size (.length f)
          :modified (.lastModified f)
          :extension (str/lower-case (or (re-find #"\.[^.]+$" file-path) ""))
          :exists (.exists f)
          :readable (.canRead f)})
       (catch Exception e
         {:path file-path :error (.getMessage e)}))))

;; =============================================================================
;; BATCH EXTRACTION
;; =============================================================================

#?(:clj
   (defn extract-batch
     "Extract text from multiple files in parallel"
     [file-paths]
     (->> file-paths
          (pmap (fn [path]
                  {:path path
                   :text (extract-text path)
                   :metadata (get-file-metadata path)}))
          (filter #(:text %)))))

;; =============================================================================
;; TEXT PREPROCESSING
;; =============================================================================

#?(:clj
   (defn preprocess-text
     "Clean and normalize extracted text"
     [text]
     (when text
       (-> text
           ;; Normalize line endings
           (str/replace #"\r\n" "\n")
           (str/replace #"\r" "\n")
           ;; Remove excessive newlines
           (str/replace #"\n{3,}" "\n\n")
           ;; Normalize whitespace
           (str/replace #"[ \t]+" " ")
           ;; Trim
           str/trim))))

#?(:clj
   (defn extract-and-preprocess
     "Extract text and preprocess it"
     [file-path]
     (-> file-path
         extract-text
         preprocess-text)))
