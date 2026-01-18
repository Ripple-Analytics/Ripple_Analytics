(ns mental-models.io.file-ingestion
  "File Ingestion and Processing - Electric Clojure
   Replaces Python file processing and folder watching
   Supports: txt, md, pdf, docx, csv, json"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom])
  (:import [java.nio.file Files Paths WatchService StandardWatchEventKinds]
           [java.nio.file.attribute FileTime]
           [java.io File]))

;; -- File Type Detection --

(def supported-extensions
  #{".txt" ".md" ".markdown" ".pdf" ".docx" ".csv" ".json" ".log" ".html"})

(defn get-file-extension [filename]
  (let [idx (.lastIndexOf filename ".")]
    (if (>= idx 0)
      (str/lower-case (subs filename idx))
      "")))

(defn is-supported-file? [filename]
  (contains? supported-extensions (get-file-extension filename)))

;; -- Text Extraction --

(defn read-text-file
  "Read plain text file"
  [filepath]
  (try
    (slurp filepath)
    (catch Exception e
      (log/error e "Failed to read text file:" filepath)
      nil)))

(defn read-markdown-file
  "Read markdown file (same as text)"
  [filepath]
  (read-text-file filepath))

(defn read-csv-file
  "Read CSV file and convert to text"
  [filepath]
  (try
    (let [lines (str/split (slurp filepath) #"\n")]
      (str/join "\n" lines))
    (catch Exception e
      (log/error e "Failed to read CSV file:" filepath)
      nil)))

(defn read-json-file
  "Read JSON file and convert to text"
  [filepath]
  (try
    (let [content (slurp filepath)]
      ;; Pretty-print JSON for readability
      (str/replace content #"," ",\n"))
    (catch Exception e
      (log/error e "Failed to read JSON file:" filepath)
      nil)))

(defn read-pdf-file
  "Read PDF file using pdfbox"
  [filepath]
  (try
    ;; Use pdfbox library via Java interop
    (let [pdfbox (Class/forName "org.apache.pdfbox.pdmodel.PDDocument")
          load-method (.getMethod pdfbox "load" (into-array Class [File]))
          pdf-doc (.invoke load-method nil (into-array [(File. filepath)]))
          
          stripper (Class/forName "org.apache.pdfbox.text.PDFTextStripper")
          stripper-instance (.newInstance stripper)
          get-text-method (.getMethod stripper "getText" (into-array Class [pdfbox]))
          
          text (.invoke get-text-method stripper-instance (into-array [pdf-doc]))]
      
      (.close pdf-doc)
      text)
    (catch Exception e
      (log/error e "Failed to read PDF file:" filepath)
      nil)))

(defn read-docx-file
  "Read DOCX file using Apache POI"
  [filepath]
  (try
    ;; Use Apache POI library via Java interop
    (let [xwpf-doc (Class/forName "org.apache.poi.xwpf.usermodel.XWPFDocument")
          fis (java.io.FileInputStream. filepath)
          doc (.getConstructor xwpf-doc (into-array Class [java.io.InputStream]))
          doc-instance (.newInstance doc (into-array [fis]))
          
          paragraphs-method (.getMethod xwpf-doc "getParagraphs" (into-array Class []))
          paragraphs (.invoke paragraphs-method doc-instance (into-array []))
          
          text (str/join "\n" (map #(.getText %) paragraphs))]
      
      (.close fis)
      text)
    (catch Exception e
      (log/error e "Failed to read DOCX file:" filepath)
      nil)))

(defn extract-text
  "Extract text from file based on extension"
  [filepath]
  (let [ext (get-file-extension filepath)]
    (case ext
      ".txt" (read-text-file filepath)
      ".md" (read-markdown-file filepath)
      ".markdown" (read-markdown-file filepath)
      ".csv" (read-csv-file filepath)
      ".json" (read-json-file filepath)
      ".pdf" (read-pdf-file filepath)
      ".docx" (read-docx-file filepath)
      ".log" (read-text-file filepath)
      ".html" (read-text-file filepath)
      (do
        (log/warn "Unsupported file type:" ext)
        nil))))

;; -- File Metadata --

(defrecord FileMetadata
  [filename filepath size extension modified-at])

(defn get-file-metadata
  "Extract metadata from file"
  [filepath]
  (try
    (let [file (File. filepath)
          path (Paths/get filepath (into-array String []))]
      (->FileMetadata
       (.getName file)
       filepath
       (.length file)
       (get-file-extension filepath)
       (-> (Files/getLastModifiedTime path (into-array java.nio.file.LinkOption []))
           .toInstant)))
    (catch Exception e
      (log/error e "Failed to get file metadata:" filepath)
      nil)))

;; -- Folder Watching --

(defn watch-folder
  "Watch folder for new files and return channel of new files"
  [folder-path]
  (let [watch-chan (async/chan)
        watch-service (.newWatchService (java.nio.file.FileSystems/getDefault))
        path (Paths/get folder-path (into-array String []))]
    
    (try
      (.register path watch-service
                 (into-array [StandardWatchEventKinds/ENTRY_CREATE
                             StandardWatchEventKinds/ENTRY_MODIFY]))
      
      ;; Start watching in background thread
      (future
        (loop []
          (try
            (let [key (.poll watch-service)
                  events (when key (.pollEvents key))]
              
              (doseq [event events]
                (let [filename (.toString (.context event))
                      filepath (str folder-path "/" filename)]
                  (when (is-supported-file? filename)
                    (log/info "Detected file:" filepath)
                    (async/>!! watch-chan filepath))))
              
              (when key (.reset key))
              (recur))
            (catch Exception e
              (log/error e "Error in folder watcher")
              (recur)))))
      
      watch-chan)
    
    (catch Exception e
      (log/error e "Failed to start folder watcher:" folder-path)
      nil)))

;; -- Batch File Processing --

(defn scan-folder
  "Scan folder for all supported files"
  [folder-path]
  (try
    (let [file (File. folder-path)]
      (if (.isDirectory file)
        (->> (.listFiles file)
             (filter #(.isFile %))
             (filter #(is-supported-file? (.getName %)))
             (map #(.getAbsolutePath %))
             vec)
        (do
          (log/warn "Not a directory:" folder-path)
          [])))
    (catch Exception e
      (log/error e "Failed to scan folder:" folder-path)
      [])))

(defn process-folder
  "Process all files in folder"
  [folder-path]
  (let [files (scan-folder folder-path)]
    (log/info "Found" (count files) "supported files in" folder-path)
    
    (mapv (fn [filepath]
           (let [text (extract-text filepath)
                 metadata (get-file-metadata filepath)]
             {:filepath filepath
              :text text
              :metadata metadata
              :size (count text)}))
         files)))

;; -- Electric Components --

(e/defn FileUploadComponent
  "Component for file upload and processing"
  []
  (e/client
    (let [selected-file (e/server (atom nil))]
      (dom/div {:class "file-upload"}
        (dom/h3 "Upload File for Analysis")
        (dom/input {:type "file"
                   :accept (str/join "," (map #(str "*" %) supported-extensions))
                   :on-change (fn [e]
                               (let [file (-> e .-target .-files (aget 0))]
                                 (reset! selected-file file)))})
        
        (when @selected-file
          (dom/div
            (dom/p (str "Selected: " (.-name @selected-file)))
            (dom/p (str "Size: " (format "%.2f KB" (/ (.-size @selected-file) 1024))))))))))

(e/defn FolderWatcherComponent
  "Component for folder watching"
  [folder-path]
  (e/server
    (let [watch-chan (watch-folder folder-path)
          file-list (atom [])]
      
      ;; Start listening for files
      (future
        (loop []
          (when-let [filepath (async/<!! watch-chan)]
            (swap! file-list conj filepath)
            (recur))))
      
      (e/client
        (dom/div {:class "folder-watcher"}
          (dom/h3 (str "Watching: " folder-path))
          (dom/p (str "Files detected: " (count @file-list)))
          (dom/ul
            (doseq [file @file-list]
              (dom/li file))))))))

;; -- Utilities --

(defn clean-text
  "Clean extracted text"
  [text]
  (when text
    (-> text
        (str/replace #"\s+" " ")  ;; Normalize whitespace
        (str/trim))))

(defn chunk-text
  "Split text into chunks for analysis"
  [text chunk-size overlap]
  (loop [text text chunks []]
    (if (<= (count text) chunk-size)
      (conj chunks text)
      (let [chunk (subs text 0 chunk-size)
            remaining (subs text (- chunk-size overlap))]
        (recur remaining (conj chunks chunk))))))

(defn validate-text
  "Validate extracted text"
  [text]
  (and text
       (> (count text) 50)  ;; Minimum 50 characters
       (< (count text) 1000000)))  ;; Maximum 1MB

(defn get-file-stats
  "Get statistics about a file"
  [filepath]
  (let [text (extract-text filepath)
        metadata (get-file-metadata filepath)]
    {:filepath filepath
     :filename (:filename metadata)
     :size (:size metadata)
     :text-length (count text)
     :word-count (count (str/split text #"\s+"))
     :line-count (count (str/split text #"\n"))
     :modified-at (:modified-at metadata)}))
