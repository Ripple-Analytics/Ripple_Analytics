(ns mental-models.desktop.extractor
  "Text extraction from various file formats"
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log])
  (:import [org.apache.pdfbox Loader]
           [org.apache.pdfbox.text PDFTextStripper]
           [org.apache.poi.xwpf.usermodel XWPFDocument]
           [java.io File FileInputStream]))

(defn extract-pdf-text [path]
  (log/debug "Extracting PDF:" path)
  (try
    (with-open [doc (Loader/loadPDF (io/file path))]
      (let [stripper (PDFTextStripper.)]
        (.getText stripper doc)))
    (catch Exception e
      (log/error e "Failed to extract PDF:" path)
      nil)))

(defn extract-docx-text [path]
  (log/debug "Extracting DOCX:" path)
  (try
    (with-open [fis (FileInputStream. (io/file path))
                doc (XWPFDocument. fis)]
      (->> (.getParagraphs doc)
           (map #(.getText %))
           (clojure.string/join "\n")))
    (catch Exception e
      (log/error e "Failed to extract DOCX:" path)
      nil)))
