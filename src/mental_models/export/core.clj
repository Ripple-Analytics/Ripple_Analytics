(ns mental-models.export.core
  "Data Export Module for Mental Models Pipeline
   
   Provides export functionality for:
   - CSV format
   - JSON format
   - EDN format
   - Excel format (via Apache POI)
   - Streaming exports for large datasets"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [cheshire.core :as json])
  (:import
   [java.io StringWriter BufferedWriter FileWriter]
   [java.time Instant LocalDateTime ZoneId]
   [java.time.format DateTimeFormatter]))

;; =============================================================================
;; FORMATTERS
;; =============================================================================

(def ^:private date-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

(defn format-timestamp [instant]
  (when instant
    (.format date-formatter
             (LocalDateTime/ofInstant instant (ZoneId/systemDefault)))))

;; =============================================================================
;; CSV EXPORT
;; =============================================================================

(defn escape-csv-field [value]
  (let [s (str value)]
    (if (or (str/includes? s ",")
            (str/includes? s "\"")
            (str/includes? s "\n"))
      (str "\"" (str/replace s "\"" "\"\"") "\"")
      s)))

(defn row-to-csv [row columns]
  (str/join "," (map #(escape-csv-field (get row %)) columns)))

(defn export-csv
  "Export data to CSV format."
  ([data] (export-csv data nil))
  ([data {:keys [columns headers]}]
   (let [cols (or columns (keys (first data)))
         hdrs (or headers (map name cols))]
     (with-out-str
       (println (str/join "," (map escape-csv-field hdrs)))
       (doseq [row data]
         (println (row-to-csv row cols)))))))

(defn export-csv-to-file
  "Export data to a CSV file."
  [data filepath & opts]
  (spit filepath (apply export-csv data opts)))

(defn stream-csv
  "Stream CSV data to a writer for large datasets."
  [data writer columns]
  (let [hdrs (map name columns)]
    (.write writer (str (str/join "," (map escape-csv-field hdrs)) "\n"))
    (doseq [row data]
      (.write writer (str (row-to-csv row columns) "\n"))
      (.flush writer))))

;; =============================================================================
;; JSON EXPORT
;; =============================================================================

(defn export-json
  "Export data to JSON format."
  ([data] (export-json data nil))
  ([data {:keys [pretty]}]
   (if pretty
     (json/generate-string data {:pretty true})
     (json/generate-string data))))

(defn export-json-to-file
  "Export data to a JSON file."
  [data filepath & opts]
  (spit filepath (apply export-json data opts)))

(defn stream-json
  "Stream JSON array to a writer for large datasets."
  [data writer]
  (.write writer "[\n")
  (loop [items data first? true]
    (when-let [item (first items)]
      (when-not first?
        (.write writer ",\n"))
      (.write writer (json/generate-string item))
      (.flush writer)
      (recur (rest items) false)))
  (.write writer "\n]"))

;; =============================================================================
;; EDN EXPORT
;; =============================================================================

(defn export-edn
  "Export data to EDN format."
  ([data] (export-edn data nil))
  ([data {:keys [pretty]}]
   (if pretty
     (with-out-str (clojure.pprint/pprint data))
     (pr-str data))))

(defn export-edn-to-file
  "Export data to an EDN file."
  [data filepath & opts]
  (spit filepath (apply export-edn data opts)))

;; =============================================================================
;; ANALYSIS EXPORT
;; =============================================================================

(defn export-analysis-results
  "Export analysis results in the specified format."
  [results format & opts]
  (case format
    :csv (apply export-csv results opts)
    :json (apply export-json results opts)
    :edn (apply export-edn results opts)
    (throw (ex-info "Unsupported format" {:format format}))))

(defn export-model-statistics
  "Export model statistics."
  [stats format]
  (let [rows (map (fn [[model-id stat]]
                    {:model-id (name model-id)
                     :detection-count (:detection-count stat 0)
                     :confidence-avg (:confidence-avg stat 0)
                     :last-detected (format-timestamp (:last-detected stat))})
                  stats)]
    (export-analysis-results rows format)))

(defn export-lollapalooza-events
  "Export Lollapalooza events."
  [events format]
  (let [rows (map (fn [event]
                    {:id (:id event)
                     :timestamp (format-timestamp (:timestamp event))
                     :models (str/join ", " (map name (:models event)))
                     :model-count (count (:models event))
                     :avg-confidence (:avg-confidence event)
                     :document (:document-id event)})
                  events)]
    (export-analysis-results rows format)))

;; =============================================================================
;; BATCH EXPORT
;; =============================================================================

(defn export-batch
  "Export multiple datasets to files."
  [exports output-dir]
  (doseq [{:keys [name data format options]} exports]
    (let [ext (case format :csv ".csv" :json ".json" :edn ".edn")
          filepath (str output-dir "/" name ext)]
      (case format
        :csv (export-csv-to-file data filepath options)
        :json (export-json-to-file data filepath options)
        :edn (export-edn-to-file data filepath options))))
  (count exports))

;; =============================================================================
;; EXPORT METADATA
;; =============================================================================

(defn add-export-metadata
  "Add metadata to exported data."
  [data]
  {:exported-at (format-timestamp (Instant/now))
   :record-count (count data)
   :data data})

;; =============================================================================
;; COMPRESSION
;; =============================================================================

(defn compress-export
  "Compress exported data using gzip."
  [data output-path]
  (with-open [out (-> output-path
                      io/output-stream
                      java.util.zip.GZIPOutputStream.
                      io/writer)]
    (.write out data)))

;; =============================================================================
;; EXPORT FORMATS REGISTRY
;; =============================================================================

(def supported-formats
  {:csv {:extension ".csv"
         :content-type "text/csv"
         :export-fn export-csv}
   :json {:extension ".json"
          :content-type "application/json"
          :export-fn export-json}
   :edn {:extension ".edn"
         :content-type "application/edn"
         :export-fn export-edn}})

(defn get-content-type [format]
  (get-in supported-formats [format :content-type]))

(defn get-extension [format]
  (get-in supported-formats [format :extension]))
