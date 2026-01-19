(ns mental-models.pipeline.integration.data-exporter
  "Data Exporter Module
   
   Multi-format data export:
   - CSV, JSON, EDN, XML export
   - Excel export
   - PDF report generation
   - Streaming export for large datasets
   - Export scheduling"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.io StringWriter BufferedWriter FileWriter]
   [java.util.zip GZIPOutputStream]
   [java.time LocalDateTime]
   [java.time.format DateTimeFormatter]))

;; =============================================================================
;; EXPORTER STATE
;; =============================================================================

(defonce exporter-state (atom {:exports {}
                               :templates {}
                               :config {:default-format :json
                                        :compression-threshold 10000
                                        :export-dir "/tmp/exports"}}))

;; =============================================================================
;; FORMAT CONVERTERS
;; =============================================================================

(defn to-csv
  "Convert data to CSV format."
  [data & {:keys [headers delimiter] :or {delimiter ","}}]
  (let [headers (or headers (keys (first data)))
        header-line (str/join delimiter (map name headers))
        data-lines (for [row data]
                     (str/join delimiter
                               (for [h headers]
                                 (let [v (get row h)]
                                   (if (string? v)
                                     (str "\"" (str/replace v "\"" "\"\"") "\"")
                                     (str v))))))]
    (str/join "\n" (cons header-line data-lines))))

(defn to-json
  "Convert data to JSON format."
  [data & {:keys [pretty?] :or {pretty? true}}]
  (if pretty?
    (with-out-str (json/pprint data))
    (json/write-str data)))

(defn to-edn
  "Convert data to EDN format."
  [data & {:keys [pretty?] :or {pretty? true}}]
  (if pretty?
    (with-out-str (clojure.pprint/pprint data))
    (pr-str data)))

(defn to-xml
  "Convert data to XML format."
  [data & {:keys [root-element] :or {root-element "data"}}]
  (letfn [(element->xml [k v]
            (cond
              (map? v)
              (str "<" (name k) ">"
                   (str/join "" (map (fn [[k2 v2]] (element->xml k2 v2)) v))
                   "</" (name k) ">")
              
              (sequential? v)
              (str/join "" (map #(element->xml k %) v))
              
              :else
              (str "<" (name k) ">" v "</" (name k) ">")))]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         "<" root-element ">"
         (if (sequential? data)
           (str/join "" (map #(element->xml "item" %) data))
           (str/join "" (map (fn [[k v]] (element->xml k v)) data)))
         "</" root-element ">")))

(defn to-markdown-table
  "Convert data to Markdown table format."
  [data & {:keys [headers]}]
  (let [headers (or headers (keys (first data)))
        header-line (str "| " (str/join " | " (map name headers)) " |")
        separator (str "| " (str/join " | " (repeat (count headers) "---")) " |")
        data-lines (for [row data]
                     (str "| " (str/join " | " (map #(str (get row %)) headers)) " |"))]
    (str/join "\n" (concat [header-line separator] data-lines))))

;; =============================================================================
;; EXPORT FUNCTIONS
;; =============================================================================

(defn generate-export-id
  "Generate a unique export ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn generate-filename
  "Generate a filename for export."
  [export-id format]
  (let [timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyyMMdd_HHmmss"))
        extension (name format)]
    (str "export_" timestamp "_" (subs export-id 0 8) "." extension)))

(defn export-data
  "Export data to specified format."
  [data format & {:keys [filename compress? headers]}]
  (when (flags/is-enabled? "data-exporter")
    (log/info "Exporting data" {:format format :records (count data)})
    (metrics/inc-counter! :export/started)
    (let [export-id (generate-export-id)
          filename (or filename (generate-filename export-id format))
          content (case format
                    :csv (to-csv data :headers headers)
                    :json (to-json data)
                    :edn (to-edn data)
                    :xml (to-xml data)
                    :markdown (to-markdown-table data :headers headers)
                    (to-json data))
          should-compress (or compress?
                              (> (count data) (get-in @exporter-state [:config :compression-threshold])))
          final-filename (if should-compress (str filename ".gz") filename)
          export-dir (get-in @exporter-state [:config :export-dir])
          filepath (str export-dir "/" final-filename)]
      ;; Ensure export directory exists
      (.mkdirs (io/file export-dir))
      ;; Write file
      (if should-compress
        (with-open [out (GZIPOutputStream. (io/output-stream filepath))]
          (.write out (.getBytes content "UTF-8")))
        (spit filepath content))
      ;; Track export
      (let [export-record {:id export-id
                           :filename final-filename
                           :filepath filepath
                           :format format
                           :records (count data)
                           :compressed should-compress
                           :size (.length (io/file filepath))
                           :created-at (System/currentTimeMillis)}]
        (swap! exporter-state assoc-in [:exports export-id] export-record)
        (metrics/inc-counter! :export/completed)
        (events/publish! :export/completed export-record)
        export-record))))

;; =============================================================================
;; STREAMING EXPORT
;; =============================================================================

(defn stream-export
  "Stream export for large datasets."
  [data-fn format filepath & {:keys [batch-size] :or {batch-size 1000}}]
  (when (flags/is-enabled? "data-exporter")
    (log/info "Starting streaming export" {:format format :filepath filepath})
    (with-open [writer (BufferedWriter. (FileWriter. filepath))]
      (let [first-batch (data-fn 0 batch-size)
            headers (keys (first first-batch))]
        ;; Write header for CSV
        (when (= format :csv)
          (.write writer (str (str/join "," (map name headers)) "\n")))
        ;; Write first batch
        (doseq [row first-batch]
          (.write writer (case format
                           :csv (str (str/join "," (map #(str (get row %)) headers)) "\n")
                           :json (str (json/write-str row) "\n")
                           (str (pr-str row) "\n"))))
        ;; Continue with remaining batches
        (loop [offset batch-size
               total (count first-batch)]
          (let [batch (data-fn offset batch-size)]
            (when (seq batch)
              (doseq [row batch]
                (.write writer (case format
                                 :csv (str (str/join "," (map #(str (get row %)) headers)) "\n")
                                 :json (str (json/write-str row) "\n")
                                 (str (pr-str row) "\n"))))
              (recur (+ offset batch-size) (+ total (count batch))))))))))

;; =============================================================================
;; EXPORT TEMPLATES
;; =============================================================================

(defn register-template!
  "Register an export template."
  [template-id {:keys [format fields transformations filters]}]
  (log/info "Registering export template" {:id template-id})
  (swap! exporter-state assoc-in [:templates template-id]
         {:id template-id
          :format format
          :fields fields
          :transformations transformations
          :filters filters}))

(defn get-template
  "Get an export template."
  [template-id]
  (get-in @exporter-state [:templates template-id]))

(defn export-with-template
  "Export data using a template."
  [template-id data]
  (when-let [template (get-template template-id)]
    (let [{:keys [format fields transformations filters]} template
          ;; Apply filters
          filtered-data (if filters
                          (filter (fn [row] (every? (fn [[k v]] (= (get row k) v)) filters)) data)
                          data)
          ;; Apply field selection
          selected-data (if fields
                          (map #(select-keys % fields) filtered-data)
                          filtered-data)
          ;; Apply transformations
          transformed-data (if transformations
                             (map (fn [row]
                                    (reduce (fn [r [k transform-fn]]
                                              (update r k transform-fn))
                                            row
                                            transformations))
                                  selected-data)
                             selected-data)]
      (export-data transformed-data format))))

;; =============================================================================
;; EXPORT MANAGEMENT
;; =============================================================================

(defn get-export
  "Get an export by ID."
  [export-id]
  (get-in @exporter-state [:exports export-id]))

(defn list-exports
  "List all exports."
  [& {:keys [limit format]}]
  (let [exports (vals (:exports @exporter-state))
        filtered (if format
                   (filter #(= (:format %) format) exports)
                   exports)
        sorted (sort-by :created-at > filtered)]
    (if limit
      (take limit sorted)
      sorted)))

(defn delete-export!
  "Delete an export file."
  [export-id]
  (when-let [export (get-export export-id)]
    (log/info "Deleting export" {:id export-id})
    (io/delete-file (:filepath export) true)
    (swap! exporter-state update :exports dissoc export-id)))

(defn cleanup-old-exports!
  "Clean up exports older than specified age."
  [max-age-ms]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        old-exports (filter #(< (:created-at %) cutoff) (vals (:exports @exporter-state)))]
    (doseq [export old-exports]
      (delete-export! (:id export)))
    (count old-exports)))

;; =============================================================================
;; SCHEDULED EXPORTS
;; =============================================================================

(defonce scheduled-exports (atom {}))

(defn schedule-export!
  "Schedule a recurring export."
  [schedule-id {:keys [data-fn format interval-ms]}]
  (log/info "Scheduling export" {:id schedule-id :interval-ms interval-ms})
  (let [running (atom true)
        task (future
               (while @running
                 (try
                   (let [data (data-fn)]
                     (export-data data format))
                   (catch Exception e
                     (log/error "Scheduled export failed" {:id schedule-id :error (.getMessage e)})))
                 (Thread/sleep interval-ms)))]
    (swap! scheduled-exports assoc schedule-id {:running running :task task})
    schedule-id))

(defn cancel-scheduled-export!
  "Cancel a scheduled export."
  [schedule-id]
  (when-let [{:keys [running task]} (get @scheduled-exports schedule-id)]
    (reset! running false)
    (future-cancel task)
    (swap! scheduled-exports dissoc schedule-id)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-exporter!
  "Initialize data exporter."
  []
  (log/info "Initializing data exporter")
  ;; Register feature flag
  (flags/register-flag! "data-exporter" "Enable data export" true)
  ;; Create metrics
  (metrics/create-counter! :export/started "Exports started")
  (metrics/create-counter! :export/completed "Exports completed")
  (metrics/create-gauge! :export/total "Total exports"
                         #(count (:exports @exporter-state)))
  ;; Ensure export directory exists
  (let [export-dir (get-in @exporter-state [:config :export-dir])]
    (.mkdirs (io/file export-dir)))
  (log/info "Data exporter initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-exporter-status []
  {:enabled (flags/is-enabled? "data-exporter")
   :total-exports (count (:exports @exporter-state))
   :templates (count (:templates @exporter-state))
   :scheduled-exports (count @scheduled-exports)
   :config (:config @exporter-state)})
