(ns mental-models.desktop.export.manager
  "Export and import functionality for Mental Models desktop app.
   Supports CSV, JSON, and PDF exports for scan results, decisions,
   and analysis reports."
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [mental-models.desktop.db :as db]
            [mental-models.desktop.api.web-client :as api])
  (:import [java.io File FileWriter BufferedWriter]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

;; =============================================================================
;; Export Paths
;; =============================================================================

(def export-dir (str (System/getProperty "user.home") "/Documents/MentalModels/exports"))

(defn ensure-export-dir! []
  (let [dir (io/file export-dir)]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn generate-export-filename
  "Generate a unique export filename"
  [prefix extension]
  (let [timestamp (-> (LocalDateTime/now)
                      (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss")))]
    (str prefix "_" timestamp "." extension)))

;; =============================================================================
;; CSV Export
;; =============================================================================

(defn escape-csv-field
  "Escape a field for CSV format"
  [field]
  (if (nil? field)
    ""
    (let [s (str field)]
      (if (or (str/includes? s ",")
              (str/includes? s "\"")
              (str/includes? s "\n"))
        (str "\"" (str/replace s "\"" "\"\"") "\"")
        s))))

(defn write-csv-row
  "Write a row to CSV"
  [writer fields]
  (.write writer (str (str/join "," (map escape-csv-field fields)) "\n")))

(defn export-scan-results-csv!
  "Export scan results to CSV"
  [& {:keys [file-path limit] :or {limit 1000}}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "scan_results" "csv")))
        results (db/get-recent-analyses limit)]
    (try
      (with-open [writer (BufferedWriter. (FileWriter. path))]
        ;; Header
        (write-csv-row writer ["File Path" "File Name" "Analyzed At" "Models Found" 
                               "Model Names" "Lollapalooza" "Confidence Avg"])
        ;; Data rows
        (doseq [result results]
          (let [models (:mental-models result)
                model-names (str/join "; " (map :name models))
                avg-confidence (if (seq models)
                                 (/ (reduce + (map #(or (:confidence %) 0) models)) (count models))
                                 0)]
            (write-csv-row writer [(:file-path result)
                                   (:file-name result)
                                   (:analyzed-at result)
                                   (count models)
                                   model-names
                                   (:lollapalooza result)
                                   (format "%.2f" (double avg-confidence))]))))
      {:success true :path path :count (count results)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn export-decisions-csv!
  "Export decisions to CSV"
  [& {:keys [file-path]}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "decisions" "csv")))
        decisions (or (api/get-decisions) [])]
    (try
      (with-open [writer (BufferedWriter. (FileWriter. path))]
        ;; Header
        (write-csv-row writer ["ID" "Title" "Description" "Decision Date" "Outcome" 
                               "Outcome Date" "Models Used" "Confidence" "Notes"])
        ;; Data rows
        (doseq [decision decisions]
          (write-csv-row writer [(:id decision)
                                 (:title decision)
                                 (:description decision)
                                 (:decision-date decision)
                                 (:outcome decision)
                                 (:outcome-date decision)
                                 (str/join "; " (map :name (:models decision)))
                                 (:confidence decision)
                                 (:notes decision)])))
      {:success true :path path :count (count decisions)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn export-signals-csv!
  "Export signals to CSV"
  [& {:keys [file-path]}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "signals" "csv")))
        signals (or (api/get-signals) [])]
    (try
      (with-open [writer (BufferedWriter. (FileWriter. path))]
        ;; Header
        (write-csv-row writer ["ID" "Title" "Source" "Severity" "Risk Level" 
                               "Created At" "Models" "Status" "URL"])
        ;; Data rows
        (doseq [signal signals]
          (write-csv-row writer [(:id signal)
                                 (:title signal)
                                 (:source signal)
                                 (:severity signal)
                                 (:risk-level signal)
                                 (:created-at signal)
                                 (str/join "; " (map :name (:models signal)))
                                 (:status signal)
                                 (:url signal)])))
      {:success true :path path :count (count signals)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

;; =============================================================================
;; JSON Export
;; =============================================================================

(defn export-scan-results-json!
  "Export scan results to JSON"
  [& {:keys [file-path limit pretty] :or {limit 1000 pretty true}}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "scan_results" "json")))
        results (db/get-recent-analyses limit)
        export-data {:export-type "scan-results"
                     :exported-at (str (Instant/now))
                     :count (count results)
                     :data results}]
    (try
      (spit path (if pretty
                   (json/write-str export-data :indent true)
                   (json/write-str export-data)))
      {:success true :path path :count (count results)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn export-decisions-json!
  "Export decisions to JSON"
  [& {:keys [file-path pretty] :or {pretty true}}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "decisions" "json")))
        decisions (or (api/get-decisions) [])
        export-data {:export-type "decisions"
                     :exported-at (str (Instant/now))
                     :count (count decisions)
                     :data decisions}]
    (try
      (spit path (if pretty
                   (json/write-str export-data :indent true)
                   (json/write-str export-data)))
      {:success true :path path :count (count decisions)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn export-all-data-json!
  "Export all data to a single JSON file"
  [& {:keys [file-path pretty] :or {pretty true}}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "all_data" "json")))
        scan-results (db/get-recent-analyses 10000)
        decisions (or (api/get-decisions) [])
        signals (or (api/get-signals) [])
        case-studies (or (api/get-case-studies) [])
        export-data {:export-type "full-backup"
                     :exported-at (str (Instant/now))
                     :scan-results {:count (count scan-results) :data scan-results}
                     :decisions {:count (count decisions) :data decisions}
                     :signals {:count (count signals) :data signals}
                     :case-studies {:count (count case-studies) :data case-studies}}]
    (try
      (spit path (if pretty
                   (json/write-str export-data :indent true)
                   (json/write-str export-data)))
      {:success true :path path 
       :counts {:scan-results (count scan-results)
                :decisions (count decisions)
                :signals (count signals)
                :case-studies (count case-studies)}}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

;; =============================================================================
;; PDF Export (Text-based for simplicity)
;; =============================================================================

(defn generate-pdf-content
  "Generate text content for PDF-style report"
  [title sections]
  (let [sb (StringBuilder.)]
    (.append sb (str "=" (apply str (repeat 60 "=")) "\n"))
    (.append sb (str title "\n"))
    (.append sb (str "Generated: " (str (LocalDateTime/now)) "\n"))
    (.append sb (str "=" (apply str (repeat 60 "=")) "\n\n"))
    
    (doseq [{:keys [heading content]} sections]
      (.append sb (str heading "\n"))
      (.append sb (str (apply str (repeat (count heading) "-")) "\n"))
      (.append sb (str content "\n\n")))
    
    (.toString sb)))

(defn export-analysis-report!
  "Export an analysis report (text format, can be converted to PDF)"
  [file-path models & {:keys [include-quotes include-explanations] :or {include-quotes true include-explanations true}}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "analysis_report" "txt")))
        lollapalooza? (>= (count (filter #(>= (or (:confidence %) 0) 0.7) models)) 3)
        sections [{:heading "Summary"
                   :content (str "Total models detected: " (count models) "\n"
                                 "High confidence (>70%): " (count (filter #(>= (or (:confidence %) 0) 0.7) models)) "\n"
                                 "Lollapalooza effect: " (if lollapalooza? "YES - Multiple converging biases detected!" "No"))}
                  {:heading "Detected Mental Models"
                   :content (str/join "\n\n" 
                                      (for [model models]
                                        (str "* " (:name model) " (" (:category model) ")\n"
                                             "  Confidence: " (int (* 100 (or (:confidence model) 0))) "%\n"
                                             (when include-explanations
                                               (str "  Explanation: " (:explanation model) "\n"))
                                             (when (and include-quotes (:quote model))
                                               (str "  Quote: \"" (:quote model) "\"\n")))))}
                  (when lollapalooza?
                    {:heading "Lollapalooza Warning"
                     :content "CAUTION: Multiple mental models are converging in this analysis.\nThis combination of biases can lead to extreme outcomes.\nRecommendation: Seek diverse perspectives before making decisions."})]]
    (try
      (spit path (generate-pdf-content "Mental Models Analysis Report" (remove nil? sections)))
      {:success true :path path}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn export-decision-report!
  "Export a decision journal report"
  [& {:keys [file-path]}]
  (ensure-export-dir!)
  (let [path (or file-path (str export-dir "/" (generate-export-filename "decision_report" "txt")))
        decisions (or (api/get-decisions) [])
        correct-decisions (filter #(= (:outcome %) "correct") decisions)
        incorrect-decisions (filter #(= (:outcome %) "incorrect") decisions)
        pending-decisions (filter #(nil? (:outcome %)) decisions)
        accuracy (if (pos? (+ (count correct-decisions) (count incorrect-decisions)))
                   (/ (count correct-decisions) (+ (count correct-decisions) (count incorrect-decisions)))
                   0)
        sections [{:heading "Decision Journal Summary"
                   :content (str "Total decisions: " (count decisions) "\n"
                                 "Correct outcomes: " (count correct-decisions) "\n"
                                 "Incorrect outcomes: " (count incorrect-decisions) "\n"
                                 "Pending review: " (count pending-decisions) "\n"
                                 "Accuracy rate: " (format "%.1f%%" (* 100 (double accuracy))))}
                  {:heading "Recent Decisions"
                   :content (str/join "\n\n"
                                      (for [decision (take 10 decisions)]
                                        (str "Decision: " (:title decision) "\n"
                                             "Date: " (:decision-date decision) "\n"
                                             "Outcome: " (or (:outcome decision) "Pending") "\n"
                                             "Models used: " (str/join ", " (map :name (:models decision))))))}]]
    (try
      (spit path (generate-pdf-content "Decision Journal Report" sections))
      {:success true :path path :count (count decisions)}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

;; =============================================================================
;; Import Functions
;; =============================================================================

(defn import-scan-results-json!
  "Import scan results from JSON file"
  [file-path]
  (try
    (let [data (json/read-str (slurp file-path) :key-fn keyword)
          results (:data data)]
      (doseq [result results]
        (db/save-analysis! result))
      {:success true :count (count results)})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn import-decisions-json!
  "Import decisions from JSON file"
  [file-path]
  (try
    (let [data (json/read-str (slurp file-path) :key-fn keyword)
          decisions (:data data)]
      (when (api/is-online?)
        (doseq [decision decisions]
          (api/create-decision decision)))
      {:success true :count (count decisions)})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn import-from-csv!
  "Import data from CSV file (generic)"
  [file-path import-fn]
  (try
    (with-open [reader (io/reader file-path)]
      (let [lines (line-seq reader)
            header (first lines)
            data-lines (rest lines)
            headers (str/split header #",")
            records (for [line data-lines]
                      (zipmap (map keyword headers) (str/split line #",")))]
        (doseq [record records]
          (import-fn record))
        {:success true :count (count records)}))
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Batch Export
;; =============================================================================

(defn batch-export-all!
  "Export all data in multiple formats"
  []
  (ensure-export-dir!)
  (let [timestamp (-> (LocalDateTime/now)
                      (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss")))
        batch-dir (str export-dir "/batch_" timestamp)]
    (.mkdirs (io/file batch-dir))
    
    {:scan-results-csv (export-scan-results-csv! :file-path (str batch-dir "/scan_results.csv"))
     :scan-results-json (export-scan-results-json! :file-path (str batch-dir "/scan_results.json"))
     :decisions-csv (export-decisions-csv! :file-path (str batch-dir "/decisions.csv"))
     :decisions-json (export-decisions-json! :file-path (str batch-dir "/decisions.json"))
     :signals-csv (export-signals-csv! :file-path (str batch-dir "/signals.csv"))
     :all-data-json (export-all-data-json! :file-path (str batch-dir "/all_data.json"))
     :batch-dir batch-dir}))

;; =============================================================================
;; Scheduled Exports
;; =============================================================================

(def scheduled-exports (atom []))

(defn add-scheduled-export!
  "Add a scheduled export job"
  [schedule export-type file-path]
  (let [job {:id (str (java.util.UUID/randomUUID))
             :schedule schedule
             :export-type export-type
             :file-path file-path
             :created-at (str (Instant/now))
             :last-run nil
             :enabled true}]
    (swap! scheduled-exports conj job)
    job))

(defn remove-scheduled-export!
  "Remove a scheduled export job"
  [job-id]
  (swap! scheduled-exports #(vec (remove (fn [j] (= (:id j) job-id)) %))))

(defn run-scheduled-export!
  "Run a scheduled export"
  [job]
  (case (:export-type job)
    :scan-results-csv (export-scan-results-csv! :file-path (:file-path job))
    :scan-results-json (export-scan-results-json! :file-path (:file-path job))
    :decisions-csv (export-decisions-csv! :file-path (:file-path job))
    :decisions-json (export-decisions-json! :file-path (:file-path job))
    :all-data (export-all-data-json! :file-path (:file-path job))
    nil))

;; =============================================================================
;; Cloud Storage Export (Placeholder)
;; =============================================================================

(defn export-to-cloud!
  "Export to cloud storage (placeholder for future implementation)"
  [export-type cloud-provider]
  {:success false :error "Cloud export not yet implemented"})

;; =============================================================================
;; Share via Link (Placeholder)
;; =============================================================================

(defn create-share-link!
  "Create a shareable link for exported data (placeholder)"
  [file-path]
  {:success false :error "Share links not yet implemented"})
