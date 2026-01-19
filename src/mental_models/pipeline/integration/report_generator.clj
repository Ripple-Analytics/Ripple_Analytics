(ns mental-models.pipeline.integration.report-generator
  "Report Generator Module
   
   Generates comprehensive analysis reports:
   - Executive summaries
   - Detailed model breakdowns
   - Trend analysis reports
   - Lollapalooza alerts
   - Export to multiple formats"
  (:require
   [mental-models.pipeline.integration.history-tracker :as history]
   [mental-models.pipeline.integration.result-formatter :as formatter]
   [mental-models.registry.models :as registry]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; =============================================================================
;; REPORT TYPES
;; =============================================================================

(def report-types
  #{:executive-summary
    :detailed-analysis
    :trend-report
    :lollapalooza-report
    :model-breakdown
    :daily-digest
    :weekly-summary})

;; =============================================================================
;; EXECUTIVE SUMMARY
;; =============================================================================

(defn generate-executive-summary
  "Generate an executive summary report."
  [& {:keys [days] :or {days 7}}]
  (log/info "Generating executive summary" {:days days})
  (let [stats (history/get-summary-stats)
        trends (history/get-model-trends :days days)
        lollapalooza-trends (history/get-lollapalooza-trends :days days)
        confidence-trends (history/get-confidence-trends :days days)]
    {:report-type :executive-summary
     :generated-at (System/currentTimeMillis)
     :period-days days
     :summary {:total-analyses (:total-analyses stats)
               :total-lollapaloozas (:total-lollapaloozas stats)
               :lollapalooza-rate (:lollapalooza-rate stats)
               :unique-models (:unique-models stats)}
     :trends {:top-models (:top-models trends)
              :lollapalooza-frequency (:avg-per-day lollapalooza-trends)
              :avg-confidence (:avg-confidence confidence-trends)}
     :highlights (generate-highlights stats trends lollapalooza-trends)}))

(defn generate-highlights [stats trends lollapalooza-trends]
  (let [highlights []]
    (cond-> highlights
      (> (:lollapalooza-rate stats) 0.1)
      (conj {:type :warning
             :message (str "High Lollapalooza rate: " (format "%.1f%%" (* 100 (:lollapalooza-rate stats))))})
      
      (seq (:top-models trends))
      (conj {:type :info
             :message (str "Most detected model: " (name (first (first (:top-models trends)))))})
      
      (> (:avg-per-day lollapalooza-trends 0) 1)
      (conj {:type :alert
             :message (str "Averaging " (format "%.1f" (:avg-per-day lollapalooza-trends)) " Lollapaloozas per day")}))))

;; =============================================================================
;; DETAILED ANALYSIS REPORT
;; =============================================================================

(defn generate-detailed-report
  "Generate a detailed analysis report for specific results."
  [results]
  (log/info "Generating detailed report" {:result-count (count results)})
  {:report-type :detailed-analysis
   :generated-at (System/currentTimeMillis)
   :result-count (count results)
   :results (map (fn [result]
                   {:file-path (:file-path result)
                    :timestamp (:timestamp result)
                    :detections (map (fn [d]
                                       {:model-id (:model-id d)
                                        :model-name (:model-name d)
                                        :confidence (:confidence d)
                                        :description (registry/get-model-description (:model-id d))})
                                     (:detections result))
                    :lollapalooza (:lollapalooza result)})
                 results)
   :aggregate {:total-detections (reduce + (map #(count (:detections %)) results))
               :unique-models (count (distinct (mapcat #(map :model-id (:detections %)) results)))
               :lollapalooza-count (count (filter :lollapalooza results))}})

;; =============================================================================
;; TREND REPORT
;; =============================================================================

(defn generate-trend-report
  "Generate a trend analysis report."
  [& {:keys [days] :or {days 30}}]
  (log/info "Generating trend report" {:days days})
  (let [model-trends (history/get-model-trends :days days)
        lollapalooza-trends (history/get-lollapalooza-trends :days days)
        confidence-trends (history/get-confidence-trends :days days)
        daily-stats (history/get-recent-daily-stats :days days)]
    {:report-type :trend-report
     :generated-at (System/currentTimeMillis)
     :period-days days
     :model-trends {:total-analyses (:total-analyses model-trends)
                    :model-distribution (:model-counts model-trends)
                    :top-10-models (:top-models model-trends)}
     :lollapalooza-trends {:total (:total-lollapaloozas lollapalooza-trends)
                           :daily-average (:avg-per-day lollapalooza-trends)
                           :common-combinations (:common-model-combinations lollapalooza-trends)}
     :confidence-trends confidence-trends
     :daily-breakdown daily-stats}))

;; =============================================================================
;; LOLLAPALOOZA REPORT
;; =============================================================================

(defn generate-lollapalooza-report
  "Generate a report focused on Lollapalooza detections."
  [& {:keys [limit] :or {limit 50}}]
  (log/info "Generating Lollapalooza report" {:limit limit})
  (let [lollapaloozas (history/get-analyses-with-lollapalooza :limit limit)
        trends (history/get-lollapalooza-trends)]
    {:report-type :lollapalooza-report
     :generated-at (System/currentTimeMillis)
     :total-lollapaloozas (count lollapaloozas)
     :trends trends
     :recent-lollapaloozas (map (fn [analysis]
                                  {:timestamp (:timestamp analysis)
                                   :file-path (:file-path analysis)
                                   :models (map :model-name (:detections analysis))
                                   :confidence-avg (:confidence-avg analysis)})
                                lollapaloozas)
     :model-co-occurrence (frequencies
                           (mapcat #(map :model-id (:detections %)) lollapaloozas))}))

;; =============================================================================
;; MODEL BREAKDOWN REPORT
;; =============================================================================

(defn generate-model-breakdown
  "Generate a breakdown report for a specific model."
  [model-id & {:keys [limit] :or {limit 100}}]
  (log/info "Generating model breakdown" {:model-id model-id})
  (let [model-info (registry/get-model model-id)
        analyses (history/get-analyses-by-model model-id :limit limit)
        frequencies (:model-frequencies @history/history-state)]
    {:report-type :model-breakdown
     :generated-at (System/currentTimeMillis)
     :model {:id model-id
             :name (:name model-info)
             :description (:description model-info)
             :category (:category model-info)
             :tags (:tags model-info)}
     :statistics {:total-detections (get frequencies model-id 0)
                  :recent-analyses (count analyses)
                  :avg-confidence (when (seq analyses)
                                    (/ (reduce + (map :confidence-avg analyses))
                                       (count analyses)))}
     :recent-detections (take 20 analyses)}))

;; =============================================================================
;; DAILY DIGEST
;; =============================================================================

(defn generate-daily-digest
  "Generate a daily digest report."
  []
  (log/info "Generating daily digest")
  (let [today (quot (System/currentTimeMillis) 86400000)
        today-stats (history/get-daily-summary today)
        recent (history/get-recent-analyses :limit 50)
        today-analyses (filter #(= (quot (:timestamp %) 86400000) today) recent)]
    {:report-type :daily-digest
     :generated-at (System/currentTimeMillis)
     :date (java.util.Date.)
     :summary today-stats
     :analyses-today (count today-analyses)
     :top-models-today (take 5 (sort-by val >
                                        (frequencies
                                         (mapcat #(map :model-id (:detections %)) today-analyses))))
     :lollapaloozas-today (count (filter :lollapalooza today-analyses))}))

;; =============================================================================
;; REPORT FORMATTING
;; =============================================================================

(defn format-report
  "Format a report for output."
  [report format]
  (case format
    :json (formatter/format-json report)
    :markdown (format-report-markdown report)
    :html (format-report-html report)
    :text (format-report-text report)
    (formatter/format-json report)))

(defn format-report-markdown [report]
  (str "# " (name (:report-type report)) "\n\n"
       "Generated: " (java.util.Date. (:generated-at report)) "\n\n"
       "## Summary\n\n"
       (str/join "\n" (map (fn [[k v]] (str "- **" (name k) "**: " v))
                           (dissoc report :report-type :generated-at :results :recent-detections)))))

(defn format-report-html [report]
  (str "<html><head><title>" (name (:report-type report)) "</title></head>"
       "<body><h1>" (name (:report-type report)) "</h1>"
       "<p>Generated: " (java.util.Date. (:generated-at report)) "</p>"
       "<pre>" (formatter/format-json report) "</pre>"
       "</body></html>"))

(defn format-report-text [report]
  (str (str/upper-case (name (:report-type report))) "\n"
       (str/join "" (repeat 60 "=")) "\n"
       "Generated: " (java.util.Date. (:generated-at report)) "\n\n"
       (formatter/format-json report :pretty true)))

;; =============================================================================
;; REPORT EXPORT
;; =============================================================================

(defn export-report!
  "Export a report to a file."
  [report file-path & {:keys [format] :or {format :json}}]
  (log/info "Exporting report" {:path file-path :format format})
  (io/make-parents file-path)
  (spit file-path (format-report report format))
  (audit/log-operation! {:operation :report-exported
                         :report-type (:report-type report)
                         :file-path file-path
                         :format format})
  (log/info "Report exported" {:path file-path}))

;; =============================================================================
;; SCHEDULED REPORTS
;; =============================================================================

(defn schedule-daily-digest!
  "Schedule daily digest generation."
  [output-dir]
  (log/info "Scheduling daily digest" {:output-dir output-dir})
  (events/subscribe! :scheduler/daily
                     (fn [_]
                       (let [report (generate-daily-digest)
                             file-path (str output-dir "/daily-digest-"
                                            (.format (java.text.SimpleDateFormat. "yyyy-MM-dd")
                                                     (java.util.Date.))
                                            ".json")]
                         (export-report! report file-path)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-report-generator!
  "Initialize report generator."
  []
  (log/info "Initializing report generator")
  ;; Register feature flag
  (flags/register-flag! "report-generation" "Enable report generation" true)
  ;; Create metrics
  (metrics/create-counter! :reports/generated "Reports generated")
  (metrics/create-counter! :reports/exported "Reports exported")
  (log/info "Report generator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-generator-status []
  {:enabled (flags/is-enabled? "report-generation")
   :available-report-types report-types})
