(ns mental-models.pipeline.integration.report-builder
  "Advanced report building for mental model analysis results.
   
   Features:
   - Multiple report formats (PDF, HTML, Markdown, JSON)
   - Customizable report templates
   - Chart and visualization generation
   - Executive summary generation
   - Trend analysis reports
   - Comparative analysis reports
   - Scheduled report generation
   - Report distribution"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:templates {}        ;; template-id -> template
         :reports {}          ;; report-id -> report
         :schedules {}        ;; schedule-id -> schedule
         :distributions {}    ;; distribution-id -> distribution-config
         :initialized? false}))

;; ============================================================================
;; Template Management
;; ============================================================================

(defn register-template!
  "Register a report template."
  [template-id config]
  (let [template {:id template-id
                  :name (get config :name (name template-id))
                  :description (get config :description "")
                  :format (get config :format :markdown)
                  :sections (get config :sections [])
                  :styles (get config :styles {})
                  :variables (get config :variables [])
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:templates template-id] template)
    (logging/log :info "Registered report template" {:template-id template-id})
    (events/emit! :template-registered {:template-id template-id})
    template-id))

(defn get-template
  "Get a report template."
  [template-id]
  (get-in @state [:templates template-id]))

(defn list-templates
  "List all report templates."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :format (:format t)
           :sections (count (:sections t))})
        (:templates @state)))

;; ============================================================================
;; Section Renderers
;; ============================================================================

(defn- format-date
  "Format a timestamp."
  [timestamp & {:keys [pattern] :or {pattern "yyyy-MM-dd HH:mm:ss"}}]
  (let [instant (Instant/ofEpochMilli timestamp)
        zoned (.atZone instant (ZoneId/systemDefault))
        formatter (DateTimeFormatter/ofPattern pattern)]
    (.format zoned formatter)))

(defn- render-header
  "Render report header section."
  [data format]
  (let [title (get data :title "Mental Model Analysis Report")
        subtitle (get data :subtitle "")
        date (format-date (System/currentTimeMillis))]
    (case format
      :markdown (str "# " title "\n\n"
                     (when (not (str/blank? subtitle))
                       (str "*" subtitle "*\n\n"))
                     "**Generated:** " date "\n\n---\n\n")
      :html (str "<header>\n"
                 "  <h1>" title "</h1>\n"
                 (when (not (str/blank? subtitle))
                   (str "  <p class=\"subtitle\">" subtitle "</p>\n"))
                 "  <p class=\"date\">Generated: " date "</p>\n"
                 "</header>\n\n")
      :json {:title title :subtitle subtitle :generated-at date}
      (str title "\n" (apply str (repeat (count title) "=")) "\n\n"))))

(defn- render-executive-summary
  "Render executive summary section."
  [data format]
  (let [summary (get data :executive-summary "")
        key-findings (get data :key-findings [])
        recommendations (get data :recommendations [])]
    (case format
      :markdown (str "## Executive Summary\n\n"
                     summary "\n\n"
                     (when (seq key-findings)
                       (str "### Key Findings\n\n"
                            (str/join "\n" (map #(str "- " %) key-findings))
                            "\n\n"))
                     (when (seq recommendations)
                       (str "### Recommendations\n\n"
                            (str/join "\n" (map #(str "- " %) recommendations))
                            "\n\n")))
      :html (str "<section class=\"executive-summary\">\n"
                 "  <h2>Executive Summary</h2>\n"
                 "  <p>" summary "</p>\n"
                 (when (seq key-findings)
                   (str "  <h3>Key Findings</h3>\n"
                        "  <ul>\n"
                        (str/join "\n" (map #(str "    <li>" % "</li>") key-findings))
                        "\n  </ul>\n"))
                 (when (seq recommendations)
                   (str "  <h3>Recommendations</h3>\n"
                        "  <ul>\n"
                        (str/join "\n" (map #(str "    <li>" % "</li>") recommendations))
                        "\n  </ul>\n"))
                 "</section>\n\n")
      :json {:summary summary
             :key-findings key-findings
             :recommendations recommendations}
      (str "EXECUTIVE SUMMARY\n\n" summary "\n\n"))))

(defn- render-model-analysis
  "Render mental model analysis section."
  [data format]
  (let [models (get data :models [])
        total (count models)
        by-category (group-by :category models)]
    (case format
      :markdown (str "## Mental Model Analysis\n\n"
                     "**Total Models Detected:** " total "\n\n"
                     (str/join "\n\n"
                               (for [[category ms] by-category]
                                 (str "### " (name (or category :uncategorized)) "\n\n"
                                      (str/join "\n"
                                                (for [m ms]
                                                  (str "- **" (:name m) "** (Confidence: "
                                                       (format "%.1f%%" (* 100 (:confidence m 0)))
                                                       ")")))))))
      :html (str "<section class=\"model-analysis\">\n"
                 "  <h2>Mental Model Analysis</h2>\n"
                 "  <p><strong>Total Models Detected:</strong> " total "</p>\n"
                 (str/join "\n"
                           (for [[category ms] by-category]
                             (str "  <div class=\"category\">\n"
                                  "    <h3>" (name (or category :uncategorized)) "</h3>\n"
                                  "    <ul>\n"
                                  (str/join "\n"
                                            (for [m ms]
                                              (str "      <li><strong>" (:name m) "</strong> - "
                                                   (format "%.1f%%" (* 100 (:confidence m 0)))
                                                   "</li>")))
                                  "\n    </ul>\n"
                                  "  </div>\n")))
                 "</section>\n\n")
      :json {:total-models total
             :by-category (into {} (map (fn [[k v]] [k (count v)]) by-category))
             :models models}
      (str "MENTAL MODEL ANALYSIS\n\nTotal: " total "\n\n"))))

(defn- render-statistics
  "Render statistics section."
  [data format]
  (let [stats (get data :statistics {})]
    (case format
      :markdown (str "## Statistics\n\n"
                     "| Metric | Value |\n"
                     "|--------|-------|\n"
                     (str/join "\n"
                               (for [[k v] stats]
                                 (str "| " (name k) " | " v " |")))
                     "\n\n")
      :html (str "<section class=\"statistics\">\n"
                 "  <h2>Statistics</h2>\n"
                 "  <table>\n"
                 "    <tr><th>Metric</th><th>Value</th></tr>\n"
                 (str/join "\n"
                           (for [[k v] stats]
                             (str "    <tr><td>" (name k) "</td><td>" v "</td></tr>")))
                 "\n  </table>\n"
                 "</section>\n\n")
      :json {:statistics stats}
      (str "STATISTICS\n\n"
           (str/join "\n" (for [[k v] stats] (str (name k) ": " v)))
           "\n\n"))))

(defn- render-trends
  "Render trends section."
  [data format]
  (let [trends (get data :trends [])
        period (get data :trend-period "last 30 days")]
    (case format
      :markdown (str "## Trends (" period ")\n\n"
                     (str/join "\n"
                               (for [t trends]
                                 (str "- **" (:name t) "**: "
                                      (:direction t) " "
                                      (format "%.1f%%" (* 100 (Math/abs (:change t 0))))))))
      :html (str "<section class=\"trends\">\n"
                 "  <h2>Trends (" period ")</h2>\n"
                 "  <ul>\n"
                 (str/join "\n"
                           (for [t trends]
                             (str "    <li class=\"trend-" (name (:direction t)) "\">"
                                  "<strong>" (:name t) "</strong>: "
                                  (:direction t) " "
                                  (format "%.1f%%" (* 100 (Math/abs (:change t 0))))
                                  "</li>")))
                 "\n  </ul>\n"
                 "</section>\n\n")
      :json {:period period :trends trends}
      (str "TRENDS (" period ")\n\n"))))

(defn- render-chart-placeholder
  "Render chart placeholder."
  [data format]
  (let [chart-type (get data :chart-type :bar)
        chart-title (get data :chart-title "Chart")]
    (case format
      :markdown (str "### " chart-title "\n\n"
                     "*[" (name chart-type) " chart would be rendered here]*\n\n")
      :html (str "<div class=\"chart\" data-type=\"" (name chart-type) "\">\n"
                 "  <h3>" chart-title "</h3>\n"
                 "  <div class=\"chart-placeholder\">[Chart: " (name chart-type) "]</div>\n"
                 "</div>\n\n")
      :json {:chart-type chart-type :title chart-title :data (get data :chart-data [])}
      (str "[CHART: " chart-title "]\n\n"))))

(defn- render-footer
  "Render report footer."
  [data format]
  (let [disclaimer (get data :disclaimer "This report is generated automatically.")
        contact (get data :contact "")]
    (case format
      :markdown (str "---\n\n"
                     "*" disclaimer "*\n\n"
                     (when (not (str/blank? contact))
                       (str "Contact: " contact "\n")))
      :html (str "<footer>\n"
                 "  <p class=\"disclaimer\">" disclaimer "</p>\n"
                 (when (not (str/blank? contact))
                   (str "  <p class=\"contact\">Contact: " contact "</p>\n"))
                 "</footer>\n")
      :json {:disclaimer disclaimer :contact contact}
      (str "\n" disclaimer "\n"))))

;; ============================================================================
;; Report Building
;; ============================================================================

(defn- render-section
  "Render a single section."
  [section-type data format]
  (case section-type
    :header (render-header data format)
    :executive-summary (render-executive-summary data format)
    :model-analysis (render-model-analysis data format)
    :statistics (render-statistics data format)
    :trends (render-trends data format)
    :chart (render-chart-placeholder data format)
    :footer (render-footer data format)
    ""))

(defn build-report
  "Build a report from template and data."
  [template-id data & {:keys [format]}]
  (when (flags/enabled? :report-builder)
    (let [template (get-template template-id)
          report-format (or format (:format template) :markdown)
          sections (:sections template)
          report-id (str (UUID/randomUUID))
          start-time (System/currentTimeMillis)
          
          ;; Render all sections
          content (if (= report-format :json)
                    ;; For JSON, collect all sections into a map
                    (reduce (fn [acc section]
                              (merge acc (render-section section data report-format)))
                            {:report-id report-id
                             :template-id template-id
                             :generated-at (System/currentTimeMillis)}
                            sections)
                    ;; For other formats, concatenate strings
                    (str/join ""
                              (for [section sections]
                                (render-section section data report-format))))
          
          processing-time (- (System/currentTimeMillis) start-time)
          
          report {:id report-id
                  :template-id template-id
                  :format report-format
                  :content content
                  :data data
                  :created-at (System/currentTimeMillis)
                  :processing-time-ms processing-time}]
      
      (swap! state assoc-in [:reports report-id] report)
      (metrics/increment :reports-generated {:template-id template-id})
      (logging/log :info "Built report" {:report-id report-id :template-id template-id})
      (events/emit! :report-built {:report-id report-id})
      
      report)))

(defn get-report
  "Get a generated report."
  [report-id]
  (get-in @state [:reports report-id]))

(defn list-reports
  "List generated reports."
  [& {:keys [template-id since limit]}]
  (let [reports (vals (:reports @state))
        filtered (cond->> reports
                   template-id (filter #(= (:template-id %) template-id))
                   since (filter #(>= (:created-at %) since))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :template-id :format :created-at]) filtered)))

(defn export-report
  "Export a report to a file."
  [report-id output-path]
  (when-let [report (get-report report-id)]
    (let [content (if (= (:format report) :json)
                    (pr-str (:content report))
                    (:content report))]
      (spit output-path content)
      (logging/log :info "Exported report" {:report-id report-id :path output-path})
      output-path)))

;; ============================================================================
;; Scheduled Reports
;; ============================================================================

(defn schedule-report!
  "Schedule a recurring report."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :template-id (get config :template-id)
                  :data-source (get config :data-source)
                  :cron (get config :cron "0 0 * * *") ;; Daily at midnight
                  :distribution-ids (get config :distribution-ids [])
                  :enabled? (get config :enabled? true)
                  :last-run nil
                  :next-run nil
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schedules schedule-id] schedule)
    (logging/log :info "Scheduled report" {:schedule-id schedule-id})
    schedule-id))

(defn run-scheduled-report!
  "Run a scheduled report manually."
  [schedule-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (let [data-fn (:data-source schedule)
          data (if (fn? data-fn) (data-fn) {})
          report (build-report (:template-id schedule) data)]
      (swap! state assoc-in [:schedules schedule-id :last-run] (System/currentTimeMillis))
      (logging/log :info "Ran scheduled report" {:schedule-id schedule-id :report-id (:id report)})
      report)))

(defn list-schedules
  "List all report schedules."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :template-id (:template-id s)
           :cron (:cron s)
           :enabled? (:enabled? s)
           :last-run (:last-run s)})
        (:schedules @state)))

;; ============================================================================
;; Distribution
;; ============================================================================

(defn register-distribution!
  "Register a report distribution channel."
  [distribution-id config]
  (let [distribution {:id distribution-id
                      :name (get config :name (name distribution-id))
                      :type (get config :type :email)
                      :recipients (get config :recipients [])
                      :options (get config :options {})
                      :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:distributions distribution-id] distribution)
    (logging/log :info "Registered distribution" {:distribution-id distribution-id})
    distribution-id))

(defn distribute-report!
  "Distribute a report through configured channels."
  [report-id distribution-ids]
  (let [report (get-report report-id)]
    (doseq [dist-id distribution-ids]
      (when-let [dist (get-in @state [:distributions dist-id])]
        (logging/log :info "Distributing report"
                     {:report-id report-id
                      :distribution-id dist-id
                      :type (:type dist)
                      :recipients (count (:recipients dist))})
        (events/emit! :report-distributed
                      {:report-id report-id
                       :distribution-id dist-id
                       :recipients (:recipients dist)})))
    true))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-builder-stats
  "Get report builder statistics."
  []
  {:total-templates (count (:templates @state))
   :total-reports (count (:reports @state))
   :total-schedules (count (:schedules @state))
   :total-distributions (count (:distributions @state))
   :reports-by-template (frequencies (map :template-id (vals (:reports @state))))
   :reports-by-format (frequencies (map :format (vals (:reports @state))))})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-report-builder!
  "Initialize the report builder."
  []
  (when-not (:initialized? @state)
    ;; Register default templates
    (register-template! :executive-summary
                        {:name "Executive Summary Report"
                         :format :markdown
                         :sections [:header :executive-summary :statistics :footer]})
    
    (register-template! :full-analysis
                        {:name "Full Analysis Report"
                         :format :markdown
                         :sections [:header :executive-summary :model-analysis
                                    :statistics :trends :footer]})
    
    (register-template! :trend-report
                        {:name "Trend Analysis Report"
                         :format :markdown
                         :sections [:header :trends :chart :statistics :footer]})
    
    (register-template! :json-export
                        {:name "JSON Data Export"
                         :format :json
                         :sections [:header :model-analysis :statistics :trends]})
    
    ;; Register default distribution
    (register-distribution! :email-team
                            {:name "Team Email Distribution"
                             :type :email
                             :recipients []
                             :options {:subject-prefix "[Mental Models Report]"}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Report builder initialized")
    (events/emit! :report-builder-initialized {})
    true))
