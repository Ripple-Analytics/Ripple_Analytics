(ns mental-models.ui
  "Reactive UI Components - Electric Clojure
   
   Beautiful, reactive UI components for the Mental Models System.
   These components automatically update when data changes.
   
   Design Philosophy: Steve Jobs-level attention to detail
   - Clean, minimal interfaces
   - Information density like Value Line
   - Instant feedback
   - Delightful interactions"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [mental-models.models :as models]
            [mental-models.analysis :as analysis]
            [mental-models.statistics :as stats]
            [mental-models.data-processing :as data]
            [clojure.string :as str]))

;; ============================================
;; Reactive State
;; ============================================

(e/def !selected-model (atom nil))
(e/def !analysis-context (atom ""))
(e/def !selected-models (atom #{}))
(e/def !active-tab (atom :dashboard))
(e/def !search-query (atom ""))

;; ============================================
;; Style Constants (Value Line density)
;; ============================================

(def styles
  {:container "font-sans text-xs leading-tight bg-white"
   :header "bg-gray-900 text-white px-2 py-1 font-bold text-sm"
   :card "border border-gray-300 bg-white shadow-sm"
   :card-header "bg-gray-100 px-2 py-1 font-semibold border-b text-xs"
   :card-body "p-2"
   :table "w-full border-collapse text-xs"
   :th "bg-gray-200 px-1 py-0.5 text-left font-semibold border border-gray-300"
   :td "px-1 py-0.5 border border-gray-300"
   :input "border border-gray-400 px-1 py-0.5 text-xs w-full"
   :button "bg-blue-600 text-white px-2 py-0.5 text-xs font-semibold hover:bg-blue-700"
   :button-secondary "bg-gray-200 text-gray-800 px-2 py-0.5 text-xs hover:bg-gray-300"
   :badge "inline-block px-1 py-0.5 text-xs rounded"
   :badge-high "bg-red-100 text-red-800"
   :badge-medium "bg-yellow-100 text-yellow-800"
   :badge-low "bg-green-100 text-green-800"
   :tab "px-3 py-1 text-xs cursor-pointer"
   :tab-active "bg-white border-b-2 border-blue-600 font-semibold"
   :tab-inactive "bg-gray-100 text-gray-600 hover:bg-gray-200"
   :metric "text-center"
   :metric-value "text-lg font-bold text-blue-600"
   :metric-label "text-xs text-gray-500"
   :sparkline "h-8 w-24"})

;; ============================================
;; Utility Components
;; ============================================

(e/defn Badge [severity text]
  (e/client
   (dom/span
    (dom/props {:class (str (:badge styles) " "
                            (case severity
                              "high" (:badge-high styles)
                              "critical" (:badge-high styles)
                              "medium" (:badge-medium styles)
                              (:badge-low styles)))})
    (dom/text text))))

(e/defn MetricCard [label value & [unit]]
  (e/client
   (dom/div
    (dom/props {:class (:metric styles)})
    (dom/div
     (dom/props {:class (:metric-value styles)})
     (dom/text (str value (when unit (str " " unit)))))
    (dom/div
     (dom/props {:class (:metric-label styles)})
     (dom/text label)))))

(e/defn ProgressBar [value max-val & [color]]
  (e/client
   (let [pct (* 100 (/ value (max 1 max-val)))
         bar-color (or color "bg-blue-600")]
     (dom/div
      (dom/props {:class "w-full bg-gray-200 h-2 rounded"})
      (dom/div
       (dom/props {:class (str bar-color " h-2 rounded")
                   :style (str "width: " pct "%")}))))))

;; ============================================
;; Model Components
;; ============================================

(e/defn ModelRow [model]
  (e/client
   (let [name (:name model)
         category (:category model)
         failure-count (count (:failure-modes model))]
     (dom/tr
      (dom/props {:class "hover:bg-blue-50 cursor-pointer"
                  :onclick (fn [_] (reset! !selected-model name))})
      (dom/td (dom/props {:class (:td styles)}) (dom/text name))
      (dom/td (dom/props {:class (:td styles)}) (dom/text category))
      (dom/td (dom/props {:class (:td styles)}) (dom/text (:originator model)))
      (dom/td (dom/props {:class (:td styles)}) (dom/text failure-count))
      (dom/td (dom/props {:class (:td styles)})
              (Badge. (if (>= failure-count 5) "high" "medium")
                      (str failure-count " modes")))))))

(e/defn ModelTable []
  (e/client
   (let [query @!search-query
         all-models (models/get-all-models)
         filtered (if (empty? query)
                    all-models
                    (models/search-models query))]
     (dom/div
      (dom/props {:class (:card styles)})
      (dom/div
       (dom/props {:class (:card-header styles)})
       (dom/text (str "Mental Models (" (count filtered) ")")))
      (dom/div
       (dom/props {:class (:card-body styles)})
       (dom/input
        (dom/props {:class (:input styles)
                    :placeholder "Search models..."
                    :value query})
        (dom/on "input" (fn [e] (reset! !search-query (.. e -target -value)))))
       (dom/table
        (dom/props {:class (:table styles)})
        (dom/thead
         (dom/tr
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Name"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Category"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Originator"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Failures"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Status"))))
        (dom/tbody
         (e/for [model filtered]
           (ModelRow. model)))))))))

(e/defn ModelDetail []
  (e/client
   (let [name @!selected-model
         model (when name (models/get-model name))]
     (when model
       (dom/div
        (dom/props {:class (:card styles)})
        (dom/div
         (dom/props {:class (:card-header styles)})
         (dom/text (:name model)))
        (dom/div
         (dom/props {:class (:card-body styles)})
         (dom/p (dom/text (:description model)))
         (dom/p
          (dom/strong (dom/text "Key Insight: "))
          (dom/text (:key-insight model)))
         (dom/p
          (dom/strong (dom/text "Application: "))
          (dom/text (:application model)))
         (dom/h4 (dom/text "Failure Modes:"))
         (dom/table
          (dom/props {:class (:table styles)})
          (dom/thead
           (dom/tr
            (dom/th (dom/props {:class (:th styles)}) (dom/text "Name"))
            (dom/th (dom/props {:class (:th styles)}) (dom/text "Severity"))
            (dom/th (dom/props {:class (:th styles)}) (dom/text "Description"))))
          (dom/tbody
           (e/for [fm (:failure-modes model)]
             (dom/tr
              (dom/td (dom/props {:class (:td styles)}) (dom/text (:name fm)))
              (dom/td (dom/props {:class (:td styles)})
                      (Badge. (:severity fm) (:severity fm)))
              (dom/td (dom/props {:class (:td styles)}) (dom/text (:description fm)))))))))))))

;; ============================================
;; Analysis Components
;; ============================================

(e/defn AnalysisPanel []
  (e/client
   (let [context @!analysis-context
         selected @!selected-models]
     (dom/div
      (dom/props {:class (:card styles)})
      (dom/div
       (dom/props {:class (:card-header styles)})
       (dom/text "Analysis"))
      (dom/div
       (dom/props {:class (:card-body styles)})
       (dom/textarea
        (dom/props {:class (str (:input styles) " h-24")
                    :placeholder "Enter situation to analyze..."
                    :value context})
        (dom/on "input" (fn [e] (reset! !analysis-context (.. e -target -value)))))
       (dom/div
        (dom/props {:class "mt-2 flex gap-2"})
        (dom/button
         (dom/props {:class (:button styles)})
         (dom/on "click" (fn [_] (println "Latticework analysis")))
         (dom/text "Latticework"))
        (dom/button
         (dom/props {:class (:button styles)})
         (dom/on "click" (fn [_] (println "Lollapalooza detection")))
         (dom/text "Lollapalooza"))
        (dom/button
         (dom/props {:class (:button styles)})
         (dom/on "click" (fn [_] (println "Inversion")))
         (dom/text "Invert"))
        (dom/button
         (dom/props {:class (:button styles)})
         (dom/on "click" (fn [_] (println "Two-track")))
         (dom/text "Two-Track"))))))))

(e/defn BiasDetector []
  (e/client
   (let [context @!analysis-context
         biases (when (not (empty? context))
                  (analysis/detect-biases context))]
     (dom/div
      (dom/props {:class (:card styles)})
      (dom/div
       (dom/props {:class (:card-header styles)})
       (dom/text "Bias Detection"))
      (dom/div
       (dom/props {:class (:card-body styles)})
       (if (and biases (seq (:biases-detected biases)))
         (dom/div
          (dom/p (dom/text (str "Risk Level: " (:risk-level biases))))
          (e/for [bias (:biases-detected biases)]
            (dom/div
             (dom/props {:class "mb-1"})
             (Badge. (:severity bias) (:bias bias))
             (dom/span (dom/text (str " - " (str/join ", " (:triggers bias))))))))
         (dom/p (dom/text "Enter text above to detect biases"))))))))

;; ============================================
;; Statistics Components
;; ============================================

(e/defn StatisticsPanel []
  (e/client
   (dom/div
    (dom/props {:class (:card styles)})
    (dom/div
     (dom/props {:class (:card-header styles)})
     (dom/text "Statistics"))
    (dom/div
     (dom/props {:class (:card-body styles)})
     (dom/p (dom/text "Enter data for statistical analysis"))
     (dom/div
      (dom/props {:class "grid grid-cols-4 gap-2 mt-2"})
      (MetricCard. "Mean" "0.00")
      (MetricCard. "Median" "0.00")
      (MetricCard. "Std Dev" "0.00")
      (MetricCard. "Correlation" "0.00"))))))

;; ============================================
;; Dashboard Components
;; ============================================

(e/defn DashboardMetrics []
  (e/client
   (let [total-models (count (models/get-all-models))
         total-failures (count @models/!failure-modes)
         categories (count (models/get-all-categories))]
     (dom/div
      (dom/props {:class "grid grid-cols-6 gap-2 mb-4"})
      (MetricCard. "Models" total-models)
      (MetricCard. "Failure Modes" total-failures)
      (MetricCard. "Categories" categories)
      (MetricCard. "Avg Failures/Model" (if (> total-models 0)
                                          (format "%.1f" (double (/ total-failures total-models)))
                                          "0"))
      (MetricCard. "Coverage" "100%")
      (MetricCard. "Last Updated" "Now")))))

(e/defn CategoryBreakdown []
  (e/client
   (let [categories @models/!categories]
     (dom/div
      (dom/props {:class (:card styles)})
      (dom/div
       (dom/props {:class (:card-header styles)})
       (dom/text "Categories"))
      (dom/div
       (dom/props {:class (:card-body styles)})
       (dom/table
        (dom/props {:class (:table styles)})
        (dom/thead
         (dom/tr
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Category"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Models"))
          (dom/th (dom/props {:class (:th styles)}) (dom/text "Coverage"))))
        (dom/tbody
         (e/for [[cat model-names] categories]
           (dom/tr
            (dom/td (dom/props {:class (:td styles)}) (dom/text (name cat)))
            (dom/td (dom/props {:class (:td styles)}) (dom/text (count model-names)))
            (dom/td (dom/props {:class (:td styles)})
                    (ProgressBar. (count model-names) 30)))))))))))

;; ============================================
;; Navigation
;; ============================================

(e/defn TabNav []
  (e/client
   (let [active @!active-tab
         tabs [{:id :dashboard :label "Dashboard"}
               {:id :models :label "Models"}
               {:id :analysis :label "Analysis"}
               {:id :statistics :label "Statistics"}
               {:id :data :label "Data"}]]
     (dom/div
      (dom/props {:class "flex border-b border-gray-300 bg-gray-100"})
      (e/for [{:keys [id label]} tabs]
        (dom/div
         (dom/props {:class (str (:tab styles) " "
                                 (if (= active id)
                                   (:tab-active styles)
                                   (:tab-inactive styles)))})
         (dom/on "click" (fn [_] (reset! !active-tab id)))
         (dom/text label)))))))

;; ============================================
;; Main App
;; ============================================

(e/defn Dashboard []
  (e/client
   (dom/div
    (DashboardMetrics.)
    (dom/div
     (dom/props {:class "grid grid-cols-2 gap-4"})
     (CategoryBreakdown.)
     (dom/div
      (AnalysisPanel.)
      (BiasDetector.))))))

(e/defn ModelsView []
  (e/client
   (dom/div
    (dom/props {:class "grid grid-cols-2 gap-4"})
    (ModelTable.)
    (ModelDetail.))))

(e/defn AnalysisView []
  (e/client
   (dom/div
    (AnalysisPanel.)
    (BiasDetector.))))

(e/defn StatisticsView []
  (e/client
   (StatisticsPanel.)))

(e/defn DataView []
  (e/client
   (dom/div
    (dom/props {:class (:card styles)})
    (dom/div
     (dom/props {:class (:card-header styles)})
     (dom/text "Data Processing"))
    (dom/div
     (dom/props {:class (:card-body styles)})
     (dom/textarea
      (dom/props {:class (str (:input styles) " h-32")
                  :placeholder "Paste text to analyze..."}))
     (dom/div
      (dom/props {:class "mt-2 flex gap-2"})
      (dom/button
       (dom/props {:class (:button styles)})
       (dom/text "Analyze Document"))
      (dom/button
       (dom/props {:class (:button styles)})
       (dom/text "Extract Entities"))
      (dom/button
       (dom/props {:class (:button styles)})
       (dom/text "Classify by Models")))))))

(e/defn App []
  (e/client
   (dom/div
    (dom/props {:class (:container styles)})
    ;; Header
    (dom/div
     (dom/props {:class (:header styles)})
     (dom/text "Mental Models System - Electric Clojure"))
    ;; Navigation
    (TabNav.)
    ;; Content
    (dom/div
     (dom/props {:class "p-4"})
     (case @!active-tab
       :dashboard (Dashboard.)
       :models (ModelsView.)
       :analysis (AnalysisView.)
       :statistics (StatisticsView.)
       :data (DataView.)
       (Dashboard.))))))
