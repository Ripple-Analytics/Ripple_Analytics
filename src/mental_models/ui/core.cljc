(ns mental-models.ui.core
  "Core Electric UI - M&S + Costco Design Language
   Clean typography, high information density, monochrome + red accents"
  #?(:cljs (:require-macros [mental-models.ui.core :refer [e-fn]]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.data.models :as models])
            #?(:clj [mental-models.services.llm :as llm])))

;; -- Design Tokens (M&S + Costco) --------------------------------------------
;; Monochrome base with strategic red accents for actions/alerts

(def colors
  {:bg-primary "#ffffff"
   :bg-secondary "#f8f8f8"
   :bg-tertiary "#f0f0f0"
   :text-primary "#1a1a1a"
   :text-secondary "#525252"
   :text-tertiary "#737373"
   :border "#e5e5e5"
   :border-dark "#d4d4d4"
   :accent "#dc2626"        ; Costco red for CTAs
   :accent-hover "#b91c1c"
   :success "#16a34a"
   :warning "#ca8a04"
   :info "#525252"})

(def typography
  {:font-family "'Inter', -apple-system, BlinkMacSystemFont, sans-serif"
   :font-mono "'JetBrains Mono', 'SF Mono', monospace"
   :size-xs "11px"
   :size-sm "12px"
   :size-base "13px"
   :size-lg "14px"
   :size-xl "16px"
   :size-2xl "20px"
   :size-3xl "24px"
   :weight-normal "400"
   :weight-medium "500"
   :weight-semibold "600"
   :weight-bold "700"})

;; -- Base Styles -------------------------------------------------------------

(def base-styles
  "* { box-sizing: border-box; margin: 0; padding: 0; }
   body { 
     font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
     font-size: 13px;
     line-height: 1.5;
     color: #1a1a1a;
     background: #ffffff;
     -webkit-font-smoothing: antialiased;
   }
   
   /* High density table styles - Costco warehouse aesthetic */
   .data-table {
     width: 100%;
     border-collapse: collapse;
     font-size: 12px;
   }
   .data-table th {
     background: #f0f0f0;
     font-weight: 600;
     text-align: left;
     padding: 8px 12px;
     border-bottom: 2px solid #d4d4d4;
     text-transform: uppercase;
     font-size: 11px;
     letter-spacing: 0.5px;
     color: #525252;
   }
   .data-table td {
     padding: 6px 12px;
     border-bottom: 1px solid #e5e5e5;
     vertical-align: top;
   }
   .data-table tr:hover {
     background: #f8f8f8;
   }
   
   /* Compact cards - M&S clean aesthetic */
   .card {
     background: #ffffff;
     border: 1px solid #e5e5e5;
     border-radius: 4px;
     padding: 12px;
   }
   .card-header {
     font-weight: 600;
     font-size: 13px;
     margin-bottom: 8px;
     color: #1a1a1a;
   }
   
   /* Metric displays - Value Line density */
   .metric {
     display: flex;
     flex-direction: column;
   }
   .metric-value {
     font-size: 20px;
     font-weight: 700;
     color: #1a1a1a;
     font-variant-numeric: tabular-nums;
   }
   .metric-label {
     font-size: 11px;
     color: #737373;
     text-transform: uppercase;
     letter-spacing: 0.5px;
   }
   .metric-delta {
     font-size: 11px;
     font-weight: 500;
   }
   .metric-delta.positive { color: #16a34a; }
   .metric-delta.negative { color: #dc2626; }
   
   /* Navigation - clean M&S style */
   .nav-item {
     display: flex;
     align-items: center;
     padding: 8px 12px;
     color: #525252;
     text-decoration: none;
     font-size: 13px;
     border-radius: 4px;
     transition: all 0.15s;
   }
   .nav-item:hover {
     background: #f0f0f0;
     color: #1a1a1a;
   }
   .nav-item.active {
     background: #f0f0f0;
     color: #1a1a1a;
     font-weight: 500;
   }
   
   /* Buttons - Costco red for primary actions */
   .btn {
     display: inline-flex;
     align-items: center;
     justify-content: center;
     padding: 6px 12px;
     font-size: 13px;
     font-weight: 500;
     border-radius: 4px;
     border: none;
     cursor: pointer;
     transition: all 0.15s;
   }
   .btn-primary {
     background: #dc2626;
     color: white;
   }
   .btn-primary:hover {
     background: #b91c1c;
   }
   .btn-secondary {
     background: #f0f0f0;
     color: #1a1a1a;
     border: 1px solid #d4d4d4;
   }
   .btn-secondary:hover {
     background: #e5e5e5;
   }
   
   /* Tags and badges */
   .tag {
     display: inline-flex;
     align-items: center;
     padding: 2px 6px;
     font-size: 11px;
     font-weight: 500;
     border-radius: 2px;
     background: #f0f0f0;
     color: #525252;
   }
   .tag-red {
     background: #fef2f2;
     color: #dc2626;
   }
   
   /* Sparklines */
   .sparkline {
     display: inline-block;
     vertical-align: middle;
   }
   
   /* Scrollbar styling */
   ::-webkit-scrollbar {
     width: 8px;
     height: 8px;
   }
   ::-webkit-scrollbar-track {
     background: #f0f0f0;
   }
   ::-webkit-scrollbar-thumb {
     background: #d4d4d4;
     border-radius: 4px;
   }
   ::-webkit-scrollbar-thumb:hover {
     background: #a3a3a3;
   }")

;; -- Layout Components -------------------------------------------------------

(e/defn Sidebar [current-page]
  (dom/aside
    (dom/props {:style {:width "200px"
                        :background "#f8f8f8"
                        :border-right "1px solid #e5e5e5"
                        :height "100vh"
                        :position "fixed"
                        :left "0"
                        :top "0"
                        :padding "16px 0"
                        :overflow-y "auto"}})
    
    ;; Logo
    (dom/div
      (dom/props {:style {:padding "0 16px 16px"
                          :border-bottom "1px solid #e5e5e5"
                          :margin-bottom "16px"}})
      (dom/h1
        (dom/props {:style {:font-size "14px"
                            :font-weight "700"
                            :color "#1a1a1a"
                            :letter-spacing "-0.5px"}})
        (dom/text "MENTAL MODELS")))
    
    ;; Navigation
    (dom/nav
      (dom/props {:style {:padding "0 8px"}})
      
      (e/for [[label page icon] [["Dashboard" :dashboard "◉"]
                                  ["Models" :models "◎"]
                                  ["Decisions" :decisions "◇"]
                                  ["Analysis" :analysis "△"]
                                  ["Statistics" :statistics "▤"]
                                  ["Metrics" :metrics "▥"]
                                  ["World Map" :world-map "◯"]
                                  ["Knowledge Graph" :knowledge-graph "◈"]
                                  ["Signals" :signals "◆"]
                                  ["Connectors" :connectors "⬡"]]]
        (dom/a
          (dom/props {:class (str "nav-item" (when (= current-page page) " active"))
                      :href (str "#" (name page))})
          (dom/span
            (dom/props {:style {:margin-right "8px" :font-size "12px"}})
            (dom/text icon))
          (dom/text label))))))

(e/defn Header [title]
  (dom/header
    (dom/props {:style {:display "flex"
                        :justify-content "space-between"
                        :align-items "center"
                        :padding "12px 24px"
                        :border-bottom "1px solid #e5e5e5"
                        :background "#ffffff"}})
    (dom/h2
      (dom/props {:style {:font-size "16px"
                          :font-weight "600"
                          :color "#1a1a1a"}})
      (dom/text title))
    
    ;; Search
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :gap "12px"}})
      (dom/input
        (dom/props {:type "text"
                    :placeholder "Search models... (⌘K)"
                    :style {:width "240px"
                            :padding "6px 12px"
                            :border "1px solid #e5e5e5"
                            :border-radius "4px"
                            :font-size "13px"
                            :background "#f8f8f8"}}))
      (dom/button
        (dom/props {:class "btn btn-primary"})
        (dom/text "New Decision")))))

;; -- Data Display Components -------------------------------------------------

(e/defn MetricCard [label value delta delta-label]
  (dom/div
    (dom/props {:class "card"
                :style {:min-width "140px"}})
    (dom/div
      (dom/props {:class "metric"})
      (dom/span
        (dom/props {:class "metric-label"})
        (dom/text label))
      (dom/span
        (dom/props {:class "metric-value"})
        (dom/text (str value)))
      (when delta
        (dom/span
          (dom/props {:class (str "metric-delta " (if (pos? delta) "positive" "negative"))})
          (dom/text (str (when (pos? delta) "+") delta "% " delta-label)))))))

(e/defn DataTable [headers rows]
  (dom/table
    (dom/props {:class "data-table"})
    (dom/thead
      (dom/tr
        (e/for [h headers]
          (dom/th (dom/text h)))))
    (dom/tbody
      (e/for [row rows]
        (dom/tr
          (e/for [cell row]
            (dom/td (dom/text (str cell)))))))))

(e/defn ModelRow [{:keys [id name slug category-id description]}]
  (let [category (e/server (models/get-category-by-id category-id))]
    (dom/tr
      (dom/props {:style {:cursor "pointer"}})
      (dom/td
        (dom/props {:style {:font-weight "500"}})
        (dom/text name))
      (dom/td
        (dom/span
          (dom/props {:class "tag"})
          (dom/text (:name category))))
      (dom/td
        (dom/props {:style {:color "#525252" :max-width "400px"}})
        (dom/text (subs description 0 (min 100 (count description))) "..."))
      (dom/td
        (dom/props {:style {:text-align "right"}})
        (dom/button
          (dom/props {:class "btn btn-secondary"
                      :style {:padding "4px 8px" :font-size "11px"}})
          (dom/text "View"))))))

;; -- Page Components ---------------------------------------------------------

(e/defn DashboardPage []
  (let [model-count (e/server (models/model-count))
        category-count (e/server (models/category-count))
        recent-models (e/server (take 10 (models/get-all-models)))]
    
    (dom/div
      (dom/props {:style {:padding "24px"}})
      
      ;; Metrics row
      (dom/div
        (dom/props {:style {:display "flex"
                            :gap "16px"
                            :margin-bottom "24px"}})
        (MetricCard. "Total Models" model-count 12 "this month")
        (MetricCard. "Categories" category-count nil nil)
        (MetricCard. "Decisions" 47 8 "this week")
        (MetricCard. "Accuracy" "73%" 3 "improvement")
        (MetricCard. "Active Streaks" 12 nil nil))
      
      ;; Two column layout
      (dom/div
        (dom/props {:style {:display "grid"
                            :grid-template-columns "2fr 1fr"
                            :gap "24px"}})
        
        ;; Recent activity table
        (dom/div
          (dom/props {:class "card"})
          (dom/div
            (dom/props {:class "card-header"
                        :style {:display "flex"
                                :justify-content "space-between"
                                :align-items "center"}})
            (dom/text "Recent Models")
            (dom/a
              (dom/props {:href "#models"
                          :style {:font-size "12px"
                                  :color "#dc2626"
                                  :text-decoration "none"}})
              (dom/text "View all →")))
          
          (dom/table
            (dom/props {:class "data-table"})
            (dom/thead
              (dom/tr
                (dom/th (dom/text "Model"))
                (dom/th (dom/text "Category"))
                (dom/th (dom/text "Description"))
                (dom/th (dom/text ""))))
            (dom/tbody
              (e/for [model recent-models]
                (ModelRow. model)))))
        
        ;; Quick stats sidebar
        (dom/div
          (dom/props {:style {:display "flex"
                              :flex-direction "column"
                              :gap "16px"}})
          
          ;; Category breakdown
          (dom/div
            (dom/props {:class "card"})
            (dom/div
              (dom/props {:class "card-header"})
              (dom/text "By Category"))
            (dom/div
              (dom/props {:style {:display "flex"
                                  :flex-direction "column"
                                  :gap "8px"}})
              (e/for [cat (e/server (models/get-all-categories))]
                (let [count (e/server (count (models/get-models-by-category (:id cat))))]
                  (dom/div
                    (dom/props {:style {:display "flex"
                                        :justify-content "space-between"
                                        :align-items "center"
                                        :padding "4px 0"}})
                    (dom/span
                      (dom/props {:style {:font-size "12px"}})
                      (dom/text (:name cat)))
                    (dom/span
                      (dom/props {:style {:font-size "12px"
                                          :font-weight "600"
                                          :color "#525252"}})
                      (dom/text (str count))))))))
          
          ;; System status
          (dom/div
            (dom/props {:class "card"})
            (dom/div
              (dom/props {:class "card-header"})
              (dom/text "System Status"))
            (dom/div
              (dom/props {:style {:display "flex"
                                  :flex-direction "column"
                                  :gap "6px"
                                  :font-size "12px"}})
              (dom/div
                (dom/props {:style {:display "flex"
                                    :justify-content "space-between"}})
                (dom/text "Database")
                (dom/span
                  (dom/props {:style {:color "#16a34a"}})
                  (dom/text "● Connected")))
              (dom/div
                (dom/props {:style {:display "flex"
                                    :justify-content "space-between"}})
                (dom/text "LLM Service")
                (dom/span
                  (dom/props {:style {:color "#16a34a"}})
                  (dom/text "● Ready")))
              (dom/div
                (dom/props {:style {:display "flex"
                                    :justify-content "space-between"}})
                (dom/text "Last Sync")
                (dom/span
                  (dom/props {:style {:color "#525252"}})
                  (dom/text "2 min ago"))))))))))

(e/defn ModelsPage []
  (let [all-models (e/server (models/get-all-models))
        categories (e/server (models/get-all-categories))]
    
    (dom/div
      (dom/props {:style {:padding "24px"}})
      
      ;; Filters
      (dom/div
        (dom/props {:style {:display "flex"
                            :gap "12px"
                            :margin-bottom "16px"}})
        (dom/select
          (dom/props {:style {:padding "6px 12px"
                              :border "1px solid #e5e5e5"
                              :border-radius "4px"
                              :font-size "13px"
                              :background "#ffffff"}})
          (dom/option (dom/text "All Categories"))
          (e/for [cat categories]
            (dom/option
              (dom/props {:value (:id cat)})
              (dom/text (:name cat)))))
        
        (dom/input
          (dom/props {:type "text"
                      :placeholder "Filter models..."
                      :style {:padding "6px 12px"
                              :border "1px solid #e5e5e5"
                              :border-radius "4px"
                              :font-size "13px"
                              :width "200px"}})))
      
      ;; Models table
      (dom/div
        (dom/props {:class "card"})
        (dom/table
          (dom/props {:class "data-table"})
          (dom/thead
            (dom/tr
              (dom/th (dom/text "Model"))
              (dom/th (dom/text "Category"))
              (dom/th (dom/text "Thinker"))
              (dom/th (dom/text "Failure Modes"))
              (dom/th (dom/text ""))))
          (dom/tbody
            (e/for [{:keys [id name category-id thinker failure-modes] :as model} all-models]
              (let [category (e/server (models/get-category-by-id category-id))]
                (dom/tr
                  (dom/props {:style {:cursor "pointer"}})
                  (dom/td
                    (dom/props {:style {:font-weight "500"}})
                    (dom/text name))
                  (dom/td
                    (dom/span
                      (dom/props {:class "tag"})
                      (dom/text (:name category))))
                  (dom/td
                    (dom/props {:style {:color "#525252" :font-size "12px"}})
                    (dom/text (or thinker "—")))
                  (dom/td
                    (dom/props {:style {:font-size "12px"}})
                    (dom/text (str (count failure-modes) " modes")))
                  (dom/td
                    (dom/props {:style {:text-align "right"}})
                    (dom/button
                      (dom/props {:class "btn btn-secondary"
                                  :style {:padding "4px 8px" :font-size "11px"}})
                      (dom/text "Details"))))))))))))

;; -- Main App ----------------------------------------------------------------

(e/defn App []
  (let [current-page (e/client (atom :dashboard))]
    (dom/div
      (dom/props {:style {:display "flex" :min-height "100vh"}})
      
      ;; Inject styles
      (dom/style (dom/text base-styles))
      
      ;; Sidebar
      (Sidebar. @current-page)
      
      ;; Main content
      (dom/main
        (dom/props {:style {:margin-left "200px"
                            :flex "1"
                            :background "#ffffff"}})
        
        (Header. (case @current-page
                   :dashboard "Dashboard"
                   :models "Mental Models"
                   :decisions "Decision Journal"
                   :analysis "Analysis"
                   :statistics "Statistics"
                   :metrics "Metrics"
                   :world-map "World Map"
                   :knowledge-graph "Knowledge Graph"
                   :signals "Signal Harvester"
                   :connectors "Connectors"
                   "Dashboard"))
        
        (case @current-page
          :dashboard (DashboardPage.)
          :models (ModelsPage.)
          (DashboardPage.))))))
