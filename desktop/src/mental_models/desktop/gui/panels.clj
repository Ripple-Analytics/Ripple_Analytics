(ns mental-models.desktop.gui.panels
  "Integration panels for Mental Models Desktop App.
   Includes Case Studies, Signals, Model Effectiveness, Knowledge Graph,
   Decision Journal, and News Analyzer panels."
  (:require [cljfx.api :as fx]
            [clojure.string :as str]
            [mental-models.desktop.api.web-client :as api]
            [mental-models.desktop.db :as db])
  (:import [javafx.scene.canvas Canvas GraphicsContext]
           [javafx.scene.paint Color]
           [javafx.geometry VPos]
           [javafx.scene.text TextAlignment]))

;; =============================================================================
;; Color Scheme (matching app.clj)
;; =============================================================================

(def colors
  {:bg-primary "#ffffff"
   :bg-secondary "#f8fafc"
   :bg-tertiary "#f1f5f9"
   :bg-card "#ffffff"
   :border "#e2e8f0"
   :text-primary "#0f172a"
   :text-secondary "#475569"
   :text-muted "#94a3b8"
   :accent "#3b82f6"
   :accent-hover "#2563eb"
   :success "#22c55e"
   :warning "#f59e0b"
   :error "#ef4444"
   :purple "#a855f7"})

;; =============================================================================
;; Panel State
;; =============================================================================

(def panel-state
  (atom {:case-studies {:data []
                        :loading false
                        :search-query ""
                        :selected nil
                        :filter :all}
         :signals {:data []
                   :loading false
                   :filter {:source :all
                            :risk-level :all}
                   :selected nil
                   :watchlist []}
         :effectiveness {:data nil
                         :loading false
                         :selected-model nil
                         :time-range :week}
         :knowledge-graph {:nodes []
                           :edges []
                           :loading false
                           :selected-node nil
                           :zoom 1.0
                           :pan-x 0
                           :pan-y 0
                           :filter-category :all}
         :decisions {:data []
                     :loading false
                     :selected nil
                     :new-decision nil}
         :news {:articles []
                :loading false
                :selected nil
                :analyzed []}}))

;; =============================================================================
;; Data Fetching Functions
;; =============================================================================

(defn fetch-case-studies! []
  (swap! panel-state assoc-in [:case-studies :loading] true)
  (future
    (try
      (let [data (api/get-case-studies)]
        (swap! panel-state assoc-in [:case-studies :data] (or data []))
        (swap! panel-state assoc-in [:case-studies :loading] false))
      (catch Exception e
        (println "Error fetching case studies:" (.getMessage e))
        (swap! panel-state assoc-in [:case-studies :loading] false)))))

(defn fetch-signals! []
  (swap! panel-state assoc-in [:signals :loading] true)
  (future
    (try
      (let [data (api/get-signals :limit 100)]
        (swap! panel-state assoc-in [:signals :data] (or data []))
        (swap! panel-state assoc-in [:signals :loading] false))
      (catch Exception e
        (println "Error fetching signals:" (.getMessage e))
        (swap! panel-state assoc-in [:signals :loading] false)))))

(defn fetch-effectiveness! []
  (swap! panel-state assoc-in [:effectiveness :loading] true)
  (future
    (try
      (let [data (api/get-effectiveness-stats)]
        (swap! panel-state assoc-in [:effectiveness :data] data)
        (swap! panel-state assoc-in [:effectiveness :loading] false))
      (catch Exception e
        (println "Error fetching effectiveness:" (.getMessage e))
        (swap! panel-state assoc-in [:effectiveness :loading] false)))))

(defn fetch-knowledge-graph! []
  (swap! panel-state assoc-in [:knowledge-graph :loading] true)
  (future
    (try
      (let [data (api/get-knowledge-graph)]
        (swap! panel-state assoc-in [:knowledge-graph :nodes] (or (:nodes data) []))
        (swap! panel-state assoc-in [:knowledge-graph :edges] (or (:edges data) []))
        (swap! panel-state assoc-in [:knowledge-graph :loading] false))
      (catch Exception e
        (println "Error fetching knowledge graph:" (.getMessage e))
        (swap! panel-state assoc-in [:knowledge-graph :loading] false)))))

(defn fetch-decisions! []
  (swap! panel-state assoc-in [:decisions :loading] true)
  (future
    (try
      (let [data (api/get-decisions)]
        (swap! panel-state assoc-in [:decisions :data] (or data []))
        (swap! panel-state assoc-in [:decisions :loading] false))
      (catch Exception e
        (println "Error fetching decisions:" (.getMessage e))
        (swap! panel-state assoc-in [:decisions :loading] false)))))

;; =============================================================================
;; Case Studies Panel
;; =============================================================================

(defn case-study-card [{:keys [case-study selected? on-click]}]
  {:fx/type :v-box
   :style {:-fx-background-color (if selected? (:accent colors) (:bg-card colors))
           :-fx-background-radius 8
           :-fx-border-color (:border colors)
           :-fx-border-radius 8
           :-fx-cursor :hand}
   :padding 16
   :spacing 8
   :on-mouse-clicked on-click
   :children [{:fx/type :label
               :text (or (:title case-study) "Untitled")
               :style {:-fx-font-size 14
                       :-fx-font-weight :bold
                       :-fx-text-fill (if selected? :white (:text-primary colors))}}
              {:fx/type :label
               :text (or (:company case-study) "Unknown Company")
               :style {:-fx-font-size 12
                       :-fx-text-fill (if selected? :white (:text-secondary colors))}}
              {:fx/type :h-box
               :spacing 8
               :children [{:fx/type :label
                           :text (str (count (or (:models case-study) [])) " models")
                           :style {:-fx-background-color (if selected? "rgba(255,255,255,0.2)" (:bg-tertiary colors))
                                   :-fx-padding "2 6"
                                   :-fx-background-radius 4
                                   :-fx-font-size 10
                                   :-fx-text-fill (if selected? :white (:text-muted colors))}}
                          (when (:outcome case-study)
                            {:fx/type :label
                             :text (:outcome case-study)
                             :style {:-fx-background-color (case (:outcome case-study)
                                                             "positive" (:success colors)
                                                             "negative" (:error colors)
                                                             (:warning colors))
                                     :-fx-padding "2 6"
                                     :-fx-background-radius 4
                                     :-fx-font-size 10
                                     :-fx-text-fill :white}})]}]})

(defn case-study-detail [{:keys [case-study]}]
  (when case-study
    {:fx/type :scroll-pane
     :fit-to-width true
     :style {:-fx-background-color (:bg-card colors)}
     :content
     {:fx/type :v-box
      :padding 20
      :spacing 16
      :children [{:fx/type :label
                  :text (or (:title case-study) "Untitled")
                  :style {:-fx-font-size 20
                          :-fx-font-weight :bold
                          :-fx-text-fill (:text-primary colors)}}
                 {:fx/type :label
                  :text (or (:description case-study) "No description available")
                  :wrap-text true
                  :style {:-fx-text-fill (:text-secondary colors)}}
                 
                 ;; Timeline
                 {:fx/type :v-box
                  :spacing 8
                  :children [{:fx/type :label
                              :text "Timeline"
                              :style {:-fx-font-size 14
                                      :-fx-font-weight :bold
                                      :-fx-text-fill (:text-primary colors)}}
                             {:fx/type :v-box
                              :spacing 4
                              :children (for [event (or (:timeline case-study) [])]
                                          {:fx/type :h-box
                                           :spacing 8
                                           :children [{:fx/type :label
                                                       :text (or (:date event) "")
                                                       :min-width 100
                                                       :style {:-fx-text-fill (:text-muted colors)
                                                               :-fx-font-size 12}}
                                                      {:fx/type :label
                                                       :text (or (:event event) "")
                                                       :style {:-fx-text-fill (:text-primary colors)}}]})}]}
                 
                 ;; Models Involved
                 {:fx/type :v-box
                  :spacing 8
                  :children [{:fx/type :label
                              :text "Models Involved"
                              :style {:-fx-font-size 14
                                      :-fx-font-weight :bold
                                      :-fx-text-fill (:text-primary colors)}}
                             {:fx/type :flow-pane
                              :hgap 8
                              :vgap 8
                              :children (for [model (or (:models case-study) [])]
                                          {:fx/type :label
                                           :text (or (:name model) model)
                                           :style {:-fx-background-color (:bg-tertiary colors)
                                                   :-fx-padding "4 8"
                                                   :-fx-background-radius 4
                                                   :-fx-font-size 12
                                                   :-fx-text-fill (:text-primary colors)}})}]}
                 
                 ;; Outcome
                 (when (:outcome case-study)
                   {:fx/type :v-box
                    :spacing 8
                    :children [{:fx/type :label
                                :text "Outcome"
                                :style {:-fx-font-size 14
                                        :-fx-font-weight :bold
                                        :-fx-text-fill (:text-primary colors)}}
                               {:fx/type :label
                                :text (or (:outcome-description case-study) (:outcome case-study))
                                :wrap-text true
                                :style {:-fx-text-fill (:text-secondary colors)}}]})]}}))

(defn case-studies-panel [{:keys [state]}]
  (let [{:keys [data loading search-query selected filter]} (:case-studies state)
        filtered-data (cond->> data
                        (not (str/blank? search-query))
                        (filter #(or (str/includes? (str/lower-case (or (:title %) "")) 
                                                    (str/lower-case search-query))
                                     (str/includes? (str/lower-case (or (:company %) ""))
                                                    (str/lower-case search-query))))
                        (not= filter :all)
                        (filter #(= (:outcome %) (name filter))))]
    {:fx/type :h-box
     :style {:-fx-background-color (:bg-primary colors)}
     :children [;; Left panel - list
                {:fx/type :v-box
                 :pref-width 350
                 :style {:-fx-border-color (:border colors)
                         :-fx-border-width "0 1 0 0"}
                 :children [{:fx/type :v-box
                             :padding 16
                             :spacing 12
                             :children [{:fx/type :label
                                         :text "Case Studies"
                                         :style {:-fx-font-size 20
                                                 :-fx-font-weight :bold
                                                 :-fx-text-fill (:text-primary colors)}}
                                        ;; Search
                                        {:fx/type :text-field
                                         :prompt-text "Search case studies..."
                                         :text search-query
                                         :on-text-changed {:event/type ::search-case-studies}
                                         :style {:-fx-background-color (:bg-tertiary colors)
                                                 :-fx-border-radius 4
                                                 :-fx-background-radius 4}}
                                        ;; Filter buttons
                                        {:fx/type :h-box
                                         :spacing 8
                                         :children (for [[f label] [[:all "All"] [:positive "Positive"] [:negative "Negative"]]]
                                                     {:fx/type :button
                                                      :text label
                                                      :on-action {:event/type ::filter-case-studies :filter f}
                                                      :style {:-fx-background-color (if (= filter f)
                                                                                      (:accent colors)
                                                                                      (:bg-tertiary colors))
                                                              :-fx-text-fill (if (= filter f) :white (:text-primary colors))
                                                              :-fx-background-radius 4
                                                              :-fx-padding "4 12"}})}
                                        ;; Refresh button
                                        {:fx/type :button
                                         :text (if loading "Loading..." "Refresh")
                                         :disable loading
                                         :on-action {:event/type ::refresh-case-studies}
                                         :style {:-fx-background-color (:accent colors)
                                                 :-fx-text-fill :white
                                                 :-fx-background-radius 4}}]}
                            ;; List
                            {:fx/type :scroll-pane
                             :fit-to-width true
                             :v-box/vgrow :always
                             :content
                             {:fx/type :v-box
                              :padding 16
                              :spacing 12
                              :children (if (empty? filtered-data)
                                          [{:fx/type :label
                                            :text (if loading "Loading..." "No case studies found")
                                            :style {:-fx-text-fill (:text-muted colors)}}]
                                          (for [cs filtered-data]
                                            (case-study-card {:case-study cs
                                                              :selected? (= (:id cs) (:id selected))
                                                              :on-click {:event/type ::select-case-study :case-study cs}})))}}]}
                ;; Right panel - detail
                {:fx/type :v-box
                 :h-box/hgrow :always
                 :children [(if selected
                              (case-study-detail {:case-study selected})
                              {:fx/type :v-box
                               :alignment :center
                               :v-box/vgrow :always
                               :children [{:fx/type :label
                                           :text "Select a case study to view details"
                                           :style {:-fx-text-fill (:text-muted colors)}}]})]}]}))

;; =============================================================================
;; Signals Panel
;; =============================================================================

(defn signal-severity-badge [{:keys [level]}]
  {:fx/type :label
   :text (str/upper-case (or level "unknown"))
   :style {:-fx-background-color (case level
                                   "high" (:error colors)
                                   "medium" (:warning colors)
                                   "low" (:success colors)
                                   (:text-muted colors))
           :-fx-text-fill :white
           :-fx-padding "2 6"
           :-fx-background-radius 4
           :-fx-font-size 10}})

(defn signal-card [{:keys [signal selected? on-click on-watchlist]}]
  {:fx/type :v-box
   :style {:-fx-background-color (if selected? (:accent colors) (:bg-card colors))
           :-fx-background-radius 8
           :-fx-border-color (:border colors)
           :-fx-border-radius 8
           :-fx-cursor :hand}
   :padding 12
   :spacing 8
   :on-mouse-clicked on-click
   :children [{:fx/type :h-box
               :alignment :center-left
               :children [{:fx/type :label
                           :text (or (:title signal) "Untitled Signal")
                           :h-box/hgrow :always
                           :style {:-fx-font-size 13
                                   :-fx-font-weight :bold
                                   :-fx-text-fill (if selected? :white (:text-primary colors))}}
                          (signal-severity-badge {:level (:risk-level signal)})]}
              {:fx/type :label
               :text (or (:source signal) "Unknown source")
               :style {:-fx-font-size 11
                       :-fx-text-fill (if selected? :white (:text-muted colors))}}
              {:fx/type :h-box
               :spacing 6
               :children (for [model (take 3 (or (:models signal) []))]
                           {:fx/type :label
                            :text (or (:name model) model)
                            :style {:-fx-background-color (if selected? "rgba(255,255,255,0.2)" (:bg-tertiary colors))
                                    :-fx-padding "2 4"
                                    :-fx-background-radius 3
                                    :-fx-font-size 10
                                    :-fx-text-fill (if selected? :white (:text-muted colors))}})}]})

(defn signals-panel [{:keys [state]}]
  (let [{:keys [data loading filter selected watchlist]} (:signals state)
        {:keys [source risk-level]} filter
        filtered-data (cond->> data
                        (not= source :all)
                        (clojure.core/filter #(= (:source %) (name source)))
                        (not= risk-level :all)
                        (clojure.core/filter #(= (:risk-level %) (name risk-level))))]
    {:fx/type :h-box
     :style {:-fx-background-color (:bg-primary colors)}
     :children [;; Left panel - list
                {:fx/type :v-box
                 :pref-width 400
                 :style {:-fx-border-color (:border colors)
                         :-fx-border-width "0 1 0 0"}
                 :children [{:fx/type :v-box
                             :padding 16
                             :spacing 12
                             :children [{:fx/type :h-box
                                         :alignment :center-left
                                         :children [{:fx/type :label
                                                     :text "Signals"
                                                     :h-box/hgrow :always
                                                     :style {:-fx-font-size 20
                                                             :-fx-font-weight :bold
                                                             :-fx-text-fill (:text-primary colors)}}
                                                    {:fx/type :label
                                                     :text (str (count data) " signals")
                                                     :style {:-fx-text-fill (:text-muted colors)}}]}
                                        ;; Source filter
                                        {:fx/type :h-box
                                         :spacing 8
                                         :children [{:fx/type :label
                                                     :text "Source:"
                                                     :style {:-fx-text-fill (:text-secondary colors)}}
                                                    {:fx/type :combo-box
                                                     :value (name source)
                                                     :items ["all" "SEC" "News" "Social"]
                                                     :on-value-changed {:event/type ::filter-signals-source}}]}
                                        ;; Risk level filter
                                        {:fx/type :h-box
                                         :spacing 8
                                         :children [{:fx/type :label
                                                     :text "Risk:"
                                                     :style {:-fx-text-fill (:text-secondary colors)}}
                                                    {:fx/type :combo-box
                                                     :value (name risk-level)
                                                     :items ["all" "high" "medium" "low"]
                                                     :on-value-changed {:event/type ::filter-signals-risk}}]}
                                        {:fx/type :button
                                         :text (if loading "Loading..." "Refresh")
                                         :disable loading
                                         :on-action {:event/type ::refresh-signals}
                                         :style {:-fx-background-color (:accent colors)
                                                 :-fx-text-fill :white
                                                 :-fx-background-radius 4}}]}
                            ;; List
                            {:fx/type :scroll-pane
                             :fit-to-width true
                             :v-box/vgrow :always
                             :content
                             {:fx/type :v-box
                              :padding 16
                              :spacing 8
                              :children (if (empty? filtered-data)
                                          [{:fx/type :label
                                            :text (if loading "Loading..." "No signals found")
                                            :style {:-fx-text-fill (:text-muted colors)}}]
                                          (for [sig filtered-data]
                                            (signal-card {:signal sig
                                                          :selected? (= (:id sig) (:id selected))
                                                          :on-click {:event/type ::select-signal :signal sig}})))}}]}
                ;; Right panel - detail
                {:fx/type :v-box
                 :h-box/hgrow :always
                 :padding 20
                 :spacing 16
                 :children (if selected
                             [{:fx/type :h-box
                               :alignment :center-left
                               :children [{:fx/type :label
                                           :text (or (:title selected) "Signal Details")
                                           :h-box/hgrow :always
                                           :style {:-fx-font-size 18
                                                   :-fx-font-weight :bold
                                                   :-fx-text-fill (:text-primary colors)}}
                                          {:fx/type :button
                                           :text (if (some #(= (:id %) (:id selected)) watchlist)
                                                   "Remove from Watchlist"
                                                   "Add to Watchlist")
                                           :on-action {:event/type ::toggle-watchlist :signal selected}
                                           :style {:-fx-background-color (:accent colors)
                                                   :-fx-text-fill :white
                                                   :-fx-background-radius 4}}]}
                              {:fx/type :label
                               :text (or (:content selected) "No content available")
                               :wrap-text true
                               :style {:-fx-text-fill (:text-secondary colors)}}
                              {:fx/type :v-box
                               :spacing 8
                               :children [{:fx/type :label
                                           :text "Detected Models"
                                           :style {:-fx-font-weight :bold
                                                   :-fx-text-fill (:text-primary colors)}}
                                          {:fx/type :flow-pane
                                           :hgap 8
                                           :vgap 8
                                           :children (for [model (or (:models selected) [])]
                                                       {:fx/type :label
                                                        :text (or (:name model) model)
                                                        :style {:-fx-background-color (:bg-tertiary colors)
                                                                :-fx-padding "4 8"
                                                                :-fx-background-radius 4
                                                                :-fx-text-fill (:text-primary colors)}})}]}
                              {:fx/type :h-box
                               :spacing 16
                               :children [{:fx/type :button
                                           :text "Mark as Reviewed"
                                           :on-action {:event/type ::mark-signal-reviewed :signal selected}
                                           :style {:-fx-background-color (:success colors)
                                                   :-fx-text-fill :white
                                                   :-fx-background-radius 4}}
                                          {:fx/type :button
                                           :text "Dismiss"
                                           :on-action {:event/type ::dismiss-signal :signal selected}
                                           :style {:-fx-background-color (:text-muted colors)
                                                   :-fx-text-fill :white
                                                   :-fx-background-radius 4}}]}]
                             [{:fx/type :v-box
                               :alignment :center
                               :v-box/vgrow :always
                               :children [{:fx/type :label
                                           :text "Select a signal to view details"
                                           :style {:-fx-text-fill (:text-muted colors)}}]}])}]}))

;; =============================================================================
;; Model Effectiveness Panel
;; =============================================================================

(defn effectiveness-stat-card [{:keys [label value color]}]
  {:fx/type :v-box
   :style {:-fx-background-color (:bg-card colors)
           :-fx-background-radius 8
           :-fx-border-color (:border colors)
           :-fx-border-radius 8}
   :padding 16
   :spacing 4
   :h-box/hgrow :always
   :children [{:fx/type :label
               :text label
               :style {:-fx-font-size 12
                       :-fx-text-fill (:text-muted colors)}}
              {:fx/type :label
               :text (str value)
               :style {:-fx-font-size 24
                       :-fx-font-weight :bold
                       :-fx-text-fill (or color (:text-primary colors))}}]})

(defn model-effectiveness-row [{:keys [model]}]
  {:fx/type :h-box
   :alignment :center-left
   :spacing 12
   :padding {:top 8 :bottom 8}
   :style {:-fx-border-color (:border colors)
           :-fx-border-width "0 0 1 0"}
   :children [{:fx/type :label
               :text (or (:name model) "Unknown")
               :min-width 200
               :style {:-fx-text-fill (:text-primary colors)}}
              {:fx/type :progress-bar
               :progress (or (:accuracy model) 0)
               :pref-width 150
               :style {:-fx-accent (cond
                                     (>= (or (:accuracy model) 0) 0.8) (:success colors)
                                     (>= (or (:accuracy model) 0) 0.6) (:warning colors)
                                     :else (:error colors))}}
              {:fx/type :label
               :text (str (int (* 100 (or (:accuracy model) 0))) "%")
               :min-width 50
               :style {:-fx-text-fill (:text-secondary colors)}}
              {:fx/type :label
               :text (str (or (:predictions model) 0) " predictions")
               :style {:-fx-text-fill (:text-muted colors)}}]})

(defn effectiveness-panel [{:keys [state]}]
  (let [{:keys [data loading time-range]} (:effectiveness state)
        models (or (:models data) [])
        sorted-models (sort-by :accuracy > models)]
    {:fx/type :scroll-pane
     :fit-to-width true
     :style {:-fx-background-color (:bg-primary colors)}
     :content
     {:fx/type :v-box
      :padding 24
      :spacing 24
      :children [{:fx/type :h-box
                  :alignment :center-left
                  :children [{:fx/type :label
                              :text "Model Effectiveness"
                              :h-box/hgrow :always
                              :style {:-fx-font-size 20
                                      :-fx-font-weight :bold
                                      :-fx-text-fill (:text-primary colors)}}
                             {:fx/type :combo-box
                              :value (name time-range)
                              :items ["day" "week" "month" "year"]
                              :on-value-changed {:event/type ::change-effectiveness-range}}
                             {:fx/type :button
                              :text (if loading "Loading..." "Refresh")
                              :disable loading
                              :on-action {:event/type ::refresh-effectiveness}
                              :style {:-fx-background-color (:accent colors)
                                      :-fx-text-fill :white
                                      :-fx-background-radius 4
                                      :-fx-margin-left 8}}]}
                 
                 ;; Stats cards
                 {:fx/type :h-box
                  :spacing 16
                  :children [(effectiveness-stat-card {:label "Total Predictions"
                                                       :value (or (:total-predictions data) 0)})
                             (effectiveness-stat-card {:label "Correct Predictions"
                                                       :value (or (:correct-predictions data) 0)
                                                       :color (:success colors)})
                             (effectiveness-stat-card {:label "Overall Accuracy"
                                                       :value (str (int (* 100 (or (:overall-accuracy data) 0))) "%")
                                                       :color (:accent colors)})
                             (effectiveness-stat-card {:label "Best Model"
                                                       :value (or (:name (first sorted-models)) "N/A")})]}
                 
                 ;; Model list
                 {:fx/type :v-box
                  :style {:-fx-background-color (:bg-card colors)
                          :-fx-background-radius 8
                          :-fx-border-color (:border colors)
                          :-fx-border-radius 8}
                  :padding 20
                  :spacing 8
                  :children [{:fx/type :label
                              :text "Model Performance"
                              :style {:-fx-font-size 16
                                      :-fx-font-weight :bold
                                      :-fx-text-fill (:text-primary colors)}}
                             {:fx/type :v-box
                              :children (if (empty? sorted-models)
                                          [{:fx/type :label
                                            :text "No model data available"
                                            :style {:-fx-text-fill (:text-muted colors)}}]
                                          (for [model sorted-models]
                                            (model-effectiveness-row {:model model})))}]}
                 
                 ;; Improvement suggestions
                 {:fx/type :v-box
                  :style {:-fx-background-color (:bg-card colors)
                          :-fx-background-radius 8
                          :-fx-border-color (:border colors)
                          :-fx-border-radius 8}
                  :padding 20
                  :spacing 8
                  :children [{:fx/type :label
                              :text "Improvement Suggestions"
                              :style {:-fx-font-size 16
                                      :-fx-font-weight :bold
                                      :-fx-text-fill (:text-primary colors)}}
                             {:fx/type :v-box
                              :spacing 8
                              :children (for [suggestion (or (:suggestions data) 
                                                             ["Track more predictions to improve accuracy estimates"
                                                              "Focus on models with lower accuracy for improvement"
                                                              "Review false positives to understand model limitations"])]
                                          {:fx/type :h-box
                                           :spacing 8
                                           :children [{:fx/type :label
                                                       :text "•"
                                                       :style {:-fx-text-fill (:accent colors)}}
                                                      {:fx/type :label
                                                       :text suggestion
                                                       :wrap-text true
                                                       :style {:-fx-text-fill (:text-secondary colors)}}]})}]}]}}))

;; =============================================================================
;; Knowledge Graph Panel
;; =============================================================================

(defn knowledge-graph-panel [{:keys [state]}]
  (let [{:keys [nodes edges loading selected-node zoom filter-category]} (:knowledge-graph state)
        categories (distinct (map :category nodes))
        filtered-nodes (if (= filter-category :all)
                         nodes
                         (filter #(= (:category %) filter-category) nodes))]
    {:fx/type :v-box
     :style {:-fx-background-color (:bg-primary colors)}
     :children [{:fx/type :h-box
                 :padding 16
                 :spacing 12
                 :alignment :center-left
                 :children [{:fx/type :label
                             :text "Knowledge Graph"
                             :h-box/hgrow :always
                             :style {:-fx-font-size 20
                                     :-fx-font-weight :bold
                                     :-fx-text-fill (:text-primary colors)}}
                            {:fx/type :combo-box
                             :value (if (= filter-category :all) "All Categories" filter-category)
                             :items (cons "All Categories" categories)
                             :on-value-changed {:event/type ::filter-graph-category}}
                            {:fx/type :button
                             :text "Zoom In"
                             :on-action {:event/type ::zoom-graph :direction :in}
                             :style {:-fx-background-color (:bg-tertiary colors)
                                     :-fx-background-radius 4}}
                            {:fx/type :button
                             :text "Zoom Out"
                             :on-action {:event/type ::zoom-graph :direction :out}
                             :style {:-fx-background-color (:bg-tertiary colors)
                                     :-fx-background-radius 4}}
                            {:fx/type :button
                             :text (if loading "Loading..." "Refresh")
                             :disable loading
                             :on-action {:event/type ::refresh-knowledge-graph}
                             :style {:-fx-background-color (:accent colors)
                                     :-fx-text-fill :white
                                     :-fx-background-radius 4}}]}
                
                {:fx/type :h-box
                 :v-box/vgrow :always
                 :children [;; Graph visualization area
                            {:fx/type :v-box
                             :h-box/hgrow :always
                             :style {:-fx-background-color (:bg-secondary colors)
                                     :-fx-border-color (:border colors)
                                     :-fx-border-width "1"}
                             :alignment :center
                             :children [{:fx/type :label
                                         :text (str (count filtered-nodes) " nodes, " (count edges) " edges")
                                         :style {:-fx-text-fill (:text-muted colors)
                                                 :-fx-padding 8}}
                                        ;; Node list (simplified graph view)
                                        {:fx/type :scroll-pane
                                         :fit-to-width true
                                         :v-box/vgrow :always
                                         :content
                                         {:fx/type :flow-pane
                                          :padding 20
                                          :hgap 12
                                          :vgap 12
                                          :children (for [node filtered-nodes]
                                                      {:fx/type :v-box
                                                       :alignment :center
                                                       :style {:-fx-background-color (if (= (:id node) (:id selected-node))
                                                                                       (:accent colors)
                                                                                       (:bg-card colors))
                                                               :-fx-background-radius 8
                                                               :-fx-border-color (:border colors)
                                                               :-fx-border-radius 8
                                                               :-fx-cursor :hand}
                                                       :padding 12
                                                       :on-mouse-clicked {:event/type ::select-graph-node :node node}
                                                       :children [{:fx/type :label
                                                                   :text (or (:name node) "Node")
                                                                   :style {:-fx-font-weight :bold
                                                                           :-fx-text-fill (if (= (:id node) (:id selected-node))
                                                                                            :white
                                                                                            (:text-primary colors))}}
                                                                  {:fx/type :label
                                                                   :text (or (:category node) "")
                                                                   :style {:-fx-font-size 10
                                                                           :-fx-text-fill (if (= (:id node) (:id selected-node))
                                                                                            :white
                                                                                            (:text-muted colors))}}]})}}]}
                            
                            ;; Node detail panel
                            {:fx/type :v-box
                             :pref-width 300
                             :style {:-fx-background-color (:bg-card colors)
                                     :-fx-border-color (:border colors)
                                     :-fx-border-width "0 0 0 1"}
                             :padding 16
                             :spacing 12
                             :children (if selected-node
                                         [{:fx/type :label
                                           :text (or (:name selected-node) "Node Details")
                                           :style {:-fx-font-size 16
                                                   :-fx-font-weight :bold
                                                   :-fx-text-fill (:text-primary colors)}}
                                          {:fx/type :label
                                           :text (str "Category: " (or (:category selected-node) "Unknown"))
                                           :style {:-fx-text-fill (:text-secondary colors)}}
                                          {:fx/type :label
                                           :text (or (:description selected-node) "No description")
                                           :wrap-text true
                                           :style {:-fx-text-fill (:text-secondary colors)}}
                                          {:fx/type :v-box
                                           :spacing 4
                                           :children [{:fx/type :label
                                                       :text "Connections"
                                                       :style {:-fx-font-weight :bold
                                                               :-fx-text-fill (:text-primary colors)}}
                                                      {:fx/type :v-box
                                                       :spacing 4
                                                       :children (for [edge (filter #(or (= (:source %) (:id selected-node))
                                                                                         (= (:target %) (:id selected-node)))
                                                                                    edges)]
                                                                   {:fx/type :label
                                                                    :text (str (:relationship edge) " -> " 
                                                                               (if (= (:source edge) (:id selected-node))
                                                                                 (:target edge)
                                                                                 (:source edge)))
                                                                    :style {:-fx-text-fill (:text-muted colors)
                                                                            :-fx-font-size 12}})}]}]
                                         [{:fx/type :label
                                           :text "Select a node to view details"
                                           :style {:-fx-text-fill (:text-muted colors)}}])}]}]}))

;; =============================================================================
;; Decision Journal Panel
;; =============================================================================

(defn decision-card [{:keys [decision selected? on-click]}]
  {:fx/type :v-box
   :style {:-fx-background-color (if selected? (:accent colors) (:bg-card colors))
           :-fx-background-radius 8
           :-fx-border-color (:border colors)
           :-fx-border-radius 8
           :-fx-cursor :hand}
   :padding 12
   :spacing 6
   :on-mouse-clicked on-click
   :children [{:fx/type :h-box
               :alignment :center-left
               :children [{:fx/type :label
                           :text (or (:title decision) "Untitled Decision")
                           :h-box/hgrow :always
                           :style {:-fx-font-weight :bold
                                   :-fx-text-fill (if selected? :white (:text-primary colors))}}
                          (when (:outcome decision)
                            {:fx/type :label
                             :text (case (:outcome decision)
                                     "correct" "Correct"
                                     "incorrect" "Incorrect"
                                     "pending" "Pending"
                                     "Unknown")
                             :style {:-fx-background-color (case (:outcome decision)
                                                            "correct" (:success colors)
                                                            "incorrect" (:error colors)
                                                            (:warning colors))
                                     :-fx-text-fill :white
                                     :-fx-padding "2 6"
                                     :-fx-background-radius 4
                                     :-fx-font-size 10}})]}
              {:fx/type :label
               :text (or (:date decision) "")
               :style {:-fx-font-size 11
                       :-fx-text-fill (if selected? :white (:text-muted colors))}}]})

(defn decisions-panel [{:keys [state]}]
  (let [{:keys [data loading selected new-decision]} (:decisions state)]
    {:fx/type :h-box
     :style {:-fx-background-color (:bg-primary colors)}
     :children [;; Left panel - list
                {:fx/type :v-box
                 :pref-width 350
                 :style {:-fx-border-color (:border colors)
                         :-fx-border-width "0 1 0 0"}
                 :children [{:fx/type :v-box
                             :padding 16
                             :spacing 12
                             :children [{:fx/type :h-box
                                         :alignment :center-left
                                         :children [{:fx/type :label
                                                     :text "Decision Journal"
                                                     :h-box/hgrow :always
                                                     :style {:-fx-font-size 20
                                                             :-fx-font-weight :bold
                                                             :-fx-text-fill (:text-primary colors)}}
                                                    {:fx/type :button
                                                     :text "+"
                                                     :on-action {:event/type ::new-decision}
                                                     :style {:-fx-background-color (:accent colors)
                                                             :-fx-text-fill :white
                                                             :-fx-background-radius 4
                                                             :-fx-font-size 16
                                                             :-fx-padding "4 12"}}]}
                                        {:fx/type :button
                                         :text (if loading "Loading..." "Refresh")
                                         :disable loading
                                         :on-action {:event/type ::refresh-decisions}
                                         :style {:-fx-background-color (:bg-tertiary colors)
                                                 :-fx-text-fill (:text-primary colors)
                                                 :-fx-background-radius 4}}]}
                            ;; List
                            {:fx/type :scroll-pane
                             :fit-to-width true
                             :v-box/vgrow :always
                             :content
                             {:fx/type :v-box
                              :padding 16
                              :spacing 8
                              :children (if (empty? data)
                                          [{:fx/type :label
                                            :text (if loading "Loading..." "No decisions recorded")
                                            :style {:-fx-text-fill (:text-muted colors)}}]
                                          (for [dec data]
                                            (decision-card {:decision dec
                                                            :selected? (= (:id dec) (:id selected))
                                                            :on-click {:event/type ::select-decision :decision dec}})))}}]}
                ;; Right panel - detail/new
                {:fx/type :scroll-pane
                 :fit-to-width true
                 :h-box/hgrow :always
                 :content
                 {:fx/type :v-box
                  :padding 20
                  :spacing 16
                  :children (if new-decision
                              ;; New decision form
                              [{:fx/type :label
                                :text "New Decision"
                                :style {:-fx-font-size 18
                                        :-fx-font-weight :bold
                                        :-fx-text-fill (:text-primary colors)}}
                               {:fx/type :v-box
                                :spacing 8
                                :children [{:fx/type :label
                                            :text "Title"
                                            :style {:-fx-text-fill (:text-secondary colors)}}
                                           {:fx/type :text-field
                                            :prompt-text "Decision title..."
                                            :on-text-changed {:event/type ::update-new-decision :field :title}}]}
                               {:fx/type :v-box
                                :spacing 8
                                :children [{:fx/type :label
                                            :text "Description"
                                            :style {:-fx-text-fill (:text-secondary colors)}}
                                           {:fx/type :text-area
                                            :prompt-text "Describe your decision..."
                                            :pref-row-count 4
                                            :on-text-changed {:event/type ::update-new-decision :field :description}}]}
                               {:fx/type :v-box
                                :spacing 8
                                :children [{:fx/type :label
                                            :text "Models Considered"
                                            :style {:-fx-text-fill (:text-secondary colors)}}
                                           {:fx/type :text-field
                                            :prompt-text "Comma-separated model names..."
                                            :on-text-changed {:event/type ::update-new-decision :field :models}}]}
                               {:fx/type :h-box
                                :spacing 12
                                :children [{:fx/type :button
                                            :text "Save Decision"
                                            :on-action {:event/type ::save-decision}
                                            :style {:-fx-background-color (:success colors)
                                                    :-fx-text-fill :white
                                                    :-fx-background-radius 4}}
                                           {:fx/type :button
                                            :text "Cancel"
                                            :on-action {:event/type ::cancel-new-decision}
                                            :style {:-fx-background-color (:text-muted colors)
                                                    :-fx-text-fill :white
                                                    :-fx-background-radius 4}}]}]
                              ;; Selected decision detail
                              (if selected
                                [{:fx/type :label
                                  :text (or (:title selected) "Decision Details")
                                  :style {:-fx-font-size 18
                                          :-fx-font-weight :bold
                                          :-fx-text-fill (:text-primary colors)}}
                                 {:fx/type :label
                                  :text (or (:description selected) "No description")
                                  :wrap-text true
                                  :style {:-fx-text-fill (:text-secondary colors)}}
                                 {:fx/type :v-box
                                  :spacing 8
                                  :children [{:fx/type :label
                                              :text "Models Considered"
                                              :style {:-fx-font-weight :bold
                                                      :-fx-text-fill (:text-primary colors)}}
                                             {:fx/type :flow-pane
                                              :hgap 8
                                              :vgap 8
                                              :children (for [model (or (:models selected) [])]
                                                          {:fx/type :label
                                                           :text (or (:name model) model)
                                                           :style {:-fx-background-color (:bg-tertiary colors)
                                                                   :-fx-padding "4 8"
                                                                   :-fx-background-radius 4
                                                                   :-fx-text-fill (:text-primary colors)}})}]}
                                 {:fx/type :v-box
                                  :spacing 8
                                  :children [{:fx/type :label
                                              :text "Record Outcome"
                                              :style {:-fx-font-weight :bold
                                                      :-fx-text-fill (:text-primary colors)}}
                                             {:fx/type :h-box
                                              :spacing 12
                                              :children [{:fx/type :button
                                                          :text "Correct"
                                                          :on-action {:event/type ::record-outcome :decision selected :outcome "correct"}
                                                          :style {:-fx-background-color (:success colors)
                                                                  :-fx-text-fill :white
                                                                  :-fx-background-radius 4}}
                                                         {:fx/type :button
                                                          :text "Incorrect"
                                                          :on-action {:event/type ::record-outcome :decision selected :outcome "incorrect"}
                                                          :style {:-fx-background-color (:error colors)
                                                                  :-fx-text-fill :white
                                                                  :-fx-background-radius 4}}]}]}]
                                [{:fx/type :v-box
                                  :alignment :center
                                  :v-box/vgrow :always
                                  :children [{:fx/type :label
                                              :text "Select a decision or create a new one"
                                              :style {:-fx-text-fill (:text-muted colors)}}]}]))}}]}))

;; =============================================================================
;; Event Handlers
;; =============================================================================

(defmulti panel-event-handler :event/type)

;; Case Studies events
(defmethod panel-event-handler ::search-case-studies [{:keys [fx/event]}]
  (swap! panel-state assoc-in [:case-studies :search-query] event))

(defmethod panel-event-handler ::filter-case-studies [{:keys [filter]}]
  (swap! panel-state assoc-in [:case-studies :filter] filter))

(defmethod panel-event-handler ::refresh-case-studies [_]
  (fetch-case-studies!))

(defmethod panel-event-handler ::select-case-study [{:keys [case-study]}]
  (swap! panel-state assoc-in [:case-studies :selected] case-study))

;; Signals events
(defmethod panel-event-handler ::filter-signals-source [{:keys [fx/event]}]
  (swap! panel-state assoc-in [:signals :filter :source] (keyword event)))

(defmethod panel-event-handler ::filter-signals-risk [{:keys [fx/event]}]
  (swap! panel-state assoc-in [:signals :filter :risk-level] (keyword event)))

(defmethod panel-event-handler ::refresh-signals [_]
  (fetch-signals!))

(defmethod panel-event-handler ::select-signal [{:keys [signal]}]
  (swap! panel-state assoc-in [:signals :selected] signal))

(defmethod panel-event-handler ::toggle-watchlist [{:keys [signal]}]
  (swap! panel-state update-in [:signals :watchlist]
         (fn [wl]
           (if (some #(= (:id %) (:id signal)) wl)
             (vec (remove #(= (:id %) (:id signal)) wl))
             (conj wl signal)))))

(defmethod panel-event-handler ::mark-signal-reviewed [{:keys [signal]}]
  (api/mark-signal-read (:id signal)))

(defmethod panel-event-handler ::dismiss-signal [{:keys [signal]}]
  (swap! panel-state update-in [:signals :data]
         (fn [data] (vec (remove #(= (:id %) (:id signal)) data))))
  (swap! panel-state assoc-in [:signals :selected] nil))

;; Effectiveness events
(defmethod panel-event-handler ::change-effectiveness-range [{:keys [fx/event]}]
  (swap! panel-state assoc-in [:effectiveness :time-range] (keyword event))
  (fetch-effectiveness!))

(defmethod panel-event-handler ::refresh-effectiveness [_]
  (fetch-effectiveness!))

;; Knowledge Graph events
(defmethod panel-event-handler ::filter-graph-category [{:keys [fx/event]}]
  (swap! panel-state assoc-in [:knowledge-graph :filter-category]
         (if (= event "All Categories") :all event)))

(defmethod panel-event-handler ::zoom-graph [{:keys [direction]}]
  (swap! panel-state update-in [:knowledge-graph :zoom]
         (fn [z] (case direction
                   :in (min 2.0 (* z 1.2))
                   :out (max 0.5 (/ z 1.2))))))

(defmethod panel-event-handler ::refresh-knowledge-graph [_]
  (fetch-knowledge-graph!))

(defmethod panel-event-handler ::select-graph-node [{:keys [node]}]
  (swap! panel-state assoc-in [:knowledge-graph :selected-node] node))

;; Decision events
(defmethod panel-event-handler ::refresh-decisions [_]
  (fetch-decisions!))

(defmethod panel-event-handler ::select-decision [{:keys [decision]}]
  (swap! panel-state assoc-in [:decisions :selected] decision)
  (swap! panel-state assoc-in [:decisions :new-decision] nil))

(defmethod panel-event-handler ::new-decision [_]
  (swap! panel-state assoc-in [:decisions :new-decision] {:title "" :description "" :models ""})
  (swap! panel-state assoc-in [:decisions :selected] nil))

(defmethod panel-event-handler ::update-new-decision [{:keys [field fx/event]}]
  (swap! panel-state assoc-in [:decisions :new-decision field] event))

(defmethod panel-event-handler ::save-decision [_]
  (let [new-dec (:new-decision (:decisions @panel-state))]
    (when (and new-dec (not (str/blank? (:title new-dec))))
      (future
        (let [result (api/create-decision {:title (:title new-dec)
                                           :description (:description new-dec)
                                           :models (str/split (:models new-dec) #",")})]
          (when result
            (fetch-decisions!)
            (swap! panel-state assoc-in [:decisions :new-decision] nil)))))))

(defmethod panel-event-handler ::cancel-new-decision [_]
  (swap! panel-state assoc-in [:decisions :new-decision] nil))

(defmethod panel-event-handler ::record-outcome [{:keys [decision outcome]}]
  (future
    (when (api/update-decision (:id decision) {:outcome outcome})
      (fetch-decisions!))))

(defmethod panel-event-handler :default [event]
  (println "Unhandled panel event:" event))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-panels!
  "Initialize all panels by fetching data"
  []
  (fetch-case-studies!)
  (fetch-signals!)
  (fetch-effectiveness!)
  (fetch-knowledge-graph!)
  (fetch-decisions!))
