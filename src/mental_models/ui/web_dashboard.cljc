(ns mental-models.ui.web-dashboard
  "Electric Clojure Web Dashboard - Real-time news with click-to-analyze
   Design: M&S + Costco - Clean typography, high density, monochrome + red accents"
  #?(:clj (:require [mental-models.news.aggregator :as news]
                    [mental-models.services.llm :as llm]
                    [mental-models.data.models :as models]
                    [mental-models.beast.engine :as beast]
                    [mental-models.mesh.core :as mesh])
     :cljs (:require [goog.string :as gstring]
                     [goog.string.format])))

;; =============================================================================
;; Design System - M&S + Costco
;; =============================================================================

(def colors
  {:background "#FAFAFA"
   :surface "#FFFFFF"
   :text "#1A1A1A"
   :text-secondary "#666666"
   :text-tertiary "#999999"
   :border "#E5E5E5"
   :accent "#CC0000"           ; Costco red
   :accent-light "#FFE5E5"
   :success "#2E7D32"
   :warning "#F57C00"})

(def typography
  {:large-title {:font-size "28px" :font-weight "700"}
   :title {:font-size "22px" :font-weight "600"}
   :headline {:font-size "17px" :font-weight "600"}
   :body {:font-size "15px" :font-weight "400"}
   :callout {:font-size "14px" :font-weight "400"}
   :caption {:font-size "12px" :font-weight "400"}
   :mono {:font-size "13px" :font-weight "500" :font-family "monospace"}})

;; =============================================================================
;; Styles
;; =============================================================================

(def base-styles
  {:container {:max-width "1400px"
               :margin "0 auto"
               :padding "24px"
               :font-family "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif"
               :background (:background colors)
               :min-height "100vh"}
   
   :header {:display "flex"
            :justify-content "space-between"
            :align-items "center"
            :margin-bottom "24px"
            :padding-bottom "16px"
            :border-bottom (str "1px solid " (:border colors))}
   
   :card {:background (:surface colors)
          :border-radius "8px"
          :border (str "1px solid " (:border colors))
          :padding "16px"}
   
   :metric-card {:background (:surface colors)
                 :border-radius "8px"
                 :border (str "1px solid " (:border colors))
                 :padding "16px"
                 :flex "1"}
   
   :news-item {:background (:surface colors)
               :border-bottom (str "1px solid " (:border colors))
               :padding "16px"
               :cursor "pointer"
               :transition "background 0.2s"}
   
   :news-item-hover {:background "#F5F5F5"}
   
   :button-primary {:background (:accent colors)
                    :color "#FFFFFF"
                    :border "none"
                    :border-radius "6px"
                    :padding "8px 16px"
                    :font-size "14px"
                    :font-weight "600"
                    :cursor "pointer"}
   
   :button-secondary {:background "transparent"
                      :color (:text colors)
                      :border (str "1px solid " (:border colors))
                      :border-radius "6px"
                      :padding "8px 16px"
                      :font-size "14px"
                      :cursor "pointer"}
   
   :tag {:display "inline-block"
         :background (:background colors)
         :color (:text-secondary colors)
         :font-size "12px"
         :padding "2px 8px"
         :border-radius "4px"
         :margin-right "4px"}
   
   :alert-badge {:display "inline-flex"
                 :align-items "center"
                 :gap "4px"
                 :padding "4px 8px"
                 :border-radius "4px"
                 :font-size "12px"
                 :font-weight "600"}})

;; =============================================================================
;; Electric Components
;; =============================================================================

#?(:cljs
   (defn e-div [props & children]
     (into [:div props] children)))

#?(:cljs
   (defn e-span [props & children]
     (into [:span props] children)))

;; =============================================================================
;; Dashboard Header
;; =============================================================================

(defn header-component []
  #?(:cljs
     [:div {:style (:header base-styles)}
      [:div
       [:h1 {:style {:font-size "28px" :font-weight "700" :color (:text colors) :margin 0}}
        "Mental Models"]
       [:p {:style {:font-size "14px" :color (:text-secondary colors) :margin "4px 0 0 0"}}
        "Real-time analysis • 129 models • Munger framework"]]
      
      [:div {:style {:display "flex" :gap "12px" :align-items "center"}}
       ;; Status indicator
       [:div {:style {:display "flex" :align-items "center" :gap "8px"}}
        [:div {:style {:width "8px" :height "8px" :border-radius "50%" 
                       :background (:success colors)}}]
        [:span {:style {:font-size "14px" :color (:text-secondary colors)}}
         "System Active"]]
       
       ;; Quick actions
       [:button {:style (:button-secondary base-styles)}
        "Settings"]
       [:button {:style (:button-primary base-styles)}
        "New Analysis"]]]))

;; =============================================================================
;; Metrics Row
;; =============================================================================

(defn metric-card [title value trend]
  #?(:cljs
     [:div {:style (:metric-card base-styles)}
      [:div {:style {:font-size "12px" :color (:text-secondary colors) :margin-bottom "4px"}}
       title]
      [:div {:style {:display "flex" :align-items "baseline" :gap "8px"}}
       [:span {:style {:font-size "24px" :font-weight "600" :color (:text colors)}}
        value]
       (when trend
         [:span {:style {:font-size "12px" 
                         :color (if (clojure.string/starts-with? trend "+")
                                  (:success colors)
                                  (:accent colors))}}
          trend])]]))

(defn metrics-row []
  #?(:cljs
     [:div {:style {:display "flex" :gap "12px" :margin-bottom "24px"}}
      [metric-card "Queue Depth" "1,247" nil]
      [metric-card "CPU Utilization" "87%" "+2%"]
      [metric-card "GPU Utilization" "92%" "+5%"]
      [metric-card "Throughput" "3.2K/min" "+12%"]
      [metric-card "Models Applied" "847" "+47"]
      [metric-card "Lollapaloozas" "12" "+3"]]))

;; =============================================================================
;; News Feed with Click-to-Analyze
;; =============================================================================

(defn news-source-badge [source]
  #?(:cljs
     [:span {:style {:font-size "12px" :color (:text-secondary colors) :font-weight "500"}}
      source]))

(defn time-badge [time]
  #?(:cljs
     [:span {:style {:font-size "12px" :color (:text-tertiary colors)}}
      time]))

(defn analysis-status [analyzed? model-count]
  #?(:cljs
     (if analyzed?
       [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
        [:svg {:width "14" :height "14" :viewBox "0 0 24 24" :fill (:success colors)}
         [:path {:d "M9 16.17L4.83 12l-1.42 1.41L9 19 21 7l-1.41-1.41z"}]]
        [:span {:style {:font-size "12px" :color (:text-secondary colors)}}
         (str model-count " models applied")]]
       [:span {:style {:font-size "12px" :color (:text-tertiary colors)}}
        "Not analyzed"])))

(defn analyze-button [analyzed? on-click]
  #?(:cljs
     [:button {:style (if analyzed?
                        (:button-secondary base-styles)
                        (:button-primary base-styles))
               :on-click on-click}
      (if analyzed? "View Analysis" "Analyze Now")]))

(defn news-item [{:keys [id title source time analyzed? model-count risk-level models-applied]}]
  #?(:cljs
     [:div {:style (:news-item base-styles)
            :on-mouse-enter #(-> % .-target .-style .-background (set! "#F5F5F5"))
            :on-mouse-leave #(-> % .-target .-style .-background (set! "#FFFFFF"))}
      [:div {:style {:display "flex" :justify-content "space-between" :align-items "flex-start"}}
       [:div {:style {:flex 1}}
        ;; Source and time
        [:div {:style {:display "flex" :gap "8px" :align-items "center" :margin-bottom "8px"}}
         [news-source-badge source]
         [:span {:style {:color (:text-tertiary colors)}} "•"]
         [time-badge time]]
        
        ;; Title
        [:h3 {:style {:font-size "16px" :font-weight "500" :color (:text colors) 
                      :margin "0 0 8px 0" :line-height "1.4"}}
         title]
        
        ;; Tags and status
        [:div {:style {:display "flex" :gap "12px" :align-items "center"}}
         [analysis-status analyzed? model-count]
         
         (when (and analyzed? risk-level)
           [:div {:style (merge (:alert-badge base-styles)
                                {:background (case risk-level
                                               :high (:accent-light colors)
                                               :medium "#FFF3E0"
                                               (:background colors))
                                 :color (case risk-level
                                          :high (:accent colors)
                                          :medium (:warning colors)
                                          (:text-secondary colors))})}
            (str "Risk: " (name risk-level))])
         
         (when (seq models-applied)
           [:div {:style {:display "flex" :gap "4px"}}
            (for [model (take 3 models-applied)]
              ^{:key model}
              [:span {:style (:tag base-styles)} model])])]]
       
       ;; Action button
       [:div {:style {:margin-left "16px"}}
        [analyze-button analyzed? #(js/console.log "Analyze" id)]]]]))

(defn news-feed []
  #?(:cljs
     (let [news-items [{:id 1
                        :title "Federal Reserve Signals Potential Rate Cut in Q2 as Inflation Shows Signs of Cooling"
                        :source "Reuters"
                        :time "2 min ago"
                        :analyzed? true
                        :model-count 7
                        :risk-level :medium
                        :models-applied ["Incentives" "Second-Order Effects" "Game Theory"]}
                       {:id 2
                        :title "NVIDIA Reports Record Quarterly Revenue, Beats Expectations on AI Chip Demand"
                        :source "Bloomberg"
                        :time "15 min ago"
                        :analyzed? true
                        :model-count 5
                        :risk-level :low
                        :models-applied ["Moats" "Network Effects" "Scale"]}
                       {:id 3
                        :title "China Announces New Trade Tariffs on US Agricultural Products"
                        :source "Financial Times"
                        :time "1 hour ago"
                        :analyzed? false
                        :model-count 0
                        :risk-level nil
                        :models-applied []}
                       {:id 4
                        :title "Apple Vision Pro Sales Below Initial Expectations, Company Adjusts Production"
                        :source "WSJ"
                        :time "2 hours ago"
                        :analyzed? true
                        :model-count 4
                        :risk-level :medium
                        :models-applied ["Survivorship Bias" "Sunk Cost"]}
                       {:id 5
                        :title "European Central Bank Holds Rates Steady, Signals Caution on Inflation"
                        :source "Reuters"
                        :time "3 hours ago"
                        :analyzed? true
                        :model-count 6
                        :risk-level :low
                        :models-applied ["Feedback Loops" "Equilibrium"]}]]
       
       [:div {:style (merge (:card base-styles) {:padding 0 :overflow "hidden"})}
        ;; Header
        [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"
                       :padding "16px" :border-bottom (str "1px solid " (:border colors))}}
         [:h2 {:style {:font-size "18px" :font-weight "600" :color (:text colors) :margin 0}}
          "Live News Feed"]
         [:div {:style {:display "flex" :gap "8px"}}
          [:select {:style {:border (str "1px solid " (:border colors))
                            :border-radius "6px" :padding "6px 12px" :font-size "14px"}}
           [:option "All Sources"]
           [:option "Bloomberg"]
           [:option "Reuters"]
           [:option "FT"]
           [:option "WSJ"]]
          [:button {:style (:button-secondary base-styles)} "Refresh"]]]
        
        ;; News items
        [:div
         (for [item news-items]
           ^{:key (:id item)}
           [news-item item])]])))

;; =============================================================================
;; Analysis Panel (shown when clicking Analyze)
;; =============================================================================

(defn analysis-panel [{:keys [title source content]}]
  #?(:cljs
     [:div {:style {:position "fixed" :top 0 :right 0 :width "600px" :height "100vh"
                    :background (:surface colors) :box-shadow "-4px 0 20px rgba(0,0,0,0.1)"
                    :overflow-y "auto" :z-index 1000}}
      ;; Header
      [:div {:style {:padding "24px" :border-bottom (str "1px solid " (:border colors))
                     :position "sticky" :top 0 :background (:surface colors)}}
       [:div {:style {:display "flex" :justify-content "space-between" :align-items "flex-start"}}
        [:div
         [:div {:style {:font-size "12px" :color (:text-secondary colors) :margin-bottom "8px"}}
          source]
         [:h2 {:style {:font-size "20px" :font-weight "600" :color (:text colors) :margin 0}}
          title]]
        [:button {:style {:background "none" :border "none" :cursor "pointer" :padding "8px"}}
         "✕"]]]
      
      ;; Analysis content
      [:div {:style {:padding "24px"}}
       ;; Summary
       [:div {:style {:margin-bottom "24px"}}
        [:h3 {:style {:font-size "14px" :font-weight "600" :color (:text colors) 
                      :margin "0 0 12px 0" :text-transform "uppercase" :letter-spacing "0.5px"}}
         "Executive Summary"]
        [:p {:style {:font-size "15px" :color (:text colors) :line-height "1.6"}}
         "Analysis indicates moderate risk with potential second-order effects on related markets. 
          Key mental models suggest watching for incentive misalignment and feedback loops."]]
       
       ;; Models Applied
       [:div {:style {:margin-bottom "24px"}}
        [:h3 {:style {:font-size "14px" :font-weight "600" :color (:text colors) 
                      :margin "0 0 12px 0" :text-transform "uppercase" :letter-spacing "0.5px"}}
         "Mental Models Applied (7)"]
        [:div {:style {:display "grid" :grid-template-columns "1fr 1fr" :gap "8px"}}
         (for [model ["Incentives" "Second-Order Effects" "Game Theory" "Feedback Loops"
                      "Equilibrium" "Margin of Safety" "Inversion"]]
           ^{:key model}
           [:div {:style {:background (:background colors) :padding "12px" :border-radius "6px"}}
            [:div {:style {:font-size "14px" :font-weight "500" :color (:text colors)}}
             model]
            [:div {:style {:font-size "12px" :color (:text-secondary colors) :margin-top "4px"}}
             "High relevance"]])]]
       
       ;; Risk Assessment
       [:div {:style {:margin-bottom "24px"}}
        [:h3 {:style {:font-size "14px" :font-weight "600" :color (:text colors) 
                      :margin "0 0 12px 0" :text-transform "uppercase" :letter-spacing "0.5px"}}
         "Risk Assessment"]
        [:div {:style {:background (:accent-light colors) :padding "16px" :border-radius "8px"
                       :border-left (str "4px solid " (:accent colors))}}
         [:div {:style {:font-size "14px" :font-weight "600" :color (:accent colors) :margin-bottom "8px"}}
          "Medium Risk"]
         [:p {:style {:font-size "14px" :color (:text colors) :margin 0}}
          "Potential for confirmation bias in market reaction. Watch for overreaction in related sectors."]]]
       
       ;; Failure Modes
       [:div {:style {:margin-bottom "24px"}}
        [:h3 {:style {:font-size "14px" :font-weight "600" :color (:text colors) 
                      :margin "0 0 12px 0" :text-transform "uppercase" :letter-spacing "0.5px"}}
         "Potential Failure Modes"]
        [:ul {:style {:margin 0 :padding-left "20px"}}
         [:li {:style {:font-size "14px" :color (:text colors) :margin-bottom "8px"}}
          "Anchoring on current rate expectations"]
         [:li {:style {:font-size "14px" :color (:text colors) :margin-bottom "8px"}}
          "Neglecting base rates of Fed policy changes"]
         [:li {:style {:font-size "14px" :color (:text colors)}}
          "Availability bias from recent market moves"]]]
       
       ;; Second-Order Effects
       [:div {:style {:margin-bottom "24px"}}
        [:h3 {:style {:font-size "14px" :font-weight "600" :color (:text colors) 
                      :margin "0 0 12px 0" :text-transform "uppercase" :letter-spacing "0.5px"}}
         "Second-Order Effects"]
        [:div {:style {:display "flex" :flex-direction "column" :gap "8px"}}
         (for [[effect impact] [["Housing market adjustment" "High"]
                                ["Corporate borrowing costs" "Medium"]
                                ["Currency fluctuations" "Medium"]
                                ["Emerging market flows" "Low"]]]
           ^{:key effect}
           [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"
                          :padding "8px 12px" :background (:background colors) :border-radius "4px"}}
            [:span {:style {:font-size "14px" :color (:text colors)}} effect]
            [:span {:style {:font-size "12px" :font-weight "500"
                            :color (case impact
                                     "High" (:accent colors)
                                     "Medium" (:warning colors)
                                     (:text-secondary colors))}}
             impact]])]]
       
       ;; Actions
       [:div {:style {:display "flex" :gap "12px"}}
        [:button {:style (:button-primary base-styles)} "Save Analysis"]
        [:button {:style (:button-secondary base-styles)} "Export PDF"]
        [:button {:style (:button-secondary base-styles)} "Share"]]]]))

;; =============================================================================
;; Mesh Status Panel
;; =============================================================================

(defn mesh-status-panel []
  #?(:cljs
     [:div {:style (merge (:card base-styles) {:margin-bottom "24px"})}
      [:div {:style {:display "flex" :justify-content "space-between" :align-items "center" :margin-bottom "16px"}}
       [:h2 {:style {:font-size "18px" :font-weight "600" :color (:text colors) :margin 0}}
        "Compute Mesh"]
       [:div {:style {:display "flex" :align-items "center" :gap "8px"}}
        [:div {:style {:width "8px" :height "8px" :border-radius "50%" :background (:success colors)}}]
        [:span {:style {:font-size "14px" :color (:success colors)}} "Healthy"]]]
      
      ;; Device grid
      [:div {:style {:display "grid" :grid-template-columns "repeat(4, 1fr)" :gap "12px"}}
       (for [{:keys [name type cpu gpu status]} [{:name "MacBook Pro" :type "Desktop" :cpu 87 :gpu 92 :status :active}
                                                  {:name "iPhone 15 Pro" :type "Mobile" :cpu 23 :gpu nil :status :active}
                                                  {:name "Apple Watch" :type "Watch" :cpu 5 :gpu nil :status :idle}
                                                  {:name "Home Server" :type "Server" :cpu 95 :gpu 88 :status :active}]]
         ^{:key name}
         [:div {:style {:background (:background colors) :padding "12px" :border-radius "6px"}}
          [:div {:style {:display "flex" :justify-content "space-between" :align-items "center" :margin-bottom "8px"}}
           [:span {:style {:font-size "14px" :font-weight "500" :color (:text colors)}} name]
           [:div {:style {:width "6px" :height "6px" :border-radius "50%"
                          :background (case status :active (:success colors) :idle (:warning colors) (:text-tertiary colors))}}]]
          [:div {:style {:font-size "12px" :color (:text-secondary colors) :margin-bottom "8px"}} type]
          [:div {:style {:display "flex" :gap "8px"}}
           [:span {:style {:font-size "13px" :font-family "monospace" :color (:text colors)}}
            (str "CPU " cpu "%")]
           (when gpu
             [:span {:style {:font-size "13px" :font-family "monospace" :color (:accent colors)}}
              (str "GPU " gpu "%")])]])]]))

;; =============================================================================
;; Main Dashboard
;; =============================================================================

(defn main-dashboard []
  #?(:cljs
     [:div {:style (:container base-styles)}
      [header-component]
      [metrics-row]
      
      [:div {:style {:display "grid" :grid-template-columns "2fr 1fr" :gap "24px"}}
       ;; Left column - News
       [:div
        [news-feed]]
       
       ;; Right column - Status panels
       [:div
        [mesh-status-panel]
        
        ;; Lollapalooza alerts
        [:div {:style (merge (:card base-styles) {:background (:accent-light colors)
                                                   :border-color (str (:accent colors) "33")})}
         [:div {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "12px"}}
          [:span {:style {:font-size "18px"}} "⭐"]
          [:h3 {:style {:font-size "16px" :font-weight "600" :color (:text colors) :margin 0}}
           "Lollapalooza Detector"]]
         [:p {:style {:font-size "14px" :color (:text-secondary colors) :margin "0 0 12px 0"}}
          "5 models converging on NVDA analysis"]
         [:button {:style (:button-primary base-styles)} "View Details"]]]]]))

;; =============================================================================
;; Entry Point
;; =============================================================================

(defn ^:export init []
  #?(:cljs
     (js/console.log "Mental Models Dashboard initialized")))
