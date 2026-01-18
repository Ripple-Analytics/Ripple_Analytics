(ns mental-models.ui.components
  "Reusable UI components - Electric Clojure unified client/server"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]))

;; -- Design Tokens -----------------------------------------------------------

(def colors
  {:bg-primary "bg-zinc-950"
   :bg-secondary "bg-zinc-900"
   :bg-card "bg-zinc-900/50"
   :text-primary "text-zinc-100"
   :text-secondary "text-zinc-400"
   :accent "text-amber-500"
   :accent-bg "bg-amber-500"
   :border "border-zinc-800"})

;; -- Icons (SVG) -------------------------------------------------------------

(e/defn Icon [name & [class]]
  (e/client
    (let [icons {:home "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
                 :grid "M4 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2V6zM14 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V6zM4 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2v-2zM14 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"
                 :brain "M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"
                 :chart "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
                 :book "M12 6.253v13m0-13C10.832 5.477 9.246 5 7.5 5S4.168 5.477 3 6.253v13C4.168 18.477 5.754 18 7.5 18s3.332.477 4.5 1.253m0-13C13.168 5.477 14.754 5 16.5 5c1.747 0 3.332.477 4.5 1.253v13C19.832 18.477 18.247 18 16.5 18c-1.746 0-3.332.477-4.5 1.253"
                 :globe "M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                 :lightning "M13 10V3L4 14h7v7l9-11h-7z"
                 :warning "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                 :cog "M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z M15 12a3 3 0 11-6 0 3 3 0 016 0z"
                 :search "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
                 :plus "M12 4v16m8-8H4"
                 :check "M5 13l4 4L19 7"
                 :x "M6 18L18 6M6 6l12 12"
                 :arrow-right "M14 5l7 7m0 0l-7 7m7-7H3"
                 :trending-up "M13 7h8m0 0v8m0-8l-8 8-4-4-6 6"
                 :trending-down "M13 17h8m0 0V9m0 8l-8-8-4 4-6-6"}]
      (svg/svg
        (dom/props {:class (or class "w-5 h-5")
                    :fill "none"
                    :viewBox "0 0 24 24"
                    :stroke "currentColor"
                    :stroke-width "2"})
        (svg/path (dom/props {:stroke-linecap "round"
                              :stroke-linejoin "round"
                              :d (get icons name "")}))))))

;; -- Sidebar Navigation ------------------------------------------------------

(e/defn NavItem [icon label page current-route navigate!]
  (e/client
    (let [active? (= page (:page @current-route))]
      (dom/button
        (dom/props {:class (str "w-full flex items-center gap-3 px-4 py-3 rounded-xl "
                                "transition-all duration-200 "
                                (if active?
                                  "bg-amber-500/10 text-amber-500"
                                  "text-zinc-400 hover:text-zinc-100 hover:bg-zinc-800/50"))})
        (dom/on "click" (e/fn [_] (navigate! page)))
        (Icon. icon "w-5 h-5")
        (dom/span (dom/text label))))))

(e/defn Sidebar [current-route navigate!]
  (e/client
    (dom/aside
      (dom/props {:class "fixed left-0 top-0 h-screen w-64 bg-zinc-900/50 border-r border-zinc-800 p-6"})
      
      ;; Logo
      (dom/div
        (dom/props {:class "flex items-center gap-3 mb-10"})
        (dom/div
          (dom/props {:class "w-10 h-10 bg-gradient-to-br from-amber-500 to-orange-600 rounded-xl flex items-center justify-center"})
          (Icon. :brain "w-6 h-6 text-white"))
        (dom/div
          (dom/span (dom/props {:class "text-lg font-semibold text-zinc-100"})
                    (dom/text "Mental Models"))
          (dom/span (dom/props {:class "text-xs text-zinc-500 block"})
                    (dom/text "Electric Edition"))))
      
      ;; Navigation Groups
      (dom/nav
        (dom/props {:class "space-y-8"})
        
        ;; Core
        (dom/div
          (dom/p (dom/props {:class "text-xs font-medium text-zinc-500 uppercase tracking-wider mb-3 px-4"})
                 (dom/text "Core"))
          (NavItem. :home "Dashboard" :dashboard current-route navigate!)
          (NavItem. :grid "Models" :models current-route navigate!)
          (NavItem. :book "Decisions" :decisions current-route navigate!))
        
        ;; Analysis
        (dom/div
          (dom/p (dom/props {:class "text-xs font-medium text-zinc-500 uppercase tracking-wider mb-3 px-4"})
                 (dom/text "Analysis"))
          (NavItem. :lightning "Lollapalooza" :lollapalooza current-route navigate!)
          (NavItem. :warning "Failure Modes" :failure-modes current-route navigate!)
          (NavItem. :chart "Effectiveness" :effectiveness current-route navigate!)
          (NavItem. :brain "Analysis" :analysis current-route navigate!))
        
        ;; Visualization
        (dom/div
          (dom/p (dom/props {:class "text-xs font-medium text-zinc-500 uppercase tracking-wider mb-3 px-4"})
                 (dom/text "Visualization"))
          (NavItem. :globe "World Map" :world-map current-route navigate!)
          (NavItem. :grid "Knowledge Graph" :knowledge-graph current-route navigate!))
        
        ;; System
        (dom/div
          (dom/p (dom/props {:class "text-xs font-medium text-zinc-500 uppercase tracking-wider mb-3 px-4"})
                 (dom/text "System"))
          (NavItem. :chart "Metrics" :metrics current-route navigate!)
          (NavItem. :cog "Statistics" :statistics current-route navigate!))))))

;; -- Card Components ---------------------------------------------------------

(e/defn Card [& children]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 backdrop-blur-sm"})
      (e/for [child children]
        child))))

(e/defn StatCard [label value change trend]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-4"})
        (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text label))
        (dom/span
          (dom/props {:class (str "text-xs px-2 py-1 rounded-full "
                                  (if (= trend :up)
                                    "bg-emerald-500/10 text-emerald-500"
                                    "bg-red-500/10 text-red-500"))})
          (dom/text change)))
      (dom/div
        (dom/props {:class "text-3xl font-bold text-zinc-100"})
        (dom/text value)))))

;; -- Sparkline ---------------------------------------------------------------

(e/defn Sparkline [data & [color]]
  (e/client
    (let [width 100
          height 30
          max-val (apply max data)
          min-val (apply min data)
          range (- max-val min-val)
          points (map-indexed
                  (fn [i v]
                    (let [x (* (/ i (dec (count data))) width)
                          y (- height (* (/ (- v min-val) (if (zero? range) 1 range)) height))]
                      (str x "," y)))
                  data)]
      (svg/svg
        (dom/props {:class "w-full h-8"
                    :viewBox (str "0 0 " width " " height)
                    :preserveAspectRatio "none"})
        (svg/polyline
          (dom/props {:fill "none"
                      :stroke (or color "#f59e0b")
                      :stroke-width "2"
                      :points (clojure.string/join " " points)}))))))

;; -- Button ------------------------------------------------------------------

(e/defn Button [label on-click & [variant]]
  (e/client
    (dom/button
      (dom/props {:class (str "px-4 py-2 rounded-xl font-medium transition-all duration-200 "
                              (case variant
                                :primary "bg-amber-500 text-zinc-900 hover:bg-amber-400"
                                :secondary "bg-zinc-800 text-zinc-100 hover:bg-zinc-700"
                                :ghost "text-zinc-400 hover:text-zinc-100 hover:bg-zinc-800/50"
                                "bg-amber-500 text-zinc-900 hover:bg-amber-400"))})
      (dom/on "click" on-click)
      (dom/text label))))

;; -- Input -------------------------------------------------------------------

(e/defn Input [placeholder value on-change & [type]]
  (e/client
    (dom/input
      (dom/props {:class "w-full bg-zinc-800/50 border border-zinc-700 rounded-xl px-4 py-3 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 transition-colors"
                  :type (or type "text")
                  :placeholder placeholder
                  :value value})
      (dom/on "input" (e/fn [e] (on-change (.. e -target -value)))))))

;; -- Search Box --------------------------------------------------------------

(e/defn SearchBox [placeholder value on-change]
  (e/client
    (dom/div
      (dom/props {:class "relative"})
      (dom/div
        (dom/props {:class "absolute left-4 top-1/2 -translate-y-1/2 text-zinc-500"})
        (Icon. :search "w-5 h-5"))
      (dom/input
        (dom/props {:class "w-full bg-zinc-800/50 border border-zinc-700 rounded-xl pl-12 pr-4 py-3 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 transition-colors"
                    :type "text"
                    :placeholder placeholder
                    :value value})
        (dom/on "input" (e/fn [e] (on-change (.. e -target -value))))))))

;; -- Badge -------------------------------------------------------------------

(e/defn Badge [label & [color]]
  (e/client
    (dom/span
      (dom/props {:class (str "px-2 py-1 rounded-full text-xs font-medium "
                              (case color
                                :red "bg-red-500/10 text-red-500"
                                :green "bg-emerald-500/10 text-emerald-500"
                                :blue "bg-blue-500/10 text-blue-500"
                                :amber "bg-amber-500/10 text-amber-500"
                                :purple "bg-purple-500/10 text-purple-500"
                                "bg-zinc-700 text-zinc-300"))})
      (dom/text label))))

;; -- Loading Spinner ---------------------------------------------------------

(e/defn Spinner []
  (e/client
    (dom/div
      (dom/props {:class "animate-spin w-6 h-6 border-2 border-zinc-700 border-t-amber-500 rounded-full"}))))

;; -- Empty State -------------------------------------------------------------

(e/defn EmptyState [icon title description]
  (e/client
    (dom/div
      (dom/props {:class "flex flex-col items-center justify-center py-16 text-center"})
      (dom/div
        (dom/props {:class "w-16 h-16 bg-zinc-800 rounded-2xl flex items-center justify-center mb-4"})
        (Icon. icon "w-8 h-8 text-zinc-500"))
      (dom/h3 (dom/props {:class "text-lg font-medium text-zinc-300 mb-2"}) (dom/text title))
      (dom/p (dom/props {:class "text-sm text-zinc-500 max-w-sm"}) (dom/text description)))))

;; -- World Map Placeholder ---------------------------------------------------

(e/defn WorldMap []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-8"})
              (dom/text "World Map"))
      (Card.
        (EmptyState. :globe "World Map Visualization"
                     "Spatial-temporal visualization of mental model applications across the globe")))))

;; -- Knowledge Graph Placeholder ---------------------------------------------

(e/defn KnowledgeGraph []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-8"})
              (dom/text "Knowledge Graph"))
      (Card.
        (EmptyState. :grid "Knowledge Graph"
                     "Interactive force-directed graph showing relationships between mental models")))))

;; -- Signal Harvester Placeholder --------------------------------------------

(e/defn SignalHarvester []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-8"})
              (dom/text "Signal Harvester"))
      (Card.
        (EmptyState. :lightning "Signal Harvester"
                     "Real-time signal detection from news, markets, and social media")))))

;; -- Connector Hub Placeholder -----------------------------------------------

(e/defn ConnectorHub []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-8"})
              (dom/text "Connector Hub"))
      (Card.
        (EmptyState. :cog "Connector Hub"
                     "Integration hub for external services and data sources")))))
