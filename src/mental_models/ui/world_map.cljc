(ns mental-models.ui.world-map
  "World Map - Electric Clojure unified
   Spatial-temporal visualization of mental model applications"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]
            #?(:clj [mental-models.db.core :as db])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def selected-region (e/client (atom nil)))
(e/def time-range (e/client (atom :all)))
(e/def heatmap-data (e/client (atom {})))

;; -- World Map SVG Paths -----------------------------------------------------

(def world-regions
  {:north-america {:name "North America" :path "M 50,80 L 180,60 L 200,150 L 120,200 L 40,180 Z" :color "#f59e0b"}
   :south-america {:name "South America" :path "M 120,200 L 180,190 L 200,350 L 140,400 L 100,300 Z" :color "#10b981"}
   :europe {:name "Europe" :path "M 280,60 L 380,50 L 400,120 L 320,150 L 260,120 Z" :color "#3b82f6"}
   :africa {:name "Africa" :path "M 280,150 L 380,140 L 400,300 L 320,350 L 260,280 Z" :color "#8b5cf6"}
   :asia {:name "Asia" :path "M 400,50 L 600,40 L 650,200 L 500,250 L 380,150 Z" :color "#ef4444"}
   :oceania {:name "Oceania" :path "M 550,280 L 650,260 L 680,350 L 600,380 L 540,340 Z" :color "#06b6d4"}})

;; -- Server Data Functions ---------------------------------------------------

#?(:clj
   (defn get-regional-stats []
     {:north-america {:decisions 1247 :models-used 3421 :effectiveness 78.3 :top-model "Second-Order Thinking"}
      :south-america {:decisions 342 :models-used 891 :effectiveness 72.1 :top-model "Inversion"}
      :europe {:decisions 2134 :models-used 5672 :effectiveness 81.2 :top-model "First Principles"}
      :asia {:decisions 3421 :models-used 8934 :effectiveness 79.8 :top-model "Margin of Safety"}
      :africa {:decisions 234 :models-used 612 :effectiveness 74.5 :top-model "Circle of Competence"}
      :oceania {:decisions 567 :models-used 1423 :effectiveness 76.9 :top-model "Opportunity Cost"}}))

#?(:clj
   (defn get-temporal-data []
     {:daily (vec (for [i (range 30)] {:day i :decisions (+ 50 (rand-int 100)) :effectiveness (+ 70 (rand-int 15))}))
      :weekly (vec (for [i (range 12)] {:week i :decisions (+ 300 (rand-int 500)) :effectiveness (+ 72 (rand-int 12))}))
      :monthly (vec (for [i (range 12)] {:month i :decisions (+ 1200 (rand-int 2000)) :effectiveness (+ 74 (rand-int 10))}))}))

;; -- Region Component --------------------------------------------------------

(e/defn MapRegion [region-key region-data stats on-select selected?]
  (e/client
    (svg/path
      (dom/props {:d (:path region-data)
                  :fill (if selected? (:color region-data) (str (:color region-data) "40"))
                  :stroke (:color region-data)
                  :stroke-width (if selected? "3" "1")
                  :class "cursor-pointer transition-all duration-200 hover:opacity-80"})
      (dom/on "click" (e/fn [_] (on-select region-key)))
      (dom/on "mouseenter" (e/fn [_] nil))
      (dom/on "mouseleave" (e/fn [_] nil)))))

;; -- Stats Panel -------------------------------------------------------------

(e/defn RegionStatsPanel [region-key stats]
  (e/client
    (let [region (get world-regions region-key)
          data (get stats region-key)]
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
        
        ;; Header
        (dom/div
          (dom/props {:class "flex items-center gap-3 mb-6"})
          (dom/div
            (dom/props {:class "w-4 h-4 rounded-full"
                        :style {:background-color (:color region)}}))
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                  (dom/text (:name region))))
        
        ;; Stats Grid
        (dom/div
          (dom/props {:class "grid grid-cols-2 gap-4"})
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
            (dom/span (dom/props {:class "text-2xl font-bold text-zinc-100 block"})
                      (dom/text (str (:decisions data))))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text "Decisions")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
            (dom/span (dom/props {:class "text-2xl font-bold text-zinc-100 block"})
                      (dom/text (str (:models-used data))))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text "Models Used")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
            (dom/span (dom/props {:class "text-2xl font-bold text-emerald-500 block"})
                      (dom/text (str (:effectiveness data) "%")))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text "Effectiveness")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
            (dom/span (dom/props {:class "text-sm font-medium text-amber-500 block"})
                      (dom/text (:top-model data)))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text "Top Model"))))))))

;; -- World Map Page ----------------------------------------------------------

(e/defn WorldMap []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-8"})
        (dom/div
          (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                  (dom/text "World Map"))
          (dom/p (dom/props {:class "text-zinc-400"})
                 (dom/text "Spatial-temporal visualization of mental model applications")))
        
        ;; Time Range Filter
        (dom/div
          (dom/props {:class "flex items-center gap-2 bg-zinc-800 rounded-xl p-1"})
          (e/for [[key label] [[:all "All Time"] [:month "This Month"] [:week "This Week"]]]
            (dom/button
              (dom/props {:class (str "px-4 py-2 rounded-lg text-sm font-medium transition-all "
                                      (if (= @time-range key)
                                        "bg-amber-500 text-zinc-900"
                                        "text-zinc-400 hover:text-zinc-100"))})
              (dom/on "click" (e/fn [_] (reset! time-range key)))
              (dom/text label)))))
      
      ;; Main Content
      (dom/div
        (dom/props {:class "grid grid-cols-1 lg:grid-cols-3 gap-6"})
        
        ;; Map
        (dom/div
          (dom/props {:class "lg:col-span-2 bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
          
          (svg/svg
            (dom/props {:viewBox "0 0 700 420"
                        :class "w-full h-auto"})
            
            ;; Background
            (svg/rect (dom/props {:x "0" :y "0" :width "700" :height "420"
                                  :fill "#18181b" :rx "12"}))
            
            ;; Grid lines
            (e/for [i (range 1 7)]
              (svg/line (dom/props {:x1 (* i 100) :y1 "0" :x2 (* i 100) :y2 "420"
                                    :stroke "#27272a" :stroke-width "1"})))
            (e/for [i (range 1 5)]
              (svg/line (dom/props {:x1 "0" :y1 (* i 100) :x2 "700" :y2 (* i 100)
                                    :stroke "#27272a" :stroke-width "1"})))
            
            ;; Regions
            (e/server
              (let [stats (get-regional-stats)]
                (e/client
                  (e/for [[region-key region-data] world-regions]
                    (MapRegion. region-key region-data stats
                                (e/fn [k] (reset! selected-region k))
                                (= @selected-region region-key))))))))
        
        ;; Stats Panel
        (dom/div
          (dom/props {:class "space-y-6"})
          
          (e/server
            (let [stats (get-regional-stats)]
              (e/client
                (if @selected-region
                  (RegionStatsPanel. @selected-region stats)
                  (dom/div
                    (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                    (ui/EmptyState. :globe "Select a Region"
                                    "Click on a region to see detailed statistics"))))))
          
          ;; Global Stats
          (e/server
            (let [stats (get-regional-stats)
                  total-decisions (reduce + (map :decisions (vals stats)))
                  avg-effectiveness (/ (reduce + (map :effectiveness (vals stats))) (count stats))]
              (e/client
                (dom/div
                  (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                  (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                          (dom/text "Global Overview"))
                  (dom/div
                    (dom/props {:class "space-y-3"})
                    (dom/div
                      (dom/props {:class "flex items-center justify-between"})
                      (dom/span (dom/props {:class "text-zinc-400"}) (dom/text "Total Decisions"))
                      (dom/span (dom/props {:class "text-zinc-100 font-medium"})
                                (dom/text (e/server (str total-decisions)))))
                    (dom/div
                      (dom/props {:class "flex items-center justify-between"})
                      (dom/span (dom/props {:class "text-zinc-400"}) (dom/text "Avg Effectiveness"))
                      (dom/span (dom/props {:class "text-emerald-500 font-medium"})
                                (dom/text (e/server (str (int avg-effectiveness) "%")))))
                    (dom/div
                      (dom/props {:class "flex items-center justify-between"})
                      (dom/span (dom/props {:class "text-zinc-400"}) (dom/text "Active Regions"))
                      (dom/span (dom/props {:class "text-zinc-100 font-medium"})
                                (dom/text "6"))))))))))))
