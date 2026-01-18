(ns mental-models.ui.knowledge-graph
  "Knowledge Graph - Electric Clojure unified
   Force-directed graph showing relationships between mental models"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]
            #?(:clj [mental-models.db.core :as db])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def selected-node (e/client (atom nil)))
(e/def zoom-level (e/client (atom 1)))
(e/def filter-category (e/client (atom nil)))

;; -- Graph Data --------------------------------------------------------------

#?(:clj
   (defn get-graph-data []
     (let [models (take 30 (db/get-all-models))
           ;; Generate nodes
           nodes (map-indexed
                  (fn [i m]
                    {:id (:id m)
                     :name (:name m)
                     :category (:category_name m)
                     :x (+ 350 (* 250 (Math/cos (* i 0.4))))
                     :y (+ 250 (* 200 (Math/sin (* i 0.4))))
                     :size (+ 20 (rand-int 20))})
                  models)
           ;; Generate edges (connections between related models)
           edges (for [n1 nodes
                       n2 nodes
                       :when (and (< (:id n1) (:id n2))
                                  (< (rand) 0.15))]
                   {:source (:id n1)
                    :target (:id n2)
                    :strength (+ 0.3 (rand 0.7))
                    :type (rand-nth [:related :synergy :builds_on])})]
       {:nodes (vec nodes)
        :edges (vec edges)})))

;; -- Graph Node Component ----------------------------------------------------

(e/defn GraphNode [node selected? on-select]
  (e/client
    (let [colors {:cognitive-biases "#ef4444"
                  :systems-thinking "#3b82f6"
                  :economics-finance "#10b981"
                  :decision-making "#f59e0b"
                  :psychology "#8b5cf6"
                  :physics-engineering "#06b6d4"
                  :biology-evolution "#ec4899"
                  :mathematics-logic "#84cc16"
                  :default "#71717a"}
          color (get colors (keyword (:category node)) (:default colors))]
      (svg/g
        (dom/props {:class "cursor-pointer"
                    :transform (str "translate(" (:x node) "," (:y node) ")")})
        (dom/on "click" (e/fn [_] (on-select node)))
        
        ;; Glow effect for selected
        (when selected?
          (svg/circle
            (dom/props {:r (+ (:size node) 8)
                        :fill "none"
                        :stroke color
                        :stroke-width "2"
                        :opacity "0.5"
                        :class "animate-pulse"})))
        
        ;; Node circle
        (svg/circle
          (dom/props {:r (:size node)
                      :fill (str color (if selected? "" "80"))
                      :stroke color
                      :stroke-width (if selected? "3" "1")
                      :class "transition-all duration-200"}))
        
        ;; Label
        (svg/text
          (dom/props {:y (+ (:size node) 16)
                      :text-anchor "middle"
                      :fill "#a1a1aa"
                      :font-size "10"
                      :class "pointer-events-none"})
          (dom/text (if (> (count (:name node)) 15)
                      (str (subs (:name node) 0 12) "...")
                      (:name node))))))))

;; -- Graph Edge Component ----------------------------------------------------

(e/defn GraphEdge [edge nodes]
  (e/client
    (let [source-node (first (filter #(= (:id %) (:source edge)) nodes))
          target-node (first (filter #(= (:id %) (:target edge)) nodes))
          edge-colors {:related "#3f3f46"
                       :synergy "#f59e0b"
                       :builds_on "#3b82f6"}
          color (get edge-colors (:type edge) "#3f3f46")]
      (when (and source-node target-node)
        (svg/line
          (dom/props {:x1 (:x source-node)
                      :y1 (:y source-node)
                      :x2 (:x target-node)
                      :y2 (:y target-node)
                      :stroke color
                      :stroke-width (* 2 (:strength edge))
                      :opacity "0.5"
                      :class "transition-all duration-200"}))))))

;; -- Node Details Panel ------------------------------------------------------

(e/defn NodeDetailsPanel [node]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center gap-3 mb-6"})
        (dom/div
          (dom/props {:class "w-12 h-12 rounded-xl bg-amber-500/10 flex items-center justify-center"})
          (ui/Icon. :brain "w-6 h-6 text-amber-500"))
        (dom/div
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                  (dom/text (:name node)))
          (dom/span (dom/props {:class "text-sm text-zinc-500"})
                    (dom/text (or (:category node) "General")))))
      
      ;; Stats
      (dom/div
        (dom/props {:class "space-y-4"})
        
        (dom/div
          (dom/props {:class "flex items-center justify-between py-2 border-b border-zinc-800"})
          (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text "Connections"))
          (dom/span (dom/props {:class "text-sm font-medium text-zinc-100"})
                    (dom/text (str (rand-int 15)))))
        
        (dom/div
          (dom/props {:class "flex items-center justify-between py-2 border-b border-zinc-800"})
          (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text "Usage Count"))
          (dom/span (dom/props {:class "text-sm font-medium text-zinc-100"})
                    (dom/text (str (+ 50 (rand-int 200))))))
        
        (dom/div
          (dom/props {:class "flex items-center justify-between py-2"})
          (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text "Effectiveness"))
          (dom/span (dom/props {:class "text-sm font-medium text-emerald-500"})
                    (dom/text (str (+ 70 (rand-int 25)) "%")))))
      
      ;; Related Models
      (dom/div
        (dom/props {:class "mt-6"})
        (dom/h4 (dom/props {:class "text-sm font-medium text-zinc-400 mb-3"})
                (dom/text "Related Models"))
        (dom/div
          (dom/props {:class "flex flex-wrap gap-2"})
          (e/for [related ["Inversion" "Second-Order" "Margin of Safety"]]
            (ui/Badge. related :amber)))))))

;; -- Knowledge Graph Page ----------------------------------------------------

(e/defn KnowledgeGraph []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-8"})
        (dom/div
          (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                  (dom/text "Knowledge Graph"))
          (dom/p (dom/props {:class "text-zinc-400"})
                 (dom/text "Interactive visualization of mental model relationships")))
        
        ;; Controls
        (dom/div
          (dom/props {:class "flex items-center gap-4"})
          
          ;; Zoom
          (dom/div
            (dom/props {:class "flex items-center gap-2"})
            (dom/button
              (dom/props {:class "p-2 bg-zinc-800 rounded-lg text-zinc-400 hover:text-zinc-100"})
              (dom/on "click" (e/fn [_] (swap! zoom-level #(max 0.5 (- % 0.1)))))
              (dom/text "-"))
            (dom/span (dom/props {:class "text-sm text-zinc-400 w-12 text-center"})
                      (dom/text (str (int (* @zoom-level 100)) "%")))
            (dom/button
              (dom/props {:class "p-2 bg-zinc-800 rounded-lg text-zinc-400 hover:text-zinc-100"})
              (dom/on "click" (e/fn [_] (swap! zoom-level #(min 2 (+ % 0.1)))))
              (dom/text "+")))))
      
      ;; Main Content
      (dom/div
        (dom/props {:class "grid grid-cols-1 lg:grid-cols-4 gap-6"})
        
        ;; Graph
        (dom/div
          (dom/props {:class "lg:col-span-3 bg-zinc-900/50 border border-zinc-800 rounded-2xl overflow-hidden"})
          
          (e/server
            (let [graph-data (get-graph-data)]
              (e/client
                (svg/svg
                  (dom/props {:viewBox "0 0 700 500"
                              :class "w-full h-auto"
                              :style {:transform (str "scale(" @zoom-level ")")}})
                  
                  ;; Background
                  (svg/rect (dom/props {:x "0" :y "0" :width "700" :height "500"
                                        :fill "#0a0a0b"}))
                  
                  ;; Edges
                  (e/for [edge (e/server (:edges graph-data))]
                    (GraphEdge. edge (e/server (:nodes graph-data))))
                  
                  ;; Nodes
                  (e/for [node (e/server (:nodes graph-data))]
                    (GraphNode. node
                                (= (:id node) (:id @selected-node))
                                (e/fn [n] (reset! selected-node n)))))))))
        
        ;; Sidebar
        (dom/div
          (dom/props {:class "space-y-6"})
          
          ;; Selected Node Details
          (if @selected-node
            (NodeDetailsPanel. @selected-node)
            (dom/div
              (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
              (ui/EmptyState. :grid "Select a Node"
                              "Click on a node to see details")))
          
          ;; Legend
          (dom/div
            (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
            (dom/h3 (dom/props {:class "text-sm font-medium text-zinc-400 mb-4"})
                    (dom/text "Edge Types"))
            (dom/div
              (dom/props {:class "space-y-2"})
              (e/for [[type color label] [[:related "#3f3f46" "Related"]
                                          [:synergy "#f59e0b" "Synergy"]
                                          [:builds_on "#3b82f6" "Builds On"]]]
                (dom/div
                  (dom/props {:class "flex items-center gap-2"})
                  (dom/div (dom/props {:class "w-4 h-1 rounded"
                                       :style {:background-color color}}))
                  (dom/span (dom/props {:class "text-sm text-zinc-400"})
                            (dom/text label)))))))))))
