(ns mental-models.ui.models
  "Models Explorer & Detail - Electric Clojure unified
   Live search with automatic server queries"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.services.llm :as llm])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def search-query (e/client (atom "")))
(e/def selected-category (e/client (atom nil)))
(e/def view-mode (e/client (atom :grid)))

;; -- Server Data Functions ---------------------------------------------------

#?(:clj
   (defn search-models-server [query category]
     (cond
       (and query (not (empty? query)))
       (db/search-models query)
       
       category
       (db/get-models-by-category category)
       
       :else
       (db/get-all-models))))

#?(:clj
   (defn get-model-detail [id]
     (when id
       (let [model (db/get-model-by-id id)
             failure-modes (db/get-failure-modes id)
             effectiveness (db/get-model-effectiveness id)]
         (assoc model
                :failure-modes failure-modes
                :effectiveness effectiveness)))))

;; -- Model Card Component ----------------------------------------------------

(e/defn ModelCard [model on-click]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-amber-500/30 transition-all duration-200 cursor-pointer group"})
      (dom/on "click" (e/fn [_] (on-click model)))
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-start justify-between mb-4"})
        (dom/div
          (dom/props {:class "w-12 h-12 rounded-xl bg-amber-500/10 flex items-center justify-center group-hover:bg-amber-500/20 transition-colors"})
          (ui/Icon. :brain "w-6 h-6 text-amber-500"))
        (dom/div
          (dom/props {:class "flex items-center gap-2"})
          (e/for [_ (range (or (:complexity model) 2))]
            (dom/div (dom/props {:class "w-2 h-2 rounded-full bg-amber-500"})))))
      
      ;; Content
      (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2 group-hover:text-amber-500 transition-colors"})
              (dom/text (:name model)))
      
      (dom/p (dom/props {:class "text-sm text-zinc-400 line-clamp-3 mb-4"})
             (dom/text (or (:description model) "")))
      
      ;; Footer
      (dom/div
        (dom/props {:class "flex items-center justify-between pt-4 border-t border-zinc-800"})
        (dom/span (dom/props {:class "text-xs text-zinc-500"})
                  (dom/text (or (:thinker model) "Unknown")))
        (ui/Badge. (or (:category_name model) "General") :amber)))))

;; -- Model List Row ----------------------------------------------------------

(e/defn ModelRow [model on-click]
  (e/client
    (dom/div
      (dom/props {:class "flex items-center gap-4 p-4 bg-zinc-900/50 border border-zinc-800 rounded-xl hover:border-amber-500/30 transition-all cursor-pointer"})
      (dom/on "click" (e/fn [_] (on-click model)))
      
      (dom/div
        (dom/props {:class "w-10 h-10 rounded-lg bg-amber-500/10 flex items-center justify-center"})
        (ui/Icon. :brain "w-5 h-5 text-amber-500"))
      
      (dom/div
        (dom/props {:class "flex-1 min-w-0"})
        (dom/span (dom/props {:class "text-zinc-100 font-medium block truncate"})
                  (dom/text (:name model)))
        (dom/span (dom/props {:class "text-xs text-zinc-500 truncate block"})
                  (dom/text (or (:description model) ""))))
      
      (dom/div
        (dom/props {:class "flex items-center gap-4"})
        (ui/Badge. (or (:category_name model) "General") :amber)
        (dom/span (dom/props {:class "text-xs text-zinc-500"})
                  (dom/text (or (:thinker model) "")))))))

;; -- Category Filter ---------------------------------------------------------

(e/defn CategoryFilter [categories selected on-select]
  (e/client
    (dom/div
      (dom/props {:class "flex flex-wrap gap-2 mb-6"})
      
      ;; All button
      (dom/button
        (dom/props {:class (str "px-4 py-2 rounded-xl text-sm font-medium transition-all "
                                (if (nil? @selected)
                                  "bg-amber-500 text-zinc-900"
                                  "bg-zinc-800 text-zinc-400 hover:text-zinc-100"))})
        (dom/on "click" (e/fn [_] (on-select nil)))
        (dom/text "All"))
      
      ;; Category buttons
      (e/for [cat categories]
        (dom/button
          (dom/props {:class (str "px-4 py-2 rounded-xl text-sm font-medium transition-all "
                                  (if (= (:id cat) @selected)
                                    "bg-amber-500 text-zinc-900"
                                    "bg-zinc-800 text-zinc-400 hover:text-zinc-100"))})
          (dom/on "click" (e/fn [_] (on-select (:id cat))))
          (dom/text (:name cat)))))))

;; -- Models Explorer Page ----------------------------------------------------

(e/defn ModelsExplorer []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-8"})
        (dom/div
          (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                  (dom/text "Mental Models"))
          (dom/p (dom/props {:class "text-zinc-400"})
                 (dom/text "Explore Charlie Munger's latticework of mental models")))
        
        ;; View Toggle
        (dom/div
          (dom/props {:class "flex items-center gap-2 bg-zinc-800 rounded-xl p-1"})
          (dom/button
            (dom/props {:class (str "p-2 rounded-lg transition-colors "
                                    (if (= @view-mode :grid)
                                      "bg-zinc-700 text-zinc-100"
                                      "text-zinc-500 hover:text-zinc-300"))})
            (dom/on "click" (e/fn [_] (reset! view-mode :grid)))
            (ui/Icon. :grid "w-5 h-5"))
          (dom/button
            (dom/props {:class (str "p-2 rounded-lg transition-colors "
                                    (if (= @view-mode :list)
                                      "bg-zinc-700 text-zinc-100"
                                      "text-zinc-500 hover:text-zinc-300"))})
            (dom/on "click" (e/fn [_] (reset! view-mode :list)))
            (ui/Icon. :book "w-5 h-5")))))
      
      ;; Search
      (dom/div
        (dom/props {:class "mb-6"})
        (ui/SearchBox. "Search models by name, description, or thinker..."
                       @search-query
                       (e/fn [v] (reset! search-query v))))
      
      ;; Category Filter
      (e/server
        (let [categories (db/get-categories)]
          (e/client
            (CategoryFilter. categories selected-category
                             (e/fn [id] (reset! selected-category id))))))
      
      ;; Models Grid/List - Live search from server
      (e/server
        (let [query (e/client @search-query)
              category (e/client @selected-category)
              models (search-models-server query category)]
          (e/client
            (let [mode @view-mode]
              (if (= mode :grid)
                ;; Grid View
                (dom/div
                  (dom/props {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"})
                  (e/for [model models]
                    (ModelCard. model (e/fn [m] (println "Selected:" (:name m))))))
                
                ;; List View
                (dom/div
                  (dom/props {:class "space-y-3"})
                  (e/for [model models]
                    (ModelRow. model (e/fn [m] (println "Selected:" (:name m)))))))))))))

;; -- Model Detail Page -------------------------------------------------------

(e/defn ModelDetail [model-id]
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      (e/server
        (let [model (get-model-detail model-id)]
          (e/client
            (if model
              (dom/div
                ;; Back Button
                (dom/button
                  (dom/props {:class "flex items-center gap-2 text-zinc-400 hover:text-zinc-100 mb-8 transition-colors"})
                  (ui/Icon. :arrow-right "w-5 h-5 rotate-180")
                  (dom/text "Back to Models"))
                
                ;; Header
                (dom/div
                  (dom/props {:class "flex items-start gap-6 mb-8"})
                  (dom/div
                    (dom/props {:class "w-20 h-20 rounded-2xl bg-amber-500/10 flex items-center justify-center"})
                    (ui/Icon. :brain "w-10 h-10 text-amber-500"))
                  (dom/div
                    (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                            (dom/text (e/server (:name model))))
                    (dom/div
                      (dom/props {:class "flex items-center gap-4"})
                      (ui/Badge. (e/server (or (:category_name model) "General")) :amber)
                      (dom/span (dom/props {:class "text-zinc-400"})
                                (dom/text (str "by " (e/server (or (:thinker model) "Unknown"))))))))
                
                ;; Content Grid
                (dom/div
                  (dom/props {:class "grid grid-cols-1 lg:grid-cols-3 gap-6"})
                  
                  ;; Main Content
                  (dom/div
                    (dom/props {:class "lg:col-span-2 space-y-6"})
                    
                    ;; Description
                    (dom/div
                      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                      (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                              (dom/text "Description"))
                      (dom/p (dom/props {:class "text-zinc-400 leading-relaxed"})
                             (dom/text (e/server (or (:description model) "")))))
                    
                    ;; Detailed Explanation
                    (dom/div
                      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                      (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                              (dom/text "Detailed Explanation"))
                      (dom/p (dom/props {:class "text-zinc-400 leading-relaxed"})
                             (dom/text (e/server (or (:detailed_explanation model) ""))))))
                  
                  ;; Sidebar
                  (dom/div
                    (dom/props {:class "space-y-6"})
                    
                    ;; Effectiveness
                    (dom/div
                      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                      (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                              (dom/text "Effectiveness"))
                      (let [eff (e/server (:effectiveness model))]
                        (dom/div
                          (dom/props {:class "text-center"})
                          (dom/div (dom/props {:class "text-4xl font-bold text-amber-500 mb-2"})
                                   (dom/text (str (e/server (or (:successes eff) 0)) "/" (e/server (or (:total eff) 0)))))
                          (dom/span (dom/props {:class "text-sm text-zinc-500"})
                                    (dom/text "Success Rate")))))
                    
                    ;; Complexity
                    (dom/div
                      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
                      (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                              (dom/text "Complexity"))
                      (dom/div
                        (dom/props {:class "flex items-center gap-2"})
                        (e/for [_ (range (e/server (or (:complexity model) 2)))]
                          (dom/div (dom/props {:class "w-4 h-4 rounded-full bg-amber-500"})))
                        (e/for [_ (range (- 5 (e/server (or (:complexity model) 2))))]
                          (dom/div (dom/props {:class "w-4 h-4 rounded-full bg-zinc-700"}))))))))
              
              ;; Loading/Not Found
              (ui/EmptyState. :brain "Model Not Found"
                              "The requested mental model could not be found"))))))))

;; -- Failure Modes Page ------------------------------------------------------

(e/defn FailureModeCard [failure-mode]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
      
      (dom/div
        (dom/props {:class "flex items-start justify-between mb-4"})
        (dom/div
          (dom/props {:class (str "w-10 h-10 rounded-xl flex items-center justify-center "
                                  (case (:risk_level failure-mode)
                                    "high" "bg-red-500/10"
                                    "medium" "bg-amber-500/10"
                                    "bg-emerald-500/10"))})
          (ui/Icon. :warning (str "w-5 h-5 "
                                  (case (:risk_level failure-mode)
                                    "high" "text-red-500"
                                    "medium" "text-amber-500"
                                    "text-emerald-500"))))
        (ui/Badge. (or (:risk_level failure-mode) "low")
                   (case (:risk_level failure-mode)
                     "high" :red
                     "medium" :amber
                     :green)))
      
      (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
              (dom/text (:name failure-mode)))
      
      (dom/p (dom/props {:class "text-sm text-zinc-400 mb-4"})
             (dom/text (or (:description failure-mode) "")))
      
      (dom/div
        (dom/props {:class "space-y-3 pt-4 border-t border-zinc-800"})
        
        (when (:triggers failure-mode)
          (dom/div
            (dom/span (dom/props {:class "text-xs text-zinc-500 block mb-1"})
                      (dom/text "Triggers"))
            (dom/p (dom/props {:class "text-sm text-zinc-400"})
                   (dom/text (:triggers failure-mode)))))
        
        (when (:safeguard failure-mode)
          (dom/div
            (dom/span (dom/props {:class "text-xs text-zinc-500 block mb-1"})
                      (dom/text "Safeguard"))
            (dom/p (dom/props {:class "text-sm text-emerald-400"})
                   (dom/text (:safeguard failure-mode)))))))))

(e/defn FailureModes []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Failure Modes"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Common ways mental models can fail or be misapplied")))
      
      ;; Failure Modes Grid
      (e/server
        (let [failure-modes (db/get-all-failure-modes)]
          (e/client
            (if (seq failure-modes)
              (dom/div
                (dom/props {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"})
                (e/for [fm failure-modes]
                  (FailureModeCard. fm)))
              (ui/EmptyState. :warning "No Failure Modes"
                              "No failure modes have been documented yet"))))))))
