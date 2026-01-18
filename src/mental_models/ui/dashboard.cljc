(ns mental-models.ui.dashboard
  "Dashboard - Electric Clojure unified client/server
   Real-time metrics without manual API calls"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            [mental-models.ui.components :as ui]))

;; -- Server-side Data Fetching (runs on server, streams to client) -----------

#?(:clj
   (defn get-dashboard-data []
     {:metrics (db/get-system-metrics)
      :recent-decisions (take 5 (db/get-user-decisions nil))
      :top-models (take 10 (db/get-all-models))
      :categories (db/get-categories)}))

;; -- Dashboard Component -----------------------------------------------------

(e/defn MetricCard [label value change sparkline-data]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-2"})
        (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text label))
        (dom/span
          (dom/props {:class (str "text-xs px-2 py-1 rounded-full "
                                  (if (clojure.string/starts-with? change "+")
                                    "bg-emerald-500/10 text-emerald-500"
                                    "bg-red-500/10 text-red-500"))})
          (dom/text change)))
      (dom/div
        (dom/props {:class "text-3xl font-bold text-zinc-100 mb-4"})
        (dom/text (str value)))
      (when sparkline-data
        (ui/Sparkline. sparkline-data)))))

(e/defn RecentDecisionRow [decision]
  (e/client
    (dom/div
      (dom/props {:class "flex items-center justify-between py-3 border-b border-zinc-800 last:border-0"})
      (dom/div
        (dom/span (dom/props {:class "text-zinc-100 font-medium block"})
                  (dom/text (:title decision)))
        (dom/span (dom/props {:class "text-xs text-zinc-500"})
                  (dom/text (str "Context: " (or (:context decision) "General")))))
      (dom/div
        (dom/props {:class "text-right"})
        (dom/span
          (dom/props {:class (str "text-xs px-2 py-1 rounded-full "
                                  (case (:status decision)
                                    "completed" "bg-emerald-500/10 text-emerald-500"
                                    "pending" "bg-amber-500/10 text-amber-500"
                                    "bg-zinc-700 text-zinc-400"))})
          (dom/text (or (:status decision) "pending")))))))

(e/defn TopModelCard [model]
  (e/client
    (dom/div
      (dom/props {:class "flex items-center gap-4 p-4 bg-zinc-800/30 rounded-xl hover:bg-zinc-800/50 transition-colors cursor-pointer"})
      (dom/div
        (dom/props {:class "w-10 h-10 rounded-xl bg-amber-500/10 flex items-center justify-center"})
        (ui/Icon. :brain "w-5 h-5 text-amber-500"))
      (dom/div
        (dom/props {:class "flex-1 min-w-0"})
        (dom/span (dom/props {:class "text-zinc-100 font-medium block truncate"})
                  (dom/text (:name model)))
        (dom/span (dom/props {:class "text-xs text-zinc-500"})
                  (dom/text (or (:category_name model) "General"))))
      (dom/div
        (dom/props {:class "text-right"})
        (dom/span (dom/props {:class "text-xs text-zinc-400"})
                  (dom/text (str "Complexity: " (or (:complexity model) 2))))))))

(e/defn CategoryCard [category]
  (e/client
    (dom/div
      (dom/props {:class "p-4 bg-zinc-800/30 rounded-xl hover:bg-zinc-800/50 transition-colors cursor-pointer"})
      (dom/div
        (dom/props {:class "flex items-center gap-3 mb-2"})
        (dom/div
          (dom/props {:class (str "w-8 h-8 rounded-lg flex items-center justify-center "
                                  "bg-" (or (:color category) "amber") "-500/10")})
          (ui/Icon. :grid (str "w-4 h-4 text-" (or (:color category) "amber") "-500")))
        (dom/span (dom/props {:class "text-zinc-100 font-medium"})
                  (dom/text (:name category))))
      (dom/p (dom/props {:class "text-xs text-zinc-500 line-clamp-2"})
             (dom/text (or (:description category) ""))))))

(e/defn Dashboard []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Dashboard"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Real-time overview of your mental models system")))
      
      ;; Metrics Grid - Data streams from server automatically
      (e/server
        (let [data (get-dashboard-data)
              metrics (:metrics data)]
          (e/client
            (dom/div
              (dom/props {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8"})
              
              (MetricCard. "Total Models"
                           (e/server (:total-models metrics))
                           "+12"
                           [10 12 11 14 13 15 14 16 15 17])
              
              (MetricCard. "Decisions Made"
                           (e/server (:total-decisions metrics))
                           "+8"
                           [5 6 4 7 8 6 9 8 10 11])
              
              (MetricCard. "Active Users"
                           (e/server (:total-users metrics))
                           "+3"
                           [2 3 2 4 3 5 4 6 5 7])
              
              (MetricCard. "Analyses Run"
                           (e/server (:total-analyses metrics))
                           "+24"
                           [20 22 25 23 28 26 30 29 32 35])))))
      
      ;; Main Content Grid
      (dom/div
        (dom/props {:class "grid grid-cols-1 lg:grid-cols-3 gap-6"})
        
        ;; Recent Decisions
        (dom/div
          (dom/props {:class "lg:col-span-2 bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
          (dom/div
            (dom/props {:class "flex items-center justify-between mb-6"})
            (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                    (dom/text "Recent Decisions"))
            (ui/Button. "View All" (e/fn [_] nil) :ghost))
          
          (e/server
            (let [decisions (:recent-decisions (get-dashboard-data))]
              (e/client
                (if (seq decisions)
                  (dom/div
                    (e/for [decision decisions]
                      (RecentDecisionRow. decision)))
                  (ui/EmptyState. :book "No decisions yet"
                                  "Start making decisions to see them here"))))))
        
        ;; Top Models
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
          (dom/div
            (dom/props {:class "flex items-center justify-between mb-6"})
            (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                    (dom/text "Top Models"))
            (ui/Button. "Browse All" (e/fn [_] nil) :ghost))
          
          (e/server
            (let [models (:top-models (get-dashboard-data))]
              (e/client
                (dom/div
                  (dom/props {:class "space-y-3"})
                  (e/for [model (take 5 models)]
                    (TopModelCard. model))))))))
      
      ;; Categories Grid
      (dom/div
        (dom/props {:class "mt-8"})
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                (dom/text "Categories"))
        (e/server
          (let [categories (:categories (get-dashboard-data))]
            (e/client
              (dom/div
                (dom/props {:class "grid grid-cols-2 md:grid-cols-4 gap-4"})
                (e/for [category categories]
                  (CategoryCard. category))))))))))
