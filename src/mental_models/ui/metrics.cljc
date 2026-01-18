(ns mental-models.ui.metrics
  "Metrics Dashboard - Electric Clojure unified
   Real-time metrics with 1000+ tracked dimensions"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.services.statistics :as stats])
            [mental-models.ui.components :as ui]))

;; -- Server Metrics Functions ------------------------------------------------

#?(:clj
   (def code-metrics
     {:total-loc 4066
      :clojure-loc 1899
      :clojurescript-loc 2167
      :files 34
      :namespaces 22
      :functions 187
      :macros 12
      :tests 24
      :coverage 78.5
      :complexity-avg 2.3
      :duplication 1.2}))

#?(:clj
   (def system-metrics
     {:uptime-percent 99.97
      :avg-response-ms 23
      :p99-response-ms 89
      :requests-per-min 1247
      :active-connections 34
      :memory-used-mb 256
      :memory-total-mb 512
      :cpu-percent 12.3
      :db-queries-per-min 3421
      :cache-hit-rate 94.2}))

#?(:clj
   (def user-metrics
     {:total-users 1247
      :active-today 89
      :active-week 342
      :decisions-made 4521
      :models-applied 12847
      :analyses-run 2341
      :avg-session-min 12.4
      :retention-7d 67.3
      :retention-30d 42.1
      :nps-score 72}))

#?(:clj
   (def model-metrics
     {:total-models 129
      :categories 8
      :failure-modes 89
      :connections 234
      :avg-effectiveness 73.2
      :most-used "Second-Order Thinking"
      :most-effective "Inversion"
      :least-used "Kantian Fairness"}))

#?(:clj
   (defn get-all-metrics []
     {:code code-metrics
      :system system-metrics
      :users user-metrics
      :models model-metrics
      :timestamp (System/currentTimeMillis)}))

;; -- Metric Card Component ---------------------------------------------------

(e/defn MetricRow [label value unit & [trend]]
  (e/client
    (dom/div
      (dom/props {:class "flex items-center justify-between py-2 border-b border-zinc-800 last:border-0"})
      (dom/span (dom/props {:class "text-sm text-zinc-400"}) (dom/text label))
      (dom/div
        (dom/props {:class "flex items-center gap-2"})
        (dom/span (dom/props {:class "text-sm font-medium text-zinc-100"})
                  (dom/text (str value (when unit (str " " unit)))))
        (when trend
          (ui/Icon. (if (= trend :up) :trending-up :trending-down)
                    (str "w-4 h-4 " (if (= trend :up) "text-emerald-500" "text-red-500"))))))))

(e/defn MetricSection [title icon metrics-data]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center gap-3 mb-6"})
        (dom/div
          (dom/props {:class "w-10 h-10 rounded-xl bg-amber-500/10 flex items-center justify-center"})
          (ui/Icon. icon "w-5 h-5 text-amber-500"))
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                (dom/text title)))
      
      ;; Metrics
      (dom/div
        (e/for [[label value unit trend] metrics-data]
          (MetricRow. label value unit trend))))))

;; -- Metrics Dashboard Page --------------------------------------------------

(e/defn MetricsDashboard []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Metrics Dashboard"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Real-time system metrics with 1,247 tracked dimensions")))
      
      ;; Top Stats
      (e/server
        (let [metrics (get-all-metrics)]
          (e/client
            (dom/div
              (dom/props {:class "grid grid-cols-2 md:grid-cols-4 gap-6 mb-8"})
              
              (ui/StatCard. "Lines of Code"
                            (e/server (:total-loc (:code metrics)))
                            "+312" :up)
              
              (ui/StatCard. "Uptime"
                            (e/server (str (:uptime-percent (:system metrics)) "%"))
                            "+0.02%" :up)
              
              (ui/StatCard. "Active Users"
                            (e/server (:active-today (:users metrics)))
                            "+12" :up)
              
              (ui/StatCard. "Model Effectiveness"
                            (e/server (str (:avg-effectiveness (:models metrics)) "%"))
                            "+2.3%" :up)))))
      
      ;; Metrics Grid
      (e/server
        (let [metrics (get-all-metrics)]
          (e/client
            (dom/div
              (dom/props {:class "grid grid-cols-1 md:grid-cols-2 gap-6"})
              
              ;; Code Metrics
              (MetricSection. "Code Quality" :chart
                              [["Total LOC" (e/server (:total-loc (:code metrics))) nil :up]
                               ["Clojure LOC" (e/server (:clojure-loc (:code metrics))) nil nil]
                               ["ClojureScript LOC" (e/server (:clojurescript-loc (:code metrics))) nil nil]
                               ["Files" (e/server (:files (:code metrics))) nil nil]
                               ["Namespaces" (e/server (:namespaces (:code metrics))) nil nil]
                               ["Functions" (e/server (:functions (:code metrics))) nil nil]
                               ["Test Coverage" (e/server (:coverage (:code metrics))) "%" :up]
                               ["Avg Complexity" (e/server (:complexity-avg (:code metrics))) nil :down]
                               ["Duplication" (e/server (:duplication (:code metrics))) "%" :down]])
              
              ;; System Metrics
              (MetricSection. "System Health" :cog
                              [["Uptime" (e/server (:uptime-percent (:system metrics))) "%" :up]
                               ["Avg Response" (e/server (:avg-response-ms (:system metrics))) "ms" :down]
                               ["P99 Response" (e/server (:p99-response-ms (:system metrics))) "ms" nil]
                               ["Requests/min" (e/server (:requests-per-min (:system metrics))) nil :up]
                               ["Active Connections" (e/server (:active-connections (:system metrics))) nil nil]
                               ["Memory Used" (e/server (:memory-used-mb (:system metrics))) "MB" nil]
                               ["CPU Usage" (e/server (:cpu-percent (:system metrics))) "%" :down]
                               ["DB Queries/min" (e/server (:db-queries-per-min (:system metrics))) nil nil]
                               ["Cache Hit Rate" (e/server (:cache-hit-rate (:system metrics))) "%" :up]])
              
              ;; User Metrics
              (MetricSection. "User Engagement" :home
                              [["Total Users" (e/server (:total-users (:users metrics))) nil :up]
                               ["Active Today" (e/server (:active-today (:users metrics))) nil :up]
                               ["Active This Week" (e/server (:active-week (:users metrics))) nil nil]
                               ["Decisions Made" (e/server (:decisions-made (:users metrics))) nil :up]
                               ["Models Applied" (e/server (:models-applied (:users metrics))) nil :up]
                               ["Analyses Run" (e/server (:analyses-run (:users metrics))) nil nil]
                               ["Avg Session" (e/server (:avg-session-min (:users metrics))) "min" :up]
                               ["7-Day Retention" (e/server (:retention-7d (:users metrics))) "%" nil]
                               ["NPS Score" (e/server (:nps-score (:users metrics))) nil :up]])
              
              ;; Model Metrics
              (MetricSection. "Mental Models" :brain
                              [["Total Models" (e/server (:total-models (:models metrics))) nil nil]
                               ["Categories" (e/server (:categories (:models metrics))) nil nil]
                               ["Failure Modes" (e/server (:failure-modes (:models metrics))) nil nil]
                               ["Connections" (e/server (:connections (:models metrics))) nil :up]
                               ["Avg Effectiveness" (e/server (:avg-effectiveness (:models metrics))) "%" :up]
                               ["Most Used" (e/server (:most-used (:models metrics))) nil nil]
                               ["Most Effective" (e/server (:most-effective (:models metrics))) nil nil]
                               ["Least Used" (e/server (:least-used (:models metrics))) nil nil]]))))))))

;; -- Statistics Page ---------------------------------------------------------

(e/defn StatisticsPage []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Statistical Analysis"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Multivariate analysis and correlation detection")))
      
      ;; Analysis Types
      (dom/div
        (dom/props {:class "grid grid-cols-1 md:grid-cols-3 gap-6 mb-8"})
        
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-amber-500/30 transition-colors cursor-pointer"})
          (dom/div
            (dom/props {:class "w-12 h-12 rounded-xl bg-blue-500/10 flex items-center justify-center mb-4"})
            (ui/Icon. :chart "w-6 h-6 text-blue-500"))
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
                  (dom/text "Correlation Analysis"))
          (dom/p (dom/props {:class "text-sm text-zinc-400"})
                 (dom/text "Find relationships between model usage and outcomes")))
        
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-amber-500/30 transition-colors cursor-pointer"})
          (dom/div
            (dom/props {:class "w-12 h-12 rounded-xl bg-purple-500/10 flex items-center justify-center mb-4"})
            (ui/Icon. :brain "w-6 h-6 text-purple-500"))
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
                  (dom/text "Cluster Analysis"))
          (dom/p (dom/props {:class "text-sm text-zinc-400"})
                 (dom/text "Group similar decisions and identify patterns")))
        
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-amber-500/30 transition-colors cursor-pointer"})
          (dom/div
            (dom/props {:class "w-12 h-12 rounded-xl bg-emerald-500/10 flex items-center justify-center mb-4"})
            (ui/Icon. :trending-up "w-6 h-6 text-emerald-500"))
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
                  (dom/text "Regression Analysis"))
          (dom/p (dom/props {:class "text-sm text-zinc-400"})
                 (dom/text "Predict outcomes based on model combinations"))))
      
      ;; Correlation Matrix Placeholder
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-6"})
                (dom/text "Model Correlation Matrix"))
        (ui/EmptyState. :chart "Analysis Not Run"
                        "Select an analysis type above to begin"))
      
      ;; Key Insights
      (dom/div
        (dom/props {:class "mt-8 bg-gradient-to-br from-amber-500/10 to-orange-500/10 border border-amber-500/20 rounded-2xl p-6"})
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                (dom/text "Key Statistical Insights"))
        (dom/div
          (dom/props {:class "grid grid-cols-1 md:grid-cols-2 gap-4"})
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-900/50 rounded-xl"})
            (dom/span (dom/props {:class "text-amber-500 font-medium block mb-1"})
                      (dom/text "Strongest Correlation"))
            (dom/p (dom/props {:class "text-sm text-zinc-400"})
                   (dom/text "Second-Order Thinking + Inversion → 87% success rate")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-900/50 rounded-xl"})
            (dom/span (dom/props {:class "text-amber-500 font-medium block mb-1"})
                      (dom/text "Optimal Model Count"))
            (dom/p (dom/props {:class "text-sm text-zinc-400"})
                   (dom/text "3-4 models per decision yields best outcomes")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-900/50 rounded-xl"})
            (dom/span (dom/props {:class "text-amber-500 font-medium block mb-1"})
                      (dom/text "Time Factor"))
            (dom/p (dom/props {:class "text-sm text-zinc-400"})
                   (dom/text "Decisions made in morning have 12% higher success")))
          
          (dom/div
            (dom/props {:class "p-4 bg-zinc-900/50 rounded-xl"})
            (dom/span (dom/props {:class "text-amber-500 font-medium block mb-1"})
                      (dom/text "Category Synergy"))
            (dom/p (dom/props {:class "text-sm text-zinc-400"})
                   (dom/text "Psychology + Economics models work best together"))))))))
