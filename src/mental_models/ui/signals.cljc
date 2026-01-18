(ns mental-models.ui.signals
  "Signal Harvester - Electric Clojure unified
   Real-time signal detection from news, markets, and social media"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def filter-source (e/client (atom :all)))
(e/def filter-strength (e/client (atom :all)))

;; -- Server Data -------------------------------------------------------------

#?(:clj
   (defn get-signals []
     [{:id 1 :source "News" :title "Fed signals potential rate pause" 
       :content "Federal Reserve officials indicate a possible pause in rate hikes..."
       :signal_type "economic" :strength 0.85 :models ["Second-Order Thinking" "Incentives"]
       :created_at (- (System/currentTimeMillis) 3600000)}
      {:id 2 :source "Markets" :title "Tech sector rotation detected"
       :content "Significant capital flows from growth to value stocks observed..."
       :signal_type "market" :strength 0.72 :models ["Mean Reversion" "Margin of Safety"]
       :created_at (- (System/currentTimeMillis) 7200000)}
      {:id 3 :source "Social" :title "Sentiment shift in crypto community"
       :content "Twitter analysis shows increasing bearish sentiment..."
       :signal_type "sentiment" :strength 0.68 :models ["Social Proof" "Availability Bias"]
       :created_at (- (System/currentTimeMillis) 10800000)}
      {:id 4 :source "Research" :title "New study on cognitive biases in trading"
       :content "Academic paper reveals systematic biases in retail trading..."
       :signal_type "research" :strength 0.91 :models ["Confirmation Bias" "Overconfidence"]
       :created_at (- (System/currentTimeMillis) 14400000)}
      {:id 5 :source "News" :title "Geopolitical tensions affecting supply chains"
       :content "Trade route disruptions causing ripple effects..."
       :signal_type "geopolitical" :strength 0.78 :models ["Systems Thinking" "Second-Order Effects"]
       :created_at (- (System/currentTimeMillis) 18000000)}]))

;; -- Signal Card Component ---------------------------------------------------

(e/defn SignalCard [signal]
  (e/client
    (let [source-colors {:News "#3b82f6"
                         :Markets "#10b981"
                         :Social "#8b5cf6"
                         :Research "#f59e0b"}
          color (get source-colors (keyword (:source signal)) "#71717a")]
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-zinc-700 transition-colors"})
        
        ;; Header
        (dom/div
          (dom/props {:class "flex items-start justify-between mb-4"})
          (dom/div
            (dom/props {:class "flex items-center gap-3"})
            (dom/div
              (dom/props {:class "w-10 h-10 rounded-xl flex items-center justify-center"
                          :style {:background-color (str color "20")}}))
            (dom/div
              (dom/span (dom/props {:class "text-xs font-medium px-2 py-1 rounded-full"
                                    :style {:background-color (str color "20")
                                            :color color}})
                        (dom/text (:source signal)))
              (dom/span (dom/props {:class "text-xs text-zinc-500 ml-2"})
                        (dom/text "2h ago"))))
          
          ;; Strength indicator
          (dom/div
            (dom/props {:class "flex items-center gap-2"})
            (dom/div
              (dom/props {:class "w-16 h-2 bg-zinc-800 rounded-full overflow-hidden"})
              (dom/div
                (dom/props {:class "h-full rounded-full"
                            :style {:width (str (* 100 (:strength signal)) "%")
                                    :background-color (cond
                                                        (> (:strength signal) 0.8) "#10b981"
                                                        (> (:strength signal) 0.6) "#f59e0b"
                                                        :else "#ef4444")}})))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text (str (int (* 100 (:strength signal))) "%")))))
        
        ;; Content
        (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
                (dom/text (:title signal)))
        (dom/p (dom/props {:class "text-sm text-zinc-400 mb-4"})
               (dom/text (:content signal)))
        
        ;; Related Models
        (dom/div
          (dom/props {:class "flex flex-wrap gap-2"})
          (e/for [model (:models signal)]
            (ui/Badge. model :amber)))))))

;; -- Signal Harvester Page ---------------------------------------------------

(e/defn SignalHarvester []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-8"})
        (dom/div
          (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                  (dom/text "Signal Harvester"))
          (dom/p (dom/props {:class "text-zinc-400"})
                 (dom/text "Real-time signal detection from news, markets, and social media")))
        
        ;; Add Signal Button
        (ui/Button. "Add Source" (e/fn [_] nil) :primary))
      
      ;; Filters
      (dom/div
        (dom/props {:class "flex items-center gap-6 mb-8"})
        
        ;; Source Filter
        (dom/div
          (dom/props {:class "flex items-center gap-2"})
          (dom/span (dom/props {:class "text-sm text-zinc-500"}) (dom/text "Source:"))
          (dom/div
            (dom/props {:class "flex items-center gap-1 bg-zinc-800 rounded-xl p-1"})
            (e/for [[key label] [[:all "All"] [:news "News"] [:markets "Markets"] [:social "Social"] [:research "Research"]]]
              (dom/button
                (dom/props {:class (str "px-3 py-1.5 rounded-lg text-sm font-medium transition-all "
                                        (if (= @filter-source key)
                                          "bg-amber-500 text-zinc-900"
                                          "text-zinc-400 hover:text-zinc-100"))})
                (dom/on "click" (e/fn [_] (reset! filter-source key)))
                (dom/text label)))))
        
        ;; Strength Filter
        (dom/div
          (dom/props {:class "flex items-center gap-2"})
          (dom/span (dom/props {:class "text-sm text-zinc-500"}) (dom/text "Strength:"))
          (dom/div
            (dom/props {:class "flex items-center gap-1 bg-zinc-800 rounded-xl p-1"})
            (e/for [[key label] [[:all "All"] [:high "High"] [:medium "Medium"] [:low "Low"]]]
              (dom/button
                (dom/props {:class (str "px-3 py-1.5 rounded-lg text-sm font-medium transition-all "
                                        (if (= @filter-strength key)
                                          "bg-amber-500 text-zinc-900"
                                          "text-zinc-400 hover:text-zinc-100"))})
                (dom/on "click" (e/fn [_] (reset! filter-strength key)))
                (dom/text label))))))
      
      ;; Stats Overview
      (dom/div
        (dom/props {:class "grid grid-cols-4 gap-6 mb-8"})
        (ui/StatCard. "Active Sources" "12" "+2" :up)
        (ui/StatCard. "Signals Today" "47" "+15" :up)
        (ui/StatCard. "High Strength" "8" "+3" :up)
        (ui/StatCard. "Models Triggered" "23" "+7" :up))
      
      ;; Signals Grid
      (e/server
        (let [signals (get-signals)]
          (e/client
            (dom/div
              (dom/props {:class "grid grid-cols-1 lg:grid-cols-2 gap-6"})
              (e/for [signal signals]
                (SignalCard. signal)))))))))
