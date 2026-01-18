(ns mental-models.ui.connectors
  "Connector Hub - Electric Clojure unified
   Integration hub for external services and data sources"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [mental-models.ui.components :as ui]))

;; -- Connector Data ----------------------------------------------------------

(def connectors
  [{:id 1 :name "Notion" :category "Productivity" :status :connected
    :description "Sync decisions and notes with Notion workspace"
    :icon "📝" :color "#000000" :last-sync "2 min ago"}
   {:id 2 :name "Slack" :category "Communication" :status :connected
    :description "Receive signal alerts and decision reminders"
    :icon "💬" :color "#4A154B" :last-sync "5 min ago"}
   {:id 3 :name "Google Calendar" :category "Productivity" :status :connected
    :description "Schedule decision reviews and model practice"
    :icon "📅" :color "#4285F4" :last-sync "1 hour ago"}
   {:id 4 :name "Obsidian" :category "Knowledge" :status :disconnected
    :description "Sync mental models with your knowledge base"
    :icon "💎" :color "#7C3AED" :last-sync nil}
   {:id 5 :name "Twitter/X" :category "Social" :status :connected
    :description "Monitor sentiment and harvest signals"
    :icon "🐦" :color "#1DA1F2" :last-sync "30 min ago"}
   {:id 6 :name "Bloomberg" :category "Finance" :status :disconnected
    :description "Real-time market data and financial signals"
    :icon "📊" :color "#FF6600" :last-sync nil}
   {:id 7 :name "arXiv" :category "Research" :status :connected
    :description "Auto-update models from academic papers"
    :icon "📚" :color "#B31B1B" :last-sync "1 day ago"}
   {:id 8 :name "GitHub" :category "Development" :status :connected
    :description "Track code metrics and development velocity"
    :icon "🐙" :color "#333333" :last-sync "15 min ago"}])

;; -- Connector Card Component ------------------------------------------------

(e/defn ConnectorCard [connector]
  (e/client
    (dom/div
      (dom/props {:class (str "bg-zinc-900/50 border rounded-2xl p-6 transition-all "
                              (if (= (:status connector) :connected)
                                "border-zinc-800 hover:border-zinc-700"
                                "border-dashed border-zinc-700 hover:border-zinc-600"))})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-start justify-between mb-4"})
        (dom/div
          (dom/props {:class "flex items-center gap-3"})
          (dom/div
            (dom/props {:class "w-12 h-12 rounded-xl flex items-center justify-center text-2xl"
                        :style {:background-color (str (:color connector) "20")}})
            (dom/text (:icon connector)))
          (dom/div
            (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                    (dom/text (:name connector)))
            (dom/span (dom/props {:class "text-xs text-zinc-500"})
                      (dom/text (:category connector)))))
        
        ;; Status Badge
        (dom/span
          (dom/props {:class (str "text-xs px-2 py-1 rounded-full "
                                  (if (= (:status connector) :connected)
                                    "bg-emerald-500/10 text-emerald-500"
                                    "bg-zinc-700 text-zinc-400"))})
          (dom/text (if (= (:status connector) :connected) "Connected" "Disconnected"))))
      
      ;; Description
      (dom/p (dom/props {:class "text-sm text-zinc-400 mb-4"})
             (dom/text (:description connector)))
      
      ;; Footer
      (dom/div
        (dom/props {:class "flex items-center justify-between pt-4 border-t border-zinc-800"})
        (if (:last-sync connector)
          (dom/span (dom/props {:class "text-xs text-zinc-500"})
                    (dom/text (str "Last sync: " (:last-sync connector))))
          (dom/span (dom/props {:class "text-xs text-zinc-500"})
                    (dom/text "Not configured")))
        (ui/Button. (if (= (:status connector) :connected) "Configure" "Connect")
                    (e/fn [_] nil)
                    (if (= (:status connector) :connected) :ghost :primary))))))

;; -- Connector Hub Page ------------------------------------------------------

(e/defn ConnectorHub []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-center justify-between mb-8"})
        (dom/div
          (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                  (dom/text "Connector Hub"))
          (dom/p (dom/props {:class "text-zinc-400"})
                 (dom/text "Integration hub for external services and data sources")))
        (ui/Button. "Add Connector" (e/fn [_] nil) :primary))
      
      ;; Stats
      (dom/div
        (dom/props {:class "grid grid-cols-4 gap-6 mb-8"})
        (ui/StatCard. "Connected" "6" "+1" :up)
        (ui/StatCard. "Available" "8" nil nil)
        (ui/StatCard. "Syncs Today" "147" "+23" :up)
        (ui/StatCard. "Data Points" "12.4K" "+2.1K" :up))
      
      ;; Categories
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                (dom/text "By Category"))
        (dom/div
          (dom/props {:class "flex flex-wrap gap-2"})
          (e/for [cat ["All" "Productivity" "Communication" "Knowledge" "Social" "Finance" "Research" "Development"]]
            (dom/button
              (dom/props {:class (str "px-4 py-2 rounded-xl text-sm font-medium transition-all "
                                      (if (= cat "All")
                                        "bg-amber-500 text-zinc-900"
                                        "bg-zinc-800 text-zinc-400 hover:text-zinc-100"))})
              (dom/text cat)))))
      
      ;; Connectors Grid
      (dom/div
        (dom/props {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"})
        (e/for [connector connectors]
          (ConnectorCard. connector)))
      
      ;; Request New Connector
      (dom/div
        (dom/props {:class "mt-8 bg-zinc-900/50 border border-dashed border-zinc-700 rounded-2xl p-8 text-center"})
        (dom/div
          (dom/props {:class "w-16 h-16 bg-zinc-800 rounded-2xl flex items-center justify-center mx-auto mb-4"})
          (ui/Icon. :plus "w-8 h-8 text-zinc-500"))
        (dom/h3 (dom/props {:class "text-lg font-medium text-zinc-300 mb-2"})
                (dom/text "Request a Connector"))
        (dom/p (dom/props {:class "text-sm text-zinc-500 mb-4"})
               (dom/text "Don't see the integration you need? Let us know!"))
        (ui/Button. "Request Integration" (e/fn [_] nil) :secondary)))))
