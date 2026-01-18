(ns mental-models.ui.decisions
  "Decision Journal - Electric Clojure unified
   Real-time auto-save and streaming updates"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def show-new-form (e/client (atom false)))
(e/def new-decision (e/client (atom {:title "" :context "" :options "" :models-applied []})))
(e/def filter-status (e/client (atom "all")))

;; -- Server Functions --------------------------------------------------------

#?(:clj
   (defn get-decisions []
     (db/get-user-decisions nil)))

#?(:clj
   (defn create-decision! [decision]
     (db/create-decision! (assoc decision
                                 :user_id nil
                                 :created_at (System/currentTimeMillis)
                                 :status "pending"))))

#?(:clj
   (defn update-decision-status! [id status outcome-rating]
     (db/update-decision! id {:status status
                              :outcome_rating outcome-rating
                              :updated_at (System/currentTimeMillis)})))

;; -- Decision Card -----------------------------------------------------------

(e/defn DecisionCard [decision on-update]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 hover:border-zinc-700 transition-colors"})
      
      ;; Header
      (dom/div
        (dom/props {:class "flex items-start justify-between mb-4"})
        (dom/div
          (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-1"})
                  (dom/text (:title decision)))
          (dom/span (dom/props {:class "text-xs text-zinc-500"})
                    (dom/text (str "Created: " (-> (:created_at decision)
                                                   #?(:clj (java.util.Date.)
                                                      :cljs (js/Date.))
                                                   str)))))
        (ui/Badge. (or (:status decision) "pending")
                   (case (:status decision)
                     "completed" :green
                     "pending" :amber
                     "abandoned" :red
                     :amber)))
      
      ;; Context
      (when (:context decision)
        (dom/div
          (dom/props {:class "mb-4"})
          (dom/span (dom/props {:class "text-xs text-zinc-500 block mb-1"})
                    (dom/text "Context"))
          (dom/p (dom/props {:class "text-sm text-zinc-400"})
                 (dom/text (:context decision)))))
      
      ;; Options Considered
      (when (:options decision)
        (dom/div
          (dom/props {:class "mb-4"})
          (dom/span (dom/props {:class "text-xs text-zinc-500 block mb-1"})
                    (dom/text "Options Considered"))
          (dom/p (dom/props {:class "text-sm text-zinc-400"})
                 (dom/text (:options decision)))))
      
      ;; Models Applied
      (when (seq (:models_applied decision))
        (dom/div
          (dom/props {:class "mb-4"})
          (dom/span (dom/props {:class "text-xs text-zinc-500 block mb-2"})
                    (dom/text "Models Applied"))
          (dom/div
            (dom/props {:class "flex flex-wrap gap-2"})
            (e/for [model (clojure.string/split (or (:models_applied decision) "") #",")]
              (when (not (empty? model))
                (ui/Badge. model :amber))))))
      
      ;; Actions
      (when (= (:status decision) "pending")
        (dom/div
          (dom/props {:class "flex items-center gap-2 pt-4 border-t border-zinc-800"})
          (ui/Button. "Complete"
                      (e/fn [_]
                        (e/server
                          (update-decision-status! (:id decision) "completed" 4)
                          (e/client (on-update))))
                      :primary)
          (ui/Button. "Abandon"
                      (e/fn [_]
                        (e/server
                          (update-decision-status! (:id decision) "abandoned" 1)
                          (e/client (on-update))))
                      :ghost))))))

;; -- New Decision Form -------------------------------------------------------

(e/defn NewDecisionForm [on-save on-cancel]
  (e/client
    (dom/div
      (dom/props {:class "bg-zinc-900/50 border border-amber-500/30 rounded-2xl p-6"})
      
      (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-6"})
              (dom/text "New Decision"))
      
      (dom/div
        (dom/props {:class "space-y-4"})
        
        ;; Title
        (dom/div
          (dom/label (dom/props {:class "text-sm text-zinc-400 block mb-2"})
                     (dom/text "Decision Title"))
          (ui/Input. "What decision are you making?"
                     (:title @new-decision)
                     (e/fn [v] (swap! new-decision assoc :title v))))
        
        ;; Context
        (dom/div
          (dom/label (dom/props {:class "text-sm text-zinc-400 block mb-2"})
                     (dom/text "Context"))
          (dom/textarea
            (dom/props {:class "w-full h-24 bg-zinc-800/50 border border-zinc-700 rounded-xl p-4 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 resize-none"
                        :placeholder "What's the background? Why is this decision important?"
                        :value (:context @new-decision)})
            (dom/on "input" (e/fn [e] (swap! new-decision assoc :context (.. e -target -value))))))
        
        ;; Options
        (dom/div
          (dom/label (dom/props {:class "text-sm text-zinc-400 block mb-2"})
                     (dom/text "Options Considered"))
          (dom/textarea
            (dom/props {:class "w-full h-24 bg-zinc-800/50 border border-zinc-700 rounded-xl p-4 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 resize-none"
                        :placeholder "What options are you considering?"
                        :value (:options @new-decision)})
            (dom/on "input" (e/fn [e] (swap! new-decision assoc :options (.. e -target -value))))))
        
        ;; Actions
        (dom/div
          (dom/props {:class "flex items-center gap-3 pt-4"})
          (ui/Button. "Save Decision"
                      (e/fn [_]
                        (when (not (empty? (:title @new-decision)))
                          (e/server
                            (create-decision! (e/client @new-decision)))
                          (reset! new-decision {:title "" :context "" :options "" :models-applied []})
                          (on-save)))
                      :primary)
          (ui/Button. "Cancel"
                      (e/fn [_] (on-cancel))
                      :ghost))))))

;; -- Decision Journal Page ---------------------------------------------------

(e/defn DecisionJournal []
  (e/client
    (let [refresh-key (atom 0)]
      (dom/div
        (dom/props {:class "p-8"})
        
        ;; Header
        (dom/div
          (dom/props {:class "flex items-center justify-between mb-8"})
          (dom/div
            (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                    (dom/text "Decision Journal"))
            (dom/p (dom/props {:class "text-zinc-400"})
                   (dom/text "Track decisions and the mental models you apply")))
          (ui/Button. "New Decision"
                      (e/fn [_] (reset! show-new-form true))
                      :primary))
        
        ;; New Decision Form
        (when @show-new-form
          (dom/div
            (dom/props {:class "mb-6"})
            (NewDecisionForm.
             (e/fn []
               (reset! show-new-form false)
               (swap! refresh-key inc))
             (e/fn [] (reset! show-new-form false)))))
        
        ;; Filter Tabs
        (dom/div
          (dom/props {:class "flex items-center gap-2 mb-6"})
          (e/for [[status label] [["all" "All"] ["pending" "Pending"] ["completed" "Completed"] ["abandoned" "Abandoned"]]]
            (dom/button
              (dom/props {:class (str "px-4 py-2 rounded-xl text-sm font-medium transition-all "
                                      (if (= @filter-status status)
                                        "bg-amber-500 text-zinc-900"
                                        "bg-zinc-800 text-zinc-400 hover:text-zinc-100"))})
              (dom/on "click" (e/fn [_] (reset! filter-status status)))
              (dom/text label))))
        
        ;; Decisions List
        (e/server
          (let [_ (e/client @refresh-key)  ; Force refresh on change
                decisions (get-decisions)
                status-filter (e/client @filter-status)
                filtered (if (= status-filter "all")
                           decisions
                           (filter #(= (:status %) status-filter) decisions))]
            (e/client
              (if (seq filtered)
                (dom/div
                  (dom/props {:class "grid grid-cols-1 lg:grid-cols-2 gap-6"})
                  (e/for [decision filtered]
                    (DecisionCard. decision (e/fn [] (swap! refresh-key inc)))))
                (ui/EmptyState. :book "No Decisions"
                                "Start tracking your decisions to build your decision-making history")))))))))
