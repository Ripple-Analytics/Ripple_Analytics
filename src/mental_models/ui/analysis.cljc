(ns mental-models.ui.analysis
  "Analysis Pages - Electric Clojure unified
   Real-time streaming analysis with LLM integration"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:clj [mental-models.db.core :as db])
            #?(:clj [mental-models.services.llm :as llm])
            #?(:clj [mental-models.services.statistics :as stats])
            [mental-models.ui.components :as ui]))

;; -- Client State ------------------------------------------------------------

(e/def analysis-text (e/client (atom "")))
(e/def analysis-result (e/client (atom nil)))
(e/def is-analyzing (e/client (atom false)))

;; -- Server Analysis Functions -----------------------------------------------

#?(:clj
   (defn analyze-for-models [text]
     (when (and text (> (count text) 50))
       (llm/analyze-text-for-models text))))

#?(:clj
   (defn detect-lollapalooza [text]
     (when (and text (> (count text) 50))
       (llm/detect-lollapalooza text))))

#?(:clj
   (defn get-effectiveness-data [user-id]
     {:model-stats (db/get-user-model-stats user-id)
      :recent-usage (take 20 (db/query ["SELECT * FROM model_usage WHERE user_id = ? ORDER BY created_at DESC LIMIT 20" user-id]))}))

;; -- Analysis Page -----------------------------------------------------------

(e/defn AnalysisPage []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Mental Model Analysis"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Analyze text to identify relevant mental models")))
      
      ;; Input Section
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6 mb-6"})
        
        (dom/textarea
          (dom/props {:class "w-full h-48 bg-zinc-800/50 border border-zinc-700 rounded-xl p-4 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 resize-none"
                      :placeholder "Paste text to analyze for mental models..."
                      :value @analysis-text})
          (dom/on "input" (e/fn [e] (reset! analysis-text (.. e -target -value)))))
        
        (dom/div
          (dom/props {:class "flex items-center justify-between mt-4"})
          (dom/span (dom/props {:class "text-sm text-zinc-500"})
                    (dom/text (str (count @analysis-text) " characters")))
          (ui/Button. (if @is-analyzing "Analyzing..." "Analyze")
                      (e/fn [_]
                        (when (and (not @is-analyzing) (> (count @analysis-text) 50))
                          (reset! is-analyzing true)
                          (e/server
                            (let [result (analyze-for-models (e/client @analysis-text))]
                              (e/client
                                (reset! analysis-result result)
                                (reset! is-analyzing false))))))
                      :primary)))
      
      ;; Results Section
      (when @analysis-result
        (dom/div
          (dom/props {:class "space-y-6"})
          
          ;; Models Found
          (dom/div
            (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
            (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                    (dom/text "Models Identified"))
            (dom/div
              (dom/props {:class "grid grid-cols-1 md:grid-cols-2 gap-4"})
              (e/for [model (get @analysis-result :models [])]
                (dom/div
                  (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
                  (dom/div
                    (dom/props {:class "flex items-center justify-between mb-2"})
                    (dom/span (dom/props {:class "font-medium text-zinc-100"})
                              (dom/text (:name model)))
                    (dom/span (dom/props {:class "text-sm text-amber-500"})
                              (dom/text (str (int (* 100 (:relevance model 0))) "%"))))
                  (dom/p (dom/props {:class "text-sm text-zinc-400"})
                         (dom/text (or (:evidence model) "")))))))
          
          ;; Lollapalooza Detection
          (when-let [lolla (get @analysis-result :lollapalooza)]
            (when (:detected lolla)
              (dom/div
                (dom/props {:class "bg-amber-500/10 border border-amber-500/30 rounded-2xl p-6"})
                (dom/div
                  (dom/props {:class "flex items-center gap-3 mb-4"})
                  (ui/Icon. :lightning "w-6 h-6 text-amber-500")
                  (dom/h2 (dom/props {:class "text-lg font-semibold text-amber-500"})
                          (dom/text "Lollapalooza Effect Detected!")))
                (dom/p (dom/props {:class "text-zinc-300 mb-4"})
                       (dom/text (str "Strength: " (or (:strength lolla) "moderate"))))
                (dom/div
                  (dom/props {:class "flex flex-wrap gap-2"})
                  (e/for [model (get lolla :converging_models [])]
                    (ui/Badge. model :amber)))))))))))

;; -- Lollapalooza Detector Page ----------------------------------------------

(e/defn LollapaloozaDetector []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Lollapalooza Detector"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Detect when multiple mental models converge for extreme outcomes")))
      
      ;; Explanation Card
      (dom/div
        (dom/props {:class "bg-gradient-to-br from-amber-500/10 to-orange-500/10 border border-amber-500/20 rounded-2xl p-6 mb-8"})
        (dom/div
          (dom/props {:class "flex items-start gap-4"})
          (dom/div
            (dom/props {:class "w-12 h-12 rounded-xl bg-amber-500/20 flex items-center justify-center"})
            (ui/Icon. :lightning "w-6 h-6 text-amber-500"))
          (dom/div
            (dom/h3 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-2"})
                    (dom/text "What is a Lollapalooza Effect?"))
            (dom/p (dom/props {:class "text-zinc-400"})
                   (dom/text "Charlie Munger coined this term to describe situations where multiple psychological tendencies or mental models combine to produce an extreme outcome. When several biases or forces align, the result is often far more powerful than any single factor would predict.")))))
      
      ;; Detection Interface
      (dom/div
        (dom/props {:class "grid grid-cols-1 lg:grid-cols-2 gap-6"})
        
        ;; Input
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
          (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                  (dom/text "Describe the Situation"))
          (dom/textarea
            (dom/props {:class "w-full h-64 bg-zinc-800/50 border border-zinc-700 rounded-xl p-4 text-zinc-100 placeholder-zinc-500 focus:outline-none focus:border-amber-500/50 resize-none"
                        :placeholder "Describe a situation, decision, or phenomenon you want to analyze for converging mental models..."
                        :value @analysis-text})
            (dom/on "input" (e/fn [e] (reset! analysis-text (.. e -target -value)))))
          (dom/div
            (dom/props {:class "mt-4"})
            (ui/Button. "Detect Lollapalooza"
                        (e/fn [_]
                          (when (> (count @analysis-text) 50)
                            (reset! is-analyzing true)
                            (e/server
                              (let [result (detect-lollapalooza (e/client @analysis-text))]
                                (e/client
                                  (reset! analysis-result result)
                                  (reset! is-analyzing false))))))
                        :primary)))
        
        ;; Results
        (dom/div
          (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl p-6"})
          (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                  (dom/text "Detection Results"))
          
          (if @analysis-result
            (dom/div
              (dom/props {:class "space-y-4"})
              
              ;; Strength Indicator
              (dom/div
                (dom/props {:class "flex items-center gap-4 p-4 bg-zinc-800/50 rounded-xl"})
                (dom/span (dom/props {:class "text-zinc-400"}) (dom/text "Strength:"))
                (dom/div
                  (dom/props {:class "flex-1 h-3 bg-zinc-700 rounded-full overflow-hidden"})
                  (dom/div
                    (dom/props {:class "h-full bg-gradient-to-r from-amber-500 to-orange-500"
                                :style {:width (str (* 100 (get @analysis-result :strength 0)) "%")}})))
                (dom/span (dom/props {:class "text-amber-500 font-medium"})
                          (dom/text (str (int (* 100 (get @analysis-result :strength 0))) "%"))))
              
              ;; Converging Models
              (dom/div
                (dom/h3 (dom/props {:class "text-sm font-medium text-zinc-400 mb-2"})
                        (dom/text "Converging Models"))
                (dom/div
                  (dom/props {:class "flex flex-wrap gap-2"})
                  (e/for [model (get @analysis-result :models [])]
                    (ui/Badge. (:name model) :amber))))
              
              ;; Explanation
              (when-let [explanation (get @analysis-result :explanation)]
                (dom/div
                  (dom/props {:class "p-4 bg-zinc-800/50 rounded-xl"})
                  (dom/p (dom/props {:class "text-zinc-300"})
                         (dom/text explanation)))))
            
            (ui/EmptyState. :lightning "No Analysis Yet"
                            "Enter a situation description and click detect")))))))

;; -- Effectiveness Tracker ---------------------------------------------------

(e/defn EffectivenessTracker []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Effectiveness Tracker"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Track how well mental models perform in your decisions")))
      
      ;; Stats Overview
      (e/server
        (let [data (get-effectiveness-data nil)
              stats (:model-stats data)]
          (e/client
            (dom/div
              (dom/props {:class "grid grid-cols-1 md:grid-cols-3 gap-6 mb-8"})
              
              (ui/StatCard. "Models Used"
                            (e/server (count stats))
                            "+5" :up)
              
              (ui/StatCard. "Avg Effectiveness"
                            (e/server (str (int (* 100 (/ (reduce + (map :avg_outcome stats)) (max 1 (count stats))))) "%"))
                            "+3%" :up)
              
              (ui/StatCard. "Total Applications"
                            (e/server (reduce + (map :usage_count stats)))
                            "+12" :up)))))
      
      ;; Model Performance Table
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border border-zinc-800 rounded-2xl overflow-hidden"})
        
        (dom/div
          (dom/props {:class "p-6 border-b border-zinc-800"})
          (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100"})
                  (dom/text "Model Performance")))
        
        (e/server
          (let [stats (:model-stats (get-effectiveness-data nil))]
            (e/client
              (if (seq stats)
                (dom/table
                  (dom/props {:class "w-full"})
                  (dom/thead
                    (dom/tr
                      (dom/props {:class "text-left text-xs text-zinc-500 uppercase tracking-wider"})
                      (dom/th (dom/props {:class "px-6 py-3"}) (dom/text "Model"))
                      (dom/th (dom/props {:class "px-6 py-3"}) (dom/text "Uses"))
                      (dom/th (dom/props {:class "px-6 py-3"}) (dom/text "Avg Outcome"))
                      (dom/th (dom/props {:class "px-6 py-3"}) (dom/text "Trend"))))
                  (dom/tbody
                    (e/for [stat stats]
                      (dom/tr
                        (dom/props {:class "border-t border-zinc-800"})
                        (dom/td (dom/props {:class "px-6 py-4 text-zinc-100"})
                                (dom/text (str "Model #" (:model_id stat))))
                        (dom/td (dom/props {:class "px-6 py-4 text-zinc-400"})
                                (dom/text (str (:usage_count stat))))
                        (dom/td (dom/props {:class "px-6 py-4"})
                                (dom/span
                                  (dom/props {:class (if (> (:avg_outcome stat) 3)
                                                       "text-emerald-500"
                                                       "text-amber-500")})
                                  (dom/text (str (int (* 20 (:avg_outcome stat))) "%"))))
                        (dom/td (dom/props {:class "px-6 py-4"})
                                (ui/Icon. (if (> (:avg_outcome stat) 3)
                                            :trending-up
                                            :trending-down)
                                          (str "w-5 h-5 "
                                               (if (> (:avg_outcome stat) 3)
                                                 "text-emerald-500"
                                                 "text-red-500"))))))))
                (dom/div
                  (dom/props {:class "p-8"})
                  (ui/EmptyState. :chart "No Data Yet"
                                  "Start using mental models in your decisions to track effectiveness"))))))))))

;; -- Document Analysis -------------------------------------------------------

(e/defn DocumentAnalysis []
  (e/client
    (dom/div
      (dom/props {:class "p-8"})
      
      ;; Header
      (dom/div
        (dom/props {:class "mb-8"})
        (dom/h1 (dom/props {:class "text-3xl font-bold text-zinc-100 mb-2"})
                (dom/text "Document Analysis"))
        (dom/p (dom/props {:class "text-zinc-400"})
               (dom/text "Upload documents to extract mental model insights")))
      
      ;; Upload Area
      (dom/div
        (dom/props {:class "bg-zinc-900/50 border-2 border-dashed border-zinc-700 rounded-2xl p-12 text-center hover:border-amber-500/50 transition-colors cursor-pointer"})
        (dom/div
          (dom/props {:class "w-16 h-16 bg-zinc-800 rounded-2xl flex items-center justify-center mx-auto mb-4"})
          (ui/Icon. :plus "w-8 h-8 text-zinc-500"))
        (dom/h3 (dom/props {:class "text-lg font-medium text-zinc-300 mb-2"})
                (dom/text "Drop files here or click to upload"))
        (dom/p (dom/props {:class "text-sm text-zinc-500"})
               (dom/text "Supports PDF, TXT, MD, and more")))
      
      ;; Recent Analyses
      (dom/div
        (dom/props {:class "mt-8"})
        (dom/h2 (dom/props {:class "text-lg font-semibold text-zinc-100 mb-4"})
                (dom/text "Recent Analyses"))
        (ui/EmptyState. :book "No Documents Analyzed"
                        "Upload a document to get started")))))
