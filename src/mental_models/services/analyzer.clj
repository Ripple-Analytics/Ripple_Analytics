(ns mental-models.services.analyzer
  "Mental Model Analysis Engine using Electric Clojure
   Reactive analysis pipeline: text → 129 models → streaming results to frontend
   Single LM Studio model with 129 prompt templates for efficient batch processing"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [mental-models.services.lm-studio :as llm]
            [mental-models.data.models :as models]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [clojure.core.async :as async]))

;; -- Prompt Templates by Mental Model ------------------------------------------

(def model-prompts
  "Prompt templates for each mental model - tailored to detect that model's patterns
   Using single LM Studio model with different prompts for each model"
  {
   ;; COGNITIVE BIASES
   "confirmation-bias"
   {:system "You are an expert in identifying cognitive biases in text. Analyze the following text for signs of Confirmation Bias - the tendency to search for, interpret, and recall information that confirms pre-existing beliefs. Look for: selective evidence, dismissing contradictions, echo chambers, cherry-picking."
    :scoring-prompt "Rate how strongly this text exhibits Confirmation Bias on a scale of 0.0 (no evidence) to 1.0 (strong evidence). Respond with ONLY a number like 0.75"}

   "availability-heuristic"
   {:system "You are an expert in identifying cognitive biases. Analyze for Availability Heuristic - judging probability by how easily examples come to mind, not actual frequency. Look for: recent events overweighted, vivid examples used as proof, frequency misjudged based on memorability."
    :scoring-prompt "Rate how strongly this text exhibits Availability Heuristic (0.0-1.0). Respond with ONLY a number."}

   "anchoring"
   {:system "You are an expert in identifying cognitive biases. Analyze for Anchoring - over-relying on the first piece of information when making decisions. Look for: initial numbers influencing judgments, insufficient adjustment from starting point, arbitrary reference points."
    :scoring-prompt "Rate Anchoring bias strength (0.0-1.0). Respond with ONLY a number."}

   "hindsight-bias"
   {:system "You are an expert in identifying cognitive biases. Analyze for Hindsight Bias - seeing past events as predictable when they weren't. Look for: 'I knew it would happen', overconfidence about past predictions, blame misattribution."
    :scoring-prompt "Rate Hindsight Bias strength (0.0-1.0). Respond with ONLY a number."}

   "survivorship-bias"
   {:system "You are an expert in identifying cognitive biases. Analyze for Survivorship Bias - focusing on successes while ignoring failures. Look for: only studying winners, ignoring failures, false pattern recognition, overoptimism."
    :scoring-prompt "Rate Survivorship Bias strength (0.0-1.0). Respond with ONLY a number."}

   "circle-of-competence"
   {:system "You are an expert in identifying decision-making patterns. Analyze for Circle of Competence - staying within areas of genuine expertise. Look for: appropriate expertise claims, acknowledging limits, staying in domain, avoiding overreach."
    :scoring-prompt "Rate how strongly this text demonstrates Circle of Competence awareness (0.0-1.0). Respond with ONLY a number."}

   "margin-of-safety"
   {:system "You are an expert in risk analysis. Analyze for Margin of Safety - building in buffer for uncertainty. Look for: conservative estimates, risk buffers, downside protection, avoiding overcommitment."
    :scoring-prompt "Rate Margin of Safety application (0.0-1.0). Respond with ONLY a number."}

   "second-order-thinking"
   {:system "You are an expert in strategic thinking. Analyze for Second-Order Thinking - considering consequences of consequences. Look for: long-term implications, unintended effects, systemic thinking, multi-step reasoning."
    :scoring-prompt "Rate Second-Order Thinking strength (0.0-1.0). Respond with ONLY a number."}

   "inversion"
   {:system "You are an expert in problem-solving. Analyze for Inversion - thinking backwards from desired outcome. Look for: reverse engineering, avoiding negatives, working backwards, opposite perspective."
    :scoring-prompt "Rate Inversion technique usage (0.0-1.0). Respond with ONLY a number."}

   "incentives"
   {:system "You are an expert in behavioral economics. Analyze for Incentive Analysis - understanding what drives behavior. Look for: recognizing motivations, aligning incentives, understanding perverse incentives, behavioral drivers."
    :scoring-prompt "Rate Incentive Analysis depth (0.0-1.0). Respond with ONLY a number."}

   "network-effects"
   {:system "You are an expert in systems analysis. Analyze for Network Effects - value increases with more users. Look for: platform dynamics, user growth benefits, lock-in effects, exponential scaling."
    :scoring-prompt "Rate Network Effects presence (0.0-1.0). Respond with ONLY a number."}

   "feedback-loops"
   {:system "You are an expert in systems thinking. Analyze for Feedback Loops - reinforcing or balancing cycles. Look for: positive feedback, negative feedback, self-reinforcing patterns, equilibrium seeking."
    :scoring-prompt "Rate Feedback Loop identification (0.0-1.0). Respond with ONLY a number."}

   "scale"
   {:system "You are an expert in systems analysis. Analyze for Scale - how systems change with size. Look for: economies of scale, diseconomies, size advantages, scaling challenges."
    :scoring-prompt "Rate Scale analysis depth (0.0-1.0). Respond with ONLY a number."}

   "moats"
   {:system "You are an expert in competitive analysis. Analyze for Competitive Moats - sustainable advantages. Look for: barriers to entry, switching costs, brand power, network effects, cost advantages."
    :scoring-prompt "Rate Moat identification (0.0-1.0). Respond with ONLY a number."}

   "compound-interest"
   {:system "You are an expert in exponential thinking. Analyze for Compound Interest - exponential growth over time. Look for: long-term accumulation, exponential thinking, time value, compounding effects."
    :scoring-prompt "Rate Compound Interest thinking (0.0-1.0). Respond with ONLY a number."}
  })

;; -- Default prompt template for unmapped models --

(def default-prompt-template
  {:system "You are an expert in identifying mental models and decision-making patterns. Analyze the following text for the presence of the mental model: {{model_name}}. Definition: {{model_description}}. Look for evidence of this model being applied or violated."
   :scoring-prompt "Rate how strongly this text exhibits {{model_name}} on a scale of 0.0 (no evidence) to 1.0 (strong evidence). Respond with ONLY a number like 0.75"})

;; -- Core Analysis Functions --------------------------------------------------

(defn get-prompt-for-model
  "Get prompt template for a specific mental model"
  [model-slug model-name model-description]
  (or (get model-prompts model-slug)
      (-> default-prompt-template
          (update :system #(str/replace % "{{model_name}}" model-name))
          (update :system #(str/replace % "{{model_description}}" model-description))
          (update :scoring-prompt #(str/replace % "{{model_name}}" model-name)))))

(defn analyze-text-for-model
  "Synchronous analysis of text against a single mental model using LM Studio"
  [text model-slug model-name model-description]
  (try
    (let [prompt-template (get-prompt-for-model model-slug model-name model-description)
          system-prompt (:system prompt-template)
          scoring-prompt (str text "\n\n" (:scoring-prompt prompt-template))
          
          response (llm/simple-chat
                    scoring-prompt
                    :system-prompt system-prompt
                    :temperature 0.3)]  ;; Low temperature for consistent scoring
      
      (if response
        (try
          (let [score (Double/parseDouble (str/trim response))]
            {:model-slug model-slug
             :model-name model-name
             :score (max 0.0 (min 1.0 score))  ;; Clamp to 0-1
             :success true})
          (catch Exception e
            (log/warn "Failed to parse score for" model-slug ":" response)
            {:model-slug model-slug
             :model-name model-name
             :score 0.0
             :success false
             :error "Invalid score format"}))
        {:model-slug model-slug
         :model-name model-name
         :score 0.0
         :success false
         :error "No response from LM"}))
    (catch Exception e
      (log/error e "Analysis failed for" model-slug)
      {:model-slug model-slug
       :model-name model-name
       :score 0.0
       :success false
       :error (.getMessage e)})))

;; -- Electric Reactive Components --

(e/defn AnalysisStream
  "Electric component that streams analysis results as they complete
   Renders live progress and updates top models in real-time"
  [text]
  (e/server
    (let [all-models (models/all-models)
          analysis-chan (async/chan (count all-models))]
      
      ;; Start async analysis in background
      (async/thread
        (doseq [model all-models]
          (let [result (analyze-text-for-model
                       text
                       (:slug model)
                       (:name model)
                       (:description model))]
            (async/>!! analysis-chan result)))
        (async/close! analysis-chan))
      
      ;; Collect results as they arrive
      (loop [results [] completed 0]
        (if-let [result (async/<!! analysis-chan)]
          (let [new-results (conj results result)
                new-completed (inc completed)]
            (e/client
              (dom/div
                (dom/p (str "Analyzed: " new-completed "/" (count all-models) " models"))
                (when (> new-completed 0)
                  (let [top-10 (->> new-results
                                   (filter :success)
                                   (sort-by :score >)
                                   (take 10))]
                    (dom/div
                      (dom/h3 "Top Models")
                      (dom/ul
                        (doseq [model top-10]
                          (dom/li
                            (str (:model-name model) " - " 
                                 (format "%.2f" (:score model))))))))))
            (recur new-results new-completed))
          results)))))

(e/defn AnalyzeText
  "Electric component for text analysis with streaming results
   Analyzes text against all 129 mental models and displays results"
  [text]
  (e/server
    (let [all-models (models/all-models)
          results (mapv #(analyze-text-for-model
                         text
                         (:slug %)
                         (:name %)
                         (:description %))
                       all-models)
          
          successful (filter :success results)
          failed (filter (complement :success) results)
          
          top-models (->> results
                         (filter :success)
                         (sort-by :score >)
                         (take 10))
          
          average-score (if (empty? successful)
                         0.0
                         (double (/ (reduce + (map :score successful))
                                   (count successful))))
          
          ;; Detect Lollapalooza (3+ models converging at > 0.7)
          high-scoring (->> results
                           (filter :success)
                           (filter #(> (:score %) 0.7)))
          
          lollapalooza-detected (>= (count high-scoring) 3)
          convergence-score (if (empty? high-scoring)
                            0.0
                            (double (/ (reduce + (map :score high-scoring))
                                      (count high-scoring))))]
      
      {:text-length (count text)
       :models-analyzed (count all-models)
       :successful-analyses (count successful)
       :failed-analyses (count failed)
       :all-scores results
       :top-10-models top-models
       :average-score average-score
       :lollapalooza-detected lollapalooza-detected
       :converging-models high-scoring
       :convergence-count (count high-scoring)
       :convergence-score convergence-score})))

(e/defn AnalysisResults
  "Electric component to display analysis results"
  [analysis]
  (e/client
    (dom/div
      (dom/h2 "Analysis Results")
      
      ;; Summary stats
      (dom/div
        (dom/p (str "Text Length: " (:text-length analysis) " characters"))
        (dom/p (str "Models Analyzed: " (:models-analyzed analysis)))
        (dom/p (str "Successful: " (:successful-analyses analysis)))
        (dom/p (str "Average Score: " (format "%.2f" (:average-score analysis)))))
      
      ;; Lollapalooza alert
      (when (:lollapalooza-detected analysis)
        (dom/div {:class "alert alert-warning"}
          (dom/h3 "🔥 LOLLAPALOOZA EFFECT DETECTED!")
          (dom/p (str "Converging Models: " (:convergence-count analysis)))
          (dom/p (str "Convergence Score: " (format "%.2f" (:convergence-score analysis))))))
      
      ;; Top 10 models table
      (dom/h3 "Top 10 Models")
      (dom/table
        (dom/thead
          (dom/tr
            (dom/th "Rank")
            (dom/th "Model")
            (dom/th "Score")
            (dom/th "Confidence")))
        (dom/tbody
          (doseq [[idx model] (map-indexed vector (:top-10-models analysis))]
            (dom/tr
              (dom/td (str (inc idx)))
              (dom/td (:model-name model))
              (dom/td (format "%.3f" (:score model)))
              (dom/td (score-to-confidence (:score model))))))))))

;; -- Utility Functions -------

(defn score-to-confidence
  "Convert 0-1 score to human-readable confidence"
  [score]
  (cond
    (>= score 0.9) "Very Strong"
    (>= score 0.7) "Strong"
    (>= score 0.5) "Moderate"
    (>= score 0.3) "Weak"
    :else "Very Weak"))

(defn get-model-by-slug
  "Get model definition by slug"
  [slug]
  (first (filter #(= (:slug %) slug) (models/all-models))))

(defn format-analysis-report
  "Format analysis results as human-readable report"
  [analysis]
  (str
   "=== MENTAL MODEL ANALYSIS REPORT ===\n"
   "Text Length: " (:text-length analysis) " characters\n"
   "Models Analyzed: " (:models-analyzed analysis) "\n"
   "Successful: " (:successful-analyses analysis) "\n"
   "Average Score: " (format "%.2f" (:average-score analysis)) "\n\n"
   
   "=== TOP 10 MODELS ===\n"
   (str/join "\n"
    (map-indexed (fn [i model]
                   (str (inc i) ". " (:model-name model)
                        " (" (format "%.2f" (:score model)) ") - "
                        (score-to-confidence (:score model))))
                 (:top-10-models analysis)))
   "\n\n"
   
   (if (:lollapalooza-detected analysis)
     (str "🔥 LOLLAPALOOZA EFFECT DETECTED!\n"
          "Converging Models: " (:convergence-count analysis) "\n"
          "Convergence Score: " (format "%.2f" (:convergence-score analysis)))
     "No Lollapalooza effect detected")))

(defn analyze-chunks
  "For very long documents, split into chunks and analyze separately"
  [text chunk-size & {:keys [overlap]}]
  (let [overlap (or overlap (/ chunk-size 4))
        chunks (loop [text text chunks []]
                 (if (<= (count text) chunk-size)
                   (conj chunks text)
                   (let [chunk (subs text 0 chunk-size)
                         remaining (subs text (- chunk-size overlap))]
                     (recur remaining (conj chunks chunk)))))]
    
    (log/info "Analyzing" (count chunks) "chunks")
    
    (mapv #(AnalyzeText %) chunks)))
