(ns mental-models.analysis.core
  "Mental Model Analysis Engine - Electric Clojure
   Replaces Python mental_model_analyzer.py
   Analyzes text against all 129 mental models with LM Studio integration"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [mental-models.services.lm-studio :as llm]
            [mental-models.data.models :as models]
            [mental-models.db.analysis :as db-analysis]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [java.time :refer [Instant]]))

;; -- Analysis Types --

(def analysis-types
  {:classify "classify"
   :detect-bias "detect_bias"
   :find-patterns "find_patterns"
   :lollapalooza "lollapalooza"
   :invert "invert"
   :full "full"})

;; -- Data Structures --

(defrecord ModelMatch
  [model-id model-name relevance-score explanation evidence category])

(defrecord BiasDetection
  [bias-name confidence evidence mitigation])

(defrecord DocumentAnalysis
  [document-id document-path chunk-id content-preview
   applicable-models detected-biases patterns
   lollapalooza-score lollapalooza-models key-insights
   inverted-perspective analyzed-at metadata])

;; -- Configuration --

(def default-config
  {:lm-studio-url (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234")
   :model-name (or (System/getenv "LM_MODEL_NAME") "local-model")
   :temperature 0.3
   :max-tokens 2048})

;; -- Core Analysis Functions --

(defn classify-models
  "Classify which mental models apply to text"
  [text]
  (let [all-models (models/all-models)
        results (mapv (fn [model]
                       (let [response (llm/simple-chat
                                      (str text "\n\nWhich mental models from the list apply here? "
                                           "Model: " (:name model) "\n"
                                           "Definition: " (:description model) "\n"
                                           "Rate applicability 0.0-1.0. Respond with ONLY a number.")
                                      :system-prompt "You are an expert in mental models. Rate how strongly each model applies."
                                      :temperature (:temperature default-config))]
                         (if response
                           (try
                             (let [score (Double/parseDouble (str/trim response))]
                               (->ModelMatch
                                (:id model)
                                (:name model)
                                (max 0.0 (min 1.0 score))
                                (str "Model detected with " (format "%.1f%%" (* score 100)) " confidence")
                                []
                                (:category model)))
                             (catch Exception e
                               (log/warn "Failed to parse score for" (:name model))
                               (->ModelMatch (:id model) (:name model) 0.0 "Parse error" [] (:category model))))
                           (->ModelMatch (:id model) (:name model) 0.0 "No response" [] (:category model)))))
                     all-models)]
    
    ;; Filter to top models
    (->> results
         (filter #(> (:relevance-score %) 0.3))
         (sort-by :relevance-score >)
         (take 20))))

(defn detect-biases
  "Detect cognitive biases in text"
  [text]
  (let [bias-models (filter #(= (:category %) "Cognitive Biases") (models/all-models))
        results (mapv (fn [model]
                       (let [response (llm/simple-chat
                                      (str text "\n\nDoes this text exhibit the bias: " (:name model) "?\n"
                                           "Respond with JSON: {\"detected\": true/false, \"confidence\": 0.0-1.0, \"evidence\": \"...\", \"mitigation\": \"...\"}")
                                      :system-prompt "You are an expert in cognitive biases. Analyze for bias presence."
                                      :temperature (:temperature default-config))]
                         (if response
                           (try
                             (let [parsed (clojure.data.json/read-str response :key-fn keyword)]
                               (when (:detected parsed)
                                 (->BiasDetection
                                  (:name model)
                                  (:confidence parsed)
                                  (:evidence parsed)
                                  (:mitigation parsed))))
                             (catch Exception e
                               (log/warn "Failed to parse bias detection for" (:name model))
                               nil))
                           nil)))
                     bias-models)]
    
    (filter some? results)))

(defn find-patterns
  "Find recurring patterns in text"
  [text]
  (let [response (llm/simple-chat
                 (str text "\n\nWhat are the key decision-making patterns visible in this text? "
                      "List as JSON array of strings.")
                 :system-prompt "You are an expert in identifying decision-making patterns."
                 :temperature (:temperature default-config))]
    
    (if response
      (try
        (clojure.data.json/read-str response)
        (catch Exception e
          (log/warn "Failed to parse patterns")
          []))
      [])))

(defn detect-lollapalooza
  "Detect Lollapalooza effect - 3+ models converging"
  [applicable-models]
  (let [high-scoring (filter #(> (:relevance-score %) 0.7) applicable-models)]
    {:detected (>= (count high-scoring) 3)
     :score (if (empty? high-scoring)
             0.0
             (double (/ (reduce + (map :relevance-score high-scoring))
                       (count high-scoring))))
     :models (map :model-name high-scoring)}))

(defn invert-perspective
  "Generate inverted perspective on situation"
  [text]
  (let [response (llm/simple-chat
                 (str text "\n\nWhat is the opposite or inverted perspective on this situation? "
                      "How would someone with opposite beliefs view this?")
                 :system-prompt "You are an expert in inversion and perspective-taking. Generate the opposite viewpoint."
                 :temperature 0.7)]  ;; Higher temp for creative inversion
    
    (or response "Unable to generate inverted perspective")))

(defn extract-key-insights
  "Extract key insights from analysis"
  [text applicable-models]
  (let [model-names (str/join ", " (map :model-name (take 5 applicable-models)))
        response (llm/simple-chat
                 (str text "\n\nGiven that these mental models apply: " model-names
                      "\n\nWhat are the 3-5 key insights or implications? "
                      "List as JSON array of strings.")
                 :system-prompt "You are an expert analyst. Extract key insights."
                 :temperature (:temperature default-config))]
    
    (if response
      (try
        (clojure.data.json/read-str response)
        (catch Exception e
          (log/warn "Failed to parse insights")
          []))
      [])))

;; -- Full Analysis Pipeline --

(defn analyze-document
  "Complete analysis of a document"
  [{:keys [document-id document-path content metadata analysis-type]
    :or {analysis-type :full}}]
  
  (log/info "Starting analysis of" document-id)
  
  (let [start-time (Instant/now)
        
        ;; Run analyses
        applicable-models (classify-models content)
        detected-biases (when (#{:detect-bias :full} analysis-type)
                         (detect-biases content))
        patterns (when (#{:find-patterns :full} analysis-type)
                  (find-patterns content))
        lollapalooza (detect-lollapalooza applicable-models)
        inverted (when (#{:invert :full} analysis-type)
                  (invert-perspective content))
        insights (when (#{:full} analysis-type)
                  (extract-key-insights content applicable-models))
        
        end-time (Instant/now)
        duration-ms (- (.toEpochMilli end-time) (.toEpochMilli start-time))]
    
    (log/info "Analysis completed in" duration-ms "ms")
    
    (->DocumentAnalysis
     document-id
     document-path
     nil
     (subs content 0 (min 200 (count content)))
     applicable-models
     (or detected-biases [])
     (or patterns [])
     (:score lollapalooza)
     (:models lollapalooza)
     (or insights [])
     (or inverted "")
     start-time
     (assoc metadata
           :duration-ms duration-ms
           :models-analyzed (count (models/all-models))
           :top-models-count (count applicable-models)))))

;; -- Electric Reactive Components --

(e/defn AnalysisProgress
  "Display real-time analysis progress"
  [total-models completed]
  (e/client
    (dom/div {:class "analysis-progress"}
      (dom/div {:class "progress-bar"}
        (dom/div {:class "progress-fill"
                 :style {:width (str (/ completed total-models 100) "%")}}))
      (dom/p (str "Analyzed " completed " of " total-models " models")))))

(e/defn DocumentAnalysisComponent
  "Display document analysis results"
  [analysis]
  (e/client
    (dom/div {:class "document-analysis"}
      (dom/h2 "Analysis Results")
      
      ;; Summary
      (dom/div {:class "summary"}
        (dom/p (str "Document: " (:document-id analysis)))
        (dom/p (str "Analyzed at: " (:analyzed-at analysis))))
      
      ;; Top Models
      (dom/h3 "Applicable Models")
      (dom/ul
        (doseq [model (take 10 (:applicable-models analysis))]
          (dom/li
            (str (:model-name model) " - " 
                 (format "%.1f%%" (* (:relevance-score model) 100))))))
      
      ;; Biases
      (when (seq (:detected-biases analysis))
        (dom/div
          (dom/h3 "Detected Biases")
          (dom/ul
            (doseq [bias (:detected-biases analysis)]
              (dom/li
                (str (:bias-name bias) " - " 
                     (format "%.1f%%" (* (:confidence bias) 100))))))))
      
      ;; Lollapalooza
      (when (> (:lollapalooza-score analysis) 0.5)
        (dom/div {:class "lollapalooza-alert"}
          (dom/h3 "🔥 Lollapalooza Effect Detected!")
          (dom/p (str "Score: " (format "%.2f" (:lollapalooza-score analysis))))
          (dom/p (str "Converging Models: " (str/join ", " (:lollapalooza-models analysis))))))
      
      ;; Patterns
      (when (seq (:patterns analysis))
        (dom/div
          (dom/h3 "Patterns")
          (dom/ul
            (doseq [pattern (:patterns analysis)]
              (dom/li pattern)))))
      
      ;; Key Insights
      (when (seq (:key-insights analysis))
        (dom/div
          (dom/h3 "Key Insights")
          (dom/ul
            (doseq [insight (:key-insights analysis)]
              (dom/li insight)))))
      
      ;; Inverted Perspective
      (when (seq (:inverted-perspective analysis))
        (dom/div
          (dom/h3 "Inverted Perspective")
          (dom/p (:inverted-perspective analysis)))))))

;; -- Batch Analysis --

(defn analyze-batch
  "Analyze multiple documents"
  [documents]
  (log/info "Starting batch analysis of" (count documents) "documents")
  
  (mapv #(analyze-document %) documents))

;; -- Export for use in other modules --

(defn get-analyzer-config []
  default-config)

(defn set-analyzer-config! [config]
  (alter-var-root #'default-config merge config))
