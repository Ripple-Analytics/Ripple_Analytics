(ns mental-models.services.llm
  "LLM integration service for mental model analysis"
  (:require [clj-http.client :as http]
            [jsonista.core :as json]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]))

;; -- Configuration -----------------------------------------------------------

(def api-url (or (env :built-in-forge-api-url) "https://api.manus.im"))
(def api-key (env :built-in-forge-api-key))

;; -- Core LLM Invocation -----------------------------------------------------

(defn invoke-llm
  "Invoke the LLM with messages and optional parameters"
  [{:keys [messages temperature max-tokens response-format]
    :or {temperature 0.7 max-tokens 4096}}]
  (try
    (let [body {:messages messages
                :temperature temperature
                :max_tokens max-tokens}
          body (if response-format
                 (assoc body :response_format response-format)
                 body)
          response (http/post (str api-url "/v1/chat/completions")
                              {:headers {"Authorization" (str "Bearer " api-key)
                                         "Content-Type" "application/json"}
                               :body (json/write-value-as-string body)
                               :as :json})]
      (:body response))
    (catch Exception e
      (log/error e "LLM invocation failed")
      nil)))

;; -- Mental Model Analysis ---------------------------------------------------

(def model-analysis-prompt
  "You are an expert in mental models, cognitive biases, and decision-making frameworks.
   Analyze the following text and identify which mental models are most relevant.
   
   For each model, provide:
   - name: The canonical name
   - relevance: Score from 0.0 to 1.0
   - evidence: Specific text that suggests this model applies
   - application: How this model could be applied to the situation
   
   Return as JSON with structure: {\"models\": [...], \"summary\": \"...\", \"recommendation\": \"...\"}")

(defn analyze-text-for-models
  "Analyze text to identify relevant mental models"
  [text]
  (when (and text (> (count text) 50))
    (try
      (let [response (invoke-llm
                      {:messages [{:role "system" :content model-analysis-prompt}
                                  {:role "user" :content text}]
                       :temperature 0.3
                       :response-format {:type "json_object"}})
            content (get-in response [:choices 0 :message :content])]
        (json/read-value content json/keyword-keys-object-mapper))
      (catch Exception e
        (log/warn "Model analysis failed:" (.getMessage e))
        {:models [] :summary "Analysis failed" :recommendation "Try again"}))))

;; -- Lollapalooza Detection --------------------------------------------------

(def lollapalooza-prompt
  "You are an expert in Charlie Munger's concept of 'Lollapalooza Effects' - 
   situations where multiple psychological tendencies or mental models combine 
   to produce extreme outcomes.
   
   Analyze the following situation for potential Lollapalooza effects.
   
   Identify:
   - Whether a Lollapalooza effect is present (true/false)
   - The strength of the effect (0.0 to 1.0)
   - Which mental models are converging
   - Why these models amplify each other
   - Potential outcomes (positive and negative)
   
   Return as JSON with structure:
   {\"detected\": boolean, \"strength\": number, \"models\": [...], 
    \"explanation\": \"...\", \"potential_outcomes\": {...}}")

(defn detect-lollapalooza
  "Detect Lollapalooza effects in a situation"
  [text]
  (when (and text (> (count text) 50))
    (try
      (let [response (invoke-llm
                      {:messages [{:role "system" :content lollapalooza-prompt}
                                  {:role "user" :content text}]
                       :temperature 0.4
                       :response-format {:type "json_object"}})
            content (get-in response [:choices 0 :message :content])]
        (json/read-value content json/keyword-keys-object-mapper))
      (catch Exception e
        (log/warn "Lollapalooza detection failed:" (.getMessage e))
        {:detected false :strength 0 :models [] :explanation "Detection failed"}))))

;; -- Decision Analysis -------------------------------------------------------

(def decision-analysis-prompt
  "You are a decision-making expert trained in mental models and cognitive biases.
   
   Analyze this decision and provide:
   - Key factors to consider
   - Relevant mental models to apply
   - Potential biases that might affect judgment
   - Recommended approach
   - Risk factors
   
   Return as JSON with appropriate structure.")

(defn analyze-decision
  "Analyze a decision using mental models"
  [decision-text context]
  (when decision-text
    (try
      (let [response (invoke-llm
                      {:messages [{:role "system" :content decision-analysis-prompt}
                                  {:role "user" :content (str "Decision: " decision-text
                                                              "\n\nContext: " (or context "None provided"))}]
                       :temperature 0.5
                       :response-format {:type "json_object"}})
            content (get-in response [:choices 0 :message :content])]
        (json/read-value content json/keyword-keys-object-mapper))
      (catch Exception e
        (log/warn "Decision analysis failed:" (.getMessage e))
        nil))))

;; -- Document Analysis -------------------------------------------------------

(defn analyze-document
  "Analyze a document for mental model insights"
  [document-text]
  (when (and document-text (> (count document-text) 100))
    (let [;; Chunk if too long
          chunks (if (> (count document-text) 10000)
                   (partition-all 5000 document-text)
                   [document-text])
          analyses (map #(analyze-text-for-models (apply str %)) chunks)]
      ;; Merge analyses
      {:models (distinct (mapcat :models analyses))
       :summary (clojure.string/join "\n" (map :summary analyses))
       :chunks-analyzed (count chunks)})))
