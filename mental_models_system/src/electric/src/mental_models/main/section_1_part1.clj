(ns mental-models.main.section-1-part1
  "Main Module - Section 1 Part1"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

;; ============================================

(def lm-studio-config
  "Configuration for LM Studio connection."
  {:base-url (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234")
   :model (or (System/getenv "LM_STUDIO_MODEL") "local-model")
   :timeout 60000})

(defn call-lm-studio
  "Call LM Studio's OpenAI-compatible API.
   Returns the response text or nil on error."
  [prompt & {:keys [system-prompt max-tokens temperature]
             :or {system-prompt "You are a helpful assistant that analyzes situations using mental models."
                  max-tokens 1000
                  temperature 0.7}}]
  (try
    (let [url (URL. (str (:base-url lm-studio-config) "/v1/chat/completions"))
          conn (doto (.openConnection url)
                 (.setRequestMethod "POST")
                 (.setRequestProperty "Content-Type" "application/json")
                 (.setDoOutput true)
                 (.setConnectTimeout (:timeout lm-studio-config))
                 (.setReadTimeout (:timeout lm-studio-config)))
          request-body (cheshire.core/generate-string
                        {:model (:model lm-studio-config)
                         :messages [{:role "system" :content system-prompt}
                                    {:role "user" :content prompt}]
                         :max_tokens max-tokens
                         :temperature temperature})]
      (with-open [writer (OutputStreamWriter. (.getOutputStream conn))]
        (.write writer request-body)
        (.flush writer))
      (if (= 200 (.getResponseCode conn))
        (with-open [reader (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
          (let [response (cheshire.core/parse-string (slurp reader) true)]
            (get-in response [:choices 0 :message :content])))
        (do
          (println "LM Studio error:" (.getResponseCode conn))
          nil)))
    (catch Exception e
      (println "LM Studio connection error:" (.getMessage e))
      nil)))

(defn analyze-with-llm
  "Analyze a situation using LM Studio and mental models."
  [situation model-names]
  (let [models-context (if (empty? model-names)
                         (take 10 (models/get-all-models))
                         (map models/get-model model-names))
        models-str (str/join "\n\n" 
                            (map (fn [m]
                                   (str "**" (:name m) "** (" (:category m) ")\n"
                                        "Description: " (:description m) "\n"
                                        "Key Insight: " (:key-insight m) "\n"
                                        "Application: " (:application m)))
                                 models-context))
        prompt (str "Analyze the following situation using these mental models:\n\n"
                   "MENTAL MODELS:\n" models-str "\n\n"
                   "SITUATION:\n" situation "\n\n"
                   "Provide a comprehensive analysis that:\n"
                   "1. Identifies which mental models are most relevant\n"
                   "2. Explains how each relevant model applies\n"
                   "3. Identifies potential failure modes to watch for\n"
                   "4. Provides actionable recommendations\n"
                   "5. Notes any lollapalooza effects (multiple models reinforcing each other)")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :analysis response
       :models-used (map :name models-context)
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio. Make sure it's running on localhost:1234"
       :fallback (analysis/latticework-analyze model-names situation)
       :llm-powered false})))

(defn detect-biases-with-llm
  "Detect cognitive biases in text using LM Studio."
  [text]
  (let [bias-models ["confirmation-bias" "hindsight-bias" "availability-heuristic" 
                     "loss-aversion" "social-proof" "anchoring-negotiation"
                     "dunning-kruger" "status-quo-bias" "narrative-fallacy"]
        prompt (str "Analyze the following text for cognitive biases:\n\n"
                   "TEXT:\n" text "\n\n"
                   "Look for these specific biases:\n"
                   "- Confirmation bias: seeking confirming evidence\n"
                   "- Hindsight bias: believing past was predictable\n"
                   "- Availability heuristic: overweighting recent/vivid events\n"
                   "- Loss aversion: overweighting losses vs gains\n"
                   "- Social proof: following the crowd\n"
                   "- Anchoring: being influenced by first numbers\n"
                   "- Dunning-Kruger: overconfidence without competence\n"
                   "- Status quo bias: preferring current state\n"
                   "- Narrative fallacy: creating stories for random events\n\n"
                   "For each bias detected, explain:\n"
                   "1. What specific phrases or patterns indicate the bias\n"
                   "2. How severe the bias appears (low/medium/high)\n"
                   "3. Recommendations to counteract the bias")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :analysis response
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (analysis/detect-biases text)
       :llm-powered false})))

(defn generate-decision-checklist-with-llm
  "Generate a decision checklist using LM Studio."
  [decision-context]
  (let [prompt (str "Create a comprehensive decision checklist for the following situation:\n\n"
                   "DECISION CONTEXT:\n" decision-context "\n\n"
                   "Generate a checklist that includes:\n"
                   "1. Key questions to answer before deciding\n"
                   "2. Information gaps to fill\n"
                   "3. Stakeholders to consult\n"
                   "4. Potential failure modes to consider\n"
                   "5. Second-order effects to anticipate\n"
                   "6. Reversibility assessment\n"
                   "7. Opportunity cost analysis\n"
                   "8. Pre-mortem: what could go wrong?\n"
                   "9. Kill criteria: when to abandon this path\n"
                   "10. Success metrics: how will you know it worked?")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :checklist response
       :llm-powered true}
      {:success false
       :error "Could not connect to LM Studio"
       :fallback (analysis/decision-checklist decision-context)
       :llm-powered false})))

(defn classify-document-with-llm
  "Classify a document by relevant mental models using LM Studio."
  [text]
  (let [categories (models/get-all-categories)
        prompt (str "Classify the following document by which mental models are most relevant:\n\n"
                   "DOCUMENT:\n" text "\n\n"
                   "Available categories: " (str/join ", " categories) "\n\n"
                   "For each relevant mental model:\n"
                   "1. Name the model and category\n"
                   "2. Explain why it's relevant to this document\n"
                   "3. Rate relevance (1-10)\n"
                   "4. Identify specific passages that relate to the model\n\n"
                   "Also identify:\n"
                   "- The primary theme/topic of the document\n"
                   "- Key decisions or situations described\n"
                   "- Potential biases in the author's perspective")]
    (if-let [response (call-lm-studio prompt)]
      {:success true
       :classification response
