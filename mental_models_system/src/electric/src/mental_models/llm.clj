(ns mental-models.llm
  "Local LLM Integration Layer - Electric Clojure
   
   Unified LLM client supporting multiple backends:
   - LM Studio (primary - saves memory with single model)
   - Ollama
   - llama.cpp
   - vLLM
   - Any OpenAI-compatible endpoint
   
   Uses a single model with different prompts for each mental model.
   
   Architecture:
   ┌─────────────────────────────────────────────────────────────────┐
   │                    Local LLM Integration Layer                   │
   ├─────────────────────────────────────────────────────────────────┤
   │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
   │  │LM Studio│  │ Ollama  │  │llama.cpp│  │  vLLM   │           │
   │  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘           │
   │       └────────────┴────────────┴────────────┘                 │
   │                          │                                      │
   │              ┌───────────┴───────────┐                         │
   │              │   Unified LLM Client  │                         │
   │              └───────────┬───────────┘                         │
   │                          │                                      │
   │  ┌───────────────────────┴───────────────────────┐             │
   │  │           Batch Processing Engine              │             │
   │  │  - Parallel document processing                │             │
   │  │  - Mental model prompt routing                │             │
   │  │  - Result aggregation                         │             │
   │  └───────────────────────────────────────────────┘             │
   └─────────────────────────────────────────────────────────────────┘"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.algorithms :as algo])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader OutputStreamWriter]))

;; ============================================
;; Configuration (Single Model to Save Memory)
;; ============================================

(def ^:dynamic *llm-config*
  "LLM configuration - uses single model with different prompts."
  {:backend :lm-studio
   :base-url (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234")
   :model (or (System/getenv "LM_STUDIO_MODEL") "local-model")
   :max-tokens 2000
   :temperature 0.7
   :timeout 60000
   :max-concurrent 5})

(def backend-configs
  "Supported backend configurations."
  {:lm-studio {:endpoint "/v1/chat/completions"
               :health-endpoint "/v1/models"
               :format :openai}
   :ollama {:endpoint "/api/generate"
            :health-endpoint "/api/tags"
            :format :ollama}
   :llama-cpp {:endpoint "/completion"
               :health-endpoint "/health"
               :format :llama-cpp}
   :vllm {:endpoint "/v1/completions"
          :health-endpoint "/health"
          :format :openai}
   :openai-compatible {:endpoint "/v1/chat/completions"
                       :health-endpoint "/v1/models"
                       :format :openai}})

;; ============================================
;; HTTP Client
;; ============================================

(defn- json-encode [data]
  "Simple JSON encoding."
  (letfn [(encode-value [v]
            (cond
              (nil? v) "null"
              (string? v) (str "\"" (str/escape v {\" "\\\"" \\ "\\\\" \newline "\\n" \return "\\r" \tab "\\t"}) "\"")
              (number? v) (str v)
              (boolean? v) (if v "true" "false")
              (keyword? v) (encode-value (name v))
              (map? v) (str "{" (str/join "," (map (fn [[k v]] (str (encode-value (name k)) ":" (encode-value v))) v)) "}")
              (sequential? v) (str "[" (str/join "," (map encode-value v)) "]")
              :else (encode-value (str v))))]
    (encode-value data)))

(defn- json-decode [s]
  "Simple JSON decoding using read-string with safety."
  (try
    (-> s
        (str/replace #"\"([^\"]+)\":" "#_$1 ")
        (str/replace "null" "nil")
        (str/replace "true" "true")
        (str/replace "false" "false")
        read-string)
    (catch Exception e
      {:error (str "JSON parse error: " (.getMessage e))})))

(defn http-post
  "Make HTTP POST request."
  [url body & {:keys [timeout headers]
               :or {timeout 60000 headers {}}}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "POST")
                 (.setRequestProperty "Content-Type" "application/json")
                 (.setDoOutput true)
                 (.setConnectTimeout timeout)
                 (.setReadTimeout timeout))]
      ;; Add custom headers
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      
      ;; Write body
      (with-open [writer (OutputStreamWriter. (.getOutputStream conn))]
        (.write writer (json-encode body))
        (.flush writer))
      
      ;; Read response
      (let [status (.getResponseCode conn)]
        (if (= 200 status)
          (with-open [reader (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
            {:success true
             :status status
             :body (json-decode (slurp reader))})
          {:success false
           :status status
           :error (str "HTTP error: " status)})))
    (catch Exception e
      {:success false
       :error (.getMessage e)})))

(defn http-get
  "Make HTTP GET request."
  [url & {:keys [timeout]
          :or {timeout 5000}}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout timeout)
                 (.setReadTimeout timeout))]
      (let [status (.getResponseCode conn)]
        {:success (= 200 status)
         :status status}))
    (catch Exception e
      {:success false
       :error (.getMessage e)})))

;; ============================================
;; LLM Client Protocol
;; ============================================

(defprotocol LLMClient
  "Protocol for LLM clients."
  (generate [this prompt opts] "Generate response from prompt.")
  (health-check [this] "Check if LLM service is healthy."))

;; ============================================
;; LM Studio Client (Primary - Single Model)
;; ============================================

(defrecord LMStudioClient [config]
  LLMClient
  (generate [this prompt opts]
    (let [url (str (:base-url config) "/v1/chat/completions")
          system-prompt (or (:system opts) "You are a helpful assistant that analyzes situations using mental models.")
          body {:model (:model config)
                :messages [{:role "system" :content system-prompt}
                           {:role "user" :content prompt}]
                :max_tokens (or (:max-tokens opts) (:max-tokens config))
                :temperature (or (:temperature opts) (:temperature config))}
          response (http-post url body :timeout (:timeout config))]
      (if (:success response)
        {:success true
         :content (get-in (:body response) [:choices 0 :message :content])
         :model (:model config)
         :backend :lm-studio}
        {:success false
         :error (:error response)
         :backend :lm-studio})))
  
  (health-check [this]
    (let [url (str (:base-url config) "/v1/models")
          response (http-get url :timeout 5000)]
      (:success response))))

;; ============================================
;; Ollama Client
;; ============================================

(defrecord OllamaClient [config]
  LLMClient
  (generate [this prompt opts]
    (let [url (str (:base-url config) "/api/generate")
          body {:model (:model config)
                :prompt prompt
                :stream false
                :options {:num_predict (or (:max-tokens opts) (:max-tokens config))
                          :temperature (or (:temperature opts) (:temperature config))}}
          response (http-post url body :timeout (:timeout config))]
      (if (:success response)
        {:success true
         :content (get-in (:body response) [:response])
         :model (:model config)
         :backend :ollama}
        {:success false
         :error (:error response)
         :backend :ollama})))
  
  (health-check [this]
    (let [url (str (:base-url config) "/api/tags")
          response (http-get url :timeout 5000)]
      (:success response))))

;; ============================================
;; OpenAI-Compatible Client (vLLM, TGI, etc.)
;; ============================================

(defrecord OpenAICompatibleClient [config]
  LLMClient
  (generate [this prompt opts]
    (let [url (str (:base-url config) "/v1/chat/completions")
          system-prompt (or (:system opts) "You are a helpful assistant.")
          headers (if-let [api-key (:api-key config)]
                    {"Authorization" (str "Bearer " api-key)}
                    {})
          body {:model (:model config)
                :messages [{:role "system" :content system-prompt}
                           {:role "user" :content prompt}]
                :max_tokens (or (:max-tokens opts) (:max-tokens config))
                :temperature (or (:temperature opts) (:temperature config))}
          response (http-post url body :timeout (:timeout config) :headers headers)]
      (if (:success response)
        {:success true
         :content (get-in (:body response) [:choices 0 :message :content])
         :model (:model config)
         :backend :openai-compatible}
        {:success false
         :error (:error response)
         :backend :openai-compatible})))
  
  (health-check [this]
    (let [url (str (:base-url config) "/v1/models")
          response (http-get url :timeout 5000)]
      (:success response))))

;; ============================================
;; Unified LLM Client (with Failover)
;; ============================================

(defn create-client
  "Create LLM client based on backend type."
  [config]
  (case (:backend config)
    :lm-studio (->LMStudioClient config)
    :ollama (->OllamaClient config)
    :openai-compatible (->OpenAICompatibleClient config)
    (->LMStudioClient config)))

(def ^:private default-client
  "Default LLM client instance."
  (delay (create-client *llm-config*)))

(defn get-client
  "Get or create LLM client."
  []
  @default-client)

;; ============================================
;; Mental Model Analysis Functions
;; ============================================

(defn analyze-with-model
  "Analyze text using a specific mental model's prompt.
   Uses single LLM model with model-specific prompt."
  [model-name text & {:keys [client]
                      :or {client (get-client)}}]
  (if-let [prompt-config (algo/get-model-prompt model-name)]
    (let [prompt (str/replace (:user-template prompt-config) "{{text}}" text)
          result (generate client prompt {:system (:system prompt-config)})]
      (if (:success result)
        {:success true
         :model model-name
         :analysis (:content result)
         :llm-powered true}
        {:success false
         :model model-name
         :error (:error result)
         :fallback (algo/comprehensive-analysis text)
         :llm-powered false}))
    {:success false
     :error (str "Unknown model: " model-name)}))

(defn analyze-with-top-models
  "Analyze text using the most relevant mental models."
  [text & {:keys [n client]
           :or {n 3 client (get-client)}}]
  (let [top-models (algo/get-top-models text n)]
    {:relevant-models (map :model top-models)
     :analyses (for [{:keys [model relevance]} top-models]
                 (assoc (analyze-with-model model text :client client)
                        :relevance relevance))}))

(defn detect-biases
  "Detect cognitive biases in text using LLM."
  [text & {:keys [client]
           :or {client (get-client)}}]
  (let [bias-models ["confirmation-bias" "hindsight-bias" "availability-heuristic"
                     "loss-aversion" "social-proof" "anchoring"
                     "dunning-kruger" "status-quo-bias" "narrative-fallacy"]
        prompt (str "Analyze the following text for cognitive biases:\n\n"
                    "TEXT:\n" text "\n\n"
                    "Look for these specific biases:\n"
                    (str/join "\n" (map #(str "- " (str/replace % "-" " ")) bias-models))
                    "\n\nFor each bias detected, explain:\n"
                    "1. What specific phrases or patterns indicate the bias\n"
                    "2. How severe the bias appears (low/medium/high)\n"
                    "3. Recommendations to counteract the bias")
        result (generate client prompt {:system "You are an expert at detecting cognitive biases in text."})]
    (if (:success result)
      {:success true
       :analysis (:content result)
       :biases-checked bias-models
       :llm-powered true}
      {:success false
       :error (:error result)
       :fallback (algo/analyze-failure-modes text)
       :llm-powered false})))

(defn detect-lollapalooza
  "Detect lollapalooza effects (multiple reinforcing biases) using LLM."
  [text & {:keys [client]
           :or {client (get-client)}}]
  (let [rule-based (algo/detect-lollapalooza text)
        prompt (str "Analyze the following text for LOLLAPALOOZA EFFECTS - "
                    "situations where multiple mental models or biases combine "
                    "to create powerful reinforcing effects:\n\n"
                    "TEXT:\n" text "\n\n"
                    "Identify:\n"
                    "1. Which mental models/biases are present?\n"
                    "2. How do they reinforce each other?\n"
                    "3. What is the combined effect?\n"
                    "4. How dangerous is this combination?\n"
                    "5. What safeguards should be applied?")
        result (generate client prompt {:system "You are Charlie Munger analyzing for lollapalooza effects."})]
    {:rule-based-detection rule-based
     :llm-analysis (if (:success result)
                     {:success true
                      :analysis (:content result)
                      :llm-powered true}
                     {:success false
                      :error (:error result)
                      :llm-powered false})}))

(defn generate-decision-checklist
  "Generate a decision checklist using mental models."
  [decision-context & {:keys [client]
                       :or {client (get-client)}}]
  (let [prompt (str "Create a comprehensive decision checklist for:\n\n"
                    "DECISION CONTEXT:\n" decision-context "\n\n"
                    "Generate a checklist that includes:\n"
                    "1. Key questions to answer before deciding\n"
                    "2. Information gaps to fill\n"
                    "3. Stakeholders to consult\n"
                    "4. Potential failure modes to consider (use mental models)\n"
                    "5. Second-order effects to anticipate\n"
                    "6. Reversibility assessment\n"
                    "7. Opportunity cost analysis\n"
                    "8. Pre-mortem: what could go wrong?\n"
                    "9. Kill criteria: when to abandon this path\n"
                    "10. Success metrics: how will you know it worked?")
        result (generate client prompt {:system "You are a decision-making expert using Charlie Munger's mental models framework."})]
    (if (:success result)
      {:success true
       :checklist (:content result)
       :llm-powered true}
      {:success false
       :error (:error result)
       :llm-powered false})))

;; ============================================
;; Batch Processing
;; ============================================

(defn process-batch
  "Process multiple texts in parallel."
  [texts model-name & {:keys [max-concurrent]
                       :or {max-concurrent 5}}]
  (let [client (get-client)
        results (atom [])
        process-one (fn [idx text]
                      (let [result (analyze-with-model model-name text :client client)]
                        (swap! results conj (assoc result :index idx))))]
    ;; Process in batches
    (doseq [batch (partition-all max-concurrent (map-indexed vector texts))]
      (doseq [[idx text] batch]
        (process-one idx text)))
    (sort-by :index @results)))

;; ============================================
;; Document Processing Pipeline
;; ============================================

(defn chunk-text
  "Split text into chunks for processing."
  [text & {:keys [chunk-size overlap]
           :or {chunk-size 4000 overlap 200}}]
  (let [words (str/split text #"\s+")
        chunk-words (int (/ chunk-size 5)) ; ~5 chars per word
        overlap-words (int (/ overlap 5))]
    (loop [remaining words
           chunks []]
      (if (empty? remaining)
        chunks
        (let [chunk (take chunk-words remaining)
              next-start (max 1 (- chunk-words overlap-words))]
          (recur (drop next-start remaining)
                 (conj chunks (str/join " " chunk))))))))

(defn process-document
  "Process a document through the mental models analysis pipeline."
  [text & {:keys [models chunk-size]
           :or {models nil chunk-size 4000}}]
  (let [chunks (chunk-text text :chunk-size chunk-size)
        client (get-client)
        
        ;; Analyze each chunk
        chunk-analyses (for [chunk chunks]
                         (if models
                           {:analyses (for [model models]
                                        (analyze-with-model model chunk :client client))}
                           (analyze-with-top-models chunk :client client)))
        
        ;; Aggregate results
        all-models (distinct (mapcat #(map :model (:analyses %)) chunk-analyses))
        lollapalooza (detect-lollapalooza text :client client)]
    
    {:document-length (count text)
     :chunks-processed (count chunks)
     :models-applied all-models
     :chunk-analyses chunk-analyses
     :lollapalooza-detection lollapalooza
     :summary (generate client 
                        (str "Summarize the key mental model insights from this analysis:\n\n"
                             "Document: " (subs text 0 (min 1000 (count text))) "...\n\n"
                             "Models applied: " (str/join ", " all-models))
                        {:system "You are summarizing mental model analysis results."})}))

;; ============================================
;; API Functions for Electric Clojure UI
;; ============================================

(defn check-llm-status
  "Check if LLM is connected and healthy."
  []
  (let [client (get-client)
        healthy (health-check client)]
    {:status (if healthy "connected" "disconnected")
     :backend (:backend *llm-config*)
     :model (:model *llm-config*)
     :base-url (:base-url *llm-config*)}))

(defn quick-analyze
  "Quick analysis endpoint for UI."
  [text]
  (let [relevant (algo/get-top-models text 3)
        client (get-client)]
    {:relevant-models (map :model relevant)
     :analysis (when (seq relevant)
                 (analyze-with-model (:model (first relevant)) text :client client))
     :lollapalooza (algo/detect-lollapalooza text)
     :failure-modes (algo/analyze-failure-modes text)}))
