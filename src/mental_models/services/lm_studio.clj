(ns mental-models.services.lm-studio
  "LM Studio HTTP Client - Local LLM Integration
   Connects to LM Studio server at localhost:1234 for inference"
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]))

;; -- Configuration -----------------------------------------------------------

(def default-config
  {:base-url (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234")
   :api-version "v1"
   :timeout 60000
   :default-model "local-model"})

(defn api-url [endpoint]
  (str (:base-url default-config) "/" (:api-version default-config) "/" endpoint))

;; -- HTTP Helpers ------------------------------------------------------------

(defn- make-request
  "Make HTTP request to LM Studio API"
  [method endpoint body]
  (try
    (let [response (http/request
                    {:method method
                     :url (api-url endpoint)
                     :headers {"Content-Type" "application/json"}
                     :body (when body (json/generate-string body))
                     :socket-timeout (:timeout default-config)
                     :connection-timeout 5000
                     :as :json})]
      {:success true
       :data (:body response)})
    (catch Exception e
      (log/error e "LM Studio request failed")
      {:success false
       :error (.getMessage e)})))

;; -- Chat Completions --------------------------------------------------------

(defn chat-completion
  "Send chat completion request to LM Studio
   
   Options:
   - :messages - Vector of {:role :content} maps
   - :model - Model name (optional, uses loaded model)
   - :temperature - Sampling temperature (0.0-2.0)
   - :max-tokens - Maximum tokens to generate
   - :stream - Whether to stream response (default false)
   - :stop - Stop sequences"
  [{:keys [messages model temperature max-tokens stream stop]
    :or {temperature 0.7 max-tokens 2048 stream false}}]
  (make-request
   :post
   "chat/completions"
   {:model (or model (:default-model default-config))
    :messages messages
    :temperature temperature
    :max_tokens max-tokens
    :stream stream
    :stop stop}))

(defn simple-chat
  "Simple chat helper - single user message, returns content string"
  [user-message & {:keys [system-prompt temperature]
                   :or {temperature 0.7}}]
  (let [messages (cond-> []
                   system-prompt (conj {:role "system" :content system-prompt})
                   true (conj {:role "user" :content user-message}))
        response (chat-completion {:messages messages :temperature temperature})]
    (if (:success response)
      (get-in response [:data :choices 0 :message :content])
      nil)))

;; -- Completions (Legacy) ----------------------------------------------------

(defn completion
  "Send completion request (legacy endpoint)"
  [{:keys [prompt model temperature max-tokens]
    :or {temperature 0.7 max-tokens 2048}}]
  (make-request
   :post
   "completions"
   {:model (or model (:default-model default-config))
    :prompt prompt
    :temperature temperature
    :max_tokens max-tokens}))

;; -- Embeddings --------------------------------------------------------------

(defn get-embedding
  "Get embedding vector for text"
  [text & {:keys [model]}]
  (let [response (make-request
                  :post
                  "embeddings"
                  {:model (or model (:default-model default-config))
                   :input text})]
    (if (:success response)
      (get-in response [:data :data 0 :embedding])
      nil)))

(defn get-embeddings
  "Get embedding vectors for multiple texts"
  [texts & {:keys [model]}]
  (let [response (make-request
                  :post
                  "embeddings"
                  {:model (or model (:default-model default-config))
                   :input texts})]
    (if (:success response)
      (mapv :embedding (get-in response [:data :data]))
      nil)))

;; -- Model Management --------------------------------------------------------

(defn list-models
  "List available models in LM Studio"
  []
  (make-request :get "models" nil))

(defn get-model-info
  "Get information about a specific model"
  [model-id]
  (make-request :get (str "models/" model-id) nil))

;; -- Health Check ------------------------------------------------------------

(defn health-check
  "Check if LM Studio server is running and responsive"
  []
  (try
    (let [response (http/get (str (:base-url default-config) "/v1/models")
                             {:socket-timeout 5000
                              :connection-timeout 2000
                              :as :json})]
      {:healthy true
       :models (get-in response [:body :data])})
    (catch Exception e
      {:healthy false
       :error (.getMessage e)})))

;; -- Mental Models Analysis --------------------------------------------------

(def analysis-system-prompt
  "You are an expert in mental models, cognitive biases, and decision-making frameworks.
   Your role is to analyze text and identify which mental models are relevant.
   
   When analyzing, consider:
   1. Which mental models directly apply to the situation
   2. Potential cognitive biases at play
   3. Failure modes to watch out for
   4. Recommended actions based on the models
   
   Be specific and cite the exact mental models by name.
   Provide confidence scores (0.0-1.0) for each identified model.")

(defn analyze-text
  "Analyze text for mental models"
  [text]
  (let [response (chat-completion
                  {:messages [{:role "system" :content analysis-system-prompt}
                              {:role "user" :content (str "Analyze this text for relevant mental models:\n\n" text)}]
                   :temperature 0.3
                   :max-tokens 2048})]
    (if (:success response)
      {:analysis (get-in response [:data :choices 0 :message :content])
       :usage (get-in response [:data :usage])}
      {:error (:error response)})))

(defn identify-models
  "Identify specific mental models in text, return structured data"
  [text model-list]
  (let [prompt (str "Given this list of mental models:\n"
                    (clojure.string/join ", " (map :name model-list))
                    "\n\nIdentify which models are relevant to this text. "
                    "Return a JSON array of objects with 'model_name', 'relevance_score' (0-1), and 'explanation'.\n\n"
                    "Text to analyze:\n" text)
        response (chat-completion
                  {:messages [{:role "system" :content "You are a mental models expert. Return only valid JSON."}
                              {:role "user" :content prompt}]
                   :temperature 0.2
                   :max-tokens 2048})]
    (if (:success response)
      (try
        (json/parse-string (get-in response [:data :choices 0 :message :content]) true)
        (catch Exception e
          (log/warn "Failed to parse model identification response")
          []))
      [])))

(defn suggest-models-for-decision
  "Suggest mental models for a decision context"
  [decision-context options]
  (let [prompt (str "A person is making a decision:\n\n"
                    "Context: " decision-context "\n\n"
                    "Options being considered:\n" options "\n\n"
                    "Recommend the top 5 mental models that would help with this decision. "
                    "For each, explain why it's relevant and how to apply it.")
        response (chat-completion
                  {:messages [{:role "system" :content analysis-system-prompt}
                              {:role "user" :content prompt}]
                   :temperature 0.4
                   :max-tokens 2048})]
    (if (:success response)
      (get-in response [:data :choices 0 :message :content])
      nil)))

(defn detect-cognitive-biases
  "Detect potential cognitive biases in reasoning"
  [reasoning-text]
  (let [prompt (str "Analyze this reasoning for potential cognitive biases:\n\n"
                    reasoning-text "\n\n"
                    "Identify any biases present, rate their severity (low/medium/high), "
                    "and suggest how to mitigate them.")
        response (chat-completion
                  {:messages [{:role "system" :content "You are an expert in cognitive biases and critical thinking."}
                              {:role "user" :content prompt}]
                   :temperature 0.3
                   :max-tokens 1024})]
    (if (:success response)
      (get-in response [:data :choices 0 :message :content])
      nil)))

(defn generate-pre-mortem
  "Generate a pre-mortem analysis for a decision"
  [decision-description]
  (let [prompt (str "Perform a pre-mortem analysis for this decision:\n\n"
                    decision-description "\n\n"
                    "Imagine it's 6 months from now and this decision has failed spectacularly. "
                    "What went wrong? List the most likely failure modes and how to prevent them.")
        response (chat-completion
                  {:messages [{:role "system" :content "You are a strategic advisor skilled in pre-mortem analysis."}
                              {:role "user" :content prompt}]
                   :temperature 0.5
                   :max-tokens 1500})]
    (if (:success response)
      (get-in response [:data :choices 0 :message :content])
      nil)))

;; -- Streaming Support -------------------------------------------------------

(defn stream-chat-completion
  "Stream chat completion response (returns lazy sequence of chunks)"
  [{:keys [messages model temperature max-tokens]
    :or {temperature 0.7 max-tokens 2048}}
   on-chunk]
  (try
    (http/request
     {:method :post
      :url (api-url "chat/completions")
      :headers {"Content-Type" "application/json"}
      :body (json/generate-string
             {:model (or model (:default-model default-config))
              :messages messages
              :temperature temperature
              :max_tokens max-tokens
              :stream true})
      :as :stream
      :socket-timeout (:timeout default-config)}
     (fn [response]
       (with-open [reader (clojure.java.io/reader (:body response))]
         (doseq [line (line-seq reader)]
           (when (and (not (empty? line))
                      (.startsWith line "data: ")
                      (not= line "data: [DONE]"))
             (let [data (json/parse-string (subs line 6) true)
                   content (get-in data [:choices 0 :delta :content])]
               (when content
                 (on-chunk content))))))))
    (catch Exception e
      (log/error e "Stream request failed"))))
