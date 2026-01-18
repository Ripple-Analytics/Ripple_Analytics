(ns mental-models.llm.lm-studio
  "LM Studio HTTP Client Integration for Mental Model Analysis
   
   Connects to local LM Studio server for semantic analysis of text.
   Uses a single model loaded once, with different prompts for each mental model.
   
   Configuration:
   - LM_STUDIO_URL: Base URL (default: http://localhost:1234)
   - LM_STUDIO_MODEL: Model name (default: auto-detect)
   
   Electric Clojure reactive components for real-time streaming analysis."
  #?(:clj (:require [clj-http.client :as http]
                    [cheshire.core :as json]
                    [clojure.core.async :as async :refer [go chan <! >! put! close!]]
                    [taoensso.timbre :as log])
     :cljs (:require [cljs-http.client :as http]
                     [cljs.core.async :as async :refer [go chan <! >! put! close!]])))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def ^:dynamic *lm-studio-url* 
  (or #?(:clj (System/getenv "LM_STUDIO_URL")) "http://localhost:1234"))

(def ^:dynamic *default-model* 
  (or #?(:clj (System/getenv "LM_STUDIO_MODEL")) "local-model"))

(def ^:dynamic *timeout-ms* 60000)
(def ^:dynamic *max-tokens* 2048)
(def ^:dynamic *temperature* 0.1)  ; Low temperature for consistent analysis

;; =============================================================================
;; HTTP CLIENT
;; =============================================================================

#?(:clj
   (defn- make-request
     "Make HTTP request to LM Studio API"
     [endpoint method body]
     (try
       (let [url (str *lm-studio-url* endpoint)
             opts {:content-type :json
                   :accept :json
                   :socket-timeout *timeout-ms*
                   :connection-timeout 5000
                   :body (when body (json/generate-string body))}
             response (case method
                        :get (http/get url opts)
                        :post (http/post url opts))]
         {:success true
          :data (json/parse-string (:body response) true)})
       (catch Exception e
         (log/error "LM Studio request failed:" (.getMessage e))
         {:success false
          :error (.getMessage e)}))))

#?(:cljs
   (defn- make-request
     "Make HTTP request to LM Studio API (ClojureScript)"
     [endpoint method body]
     (go
       (try
         (let [url (str *lm-studio-url* endpoint)
               response (<! (case method
                              :get (http/get url)
                              :post (http/post url {:json-params body})))]
           (if (:success response)
             {:success true :data (:body response)}
             {:success false :error (:error-text response)}))
         (catch :default e
           {:success false :error (str e)})))))

;; =============================================================================
;; MODEL MANAGEMENT
;; =============================================================================

(defn list-models
  "List available models in LM Studio"
  []
  #?(:clj (make-request "/v1/models" :get nil)
     :cljs (go (<! (make-request "/v1/models" :get nil)))))

(defn get-model-info
  "Get information about the currently loaded model"
  []
  #?(:clj (make-request "/v1/models" :get nil)
     :cljs (go (<! (make-request "/v1/models" :get nil)))))

(defn health-check
  "Check if LM Studio is running and responsive"
  []
  (let [result #?(:clj (make-request "/v1/models" :get nil)
                  :cljs (go (<! (make-request "/v1/models" :get nil))))]
    #?(:clj {:healthy (:success result)
             :url *lm-studio-url*
             :models (when (:success result) 
                       (get-in result [:data :data]))}
       :cljs (go (let [r (<! result)]
                   {:healthy (:success r)
                    :url *lm-studio-url*
                    :models (when (:success r) 
                              (get-in r [:data :data]))})))))

;; =============================================================================
;; CHAT COMPLETION
;; =============================================================================

(defn chat-completion
  "Send chat completion request to LM Studio
   
   Parameters:
   - messages: Vector of {:role :content} maps
   - opts: Optional settings {:model :temperature :max_tokens :stream}"
  [messages & [{:keys [model temperature max_tokens stream]
                :or {model *default-model*
                     temperature *temperature*
                     max_tokens *max-tokens*
                     stream false}}]]
  (let [body {:model model
              :messages messages
              :temperature temperature
              :max_tokens max_tokens
              :stream stream}]
    #?(:clj (make-request "/v1/chat/completions" :post body)
       :cljs (go (<! (make-request "/v1/chat/completions" :post body))))))

(defn simple-completion
  "Simple text completion with system and user message"
  [system-prompt user-prompt & [opts]]
  (chat-completion [{:role "system" :content system-prompt}
                    {:role "user" :content user-prompt}]
                   opts))

;; =============================================================================
;; MENTAL MODEL ANALYSIS PROMPTS
;; =============================================================================

(def mental-model-system-prompt
  "You are an expert analyst trained in Charlie Munger's mental models and cognitive biases.
Your task is to analyze text and identify which mental models, cognitive biases, or thinking patterns are present.

For each identified pattern, provide:
1. The name of the mental model or bias
2. A confidence score from 0.0 to 1.0
3. Specific evidence from the text (exact quotes)
4. The potential impact (positive, negative, or neutral)

Be precise and cite specific text. Do not speculate beyond what the text shows.
Format your response as JSON.")

(defn analyze-for-model
  "Analyze text for a specific mental model
   
   Returns:
   {:detected true/false
    :confidence 0.0-1.0
    :evidence [\"quote1\" \"quote2\"]
    :explanation \"why this model applies\"}"
  [text model-name model-description]
  (let [prompt (str "Analyze the following text for evidence of the mental model: " model-name "\n\n"
                    "Model Description: " model-description "\n\n"
                    "Text to analyze:\n" text "\n\n"
                    "Respond with JSON: {\"detected\": boolean, \"confidence\": 0.0-1.0, \"evidence\": [\"quotes\"], \"explanation\": \"string\"}")
        result #?(:clj (simple-completion mental-model-system-prompt prompt)
                  :cljs (go (<! (simple-completion mental-model-system-prompt prompt))))]
    #?(:clj (when (:success result)
              (try
                (-> result :data :choices first :message :content
                    (json/parse-string true))
                (catch Exception e
                  {:detected false :confidence 0 :error (.getMessage e)})))
       :cljs (go (let [r (<! result)]
                   (when (:success r)
                     (try
                       (-> r :data :choices first :message :content
                           (js/JSON.parse)
                           (js->clj :keywordize-keys true))
                       (catch :default e
                         {:detected false :confidence 0 :error (str e)}))))))))

(defn analyze-for-all-biases
  "Analyze text for all cognitive biases at once (more efficient)"
  [text]
  (let [prompt (str "Analyze the following text and identify ALL cognitive biases, mental models, and thinking patterns present.\n\n"
                    "Text to analyze:\n" text "\n\n"
                    "For each identified pattern, provide:\n"
                    "- name: The bias or model name\n"
                    "- confidence: 0.0-1.0\n"
                    "- evidence: Exact quotes from the text\n"
                    "- severity: low/medium/high\n\n"
                    "Respond with JSON array: [{\"name\": \"\", \"confidence\": 0.0, \"evidence\": [\"\"], \"severity\": \"\"}]")
        result #?(:clj (simple-completion mental-model-system-prompt prompt)
                  :cljs (go (<! (simple-completion mental-model-system-prompt prompt))))]
    #?(:clj (when (:success result)
              (try
                (-> result :data :choices first :message :content
                    (json/parse-string true))
                (catch Exception e
                  {:error (.getMessage e)})))
       :cljs (go (let [r (<! result)]
                   (when (:success r)
                     (try
                       (-> r :data :choices first :message :content
                           (js/JSON.parse)
                           (js->clj :keywordize-keys true))
                       (catch :default e
                         {:error (str e)}))))))))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn detect-lollapalooza
  "Analyze text for Lollapalooza effect - multiple biases converging
   
   A Lollapalooza occurs when 3+ biases/models converge with high confidence (>0.7)"
  [text]
  (let [prompt (str "Analyze this text for a LOLLAPALOOZA EFFECT - where multiple cognitive biases or mental models converge to create an extreme outcome.\n\n"
                    "Text to analyze:\n" text "\n\n"
                    "Identify:\n"
                    "1. All cognitive biases present (with confidence scores)\n"
                    "2. How they interact and reinforce each other\n"
                    "3. The combined effect (amplification factor)\n"
                    "4. Risk level: low/medium/high/critical\n\n"
                    "Respond with JSON:\n"
                    "{\n"
                    "  \"is_lollapalooza\": boolean,\n"
                    "  \"biases\": [{\"name\": \"\", \"confidence\": 0.0}],\n"
                    "  \"interactions\": [\"bias1 + bias2 = effect\"],\n"
                    "  \"amplification_factor\": 1.0-10.0,\n"
                    "  \"risk_level\": \"string\",\n"
                    "  \"explanation\": \"string\"\n"
                    "}")
        result #?(:clj (simple-completion mental-model-system-prompt prompt)
                  :cljs (go (<! (simple-completion mental-model-system-prompt prompt))))]
    #?(:clj (when (:success result)
              (try
                (-> result :data :choices first :message :content
                    (json/parse-string true))
                (catch Exception e
                  {:is_lollapalooza false :error (.getMessage e)})))
       :cljs (go (let [r (<! result)]
                   (when (:success r)
                     (try
                       (-> r :data :choices first :message :content
                           (js/JSON.parse)
                           (js->clj :keywordize-keys true))
                       (catch :default e
                         {:is_lollapalooza false :error (str e)}))))))))

;; =============================================================================
;; INVERSION ANALYSIS
;; =============================================================================

(defn invert-analysis
  "Apply Munger's inversion principle - analyze what could go wrong"
  [text context]
  (let [prompt (str "Apply INVERSION to analyze this situation. Instead of asking 'how can this succeed?', ask 'how can this fail?'\n\n"
                    "Context: " context "\n\n"
                    "Text to analyze:\n" text "\n\n"
                    "Identify:\n"
                    "1. All ways this could fail\n"
                    "2. Hidden assumptions that might be wrong\n"
                    "3. Risks that are being ignored\n"
                    "4. What the author/speaker is NOT saying\n"
                    "5. Worst-case scenarios\n\n"
                    "Respond with JSON:\n"
                    "{\n"
                    "  \"failure_modes\": [\"string\"],\n"
                    "  \"hidden_assumptions\": [\"string\"],\n"
                    "  \"ignored_risks\": [\"string\"],\n"
                    "  \"omissions\": [\"string\"],\n"
                    "  \"worst_case\": \"string\",\n"
                    "  \"recommendation\": \"string\"\n"
                    "}")
        result #?(:clj (simple-completion mental-model-system-prompt prompt)
                  :cljs (go (<! (simple-completion mental-model-system-prompt prompt))))]
    #?(:clj (when (:success result)
              (try
                (-> result :data :choices first :message :content
                    (json/parse-string true))
                (catch Exception e
                  {:error (.getMessage e)})))
       :cljs (go (let [r (<! result)]
                   (when (:success r)
                     (try
                       (-> r :data :choices first :message :content
                           (js/JSON.parse)
                           (js->clj :keywordize-keys true))
                       (catch :default e
                         {:error (str e)}))))))))

;; =============================================================================
;; STREAMING ANALYSIS
;; =============================================================================

#?(:clj
   (defn stream-analysis
     "Stream analysis results as they're generated
      
      Returns a channel that emits partial results"
     [text]
     (let [out-chan (chan 100)]
       (go
         (try
           ;; First pass: Quick keyword analysis
           (>! out-chan {:type :progress :stage "keyword_analysis" :progress 10})
           
           ;; Second pass: LLM analysis
           (>! out-chan {:type :progress :stage "llm_analysis" :progress 30})
           (let [biases (analyze-for-all-biases text)]
             (>! out-chan {:type :biases :data biases :progress 60}))
           
           ;; Third pass: Lollapalooza detection
           (>! out-chan {:type :progress :stage "lollapalooza_detection" :progress 80})
           (let [lollapalooza (detect-lollapalooza text)]
             (>! out-chan {:type :lollapalooza :data lollapalooza :progress 90}))
           
           ;; Complete
           (>! out-chan {:type :complete :progress 100})
           
           (catch Exception e
             (>! out-chan {:type :error :message (.getMessage e)}))
           (finally
             (close! out-chan))))
       out-chan)))

;; =============================================================================
;; BATCH ANALYSIS
;; =============================================================================

(defn batch-analyze
  "Analyze multiple texts in batch
   
   Returns a map of {text-id -> analysis-result}"
  [texts-with-ids]
  #?(:clj
     (let [results (atom {})]
       (doseq [{:keys [id text]} texts-with-ids]
         (let [analysis (analyze-for-all-biases text)
               lollapalooza (detect-lollapalooza text)]
           (swap! results assoc id {:biases analysis
                                    :lollapalooza lollapalooza
                                    :analyzed_at (System/currentTimeMillis)})))
       @results)
     :cljs
     (go
       (let [results (atom {})]
         (doseq [{:keys [id text]} texts-with-ids]
           (let [analysis (<! (analyze-for-all-biases text))
                 lollapalooza (<! (detect-lollapalooza text))]
             (swap! results assoc id {:biases analysis
                                      :lollapalooza lollapalooza
                                      :analyzed_at (.now js/Date)})))
         @results))))

;; =============================================================================
;; ELECTRIC CLOJURE REACTIVE COMPONENTS
;; =============================================================================

#?(:clj
   (defmacro e-analyze
     "Electric Clojure reactive analysis component
      
      Usage in Electric:
      (e/defn AnalysisView [text]
        (let [result (e-analyze text)]
          (e/client
            (dom/div
              (dom/text (str \"Biases found: \" (count (:biases result))))))))"
     [text]
     `(let [result# (analyze-for-all-biases ~text)]
        {:biases result#
         :timestamp (System/currentTimeMillis)})))

;; =============================================================================
;; CONVENIENCE FUNCTIONS
;; =============================================================================

(defn quick-analyze
  "Quick analysis returning just the top biases"
  [text & [{:keys [top-n threshold]
            :or {top-n 5 threshold 0.5}}]]
  (let [result #?(:clj (analyze-for-all-biases text)
                  :cljs (go (<! (analyze-for-all-biases text))))]
    #?(:clj (->> result
                 (filter #(>= (:confidence %) threshold))
                 (sort-by :confidence >)
                 (take top-n))
       :cljs (go (->> (<! result)
                      (filter #(>= (:confidence %) threshold))
                      (sort-by :confidence >)
                      (take top-n))))))

(defn is-lollapalooza?
  "Quick check if text contains a Lollapalooza effect"
  [text]
  (let [result #?(:clj (detect-lollapalooza text)
                  :cljs (go (<! (detect-lollapalooza text))))]
    #?(:clj (:is_lollapalooza result)
       :cljs (go (:is_lollapalooza (<! result))))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init!
  "Initialize LM Studio connection
   
   Options:
   - :url - LM Studio server URL
   - :model - Model to use
   - :timeout - Request timeout in ms"
  [& [{:keys [url model timeout]}]]
  (when url (alter-var-root #'*lm-studio-url* (constantly url)))
  (when model (alter-var-root #'*default-model* (constantly model)))
  (when timeout (alter-var-root #'*timeout-ms* (constantly timeout)))
  
  ;; Health check
  (let [health (health-check)]
    #?(:clj (log/info "LM Studio initialized:" health)
       :cljs (js/console.log "LM Studio initialized:" (clj->js health)))
    health))
