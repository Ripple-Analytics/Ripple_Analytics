(ns mental-models.llm.lm-studio
  "LM Studio HTTP Client Integration for Mental Model Analysis
   
   Connects to local LM Studio server for semantic analysis of text.
   Uses a single model loaded once, with different prompts for each mental model.
   
   Features:
   - Real HTTP client with clj-http
   - Connection pooling for performance
   - Streaming responses for real-time analysis
   - Retry logic with exponential backoff
   - Circuit breaker for fault tolerance
   
   Configuration:
   - LM_STUDIO_URL: Base URL (default: http://localhost:1234)
   - LM_STUDIO_MODEL: Model name (default: auto-detect)
   
   Electric Clojure reactive components for real-time streaming analysis."
  #?(:clj (:require [clj-http.client :as http]
                    [cheshire.core :as json]
                    [clojure.core.async :as async :refer [go go-loop chan <! >! put! close! timeout]]
                    [taoensso.timbre :as log]
                    [clojure.java.io :as io]
                    [clojure.string :as str])
     :cljs (:require [cljs-http.client :as http]
                     [cljs.core.async :as async :refer [go chan <! >! put! close!]]
                     [clojure.string :as str])))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def ^:dynamic *lm-studio-url* 
  (or #?(:clj (System/getenv "LM_STUDIO_URL")) "http://localhost:1234"))

(def ^:dynamic *default-model* 
  (or #?(:clj (System/getenv "LM_STUDIO_MODEL")) "local-model"))

(def ^:dynamic *timeout-ms* 60000)
(def ^:dynamic *max-tokens* 2048)
(def ^:dynamic *temperature* 0.1)

;; =============================================================================
;; CONNECTION POOLING
;; =============================================================================

#?(:clj
   (def connection-pool-config
     "Configuration for HTTP connection pooling."
     (atom {:max-connections 20
            :max-per-route 10
            :connection-timeout-ms 5000
            :socket-timeout-ms 60000
            :validate-after-inactivity-ms 2000})))

#?(:clj
   (defonce connection-manager
     "Pooled connection manager for HTTP clients."
     (atom nil)))

#?(:clj
   (defn init-connection-pool!
     "Initialize the HTTP connection pool."
     []
     (when-not @connection-manager
       (let [config @connection-pool-config
             cm (org.apache.http.impl.conn.PoolingHttpClientConnectionManager.)]
         (.setMaxTotal cm (:max-connections config))
         (.setDefaultMaxPerRoute cm (:max-per-route config))
         (.setValidateAfterInactivity cm (:validate-after-inactivity-ms config))
         (reset! connection-manager cm)
         (log/info "Connection pool initialized with" (:max-connections config) "max connections")
         cm))))

#?(:clj
   (defn get-pool-stats
     "Get connection pool statistics."
     []
     (when-let [cm @connection-manager]
       (let [stats (.getTotalStats cm)]
         {:available (.getAvailable stats)
          :leased (.getLeased stats)
          :pending (.getPending stats)
          :max (.getMax stats)}))))

;; =============================================================================
;; RETRY LOGIC WITH EXPONENTIAL BACKOFF
;; =============================================================================

(def retry-config
  "Configuration for retry logic."
  (atom {:max-retries 3
         :initial-delay-ms 1000
         :max-delay-ms 30000
         :backoff-multiplier 2.0
         :retryable-status-codes #{429 500 502 503 504}}))

#?(:clj
   (defn calculate-backoff
     "Calculate exponential backoff delay."
     [attempt]
     (let [config @retry-config
           delay (* (:initial-delay-ms config)
                    (Math/pow (:backoff-multiplier config) attempt))]
       (min delay (:max-delay-ms config)))))

#?(:clj
   (defn retryable-error?
     "Check if an error is retryable."
     [error]
     (or (instance? java.net.SocketTimeoutException error)
         (instance? java.net.ConnectException error)
         (and (instance? clojure.lang.ExceptionInfo error)
              (contains? (:retryable-status-codes @retry-config)
                         (:status (ex-data error)))))))

#?(:clj
   (defn with-retry
     "Execute a function with retry logic and exponential backoff."
     [f & {:keys [max-retries on-retry]
           :or {max-retries (:max-retries @retry-config)
                on-retry (fn [attempt delay error] 
                           (log/warn "Retry attempt" attempt "after" delay "ms due to:" (.getMessage error)))}}]
     (loop [attempt 0]
       (let [result (try
                      {:success true :value (f)}
                      (catch Exception e
                        {:success false :error e}))]
         (if (:success result)
           (:value result)
           (if (and (< attempt max-retries)
                    (retryable-error? (:error result)))
             (let [delay (calculate-backoff attempt)]
               (on-retry (inc attempt) delay (:error result))
               (Thread/sleep (long delay))
               (recur (inc attempt)))
             (throw (:error result))))))))

;; =============================================================================
;; CIRCUIT BREAKER
;; =============================================================================

(def circuit-breaker-state
  "Circuit breaker state for LM Studio connection."
  (atom {:state :closed
         :failure-count 0
         :success-count 0
         :last-failure-time nil
         :failure-threshold 5
         :success-threshold 3
         :timeout-ms 30000}))

#?(:clj
   (defn circuit-open?
     "Check if circuit breaker is open."
     []
     (let [state @circuit-breaker-state]
       (and (= :open (:state state))
            (< (- (System/currentTimeMillis) (or (:last-failure-time state) 0))
               (:timeout-ms state))))))

#?(:clj
   (defn record-success!
     "Record a successful call."
     []
     (swap! circuit-breaker-state
            (fn [state]
              (if (= :half-open (:state state))
                (if (>= (inc (:success-count state)) (:success-threshold state))
                  (assoc state :state :closed :failure-count 0 :success-count 0)
                  (update state :success-count inc))
                (assoc state :failure-count 0))))))

#?(:clj
   (defn record-failure!
     "Record a failed call."
     []
     (swap! circuit-breaker-state
            (fn [state]
              (let [new-count (inc (:failure-count state))]
                (if (>= new-count (:failure-threshold state))
                  (assoc state
                         :state :open
                         :failure-count new-count
                         :last-failure-time (System/currentTimeMillis))
                  (assoc state :failure-count new-count)))))))

#?(:clj
   (defn with-circuit-breaker
     "Execute function with circuit breaker protection."
     [f]
     (cond
       (circuit-open?)
       (throw (ex-info "Circuit breaker is open" {:type :circuit-open}))
       
       (= :open (:state @circuit-breaker-state))
       (do
         (swap! circuit-breaker-state assoc :state :half-open :success-count 0)
         (try
           (let [result (f)]
             (record-success!)
             result)
           (catch Exception e
             (record-failure!)
             (throw e))))
       
       :else
       (try
         (let [result (f)]
           (record-success!)
           result)
         (catch Exception e
           (record-failure!)
           (throw e))))))

;; =============================================================================
;; HTTP CLIENT WITH POOLING
;; =============================================================================

#?(:clj
   (defn- make-request
     "Make HTTP request to LM Studio API with connection pooling and retry."
     [endpoint method body]
     (init-connection-pool!)
     (with-circuit-breaker
       (fn []
         (with-retry
           (fn []
             (let [url (str *lm-studio-url* endpoint)
                   opts {:content-type :json
                         :accept :json
                         :socket-timeout *timeout-ms*
                         :connection-timeout 5000
                         :connection-manager @connection-manager
                         :body (when body (json/generate-string body))}
                   response (case method
                              :get (http/get url opts)
                              :post (http/post url opts))]
               {:success true
                :data (json/parse-string (:body response) true)})))))))

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
;; STREAMING RESPONSES
;; =============================================================================

#?(:clj
   (defn stream-chat-completion
     "Stream chat completion responses from LM Studio.
      Returns a channel that emits tokens as they arrive."
     [messages & [{:keys [model temperature max_tokens]
                   :or {model *default-model*
                        temperature *temperature*
                        max_tokens *max-tokens*}}]]
     (let [out-chan (chan 1000)
           url (str *lm-studio-url* "/v1/chat/completions")
           body {:model model
                 :messages messages
                 :temperature temperature
                 :max_tokens max_tokens
                 :stream true}]
       (go
         (try
           (init-connection-pool!)
           (let [response (http/post url
                                     {:content-type :json
                                      :accept :json
                                      :as :stream
                                      :socket-timeout *timeout-ms*
                                      :connection-timeout 5000
                                      :connection-manager @connection-manager
                                      :body (json/generate-string body)})]
             (with-open [reader (java.io.BufferedReader. 
                                 (java.io.InputStreamReader. (:body response) "UTF-8"))]
               (loop []
                 (when-let [line (.readLine reader)]
                   (when (str/starts-with? line "data: ")
                     (let [data (subs line 6)]
                       (when (not= data "[DONE]")
                         (try
                           (let [parsed (json/parse-string data true)
                                 content (get-in parsed [:choices 0 :delta :content])]
                             (when content
                               (>! out-chan {:type :token :content content})))
                           (catch Exception e
                             (log/debug "Parse error:" (.getMessage e)))))))
                   (recur)))))
           (>! out-chan {:type :done})
           (catch Exception e
             (log/error "Streaming error:" (.getMessage e))
             (>! out-chan {:type :error :message (.getMessage e)}))
           (finally
             (close! out-chan))))
       out-chan)))

#?(:clj
   (defn stream-to-string
     "Consume a streaming channel and return the full response as a string."
     [stream-chan]
     (let [result (StringBuilder.)]
       (loop []
         (when-let [msg (async/<!! stream-chan)]
           (when (= :token (:type msg))
             (.append result (:content msg)))
           (when (not= :done (:type msg))
             (recur))))
       (.toString result))))

#?(:clj
   (defn stream-with-callback
     "Stream chat completion with a callback for each token."
     [messages callback & [opts]]
     (let [stream-chan (stream-chat-completion messages opts)]
       (go-loop []
         (when-let [msg (<! stream-chan)]
           (callback msg)
           (when (not= :done (:type msg))
             (recur)))))))

;; =============================================================================
;; MODEL MANAGEMENT
;; =============================================================================

(defn list-models
  "List available models in LM Studio"
  []
  #?(:clj (try
            (make-request "/v1/models" :get nil)
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs (go (<! (make-request "/v1/models" :get nil)))))

(defn get-model-info
  "Get information about the currently loaded model"
  []
  #?(:clj (try
            (make-request "/v1/models" :get nil)
            (catch Exception e
              {:success false :error (.getMessage e)}))
     :cljs (go (<! (make-request "/v1/models" :get nil)))))

(defn health-check
  "Check if LM Studio is running and responsive"
  []
  (let [result #?(:clj (try
                         (make-request "/v1/models" :get nil)
                         (catch Exception e
                           {:success false :error (.getMessage e)}))
                  :cljs (go (<! (make-request "/v1/models" :get nil))))]
    #?(:clj {:healthy (:success result)
             :url *lm-studio-url*
             :pool-stats (get-pool-stats)
             :circuit-breaker @circuit-breaker-state
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
  "Send chat completion request to LM Studio"
  [messages & [{:keys [model temperature max_tokens stream]
                :or {model *default-model*
                     temperature *temperature*
                     max_tokens *max-tokens*
                     stream false}}]]
  (if stream
    #?(:clj (stream-chat-completion messages {:model model
                                               :temperature temperature
                                               :max_tokens max_tokens})
       :cljs (go {:error "Streaming not supported in ClojureScript"}))
    (let [body {:model model
                :messages messages
                :temperature temperature
                :max_tokens max_tokens
                :stream false}]
      #?(:clj (try
                (make-request "/v1/chat/completions" :post body)
                (catch Exception e
                  {:success false :error (.getMessage e)}))
         :cljs (go (<! (make-request "/v1/chat/completions" :post body)))))))

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
  "Analyze text for a specific mental model"
  [text model-name model-description]
  (let [prompt (str "Analyze the following text for evidence of the mental model: " model-name "

"
                    "Model Description: " model-description "

"
                    "Text to analyze:
" text "

"
                    "Respond with JSON: {"detected": boolean, "confidence": 0.0-1.0, "evidence": ["quotes"], "explanation": "string"}")
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
  (let [prompt (str "Analyze the following text and identify ALL cognitive biases, mental models, and thinking patterns present.

"
                    "Text to analyze:
" text "

"
                    "For each identified pattern, provide:
"
                    "- name: The bias or model name
"
                    "- confidence: 0.0-1.0
"
                    "- evidence: Exact quotes from the text
"
                    "- severity: low/medium/high

"
                    "Respond with JSON array: [{"name": "", "confidence": 0.0, "evidence": [""], "severity": ""}]")
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
     "Stream analysis results as they're generated"
     [text]
     (let [out-chan (chan 100)]
       (go
         (try
           (>! out-chan {:type :progress :stage "keyword_analysis" :progress 10})
           (>! out-chan {:type :progress :stage "llm_analysis" :progress 30})
           
           (let [prompt (str "Analyze this text for cognitive biases and mental models:

" text)
                 stream-chan (stream-chat-completion 
                              [{:role "system" :content mental-model-system-prompt}
                               {:role "user" :content prompt}])]
             (loop [full-response ""]
               (when-let [msg (<! stream-chan)]
                 (case (:type msg)
                   :token (do
                            (>! out-chan {:type :token :content (:content msg)})
                            (recur (str full-response (:content msg))))
                   :done (do
                           (>! out-chan {:type :progress :stage "parsing" :progress 80})
                           (try
                             (let [biases (json/parse-string full-response true)]
                               (>! out-chan {:type :biases :data biases :progress 90}))
                             (catch Exception e
                               (>! out-chan {:type :biases :data {:raw full-response} :progress 90}))))
                   :error (>! out-chan {:type :error :message (:message msg)})
                   (recur full-response)))))
           
           (>! out-chan {:type :complete :progress 100})
           
           (catch Exception e
             (>! out-chan {:type :error :message (.getMessage e)}))
           (finally
             (close! out-chan))))
       out-chan)))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn detect-lollapalooza
  "Analyze text for Lollapalooza effect - multiple biases converging"
  [text]
  (let [prompt (str "Analyze this text for a LOLLAPALOOZA EFFECT - where multiple cognitive biases or mental models converge to create an extreme outcome.

"
                    "Text to analyze:
" text "

"
                    "Identify:
"
                    "1. All cognitive biases present (with confidence scores)
"
                    "2. How they interact and reinforce each other
"
                    "3. The combined effect (amplification factor)
"
                    "4. Risk level: low/medium/high/critical

"
                    "Respond with JSON:
"
                    "{
"
                    "  "is_lollapalooza": boolean,
"
                    "  "biases": [{"name": "", "confidence": 0.0}],
"
                    "  "interactions": ["bias1 + bias2 = effect"],
"
                    "  "amplification_factor": 1.0-10.0,
"
                    "  "risk_level": "string",
"
                    "  "explanation": "string"
"
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
  (let [prompt (str "Apply INVERSION to analyze this situation. Instead of asking 'how can this succeed?', ask 'how can this fail?'

"
                    "Context: " context "

"
                    "Text to analyze:
" text "

"
                    "Identify:
"
                    "1. All ways this could fail
"
                    "2. Hidden assumptions that might be wrong
"
                    "3. Risks that are being ignored
"
                    "4. What the author/speaker is NOT saying
"
                    "5. Worst-case scenarios

"
                    "Respond with JSON:
"
                    "{
"
                    "  "failure_modes": ["string"],
"
                    "  "hidden_assumptions": ["string"],
"
                    "  "ignored_risks": ["string"],
"
                    "  "omissions": ["string"],
"
                    "  "worst_case": "string",
"
                    "  "recommendation": "string"
"
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
;; BATCH ANALYSIS WITH PARALLEL PROCESSING
;; =============================================================================

#?(:clj
   (defn batch-analyze-parallel
     "Analyze multiple texts in parallel using core.async"
     [texts-with-ids & {:keys [concurrency]
                        :or {concurrency 4}}]
     (let [out-chan (chan 100)
           work-chan (chan)
           done-count (atom 0)
           total (count texts-with-ids)]
       (go
         (doseq [item texts-with-ids]
           (>! work-chan item))
         (close! work-chan))
       
       (dotimes [_ concurrency]
         (go-loop []
           (when-let [{:keys [id text]} (<! work-chan)]
             (try
               (let [analysis (analyze-for-all-biases text)
                     lollapalooza (detect-lollapalooza text)]
                 (>! out-chan {:id id
                               :biases analysis
                               :lollapalooza lollapalooza
                               :analyzed_at (System/currentTimeMillis)}))
               (catch Exception e
                 (>! out-chan {:id id
                               :error (.getMessage e)
                               :analyzed_at (System/currentTimeMillis)})))
             (swap! done-count inc)
             (when (= @done-count total)
               (close! out-chan))
             (recur))))
       
       out-chan)))

(defn batch-analyze
  "Analyze multiple texts in batch"
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
;; METRICS AND MONITORING
;; =============================================================================

(def request-metrics
  "Metrics for LM Studio requests."
  (atom {:total-requests 0
         :successful-requests 0
         :failed-requests 0
         :total-tokens 0
         :avg-latency-ms 0
         :last-request-time nil}))

#?(:clj
   (defn record-request-metrics!
     "Record metrics for a request."
     [success? latency-ms tokens]
     (swap! request-metrics
            (fn [m]
              (let [total (inc (:total-requests m))
                    avg-latency (/ (+ (* (:avg-latency-ms m) (:total-requests m)) latency-ms)
                                   total)]
                (-> m
                    (update :total-requests inc)
                    (update (if success? :successful-requests :failed-requests) inc)
                    (update :total-tokens + (or tokens 0))
                    (assoc :avg-latency-ms avg-latency)
                    (assoc :last-request-time (System/currentTimeMillis))))))))

(defn get-metrics
  "Get current request metrics."
  []
  @request-metrics)

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init!
  "Initialize LM Studio connection"
  [& [{:keys [url model timeout pool-size]}]]
  (when url (alter-var-root #'*lm-studio-url* (constantly url)))
  (when model (alter-var-root #'*default-model* (constantly model)))
  (when timeout (alter-var-root #'*timeout-ms* (constantly timeout)))
  
  #?(:clj
     (do
       (when pool-size
         (swap! connection-pool-config assoc :max-connections pool-size))
       
       (init-connection-pool!)
       
       (reset! circuit-breaker-state
               {:state :closed
                :failure-count 0
                :success-count 0
                :last-failure-time nil
                :failure-threshold 5
                :success-threshold 3
                :timeout-ms 30000})
       
       (reset! request-metrics
               {:total-requests 0
                :successful-requests 0
                :failed-requests 0
                :total-tokens 0
                :avg-latency-ms 0
                :last-request-time nil})))
  
  (let [health (health-check)]
    #?(:clj (log/info "LM Studio initialized:" health)
       :cljs (js/console.log "LM Studio initialized:" (clj->js health)))
    health))

;; =============================================================================
;; SHUTDOWN
;; =============================================================================

#?(:clj
   (defn shutdown!
     "Shutdown LM Studio client and release resources."
     []
     (when-let [cm @connection-manager]
       (.close cm)
       (reset! connection-manager nil)
       (log/info "LM Studio connection pool closed"))))
