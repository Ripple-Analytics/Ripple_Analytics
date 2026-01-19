(ns mental-models.pipeline.integration.prompt-manager
  "Prompt manager for LLM interactions in mental model analysis.
   
   Features:
   - Prompt templates
   - Variable substitution
   - Prompt versioning
   - A/B testing prompts
   - Prompt optimization
   - Response parsing
   - Token counting
   - Prompt chaining"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:prompts {}          ;; prompt-id -> prompt
         :versions {}         ;; prompt-id -> [versions]
         :chains {}           ;; chain-id -> chain
         :parsers {}          ;; parser-id -> parser-fn
         :experiments {}      ;; experiment-id -> experiment
         :usage {}            ;; prompt-id -> usage-stats
         :stats {:prompts-created 0 :prompts-executed 0 :tokens-used 0}
         :initialized? false}))

;; ============================================================================
;; Token Estimation
;; ============================================================================

(defn estimate-tokens
  "Estimate token count for text."
  [text]
  ;; Rough estimation: ~4 characters per token for English
  (int (Math/ceil (/ (count text) 4.0))))

(defn estimate-prompt-tokens
  "Estimate tokens for a prompt with variables."
  [prompt-id variables]
  (when-let [prompt (get-in @state [:prompts prompt-id])]
    (let [rendered (render-prompt prompt-id variables)]
      (estimate-tokens rendered))))

;; ============================================================================
;; Prompt Management
;; ============================================================================

(defn create-prompt!
  "Create a prompt template."
  [prompt-id config]
  (let [prompt {:id prompt-id
                :name (get config :name (name prompt-id))
                :description (get config :description "")
                :template (get config :template "")
                :system-message (get config :system-message)
                :variables (get config :variables [])
                :model (get config :model "default")
                :temperature (get config :temperature 0.7)
                :max-tokens (get config :max-tokens 1000)
                :stop-sequences (get config :stop-sequences [])
                :parser-id (get config :parser-id)
                :tags (get config :tags #{})
                :current-version 1
                :created-at (System/currentTimeMillis)
                :updated-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:prompts prompt-id] prompt)
    (swap! state assoc-in [:versions prompt-id]
           [{:version 1
             :template (:template prompt)
             :system-message (:system-message prompt)
             :created-at (System/currentTimeMillis)}])
    (swap! state assoc-in [:usage prompt-id] {:executions 0 :total-tokens 0 :avg-latency-ms 0})
    (swap! state update-in [:stats :prompts-created] inc)
    
    (logging/log :info "Created prompt" {:prompt-id prompt-id})
    prompt-id))

(defn get-prompt
  "Get a prompt."
  [prompt-id]
  (get-in @state [:prompts prompt-id]))

(defn list-prompts
  "List all prompts."
  [& {:keys [tag model]}]
  (let [prompts (vals (:prompts @state))
        filtered (cond->> prompts
                   tag (filter #(contains? (:tags %) tag))
                   model (filter #(= (:model %) model)))]
    (mapv #(select-keys % [:id :name :model :current-version :tags]) filtered)))

(defn update-prompt!
  "Update a prompt (creates new version)."
  [prompt-id updates]
  (when-let [prompt (get-prompt prompt-id)]
    (let [new-version (inc (:current-version prompt))
          version-entry {:version new-version
                         :template (or (:template updates) (:template prompt))
                         :system-message (or (:system-message updates) (:system-message prompt))
                         :created-at (System/currentTimeMillis)}]
      
      (swap! state update-in [:prompts prompt-id]
             (fn [p]
               (merge p updates {:current-version new-version
                                 :updated-at (System/currentTimeMillis)})))
      (swap! state update-in [:versions prompt-id] conj version-entry)
      
      (logging/log :info "Updated prompt" {:prompt-id prompt-id :version new-version})
      new-version)))

(defn delete-prompt!
  "Delete a prompt."
  [prompt-id]
  (swap! state update :prompts dissoc prompt-id)
  (swap! state update :versions dissoc prompt-id)
  (swap! state update :usage dissoc prompt-id)
  (logging/log :info "Deleted prompt" {:prompt-id prompt-id}))

;; ============================================================================
;; Prompt Rendering
;; ============================================================================

(defn render-prompt
  "Render a prompt with variables."
  [prompt-id variables]
  (when-let [prompt (get-prompt prompt-id)]
    (let [template (:template prompt)]
      (reduce (fn [text [var-name var-value]]
                (str/replace text
                             (re-pattern (str "\\{\\{\\s*" (name var-name) "\\s*\\}\\}"))
                             (str var-value)))
              template
              variables))))

(defn render-with-version
  "Render a specific version of a prompt."
  [prompt-id version variables]
  (let [versions (get-in @state [:versions prompt-id] [])
        version-data (first (filter #(= (:version %) version) versions))]
    (when version-data
      (reduce (fn [text [var-name var-value]]
                (str/replace text
                             (re-pattern (str "\\{\\{\\s*" (name var-name) "\\s*\\}\\}"))
                             (str var-value)))
              (:template version-data)
              variables))))

(defn build-messages
  "Build messages array for LLM API."
  [prompt-id variables]
  (when-let [prompt (get-prompt prompt-id)]
    (let [rendered (render-prompt prompt-id variables)]
      (cond-> []
        (:system-message prompt)
        (conj {:role "system" :content (:system-message prompt)})
        
        true
        (conj {:role "user" :content rendered})))))

;; ============================================================================
;; Response Parsing
;; ============================================================================

(defn register-parser!
  "Register a response parser."
  [parser-id parser-fn]
  (swap! state assoc-in [:parsers parser-id] parser-fn)
  (logging/log :info "Registered parser" {:parser-id parser-id})
  parser-id)

(defn get-parser
  "Get a parser."
  [parser-id]
  (get-in @state [:parsers parser-id]))

(defn parse-response
  "Parse an LLM response."
  [prompt-id response]
  (when-let [prompt (get-prompt prompt-id)]
    (if-let [parser-id (:parser-id prompt)]
      (if-let [parser-fn (get-parser parser-id)]
        (try
          (parser-fn response)
          (catch Exception e
            (logging/log :error "Parse error" {:prompt-id prompt-id :error (.getMessage e)})
            {:raw response :parse-error (.getMessage e)}))
        {:raw response})
      {:raw response})))

;; ============================================================================
;; Prompt Chaining
;; ============================================================================

(defn create-chain!
  "Create a prompt chain."
  [chain-id config]
  (let [chain {:id chain-id
               :name (get config :name (name chain-id))
               :description (get config :description "")
               :steps (get config :steps [])
               :error-handling (get config :error-handling :stop) ;; :stop, :skip, :retry
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:chains chain-id] chain)
    (logging/log :info "Created chain" {:chain-id chain-id})
    chain-id))

(defn get-chain
  "Get a chain."
  [chain-id]
  (get-in @state [:chains chain-id]))

(defn list-chains
  "List all chains."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :step-count (count (:steps c))})
        (:chains @state)))

(defn execute-chain
  "Execute a prompt chain."
  [chain-id initial-variables execute-fn]
  (when-let [chain (get-chain chain-id)]
    (loop [steps (:steps chain)
           variables initial-variables
           results []]
      (if (empty? steps)
        {:success true
         :results results
         :final-variables variables}
        (let [step (first steps)
              prompt-id (:prompt-id step)
              output-key (:output-key step)
              
              ;; Execute prompt
              result (try
                       (execute-fn prompt-id variables)
                       (catch Exception e
                         {:error (.getMessage e)}))]
          
          (if (:error result)
            (case (:error-handling chain)
              :stop {:success false
                     :error (:error result)
                     :failed-step prompt-id
                     :results results}
              :skip (recur (rest steps) variables (conj results result))
              :retry (recur steps variables results))
            
            (let [parsed (parse-response prompt-id (:response result))
                  new-variables (if output-key
                                  (assoc variables output-key parsed)
                                  variables)]
              (recur (rest steps)
                     new-variables
                     (conj results {:prompt-id prompt-id
                                    :response (:response result)
                                    :parsed parsed})))))))))

;; ============================================================================
;; Prompt Experiments
;; ============================================================================

(defn create-experiment!
  "Create a prompt A/B test."
  [experiment-id config]
  (let [experiment {:id experiment-id
                    :name (get config :name (name experiment-id))
                    :prompt-id (get config :prompt-id)
                    :variants (get config :variants []) ;; [{:id :a :version 1} {:id :b :version 2}]
                    :traffic-split (get config :traffic-split {}) ;; {:a 50 :b 50}
                    :metrics (get config :metrics [:latency :quality])
                    :status :created
                    :results {}
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:experiments experiment-id] experiment)
    (logging/log :info "Created prompt experiment" {:experiment-id experiment-id})
    experiment-id))

(defn get-experiment
  "Get an experiment."
  [experiment-id]
  (get-in @state [:experiments experiment-id]))

(defn select-variant
  "Select a variant for an experiment."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (when (= :running (:status experiment))
      (let [traffic-split (:traffic-split experiment)
            rand-val (rand-int 100)
            cumulative (reductions + (vals traffic-split))]
        (first (filter (fn [[variant threshold]]
                         (<= rand-val threshold))
                       (map vector (keys traffic-split) cumulative)))))))

(defn record-result!
  "Record experiment result."
  [experiment-id variant-id metrics]
  (swap! state update-in [:experiments experiment-id :results variant-id]
         (fnil conj [])
         (merge metrics {:recorded-at (System/currentTimeMillis)})))

(defn start-experiment!
  "Start an experiment."
  [experiment-id]
  (swap! state assoc-in [:experiments experiment-id :status] :running)
  (swap! state assoc-in [:experiments experiment-id :started-at] (System/currentTimeMillis)))

(defn stop-experiment!
  "Stop an experiment."
  [experiment-id]
  (swap! state assoc-in [:experiments experiment-id :status] :completed)
  (swap! state assoc-in [:experiments experiment-id :completed-at] (System/currentTimeMillis)))

;; ============================================================================
;; Usage Tracking
;; ============================================================================

(defn record-usage!
  "Record prompt usage."
  [prompt-id tokens latency-ms]
  (swap! state update-in [:usage prompt-id]
         (fn [u]
           (let [executions (inc (:executions u 0))
                 total-tokens (+ (:total-tokens u 0) tokens)
                 prev-avg (:avg-latency-ms u 0)
                 new-avg (/ (+ (* prev-avg (dec executions)) latency-ms) executions)]
             {:executions executions
              :total-tokens total-tokens
              :avg-latency-ms new-avg
              :last-used (System/currentTimeMillis)})))
  (swap! state update-in [:stats :prompts-executed] inc)
  (swap! state update-in [:stats :tokens-used] + tokens))

(defn get-usage
  "Get usage statistics for a prompt."
  [prompt-id]
  (get-in @state [:usage prompt-id]))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-prompt-stats
  "Get prompt manager statistics."
  []
  (let [stats (:stats @state)]
    {:total-prompts (count (:prompts @state))
     :total-chains (count (:chains @state))
     :total-parsers (count (:parsers @state))
     :total-experiments (count (:experiments @state))
     :prompts-created (:prompts-created stats)
     :prompts-executed (:prompts-executed stats)
     :tokens-used (:tokens-used stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-prompt-manager!
  "Initialize the prompt manager."
  []
  (when-not (:initialized? @state)
    ;; Register parsers
    (register-parser! :json
                      (fn [response]
                        (try
                          (edn/read-string (str/replace response #"```json|```" ""))
                          (catch Exception _
                            {:raw response}))))
    
    (register-parser! :list
                      (fn [response]
                        (str/split-lines response)))
    
    (register-parser! :mental-models
                      (fn [response]
                        (let [lines (str/split-lines response)]
                          (mapv (fn [line]
                                  (let [[model confidence] (str/split line #":")]
                                    {:model (str/trim (or model ""))
                                     :confidence (try (Double/parseDouble (str/trim (or confidence "0")))
                                                      (catch Exception _ 0.0))}))
                                (filter #(str/includes? % ":") lines)))))
    
    ;; Create mental model analysis prompt
    (create-prompt! :analyze-mental-models
                    {:name "Analyze Mental Models"
                     :description "Analyze text for mental models"
                     :system-message "You are an expert in Charlie Munger's mental models framework. Analyze the given text and identify which mental models are present."
                     :template "Analyze the following text for mental models:

{{text}}

List each mental model detected with a confidence score (0-1):
Format: ModelName: 0.XX"
                     :variables [:text]
                     :model "default"
                     :temperature 0.3
                     :max-tokens 500
                     :parser-id :mental-models
                     :tags #{:analysis :mental-models}})
    
    ;; Create lollapalooza detection prompt
    (create-prompt! :detect-lollapalooza
                    {:name "Detect Lollapalooza Effect"
                     :description "Detect convergence of multiple mental models"
                     :system-message "You are an expert in identifying the Lollapalooza effect - when multiple mental models converge to create a powerful outcome."
                     :template "Given these detected mental models:
{{models}}

Analyze if there is a Lollapalooza effect (3+ models converging with high confidence).

Provide:
1. Is Lollapalooza present? (yes/no)
2. Combined confidence score
3. Explanation of how the models interact"
                     :variables [:models]
                     :model "default"
                     :temperature 0.5
                     :max-tokens 300
                     :tags #{:analysis :lollapalooza}})
    
    ;; Create summary prompt
    (create-prompt! :summarize-analysis
                    {:name "Summarize Analysis"
                     :description "Summarize mental model analysis results"
                     :template "Summarize the following mental model analysis in 2-3 sentences:

Document: {{document_name}}
Models Detected: {{models}}
Lollapalooza: {{lollapalooza}}

Summary:"
                     :variables [:document_name :models :lollapalooza]
                     :model "default"
                     :temperature 0.7
                     :max-tokens 150
                     :tags #{:summary}})
    
    ;; Create analysis chain
    (create-chain! :full-analysis
                   {:name "Full Analysis Chain"
                    :description "Complete mental model analysis pipeline"
                    :steps [{:prompt-id :analyze-mental-models
                             :output-key :detected-models}
                            {:prompt-id :detect-lollapalooza
                             :output-key :lollapalooza-result}
                            {:prompt-id :summarize-analysis
                             :output-key :summary}]
                    :error-handling :stop})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Prompt manager initialized")
    (events/emit! :prompt-manager-initialized {})
    true))
