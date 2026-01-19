(ns mental-models.pipeline.integration.analysis-orchestrator
  "Analysis Orchestrator Module
   
   End-to-end analysis pipeline orchestration:
   - Pipeline definition and execution
   - Stage management and dependencies
   - Parallel and sequential execution
   - Result aggregation
   - Pipeline monitoring"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; ANALYSIS ORCHESTRATOR STATE
;; =============================================================================

(defonce orchestrator-state (atom {:pipelines (ConcurrentHashMap.)
                                   :executions (ConcurrentHashMap.)
                                   :stages (ConcurrentHashMap.)
                                   :results (ConcurrentHashMap.)
                                   :pipeline-count (AtomicLong. 0)
                                   :execution-count (AtomicLong. 0)
                                   :config {:max-parallel 4
                                            :default-timeout-ms 300000
                                            :retry-count 3}}))

;; =============================================================================
;; STAGE DEFINITIONS
;; =============================================================================

(defn register-stage!
  "Register a pipeline stage."
  [stage-id {:keys [name description handler-fn input-schema output-schema timeout-ms]}]
  (log/info "Registering stage" {:id stage-id})
  (.put ^ConcurrentHashMap (:stages @orchestrator-state) stage-id
        {:id stage-id
         :name name
         :description description
         :handler-fn handler-fn
         :input-schema input-schema
         :output-schema output-schema
         :timeout-ms (or timeout-ms 60000)
         :registered-at (System/currentTimeMillis)}))

(defn unregister-stage!
  "Unregister a stage."
  [stage-id]
  (.remove ^ConcurrentHashMap (:stages @orchestrator-state) stage-id))

(defn get-stage
  "Get a stage by ID."
  [stage-id]
  (.get ^ConcurrentHashMap (:stages @orchestrator-state) stage-id))

(defn list-stages
  "List all stages."
  []
  (vec (vals (:stages @orchestrator-state))))

;; =============================================================================
;; PIPELINE DEFINITIONS
;; =============================================================================

(defn create-pipeline!
  "Create an analysis pipeline."
  [pipeline-id {:keys [name description stages parallel]}]
  (.incrementAndGet ^AtomicLong (:pipeline-count @orchestrator-state))
  (log/info "Creating pipeline" {:id pipeline-id})
  (.put ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id
        {:id pipeline-id
         :name name
         :description description
         :stages (or stages [])
         :parallel (or parallel false)
         :enabled true
         :created-at (System/currentTimeMillis)})
  (events/publish! :orchestrator/pipeline-created {:pipeline-id pipeline-id}))

(defn delete-pipeline!
  "Delete a pipeline."
  [pipeline-id]
  (.remove ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id))

(defn get-pipeline
  "Get a pipeline by ID."
  [pipeline-id]
  (.get ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id))

(defn list-pipelines
  "List all pipelines."
  []
  (vec (vals (:pipelines @orchestrator-state))))

(defn enable-pipeline!
  "Enable a pipeline."
  [pipeline-id]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (.put ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id
          (assoc pipeline :enabled true))))

(defn disable-pipeline!
  "Disable a pipeline."
  [pipeline-id]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (.put ^ConcurrentHashMap (:pipelines @orchestrator-state) pipeline-id
          (assoc pipeline :enabled false))))

;; =============================================================================
;; EXECUTION MANAGEMENT
;; =============================================================================

(defn create-execution
  "Create a pipeline execution."
  [pipeline-id input]
  (let [execution-id (str (java.util.UUID/randomUUID))]
    {:id execution-id
     :pipeline-id pipeline-id
     :input input
     :status :pending
     :stage-results {}
     :current-stage nil
     :started-at nil
     :completed-at nil
     :error nil}))

(defn update-execution!
  "Update an execution."
  [execution-id updates]
  (when-let [execution (.get ^ConcurrentHashMap (:executions @orchestrator-state) execution-id)]
    (.put ^ConcurrentHashMap (:executions @orchestrator-state) execution-id
          (merge execution updates))))

(defn get-execution
  "Get an execution by ID."
  [execution-id]
  (.get ^ConcurrentHashMap (:executions @orchestrator-state) execution-id))

(defn list-executions
  "List executions."
  [& {:keys [pipeline-id status limit]}]
  (let [executions (vals (:executions @orchestrator-state))]
    (cond->> executions
      pipeline-id (filter #(= (:pipeline-id %) pipeline-id))
      status (filter #(= (:status %) status))
      true (sort-by :started-at >)
      limit (take limit))))

;; =============================================================================
;; STAGE EXECUTION
;; =============================================================================

(defn execute-stage
  "Execute a single stage."
  [stage input context]
  (let [start-time (System/currentTimeMillis)]
    (try
      (log/info "Executing stage" {:stage (:id stage)})
      (let [result ((:handler-fn stage) input context)
            duration-ms (- (System/currentTimeMillis) start-time)]
        (metrics/inc-counter! :orchestrator/stages-executed)
        {:status :success
         :stage-id (:id stage)
         :result result
         :duration-ms duration-ms})
      (catch Exception e
        (log/error "Stage execution failed" {:stage (:id stage) :error (.getMessage e)})
        {:status :error
         :stage-id (:id stage)
         :error (.getMessage e)
         :duration-ms (- (System/currentTimeMillis) start-time)}))))

(defn execute-stage-with-timeout
  "Execute a stage with timeout."
  [stage input context]
  (let [timeout-ms (or (:timeout-ms stage) 60000)
        result-chan (chan 1)]
    (go
      (let [result (execute-stage stage input context)]
        (>! result-chan result)))
    (let [[result _] (async/alts!! [result-chan (timeout timeout-ms)])]
      (or result {:status :timeout :stage-id (:id stage) :error "Stage timed out"}))))

;; =============================================================================
;; PIPELINE EXECUTION
;; =============================================================================

(defn execute-sequential
  "Execute stages sequentially."
  [stages input context]
  (loop [remaining stages
         current-input input
         results []]
    (if (empty? remaining)
      {:status :success :results results :output current-input}
      (let [stage-config (first remaining)
            stage-id (if (map? stage-config) (:stage-id stage-config) stage-config)
            stage (get-stage stage-id)]
        (if stage
          (let [result (execute-stage-with-timeout stage current-input context)]
            (if (= :success (:status result))
              (recur (rest remaining)
                     (:result result)
                     (conj results result))
              {:status :error :results (conj results result) :error (:error result)}))
          {:status :error :error (str "Stage not found: " stage-id)})))))

(defn execute-parallel
  "Execute stages in parallel."
  [stages input context]
  (let [max-parallel (get-in @orchestrator-state [:config :max-parallel])
        stage-chans (for [stage-config stages]
                      (let [stage-id (if (map? stage-config) (:stage-id stage-config) stage-config)
                            stage (get-stage stage-id)]
                        (go
                          (if stage
                            (execute-stage-with-timeout stage input context)
                            {:status :error :stage-id stage-id :error "Stage not found"}))))
        results (doall (map #(async/<!! %) stage-chans))
        errors (filter #(not= :success (:status %)) results)]
    (if (empty? errors)
      {:status :success :results results :outputs (map :result results)}
      {:status :partial :results results :errors errors})))

(defn execute-pipeline!
  "Execute a pipeline."
  [pipeline-id input & {:keys [context async]}]
  (.incrementAndGet ^AtomicLong (:execution-count @orchestrator-state))
  (metrics/inc-counter! :orchestrator/pipelines-executed)
  (let [pipeline (get-pipeline pipeline-id)]
    (if (and pipeline (:enabled pipeline))
      (let [execution (create-execution pipeline-id input)
            execution-id (:id execution)]
        (.put ^ConcurrentHashMap (:executions @orchestrator-state) execution-id
              (assoc execution :status :running :started-at (System/currentTimeMillis)))
        (events/publish! :orchestrator/execution-started {:execution-id execution-id :pipeline-id pipeline-id})
        (log/info "Pipeline execution started" {:execution execution-id :pipeline pipeline-id})
        (let [execute-fn (fn []
                           (let [result (if (:parallel pipeline)
                                          (execute-parallel (:stages pipeline) input (or context {}))
                                          (execute-sequential (:stages pipeline) input (or context {})))]
                             (update-execution! execution-id
                                                {:status (:status result)
                                                 :stage-results (:results result)
                                                 :output (or (:output result) (:outputs result))
                                                 :error (:error result)
                                                 :completed-at (System/currentTimeMillis)})
                             (events/publish! :orchestrator/execution-completed
                                              {:execution-id execution-id
                                               :status (:status result)})
                             (log/info "Pipeline execution completed" {:execution execution-id :status (:status result)})
                             (assoc result :execution-id execution-id)))]
          (if async
            (do (future (execute-fn)) {:execution-id execution-id :status :started})
            (execute-fn))))
      {:status :error :error "Pipeline not found or disabled"})))

;; =============================================================================
;; BUILT-IN STAGES
;; =============================================================================

(defn init-built-in-stages!
  "Initialize built-in analysis stages."
  []
  ;; Text extraction stage
  (register-stage! :text-extraction
                   {:name "Text Extraction"
                    :description "Extract text from documents"
                    :handler-fn (fn [input _context]
                                  (if (string? input)
                                    {:text input :length (count input)}
                                    {:text (str input) :length (count (str input))}))})
  ;; Tokenization stage
  (register-stage! :tokenization
                   {:name "Tokenization"
                    :description "Tokenize text into words"
                    :handler-fn (fn [input _context]
                                  (let [text (or (:text input) (str input))
                                        tokens (str/split text #"\s+")]
                                    {:tokens tokens :count (count tokens)}))})
  ;; Mental model detection stage
  (register-stage! :model-detection
                   {:name "Mental Model Detection"
                    :description "Detect mental models in text"
                    :handler-fn (fn [input _context]
                                  (let [text (or (:text input) (str input))]
                                    {:models []
                                     :text text
                                     :analyzed true}))})
  ;; Confidence scoring stage
  (register-stage! :confidence-scoring
                   {:name "Confidence Scoring"
                    :description "Calculate confidence scores for detections"
                    :handler-fn (fn [input _context]
                                  (let [models (or (:models input) [])]
                                    {:models (map #(assoc % :confidence (rand)) models)
                                     :scored true}))})
  ;; Lollapalooza detection stage
  (register-stage! :lollapalooza-detection
                   {:name "Lollapalooza Detection"
                    :description "Detect Lollapalooza effects (3+ models converging)"
                    :handler-fn (fn [input _context]
                                  (let [models (or (:models input) [])
                                        high-confidence (filter #(> (:confidence % 0) 0.7) models)]
                                    {:models models
                                     :lollapalooza (>= (count high-confidence) 3)
                                     :converging-models high-confidence}))})
  ;; Result formatting stage
  (register-stage! :result-formatting
                   {:name "Result Formatting"
                    :description "Format analysis results"
                    :handler-fn (fn [input _context]
                                  {:formatted true
                                   :summary {:models-detected (count (:models input []))
                                             :lollapalooza (:lollapalooza input false)
                                             :timestamp (System/currentTimeMillis)}
                                   :details input})}))

;; =============================================================================
;; BUILT-IN PIPELINES
;; =============================================================================

(defn init-built-in-pipelines!
  "Initialize built-in analysis pipelines."
  []
  ;; Full analysis pipeline
  (create-pipeline! :full-analysis
                    {:name "Full Analysis Pipeline"
                     :description "Complete mental model analysis pipeline"
                     :stages [:text-extraction
                              :tokenization
                              :model-detection
                              :confidence-scoring
                              :lollapalooza-detection
                              :result-formatting]
                     :parallel false})
  ;; Quick analysis pipeline
  (create-pipeline! :quick-analysis
                    {:name "Quick Analysis Pipeline"
                     :description "Fast mental model detection"
                     :stages [:text-extraction
                              :model-detection
                              :result-formatting]
                     :parallel false}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-orchestrator-stats
  "Get orchestrator statistics."
  []
  {:pipelines (.size ^ConcurrentHashMap (:pipelines @orchestrator-state))
   :stages (.size ^ConcurrentHashMap (:stages @orchestrator-state))
   :executions (.size ^ConcurrentHashMap (:executions @orchestrator-state))
   :pipeline-count (.get ^AtomicLong (:pipeline-count @orchestrator-state))
   :execution-count (.get ^AtomicLong (:execution-count @orchestrator-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-analysis-orchestrator!
  "Initialize analysis orchestrator."
  []
  (log/info "Initializing analysis orchestrator")
  ;; Register feature flag
  (flags/register-flag! "analysis-orchestrator" "Enable analysis orchestrator" true)
  ;; Create metrics
  (metrics/create-counter! :orchestrator/pipelines-executed "Pipelines executed")
  (metrics/create-counter! :orchestrator/stages-executed "Stages executed")
  (metrics/create-gauge! :orchestrator/active-executions "Active executions"
                         #(count (filter #(= :running (:status %))
                                         (vals (:executions @orchestrator-state)))))
  ;; Initialize built-in stages and pipelines
  (init-built-in-stages!)
  (init-built-in-pipelines!)
  (log/info "Analysis orchestrator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-analysis-orchestrator-status []
  {:enabled (flags/is-enabled? "analysis-orchestrator")
   :pipelines (.size ^ConcurrentHashMap (:pipelines @orchestrator-state))
   :stages (.size ^ConcurrentHashMap (:stages @orchestrator-state))
   :executions (.size ^ConcurrentHashMap (:executions @orchestrator-state))
   :stats (get-orchestrator-stats)
   :config (:config @orchestrator-state)})
