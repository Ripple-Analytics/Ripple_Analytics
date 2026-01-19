(ns mental-models.pipeline.integration.data-processor
  "Data processor for mental model analysis pipelines.
   
   Features:
   - Data transformation
   - Data validation
   - Data enrichment
   - Batch processing
   - Stream processing
   - Data quality checks
   - Schema enforcement
   - Error handling"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
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
  (atom {:processors {}       ;; processor-id -> processor
         :pipelines {}        ;; pipeline-id -> pipeline
         :schemas {}          ;; schema-id -> schema
         :enrichers {}        ;; enricher-id -> enricher-fn
         :validators {}       ;; validator-id -> validator-fn
         :stats {:records-processed 0 :records-failed 0 :batches-completed 0}
         :initialized? false}))

;; ============================================================================
;; Schema Management
;; ============================================================================

(defn define-schema!
  "Define a data schema."
  [schema-id config]
  (let [schema {:id schema-id
                :name (get config :name (name schema-id))
                :fields (get config :fields [])
                :required (get config :required [])
                :strict? (get config :strict? false)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schemas schema-id] schema)
    (logging/log :info "Defined schema" {:schema-id schema-id})
    schema-id))

(defn get-schema
  "Get a schema."
  [schema-id]
  (get-in @state [:schemas schema-id]))

(defn list-schemas
  "List all schemas."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :field-count (count (:fields s))})
        (:schemas @state)))

(defn- validate-against-schema
  "Validate a record against a schema."
  [record schema]
  (let [required (:required schema)
        fields (:fields schema)
        field-map (into {} (map (juxt :name identity) fields))
        
        ;; Check required fields
        missing-required (filter #(nil? (get record (keyword %))) required)
        
        ;; Check field types
        type-errors (reduce (fn [errors field]
                              (let [field-name (keyword (:name field))
                                    value (get record field-name)
                                    expected-type (:type field)]
                                (if (and (some? value)
                                         (not (case expected-type
                                                :string (string? value)
                                                :number (number? value)
                                                :boolean (boolean? value)
                                                :array (vector? value)
                                                :object (map? value)
                                                :any true
                                                true)))
                                  (conj errors {:field field-name :expected expected-type :actual (type value)})
                                  errors)))
                            []
                            fields)
        
        ;; Check for unknown fields in strict mode
        unknown-fields (when (:strict? schema)
                         (let [known-fields (set (map (comp keyword :name) fields))]
                           (filter #(not (contains? known-fields %)) (keys record))))]
    
    {:valid? (and (empty? missing-required) (empty? type-errors) (empty? unknown-fields))
     :errors (cond-> {}
               (seq missing-required) (assoc :missing-required missing-required)
               (seq type-errors) (assoc :type-errors type-errors)
               (seq unknown-fields) (assoc :unknown-fields unknown-fields))}))

;; ============================================================================
;; Validator Management
;; ============================================================================

(defn register-validator!
  "Register a validator function."
  [validator-id validator-fn]
  (swap! state assoc-in [:validators validator-id] validator-fn)
  (logging/log :info "Registered validator" {:validator-id validator-id})
  validator-id)

(defn get-validator
  "Get a validator function."
  [validator-id]
  (get-in @state [:validators validator-id]))

(defn list-validators
  "List all validators."
  []
  (keys (:validators @state)))

(defn validate-record
  "Validate a record using registered validators."
  [record validator-ids]
  (let [results (map (fn [validator-id]
                       (if-let [validator-fn (get-validator validator-id)]
                         {:validator validator-id
                          :result (validator-fn record)}
                         {:validator validator-id
                          :result {:valid? false :error "Validator not found"}}))
                     validator-ids)]
    {:valid? (every? #(get-in % [:result :valid?]) results)
     :results results}))

;; ============================================================================
;; Enricher Management
;; ============================================================================

(defn register-enricher!
  "Register an enricher function."
  [enricher-id enricher-fn]
  (swap! state assoc-in [:enrichers enricher-id] enricher-fn)
  (logging/log :info "Registered enricher" {:enricher-id enricher-id})
  enricher-id)

(defn get-enricher
  "Get an enricher function."
  [enricher-id]
  (get-in @state [:enrichers enricher-id]))

(defn list-enrichers
  "List all enrichers."
  []
  (keys (:enrichers @state)))

(defn enrich-record
  "Enrich a record using registered enrichers."
  [record enricher-ids]
  (reduce (fn [rec enricher-id]
            (if-let [enricher-fn (get-enricher enricher-id)]
              (enricher-fn rec)
              rec))
          record
          enricher-ids))

;; ============================================================================
;; Processor Definition
;; ============================================================================

(defn create-processor!
  "Create a data processor."
  [processor-id config]
  (let [processor {:id processor-id
                   :name (get config :name (name processor-id))
                   :input-schema (get config :input-schema)
                   :output-schema (get config :output-schema)
                   :validators (get config :validators [])
                   :enrichers (get config :enrichers [])
                   :transform-fn (get config :transform-fn identity)
                   :error-handler (get config :error-handler)
                   :batch-size (get config :batch-size 100)
                   :parallel? (get config :parallel? false)
                   :enabled? (get config :enabled? true)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:processors processor-id] processor)
    (logging/log :info "Created processor" {:processor-id processor-id})
    processor-id))

(defn get-processor
  "Get a processor."
  [processor-id]
  (get-in @state [:processors processor-id]))

(defn list-processors
  "List all processors."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :enabled? (:enabled? p)})
        (:processors @state)))

(defn enable-processor!
  "Enable a processor."
  [processor-id]
  (swap! state assoc-in [:processors processor-id :enabled?] true))

(defn disable-processor!
  "Disable a processor."
  [processor-id]
  (swap! state assoc-in [:processors processor-id :enabled?] false))

;; ============================================================================
;; Record Processing
;; ============================================================================

(defn- process-single-record
  "Process a single record through a processor."
  [processor record]
  (try
    (let [;; Validate input
          input-validation (when (:input-schema processor)
                             (validate-against-schema record (get-schema (:input-schema processor))))
          
          _ (when (and input-validation (not (:valid? input-validation)))
              (throw (ex-info "Input validation failed" {:errors (:errors input-validation)})))
          
          ;; Run validators
          validation-result (when (seq (:validators processor))
                              (validate-record record (:validators processor)))
          
          _ (when (and validation-result (not (:valid? validation-result)))
              (throw (ex-info "Validation failed" {:results (:results validation-result)})))
          
          ;; Enrich record
          enriched (enrich-record record (:enrichers processor))
          
          ;; Transform
          transformed ((:transform-fn processor) enriched)
          
          ;; Validate output
          output-validation (when (:output-schema processor)
                              (validate-against-schema transformed (get-schema (:output-schema processor))))
          
          _ (when (and output-validation (not (:valid? output-validation)))
              (throw (ex-info "Output validation failed" {:errors (:errors output-validation)})))]
      
      {:success true
       :record transformed})
    
    (catch Exception e
      (when-let [error-handler (:error-handler processor)]
        (error-handler record e))
      {:success false
       :error (.getMessage e)
       :record record})))

(defn process-record
  "Process a record through a processor."
  [processor-id record]
  (when (flags/enabled? :data-processor)
    (when-let [processor (get-processor processor-id)]
      (when (:enabled? processor)
        (let [result (process-single-record processor record)]
          (if (:success result)
            (swap! state update-in [:stats :records-processed] inc)
            (swap! state update-in [:stats :records-failed] inc))
          result)))))

(defn process-batch
  "Process a batch of records."
  [processor-id records]
  (when (flags/enabled? :data-processor)
    (when-let [processor (get-processor processor-id)]
      (when (:enabled? processor)
        (let [process-fn (if (:parallel? processor) pmap map)
              results (doall (process-fn #(process-single-record processor %) records))
              successful (filter :success results)
              failed (filter (complement :success) results)]
          
          (swap! state update-in [:stats :records-processed] + (count successful))
          (swap! state update-in [:stats :records-failed] + (count failed))
          (swap! state update-in [:stats :batches-completed] inc)
          
          {:total (count records)
           :successful (count successful)
           :failed (count failed)
           :results results})))))

;; ============================================================================
;; Pipeline Definition
;; ============================================================================

(defn create-pipeline!
  "Create a processing pipeline."
  [pipeline-id config]
  (let [pipeline {:id pipeline-id
                  :name (get config :name (name pipeline-id))
                  :stages (get config :stages [])
                  :error-strategy (get config :error-strategy :stop) ;; :stop, :skip, :collect
                  :enabled? (get config :enabled? true)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:pipelines pipeline-id] pipeline)
    (logging/log :info "Created pipeline" {:pipeline-id pipeline-id})
    pipeline-id))

(defn get-pipeline
  "Get a pipeline."
  [pipeline-id]
  (get-in @state [:pipelines pipeline-id]))

(defn list-pipelines
  "List all pipelines."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :stage-count (count (:stages p))
           :enabled? (:enabled? p)})
        (:pipelines @state)))

(defn run-pipeline
  "Run a pipeline on a record."
  [pipeline-id record]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (when (:enabled? pipeline)
      (loop [stages (:stages pipeline)
             current-record record
             results []]
        (if (empty? stages)
          {:success true
           :record current-record
           :stage-results results}
          (let [stage (first stages)
                processor-id (:processor stage)
                result (process-record processor-id current-record)]
            (case (:error-strategy pipeline)
              :stop
              (if (:success result)
                (recur (rest stages) (:record result) (conj results result))
                {:success false
                 :error (:error result)
                 :failed-stage processor-id
                 :stage-results (conj results result)})
              
              :skip
              (recur (rest stages)
                     (if (:success result) (:record result) current-record)
                     (conj results result))
              
              :collect
              (recur (rest stages)
                     (if (:success result) (:record result) current-record)
                     (conj results result)))))))))

(defn run-pipeline-batch
  "Run a pipeline on a batch of records."
  [pipeline-id records]
  (let [results (mapv #(run-pipeline pipeline-id %) records)
        successful (filter :success results)
        failed (filter (complement :success) results)]
    {:total (count records)
     :successful (count successful)
     :failed (count failed)
     :results results}))

;; ============================================================================
;; Stream Processing
;; ============================================================================

(defn create-stream-processor
  "Create a stream processor."
  [processor-id & {:keys [buffer-size] :or {buffer-size 100}}]
  (let [input-chan (chan buffer-size)
        output-chan (chan buffer-size)]
    
    (go-loop []
      (when-let [record (<! input-chan)]
        (let [result (process-record processor-id record)]
          (>! output-chan result))
        (recur)))
    
    {:input input-chan
     :output output-chan
     :processor-id processor-id}))

(defn close-stream-processor!
  "Close a stream processor."
  [stream-processor]
  (close! (:input stream-processor))
  (close! (:output stream-processor)))

;; ============================================================================
;; Data Quality
;; ============================================================================

(defn check-data-quality
  "Check data quality for a batch of records."
  [records & {:keys [checks]}]
  (let [default-checks {:completeness (fn [recs]
                                        (let [total-fields (* (count recs) (count (keys (first recs))))
                                              non-null-fields (reduce + (map #(count (filter some? (vals %))) recs))]
                                          (if (pos? total-fields)
                                            (/ non-null-fields total-fields)
                                            1.0)))
                        :uniqueness (fn [recs]
                                      (if (seq recs)
                                        (/ (count (distinct recs)) (count recs))
                                        1.0))
                        :consistency (fn [recs]
                                       ;; Check if all records have same keys
                                       (if (seq recs)
                                         (let [key-sets (map (comp set keys) recs)]
                                           (if (apply = key-sets) 1.0 0.5))
                                         1.0))}
        checks-to-run (or checks default-checks)]
    (reduce (fn [results [check-name check-fn]]
              (assoc results check-name (check-fn records)))
            {}
            checks-to-run)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-processor-stats
  "Get processor statistics."
  []
  (let [stats (:stats @state)]
    {:total-processors (count (:processors @state))
     :total-pipelines (count (:pipelines @state))
     :total-schemas (count (:schemas @state))
     :total-validators (count (:validators @state))
     :total-enrichers (count (:enrichers @state))
     :records-processed (:records-processed stats)
     :records-failed (:records-failed stats)
     :batches-completed (:batches-completed stats)
     :success-rate (if (pos? (+ (:records-processed stats) (:records-failed stats)))
                     (/ (:records-processed stats)
                        (+ (:records-processed stats) (:records-failed stats)))
                     1.0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-data-processor!
  "Initialize the data processor."
  []
  (when-not (:initialized? @state)
    ;; Define schemas
    (define-schema! :document
                    {:name "Document"
                     :fields [{:name "id" :type :string}
                              {:name "content" :type :string}
                              {:name "source" :type :string}
                              {:name "timestamp" :type :number}]
                     :required ["id" "content"]})
    
    (define-schema! :analysis-result
                    {:name "Analysis Result"
                     :fields [{:name "document-id" :type :string}
                              {:name "models" :type :array}
                              {:name "confidence" :type :number}
                              {:name "timestamp" :type :number}]
                     :required ["document-id" "models"]})
    
    ;; Register validators
    (register-validator! :not-empty
                         (fn [record]
                           {:valid? (and (some? (:content record))
                                         (not (str/blank? (:content record))))
                            :error (when (str/blank? (:content record)) "Content is empty")}))
    
    (register-validator! :valid-confidence
                         (fn [record]
                           (let [conf (:confidence record)]
                             {:valid? (or (nil? conf) (and (>= conf 0) (<= conf 1)))
                              :error (when (and conf (or (< conf 0) (> conf 1)))
                                       "Confidence must be between 0 and 1")})))
    
    ;; Register enrichers
    (register-enricher! :add-timestamp
                        (fn [record]
                          (assoc record :processed-at (System/currentTimeMillis))))
    
    (register-enricher! :normalize-text
                        (fn [record]
                          (if (:content record)
                            (update record :content str/trim)
                            record)))
    
    ;; Create default processor
    (create-processor! :document-processor
                       {:name "Document Processor"
                        :input-schema :document
                        :validators [:not-empty]
                        :enrichers [:add-timestamp :normalize-text]
                        :transform-fn identity})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Data processor initialized")
    (events/emit! :data-processor-initialized {})
    true))
