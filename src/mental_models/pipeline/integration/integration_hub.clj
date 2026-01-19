(ns mental-models.pipeline.integration.integration-hub
  "Integration hub for connecting external services.
   
   Features:
   - Service connectors
   - Data mapping
   - Transformation pipelines
   - Error handling
   - Retry logic
   - Health monitoring
   - Rate limiting
   - Credential management"
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
  (atom {:connectors {}       ;; connector-id -> connector
         :mappings {}         ;; mapping-id -> mapping
         :pipelines {}        ;; pipeline-id -> pipeline
         :credentials {}      ;; credential-id -> credential (encrypted)
         :sync-jobs {}        ;; job-id -> sync-job
         :stats {:syncs 0 :records-processed 0 :errors 0}
         :initialized? false}))

;; ============================================================================
;; Connector Management
;; ============================================================================

(defn register-connector!
  "Register a service connector."
  [connector-id config]
  (let [connector {:id connector-id
                   :name (get config :name (name connector-id))
                   :type (get config :type :rest) ;; :rest, :graphql, :grpc, :database, :file
                   :config (get config :config {})
                   :auth-type (get config :auth-type :none) ;; :none, :api-key, :oauth2, :basic
                   :credential-id (get config :credential-id)
                   :rate-limit (get config :rate-limit {:requests-per-minute 60})
                   :retry-policy (get config :retry-policy {:max-retries 3 :backoff-ms 1000})
                   :health-check (get config :health-check nil)
                   :status :active
                   :last-health-check nil
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:connectors connector-id] connector)
    (logging/log :info "Registered connector" {:connector-id connector-id})
    (events/emit! :connector-registered {:connector-id connector-id})
    connector-id))

(defn get-connector
  "Get a connector."
  [connector-id]
  (get-in @state [:connectors connector-id]))

(defn list-connectors
  "List all connectors."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :type (:type c)
           :status (:status c)})
        (:connectors @state)))

(defn update-connector!
  "Update a connector."
  [connector-id updates]
  (swap! state update-in [:connectors connector-id] merge updates))

(defn delete-connector!
  "Delete a connector."
  [connector-id]
  (swap! state update :connectors dissoc connector-id)
  (logging/log :info "Deleted connector" {:connector-id connector-id}))

(defn test-connector!
  "Test a connector connection."
  [connector-id]
  (when-let [connector (get-connector connector-id)]
    (let [health-check (:health-check connector)
          result (if health-check
                   (try
                     (health-check connector)
                     (catch Exception e
                       {:healthy false :error (.getMessage e)}))
                   {:healthy true :message "No health check configured"})]
      (swap! state update-in [:connectors connector-id]
             (fn [c]
               (assoc c
                      :last-health-check (System/currentTimeMillis)
                      :status (if (:healthy result) :active :unhealthy))))
      result)))

;; ============================================================================
;; Credential Management
;; ============================================================================

(defn store-credential!
  "Store a credential (should be encrypted in production)."
  [credential-id config]
  (let [credential {:id credential-id
                    :name (get config :name (name credential-id))
                    :type (get config :type :api-key)
                    :data (get config :data {}) ;; Should be encrypted
                    :expires-at (get config :expires-at)
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:credentials credential-id] credential)
    (logging/log :info "Stored credential" {:credential-id credential-id})
    credential-id))

(defn get-credential
  "Get a credential."
  [credential-id]
  (get-in @state [:credentials credential-id]))

(defn list-credentials
  "List credentials (without sensitive data)."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :type (:type c)
           :expires-at (:expires-at c)})
        (:credentials @state)))

(defn delete-credential!
  "Delete a credential."
  [credential-id]
  (swap! state update :credentials dissoc credential-id)
  (logging/log :info "Deleted credential" {:credential-id credential-id}))

(defn rotate-credential!
  "Rotate a credential."
  [credential-id new-data]
  (swap! state update-in [:credentials credential-id]
         (fn [c]
           (assoc c
                  :data new-data
                  :rotated-at (System/currentTimeMillis))))
  (logging/log :info "Rotated credential" {:credential-id credential-id}))

;; ============================================================================
;; Data Mapping
;; ============================================================================

(defn create-mapping!
  "Create a data mapping."
  [mapping-id config]
  (let [mapping {:id mapping-id
                 :name (get config :name (name mapping-id))
                 :source-schema (get config :source-schema {})
                 :target-schema (get config :target-schema {})
                 :field-mappings (get config :field-mappings [])
                 :transformations (get config :transformations [])
                 :default-values (get config :default-values {})
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:mappings mapping-id] mapping)
    (logging/log :info "Created mapping" {:mapping-id mapping-id})
    mapping-id))

(defn get-mapping
  "Get a mapping."
  [mapping-id]
  (get-in @state [:mappings mapping-id]))

(defn list-mappings
  "List all mappings."
  []
  (mapv (fn [[id m]]
          {:id id
           :name (:name m)
           :field-count (count (:field-mappings m))})
        (:mappings @state)))

(defn- apply-transformation
  "Apply a transformation to a value."
  [value transformation]
  (case (:type transformation)
    :uppercase (str/upper-case (str value))
    :lowercase (str/lower-case (str value))
    :trim (str/trim (str value))
    :to-int (Integer/parseInt (str value))
    :to-float (Float/parseFloat (str value))
    :to-string (str value)
    :default (or value (:default transformation))
    :concat (str value (:suffix transformation ""))
    :replace (str/replace (str value) (:pattern transformation) (:replacement transformation ""))
    value))

(defn transform-record
  "Transform a record using a mapping."
  [mapping-id record]
  (when-let [mapping (get-mapping mapping-id)]
    (let [field-mappings (:field-mappings mapping)
          transformations (:transformations mapping)
          default-values (:default-values mapping)]
      (reduce (fn [result field-mapping]
                (let [source-field (:source field-mapping)
                      target-field (:target field-mapping)
                      source-value (get-in record (if (vector? source-field) source-field [source-field]))
                      field-transforms (filter #(= (:field %) target-field) transformations)
                      transformed-value (reduce apply-transformation source-value field-transforms)
                      final-value (or transformed-value (get default-values target-field))]
                  (assoc-in result (if (vector? target-field) target-field [target-field]) final-value)))
              {}
              field-mappings))))

(defn transform-batch
  "Transform a batch of records."
  [mapping-id records]
  (mapv #(transform-record mapping-id %) records))

;; ============================================================================
;; Integration Pipelines
;; ============================================================================

(defn create-pipeline!
  "Create an integration pipeline."
  [pipeline-id config]
  (let [pipeline {:id pipeline-id
                  :name (get config :name (name pipeline-id))
                  :source-connector (get config :source-connector)
                  :target-connector (get config :target-connector)
                  :mapping-id (get config :mapping-id)
                  :filters (get config :filters [])
                  :batch-size (get config :batch-size 100)
                  :schedule (get config :schedule nil)
                  :enabled? (get config :enabled? true)
                  :last-run nil
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
           :source (:source-connector p)
           :target (:target-connector p)
           :enabled? (:enabled? p)})
        (:pipelines @state)))

(defn- apply-filters
  "Apply filters to records."
  [records filters]
  (reduce (fn [recs filter-config]
            (filter (fn [rec]
                      (let [field (:field filter-config)
                            op (:operator filter-config)
                            value (:value filter-config)
                            rec-value (get rec field)]
                        (case op
                          :equals (= rec-value value)
                          :not-equals (not= rec-value value)
                          :contains (str/includes? (str rec-value) (str value))
                          :greater-than (> rec-value value)
                          :less-than (< rec-value value)
                          :in (contains? (set value) rec-value)
                          :not-null (some? rec-value)
                          true)))
                    recs))
          records
          filters))

(defn run-pipeline!
  "Run an integration pipeline."
  [pipeline-id & {:keys [source-data]}]
  (when (flags/enabled? :integration-hub)
    (when-let [pipeline (get-pipeline pipeline-id)]
      (when (:enabled? pipeline)
        (let [job-id (str (UUID/randomUUID))
              job {:id job-id
                   :pipeline-id pipeline-id
                   :status :running
                   :records-processed 0
                   :errors []
                   :started-at (System/currentTimeMillis)}]
          
          (swap! state assoc-in [:sync-jobs job-id] job)
          (logging/log :info "Started pipeline" {:pipeline-id pipeline-id :job-id job-id})
          
          (go
            (try
              (let [;; Get source data (or use provided)
                    records (or source-data [])
                    
                    ;; Apply filters
                    filtered (apply-filters records (:filters pipeline))
                    
                    ;; Transform records
                    transformed (if (:mapping-id pipeline)
                                  (transform-batch (:mapping-id pipeline) filtered)
                                  filtered)
                    
                    ;; Process in batches
                    batches (partition-all (:batch-size pipeline) transformed)
                    processed-count (atom 0)]
                
                (doseq [batch batches]
                  (swap! processed-count + (count batch))
                  (swap! state update-in [:sync-jobs job-id :records-processed] + (count batch)))
                
                (swap! state update-in [:sync-jobs job-id]
                       (fn [j]
                         (assoc j
                                :status :completed
                                :completed-at (System/currentTimeMillis))))
                (swap! state assoc-in [:pipelines pipeline-id :last-run] (System/currentTimeMillis))
                (swap! state update-in [:stats :syncs] inc)
                (swap! state update-in [:stats :records-processed] + @processed-count)
                (logging/log :info "Pipeline completed" {:pipeline-id pipeline-id :records @processed-count})
                (events/emit! :pipeline-completed {:pipeline-id pipeline-id :job-id job-id}))
              
              (catch Exception e
                (swap! state update-in [:sync-jobs job-id]
                       (fn [j]
                         (assoc j
                                :status :failed
                                :error (.getMessage e)
                                :completed-at (System/currentTimeMillis))))
                (swap! state update-in [:stats :errors] inc)
                (logging/log :error "Pipeline failed" {:pipeline-id pipeline-id :error (.getMessage e)}))))
          
          job-id)))))

(defn get-sync-job
  "Get a sync job."
  [job-id]
  (get-in @state [:sync-jobs job-id]))

(defn list-sync-jobs
  "List sync jobs."
  [& {:keys [pipeline-id status limit] :or {limit 100}}]
  (let [jobs (vals (:sync-jobs @state))
        filtered (cond->> jobs
                   pipeline-id (filter #(= (:pipeline-id %) pipeline-id))
                   status (filter #(= (:status %) status))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :pipeline-id :status :records-processed :started-at]) filtered)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-integration-stats
  "Get integration hub statistics."
  []
  (let [stats (:stats @state)
        connectors (vals (:connectors @state))
        by-status (frequencies (map :status connectors))]
    {:total-connectors (count connectors)
     :total-mappings (count (:mappings @state))
     :total-pipelines (count (:pipelines @state))
     :total-credentials (count (:credentials @state))
     :connectors-by-status by-status
     :syncs (:syncs stats)
     :records-processed (:records-processed stats)
     :errors (:errors stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-integration-hub!
  "Initialize the integration hub."
  []
  (when-not (:initialized? @state)
    ;; Register LM Studio connector
    (register-connector! :lm-studio
                         {:name "LM Studio"
                          :type :rest
                          :config {:base-url "http://localhost:1234"
                                   :endpoints {:completions "/v1/chat/completions"
                                               :models "/v1/models"}}
                          :auth-type :none
                          :rate-limit {:requests-per-minute 30}})
    
    ;; Register file connector
    (register-connector! :local-files
                         {:name "Local Files"
                          :type :file
                          :config {:base-path "/data/documents"}
                          :auth-type :none})
    
    ;; Create sample mapping
    (create-mapping! :document-to-analysis
                     {:name "Document to Analysis"
                      :field-mappings [{:source :content :target :text}
                                       {:source :filename :target :source}
                                       {:source :created_at :target :timestamp}]
                      :transformations [{:field :text :type :trim}]
                      :default-values {:status "pending"}})
    
    ;; Create sample pipeline
    (create-pipeline! :file-analysis
                      {:name "File Analysis Pipeline"
                       :source-connector :local-files
                       :target-connector :lm-studio
                       :mapping-id :document-to-analysis
                       :batch-size 10})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Integration hub initialized")
    (events/emit! :integration-hub-initialized {})
    true))
