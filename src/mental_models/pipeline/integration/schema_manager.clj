(ns mental-models.pipeline.integration.schema-manager
  "Schema Manager Module
   
   Schema management and evolution:
   - Schema definition and validation
   - Schema versioning
   - Schema migration
   - Compatibility checking
   - Schema registry"
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; SCHEMA MANAGER STATE
;; =============================================================================

(defonce schema-state (atom {:schemas (ConcurrentHashMap.)
                             :versions (ConcurrentHashMap.)
                             :migrations (ConcurrentHashMap.)
                             :validators {}
                             :schema-count (AtomicLong. 0)
                             :validation-count (AtomicLong. 0)
                             :config {:strict-mode false
                                      :allow-unknown-fields true
                                      :default-version "1.0.0"}}))

;; =============================================================================
;; TYPE VALIDATORS
;; =============================================================================

(defn register-validator!
  "Register a type validator."
  [type-id validator-fn]
  (swap! schema-state assoc-in [:validators type-id] validator-fn))

(defn get-validator
  "Get a validator by type."
  [type-id]
  (get-in @schema-state [:validators type-id]))

(defn init-built-in-validators!
  "Initialize built-in type validators."
  []
  (register-validator! :string string?)
  (register-validator! :number number?)
  (register-validator! :integer integer?)
  (register-validator! :float float?)
  (register-validator! :boolean boolean?)
  (register-validator! :keyword keyword?)
  (register-validator! :symbol symbol?)
  (register-validator! :map map?)
  (register-validator! :vector vector?)
  (register-validator! :list list?)
  (register-validator! :set set?)
  (register-validator! :seq seq?)
  (register-validator! :nil nil?)
  (register-validator! :any (constantly true))
  (register-validator! :uuid #(instance? java.util.UUID %))
  (register-validator! :date #(instance? java.util.Date %))
  (register-validator! :instant #(instance? java.time.Instant %))
  (register-validator! :email #(and (string? %) (re-matches #".+@.+\..+" %)))
  (register-validator! :url #(and (string? %) (re-matches #"https?://.*" %)))
  (register-validator! :positive #(and (number? %) (pos? %)))
  (register-validator! :negative #(and (number? %) (neg? %)))
  (register-validator! :non-negative #(and (number? %) (>= % 0)))
  (register-validator! :non-empty-string #(and (string? %) (not (str/blank? %)))))

;; =============================================================================
;; SCHEMA DEFINITION
;; =============================================================================

(defn create-field
  "Create a field definition."
  [field-name type & {:keys [required default description validators]}]
  {:name field-name
   :type type
   :required (if (nil? required) true required)
   :default default
   :description description
   :validators (or validators [])})

(defn create-schema
  "Create a schema definition."
  [schema-id {:keys [name description version fields]}]
  {:id schema-id
   :name name
   :description description
   :version (or version "1.0.0")
   :fields (into {} (map (fn [f] [(:name f) f]) fields))
   :created-at (System/currentTimeMillis)})

(defn register-schema!
  "Register a schema."
  [schema-id schema-def]
  (.incrementAndGet ^AtomicLong (:schema-count @schema-state))
  (log/info "Registering schema" {:id schema-id :version (:version schema-def)})
  (let [schema (if (:id schema-def)
                 schema-def
                 (create-schema schema-id schema-def))]
    (.put ^ConcurrentHashMap (:schemas @schema-state) schema-id schema)
    ;; Track version
    (let [versions-key (str schema-id "-versions")
          versions (or (.get ^ConcurrentHashMap (:versions @schema-state) versions-key) [])]
      (.put ^ConcurrentHashMap (:versions @schema-state) versions-key
            (conj versions {:version (:version schema)
                            :schema schema
                            :registered-at (System/currentTimeMillis)})))
    schema))

(defn unregister-schema!
  "Unregister a schema."
  [schema-id]
  (.remove ^ConcurrentHashMap (:schemas @schema-state) schema-id))

(defn get-schema
  "Get a schema by ID."
  [schema-id]
  (.get ^ConcurrentHashMap (:schemas @schema-state) schema-id))

(defn list-schemas
  "List all schemas."
  []
  (vec (vals (:schemas @schema-state))))

(defn get-schema-versions
  "Get all versions of a schema."
  [schema-id]
  (let [versions-key (str schema-id "-versions")]
    (or (.get ^ConcurrentHashMap (:versions @schema-state) versions-key) [])))

;; =============================================================================
;; VALIDATION
;; =============================================================================

(defn validate-field
  "Validate a single field value."
  [field-def value]
  (let [{:keys [name type required validators]} field-def]
    (cond
      ;; Check required
      (and required (nil? value))
      {:valid false :field name :error "Field is required"}
      ;; Skip validation for nil optional fields
      (nil? value)
      {:valid true :field name}
      ;; Check type
      :else
      (let [type-validator (get-validator type)]
        (if (and type-validator (not (type-validator value)))
          {:valid false :field name :error (str "Expected type " type)}
          ;; Run custom validators
          (let [errors (filter some?
                               (map (fn [v]
                                      (when-not (v value)
                                        (str "Custom validation failed for " name)))
                                    validators))]
            (if (seq errors)
              {:valid false :field name :errors errors}
              {:valid true :field name})))))))

(defn validate
  "Validate data against a schema."
  [schema-id data]
  (.incrementAndGet ^AtomicLong (:validation-count @schema-state))
  (metrics/inc-counter! :schemamanager/validations)
  (let [schema (get-schema schema-id)]
    (if schema
      (let [fields (:fields schema)
            allow-unknown (get-in @schema-state [:config :allow-unknown-fields])
            ;; Validate known fields
            field-results (map (fn [[field-name field-def]]
                                 (validate-field field-def (get data field-name)))
                               fields)
            ;; Check for unknown fields
            unknown-fields (when-not allow-unknown
                             (let [known-keys (set (keys fields))
                                   data-keys (set (keys data))]
                               (clojure.set/difference data-keys known-keys)))
            all-valid (and (every? :valid field-results)
                           (empty? unknown-fields))
            errors (concat (filter #(not (:valid %)) field-results)
                           (when (seq unknown-fields)
                             [{:error "Unknown fields" :fields unknown-fields}]))]
        {:valid all-valid
         :schema-id schema-id
         :errors (when-not all-valid errors)})
      {:valid false
       :schema-id schema-id
       :error "Schema not found"})))

(defn validate!
  "Validate data and throw on error."
  [schema-id data]
  (let [result (validate schema-id data)]
    (when-not (:valid result)
      (throw (ex-info "Validation failed" result)))
    data))

;; =============================================================================
;; SCHEMA MIGRATION
;; =============================================================================

(defn register-migration!
  "Register a schema migration."
  [migration-id {:keys [from-schema from-version to-schema to-version migrate-fn]}]
  (log/info "Registering migration" {:id migration-id :from from-version :to to-version})
  (.put ^ConcurrentHashMap (:migrations @schema-state) migration-id
        {:id migration-id
         :from-schema from-schema
         :from-version from-version
         :to-schema to-schema
         :to-version to-version
         :migrate-fn migrate-fn
         :registered-at (System/currentTimeMillis)}))

(defn get-migration
  "Get a migration by ID."
  [migration-id]
  (.get ^ConcurrentHashMap (:migrations @schema-state) migration-id))

(defn find-migration
  "Find a migration between versions."
  [from-schema from-version to-version]
  (first (filter (fn [[_ m]]
                   (and (= (:from-schema m) from-schema)
                        (= (:from-version m) from-version)
                        (= (:to-version m) to-version)))
                 (:migrations @schema-state))))

(defn migrate
  "Migrate data from one schema version to another."
  [data from-schema from-version to-version]
  (if-let [[_ migration] (find-migration from-schema from-version to-version)]
    (try
      (let [migrated ((:migrate-fn migration) data)]
        (log/info "Migration successful" {:from from-version :to to-version})
        {:success true :data migrated})
      (catch Exception e
        (log/error "Migration failed" {:error (.getMessage e)})
        {:success false :error (.getMessage e)}))
    {:success false :error "No migration found"}))

;; =============================================================================
;; COMPATIBILITY CHECKING
;; =============================================================================

(defn check-compatibility
  "Check if two schema versions are compatible."
  [schema-id version1 version2]
  (let [versions (get-schema-versions schema-id)
        v1-schema (first (filter #(= (:version %) version1) versions))
        v2-schema (first (filter #(= (:version %) version2) versions))]
    (if (and v1-schema v2-schema)
      (let [v1-fields (set (keys (:fields (:schema v1-schema))))
            v2-fields (set (keys (:fields (:schema v2-schema))))
            added (clojure.set/difference v2-fields v1-fields)
            removed (clojure.set/difference v1-fields v2-fields)
            common (clojure.set/intersection v1-fields v2-fields)
            ;; Check for type changes in common fields
            type-changes (filter (fn [f]
                                   (not= (get-in (:schema v1-schema) [:fields f :type])
                                         (get-in (:schema v2-schema) [:fields f :type])))
                                 common)]
        {:compatible (and (empty? removed) (empty? type-changes))
         :added added
         :removed removed
         :type-changes type-changes
         :backward-compatible (empty? removed)
         :forward-compatible (empty? added)})
      {:compatible false :error "One or both versions not found"})))

;; =============================================================================
;; SCHEMA INFERENCE
;; =============================================================================

(defn infer-type
  "Infer the type of a value."
  [value]
  (cond
    (nil? value) :nil
    (string? value) :string
    (integer? value) :integer
    (float? value) :float
    (boolean? value) :boolean
    (keyword? value) :keyword
    (symbol? value) :symbol
    (map? value) :map
    (vector? value) :vector
    (list? value) :list
    (set? value) :set
    (instance? java.util.UUID value) :uuid
    (instance? java.util.Date value) :date
    :else :any))

(defn infer-schema
  "Infer a schema from sample data."
  [schema-id data-samples & {:keys [name description]}]
  (let [all-keys (reduce (fn [acc sample]
                           (into acc (keys sample)))
                         #{}
                         data-samples)
        fields (map (fn [k]
                      (let [values (map #(get % k) data-samples)
                            non-nil-values (filter some? values)
                            types (set (map infer-type non-nil-values))
                            inferred-type (if (= 1 (count types))
                                            (first types)
                                            :any)
                            required (every? some? values)]
                        (create-field k inferred-type :required required)))
                    all-keys)]
    (register-schema! schema-id
                      {:name (or name (str schema-id))
                       :description (or description "Inferred schema")
                       :version "1.0.0"
                       :fields fields})))

;; =============================================================================
;; SCHEMA COERCION
;; =============================================================================

(defn coerce-value
  "Coerce a value to a target type."
  [value target-type]
  (try
    (case target-type
      :string (str value)
      :integer (if (string? value) (Long/parseLong value) (long value))
      :float (if (string? value) (Double/parseDouble value) (double value))
      :boolean (if (string? value)
                 (Boolean/parseBoolean value)
                 (boolean value))
      :keyword (if (string? value) (keyword value) value)
      :symbol (if (string? value) (symbol value) value)
      value)
    (catch Exception _
      value)))

(defn coerce
  "Coerce data to match a schema."
  [schema-id data]
  (let [schema (get-schema schema-id)]
    (if schema
      (reduce (fn [acc [field-name field-def]]
                (let [value (get data field-name)
                      coerced (if (some? value)
                                (coerce-value value (:type field-def))
                                (:default field-def))]
                  (if (some? coerced)
                    (assoc acc field-name coerced)
                    acc)))
              {}
              (:fields schema))
      data)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-schema-stats
  "Get schema manager statistics."
  []
  {:schemas (.size ^ConcurrentHashMap (:schemas @schema-state))
   :versions (.size ^ConcurrentHashMap (:versions @schema-state))
   :migrations (.size ^ConcurrentHashMap (:migrations @schema-state))
   :validators (count (:validators @schema-state))
   :schema-count (.get ^AtomicLong (:schema-count @schema-state))
   :validation-count (.get ^AtomicLong (:validation-count @schema-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-schema-manager!
  "Initialize schema manager."
  []
  (log/info "Initializing schema manager")
  ;; Register feature flag
  (flags/register-flag! "schema-manager" "Enable schema manager" true)
  ;; Create metrics
  (metrics/create-counter! :schemamanager/validations "Schema validations")
  (metrics/create-gauge! :schemamanager/total-schemas "Total schemas"
                         #(.size ^ConcurrentHashMap (:schemas @schema-state)))
  ;; Initialize built-in validators
  (init-built-in-validators!)
  (log/info "Schema manager initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-schema-manager-status []
  {:enabled (flags/is-enabled? "schema-manager")
   :schemas (.size ^ConcurrentHashMap (:schemas @schema-state))
   :versions (.size ^ConcurrentHashMap (:versions @schema-state))
   :migrations (.size ^ConcurrentHashMap (:migrations @schema-state))
   :validators (count (:validators @schema-state))
   :stats (get-schema-stats)
   :config (:config @schema-state)})
