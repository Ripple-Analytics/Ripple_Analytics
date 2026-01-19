(ns mental-models.pipeline.integration.config-validator
  "Configuration Validator Module
   
   Configuration validation and schema enforcement:
   - Schema definition and registration
   - Configuration validation
   - Type coercion
   - Default value handling
   - Validation error reporting"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.regex Pattern]))

;; =============================================================================
;; CONFIG VALIDATOR STATE
;; =============================================================================

(defonce validator-state (atom {:schemas (ConcurrentHashMap.)
                                :validators (ConcurrentHashMap.)
                                :coercers {}
                                :validation-results []
                                :config {:max-results 1000
                                         :strict-mode false}}))

;; =============================================================================
;; TYPE DEFINITIONS
;; =============================================================================

(def built-in-types
  #{:string :integer :long :float :double :boolean :keyword :symbol
    :map :vector :set :list :any :uuid :instant :uri :regex})

;; =============================================================================
;; TYPE COERCION
;; =============================================================================

(defn coerce-string [v]
  (str v))

(defn coerce-integer [v]
  (cond
    (integer? v) (int v)
    (string? v) (Integer/parseInt v)
    (number? v) (int v)
    :else (throw (ex-info "Cannot coerce to integer" {:value v}))))

(defn coerce-long [v]
  (cond
    (integer? v) (long v)
    (string? v) (Long/parseLong v)
    (number? v) (long v)
    :else (throw (ex-info "Cannot coerce to long" {:value v}))))

(defn coerce-float [v]
  (cond
    (float? v) v
    (string? v) (Float/parseFloat v)
    (number? v) (float v)
    :else (throw (ex-info "Cannot coerce to float" {:value v}))))

(defn coerce-double [v]
  (cond
    (double? v) v
    (string? v) (Double/parseDouble v)
    (number? v) (double v)
    :else (throw (ex-info "Cannot coerce to double" {:value v}))))

(defn coerce-boolean [v]
  (cond
    (boolean? v) v
    (string? v) (case (str/lower-case v)
                  ("true" "yes" "1" "on") true
                  ("false" "no" "0" "off") false
                  (throw (ex-info "Cannot coerce to boolean" {:value v})))
    (number? v) (not (zero? v))
    :else (throw (ex-info "Cannot coerce to boolean" {:value v}))))

(defn coerce-keyword [v]
  (cond
    (keyword? v) v
    (string? v) (keyword v)
    (symbol? v) (keyword v)
    :else (throw (ex-info "Cannot coerce to keyword" {:value v}))))

(defn coerce-symbol [v]
  (cond
    (symbol? v) v
    (string? v) (symbol v)
    (keyword? v) (symbol (name v))
    :else (throw (ex-info "Cannot coerce to symbol" {:value v}))))

(def default-coercers
  {:string coerce-string
   :integer coerce-integer
   :long coerce-long
   :float coerce-float
   :double coerce-double
   :boolean coerce-boolean
   :keyword coerce-keyword
   :symbol coerce-symbol})

(defn register-coercer!
  "Register a custom type coercer."
  [type-name coerce-fn]
  (swap! validator-state assoc-in [:coercers type-name] coerce-fn))

(defn get-coercer
  "Get a coercer for a type."
  [type-name]
  (or (get-in @validator-state [:coercers type-name])
      (get default-coercers type-name)))

(defn coerce-value
  "Coerce a value to a type."
  [value type-name]
  (if-let [coercer (get-coercer type-name)]
    (coercer value)
    value))

;; =============================================================================
;; SCHEMA DEFINITION
;; =============================================================================

(defn create-field
  "Create a field definition."
  [{:keys [name type required default description validators enum min max pattern]}]
  {:name name
   :type type
   :required (boolean required)
   :default default
   :description description
   :validators (or validators [])
   :enum enum
   :min min
   :max max
   :pattern (when pattern (if (string? pattern) (Pattern/compile pattern) pattern))})

(defn create-schema
  "Create a configuration schema."
  [schema-id {:keys [fields description strict]}]
  {:id schema-id
   :description description
   :strict (boolean strict)
   :fields (into {} (map (fn [f] [(:name f) (create-field f)]) fields))
   :created-at (System/currentTimeMillis)})

(defn register-schema!
  "Register a configuration schema."
  [schema-id fields & {:keys [description strict]}]
  (log/info "Registering schema" {:id schema-id})
  (let [schema (create-schema schema-id {:fields fields
                                         :description description
                                         :strict strict})]
    (.put ^ConcurrentHashMap (:schemas @validator-state) schema-id schema)
    (metrics/inc-counter! :configvalidator/schemas-registered)
    (events/publish! :configvalidator/schema-registered {:schema-id schema-id})
    schema-id))

(defn unregister-schema!
  "Unregister a schema."
  [schema-id]
  (log/info "Unregistering schema" {:id schema-id})
  (.remove ^ConcurrentHashMap (:schemas @validator-state) schema-id))

(defn get-schema
  "Get a schema by ID."
  [schema-id]
  (.get ^ConcurrentHashMap (:schemas @validator-state) schema-id))

(defn list-schemas
  "List all registered schemas."
  []
  (vec (keys (:schemas @validator-state))))

;; =============================================================================
;; CUSTOM VALIDATORS
;; =============================================================================

(defn register-validator!
  "Register a custom validator."
  [validator-id validator-fn & {:keys [description]}]
  (log/info "Registering validator" {:id validator-id})
  (.put ^ConcurrentHashMap (:validators @validator-state) validator-id
        {:fn validator-fn
         :description description}))

(defn unregister-validator!
  "Unregister a validator."
  [validator-id]
  (.remove ^ConcurrentHashMap (:validators @validator-state) validator-id))

(defn get-validator
  "Get a validator by ID."
  [validator-id]
  (.get ^ConcurrentHashMap (:validators @validator-state) validator-id))

;; =============================================================================
;; VALIDATION
;; =============================================================================

(defn validate-type
  "Validate a value against a type."
  [value type-name]
  (case type-name
    :string (string? value)
    :integer (integer? value)
    :long (integer? value)
    :float (float? value)
    :double (or (float? value) (double? value))
    :boolean (boolean? value)
    :keyword (keyword? value)
    :symbol (symbol? value)
    :map (map? value)
    :vector (vector? value)
    :set (set? value)
    :list (or (list? value) (seq? value))
    :any true
    :uuid (uuid? value)
    :instant (instance? java.time.Instant value)
    :uri (instance? java.net.URI value)
    :regex (instance? Pattern value)
    ;; Custom type - check if it's a nested schema
    (if-let [schema (get-schema type-name)]
      (empty? (:errors (validate-config value type-name)))
      true)))

(defn validate-enum
  "Validate a value against an enum."
  [value enum]
  (contains? (set enum) value))

(defn validate-min
  "Validate a value against a minimum."
  [value min-val]
  (cond
    (number? value) (>= value min-val)
    (string? value) (>= (count value) min-val)
    (coll? value) (>= (count value) min-val)
    :else true))

(defn validate-max
  "Validate a value against a maximum."
  [value max-val]
  (cond
    (number? value) (<= value max-val)
    (string? value) (<= (count value) max-val)
    (coll? value) (<= (count value) max-val)
    :else true))

(defn validate-pattern
  "Validate a value against a regex pattern."
  [value pattern]
  (when (and (string? value) pattern)
    (boolean (re-matches pattern value))))

(defn validate-field
  "Validate a single field."
  [field-name field-def value config]
  (let [errors (atom [])]
    ;; Check required
    (when (and (:required field-def) (nil? value))
      (swap! errors conj {:field field-name
                          :error :required
                          :message (str "Field '" (name field-name) "' is required")}))
    ;; If value exists, validate it
    (when (some? value)
      ;; Type validation
      (when-not (validate-type value (:type field-def))
        (swap! errors conj {:field field-name
                            :error :type
                            :expected (:type field-def)
                            :actual (type value)
                            :message (str "Field '" (name field-name) "' expected type " (:type field-def))}))
      ;; Enum validation
      (when (and (:enum field-def) (not (validate-enum value (:enum field-def))))
        (swap! errors conj {:field field-name
                            :error :enum
                            :expected (:enum field-def)
                            :actual value
                            :message (str "Field '" (name field-name) "' must be one of " (:enum field-def))}))
      ;; Min validation
      (when (and (:min field-def) (not (validate-min value (:min field-def))))
        (swap! errors conj {:field field-name
                            :error :min
                            :min (:min field-def)
                            :actual value
                            :message (str "Field '" (name field-name) "' must be >= " (:min field-def))}))
      ;; Max validation
      (when (and (:max field-def) (not (validate-max value (:max field-def))))
        (swap! errors conj {:field field-name
                            :error :max
                            :max (:max field-def)
                            :actual value
                            :message (str "Field '" (name field-name) "' must be <= " (:max field-def))}))
      ;; Pattern validation
      (when (and (:pattern field-def) (not (validate-pattern value (:pattern field-def))))
        (swap! errors conj {:field field-name
                            :error :pattern
                            :pattern (str (:pattern field-def))
                            :actual value
                            :message (str "Field '" (name field-name) "' must match pattern " (:pattern field-def))}))
      ;; Custom validators
      (doseq [validator-id (:validators field-def)]
        (when-let [validator (get-validator validator-id)]
          (when-not ((:fn validator) value config)
            (swap! errors conj {:field field-name
                                :error :custom
                                :validator validator-id
                                :message (str "Field '" (name field-name) "' failed custom validation: " validator-id)})))))
    @errors))

(defn validate-config
  "Validate a configuration against a schema."
  [config schema-id & {:keys [coerce]}]
  (log/debug "Validating config" {:schema schema-id})
  (if-let [schema (get-schema schema-id)]
    (let [errors (atom [])
          coerced-config (atom config)
          fields (:fields schema)]
      ;; Validate each field
      (doseq [[field-name field-def] fields]
        (let [value (get config field-name)
              ;; Apply default if missing
              value-with-default (if (and (nil? value) (contains? field-def :default))
                                   (:default field-def)
                                   value)
              ;; Coerce if requested
              final-value (if (and coerce (some? value-with-default))
                            (try
                              (coerce-value value-with-default (:type field-def))
                              (catch Exception e
                                (swap! errors conj {:field field-name
                                                    :error :coercion
                                                    :message (.getMessage e)})
                                value-with-default))
                            value-with-default)]
          ;; Update coerced config
          (when (some? final-value)
            (swap! coerced-config assoc field-name final-value))
          ;; Validate field
          (let [field-errors (validate-field field-name field-def final-value config)]
            (swap! errors into field-errors))))
      ;; Check for unknown fields in strict mode
      (when (:strict schema)
        (let [known-fields (set (keys fields))
              config-fields (set (keys config))
              unknown-fields (set/difference config-fields known-fields)]
          (doseq [field unknown-fields]
            (swap! errors conj {:field field
                                :error :unknown
                                :message (str "Unknown field '" (name field) "' in strict mode")}))))
      ;; Record result
      (let [result {:schema-id schema-id
                    :valid (empty? @errors)
                    :errors @errors
                    :coerced-config (when coerce @coerced-config)
                    :timestamp (System/currentTimeMillis)}]
        (record-validation-result! result)
        (metrics/inc-counter! :configvalidator/validations-performed)
        (when (seq @errors)
          (metrics/inc-counter! :configvalidator/validation-failures))
        result))
    {:schema-id schema-id
     :valid false
     :errors [{:error :schema-not-found
               :message (str "Schema '" schema-id "' not found")}]}))

(defn valid?
  "Check if a configuration is valid."
  [config schema-id]
  (:valid (validate-config config schema-id)))

;; =============================================================================
;; VALIDATION RESULTS
;; =============================================================================

(defn record-validation-result!
  "Record a validation result."
  [result]
  (let [max-results (get-in @validator-state [:config :max-results])]
    (swap! validator-state update :validation-results
           (fn [results]
             (let [new-results (conj results result)]
               (if (> (count new-results) max-results)
                 (vec (drop (- (count new-results) max-results) new-results))
                 new-results))))))

(defn get-validation-results
  "Get validation results."
  [& {:keys [schema-id valid-only invalid-only limit since]}]
  (let [results (:validation-results @validator-state)]
    (cond->> results
      schema-id (filter #(= (:schema-id %) schema-id))
      valid-only (filter :valid)
      invalid-only (filter (complement :valid))
      since (filter #(>= (:timestamp %) since))
      limit (take-last limit))))

(defn clear-validation-results!
  "Clear validation results."
  []
  (swap! validator-state assoc :validation-results []))

;; =============================================================================
;; SCHEMA UTILITIES
;; =============================================================================

(defn apply-defaults
  "Apply default values to a configuration."
  [config schema-id]
  (if-let [schema (get-schema schema-id)]
    (reduce (fn [cfg [field-name field-def]]
              (if (and (nil? (get cfg field-name))
                       (contains? field-def :default))
                (assoc cfg field-name (:default field-def))
                cfg))
            config
            (:fields schema))
    config))

(defn coerce-config
  "Coerce a configuration to match schema types."
  [config schema-id]
  (if-let [schema (get-schema schema-id)]
    (reduce (fn [cfg [field-name field-def]]
              (if-let [value (get cfg field-name)]
                (try
                  (assoc cfg field-name (coerce-value value (:type field-def)))
                  (catch Exception _
                    cfg))
                cfg))
            config
            (:fields schema))
    config))

(defn get-required-fields
  "Get required fields from a schema."
  [schema-id]
  (when-let [schema (get-schema schema-id)]
    (vec (for [[field-name field-def] (:fields schema)
               :when (:required field-def)]
           field-name))))

(defn get-optional-fields
  "Get optional fields from a schema."
  [schema-id]
  (when-let [schema (get-schema schema-id)]
    (vec (for [[field-name field-def] (:fields schema)
               :when (not (:required field-def))]
           field-name))))

(defn describe-schema
  "Get a human-readable description of a schema."
  [schema-id]
  (when-let [schema (get-schema schema-id)]
    {:id schema-id
     :description (:description schema)
     :strict (:strict schema)
     :fields (into {} (for [[field-name field-def] (:fields schema)]
                        [field-name {:type (:type field-def)
                                     :required (:required field-def)
                                     :default (:default field-def)
                                     :description (:description field-def)
                                     :enum (:enum field-def)
                                     :min (:min field-def)
                                     :max (:max field-def)
                                     :pattern (when (:pattern field-def) (str (:pattern field-def)))}]))}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-validator-stats
  "Get validation statistics."
  []
  (let [results (:validation-results @validator-state)
        valid-count (count (filter :valid results))
        invalid-count (count (filter (complement :valid) results))]
    {:total-validations (count results)
     :valid-count valid-count
     :invalid-count invalid-count
     :success-rate (if (pos? (count results))
                     (double (/ valid-count (count results)))
                     1.0)
     :schemas-registered (.size ^ConcurrentHashMap (:schemas @validator-state))
     :validators-registered (.size ^ConcurrentHashMap (:validators @validator-state))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-config-validator!
  "Initialize configuration validator."
  []
  (log/info "Initializing configuration validator")
  ;; Register feature flag
  (flags/register-flag! "config-validator" "Enable configuration validator" true)
  ;; Create metrics
  (metrics/create-counter! :configvalidator/schemas-registered "Schemas registered")
  (metrics/create-counter! :configvalidator/validations-performed "Validations performed")
  (metrics/create-counter! :configvalidator/validation-failures "Validation failures")
  (metrics/create-gauge! :configvalidator/total-schemas "Total schemas"
                         #(.size ^ConcurrentHashMap (:schemas @validator-state)))
  ;; Register built-in validators
  (register-validator! :not-empty (fn [v _] (and (some? v) (if (coll? v) (seq v) true)))
                       :description "Value must not be empty")
  (register-validator! :positive (fn [v _] (and (number? v) (pos? v)))
                       :description "Value must be positive")
  (register-validator! :non-negative (fn [v _] (and (number? v) (>= v 0)))
                       :description "Value must be non-negative")
  (register-validator! :email (fn [v _] (and (string? v) (re-matches #".+@.+\..+" v)))
                       :description "Value must be a valid email")
  (register-validator! :url (fn [v _] (and (string? v) (re-matches #"https?://.*" v)))
                       :description "Value must be a valid URL")
  (log/info "Configuration validator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-config-validator-status []
  {:enabled (flags/is-enabled? "config-validator")
   :schemas (.size ^ConcurrentHashMap (:schemas @validator-state))
   :validators (.size ^ConcurrentHashMap (:validators @validator-state))
   :validation-results (count (:validation-results @validator-state))
   :stats (get-validator-stats)
   :config (:config @validator-state)})
