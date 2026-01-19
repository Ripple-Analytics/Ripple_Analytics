(ns mental-models.pipeline.integration.request-validator
  "Request Validator Module
   
   Input validation and sanitization:
   - Schema-based validation
   - Type coercion
   - Custom validators
   - Error message formatting
   - Sanitization rules"
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; VALIDATOR STATE
;; =============================================================================

(defonce validator-state (atom {:schemas {}
                                :custom-validators {}
                                :sanitizers {}
                                :error-messages {}}))

;; =============================================================================
;; BUILT-IN VALIDATORS
;; =============================================================================

(defn valid-string?
  "Validate a string with optional constraints."
  [value & {:keys [min-length max-length pattern]}]
  (and (string? value)
       (or (nil? min-length) (>= (count value) min-length))
       (or (nil? max-length) (<= (count value) max-length))
       (or (nil? pattern) (re-matches (re-pattern pattern) value))))

(defn valid-number?
  "Validate a number with optional constraints."
  [value & {:keys [min max integer?]}]
  (and (number? value)
       (or (nil? min) (>= value min))
       (or (nil? max) (<= value max))
       (or (not integer?) (integer? value))))

(defn valid-email?
  "Validate an email address."
  [value]
  (and (string? value)
       (re-matches #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$" value)))

(defn valid-url?
  "Validate a URL."
  [value]
  (and (string? value)
       (re-matches #"^https?://[^\s/$.?#].[^\s]*$" value)))

(defn valid-uuid?
  "Validate a UUID string."
  [value]
  (and (string? value)
       (re-matches #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
                   (str/lower-case value))))

(defn valid-date?
  "Validate a date string (ISO 8601)."
  [value]
  (and (string? value)
       (re-matches #"^\d{4}-\d{2}-\d{2}$" value)))

(defn valid-datetime?
  "Validate a datetime string (ISO 8601)."
  [value]
  (and (string? value)
       (re-matches #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}" value)))

(defn valid-enum?
  "Validate a value is in an enum set."
  [value allowed-values]
  (contains? (set allowed-values) value))

(defn valid-array?
  "Validate an array with optional constraints."
  [value & {:keys [min-items max-items item-validator]}]
  (and (sequential? value)
       (or (nil? min-items) (>= (count value) min-items))
       (or (nil? max-items) (<= (count value) max-items))
       (or (nil? item-validator) (every? item-validator value))))

;; =============================================================================
;; SCHEMA DEFINITION
;; =============================================================================

(defn define-schema!
  "Define a validation schema."
  [schema-id schema]
  (log/info "Defining schema" {:id schema-id})
  (swap! validator-state assoc-in [:schemas schema-id] schema)
  schema-id)

(defn get-schema
  "Get a schema by ID."
  [schema-id]
  (get-in @validator-state [:schemas schema-id]))

(defn list-schemas
  "List all defined schemas."
  []
  (keys (:schemas @validator-state)))

;; =============================================================================
;; CUSTOM VALIDATORS
;; =============================================================================

(defn register-validator!
  "Register a custom validator function."
  [validator-id validator-fn]
  (log/info "Registering validator" {:id validator-id})
  (swap! validator-state assoc-in [:custom-validators validator-id] validator-fn))

(defn get-validator
  "Get a custom validator by ID."
  [validator-id]
  (get-in @validator-state [:custom-validators validator-id]))

;; =============================================================================
;; ERROR MESSAGES
;; =============================================================================

(defn set-error-message!
  "Set a custom error message for a validation type."
  [error-type message]
  (swap! validator-state assoc-in [:error-messages error-type] message))

(defn get-error-message
  "Get an error message for a validation type."
  [error-type field & {:keys [params]}]
  (let [template (get-in @validator-state [:error-messages error-type]
                         (str "Validation failed for " (name field)))]
    (reduce (fn [msg [k v]]
              (str/replace msg (str "{{" (name k) "}}") (str v)))
            template
            (merge {:field (name field)} params))))

;; =============================================================================
;; FIELD VALIDATION
;; =============================================================================

(defn validate-field
  "Validate a single field against its schema."
  [field-name value field-schema]
  (let [{:keys [type required validators]} field-schema
        errors (atom [])]
    ;; Check required
    (when (and required (nil? value))
      (swap! errors conj {:field field-name
                          :type :required
                          :message (get-error-message :required field-name)}))
    ;; Skip further validation if nil and not required
    (when (some? value)
      ;; Type validation
      (case type
        :string (when-not (valid-string? value
                                         :min-length (:min-length field-schema)
                                         :max-length (:max-length field-schema)
                                         :pattern (:pattern field-schema))
                  (swap! errors conj {:field field-name
                                      :type :string
                                      :message (get-error-message :string field-name)}))
        :number (when-not (valid-number? value
                                         :min (:min field-schema)
                                         :max (:max field-schema)
                                         :integer? (:integer? field-schema))
                  (swap! errors conj {:field field-name
                                      :type :number
                                      :message (get-error-message :number field-name)}))
        :email (when-not (valid-email? value)
                 (swap! errors conj {:field field-name
                                     :type :email
                                     :message (get-error-message :email field-name)}))
        :url (when-not (valid-url? value)
               (swap! errors conj {:field field-name
                                   :type :url
                                   :message (get-error-message :url field-name)}))
        :uuid (when-not (valid-uuid? value)
                (swap! errors conj {:field field-name
                                    :type :uuid
                                    :message (get-error-message :uuid field-name)}))
        :date (when-not (valid-date? value)
                (swap! errors conj {:field field-name
                                    :type :date
                                    :message (get-error-message :date field-name)}))
        :datetime (when-not (valid-datetime? value)
                    (swap! errors conj {:field field-name
                                        :type :datetime
                                        :message (get-error-message :datetime field-name)}))
        :enum (when-not (valid-enum? value (:values field-schema))
                (swap! errors conj {:field field-name
                                    :type :enum
                                    :message (get-error-message :enum field-name
                                                                :params {:values (:values field-schema)})}))
        :array (when-not (valid-array? value
                                       :min-items (:min-items field-schema)
                                       :max-items (:max-items field-schema))
                 (swap! errors conj {:field field-name
                                     :type :array
                                     :message (get-error-message :array field-name)}))
        :boolean (when-not (boolean? value)
                   (swap! errors conj {:field field-name
                                       :type :boolean
                                       :message (get-error-message :boolean field-name)}))
        :map (when-not (map? value)
               (swap! errors conj {:field field-name
                                   :type :map
                                   :message (get-error-message :map field-name)}))
        nil)
      ;; Custom validators
      (doseq [validator-id validators]
        (when-let [validator-fn (get-validator validator-id)]
          (when-not (validator-fn value)
            (swap! errors conj {:field field-name
                                :type validator-id
                                :message (get-error-message validator-id field-name)})))))
    @errors))

;; =============================================================================
;; SCHEMA VALIDATION
;; =============================================================================

(defn validate
  "Validate data against a schema."
  [schema-id data]
  (when (flags/is-enabled? "request-validator")
    (metrics/inc-counter! :validator/validations)
    (if-let [schema (get-schema schema-id)]
      (let [errors (atom [])]
        (doseq [[field-name field-schema] (:fields schema)]
          (let [value (get data field-name)
                field-errors (validate-field field-name value field-schema)]
            (when (seq field-errors)
              (swap! errors into field-errors))))
        (let [result {:valid? (empty? @errors)
                      :errors @errors}]
          (when-not (:valid? result)
            (metrics/inc-counter! :validator/validation-failures)
            (log/debug "Validation failed" {:schema schema-id :errors @errors}))
          result))
      (do
        (log/warn "Schema not found" {:schema schema-id})
        {:valid? false :errors [{:type :schema-not-found :message "Schema not found"}]}))))

(defn validate-or-throw!
  "Validate data and throw if invalid."
  [schema-id data]
  (let [result (validate schema-id data)]
    (when-not (:valid? result)
      (throw (ex-info "Validation failed"
                      {:type :validation-error
                       :schema schema-id
                       :errors (:errors result)})))
    data))

;; =============================================================================
;; SANITIZATION
;; =============================================================================

(defn register-sanitizer!
  "Register a sanitizer function."
  [sanitizer-id sanitizer-fn]
  (swap! validator-state assoc-in [:sanitizers sanitizer-id] sanitizer-fn))

(defn sanitize-string
  "Sanitize a string value."
  [value & {:keys [trim? lowercase? uppercase? max-length strip-html?]}]
  (cond-> value
    trim? str/trim
    lowercase? str/lower-case
    uppercase? str/upper-case
    max-length (subs 0 (min (count value) max-length))
    strip-html? (str/replace #"<[^>]*>" "")))

(defn sanitize
  "Sanitize data according to schema."
  [schema-id data]
  (if-let [schema (get-schema schema-id)]
    (reduce (fn [result [field-name field-schema]]
              (if-let [value (get data field-name)]
                (let [sanitizers (:sanitizers field-schema)]
                  (assoc result field-name
                         (reduce (fn [v sanitizer-id]
                                   (if-let [sanitizer-fn (get-in @validator-state [:sanitizers sanitizer-id])]
                                     (sanitizer-fn v)
                                     v))
                                 value
                                 sanitizers)))
                result))
            data
            (:fields schema))
    data))

;; =============================================================================
;; DEFAULT SCHEMAS
;; =============================================================================

(def analysis-request-schema
  {:fields {:document-id {:type :uuid :required true}
            :text {:type :string :required true :min-length 1 :max-length 100000}
            :options {:type :map :required false}}})

(def user-schema
  {:fields {:email {:type :email :required true}
            :name {:type :string :required true :min-length 1 :max-length 100}
            :role {:type :enum :required false :values [:viewer :analyst :editor :admin]}}})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-request-validator!
  "Initialize request validator."
  []
  (log/info "Initializing request validator")
  ;; Register feature flag
  (flags/register-flag! "request-validator" "Enable request validation" true)
  ;; Create metrics
  (metrics/create-counter! :validator/validations "Validations performed")
  (metrics/create-counter! :validator/validation-failures "Validation failures")
  ;; Set default error messages
  (set-error-message! :required "{{field}} is required")
  (set-error-message! :string "{{field}} must be a valid string")
  (set-error-message! :number "{{field}} must be a valid number")
  (set-error-message! :email "{{field}} must be a valid email address")
  (set-error-message! :url "{{field}} must be a valid URL")
  (set-error-message! :uuid "{{field}} must be a valid UUID")
  (set-error-message! :date "{{field}} must be a valid date (YYYY-MM-DD)")
  (set-error-message! :datetime "{{field}} must be a valid datetime")
  (set-error-message! :enum "{{field}} must be one of the allowed values")
  (set-error-message! :array "{{field}} must be a valid array")
  (set-error-message! :boolean "{{field}} must be a boolean")
  (set-error-message! :map "{{field}} must be a map")
  ;; Register default sanitizers
  (register-sanitizer! :trim str/trim)
  (register-sanitizer! :lowercase str/lower-case)
  (register-sanitizer! :uppercase str/upper-case)
  (register-sanitizer! :strip-html #(str/replace % #"<[^>]*>" ""))
  ;; Register default schemas
  (define-schema! :analysis-request analysis-request-schema)
  (define-schema! :user user-schema)
  (log/info "Request validator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-validator-status []
  {:enabled (flags/is-enabled? "request-validator")
   :schemas (count (:schemas @validator-state))
   :custom-validators (count (:custom-validators @validator-state))
   :sanitizers (count (:sanitizers @validator-state))})
