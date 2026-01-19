(ns mental-models.pipeline.integration.response-validator
  "Response validator for mental model analysis system.
   
   Features:
   - Schema validation
   - Type validation
   - Range validation
   - Format validation
   - Custom validators
   - Validation rules
   - Validation errors
   - Validation metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:schemas {}          ;; schema-id -> schema
         :validators {}       ;; validator-id -> validator
         :config {:strict-mode? false
                  :coerce-types? true
                  :include-errors? true
                  :max-errors 10}
         :stats {:validations 0
                 :successes 0
                 :failures 0
                 :errors-by-type {}}
         :initialized? false}))

;; ============================================================================
;; Built-in Validators
;; ============================================================================

(defn- validate-type
  "Validate that a value is of the expected type."
  [value expected-type]
  (case expected-type
    :string (string? value)
    :number (number? value)
    :integer (integer? value)
    :boolean (boolean? value)
    :map (map? value)
    :vector (vector? value)
    :list (list? value)
    :sequential (sequential? value)
    :keyword (keyword? value)
    :uuid (or (uuid? value) (and (string? value) (try (UUID/fromString value) true (catch Exception _ false))))
    :any true
    (instance? expected-type value)))

(defn- validate-range
  "Validate that a value is within a range."
  [value min-val max-val]
  (and (or (nil? min-val) (>= value min-val))
       (or (nil? max-val) (<= value max-val))))

(defn- validate-length
  "Validate the length of a value."
  [value min-len max-len]
  (let [len (cond
              (string? value) (count value)
              (sequential? value) (count value)
              (map? value) (count value)
              :else 0)]
    (and (or (nil? min-len) (>= len min-len))
         (or (nil? max-len) (<= len max-len)))))

(defn- validate-pattern
  "Validate that a string matches a pattern."
  [value pattern]
  (when (string? value)
    (re-matches (if (string? pattern) (re-pattern pattern) pattern) value)))

(defn- validate-enum
  "Validate that a value is in an enumeration."
  [value allowed-values]
  (contains? (set allowed-values) value))

(defn- validate-required
  "Validate that a value is present."
  [value]
  (not (nil? value)))

;; ============================================================================
;; Schema Definition
;; ============================================================================

(defn register-schema!
  "Register a validation schema."
  [schema-id schema]
  (let [schema-def {:id schema-id
                    :name (get schema :name (name schema-id))
                    :fields (get schema :fields {})
                    :strict? (get schema :strict? false)
                    :enabled? (atom true)
                    :metrics {:validations (atom 0)
                              :failures (atom 0)}
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:schemas schema-id] schema-def)
    (logging/log :info "Registered schema" {:schema-id schema-id})
    schema-id))

(defn get-schema
  "Get a schema by ID."
  [schema-id]
  (get-in @state [:schemas schema-id]))

(defn list-schemas
  "List all schemas."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :fields-count (count (:fields s))
           :enabled? @(:enabled? s)})
        (:schemas @state)))

(defn delete-schema!
  "Delete a schema."
  [schema-id]
  (swap! state update :schemas dissoc schema-id))

;; ============================================================================
;; Custom Validators
;; ============================================================================

(defn register-validator!
  "Register a custom validator."
  [validator-id config]
  (let [validator {:id validator-id
                   :name (get config :name (name validator-id))
                   :validate-fn (get config :validate-fn)
                   :error-message (get config :error-message "Validation failed")
                   :enabled? (atom true)
                   :metrics {:invocations (atom 0)
                             :failures (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:validators validator-id] validator)
    (logging/log :info "Registered validator" {:validator-id validator-id})
    validator-id))

(defn get-validator
  "Get a validator by ID."
  [validator-id]
  (get-in @state [:validators validator-id]))

(defn list-validators
  "List all validators."
  []
  (mapv (fn [[id v]]
          {:id id
           :name (:name v)
           :enabled? @(:enabled? v)})
        (:validators @state)))

;; ============================================================================
;; Field Validation
;; ============================================================================

(defn- validate-field
  "Validate a single field."
  [field-name value field-spec]
  (let [errors (atom [])]
    ;; Required check
    (when (and (:required? field-spec) (not (validate-required value)))
      (swap! errors conj {:field field-name :error :required :message "Field is required"}))
    
    (when (some? value)
      ;; Type check
      (when-let [expected-type (:type field-spec)]
        (when-not (validate-type value expected-type)
          (swap! errors conj {:field field-name :error :type
                              :message (str "Expected type " expected-type)})))
      
      ;; Range check
      (when (or (:min field-spec) (:max field-spec))
        (when-not (validate-range value (:min field-spec) (:max field-spec))
          (swap! errors conj {:field field-name :error :range
                              :message (str "Value out of range")})))
      
      ;; Length check
      (when (or (:min-length field-spec) (:max-length field-spec))
        (when-not (validate-length value (:min-length field-spec) (:max-length field-spec))
          (swap! errors conj {:field field-name :error :length
                              :message "Length out of range"})))
      
      ;; Pattern check
      (when-let [pattern (:pattern field-spec)]
        (when-not (validate-pattern value pattern)
          (swap! errors conj {:field field-name :error :pattern
                              :message "Value does not match pattern"})))
      
      ;; Enum check
      (when-let [allowed (:enum field-spec)]
        (when-not (validate-enum value allowed)
          (swap! errors conj {:field field-name :error :enum
                              :message (str "Value must be one of " allowed)})))
      
      ;; Custom validator
      (when-let [validator-id (:validator field-spec)]
        (when-let [validator (get-validator validator-id)]
          (swap! (get-in validator [:metrics :invocations]) inc)
          (when-not ((:validate-fn validator) value)
            (swap! (get-in validator [:metrics :failures]) inc)
            (swap! errors conj {:field field-name :error :custom
                                :message (:error-message validator)})))))
    
    @errors))

;; ============================================================================
;; Response Validation
;; ============================================================================

(defn validate-against-schema
  "Validate data against a schema."
  [data schema-id]
  (swap! state update-in [:stats :validations] inc)
  
  (if-let [schema (get-schema schema-id)]
    (do
      (swap! (get-in schema [:metrics :validations]) inc)
      (let [fields (:fields schema)
            errors (atom [])
            max-errors (get-in @state [:config :max-errors])]
        
        ;; Validate each field
        (doseq [[field-name field-spec] fields]
          (when (< (count @errors) max-errors)
            (let [value (get data field-name)
                  field-errors (validate-field field-name value field-spec)]
              (swap! errors concat field-errors))))
        
        ;; Check for unknown fields in strict mode
        (when (:strict? schema)
          (let [known-fields (set (keys fields))
                data-fields (set (keys data))
                unknown-fields (clojure.set/difference data-fields known-fields)]
            (doseq [field unknown-fields]
              (when (< (count @errors) max-errors)
                (swap! errors conj {:field field :error :unknown
                                    :message "Unknown field"})))))
        
        (let [error-list (vec @errors)]
          (if (empty? error-list)
            (do
              (swap! state update-in [:stats :successes] inc)
              {:valid? true :data data})
            (do
              (swap! state update-in [:stats :failures] inc)
              (swap! (get-in schema [:metrics :failures]) inc)
              (doseq [err error-list]
                (swap! state update-in [:stats :errors-by-type (:error err)] (fnil inc 0)))
              {:valid? false :errors error-list})))))
    {:valid? false :errors [{:error :schema-not-found :message "Schema not found"}]}))

(defn validate-response
  "Validate a response body."
  [response schema-id]
  (let [body (:body response)]
    (if (map? body)
      (validate-against-schema body schema-id)
      {:valid? false :errors [{:error :invalid-body :message "Response body must be a map"}]})))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-validate-response
  "Ring middleware to validate responses."
  [handler schema-id]
  (fn [request]
    (let [response (handler request)
          validation (validate-response response schema-id)]
      (if (:valid? validation)
        response
        (if (get-in @state [:config :strict-mode?])
          {:status 500
           :body {:error "Response validation failed"
                  :details (:errors validation)}}
          (do
            (logging/log :warn "Response validation failed" {:errors (:errors validation)})
            response))))))

(defn wrap-validate-request
  "Ring middleware to validate request bodies."
  [handler schema-id]
  (fn [request]
    (let [body (:body request)]
      (if (map? body)
        (let [validation (validate-against-schema body schema-id)]
          (if (:valid? validation)
            (handler request)
            {:status 400
             :body {:error "Request validation failed"
                    :details (:errors validation)}}))
        (handler request)))))

;; ============================================================================
;; Spec Integration
;; ============================================================================

(defn validate-with-spec
  "Validate data using clojure.spec."
  [data spec]
  (if (s/valid? spec data)
    {:valid? true :data data}
    {:valid? false
     :errors [{:error :spec-failure
               :message (s/explain-str spec data)}]}))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-strict-mode!
  "Enable/disable strict mode."
  [enabled?]
  (swap! state assoc-in [:config :strict-mode?] enabled?))

(defn set-coerce-types!
  "Enable/disable type coercion."
  [enabled?]
  (swap! state assoc-in [:config :coerce-types?] enabled?))

(defn set-max-errors!
  "Set the maximum number of errors to collect."
  [max-errors]
  (swap! state assoc-in [:config :max-errors] max-errors))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-validator-metrics
  "Get validator metrics."
  []
  (let [stats (:stats @state)]
    {:validations (:validations stats)
     :successes (:successes stats)
     :failures (:failures stats)
     :success-rate (if (pos? (:validations stats))
                     (/ (:successes stats) (:validations stats))
                     1.0)
     :errors-by-type (:errors-by-type stats)
     :schemas-count (count (:schemas @state))
     :validators-count (count (:validators @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-validator-stats
  "Get validator statistics."
  []
  (merge (get-validator-metrics)
         {:strict-mode? (get-in @state [:config :strict-mode?])
          :coerce-types? (get-in @state [:config :coerce-types?])}))

(defn reset-stats!
  "Reset validator statistics."
  []
  (swap! state assoc :stats {:validations 0
                             :successes 0
                             :failures 0
                             :errors-by-type {}}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-validator!
  "Initialize the response validator."
  []
  (when-not (:initialized? @state)
    ;; Register common validators
    (register-validator! :email
                         {:name "Email Validator"
                          :validate-fn #(and (string? %)
                                             (re-matches #"^[^\s@]+@[^\s@]+\.[^\s@]+$" %))
                          :error-message "Invalid email format"})
    
    (register-validator! :url
                         {:name "URL Validator"
                          :validate-fn #(and (string? %)
                                             (re-matches #"^https?://.*" %))
                          :error-message "Invalid URL format"})
    
    (register-validator! :uuid
                         {:name "UUID Validator"
                          :validate-fn #(try (UUID/fromString (str %)) true (catch Exception _ false))
                          :error-message "Invalid UUID format"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response validator initialized")
    (events/emit! :response-validator-initialized {})
    true))
