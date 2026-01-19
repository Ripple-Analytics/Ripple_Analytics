(ns mental-models.pipeline.integration.response-normalizer
  "Response normalizer for mental model analysis system.
   
   Features:
   - Response normalization
   - Schema mapping
   - Field transformation
   - Type coercion
   - Default values
   - Null handling
   - Normalization rules
   - Normalizer metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:schemas {}          ;; schema-id -> schema definition
         :rules {}            ;; rule-id -> normalization rule
         :config {:strict-mode? false
                  :coerce-types? true
                  :remove-nulls? false
                  :trim-strings? true
                  :default-date-format "yyyy-MM-dd"}
         :stats {:normalizations (AtomicLong. 0)
                 :fields-transformed (AtomicLong. 0)
                 :type-coercions (AtomicLong. 0)
                 :defaults-applied (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Type Coercion
;; ============================================================================

(defn coerce-to-string
  "Coerce value to string."
  [value]
  (when (some? value)
    (str value)))

(defn coerce-to-int
  "Coerce value to integer."
  [value]
  (cond
    (nil? value) nil
    (integer? value) value
    (number? value) (int value)
    (string? value) (try (Integer/parseInt value) (catch Exception _ nil))
    :else nil))

(defn coerce-to-long
  "Coerce value to long."
  [value]
  (cond
    (nil? value) nil
    (integer? value) (long value)
    (number? value) (long value)
    (string? value) (try (Long/parseLong value) (catch Exception _ nil))
    :else nil))

(defn coerce-to-double
  "Coerce value to double."
  [value]
  (cond
    (nil? value) nil
    (number? value) (double value)
    (string? value) (try (Double/parseDouble value) (catch Exception _ nil))
    :else nil))

(defn coerce-to-boolean
  "Coerce value to boolean."
  [value]
  (cond
    (nil? value) nil
    (boolean? value) value
    (string? value) (case (str/lower-case value)
                      ("true" "yes" "1" "on") true
                      ("false" "no" "0" "off") false
                      nil)
    (number? value) (not (zero? value))
    :else nil))

(defn coerce-to-date
  "Coerce value to date."
  [value & {:keys [format] :or {format "yyyy-MM-dd"}}]
  (cond
    (nil? value) nil
    (instance? LocalDate value) value
    (instance? LocalDateTime value) (.toLocalDate value)
    (string? value) (try
                      (LocalDate/parse value (DateTimeFormatter/ofPattern format))
                      (catch Exception _ nil))
    (number? value) (LocalDate/ofEpochDay (long value))
    :else nil))

(defn coerce-to-datetime
  "Coerce value to datetime."
  [value & {:keys [format] :or {format "yyyy-MM-dd'T'HH:mm:ss"}}]
  (cond
    (nil? value) nil
    (instance? LocalDateTime value) value
    (instance? LocalDate value) (.atStartOfDay value)
    (string? value) (try
                      (LocalDateTime/parse value (DateTimeFormatter/ofPattern format))
                      (catch Exception _ nil))
    (number? value) (LocalDateTime/ofInstant (Instant/ofEpochMilli (long value))
                                              (java.time.ZoneId/systemDefault))
    :else nil))

(defn coerce-to-list
  "Coerce value to list."
  [value]
  (cond
    (nil? value) nil
    (sequential? value) (vec value)
    (set? value) (vec value)
    (string? value) (str/split value #",")
    :else [value]))

(defn coerce-value
  "Coerce a value to a target type."
  [value target-type & opts]
  (.incrementAndGet (:type-coercions (:stats @state)))
  (case target-type
    :string (coerce-to-string value)
    :int (coerce-to-int value)
    :long (coerce-to-long value)
    :double (coerce-to-double value)
    :boolean (coerce-to-boolean value)
    :date (apply coerce-to-date value opts)
    :datetime (apply coerce-to-datetime value opts)
    :list (coerce-to-list value)
    value))

;; ============================================================================
;; Schema Management
;; ============================================================================

(defn register-schema!
  "Register a normalization schema."
  [schema-id fields]
  (let [schema {:id schema-id
                :fields fields
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schemas schema-id] schema)
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
           :field-count (count (:fields s))
           :created-at (:created-at s)})
        (:schemas @state)))

(defn delete-schema!
  "Delete a schema."
  [schema-id]
  (swap! state update :schemas dissoc schema-id))

;; ============================================================================
;; Field Transformation
;; ============================================================================

(defn transform-field
  "Transform a field according to spec."
  [value spec]
  (.incrementAndGet (:fields-transformed (:stats @state)))
  
  (let [;; Apply type coercion
        coerced (if (and (get-in @state [:config :coerce-types?])
                         (:type spec))
                  (coerce-value value (:type spec))
                  value)
        
        ;; Apply custom transform
        transformed (if-let [transform-fn (:transform spec)]
                      (transform-fn coerced)
                      coerced)
        
        ;; Apply default if nil
        with-default (if (and (nil? transformed) (:default spec))
                       (do
                         (.incrementAndGet (:defaults-applied (:stats @state)))
                         (:default spec))
                       transformed)
        
        ;; Trim strings
        trimmed (if (and (get-in @state [:config :trim-strings?])
                         (string? with-default))
                  (str/trim with-default)
                  with-default)]
    
    trimmed))

(defn rename-field
  "Rename a field."
  [data old-name new-name]
  (if (contains? data old-name)
    (-> data
        (assoc new-name (get data old-name))
        (dissoc old-name))
    data))

(defn remove-field
  "Remove a field."
  [data field-name]
  (dissoc data field-name))

;; ============================================================================
;; Normalization Rules
;; ============================================================================

(defn register-rule!
  "Register a normalization rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :condition-fn (get config :condition-fn (constantly true))
              :normalize-fn (get config :normalize-fn identity)
              :priority (get config :priority 0)
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:rules rule-id] rule)
    rule-id))

(defn get-rule
  "Get a rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :priority (:priority r)
           :enabled? @(:enabled? r)})
        (:rules @state)))

(defn enable-rule!
  "Enable a rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) true)))

(defn disable-rule!
  "Disable a rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) false)))

;; ============================================================================
;; Data Normalization
;; ============================================================================

(defn normalize-with-schema
  "Normalize data according to a schema."
  [data schema-id]
  (when-let [schema (get-schema schema-id)]
    (reduce (fn [result [field-name spec]]
              (let [source-field (or (:source spec) field-name)
                    value (get data source-field)
                    transformed (transform-field value spec)]
                (if (and (nil? transformed)
                         (get-in @state [:config :remove-nulls?]))
                  result
                  (assoc result field-name transformed))))
            {}
            (:fields schema))))

(defn normalize-with-rules
  "Normalize data using registered rules."
  [data]
  (let [rules (->> (vals (:rules @state))
                   (filter #(and @(:enabled? %)
                                 ((:condition-fn %) data)))
                   (sort-by :priority >))]
    (reduce (fn [result rule]
              ((:normalize-fn rule) result))
            data
            rules)))

(defn normalize-map
  "Normalize a map with various transformations."
  [data & {:keys [schema-id rename-map remove-fields transform-map]}]
  (.incrementAndGet (:normalizations (:stats @state)))
  
  (let [;; Apply schema if provided
        with-schema (if schema-id
                      (normalize-with-schema data schema-id)
                      data)
        
        ;; Apply field renames
        with-renames (reduce (fn [d [old-name new-name]]
                               (rename-field d old-name new-name))
                             with-schema
                             (or rename-map {}))
        
        ;; Remove specified fields
        with-removals (reduce remove-field
                              with-renames
                              (or remove-fields []))
        
        ;; Apply custom transforms
        with-transforms (reduce (fn [d [field-name transform-fn]]
                                  (if (contains? d field-name)
                                    (update d field-name transform-fn)
                                    d))
                                with-removals
                                (or transform-map {}))
        
        ;; Apply rules
        with-rules (normalize-with-rules with-transforms)
        
        ;; Remove nulls if configured
        final (if (get-in @state [:config :remove-nulls?])
                (into {} (filter (fn [[_ v]] (some? v)) with-rules))
                with-rules)]
    
    final))

(defn normalize-response
  "Normalize a response body."
  [response & opts]
  (let [body (:body response)]
    (cond
      (map? body)
      (assoc response :body (apply normalize-map body opts))
      
      (sequential? body)
      (assoc response :body (mapv #(apply normalize-map % opts) body))
      
      :else response)))

;; ============================================================================
;; Deep Normalization
;; ============================================================================

(defn normalize-deep
  "Recursively normalize nested data."
  [data & {:keys [key-fn value-fn]
           :or {key-fn identity value-fn identity}}]
  (walk/postwalk
   (fn [x]
     (cond
       (map-entry? x) [(key-fn (key x)) (value-fn (val x))]
       :else x))
   data))

(defn normalize-keys
  "Normalize map keys."
  [data key-fn]
  (normalize-deep data :key-fn key-fn))

(defn kebab-case-keys
  "Convert keys to kebab-case."
  [data]
  (normalize-keys data (fn [k]
                         (-> (name k)
                             (str/replace #"_" "-")
                             (str/replace #"([a-z])([A-Z])" "$1-$2")
                             str/lower-case
                             keyword))))

(defn snake-case-keys
  "Convert keys to snake_case."
  [data]
  (normalize-keys data (fn [k]
                         (-> (name k)
                             (str/replace #"-" "_")
                             (str/replace #"([a-z])([A-Z])" "$1_$2")
                             str/lower-case
                             keyword))))

(defn camel-case-keys
  "Convert keys to camelCase."
  [data]
  (normalize-keys data (fn [k]
                         (let [parts (str/split (name k) #"[-_]")]
                           (keyword (str (first parts)
                                         (apply str (map str/capitalize (rest parts)))))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-normalize
  "Ring middleware to normalize responses."
  [handler & {:keys [schema-id]}]
  (fn [request]
    (let [response (handler request)]
      (normalize-response response :schema-id schema-id))))

(defn wrap-normalize-keys
  "Ring middleware to normalize response keys."
  [handler key-style]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (assoc response :body
             (case key-style
               :kebab (kebab-case-keys body)
               :snake (snake-case-keys body)
               :camel (camel-case-keys body)
               body)))))

(defn wrap-normalize-with-rules
  "Ring middleware to normalize with rules."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (normalize-with-rules body))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-strict-mode!
  "Enable/disable strict mode."
  [strict?]
  (swap! state assoc-in [:config :strict-mode?] strict?))

(defn set-coerce-types!
  "Enable/disable type coercion."
  [coerce?]
  (swap! state assoc-in [:config :coerce-types?] coerce?))

(defn set-remove-nulls!
  "Enable/disable null removal."
  [remove?]
  (swap! state assoc-in [:config :remove-nulls?] remove?))

(defn set-trim-strings!
  "Enable/disable string trimming."
  [trim?]
  (swap! state assoc-in [:config :trim-strings?] trim?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-normalizer-metrics
  "Get normalizer metrics."
  []
  (let [stats (:stats @state)]
    {:normalizations (.get (:normalizations stats))
     :fields-transformed (.get (:fields-transformed stats))
     :type-coercions (.get (:type-coercions stats))
     :defaults-applied (.get (:defaults-applied stats))
     :schemas-count (count (:schemas @state))
     :rules-count (count (:rules @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-normalizer-stats
  "Get normalizer statistics."
  []
  (merge (get-normalizer-metrics)
         {:strict-mode? (get-in @state [:config :strict-mode?])
          :coerce-types? (get-in @state [:config :coerce-types?])
          :remove-nulls? (get-in @state [:config :remove-nulls?])
          :trim-strings? (get-in @state [:config :trim-strings?])}))

(defn reset-stats!
  "Reset normalizer statistics."
  []
  (.set (:normalizations (:stats @state)) 0)
  (.set (:fields-transformed (:stats @state)) 0)
  (.set (:type-coercions (:stats @state)) 0)
  (.set (:defaults-applied (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-normalizer!
  "Initialize the response normalizer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response normalizer initialized")
    (events/emit! :response-normalizer-initialized {})
    true))
