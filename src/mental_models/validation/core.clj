(ns mental-models.validation.core
  "Validation Module for Mental Models Pipeline
   
   Provides data validation with:
   - Schema validation
   - Type checking
   - Range validation
   - Custom validators
   - Validation error reporting"
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))

;; =============================================================================
;; VALIDATION RESULT
;; =============================================================================

(defrecord ValidationResult [valid? errors warnings])

(defn valid [& {:keys [warnings] :or {warnings []}}]
  (->ValidationResult true [] warnings))

(defn invalid [errors & {:keys [warnings] :or {warnings []}}]
  (->ValidationResult false errors warnings))

(defn merge-results [& results]
  (->ValidationResult
   (every? :valid? results)
   (vec (mapcat :errors results))
   (vec (mapcat :warnings results))))

;; =============================================================================
;; BASIC VALIDATORS
;; =============================================================================

(defn required [field value]
  (if (or (nil? value) (and (string? value) (str/blank? value)))
    (invalid [{:field field :message "is required"}])
    (valid)))

(defn min-length [field value min-len]
  (if (and value (< (count value) min-len))
    (invalid [{:field field :message (str "must be at least " min-len " characters")}])
    (valid)))

(defn max-length [field value max-len]
  (if (and value (> (count value) max-len))
    (invalid [{:field field :message (str "must be at most " max-len " characters")}])
    (valid)))

(defn in-range [field value min-val max-val]
  (if (and value (or (< value min-val) (> value max-val)))
    (invalid [{:field field :message (str "must be between " min-val " and " max-val)}])
    (valid)))

(defn positive [field value]
  (if (and value (<= value 0))
    (invalid [{:field field :message "must be positive"}])
    (valid)))

(defn non-negative [field value]
  (if (and value (< value 0))
    (invalid [{:field field :message "must be non-negative"}])
    (valid)))

(defn matches-pattern [field value pattern]
  (if (and value (not (re-matches pattern value)))
    (invalid [{:field field :message "has invalid format"}])
    (valid)))

(defn one-of [field value allowed-values]
  (if (and value (not (contains? (set allowed-values) value)))
    (invalid [{:field field :message (str "must be one of: " (str/join ", " allowed-values))}])
    (valid)))

;; =============================================================================
;; TYPE VALIDATORS
;; =============================================================================

(defn is-string [field value]
  (if (and value (not (string? value)))
    (invalid [{:field field :message "must be a string"}])
    (valid)))

(defn is-number [field value]
  (if (and value (not (number? value)))
    (invalid [{:field field :message "must be a number"}])
    (valid)))

(defn is-boolean [field value]
  (if (and value (not (boolean? value)))
    (invalid [{:field field :message "must be a boolean"}])
    (valid)))

(defn is-map [field value]
  (if (and value (not (map? value)))
    (invalid [{:field field :message "must be a map"}])
    (valid)))

(defn is-vector [field value]
  (if (and value (not (vector? value)))
    (invalid [{:field field :message "must be a vector"}])
    (valid)))

(defn is-keyword [field value]
  (if (and value (not (keyword? value)))
    (invalid [{:field field :message "must be a keyword"}])
    (valid)))

;; =============================================================================
;; DOMAIN VALIDATORS
;; =============================================================================

(defn valid-model-id [field value]
  (if (and value (not (keyword? value)))
    (invalid [{:field field :message "must be a valid model ID (keyword)"}])
    (valid)))

(defn valid-confidence [field value]
  (merge-results
   (is-number field value)
   (in-range field value 0.0 1.0)))

(defn valid-document-id [field value]
  (merge-results
   (required field value)
   (is-string field value)))

(defn valid-timestamp [field value]
  (if (and value (not (instance? java.time.Instant value)))
    (invalid [{:field field :message "must be a valid timestamp"}])
    (valid)))

;; =============================================================================
;; ANALYSIS VALIDATION
;; =============================================================================

(defn validate-analysis-request [request]
  (merge-results
   (required :text (:text request))
   (is-string :text (:text request))
   (min-length :text (:text request) 1)))

(defn validate-analysis-result [result]
  (merge-results
   (required :document-id (:document-id result))
   (required :models (:models result))
   (is-vector :models (:models result))))

(defn validate-model-detection [detection]
  (merge-results
   (valid-model-id :model-id (:model-id detection))
   (valid-confidence :confidence (:confidence detection))))

(defn validate-lollapalooza-event [event]
  (merge-results
   (required :models (:models event))
   (is-vector :models (:models event))
   (if (< (count (:models event)) 3)
     (invalid [{:field :models :message "Lollapalooza requires at least 3 models"}])
     (valid))
   (valid-confidence :avg-confidence (:avg-confidence event))))

;; =============================================================================
;; CONFIG VALIDATION
;; =============================================================================

(defn validate-lm-studio-config [config]
  (merge-results
   (required :url (:url config))
   (is-string :url (:url config))
   (matches-pattern :url (:url config) #"https?://.*")))

(defn validate-pipeline-config [config]
  (merge-results
   (when (:concurrency config)
     (merge-results
      (is-number :concurrency (:concurrency config))
      (in-range :concurrency (:concurrency config) 1 100)))
   (when (:batch-size config)
     (merge-results
      (is-number :batch-size (:batch-size config))
      (positive :batch-size (:batch-size config))))))

;; =============================================================================
;; SCHEMA VALIDATION
;; =============================================================================

(defn validate-schema [data schema]
  (let [results (for [[field validators] schema
                      :let [value (get data field)]]
                  (reduce merge-results (valid)
                          (map #(% field value) validators)))]
    (reduce merge-results (valid) results)))

(def analysis-request-schema
  {:text [required is-string]
   :options [is-map]})

(def model-schema
  {:id [required valid-model-id]
   :name [required is-string]
   :category [required is-keyword]
   :description [is-string]})

;; =============================================================================
;; VALIDATION MIDDLEWARE
;; =============================================================================

(defn wrap-validation [handler validator]
  (fn [request]
    (let [result (validator (:body request))]
      (if (:valid? result)
        (handler request)
        {:status 400
         :body {:errors (:errors result)}}))))

;; =============================================================================
;; SPEC INTEGRATION
;; =============================================================================

(s/def ::model-id keyword?)
(s/def ::confidence (s/and number? #(<= 0 % 1)))
(s/def ::document-id string?)
(s/def ::text (s/and string? #(> (count %) 0)))

(s/def ::model-detection
  (s/keys :req-un [::model-id ::confidence]))

(s/def ::analysis-request
  (s/keys :req-un [::text]))

(defn validate-with-spec [spec data]
  (if (s/valid? spec data)
    (valid)
    (invalid [{:message (s/explain-str spec data)}])))

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

(defn format-errors [result]
  (str/join "; " (map #(str (:field %) " " (:message %)) (:errors result))))

(defn throw-if-invalid [result]
  (when-not (:valid? result)
    (throw (ex-info "Validation failed" {:errors (:errors result)}))))

(defn validate-and-throw [validator data]
  (throw-if-invalid (validator data))
  data)
