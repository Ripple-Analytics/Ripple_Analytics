(ns mental-models.pipeline.integration.response-sanitizer
  "Response sanitizer for mental model analysis system.
   
   Features:
   - Response sanitization
   - Sensitive data removal
   - PII detection
   - Data masking
   - Field filtering
   - Sanitization rules
   - Compliance support
   - Sanitization metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.util.regex Pattern]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:rules {}            ;; rule-id -> sanitization rule
         :patterns {}         ;; pattern-id -> regex pattern
         :config {:enabled? true
                  :default-mask "***"
                  :mask-length 3
                  :preserve-format? true
                  :log-sanitizations? true}
         :stats {:sanitizations (AtomicLong. 0)
                 :fields-sanitized (AtomicLong. 0)
                 :pii-detected (AtomicLong. 0)
                 :rules-applied (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Built-in Patterns
;; ============================================================================

(def built-in-patterns
  {:email #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
   :phone #"\+?[\d\s\-\(\)]{10,}"
   :ssn #"\d{3}-\d{2}-\d{4}"
   :credit-card #"\d{4}[\s\-]?\d{4}[\s\-]?\d{4}[\s\-]?\d{4}"
   :ip-address #"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}"
   :api-key #"(?i)(api[_-]?key|apikey|api_secret)[\"']?\s*[:=]\s*[\"']?[\w\-]+"
   :password #"(?i)(password|passwd|pwd)[\"']?\s*[:=]\s*[\"']?[^\s\"']+"
   :token #"(?i)(token|bearer|auth)[\"']?\s*[:=]\s*[\"']?[\w\-\.]+"
   :uuid #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
   :date-of-birth #"\d{4}[-/]\d{2}[-/]\d{2}"
   :address #"\d+\s+[\w\s]+(?:street|st|avenue|ave|road|rd|boulevard|blvd|lane|ln|drive|dr)"})

;; ============================================================================
;; Pattern Management
;; ============================================================================

(defn register-pattern!
  "Register a custom pattern."
  [pattern-id pattern]
  (let [compiled (if (instance? Pattern pattern)
                   pattern
                   (re-pattern pattern))]
    (swap! state assoc-in [:patterns pattern-id]
           {:id pattern-id
            :pattern compiled
            :created-at (System/currentTimeMillis)})))

(defn get-pattern
  "Get a pattern."
  [pattern-id]
  (or (get-in @state [:patterns pattern-id :pattern])
      (get built-in-patterns pattern-id)))

(defn list-patterns
  "List all patterns."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-patterns)
   (mapv (fn [[id p]] {:id id :type :custom :created-at (:created-at p)})
         (:patterns @state))))

;; ============================================================================
;; Masking Functions
;; ============================================================================

(defn mask-value
  "Mask a value."
  [value & {:keys [mask preserve-format?]
            :or {mask (get-in @state [:config :default-mask])
                 preserve-format? (get-in @state [:config :preserve-format?])}}]
  (if preserve-format?
    (let [s (str value)
          len (count s)]
      (if (> len 4)
        (str (subs s 0 2) (apply str (repeat (- len 4) "*")) (subs s (- len 2)))
        mask))
    mask))

(defn mask-email
  "Mask an email address."
  [email]
  (let [[local domain] (str/split email #"@")]
    (if (and local domain)
      (str (first local) "***@" domain)
      (mask-value email))))

(defn mask-phone
  "Mask a phone number."
  [phone]
  (let [digits (str/replace phone #"[^\d]" "")]
    (if (>= (count digits) 4)
      (str "***-***-" (subs digits (- (count digits) 4)))
      (mask-value phone))))

(defn mask-credit-card
  "Mask a credit card number."
  [cc]
  (let [digits (str/replace cc #"[^\d]" "")]
    (if (>= (count digits) 4)
      (str "****-****-****-" (subs digits (- (count digits) 4)))
      (mask-value cc))))

(defn mask-ssn
  "Mask a social security number."
  [ssn]
  (let [digits (str/replace ssn #"[^\d]" "")]
    (if (= (count digits) 9)
      (str "***-**-" (subs digits 5))
      (mask-value ssn))))

(defn mask-ip
  "Mask an IP address."
  [ip]
  (let [parts (str/split ip #"\.")]
    (if (= (count parts) 4)
      (str (first parts) ".***.***." (last parts))
      (mask-value ip))))

;; ============================================================================
;; Sanitization Rules
;; ============================================================================

(defn register-rule!
  "Register a sanitization rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :fields (get config :fields [])
              :patterns (get config :patterns [])
              :mask-fn (get config :mask-fn mask-value)
              :condition-fn (get config :condition-fn (constantly true))
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Registered sanitization rule" {:rule-id rule-id})
    rule-id))

(defn get-rule
  "Get a sanitization rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :fields (:fields r)
           :patterns (:patterns r)
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
;; PII Detection
;; ============================================================================

(defn detect-pii
  "Detect PII in a string."
  [s]
  (let [detections (atom [])]
    (doseq [[pattern-id pattern] built-in-patterns]
      (when-let [matches (re-seq pattern s)]
        (swap! detections conj {:type pattern-id
                                :count (count matches)
                                :matches matches})))
    @detections))

(defn contains-pii?
  "Check if string contains PII."
  [s]
  (some (fn [[_ pattern]]
          (re-find pattern s))
        built-in-patterns))

;; ============================================================================
;; Field Sanitization
;; ============================================================================

(def sensitive-field-names
  #{:password :passwd :pwd :secret :token :api-key :apikey :api_key
    :access-token :access_token :refresh-token :refresh_token
    :authorization :auth :credentials :private-key :private_key
    :ssn :social-security :credit-card :credit_card :cvv :cvc
    :pin :account-number :account_number :routing-number :routing_number})

(defn sensitive-field?
  "Check if a field name is sensitive."
  [field-name]
  (let [normalized (-> (name field-name)
                       str/lower-case
                       (str/replace #"[-_]" "")
                       keyword)]
    (or (contains? sensitive-field-names field-name)
        (contains? sensitive-field-names normalized)
        (some #(str/includes? (name field-name) (name %))
              [:password :secret :token :key :auth :credential]))))

(defn sanitize-field
  "Sanitize a field value."
  [field-name value]
  (.incrementAndGet (:fields-sanitized (:stats @state)))
  
  (cond
    (nil? value) nil
    (not (string? value)) (mask-value value)
    
    (re-find (:email built-in-patterns) value)
    (mask-email value)
    
    (re-find (:credit-card built-in-patterns) value)
    (mask-credit-card value)
    
    (re-find (:ssn built-in-patterns) value)
    (mask-ssn value)
    
    (re-find (:phone built-in-patterns) value)
    (mask-phone value)
    
    (re-find (:ip-address built-in-patterns) value)
    (mask-ip value)
    
    :else (mask-value value)))

;; ============================================================================
;; Data Sanitization
;; ============================================================================

(defn sanitize-map
  "Sanitize a map by removing/masking sensitive fields."
  [data & {:keys [fields deep?] :or {deep? true}}]
  (let [fields-to-sanitize (set (or fields sensitive-field-names))]
    (if deep?
      (walk/postwalk
       (fn [x]
         (if (map-entry? x)
           (let [[k v] x]
             (if (or (contains? fields-to-sanitize k)
                     (sensitive-field? k))
               [k (sanitize-field k v)]
               x))
           x))
       data)
      (reduce (fn [result [k v]]
                (if (or (contains? fields-to-sanitize k)
                        (sensitive-field? k))
                  (assoc result k (sanitize-field k v))
                  (assoc result k v)))
              {}
              data))))

(defn sanitize-string
  "Sanitize a string by masking PII patterns."
  [s]
  (reduce (fn [result [pattern-id pattern]]
            (let [mask-fn (case pattern-id
                            :email mask-email
                            :credit-card mask-credit-card
                            :ssn mask-ssn
                            :phone mask-phone
                            :ip-address mask-ip
                            mask-value)]
              (str/replace result pattern
                           (fn [match]
                             (.incrementAndGet (:pii-detected (:stats @state)))
                             (mask-fn match)))))
          s
          built-in-patterns))

;; ============================================================================
;; Response Sanitization
;; ============================================================================

(defn sanitize-response
  "Sanitize a response."
  [response & {:keys [fields deep?] :or {deep? true}}]
  (when (get-in @state [:config :enabled?])
    (.incrementAndGet (:sanitizations (:stats @state)))
    
    (let [body (:body response)]
      (cond
        (map? body)
        (assoc response :body (sanitize-map body :fields fields :deep? deep?))
        
        (string? body)
        (assoc response :body (sanitize-string body))
        
        :else response))))

(defn apply-rule
  "Apply a sanitization rule to data."
  [data rule-id]
  (when-let [rule (get-rule rule-id)]
    (when (and @(:enabled? rule)
               ((:condition-fn rule) data))
      (.incrementAndGet (:rules-applied (:stats @state)))
      
      (let [fields (:fields rule)
            patterns (:patterns rule)
            mask-fn (:mask-fn rule)]
        (-> data
            (sanitize-map :fields fields)
            (as-> d
                  (reduce (fn [result pattern-id]
                            (if-let [pattern (get-pattern pattern-id)]
                              (walk/postwalk
                               (fn [x]
                                 (if (string? x)
                                   (str/replace x pattern #(mask-fn %))
                                   x))
                               result)
                              result))
                          d
                          patterns)))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-sanitize
  "Ring middleware to sanitize responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (sanitize-response response))))

(defn wrap-sanitize-fields
  "Ring middleware to sanitize specific fields."
  [handler fields]
  (fn [request]
    (let [response (handler request)]
      (sanitize-response response :fields fields))))

(defn wrap-sanitize-with-rule
  "Ring middleware to sanitize with a rule."
  [handler rule-id]
  (fn [request]
    (let [response (handler request)]
      (if (map? (:body response))
        (assoc response :body (apply-rule (:body response) rule-id))
        response))))

(defn wrap-detect-pii
  "Ring middleware to detect and log PII."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (when (string? body)
        (let [detections (detect-pii body)]
          (when (seq detections)
            (logging/log :warn "PII detected in response" {:detections detections}))))
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable sanitization."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-default-mask!
  "Set default mask string."
  [mask]
  (swap! state assoc-in [:config :default-mask] mask))

(defn set-preserve-format!
  "Enable/disable format preservation."
  [preserve?]
  (swap! state assoc-in [:config :preserve-format?] preserve?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-sanitizer-metrics
  "Get sanitizer metrics."
  []
  (let [stats (:stats @state)]
    {:sanitizations (.get (:sanitizations stats))
     :fields-sanitized (.get (:fields-sanitized stats))
     :pii-detected (.get (:pii-detected stats))
     :rules-applied (.get (:rules-applied stats))
     :rules-count (count (:rules @state))
     :patterns-count (+ (count built-in-patterns) (count (:patterns @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-sanitizer-stats
  "Get sanitizer statistics."
  []
  (merge (get-sanitizer-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :default-mask (get-in @state [:config :default-mask])
          :preserve-format? (get-in @state [:config :preserve-format?])}))

(defn reset-stats!
  "Reset sanitizer statistics."
  []
  (.set (:sanitizations (:stats @state)) 0)
  (.set (:fields-sanitized (:stats @state)) 0)
  (.set (:pii-detected (:stats @state)) 0)
  (.set (:rules-applied (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-sanitizer!
  "Initialize the response sanitizer."
  []
  (when-not (:initialized? @state)
    ;; Register default rule
    (register-rule! :default {:name "Default Sanitization"
                              :fields sensitive-field-names})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response sanitizer initialized")
    (events/emit! :response-sanitizer-initialized {})
    true))
