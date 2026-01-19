(ns mental-models.pipeline.integration.security-scanner
  "Security scanning and vulnerability detection for the pipeline.
   
   Features:
   - Input validation and sanitization
   - SQL injection detection
   - XSS prevention
   - Sensitive data detection
   - Rate limiting enforcement
   - Authentication verification
   - Authorization checks
   - Security audit logging"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:rules {}            ;; rule-id -> security-rule
         :violations {}       ;; violation-id -> violation
         :blocked-ips {}      ;; ip -> block-info
         :rate-limits {}      ;; key -> rate-limit-state
         :sensitive-patterns {} ;; pattern-id -> pattern
         :scan-history []     ;; historical scans
         :initialized? false}))

;; ============================================================================
;; Security Rules
;; ============================================================================

(defn register-rule!
  "Register a security rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :type (get config :type :validation)
              :severity (get config :severity :medium)
              :pattern (get config :pattern nil)
              :check-fn (get config :check-fn nil)
              :action (get config :action :block)
              :message (get config :message "Security violation detected")
              :enabled? (get config :enabled? true)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Registered security rule" {:rule-id rule-id :type (:type rule)})
    (events/emit! :security-rule-registered {:rule-id rule-id})
    rule-id))

(defn get-rule
  "Get a security rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all security rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :type (:type r)
           :severity (:severity r)
           :enabled? (:enabled? r)})
        (:rules @state)))

(defn enable-rule!
  "Enable a security rule."
  [rule-id]
  (swap! state assoc-in [:rules rule-id :enabled?] true))

(defn disable-rule!
  "Disable a security rule."
  [rule-id]
  (swap! state assoc-in [:rules rule-id :enabled?] false))

;; ============================================================================
;; Input Validation
;; ============================================================================

(def ^:private sql-injection-patterns
  "Patterns that indicate potential SQL injection."
  [#"(?i)(\b(SELECT|INSERT|UPDATE|DELETE|DROP|UNION|ALTER|CREATE|TRUNCATE)\b)"
   #"(?i)(--|\#|\/\*)"
   #"(?i)(\bOR\b\s+\d+\s*=\s*\d+)"
   #"(?i)(\bAND\b\s+\d+\s*=\s*\d+)"
   #"(?i)(;\s*(SELECT|INSERT|UPDATE|DELETE))"
   #"(?i)('|\"|`)\s*(OR|AND)\s*('|\"|`)"
   #"(?i)(EXEC\s*\(|EXECUTE\s*\()"
   #"(?i)(xp_|sp_)"])

(def ^:private xss-patterns
  "Patterns that indicate potential XSS attacks."
  [#"(?i)<script[^>]*>"
   #"(?i)javascript:"
   #"(?i)on\w+\s*="
   #"(?i)<iframe[^>]*>"
   #"(?i)<object[^>]*>"
   #"(?i)<embed[^>]*>"
   #"(?i)<link[^>]*>"
   #"(?i)expression\s*\("
   #"(?i)url\s*\("
   #"(?i)data:text/html"])

(defn- matches-any-pattern?
  "Check if input matches any of the given patterns."
  [input patterns]
  (some #(re-find % (str input)) patterns))

(defn detect-sql-injection
  "Detect potential SQL injection in input."
  [input]
  (when (flags/enabled? :security-scanner)
    (let [detected? (matches-any-pattern? input sql-injection-patterns)]
      {:type :sql-injection
       :detected? detected?
       :input (subs (str input) 0 (min 100 (count (str input))))
       :severity (if detected? :high :none)})))

(defn detect-xss
  "Detect potential XSS in input."
  [input]
  (when (flags/enabled? :security-scanner)
    (let [detected? (matches-any-pattern? input xss-patterns)]
      {:type :xss
       :detected? detected?
       :input (subs (str input) 0 (min 100 (count (str input))))
       :severity (if detected? :high :none)})))

(defn sanitize-input
  "Sanitize input by removing potentially dangerous content."
  [input & {:keys [allow-html?] :or {allow-html? false}}]
  (let [sanitized (-> (str input)
                      ;; Remove null bytes
                      (str/replace #"\x00" "")
                      ;; Remove control characters
                      (str/replace #"[\x01-\x1f\x7f]" ""))]
    (if allow-html?
      sanitized
      (-> sanitized
          ;; Escape HTML entities
          (str/replace "&" "&amp;")
          (str/replace "<" "&lt;")
          (str/replace ">" "&gt;")
          (str/replace "\"" "&quot;")
          (str/replace "'" "&#x27;")))))

(defn validate-input
  "Validate input against security rules."
  [input & {:keys [context]}]
  (when (flags/enabled? :security-scanner)
    (let [sql-check (detect-sql-injection input)
          xss-check (detect-xss input)
          violations (filterv :detected? [sql-check xss-check])]
      {:valid? (empty? violations)
       :violations violations
       :sanitized (sanitize-input input)
       :context context
       :checked-at (System/currentTimeMillis)})))

;; ============================================================================
;; Sensitive Data Detection
;; ============================================================================

(defn register-sensitive-pattern!
  "Register a pattern for sensitive data detection."
  [pattern-id config]
  (let [pattern {:id pattern-id
                 :name (get config :name (name pattern-id))
                 :pattern (get config :pattern)
                 :type (get config :type :pii)
                 :action (get config :action :mask)
                 :mask-char (get config :mask-char "*")
                 :enabled? (get config :enabled? true)}]
    (swap! state assoc-in [:sensitive-patterns pattern-id] pattern)
    pattern-id))

(def ^:private default-sensitive-patterns
  "Default patterns for sensitive data."
  {:email {:pattern #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
           :type :pii
           :name "Email Address"}
   :phone {:pattern #"\b\d{3}[-.]?\d{3}[-.]?\d{4}\b"
           :type :pii
           :name "Phone Number"}
   :ssn {:pattern #"\b\d{3}-\d{2}-\d{4}\b"
         :type :pii
         :name "Social Security Number"}
   :credit-card {:pattern #"\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b"
                 :type :financial
                 :name "Credit Card Number"}
   :api-key {:pattern #"\b[A-Za-z0-9]{32,}\b"
             :type :credential
             :name "API Key"}})

(defn detect-sensitive-data
  "Detect sensitive data in text."
  [text]
  (when (flags/enabled? :security-scanner)
    (let [patterns (merge default-sensitive-patterns (:sensitive-patterns @state))
          detections (for [[id config] patterns
                           :let [matches (re-seq (:pattern config) text)]
                           :when (seq matches)]
                       {:pattern-id id
                        :type (:type config)
                        :name (:name config)
                        :count (count matches)
                        :positions (mapv #(str/index-of text %) matches)})]
      {:has-sensitive? (seq detections)
       :detections (vec detections)
       :total-matches (reduce + (map :count detections))})))

(defn mask-sensitive-data
  "Mask sensitive data in text."
  [text & {:keys [patterns mask-char] :or {mask-char "*"}}]
  (let [patterns-to-use (or patterns (merge default-sensitive-patterns (:sensitive-patterns @state)))]
    (reduce (fn [t [_id config]]
              (str/replace t (:pattern config)
                           (fn [match]
                             (let [len (count match)
                                   visible (min 4 (quot len 4))]
                               (str (subs match 0 visible)
                                    (apply str (repeat (- len visible) mask-char)))))))
            text
            patterns-to-use)))

;; ============================================================================
;; Rate Limiting
;; ============================================================================

(defn- get-rate-limit-key
  "Generate a rate limit key."
  [identifier action]
  (str identifier ":" action))

(defn check-rate-limit
  "Check if an action is within rate limits."
  [identifier action & {:keys [max-requests window-ms]
                         :or {max-requests 100 window-ms 60000}}]
  (let [key (get-rate-limit-key identifier action)
        now (System/currentTimeMillis)
        current (get-in @state [:rate-limits key])
        window-start (- now window-ms)]
    
    (if (or (nil? current)
            (< (:window-start current) window-start))
      ;; New window
      (do
        (swap! state assoc-in [:rate-limits key]
               {:window-start now
                :count 1})
        {:allowed? true
         :remaining (dec max-requests)
         :reset-at (+ now window-ms)})
      ;; Existing window
      (let [new-count (inc (:count current))]
        (if (<= new-count max-requests)
          (do
            (swap! state update-in [:rate-limits key :count] inc)
            {:allowed? true
             :remaining (- max-requests new-count)
             :reset-at (+ (:window-start current) window-ms)})
          {:allowed? false
           :remaining 0
           :reset-at (+ (:window-start current) window-ms)
           :retry-after (- (+ (:window-start current) window-ms) now)})))))

(defn reset-rate-limit!
  "Reset rate limit for an identifier."
  [identifier action]
  (let [key (get-rate-limit-key identifier action)]
    (swap! state update :rate-limits dissoc key)))

;; ============================================================================
;; IP Blocking
;; ============================================================================

(defn block-ip!
  "Block an IP address."
  [ip & {:keys [reason duration-ms]}]
  (let [block-info {:ip ip
                    :reason (or reason "Security violation")
                    :blocked-at (System/currentTimeMillis)
                    :expires-at (when duration-ms
                                  (+ (System/currentTimeMillis) duration-ms))}]
    (swap! state assoc-in [:blocked-ips ip] block-info)
    (logging/log :warn "Blocked IP" {:ip ip :reason reason})
    (events/emit! :ip-blocked {:ip ip})
    block-info))

(defn unblock-ip!
  "Unblock an IP address."
  [ip]
  (swap! state update :blocked-ips dissoc ip)
  (logging/log :info "Unblocked IP" {:ip ip}))

(defn is-blocked?
  "Check if an IP is blocked."
  [ip]
  (when-let [block-info (get-in @state [:blocked-ips ip])]
    (if-let [expires-at (:expires-at block-info)]
      (if (> (System/currentTimeMillis) expires-at)
        (do
          (unblock-ip! ip)
          false)
        true)
      true)))

(defn list-blocked-ips
  "List all blocked IPs."
  []
  (vals (:blocked-ips @state)))

;; ============================================================================
;; Security Violations
;; ============================================================================

(defn record-violation!
  "Record a security violation."
  [violation]
  (let [violation-id (str (UUID/randomUUID))
        record {:id violation-id
                :type (get violation :type :unknown)
                :severity (get violation :severity :medium)
                :source (get violation :source)
                :ip (get violation :ip)
                :user-id (get violation :user-id)
                :details (get violation :details {})
                :action-taken (get violation :action-taken)
                :recorded-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:violations violation-id] record)
    (swap! state update :scan-history conj record)
    (metrics/increment :security-violations {:type (:type record) :severity (:severity record)})
    (logging/log :warn "Security violation recorded" {:violation-id violation-id :type (:type record)})
    (events/emit! :security-violation {:violation-id violation-id :violation record})
    violation-id))

(defn get-violation
  "Get a security violation."
  [violation-id]
  (get-in @state [:violations violation-id]))

(defn list-violations
  "List security violations."
  [& {:keys [type severity since limit]}]
  (let [violations (vals (:violations @state))
        filtered (cond->> violations
                   type (filter #(= (:type %) type))
                   severity (filter #(= (:severity %) severity))
                   since (filter #(>= (:recorded-at %) since))
                   true (sort-by :recorded-at >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Security Scanning
;; ============================================================================

(defn scan-request
  "Perform security scan on a request."
  [request]
  (when (flags/enabled? :security-scanner)
    (let [ip (get-in request [:headers "x-forwarded-for"]
                     (get request :remote-addr "unknown"))
          body (get request :body "")
          params (merge (get request :query-params {})
                        (get request :form-params {}))
          
          ;; Check if IP is blocked
          ip-blocked? (is-blocked? ip)
          
          ;; Check rate limit
          rate-limit (check-rate-limit ip :request)
          
          ;; Validate inputs
          body-validation (validate-input body :context :body)
          param-validations (into {}
                                  (map (fn [[k v]]
                                         [k (validate-input v :context :param)])
                                       params))
          
          ;; Check for sensitive data
          sensitive-check (detect-sensitive-data (str body))
          
          ;; Aggregate results
          all-violations (concat
                          (when ip-blocked? [{:type :blocked-ip :severity :critical}])
                          (when-not (:allowed? rate-limit) [{:type :rate-limit :severity :medium}])
                          (:violations body-validation)
                          (mapcat #(:violations (val %)) param-validations))
          
          passed? (empty? all-violations)]
      
      ;; Record violations if any
      (when (seq all-violations)
        (record-violation! {:type :request-scan
                            :severity (if (some #(= (:severity %) :critical) all-violations)
                                        :critical
                                        :high)
                            :ip ip
                            :details {:violations all-violations
                                      :uri (:uri request)
                                      :method (:request-method request)}}))
      
      {:passed? passed?
       :ip-blocked? ip-blocked?
       :rate-limit rate-limit
       :body-validation body-validation
       :param-validations param-validations
       :sensitive-data sensitive-check
       :violations all-violations
       :scanned-at (System/currentTimeMillis)})))

(defn scan-text
  "Scan text for security issues."
  [text & {:keys [check-sensitive?] :or {check-sensitive? true}}]
  (when (flags/enabled? :security-scanner)
    (let [sql-check (detect-sql-injection text)
          xss-check (detect-xss text)
          sensitive-check (when check-sensitive? (detect-sensitive-data text))
          violations (filterv :detected? [sql-check xss-check])]
      {:passed? (empty? violations)
       :violations violations
       :sensitive-data sensitive-check
       :sanitized (sanitize-input text)
       :scanned-at (System/currentTimeMillis)})))

;; ============================================================================
;; Security Middleware
;; ============================================================================

(defn security-middleware
  "Create security middleware for request processing."
  [handler]
  (fn [request]
    (let [scan-result (scan-request request)]
      (if (:passed? scan-result)
        (handler request)
        {:status 403
         :body {:error "Security check failed"
                :violations (mapv :type (:violations scan-result))}}))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-scanner-stats
  "Get security scanner statistics."
  []
  (let [violations (vals (:violations @state))
        by-type (group-by :type violations)
        by-severity (group-by :severity violations)]
    {:total-rules (count (:rules @state))
     :total-violations (count violations)
     :blocked-ips (count (:blocked-ips @state))
     :active-rate-limits (count (:rate-limits @state))
     :sensitive-patterns (count (:sensitive-patterns @state))
     :violations-by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :violations-by-severity (into {} (map (fn [[k v]] [k (count v)]) by-severity))
     :recent-violations (take 10 (sort-by :recorded-at > violations))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-security-scanner!
  "Initialize the security scanner."
  []
  (when-not (:initialized? @state)
    ;; Register default rules
    (register-rule! :sql-injection
                    {:name "SQL Injection Detection"
                     :type :injection
                     :severity :critical
                     :action :block})
    
    (register-rule! :xss
                    {:name "XSS Detection"
                     :type :injection
                     :severity :high
                     :action :block})
    
    (register-rule! :rate-limit
                    {:name "Rate Limiting"
                     :type :rate-limit
                     :severity :medium
                     :action :throttle})
    
    ;; Register sensitive patterns
    (doseq [[id config] default-sensitive-patterns]
      (register-sensitive-pattern! id config))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Security scanner initialized")
    (events/emit! :security-scanner-initialized {})
    true))
