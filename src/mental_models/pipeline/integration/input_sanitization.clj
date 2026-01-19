(ns mental-models.pipeline.integration.input-sanitization
  "Input sanitization for mental model analysis system.
   
   Features:
   - XSS prevention
   - SQL injection prevention
   - HTML sanitization
   - JSON sanitization
   - Path traversal prevention
   - Command injection prevention
   - Input validation
   - Encoding normalization"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.regex Pattern]
           [java.net URLEncoder URLDecoder]
           [java.nio.charset StandardCharsets]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:xss-protection? true
                  :sql-injection-protection? true
                  :path-traversal-protection? true
                  :command-injection-protection? true
                  :max-input-length 100000
                  :allowed-html-tags #{"p" "br" "b" "i" "u" "strong" "em" "a" "ul" "ol" "li"}
                  :allowed-html-attrs #{"href" "title"}}
         :patterns {:xss [(re-pattern "(?i)<script[^>]*>")
                          (re-pattern "(?i)</script>")
                          (re-pattern "(?i)javascript:")
                          (re-pattern "(?i)on\\w+\\s*=")
                          (re-pattern "(?i)data:")
                          (re-pattern "(?i)vbscript:")]
                    :sql-injection [(re-pattern "(?i)'\\s*(or|and)\\s*'")
                                    (re-pattern "(?i)--")
                                    (re-pattern "(?i);\\s*(drop|delete|update|insert)")
                                    (re-pattern "(?i)union\\s+select")
                                    (re-pattern "(?i)exec\\s*\\(")
                                    (re-pattern "(?i)execute\\s+")]
                    :path-traversal [(re-pattern "\\.\\./")
                                     (re-pattern "\\.\\.\\\\")
                                     (re-pattern "%2e%2e%2f")
                                     (re-pattern "%2e%2e/")
                                     (re-pattern "\\.\\.%2f")]
                    :command-injection [(re-pattern "[;&|`$]")
                                        (re-pattern "\\$\\(")
                                        (re-pattern "`[^`]*`")]}
         :stats {:inputs-sanitized 0
                 :xss-blocked 0
                 :sql-injection-blocked 0
                 :path-traversal-blocked 0
                 :command-injection-blocked 0}
         :initialized? false}))

;; ============================================================================
;; Pattern Detection
;; ============================================================================

(defn- matches-any-pattern?
  "Check if input matches any pattern in the list."
  [input patterns]
  (some #(re-find % input) patterns))

(defn detect-xss
  "Detect potential XSS attack patterns."
  [input]
  (when (and (string? input) (get-in @state [:config :xss-protection?]))
    (matches-any-pattern? input (get-in @state [:patterns :xss]))))

(defn detect-sql-injection
  "Detect potential SQL injection patterns."
  [input]
  (when (and (string? input) (get-in @state [:config :sql-injection-protection?]))
    (matches-any-pattern? input (get-in @state [:patterns :sql-injection]))))

(defn detect-path-traversal
  "Detect potential path traversal patterns."
  [input]
  (when (and (string? input) (get-in @state [:config :path-traversal-protection?]))
    (matches-any-pattern? input (get-in @state [:patterns :path-traversal]))))

(defn detect-command-injection
  "Detect potential command injection patterns."
  [input]
  (when (and (string? input) (get-in @state [:config :command-injection-protection?]))
    (matches-any-pattern? input (get-in @state [:patterns :command-injection]))))

;; ============================================================================
;; HTML Sanitization
;; ============================================================================

(defn- escape-html-char
  "Escape a single HTML character."
  [c]
  (case c
    \< "&lt;"
    \> "&gt;"
    \& "&amp;"
    \" "&quot;"
    \' "&#x27;"
    \/ "&#x2F;"
    (str c)))

(defn escape-html
  "Escape HTML special characters."
  [input]
  (when (string? input)
    (apply str (map escape-html-char input))))

(defn strip-html-tags
  "Strip all HTML tags from input."
  [input]
  (when (string? input)
    (str/replace input #"<[^>]*>" "")))

(defn sanitize-html
  "Sanitize HTML, keeping only allowed tags."
  [input]
  (when (string? input)
    (let [allowed-tags (get-in @state [:config :allowed-html-tags])
          tag-pattern (re-pattern (str "</?(?!" (str/join "|" allowed-tags) ")[^>]*>"))]
      (str/replace input tag-pattern ""))))

;; ============================================================================
;; String Sanitization
;; ============================================================================

(defn sanitize-string
  "Sanitize a string input."
  [input & {:keys [escape-html? strip-tags? max-length]}]
  (when (string? input)
    (swap! state update-in [:stats :inputs-sanitized] inc)
    (let [max-len (or max-length (get-in @state [:config :max-input-length]))]
      (cond-> input
        ;; Truncate if too long
        (> (count input) max-len) (subs 0 max-len)
        
        ;; Strip HTML tags
        strip-tags? strip-html-tags
        
        ;; Escape HTML
        escape-html? escape-html
        
        ;; Trim whitespace
        true str/trim))))

(defn sanitize-for-sql
  "Sanitize input for SQL queries."
  [input]
  (when (string? input)
    (if (detect-sql-injection input)
      (do
        (swap! state update-in [:stats :sql-injection-blocked] inc)
        (logging/log :warn "SQL injection attempt blocked" {:input (subs input 0 (min 100 (count input)))})
        (events/emit! :sql-injection-blocked {:input input})
        nil)
      (-> input
          (str/replace "'" "''")
          (str/replace "\\" "\\\\")))))

(defn sanitize-for-shell
  "Sanitize input for shell commands."
  [input]
  (when (string? input)
    (if (detect-command-injection input)
      (do
        (swap! state update-in [:stats :command-injection-blocked] inc)
        (logging/log :warn "Command injection attempt blocked" {:input (subs input 0 (min 100 (count input)))})
        (events/emit! :command-injection-blocked {:input input})
        nil)
      (-> input
          (str/replace #"[;&|`$(){}]" "")
          (str/replace "\"" "\\\"")
          (str/replace "'" "\\'")))))

(defn sanitize-path
  "Sanitize a file path."
  [input]
  (when (string? input)
    (if (detect-path-traversal input)
      (do
        (swap! state update-in [:stats :path-traversal-blocked] inc)
        (logging/log :warn "Path traversal attempt blocked" {:input input})
        (events/emit! :path-traversal-blocked {:input input})
        nil)
      (-> input
          (str/replace ".." "")
          (str/replace #"[<>:\"|?*]" "")))))

;; ============================================================================
;; URL Sanitization
;; ============================================================================

(defn url-encode
  "URL encode a string."
  [input]
  (when (string? input)
    (URLEncoder/encode input StandardCharsets/UTF_8)))

(defn url-decode
  "URL decode a string."
  [input]
  (when (string? input)
    (URLDecoder/decode input StandardCharsets/UTF_8)))

(defn sanitize-url
  "Sanitize a URL."
  [input]
  (when (string? input)
    (let [decoded (url-decode input)]
      (when-not (or (detect-xss decoded)
                    (str/starts-with? (str/lower-case decoded) "javascript:"))
        input))))

;; ============================================================================
;; JSON Sanitization
;; ============================================================================

(defn sanitize-json-string
  "Sanitize a string for JSON."
  [input]
  (when (string? input)
    (-> input
        (str/replace "\\" "\\\\")
        (str/replace "\"" "\\\"")
        (str/replace "\n" "\\n")
        (str/replace "\r" "\\r")
        (str/replace "\t" "\\t"))))

(defn sanitize-json-value
  "Sanitize a value for JSON output."
  [value]
  (cond
    (string? value) (sanitize-json-string value)
    (map? value) (reduce-kv (fn [m k v]
                              (assoc m k (sanitize-json-value v)))
                            {}
                            value)
    (sequential? value) (mapv sanitize-json-value value)
    :else value))

;; ============================================================================
;; Input Validation
;; ============================================================================

(defn validate-email
  "Validate email format."
  [input]
  (when (string? input)
    (re-matches #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$" input)))

(defn validate-uuid
  "Validate UUID format."
  [input]
  (when (string? input)
    (re-matches #"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$" input)))

(defn validate-alphanumeric
  "Validate alphanumeric input."
  [input]
  (when (string? input)
    (re-matches #"^[a-zA-Z0-9]+$" input)))

(defn validate-numeric
  "Validate numeric input."
  [input]
  (when (string? input)
    (re-matches #"^-?[0-9]+\.?[0-9]*$" input)))

(defn validate-length
  "Validate input length."
  [input min-length max-length]
  (when (string? input)
    (let [len (count input)]
      (and (>= len min-length) (<= len max-length)))))

;; ============================================================================
;; Comprehensive Sanitization
;; ============================================================================

(defn sanitize-input
  "Comprehensive input sanitization."
  [input & {:keys [type] :or {type :text}}]
  (when input
    ;; Check for XSS
    (when (and (string? input) (detect-xss input))
      (swap! state update-in [:stats :xss-blocked] inc)
      (logging/log :warn "XSS attempt blocked" {:input (subs input 0 (min 100 (count input)))})
      (events/emit! :xss-blocked {:input input}))
    
    (case type
      :text (sanitize-string input :escape-html? true)
      :html (sanitize-html input)
      :sql (sanitize-for-sql input)
      :shell (sanitize-for-shell input)
      :path (sanitize-path input)
      :url (sanitize-url input)
      :json (sanitize-json-value input)
      :email (when (validate-email input) input)
      :uuid (when (validate-uuid input) input)
      (sanitize-string input))))

(defn sanitize-map
  "Sanitize all string values in a map."
  [m & {:keys [type] :or {type :text}}]
  (reduce-kv (fn [result k v]
               (assoc result k
                      (cond
                        (string? v) (sanitize-input v :type type)
                        (map? v) (sanitize-map v :type type)
                        (sequential? v) (mapv #(if (string? %)
                                                 (sanitize-input % :type type)
                                                 %)
                                              v)
                        :else v)))
             {}
             m))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn sanitization-middleware
  "Ring middleware for input sanitization."
  [handler]
  (fn [request]
    (let [sanitized-params (when (:params request)
                             (sanitize-map (:params request)))
          sanitized-body (when (and (:body request) (map? (:body request)))
                           (sanitize-map (:body request)))]
      (handler (cond-> request
                 sanitized-params (assoc :params sanitized-params)
                 sanitized-body (assoc :body sanitized-body))))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn enable-xss-protection!
  "Enable XSS protection."
  []
  (swap! state assoc-in [:config :xss-protection?] true))

(defn disable-xss-protection!
  "Disable XSS protection."
  []
  (swap! state assoc-in [:config :xss-protection?] false))

(defn add-allowed-html-tag!
  "Add an allowed HTML tag."
  [tag]
  (swap! state update-in [:config :allowed-html-tags] conj tag))

(defn set-max-input-length!
  "Set maximum input length."
  [length]
  (swap! state assoc-in [:config :max-input-length] length))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-sanitization-stats
  "Get sanitization statistics."
  []
  (:stats @state))

(defn reset-stats!
  "Reset sanitization statistics."
  []
  (swap! state assoc :stats {:inputs-sanitized 0
                             :xss-blocked 0
                             :sql-injection-blocked 0
                             :path-traversal-blocked 0
                             :command-injection-blocked 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-input-sanitization!
  "Initialize the input sanitization system."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Input sanitization initialized")
    (events/emit! :input-sanitization-initialized {})
    true))
