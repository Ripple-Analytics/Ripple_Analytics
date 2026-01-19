(ns mental-models.pipeline.integration.response-formatter
  "Response formatter for mental model analysis system.
   
   Features:
   - Response formatting
   - Template-based formatting
   - Field formatting
   - Date formatting
   - Number formatting
   - Custom formatters
   - Format profiles
   - Formatting metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZonedDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.text DecimalFormat NumberFormat]
           [java.util Locale]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:formatters {}       ;; formatter-id -> formatter
         :profiles {}         ;; profile-id -> profile
         :config {:default-date-format "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
                  :default-number-format "#,##0.##"
                  :default-currency-format "$#,##0.00"
                  :default-locale "en-US"
                  :default-timezone "UTC"}
         :stats {:formats 0
                 :date-formats 0
                 :number-formats 0
                 :template-formats 0
                 :errors 0}
         :initialized? false}))

;; ============================================================================
;; Date Formatting
;; ============================================================================

(defn- get-date-formatter
  "Get a DateTimeFormatter for a pattern."
  [pattern]
  (DateTimeFormatter/ofPattern pattern))

(defn format-date
  "Format a date value."
  [value & {:keys [pattern timezone] :or {pattern nil timezone nil}}]
  (swap! state update-in [:stats :date-formats] inc)
  (let [actual-pattern (or pattern (get-in @state [:config :default-date-format]))
        actual-timezone (or timezone (get-in @state [:config :default-timezone]))
        formatter (get-date-formatter actual-pattern)
        zone (ZoneId/of actual-timezone)]
    (cond
      (instance? Instant value)
      (.format formatter (.atZone value zone))
      
      (instance? LocalDateTime value)
      (.format formatter (.atZone value zone))
      
      (instance? ZonedDateTime value)
      (.format formatter value)
      
      (instance? LocalDate value)
      (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd") value)
      
      (number? value)
      (.format formatter (.atZone (Instant/ofEpochMilli value) zone))
      
      (string? value)
      value
      
      :else (str value))))

(defn format-relative-date
  "Format a date as relative time (e.g., '2 hours ago')."
  [value]
  (let [now (System/currentTimeMillis)
        timestamp (cond
                    (instance? Instant value) (.toEpochMilli value)
                    (number? value) value
                    :else now)
        diff-ms (- now timestamp)
        diff-seconds (quot diff-ms 1000)
        diff-minutes (quot diff-seconds 60)
        diff-hours (quot diff-minutes 60)
        diff-days (quot diff-hours 24)]
    (cond
      (< diff-seconds 60) "just now"
      (< diff-minutes 60) (str diff-minutes " minute" (when (> diff-minutes 1) "s") " ago")
      (< diff-hours 24) (str diff-hours " hour" (when (> diff-hours 1) "s") " ago")
      (< diff-days 30) (str diff-days " day" (when (> diff-days 1) "s") " ago")
      :else (format-date value))))

;; ============================================================================
;; Number Formatting
;; ============================================================================

(defn- get-number-formatter
  "Get a NumberFormat for a pattern."
  [pattern locale]
  (let [loc (Locale/forLanguageTag locale)]
    (if pattern
      (DecimalFormat. pattern)
      (NumberFormat/getInstance loc))))

(defn format-number
  "Format a number value."
  [value & {:keys [pattern locale decimals] :or {pattern nil locale nil decimals nil}}]
  (swap! state update-in [:stats :number-formats] inc)
  (let [actual-locale (or locale (get-in @state [:config :default-locale]))
        actual-pattern (or pattern (get-in @state [:config :default-number-format]))
        formatter (get-number-formatter actual-pattern actual-locale)]
    (when decimals
      (.setMaximumFractionDigits formatter decimals)
      (.setMinimumFractionDigits formatter decimals))
    (.format formatter value)))

(defn format-currency
  "Format a number as currency."
  [value & {:keys [currency locale] :or {currency "USD" locale nil}}]
  (let [actual-locale (or locale (get-in @state [:config :default-locale]))
        loc (Locale/forLanguageTag actual-locale)
        formatter (NumberFormat/getCurrencyInstance loc)]
    (.format formatter value)))

(defn format-percentage
  "Format a number as percentage."
  [value & {:keys [decimals] :or {decimals 2}}]
  (let [formatter (NumberFormat/getPercentInstance)]
    (.setMaximumFractionDigits formatter decimals)
    (.format formatter value)))

(defn format-bytes
  "Format a number as bytes (KB, MB, GB, etc.)."
  [bytes]
  (let [units ["B" "KB" "MB" "GB" "TB" "PB"]
        base 1024]
    (if (zero? bytes)
      "0 B"
      (let [exp (int (Math/floor (/ (Math/log bytes) (Math/log base))))
            exp (min exp (dec (count units)))
            value (/ bytes (Math/pow base exp))]
        (str (format-number value :decimals 2) " " (nth units exp))))))

;; ============================================================================
;; String Formatting
;; ============================================================================

(defn format-string
  "Format a string value."
  [value & {:keys [max-length truncate-suffix case]
            :or {max-length nil truncate-suffix "..." case nil}}]
  (let [s (str value)
        cased (case case
                :upper (str/upper-case s)
                :lower (str/lower-case s)
                :title (str/capitalize s)
                s)
        truncated (if (and max-length (> (count cased) max-length))
                    (str (subs cased 0 (- max-length (count truncate-suffix))) truncate-suffix)
                    cased)]
    truncated))

(defn format-slug
  "Format a string as a URL slug."
  [value]
  (-> (str value)
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn format-phone
  "Format a phone number."
  [value & {:keys [format] :or {format :us}}]
  (let [digits (str/replace (str value) #"[^0-9]" "")]
    (case format
      :us (if (= (count digits) 10)
            (str "(" (subs digits 0 3) ") " (subs digits 3 6) "-" (subs digits 6))
            value)
      :international (str "+" digits)
      value)))

;; ============================================================================
;; Template Formatting
;; ============================================================================

(defn- replace-template-vars
  "Replace template variables with values."
  [template data]
  (reduce (fn [s [k v]]
            (str/replace s (str "{{" (name k) "}}") (str v)))
          template
          data))

(defn format-template
  "Format using a template string."
  [template data]
  (swap! state update-in [:stats :template-formats] inc)
  (replace-template-vars template data))

;; ============================================================================
;; Custom Formatters
;; ============================================================================

(defn register-formatter!
  "Register a custom formatter."
  [formatter-id config]
  (let [formatter {:id formatter-id
                   :name (get config :name (name formatter-id))
                   :format-fn (get config :format-fn)
                   :type (get config :type :custom)
                   :enabled? (atom true)
                   :metrics {:invocations (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:formatters formatter-id] formatter)
    (logging/log :info "Registered formatter" {:formatter-id formatter-id})
    formatter-id))

(defn get-formatter
  "Get a formatter by ID."
  [formatter-id]
  (get-in @state [:formatters formatter-id]))

(defn list-formatters
  "List all formatters."
  []
  (mapv (fn [[id f]]
          {:id id
           :name (:name f)
           :type (:type f)
           :enabled? @(:enabled? f)
           :invocations @(get-in f [:metrics :invocations])})
        (:formatters @state)))

(defn format-with-formatter
  "Format using a custom formatter."
  [formatter-id value & opts]
  (when-let [formatter (get-formatter formatter-id)]
    (when @(:enabled? formatter)
      (swap! (get-in formatter [:metrics :invocations]) inc)
      (apply (:format-fn formatter) value opts))))

;; ============================================================================
;; Format Profiles
;; ============================================================================

(defn register-profile!
  "Register a format profile."
  [profile-id config]
  (let [profile {:id profile-id
                 :name (get config :name (name profile-id))
                 :date-format (get config :date-format)
                 :number-format (get config :number-format)
                 :locale (get config :locale)
                 :timezone (get config :timezone)
                 :field-formats (get config :field-formats {})
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:profiles profile-id] profile)
    profile-id))

(defn get-profile
  "Get a format profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn format-with-profile
  "Format data using a profile."
  [profile-id data]
  (when-let [profile (get-profile profile-id)]
    (let [field-formats (:field-formats profile)]
      (reduce (fn [d [field format-spec]]
                (if-let [value (get d field)]
                  (assoc d field
                         (case (:type format-spec)
                           :date (format-date value
                                              :pattern (or (:pattern format-spec) (:date-format profile))
                                              :timezone (or (:timezone format-spec) (:timezone profile)))
                           :number (format-number value
                                                  :pattern (or (:pattern format-spec) (:number-format profile))
                                                  :locale (or (:locale format-spec) (:locale profile)))
                           :currency (format-currency value :locale (:locale profile))
                           :percentage (format-percentage value)
                           :string (format-string value format-spec)
                           value))
                  d))
              data
              field-formats))))

;; ============================================================================
;; Response Formatting
;; ============================================================================

(defn format-response-body
  "Format a response body."
  [body & {:keys [profile-id field-formats]}]
  (swap! state update-in [:stats :formats] inc)
  
  (cond
    profile-id
    (format-with-profile profile-id body)
    
    field-formats
    (reduce (fn [b [field format-fn]]
              (if-let [value (get b field)]
                (assoc b field (format-fn value))
                b))
            body
            field-formats)
    
    :else body))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-format-response
  "Ring middleware to format responses."
  [handler & {:keys [profile-id field-formats]}]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (format-response-body body
                                                    :profile-id profile-id
                                                    :field-formats field-formats))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-date-format!
  "Set the default date format."
  [format]
  (swap! state assoc-in [:config :default-date-format] format))

(defn set-default-number-format!
  "Set the default number format."
  [format]
  (swap! state assoc-in [:config :default-number-format] format))

(defn set-default-locale!
  "Set the default locale."
  [locale]
  (swap! state assoc-in [:config :default-locale] locale))

(defn set-default-timezone!
  "Set the default timezone."
  [timezone]
  (swap! state assoc-in [:config :default-timezone] timezone))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-formatter-metrics
  "Get formatter metrics."
  []
  (let [stats (:stats @state)]
    {:formats (:formats stats)
     :date-formats (:date-formats stats)
     :number-formats (:number-formats stats)
     :template-formats (:template-formats stats)
     :errors (:errors stats)
     :formatters-count (count (:formatters @state))
     :profiles-count (count (:profiles @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-formatter-stats
  "Get formatter statistics."
  []
  (merge (get-formatter-metrics)
         {:default-date-format (get-in @state [:config :default-date-format])
          :default-number-format (get-in @state [:config :default-number-format])
          :default-locale (get-in @state [:config :default-locale])
          :default-timezone (get-in @state [:config :default-timezone])}))

(defn reset-stats!
  "Reset formatter statistics."
  []
  (swap! state assoc :stats {:formats 0
                             :date-formats 0
                             :number-formats 0
                             :template-formats 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-formatter!
  "Initialize the response formatter."
  []
  (when-not (:initialized? @state)
    ;; Register common formatters
    (register-formatter! :iso-date
                         {:name "ISO Date Formatter"
                          :type :date
                          :format-fn #(format-date % :pattern "yyyy-MM-dd")})
    
    (register-formatter! :compact-number
                         {:name "Compact Number Formatter"
                          :type :number
                          :format-fn #(format-number % :pattern "#,###")})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response formatter initialized")
    (events/emit! :response-formatter-initialized {})
    true))
