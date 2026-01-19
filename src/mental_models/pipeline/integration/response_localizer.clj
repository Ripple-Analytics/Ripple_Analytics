(ns mental-models.pipeline.integration.response-localizer
  "Response localizer for mental model analysis system.
   
   Features:
   - Response localization
   - Language detection
   - Translation support
   - Locale management
   - Number formatting
   - Date formatting
   - Currency formatting
   - Localization metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Locale]
           [java.time Instant LocalDate LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter FormatStyle]
           [java.text NumberFormat DecimalFormat]
           [java.text DecimalFormatSymbols]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:translations {}     ;; locale -> key -> translation
         :locales {}          ;; locale-id -> locale config
         :config {:default-locale "en-US"
                  :supported-locales #{"en-US" "en-GB" "es-ES" "fr-FR" "de-DE" "ja-JP" "zh-CN"}
                  :fallback-locale "en-US"
                  :locale-header "Accept-Language"
                  :locale-param "locale"}
         :stats {:localizations 0
                 :translations 0
                 :format-operations 0
                 :locale-detections 0}
         :initialized? false}))

;; ============================================================================
;; Locale Management
;; ============================================================================

(defn register-locale!
  "Register a locale."
  [locale-id config]
  (let [locale-config {:id locale-id
                       :name (get config :name locale-id)
                       :language (get config :language)
                       :country (get config :country)
                       :java-locale (Locale/forLanguageTag locale-id)
                       :date-format (get config :date-format)
                       :time-format (get config :time-format)
                       :number-format (get config :number-format)
                       :currency (get config :currency)
                       :enabled? (atom true)
                       :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:locales locale-id] locale-config)
    (swap! state update-in [:config :supported-locales] conj locale-id)
    (logging/log :info "Registered locale" {:locale-id locale-id})
    locale-id))

(defn get-locale
  "Get a locale configuration."
  [locale-id]
  (get-in @state [:locales locale-id]))

(defn list-locales
  "List all locales."
  []
  (mapv (fn [[id config]]
          {:id id
           :name (:name config)
           :language (:language config)
           :country (:country config)
           :enabled? @(:enabled? config)})
        (:locales @state)))

(defn get-java-locale
  "Get Java Locale for a locale ID."
  [locale-id]
  (if-let [config (get-locale locale-id)]
    (:java-locale config)
    (Locale/forLanguageTag (or locale-id (get-in @state [:config :default-locale])))))

;; ============================================================================
;; Translation Management
;; ============================================================================

(defn add-translations!
  "Add translations for a locale."
  [locale-id translations]
  (swap! state update-in [:translations locale-id] merge translations)
  (logging/log :info "Added translations" {:locale-id locale-id :count (count translations)}))

(defn get-translation
  "Get a translation for a key."
  [locale-id key]
  (or (get-in @state [:translations locale-id key])
      (get-in @state [:translations (get-in @state [:config :fallback-locale]) key])
      (str key)))

(defn translate
  "Translate a key with optional interpolation."
  [locale-id key & {:keys [params]}]
  (swap! state update-in [:stats :translations] inc)
  
  (let [translation (get-translation locale-id key)]
    (if params
      (reduce (fn [s [k v]]
                (str/replace s (str "{{" (name k) "}}") (str v)))
              translation
              params)
      translation)))

(defn translate-map
  "Translate all string values in a map."
  [locale-id data & {:keys [keys-to-translate]}]
  (walk/postwalk
   (fn [x]
     (if (and (string? x)
              (str/starts-with? x "i18n:"))
       (translate locale-id (subs x 5))
       x))
   data))

;; ============================================================================
;; Locale Detection
;; ============================================================================

(defn parse-accept-language
  "Parse Accept-Language header."
  [header]
  (when header
    (->> (str/split header #",")
         (map str/trim)
         (map (fn [part]
                (let [[lang q] (str/split part #";q=")]
                  {:locale (str/trim lang)
                   :q (if q (Double/parseDouble q) 1.0)})))
         (sort-by :q >)
         (map :locale))))

(defn detect-locale
  "Detect locale from request."
  [request]
  (swap! state update-in [:stats :locale-detections] inc)
  
  (let [supported (get-in @state [:config :supported-locales])
        ;; Try query param
        param-locale (get-in request [:query-params (get-in @state [:config :locale-param])])
        ;; Try header
        header (get-in request [:headers (str/lower-case (get-in @state [:config :locale-header]))])
        header-locales (parse-accept-language header)
        ;; Find first supported locale
        detected (or (when (and param-locale (contains? supported param-locale))
                       param-locale)
                     (first (filter #(contains? supported %) header-locales))
                     (get-in @state [:config :default-locale]))]
    detected))

;; ============================================================================
;; Number Formatting
;; ============================================================================

(defn format-number
  "Format a number for a locale."
  [locale-id number & {:keys [style min-fraction max-fraction]
                       :or {style :decimal}}]
  (swap! state update-in [:stats :format-operations] inc)
  
  (let [java-locale (get-java-locale locale-id)
        formatter (case style
                    :decimal (NumberFormat/getNumberInstance java-locale)
                    :integer (NumberFormat/getIntegerInstance java-locale)
                    :percent (NumberFormat/getPercentInstance java-locale)
                    :currency (NumberFormat/getCurrencyInstance java-locale)
                    (NumberFormat/getNumberInstance java-locale))]
    (when min-fraction
      (.setMinimumFractionDigits formatter min-fraction))
    (when max-fraction
      (.setMaximumFractionDigits formatter max-fraction))
    (.format formatter number)))

(defn format-currency
  "Format a currency amount."
  [locale-id amount & {:keys [currency-code]}]
  (let [java-locale (get-java-locale locale-id)
        formatter (NumberFormat/getCurrencyInstance java-locale)]
    (when currency-code
      (.setCurrency formatter (java.util.Currency/getInstance currency-code)))
    (.format formatter amount)))

(defn format-percent
  "Format a percentage."
  [locale-id value]
  (format-number locale-id value :style :percent))

;; ============================================================================
;; Date/Time Formatting
;; ============================================================================

(defn format-date
  "Format a date for a locale."
  [locale-id date & {:keys [style pattern]
                     :or {style :medium}}]
  (swap! state update-in [:stats :format-operations] inc)
  
  (let [java-locale (get-java-locale locale-id)
        formatter (if pattern
                    (DateTimeFormatter/ofPattern pattern java-locale)
                    (DateTimeFormatter/ofLocalizedDate
                     (case style
                       :short FormatStyle/SHORT
                       :medium FormatStyle/MEDIUM
                       :long FormatStyle/LONG
                       :full FormatStyle/FULL
                       FormatStyle/MEDIUM)))
        local-date (cond
                     (instance? LocalDate date) date
                     (instance? Instant date) (LocalDate/ofInstant date (ZoneId/systemDefault))
                     (number? date) (LocalDate/ofInstant (Instant/ofEpochMilli date) (ZoneId/systemDefault))
                     :else (LocalDate/parse (str date)))]
    (.format formatter local-date)))

(defn format-datetime
  "Format a datetime for a locale."
  [locale-id datetime & {:keys [date-style time-style pattern]
                         :or {date-style :medium time-style :medium}}]
  (swap! state update-in [:stats :format-operations] inc)
  
  (let [java-locale (get-java-locale locale-id)
        formatter (if pattern
                    (DateTimeFormatter/ofPattern pattern java-locale)
                    (DateTimeFormatter/ofLocalizedDateTime
                     (case date-style
                       :short FormatStyle/SHORT
                       :medium FormatStyle/MEDIUM
                       :long FormatStyle/LONG
                       :full FormatStyle/FULL
                       FormatStyle/MEDIUM)
                     (case time-style
                       :short FormatStyle/SHORT
                       :medium FormatStyle/MEDIUM
                       :long FormatStyle/LONG
                       :full FormatStyle/FULL
                       FormatStyle/MEDIUM)))
        local-datetime (cond
                         (instance? LocalDateTime datetime) datetime
                         (instance? Instant datetime) (LocalDateTime/ofInstant datetime (ZoneId/systemDefault))
                         (number? datetime) (LocalDateTime/ofInstant (Instant/ofEpochMilli datetime) (ZoneId/systemDefault))
                         :else (LocalDateTime/parse (str datetime)))]
    (.format formatter local-datetime)))

(defn format-time
  "Format a time for a locale."
  [locale-id time & {:keys [style] :or {style :medium}}]
  (format-datetime locale-id time :date-style nil :time-style style))

;; ============================================================================
;; Response Localization
;; ============================================================================

(defn localize-response
  "Localize a response."
  [response locale-id]
  (swap! state update-in [:stats :localizations] inc)
  
  (let [body (:body response)]
    (if (map? body)
      (assoc response :body (translate-map locale-id body))
      response)))

(defn localize-field
  "Localize a specific field in response."
  [response field locale-id]
  (if (map? (:body response))
    (update-in response [:body field]
               (fn [v]
                 (if (string? v)
                   (translate locale-id v)
                   v)))
    response))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-locale
  "Ring middleware to detect and set locale."
  [handler]
  (fn [request]
    (let [locale (detect-locale request)
          response (handler (assoc request :locale locale))]
      (assoc-in response [:headers "Content-Language"] locale))))

(defn wrap-localize
  "Ring middleware to localize responses."
  [handler]
  (fn [request]
    (let [locale (or (:locale request) (detect-locale request))
          response (handler request)]
      (localize-response response locale))))

(defn wrap-locale-header
  "Ring middleware to add locale header."
  [handler]
  (fn [request]
    (let [locale (or (:locale request) (detect-locale request))
          response (handler request)]
      (-> response
          (assoc-in [:headers "Content-Language"] locale)
          (assoc-in [:headers "Vary"] "Accept-Language")))))

;; ============================================================================
;; Pluralization
;; ============================================================================

(defn pluralize
  "Get plural form based on count."
  [locale-id key count]
  (let [plural-key (cond
                     (= count 0) (str key ".zero")
                     (= count 1) (str key ".one")
                     :else (str key ".other"))]
    (translate locale-id plural-key :params {:count count})))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-locale!
  "Set default locale."
  [locale]
  (swap! state assoc-in [:config :default-locale] locale))

(defn set-fallback-locale!
  "Set fallback locale."
  [locale]
  (swap! state assoc-in [:config :fallback-locale] locale))

(defn set-locale-header!
  "Set locale header name."
  [header]
  (swap! state assoc-in [:config :locale-header] header))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-localizer-metrics
  "Get localizer metrics."
  []
  (let [stats (:stats @state)]
    {:localizations (:localizations stats)
     :translations (:translations stats)
     :format-operations (:format-operations stats)
     :locale-detections (:locale-detections stats)
     :locales-count (count (:locales @state))
     :supported-locales (count (get-in @state [:config :supported-locales]))
     :translation-keys (reduce + (map count (vals (:translations @state))))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-localizer-stats
  "Get localizer statistics."
  []
  (merge (get-localizer-metrics)
         {:default-locale (get-in @state [:config :default-locale])
          :fallback-locale (get-in @state [:config :fallback-locale])
          :locale-header (get-in @state [:config :locale-header])}))

(defn reset-stats!
  "Reset localizer statistics."
  []
  (swap! state assoc :stats {:localizations 0
                             :translations 0
                             :format-operations 0
                             :locale-detections 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-localizer!
  "Initialize the response localizer."
  []
  (when-not (:initialized? @state)
    ;; Register default locales
    (register-locale! "en-US" {:name "English (US)" :language "en" :country "US"})
    (register-locale! "en-GB" {:name "English (UK)" :language "en" :country "GB"})
    (register-locale! "es-ES" {:name "Spanish (Spain)" :language "es" :country "ES"})
    (register-locale! "fr-FR" {:name "French (France)" :language "fr" :country "FR"})
    (register-locale! "de-DE" {:name "German (Germany)" :language "de" :country "DE"})
    (register-locale! "ja-JP" {:name "Japanese (Japan)" :language "ja" :country "JP"})
    (register-locale! "zh-CN" {:name "Chinese (Simplified)" :language "zh" :country "CN"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response localizer initialized")
    (events/emit! :response-localizer-initialized {})
    true))
