(ns mental-models.pipeline.integration.localization
  "Localization Module
   
   Internationalization support:
   - Translation management
   - Locale detection
   - Pluralization
   - Date/number formatting
   - RTL support"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.text NumberFormat DecimalFormat]
   [java.util Locale Currency]
   [java.time.format DateTimeFormatter FormatStyle]
   [java.time LocalDateTime ZonedDateTime]))

;; =============================================================================
;; LOCALIZATION STATE
;; =============================================================================

(defonce l10n-state (atom {:translations {}
                           :current-locale :en
                           :fallback-locale :en
                           :supported-locales #{:en}
                           :rtl-locales #{:ar :he :fa :ur}}))

;; =============================================================================
;; LOCALE MANAGEMENT
;; =============================================================================

(defn set-locale!
  "Set the current locale."
  [locale]
  (log/info "Setting locale" {:locale locale})
  (swap! l10n-state assoc :current-locale locale)
  (events/publish! :l10n/locale-changed {:locale locale})
  locale)

(defn get-locale
  "Get the current locale."
  []
  (:current-locale @l10n-state))

(defn get-fallback-locale
  "Get the fallback locale."
  []
  (:fallback-locale @l10n-state))

(defn set-fallback-locale!
  "Set the fallback locale."
  [locale]
  (swap! l10n-state assoc :fallback-locale locale))

(defn add-supported-locale!
  "Add a supported locale."
  [locale]
  (swap! l10n-state update :supported-locales conj locale))

(defn is-supported?
  "Check if a locale is supported."
  [locale]
  (contains? (:supported-locales @l10n-state) locale))

(defn is-rtl?
  "Check if a locale is RTL."
  [locale]
  (contains? (:rtl-locales @l10n-state) locale))

(defn locale->java-locale
  "Convert keyword locale to Java Locale."
  [locale]
  (case locale
    :en Locale/ENGLISH
    :en-US Locale/US
    :en-GB Locale/UK
    :de Locale/GERMAN
    :fr Locale/FRENCH
    :es (Locale. "es")
    :it Locale/ITALIAN
    :ja Locale/JAPANESE
    :zh Locale/CHINESE
    :ko Locale/KOREAN
    :pt (Locale. "pt")
    :ru (Locale. "ru")
    :ar (Locale. "ar")
    Locale/ENGLISH))

;; =============================================================================
;; TRANSLATION MANAGEMENT
;; =============================================================================

(defn add-translations!
  "Add translations for a locale."
  [locale translations]
  (log/info "Adding translations" {:locale locale :count (count translations)})
  (swap! l10n-state update-in [:translations locale] merge translations)
  (add-supported-locale! locale)
  (metrics/inc-counter! :l10n/translations-added))

(defn get-translation
  "Get a translation for a key."
  [key & {:keys [locale] :or {locale (get-locale)}}]
  (or (get-in @l10n-state [:translations locale key])
      (get-in @l10n-state [:translations (get-fallback-locale) key])
      (name key)))

(defn t
  "Translate a key with optional interpolation."
  [key & {:keys [locale vars] :or {locale (get-locale) vars {}}}]
  (when (flags/is-enabled? "localization")
    (let [translation (get-translation key :locale locale)]
      (reduce (fn [s [k v]]
                (str/replace s (str "{{" (name k) "}}") (str v)))
              translation
              vars))))

;; =============================================================================
;; PLURALIZATION
;; =============================================================================

(defn pluralize
  "Get the correct plural form for a count."
  [key count & {:keys [locale] :or {locale (get-locale)}}]
  (let [plural-key (cond
                     (= count 0) (keyword (str (name key) ".zero"))
                     (= count 1) (keyword (str (name key) ".one"))
                     :else (keyword (str (name key) ".other")))]
    (t plural-key :locale locale :vars {:count count})))

;; =============================================================================
;; NUMBER FORMATTING
;; =============================================================================

(defn format-number
  "Format a number according to locale."
  [n & {:keys [locale] :or {locale (get-locale)}}]
  (let [java-locale (locale->java-locale locale)
        formatter (NumberFormat/getNumberInstance java-locale)]
    (.format formatter n)))

(defn format-currency
  "Format a currency amount."
  [amount currency-code & {:keys [locale] :or {locale (get-locale)}}]
  (let [java-locale (locale->java-locale locale)
        formatter (NumberFormat/getCurrencyInstance java-locale)]
    (.setCurrency formatter (Currency/getInstance currency-code))
    (.format formatter amount)))

(defn format-percent
  "Format a percentage."
  [n & {:keys [locale] :or {locale (get-locale)}}]
  (let [java-locale (locale->java-locale locale)
        formatter (NumberFormat/getPercentInstance java-locale)]
    (.format formatter n)))

(defn format-decimal
  "Format a decimal with specific precision."
  [n precision & {:keys [locale] :or {locale (get-locale)}}]
  (let [pattern (str "#,##0." (apply str (repeat precision "0")))
        formatter (DecimalFormat. pattern)]
    (.format formatter n)))

;; =============================================================================
;; DATE/TIME FORMATTING
;; =============================================================================

(defn format-date
  "Format a date according to locale."
  [date & {:keys [locale style] :or {locale (get-locale) style :medium}}]
  (let [java-locale (locale->java-locale locale)
        format-style (case style
                       :short FormatStyle/SHORT
                       :medium FormatStyle/MEDIUM
                       :long FormatStyle/LONG
                       :full FormatStyle/FULL
                       FormatStyle/MEDIUM)
        formatter (.withLocale (DateTimeFormatter/ofLocalizedDate format-style) java-locale)]
    (.format formatter date)))

(defn format-time
  "Format a time according to locale."
  [time & {:keys [locale style] :or {locale (get-locale) style :medium}}]
  (let [java-locale (locale->java-locale locale)
        format-style (case style
                       :short FormatStyle/SHORT
                       :medium FormatStyle/MEDIUM
                       :long FormatStyle/LONG
                       FormatStyle/MEDIUM)
        formatter (.withLocale (DateTimeFormatter/ofLocalizedTime format-style) java-locale)]
    (.format formatter time)))

(defn format-datetime
  "Format a datetime according to locale."
  [datetime & {:keys [locale date-style time-style]
               :or {locale (get-locale) date-style :medium time-style :medium}}]
  (let [java-locale (locale->java-locale locale)
        date-format-style (case date-style
                            :short FormatStyle/SHORT
                            :medium FormatStyle/MEDIUM
                            :long FormatStyle/LONG
                            :full FormatStyle/FULL
                            FormatStyle/MEDIUM)
        time-format-style (case time-style
                            :short FormatStyle/SHORT
                            :medium FormatStyle/MEDIUM
                            :long FormatStyle/LONG
                            FormatStyle/MEDIUM)
        formatter (.withLocale (DateTimeFormatter/ofLocalizedDateTime date-format-style time-format-style)
                               java-locale)]
    (.format formatter datetime)))

(defn format-relative-time
  "Format a relative time (e.g., '5 minutes ago')."
  [ms-diff & {:keys [locale] :or {locale (get-locale)}}]
  (let [abs-diff (Math/abs ms-diff)
        future? (pos? ms-diff)
        seconds (/ abs-diff 1000)
        minutes (/ seconds 60)
        hours (/ minutes 60)
        days (/ hours 24)]
    (cond
      (< seconds 60) (t (if future? :time.seconds-from-now :time.seconds-ago)
                        :locale locale :vars {:count (int seconds)})
      (< minutes 60) (t (if future? :time.minutes-from-now :time.minutes-ago)
                        :locale locale :vars {:count (int minutes)})
      (< hours 24) (t (if future? :time.hours-from-now :time.hours-ago)
                      :locale locale :vars {:count (int hours)})
      :else (t (if future? :time.days-from-now :time.days-ago)
               :locale locale :vars {:count (int days)}))))

;; =============================================================================
;; DEFAULT TRANSLATIONS
;; =============================================================================

(def english-translations
  {:app.name "Mental Models System"
   :app.tagline "Analyze with Charlie Munger's Mental Models"
   :analysis.title "Analysis Results"
   :analysis.models-detected "Models Detected"
   :analysis.confidence "Confidence"
   :analysis.lollapalooza "Lollapalooza Effect!"
   :alert.critical "Critical Alert"
   :alert.warning "Warning"
   :alert.info "Information"
   :time.seconds-ago "{{count}} seconds ago"
   :time.minutes-ago "{{count}} minutes ago"
   :time.hours-ago "{{count}} hours ago"
   :time.days-ago "{{count}} days ago"
   :time.seconds-from-now "in {{count}} seconds"
   :time.minutes-from-now "in {{count}} minutes"
   :time.hours-from-now "in {{count}} hours"
   :time.days-from-now "in {{count}} days"
   :models.zero "No models"
   :models.one "1 model"
   :models.other "{{count}} models"})

(def german-translations
  {:app.name "Mental Models System"
   :app.tagline "Analysieren mit Charlie Mungers Mental Models"
   :analysis.title "Analyseergebnisse"
   :analysis.models-detected "Erkannte Modelle"
   :analysis.confidence "Konfidenz"
   :analysis.lollapalooza "Lollapalooza-Effekt!"
   :alert.critical "Kritische Warnung"
   :alert.warning "Warnung"
   :alert.info "Information"
   :time.seconds-ago "vor {{count}} Sekunden"
   :time.minutes-ago "vor {{count}} Minuten"
   :time.hours-ago "vor {{count}} Stunden"
   :time.days-ago "vor {{count}} Tagen"})

;; =============================================================================
;; LOCALE DETECTION
;; =============================================================================

(defn detect-locale-from-header
  "Detect locale from Accept-Language header."
  [accept-language]
  (when accept-language
    (let [parts (str/split accept-language #",")
          first-lang (first parts)
          lang-code (first (str/split (str/trim first-lang) #";"))]
      (keyword (str/lower-case (str/replace lang-code #"-" "-"))))))

(defn detect-locale-from-user
  "Detect locale from user preferences."
  [user]
  (when-let [pref (:locale user)]
    (keyword pref)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-localization!
  "Initialize localization system."
  []
  (log/info "Initializing localization")
  ;; Register feature flag
  (flags/register-flag! "localization" "Enable localization" true)
  ;; Create metrics
  (metrics/create-counter! :l10n/translations-added "Translations added")
  (metrics/create-gauge! :l10n/supported-locales "Supported locales"
                         #(count (:supported-locales @l10n-state)))
  ;; Add default translations
  (add-translations! :en english-translations)
  (add-translations! :de german-translations)
  (log/info "Localization initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-l10n-status []
  {:enabled (flags/is-enabled? "localization")
   :current-locale (get-locale)
   :fallback-locale (get-fallback-locale)
   :supported-locales (:supported-locales @l10n-state)
   :translation-counts (into {} (map (fn [[k v]] [k (count v)])
                                     (:translations @l10n-state)))})
