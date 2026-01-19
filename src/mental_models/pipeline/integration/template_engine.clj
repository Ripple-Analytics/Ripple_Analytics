(ns mental-models.pipeline.integration.template-engine
  "Template Engine Module
   
   Dynamic content generation:
   - Template registration
   - Variable interpolation
   - Conditional rendering
   - Loop constructs
   - Template inheritance"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; TEMPLATE STATE
;; =============================================================================

(defonce template-state (atom {:templates {}
                               :partials {}
                               :helpers {}
                               :cache {}
                               :config {:cache-enabled true
                                        :max-cache-size 1000}}))

;; =============================================================================
;; TEMPLATE REGISTRATION
;; =============================================================================

(defn register-template!
  "Register a template."
  [template-id template-str & {:keys [description]}]
  (log/info "Registering template" {:id template-id})
  (swap! template-state assoc-in [:templates template-id]
         {:id template-id
          :template template-str
          :description description
          :registered-at (System/currentTimeMillis)})
  template-id)

(defn get-template
  "Get a template by ID."
  [template-id]
  (get-in @template-state [:templates template-id :template]))

(defn list-templates
  "List all registered templates."
  []
  (keys (:templates @template-state)))

(defn unregister-template!
  "Unregister a template."
  [template-id]
  (log/info "Unregistering template" {:id template-id})
  (swap! template-state update :templates dissoc template-id)
  (swap! template-state update :cache dissoc template-id))

;; =============================================================================
;; PARTIALS
;; =============================================================================

(defn register-partial!
  "Register a partial template."
  [partial-id partial-str]
  (log/info "Registering partial" {:id partial-id})
  (swap! template-state assoc-in [:partials partial-id] partial-str)
  partial-id)

(defn get-partial
  "Get a partial by ID."
  [partial-id]
  (get-in @template-state [:partials partial-id]))

;; =============================================================================
;; HELPERS
;; =============================================================================

(defn register-helper!
  "Register a helper function."
  [helper-id helper-fn]
  (log/info "Registering helper" {:id helper-id})
  (swap! template-state assoc-in [:helpers helper-id] helper-fn)
  helper-id)

(defn get-helper
  "Get a helper by ID."
  [helper-id]
  (get-in @template-state [:helpers helper-id]))

;; Built-in helpers
(def built-in-helpers
  {:upper str/upper-case
   :lower str/lower-case
   :capitalize str/capitalize
   :trim str/trim
   :join (fn [coll sep] (str/join sep coll))
   :first first
   :last last
   :count count
   :default (fn [val default] (if (nil? val) default val))
   :date-format (fn [ms fmt]
                  (.format (java.text.SimpleDateFormat. fmt)
                           (java.util.Date. ms)))
   :number-format (fn [n fmt]
                    (format fmt n))
   :json (fn [val] (pr-str val))
   :escape-html (fn [s]
                  (-> s
                      (str/replace "&" "&amp;")
                      (str/replace "<" "&lt;")
                      (str/replace ">" "&gt;")
                      (str/replace "\"" "&quot;")))})

;; =============================================================================
;; VARIABLE RESOLUTION
;; =============================================================================

(defn resolve-path
  "Resolve a dotted path in context."
  [context path]
  (reduce (fn [ctx key]
            (cond
              (map? ctx) (get ctx (keyword key) (get ctx key))
              (sequential? ctx) (try (nth ctx (Integer/parseInt key)) (catch Exception _ nil))
              :else nil))
          context
          (str/split path #"\.")))

(defn resolve-variable
  "Resolve a variable expression."
  [context expr]
  (let [parts (str/split (str/trim expr) #"\|")
        var-path (str/trim (first parts))
        filters (rest parts)
        value (resolve-path context var-path)]
    (reduce (fn [val filter-expr]
              (let [filter-parts (str/split (str/trim filter-expr) #":")
                    filter-name (keyword (first filter-parts))
                    filter-args (rest filter-parts)
                    helper (or (get-helper filter-name)
                               (get built-in-helpers filter-name))]
                (if helper
                  (apply helper val filter-args)
                  val)))
            value
            filters)))

;; =============================================================================
;; TEMPLATE PARSING
;; =============================================================================

(defn parse-variable
  "Parse a variable expression {{ var }}."
  [template]
  (str/replace template #"\{\{\s*([^}]+)\s*\}\}"
               (fn [[_ expr]]
                 (str "{{VAR:" expr "}}"))))

(defn parse-conditional
  "Parse conditional blocks {% if %} {% endif %}."
  [template]
  (str/replace template #"\{%\s*if\s+([^%]+)\s*%\}(.*?)\{%\s*endif\s*%\}"
               (fn [[_ condition body]]
                 (str "{{IF:" condition ":" body "}}"))))

(defn parse-loop
  "Parse loop blocks {% for item in items %} {% endfor %}."
  [template]
  (str/replace template #"\{%\s*for\s+(\w+)\s+in\s+([^%]+)\s*%\}(.*?)\{%\s*endfor\s*%\}"
               (fn [[_ item-var collection body]]
                 (str "{{FOR:" item-var ":" collection ":" body "}}"))))

(defn parse-partial
  "Parse partial includes {% include 'partial' %}."
  [template]
  (str/replace template #"\{%\s*include\s+'([^']+)'\s*%\}"
               (fn [[_ partial-id]]
                 (str "{{PARTIAL:" partial-id "}}"))))

;; =============================================================================
;; TEMPLATE RENDERING
;; =============================================================================

(defn render-variable
  "Render a variable expression."
  [context expr]
  (let [value (resolve-variable context expr)]
    (if (nil? value) "" (str value))))

(defn render-conditional
  "Render a conditional block."
  [context condition body]
  (let [value (resolve-variable context condition)]
    (if value
      (render-template-str body context)
      "")))

(defn render-loop
  "Render a loop block."
  [context item-var collection body]
  (let [items (resolve-variable context collection)]
    (if (sequential? items)
      (str/join ""
                (map-indexed (fn [idx item]
                               (render-template-str body
                                                    (assoc context
                                                           (keyword item-var) item
                                                           :index idx
                                                           :first (= idx 0)
                                                           :last (= idx (dec (count items))))))
                             items))
      "")))

(defn render-partial
  "Render a partial template."
  [context partial-id]
  (if-let [partial (get-partial (keyword partial-id))]
    (render-template-str partial context)
    (do
      (log/warn "Partial not found" {:id partial-id})
      "")))

(defn render-template-str
  "Render a template string with context."
  [template context]
  (-> template
      ;; Parse and render partials first
      (str/replace #"\{\{PARTIAL:([^}]+)\}\}"
                   (fn [[_ partial-id]]
                     (render-partial context partial-id)))
      ;; Parse and render loops
      (str/replace #"\{\{FOR:([^:]+):([^:]+):([^}]+)\}\}"
                   (fn [[_ item-var collection body]]
                     (render-loop context item-var collection body)))
      ;; Parse and render conditionals
      (str/replace #"\{\{IF:([^:]+):([^}]+)\}\}"
                   (fn [[_ condition body]]
                     (render-conditional context condition body)))
      ;; Render variables
      (str/replace #"\{\{VAR:([^}]+)\}\}"
                   (fn [[_ expr]]
                     (render-variable context expr)))
      ;; Handle raw variable syntax
      (str/replace #"\{\{\s*([^}]+)\s*\}\}"
                   (fn [[_ expr]]
                     (render-variable context expr)))))

(defn preprocess-template
  "Preprocess a template for faster rendering."
  [template]
  (-> template
      parse-partial
      parse-loop
      parse-conditional
      parse-variable))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn render
  "Render a registered template with context."
  [template-id context]
  (when (flags/is-enabled? "template-engine")
    (log/debug "Rendering template" {:id template-id})
    (let [start-time (System/currentTimeMillis)
          cache-enabled (get-in @template-state [:config :cache-enabled])
          cached (when cache-enabled
                   (get-in @template-state [:cache template-id]))
          template (or cached
                       (when-let [t (get-template template-id)]
                         (let [preprocessed (preprocess-template t)]
                           (when cache-enabled
                             (swap! template-state assoc-in [:cache template-id] preprocessed))
                           preprocessed)))]
      (if-not template
        (do
          (log/error "Template not found" {:id template-id})
          nil)
        (let [result (render-template-str template context)
              duration (- (System/currentTimeMillis) start-time)]
          (metrics/inc-counter! :template/rendered)
          (metrics/observe-histogram! :template/render-time duration)
          result)))))

(defn render-string
  "Render a template string directly with context."
  [template-str context]
  (when (flags/is-enabled? "template-engine")
    (let [preprocessed (preprocess-template template-str)]
      (render-template-str preprocessed context))))

;; =============================================================================
;; PREDEFINED TEMPLATES
;; =============================================================================

(def analysis-report-template
  "# Analysis Report

## Document: {{ document.title }}
Analyzed at: {{ timestamp | date-format:yyyy-MM-dd HH:mm:ss }}

## Mental Models Detected
{% for model in models %}
### {{ model.name }}
- Confidence: {{ model.confidence | number-format:%.2f }}
- Category: {{ model.category }}
{% endfor %}

## Summary
Total models detected: {{ models | count }}
{% if lollapalooza %}
**Lollapalooza Effect Detected!**
{% endif %}")

(def alert-template
  "ALERT: {{ alert.type | upper }}
Severity: {{ alert.severity }}
Message: {{ alert.message }}
Time: {{ alert.timestamp | date-format:yyyy-MM-dd HH:mm:ss }}")

;; =============================================================================
;; CACHE MANAGEMENT
;; =============================================================================

(defn clear-cache!
  "Clear the template cache."
  []
  (log/info "Clearing template cache")
  (swap! template-state assoc :cache {}))

(defn invalidate-cache!
  "Invalidate cache for a specific template."
  [template-id]
  (log/info "Invalidating cache" {:id template-id})
  (swap! template-state update :cache dissoc template-id))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-template-engine!
  "Initialize template engine."
  []
  (log/info "Initializing template engine")
  ;; Register feature flag
  (flags/register-flag! "template-engine" "Enable template engine" true)
  ;; Create metrics
  (metrics/create-counter! :template/rendered "Templates rendered")
  (metrics/create-histogram! :template/render-time "Template render time" [1 5 10 50 100])
  (metrics/create-gauge! :template/cache-size "Template cache size"
                         #(count (:cache @template-state)))
  ;; Register built-in helpers
  (doseq [[id helper] built-in-helpers]
    (register-helper! id helper))
  ;; Register predefined templates
  (register-template! :analysis-report analysis-report-template
                      :description "Analysis report template")
  (register-template! :alert alert-template
                      :description "Alert notification template")
  (log/info "Template engine initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-engine-status []
  {:enabled (flags/is-enabled? "template-engine")
   :registered-templates (count (:templates @template-state))
   :registered-partials (count (:partials @template-state))
   :registered-helpers (count (:helpers @template-state))
   :cache-size (count (:cache @template-state))
   :config (:config @template-state)})
