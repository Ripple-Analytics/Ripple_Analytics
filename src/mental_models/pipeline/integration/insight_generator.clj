(ns mental-models.pipeline.integration.insight-generator
  "Insight Generator Module
   
   Actionable insight generation:
   - Pattern-based insights
   - Trend analysis
   - Anomaly detection
   - Recommendation synthesis
   - Insight prioritization"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; INSIGHT GENERATOR STATE
;; =============================================================================

(defonce generator-state (atom {:insights (ConcurrentHashMap.)
                                :insight-rules (ConcurrentHashMap.)
                                :insight-templates (ConcurrentHashMap.)
                                :feedback (ConcurrentHashMap.)
                                :insight-count (AtomicLong. 0)
                                :config {:min-confidence 0.5
                                         :max-insights-per-analysis 10
                                         :insight-ttl-hours 168}}))

;; =============================================================================
;; INSIGHT RULES
;; =============================================================================

(defn register-insight-rule!
  "Register an insight generation rule."
  [rule-id {:keys [name description trigger-conditions generator priority category]}]
  (log/info "Registering insight rule" {:id rule-id :name name})
  (.put ^ConcurrentHashMap (:insight-rules @generator-state) rule-id
        {:id rule-id
         :name name
         :description description
         :trigger-conditions trigger-conditions
         :generator generator
         :priority (or priority 5)
         :category (or category :general)
         :enabled true
         :trigger-count (AtomicLong. 0)
         :created-at (System/currentTimeMillis)}))

(defn get-insight-rule
  "Get an insight rule by ID."
  [rule-id]
  (.get ^ConcurrentHashMap (:insight-rules @generator-state) rule-id))

(defn list-insight-rules
  "List all insight rules."
  [& {:keys [category enabled-only]}]
  (let [rules (vals (:insight-rules @generator-state))]
    (cond->> rules
      enabled-only (filter :enabled)
      category (filter #(= (:category %) category)))))

(defn enable-rule!
  "Enable an insight rule."
  [rule-id]
  (when-let [rule (get-insight-rule rule-id)]
    (.put ^ConcurrentHashMap (:insight-rules @generator-state) rule-id
          (assoc rule :enabled true))))

(defn disable-rule!
  "Disable an insight rule."
  [rule-id]
  (when-let [rule (get-insight-rule rule-id)]
    (.put ^ConcurrentHashMap (:insight-rules @generator-state) rule-id
          (assoc rule :enabled false))))

;; =============================================================================
;; INSIGHT TEMPLATES
;; =============================================================================

(defn register-template!
  "Register an insight template."
  [template-id {:keys [name pattern variables default-priority]}]
  (log/debug "Registering insight template" {:id template-id :name name})
  (.put ^ConcurrentHashMap (:insight-templates @generator-state) template-id
        {:id template-id
         :name name
         :pattern pattern
         :variables (or variables [])
         :default-priority (or default-priority 5)}))

(defn get-template
  "Get an insight template."
  [template-id]
  (.get ^ConcurrentHashMap (:insight-templates @generator-state) template-id))

(defn render-template
  "Render an insight template with variables."
  [template-id bindings]
  (when-let [template (get-template template-id)]
    (reduce (fn [text [k v]]
              (str/replace text (str "{{" (name k) "}}") (str v)))
            (:pattern template)
            bindings)))

;; =============================================================================
;; INSIGHT GENERATION
;; =============================================================================

(defn check-trigger-conditions
  "Check if trigger conditions are met."
  [conditions context]
  (every? (fn [condition]
            (cond
              (fn? condition) (condition context)
              (keyword? condition) (get context condition)
              (map? condition) (let [{:keys [field operator value]} condition
                                     field-value (get context field)]
                                 (case operator
                                   := (= field-value value)
                                   :> (> field-value value)
                                   :< (< field-value value)
                                   :>= (>= field-value value)
                                   :<= (<= field-value value)
                                   :contains (and (coll? field-value) (contains? (set field-value) value))
                                   :in (contains? (set value) field-value)
                                   false))
              :else true))
          conditions))

(defn generate-insight-from-rule
  "Generate an insight from a rule."
  [rule context]
  (when (and (:enabled rule)
             (check-trigger-conditions (:trigger-conditions rule) context))
    (.incrementAndGet ^AtomicLong (:trigger-count rule))
    (let [generator (:generator rule)
          insight-data (if (fn? generator)
                         (generator context)
                         {:title (:name rule)
                          :description (:description rule)})]
      (when insight-data
        (merge insight-data
               {:rule-id (:id rule)
                :category (:category rule)
                :priority (:priority rule)})))))

(defn generate-insights!
  "Generate insights from analysis context."
  [analysis-id context]
  (.incrementAndGet ^AtomicLong (:insight-count @generator-state))
  (metrics/inc-counter! :insightgenerator/generations)
  (let [rules (list-insight-rules :enabled-only true)
        min-confidence (get-in @generator-state [:config :min-confidence])
        max-insights (get-in @generator-state [:config :max-insights-per-analysis])
        raw-insights (keep #(generate-insight-from-rule % context) rules)
        filtered (filter #(>= (get % :confidence 1.0) min-confidence) raw-insights)
        sorted (sort-by :priority filtered)
        insights (take max-insights sorted)]
    ;; Store insights
    (doseq [insight insights]
      (let [insight-id (str analysis-id "-" (:rule-id insight) "-" (System/currentTimeMillis))
            full-insight (assoc insight
                                :id insight-id
                                :analysis-id analysis-id
                                :generated-at (System/currentTimeMillis)
                                :status :new)]
        (.put ^ConcurrentHashMap (:insights @generator-state) insight-id full-insight)))
    (log/info "Insights generated" {:analysis analysis-id :count (count insights)})
    (events/publish! :insights/generated {:analysis-id analysis-id :count (count insights)})
    insights))

;; =============================================================================
;; PATTERN-BASED INSIGHTS
;; =============================================================================

(defn detect-trend
  "Detect trend in a series of values."
  [values]
  (when (>= (count values) 3)
    (let [diffs (map - (rest values) values)
          avg-diff (/ (reduce + diffs) (count diffs))]
      (cond
        (> avg-diff 0.1) :increasing
        (< avg-diff -0.1) :decreasing
        :else :stable))))

(defn detect-anomaly
  "Detect anomaly in values."
  [values threshold]
  (when (>= (count values) 5)
    (let [mean (/ (reduce + values) (count values))
          variance (/ (reduce + (map #(Math/pow (- % mean) 2) values)) (count values))
          std-dev (Math/sqrt variance)
          latest (last values)]
      (when (> (Math/abs (- latest mean)) (* threshold std-dev))
        {:is-anomaly true
         :value latest
         :mean mean
         :std-dev std-dev
         :deviation (/ (Math/abs (- latest mean)) std-dev)}))))

(defn generate-trend-insight
  "Generate insight from trend detection."
  [metric-name values]
  (let [trend (detect-trend values)]
    (when (not= trend :stable)
      {:type :trend
       :title (str metric-name " " (name trend))
       :description (str "The " metric-name " metric is " (name trend)
                         " over the last " (count values) " observations")
       :trend trend
       :confidence 0.7
       :priority (if (= trend :decreasing) 3 5)})))

(defn generate-anomaly-insight
  "Generate insight from anomaly detection."
  [metric-name values]
  (when-let [anomaly (detect-anomaly values 2.0)]
    {:type :anomaly
     :title (str "Anomaly detected in " metric-name)
     :description (str "Unusual value detected: " (:value anomaly)
                       " (mean: " (format "%.2f" (:mean anomaly))
                       ", deviation: " (format "%.1f" (:deviation anomaly)) " std)")
     :anomaly anomaly
     :confidence 0.8
     :priority 2}))

;; =============================================================================
;; MENTAL MODEL INSIGHTS
;; =============================================================================

(defn generate-bias-insight
  "Generate insight from bias detection."
  [biases]
  (when (seq biases)
    (let [high-severity (filter #(= (:severity %) :high) biases)
          primary-bias (first (sort-by :confidence > biases))]
      {:type :bias-detection
       :title (str "Cognitive bias detected: " (:bias-name primary-bias))
       :description (str (count biases) " cognitive biases detected. "
                         (when (seq high-severity)
                           (str (count high-severity) " are high severity. "))
                         "Consider debiasing strategies.")
       :biases (map :bias-id biases)
       :debiasing-tips (:debiasing-tips primary-bias)
       :confidence (:confidence primary-bias)
       :priority (if (seq high-severity) 1 3)})))

(defn generate-lollapalooza-insight
  "Generate insight from lollapalooza detection."
  [lollapalooza-event]
  (when lollapalooza-event
    {:type :lollapalooza
     :title "Lollapalooza Effect Detected!"
     :description (str (:model-count lollapalooza-event) " mental models converging. "
                       "This combination can lead to extreme outcomes. "
                       "Exercise extra caution.")
     :models (:models lollapalooza-event)
     :synergy (:synergy lollapalooza-event)
     :confidence (:final-score lollapalooza-event)
     :priority 1}))

(defn generate-model-recommendation-insight
  "Generate insight recommending mental models to apply."
  [context detected-models]
  (let [all-models #{"second-order-thinking" "inversion" "circle-of-competence"
                     "margin-of-safety" "incentives" "opportunity-cost"}
        missing-models (set/difference all-models (set detected-models))]
    (when (seq missing-models)
      {:type :recommendation
       :title "Consider additional mental models"
       :description (str "You might benefit from applying: "
                         (str/join ", " (take 3 missing-models)))
       :recommended-models (take 3 missing-models)
       :confidence 0.6
       :priority 4})))

;; =============================================================================
;; INSIGHT RETRIEVAL
;; =============================================================================

(defn get-insight
  "Get an insight by ID."
  [insight-id]
  (.get ^ConcurrentHashMap (:insights @generator-state) insight-id))

(defn list-insights
  "List insights."
  [& {:keys [analysis-id category status priority-max limit since]}]
  (let [insights (vals (:insights @generator-state))]
    (cond->> insights
      analysis-id (filter #(= (:analysis-id %) analysis-id))
      category (filter #(= (:category %) category))
      status (filter #(= (:status %) status))
      priority-max (filter #(<= (:priority %) priority-max))
      since (filter #(>= (:generated-at %) since))
      true (sort-by (juxt :priority :generated-at))
      limit (take limit))))

(defn update-insight-status!
  "Update insight status."
  [insight-id status]
  (when-let [insight (get-insight insight-id)]
    (let [updated (assoc insight :status status :updated-at (System/currentTimeMillis))]
      (.put ^ConcurrentHashMap (:insights @generator-state) insight-id updated)
      updated)))

;; =============================================================================
;; FEEDBACK
;; =============================================================================

(defn record-feedback!
  "Record feedback on an insight."
  [insight-id {:keys [helpful actionable rating notes user-id]}]
  (let [feedback-id (str insight-id "-feedback")
        feedback {:id feedback-id
                  :insight-id insight-id
                  :helpful helpful
                  :actionable actionable
                  :rating rating
                  :notes notes
                  :user-id user-id
                  :recorded-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:feedback @generator-state) feedback-id feedback)
    (log/info "Feedback recorded" {:insight insight-id :helpful helpful})
    feedback))

(defn get-feedback-stats
  "Get feedback statistics."
  []
  (let [feedback (vals (:feedback @generator-state))
        helpful-count (count (filter :helpful feedback))
        actionable-count (count (filter :actionable feedback))
        total (count feedback)]
    {:total-feedback total
     :helpful-rate (if (pos? total) (/ helpful-count total) 0)
     :actionable-rate (if (pos? total) (/ actionable-count total) 0)
     :avg-rating (if (pos? total)
                   (/ (reduce + (keep :rating feedback)) total)
                   0)}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-generator-stats
  "Get insight generator statistics."
  []
  {:insights (.size ^ConcurrentHashMap (:insights @generator-state))
   :insight-rules (.size ^ConcurrentHashMap (:insight-rules @generator-state))
   :templates (.size ^ConcurrentHashMap (:insight-templates @generator-state))
   :feedback (.size ^ConcurrentHashMap (:feedback @generator-state))
   :insight-count (.get ^AtomicLong (:insight-count @generator-state))
   :feedback-stats (get-feedback-stats)
   :insights-by-type (frequencies (map :type (vals (:insights @generator-state))))
   :insights-by-priority (frequencies (map :priority (vals (:insights @generator-state))))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-default-rules!
  "Initialize default insight rules."
  []
  (log/info "Initializing default insight rules")
  ;; High Bias Count Rule
  (register-insight-rule! "high-bias-count"
                          {:name "High Bias Count Alert"
                           :description "Alert when multiple biases are detected"
                           :trigger-conditions [{:field :bias-count :operator :>= :value 3}]
                           :generator (fn [ctx]
                                        {:title "Multiple cognitive biases detected"
                                         :description (str (:bias-count ctx) " biases found. Review carefully.")
                                         :confidence 0.8})
                           :priority 2
                           :category :bias})
  ;; Low Confidence Rule
  (register-insight-rule! "low-confidence"
                          {:name "Low Confidence Warning"
                           :description "Warn when analysis confidence is low"
                           :trigger-conditions [{:field :avg-confidence :operator :< :value 0.5}]
                           :generator (fn [ctx]
                                        {:title "Low confidence analysis"
                                         :description "Consider gathering more data or context."
                                         :confidence 0.6})
                           :priority 4
                           :category :quality})
  ;; First-Time Model Rule
  (register-insight-rule! "first-time-model"
                          {:name "New Model Detection"
                           :description "Note when a model is detected for the first time"
                           :trigger-conditions [{:field :new-models :operator :contains :value true}]
                           :generator (fn [ctx]
                                        {:title "New mental model detected"
                                         :description "A mental model was detected for the first time in this context."
                                         :confidence 0.7})
                           :priority 5
                           :category :discovery})
  (log/info "Default insight rules initialized" {:count 3}))

(defn init-default-templates!
  "Initialize default insight templates."
  []
  (log/info "Initializing default insight templates")
  (register-template! "bias-detected"
                      {:name "Bias Detection Template"
                       :pattern "{{bias_name}} bias detected with {{confidence}}% confidence. {{debiasing_tip}}"
                       :variables [:bias_name :confidence :debiasing_tip]})
  (register-template! "trend-alert"
                      {:name "Trend Alert Template"
                       :pattern "{{metric}} is {{direction}} over the last {{period}}. Current value: {{value}}"
                       :variables [:metric :direction :period :value]})
  (register-template! "recommendation"
                      {:name "Recommendation Template"
                       :pattern "Consider applying {{model}} to gain insight into {{context}}"
                       :variables [:model :context]})
  (log/info "Default insight templates initialized" {:count 3}))

(defn init-insight-generator!
  "Initialize insight generator."
  []
  (log/info "Initializing insight generator")
  ;; Register feature flag
  (flags/register-flag! "insight-generator" "Enable insight generator" true)
  ;; Create metrics
  (metrics/create-counter! :insightgenerator/generations "Insight generations performed")
  (metrics/create-gauge! :insightgenerator/insights "Total insights"
                         #(.size ^ConcurrentHashMap (:insights @generator-state)))
  ;; Initialize defaults
  (init-default-rules!)
  (init-default-templates!)
  (log/info "Insight generator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-insight-generator-status []
  {:enabled (flags/is-enabled? "insight-generator")
   :insights (.size ^ConcurrentHashMap (:insights @generator-state))
   :insight-rules (.size ^ConcurrentHashMap (:insight-rules @generator-state))
   :templates (.size ^ConcurrentHashMap (:insight-templates @generator-state))
   :stats (get-generator-stats)
   :config (:config @generator-state)})
