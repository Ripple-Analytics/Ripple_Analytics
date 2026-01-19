(ns mental-models.pipeline.integration.lollapalooza-detector
  "Lollapalooza Detector Module
   
   Lollapalooza effect detection:
   - Multi-model convergence detection
   - Synergy scoring
   - Historical pattern matching
   - Alert generation
   - Trend analysis"
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
;; LOLLAPALOOZA DETECTOR STATE
;; =============================================================================

(defonce detector-state (atom {:model-detections (ConcurrentHashMap.)
                               :lollapalooza-events (ConcurrentHashMap.)
                               :synergy-rules (ConcurrentHashMap.)
                               :alerts (ConcurrentHashMap.)
                               :historical-patterns (ConcurrentHashMap.)
                               :detection-count (AtomicLong. 0)
                               :config {:min-models 3
                                        :min-confidence 0.7
                                        :synergy-threshold 0.6
                                        :alert-cooldown-ms 300000}}))

;; =============================================================================
;; MODEL DETECTION TRACKING
;; =============================================================================

(defn record-model-detection!
  "Record a mental model detection."
  [detection-id {:keys [model-id model-name confidence context source timestamp]}]
  (let [detection {:id detection-id
                   :model-id model-id
                   :model-name model-name
                   :confidence (or confidence 0.5)
                   :context context
                   :source source
                   :timestamp (or timestamp (System/currentTimeMillis))}]
    (.put ^ConcurrentHashMap (:model-detections @detector-state) detection-id detection)
    detection))

(defn get-recent-detections
  "Get recent model detections within a time window."
  [& {:keys [source since limit]}]
  (let [now (System/currentTimeMillis)
        since-time (or since (- now 3600000))
        detections (vals (:model-detections @detector-state))]
    (cond->> detections
      true (filter #(>= (:timestamp %) since-time))
      source (filter #(= (:source %) source))
      true (sort-by :timestamp >)
      limit (take limit))))

(defn get-detections-by-source
  "Get all detections for a specific source."
  [source]
  (filter #(= (:source %) source)
          (vals (:model-detections @detector-state))))

;; =============================================================================
;; SYNERGY RULES
;; =============================================================================

(defn register-synergy-rule!
  "Register a synergy rule for model combinations."
  [rule-id {:keys [name description models synergy-score amplification-factor]}]
  (log/info "Registering synergy rule" {:id rule-id :name name})
  (.put ^ConcurrentHashMap (:synergy-rules @detector-state) rule-id
        {:id rule-id
         :name name
         :description description
         :models (set models)
         :synergy-score (or synergy-score 1.0)
         :amplification-factor (or amplification-factor 1.5)
         :trigger-count (AtomicLong. 0)
         :created-at (System/currentTimeMillis)}))

(defn get-synergy-rule
  "Get a synergy rule by ID."
  [rule-id]
  (.get ^ConcurrentHashMap (:synergy-rules @detector-state) rule-id))

(defn list-synergy-rules
  "List all synergy rules."
  []
  (vals (:synergy-rules @detector-state)))

(defn find-matching-synergy-rules
  "Find synergy rules that match a set of models."
  [model-ids]
  (let [model-set (set model-ids)]
    (filter (fn [rule]
              (set/subset? (:models rule) model-set))
            (list-synergy-rules))))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn calculate-convergence-score
  "Calculate convergence score for a set of model detections."
  [detections]
  (let [confidences (map :confidence detections)
        avg-confidence (/ (reduce + confidences) (count confidences))
        model-count (count (distinct (map :model-id detections)))
        time-spread (- (apply max (map :timestamp detections))
                       (apply min (map :timestamp detections)))
        time-factor (if (< time-spread 60000) 1.2 1.0)]
    (* avg-confidence
       (Math/log (inc model-count))
       time-factor)))

(defn calculate-synergy-bonus
  "Calculate synergy bonus from matching rules."
  [model-ids]
  (let [matching-rules (find-matching-synergy-rules model-ids)]
    (if (seq matching-rules)
      (let [total-synergy (reduce + (map :synergy-score matching-rules))
            max-amplification (apply max (map :amplification-factor matching-rules))]
        ;; Increment trigger counts
        (doseq [rule matching-rules]
          (.incrementAndGet ^AtomicLong (:trigger-count rule)))
        {:synergy-score total-synergy
         :amplification max-amplification
         :rules-triggered (count matching-rules)
         :rule-names (map :name matching-rules)})
      {:synergy-score 0 :amplification 1.0 :rules-triggered 0 :rule-names []})))

(defn detect-lollapalooza
  "Detect lollapalooza effect in a set of detections."
  [detections]
  (let [min-models (get-in @detector-state [:config :min-models])
        min-confidence (get-in @detector-state [:config :min-confidence])
        model-ids (distinct (map :model-id detections))
        model-count (count model-ids)]
    (when (>= model-count min-models)
      (let [convergence-score (calculate-convergence-score detections)
            synergy (calculate-synergy-bonus model-ids)
            final-score (* convergence-score (:amplification synergy))]
        (when (>= final-score min-confidence)
          {:is-lollapalooza true
           :model-count model-count
           :models model-ids
           :convergence-score convergence-score
           :synergy synergy
           :final-score final-score
           :detections detections})))))

(defn analyze-source-for-lollapalooza!
  "Analyze a source for lollapalooza effects."
  [source]
  (.incrementAndGet ^AtomicLong (:detection-count @detector-state))
  (metrics/inc-counter! :lollapaloozadetector/analyses)
  (let [detections (get-detections-by-source source)
        result (detect-lollapalooza detections)]
    (when result
      (let [event-id (str source "-" (System/currentTimeMillis))
            event (assoc result
                         :id event-id
                         :source source
                         :detected-at (System/currentTimeMillis))]
        (.put ^ConcurrentHashMap (:lollapalooza-events @detector-state) event-id event)
        (log/warn "Lollapalooza effect detected!" {:source source
                                                   :models (:model-count result)
                                                   :score (:final-score result)})
        (events/publish! :lollapalooza/detected event)
        event))))

;; =============================================================================
;; REAL-TIME MONITORING
;; =============================================================================

(defn check-for-lollapalooza!
  "Check if adding a new detection triggers a lollapalooza."
  [new-detection]
  (let [source (:source new-detection)
        existing (get-detections-by-source source)
        all-detections (conj existing new-detection)
        result (detect-lollapalooza all-detections)]
    (when result
      (let [event-id (str source "-" (System/currentTimeMillis))]
        ;; Check cooldown
        (let [recent-events (filter #(and (= (:source %) source)
                                          (> (:detected-at %)
                                             (- (System/currentTimeMillis)
                                                (get-in @detector-state [:config :alert-cooldown-ms]))))
                                    (vals (:lollapalooza-events @detector-state)))]
          (when (empty? recent-events)
            (let [event (assoc result
                               :id event-id
                               :source source
                               :trigger-detection (:id new-detection)
                               :detected-at (System/currentTimeMillis))]
              (.put ^ConcurrentHashMap (:lollapalooza-events @detector-state) event-id event)
              (log/warn "Lollapalooza triggered by new detection!" {:source source
                                                                    :trigger (:model-id new-detection)
                                                                    :total-models (:model-count result)})
              (events/publish! :lollapalooza/triggered event)
              event)))))))

;; =============================================================================
;; ALERTS
;; =============================================================================

(defn create-alert!
  "Create an alert for a lollapalooza event."
  [event {:keys [severity channels message]}]
  (let [alert-id (str "alert-" (:id event))
        alert {:id alert-id
               :event-id (:id event)
               :source (:source event)
               :severity (or severity :high)
               :channels (or channels [:web :email])
               :message (or message (str "Lollapalooza effect detected with "
                                         (:model-count event) " converging models"))
               :models (:models event)
               :score (:final-score event)
               :status :pending
               :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:alerts @detector-state) alert-id alert)
    (log/info "Alert created" {:id alert-id :severity severity})
    (events/publish! :lollapalooza/alert-created alert)
    alert))

(defn get-alert
  "Get an alert by ID."
  [alert-id]
  (.get ^ConcurrentHashMap (:alerts @detector-state) alert-id))

(defn list-alerts
  "List alerts."
  [& {:keys [status severity limit since]}]
  (let [alerts (vals (:alerts @detector-state))]
    (cond->> alerts
      status (filter #(= (:status %) status))
      severity (filter #(= (:severity %) severity))
      since (filter #(>= (:created-at %) since))
      true (sort-by :created-at >)
      limit (take limit))))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id {:keys [acknowledged-by notes]}]
  (when-let [alert (get-alert alert-id)]
    (let [updated (assoc alert
                         :status :acknowledged
                         :acknowledged-by acknowledged-by
                         :acknowledged-at (System/currentTimeMillis)
                         :notes notes)]
      (.put ^ConcurrentHashMap (:alerts @detector-state) alert-id updated)
      updated)))

;; =============================================================================
;; HISTORICAL PATTERNS
;; =============================================================================

(defn record-historical-pattern!
  "Record a historical lollapalooza pattern."
  [pattern-id {:keys [models outcome context frequency]}]
  (.put ^ConcurrentHashMap (:historical-patterns @detector-state) pattern-id
        {:id pattern-id
         :models (set models)
         :outcome outcome
         :context context
         :frequency (or frequency 1)
         :first-seen (System/currentTimeMillis)
         :last-seen (System/currentTimeMillis)}))

(defn find-similar-patterns
  "Find historical patterns similar to current detection."
  [model-ids]
  (let [model-set (set model-ids)
        patterns (vals (:historical-patterns @detector-state))]
    (filter (fn [pattern]
              (let [overlap (set/intersection model-set (:models pattern))
                    similarity (/ (count overlap)
                                  (max (count model-set) (count (:models pattern))))]
                (>= similarity 0.6)))
            patterns)))

(defn get-pattern-insights
  "Get insights from historical patterns."
  [model-ids]
  (let [similar (find-similar-patterns model-ids)]
    (when (seq similar)
      {:similar-patterns (count similar)
       :common-outcomes (frequencies (map :outcome similar))
       :total-occurrences (reduce + (map :frequency similar))
       :patterns similar})))

;; =============================================================================
;; LOLLAPALOOZA EVENTS
;; =============================================================================

(defn get-lollapalooza-event
  "Get a lollapalooza event by ID."
  [event-id]
  (.get ^ConcurrentHashMap (:lollapalooza-events @detector-state) event-id))

(defn list-lollapalooza-events
  "List lollapalooza events."
  [& {:keys [source min-score limit since]}]
  (let [events (vals (:lollapalooza-events @detector-state))]
    (cond->> events
      source (filter #(= (:source %) source))
      min-score (filter #(>= (:final-score %) min-score))
      since (filter #(>= (:detected-at %) since))
      true (sort-by :detected-at >)
      limit (take limit))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-detector-stats
  "Get detector statistics."
  []
  {:model-detections (.size ^ConcurrentHashMap (:model-detections @detector-state))
   :lollapalooza-events (.size ^ConcurrentHashMap (:lollapalooza-events @detector-state))
   :synergy-rules (.size ^ConcurrentHashMap (:synergy-rules @detector-state))
   :alerts (.size ^ConcurrentHashMap (:alerts @detector-state))
   :historical-patterns (.size ^ConcurrentHashMap (:historical-patterns @detector-state))
   :detection-count (.get ^AtomicLong (:detection-count @detector-state))
   :top-model-combinations (->> (vals (:lollapalooza-events @detector-state))
                                (map :models)
                                frequencies
                                (sort-by val >)
                                (take 5))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-synergy-rules!
  "Initialize default synergy rules."
  []
  (log/info "Initializing synergy rules")
  ;; Bias Cascade
  (register-synergy-rule! "bias-cascade"
                          {:name "Bias Cascade"
                           :description "Multiple cognitive biases reinforcing each other"
                           :models #{"confirmation-bias" "availability-heuristic" "anchoring"}
                           :synergy-score 1.5
                           :amplification-factor 2.0})
  ;; Fear Spiral
  (register-synergy-rule! "fear-spiral"
                          {:name "Fear Spiral"
                           :description "Loss aversion combined with social proof"
                           :models #{"loss-aversion" "social-proof" "authority-bias"}
                           :synergy-score 1.8
                           :amplification-factor 2.5})
  ;; Commitment Trap
  (register-synergy-rule! "commitment-trap"
                          {:name "Commitment Trap"
                           :description "Sunk cost combined with consistency bias"
                           :models #{"sunk-cost" "consistency-bias" "ego-protection"}
                           :synergy-score 1.6
                           :amplification-factor 2.2})
  ;; Overconfidence Cluster
  (register-synergy-rule! "overconfidence-cluster"
                          {:name "Overconfidence Cluster"
                           :description "Multiple factors leading to overconfidence"
                           :models #{"hindsight-bias" "self-serving-bias" "dunning-kruger"}
                           :synergy-score 1.7
                           :amplification-factor 2.3})
  ;; Munger's Classic
  (register-synergy-rule! "munger-classic"
                          {:name "Munger's Classic Lollapalooza"
                           :description "The classic combination Munger warns about"
                           :models #{"incentives" "social-proof" "commitment-consistency" "reciprocation"}
                           :synergy-score 2.0
                           :amplification-factor 3.0})
  (log/info "Synergy rules initialized" {:count 5}))

(defn init-lollapalooza-detector!
  "Initialize lollapalooza detector."
  []
  (log/info "Initializing lollapalooza detector")
  ;; Register feature flag
  (flags/register-flag! "lollapalooza-detector" "Enable lollapalooza detector" true)
  ;; Create metrics
  (metrics/create-counter! :lollapaloozadetector/analyses "Lollapalooza analyses performed")
  (metrics/create-counter! :lollapaloozadetector/events "Lollapalooza events detected")
  (metrics/create-gauge! :lollapaloozadetector/alerts "Active alerts"
                         #(count (list-alerts :status :pending)))
  ;; Initialize synergy rules
  (init-synergy-rules!)
  (log/info "Lollapalooza detector initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-lollapalooza-detector-status []
  {:enabled (flags/is-enabled? "lollapalooza-detector")
   :model-detections (.size ^ConcurrentHashMap (:model-detections @detector-state))
   :lollapalooza-events (.size ^ConcurrentHashMap (:lollapalooza-events @detector-state))
   :synergy-rules (.size ^ConcurrentHashMap (:synergy-rules @detector-state))
   :pending-alerts (count (list-alerts :status :pending))
   :stats (get-detector-stats)
   :config (:config @detector-state)})
