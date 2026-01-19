(ns mental-models.pipeline.integration.model-evaluator
  "Model Evaluator Module
   
   Mental model evaluation and scoring:
   - Model confidence scoring
   - Cross-model validation
   - Lollapalooza detection
   - Model interaction analysis
   - Evaluation metrics"
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
;; MODEL EVALUATOR STATE
;; =============================================================================

(defonce evaluator-state (atom {:evaluations (ConcurrentHashMap.)
                                :model-weights {}
                                :interaction-matrix {}
                                :lollapalooza-history []
                                :evaluation-count (AtomicLong. 0)
                                :config {:confidence-threshold 0.7
                                         :lollapalooza-min-models 3
                                         :lollapalooza-min-confidence 0.7
                                         :max-history 1000}}))

;; =============================================================================
;; MODEL DEFINITIONS
;; =============================================================================

(def munger-models
  #{:reciprocation :liking :social-proof :authority :scarcity
    :contrast :commitment-consistency :reason-respecting
    :association :pain-avoidance :reward-superresponse
    :doubt-avoidance :inconsistency-avoidance :curiosity
    :kantian-fairness :envy-jealousy :chemical-dependency
    :senescence-misinfluence :authority-misinfluence
    :twaddle :reason-respecting-tendency :lollapalooza
    :availability :anchoring :confirmation-bias
    :hindsight :overconfidence :loss-aversion
    :sunk-cost :framing :mental-accounting
    :status-quo :endowment :bandwagon
    :dunning-kruger :spotlight :illusion-of-control
    :planning-fallacy :optimism-bias :pessimism-bias
    :self-serving :fundamental-attribution :halo-effect
    :horn-effect :stereotyping :in-group :out-group
    :just-world :belief-perseverance :backfire
    :illusory-correlation :gambler-fallacy :hot-hand
    :regression-to-mean :base-rate-neglect :conjunction
    :representativeness :availability-cascade :mere-exposure
    :peak-end :duration-neglect :primacy :recency
    :serial-position :von-restorff :zeigarnik
    :generation :testing :spacing :interleaving
    :elaboration :dual-coding :concrete-examples
    :retrieval-practice})

;; =============================================================================
;; CONFIDENCE SCORING
;; =============================================================================

(defn calculate-confidence
  "Calculate confidence score for a model detection."
  [detection]
  (let [{:keys [keyword-matches context-relevance pattern-strength evidence-count]} detection
        keyword-score (or keyword-matches 0)
        context-score (or context-relevance 0)
        pattern-score (or pattern-strength 0)
        evidence-score (min 1.0 (/ (or evidence-count 0) 5))]
    (/ (+ (* 0.3 keyword-score)
          (* 0.3 context-score)
          (* 0.25 pattern-score)
          (* 0.15 evidence-score))
       1.0)))

(defn normalize-confidence
  "Normalize confidence to 0-1 range."
  [confidence]
  (max 0.0 (min 1.0 (double confidence))))

(defn adjust-confidence
  "Adjust confidence based on model weight."
  [model-id confidence]
  (let [weight (get-in @evaluator-state [:model-weights model-id] 1.0)]
    (normalize-confidence (* confidence weight))))

(defn set-model-weight!
  "Set weight for a model."
  [model-id weight]
  (swap! evaluator-state assoc-in [:model-weights model-id] weight))

(defn get-model-weight
  "Get weight for a model."
  [model-id]
  (get-in @evaluator-state [:model-weights model-id] 1.0))

;; =============================================================================
;; MODEL EVALUATION
;; =============================================================================

(defn create-evaluation
  "Create an evaluation record."
  [evaluation-id {:keys [text models detections metadata]}]
  {:id evaluation-id
   :text text
   :models models
   :detections detections
   :metadata metadata
   :created-at (System/currentTimeMillis)})

(defn evaluate-detection
  "Evaluate a single model detection."
  [detection]
  (let [model-id (:model-id detection)
        raw-confidence (calculate-confidence detection)
        adjusted-confidence (adjust-confidence model-id raw-confidence)
        threshold (get-in @evaluator-state [:config :confidence-threshold])]
    (assoc detection
           :raw-confidence raw-confidence
           :adjusted-confidence adjusted-confidence
           :above-threshold (>= adjusted-confidence threshold)
           :evaluated-at (System/currentTimeMillis))))

(defn evaluate-detections
  "Evaluate multiple detections."
  [detections]
  (mapv evaluate-detection detections))

(defn filter-confident-detections
  "Filter detections above confidence threshold."
  [detections & {:keys [threshold]}]
  (let [threshold (or threshold (get-in @evaluator-state [:config :confidence-threshold]))]
    (filter #(>= (:adjusted-confidence %) threshold) detections)))

(defn rank-detections
  "Rank detections by confidence."
  [detections]
  (sort-by :adjusted-confidence > detections))

(defn evaluate-text
  "Evaluate text for mental model presence."
  [text detections & {:keys [metadata]}]
  (.incrementAndGet ^AtomicLong (:evaluation-count @evaluator-state))
  (metrics/inc-counter! :modelevaluator/evaluations-performed)
  (let [evaluation-id (str (java.util.UUID/randomUUID))
        evaluated-detections (evaluate-detections detections)
        confident-detections (filter-confident-detections evaluated-detections)
        ranked-detections (rank-detections confident-detections)
        models-detected (set (map :model-id confident-detections))
        evaluation (create-evaluation evaluation-id
                                      {:text text
                                       :models models-detected
                                       :detections ranked-detections
                                       :metadata metadata})]
    ;; Store evaluation
    (.put ^ConcurrentHashMap (:evaluations @evaluator-state) evaluation-id evaluation)
    ;; Check for Lollapalooza
    (when-let [lollapalooza (detect-lollapalooza ranked-detections)]
      (record-lollapalooza! lollapalooza))
    ;; Publish event
    (events/publish! :modelevaluator/text-evaluated {:evaluation-id evaluation-id
                                                      :models-count (count models-detected)})
    evaluation))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn detect-lollapalooza
  "Detect Lollapalooza effect (multiple models converging)."
  [detections]
  (let [min-models (get-in @evaluator-state [:config :lollapalooza-min-models])
        min-confidence (get-in @evaluator-state [:config :lollapalooza-min-confidence])
        high-confidence (filter #(>= (:adjusted-confidence %) min-confidence) detections)]
    (when (>= (count high-confidence) min-models)
      (let [models (map :model-id high-confidence)
            avg-confidence (/ (reduce + (map :adjusted-confidence high-confidence))
                              (count high-confidence))]
        {:detected true
         :models models
         :model-count (count models)
         :avg-confidence avg-confidence
         :detections high-confidence
         :detected-at (System/currentTimeMillis)}))))

(defn record-lollapalooza!
  "Record a Lollapalooza detection."
  [lollapalooza]
  (log/info "Lollapalooza detected!" {:models (:models lollapalooza)
                                       :confidence (:avg-confidence lollapalooza)})
  (let [max-history (get-in @evaluator-state [:config :max-history])]
    (swap! evaluator-state update :lollapalooza-history
           (fn [history]
             (let [new-history (conj history lollapalooza)]
               (if (> (count new-history) max-history)
                 (vec (drop (- (count new-history) max-history) new-history))
                 new-history)))))
  (metrics/inc-counter! :modelevaluator/lollapaloozas-detected)
  (events/publish! :modelevaluator/lollapalooza-detected lollapalooza))

(defn get-lollapalooza-history
  "Get Lollapalooza detection history."
  [& {:keys [limit since]}]
  (let [history (:lollapalooza-history @evaluator-state)]
    (cond->> history
      since (filter #(>= (:detected-at %) since))
      limit (take-last limit))))

;; =============================================================================
;; MODEL INTERACTIONS
;; =============================================================================

(defn record-interaction!
  "Record an interaction between models."
  [model1 model2 interaction-type & {:keys [strength]}]
  (let [key [model1 model2]
        strength (or strength 1.0)]
    (swap! evaluator-state update-in [:interaction-matrix key]
           (fn [existing]
             (if existing
               (update existing interaction-type (fnil + 0) strength)
               {interaction-type strength})))))

(defn get-interaction
  "Get interaction between two models."
  [model1 model2]
  (or (get-in @evaluator-state [:interaction-matrix [model1 model2]])
      (get-in @evaluator-state [:interaction-matrix [model2 model1]])))

(defn analyze-interactions
  "Analyze interactions between detected models."
  [detections]
  (let [models (map :model-id detections)
        pairs (for [m1 models m2 models :when (not= m1 m2)] [m1 m2])]
    (into {} (for [[m1 m2] (distinct (map sort pairs))]
               [[m1 m2] (get-interaction m1 m2)]))))

(defn find-synergies
  "Find synergistic model combinations."
  [detections]
  (let [interactions (analyze-interactions detections)]
    (filter (fn [[_ v]] (and v (pos? (get v :synergy 0)))) interactions)))

(defn find-conflicts
  "Find conflicting model combinations."
  [detections]
  (let [interactions (analyze-interactions detections)]
    (filter (fn [[_ v]] (and v (pos? (get v :conflict 0)))) interactions)))

;; =============================================================================
;; EVALUATION METRICS
;; =============================================================================

(defn calculate-precision
  "Calculate precision for model detection."
  [true-positives false-positives]
  (let [total (+ true-positives false-positives)]
    (if (pos? total)
      (double (/ true-positives total))
      0.0)))

(defn calculate-recall
  "Calculate recall for model detection."
  [true-positives false-negatives]
  (let [total (+ true-positives false-negatives)]
    (if (pos? total)
      (double (/ true-positives total))
      0.0)))

(defn calculate-f1
  "Calculate F1 score."
  [precision recall]
  (if (pos? (+ precision recall))
    (/ (* 2 precision recall) (+ precision recall))
    0.0))

(defn evaluate-accuracy
  "Evaluate accuracy against ground truth."
  [predictions ground-truth]
  (let [pred-set (set predictions)
        truth-set (set ground-truth)
        true-positives (count (set/intersection pred-set truth-set))
        false-positives (count (set/difference pred-set truth-set))
        false-negatives (count (set/difference truth-set pred-set))
        precision (calculate-precision true-positives false-positives)
        recall (calculate-recall true-positives false-negatives)
        f1 (calculate-f1 precision recall)]
    {:true-positives true-positives
     :false-positives false-positives
     :false-negatives false-negatives
     :precision precision
     :recall recall
     :f1 f1}))

;; =============================================================================
;; EVALUATION RETRIEVAL
;; =============================================================================

(defn get-evaluation
  "Get an evaluation by ID."
  [evaluation-id]
  (.get ^ConcurrentHashMap (:evaluations @evaluator-state) evaluation-id))

(defn list-evaluations
  "List all evaluations."
  [& {:keys [limit since model-id]}]
  (let [evaluations (vals (:evaluations @evaluator-state))]
    (cond->> evaluations
      model-id (filter #(contains? (:models %) model-id))
      since (filter #(>= (:created-at %) since))
      limit (take-last limit))))

(defn clear-evaluations!
  "Clear all evaluations."
  []
  (.clear ^ConcurrentHashMap (:evaluations @evaluator-state)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-model-statistics
  "Get statistics for a specific model."
  [model-id]
  (let [evaluations (list-evaluations :model-id model-id)]
    {:model-id model-id
     :detection-count (count evaluations)
     :avg-confidence (if (seq evaluations)
                       (/ (reduce + (map #(-> % :detections first :adjusted-confidence) evaluations))
                          (count evaluations))
                       0.0)
     :weight (get-model-weight model-id)}))

(defn get-all-model-statistics
  "Get statistics for all models."
  []
  (into {} (for [model-id munger-models]
             [model-id (get-model-statistics model-id)])))

(defn get-evaluator-stats
  "Get overall evaluator statistics."
  []
  {:total-evaluations (.size ^ConcurrentHashMap (:evaluations @evaluator-state))
   :evaluation-count (.get ^AtomicLong (:evaluation-count @evaluator-state))
   :lollapalooza-count (count (:lollapalooza-history @evaluator-state))
   :models-tracked (count munger-models)
   :interactions-recorded (count (:interaction-matrix @evaluator-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-model-evaluator!
  "Initialize model evaluator."
  []
  (log/info "Initializing model evaluator")
  ;; Register feature flag
  (flags/register-flag! "model-evaluator" "Enable model evaluator" true)
  ;; Create metrics
  (metrics/create-counter! :modelevaluator/evaluations-performed "Evaluations performed")
  (metrics/create-counter! :modelevaluator/lollapaloozas-detected "Lollapaloozas detected")
  (metrics/create-gauge! :modelevaluator/total-evaluations "Total evaluations"
                         #(.size ^ConcurrentHashMap (:evaluations @evaluator-state)))
  ;; Initialize default model weights
  (doseq [model-id munger-models]
    (set-model-weight! model-id 1.0))
  ;; Initialize common interactions
  (record-interaction! :social-proof :authority :synergy :strength 0.8)
  (record-interaction! :scarcity :loss-aversion :synergy :strength 0.9)
  (record-interaction! :commitment-consistency :sunk-cost :synergy :strength 0.7)
  (record-interaction! :anchoring :framing :synergy :strength 0.8)
  (record-interaction! :confirmation-bias :belief-perseverance :synergy :strength 0.9)
  (log/info "Model evaluator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-model-evaluator-status []
  {:enabled (flags/is-enabled? "model-evaluator")
   :evaluations (.size ^ConcurrentHashMap (:evaluations @evaluator-state))
   :evaluation-count (.get ^AtomicLong (:evaluation-count @evaluator-state))
   :lollapalooza-count (count (:lollapalooza-history @evaluator-state))
   :models-tracked (count munger-models)
   :interactions (count (:interaction-matrix @evaluator-state))
   :stats (get-evaluator-stats)
   :config (:config @evaluator-state)})
