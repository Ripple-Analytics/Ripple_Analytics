(ns mental-models.pipeline.integration.decision-engine
  "Decision Engine Module
   
   Mental model-based decision support:
   - Decision tree evaluation
   - Multi-criteria analysis
   - Risk assessment
   - Outcome prediction
   - Decision logging and audit"
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
;; DECISION ENGINE STATE
;; =============================================================================

(defonce engine-state (atom {:decision-trees (ConcurrentHashMap.)
                             :criteria (ConcurrentHashMap.)
                             :decisions (ConcurrentHashMap.)
                             :outcomes (ConcurrentHashMap.)
                             :decision-count (AtomicLong. 0)
                             :config {:max-tree-depth 10
                                      :min-confidence 0.5
                                      :risk-threshold 0.7}}))

;; =============================================================================
;; DECISION TREES
;; =============================================================================

(defn create-decision-tree!
  "Create a decision tree."
  [tree-id {:keys [name description root-node mental-models]}]
  (log/info "Creating decision tree" {:id tree-id :name name})
  (.put ^ConcurrentHashMap (:decision-trees @engine-state) tree-id
        {:id tree-id
         :name name
         :description description
         :root-node root-node
         :mental-models (or mental-models [])
         :enabled true
         :usage-count (AtomicLong. 0)
         :created-at (System/currentTimeMillis)}))

(defn get-decision-tree
  "Get a decision tree by ID."
  [tree-id]
  (.get ^ConcurrentHashMap (:decision-trees @engine-state) tree-id))

(defn list-decision-trees
  "List all decision trees."
  [& {:keys [mental-model enabled-only]}]
  (let [trees (vals (:decision-trees @engine-state))]
    (cond->> trees
      enabled-only (filter :enabled)
      mental-model (filter #(some #{mental-model} (:mental-models %))))))

(defn delete-decision-tree!
  "Delete a decision tree."
  [tree-id]
  (.remove ^ConcurrentHashMap (:decision-trees @engine-state) tree-id))

;; =============================================================================
;; DECISION NODES
;; =============================================================================

(defn create-condition-node
  "Create a condition node for decision tree."
  [{:keys [condition true-branch false-branch]}]
  {:type :condition
   :condition condition
   :true-branch true-branch
   :false-branch false-branch})

(defn create-action-node
  "Create an action/outcome node."
  [{:keys [action recommendation confidence risk-level mental-models]}]
  {:type :action
   :action action
   :recommendation recommendation
   :confidence (or confidence 1.0)
   :risk-level (or risk-level :low)
   :mental-models (or mental-models [])})

(defn create-weight-node
  "Create a weighted decision node."
  [{:keys [options weights]}]
  {:type :weighted
   :options options
   :weights weights})

;; =============================================================================
;; CRITERIA MANAGEMENT
;; =============================================================================

(defn register-criterion!
  "Register a decision criterion."
  [criterion-id {:keys [name description weight type evaluator mental-model]}]
  (log/debug "Registering criterion" {:id criterion-id :name name})
  (.put ^ConcurrentHashMap (:criteria @engine-state) criterion-id
        {:id criterion-id
         :name name
         :description description
         :weight (or weight 1.0)
         :type (or type :numeric)
         :evaluator evaluator
         :mental-model mental-model
         :enabled true}))

(defn get-criterion
  "Get a criterion by ID."
  [criterion-id]
  (.get ^ConcurrentHashMap (:criteria @engine-state) criterion-id))

(defn list-criteria
  "List all criteria."
  [& {:keys [mental-model]}]
  (let [criteria (vals (:criteria @engine-state))]
    (if mental-model
      (filter #(= (:mental-model %) mental-model) criteria)
      criteria)))

;; =============================================================================
;; TREE EVALUATION
;; =============================================================================

(defn evaluate-condition
  "Evaluate a condition against context."
  [condition context]
  (cond
    (fn? condition) (condition context)
    (keyword? condition) (get context condition)
    (map? condition) (let [{:keys [field operator value]} condition
                           field-value (get context field)]
                       (case operator
                         := (= field-value value)
                         :!= (not= field-value value)
                         :> (> field-value value)
                         :< (< field-value value)
                         :>= (>= field-value value)
                         :<= (<= field-value value)
                         :contains (str/includes? (str field-value) (str value))
                         :in (contains? (set value) field-value)
                         false))
    :else (boolean condition)))

(defn evaluate-tree-node
  "Evaluate a decision tree node."
  [node context depth]
  (let [max-depth (get-in @engine-state [:config :max-tree-depth])]
    (when (< depth max-depth)
      (case (:type node)
        :condition
        (let [result (evaluate-condition (:condition node) context)]
          (if result
            (evaluate-tree-node (:true-branch node) context (inc depth))
            (evaluate-tree-node (:false-branch node) context (inc depth))))
        
        :action
        {:decision (:action node)
         :recommendation (:recommendation node)
         :confidence (:confidence node)
         :risk-level (:risk-level node)
         :mental-models (:mental-models node)
         :path-depth depth}
        
        :weighted
        (let [scored-options (map (fn [[option weight]]
                                    {:option option
                                     :score (* weight (get context option 0.5))})
                                  (map vector (:options node) (:weights node)))
              best (apply max-key :score scored-options)]
          {:decision (:option best)
           :score (:score best)
           :all-scores scored-options
           :path-depth depth})
        
        ;; Default: return node as-is if it's a leaf
        node))))

(defn evaluate-decision-tree
  "Evaluate a decision tree with given context."
  [tree-id context]
  (when-let [tree (get-decision-tree tree-id)]
    (when (:enabled tree)
      (.incrementAndGet ^AtomicLong (:usage-count tree))
      (let [result (evaluate-tree-node (:root-node tree) context 0)]
        (assoc result
               :tree-id tree-id
               :tree-name (:name tree)
               :evaluated-at (System/currentTimeMillis))))))

;; =============================================================================
;; MULTI-CRITERIA ANALYSIS
;; =============================================================================

(defn evaluate-criterion
  "Evaluate a single criterion."
  [criterion option context]
  (let [evaluator (:evaluator criterion)
        raw-score (if (fn? evaluator)
                    (evaluator option context)
                    (get option (:id criterion) 0.5))
        weighted-score (* raw-score (:weight criterion))]
    {:criterion-id (:id criterion)
     :criterion-name (:name criterion)
     :raw-score raw-score
     :weight (:weight criterion)
     :weighted-score weighted-score}))

(defn multi-criteria-analysis
  "Perform multi-criteria decision analysis."
  [options criteria-ids context]
  (let [criteria (keep get-criterion criteria-ids)
        total-weight (reduce + (map :weight criteria))
        scored-options (map (fn [option]
                              (let [scores (map #(evaluate-criterion % option context) criteria)
                                    total-score (/ (reduce + (map :weighted-score scores)) total-weight)]
                                {:option option
                                 :scores scores
                                 :total-score total-score}))
                            options)
        ranked (sort-by :total-score > scored-options)]
    {:options ranked
     :best-option (first ranked)
     :criteria-used (count criteria)
     :analyzed-at (System/currentTimeMillis)}))

;; =============================================================================
;; RISK ASSESSMENT
;; =============================================================================

(defn assess-risk
  "Assess risk for a decision."
  [decision context]
  (let [risk-factors (get context :risk-factors {})
        base-risk (get decision :risk-level :medium)
        risk-scores {:low 0.2 :medium 0.5 :high 0.8 :critical 1.0}
        base-score (get risk-scores base-risk 0.5)
        ;; Adjust based on context risk factors
        factor-adjustment (reduce + (vals risk-factors))
        adjusted-score (min 1.0 (+ base-score (* 0.1 factor-adjustment)))
        threshold (get-in @engine-state [:config :risk-threshold])]
    {:risk-score adjusted-score
     :risk-level (cond
                   (< adjusted-score 0.3) :low
                   (< adjusted-score 0.5) :medium
                   (< adjusted-score 0.7) :high
                   :else :critical)
     :exceeds-threshold (> adjusted-score threshold)
     :risk-factors risk-factors
     :recommendation (if (> adjusted-score threshold)
                       "Consider additional review before proceeding"
                       "Risk within acceptable limits")}))

;; =============================================================================
;; DECISION MAKING
;; =============================================================================

(defn make-decision!
  "Make a decision using the engine."
  [decision-id {:keys [tree-id options criteria context mental-models]}]
  (.incrementAndGet ^AtomicLong (:decision-count @engine-state))
  (metrics/inc-counter! :decisionengine/decisions)
  (let [;; Evaluate decision tree if provided
        tree-result (when tree-id
                      (evaluate-decision-tree tree-id context))
        ;; Perform multi-criteria analysis if options provided
        mca-result (when (and options criteria)
                     (multi-criteria-analysis options criteria context))
        ;; Combine results
        primary-decision (or (:decision tree-result)
                             (:option (:best-option mca-result)))
        ;; Assess risk
        risk-assessment (assess-risk {:risk-level (or (:risk-level tree-result) :medium)} context)
        ;; Build decision record
        decision {:id decision-id
                  :tree-result tree-result
                  :mca-result mca-result
                  :primary-decision primary-decision
                  :risk-assessment risk-assessment
                  :mental-models (or mental-models
                                     (:mental-models tree-result)
                                     [])
                  :context context
                  :decided-at (System/currentTimeMillis)}]
    ;; Store decision
    (.put ^ConcurrentHashMap (:decisions @engine-state) decision-id decision)
    (log/info "Decision made" {:id decision-id :decision primary-decision})
    (events/publish! :decision/made {:decision-id decision-id
                                     :decision primary-decision
                                     :risk-level (:risk-level risk-assessment)})
    decision))

;; =============================================================================
;; OUTCOME TRACKING
;; =============================================================================

(defn record-outcome!
  "Record the outcome of a decision."
  [decision-id {:keys [outcome success feedback actual-result]}]
  (let [outcome-id (str decision-id "-outcome")
        outcome-record {:id outcome-id
                        :decision-id decision-id
                        :outcome outcome
                        :success success
                        :feedback feedback
                        :actual-result actual-result
                        :recorded-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:outcomes @engine-state) outcome-id outcome-record)
    (log/info "Outcome recorded" {:decision decision-id :success success})
    (events/publish! :decision/outcome {:decision-id decision-id :success success})
    outcome-record))

(defn get-decision
  "Get a decision by ID."
  [decision-id]
  (.get ^ConcurrentHashMap (:decisions @engine-state) decision-id))

(defn get-outcome
  "Get outcome for a decision."
  [decision-id]
  (.get ^ConcurrentHashMap (:outcomes @engine-state) (str decision-id "-outcome")))

(defn list-decisions
  "List decisions."
  [& {:keys [limit since mental-model]}]
  (let [decisions (vals (:decisions @engine-state))]
    (cond->> decisions
      mental-model (filter #(some #{mental-model} (:mental-models %)))
      since (filter #(>= (:decided-at %) since))
      true (sort-by :decided-at >)
      limit (take limit))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-engine-stats
  "Get decision engine statistics."
  []
  (let [decisions (vals (:decisions @engine-state))
        outcomes (vals (:outcomes @engine-state))
        successful (filter :success outcomes)]
    {:decision-trees (.size ^ConcurrentHashMap (:decision-trees @engine-state))
     :criteria (.size ^ConcurrentHashMap (:criteria @engine-state))
     :decisions (.size ^ConcurrentHashMap (:decisions @engine-state))
     :outcomes (.size ^ConcurrentHashMap (:outcomes @engine-state))
     :decision-count (.get ^AtomicLong (:decision-count @engine-state))
     :success-rate (if (seq outcomes)
                     (/ (count successful) (count outcomes))
                     0.0)
     :risk-distribution (frequencies (map #(get-in % [:risk-assessment :risk-level]) decisions))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-mental-model-criteria!
  "Initialize criteria based on mental models."
  []
  (log/info "Initializing mental model criteria")
  ;; Second-Order Thinking
  (register-criterion! "second-order-effects"
                       {:name "Second-Order Effects"
                        :description "Consider downstream consequences"
                        :weight 1.2
                        :type :qualitative
                        :mental-model "second-order-thinking"})
  ;; Inversion
  (register-criterion! "failure-avoidance"
                       {:name "Failure Avoidance"
                        :description "How well does this avoid known failure modes"
                        :weight 1.1
                        :type :numeric
                        :mental-model "inversion"})
  ;; Circle of Competence
  (register-criterion! "competence-match"
                       {:name "Competence Match"
                        :description "Is this within our circle of competence"
                        :weight 1.0
                        :type :boolean
                        :mental-model "circle-of-competence"})
  ;; Incentives
  (register-criterion! "incentive-alignment"
                       {:name "Incentive Alignment"
                        :description "Are incentives properly aligned"
                        :weight 1.3
                        :type :qualitative
                        :mental-model "incentives"})
  ;; Margin of Safety
  (register-criterion! "margin-of-safety"
                       {:name "Margin of Safety"
                        :description "Buffer against uncertainty"
                        :weight 1.2
                        :type :numeric
                        :mental-model "margin-of-safety"})
  (log/info "Mental model criteria initialized" {:count 5}))

(defn init-decision-engine!
  "Initialize decision engine."
  []
  (log/info "Initializing decision engine")
  ;; Register feature flag
  (flags/register-flag! "decision-engine" "Enable decision engine" true)
  ;; Create metrics
  (metrics/create-counter! :decisionengine/decisions "Decisions made")
  (metrics/create-gauge! :decisionengine/trees "Total decision trees"
                         #(.size ^ConcurrentHashMap (:decision-trees @engine-state)))
  ;; Initialize mental model criteria
  (init-mental-model-criteria!)
  (log/info "Decision engine initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-decision-engine-status []
  {:enabled (flags/is-enabled? "decision-engine")
   :decision-trees (.size ^ConcurrentHashMap (:decision-trees @engine-state))
   :criteria (.size ^ConcurrentHashMap (:criteria @engine-state))
   :decisions (.size ^ConcurrentHashMap (:decisions @engine-state))
   :stats (get-engine-stats)
   :config (:config @engine-state)})
