(ns mental-models.safeguards
  "Safeguards Module - Electric Clojure
   
   Failure mode detection and safeguards for mental models.
   Ported from Python to Electric Clojure for unified codebase.
   
   Features:
   - Failure mode registry
   - Safeguard engine
   - Failure detection
   - Risk assessment"
  (:require [clojure.string :as str]
            [mental-models.models :as models]))

;; ============================================
;; Failure Mode Registry
;; ============================================

(def failure-mode-categories
  "Categories of failure modes."
  {:cognitive ["confirmation-bias" "availability-heuristic" "anchoring"
               "overconfidence" "hindsight-bias" "sunk-cost-fallacy"]
   :social ["social-proof" "authority-bias" "groupthink" "herd-behavior"]
   :emotional ["loss-aversion" "fear-of-missing-out" "panic-selling"
               "euphoria-buying" "regret-aversion"]
   :analytical ["overfitting" "survivorship-bias" "selection-bias"
                "correlation-causation" "base-rate-neglect"]
   :structural ["incentive-misalignment" "principal-agent" "moral-hazard"
                "adverse-selection" "information-asymmetry"]})

(def severity-levels
  "Severity levels for failure modes."
  {:critical {:level 5 :description "Catastrophic failure likely"}
   :high {:level 4 :description "Significant negative impact"}
   :medium {:level 3 :description "Moderate negative impact"}
   :low {:level 2 :description "Minor negative impact"}
   :minimal {:level 1 :description "Negligible impact"}})

;; ============================================
;; Failure Mode Detection
;; ============================================

(def failure-mode-signals
  "Signals that indicate potential failure modes."
  {:confirmation-bias
   {:signals ["confirms my belief" "proves I was right" "as expected"
              "knew it all along" "obviously" "clearly shows"]
    :severity :high
    :mitigation "Actively seek disconfirming evidence"}
   
   :availability-heuristic
   {:signals ["just saw" "recently" "in the news" "trending"
              "everyone's talking about" "viral"]
    :severity :medium
    :mitigation "Consider base rates and historical data"}
   
   :anchoring
   {:signals ["starting from" "based on" "compared to" "relative to"
              "originally" "initial estimate"]
    :severity :medium
    :mitigation "Generate estimates independently before seeing anchors"}
   
   :overconfidence
   {:signals ["definitely" "certainly" "guaranteed" "no doubt"
              "100%" "impossible to fail" "can't lose"]
    :severity :critical
    :mitigation "Assign probabilities and track calibration"}
   
   :sunk-cost-fallacy
   {:signals ["already invested" "come this far" "can't stop now"
              "too late to turn back" "wasted if we stop"]
    :severity :high
    :mitigation "Evaluate decisions based on future value only"}
   
   :social-proof
   {:signals ["everyone is" "most people" "they all" "popular"
              "trending" "viral" "mainstream"]
    :severity :medium
    :mitigation "Evaluate independently before checking consensus"}
   
   :loss-aversion
   {:signals ["can't afford to lose" "protect gains" "avoid losses"
              "downside risk" "worst case"]
    :severity :medium
    :mitigation "Frame decisions in terms of expected value"}
   
   :groupthink
   {:signals ["we all agree" "unanimous" "no objections"
              "everyone thinks" "consensus"]
    :severity :high
    :mitigation "Assign devil's advocate role, encourage dissent"}
   
   :hindsight-bias
   {:signals ["knew it" "obvious in retrospect" "should have seen"
              "predictable" "inevitable"]
    :severity :low
    :mitigation "Document predictions before outcomes are known"}
   
   :survivorship-bias
   {:signals ["successful companies" "winners" "best performers"
              "top" "leading"]
    :severity :high
    :mitigation "Include failed cases in analysis"}})

(defn detect-failure-mode
  "Detect a specific failure mode in text."
  [text failure-mode]
  (let [text-lower (str/lower-case (or text ""))
        config (get failure-mode-signals failure-mode)
        signals (:signals config)
        matches (filter #(str/includes? text-lower %) signals)]
    (when (seq matches)
      {:failure-mode failure-mode
       :matches matches
       :match-count (count matches)
       :severity (:severity config)
       :mitigation (:mitigation config)})))

(defn detect-all-failure-modes
  "Detect all failure modes in text."
  [text]
  (let [detections (keep #(detect-failure-mode text (first %))
                         failure-mode-signals)
        sorted (sort-by #(get-in severity-levels [(:severity %) :level]) > detections)]
    {:text-analyzed (subs text 0 (min 200 (count text)))
     :failure-modes-detected sorted
     :total-detected (count sorted)
     :highest-severity (when (seq sorted)
                         (:severity (first sorted)))
     :risk-level (cond
                   (some #(= :critical (:severity %)) sorted) :critical
                   (some #(= :high (:severity %)) sorted) :high
                   (some #(= :medium (:severity %)) sorted) :medium
                   (seq sorted) :low
                   :else :none)}))

;; ============================================
;; Safeguard Engine
;; ============================================

(def safeguards
  "Safeguards for each failure mode category."
  {:cognitive
   [{:name "Pre-mortem analysis"
     :description "Imagine the decision failed - what went wrong?"
     :when-to-use "Before major decisions"}
    {:name "Devil's advocate"
     :description "Assign someone to argue the opposite position"
     :when-to-use "When consensus forms too quickly"}
    {:name "Outside view"
     :description "Look at base rates from similar situations"
     :when-to-use "When making predictions"}]
   
   :social
   [{:name "Independent evaluation"
     :description "Form opinion before seeing others' views"
     :when-to-use "Before group discussions"}
    {:name "Anonymous input"
     :description "Collect opinions anonymously first"
     :when-to-use "In hierarchical settings"}
    {:name "Diverse perspectives"
     :description "Actively seek dissenting views"
     :when-to-use "When making group decisions"}]
   
   :emotional
   [{:name "Cooling-off period"
     :description "Wait 24-48 hours before major decisions"
     :when-to-use "When feeling strong emotions"}
    {:name "Decision journal"
     :description "Document reasoning and emotions at decision time"
     :when-to-use "For all significant decisions"}
    {:name "Pre-commitment"
     :description "Set rules in advance for emotional situations"
     :when-to-use "Before entering volatile situations"}]
   
   :analytical
   [{:name "Out-of-sample testing"
     :description "Test models on data not used for fitting"
     :when-to-use "When building predictive models"}
    {:name "Base rate check"
     :description "Always start with base rates"
     :when-to-use "When making probability estimates"}
    {:name "Survivorship check"
     :description "Explicitly consider failures and dropouts"
     :when-to-use "When analyzing success stories"}]
   
   :structural
   [{:name "Incentive audit"
     :description "Map all stakeholder incentives"
     :when-to-use "Before entering agreements"}
    {:name "Skin in the game"
     :description "Ensure decision-makers bear consequences"
     :when-to-use "When delegating decisions"}
    {:name "Information symmetry"
     :description "Ensure all parties have same information"
     :when-to-use "In negotiations and transactions"}]})

(defn get-safeguards-for-failure-mode
  "Get recommended safeguards for a detected failure mode."
  [failure-mode]
  (let [category (cond
                   (some #{failure-mode} (:cognitive failure-mode-categories)) :cognitive
                   (some #{failure-mode} (:social failure-mode-categories)) :social
                   (some #{failure-mode} (:emotional failure-mode-categories)) :emotional
                   (some #{failure-mode} (:analytical failure-mode-categories)) :analytical
                   (some #{failure-mode} (:structural failure-mode-categories)) :structural
                   :else :cognitive)]
    {:failure-mode failure-mode
     :category category
     :safeguards (get safeguards category)
     :specific-mitigation (get-in failure-mode-signals [failure-mode :mitigation])}))

(defn generate-safeguard-report
  "Generate a comprehensive safeguard report for detected failure modes."
  [text]
  (let [detection (detect-all-failure-modes text)
        failure-modes (:failure-modes-detected detection)
        safeguard-recommendations (map #(get-safeguards-for-failure-mode (:failure-mode %))
                                       failure-modes)]
    {:detection detection
     :safeguard-recommendations safeguard-recommendations
     :priority-actions (take 3 (mapcat :safeguards safeguard-recommendations))
     :summary {:total-risks (count failure-modes)
               :risk-level (:risk-level detection)
               :categories-affected (distinct (map :category safeguard-recommendations))}}))

;; ============================================
;; Risk Assessment
;; ============================================

(defn calculate-risk-score
  "Calculate overall risk score from detected failure modes."
  [failure-modes]
  (let [severity-scores {:critical 10 :high 7 :medium 4 :low 2 :minimal 1}
        scores (map #(get severity-scores (:severity %) 0) failure-modes)
        total (reduce + scores)
        max-possible (* 10 (count failure-mode-signals))]
    {:raw-score total
     :normalized-score (if (> max-possible 0)
                         (/ total max-possible)
                         0)
     :risk-level (cond
                   (>= total 30) :critical
                   (>= total 20) :high
                   (>= total 10) :medium
                   (> total 0) :low
                   :else :none)
     :failure-mode-count (count failure-modes)}))

(defn assess-decision-risk
  "Assess the risk of a decision based on context."
  [decision-text context]
  (let [combined-text (str decision-text " " context)
        detection (detect-all-failure-modes combined-text)
        risk-score (calculate-risk-score (:failure-modes-detected detection))]
    {:decision decision-text
     :context context
     :detection detection
     :risk-score risk-score
     :recommendation (case (:risk-level risk-score)
                       :critical "STOP - Address critical failure modes before proceeding"
                       :high "CAUTION - Implement safeguards before proceeding"
                       :medium "REVIEW - Consider safeguards for identified risks"
                       :low "PROCEED - Monitor for identified risks"
                       "PROCEED - No significant risks detected")}))

;; ============================================
;; Model-Specific Failure Modes
;; ============================================

(defn get-model-failure-modes
  "Get failure modes for a specific mental model."
  [model-name]
  (if-let [model (models/get-model model-name)]
    {:model model-name
     :failure-modes (:failure-modes model)
     :category (:category model)
     :total-failure-modes (count (:failure-modes model))}
    {:error (str "Model not found: " model-name)}))

(defn check-model-failure-modes
  "Check if any failure modes for a model are active in the given context."
  [model-name context]
  (if-let [model (models/get-model model-name)]
    (let [context-lower (str/lower-case (or context ""))
          failure-modes (:failure-modes model)
          active-modes (filter (fn [fm]
                                 (some #(str/includes? context-lower (str/lower-case %))
                                       (:signals fm)))
                               failure-modes)]
      {:model model-name
       :context-analyzed (subs context 0 (min 200 (count context)))
       :active-failure-modes active-modes
       :total-active (count active-modes)
       :risk-level (cond
                     (>= (count active-modes) 3) :high
                     (>= (count active-modes) 1) :medium
                     :else :low)})
    {:error (str "Model not found: " model-name)}))

;; ============================================
;; Comprehensive Safeguard Analysis
;; ============================================

(defn comprehensive-safeguard-analysis
  "Run comprehensive safeguard analysis on a decision."
  [decision context model-names]
  (let [text-analysis (generate-safeguard-report (str decision " " context))
        model-analyses (map #(check-model-failure-modes % context) model-names)
        all-active-modes (mapcat :active-failure-modes model-analyses)
        combined-risk (calculate-risk-score
                        (concat (:failure-modes-detected (:detection text-analysis))
                                (map (fn [fm] {:severity (keyword (:severity fm))})
                                     all-active-modes)))]
    {:decision decision
     :context context
     :models-analyzed model-names
     :text-analysis text-analysis
     :model-analyses model-analyses
     :combined-risk combined-risk
     :all-active-failure-modes all-active-modes
     :priority-safeguards (:priority-actions text-analysis)
     :final-recommendation
     (case (:risk-level combined-risk)
       :critical {:action "STOP"
                  :message "Critical risks detected - do not proceed without addressing"}
       :high {:action "PAUSE"
              :message "High risks detected - implement safeguards before proceeding"}
       :medium {:action "REVIEW"
                :message "Moderate risks detected - consider safeguards"}
       :low {:action "PROCEED"
             :message "Low risks detected - monitor and proceed"}
       {:action "PROCEED"
        :message "No significant risks detected"})}))
