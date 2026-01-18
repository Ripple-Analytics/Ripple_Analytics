(ns mental-models.analysis
  "Analysis Engine - Electric Clojure
   
   Comprehensive analysis functions for mental models.
   Runs on both client and server for reactive analysis.
   
   This module provides:
   - Latticework analysis (Munger's approach)
   - Lollapalooza detection
   - Inversion techniques
   - Two-track analysis
   - Bias detection"
  (:require [mental-models.models :as models]
            [clojure.string :as str]))

;; ============================================
;; Core Analysis Functions
;; ============================================

(defn analyze-with-model
  "Apply a single mental model to analyze a context."
  [model context]
  (let [name (:name model)
        failure-modes (:failure-modes model)
        context-str (str context)
        context-lower (str/lower-case context-str)
        
        active-failures
        (filter (fn [fm]
                  (some #(str/includes? context-lower (str/lower-case %))
                        (:signals fm)))
                failure-modes)
        
        relevance-keywords (concat [(:name model)]
                                   (str/split (or (:description model) "") #"\s+"))
        matches (count (filter #(str/includes? context-lower (str/lower-case %))
                               relevance-keywords))
        confidence (min 1.0 (+ 0.3 (* 0.1 matches)))]
    
    {:model name
     :category (:category model)
     :applicable true
     :confidence confidence
     :active-failure-modes (map :name active-failures)
     :warnings (map (fn [fm]
                      {:failure-mode (:name fm)
                       :severity (:severity fm)
                       :description (:description fm)})
                    active-failures)
     :insights [(str "Consider " name ": " (:key-insight model))]}))

(defn latticework-analyze
  "Apply Munger's latticework of mental models to a problem."
  [model-names context]
  (let [models-to-use (if (seq model-names)
                        (keep models/get-model model-names)
                        (take 10 (models/get-all-models)))
        results (map #(analyze-with-model % context) models-to-use)
        by-discipline (group-by :category results)
        high-confidence (filter #(> (:confidence %) 0.6) results)
        confidences (map :confidence results)
        combined-confidence (if (seq confidences)
                              (/ (reduce + confidences) (count confidences))
                              0.0)]
    
    {:context context
     :models-applied (count results)
     :individual-analyses results
     :by-discipline by-discipline
     :cross-discipline-insights
     (when (>= (count (keys by-discipline)) 2)
       [{:type "multi-discipline-convergence"
         :description "Multiple disciplines point to similar conclusions"
         :strength (/ (count high-confidence) (max 1 (count results)))}])
     :combined-confidence combined-confidence
     :recommendation (cond
                       (>= (count high-confidence) 3)
                       {:type "strong"
                        :message "Multiple models strongly support this direction"
                        :supporting-models (map :model high-confidence)}
                       
                       (seq high-confidence)
                       {:type "moderate"
                        :message "Some models support this direction"
                        :supporting-models (map :model high-confidence)}
                       
                       :else
                       {:type "weak"
                        :message "No strong model support - proceed with caution"
                        :supporting-models []})}))

;; ============================================
;; Lollapalooza Detection
;; ============================================

(defn calculate-lollapalooza-multiplier [n]
  (cond
    (>= n 5) 3.0
    (>= n 4) 2.5
    (>= n 3) 2.0
    (>= n 2) 1.5
    :else 1.0))

(defn detect-lollapalooza
  "Detect lollapalooza effects - when multiple models reinforce each other."
  [model-names context]
  (let [analysis (latticework-analyze model-names context)
        results (:individual-analyses analysis)
        high-confidence (filter #(> (:confidence %) 0.7) results)
        is-lollapalooza (>= (count high-confidence) 3)]
    
    {:is-lollapalooza is-lollapalooza
     :models-aligned (count high-confidence)
     :alignment-groups (when (>= (count high-confidence) 2)
                         [{:models (map :model high-confidence)
                           :type "convergent"
                           :description "Models converge on similar conclusions"}])
     :strength (if (seq high-confidence)
                 (/ (reduce + (map :confidence high-confidence))
                    (count high-confidence))
                 0.0)
     :effect-multiplier (calculate-lollapalooza-multiplier (count high-confidence))
     :recommendation (if is-lollapalooza
                       "Strong lollapalooza effect detected - high conviction warranted"
                       "No lollapalooza effect - consider more models")}))

;; ============================================
;; Inversion Analysis
;; ============================================

(defn invert
  "Apply Munger's inversion technique."
  [problem]
  {:original-problem problem
   :inverted-questions
   [(str "What would guarantee failure in: " problem "?")
    (str "What should I definitely NOT do regarding: " problem "?")
    (str "What assumptions am I making about: " problem "?")
    (str "Who has failed at: " problem " and why?")
    (str "What are the worst possible outcomes of: " problem "?")]
   :common-failure-categories
   ["Overconfidence" "Ignoring incentives" "Not considering second-order effects"
    "Social proof following" "Commitment escalation" "Availability bias"
    "Confirmation bias" "Sunk cost fallacy" "Anchoring"]
   :approach "Answer the inverted questions first, then design to avoid those pitfalls"
   :munger-quote "All I want to know is where I'm going to die, so I'll never go there."})

;; ============================================
;; Two-Track Analysis
;; ============================================

(defn two-track-analysis
  "Perform Munger's two-track analysis."
  [situation]
  {:situation situation
   :track-1-rational
   {:economic-incentives
    {:question "Who benefits financially?"
     :importance "critical"}
    :opportunity-costs
    {:question "What are we giving up?"
     :importance "high"}
    :second-order-effects
    {:question "What happens next? And then what?"
     :importance "critical"}
    :margin-of-safety
    {:question "What's our buffer for error?"
     :importance "critical"}}
   
   :track-2-psychological
   {:social-proof
    {:question "Are we following the crowd?"
     :danger-level "high"}
    :commitment-consistency
    {:question "Are we doubling down on past decisions?"
     :danger-level "high"}
    :incentive-caused-bias
    {:question "Are incentives distorting our view?"
     :danger-level "critical"}
    :availability-heuristic
    {:question "Are we overweighting recent/vivid events?"
     :danger-level "medium"}
    :confirmation-bias
    {:question "Are we seeking confirming evidence only?"
     :danger-level "high"}
    :loss-aversion
    {:question "Are we irrationally avoiding losses?"
     :danger-level "medium"}}
   
   :integration-questions
   ["How do rational and psychological factors interact here?"
    "Which psychological biases might distort our rational analysis?"
    "Are the incentives aligned with good decision-making?"]})

;; ============================================
;; Bias Detection
;; ============================================

(def bias-patterns
  {:confirmation-bias ["confirms" "proves" "as expected" "knew it" "obviously"]
   :availability-bias ["recently" "just saw" "heard about" "in the news" "trending"]
   :anchoring ["starting point" "based on" "compared to" "relative to"]
   :social-proof ["everyone" "most people" "they all" "popular" "trending"]
   :authority-bias ["expert says" "according to" "studies show" "research proves"]
   :sunk-cost ["already invested" "come this far" "can't stop now" "too late"]
   :overconfidence ["definitely" "certainly" "guaranteed" "no doubt" "100%"]
   :hindsight-bias ["knew it" "obvious" "should have seen" "predictable"]})

(defn detect-biases
  "Detect potential cognitive biases in text."
  [text]
  (let [text-lower (str/lower-case (or text ""))
        detected
        (keep (fn [[bias patterns]]
                (let [matches (filter #(str/includes? text-lower %) patterns)]
                  (when (seq matches)
                    {:bias (name bias)
                     :triggers matches
                     :severity (if (> (count matches) 2) "high" "medium")})))
              bias-patterns)]
    
    {:text-analyzed (subs text 0 (min 200 (count text)))
     :biases-detected detected
     :total-biases (count detected)
     :risk-level (cond
                   (>= (count detected) 3) "high"
                   (>= (count detected) 1) "medium"
                   :else "low")
     :recommendation (if (seq detected)
                       "Review reasoning for identified biases"
                       "No obvious bias patterns detected")}))

;; ============================================
;; Decision Checklist
;; ============================================

(defn decision-checklist
  "Generate a comprehensive decision checklist."
  [decision context]
  {:decision decision
   :checklist
   [{:category "Circle of Competence"
     :questions ["Is this within my circle of competence?"
                 "Who knows more about this than me?"
                 "What am I likely missing?"]}
    {:category "Inversion"
     :questions ["How could this fail?"
                 "What would I need to believe for this to be wrong?"
                 "What's the worst case scenario?"]}
    {:category "Second-Order Effects"
     :questions ["What happens next?"
                 "And then what?"
                 "Who else will be affected?"]}
    {:category "Incentives"
     :questions ["What are the incentives at play?"
                 "Whose interests are aligned/misaligned?"
                 "How might incentives change behavior?"]}
    {:category "Margin of Safety"
     :questions ["What's my margin for error?"
                 "Can I afford to be wrong?"
                 "What's the downside protection?"]}
    {:category "Opportunity Cost"
     :questions ["What am I giving up?"
                 "What's the next best alternative?"
                 "Is this the best use of resources?"]}
    {:category "Reversibility"
     :questions ["Is this reversible?"
                 "What's the cost of being wrong?"
                 "Can I start small and scale?"]}]})

;; ============================================
;; Comprehensive Analysis
;; ============================================

(defn analyze-comprehensive
  "Comprehensive analysis combining all techniques."
  [model-names context]
  (let [situation (get context :situation "Unknown situation")
        decision (get context :decision "Decision")]
    {:latticework-analysis (latticework-analyze model-names context)
     :lollapalooza-detection (detect-lollapalooza model-names context)
     :inversion-analysis (invert situation)
     :two-track-analysis (two-track-analysis situation)
     :bias-detection (detect-biases (str context))
     :decision-checklist (decision-checklist decision context)}))
