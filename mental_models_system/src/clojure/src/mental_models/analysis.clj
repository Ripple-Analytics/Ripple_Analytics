(ns mental-models.analysis
  "Analysis Engine in Clojure
   Comprehensive analysis functions for mental models
   
   This module provides the core analysis capabilities:
   - Latticework analysis (Munger's approach)
   - Lollapalooza detection
   - Inversion techniques
   - Two-track analysis
   - Bias detection
   - Pattern recognition"
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
        
        ;; Check which failure modes might be active
        active-failures
        (filter (fn [fm]
                  (some #(str/includes? context-lower (str/lower-case %))
                        (:signals fm)))
                failure-modes)
        
        ;; Calculate confidence based on relevance
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
  "Apply Munger's latticework of mental models to a problem.
   
   The latticework approach uses multiple models from different
   disciplines to analyze a single problem, providing a more
   complete understanding than any single model could.
   
   Returns a comprehensive analysis with insights from each model."
  [model-names context]
  (let [models-to-use (if (seq model-names)
                        (keep models/get-model model-names)
                        (take 10 (models/get-all-models)))
        results (map #(analyze-with-model % context) models-to-use)
        
        ;; Group by discipline/category
        by-discipline (group-by :category results)
        
        ;; Find cross-discipline patterns
        high-confidence (filter #(> (:confidence %) 0.6) results)
        
        ;; Calculate combined confidence
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
                        :supporting-models []})
     :timestamp (java.time.Instant/now)}))

;; ============================================
;; Lollapalooza Detection
;; ============================================

(defn calculate-lollapalooza-multiplier
  "Calculate the effect multiplier from lollapalooza.
   
   When models reinforce each other, the effect is multiplicative,
   not additive. This calculates that multiplier."
  [n]
  (cond
    (>= n 5) 3.0
    (>= n 4) 2.5
    (>= n 3) 2.0
    (>= n 2) 1.5
    :else 1.0))

(defn detect-lollapalooza
  "Detect lollapalooza effects - when multiple models reinforce each other.
   
   A lollapalooza occurs when 3+ mental models point in the same
   direction with high confidence, creating a powerful combined effect.
   This is one of Munger's key insights."
  [model-names context]
  (let [analysis (latticework-analyze model-names context)
        results (:individual-analyses analysis)
        high-confidence (filter #(> (:confidence %) 0.7) results)
        is-lollapalooza (and (>= (count high-confidence) 3)
                             (> (count high-confidence) 0))]
    
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
  "Apply Munger's inversion technique.
   
   'Invert, always invert' - Carl Jacobi
   
   Instead of asking 'How do I succeed?', ask 'How do I fail?'
   Then avoid those failure modes."
  [problem]
  (let [inverted-questions
        [(str "What would guarantee failure in: " problem "?")
         (str "What should I definitely NOT do regarding: " problem "?")
         (str "What assumptions am I making about: " problem "?")
         (str "Who has failed at: " problem " and why?")
         (str "What are the worst possible outcomes of: " problem "?")]
        
        failure-categories
        ["Overconfidence" "Ignoring incentives" "Not considering second-order effects"
         "Social proof following" "Commitment escalation" "Availability bias"
         "Confirmation bias" "Sunk cost fallacy" "Anchoring"]]
    
    {:original-problem problem
     :inverted-questions inverted-questions
     :common-failure-categories failure-categories
     :approach "Answer the inverted questions first, then design to avoid those pitfalls"
     :munger-quote "All I want to know is where I'm going to die, so I'll never go there."}))

(defn invert-with-models
  "Apply inversion using specific mental models."
  [problem model-names]
  (let [base-inversion (invert problem)
        models-to-use (keep models/get-model model-names)
        model-inversions
        (map (fn [model]
               (let [failure-modes (:failure-modes model)]
                 {:model (:name model)
                  :how-to-fail (str "To fail using " (:name model) ": "
                                    (if (seq failure-modes)
                                      (:description (first failure-modes))
                                      "misapply the model"))
                  :avoidance-strategies (if (seq failure-modes)
                                          (mapcat :safeguards failure-modes)
                                          ["Apply model carefully"])}))
             models-to-use)]
    (assoc base-inversion :model-specific-inversions model-inversions)))

;; ============================================
;; Two-Track Analysis
;; ============================================

(defn two-track-analysis
  "Perform Munger's two-track analysis.
   
   Track 1: Rational/economic factors
   Track 2: Psychological/behavioral factors
   
   Both tracks must be considered for complete analysis."
  [situation]
  (let [rational-factors
        {:economic-incentives
         {:question "Who benefits financially?"
          :importance "critical"
          :munger-quote "Never think about anything else when you should be thinking about incentives"}
         
         :opportunity-costs
         {:question "What are we giving up?"
          :importance "high"
          :munger-quote "The best thing a human being can do is help another human being know more"}
         
         :second-order-effects
         {:question "What happens next? And then what?"
          :importance "critical"
          :munger-quote "Consequences have consequences"}
         
         :margin-of-safety
         {:question "What's our buffer for error?"
          :importance "critical"
          :munger-quote "A margin of safety is achieved when securities are purchased at prices sufficiently below underlying value"}
         
         :competitive-dynamics
         {:question "How will competitors respond?"
          :importance "high"}
         
         :scalability
         {:question "Does this scale? What are the limits?"
          :importance "medium"}}
        
        psychological-factors
        {:social-proof
         {:question "Are we following the crowd?"
          :bias-type "conformity"
          :danger-level "high"}
         
         :commitment-consistency
         {:question "Are we doubling down on past decisions?"
          :bias-type "escalation"
          :danger-level "high"}
         
         :incentive-caused-bias
         {:question "Are incentives distorting our view?"
          :bias-type "motivated reasoning"
          :danger-level "critical"}
         
         :availability-heuristic
         {:question "Are we overweighting recent/vivid events?"
          :bias-type "recency"
          :danger-level "medium"}
         
         :confirmation-bias
         {:question "Are we seeking confirming evidence only?"
          :bias-type "selective attention"
          :danger-level "high"}
         
         :loss-aversion
         {:question "Are we irrationally avoiding losses?"
          :bias-type "prospect theory"
          :danger-level "medium"}
         
         :anchoring
         {:question "Are we anchored to an initial number/idea?"
          :bias-type "adjustment"
          :danger-level "medium"}
         
         :authority-bias
         {:question "Are we deferring to authority inappropriately?"
          :bias-type "deference"
          :danger-level "medium"}}]
    
    {:situation situation
     :track-1-rational rational-factors
     :track-2-psychological psychological-factors
     :integration-questions
     ["How do rational and psychological factors interact here?"
      "Which psychological biases might distort our rational analysis?"
      "Are the incentives aligned with good decision-making?"]
     :recommendation "Analyze both tracks thoroughly before deciding"
     :timestamp (java.time.Instant/now)}))

;; ============================================
;; Bias Detection
;; ============================================

(def bias-patterns
  {:confirmation-bias
   ["confirms" "proves" "as expected" "knew it" "obviously"]
   
   :availability-bias
   ["recently" "just saw" "heard about" "in the news" "trending"]
   
   :anchoring
   ["starting point" "based on" "compared to" "relative to"]
   
   :social-proof
   ["everyone" "most people" "they all" "popular" "trending"]
   
   :authority-bias
   ["expert says" "according to" "studies show" "research proves"]
   
   :sunk-cost
   ["already invested" "come this far" "can't stop now" "too late"]
   
   :overconfidence
   ["definitely" "certainly" "guaranteed" "no doubt" "100%"]
   
   :hindsight-bias
   ["knew it" "obvious" "should have seen" "predictable"]})

(defn detect-biases
  "Detect potential cognitive biases in text or reasoning."
  [text]
  (let [text-lower (str/lower-case text)
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
;; Pattern Recognition
;; ============================================

(defn find-trend-patterns
  "Find trend patterns in sequential data."
  [data]
  (if (and (sequential? data) (> (count data) 2))
    (let [pairs (partition 2 1 data)
          diffs (map (fn [[a b]] (- b a)) pairs)
          avg-diff (/ (reduce + diffs) (count diffs))
          trend (cond
                  (> avg-diff 0.1) "increasing"
                  (< avg-diff -0.1) "decreasing"
                  :else "stable")]
      {:pattern-type "trend"
       :direction trend
       :average-change avg-diff
       :data-points (count data)})
    {:pattern-type "trend"
     :error "Insufficient data"}))

(defn find-anomaly-patterns
  "Find anomalies in data."
  [data]
  (if (and (sequential? data) (> (count data) 3))
    (let [mean-val (/ (reduce + data) (count data))
          variance (/ (reduce + (map #(Math/pow (- % mean-val) 2) data))
                      (count data))
          std-dev (Math/sqrt variance)
          anomalies (keep-indexed
                     (fn [i x]
                       (when (> (Math/abs (- x mean-val)) (* 2 std-dev))
                         {:index i :value x}))
                     data)]
      {:pattern-type "anomaly"
       :mean mean-val
       :std-dev std-dev
       :anomalies anomalies
       :anomaly-count (count anomalies)})
    {:pattern-type "anomaly"
     :error "Insufficient data"}))

(defn find-patterns
  "Find patterns in data based on pattern type."
  [data pattern-type]
  (case pattern-type
    "trend" (find-trend-patterns data)
    "anomaly" (find-anomaly-patterns data)
    "cycle" {:pattern-type "cycle"
             :detected false
             :note "Cycle detection requires more sophisticated analysis"}
    "correlation" {:pattern-type "correlation"
                   :note "Use statistical-engine for correlation analysis"}
    {:error "Unknown pattern type"}))

;; ============================================
;; Decision Framework
;; ============================================

(defn decision-checklist
  "Generate a comprehensive decision checklist using mental models."
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
                 "Can I start small and scale?"]}]
   
   :timestamp (java.time.Instant/now)})

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
     :decision-checklist (decision-checklist decision context)
     :timestamp (java.time.Instant/now)}))
