;; Analysis Engine in Hy (Lisp)
;; Comprehensive analysis functions for mental models
;; 
;; This module provides the core analysis capabilities:
;; - Latticework analysis (Munger's approach)
;; - Lollapalooza detection
;; - Inversion techniques
;; - Two-track analysis
;; - Bias detection
;; - Pattern recognition

(import typing [Dict List Any Optional Tuple Set])
(import datetime [datetime])
(import functools [reduce partial])
(import itertools [combinations permutations])
(import collections [Counter defaultdict])
(import math)

;; ============================================
;; Core Analysis Functions
;; ============================================

(defn latticework-analyze [models context]
  "Apply Munger's latticework of mental models to a problem.
   
   The latticework approach uses multiple models from different
   disciplines to analyze a single problem, providing a more
   complete understanding than any single model could.
   
   Returns a comprehensive analysis with insights from each model."
  (let [results (lfor model models
                     (analyze-with-model model context))
        disciplines (group-by-discipline results)
        cross-discipline-insights (find-cross-discipline-patterns disciplines)
        confidence (calculate-combined-confidence results)]
    {"context" context
     "models_applied" (len models)
     "individual_analyses" results
     "by_discipline" disciplines
     "cross_discipline_insights" cross-discipline-insights
     "combined_confidence" confidence
     "recommendation" (synthesize-recommendation results)
     "timestamp" (str (datetime.now))}))

(defn analyze-with-model [model context]
  "Apply a single mental model to analyze a context."
  (let [name (get model "name" "unknown")
        rules (get model "rules" [])
        applicable (lfor rule rules
                       :if (rule-applies? rule context)
                       rule)
        insights (lfor rule applicable
                      (apply-rule rule context))
        confidence (if applicable
                     (/ (sum (lfor r applicable (get r "confidence" 0.5)))
                        (len applicable))
                     0.3)]
    {"model" name
     "category" (get model "category" "general")
     "applicable_rules" (len applicable)
     "insights" insights
     "confidence" confidence
     "warnings" (detect-model-warnings model context)}))

(defn rule-applies? [rule context]
  "Check if a rule's conditions are met in the given context."
  (let [when-fn (get rule "when" None)]
    (if when-fn
        (try
          (when-fn context)
          (except [e Exception]
            False))
        True)))

(defn apply-rule [rule context]
  "Apply a rule and get its recommendation."
  (let [then-fn (get rule "then" None)
        name (get rule "name" "unnamed")]
    (if then-fn
        {"rule" name
         "recommendation" (try
                           (then-fn context)
                           (except [e Exception]
                             (+ "Apply " name)))
         "confidence" (get rule "confidence" 0.5)}
        {"rule" name
         "recommendation" (+ "Consider " name)
         "confidence" 0.5})))

(defn group-by-discipline [results]
  "Group analysis results by their discipline/category."
  (let [grouped (defaultdict list)]
    (for [r results]
      (.append (get grouped (get r "category" "general")) r))
    (dict grouped)))

(defn find-cross-discipline-patterns [disciplines]
  "Find patterns that appear across multiple disciplines."
  (let [all-insights []
        _ (for [[disc results] (.items disciplines)]
            (for [r results]
              (for [i (get r "insights" [])]
                (.append all-insights {"discipline" disc
                                       "insight" i}))))
        patterns []]
    (when (>= (len (list (.keys disciplines))) 2)
      (.append patterns {"type" "multi-discipline-convergence"
                        "description" "Multiple disciplines point to similar conclusions"
                        "strength" (/ (len all-insights) (max 1 (len (list (.keys disciplines)))))}))
    patterns))

(defn calculate-combined-confidence [results]
  "Calculate combined confidence from multiple model analyses."
  (if results
      (let [confidences (lfor r results (get r "confidence" 0.5))
            weights (lfor c confidences (if (> c 0.7) 1.5 1.0))
            weighted-sum (sum (lfor [c w] (zip confidences weights) (* c w)))
            total-weight (sum weights)]
        (/ weighted-sum total-weight))
      0.0))

(defn synthesize-recommendation [results]
  "Synthesize a final recommendation from multiple analyses."
  (let [high-confidence (lfor r results
                             :if (> (get r "confidence" 0) 0.7)
                             r)
        all-insights (reduce (fn [acc r]
                              (+ acc (get r "insights" [])))
                            results
                            [])]
    (if (>= (len high-confidence) 3)
        {"type" "strong"
         "message" "Multiple models strongly support this direction"
         "supporting_models" (lfor r high-confidence (get r "model"))}
        (if high-confidence
            {"type" "moderate"
             "message" "Some models support this direction"
             "supporting_models" (lfor r high-confidence (get r "model"))}
            {"type" "weak"
             "message" "No strong model support - proceed with caution"
             "supporting_models" []}))))

(defn detect-model-warnings [model context]
  "Detect potential issues when applying a model to a context."
  (let [warnings []
        failure-modes (get model "failure-modes" [])]
    (for [fm failure-modes]
      (when (failure-mode-active? fm context)
        (.append warnings {"failure_mode" (get fm "name")
                          "severity" (get fm "severity" "medium")
                          "description" (get fm "description" "")})))
    warnings))

(defn failure-mode-active? [failure-mode context]
  "Check if a failure mode's signals are present in context."
  (let [signals (get failure-mode "detection-signals" [])
        context-str (str context)]
    (any (lfor signal signals
              (in (str.lower (str signal)) (str.lower context-str))))))

;; ============================================
;; Lollapalooza Detection
;; ============================================

(defn detect-lollapalooza [models context]
  "Detect lollapalooza effects - when multiple models reinforce each other.
   
   A lollapalooza occurs when 3+ mental models point in the same
   direction with high confidence, creating a powerful combined effect.
   This is one of Munger's key insights."
  (let [results (lfor model models (analyze-with-model model context))
        high-confidence (lfor r results
                            :if (> (get r "confidence" 0) 0.7)
                            r)
        aligned (find-aligned-models high-confidence)
        is-lollapalooza (and (>= (len high-confidence) 3)
                             (> (len aligned) 0))]
    {"is_lollapalooza" is-lollapalooza
     "models_aligned" (len high-confidence)
     "alignment_groups" aligned
     "strength" (if high-confidence
                  (/ (sum (lfor r high-confidence (get r "confidence")))
                     (len high-confidence))
                  0.0)
     "effect_multiplier" (calculate-lollapalooza-multiplier high-confidence)
     "recommendation" (if is-lollapalooza
                        "Strong lollapalooza effect detected - high conviction warranted"
                        "No lollapalooza effect - consider more models")}))

(defn find-aligned-models [results]
  "Find groups of models that point in the same direction."
  (let [groups []
        insights-by-model {}]
    (for [r results]
      (setv (get insights-by-model (get r "model"))
            (lfor i (get r "insights" []) (get i "recommendation" ""))))
    (when (>= (len results) 2)
      (.append groups {"models" (lfor r results (get r "model"))
                      "type" "convergent"
                      "description" "Models converge on similar conclusions"}))
    groups))

(defn calculate-lollapalooza-multiplier [high-confidence-results]
  "Calculate the effect multiplier from lollapalooza.
   
   When models reinforce each other, the effect is multiplicative,
   not additive. This calculates that multiplier."
    (let [n (len high-confidence-results)]
      (cond
        (>= n 5) 3.0
        (>= n 4) 2.5
        (>= n 3) 2.0
        (>= n 2) 1.5
        True 1.0)))

;; ============================================
;; Inversion Analysis
;; ============================================

(defn invert [problem]
  "Apply Munger's inversion technique.
   
   'Invert, always invert' - Carl Jacobi
   
   Instead of asking 'How do I succeed?', ask 'How do I fail?'
   Then avoid those failure modes."
  (let [inverted-questions
        [(+ "What would guarantee failure in: " problem "?")
         (+ "What should I definitely NOT do regarding: " problem "?")
         (+ "What assumptions am I making about: " problem "?")
         (+ "Who has failed at: " problem " and why?")
         (+ "What are the worst possible outcomes of: " problem "?")]
        
        failure-categories
        ["Overconfidence" "Ignoring incentives" "Not considering second-order effects"
         "Social proof following" "Commitment escalation" "Availability bias"
         "Confirmation bias" "Sunk cost fallacy" "Anchoring"]]
    
    {"original_problem" problem
     "inverted_questions" inverted-questions
     "common_failure_categories" failure-categories
     "approach" "Answer the inverted questions first, then design to avoid those pitfalls"
     "munger_quote" "All I want to know is where I'm going to die, so I'll never go there."}))

(defn invert-with-models [problem models]
  "Apply inversion using specific mental models."
  (let [base-inversion (invert problem)
        model-inversions (lfor model models
                             (invert-model-application model problem))]
    (setv (get base-inversion "model_specific_inversions") model-inversions)
    base-inversion))

(defn invert-model-application [model problem]
  "Generate inverted analysis for a specific model."
  (let [name (get model "name" "unknown")
        failure-modes (get model "failure-modes" [])]
    {"model" name
     "how_to_fail" (+ "To fail using " name ": " 
                     (if failure-modes
                         (get (get failure-modes 0) "description" "misapply the model")
                         "misapply the model"))
     "avoidance_strategies" (if failure-modes
                              (lfor fm failure-modes
                                   (get fm "safeguards" ["Be careful"]))
                              [["Apply model carefully"]])}))

;; ============================================
;; Two-Track Analysis
;; ============================================

(defn two-track-analysis [situation]
  "Perform Munger's two-track analysis.
   
   Track 1: Rational/economic factors
   Track 2: Psychological/behavioral factors
   
   Both tracks must be considered for complete analysis."
  (let [rational-factors
        {"economic_incentives" 
         {"question" "Who benefits financially?"
          "importance" "critical"
          "munger_quote" "Never think about anything else when you should be thinking about incentives"}
         
         "opportunity_costs"
         {"question" "What are we giving up?"
          "importance" "high"
          "munger_quote" "The best thing a human being can do is help another human being know more"}
         
         "second_order_effects"
         {"question" "What happens next? And then what?"
          "importance" "critical"
          "munger_quote" "Consequences have consequences"}
         
         "margin_of_safety"
         {"question" "What's our buffer for error?"
          "importance" "critical"
          "munger_quote" "A margin of safety is achieved when securities are purchased at prices sufficiently below underlying value"}
         
         "competitive_dynamics"
         {"question" "How will competitors respond?"
          "importance" "high"}
         
         "scalability"
         {"question" "Does this scale? What are the limits?"
          "importance" "medium"}}
        
        psychological-factors
        {"social_proof"
         {"question" "Are we following the crowd?"
          "bias_type" "conformity"
          "danger_level" "high"}
         
         "commitment_consistency"
         {"question" "Are we doubling down on past decisions?"
          "bias_type" "escalation"
          "danger_level" "high"}
         
         "incentive_caused_bias"
         {"question" "Are incentives distorting our view?"
          "bias_type" "motivated reasoning"
          "danger_level" "critical"}
         
         "availability_heuristic"
         {"question" "Are we overweighting recent/vivid events?"
          "bias_type" "recency"
          "danger_level" "medium"}
         
         "confirmation_bias"
         {"question" "Are we seeking confirming evidence only?"
          "bias_type" "selective attention"
          "danger_level" "high"}
         
         "loss_aversion"
         {"question" "Are we irrationally avoiding losses?"
          "bias_type" "prospect theory"
          "danger_level" "medium"}
         
         "anchoring"
         {"question" "Are we anchored to an initial number/idea?"
          "bias_type" "adjustment"
          "danger_level" "medium"}
         
         "authority_bias"
         {"question" "Are we deferring to authority inappropriately?"
          "bias_type" "deference"
          "danger_level" "medium"}}]
    
    {"situation" situation
     "track_1_rational" rational-factors
     "track_2_psychological" psychological-factors
     "integration_questions" 
     ["How do rational and psychological factors interact here?"
      "Which psychological biases might distort our rational analysis?"
      "Are the incentives aligned with good decision-making?"]
     "recommendation" "Analyze both tracks thoroughly before deciding"
     "timestamp" (str (datetime.now))}))

;; ============================================
;; Bias Detection
;; ============================================

(defn detect-biases [text]
  "Detect potential cognitive biases in text or reasoning."
  (let [bias-patterns
        {"confirmation_bias" 
         ["confirms" "proves" "as expected" "knew it" "obviously"]
         
         "availability_bias"
         ["recently" "just saw" "heard about" "in the news" "trending"]
         
         "anchoring"
         ["starting point" "based on" "compared to" "relative to"]
         
         "social_proof"
         ["everyone" "most people" "they all" "popular" "trending"]
         
         "authority_bias"
         ["expert says" "according to" "studies show" "research proves"]
         
         "sunk_cost"
         ["already invested" "come this far" "can't stop now" "too late"]
         
         "overconfidence"
         ["definitely" "certainly" "guaranteed" "no doubt" "100%"]
         
         "hindsight_bias"
         ["knew it" "obvious" "should have seen" "predictable"]}
        
        text-lower (str.lower text)
        detected []]
    
    (for [[bias patterns] (.items bias-patterns)]
      (let [matches (lfor p patterns
                        :if (in p text-lower)
                        p)]
        (when matches
          (.append detected {"bias" bias
                            "triggers" matches
                            "severity" (if (> (len matches) 2) "high" "medium")}))))
    
    {"text_analyzed" (cut text 0 200)
     "biases_detected" detected
     "total_biases" (len detected)
          "risk_level" (cond
                        (>= (len detected) 3) "high"
                        (>= (len detected) 1) "medium"
                        True "low")
     "recommendation" (if detected
                        "Review reasoning for identified biases"
                        "No obvious bias patterns detected")}))

;; ============================================
;; Pattern Recognition
;; ============================================

(defn find-patterns [data pattern-type]
  "Find patterns in data based on pattern type."
  (cond
    (= pattern-type "trend") (find-trend-patterns data)
    (= pattern-type "cycle") (find-cycle-patterns data)
    (= pattern-type "anomaly") (find-anomaly-patterns data)
    (= pattern-type "correlation") (find-correlation-patterns data)
    True {"error" "Unknown pattern type"}))

(defn find-trend-patterns [data]
  "Find trend patterns in sequential data."
  (if (and (isinstance data list) (> (len data) 2))
      (let [diffs (lfor [a b] (zip data (cut data 1))
                       (- b a))
            avg-diff (/ (sum diffs) (len diffs))
                        trend (cond
                               (> avg-diff 0.1) "increasing"
                               (< avg-diff -0.1) "decreasing"
                               True "stable")]
        {"pattern_type" "trend"
         "direction" trend
         "average_change" avg-diff
         "data_points" (len data)})
      {"pattern_type" "trend"
       "error" "Insufficient data"}))

(defn find-cycle-patterns [data]
  "Find cyclical patterns in data."
  {"pattern_type" "cycle"
   "detected" False
   "note" "Cycle detection requires more sophisticated analysis"})

(defn find-anomaly-patterns [data]
  "Find anomalies in data."
  (if (and (isinstance data list) (> (len data) 3))
      (let [mean-val (/ (sum data) (len data))
            variance (/ (sum (lfor x data (** (- x mean-val) 2)))
                       (len data))
            std-dev (** variance 0.5)
            anomalies (lfor [i x] (enumerate data)
                          :if (> (abs (- x mean-val)) (* 2 std-dev))
                          {"index" i "value" x})]
        {"pattern_type" "anomaly"
         "mean" mean-val
         "std_dev" std-dev
         "anomalies" anomalies
         "anomaly_count" (len anomalies)})
      {"pattern_type" "anomaly"
       "error" "Insufficient data"}))

(defn find-correlation-patterns [data]
  "Find correlation patterns between variables."
  {"pattern_type" "correlation"
   "note" "Use statistical_engine.hy for correlation analysis"})

;; ============================================
;; Decision Framework
;; ============================================

(defn decision-checklist [decision context]
  "Generate a comprehensive decision checklist using mental models."
  {"decision" decision
   "checklist" [
     {"category" "Circle of Competence"
      "questions" ["Is this within my circle of competence?"
                   "Who knows more about this than me?"
                   "What am I likely missing?"]}
     
     {"category" "Inversion"
      "questions" ["How could this fail?"
                   "What would I need to believe for this to be wrong?"
                   "What's the worst case scenario?"]}
     
     {"category" "Second-Order Effects"
      "questions" ["What happens next?"
                   "And then what?"
                   "Who else will be affected?"]}
     
     {"category" "Incentives"
      "questions" ["What are the incentives at play?"
                   "Whose interests are aligned/misaligned?"
                   "How might incentives change behavior?"]}
     
     {"category" "Margin of Safety"
      "questions" ["What's my margin for error?"
                   "Can I afford to be wrong?"
                   "What's the downside protection?"]}
     
     {"category" "Opportunity Cost"
      "questions" ["What am I giving up?"
                   "What's the next best alternative?"
                   "Is this the best use of resources?"]}
     
     {"category" "Reversibility"
      "questions" ["Is this reversible?"
                   "What's the cost of being wrong?"
                   "Can I start small and scale?"]}]
   
   "timestamp" (str (datetime.now))})

;; ============================================
;; Export Functions for Python
;; ============================================

(defn analyze-comprehensive [models context]
  "Comprehensive analysis combining all techniques."
  (let [latticework (latticework-analyze models context)
        lollapalooza (detect-lollapalooza models context)
        inversion (invert (get context "situation" "Unknown situation"))
        two-track (two-track-analysis (get context "situation" "Unknown"))
        biases (detect-biases (str context))
        checklist (decision-checklist (get context "decision" "Decision") context)]
    {"latticework_analysis" latticework
     "lollapalooza_detection" lollapalooza
     "inversion_analysis" inversion
     "two_track_analysis" two-track
     "bias_detection" biases
     "decision_checklist" checklist
     "timestamp" (str (datetime.now))}))
