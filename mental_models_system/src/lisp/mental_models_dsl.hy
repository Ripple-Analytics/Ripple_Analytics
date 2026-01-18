;; Mental Models DSL in Hy (Lisp that compiles to Python)
;; This provides a more expressive way to define and analyze mental models
;; 
;; Benefits of Lisp for this domain:
;; 1. Homoiconicity - code as data, perfect for self-modifying systems
;; 2. Macros - can create domain-specific abstractions
;; 3. REPL-driven development - rapid iteration
;; 4. Functional programming - immutable data, pure functions

(import [dataclasses [dataclass field]])
(import [typing [Dict List Any Optional Callable Set]])
(import [enum [Enum]])
(import [datetime [datetime]])
(import [functools [reduce]])

;; ============================================
;; Core Data Structures
;; ============================================

(defclass Category [Enum]
  "Mental model categories"
  (setv ECONOMICS "economics")
  (setv PSYCHOLOGY "psychology")
  (setv PHYSICS "physics")
  (setv BIOLOGY "biology")
  (setv MATHEMATICS "mathematics")
  (setv SYSTEMS "systems")
  (setv DECISION_MAKING "decision_making")
  (setv STRATEGY "strategy"))

(defclass Severity [Enum]
  "Failure mode severity levels"
  (setv LOW "low")
  (setv MEDIUM "medium")
  (setv HIGH "high")
  (setv CRITICAL "critical"))

;; ============================================
;; Mental Model Definition Macros
;; ============================================

(defmacro defmodel [name category originator &rest body]
  "Define a mental model with its properties and rules.
   
   Usage:
   (defmodel circle-of-competence
     :category Category.DECISION_MAKING
     :originator \"Warren Buffett\"
     :description \"Know what you know and what you don't know\"
     :rules [(rule :when (outside-competence?) :then (avoid-decision))
             (rule :when (inside-competence?) :then (proceed-with-confidence))]
     :failure-modes [(failure-mode :name \"Overconfidence\" :severity Severity.HIGH)])"
  
  `(do
     (setv ~name
       {"name" ~(str name)
        "category" ~category
        "originator" ~originator
        ~@body})
     ~name))

(defmacro defrule [name &rest conditions-and-actions]
  "Define a rule for a mental model.
   
   Usage:
   (defrule avoid-outside-competence
     :when (fn [context] (not (in-competence-circle? context)))
     :then (fn [context] (recommend-caution context))
     :confidence 0.9)"
  
  `{"name" ~(str name)
    ~@conditions-and-actions})

(defmacro deffailure [name severity &rest body]
  "Define a failure mode for a mental model.
   
   Usage:
   (deffailure overconfidence-bias Severity.HIGH
     :description \"Believing you know more than you do\"
     :detection-signals [\"Ignoring contrary evidence\" \"Not seeking feedback\"]
     :safeguards [\"Regular competence audits\" \"Seek disconfirming evidence\"])"
  
  `{"name" ~(str name)
    "severity" ~severity
    ~@body})

;; ============================================
;; Analysis Functions (Pure Functional Style)
;; ============================================

(defn apply-model [model context]
  "Apply a mental model to a given context and return insights."
  (let [rules (get model "rules" [])
        applicable-rules (lfor rule rules
                              :if ((get rule "when") context)
                              rule)]
    {"model" (get model "name")
     "applicable_rules" (len applicable-rules)
     "recommendations" (lfor rule applicable-rules
                            ((get rule "then") context))
     "confidence" (if applicable-rules
                    (/ (sum (lfor r applicable-rules (get r "confidence" 0.5)))
                       (len applicable-rules))
                    0.0)}))

(defn compose-models [models context]
  "Compose multiple mental models to analyze a situation.
   This is the Munger latticework approach."
  (let [results (lfor model models (apply-model model context))
        combined-confidence (if results
                              (/ (sum (lfor r results (get r "confidence")))
                                 (len results))
                              0.0)]
    {"models_applied" (len models)
     "individual_results" results
     "combined_confidence" combined-confidence
     "lollapalooza_potential" (>= (len (lfor r results
                                            :if (> (get r "confidence") 0.7)
                                            r))
                                  3)}))

(defn detect-failure-modes [model context]
  "Detect potential failure modes in the current context."
  (let [failure-modes (get model "failure-modes" [])
        active-failures (lfor fm failure-modes
                             :if (check-failure-signals fm context)
                             fm)]
    {"model" (get model "name")
     "active_failures" active-failures
     "risk_level" (if active-failures
                    (max (lfor f active-failures
                              (severity-to-number (get f "severity"))))
                    0)}))

(defn check-failure-signals [failure-mode context]
  "Check if failure mode signals are present in context."
  (let [signals (get failure-mode "detection-signals" [])]
    (any (lfor signal signals
              (in (str.lower signal) (str.lower (str context)))))))

(defn severity-to-number [severity]
  "Convert severity enum to numeric value."
  (cond
    [(= severity Severity.LOW) 1]
    [(= severity Severity.MEDIUM) 2]
    [(= severity Severity.HIGH) 3]
    [(= severity Severity.CRITICAL) 4]
    [True 0]))

;; ============================================
;; Inversion (Munger's favorite technique)
;; ============================================

(defn invert [problem]
  "Apply inversion to a problem - think about what to avoid.
   'Invert, always invert' - Carl Jacobi"
  (let [inverted-questions
        [(+ "What would guarantee failure in: " problem "?")
         (+ "What should I definitely NOT do regarding: " problem "?")
         (+ "What assumptions am I making about: " problem "?")
         (+ "Who has failed at: " problem " and why?")]]
    {"original_problem" problem
     "inverted_questions" inverted-questions
     "approach" "Answer these questions first, then avoid those pitfalls"}))

(defn apply-inversion-to-model [model]
  "Generate inverted failure scenarios for a mental model."
  (let [name (get model "name")
        failure-modes (get model "failure-modes" [])]
    {"model" name
     "inverted_scenarios" (lfor fm failure-modes
                               {"failure" (get fm "name")
                                "how_to_cause" (+ "To fail at " name ", one would: "
                                                 (get fm "description" ""))
                                "prevention" (get fm "safeguards" [])})}))

;; ============================================
;; Lollapalooza Effect Detection
;; ============================================

(defn detect-lollapalooza [models context]
  "Detect when multiple models reinforce each other (lollapalooza effect).
   This is when 3+ models point in the same direction with high confidence."
  (let [results (lfor model models (apply-model model context))
        high-confidence (lfor r results
                             :if (> (get r "confidence") 0.7)
                             r)
        aligned-recommendations (find-aligned-recommendations high-confidence)]
    {"is_lollapalooza" (and (>= (len high-confidence) 3)
                            (> (len aligned-recommendations) 0))
     "models_aligned" (len high-confidence)
     "aligned_recommendations" aligned-recommendations
     "strength" (if high-confidence
                  (/ (sum (lfor r high-confidence (get r "confidence")))
                     (len high-confidence))
                  0.0)}))

(defn find-aligned-recommendations [results]
  "Find recommendations that appear across multiple model results."
  (let [all-recs (reduce (fn [acc r] (+ acc (get r "recommendations" [])))
                        results
                        [])
        rec-counts (reduce (fn [acc rec]
                           (do
                             (setv (get acc rec) (+ (get acc rec 0) 1))
                             acc))
                          all-recs
                          {})]
    (lfor [rec count] (.items rec-counts)
          :if (>= count 2)
          rec)))

;; ============================================
;; Two-Track Analysis (Rational + Psychological)
;; ============================================

(defn two-track-analysis [situation]
  "Perform Munger's two-track analysis:
   Track 1: What are the rational factors?
   Track 2: What psychological factors might distort judgment?"
  
  (let [rational-factors
        {"economic_incentives" "Who benefits financially?"
         "opportunity_costs" "What are we giving up?"
         "second_order_effects" "What happens next?"
         "margin_of_safety" "What's our buffer for error?"}
        
        psychological-factors
        {"social_proof" "Are we following the crowd?"
         "commitment_bias" "Are we doubling down on past decisions?"
         "incentive_bias" "Are incentives distorting our view?"
         "availability_bias" "Are we overweighting recent/vivid events?"
         "confirmation_bias" "Are we seeking confirming evidence?"
         "loss_aversion" "Are we irrationally avoiding losses?"}]
    
    {"situation" situation
     "track_1_rational" rational-factors
     "track_2_psychological" psychological-factors
     "recommendation" "Analyze both tracks before deciding"}))

;; ============================================
;; Model Registry and Lookup
;; ============================================

(setv *model-registry* {})

(defn register-model [model]
  "Register a mental model in the global registry."
  (setv (get *model-registry* (get model "name")) model)
  model)

(defn get-model [name]
  "Retrieve a mental model by name."
  (get *model-registry* name None))

(defn get-models-by-category [category]
  "Get all models in a category."
  (lfor [name model] (.items *model-registry*)
        :if (= (get model "category") category)
        model))

(defn search-models [query]
  "Search models by name or description."
  (let [query-lower (str.lower query)]
    (lfor [name model] (.items *model-registry*)
          :if (or (in query-lower (str.lower name))
                  (in query-lower (str.lower (get model "description" ""))))
          model)))

;; ============================================
;; Example Mental Model Definitions
;; ============================================

(setv circle-of-competence
  (defmodel circle-of-competence
    Category.DECISION_MAKING
    "Warren Buffett"
    "description" "Know the boundaries of your knowledge and stay within them"
    "rules" [(defrule stay-inside
               "when" (fn [ctx] (get ctx "in_competence" False))
               "then" (fn [ctx] "Proceed with confidence")
               "confidence" 0.9)
             (defrule avoid-outside
               "when" (fn [ctx] (not (get ctx "in_competence" True)))
               "then" (fn [ctx] "Seek expert advice or avoid")
               "confidence" 0.85)]
    "failure-modes" [(deffailure overconfidence Severity.HIGH
                       "description" "Believing your circle is larger than it is"
                       "detection-signals" ["No recent failures" "Ignoring expert advice"]
                       "safeguards" ["Regular competence audits" "Seek disconfirming evidence"])
                     (deffailure underconfidence Severity.MEDIUM
                       "description" "Not acting within your actual competence"
                       "detection-signals" ["Excessive hesitation" "Deferring on known topics"]
                       "safeguards" ["Track past successes" "Build confidence gradually"])]))

(register-model circle-of-competence)

(setv margin-of-safety
  (defmodel margin-of-safety
    Category.DECISION_MAKING
    "Benjamin Graham"
    "description" "Always leave room for error in your calculations"
    "rules" [(defrule require-margin
               "when" (fn [ctx] True)
               "then" (fn [ctx] (+ "Require " (str (get ctx "margin_percent" 25)) "% margin"))
               "confidence" 0.95)]
    "failure-modes" [(deffailure insufficient-margin Severity.CRITICAL
                       "description" "Not leaving enough buffer for unexpected events"
                       "detection-signals" ["Optimistic projections" "Ignoring tail risks"]
                       "safeguards" ["Double your estimated margin" "Stress test assumptions"])]))

(register-model margin-of-safety)

(setv second-order-thinking
  (defmodel second-order-thinking
    Category.SYSTEMS
    "Howard Marks"
    "description" "Think about the consequences of the consequences"
    "rules" [(defrule ask-then-what
               "when" (fn [ctx] True)
               "then" (fn [ctx] "Ask 'And then what?' at least 3 times")
               "confidence" 0.9)]
    "failure-modes" [(deffailure first-order-only Severity.HIGH
                       "description" "Only considering immediate effects"
                       "detection-signals" ["Quick decisions" "No scenario planning"]
                       "safeguards" ["Mandatory 'then what' exercise" "Consider 3 time horizons"])]))

(register-model second-order-thinking)

;; ============================================
;; Python Interop Functions
;; ============================================

(defn analyze-with-all-models [context]
  "Analyze a context using all registered models.
   Returns a comprehensive analysis suitable for Python consumption."
  (let [models (list (.values *model-registry*))
        composition (compose-models models context)
        lollapalooza (detect-lollapalooza models context)
        two-track (two-track-analysis (get context "situation" "Unknown situation"))
        failures (lfor model models (detect-failure-modes model context))]
    {"composition" composition
     "lollapalooza" lollapalooza
     "two_track_analysis" two-track
     "failure_analysis" failures
     "total_models" (len models)
     "timestamp" (str (datetime.now))}))

(defn get-all-models []
  "Get all registered models as a list (for Python)."
  (list (.values *model-registry*)))

(defn get-model-names []
  "Get all registered model names (for Python)."
  (list (.keys *model-registry*)))
