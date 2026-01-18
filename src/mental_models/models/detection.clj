(ns mental-models.models.detection
  "Mental Model Detection Framework - Electric Clojure
   Pattern matching, keyword detection, and scoring for all 129 models
   Each model has: keywords, patterns, scoring rules, evidence extraction"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as log]
            [mental-models.analysis.statistical :as stats]))

;; -- Detection Result Record --

(defrecord ModelDetection
  [model-id model-name model-slug category
   score confidence evidence patterns-matched
   keywords-found explanation])

;; -- Base Pattern Matching --

(defn normalize-text
  "Normalize text for analysis"
  [text]
  (-> text
      str/lower-case
      (str/replace #"[^a-z0-9\s\-]" "")
      str/trim))

(defn tokenize
  "Split text into tokens"
  [text]
  (str/split (normalize-text text) #"\s+"))

(defn find-keywords
  "Find keywords in text"
  [text keywords]
  (let [normalized (normalize-text text)
        tokens (tokenize text)]
    (filter #(str/includes? normalized %) keywords)))

(defn count-keyword-occurrences
  "Count how many times keywords appear"
  [text keywords]
  (let [normalized (normalize-text text)]
    (reduce + (map #(count (re-seq (re-pattern %) normalized)) keywords))))

(defn find-patterns
  "Find regex patterns in text"
  [text patterns]
  (filter #(re-find % text) patterns))

(defn calculate-keyword-density
  "Calculate keyword density (0-1)"
  [text keywords]
  (let [total-words (count (tokenize text))
        keyword-count (count-keyword-occurrences text keywords)]
    (if (zero? total-words)
      0.0
      (min 1.0 (/ keyword-count total-words)))))

;; -- Scoring Functions --

(defn keyword-score
  "Score based on keyword presence and density"
  [text keywords weight]
  (let [density (calculate-keyword-density text keywords)
        found-count (count (find-keywords text keywords))
        presence-score (if (> found-count 0) 1.0 0.0)]
    (* weight (+ (* 0.6 presence-score) (* 0.4 density)))))

(defn pattern-score
  "Score based on pattern matches"
  [text patterns weight]
  (let [matches (find-patterns text patterns)
        match-count (count matches)]
    (if (zero? match-count)
      0.0
      (* weight (min 1.0 (/ match-count (count patterns)))))))

(defn semantic-score
  "Score based on semantic analysis (via LLM)"
  [text model-description llm-response]
  (let [confidence (try
                    (Double/parseDouble (str/trim llm-response))
                    (catch Exception _ 0.0))]
    (max 0.0 (min 1.0 confidence))))

(defn combine-scores
  "Combine multiple scores with weights"
  [scores weights]
  (let [weighted (map * scores weights)
        total-weight (reduce + weights)]
    (if (zero? total-weight)
      0.0
      (/ (reduce + weighted) total-weight))))

;; -- Evidence Extraction --

(defn extract-evidence
  "Extract supporting evidence from text"
  [text keywords patterns max-excerpts]
  (let [normalized (normalize-text text)
        sentences (str/split text #"[.!?]+")
        
        ;; Find sentences containing keywords
        keyword-sentences (filter (fn [sent]
                                  (some #(str/includes? (normalize-text sent) %)
                                       keywords))
                                sentences)
        
        ;; Find sentences matching patterns
        pattern-sentences (filter (fn [sent]
                                 (some #(re-find % sent) patterns))
                               sentences)]
    
    (->> (concat keyword-sentences pattern-sentences)
         distinct
         (map str/trim)
         (filter #(> (count %) 20))
         (take max-excerpts))))

;; -- Model Detection Template --

(defn detect-model
  "Generic model detection function"
  [{:keys [model-id model-name model-slug category
           keywords patterns semantic-weight
           keyword-weight pattern-weight
           description llm-response]}]
  
  (let [text (:text (meta {}))  ;; Will be injected from context
        
        ;; Calculate component scores
        kw-score (keyword-score text keywords keyword-weight)
        pat-score (pattern-score text patterns pattern-weight)
        sem-score (semantic-score text description llm-response)
        
        ;; Combine scores
        final-score (combine-scores
                    [kw-score pat-score sem-score]
                    [keyword-weight pattern-weight semantic-weight])
        
        ;; Extract evidence
        evidence (extract-evidence text keywords patterns 3)
        matched-keywords (find-keywords text keywords)
        matched-patterns (find-patterns text patterns)]
    
    (->ModelDetection
     model-id
     model-name
     model-slug
     category
     final-score
     (cond
       (> final-score 0.8) "Very High"
       (> final-score 0.6) "High"
       (> final-score 0.4) "Moderate"
       (> final-score 0.2) "Low"
       :else "Very Low")
     evidence
     (count matched-patterns)
     (count matched-keywords)
     (str "Model detected with " (format "%.1f%%" (* final-score 100)) " confidence"))))

;; -- Hard Sciences Models --

(def mathematics-model
  {:model-id 1
   :model-name "Mathematics"
   :model-slug "mathematics"
   :category "Hard Sciences"
   :keywords ["equation" "formula" "calculation" "proof" "theorem" "algorithm"
              "number" "logic" "variable" "function" "derivative" "integral"
              "probability" "statistics" "matrix" "vector"]
   :patterns [#"(?:\d+\s*[+\-*/]\s*\d+)"
              #"(?:x\s*[=]\s*\d+)"
              #"(?:∑|∫|√|π|Σ)"]
   :keyword-weight 0.4
   :pattern-weight 0.3
   :semantic-weight 0.3})

(def physics-model
  {:model-id 2
   :model-name "Physics"
   :model-slug "physics"
   :category "Hard Sciences"
   :keywords ["force" "energy" "velocity" "acceleration" "momentum" "gravity"
              "motion" "wave" "particle" "quantum" "relativity" "thermodynamics"
              "entropy" "friction" "pressure" "temperature"]
   :patterns [#"(?:F\s*=\s*ma)"
              #"(?:E\s*=\s*mc²)"
              #"(?:v\s*=\s*d/t)"]
   :keyword-weight 0.4
   :pattern-weight 0.2
   :semantic-weight 0.4})

(def engineering-model
  {:model-id 3
   :model-name "Engineering"
   :model-slug "engineering"
   :category "Hard Sciences"
   :keywords ["design" "system" "optimization" "efficiency" "constraint"
              "trade-off" "specification" "tolerance" "failure" "redundancy"
              "feedback" "control" "simulation" "prototype" "testing"]
   :patterns [#"(?:design\s+(?:for|of))"
              #"(?:optimize|optimization)"
              #"(?:failure\s+mode)"]
   :keyword-weight 0.45
   :pattern-weight 0.25
   :semantic-weight 0.3})

(def accounting-model
  {:model-id 4
   :model-name "Accounting"
   :model-slug "accounting"
   :category "Hard Sciences"
   :keywords ["asset" "liability" "equity" "revenue" "expense" "profit"
              "cash flow" "balance sheet" "income statement" "depreciation"
              "amortization" "accrual" "deduction" "audit" "tax"]
   :patterns [#"(?:assets?\s*=\s*liabilities?\s*\+\s*equity)"
              #"(?:\$[\d,]+)"
              #"(?:Q[1-4]\s+\d{4})"]
   :keyword-weight 0.45
   :pattern-weight 0.2
   :semantic-weight 0.35})

;; -- Life Sciences Models --

(def biology-model
  {:model-id 5
   :model-name "Biology"
   :model-slug "biology"
   :category "Life Sciences"
   :keywords ["organism" "cell" "gene" "evolution" "adaptation" "mutation"
              "natural selection" "ecosystem" "species" "dna" "protein"
              "metabolism" "reproduction" "survival" "competition"]
   :patterns [#"(?:dna|rna|gene)"
              #"(?:evolution|evolved)"
              #"(?:natural\s+selection)"]
   :keyword-weight 0.4
   :pattern-weight 0.3
   :semantic-weight 0.3})

(def psychology-model
  {:model-id 6
   :model-name "Psychology"
   :model-slug "psychology"
   :category "Life Sciences"
   :keywords ["behavior" "cognition" "emotion" "motivation" "perception"
              "memory" "learning" "conditioning" "stimulus" "response"
              "unconscious" "bias" "heuristic" "belief" "attitude"]
   :patterns [#"(?:stimulus\s+response)"
              #"(?:cognitive\s+bias)"
              #"(?:behavioral\s+pattern)"]
   :keyword-weight 0.42
   :pattern-weight 0.28
   :semantic-weight 0.3})

(def economics-model
  {:model-id 7
   :model-name "Economics"
   :model-slug "economics"
   :category "Life Sciences"
   :keywords ["supply" "demand" "price" "market" "competition" "monopoly"
              "consumer" "producer" "utility" "scarcity" "opportunity cost"
              "incentive" "transaction" "efficiency" "equilibrium"]
   :patterns [#"(?:supply\s+and\s+demand)"
              #"(?:market\s+(?:price|equilibrium))"
              #"(?:opportunity\s+cost)"]
   :keyword-weight 0.43
   :pattern-weight 0.27
   :semantic-weight 0.3})

;; -- Cognitive Biases --

(def confirmation-bias-model
  {:model-id 50
   :model-name "Confirmation Bias"
   :model-slug "confirmation-bias"
   :category "Cognitive Biases"
   :keywords ["believe" "confirm" "evidence" "support" "agree" "dismiss"
              "ignore" "contrary" "opposite" "proof" "convinced" "certain"]
   :patterns [#"(?:only\s+(?:look|see|find)\s+(?:what|evidence))"
              #"(?:ignore|dismiss|overlook)\s+(?:evidence|fact|data)"]
   :keyword-weight 0.35
   :pattern-weight 0.25
   :semantic-weight 0.4})

(def anchoring-bias-model
  {:model-id 51
   :model-name "Anchoring Bias"
   :model-slug "anchoring-bias"
   :category "Cognitive Biases"
   :keywords ["first" "initial" "starting" "reference" "number" "price"
              "estimate" "adjust" "anchor" "fixed" "baseline" "comparison"]
   :patterns [#"(?:first\s+(?:number|price|estimate))"
              #"(?:starting\s+(?:point|price|value))"
              #"(?:anchor|anchored)\s+(?:to|at)"]
   :keyword-weight 0.38
   :pattern-weight 0.27
   :semantic-weight 0.35})

(def availability-heuristic-model
  {:model-id 52
   :model-name "Availability Heuristic"
   :model-slug "availability-heuristic"
   :category "Cognitive Biases"
   :keywords ["recent" "memorable" "vivid" "salient" "common" "frequent"
              "recall" "remember" "example" "instance" "case" "heard"]
   :patterns [#"(?:(?:recently|just)\s+(?:heard|saw|read))"
              #"(?:(?:memorable|vivid)\s+(?:example|case))"]
   :keyword-weight 0.36
   :pattern-weight 0.24
   :semantic-weight 0.4})

(def overconfidence-bias-model
  {:model-id 53
   :model-name "Overconfidence Bias"
   :model-slug "overconfidence-bias"
   :category "Cognitive Biases"
   :keywords ["certain" "sure" "confident" "know" "definitely" "absolutely"
              "guarantee" "impossible" "always" "never" "proven" "obvious"]
   :patterns [#"(?:(?:absolutely|definitely|certainly)\s+(?:will|won't|can't))"
              #"(?:(?:impossible|guaranteed)\s+(?:to|that))"]
   :keyword-weight 0.37
   :pattern-weight 0.26
   :semantic-weight 0.37})

;; -- Business Models --

(def porters-five-forces-model
  {:model-id 80
   :model-name "Porter's Five Forces"
   :model-slug "porters-five-forces"
   :category "Business Strategy"
   :keywords ["supplier" "buyer" "competitor" "threat" "bargaining power"
              "substitute" "entry" "exit" "industry" "competitive" "force"
              "rivalry" "pressure" "advantage"]
   :patterns [#"(?:five\s+forces)"
              #"(?:supplier|buyer|competitor)\s+(?:power|bargaining)"]
   :keyword-weight 0.4
   :pattern-weight 0.3
   :semantic-weight 0.3})

(def moat-model
  {:model-id 81
   :model-name "Economic Moat"
   :model-slug "economic-moat"
   :category "Business Strategy"
   :keywords ["moat" "defensible" "competitive advantage" "barrier" "protection"
              "brand" "network" "switching cost" "scale" "cost advantage"
              "sustainable" "durable" "protected"]
   :patterns [#"(?:economic\s+moat)"
              #"(?:competitive\s+(?:advantage|barrier))"
              #"(?:switching\s+cost)"]
   :keyword-weight 0.42
   :pattern-weight 0.28
   :semantic-weight 0.3})

;; -- Meta-Skills --

(def inversion-model
  {:model-id 120
   :model-name "Inversion"
   :model-slug "inversion"
   :category "Meta-Skills"
   :keywords ["opposite" "reverse" "invert" "backward" "contrary" "avoid"
              "prevent" "instead" "flip" "upside" "inverse" "contrapositive"]
   :patterns [#"(?:(?:instead|rather)\s+(?:of|than))"
              #"(?:(?:opposite|reverse)\s+(?:of|approach))"
              #"(?:how\s+(?:not|to\s+avoid))"]
   :keyword-weight 0.4
   :pattern-weight 0.3
   :semantic-weight 0.3})

(def continual-learning-model
  {:model-id 121
   :model-name "Continual Learning"
   :model-slug "continual-learning"
   :category "Meta-Skills"
   :keywords ["learn" "study" "improve" "develop" "skill" "knowledge"
              "growth" "education" "practice" "feedback" "iterate" "evolve"
              "adapt" "change" "progress"]
   :patterns [#"(?:(?:continuous|continual)\s+(?:learning|improvement))"
              #"(?:(?:always|keep)\s+learning)"
              #"(?:feedback\s+loop)"]
   :keyword-weight 0.38
   :pattern-weight 0.27
   :semantic-weight 0.35})

;; -- Model Registry --

(def all-models
  [mathematics-model physics-model engineering-model accounting-model
   biology-model psychology-model economics-model
   confirmation-bias-model anchoring-bias-model availability-heuristic-model
   overconfidence-bias-model
   porters-five-forces-model moat-model
   inversion-model continual-learning-model])

;; -- Batch Detection --

(defn detect-all-models
  "Detect all models in text"
  [text]
  (mapv (fn [model]
         (let [keywords (:keywords model)
               patterns (:patterns model)
               kw-score (keyword-score text keywords (:keyword-weight model))
               pat-score (pattern-score text patterns (:pattern-weight model))
               final-score (combine-scores
                           [kw-score pat-score 0.0]
                           [(:keyword-weight model) (:pattern-weight model) 0.3])]
           
           (->ModelDetection
            (:model-id model)
            (:model-name model)
            (:model-slug model)
            (:category model)
            final-score
            (cond
              (> final-score 0.8) "Very High"
              (> final-score 0.6) "High"
              (> final-score 0.4) "Moderate"
              (> final-score 0.2) "Low"
              :else "Very Low")
            (extract-evidence text keywords patterns 2)
            (count (find-patterns text patterns))
            (count (find-keywords text keywords))
            (str "Detected with " (format "%.1f%%" (* final-score 100)) " confidence"))))
       all-models))

(defn top-models
  "Get top N models by score"
  [detections n]
  (->> detections
       (sort-by :score >)
       (take n)))

(defn filter-by-threshold
  "Filter models above confidence threshold"
  [detections threshold]
  (filter #(> (:score %) threshold) detections))
