(ns mental-models.predict.engine
  "Predictive Mental Models Engine
   
   Learns from:
   - Base mental models (Munger's 25 + extended)
   - Historical case studies with known outcomes
   - Pattern combinations that led to specific results
   
   Applies to:
   - News stories
   - Business situations
   - Investment decisions
   
   Outputs:
   - % overlap with known patterns
   - Predicted likely outcomes
   - Confidence scores
   - Similar historical cases"
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; ============================================================================
;; Historical Case Studies with Known Outcomes
;; ============================================================================

(def case-studies
  "Historical cases with mental models detected and actual outcomes"
  [
   ;; FINANCIAL DISASTERS
   {:id 1
    :name "Enron Collapse (2001)"
    :category "Corporate Fraud"
    :models-detected ["Incentive-Caused Bias" "Social Proof" "Commitment & Consistency" 
                      "Authority Bias" "Denial" "Excessive Self-Regard"]
    :outcome :catastrophic-failure
    :outcome-description "Complete corporate collapse, $74B market cap destroyed, executives imprisoned"
    :warning-signs ["Complex accounting structures" "Aggressive revenue recognition" "Executive stock sales"
                    "Whistleblower suppression" "Auditor conflicts of interest"]
    :lessons ["When incentives are misaligned, fraud is likely" "Social proof can amplify bad behavior"
              "Complexity often hides problems"]}
   
   {:id 2
    :name "2008 Financial Crisis"
    :category "Systemic Risk"
    :models-detected ["Social Proof" "Incentive-Caused Bias" "Availability Bias" 
                      "Overconfidence" "Denial" "Lollapalooza Effect"]
    :outcome :catastrophic-failure
    :outcome-description "Global financial meltdown, $10T+ wealth destroyed, multi-year recession"
    :warning-signs ["Housing prices always go up mentality" "Complex derivatives nobody understood"
                    "Rating agency conflicts" "Leverage at historic highs" "Everyone doing it"]
    :lessons ["When everyone believes the same thing, be skeptical" "Leverage amplifies everything"
              "Lollapalooza of biases creates catastrophic outcomes"]}
   
   {:id 3
    :name "Theranos Fraud"
    :category "Corporate Fraud"
    :models-detected ["Authority Bias" "Social Proof" "Commitment & Consistency"
                      "Liking/Loving Tendency" "Denial" "Excessive Self-Regard"]
    :outcome :catastrophic-failure
    :outcome-description "Complete fraud exposed, founder imprisoned, $9B valuation to zero"
    :warning-signs ["Charismatic founder" "Board of famous names, no domain experts"
                    "Secrecy about technology" "Attacking critics" "No peer review"]
    :lessons ["Authority figures can be wrong" "Charisma is not competence"
              "Extraordinary claims require extraordinary evidence"]}
   
   ;; SUCCESSFUL INVESTMENTS
   {:id 4
    :name "Berkshire Hathaway - See's Candies (1972)"
    :category "Value Investment"
    :models-detected ["Circle of Competence" "Margin of Safety" "Moat" 
                      "Brand Value" "Pricing Power" "Owner Earnings"]
    :outcome :exceptional-success
    :outcome-description "$25M investment generated $2B+ in pre-tax earnings over 50 years"
    :success-factors ["Strong brand loyalty" "Pricing power" "Simple business model"
                      "Low capital requirements" "Regional monopoly"]
    :lessons ["Great brands can raise prices" "Simple businesses are easier to understand"
              "Low capital intensity = high returns on capital"]}
   
   {:id 5
    :name "Amazon Early Investment"
    :category "Growth Investment"
    :models-detected ["Network Effects" "Scale Economics" "Switching Costs"
                      "First Mover Advantage" "Long-term Thinking" "Reinvestment"]
    :outcome :exceptional-success
    :outcome-description "1997 IPO at $18, now worth $3000+, created $1.5T+ in value"
    :success-factors ["Customer obsession" "Willingness to be misunderstood" "Long-term focus"
                      "Continuous reinvestment" "Platform economics"]
    :lessons ["Network effects compound" "Short-term losses can fund long-term dominance"
              "Customer focus beats competitor focus"]}
   
   {:id 6
    :name "Apple Turnaround (1997-2011)"
    :category "Corporate Turnaround"
    :models-detected ["Circle of Competence" "Focus" "Brand Value" "Design Thinking"
                      "Vertical Integration" "Ecosystem Lock-in"]
    :outcome :exceptional-success
    :outcome-description "Near-bankrupt to world's most valuable company"
    :success-factors ["Ruthless focus on few products" "Design excellence" "Ecosystem creation"
                      "Supply chain mastery" "Retail innovation"]
    :lessons ["Focus beats diversification" "Design is a competitive advantage"
              "Ecosystems create switching costs"]}
   
   ;; FAILURES TO LEARN FROM
   {:id 7
    :name "Kodak Digital Photography"
    :category "Disruption Failure"
    :models-detected ["Denial" "Commitment & Consistency" "Incentive-Caused Bias"
                      "Status Quo Bias" "Sunk Cost Fallacy"]
    :outcome :catastrophic-failure
    :outcome-description "Invented digital camera but failed to adapt, bankruptcy in 2012"
    :warning-signs ["Protecting legacy business" "Dismissing new technology" "Short-term focus"
                    "Incentives tied to film sales"]
    :lessons ["Disrupt yourself before others do" "Past success doesn't guarantee future success"
              "Incentives must align with strategic needs"]}
   
   {:id 8
    :name "Nokia Smartphone Failure"
    :category "Disruption Failure"
    :models-detected ["Denial" "Overconfidence" "Commitment & Consistency"
                      "Not-Invented-Here Bias" "Bureaucracy"]
    :outcome :catastrophic-failure
    :outcome-description "From 50% market share to sold to Microsoft for scraps"
    :warning-signs ["Dismissed iPhone as niche" "Internal politics" "Slow decision making"
                    "Platform fragmentation"]
    :lessons ["Market leaders can fall fast" "Software eating hardware"
              "Speed of adaptation matters"]}
   
   ;; BUBBLE PATTERNS
   {:id 9
    :name "Dot-com Bubble (1999-2000)"
    :category "Market Bubble"
    :models-detected ["Social Proof" "FOMO" "Overconfidence" "Availability Bias"
                      "Greater Fool Theory" "Narrative Fallacy"]
    :outcome :major-correction
    :outcome-description "NASDAQ fell 78%, $5T in market value destroyed"
    :warning-signs ["Eyeballs over earnings" "IPOs with no revenue" "Day trading mania"
                    "New paradigm talk" "Price-to-sales ratios ignored"]
    :lessons ["Fundamentals eventually matter" "When taxi drivers give stock tips, sell"
              "New technology doesn't mean new economics"]}
   
   {:id 10
    :name "Crypto Bubble (2021-2022)"
    :category "Market Bubble"
    :models-detected ["Social Proof" "FOMO" "Overconfidence" "Greed"
                      "Greater Fool Theory" "Complexity as Feature"]
    :outcome :major-correction
    :outcome-description "Bitcoin fell 75%, $2T+ in crypto value destroyed, major frauds exposed"
    :warning-signs ["Celebrity endorsements" "Guaranteed returns promises" "Complexity celebrated"
                    "Leverage everywhere" "HODL culture preventing selling"]
    :lessons ["If you don't understand it, don't invest" "Leverage kills"
              "Complexity often hides fraud"]}
   
   ;; SUCCESSFUL CONTRARIAN BETS
   {:id 11
    :name "Michael Burry - Big Short (2007)"
    :category "Contrarian Investment"
    :models-detected ["Independent Thinking" "First Principles" "Margin of Safety"
                      "Patience" "Conviction" "Anti-Social Proof"]
    :outcome :exceptional-success
    :outcome-description "489% return betting against housing market"
    :success-factors ["Deep research" "Willingness to be wrong for years" "Understanding incentives"
                      "Reading primary sources" "Ignoring consensus"]
    :lessons ["Consensus can be spectacularly wrong" "Deep research pays off"
              "Patience is required for contrarian bets"]}
   
   {:id 12
    :name "Warren Buffett - American Express Salad Oil (1964)"
    :category "Contrarian Investment"
    :models-detected ["Circle of Competence" "Margin of Safety" "Brand Value"
                      "Temporary vs Permanent Problems" "Independent Thinking"]
    :outcome :exceptional-success
    :outcome-description "Stock fell 50%, Buffett bought heavily, made 3x in 2 years"
    :success-factors ["Understood brand value" "Recognized temporary problem" "Ignored panic"
                      "Did primary research (visited restaurants)"]
    :lessons ["Temporary problems create opportunities" "Brand value persists through crises"
              "Do your own research"]}])

;; ============================================================================
;; Outcome Prediction Patterns
;; ============================================================================

(def outcome-patterns
  "Patterns of model combinations and their typical outcomes"
  [
   ;; DANGER PATTERNS - High probability of failure
   {:pattern-name "Fraud Triangle"
    :required-models ["Incentive-Caused Bias" "Rationalization" "Opportunity"]
    :optional-models ["Authority Bias" "Social Proof" "Denial"]
    :predicted-outcome :high-risk-fraud
    :confidence 0.85
    :description "Classic fraud conditions - pressure + opportunity + rationalization"
    :action "Avoid or short"}
   
   {:pattern-name "Bubble Formation"
    :required-models ["Social Proof" "FOMO" "Overconfidence"]
    :optional-models ["Greater Fool Theory" "Narrative Fallacy" "Availability Bias"]
    :predicted-outcome :bubble-likely
    :confidence 0.80
    :description "Classic bubble psychology - everyone buying because everyone else is"
    :action "Reduce exposure, prepare to short"}
   
   {:pattern-name "Disruption Blindness"
    :required-models ["Denial" "Commitment & Consistency" "Status Quo Bias"]
    :optional-models ["Incentive-Caused Bias" "Sunk Cost Fallacy"]
    :predicted-outcome :disruption-victim
    :confidence 0.75
    :description "Company unable to adapt to changing market"
    :action "Avoid long-term investment"}
   
   {:pattern-name "Lollapalooza Danger"
    :required-models ["Social Proof" "Incentive-Caused Bias" "Authority Bias" "Denial"]
    :optional-models ["Commitment & Consistency" "Overconfidence"]
    :predicted-outcome :catastrophic-failure
    :confidence 0.90
    :description "Multiple biases reinforcing each other - extreme danger"
    :action "Maximum caution, consider shorting"}
   
   ;; SUCCESS PATTERNS - High probability of good outcomes
   {:pattern-name "Durable Moat"
    :required-models ["Network Effects" "Switching Costs" "Brand Value"]
    :optional-models ["Scale Economics" "Regulatory Capture" "Patents"]
    :predicted-outcome :sustainable-advantage
    :confidence 0.80
    :description "Multiple reinforcing competitive advantages"
    :action "Long-term hold candidate"}
   
   {:pattern-name "Value Investment Setup"
    :required-models ["Margin of Safety" "Circle of Competence" "Owner Earnings"]
    :optional-models ["Moat" "Management Quality" "Capital Allocation"]
    :predicted-outcome :above-average-returns
    :confidence 0.70
    :description "Classic Buffett-style value investment"
    :action "Consider buying"}
   
   {:pattern-name "Contrarian Opportunity"
    :required-models ["Independent Thinking" "Temporary vs Permanent" "Margin of Safety"]
    :optional-models ["Patience" "Conviction" "First Principles"]
    :predicted-outcome :potential-outperformance
    :confidence 0.65
    :description "Going against crowd with good reasoning"
    :action "Research deeply, consider position"}
   
   {:pattern-name "Compounding Machine"
    :required-models ["Reinvestment" "High ROIC" "Long Runway"]
    :optional-models ["Network Effects" "Pricing Power" "Low Capital Intensity"]
    :predicted-outcome :exceptional-long-term
    :confidence 0.75
    :description "Business that can reinvest at high rates for decades"
    :action "Buy and hold forever"}])

;; ============================================================================
;; Pattern Matching Engine
;; ============================================================================

(defn normalize-model-name [name]
  "Normalize model name for matching"
  (-> (str name)
      str/lower-case
      (str/replace #"[^a-z0-9]+" " ")
      str/trim))

(defn models-match? [detected-model pattern-model]
  "Check if detected model matches pattern model (fuzzy)"
  (let [d (normalize-model-name detected-model)
        p (normalize-model-name pattern-model)]
    (or (= d p)
        (str/includes? d p)
        (str/includes? p d)
        ;; Check for common variations
        (and (str/includes? d "incentive") (str/includes? p "incentive"))
        (and (str/includes? d "social") (str/includes? p "social"))
        (and (str/includes? d "denial") (str/includes? p "denial"))
        (and (str/includes? d "bias") (str/includes? p "bias")))))

(defn calculate-pattern-match [detected-models pattern]
  "Calculate how well detected models match a pattern"
  (let [required (:required-models pattern)
        optional (:optional-models pattern [])
        
        ;; Count required matches
        required-matches (count (filter (fn [req]
                                         (some #(models-match? % req) detected-models))
                                       required))
        required-total (count required)
        required-pct (if (zero? required-total) 0 (/ required-matches required-total))
        
        ;; Count optional matches
        optional-matches (count (filter (fn [opt]
                                         (some #(models-match? % opt) detected-models))
                                       optional))
        optional-total (count optional)
        optional-pct (if (zero? optional-total) 0 (/ optional-matches optional-total))
        
        ;; Overall score: 70% required, 30% optional
        overall-score (+ (* 0.7 required-pct) (* 0.3 optional-pct))]
    
    {:pattern-name (:pattern-name pattern)
     :required-matches required-matches
     :required-total required-total
     :optional-matches optional-matches
     :optional-total optional-total
     :match-score overall-score
     :predicted-outcome (:predicted-outcome pattern)
     :confidence (* overall-score (:confidence pattern))
     :description (:description pattern)
     :action (:action pattern)}))

(defn find-matching-patterns [detected-models]
  "Find all patterns that match detected models"
  (->> outcome-patterns
       (map #(calculate-pattern-match detected-models %))
       (filter #(> (:match-score %) 0.3))
       (sort-by :confidence >)))

;; ============================================================================
;; Case Study Matching
;; ============================================================================

(defn calculate-case-similarity [detected-models case-study]
  "Calculate similarity between detected models and historical case"
  (let [case-models (:models-detected case-study)
        
        ;; Jaccard similarity
        detected-set (set (map normalize-model-name detected-models))
        case-set (set (map normalize-model-name case-models))
        intersection (count (set/intersection detected-set case-set))
        union (count (set/union detected-set case-set))
        jaccard (if (zero? union) 0 (/ intersection union))
        
        ;; Also check for fuzzy matches
        fuzzy-matches (count (filter (fn [d]
                                      (some #(models-match? d %) case-models))
                                    detected-models))
        fuzzy-pct (/ fuzzy-matches (max 1 (count detected-models)))]
    
    {:case-id (:id case-study)
     :case-name (:name case-study)
     :category (:category case-study)
     :outcome (:outcome case-study)
     :outcome-description (:outcome-description case-study)
     :similarity-score (/ (+ jaccard fuzzy-pct) 2)
     :matching-models intersection
     :case-models case-models
     :lessons (:lessons case-study)
     :warning-signs (:warning-signs case-study)}))

(defn find-similar-cases [detected-models & {:keys [limit] :or {limit 5}}]
  "Find historical cases most similar to current situation"
  (->> case-studies
       (map #(calculate-case-similarity detected-models %))
       (filter #(> (:similarity-score %) 0.2))
       (sort-by :similarity-score >)
       (take limit)))

;; ============================================================================
;; Main Prediction Function
;; ============================================================================

(defn predict-outcome [detected-models text-content]
  "Main prediction function - analyzes detected models and predicts outcomes"
  (let [patterns (find-matching-patterns detected-models)
        similar-cases (find-similar-cases detected-models)
        
        ;; Aggregate predictions
        danger-patterns (filter #(#{:high-risk-fraud :bubble-likely :disruption-victim :catastrophic-failure}
                                  (:predicted-outcome %))
                               patterns)
        success-patterns (filter #(#{:sustainable-advantage :above-average-returns :potential-outperformance :exceptional-long-term}
                                   (:predicted-outcome %))
                                patterns)
        
        ;; Calculate overall risk score
        danger-score (if (seq danger-patterns)
                      (apply max (map :confidence danger-patterns))
                      0)
        success-score (if (seq success-patterns)
                       (apply max (map :confidence success-patterns))
                       0)
        
        ;; Historical outcome distribution
        case-outcomes (frequencies (map :outcome similar-cases))
        failure-cases (+ (get case-outcomes :catastrophic-failure 0)
                        (get case-outcomes :major-correction 0))
        success-cases (get case-outcomes :exceptional-success 0)
        
        ;; Overall assessment
        overall-risk (cond
                      (> danger-score 0.7) :high-risk
                      (> danger-score 0.5) :elevated-risk
                      (> success-score 0.6) :favorable
                      :else :neutral)]
    
    {:detected-models detected-models
     :model-count (count detected-models)
     
     ;; Pattern analysis
     :matching-patterns patterns
     :danger-patterns danger-patterns
     :success-patterns success-patterns
     
     ;; Historical comparison
     :similar-cases similar-cases
     :historical-outcomes case-outcomes
     
     ;; Scores
     :danger-score danger-score
     :success-score success-score
     :overall-risk overall-risk
     
     ;; Recommendations
     :primary-recommendation (cond
                              (> danger-score 0.7) "HIGH CAUTION: Multiple danger patterns detected"
                              (> danger-score 0.5) "CAUTION: Elevated risk indicators present"
                              (> success-score 0.6) "FAVORABLE: Success patterns detected"
                              :else "NEUTRAL: Insufficient pattern matches")
     
     :key-lessons (distinct (mapcat :lessons (take 3 similar-cases)))
     :warning-signs (distinct (mapcat :warning-signs (take 3 similar-cases)))}))

;; ============================================================================
;; News Story Analysis
;; ============================================================================

(defn analyze-news-story [headline content detected-models]
  "Analyze a news story for mental model patterns and predictions"
  (let [prediction (predict-outcome detected-models content)
        
        ;; Calculate overlap percentage with known patterns
        total-patterns (count outcome-patterns)
        matching-count (count (:matching-patterns prediction))
        pattern-overlap (/ (* 100 matching-count) total-patterns)
        
        ;; Calculate overlap with historical cases
        total-cases (count case-studies)
        similar-count (count (:similar-cases prediction))
        case-overlap (/ (* 100 similar-count) total-cases)]
    
    (merge prediction
           {:headline headline
            :pattern-overlap-pct pattern-overlap
            :case-overlap-pct case-overlap
            :analysis-timestamp (System/currentTimeMillis)})))

;; ============================================================================
;; Learning / Training Functions
;; ============================================================================

(def learned-patterns (atom []))

(defn learn-from-outcome! [detected-models actual-outcome description]
  "Learn from a new case with known outcome"
  (swap! learned-patterns conj
         {:models detected-models
          :outcome actual-outcome
          :description description
          :learned-at (System/currentTimeMillis)})
  (count @learned-patterns))

(defn get-learned-patterns []
  "Get all learned patterns"
  @learned-patterns)

(defn export-learning-data []
  "Export learning data for persistence"
  {:case-studies case-studies
   :outcome-patterns outcome-patterns
   :learned-patterns @learned-patterns})
