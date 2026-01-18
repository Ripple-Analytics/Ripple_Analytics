(ns mental-models.checklists.investment-analysis
  "Investment analysis checklists derived from Organised Uncommon Sense.
   Implements the two-cut system and comprehensive evaluation framework."
  (:require [hyperfiddle.electric :as e]
            [clojure.string :as str]))

;; =============================================================================
;; FIRST CUT - INITIAL FILTER
;; =============================================================================

(def first-cut-filters
  "Initial filters to eliminate unsuitable investments"
  {:too-difficult
   {:name "Too Difficult/Complicated"
    :description "Put in the 'too hard' pile"
    :red-flags ["Complex business model"
                "Pharmaceutical companies (drug approval uncertainty)"
                "Cutting-edge technology (unpredictable)"
                "Financial engineering"
                "Opaque accounting"
                "Multiple business segments hard to value"
                "Regulatory uncertainty"
                "Geopolitical exposure"]}
   
   :heavily-promoted
   {:name "Heavily Promoted"
    :description "Avoid the hype"
    :red-flags ["IPOs (Initial Public Offerings)"
                "SPACs"
                "Hot tips"
                "Great deals that seem too good"
                "Celebrity endorsements"
                "Social media pump"
                "Aggressive marketing to retail investors"
                "Complex fee structures"]}})

;; =============================================================================
;; SECOND CUT - DEEP ANALYSIS
;; =============================================================================

(def financial-analysis-checklist
  "Recast financial figures for true picture"
  {:free-cash-flow
   {:name "Free Cash Flow"
    :questions ["What is owner earnings (net income + depreciation - capex)?"
                "Is FCF growing consistently?"
                "Is FCF being used for buybacks, dividends, or reinvestment?"
                "What is FCF yield vs market cap?"]}
   
   :inventory
   {:name "Inventory Analysis"
    :questions ["Is inventory growing faster than sales?"
                "What is inventory turnover trend?"
                "Is there obsolescence risk?"
                "LIFO vs FIFO impact?"]}
   
   :working-capital
   {:name "Working Capital"
    :questions ["What is the cash conversion cycle?"
                "Is working capital being managed efficiently?"
                "Are receivables growing faster than sales?"
                "What are payment terms with suppliers?"]}
   
   :fixed-intangible-assets
   {:name "Fixed and Intangible Assets"
    :questions ["What is the true replacement cost?"
                "Is goodwill justified or inflated?"
                "What is the useful life of assets?"
                "Are there hidden off-balance-sheet assets?"]}
   
   :stock-options
   {:name "Stock Options"
    :questions ["What is the dilution impact?"
                "Are options being expensed properly?"
                "What is the true cost to shareholders?"
                "Is management incentive aligned with owners?"]}
   
   :pension-plans
   {:name "Pension Plans"
    :questions ["What are the assumed return rates?"
                "Is the plan underfunded?"
                "What is the true liability?"
                "What are the cash contribution requirements?"]}
   
   :retiree-medical
   {:name "Retiree Medical Benefits"
    :questions ["What is the unfunded liability?"
                "What assumptions are being used?"
                "How does this compare to peers?"
                "What is the trend in healthcare costs?"]}})

(def management-checklist
  "Assess management quality"
  {:ability
   {:name "Ability"
    :questions ["Track record of capital allocation?"
                "Operating performance vs peers?"
                "Ability to adapt to change?"
                "Technical competence in industry?"]}
   
   :trustworthiness
   {:name "Trustworthiness"
    :questions ["History of honest communication?"
                "Do they admit mistakes?"
                "Insider buying vs selling?"
                "Related party transactions?"]}
   
   :owner-orientation
   {:name "Owner-Oriented"
    :questions ["Do they think like owners?"
                "Compensation aligned with shareholders?"
                "Focus on per-share value vs empire building?"
                "Dividend and buyback discipline?"]}
   
   :capital-deployment
   {:name "Capital Deployment"
    :questions ["How do they deploy excess cash?"
                "Acquisition track record?"
                "Return on invested capital trend?"
                "Debt management philosophy?"]}
   
   :ego-growth
   {:name "Ego-Oriented Growth"
    :red-flags ["Acquisitions for size not value"
                "Excessive executive perks"
                "Vanity projects"
                "Empire building"
                "Excessive compensation"
                "Private jets, art collections"]}})

(def competitive-advantage-checklist
  "Assess durability of competitive advantage"
  {:products
   {:name "Products"
    :questions ["Is it dominated by a few companies?"
                "More concentrated than selling industry?"
                "No substitute products?"
                "Important to buyer's business?"
                "Products differentiated?"
                "Switching costs built up?"
                "Credible threat of forward integration?"]}
   
   :markets
   {:name "Markets"
    :questions ["Inefficient, neglected markets?"
                "Outside firms lack resources/skills?"
                "Competitors myopic or complacent?"
                "Lack of attention by outside firms?"]}
   
   :trademarks
   {:name "Trademarks & Brand"
    :questions ["Brand identification through advertising?"
                "Strong sales force?"
                "Customer loyalty?"
                "Premium pricing power?"]}
   
   :employees
   {:name "Employees"
    :questions ["Exceptional quality?"
                "Strong sales force?"
                "Healthy culture?"
                "Training and upskilling?"
                "Low turnover?"]}
   
   :distribution
   {:name "Distribution Channels"
    :questions ["High switching costs for competitors?"
                "Easy access?"
                "Favorable transportation access?"
                "Exclusive relationships?"
                "Long relationships?"
                "High quality service?"]}
   
   :durability
   {:name "Durability of Advantage"
    :questions ["Favorable locations?"
                "Government subsidies?"
                "Favorable raw material access?"
                "Proprietary technology?"
                "How long can moat last?"
                "What could destroy the moat?"]}})

;; =============================================================================
;; ASININITIES DETECTION (CRITICAL)
;; =============================================================================

(def asininities-checklist
  "The secret to power is to consistently avoid the asininities"
  {:credit-climate-current
   {:name "Current Credit Climate"
    :favorable ["No leverage"
                "No availability of credit"
                "Strict regulations"
                "Low inflation exposure"
                "Low interest rate exposure"
                "Not messing with customers"]
    :unfavorable ["High leverage"
                  "Easy credit availability"
                  "Loose regulations"
                  "High inflation exposure"
                  "High interest rate exposure"]}
   
   :credit-climate-prospective
   {:name "Prospective Credit Climate"
    :questions ["Where are we in the credit cycle?"
                "Is credit tightening or loosening?"
                "What is the Fed doing?"
                "What are spreads indicating?"]}
   
   :regulatory-climate
   {:name "Regulatory Climate"
    :entry-barriers ["Licensing requirements"
                     "Limits on access"
                     "Government subsidies"
                     "Environmental regulations"
                     "Antitrust considerations"]
    :questions ["Is regulation tightening or loosening?"
                "Political risk?"
                "Potential for adverse changes?"]}
   
   :labor-state
   {:name "State of Labour"
    :questions ["Experience curve advantage?"
                "Costs declining with experience?"
                "Proprietary knowledge?"
                "Union exposure?"
                "Labor availability?"]}
   
   :supplier-relations
   {:name "Supplier Relations"
    :buyer-power-high-when ["Buyer group concentrated vs seller sales"
                            "Product is significant fraction of buyer costs"
                            "Products are standard/undifferentiated"
                            "Few switching costs"
                            "Buyer earns low profits"
                            "Credible backward integration threat"
                            "Product unimportant to buyer quality"]}
   
   :customer-relations
   {:name "Customer Relations"
    :differentiation-factors ["Product differentiation"
                              "Customer loyalties"
                              "Past advertising"
                              "Brand identification"
                              "Responsive customer service"
                              "Engineering assistance"
                              "Credit or rapid delivery"
                              "New product features"
                              "Redefining buyer perception"]}
   
   :technology-impact
   {:name "Technology Changes"
    :questions ["Volume of throughput changing?"
                "Efficient scale changing?"
                "Rising economies of scale?"
                "Rising capital necessary?"
                "Substitute products emerging?"]}
   
   :competitive-vulnerabilities
   {:name "Competitive Vulnerabilities"
    :red-flags ["Biases and blind spots"
                "Ignoring market signals"
                "Slow growth"
                "High fixed costs"
                "High industry concentration"
                "High strategic stakes for competitors"
                "Lack of differentiation"
                "Capacity in large increments"
                "Diverse competitors"
                "Chronic overcapacity"]}})

;; =============================================================================
;; TRADING CONSIDERATIONS
;; =============================================================================

(def trading-checklist
  "Final trading considerations"
  {:price
   {:name "Current Price"
    :questions ["What is intrinsic value?"
                "What is margin of safety?"
                "What is the bargain ratio?"]}
   
   :volume
   {:name "Volume"
    :questions ["Is there sufficient liquidity?"
                "Can position be built without moving price?"
                "Can position be exited if needed?"]}
   
   :timing
   {:name "Disclosure Timing"
    :questions ["Any upcoming announcements?"
                "Earnings date?"
                "Regulatory filings due?"
                "Insider trading windows?"]}
   
   :opportunity-cost
   {:name "Opportunity Cost"
    :questions ["Are better uses of capital available?"
                "What is the hurdle rate?"
                "What are alternatives yielding?"]}
   
   :liquidity
   {:name "Liquidity"
    :questions ["Is sufficient liquidity available?"
                "Must capital be borrowed?"
                "What is cost of capital?"]}})

;; =============================================================================
;; SCORING FUNCTIONS
;; =============================================================================

(defn score-first-cut
  "Score an investment against first-cut filters. Returns :pass or :fail with reasons."
  [analysis-data]
  (let [red-flags-found (atom [])]
    ;; Check too-difficult
    (doseq [flag (get-in first-cut-filters [:too-difficult :red-flags])]
      (when (contains? (:characteristics analysis-data) flag)
        (swap! red-flags-found conj {:category :too-difficult :flag flag})))
    
    ;; Check heavily-promoted
    (doseq [flag (get-in first-cut-filters [:heavily-promoted :red-flags])]
      (when (contains? (:characteristics analysis-data) flag)
        (swap! red-flags-found conj {:category :heavily-promoted :flag flag})))
    
    (if (seq @red-flags-found)
      {:result :fail
       :reason "Failed first cut"
       :red-flags @red-flags-found}
      {:result :pass
       :reason "Passed first cut - proceed to deep analysis"})))

(defn score-management
  "Score management quality 0-100"
  [management-data]
  (let [scores {:ability (get management-data :ability-score 50)
                :trustworthiness (get management-data :trust-score 50)
                :owner-orientation (get management-data :owner-score 50)
                :capital-deployment (get management-data :capital-score 50)}
        ego-penalty (if (seq (:ego-red-flags management-data)) -20 0)]
    {:total (+ ego-penalty (/ (reduce + (vals scores)) 4))
     :breakdown scores
     :red-flags (:ego-red-flags management-data)}))

(defn score-competitive-advantage
  "Score competitive advantage durability 0-100"
  [moat-data]
  (let [scores {:products (get moat-data :products-score 50)
                :markets (get moat-data :markets-score 50)
                :trademarks (get moat-data :trademarks-score 50)
                :employees (get moat-data :employees-score 50)
                :distribution (get moat-data :distribution-score 50)
                :durability (get moat-data :durability-score 50)}]
    {:total (/ (reduce + (vals scores)) 6)
     :breakdown scores
     :moat-type (:moat-type moat-data)
     :estimated-duration (:estimated-duration moat-data)}))

(defn detect-asininities
  "Detect potential asininities in an investment"
  [analysis-data]
  (let [warnings (atom [])]
    ;; Credit climate
    (when (> (:leverage-ratio analysis-data 0) 3)
      (swap! warnings conj {:category :credit :severity :high
                            :message "High leverage ratio"}))
    
    ;; Regulatory
    (when (:regulatory-uncertainty analysis-data)
      (swap! warnings conj {:category :regulatory :severity :medium
                            :message "Regulatory uncertainty present"}))
    
    ;; Competitive vulnerabilities
    (doseq [vuln (get-in asininities-checklist [:competitive-vulnerabilities :red-flags])]
      (when (contains? (:vulnerabilities analysis-data) vuln)
        (swap! warnings conj {:category :competitive :severity :medium
                              :message vuln})))
    
    {:asininity-count (count @warnings)
     :warnings @warnings
     :recommendation (cond
                       (>= (count @warnings) 5) :avoid
                       (>= (count @warnings) 3) :caution
                       (>= (count @warnings) 1) :monitor
                       :else :proceed)}))

;; =============================================================================
;; COMPREHENSIVE ANALYSIS
;; =============================================================================

(defn run-full-analysis
  "Run complete investment analysis using all checklists"
  [investment-data]
  (let [first-cut (score-first-cut investment-data)]
    (if (= :fail (:result first-cut))
      {:recommendation :reject
       :reason "Failed first cut"
       :details first-cut}
      
      ;; Proceed to deep analysis
      (let [management (score-management (:management investment-data))
            moat (score-competitive-advantage (:competitive-advantage investment-data))
            asininities (detect-asininities investment-data)
            
            ;; Calculate overall score
            overall-score (/ (+ (:total management)
                                (:total moat)
                                (- 100 (* 10 (:asininity-count asininities))))
                             3)]
        
        {:recommendation (cond
                           (< overall-score 40) :reject
                           (< overall-score 60) :watchlist
                           (< overall-score 80) :consider
                           :else :strong-buy)
         :overall-score overall-score
         :management management
         :competitive-advantage moat
         :asininities asininities
         :margin-of-safety-required (cond
                                      (< overall-score 60) "50%+"
                                      (< overall-score 70) "30-50%"
                                      (< overall-score 80) "20-30%"
                                      :else "15-20%")}))))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

(e/defn InvestmentAnalyzer [investment-data]
  "Reactive investment analysis component"
  (e/server
    (let [analysis (e/offload #(run-full-analysis investment-data))]
      analysis)))

(e/defn ChecklistView [checklist-type]
  "Display a checklist for manual review"
  (e/client
    (let [checklist (case checklist-type
                      :financial financial-analysis-checklist
                      :management management-checklist
                      :competitive competitive-advantage-checklist
                      :asininities asininities-checklist
                      :trading trading-checklist
                      {})]
      (dom/div
        (dom/props {:class "checklist"})
        (e/for [[k v] checklist]
          (dom/div
            (dom/props {:class "checklist-section"})
            (dom/h3 (dom/text (:name v)))
            (when (:questions v)
              (dom/ul
                (e/for [q (:questions v)]
                  (dom/li
                    (dom/input (dom/props {:type "checkbox"}))
                    (dom/text q)))))
            (when (:red-flags v)
              (dom/div
                (dom/props {:class "red-flags"})
                (dom/h4 (dom/text "Red Flags:"))
                (dom/ul
                  (e/for [rf (:red-flags v)]
                    (dom/li (dom/text rf))))))))))))
