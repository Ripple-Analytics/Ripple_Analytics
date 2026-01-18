(ns mental-models.new-models
  "New Mental Models - Renaissance Technologies, Ray Dalio, Lee Kuan Yew
   
   10 additional high-value mental models for the system.
   To be integrated into models.cljc"
  #?(:clj (:require [mental-models.models :refer [register-model failure]])
     :cljs (:require [mental-models.models :refer [register-model failure]])))

;; ============================================
;; Category: Quantitative Finance (Renaissance Technologies)
;; ============================================

(register-model
 {:name "regime-detection"
  :category "quantitative_finance"
  :originator "Jim Simons / Renaissance Technologies"
  :description "Identify distinct market states with different statistical properties"
  :key-insight "Markets operate in different regimes; strategies that work in one fail in another"
  :application "Detect regime shifts before adjusting strategy"
  :failure-modes
  [(failure "regime-lag" "critical"
            "Detecting regime change too late"
            :signals ["Losses after shift" "Stale indicators"]
            :safeguards ["Leading indicators" "Multiple detection methods" "Fast adaptation"])
   (failure "false-regime-signal" "high"
            "Seeing regime change when none exists"
            :signals ["Excessive switching" "Strategy whipsaw"]
            :safeguards ["Confirmation thresholds" "Multiple timeframes" "Statistical significance"])
   (failure "regime-overfitting" "high"
            "Too many regimes, too specific"
            :signals ["Overfit to history" "Poor out-of-sample"]
            :safeguards ["Parsimony" "Out-of-sample testing" "Regime stability"])
   (failure "missing-regime" "critical"
            "New regime not in model"
            :signals ["Unprecedented losses" "Model breakdown"]
            :safeguards ["Adaptive learning" "Anomaly detection" "Human oversight"])
   (failure "regime-blindness" "high"
            "Assuming single regime forever"
            :signals ["Consistent strategy failure" "Ignoring context"]
            :safeguards ["Regular regime analysis" "Context awareness" "Adaptive systems"])]})

(register-model
 {:name "factor-decomposition"
  :category "quantitative_finance"
  :originator "Renaissance Technologies"
  :description "Break down returns into underlying factors to understand true drivers"
  :key-insight "Returns are combinations of factors; isolate them to find alpha"
  :application "Decompose performance into market, sector, style, and alpha factors"
  :failure-modes
  [(failure "factor-confusion" "high"
            "Mistaking beta for alpha"
            :signals ["Returns disappear in different market" "Correlation with index"]
            :safeguards ["Factor analysis" "Multiple market conditions" "Risk decomposition"])
   (failure "missing-factors" "high"
            "Incomplete factor model"
            :signals ["Unexplained variance" "Residual patterns"]
            :safeguards ["Comprehensive factor set" "Residual analysis" "Factor discovery"])
   (failure "factor-multicollinearity" "medium"
            "Factors too correlated"
            :signals ["Unstable coefficients" "Interpretation difficulty"]
            :safeguards ["Factor orthogonalization" "Principal components" "Factor selection"])
   (failure "time-varying-factors" "high"
            "Factor loadings change over time"
            :signals ["Model drift" "Degrading performance"]
            :safeguards ["Rolling analysis" "Adaptive factors" "Regime-dependent factors"])
   (failure "factor-overfitting" "critical"
            "Too many factors, data mining"
            :signals ["Perfect in-sample, poor out-of-sample"]
            :safeguards ["Parsimony" "Economic rationale" "Out-of-sample validation"])]})

(register-model
 {:name "mean-reversion"
  :category "quantitative_finance"
  :originator "Renaissance Technologies"
  :description "Prices that deviate from mean tend to return to it"
  :key-insight "Temporary dislocations create opportunities; extremes don't last"
  :application "Buy undervalued, sell overvalued, wait for reversion"
  :failure-modes
  [(failure "structural-shift" "critical"
            "Mean has permanently changed"
            :signals ["No reversion" "New equilibrium"]
            :safeguards ["Regime detection" "Fundamental analysis" "Adaptive means"])
   (failure "slow-reversion" "high"
            "Reversion takes longer than capital lasts"
            :signals ["Correct direction, wrong timing" "Margin calls"]
            :safeguards ["Position sizing" "Time horizons" "Funding stability"])
   (failure "trend-vs-reversion" "high"
            "Mistaking trend for mean reversion opportunity"
            :signals ["Catching falling knife" "Fighting trend"]
            :safeguards ["Trend detection" "Momentum filters" "Multi-timeframe analysis"])
   (failure "mean-calculation-error" "high"
            "Wrong mean estimate"
            :signals ["Consistent losses" "Reversion to wrong level"]
            :safeguards ["Multiple mean estimates" "Fundamental anchors" "Adaptive means"])
   (failure "leverage-on-reversion" "critical"
            "Overleveraging mean reversion trades"
            :signals ["Blowup risk" "Margin pressure"]
            :safeguards ["Conservative leverage" "Risk limits" "Diversification"])]})

;; ============================================
;; Category: Organizational Design (Ray Dalio)
;; ============================================

(register-model
 {:name "radical-transparency"
  :category "organizational_design"
  :originator "Ray Dalio"
  :description "Make almost everything open and accessible to everyone"
  :key-insight "Transparency creates trust and enables better decisions"
  :application "Record meetings, share feedback openly, make information accessible"
  :failure-modes
  [(failure "privacy-violation" "high"
            "Transparency without boundaries"
            :signals ["Personal issues public" "Legal problems"]
            :safeguards ["Clear boundaries" "Privacy protection" "Consent"])
   (failure "transparency-paralysis" "medium"
            "Fear of being recorded prevents action"
            :signals ["Reduced candor" "Performative behavior"]
            :safeguards ["Psychological safety" "Clear purpose" "Trust building"])
   (failure "information-overload" "medium"
            "Too much transparency, can't process"
            :signals ["Overwhelmed" "Can't find signal"]
            :safeguards ["Curation" "Relevance filtering" "Summaries"])
   (failure "weaponized-transparency" "high"
            "Using transparency to attack others"
            :signals ["Gotcha culture" "Fear-based environment"]
            :safeguards ["Constructive intent" "Learning focus" "No blame culture"])
   (failure "selective-transparency" "high"
            "Transparency only when convenient"
            :signals ["Trust erosion" "Cynicism"]
            :safeguards ["Consistent application" "Leadership modeling" "Accountability"])]})

(register-model
 {:name "idea-meritocracy"
  :category "organizational_design"
  :originator "Ray Dalio"
  :description "Best ideas win regardless of source"
  :key-insight "Truth and excellence come from rigorous evaluation, not hierarchy"
  :application "Weight ideas by quality and track record, not title"
  :failure-modes
  [(failure "false-meritocracy" "critical"
            "Claiming meritocracy while maintaining hierarchy"
            :signals ["Same people always win" "Lip service to ideas"]
            :safeguards ["Track decision outcomes" "Transparent weighting" "Actual power shifts"])
   (failure "credibility-calculation-error" "high"
            "Wrong weights for believability"
            :signals ["Poor decisions" "Ignored expertise"]
            :safeguards ["Track record data" "Domain specificity" "Regular recalibration"])
   (failure "analysis-paralysis" "medium"
            "Too much debate, no decisions"
            :signals ["Endless discussion" "Missed opportunities"]
            :safeguards ["Decision deadlines" "Escalation paths" "Minimum viable consensus"])
   (failure "expertise-tyranny" "medium"
            "Only experts can contribute"
            :signals ["Outsider ideas ignored" "Innovation stagnation"]
            :safeguards ["Beginner's mind" "Cross-pollination" "Diverse perspectives"])
   (failure "meritocracy-without-safety" "high"
            "Brutal honesty without psychological safety"
            :signals ["Fear" "Reduced participation"]
            :safeguards ["Respect" "Learning intent" "Supportive challenge"])]})

(register-model
 {:name "believability-weighted-decision-making"
  :category "decision_theory"
  :originator "Ray Dalio"
  :description "Weight opinions by track record and expertise"
  :key-insight "Not all opinions are equal; weight by demonstrated competence"
  :application "Calculate believability scores, weight votes accordingly"
  :failure-modes
  [(failure "track-record-overfitting" "high"
            "Past success doesn't guarantee future success"
            :signals ["Experts failing" "Context changed"]
            :safeguards ["Context awareness" "Decay old data" "Regime detection"])
   (failure "narrow-expertise" "medium"
            "Expert in one domain, weighted in all"
            :signals ["Poor cross-domain decisions" "Halo effect"]
            :safeguards ["Domain-specific weights" "Expertise boundaries" "Humility"])
   (failure "newcomer-discount" "medium"
            "New people have no weight"
            :signals ["Fresh perspectives ignored" "Innovation loss"]
            :safeguards ["Starter credibility" "Idea evaluation independent of source" "Fast credibility building"])
   (failure "weight-gaming" "high"
            "People optimize for weight, not truth"
            :signals ["Political behavior" "Safe predictions"]
            :safeguards ["Outcome tracking" "Calibration scoring" "Intellectual honesty rewards"])
   (failure "groupthink-by-weight" "high"
            "High-weight people converge, suppress dissent"
            :signals ["Uniform views" "Surprises"]
            :safeguards ["Devil's advocate" "Diversity" "Contrarian bonus"])]})

;; ============================================
;; Category: Governance (Lee Kuan Yew)
;; ============================================

(register-model
 {:name "pragmatic-authoritarianism"
  :category "governance"
  :originator "Lee Kuan Yew"
  :description "Results matter more than ideology; do what works"
  :key-insight "Ideology is a luxury; pragmatism delivers outcomes"
  :application "Test policies empirically, keep what works, discard what doesn't"
  :failure-modes
  [(failure "authoritarianism-creep" "critical"
            "Pragmatism becomes excuse for oppression"
            :signals ["Rights erosion" "No accountability"]
            :safeguards ["Clear limits" "Rule of law" "Checks and balances"])
   (failure "short-term-pragmatism" "high"
            "Optimizing for immediate results, ignoring long-term"
            :signals ["Quick wins, future problems" "Sustainability issues"]
            :safeguards ["Long-term thinking" "Intergenerational planning" "Sustainability metrics"])
   (failure "pragmatism-without-values" "high"
            "Anything goes if it works"
            :signals ["Ethical violations" "Trust erosion"]
            :safeguards ["Core values" "Ethical boundaries" "Moral compass"])
   (failure "measurement-myopia" "medium"
            "Only what's measured matters"
            :signals ["Goodhart's law" "Unintended consequences"]
            :safeguards ["Holistic evaluation" "Qualitative assessment" "System thinking"])
   (failure "context-blindness" "high"
            "What worked in Singapore won't work elsewhere"
            :signals ["Failed transplants" "Cultural mismatch"]
            :safeguards ["Context adaptation" "Local knowledge" "Cultural sensitivity"])]})

(register-model
 {:name "long-term-thinking"
  :category "strategy"
  :originator "Lee Kuan Yew"
  :description "Plan for decades and generations, not quarters"
  :key-insight "Compounding requires time; short-term thinking destroys value"
  :application "Make decisions that benefit your grandchildren"
  :failure-modes
  [(failure "long-term-paralysis" "medium"
            "So focused on long-term, miss immediate needs"
            :signals ["Current crisis ignored" "Present suffering"]
            :safeguards ["Balance short and long" "Survival first" "Staged planning"])
   (failure "prediction-hubris" "high"
            "Overconfidence in long-term predictions"
            :signals ["Rigid plans" "Surprise shocks"]
            :safeguards ["Scenario planning" "Optionality" "Adaptive plans"])
   (failure "discount-rate-error" "high"
            "Wrong time preference"
            :signals ["Overinvestment in distant future" "Underinvestment in near-term"]
            :safeguards ["Appropriate discounting" "Balanced portfolio" "Staged investments"])
   (failure "intergenerational-conflict" "medium"
            "Current generation sacrifices too much"
            :signals ["Resentment" "Political backlash"]
            :safeguards ["Shared benefits" "Visible progress" "Fairness"])
   (failure "long-term-excuse" "high"
            "Using long-term as excuse for poor short-term results"
            :signals ["Perpetual promises" "No accountability"]
            :safeguards ["Milestones" "Intermediate metrics" "Transparent tracking"])]})

(register-model
 {:name "meritocratic-governance"
  :category "governance"
  :originator "Lee Kuan Yew"
  :description "Best people in key positions, regardless of background"
  :key-insight "Talent is scarce; find it, develop it, deploy it"
  :application "Rigorous selection, competitive compensation, performance accountability"
  :failure-modes
  [(failure "meritocracy-myth" "critical"
            "Claiming meritocracy while maintaining privilege"
            :signals ["Same backgrounds" "Nepotism"]
            :safeguards ["Blind selection" "Diverse pipelines" "Outcome tracking"])
   (failure "narrow-merit-definition" "high"
            "Merit defined by one dimension (e.g., test scores)"
            :signals ["Groupthink" "Missing capabilities"]
            :safeguards ["Multidimensional evaluation" "Diverse excellence" "Holistic assessment"])
   (failure "meritocracy-without-opportunity" "critical"
            "Only those with advantages can compete"
            :signals ["Inequality" "Talent waste"]
            :safeguards ["Equal opportunity" "Talent development" "Remove barriers"])
   (failure "elite-capture" "high"
            "Meritocratic elite becomes self-serving"
            :signals ["Corruption" "Rent-seeking"]
            :safeguards ["Accountability" "Rotation" "Public service ethos"])
   (failure "burnout-culture" "high"
            "Meritocracy becomes brutal competition"
            :signals ["Exhaustion" "Mental health crisis"]
            :safeguards ["Sustainable excellence" "Well-being" "Long-term performance"])]})

(register-model
 {:name "economic-pragmatism"
  :category "economics"
  :originator "Lee Kuan Yew"
  :description "Whatever economic system works, use it"
  :key-insight "Ideology is less important than results; mix and match"
  :application "Free market where it works, state intervention where needed"
  :failure-modes
  [(failure "ideological-rigidity" "high"
            "Sticking to pure capitalism or socialism"
            :signals ["Poor outcomes" "Ideological purity over results"]
            :safeguards ["Pragmatic experimentation" "Evidence-based policy" "Flexibility"])
   (failure "state-capture" "critical"
            "Government intervention becomes corruption"
            :signals ["Cronyism" "Inefficiency"]
            :safeguards ["Transparency" "Accountability" "Competitive processes"])
   (failure "market-fundamentalism" "high"
            "Markets solve everything"
            :signals ["Market failures" "Externalities ignored"]
            :safeguards ["Recognize market limits" "Strategic intervention" "Public goods provision"])
   (failure "policy-whiplash" "medium"
            "Constant switching between approaches"
            :signals ["Instability" "No compounding"]
            :safeguards ["Policy stability" "Long-term commitment" "Gradual adjustment"])
   (failure "context-blindness" "high"
            "Copying policies without understanding context"
            :signals ["Failed implementations" "Unintended consequences"]
            :safeguards ["Local adaptation" "Pilot programs" "Context analysis"])]})

;; ============================================
;; Model Count: 10 new models added
;; Total system models: 129 (119 existing + 10 new)
;; ============================================
