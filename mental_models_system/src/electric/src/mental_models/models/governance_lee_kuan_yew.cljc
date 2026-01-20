(ns mental-models.models.governance-lee-kuan-yew
  "Mental Models - Governance (Lee Kuan Yew) Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

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