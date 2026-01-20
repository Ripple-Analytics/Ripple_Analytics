(ns mental-models.models.decision-making
  "Mental Models - Decision Making Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Decision Making
;; ============================================

(register-model
 {:name "circle-of-competence"
  :category "decision_making"
  :originator "Warren Buffett"
  :description "Know the boundaries of your knowledge and stay within them"
  :key-insight "The size of your circle matters less than knowing its boundaries"
  :application "Before any decision, ask: Is this within my circle?"
  :failure-modes
  [(failure "overconfidence" "high"
            "Believing your circle is larger than it is"
            :signals ["No recent failures" "Ignoring expert advice" "Dismissing contrary evidence"]
            :safeguards ["Regular competence audits" "Seek disconfirming evidence" "Track prediction accuracy"])
   (failure "underconfidence" "medium"
            "Not acting within your actual competence"
            :signals ["Excessive hesitation" "Deferring on known topics" "Analysis paralysis"]
            :safeguards ["Track past successes" "Build confidence gradually" "Start with small decisions"])
   (failure "static-circle" "medium"
            "Not expanding your circle over time"
            :signals ["No new learning" "Same decisions for years" "Avoiding challenges"]
            :safeguards ["Deliberate practice" "Adjacent learning" "Stretch assignments"])
   (failure "boundary-blindness" "high"
            "Not knowing where your circle ends"
            :signals ["Overconfident predictions" "No uncertainty acknowledgment"]
            :safeguards ["Explicit uncertainty ranges" "Pre-mortems" "Outside feedback"])
   (failure "competence-creep" "high"
            "Gradually drifting outside your circle"
            :signals ["Slow expansion without validation" "Success breeding overreach"]
            :safeguards ["Regular boundary checks" "Milestone reviews" "External validation"])]})

(register-model
 {:name "margin-of-safety"
  :category "decision_making"
  :originator "Benjamin Graham"
  :description "Always leave room for error in your calculations"
  :key-insight "The future is uncertain; build in buffers"
  :application "Add 25-50% buffer to all estimates"
  :failure-modes
  [(failure "insufficient-margin" "critical"
            "Not leaving enough buffer for unexpected events"
            :signals ["Optimistic projections" "Ignoring tail risks" "Best-case planning"]
            :safeguards ["Double estimated margin" "Stress test assumptions" "Plan for worst case"])
   (failure "excessive-margin" "low"
            "Being so conservative you miss opportunities"
            :signals ["Never taking action" "Paralysis by analysis" "Missing obvious wins"]
            :safeguards ["Balance risk/reward" "Time-bound decisions" "Opportunity cost awareness"])
   (failure "false-precision" "medium"
            "Believing precise calculations eliminate uncertainty"
            :signals ["Many decimal places" "Complex models" "Overconfidence in numbers"]
            :safeguards ["Round estimates" "Sensitivity analysis" "Scenario planning"])
   (failure "margin-erosion" "high"
            "Gradually reducing margins under pressure"
            :signals ["Competitive pressure" "Short-term thinking" "Margin compression"]
            :safeguards ["Hard minimum thresholds" "Regular margin reviews" "Long-term focus"])
   (failure "wrong-margin" "high"
            "Building margin against the wrong risks"
            :signals ["Protecting against unlikely events" "Ignoring likely risks"]
            :safeguards ["Risk prioritization" "Probability assessment" "Historical analysis"])]})

(register-model
 {:name "second-order-thinking"
  :category "decision_making"
  :originator "Howard Marks"
  :description "Think about the consequences of the consequences"
  :key-insight "First-level thinking is simplistic; second-level thinking is deep"
  :application "Ask 'And then what?' at least three times"
  :failure-modes
  [(failure "first-order-only" "high"
            "Only considering immediate effects"
            :signals ["Quick decisions" "No scenario planning" "Surprise by consequences"]
            :safeguards ["Mandatory 'then what' exercise" "Consider 3 time horizons" "Scenario planning"])
   (failure "infinite-regress" "medium"
            "Getting lost in endless chains of consequences"
            :signals ["Analysis paralysis" "Never deciding" "Overthinking"]
            :safeguards ["Set analysis limits" "Time-box thinking" "Focus on material effects"])
   (failure "wrong-chain" "high"
            "Following the wrong causal chain"
            :signals ["Ignoring key variables" "Linear thinking in complex systems"]
            :safeguards ["Multiple scenario paths" "Expert consultation" "Historical analogies"])
   (failure "probability-neglect" "high"
            "Not weighting consequences by likelihood"
            :signals ["Equal weight to all outcomes" "Ignoring base rates"]
            :safeguards ["Probability estimates" "Expected value calculations" "Base rate research"])
   (failure "time-horizon-mismatch" "medium"
            "Optimizing for wrong time frame"
            :signals ["Short-term focus" "Ignoring long-term effects"]
            :safeguards ["Explicit time horizons" "Multi-period analysis" "Stakeholder mapping"])]})

(register-model
 {:name "inversion"
  :category "decision_making"
  :originator "Carl Jacobi / Charlie Munger"
  :description "Invert, always invert - think about what to avoid"
  :key-insight "It's often easier to avoid stupidity than to seek brilliance"
  :application "Ask 'How could this fail?' before 'How could this succeed?'"
  :failure-modes
  [(failure "forward-only" "high"
            "Only thinking about how to succeed"
            :signals ["No failure analysis" "Optimism bias" "Ignoring risks"]
            :safeguards ["Pre-mortems" "Failure mode analysis" "Devil's advocate"])
   (failure "incomplete-inversion" "medium"
            "Not inverting thoroughly enough"
            :signals ["Surface-level inversion" "Missing key failure modes"]
            :safeguards ["Systematic failure enumeration" "Multiple perspectives" "Historical failures"])
   (failure "paralysis-by-inversion" "medium"
            "Finding so many failure modes you can't act"
            :signals ["Endless risk lists" "No action" "Fear-based decisions"]
            :safeguards ["Prioritize risks" "Accept residual risk" "Time-bound analysis"])
   (failure "inversion-bias" "low"
            "Becoming too focused on avoiding failure"
            :signals ["Missing opportunities" "Excessive caution" "Defensive posture"]
            :safeguards ["Balance with opportunity seeking" "Upside analysis" "Growth mindset"])
   (failure "wrong-inversion" "high"
            "Inverting the wrong question"
            :signals ["Solving wrong problem" "Misframed question"]
            :safeguards ["Question the question" "Multiple framings" "Stakeholder input"])]})

(register-model
 {:name "opportunity-cost"
  :category "decision_making"
  :originator "Economics"
  :description "The cost of any choice is what you give up"
  :key-insight "Every yes is a no to something else"
  :application "Always consider the next best alternative"
  :failure-modes
  [(failure "ignoring-alternatives" "high"
            "Not considering what else you could do"
            :signals ["Single option focus" "No comparison" "First idea accepted"]
            :safeguards ["Generate 3+ alternatives" "Explicit comparison" "Opportunity cost calculation"])
   (failure "sunk-cost-confusion" "high"
            "Confusing sunk costs with opportunity costs"
            :signals ["Considering past investments" "Throwing good money after bad"]
            :safeguards ["Ignore sunk costs" "Fresh start thinking" "Zero-based decisions"])
   (failure "narrow-framing" "medium"
            "Considering too few alternatives"
            :signals ["Binary choices" "Limited options" "Tunnel vision"]
            :safeguards ["Brainstorm alternatives" "Outside perspectives" "Creative options"])
   (failure "time-blindness" "medium"
            "Not considering time as a resource"
            :signals ["Ignoring time costs" "No time valuation"]
            :safeguards ["Value your time" "Time opportunity costs" "Delegation analysis"])
   (failure "comparison-paralysis" "low"
            "Unable to choose between alternatives"
            :signals ["Endless comparison" "No decision" "Perfect option seeking"]
            :safeguards ["Good enough threshold" "Time limits" "Reversibility check"])]})

;; ============================================