(ns mental-models.models.complexity-part1
  "Mental Models - Complexity Category (Part 1)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 1)
;; ============================================

(register-model
 {:name "cynefin"
  :category "complexity"
  :originator "Dave Snowden"
  :description "Framework for understanding different types of problems"
  :key-insight "Different problems require different approaches"
  :application "Match your approach to the type of problem"
  :failure-modes
  [(failure "domain-misidentification" "high"
            "Treating complex as complicated or vice versa"
            :signals ["Wrong approach" "Unexpected outcomes"]
            :safeguards ["Careful domain assessment" "Probe first"])
   (failure "simple-solution-bias" "high"
            "Wanting simple solutions for complex problems"
            :signals ["Oversimplification" "Missing dynamics"]
            :safeguards ["Accept complexity" "Appropriate methods"])
   (failure "complexity-everywhere" "medium"
            "Treating everything as complex"
            :signals ["Overcomplicating simple problems"]
            :safeguards ["Domain assessment" "Simple when appropriate"])
   (failure "static-domain-thinking" "medium"
            "Not seeing domain shifts"
            :signals ["Approach no longer working"]
            :safeguards ["Monitor for shifts" "Adaptive approach"])
   (failure "chaos-panic" "high"
            "Paralysis in chaotic domain"
            :signals ["No action" "Waiting for clarity"]
            :safeguards ["Act to stabilize" "Then assess"])]})

(register-model
 {:name "tight-coupling"
  :category "complexity"
  :originator "Charles Perrow"
  :description "Systems where components are highly interdependent"
  :key-insight "Tight coupling amplifies failures and reduces recovery time"
  :application "Understand coupling; add slack where needed"
  :failure-modes
  [(failure "coupling-blindness" "high"
            "Not seeing tight coupling"
            :signals ["Cascade failures" "Unexpected propagation"]
            :safeguards ["Map dependencies" "Analyze coupling"])
   (failure "over-coupling" "high"
            "Creating unnecessary tight coupling"
            :signals ["Fragility" "No buffer"]
            :safeguards ["Loose coupling" "Add buffers"])
   (failure "under-coupling" "medium"
            "Too loose coupling losing coordination"
            :signals ["No coordination" "Duplication"]
            :safeguards ["Appropriate coupling" "Integration points"])
   (failure "coupling-rigidity" "medium"
            "Unable to change coupling"
            :signals ["Locked into architecture"]
            :safeguards ["Modular design" "Coupling flexibility"])
   (failure "hidden-coupling" "high"
            "Coupling through unexpected channels"
            :signals ["Surprising interactions"]
            :safeguards ["Map all connections" "Test interactions"])]})

(register-model
 {:name "normal-accidents"
  :category "complexity"
  :originator "Charles Perrow"
  :description "Accidents that are inevitable in complex, tightly-coupled systems"
  :key-insight "Some systems will fail despite best efforts"
  :application "Accept some failures; design for graceful degradation"
  :failure-modes
  [(failure "accident-denial" "high"
            "Believing accidents can be eliminated"
            :signals ["Zero defect goals" "Blame culture"]
            :safeguards ["Accept inevitability" "Design for failure"])
   (failure "over-engineering" "medium"
            "Adding complexity to prevent accidents"
            :signals ["More complexity, more failure modes"]
            :safeguards ["Simplify" "Reduce coupling"])
   (failure "blame-individuals" "high"
            "Blaming people for system failures"
            :signals ["Scapegoating" "Missing system issues"]
            :safeguards ["System analysis" "Just culture"])
   (failure "complacency" "high"
            "Long accident-free period breeding complacency"
            :signals ["Relaxed vigilance" "Drift"]
            :safeguards ["Maintain vigilance" "Near-miss analysis"])
   (failure "recovery-neglect" "high"
            "Not preparing for recovery"
            :signals ["No recovery plans" "Catastrophic failures"]
            :safeguards ["Recovery planning" "Graceful degradation"])]})

;; ============================================
;; Additional Models - Batch 4 (Models 92-129)
;; ============================================

;; ---- BEHAVIORAL ECONOMICS ----

(register-model
 {:name "prospect-theory"
  :category "behavioral-economics"
  :originator "Kahneman & Tversky"
  :description "People value gains and losses differently, with losses weighing more heavily"
  :key-insight "Loss aversion is roughly 2x gain attraction"
  :application "Frame choices considering reference points and loss aversion"
  :failure-modes
  [(failure "reference-point-blindness" "high"
            "Not recognizing the reference point being used"
            :signals ["Inconsistent preferences" "Framing effects"]
            :safeguards ["Identify reference points" "Test multiple frames"])
   (failure "loss-aversion-exploitation" "high"
            "Being manipulated through loss framing"
            :signals ["Fear-based decisions" "Status quo bias"]
            :safeguards ["Reframe as gains" "Objective analysis"])
   (failure "certainty-effect-trap" "medium"
            "Overweighting certain outcomes vs probable ones"
            :signals ["Risk aversion for gains" "Risk seeking for losses"]
            :safeguards ["Expected value calculation" "Probability calibration"])
   (failure "isolation-effect" "medium"
            "Focusing on differences while ignoring commonalities"
            :signals ["Inconsistent choices" "Context dependence"]
            :safeguards ["Full option comparison" "Systematic evaluation"])
   (failure "endowment-effect" "medium"
            "Overvaluing what you already own"
            :signals ["Reluctance to trade" "Ownership premium"]
            :safeguards ["Objective valuation" "Ownership-blind analysis"])]})

(register-model
 {:name "hyperbolic-discounting"
  :category "behavioral-economics"
  :originator "Richard Thaler"
  :description "People prefer smaller immediate rewards over larger later rewards"
  :key-insight "Time preferences are inconsistent and favor the present"
  :application "Design commitment devices; account for present bias"
  :failure-modes
  [(failure "present-bias" "high"
            "Systematically overvaluing immediate gratification"
            :signals ["Procrastination" "Impulsive decisions"]
            :safeguards ["Pre-commitment" "Remove temptations"])
   (failure "preference-reversal" "high"
            "Changing preferences as options approach"
            :signals ["Broken commitments" "Last-minute changes"]
            :safeguards ["Binding commitments" "Cooling-off periods"])
   (failure "naive-forecasting" "medium"
            "Believing future self will be more patient"
            :signals ["Unrealistic plans" "Repeated failures"]
            :safeguards ["Assume present bias continues" "Build in slack"])
   (failure "over-commitment" "medium"
            "Committing to too much future work"
            :signals ["Overloaded schedule" "Burnout"]
            :safeguards ["Discount future capacity" "Buffer time"])
   (failure "savings-failure" "high"
            "Inability to save for future needs"
            :signals ["No emergency fund" "Retirement shortfall"]
            :safeguards ["Automatic savings" "Default enrollment"])]})