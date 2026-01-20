(ns mental-models.models.cognitive-biases-daniel-kahneman-part1
  "Mental Models - Cognitive Biases (Daniel Kahneman) Category (Part 1)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Cognitive Biases (Daniel Kahneman) (Part 1)
;; ============================================

(register-model
 {:name "base-rate-neglect"
  :category "cognitive_biases"
  :originator "Daniel Kahneman / Amos Tversky"
  :description "Ignoring statistical base rates in favor of specific information"
  :key-insight "Start with base rates, then adjust for specific evidence"
  :application "Always ask: What's the base rate? Then update from there"
  :failure-modes
  [(failure "pure-base-rate" "medium"
            "Ignoring all specific information"
            :signals ["Generic predictions" "No personalization"]
            :safeguards ["Bayesian updating" "Incorporate specific evidence" "Balanced weighting"])
   (failure "narrative-override" "high"
            "Compelling story overrides statistics"
            :signals ["Ignoring data" "Anecdote-driven decisions"]
            :safeguards ["Explicit base rate check" "Data discipline" "Statistical anchoring"])
   (failure "inside-view-bias" "high"
            "Focusing on case specifics, ignoring reference class"
            :signals ["Overconfident predictions" "Planning fallacy"]
            :safeguards ["Outside view" "Reference class forecasting" "Historical data"])
   (failure "wrong-reference-class" "high"
            "Using incorrect base rate"
            :signals ["Misleading predictions" "Poor calibration"]
            :safeguards ["Careful reference class selection" "Multiple reference classes" "Domain expertise"])
   (failure "base-rate-fallacy-fallacy" "medium"
            "Overcorrecting for base rate neglect"
            :signals ["Ignoring strong specific evidence" "Underconfidence"]
            :safeguards ["Appropriate Bayesian updating" "Evidence strength assessment" "Balanced approach"])]})

(register-model
 {:name "availability-cascade"
  :category "cognitive_biases"
  :originator "Daniel Kahneman / Cass Sunstein"
  :description "Self-reinforcing cycle where belief spreads through repetition"
  :key-insight "Availability heuristic + social proof = runaway belief"
  :application "Question widely held beliefs; check for availability cascade"
  :failure-modes
  [(failure "cascade-participation" "high"
            "Joining the cascade without verification"
            :signals ["Accepting popular belief" "No independent analysis"]
            :safeguards ["Independent verification" "Contrarian check" "Primary sources"])
   (failure "cascade-amplification" "medium"
            "Spreading unverified information"
            :signals ["Viral misinformation" "Echo chamber"]
            :safeguards ["Fact checking" "Source verification" "Responsible sharing"])
   (failure "contrarian-for-sake-of-it" "medium"
            "Rejecting all popular beliefs"
            :signals ["Conspiracy thinking" "Isolation"]
            :safeguards ["Evidence-based skepticism" "Balanced evaluation" "Humble inquiry"])
   (failure "cascade-blindness" "high"
            "Not recognizing when in a cascade"
            :signals ["Groupthink" "Surprise when cascade reverses"]
            :safeguards ["Cascade detection" "Dissent monitoring" "Independent thinkers"])
   (failure "late-cascade-entry" "high"
            "Joining cascade near peak"
            :signals ["Buying tops" "Following crowds late"]
            :safeguards ["Contrarian timing" "Sentiment analysis" "Trend maturity assessment"])]})

(register-model
 {:name "prospect-theory"
  :category "decision_theory"
  :originator "Daniel Kahneman / Amos Tversky"
  :description "People value gains and losses differently; losses hurt more than equivalent gains feel good"
  :key-insight "Loss aversion, reference dependence, and diminishing sensitivity shape decisions"
  :application "Frame decisions to account for loss aversion and reference points"
  :failure-modes
  [(failure "loss-aversion-paralysis" "high"
            "Fear of losses prevents necessary action"
            :signals ["Status quo bias" "Missed opportunities"]
            :safeguards ["Reframe as opportunity cost" "Long-term thinking" "Risk-adjusted perspective"])
   (failure "reference-point-manipulation" "high"
            "Manipulating reference points to influence decisions"
            :signals ["Anchoring tricks" "Framing effects"]
            :safeguards ["Multiple reference points" "Absolute value assessment" "Awareness of framing"])
   (failure "sunk-cost-fallacy" "high"
            "Continuing due to past losses"
            :signals ["Throwing good money after bad" "Escalation of commitment"]
            :safeguards ["Forward-looking analysis" "Ignore sunk costs" "Exit criteria"])
   (failure "risk-seeking-in-losses" "critical"
            "Taking excessive risks to avoid realizing losses"
            :signals ["Doubling down" "Gambling for redemption"]
            :safeguards ["Loss limits" "Rational risk assessment" "Accept losses"])
   (failure "narrow-framing" "high"
            "Evaluating each decision in isolation"
            :signals ["Inconsistent risk preferences" "Suboptimal portfolio"]
            :safeguards ["Broad framing" "Portfolio perspective" "Aggregate view"])]})

(register-model
 {:name "planning-fallacy"
  :category "forecasting"
  :originator "Daniel Kahneman / Amos Tversky"
  :description "Systematic tendency to underestimate time, costs, and risks"
  :key-insight "Inside view is optimistic; outside view provides realistic estimates"
  :application "Use reference class forecasting; multiply estimates by 2-3x"
  :failure-modes
  [(failure "inside-view-only" "critical"
            "Using only project-specific information"
            :signals ["Consistent underestimation" "Surprised by delays"]
            :safeguards ["Outside view" "Reference class forecasting" "Historical data"])
   (failure "optimism-bias" "high"
            "Assuming best-case scenarios"
            :signals ["No contingency planning" "Unrealistic schedules"]
            :safeguards ["Pessimistic scenarios" "Pre-mortem" "Buffer time"])
   (failure "insufficient-buffer" "high"
            "Adding small buffer to optimistic estimate"
            :signals ["Still missing deadlines" "Inadequate contingency"]
            :safeguards ["2-3x multiplier" "Generous buffers" "Murphy's law"])
   (failure "planning-fallacy-fallacy" "medium"
            "Overcorrecting with excessive pessimism"
            :signals ["Missed opportunities" "Excessive caution"]
            :safeguards ["Calibrated estimates" "Track record" "Balanced approach"])
   (failure "ignoring-black-swans" "critical"
            "Not accounting for unknown unknowns"
            :signals ["Catastrophic surprises" "No tail risk planning"]
            :safeguards ["Scenario planning" "Stress testing" "Margin of safety"])]})

;; ============================================
;; Iteration 8 Summary
;; ============================================
;; Added 10 new mental models:
;; 1. Antifragility (Taleb)
;; 2. Barbell Strategy (Taleb)
;; 3. Via Negativa (Taleb)
;; 4. Skin in the Game (Taleb)
;; 5. Lindy Effect (Taleb)
;; 6. Kelly Criterion (Thorp)
;; 7. Base Rate Neglect (Kahneman)
;; 8. Availability Cascade (Kahneman)
;; 9. Prospect Theory (Kahneman)
;; 10. Planning Fallacy (Kahneman)
;;
;; Previous total: 129 models
;; New total: 139 models
;; ============================================


;; ============================================
;; Iteration 9 - High-Magnitude Expansion
;; ============================================