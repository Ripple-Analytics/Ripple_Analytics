(ns mental-models.models.complexity-part5
  "Mental Models - Complexity Category (Part 5)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 5)
;; ============================================

(register-model
 {:name "epistemic-humility"
  :category "epistemology"
  :originator "Socrates"
  :description "Recognizing the limits of one's knowledge"
  :key-insight "Knowing what you don't know is crucial wisdom"
  :application "Map knowledge boundaries; seek to expand them"
  :failure-modes
  [(failure "overconfidence" "high"
            "Believing you know more than you do"
            :signals ["Surprised by outcomes" "Narrow confidence intervals"]
            :safeguards ["Calibration training" "Track predictions"])
   (failure "false-humility" "medium"
            "Claiming ignorance to avoid responsibility"
            :signals ["Learned helplessness" "No decisions"]
            :safeguards ["Act on best knowledge" "Responsible uncertainty"])
   (failure "expertise-blindspot" "high"
            "Not knowing what you don't know"
            :signals ["Unknown unknowns" "Blind spots"]
            :safeguards ["Seek feedback" "Diverse perspectives"])
   (failure "humility-paralysis" "medium"
            "Too humble to act"
            :signals ["Indecision" "Missed opportunities"]
            :safeguards ["Act under uncertainty" "Reversible experiments"])
   (failure "selective-humility" "medium"
            "Humble in some domains, overconfident in others"
            :signals ["Inconsistent calibration"]
            :safeguards ["Domain-specific assessment" "Uniform standards"])]})

(register-model
 {:name "bayesian-updating"
  :category "epistemology"
  :originator "Thomas Bayes"
  :description "Updating beliefs based on new evidence using probability"
  :key-insight "Beliefs should change proportionally to evidence strength"
  :application "Start with priors; update with evidence; avoid extremes"
  :failure-modes
  [(failure "prior-anchoring" "high"
            "Not updating enough from priors"
            :signals ["Beliefs unchanged by evidence"]
            :safeguards ["Track updates" "Evidence sensitivity"])
   (failure "prior-abandonment" "high"
            "Abandoning priors too quickly"
            :signals ["Whipsawing beliefs" "Recency bias"]
            :safeguards ["Appropriate weighting" "Base rate respect"])
   (failure "evidence-selection" "high"
            "Only updating on confirming evidence"
            :signals ["Confirmation bias" "Asymmetric updating"]
            :safeguards ["Seek disconfirming" "Symmetric updating"])
   (failure "base-rate-neglect" "high"
            "Ignoring prior probabilities"
            :signals ["Overweighting specific evidence"]
            :safeguards ["Always consider base rates" "Reference class"])
   (failure "update-frequency" "medium"
            "Updating too often or too rarely"
            :signals ["Noise-driven or stale beliefs"]
            :safeguards ["Appropriate frequency" "Significant evidence only"])]})

(register-model
 {:name "falsificationism"
  :category "epistemology"
  :originator "Karl Popper"
  :description "Knowledge advances by attempting to falsify theories"
  :key-insight "Seek to disprove, not prove; surviving tests builds confidence"
  :application "Design tests that could falsify; value negative results"
  :failure-modes
  [(failure "confirmation-seeking" "high"
            "Looking for evidence that confirms"
            :signals ["Only positive tests" "Weak tests"]
            :safeguards ["Design falsifying tests" "Strong tests"])
   (failure "unfalsifiable-beliefs" "high"
            "Holding beliefs that can't be tested"
            :signals ["No possible disconfirmation" "Moving goalposts"]
            :safeguards ["Require falsifiability" "Specific predictions"])
   (failure "premature-falsification" "medium"
            "Abandoning theory on weak disconfirmation"
            :signals ["Discarding good theories" "Noise sensitivity"]
            :safeguards ["Replication" "Strong evidence required"])
   (failure "auxiliary-hypothesis-abuse" "medium"
            "Protecting theory with ad hoc additions"
            :signals ["Increasingly complex" "Epicycles"]
            :safeguards ["Parsimony" "Occam's razor"])
   (failure "falsification-avoidance" "high"
            "Avoiding tests that might falsify"
            :signals ["Untested beliefs" "Comfortable ignorance"]
            :safeguards ["Seek hard tests" "Value disconfirmation"])]})

;; ---- ORGANIZATIONAL BEHAVIOR ----

(register-model
 {:name "conways-law"
  :category "organizational"
  :originator "Melvin Conway"
  :description "Organizations design systems that mirror their communication structure"
  :key-insight "To change the system, change the organization"
  :application "Align org structure with desired system architecture"
  :failure-modes
  [(failure "structure-blindness" "high"
            "Not seeing org structure in system design"
            :signals ["Unexpected system boundaries" "Communication overhead"]
            :safeguards ["Map org to system" "Intentional alignment"])
   (failure "reverse-conway" "medium"
            "Trying to change system without changing org"
            :signals ["Resistance" "Reversion to old patterns"]
            :safeguards ["Change org first" "Aligned transformation"])
   (failure "over-alignment" "medium"
            "Too tight coupling of org and system"
            :signals ["Rigidity" "Can't evolve independently"]
            :safeguards ["Appropriate coupling" "Interface boundaries"])
   (failure "communication-assumption" "medium"
            "Assuming communication follows org chart"
            :signals ["Informal networks matter" "Shadow org"]
            :safeguards ["Map actual communication" "Informal structure"])
   (failure "static-thinking" "medium"
            "Not seeing org/system co-evolution"
            :signals ["Outdated alignment" "Drift"]
            :safeguards ["Regular reassessment" "Dynamic alignment"])]})

(register-model
 {:name "goodharts-law"
  :category "organizational"
  :originator "Charles Goodhart"
  :description "When a measure becomes a target, it ceases to be a good measure"
  :key-insight "Metrics get gamed when they become goals"
  :application "Use multiple metrics; measure what matters; expect gaming"
  :failure-modes
  [(failure "single-metric-focus" "high"
            "Optimizing one metric at expense of others"
            :signals ["Gaming" "Neglected dimensions"]
            :safeguards ["Multiple metrics" "Balanced scorecard"])
   (failure "metric-gaming" "high"
            "Hitting metric without achieving goal"
            :signals ["Good numbers, bad outcomes"]
            :safeguards ["Outcome focus" "Qualitative assessment"])
   (failure "metric-ossification" "medium"
            "Keeping metrics past usefulness"
            :signals ["Outdated measures" "Wrong incentives"]
            :safeguards ["Regular review" "Metric rotation"])
   (failure "measurement-distortion" "medium"
            "Measurement changing the measured"
            :signals ["Hawthorne effect" "Teaching to test"]
            :safeguards ["Unobtrusive measures" "Multiple methods"])
   (failure "proxy-confusion" "high"
            "Confusing proxy metric with actual goal"
            :signals ["Proxy optimization" "Goal displacement"]
            :safeguards ["Remember true goal" "Direct measurement"])]})