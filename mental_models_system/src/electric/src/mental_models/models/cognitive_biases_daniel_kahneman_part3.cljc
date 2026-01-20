(ns mental-models.models.cognitive-biases-daniel-kahneman-part3
  "Mental Models - Cognitive Biases (Daniel Kahneman) Category (Part 3)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Cognitive Biases (Daniel Kahneman) (Part 3)
;; ============================================

(register-model
 {:name "redundancy"
  :category "engineering"
  :originator "Engineering / Nassim Taleb"
  :description "Backup systems and spare capacity that seem wasteful but provide resilience"
  :key-insight "Apparent inefficiency can be essential for survival; redundancy prevents catastrophic failure"
  :application "Build redundancy in critical systems; accept inefficiency for robustness"
  :failure-modes
  [(failure "no-redundancy" "critical"
            "Single points of failure"
            :signals ["Fragile systems" "Catastrophic failures" "No backup"]
            :safeguards ["Redundant systems" "Backup plans" "Spare capacity"])
   (failure "excessive-redundancy" "medium"
            "Too much redundancy, wasting resources"
            :signals ["Inefficiency" "High costs" "Diminishing returns"]
            :safeguards ["Cost-benefit analysis" "Optimal redundancy" "Risk-based approach"])
   (failure "correlated-redundancy" "critical"
            "Backups fail for same reason as primary"
            :signals ["Common mode failures" "Simultaneous failures" "False security"]
            :safeguards ["Independent systems" "Diverse backups" "Uncorrelated failures"])
   (failure "redundancy-neglect" "high"
            "Letting backup systems atrophy"
            :signals ["Untested backups" "Degraded systems" "Failure when needed"]
            :safeguards ["Regular testing" "Maintenance" "Active redundancy"])
   (failure "efficiency-obsession" "high"
            "Eliminating all redundancy for efficiency"
            :signals ["Lean to fragile" "No slack" "Brittleness"]
            :safeguards ["Robustness first" "Strategic inefficiency" "Margin of safety"])]})

(register-model
 {:name "degeneracy"
  :category "biology"
  :originator "Biology / Complex Systems"
  :description "Multiple different pathways can lead to the same outcome; redundancy at the functional level"
  :key-insight "Systems with degeneracy are robust; one pathway fails, others compensate"
  :application "Build multiple paths to goals; don't rely on single method"
  :failure-modes
  [(failure "single-pathway" "critical"
            "Only one way to achieve goal"
            :signals ["Fragile plans" "No alternatives" "Failure if blocked"]
            :safeguards ["Multiple pathways" "Alternative methods" "Redundant approaches"])
   (failure "pathway-correlation" "high"
            "All pathways vulnerable to same failure"
            :signals ["Common vulnerabilities" "Simultaneous failure" "False diversity"]
            :safeguards ["Independent pathways" "Diverse methods" "Uncorrelated approaches"])
   (failure "degeneracy-excess" "medium"
            "Too many pathways, losing focus"
            :signals ["Scattered effort" "No mastery" "Inefficiency"]
            :safeguards ["Strategic pathways" "Focus on best" "Prune weak options"])
   (failure "pathway-neglect" "high"
            "Not maintaining alternative pathways"
            :signals ["Atrophied skills" "Lost options" "Reduced flexibility"]
            :safeguards ["Practice alternatives" "Maintain skills" "Keep pathways viable"])
   (failure "degeneracy-blindness" "medium"
            "Not recognizing multiple valid approaches"
            :signals ["Dogmatic thinking" "One true way" "Missed opportunities"]
            :safeguards ["Open-mindedness" "Explore alternatives" "Multiple perspectives"])]})

(register-model
 {:name "satisficing"
  :category "decision_theory"
  :originator "Herbert Simon"
  :description "Seeking a satisfactory solution rather than the optimal one; good enough is often better than perfect"
  :key-insight "Optimization has costs; satisficing is rational when search costs exceed marginal benefits"
  :application "Define 'good enough' criteria; stop searching when met"
  :failure-modes
  [(failure "premature-satisficing" "high"
            "Accepting first acceptable option without adequate search"
            :signals ["Suboptimal outcomes" "Regret" "Missed better options"]
            :safeguards ["Minimum search" "Multiple options" "Adequate exploration"])
   (failure "perfectionism" "high"
            "Refusing to satisfice, seeking perfection"
            :signals ["Analysis paralysis" "Missed deadlines" "Excessive costs"]
            :safeguards ["Good enough criteria" "Diminishing returns" "Opportunity cost"])
   (failure "low-standards" "high"
            "Setting satisficing bar too low"
            :signals ["Poor outcomes" "Mediocrity" "Regret"]
            :safeguards ["Appropriate standards" "Calibration" "Context-dependent criteria"])
   (failure "high-standards" "medium"
            "Setting satisficing bar too high"
            :signals ["Effective perfectionism" "Never satisfied" "Wasted effort"]
            :safeguards ["Realistic standards" "Pareto principle" "Cost-benefit"])
   (failure "satisficing-rigidity" "medium"
            "Not adjusting satisficing criteria to context"
            :signals ["Wrong standards" "Context mismatch" "Poor decisions"]
            :safeguards ["Context-dependent" "Flexible standards" "Situational awareness"])]})

;; ============================================
;; Iteration 9 Summary
;; ============================================
;; Added 8 new mental models:
;; 1. Hormesis (Biology/Taleb) - Beneficial stress
;; 2. Ergodicity (Ole Peters) - Time vs ensemble averages
;; 3. Reflexivity (George Soros) - Self-reinforcing feedback
;; 4. Convexity (Nassim Taleb) - Non-linear payoffs
;; 5. Optionality (Nassim Taleb) - Asymmetric upside
;; 6. Redundancy (Engineering/Taleb) - Backup systems
;; 7. Degeneracy (Biology) - Multiple pathways
;; 8. Satisficing (Herbert Simon) - Good enough vs optimal
;;
;; Previous total (after Iteration 8): 139 models
;; New total: 147 models (in models.cljc)
;; Combined with new_models.cljc (12 models): 159 total models
;; ============================================

;; ============================================
;; Iteration 15 - High-Magnitude Enhancement
;; ============================================
;; Adding 10 new mental models from Renaissance Technologies (Jim Simons),
;; Ray Dalio, and Lee Kuan Yew frameworks
;; ============================================

;; ============================================