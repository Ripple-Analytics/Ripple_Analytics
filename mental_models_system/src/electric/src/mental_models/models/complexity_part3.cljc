(ns mental-models.models.complexity-part3
  "Mental Models - Complexity Category (Part 3)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 3)
;; ============================================

(register-model
 {:name "leverage-points"
  :category "systems-dynamics"
  :originator "Donella Meadows"
  :description "Places in a system where small changes can produce large effects"
  :key-insight "Higher leverage points are harder to find but more powerful"
  :application "Identify and act on high-leverage intervention points"
  :failure-modes
  [(failure "low-leverage-focus" "high"
            "Working on parameters instead of structure"
            :signals ["Lots of effort, little change"]
            :safeguards ["Seek higher leverage" "System structure"])
   (failure "leverage-reversal" "high"
            "Pushing leverage points in wrong direction"
            :signals ["Counterintuitive results" "System resistance"]
            :safeguards ["Understand system" "Test direction"])
   (failure "single-point-focus" "medium"
            "Ignoring multiple leverage points"
            :signals ["Incomplete intervention" "Side effects"]
            :safeguards ["Multiple points" "System view"])
   (failure "paradigm-blindness" "high"
            "Not seeing highest leverage: paradigm change"
            :signals ["Stuck in current thinking"]
            :safeguards ["Question assumptions" "Paradigm awareness"])
   (failure "implementation-gap" "medium"
            "Knowing leverage point but not acting"
            :signals ["Analysis without action"]
            :safeguards ["Bias to action" "Small experiments"])]})

(register-model
 {:name "system-archetypes"
  :category "systems-dynamics"
  :originator "Peter Senge"
  :description "Common patterns of system behavior that recur across domains"
  :key-insight "Recognizing archetypes enables faster diagnosis and intervention"
  :application "Learn archetypes; recognize them in new situations"
  :failure-modes
  [(failure "archetype-forcing" "medium"
            "Forcing situations into familiar archetypes"
            :signals ["Poor fit" "Missing uniqueness"]
            :safeguards ["Test fit" "Allow novelty"])
   (failure "archetype-blindness" "high"
            "Not recognizing common patterns"
            :signals ["Reinventing wheel" "Repeated mistakes"]
            :safeguards ["Learn archetypes" "Pattern recognition"])
   (failure "single-archetype" "medium"
            "Seeing only one archetype when multiple apply"
            :signals ["Incomplete diagnosis" "Partial solutions"]
            :safeguards ["Check multiple" "Layered analysis"])
   (failure "static-archetype" "medium"
            "Not seeing archetype evolution"
            :signals ["Outdated intervention" "System changed"]
            :safeguards ["Monitor evolution" "Adaptive response"])
   (failure "archetype-fatalism" "medium"
            "Believing archetypes are inevitable"
            :signals ["No intervention" "Learned helplessness"]
            :safeguards ["Archetypes can be broken" "Intervention points"])]})

;; ---- DECISION THEORY ----

(register-model
 {:name "regret-minimization"
  :category "decision-theory"
  :originator "Jeff Bezos"
  :description "Make decisions by minimizing future regret"
  :key-insight "Project to end of life and ask what you'd regret not doing"
  :application "Use for major life decisions; consider long-term regret"
  :failure-modes
  [(failure "short-term-regret" "medium"
            "Focusing on immediate regret over long-term"
            :signals ["Safe choices" "Missed opportunities"]
            :safeguards ["Long time horizon" "End-of-life perspective"])
   (failure "regret-asymmetry" "medium"
            "Not seeing regret of inaction"
            :signals ["Status quo bias" "Omission regret"]
            :safeguards ["Consider both" "Action vs inaction"])
   (failure "hindsight-contamination" "medium"
            "Judging past decisions with current knowledge"
            :signals ["Unfair self-criticism" "Wrong lessons"]
            :safeguards ["Process over outcome" "Information available then"])
   (failure "regret-paralysis" "high"
            "Unable to decide due to potential regret"
            :signals ["Indecision" "Anxiety"]
            :safeguards ["Accept some regret" "Reversible choices"])
   (failure "others-regret" "medium"
            "Minimizing others' regret instead of own"
            :signals ["People pleasing" "Inauthentic choices"]
            :safeguards ["Own values" "Personal regret focus"])]})

(register-model
 {:name "reversibility"
  :category "decision-theory"
  :originator "Jeff Bezos"
  :description "Distinguish between reversible (two-way door) and irreversible (one-way door) decisions"
  :key-insight "Reversible decisions should be made quickly; irreversible ones carefully"
  :application "Assess reversibility; match decision speed to stakes"
  :failure-modes
  [(failure "false-irreversibility" "high"
            "Treating reversible decisions as permanent"
            :signals ["Slow decisions" "Excessive analysis"]
            :safeguards ["Test reversibility" "Bias to action"])
   (failure "false-reversibility" "high"
            "Treating irreversible decisions as reversible"
            :signals ["Hasty major decisions" "Regret"]
            :safeguards ["Assess true reversibility" "Slow down"])
   (failure "reversal-cost-blindness" "medium"
            "Not seeing the cost of reversal"
            :signals ["Easy reversal assumption" "Hidden costs"]
            :safeguards ["Full reversal cost" "Include friction"])
   (failure "option-preservation-paralysis" "medium"
            "Keeping options open too long"
            :signals ["No commitment" "Opportunity cost"]
            :safeguards ["Decide when ready" "Option value decay"])
   (failure "sunk-cost-reversal" "high"
            "Not reversing due to sunk costs"
            :signals ["Continuing bad decisions"]
            :safeguards ["Ignore sunk costs" "Fresh evaluation"])]})

(register-model
 {:name "satisficing"
  :category "decision-theory"
  :originator "Herbert Simon"
  :description "Choosing an option that meets minimum criteria rather than optimizing"
  :key-insight "Good enough is often better than best"
  :application "Set minimum criteria; stop searching when met"
  :failure-modes
  [(failure "premature-satisficing" "medium"
            "Accepting too quickly without adequate search"
            :signals ["Poor outcomes" "Better options existed"]
            :safeguards ["Minimum search" "Adequate criteria"])
   (failure "criteria-creep" "medium"
            "Raising criteria as options are found"
            :signals ["Never satisfied" "Endless search"]
            :safeguards ["Fix criteria upfront" "Commit to them"])
   (failure "maximizing-disguise" "medium"
            "Satisficing label but actually maximizing"
            :signals ["Continued search after 'good enough'"]
            :safeguards ["True acceptance" "Stop searching"])
   (failure "low-standards" "medium"
            "Setting criteria too low"
            :signals ["Mediocre outcomes" "Regret"]
            :safeguards ["Appropriate standards" "Calibrate criteria"])
   (failure "context-blindness" "medium"
            "Same criteria for all decisions"
            :signals ["Over/under-investing in decisions"]
            :safeguards ["Match criteria to stakes" "Flexible standards"])]})

;; ---- INFORMATION THEORY ----