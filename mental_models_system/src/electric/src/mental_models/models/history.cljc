(ns mental-models.models.history
  "Mental Models - History Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: History
;; ============================================

(register-model
 {:name "lindy-effect"
  :category "history"
  :originator "Nassim Taleb"
  :description "The longer something has survived, the longer it's likely to survive"
  :key-insight "Time is the ultimate test; old things have proven durability"
  :application "Prefer time-tested over novel when durability matters"
  :failure-modes
  [(failure "novelty-bias" "high"
            "Preferring new over time-tested"
            :signals ["Chasing trends" "Ignoring classics"]
            :safeguards ["Consider age" "Respect survival"])
   (failure "lindy-worship" "medium"
            "Assuming old is always better"
            :signals ["Rejecting valid innovation" "Stagnation"]
            :safeguards ["Evaluate on merits" "Innovation has place"])
   (failure "wrong-domain" "medium"
            "Applying Lindy where it doesn't apply"
            :signals ["Perishables treated as non-perishable"]
            :safeguards ["Domain appropriateness" "Perishable vs durable"])
   (failure "survival-bias" "medium"
            "Not seeing what didn't survive"
            :signals ["Overestimating old things' quality"]
            :safeguards ["Consider failures" "Full population"])
   (failure "context-change" "medium"
            "Old thing surviving in changed context"
            :signals ["Outdated despite age"]
            :safeguards ["Context relevance" "Adaptation check"])]})

(register-model
 {:name "chestertons-fence"
  :category "history"
  :originator "G.K. Chesterton"
  :description "Don't remove something until you understand why it's there"
  :key-insight "Things exist for reasons; understand before changing"
  :application "Understand the purpose before removing or changing"
  :failure-modes
  [(failure "blind-removal" "high"
            "Removing without understanding"
            :signals ["Unintended consequences" "Breaking things"]
            :safeguards ["Understand first" "Ask why it exists"])
   (failure "fence-worship" "medium"
            "Never removing anything"
            :signals ["Accumulating cruft" "Outdated practices"]
            :safeguards ["Understand then evaluate" "Remove if obsolete"])
   (failure "false-understanding" "medium"
            "Thinking you understand when you don't"
            :signals ["Confident removal, bad outcomes"]
            :safeguards ["Verify understanding" "Test removal"])
   (failure "lost-knowledge" "high"
            "Original reason forgotten"
            :signals ["Nobody knows why" "Tribal knowledge lost"]
            :safeguards ["Document reasons" "Institutional memory"])
   (failure "changed-circumstances" "medium"
            "Reason no longer applies"
            :signals ["Fence for old problem"]
            :safeguards ["Evaluate current relevance" "Update or remove"])]})

(register-model
 {:name "path-dependence"
  :category "history"
  :originator "Economics/History"
  :description "Current options are constrained by past choices"
  :key-insight "History matters; we can't always start fresh"
  :application "Understand how past choices constrain present options"
  :failure-modes
  [(failure "path-blindness" "high"
            "Ignoring how history constrains options"
            :signals ["Proposing impossible changes" "Ignoring legacy"]
            :safeguards ["Understand history" "Work within constraints"])
   (failure "path-determinism" "medium"
            "Assuming path is unchangeable"
            :signals ["Fatalism" "Not trying to change"]
            :safeguards ["Look for path breaks" "Strategic pivots"])
   (failure "sunk-cost-path" "high"
            "Staying on path due to sunk costs"
            :signals ["Continuing bad path" "Throwing good after bad"]
            :safeguards ["Evaluate future only" "Path switching"])
   (failure "lock-in-blindness" "high"
            "Not seeing lock-in until too late"
            :signals ["Trapped in suboptimal path"]
            :safeguards ["Early path evaluation" "Preserve optionality"])
   (failure "path-creation-failure" "medium"
            "Not creating new paths when possible"
            :signals ["Accepting constraints unnecessarily"]
            :safeguards ["Challenge constraints" "Create new paths"])]})

;; ============================================