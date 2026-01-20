(ns mental-models.models.design
  "Mental Models - Design Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Design
;; ============================================

(register-model
 {:name "form-follows-function"
  :category "design"
  :originator "Louis Sullivan"
  :description "Design should be determined by purpose"
  :key-insight "Function should drive form, not the reverse"
  :application "Start with purpose; let design emerge from requirements"
  :failure-modes
  [(failure "form-over-function" "high"
            "Prioritizing aesthetics over utility"
            :signals ["Beautiful but unusable" "Style over substance"]
            :safeguards ["Function first" "User testing"])
   (failure "function-only" "medium"
            "Ignoring form entirely"
            :signals ["Ugly but functional" "Poor user experience"]
            :safeguards ["Balance form and function" "Design matters"])
   (failure "wrong-function" "high"
            "Designing for wrong function"
            :signals ["Solving wrong problem" "Misunderstood requirements"]
            :safeguards ["Validate requirements" "User research"])
   (failure "function-creep" "medium"
            "Adding functions that distort form"
            :signals ["Feature bloat" "Confused design"]
            :safeguards ["Scope discipline" "Core function focus"])
   (failure "context-blindness" "medium"
            "Ignoring context in form-function relationship"
            :signals ["Works in isolation, fails in context"]
            :safeguards ["Contextual design" "System thinking"])]})

(register-model
 {:name "affordances"
  :category "design"
  :originator "James Gibson / Don Norman"
  :description "Properties that suggest how something should be used"
  :key-insight "Good design makes correct use obvious"
  :application "Design affordances that guide correct behavior"
  :failure-modes
  [(failure "hidden-affordances" "high"
            "Affordances not visible or discoverable"
            :signals ["Users can't figure out how to use" "Need instructions"]
            :safeguards ["Make affordances visible" "User testing"])
   (failure "false-affordances" "high"
            "Suggesting wrong actions"
            :signals ["Users do wrong thing" "Confusion"]
            :safeguards ["Match affordance to function" "Test with users"])
   (failure "conflicting-affordances" "medium"
            "Multiple conflicting suggestions"
            :signals ["User uncertainty" "Errors"]
            :safeguards ["Clear single affordance" "Remove conflicts"])
   (failure "cultural-blindness" "medium"
            "Affordances that don't translate across cultures"
            :signals ["Works in one culture, fails in another"]
            :safeguards ["Cross-cultural testing" "Universal design"])
   (failure "affordance-overload" "medium"
            "Too many affordances"
            :signals ["Overwhelming" "Choice paralysis"]
            :safeguards ["Simplify" "Progressive disclosure"])]})

(register-model
 {:name "constraints"
  :category "design"
  :originator "Design Theory"
  :description "Limitations that guide behavior and prevent errors"
  :key-insight "Good constraints make wrong actions impossible"
  :application "Design constraints that prevent errors"
  :failure-modes
  [(failure "insufficient-constraints" "high"
            "Not enough constraints allowing errors"
            :signals ["User errors" "Misuse"]
            :safeguards ["Add appropriate constraints" "Error prevention"])
   (failure "over-constraint" "medium"
            "Too many constraints limiting useful actions"
            :signals ["Frustration" "Workarounds"]
            :safeguards ["Balance constraints" "Enable legitimate use"])
   (failure "wrong-constraints" "high"
            "Constraints that prevent right actions"
            :signals ["Can't do what you need" "Forced errors"]
            :safeguards ["Test constraints" "User feedback"])
   (failure "constraint-workarounds" "medium"
            "Users finding ways around constraints"
            :signals ["Shadow systems" "Unsafe workarounds"]
            :safeguards ["Understand why" "Better constraints"])
   (failure "invisible-constraints" "medium"
            "Constraints users don't understand"
            :signals ["Confusion" "Frustration"]
            :safeguards ["Make constraints visible" "Explain why"])]})

;; ============================================