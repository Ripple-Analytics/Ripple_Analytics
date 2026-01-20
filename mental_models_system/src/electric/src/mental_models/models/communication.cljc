(ns mental-models.models.communication
  "Mental Models - Communication Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Communication
;; ============================================

(register-model
 {:name "hanlon-razor"
  :category "communication"
  :originator "Robert Hanlon"
  :description "Never attribute to malice what can be explained by incompetence"
  :key-insight "Most bad outcomes are from mistakes, not evil intent"
  :application "Assume good intent until proven otherwise"
  :failure-modes
  [(failure "paranoia" "high"
            "Assuming malice when incompetence explains it"
            :signals ["Conspiracy thinking" "Assuming bad intent"]
            :safeguards ["Consider simpler explanations" "Assume good faith"])
   (failure "naivete" "high"
            "Ignoring actual malice"
            :signals ["Being taken advantage of" "Ignoring red flags"]
            :safeguards ["Verify trust" "Watch for patterns"])
   (failure "incompetence-excuse" "medium"
            "Excusing repeated incompetence"
            :signals ["Same mistakes repeatedly" "No improvement"]
            :safeguards ["Track patterns" "Accountability"])
   (failure "self-serving-application" "medium"
            "Applying differently to self vs others"
            :signals ["Excusing own malice as incompetence"]
            :safeguards ["Apply consistently" "Self-reflection"])
   (failure "systemic-blindness" "medium"
            "Missing systemic issues behind incompetence"
            :signals ["Blaming individuals for system failures"]
            :safeguards ["Look for root causes" "System analysis"])]})

(register-model
 {:name "occams-razor"
  :category "communication"
  :originator "William of Ockham"
  :description "The simplest explanation is usually correct"
  :key-insight "Don't multiply entities beyond necessity"
  :application "Prefer simpler explanations over complex ones"
  :failure-modes
  [(failure "oversimplification" "high"
            "Choosing too simple an explanation"
            :signals ["Missing important factors" "Reductionism"]
            :safeguards ["Test explanatory power" "Consider complexity"])
   (failure "complexity-bias" "medium"
            "Preferring complex explanations"
            :signals ["Overcomplicating" "Missing obvious answers"]
            :safeguards ["Start simple" "Add complexity only if needed"])
   (failure "false-simplicity" "high"
            "Mistaking familiar for simple"
            :signals ["Choosing comfortable over correct"]
            :safeguards ["Define simplicity carefully" "Test predictions"])
   (failure "simplicity-as-truth" "medium"
            "Assuming simple means true"
            :signals ["Rejecting correct complex explanations"]
            :safeguards ["Simplicity is heuristic not proof"])
   (failure "domain-mismatch" "medium"
            "Applying to domains where complexity is real"
            :signals ["Oversimplifying complex systems"]
            :safeguards ["Know when complexity is warranted"])]})

(register-model
 {:name "map-territory"
  :category "communication"
  :originator "Alfred Korzybski"
  :description "The map is not the territory - models are not reality"
  :key-insight "All models are wrong, some are useful"
  :application "Remember that your mental models are simplifications"
  :failure-modes
  [(failure "map-reality-confusion" "critical"
            "Confusing the model with reality"
            :signals ["Defending model over evidence" "Model worship"]
            :safeguards ["Test models against reality" "Update models"])
   (failure "map-neglect" "medium"
            "Not using maps at all"
            :signals ["No frameworks" "Reinventing wheels"]
            :safeguards ["Use models as tools" "Learn frameworks"])
   (failure "single-map" "high"
            "Using only one map"
            :signals ["One framework for everything"]
            :safeguards ["Multiple models" "Context-appropriate maps"])
   (failure "outdated-map" "high"
            "Using maps that no longer match territory"
            :signals ["Surprised by reality" "Model failures"]
            :safeguards ["Update maps regularly" "Reality checks"])
   (failure "map-precision-illusion" "medium"
            "Believing detailed maps are accurate"
            :signals ["False precision" "Overconfident predictions"]
            :safeguards ["Acknowledge uncertainty" "Rough maps often better"])]})

;; ============================================