(ns mental-models.models.learning
  "Mental Models - Learning Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Learning
;; ============================================

(register-model
 {:name "deliberate-practice"
  :category "learning"
  :originator "Anders Ericsson"
  :description "Focused practice on weaknesses with immediate feedback"
  :key-insight "10,000 hours of deliberate practice, not just any practice"
  :application "Practice at the edge of your ability with feedback"
  :failure-modes
  [(failure "mindless-repetition" "high"
            "Practicing without focus or feedback"
            :signals ["No improvement" "Going through motions"]
            :safeguards ["Focused attention" "Immediate feedback"])
   (failure "comfort-zone-practice" "high"
            "Practicing what you're already good at"
            :signals ["No challenge" "Feeling comfortable"]
            :safeguards ["Target weaknesses" "Increase difficulty"])
   (failure "feedback-absence" "high"
            "Practicing without feedback"
            :signals ["No correction" "Reinforcing errors"]
            :safeguards ["Get feedback" "Measure progress"])
   (failure "burnout" "medium"
            "Unsustainable practice intensity"
            :signals ["Exhaustion" "Declining motivation"]
            :safeguards ["Rest and recovery" "Sustainable schedule"])
   (failure "transfer-failure" "medium"
            "Practice not transferring to performance"
            :signals ["Good in practice, poor in performance"]
            :safeguards ["Realistic practice" "Performance simulation"])]})

(register-model
 {:name "first-principles"
  :category "learning"
  :originator "Aristotle / Elon Musk"
  :description "Reason from fundamental truths rather than analogy"
  :key-insight "Break down problems to their basic elements"
  :application "Ask 'What do we know to be true?' and build from there"
  :failure-modes
  [(failure "analogy-dependence" "high"
            "Always reasoning by analogy"
            :signals ["This is how it's always done" "Copying others"]
            :safeguards ["Question assumptions" "Start from scratch"])
   (failure "false-first-principles" "high"
            "Treating assumptions as first principles"
            :signals ["Unexamined beliefs" "Hidden assumptions"]
            :safeguards ["Question everything" "Verify fundamentals"])
   (failure "analysis-paralysis" "medium"
            "Getting stuck in first principles analysis"
            :signals ["Never acting" "Endless decomposition"]
            :safeguards ["Time limits" "Good enough analysis"])
   (failure "reinventing-wheels" "medium"
            "Ignoring valid existing solutions"
            :signals ["Wasted effort" "Slower than necessary"]
            :safeguards ["Learn from others" "Use analogy appropriately"])
   (failure "physics-envy" "medium"
            "Applying physics thinking to non-physics domains"
            :signals ["Oversimplifying complex systems"]
            :safeguards ["Domain-appropriate methods" "Respect complexity"])]})

(register-model
 {:name "mental-models-meta"
  :category "learning"
  :originator "Charlie Munger"
  :description "Build a latticework of mental models from multiple disciplines"
  :key-insight "The person with more models wins"
  :application "Continuously add models and practice applying them"
  :failure-modes
  [(failure "model-collection" "medium"
            "Collecting models without applying them"
            :signals ["Know models but don't use them"]
            :safeguards ["Practice application" "Use in decisions"])
   (failure "hammer-nail" "high"
            "Applying favorite model to everything"
            :signals ["One model dominates" "Forcing fit"]
            :safeguards ["Multiple models" "Match model to situation"])
   (failure "model-overload" "medium"
            "Too many models causing confusion"
            :signals ["Paralysis" "Contradictory advice"]
            :safeguards ["Prioritize models" "Context-appropriate selection"])
   (failure "shallow-understanding" "high"
            "Knowing models superficially"
            :signals ["Can't apply in novel situations"]
            :safeguards ["Deep study" "Practice application"])
   (failure "model-rigidity" "medium"
            "Not updating or discarding models"
            :signals ["Outdated models" "Ignoring better models"]
            :safeguards ["Regular review" "Update and prune"])]})

;; ============================================