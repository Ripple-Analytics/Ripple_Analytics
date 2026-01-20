(ns mental-models.models.physics-biology
  "Mental Models - Physics/Biology Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Physics/Biology
;; ============================================

(register-model
 {:name "critical-mass"
  :category "physics"
  :originator "Nuclear Physics"
  :description "A threshold amount needed to sustain a chain reaction"
  :key-insight "Below threshold, nothing happens; above it, everything changes"
  :application "Identify and reach critical mass for any initiative"
  :failure-modes
  [(failure "threshold-blindness" "high"
            "Not recognizing threshold effects"
            :signals ["Expecting linear progress" "Giving up before threshold"]
            :safeguards ["Identify thresholds" "Push through" "Measure progress"])
   (failure "premature-scaling" "high"
            "Scaling before reaching critical mass"
            :signals ["Spreading too thin" "Diluting effort"]
            :safeguards ["Focus resources" "Achieve density" "Sequential expansion"])
   (failure "threshold-overestimation" "medium"
            "Thinking threshold is higher than it is"
            :signals ["Over-preparing" "Missing opportunities"]
            :safeguards ["Test early" "Iterate quickly" "Minimum viable approach"])
   (failure "threshold-underestimation" "medium"
            "Thinking threshold is lower than it is"
            :signals ["Premature launch" "Insufficient resources"]
            :safeguards ["Research requirements" "Build buffer" "Learn from others"])
   (failure "single-threshold-thinking" "medium"
            "Not seeing multiple thresholds"
            :signals ["Assuming one breakthrough is enough"]
            :safeguards ["Map all thresholds" "Plan for multiple stages"])]})

(register-model
 {:name "evolution"
  :category "biology"
  :originator "Charles Darwin"
  :description "Variation, selection, and retention drive adaptation"
  :key-insight "What survives is what's fit for the environment"
  :application "Create variation, let selection work, retain what works"
  :failure-modes
  [(failure "variation-starvation" "high"
            "Not generating enough variation"
            :signals ["Homogeneous options" "No experimentation"]
            :safeguards ["Encourage diversity" "Experiment widely" "Tolerate failure"])
   (failure "selection-weakness" "high"
            "Not selecting rigorously enough"
            :signals ["Keeping everything" "No culling"]
            :safeguards ["Clear criteria" "Regular review" "Kill poor performers"])
   (failure "retention-failure" "high"
            "Not retaining what works"
            :signals ["Reinventing wheels" "Not documenting success"]
            :safeguards ["Document learnings" "Build on success" "Institutionalize"])
   (failure "environment-blindness" "high"
            "Not seeing environment changes"
            :signals ["Optimizing for past" "Missing shifts"]
            :safeguards ["Monitor environment" "Adapt continuously" "Stay flexible"])
   (failure "local-maximum-trap" "medium"
            "Getting stuck at local optimum"
            :signals ["Incremental improvement only" "Missing breakthroughs"]
            :safeguards ["Radical experiments" "Jump to new peaks" "Strategic pivots"])]})

(register-model
 {:name "red-queen"
  :category "biology"
  :originator "Leigh Van Valen"
  :description "You must keep running just to stay in place"
  :key-insight "Competitors are also improving; standing still means falling behind"
  :application "Continuous improvement is necessary for survival"
  :failure-modes
  [(failure "complacency" "critical"
            "Thinking current position is secure"
            :signals ["Resting on laurels" "Ignoring competitors"]
            :safeguards ["Monitor competition" "Continuous improvement" "Paranoia"])
   (failure "exhaustion" "high"
            "Running too fast and burning out"
            :signals ["Unsustainable pace" "Quality decline"]
            :safeguards ["Sustainable pace" "Strategic rest" "Efficiency focus"])
   (failure "wrong-race" "high"
            "Running in the wrong direction"
            :signals ["Improving wrong things" "Misaligned effort"]
            :safeguards ["Validate direction" "Customer feedback" "Strategic review"])
   (failure "race-to-bottom" "medium"
            "Competing on dimensions that destroy value"
            :signals ["Price wars" "Feature bloat"]
            :safeguards ["Differentiate" "Create new value" "Avoid commoditization"])
   (failure "arms-race-blindness" "medium"
            "Not seeing when to exit the race"
            :signals ["Diminishing returns" "Pyrrhic victories"]
            :safeguards ["Know when to quit" "Find new games" "Strategic retreat"])]})

;; ============================================