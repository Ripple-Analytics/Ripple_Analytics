(ns mental-models.models.ecology
  "Mental Models - Ecology Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Ecology
;; ============================================

(register-model
 {:name "carrying-capacity"
  :category "ecology"
  :originator "Ecology"
  :description "Maximum population an environment can sustain"
  :key-insight "Growth is limited by resources"
  :application "Identify and respect carrying capacity limits"
  :failure-modes
  [(failure "overshoot" "critical"
            "Exceeding carrying capacity"
            :signals ["Resource depletion" "Population crash"]
            :safeguards ["Monitor limits" "Sustainable growth"])
   (failure "capacity-blindness" "high"
            "Not seeing carrying capacity limits"
            :signals ["Assuming unlimited growth" "Ignoring constraints"]
            :safeguards ["Identify limits" "Plan for constraints"])
   (failure "static-capacity-assumption" "medium"
            "Assuming fixed carrying capacity"
            :signals ["Missing technology improvements" "Underestimating potential"]
            :safeguards ["Dynamic capacity" "Innovation"])
   (failure "local-vs-global" "medium"
            "Confusing local and global capacity"
            :signals ["Exporting problems" "Shifting burdens"]
            :safeguards ["System boundaries" "Full accounting"])
   (failure "capacity-gaming" "medium"
            "Manipulating capacity measures"
            :signals ["Goodhart's law" "Metric gaming"]
            :safeguards ["Multiple measures" "Outcome focus"])]})

(register-model
 {:name "niche"
  :category "ecology"
  :originator "Ecology"
  :description "The role and position of a species in its environment"
  :key-insight "Successful species find and defend their niche"
  :application "Find your niche; avoid direct competition"
  :failure-modes
  [(failure "niche-blindness" "high"
            "Not understanding your niche"
            :signals ["Competing everywhere" "No differentiation"]
            :safeguards ["Define niche clearly" "Specialize"])
   (failure "niche-overlap" "high"
            "Too much overlap with competitors"
            :signals ["Price wars" "Commoditization"]
            :safeguards ["Differentiate" "Find unique position"])
   (failure "niche-rigidity" "medium"
            "Unable to adapt niche"
            :signals ["Environment changing" "Niche disappearing"]
            :safeguards ["Monitor environment" "Adaptive capability"])
   (failure "niche-expansion-failure" "medium"
            "Unable to expand from niche"
            :signals ["Stuck in small market" "No growth path"]
            :safeguards ["Adjacent expansion" "Platform potential"])
   (failure "generalist-trap" "medium"
            "Trying to be everything to everyone"
            :signals ["No clear positioning" "Mediocre at everything"]
            :safeguards ["Focus" "Clear positioning"])]})

;; ============================================