(ns mental-models.models.military-warfare
  "Mental Models - Military/Warfare Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Military/Warfare
;; ============================================

(register-model
 {:name "fog-of-war"
  :category "military"
  :originator "Carl von Clausewitz"
  :description "Uncertainty and confusion in complex situations"
  :key-insight "Information is always incomplete and often wrong"
  :application "Plan for uncertainty; build adaptive systems"
  :failure-modes
  [(failure "certainty-illusion" "high"
            "Believing you have complete information"
            :signals ["Overconfident plans" "No contingencies"]
            :safeguards ["Assume incomplete info" "Multiple scenarios"])
   (failure "paralysis" "high"
            "Unable to act due to uncertainty"
            :signals ["Waiting for perfect information" "No decisions"]
            :safeguards ["Act on best available" "Iterate"])
   (failure "fog-denial" "high"
            "Pretending uncertainty doesn't exist"
            :signals ["Precise plans" "No buffers"]
            :safeguards ["Acknowledge uncertainty" "Build slack"])
   (failure "information-overload" "medium"
            "Too much information creating more fog"
            :signals ["Analysis paralysis" "Signal lost in noise"]
            :safeguards ["Focus on key signals" "Filter ruthlessly"])
   (failure "false-clarity" "high"
            "Mistaking noise for signal"
            :signals ["Acting on bad information" "Confident but wrong"]
            :safeguards ["Verify critical info" "Multiple sources"])]})

(register-model
 {:name "force-multiplier"
  :category "military"
  :originator "Military Strategy"
  :description "Factors that dramatically increase effectiveness"
  :key-insight "Small advantages can create disproportionate outcomes"
  :application "Identify and leverage force multipliers"
  :failure-modes
  [(failure "multiplier-blindness" "high"
            "Not seeing force multipliers"
            :signals ["Brute force approaches" "Linear thinking"]
            :safeguards ["Look for leverage" "Study asymmetric advantages"])
   (failure "multiplier-dependence" "high"
            "Over-relying on single multiplier"
            :signals ["Vulnerability if multiplier fails"]
            :safeguards ["Multiple multipliers" "Backup plans"])
   (failure "enemy-multipliers" "high"
            "Ignoring opponent's force multipliers"
            :signals ["Surprised by effectiveness"]
            :safeguards ["Analyze opponent capabilities" "Counter-multipliers"])
   (failure "multiplier-decay" "medium"
            "Multipliers losing effectiveness"
            :signals ["Diminishing returns" "Opponent adaptation"]
            :safeguards ["Monitor effectiveness" "Develop new multipliers"])
   (failure "false-multiplier" "medium"
            "Believing something is a multiplier when it isn't"
            :signals ["Investment without returns"]
            :safeguards ["Test multiplier effect" "Measure impact"])]})

(register-model
 {:name "schwerpunkt"
  :category "military"
  :originator "German Military Doctrine"
  :description "Concentration of force at the decisive point"
  :key-insight "Mass resources at the point of maximum impact"
  :application "Identify and concentrate on the decisive point"
  :failure-modes
  [(failure "diffusion" "high"
            "Spreading resources too thin"
            :signals ["Weak everywhere" "No decisive impact"]
            :safeguards ["Concentrate force" "Accept weakness elsewhere"])
   (failure "wrong-point" "critical"
            "Concentrating at the wrong point"
            :signals ["Wasted concentration" "Missing opportunity"]
            :safeguards ["Careful analysis" "Flexibility to shift"])
   (failure "static-schwerpunkt" "high"
            "Not shifting focus as situation changes"
            :signals ["Continuing past relevance"]
            :safeguards ["Dynamic assessment" "Willingness to shift"])
   (failure "over-concentration" "medium"
            "Too much concentration creating vulnerability"
            :signals ["Single point of failure" "Catastrophic if wrong"]
            :safeguards ["Hedged concentration" "Reserve force"])
   (failure "concentration-telegraph" "medium"
            "Revealing concentration to opponent"
            :signals ["Opponent counters" "Lost surprise"]
            :safeguards ["Deception" "Speed of execution"])]})

;; ============================================