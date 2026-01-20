(ns mental-models.models.complexity-part6
  "Mental Models - Complexity Category (Part 6)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 6)
;; ============================================

(register-model
 {:name "parkinsons-law-bureaucracy"
  :category "organizational"
  :originator "C. Northcote Parkinson"
  :description "Bureaucracies expand regardless of work to be done"
  :key-insight "Organizations grow for internal reasons, not external needs"
  :application "Actively prune; question growth; maintain lean structure"
  :failure-modes
  [(failure "growth-assumption" "high"
            "Assuming growth is always good"
            :signals ["Headcount as success" "Empire building"]
            :safeguards ["Question growth" "Productivity focus"])
   (failure "bureaucracy-creep" "high"
            "Gradual addition of unnecessary process"
            :signals ["Slow decisions" "Process overhead"]
            :safeguards ["Regular pruning" "Process audits"])
   (failure "make-work" "medium"
            "Creating work to justify existence"
            :signals ["Busy but unproductive" "Low value work"]
            :safeguards ["Value focus" "Outcome measurement"])
   (failure "coordination-overhead" "high"
            "More people means more coordination"
            :signals ["Meetings multiply" "Communication overhead"]
            :safeguards ["Small teams" "Clear interfaces"])
   (failure "pruning-resistance" "medium"
            "Inability to shrink when needed"
            :signals ["Overstaffed" "Defensive behavior"]
            :safeguards ["Regular rightsizing" "Flexible structure"])]})

;; ---- EVOLUTION & ADAPTATION ----

(register-model
 {:name "fitness-landscape"
  :category "evolution"
  :originator "Sewall Wright"
  :description "A mapping of genotypes/strategies to fitness/success"
  :key-insight "Local optima can trap you; landscape changes over time"
  :application "Explore landscape; avoid local optima traps; adapt to changes"
  :failure-modes
  [(failure "local-optima-trap" "high"
            "Stuck at local peak, missing global optimum"
            :signals ["Good but not great" "Incremental only"]
            :safeguards ["Exploration" "Radical experiments"])
   (failure "landscape-blindness" "high"
            "Not seeing the fitness landscape"
            :signals ["Random search" "No strategy"]
            :safeguards ["Map landscape" "Understand structure"])
   (failure "static-landscape-assumption" "high"
            "Assuming landscape doesn't change"
            :signals ["Optimized for past" "Disrupted"]
            :safeguards ["Monitor changes" "Adaptive strategy"])
   (failure "peak-complacency" "medium"
            "Stopping exploration at current peak"
            :signals ["No innovation" "Vulnerability"]
            :safeguards ["Continuous exploration" "Optionality"])
   (failure "rugged-landscape-underestimation" "medium"
            "Assuming smooth landscape when rugged"
            :signals ["Gradient methods fail" "Unexpected valleys"]
            :safeguards ["Test assumptions" "Multiple approaches"])]})

(register-model
 {:name "punctuated-equilibrium"
  :category "evolution"
  :originator "Gould & Eldredge"
  :description "Long periods of stability punctuated by rapid change"
  :key-insight "Change is not gradual; prepare for sudden shifts"
  :application "Build resilience for punctuation; exploit stability periods"
  :failure-modes
  [(failure "gradualism-assumption" "high"
            "Expecting change to be slow and steady"
            :signals ["Surprised by rapid change" "Unprepared"]
            :safeguards ["Expect punctuation" "Build resilience"])
   (failure "stability-complacency" "high"
            "Assuming current stability will continue"
            :signals ["No preparation" "Fragility"]
            :safeguards ["Scenario planning" "Optionality"])
   (failure "punctuation-panic" "medium"
            "Overreacting to every change as punctuation"
            :signals ["False alarms" "Exhaustion"]
            :safeguards ["Distinguish signal" "Appropriate response"])
   (failure "timing-prediction" "high"
            "Trying to predict when punctuation will occur"
            :signals ["Failed predictions" "False confidence"]
            :safeguards ["Accept unpredictability" "Always prepared"])
   (failure "post-punctuation-rigidity" "medium"
            "Not adapting after punctuation"
            :signals ["Old strategies in new world"]
            :safeguards ["Rapid adaptation" "Learning orientation"])]})

;; ---- NETWORK SCIENCE ----

(register-model
 {:name "small-world-networks"
  :category "network-science"
  :originator "Duncan Watts"
  :description "Networks with high clustering and short path lengths"
  :key-insight "A few long-range connections dramatically reduce distances"
  :application "Build bridges; leverage weak ties; create shortcuts"
  :failure-modes
  [(failure "cluster-isolation" "high"
            "Staying within your cluster"
            :signals ["Echo chamber" "Limited reach"]
            :safeguards ["Bridge building" "Weak ties"])
   (failure "bridge-neglect" "medium"
            "Not maintaining long-range connections"
            :signals ["Network fragmentation" "Lost reach"]
            :safeguards ["Invest in bridges" "Diverse connections"])
   (failure "hub-dependence" "high"
            "Over-reliance on network hubs"
            :signals ["Single point of failure" "Hub removal catastrophic"]
            :safeguards ["Multiple paths" "Redundancy"])
   (failure "path-length-blindness" "medium"
            "Not seeing how close things are"
            :signals ["Missed connections" "Unnecessary intermediaries"]
            :safeguards ["Map network" "Find short paths"])
   (failure "clustering-excess" "medium"
            "Too much clustering, not enough bridging"
            :signals ["Insular groups" "Slow diffusion"]
            :safeguards ["Encourage bridging" "Cross-cluster links"])]})

(register-model
 {:name "preferential-attachment"
  :category "network-science"
  :originator "Barabási & Albert"
  :description "New connections prefer already well-connected nodes"
  :key-insight "Rich get richer; early advantage compounds"
  :application "Get connected early; leverage existing connections"
  :failure-modes
  [(failure "late-entry-disadvantage" "high"
            "Entering network late with few connections"
            :signals ["Slow growth" "Marginalization"]
            :safeguards ["Early entry" "Niche strategy"])
   (failure "hub-worship" "medium"
            "Only connecting to hubs"
            :signals ["Crowded access" "Ignored periphery"]
            :safeguards ["Diverse connections" "Emerging nodes"])
   (failure "attachment-blindness" "medium"
            "Not seeing preferential attachment dynamics"
            :signals ["Surprised by inequality" "Wrong strategy"]
            :safeguards ["Understand dynamics" "Strategic positioning"])
   (failure "winner-take-all-assumption" "medium"
            "Assuming only hubs matter"
            :signals ["Ignored niches" "Missed opportunities"]
            :safeguards ["Niche value" "Long tail"])
   (failure "connection-quality" "medium"
            "Focusing on quantity over quality"
            :signals ["Many weak connections" "No deep relationships"]
            :safeguards ["Quality connections" "Relationship depth"])]})

;; ---- RHETORIC & PERSUASION ----