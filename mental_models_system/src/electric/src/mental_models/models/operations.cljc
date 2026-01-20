(ns mental-models.models.operations
  "Mental Models - Operations Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Operations
;; ============================================

(register-model
 {:name "bottleneck"
  :category "operations"
  :originator "Eliyahu Goldratt"
  :description "System throughput is limited by its constraint"
  :key-insight "Improving non-bottlenecks doesn't improve the system"
  :application "Identify and focus on the constraint"
  :failure-modes
  [(failure "bottleneck-blindness" "high"
            "Not identifying the true bottleneck"
            :signals ["Improving wrong things" "No system improvement"]
            :safeguards ["Map the system" "Measure throughput"])
   (failure "local-optimization" "high"
            "Optimizing non-bottlenecks"
            :signals ["Wasted effort" "No overall improvement"]
            :safeguards ["System view" "Focus on constraint"])
   (failure "bottleneck-starvation" "high"
            "Not feeding the bottleneck"
            :signals ["Bottleneck idle" "Upstream problems"]
            :safeguards ["Buffer before bottleneck" "Prioritize flow"])
   (failure "bottleneck-shift" "medium"
            "Not seeing when bottleneck moves"
            :signals ["New constraint emerging" "Old solutions not working"]
            :safeguards ["Monitor continuously" "Expect shifts"])
   (failure "constraint-elevation-failure" "medium"
            "Not elevating the constraint"
            :signals ["Same bottleneck forever" "No capacity increase"]
            :safeguards ["Invest in constraint" "Systematic improvement"])]})

(register-model
 {:name "redundancy"
  :category "operations"
  :originator "Engineering"
  :description "Backup systems that activate when primary fails"
  :key-insight "Redundancy trades efficiency for reliability"
  :application "Build redundancy for critical systems"
  :failure-modes
  [(failure "single-point-of-failure" "critical"
            "No redundancy for critical components"
            :signals ["System down when one part fails"]
            :safeguards ["Identify critical paths" "Add backups"])
   (failure "correlated-failure" "critical"
            "Redundant systems failing together"
            :signals ["Common cause failures" "Simultaneous outages"]
            :safeguards ["Independent systems" "Diverse redundancy"])
   (failure "redundancy-neglect" "high"
            "Not maintaining redundant systems"
            :signals ["Backup systems not working when needed"]
            :safeguards ["Regular testing" "Maintenance"])
   (failure "over-redundancy" "medium"
            "Too much redundancy creating complexity"
            :signals ["High costs" "Complexity failures"]
            :safeguards ["Right-size redundancy" "Cost-benefit analysis"])
   (failure "false-redundancy" "high"
            "Believing you have redundancy when you don't"
            :signals ["Hidden dependencies" "Shared failure modes"]
            :safeguards ["Test failures" "Map dependencies"])]})

(register-model
 {:name "queuing-theory"
  :category "operations"
  :originator "Agner Krarup Erlang"
  :description "Mathematical study of waiting lines"
  :key-insight "Utilization approaching 100% causes exponential wait times"
  :application "Maintain slack capacity to avoid queue explosions"
  :failure-modes
  [(failure "high-utilization-target" "high"
            "Targeting 100% utilization"
            :signals ["Long queues" "Unpredictable delays"]
            :safeguards ["Target 70-80% utilization" "Maintain slack"])
   (failure "variability-blindness" "high"
            "Ignoring variability in arrivals and service"
            :signals ["Queues despite low average utilization"]
            :safeguards ["Reduce variability" "Buffer for variation"])
   (failure "queue-blindness" "medium"
            "Not seeing hidden queues"
            :signals ["Work in progress piling up" "Long lead times"]
            :safeguards ["Visualize queues" "Measure WIP"])
   (failure "batch-size-errors" "medium"
            "Wrong batch sizes"
            :signals ["Either too much WIP or too much setup"]
            :safeguards ["Optimize batch size" "Reduce setup time"])
   (failure "priority-inversion" "medium"
            "Wrong prioritization in queues"
            :signals ["Important items waiting" "Gaming the system"]
            :safeguards ["Clear priority rules" "Regular review"])]})

;; ============================================