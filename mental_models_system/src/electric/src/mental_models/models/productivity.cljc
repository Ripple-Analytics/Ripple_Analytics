(ns mental-models.models.productivity
  "Mental Models - Productivity Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Productivity
;; ============================================

(register-model
 {:name "leverage"
  :category "productivity"
  :originator "Archimedes"
  :description "Small inputs can produce large outputs with the right lever"
  :key-insight "Give me a lever long enough and I can move the world"
  :application "Find and use leverage points in any system"
  :failure-modes
  [(failure "leverage-blindness" "high"
            "Not seeing leverage opportunities"
            :signals ["Brute force approaches" "Linear effort-result"]
            :safeguards ["Look for leverage points" "Study system dynamics"])
   (failure "wrong-lever" "high"
            "Pulling the wrong lever"
            :signals ["Effort without results" "Unintended consequences"]
            :safeguards ["Test levers" "Understand system"])
   (failure "over-leverage" "critical"
            "Too much leverage creating fragility"
            :signals ["Small changes cause big problems" "Instability"]
            :safeguards ["Limit leverage" "Build in buffers"])
   (failure "leverage-addiction" "medium"
            "Always seeking leverage over direct action"
            :signals ["Avoiding necessary work" "Shortcut seeking"]
            :safeguards ["Sometimes direct action is best" "Balance"])
   (failure "leverage-decay" "medium"
            "Leverage points changing over time"
            :signals ["Diminishing returns" "Old levers not working"]
            :safeguards ["Monitor effectiveness" "Find new levers"])]})

(register-model
 {:name "parkinson-law"
  :category "productivity"
  :originator "Cyril Parkinson"
  :description "Work expands to fill the time available"
  :key-insight "Deadlines create focus; unlimited time creates waste"
  :application "Set aggressive but realistic deadlines"
  :failure-modes
  [(failure "no-deadlines" "high"
            "Not setting deadlines"
            :signals ["Projects dragging on" "No urgency"]
            :safeguards ["Set deadlines" "Time-box work"])
   (failure "unrealistic-deadlines" "high"
            "Setting impossible deadlines"
            :signals ["Burnout" "Quality problems" "Missed deadlines"]
            :safeguards ["Realistic estimation" "Buffer time"])
   (failure "deadline-gaming" "medium"
            "Padding estimates to meet deadlines"
            :signals ["Sandbagging" "Slow delivery"]
            :safeguards ["Track actual vs estimated" "Accountability"])
   (failure "artificial-urgency" "medium"
            "Creating false urgency"
            :signals ["Everything is urgent" "Urgency fatigue"]
            :safeguards ["Prioritize genuinely" "Protect focus time"])
   (failure "quality-sacrifice" "high"
            "Meeting deadlines by cutting quality"
            :signals ["Technical debt" "Rework needed"]
            :safeguards ["Quality standards" "Scope flexibility"])]})

(register-model
 {:name "eisenhower-matrix"
  :category "productivity"
  :originator "Dwight Eisenhower"
  :description "Prioritize by urgency and importance"
  :key-insight "What is important is seldom urgent; what is urgent is seldom important"
  :application "Focus on important non-urgent; delegate or eliminate the rest"
  :failure-modes
  [(failure "urgency-addiction" "high"
            "Always responding to urgent over important"
            :signals ["Reactive mode" "No strategic work"]
            :safeguards ["Schedule important work" "Protect time"])
   (failure "importance-confusion" "high"
            "Misjudging what's truly important"
            :signals ["Busy but not productive" "Wrong priorities"]
            :safeguards ["Clarify goals" "Regular review"])
   (failure "delegation-failure" "medium"
            "Not delegating delegatable tasks"
            :signals ["Doing everything yourself" "Bottleneck"]
            :safeguards ["Build team" "Trust others"])
   (failure "elimination-fear" "medium"
            "Not eliminating unimportant tasks"
            :signals ["Overcommitment" "Saying yes to everything"]
            :safeguards ["Say no" "Ruthless elimination"])
   (failure "quadrant-rigidity" "low"
            "Over-categorizing tasks"
            :signals ["Analysis paralysis" "Categorization overhead"]
            :safeguards ["Quick categorization" "Action bias"])]})

;; ============================================