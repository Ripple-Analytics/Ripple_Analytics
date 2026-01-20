(ns mental-models.models.systems
  "Mental Models - Systems Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Systems
;; ============================================

(register-model
 {:name "feedback-loops"
  :category "systems"
  :originator "Systems Theory"
  :description "Outputs of a system become inputs that affect future outputs"
  :key-insight "Small changes can compound into large effects"
  :application "Identify reinforcing and balancing loops"
  :failure-modes
  [(failure "loop-blindness" "high"
            "Not seeing feedback loops in systems"
            :signals ["Surprised by exponential growth/decay" "Linear thinking"]
            :safeguards ["Map system dynamics" "Look for delays" "Trace causality"])
   (failure "positive-loop-neglect" "high"
            "Missing reinforcing loops"
            :signals ["Underestimating growth" "Missing compounding effects"]
            :safeguards ["Look for virtuous/vicious cycles" "Compound thinking"])
   (failure "negative-loop-neglect" "medium"
            "Missing balancing loops"
            :signals ["Expecting unlimited growth" "Ignoring constraints"]
            :safeguards ["Identify limits" "Look for stabilizing forces"])
   (failure "delay-blindness" "high"
            "Not accounting for delays in feedback"
            :signals ["Impatience" "Overcorrection" "Oscillation"]
            :safeguards ["Map delays" "Patient observation" "Avoid overreaction"])
   (failure "intervention-backfire" "high"
            "Interventions that trigger opposing feedback"
            :signals ["Unintended consequences" "System resistance"]
            :safeguards ["Model interventions" "Small experiments" "Monitor effects"])]})

(register-model
 {:name "emergence"
  :category "systems"
  :originator "Systems Theory"
  :description "Complex behaviors arise from simple rules and interactions"
  :key-insight "The whole is greater than the sum of its parts"
  :application "Look for emergent properties in complex systems"
  :failure-modes
  [(failure "reductionism" "high"
            "Trying to understand systems only through parts"
            :signals ["Missing emergent properties" "Oversimplification"]
            :safeguards ["Study interactions" "Observe whole system" "Look for patterns"])
   (failure "emergence-mysticism" "medium"
            "Attributing too much to emergence"
            :signals ["Avoiding analysis" "Magical thinking"]
            :safeguards ["Ground in mechanisms" "Test predictions" "Seek explanations"])
   (failure "scale-blindness" "high"
            "Not seeing how properties change with scale"
            :signals ["Assuming linear scaling" "Missing phase transitions"]
            :safeguards ["Study at multiple scales" "Look for thresholds"])
   (failure "interaction-neglect" "high"
            "Ignoring interactions between components"
            :signals ["Isolated analysis" "Missing synergies/conflicts"]
            :safeguards ["Map relationships" "Study interfaces" "Network analysis"])
   (failure "prediction-overconfidence" "medium"
            "Thinking emergent systems are predictable"
            :signals ["Precise forecasts" "Ignoring uncertainty"]
            :safeguards ["Embrace uncertainty" "Scenario planning" "Adaptive strategies"])]})

(register-model
 {:name "network-effects"
  :category "systems"
  :originator "Economics/Technology"
  :description "Value increases as more people use a product or service"
  :key-insight "Networks can create winner-take-all dynamics"
  :application "Identify and leverage network effects"
  :failure-modes
  [(failure "network-blindness" "high"
            "Not recognizing network effects"
            :signals ["Undervaluing platforms" "Missing growth potential"]
            :safeguards ["Map network connections" "Study adoption curves"])
   (failure "network-overestimation" "medium"
            "Overestimating network effect strength"
            :signals ["Assuming all networks are equal" "Ignoring switching costs"]
            :safeguards ["Measure actual effects" "Compare to alternatives"])
   (failure "chicken-egg-paralysis" "high"
            "Unable to bootstrap network"
            :signals ["Waiting for critical mass" "No early adopter strategy"]
            :safeguards ["Seed the network" "Create initial value" "Subsidize early users"])
   (failure "network-fragility" "medium"
            "Not seeing how networks can collapse"
            :signals ["Assuming permanence" "Ignoring alternatives"]
            :safeguards ["Monitor engagement" "Maintain value" "Watch for substitutes"])
   (failure "negative-network-effects" "high"
            "Missing when more users reduce value"
            :signals ["Congestion" "Quality dilution" "Spam"]
            :safeguards ["Monitor quality" "Manage growth" "Curate community"])]})

;; ============================================