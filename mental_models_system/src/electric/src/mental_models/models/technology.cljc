(ns mental-models.models.technology
  "Mental Models - Technology Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Technology
;; ============================================

(register-model
 {:name "moores-law"
  :category "technology"
  :originator "Gordon Moore"
  :description "Computing power doubles roughly every two years"
  :key-insight "Exponential improvement changes what's possible"
  :application "Plan for exponentially improving technology"
  :failure-modes
  [(failure "linear-tech-thinking" "high"
            "Assuming linear technology progress"
            :signals ["Underestimating future capabilities"]
            :safeguards ["Exponential thinking" "Scenario planning"])
   (failure "moores-law-everywhere" "medium"
            "Assuming all technology follows Moore's Law"
            :signals ["Expecting exponential in linear domains"]
            :safeguards ["Domain-specific analysis" "Check actual trends"])
   (failure "end-of-moores-law" "medium"
            "Not seeing when exponential trends end"
            :signals ["Physical limits" "Diminishing returns"]
            :safeguards ["Monitor actual progress" "Alternative paths"])
   (failure "waiting-for-tech" "medium"
            "Waiting for technology that may not come"
            :signals ["Paralysis" "Missing current opportunities"]
            :safeguards ["Use current tech" "Hedge bets"])
   (failure "tech-determinism" "medium"
            "Assuming technology determines outcomes"
            :signals ["Ignoring social factors" "Tech solutionism"]
            :safeguards ["Sociotechnical view" "Human factors"])]})

(register-model
 {:name "network-topology"
  :category "technology"
  :originator "Network Science"
  :description "The structure of connections determines system behavior"
  :key-insight "Hub-and-spoke vs mesh vs hierarchical have different properties"
  :application "Design network topology for desired properties"
  :failure-modes
  [(failure "topology-blindness" "high"
            "Not considering network structure"
            :signals ["Unexpected cascades" "Fragility"]
            :safeguards ["Map topology" "Analyze structure"])
   (failure "hub-vulnerability" "critical"
            "Critical hubs that can fail"
            :signals ["Single points of failure" "Cascade risk"]
            :safeguards ["Redundant hubs" "Distributed architecture"])
   (failure "over-connection" "medium"
            "Too many connections creating complexity"
            :signals ["Tight coupling" "Unexpected interactions"]
            :safeguards ["Loose coupling" "Modular design"])
   (failure "under-connection" "medium"
            "Too few connections limiting capability"
            :signals ["Isolated components" "No synergy"]
            :safeguards ["Strategic connections" "Integration"])
   (failure "static-topology-assumption" "medium"
            "Assuming topology doesn't change"
            :signals ["Outdated network maps" "Missing new connections"]
            :safeguards ["Dynamic monitoring" "Adaptive design"])]})

(register-model
 {:name "technical-debt"
  :category "technology"
  :originator "Ward Cunningham"
  :description "Shortcuts in code that must be paid back later"
  :key-insight "Debt compounds; pay it down or it will crush you"
  :application "Take on debt deliberately; pay it down systematically"
  :failure-modes
  [(failure "debt-accumulation" "high"
            "Accumulating too much technical debt"
            :signals ["Slowing velocity" "Increasing bugs"]
            :safeguards ["Regular paydown" "Debt limits"])
   (failure "debt-blindness" "high"
            "Not tracking technical debt"
            :signals ["Surprised by slowdowns" "Hidden complexity"]
            :safeguards ["Track debt" "Make visible"])
   (failure "no-debt-extremism" "medium"
            "Never taking on any debt"
            :signals ["Slow delivery" "Over-engineering"]
            :safeguards ["Strategic debt" "Speed vs quality tradeoffs"])
   (failure "wrong-debt" "high"
            "Taking on debt in wrong places"
            :signals ["Debt in critical paths" "High-interest debt"]
            :safeguards ["Deliberate debt placement" "Low-risk areas"])
   (failure "debt-denial" "high"
            "Pretending debt doesn't exist"
            :signals ["Velocity decline" "Team frustration"]
            :safeguards ["Acknowledge debt" "Plan paydown"])]})

;; ============================================