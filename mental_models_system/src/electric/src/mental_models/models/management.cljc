(ns mental-models.models.management
  "Mental Models - Management Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Management
;; ============================================

(register-model
 {:name "principal-agent"
  :category "management"
  :originator "Economics"
  :description "Conflicts between those who delegate and those who act"
  :key-insight "Agents may not act in principals' best interests"
  :application "Align incentives; monitor appropriately"
  :failure-modes
  [(failure "misaligned-incentives" "high"
            "Agent incentives don't match principal goals"
            :signals ["Agent self-dealing" "Suboptimal outcomes"]
            :safeguards ["Align incentives" "Skin in game"])
   (failure "information-asymmetry" "high"
            "Agent knows more than principal"
            :signals ["Hidden actions" "Adverse selection"]
            :safeguards ["Monitoring" "Reporting requirements"])
   (failure "over-monitoring" "medium"
            "Too much oversight destroying trust"
            :signals ["Micromanagement" "Demotivation"]
            :safeguards ["Trust but verify" "Outcome focus"])
   (failure "under-monitoring" "high"
            "Insufficient oversight allowing abuse"
            :signals ["Agent misconduct" "Principal losses"]
            :safeguards ["Appropriate monitoring" "Audit"])
   (failure "agency-cost-blindness" "medium"
            "Not accounting for agency costs"
            :signals ["Underestimating true costs"]
            :safeguards ["Include agency costs" "Direct action when possible"])]})

(register-model
 {:name "span-of-control"
  :category "management"
  :originator "Management Theory"
  :description "Number of direct reports one manager can effectively supervise"
  :key-insight "There are limits to effective supervision"
  :application "Right-size teams for effective management"
  :failure-modes
  [(failure "too-wide-span" "high"
            "Too many direct reports"
            :signals ["Manager overwhelmed" "Insufficient attention"]
            :safeguards ["Limit direct reports" "Add management layers"])
   (failure "too-narrow-span" "medium"
            "Too few direct reports"
            :signals ["Micromanagement" "Too many layers"]
            :safeguards ["Expand span" "Flatten organization"])
   (failure "uniform-span" "medium"
            "Same span regardless of context"
            :signals ["Ignoring task complexity" "One size fits all"]
            :safeguards ["Context-appropriate span" "Vary by role"])
   (failure "span-rigidity" "medium"
            "Not adjusting span as needs change"
            :signals ["Outdated structure" "Mismatched capacity"]
            :safeguards ["Regular review" "Adaptive structure"])
   (failure "informal-span-blindness" "medium"
            "Ignoring informal reporting relationships"
            :signals ["Hidden workload" "Unofficial reports"]
            :safeguards ["Map actual relationships" "Formalize if needed"])]})

(register-model
 {:name "peter-principle"
  :category "management"
  :originator "Laurence Peter"
  :description "People rise to their level of incompetence"
  :key-insight "Promotion based on current role doesn't predict next role success"
  :application "Promote based on next role requirements"
  :failure-modes
  [(failure "competence-promotion" "high"
            "Promoting based only on current competence"
            :signals ["Great individual contributors becoming poor managers"]
            :safeguards ["Assess for next role" "Different tracks"])
   (failure "no-demotion-path" "high"
            "No way to recover from over-promotion"
            :signals ["Stuck at incompetence" "No graceful exit"]
            :safeguards ["Lateral moves" "Graceful demotion"])
   (failure "promotion-as-reward" "medium"
            "Using promotion as only reward"
            :signals ["Forcing management on non-managers"]
            :safeguards ["Multiple reward paths" "IC tracks"])
   (failure "competence-ceiling-blindness" "medium"
            "Not seeing when someone has peaked"
            :signals ["Continued promotion attempts" "Repeated failures"]
            :safeguards ["Honest assessment" "Right role matching"])
   (failure "self-peter-principle" "medium"
            "Seeking promotion beyond competence"
            :signals ["Chasing title over fit"]
            :safeguards ["Self-awareness" "Role fit focus"])]})

;; ============================================