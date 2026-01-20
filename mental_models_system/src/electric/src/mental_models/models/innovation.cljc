(ns mental-models.models.innovation
  "Mental Models - Innovation Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Innovation
;; ============================================

(register-model
 {:name "creative-destruction"
  :category "innovation"
  :originator "Joseph Schumpeter"
  :description "Innovation destroys old industries while creating new ones"
  :key-insight "Capitalism's engine is the constant disruption of the status quo"
  :application "Expect and prepare for disruption; be the disruptor"
  :failure-modes
  [(failure "disruption-denial" "high"
            "Believing your industry won't be disrupted"
            :signals ["Complacency" "Ignoring new entrants"]
            :safeguards ["Monitor disruption" "Self-disrupt"])
   (failure "disruption-panic" "medium"
            "Overreacting to every potential disruption"
            :signals ["Chasing every trend" "No focus"]
            :safeguards ["Evaluate threats carefully" "Strategic patience"])
   (failure "incumbent-blindness" "high"
            "Not seeing threats from unexpected places"
            :signals ["Watching wrong competitors"]
            :safeguards ["Broad scanning" "Adjacent industries"])
   (failure "destruction-only-focus" "medium"
            "Focusing on destruction, not creation"
            :signals ["Defensive posture" "No innovation"]
            :safeguards ["Create new value" "Offensive strategy"])
   (failure "timing-errors" "high"
            "Misjudging disruption timing"
            :signals ["Too early or too late"]
            :safeguards ["Monitor adoption curves" "Staged response"])]})

(register-model
 {:name "s-curves"
  :category "innovation"
  :originator "Technology Adoption"
  :description "Technologies follow S-shaped adoption curves"
  :key-insight "Slow start, rapid growth, then saturation"
  :application "Identify where on the S-curve you are"
  :failure-modes
  [(failure "linear-projection" "high"
            "Projecting current growth rate forward"
            :signals ["Overestimating mature tech" "Underestimating early tech"]
            :safeguards ["S-curve awareness" "Inflection point identification"])
   (failure "curve-jumping-failure" "high"
            "Not jumping to next S-curve"
            :signals ["Riding curve to saturation" "Missing next wave"]
            :safeguards ["Monitor emerging curves" "Invest in next generation"])
   (failure "premature-jumping" "medium"
            "Jumping too early to new curve"
            :signals ["Abandoning profitable business" "New tech not ready"]
            :safeguards ["Timing analysis" "Staged transition"])
   (failure "saturation-blindness" "high"
            "Not seeing approaching saturation"
            :signals ["Expecting continued growth" "Overinvestment"]
            :safeguards ["Monitor growth rates" "Leading indicators"])
   (failure "single-curve-thinking" "medium"
            "Not seeing multiple overlapping curves"
            :signals ["Missing complexity" "Oversimplification"]
            :safeguards ["Map multiple curves" "System view"])]})

(register-model
 {:name "innovators-dilemma"
  :category "innovation"
  :originator "Clayton Christensen"
  :description "Successful companies fail by doing what made them successful"
  :key-insight "Listening to customers can lead to missing disruptive innovations"
  :application "Balance serving current customers with exploring disruption"
  :failure-modes
  [(failure "customer-obsession" "high"
            "Only listening to current customers"
            :signals ["Incremental improvements only" "Missing disruption"]
            :safeguards ["Study non-customers" "Explore low end"])
   (failure "margin-focus" "high"
            "Ignoring low-margin disruptive opportunities"
            :signals ["Ceding low end" "Disruption from below"]
            :safeguards ["Separate units" "Different metrics"])
   (failure "sustaining-innovation-bias" "medium"
            "Only investing in sustaining innovations"
            :signals ["Better but not different" "Same trajectory"]
            :safeguards ["Disruptive innovation portfolio" "Experimentation"])
   (failure "disruption-everywhere" "medium"
            "Seeing disruption where it isn't"
            :signals ["Overreacting to every new entrant"]
            :safeguards ["Evaluate disruption criteria" "Strategic patience"])
   (failure "organizational-antibodies" "high"
            "Organization killing disruptive efforts"
            :signals ["New initiatives failing" "Resource starvation"]
            :safeguards ["Separate organization" "Executive protection"])]})

;; ============================================