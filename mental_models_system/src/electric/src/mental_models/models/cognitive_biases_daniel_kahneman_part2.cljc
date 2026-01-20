(ns mental-models.models.cognitive-biases-daniel-kahneman-part2
  "Mental Models - Cognitive Biases (Daniel Kahneman) Category (Part 2)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Cognitive Biases (Daniel Kahneman) (Part 2)
;; ============================================

(register-model
 {:name "hormesis"
  :category "biology"
  :originator "Toxicology / Nassim Taleb"
  :description "Small doses of stress or toxins can be beneficial; what doesn't kill you makes you stronger"
  :key-insight "Moderate stressors trigger adaptive responses that make systems more robust"
  :application "Seek beneficial stressors; avoid both excessive stress and excessive comfort"
  :failure-modes
  [(failure "excessive-stress" "critical"
            "Applying too much stress, causing damage"
            :signals ["Burnout" "System breakdown" "Diminishing returns"]
            :safeguards ["Gradual increase" "Recovery periods" "Monitor stress levels"])
   (failure "insufficient-stress" "high"
            "Avoiding all stress, leading to fragility"
            :signals ["Atrophy" "Weakness" "Vulnerability"]
            :safeguards ["Regular challenges" "Controlled exposure" "Progressive overload"])
   (failure "wrong-type-stress" "high"
            "Applying stress that doesn't trigger adaptation"
            :signals ["No improvement" "Wasted effort" "Misdirected energy"]
            :safeguards ["Targeted stressors" "Specificity principle" "Feedback loops"])
   (failure "no-recovery" "critical"
            "Continuous stress without recovery"
            :signals ["Chronic stress" "No adaptation" "Declining performance"]
            :safeguards ["Rest periods" "Recovery protocols" "Periodization"])
   (failure "hormesis-denial" "medium"
            "Believing all stress is bad"
            :signals ["Overprotection" "Fragility" "Missed growth"]
            :safeguards ["Understand hormesis" "Embrace challenge" "Calculated risk"])]})

(register-model
 {:name "ergodicity"
  :category "probability"
  :originator "Ole Peters"
  :description "Time averages and ensemble averages are not always the same; what works for the group may not work for the individual"
  :key-insight "In non-ergodic systems, individual paths matter more than aggregate statistics"
  :application "Avoid ruin; focus on survival and sequential outcomes, not just expected value"
  :failure-modes
  [(failure "ensemble-fallacy" "critical"
            "Using ensemble statistics for individual decisions"
            :signals ["Ruin despite positive EV" "Bankruptcy" "Irreversible losses"]
            :safeguards ["Time average analysis" "Ruin probability" "Kelly criterion"])
   (failure "ignoring-path-dependence" "high"
            "Assuming order doesn't matter"
            :signals ["Sequence effects" "Unexpected outcomes" "Volatility drag"]
            :safeguards ["Path analysis" "Sequence testing" "Volatility consideration"])
   (failure "multiplicative-blindness" "critical"
            "Treating multiplicative processes as additive"
            :signals ["Compounding errors" "Exponential divergence" "Ruin"]
            :safeguards ["Multiplicative thinking" "Geometric mean" "Compounding awareness"])
   (failure "survival-neglect" "critical"
            "Ignoring survival constraints"
            :signals ["Gambler's ruin" "Blow-up risk" "No second chances"]
            :safeguards ["Survival first" "Never bet the farm" "Margin of safety"])
   (failure "ergodicity-assumption" "high"
            "Assuming all systems are ergodic"
            :signals ["Misapplied statistics" "Wrong models" "Prediction failures"]
            :safeguards ["Test ergodicity" "Understand system type" "Appropriate methods"])]})

(register-model
 {:name "reflexivity"
  :category "systems_thinking"
  :originator "George Soros"
  :description "Participants' biases influence events, which in turn influence participants' biases in self-reinforcing loops"
  :key-insight "Thinking and reality interact in feedback loops; perception shapes reality"
  :application "Identify self-reinforcing trends; recognize boom-bust cycles"
  :failure-modes
  [(failure "positive-feedback-blindness" "critical"
            "Missing self-reinforcing loops"
            :signals ["Bubble formation" "Unsustainable trends" "Sudden reversals"]
            :safeguards ["Feedback analysis" "Sustainability check" "Contrarian thinking"])
   (failure "reflexivity-denial" "high"
            "Believing markets are always efficient"
            :signals ["Surprised by bubbles" "Crashes" "Herd behavior"]
            :safeguards ["Soros framework" "Behavioral finance" "Sentiment analysis"])
   (failure "riding-bubble" "critical"
            "Staying in reflexive boom too long"
            :signals ["Peak euphoria" "This time is different" "Leverage"]
            :safeguards ["Exit strategy" "Contrarian signals" "Bubble indicators"])
   (failure "premature-reversal" "high"
            "Calling top/bottom too early"
            :signals ["Early exit" "Missed gains" "Wrong timing"]
            :safeguards ["Trend strength" "Momentum indicators" "Patience"])
   (failure "self-fulfilling-prophecy" "medium"
            "Creating the outcome you predict"
            :signals ["Confirmation bias" "Circular reasoning" "Reality distortion"]
            :safeguards ["Objective analysis" "External validation" "Falsification"])]})

(register-model
 {:name "convexity"
  :category "risk_management"
  :originator "Nassim Taleb"
  :description "Non-linear payoffs where gains and losses are asymmetric; more upside than downside"
  :key-insight "Convex strategies benefit from volatility and uncertainty"
  :application "Seek convex exposures; avoid concave risks"
  :failure-modes
  [(failure "concave-exposure" "critical"
            "Taking positions with limited upside, unlimited downside"
            :signals ["Selling options" "Picking up pennies" "Blow-up risk"]
            :safeguards ["Convexity analysis" "Payoff diagrams" "Tail risk assessment"])
   (failure "convexity-blindness" "high"
            "Not recognizing non-linearity"
            :signals ["Linear thinking" "Surprised by extremes" "Wrong risk assessment"]
            :safeguards ["Non-linear analysis" "Scenario testing" "Stress tests"])
   (failure "paying-too-much" "high"
            "Overpaying for convexity"
            :signals ["Expensive options" "Negative carry" "Poor risk-reward"]
            :safeguards ["Cost-benefit analysis" "Implied volatility" "Alternative strategies"])
   (failure "false-convexity" "high"
            "Believing exposure is convex when it's not"
            :signals ["Hidden risks" "Unexpected losses" "Misunderstood payoffs"]
            :safeguards ["Payoff analysis" "Stress scenarios" "Risk modeling"])
   (failure "volatility-aversion" "medium"
            "Avoiding volatility despite convexity"
            :signals ["Missed opportunities" "Volatility as enemy" "Stability bias"]
            :safeguards ["Volatility as friend" "Convex positioning" "Embrace uncertainty"])]})

(register-model
 {:name "optionality"
  :category "strategy"
  :originator "Nassim Taleb"
  :description "The right, but not the obligation, to take an action; asymmetric upside with limited downside"
  :key-insight "Options are valuable; keep doors open; avoid irreversible commitments"
  :application "Preserve optionality; make reversible decisions; avoid lock-in"
  :failure-modes
  [(failure "option-destruction" "critical"
            "Making irreversible commitments too early"
            :signals ["Locked in" "No flexibility" "Regret"]
            :safeguards ["Reversibility check" "Delay commitment" "Keep options open"])
   (failure "analysis-paralysis" "high"
            "Never exercising options due to fear"
            :signals ["Indecision" "Missed opportunities" "Option decay"]
            :safeguards ["Decision criteria" "Timing discipline" "Opportunity cost"])
   (failure "option-cost-blindness" "high"
            "Ignoring the cost of maintaining options"
            :signals ["Expensive flexibility" "Negative carry" "Wasted resources"]
            :safeguards ["Cost-benefit analysis" "Option value vs cost" "Prune dead options"])
   (failure "false-optionality" "high"
            "Believing you have options when you don't"
            :signals ["Illusory control" "Trapped" "Forced decisions"]
            :safeguards ["Reality check" "Test options" "Verify flexibility"])
   (failure "optionality-addiction" "medium"
            "Hoarding options without ever committing"
            :signals ["No progress" "Dilettante" "Jack of all trades"]
            :safeguards ["Strategic commitment" "Focus" "Exercise valuable options"])]})