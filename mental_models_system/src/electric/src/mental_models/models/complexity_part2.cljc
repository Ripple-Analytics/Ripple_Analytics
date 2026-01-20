(ns mental-models.models.complexity-part2
  "Mental Models - Complexity Category (Part 2)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 2)
;; ============================================

(register-model
 {:name "mental-accounting"
  :category "behavioral-economics"
  :originator "Richard Thaler"
  :description "People treat money differently based on subjective categories"
  :key-insight "Money is fungible but we don't treat it that way"
  :application "Recognize mental accounts; use them strategically"
  :failure-modes
  [(failure "sunk-cost-accounts" "high"
            "Continuing due to past investment in mental account"
            :signals ["Throwing good money after bad"]
            :safeguards ["Ignore sunk costs" "Fresh evaluation"])
   (failure "windfall-spending" "medium"
            "Treating unexpected money differently"
            :signals ["Splurging bonuses" "Lottery effect"]
            :safeguards ["All money is money" "Consistent rules"])
   (failure "budget-rigidity" "medium"
            "Refusing to move money between accounts"
            :signals ["Underspending in one area while overspending in another"]
            :safeguards ["Flexible budgeting" "Periodic rebalancing"])
   (failure "payment-decoupling" "medium"
            "Separating payment from consumption"
            :signals ["Credit card overspending" "Subscription creep"]
            :safeguards ["Link payment to consumption" "Regular review"])
   (failure "narrow-framing" "high"
            "Evaluating decisions in isolation"
            :signals ["Missing portfolio effects" "Suboptimal choices"]
            :safeguards ["Broad framing" "Portfolio view"])]})

;; ---- COGNITIVE SCIENCE ----

(register-model
 {:name "cognitive-load"
  :category "cognitive-science"
  :originator "John Sweller"
  :description "Working memory has limited capacity that affects learning and decision-making"
  :key-insight "Reduce extraneous load to improve performance"
  :application "Simplify information presentation; chunk complex tasks"
  :failure-modes
  [(failure "overload-blindness" "high"
            "Not recognizing when cognitive load is too high"
            :signals ["Errors increase" "Decision fatigue"]
            :safeguards ["Monitor load" "Take breaks"])
   (failure "complexity-addiction" "medium"
            "Adding unnecessary complexity"
            :signals ["Feature creep" "Information overload"]
            :safeguards ["Simplify ruthlessly" "Essential only"])
   (failure "chunking-failure" "medium"
            "Not breaking down complex information"
            :signals ["Overwhelm" "Poor retention"]
            :safeguards ["Chunk information" "Progressive disclosure"])
   (failure "multitasking-illusion" "high"
            "Believing you can process multiple streams"
            :signals ["Errors" "Slower performance"]
            :safeguards ["Single-tasking" "Sequential processing"])
   (failure "expertise-blindness" "medium"
            "Forgetting novice cognitive load"
            :signals ["Poor teaching" "Frustrated learners"]
            :safeguards ["Empathy for novices" "Scaffolding"])]})

(register-model
 {:name "dual-process-theory"
  :category "cognitive-science"
  :originator "Daniel Kahneman"
  :description "Two systems of thinking: fast/intuitive (System 1) and slow/deliberate (System 2)"
  :key-insight "Know when to trust intuition vs engage deliberate analysis"
  :application "Match thinking mode to task requirements"
  :failure-modes
  [(failure "system1-overreliance" "high"
            "Using intuition for analytical problems"
            :signals ["Quick but wrong" "Bias-driven errors"]
            :safeguards ["Engage System 2" "Slow down"])
   (failure "system2-overuse" "medium"
            "Overthinking simple decisions"
            :signals ["Analysis paralysis" "Exhaustion"]
            :safeguards ["Trust trained intuition" "Decision rules"])
   (failure "lazy-system2" "high"
            "System 2 accepting System 1 suggestions uncritically"
            :signals ["Rationalization" "Confirmation bias"]
            :safeguards ["Devil's advocate" "Challenge intuitions"])
   (failure "ego-depletion" "medium"
            "Running out of System 2 capacity"
            :signals ["Poor late-day decisions" "Willpower failure"]
            :safeguards ["Important decisions early" "Restore energy"])
   (failure "expertise-miscalibration" "high"
            "Wrong assessment of when intuition is valid"
            :signals ["Overconfident novice" "Underconfident expert"]
            :safeguards ["Domain-specific calibration" "Feedback loops"])]})

(register-model
 {:name "attention-economy"
  :category "cognitive-science"
  :originator "Herbert Simon"
  :description "Attention is the scarce resource in an information-rich world"
  :key-insight "What you attend to shapes your reality"
  :application "Guard attention fiercely; allocate it strategically"
  :failure-modes
  [(failure "attention-theft" "high"
            "Allowing others to capture your attention"
            :signals ["Constant interruptions" "Reactive mode"]
            :safeguards ["Attention boundaries" "Notification control"])
   (failure "attention-fragmentation" "high"
            "Splitting attention across too many things"
            :signals ["Shallow work" "No deep focus"]
            :safeguards ["Time blocking" "Single focus"])
   (failure "novelty-addiction" "medium"
            "Chasing new stimuli over important work"
            :signals ["Distraction" "Unfinished projects"]
            :safeguards ["Novelty diet" "Completion focus"])
   (failure "attention-residue" "medium"
            "Previous task consuming current attention"
            :signals ["Distracted" "Slow switching"]
            :safeguards ["Clean transitions" "Closure rituals"])
   (failure "inattentional-blindness" "high"
            "Missing important things due to focus elsewhere"
            :signals ["Surprised by obvious" "Tunnel vision"]
            :safeguards ["Periodic scanning" "Diverse attention"])]})

;; ---- SYSTEMS DYNAMICS ----

(register-model
 {:name "stocks-and-flows"
  :category "systems-dynamics"
  :originator "Jay Forrester"
  :description "Systems have accumulations (stocks) changed by rates (flows)"
  :key-insight "Stocks create delays and momentum in systems"
  :application "Identify stocks and flows; understand system dynamics"
  :failure-modes
  [(failure "flow-focus" "high"
            "Focusing on flows while ignoring stocks"
            :signals ["Missing accumulation effects" "Surprised by delays"]
            :safeguards ["Map stocks" "Track accumulations"])
   (failure "stock-blindness" "high"
            "Not seeing hidden stocks"
            :signals ["Unexpected system behavior" "Missing variables"]
            :safeguards ["Comprehensive mapping" "Look for accumulations"])
   (failure "delay-ignorance" "high"
            "Not accounting for stock-related delays"
            :signals ["Impatience" "Overcorrection"]
            :safeguards ["Model delays" "Patient adjustment"])
   (failure "bathtub-fallacy" "medium"
            "Confusing stocks and flows"
            :signals ["Wrong mental model" "Policy errors"]
            :safeguards ["Clear distinction" "Bathtub analogy"])
   (failure "equilibrium-assumption" "medium"
            "Assuming stocks are in equilibrium"
            :signals ["Missing dynamics" "Static thinking"]
            :safeguards ["Check for change" "Dynamic analysis"])]})