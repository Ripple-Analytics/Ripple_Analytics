(ns mental-models.models.philosophy-part1
  "Mental Models - Philosophy Category (Part 1)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Philosophy (Part 1)
;; ============================================

(register-model
 {:name "stoicism"
  :category "philosophy"
  :originator "Epictetus/Marcus Aurelius"
  :description "Focus on what you can control; accept what you cannot"
  :key-insight "Tranquility comes from accepting reality"
  :application "Distinguish between what's in your control and what isn't"
  :failure-modes
  [(failure "control-confusion" "high"
            "Trying to control the uncontrollable"
            :signals ["Frustration" "Wasted effort"]
            :safeguards ["Dichotomy of control" "Accept reality"])
   (failure "passive-fatalism" "medium"
            "Not acting on what you can control"
            :signals ["Learned helplessness" "No agency"]
            :safeguards ["Act on controllables" "Take responsibility"])
   (failure "emotional-suppression" "medium"
            "Suppressing rather than processing emotions"
            :signals ["Emotional numbness" "Delayed reactions"]
            :safeguards ["Feel then act" "Healthy processing"])
   (failure "indifference-excess" "medium"
            "Becoming indifferent to everything"
            :signals ["Apathy" "No motivation"]
            :safeguards ["Preferred indifferents" "Values-driven action"])
   (failure "control-boundary-errors" "medium"
            "Misidentifying control boundaries"
            :signals ["Giving up too easily" "Overreaching"]
            :safeguards ["Careful analysis" "Test boundaries"])]})

(register-model
 {:name "via-negativa"
  :category "philosophy"
  :originator "Nassim Taleb"
  :description "Improvement through subtraction rather than addition"
  :key-insight "What you don't do matters more than what you do"
  :application "Remove negatives before adding positives"
  :failure-modes
  [(failure "addition-bias" "high"
            "Always adding rather than subtracting"
            :signals ["Complexity growth" "Feature bloat"]
            :safeguards ["Subtract first" "Simplify"])
   (failure "subtraction-paralysis" "medium"
            "Unable to remove anything"
            :signals ["Attachment to existing" "Fear of loss"]
            :safeguards ["Regular pruning" "Sunset policies"])
   (failure "wrong-subtraction" "medium"
            "Removing the wrong things"
            :signals ["Removing value" "Breaking systems"]
            :safeguards ["Careful analysis" "Reversible removal"])
   (failure "subtraction-extremism" "medium"
            "Subtracting too much"
            :signals ["Removing necessary elements" "Over-simplification"]
            :safeguards ["Balance" "Minimum viable"])
   (failure "addition-neglect" "medium"
            "Never adding when you should"
            :signals ["Missing opportunities" "Stagnation"]
            :safeguards ["Strategic addition" "Growth mindset"])]})

(register-model
 {:name "skin-in-the-game"
  :category "philosophy"
  :originator "Nassim Taleb"
  :description "Having personal stake in outcomes"
  :key-insight "Those who take risks should bear consequences"
  :application "Ensure decision-makers have skin in the game"
  :failure-modes
  [(failure "no-skin" "critical"
            "Making decisions without consequences"
            :signals ["Reckless advice" "Moral hazard"]
            :safeguards ["Require skin" "Align incentives"])
   (failure "asymmetric-skin" "high"
            "Upside without downside"
            :signals ["Heads I win, tails you lose"]
            :safeguards ["Symmetric exposure" "Clawbacks"])
   (failure "excessive-skin" "medium"
            "Too much personal risk"
            :signals ["Paralysis" "Excessive caution"]
            :safeguards ["Appropriate exposure" "Risk limits"])
   (failure "skin-measurement" "medium"
            "Wrong measure of skin"
            :signals ["Gaming metrics" "Wrong incentives"]
            :safeguards ["Outcome-based" "Long-term measures"])
   (failure "skin-transfer" "medium"
            "Transferring skin inappropriately"
            :signals ["Scapegoating" "Blame shifting"]
            :safeguards ["Clear accountability" "Fair attribution"])]})

;; ============================================
;; Additional Models for Completeness
;; ============================================

(register-model
 {:name "confirmation-bias"
  :category "psychology"
  :originator "Peter Wason"
  :description "Tendency to search for information that confirms existing beliefs"
  :key-insight "We see what we want to see"
  :application "Actively seek disconfirming evidence"
  :failure-modes
  [(failure "echo-chambers" "high"
            "Surrounding yourself with agreeing voices"
            :signals ["No disagreement" "Homogeneous sources"]
            :safeguards ["Diverse sources" "Seek disagreement"])
   (failure "selective-attention" "high"
            "Only noticing confirming information"
            :signals ["Missing contrary data" "Surprised by outcomes"]
            :safeguards ["Systematic data collection" "Pre-registration"])
   (failure "biased-interpretation" "high"
            "Interpreting ambiguous evidence as confirming"
            :signals ["Everything supports your view"]
            :safeguards ["Blind analysis" "Multiple interpretations"])
   (failure "memory-bias" "medium"
            "Remembering confirming evidence better"
            :signals ["Selective recall" "Distorted history"]
            :safeguards ["Written records" "Data over memory"])
   (failure "over-correction" "medium"
            "Overcorrecting and dismissing valid confirmation"
            :signals ["Excessive contrarianism"]
            :safeguards ["Calibrated skepticism" "Evidence-based"])]})

(register-model
 {:name "hindsight-bias"
  :category "psychology"
  :originator "Baruch Fischhoff"
  :description "Believing past events were predictable after knowing the outcome"
  :key-insight "We knew it all along - but we didn't"
  :application "Judge decisions by process, not outcome"
  :failure-modes
  [(failure "outcome-bias" "high"
            "Judging decisions by outcomes alone"
            :signals ["Punishing bad luck" "Rewarding good luck"]
            :safeguards ["Process evaluation" "Decision journals"])
   (failure "false-learning" "high"
            "Learning wrong lessons from outcomes"
            :signals ["Superstitious learning" "Wrong causation"]
            :safeguards ["Causal analysis" "Multiple cases"])
   (failure "overconfidence-in-prediction" "high"
            "Thinking future is as predictable as past seems"
            :signals ["Precise forecasts" "Ignoring uncertainty"]
            :safeguards ["Acknowledge uncertainty" "Scenario planning"])
   (failure "blame-attribution" "medium"
            "Unfairly blaming for unpredictable outcomes"
            :signals ["Scapegoating" "Unfair criticism"]
            :safeguards ["Fair evaluation" "Context consideration"])
   (failure "history-rewriting" "medium"
            "Rewriting history to match current beliefs"
            :signals ["Changing stories" "Inconsistent narratives"]
            :safeguards ["Written records" "Date-stamped predictions"])]})