(ns mental-models.models.philosophy-part2
  "Mental Models - Philosophy Category (Part 2)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Philosophy (Part 2)
;; ============================================

(register-model
 {:name "dunning-kruger"
  :category "psychology"
  :originator "David Dunning & Justin Kruger"
  :description "Unskilled people overestimate ability; skilled people underestimate"
  :key-insight "You don't know what you don't know"
  :application "Seek feedback; calibrate confidence to competence"
  :failure-modes
  [(failure "overconfidence-incompetence" "high"
            "Confident despite lack of skill"
            :signals ["No self-doubt" "Dismissing experts"]
            :safeguards ["Seek feedback" "Track predictions"])
   (failure "underconfidence-competence" "medium"
            "Doubting despite high skill"
            :signals ["Imposter syndrome" "Not sharing expertise"]
            :safeguards ["Track successes" "External validation"])
   (failure "meta-dunning-kruger" "medium"
            "Thinking you're immune to the effect"
            :signals ["Overconfidence about self-awareness"]
            :safeguards ["Humility" "Continuous calibration"])
   (failure "expertise-blindness" "medium"
            "Experts forgetting what it's like to be a beginner"
            :signals ["Poor teaching" "Assuming knowledge"]
            :safeguards ["Beginner's mind" "Empathy"])
   (failure "calibration-neglect" "high"
            "Not calibrating confidence to accuracy"
            :signals ["Consistently over/under confident"]
            :safeguards ["Track predictions" "Feedback loops"])]})

(register-model
 {:name "status-quo-bias"
  :category "psychology"
  :originator "William Samuelson"
  :description "Preference for the current state of affairs"
  :key-insight "The default option has a powerful advantage"
  :application "Question defaults; consider change on its merits"
  :failure-modes
  [(failure "change-resistance" "high"
            "Resisting beneficial change"
            :signals ["Sticking with inferior options" "Fear of change"]
            :safeguards ["Evaluate options equally" "Consider opportunity cost"])
   (failure "default-acceptance" "high"
            "Accepting defaults without evaluation"
            :signals ["Not questioning" "Passive acceptance"]
            :safeguards ["Examine defaults" "Active choice"])
   (failure "change-for-change-sake" "medium"
            "Overcorrecting by always changing"
            :signals ["Constant churn" "No stability"]
            :safeguards ["Change when beneficial" "Stability value"])
   (failure "omission-bias" "medium"
            "Preferring harm from inaction over action"
            :signals ["Not acting when you should"]
            :safeguards ["Evaluate action and inaction equally"])
   (failure "endowment-effect" "medium"
            "Overvaluing what you have"
            :signals ["Demanding more to give up than to acquire"]
            :safeguards ["Ownership-blind evaluation"])]})

(register-model
 {:name "narrative-fallacy"
  :category "psychology"
  :originator "Nassim Taleb"
  :description "Creating stories to explain random events"
  :key-insight "We are storytelling animals, even when there's no story"
  :application "Be skeptical of neat narratives; embrace randomness"
  :failure-modes
  [(failure "pattern-imposition" "high"
            "Seeing patterns in randomness"
            :signals ["Explaining noise" "Overconfident causation"]
            :safeguards ["Statistical thinking" "Accept randomness"])
   (failure "story-over-data" "high"
            "Preferring good stories over good data"
            :signals ["Anecdotes over statistics" "Compelling but wrong"]
            :safeguards ["Data first" "Skepticism of stories"])
   (failure "hindsight-narratives" "high"
            "Creating stories after the fact"
            :signals ["Everything makes sense in retrospect"]
            :safeguards ["Pre-register predictions" "Acknowledge luck"])
   (failure "narrative-blindness" "medium"
            "Ignoring useful narratives"
            :signals ["Missing real patterns" "Over-skepticism"]
            :safeguards ["Balance" "Test narratives"])
   (failure "self-narrative-distortion" "medium"
            "Distorting personal history into neat story"
            :signals ["Coherent but false life story"]
            :safeguards ["Honest reflection" "Multiple perspectives"])]})

;; ============================================