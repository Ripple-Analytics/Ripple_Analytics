(ns mental-models.models.complexity-part4
  "Mental Models - Complexity Category (Part 4)"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; ============================================
;; Category: Complexity (Part 4)
;; ============================================

(register-model
 {:name "signal-vs-noise"
  :category "information-theory"
  :originator "Claude Shannon"
  :description "Distinguishing meaningful information from random variation"
  :key-insight "Most data is noise; finding signal requires filtering"
  :application "Develop filters; focus on signal; ignore noise"
  :failure-modes
  [(failure "noise-as-signal" "high"
            "Treating random variation as meaningful"
            :signals ["Overreaction" "False patterns"]
            :safeguards ["Statistical significance" "Base rates"])
   (failure "signal-as-noise" "high"
            "Dismissing real signals as noise"
            :signals ["Missed opportunities" "Ignored warnings"]
            :safeguards ["Anomaly investigation" "Open mind"])
   (failure "filter-failure" "medium"
            "Inadequate filtering mechanisms"
            :signals ["Information overload" "Poor decisions"]
            :safeguards ["Better filters" "Curated sources"])
   (failure "overfitting" "high"
            "Finding patterns in noise"
            :signals ["Model works on past, fails on future"]
            :safeguards ["Out-of-sample testing" "Simplicity"])
   (failure "confirmation-filtering" "high"
            "Filtering based on beliefs not validity"
            :signals ["Echo chamber" "Missed disconfirming"]
            :safeguards ["Diverse sources" "Seek disconfirmation"])]})

(register-model
 {:name "information-asymmetry"
  :category "information-theory"
  :originator "George Akerlof"
  :description "When one party has more or better information than another"
  :key-insight "Information gaps create market failures and exploitation"
  :application "Identify asymmetries; signal quality; seek information"
  :failure-modes
  [(failure "adverse-selection" "high"
            "Bad actors exploiting information advantage"
            :signals ["Market for lemons" "Quality decline"]
            :safeguards ["Signaling" "Screening" "Warranties"])
   (failure "moral-hazard" "high"
            "Changed behavior due to information gap"
            :signals ["Hidden actions" "Risk taking"]
            :safeguards ["Monitoring" "Incentive alignment"])
   (failure "signaling-failure" "medium"
            "Unable to credibly signal quality"
            :signals ["Pooling with low quality" "Undervaluation"]
            :safeguards ["Costly signals" "Reputation"])
   (failure "information-hoarding" "medium"
            "Keeping information for advantage"
            :signals ["Distrust" "Inefficiency"]
            :safeguards ["Transparency incentives" "Information sharing"])
   (failure "asymmetry-blindness" "high"
            "Not recognizing information gaps"
            :signals ["Naive trust" "Exploitation"]
            :safeguards ["Assume asymmetry" "Due diligence"])]})

;; ---- GAME THEORY EXTENSIONS ----

(register-model
 {:name "nash-equilibrium"
  :category "game-theory"
  :originator "John Nash"
  :description "A state where no player can benefit by changing strategy unilaterally"
  :key-insight "Equilibria can be suboptimal for all players"
  :application "Identify equilibria; design mechanisms for better outcomes"
  :failure-modes
  [(failure "equilibrium-trap" "high"
            "Stuck in suboptimal equilibrium"
            :signals ["Everyone worse off" "No one moves"]
            :safeguards ["Coordination" "Mechanism design"])
   (failure "multiple-equilibria" "medium"
            "Not seeing alternative equilibria"
            :signals ["Stuck in one" "Better possible"]
            :safeguards ["Search for alternatives" "Focal points"])
   (failure "equilibrium-assumption" "medium"
            "Assuming system is in equilibrium"
            :signals ["Missing dynamics" "Transition effects"]
            :safeguards ["Check stability" "Dynamic analysis"])
   (failure "rationality-assumption" "high"
            "Assuming all players are rational"
            :signals ["Unexpected behavior" "Model failure"]
            :safeguards ["Bounded rationality" "Behavioral factors"])
   (failure "static-game-thinking" "medium"
            "Ignoring repeated game dynamics"
            :signals ["Missing reputation" "Cooperation failure"]
            :safeguards ["Consider repetition" "Long-term thinking"])]})

(register-model
 {:name "mechanism-design"
  :category "game-theory"
  :originator "Leonid Hurwicz"
  :description "Designing rules and incentives to achieve desired outcomes"
  :key-insight "You can design the game, not just play it"
  :application "Design incentives that align individual and collective interests"
  :failure-modes
  [(failure "incentive-misalignment" "high"
            "Incentives don't produce desired behavior"
            :signals ["Gaming" "Unintended consequences"]
            :safeguards ["Test incentives" "Iterate design"])
   (failure "gaming-blindness" "high"
            "Not anticipating how rules will be gamed"
            :signals ["Exploitation" "Letter vs spirit"]
            :safeguards ["Adversarial thinking" "Robust design"])
   (failure "complexity-failure" "medium"
            "Mechanism too complex to work"
            :signals ["Confusion" "Non-participation"]
            :safeguards ["Simplicity" "Clear rules"])
   (failure "participation-constraint" "medium"
            "People opt out of mechanism"
            :signals ["Low adoption" "Selection effects"]
            :safeguards ["Attractive participation" "Default enrollment"])
   (failure "information-requirements" "medium"
            "Mechanism requires unavailable information"
            :signals ["Implementation failure" "Manipulation"]
            :safeguards ["Realistic information" "Robust to uncertainty"])]})

(register-model
 {:name "coordination-games"
  :category "game-theory"
  :originator "Thomas Schelling"
  :description "Situations where players benefit from making the same choice"
  :key-insight "Focal points enable coordination without communication"
  :application "Create focal points; establish conventions; coordinate"
  :failure-modes
  [(failure "coordination-failure" "high"
            "Unable to coordinate on same choice"
            :signals ["Mismatched actions" "Missed opportunities"]
            :safeguards ["Communication" "Focal points"])
   (failure "wrong-focal-point" "medium"
            "Coordinating on suboptimal equilibrium"
            :signals ["Everyone on same bad choice"]
            :safeguards ["Evaluate focal points" "Better alternatives"])
   (failure "focal-point-blindness" "medium"
            "Not seeing obvious coordination point"
            :signals ["Unnecessary complexity" "Failed coordination"]
            :safeguards ["Look for obvious" "Cultural awareness"])
   (failure "convention-lock-in" "medium"
            "Stuck with outdated convention"
            :signals ["Better alternatives exist" "Switching costs"]
            :safeguards ["Periodic review" "Coordinated switching"])
   (failure "communication-assumption" "medium"
            "Assuming coordination requires communication"
            :signals ["Missed silent coordination"]
            :safeguards ["Focal point thinking" "Common knowledge"])]})

;; ---- EPISTEMOLOGY ----