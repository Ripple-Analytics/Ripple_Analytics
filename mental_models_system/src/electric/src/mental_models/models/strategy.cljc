(ns mental-models.models.strategy
  "Mental Models - Strategy Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Strategy
;; ============================================

(register-model
 {:name "competitive-advantage"
  :category "strategy"
  :originator "Michael Porter"
  :description "Sustainable advantages that allow outperformance"
  :key-insight "Without advantage, competition erodes profits to zero"
  :application "Identify and protect sources of competitive advantage"
  :failure-modes
  [(failure "advantage-blindness" "high"
            "Not knowing your actual advantages"
            :signals ["Can't articulate why customers choose you"]
            :safeguards ["Customer research" "Competitive analysis"])
   (failure "temporary-advantage-confusion" "high"
            "Mistaking temporary for sustainable advantage"
            :signals ["Advantages being copied" "Eroding margins"]
            :safeguards ["Test durability" "Build moats"])
   (failure "advantage-erosion" "high"
            "Not maintaining advantages"
            :signals ["Competitors catching up" "Complacency"]
            :safeguards ["Continuous investment" "Innovation"])
   (failure "wrong-advantage" "medium"
            "Building advantage customers don't value"
            :signals ["Features nobody wants" "Overengineering"]
            :safeguards ["Customer validation" "Willingness to pay"])
   (failure "single-advantage-risk" "high"
            "Relying on one source of advantage"
            :signals ["Single point of failure"]
            :safeguards ["Multiple moats" "Diversification"])]})

(register-model
 {:name "moats"
  :category "strategy"
  :originator "Warren Buffett"
  :description "Structural barriers that protect competitive position"
  :key-insight "Wide moats allow sustained high returns"
  :application "Build and widen moats around your business"
  :failure-modes
  [(failure "moat-illusion" "high"
            "Believing you have a moat when you don't"
            :signals ["Easy competitor entry" "No pricing power"]
            :safeguards ["Test moat strength" "Competitive analysis"])
   (failure "moat-neglect" "high"
            "Not investing in moat maintenance"
            :signals ["Moat narrowing" "Competitors gaining"]
            :safeguards ["Continuous investment" "Monitor moat width"])
   (failure "wrong-moat-type" "medium"
            "Building the wrong type of moat"
            :signals ["Moat doesn't match business model"]
            :safeguards ["Match moat to strategy" "Study successful moats"])
   (failure "moat-obsolescence" "high"
            "Moat becoming irrelevant"
            :signals ["Technology shifts" "Market changes"]
            :safeguards ["Monitor disruption" "Adapt moat"])
   (failure "moat-overconfidence" "medium"
            "Overestimating moat durability"
            :signals ["Complacency" "Ignoring threats"]
            :safeguards ["Stress test moat" "Paranoid planning"])]})

(register-model
 {:name "first-mover-advantage"
  :category "strategy"
  :originator "Business Strategy"
  :description "Benefits from being first to market"
  :key-insight "First mover advantage is real but often overestimated"
  :application "Evaluate whether speed or learning matters more"
  :failure-modes
  [(failure "first-mover-obsession" "high"
            "Overvaluing being first"
            :signals ["Rushing to market" "Sacrificing quality for speed"]
            :safeguards ["Evaluate actual advantage" "Consider fast follower"])
   (failure "fast-follower-blindness" "medium"
            "Not seeing fast follower advantages"
            :signals ["Ignoring learning from pioneers" "Not studying failures"]
            :safeguards ["Learn from first movers" "Improve on their mistakes"])
   (failure "timing-errors" "high"
            "Misjudging market timing"
            :signals ["Too early" "Too late"]
            :safeguards ["Market readiness research" "Timing analysis"])
   (failure "resource-exhaustion" "high"
            "Depleting resources before market matures"
            :signals ["Running out of runway" "Market slower than expected"]
            :safeguards ["Capital efficiency" "Staged investment"])
   (failure "category-creation-cost" "medium"
            "Underestimating cost of educating market"
            :signals ["High customer acquisition cost" "Slow adoption"]
            :safeguards ["Market education budget" "Patience"])]})

(register-model
 {:name "game-theory"
  :category "strategy"
  :originator "John von Neumann"
  :description "Strategic interaction where outcomes depend on others' choices"
  :key-insight "Consider how others will respond to your actions"
  :application "Think through the game before making moves"
  :failure-modes
  [(failure "single-player-thinking" "high"
            "Not considering others' responses"
            :signals ["Surprised by reactions" "Ignoring competitors"]
            :safeguards ["Map all players" "Anticipate responses"])
   (failure "zero-sum-assumption" "medium"
            "Assuming all games are zero-sum"
            :signals ["Missing win-win opportunities" "Unnecessary conflict"]
            :safeguards ["Look for positive-sum games" "Expand the pie"])
   (failure "rationality-assumption" "high"
            "Assuming all players are rational"
            :signals ["Surprised by irrational behavior"]
            :safeguards ["Model bounded rationality" "Consider emotions"])
   (failure "one-shot-thinking" "high"
            "Not considering repeated game dynamics"
            :signals ["Defecting when cooperation better" "Short-term focus"]
            :safeguards ["Consider reputation" "Long-term relationships"])
   (failure "information-asymmetry-blindness" "medium"
            "Ignoring differences in information"
            :signals ["Assuming others know what you know"]
            :safeguards ["Map information asymmetries" "Signal and screen"])]})

(register-model
 {:name "optionality"
  :category "strategy"
  :originator "Nassim Taleb"
  :description "The value of having choices without obligation"
  :key-insight "Options have value even if never exercised"
  :application "Create and preserve options; avoid irreversible decisions"
  :failure-modes
  [(failure "option-blindness" "high"
            "Not seeing embedded options"
            :signals ["Undervaluing flexibility" "Ignoring optionality"]
            :safeguards ["Identify all options" "Value flexibility"])
   (failure "option-destruction" "high"
            "Unnecessarily eliminating options"
            :signals ["Premature commitment" "Burning bridges"]
            :safeguards ["Preserve options" "Delay irreversible decisions"])
   (failure "option-hoarding" "medium"
            "Never exercising options"
            :signals ["Paralysis" "Endless optionality seeking"]
            :safeguards ["Set exercise criteria" "Time limits"])
   (failure "option-cost-blindness" "medium"
            "Ignoring the cost of maintaining options"
            :signals ["Too many options" "Resource drain"]
            :safeguards ["Prune options" "Cost-benefit analysis"])
   (failure "false-optionality" "high"
            "Believing you have options you don't"
            :signals ["Options that can't be exercised"]
            :safeguards ["Verify options are real" "Test exercisability"])]})

;; ============================================