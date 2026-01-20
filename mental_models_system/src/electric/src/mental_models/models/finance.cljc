(ns mental-models.models.finance
  "Mental Models - Finance Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Finance
;; ============================================

(register-model
 {:name "time-value-of-money"
  :category "finance"
  :originator "Finance"
  :description "Money today is worth more than money tomorrow"
  :key-insight "Discount future cash flows to present value"
  :application "Always consider when money is received, not just how much"
  :failure-modes
  [(failure "present-bias" "high"
            "Overvaluing present over future"
            :signals ["Excessive discounting" "Short-term focus"]
            :safeguards ["Appropriate discount rates" "Long-term thinking"])
   (failure "future-bias" "medium"
            "Undervaluing present for future"
            :signals ["Excessive saving" "Missing current opportunities"]
            :safeguards ["Balance present and future" "Opportunity cost"])
   (failure "discount-rate-errors" "high"
            "Using wrong discount rate"
            :signals ["Mispricing" "Bad investment decisions"]
            :safeguards ["Appropriate risk adjustment" "Market rates"])
   (failure "nominal-real-confusion" "high"
            "Confusing nominal and real values"
            :signals ["Ignoring inflation" "Money illusion"]
            :safeguards ["Adjust for inflation" "Real returns focus"])
   (failure "certainty-assumption" "medium"
            "Treating future cash flows as certain"
            :signals ["No risk adjustment" "Overconfident projections"]
            :safeguards ["Risk-adjust" "Scenario analysis"])]})

(register-model
 {:name "asymmetric-information"
  :category "finance"
  :originator "George Akerlof"
  :description "One party has more information than another"
  :key-insight "Information asymmetry can cause market failures"
  :application "Identify who knows what; signal and screen appropriately"
  :failure-modes
  [(failure "adverse-selection" "high"
            "Bad actors driving out good due to information asymmetry"
            :signals ["Quality decline" "Market for lemons"]
            :safeguards ["Signaling" "Screening" "Warranties"])
   (failure "moral-hazard" "high"
            "Changed behavior when not observed"
            :signals ["Risk-taking when insured" "Principal-agent problems"]
            :safeguards ["Monitoring" "Incentive alignment" "Skin in game"])
   (failure "information-blindness" "high"
            "Not recognizing information asymmetry"
            :signals ["Assuming equal information" "Being taken advantage of"]
            :safeguards ["Map information gaps" "Due diligence"])
   (failure "over-signaling" "medium"
            "Costly signaling that destroys value"
            :signals ["Arms race" "Wasteful signaling"]
            :safeguards ["Efficient signals" "Pooling equilibria"])
   (failure "screening-failure" "medium"
            "Ineffective screening mechanisms"
            :signals ["Bad selection" "Screening costs exceed benefits"]
            :safeguards ["Effective screening" "Cost-benefit analysis"])]})

(register-model
 {:name "sunk-costs"
  :category "finance"
  :originator "Economics"
  :description "Costs already incurred that cannot be recovered"
  :key-insight "Sunk costs should not affect future decisions"
  :application "Ignore sunk costs; decide based on future costs and benefits"
  :failure-modes
  [(failure "sunk-cost-fallacy" "critical"
            "Letting sunk costs influence decisions"
            :signals ["Throwing good money after bad" "Can't quit"]
            :safeguards ["Ignore sunk costs" "Fresh evaluation"])
   (failure "premature-abandonment" "medium"
            "Quitting too early by ignoring sunk costs appropriately"
            :signals ["Not finishing valuable projects"]
            :safeguards ["Evaluate future value" "Completion benefits"])
   (failure "sunk-cost-blindness" "medium"
            "Not recognizing sunk costs"
            :signals ["Including sunk costs in analysis"]
            :safeguards ["Identify sunk costs" "Separate from future costs"])
   (failure "emotional-attachment" "high"
            "Emotional attachment to sunk investments"
            :signals ["Can't let go" "Identity tied to investment"]
            :safeguards ["Separate emotion from analysis" "Outside perspective"])
   (failure "organizational-sunk-costs" "high"
            "Organizations unable to abandon sunk costs"
            :signals ["Zombie projects" "Political protection"]
            :safeguards ["Kill criteria" "Regular portfolio review"])]})

;; ============================================