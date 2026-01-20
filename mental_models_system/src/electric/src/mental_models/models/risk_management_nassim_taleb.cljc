(ns mental-models.models.risk-management-nassim-taleb
  "Mental Models - Risk Management (Nassim Taleb) Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Risk Management (Nassim Taleb)
;; ============================================

(register-model
 {:name "antifragility"
  :category "risk_management"
  :originator "Nassim Nicholas Taleb"
  :description "Systems that benefit from volatility, stress, and disorder"
  :key-insight "Some systems get stronger from shocks; design for antifragility"
  :application "Build systems with optionality, redundancy, and stress benefits"
  :failure-modes
  [(failure "fragility-disguised" "critical"
            "System appears robust but is actually fragile"
            :signals ["Hidden dependencies" "Lack of stress testing"]
            :safeguards ["Stress testing" "Identify hidden risks" "Red team exercises"])
   (failure "excessive-optimization" "high"
            "Optimizing for efficiency removes antifragility"
            :signals ["No slack" "Just-in-time everything"]
            :safeguards ["Maintain redundancy" "Build buffers" "Preserve optionality"])
   (failure "linear-thinking" "high"
            "Assuming linear responses to stress"
            :signals ["Surprise failures" "Nonlinear losses"]
            :safeguards ["Nonlinear modeling" "Convexity analysis" "Tail risk awareness"])
   (failure "false-antifragility" "medium"
            "Confusing robustness with antifragility"
            :signals ["Surviving but not thriving" "No upside from volatility"]
            :safeguards ["Measure growth from stress" "Verify positive convexity" "Test upside"])
   (failure "antifragility-at-wrong-level" "high"
            "Individual antifragility at expense of system"
            :signals ["Moral hazard" "Externalized risks"]
            :safeguards ["Skin in the game" "Aligned incentives" "System-level thinking"])]})

(register-model
 {:name "barbell-strategy"
  :category "risk_management"
  :originator "Nassim Nicholas Taleb"
  :description "Combine extreme safety with extreme risk; avoid the middle"
  :key-insight "Bimodal exposure captures upside while limiting downside"
  :application "90% safe assets, 10% highly speculative; nothing in between"
  :failure-modes
  [(failure "wrong-proportions" "high"
            "Incorrect balance between safe and risky"
            :signals ["Too much risk" "Insufficient upside exposure"]
            :safeguards ["Risk budgeting" "Position sizing" "Regular rebalancing"])
   (failure "false-safety" "critical"
            "Safe side isn't actually safe"
            :signals ["Correlated failures" "Hidden risks"]
            :safeguards ["True diversification" "Stress testing" "Counterparty risk analysis"])
   (failure "insufficient-asymmetry" "high"
            "Risky side lacks positive asymmetry"
            :signals ["Limited upside" "Symmetric payoffs"]
            :safeguards ["Verify convexity" "Seek optionality" "Unlimited upside potential"])
   (failure "middle-creep" "medium"
            "Gradually adding middle-ground positions"
            :signals ["Moderate risk everywhere" "Lost barbell structure"]
            :safeguards ["Discipline" "Regular portfolio review" "Clear rules"])
   (failure "correlation-in-crisis" "critical"
            "Safe and risky sides correlate in crisis"
            :signals ["Both sides fail together" "No protection"]
            :safeguards ["True independence" "Crisis scenario testing" "Negative correlation verification"])]})

(register-model
 {:name "via-negativa"
  :category "decision_making"
  :originator "Nassim Nicholas Taleb"
  :description "Improve by removing bad elements rather than adding good ones"
  :key-insight "Subtraction is often more powerful than addition"
  :application "Identify what to stop doing before adding new initiatives"
  :failure-modes
  [(failure "addition-bias" "high"
            "Default to adding rather than subtracting"
            :signals ["Complexity growth" "Overload"]
            :safeguards ["Subtraction checklist" "Removal targets" "Simplicity metrics"])
   (failure "removing-good" "critical"
            "Subtracting essential elements"
            :signals ["System failure" "Missing capabilities"]
            :safeguards ["Impact analysis" "Dependency mapping" "Reversibility"])
   (failure "removal-paralysis" "medium"
            "Fear of removing anything"
            :signals ["Accumulation" "No pruning"]
            :safeguards ["Regular reviews" "Forced ranking" "Sunset clauses"])
   (failure "subtraction-without-understanding" "high"
            "Removing without knowing why it exists"
            :signals ["Unintended consequences" "System degradation"]
            :safeguards ["Chesterton's fence" "Historical analysis" "Stakeholder consultation"])
   (failure "endless-subtraction" "medium"
            "Removing too much, losing capability"
            :signals ["Insufficient resources" "Can't execute"]
            :safeguards ["Minimum viable system" "Core capability preservation" "Balance"])]})

(register-model
 {:name "skin-in-the-game"
  :category "incentive_design"
  :originator "Nassim Nicholas Taleb"
  :description "Decision makers must bear consequences of their decisions"
  :key-insight "Symmetry of risk and reward aligns incentives with outcomes"
  :application "Ensure advisors, leaders, and experts have downside exposure"
  :failure-modes
  [(failure "asymmetric-payoffs" "critical"
            "Upside for decision maker, downside for others"
            :signals ["Moral hazard" "Reckless decisions"]
            :safeguards ["Clawbacks" "Long-term incentives" "Personal liability"])
   (failure "insufficient-skin" "high"
            "Token exposure, not meaningful"
            :signals ["Symbolic stakes" "No real consequences"]
            :safeguards ["Material stakes" "Proportional exposure" "Verification"])
   (failure "excessive-skin" "medium"
            "Too much exposure causes paralysis"
            :signals ["Excessive caution" "No decisions"]
            :safeguards ["Balanced stakes" "Risk sharing" "Appropriate sizing"])
   (failure "delayed-consequences" "high"
            "Consequences come too late to matter"
            :signals ["Short-term thinking" "Time arbitrage"]
            :safeguards ["Immediate feedback" "Long vesting periods" "Deferred compensation"])
   (failure "externalized-risk" "critical"
            "Risks transferred to third parties"
            :signals ["Systemic risk" "Socialized losses"]
            :safeguards ["No bailouts" "Bankruptcy risk" "Personal accountability"])]})

(register-model
 {:name "lindy-effect"
  :category "forecasting"
  :originator "Nassim Nicholas Taleb"
  :description "Future life expectancy is proportional to current age for non-perishables"
  :key-insight "Things that have lasted tend to last longer; time is a filter"
  :application "Prefer time-tested ideas, technologies, and institutions"
  :failure-modes
  [(failure "lindy-misapplication" "high"
            "Applying Lindy to perishable items"
            :signals ["Assuming old people live forever" "Biological misapplication"]
            :safeguards ["Distinguish perishable from non-perishable" "Domain awareness" "Appropriate application"])
   (failure "innovation-resistance" "medium"
            "Rejecting all new ideas"
            :signals ["Stagnation" "Competitive disadvantage"]
            :safeguards ["Balance old and new" "Experimentation" "Optionality on new"])
   (failure "survivorship-bias" "high"
            "Only seeing what survived, not what died"
            :signals ["Overestimating durability" "Ignoring failures"]
            :safeguards ["Study failures" "Base rate analysis" "Selection bias awareness"])
   (failure "context-change" "high"
            "Environment changed, Lindy no longer applies"
            :signals ["Old solutions failing" "Regime shift"]
            :safeguards ["Context monitoring" "Adaptation" "Conditional Lindy"])
   (failure "lindy-as-excuse" "medium"
            "Using age to avoid improvement"
            :signals ["Complacency" "Resistance to change"]
            :safeguards ["Continuous improvement" "Test alternatives" "Evolution"])]})

;; ============================================