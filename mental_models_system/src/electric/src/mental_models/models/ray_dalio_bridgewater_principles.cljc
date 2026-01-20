(ns mental-models.models.ray-dalio-bridgewater-principles
  "Mental Models - Ray Dalio / Bridgewater Principles Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Ray Dalio / Bridgewater Principles
;; ============================================

(register-model
 {:name "radical-transparency"
  :category "organizational_design"
  :originator "Ray Dalio / Bridgewater Associates"
  :description "Extreme openness and honesty in all communications; making all information accessible to all people; no hidden agendas"
  :key-insight "Truth and transparency accelerate learning and improvement; hidden information breeds politics and poor decisions"
  :application "Record all meetings; make all information accessible; encourage brutal honesty; eliminate politics; surface all disagreements"
  :failure-modes
  [(failure "transparency-overload" "high"
            "Too much information, causing paralysis"
            :signals ["Information overwhelm" "Decision paralysis" "Reduced productivity" "Meeting fatigue"]
            :safeguards ["Curated information" "Relevance filtering" "Summarization" "Need-to-know with access-to-all"])
   (failure "weaponized-transparency" "critical"
            "Using transparency to attack rather than improve"
            :signals ["Personal attacks" "Toxic culture" "Defensive behavior" "Attrition"]
            :safeguards ["Idea meritocracy" "Focus on ideas not people" "Psychological safety" "Constructive criticism"])
   (failure "false-transparency" "high"
            "Appearance of transparency without substance"
            :signals ["Selective disclosure" "Spin" "Hidden agendas persist" "Distrust"]
            :safeguards ["Complete transparency" "Verification" "Anonymous feedback" "Culture enforcement"])
   (failure "privacy-violation" "high"
            "Transparency violating legitimate privacy"
            :signals ["Personal information exposed" "Legal issues" "Attrition" "Resentment"]
            :safeguards ["Clear boundaries" "Personal privacy protection" "Consent" "Professional focus"])
   (failure "transparency-without-safety" "critical"
            "Transparency without psychological safety"
            :signals ["Fear of speaking up" "Self-censorship" "Political behavior" "Transparency theater"]
            :safeguards ["Psychological safety first" "No retaliation" "Reward honesty" "Lead by example"])]})

(register-model
 {:name "idea-meritocracy"
  :category "organizational_design"
  :originator "Ray Dalio / Bridgewater Associates"
  :description "The best ideas win regardless of who they come from; decisions are made by weighing people's opinions based on their track record and believability"
  :key-insight "Hierarchy of ideas, not people; track record matters; believability-weighted decision making produces better outcomes"
  :application "Track everyone's predictions; calculate believability scores; weight opinions by track record; let best ideas win regardless of seniority"
  :failure-modes
  [(failure "credibility-without-competence" "high"
            "Giving weight to confident people without track records"
            :signals ["Loud voices dominate" "Confidence over competence" "Poor decisions" "Meritocracy theater"]
            :safeguards ["Rigorous track record tracking" "Prediction markets" "Anonymous voting" "Data-driven believability"])
   (failure "narrow-believability" "high"
            "Only valuing track record in narrow domain"
            :signals ["Missed insights" "Siloed thinking" "Ignored perspectives" "Groupthink"]
            :safeguards ["Domain-specific believability" "Cross-domain insights" "Diverse perspectives" "Beginner's mind"])
   (failure "meritocracy-manipulation" "high"
            "Gaming the believability system"
            :signals ["Strategic voting" "Coalition building" "Politics" "System gaming"]
            :safeguards ["Transparent algorithms" "Audit trails" "Calibration" "Skin in the game"])
   (failure "junior-silencing" "medium"
            "Junior people not speaking up due to low believability"
            :signals ["Missed insights" "Conformity" "Reduced innovation" "Attrition"]
            :safeguards ["Encourage all voices" "Anonymous input" "Idea evaluation separate from source" "Apprenticeship"])
   (failure "believability-ossification" "medium"
            "Past track record preventing adaptation"
            :signals ["Outdated expertise" "Regime change ignored" "Declining performance" "Complacency"]
            :safeguards ["Recency weighting" "Continuous learning" "Regime-conditional believability" "Humility"])]})

(register-model
 {:name "believability-weighted-decisions"
  :category "decision_making"
  :originator "Ray Dalio / Bridgewater Associates"
  :description "Weight people's opinions by their believability (track record + reasoning ability) rather than treating all opinions equally or deferring to authority"
  :key-insight "Not all opinions are equal; track record matters; aggregate believability-weighted opinions for better decisions than democracy or autocracy"
  :application "Track prediction accuracy; calculate believability scores; weight votes by believability; aggregate for final decision"
  :failure-modes
  [(failure "equal-weighting" "high"
            "Treating all opinions as equally valid"
            :signals ["Wisdom of crowds failure" "Poor decisions" "Lowest common denominator" "Regression to mean"]
            :safeguards ["Believability weighting" "Track records" "Domain expertise" "Meritocracy"])
   (failure "authority-bias" "high"
            "Weighting by seniority rather than believability"
            :signals ["HiPPO decisions" "Ignored expertise" "Poor outcomes" "Resentment"]
            :safeguards ["Track record over title" "Data-driven believability" "Transparent weighting" "Meritocracy"])
   (failure "recency-bias" "medium"
            "Over-weighting recent track record"
            :signals ["Volatility in believability" "Overreaction to recent events" "Unstable weights"]
            :safeguards ["Long-term track records" "Statistical smoothing" "Bayesian updating" "Regime awareness"])
   (failure "narrow-track-record" "high"
            "Believability based on insufficient sample size"
            :signals ["Luck vs skill confusion" "Volatile believability" "Poor calibration"]
            :safeguards ["Minimum sample sizes" "Statistical significance" "Confidence intervals" "Regression to mean"])
   (failure "no-skin-in-game" "high"
            "Believability without consequences"
            :signals ["Reckless predictions" "No accountability" "Cheap talk" "Moral hazard"]
            :safeguards ["Skin in the game" "Consequences for predictions" "Alignment" "Accountability"])]})

;; ============================================