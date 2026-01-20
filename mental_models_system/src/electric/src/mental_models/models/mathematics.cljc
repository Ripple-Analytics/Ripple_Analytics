(ns mental-models.models.mathematics
  "Mental Models - Mathematics Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Mathematics
;; ============================================

(register-model
 {:name "bayes-theorem"
  :category "mathematics"
  :originator "Thomas Bayes"
  :description "Update beliefs based on new evidence"
  :key-insight "Prior probability × likelihood = posterior probability"
  :application "Continuously update your beliefs as new information arrives"
  :failure-modes
  [(failure "prior-neglect" "high"
            "Ignoring base rates when evaluating evidence"
            :signals ["Overweighting new evidence" "Ignoring historical data"]
            :safeguards ["Always start with base rates" "Research priors" "Calibrate"])
   (failure "confirmation-bias" "high"
            "Only updating on confirming evidence"
            :signals ["Seeking confirming data" "Dismissing contrary evidence"]
            :safeguards ["Seek disconfirming evidence" "Pre-commit to update rules"])
   (failure "overconfidence-in-priors" "medium"
            "Not updating enough on new evidence"
            :signals ["Dismissing new data" "Anchoring on initial beliefs"]
            :safeguards ["Track prediction accuracy" "Be willing to change"])
   (failure "likelihood-ratio-errors" "high"
            "Misjudging how diagnostic evidence is"
            :signals ["Treating all evidence equally" "Not considering alternatives"]
            :safeguards ["Calculate likelihood ratios" "Consider multiple hypotheses"])
   (failure "binary-thinking" "medium"
            "Not maintaining probability distributions"
            :signals ["Certain/uncertain only" "No degrees of belief"]
            :safeguards ["Think in probabilities" "Maintain uncertainty ranges"])]})

(register-model
 {:name "regression-to-mean"
  :category "mathematics"
  :originator "Francis Galton"
  :description "Extreme outcomes tend to be followed by more average ones"
  :key-insight "Luck and skill both contribute to outcomes"
  :application "Don't overreact to extreme results"
  :failure-modes
  [(failure "hot-hand-fallacy" "high"
            "Expecting streaks to continue"
            :signals ["Chasing hot performers" "Extrapolating short-term trends"]
            :safeguards ["Look at long-term averages" "Separate skill from luck"])
   (failure "gambler-fallacy" "high"
            "Expecting regression when events are independent"
            :signals ["Due for a win" "Expecting balance"]
            :safeguards ["Understand independence" "Each event is fresh"])
   (failure "skill-luck-confusion" "high"
            "Attributing luck to skill or vice versa"
            :signals ["Praising/blaming for random outcomes"]
            :safeguards ["Analyze process not outcome" "Large sample sizes"])
   (failure "regression-blindness" "medium"
            "Not expecting regression when you should"
            :signals ["Surprised by return to average" "Overreacting to extremes"]
            :safeguards ["Expect regression" "Wait for more data"])
   (failure "intervention-illusion" "high"
            "Thinking intervention caused regression"
            :signals ["Crediting treatment for natural regression"]
            :safeguards ["Control groups" "Baseline comparison"])]})

(register-model
 {:name "power-laws"
  :category "mathematics"
  :originator "Vilfredo Pareto"
  :description "A few items account for most of the effect (80/20 rule)"
  :key-insight "Distributions are often highly skewed"
  :application "Focus on the vital few, not the trivial many"
  :failure-modes
  [(failure "average-thinking" "high"
            "Assuming normal distributions when power laws apply"
            :signals ["Using averages" "Ignoring outliers"]
            :safeguards ["Check distribution shape" "Look for fat tails"])
   (failure "equal-treatment" "high"
            "Treating all items equally"
            :signals ["Spreading effort evenly" "Not prioritizing"]
            :safeguards ["Identify the vital few" "Ruthless prioritization"])
   (failure "tail-blindness" "critical"
            "Ignoring extreme events"
            :signals ["Dismissing outliers" "Underestimating tail risk"]
            :safeguards ["Study extremes" "Prepare for black swans"])
   (failure "false-precision" "medium"
            "Precise predictions in power law domains"
            :signals ["Point estimates" "Narrow confidence intervals"]
            :safeguards ["Wide ranges" "Scenario planning"])
   (failure "static-pareto" "medium"
            "Assuming the vital few never change"
            :signals ["Not re-evaluating" "Stale priorities"]
            :safeguards ["Regular review" "Monitor shifts"])]})

(register-model
 {:name "normal-distribution"
  :category "mathematics"
  :originator "Carl Friedrich Gauss"
  :description "Many phenomena cluster around an average with predictable spread"
  :key-insight "Most observations fall within a few standard deviations of the mean"
  :application "Use when many independent factors combine additively"
  :failure-modes
  [(failure "false-normality" "critical"
            "Assuming normal when distribution is different"
            :signals ["Using normal stats on non-normal data" "Ignoring skewness"]
            :safeguards ["Test for normality" "Check distribution shape"])
   (failure "outlier-dismissal" "high"
            "Dismissing outliers as errors"
            :signals ["Removing extreme data" "Assuming measurement error"]
            :safeguards ["Investigate outliers" "Consider fat tails"])
   (failure "mean-obsession" "medium"
            "Focusing only on average"
            :signals ["Ignoring variance" "Not considering spread"]
            :safeguards ["Report variance" "Consider full distribution"])
   (failure "independence-assumption" "high"
            "Assuming independence when correlated"
            :signals ["Ignoring correlations" "Underestimating joint risk"]
            :safeguards ["Test for correlation" "Model dependencies"])
   (failure "sample-size-neglect" "high"
            "Drawing conclusions from small samples"
            :signals ["Overconfident with few data points"]
            :safeguards ["Larger samples" "Report confidence intervals"])]})

(register-model
 {:name "expected-value"
  :category "mathematics"
  :originator "Blaise Pascal"
  :description "Probability-weighted average of all possible outcomes"
  :key-insight "Multiply probability by payoff to compare options"
  :application "Make decisions based on expected value, not best/worst case"
  :failure-modes
  [(failure "probability-neglect" "high"
            "Ignoring probabilities in decisions"
            :signals ["Focusing on outcomes only" "Best case planning"]
            :safeguards ["Estimate probabilities" "Weight by likelihood"])
   (failure "payoff-neglect" "high"
            "Ignoring magnitude of outcomes"
            :signals ["Treating all outcomes equally" "Not sizing bets"]
            :safeguards ["Estimate payoffs" "Consider magnitude"])
   (failure "risk-of-ruin-blindness" "critical"
            "Ignoring catastrophic outcomes"
            :signals ["Positive EV but fatal downside" "Betting the farm"]
            :safeguards ["Never risk ruin" "Kelly criterion" "Position sizing"])
   (failure "utility-linearity" "medium"
            "Assuming linear utility"
            :signals ["Treating $1M same as 1000×$1K"]
            :safeguards ["Consider diminishing returns" "Utility functions"])
   (failure "single-play-fallacy" "medium"
            "Applying EV to one-time decisions"
            :signals ["No opportunity to average out"]
            :safeguards ["Consider variance" "Downside protection"])]})

;; ============================================