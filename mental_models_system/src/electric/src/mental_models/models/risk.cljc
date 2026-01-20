(ns mental-models.models.risk
  "Mental Models - Risk Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Risk
;; ============================================

(register-model
 {:name "black-swan"
  :category "risk"
  :originator "Nassim Taleb"
  :description "Rare, unpredictable events with massive impact"
  :key-insight "We systematically underestimate the probability and impact of rare events"
  :application "Build robustness to negative black swans; expose to positive ones"
  :failure-modes
  [(failure "prediction-obsession" "high"
            "Trying to predict black swans"
            :signals ["Forecasting rare events" "False confidence"]
            :safeguards ["Focus on robustness" "Accept unpredictability"])
   (failure "fragility" "critical"
            "Building systems that break under stress"
            :signals ["Single points of failure" "No redundancy"]
            :safeguards ["Build redundancy" "Stress test"])
   (failure "narrative-fallacy" "high"
            "Creating stories to explain black swans after the fact"
            :signals ["Hindsight bias" "False understanding"]
            :safeguards ["Acknowledge randomness" "Avoid hindsight"])
   (failure "turkey-problem" "critical"
            "Mistaking absence of evidence for evidence of absence"
            :signals ["Long track record of safety" "Complacency"]
            :safeguards ["Consider hidden risks" "Stress scenarios"])
   (failure "positive-swan-blindness" "medium"
            "Only preparing for negative black swans"
            :signals ["Missing upside opportunities"]
            :safeguards ["Expose to positive randomness" "Optionality"])]})

(register-model
 {:name "antifragility"
  :category "risk"
  :originator "Nassim Taleb"
  :description "Systems that gain from disorder and stress"
  :key-insight "Some things benefit from shocks; they thrive on volatility"
  :application "Build systems that get stronger from stress"
  :failure-modes
  [(failure "fragility-building" "high"
            "Building fragile systems"
            :signals ["Breaks under stress" "Requires stability"]
            :safeguards ["Stress test" "Add redundancy"])
   (failure "robustness-confusion" "medium"
            "Confusing robustness with antifragility"
            :signals ["Survives but doesn't improve"]
            :safeguards ["Look for improvement from stress"])
   (failure "over-optimization" "high"
            "Optimizing away antifragility"
            :signals ["Efficiency over resilience" "No slack"]
            :safeguards ["Maintain slack" "Accept inefficiency"])
   (failure "stress-avoidance" "medium"
            "Avoiding all stress"
            :signals ["Overprotection" "No challenge"]
            :safeguards ["Embrace appropriate stress" "Hormesis"])
   (failure "iatrogenics" "high"
            "Interventions that cause more harm than good"
            :signals ["Helping that hurts" "Unintended consequences"]
            :safeguards ["First do no harm" "Minimal intervention"])]})

(register-model
 {:name "fat-tails"
  :category "risk"
  :originator "Statistics/Finance"
  :description "Distributions with more extreme events than normal"
  :key-insight "Extreme events are more common than we think"
  :application "Don't use normal distribution assumptions for fat-tailed phenomena"
  :failure-modes
  [(failure "thin-tail-assumption" "critical"
            "Assuming normal distribution when tails are fat"
            :signals ["Surprised by extremes" "Risk models failing"]
            :safeguards ["Test for fat tails" "Use appropriate distributions"])
   (failure "var-reliance" "high"
            "Relying on Value at Risk for fat-tailed risks"
            :signals ["VaR breaches" "Tail losses"]
            :safeguards ["Use tail risk measures" "Stress testing"])
   (failure "correlation-breakdown" "high"
            "Ignoring correlation changes in extremes"
            :signals ["Diversification failing in crisis"]
            :safeguards ["Stress test correlations" "Tail dependence"])
   (failure "historical-extrapolation" "high"
            "Assuming future extremes limited by past"
            :signals ["Record-breaking events"]
            :safeguards ["Consider unprecedented events" "Imagination"])
   (failure "tail-hedging-cost" "medium"
            "Overpaying for tail protection"
            :signals ["Expensive hedges" "Drag on returns"]
            :safeguards ["Cost-effective hedging" "Natural hedges"])]})

;; ============================================