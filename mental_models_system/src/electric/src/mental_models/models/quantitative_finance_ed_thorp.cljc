(ns mental-models.models.quantitative-finance-ed-thorp
  "Mental Models - Quantitative Finance (Ed Thorp) Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Quantitative Finance (Ed Thorp)
;; ============================================

(register-model
 {:name "kelly-criterion"
  :category "quantitative_finance"
  :originator "Ed Thorp / John Kelly"
  :description "Optimal bet sizing to maximize long-term growth"
  :key-insight "Bet fraction of bankroll proportional to edge divided by odds"
  :application "Size positions using Kelly formula: f = (bp - q) / b"
  :failure-modes
  [(failure "parameter-estimation-error" "critical"
            "Wrong estimates of probability or payoff"
            :signals ["Overbetting" "Ruin risk"]
            :safeguards ["Conservative estimates" "Fractional Kelly" "Sensitivity analysis"])
   (failure "full-kelly-betting" "critical"
            "Using full Kelly without margin of safety"
            :signals ["High volatility" "Drawdowns"]
            :safeguards ["Half Kelly" "Quarter Kelly" "Fractional sizing"])
   (failure "correlated-bets" "high"
            "Applying Kelly to correlated positions"
            :signals ["Concentration risk" "Simultaneous losses"]
            :safeguards ["Correlation adjustment" "Portfolio Kelly" "Diversification"])
   (failure "non-ergodic-application" "critical"
            "Using Kelly in non-ergodic situations"
            :signals ["Ruin despite positive expectancy" "Path dependence"]
            :safeguards ["Ergodicity check" "Time vs ensemble" "Multiplicative vs additive"])
   (failure "leverage-on-kelly" "critical"
            "Leveraging Kelly-sized positions"
            :signals ["Excessive risk" "Blowup potential"]
            :safeguards ["No leverage on Kelly" "Conservative sizing" "Risk limits"])]})

;; ============================================