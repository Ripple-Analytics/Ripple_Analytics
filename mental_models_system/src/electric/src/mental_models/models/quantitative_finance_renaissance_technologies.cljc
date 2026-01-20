(ns mental-models.models.quantitative-finance-renaissance-technologies
  "Mental Models - Quantitative Finance (Renaissance Technologies) Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Quantitative Finance (Renaissance Technologies)
;; ============================================

(register-model
 {:name "regime-detection"
  :category "quantitative_finance"
  :originator "Jim Simons / Renaissance Technologies"
  :description "Identify distinct market states with different statistical properties"
  :key-insight "Markets operate in different regimes; strategies that work in one fail in another"
  :application "Detect regime shifts before adjusting strategy"
  :failure-modes
  [(failure "regime-lag" "critical"
            "Detecting regime change too late"
            :signals ["Losses after shift" "Stale indicators"]
            :safeguards ["Leading indicators" "Multiple detection methods" "Fast adaptation"])
   (failure "false-regime-signal" "high"
            "Seeing regime change when none exists"
            :signals ["Excessive switching" "Strategy whipsaw"]
            :safeguards ["Confirmation thresholds" "Multiple timeframes" "Statistical significance"])
   (failure "regime-overfitting" "high"
            "Too many regimes, too specific"
            :signals ["Overfit to history" "Poor out-of-sample"]
            :safeguards ["Parsimony" "Out-of-sample testing" "Regime stability"])
   (failure "missing-regime" "critical"
            "New regime not in model"
            :signals ["Unprecedented losses" "Model breakdown"]
            :safeguards ["Adaptive learning" "Anomaly detection" "Human oversight"])
   (failure "regime-blindness" "high"
            "Assuming single regime forever"
            :signals ["Consistent strategy failure" "Ignoring context"]
            :safeguards ["Regular regime analysis" "Context awareness" "Adaptive systems"])]})

(register-model
 {:name "factor-decomposition"
  :category "quantitative_finance"
  :originator "Renaissance Technologies"
  :description "Break down returns into underlying factors to understand true drivers"
  :key-insight "Returns are combinations of factors; isolate them to find alpha"
  :application "Decompose performance into market, sector, style, and alpha factors"
  :failure-modes
  [(failure "factor-confusion" "high"
            "Mistaking beta for alpha"
            :signals ["Returns disappear in different market" "Correlation with index"]
            :safeguards ["Factor analysis" "Multiple market conditions" "Risk decomposition"])
   (failure "missing-factors" "high"
            "Incomplete factor model"
            :signals ["Unexplained variance" "Residual patterns"]
            :safeguards ["Comprehensive factor set" "Residual analysis" "Factor discovery"])
   (failure "factor-multicollinearity" "medium"
            "Factors too correlated"
            :signals ["Unstable coefficients" "Interpretation difficulty"]
            :safeguards ["Factor orthogonalization" "Principal components" "Factor selection"])
   (failure "time-varying-factors" "high"
            "Factor loadings change over time"
            :signals ["Model drift" "Degrading performance"]
            :safeguards ["Rolling analysis" "Adaptive factors" "Regime-dependent factors"])
   (failure "factor-overfitting" "critical"
            "Too many factors, data mining"
            :signals ["Perfect in-sample, poor out-of-sample"]
            :safeguards ["Parsimony" "Economic rationale" "Out-of-sample validation"])]})

(register-model
 {:name "mean-reversion"
  :category "quantitative_finance"
  :originator "Renaissance Technologies"
  :description "Prices that deviate from mean tend to return to it"
  :key-insight "Temporary dislocations create opportunities; extremes don't last"
  :application "Buy undervalued, sell overvalued, wait for reversion"
  :failure-modes
  [(failure "structural-shift" "critical"
            "Mean has permanently changed"
            :signals ["No reversion" "New equilibrium"]
            :safeguards ["Regime detection" "Fundamental analysis" "Adaptive means"])
   (failure "slow-reversion" "high"
            "Reversion takes longer than capital lasts"
            :signals ["Correct direction, wrong timing" "Margin calls"]
            :safeguards ["Position sizing" "Time horizons" "Funding stability"])
   (failure "trend-vs-reversion" "high"
            "Mistaking trend for mean reversion opportunity"
            :signals ["Catching falling knife" "Fighting trend"]
            :safeguards ["Trend detection" "Momentum filters" "Multi-timeframe analysis"])
   (failure "mean-calculation-error" "high"
            "Wrong mean estimate"
            :signals ["Consistent losses" "Reversion to wrong level"]
            :safeguards ["Multiple mean estimates" "Fundamental anchors" "Adaptive means"])
   (failure "leverage-on-reversion" "critical"
            "Overleveraging mean reversion trades"
            :signals ["Blowup risk" "Margin pressure"]
            :safeguards ["Conservative leverage" "Risk limits" "Diversification"])]})

;; ============================================