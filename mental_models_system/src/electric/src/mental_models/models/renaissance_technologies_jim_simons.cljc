(ns mental-models.models.renaissance-technologies-jim-simons
  "Mental Models - Renaissance Technologies / Jim Simons Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Renaissance Technologies / Jim Simons
;; ============================================

(register-model
 {:name "regime-detection"
  :category "quantitative_analysis"
  :originator "Jim Simons / Renaissance Technologies"
  :description "Markets and systems operate in distinct regimes with different statistical properties; detecting regime shifts is critical for adaptation"
  :key-insight "The same strategy that works in one regime can fail catastrophically in another; identify the regime before acting"
  :application "Monitor statistical properties of systems; detect when fundamental dynamics change; adapt strategies to current regime"
  :failure-modes
  [(failure "regime-blindness" "critical"
            "Not recognizing that system has shifted to new regime"
            :signals ["Strategies suddenly failing" "Historical patterns breaking" "Unexpected losses" "Correlations changing"]
            :safeguards ["Real-time regime monitoring" "Statistical tests for regime shifts" "Multiple regime indicators" "Adaptive strategies"])
   (failure "false-regime-detection" "high"
            "Seeing regime change in normal volatility"
            :signals ["Excessive strategy changes" "Whipsawed by noise" "High transaction costs" "Unstable performance"]
            :safeguards ["Statistical significance tests" "Confirmation from multiple indicators" "Regime persistence requirements" "Bayesian updating"])
   (failure "regime-overfitting" "high"
            "Identifying too many regimes, fragmenting data"
            :signals ["Overly complex models" "Poor out-of-sample performance" "Data mining" "Spurious patterns"]
            :safeguards ["Parsimony principle" "Out-of-sample validation" "Regime stability requirements" "Economic rationale"])
   (failure "single-regime-assumption" "critical"
            "Assuming current regime will persist indefinitely"
            :signals ["No contingency plans" "Surprise at regime shifts" "Catastrophic losses" "Black swan events"]
            :safeguards ["Scenario planning" "Stress testing across regimes" "Regime-conditional strategies" "Tail risk hedging"])
   (failure "lagging-regime-detection" "high"
            "Detecting regime change too late to adapt"
            :signals ["Losses before adaptation" "Slow response" "Playing catch-up" "Missed opportunities"]
            :safeguards ["Leading indicators" "Real-time monitoring" "Automated detection" "Rapid response protocols"])]})

(register-model
 {:name "factor-decomposition"
  :category "quantitative_analysis"
  :originator "Jim Simons / Renaissance Technologies"
  :description "Complex outcomes can be decomposed into independent factors; understanding factor exposures enables precise risk management and alpha generation"
  :key-insight "Returns are not monolithic; they are the sum of exposures to multiple factors; isolate and manage each factor independently"
  :application "Decompose any outcome into constituent factors; measure exposure to each; manage factors separately; identify pure alpha"
  :failure-modes
  [(failure "factor-blindness" "critical"
            "Not recognizing hidden factor exposures"
            :signals ["Unexpected correlations" "Unintended risks" "Surprise losses" "Portfolio concentration"]
            :safeguards ["Comprehensive factor analysis" "Regular factor decomposition" "Stress testing" "Factor attribution"])
   (failure "incomplete-factors" "high"
            "Missing important factors in decomposition"
            :signals ["Unexplained variance" "Residual correlations" "Model errors" "Unexpected outcomes"]
            :safeguards ["Thorough factor research" "Residual analysis" "Multiple factor models" "Academic literature review"])
   (failure "factor-timing" "high"
            "Attempting to time factor performance"
            :signals ["Inconsistent exposures" "Market timing losses" "Whipsawed by factors" "Underperformance"]
            :safeguards ["Consistent factor exposures" "Long-term factor investing" "Avoid market timing" "Systematic rebalancing"])
   (failure "correlated-factors" "high"
            "Treating correlated factors as independent"
            :signals ["Underestimated risk" "Concentration" "Simultaneous factor losses" "Diversification illusion"]
            :safeguards ["Factor correlation analysis" "Principal component analysis" "Independent factor construction" "Risk parity"])
   (failure "factor-crowding" "high"
            "Over-exposure to popular factors"
            :signals ["Crowded trades" "Sudden reversals" "Liquidity issues" "Factor crashes"]
            :safeguards ["Factor capacity analysis" "Contrarian factor selection" "Diversification" "Liquidity monitoring"])]})

(register-model
 {:name "mean-reversion"
  :category "quantitative_analysis"
  :originator "Jim Simons / Renaissance Technologies"
  :description "Many systems exhibit mean-reverting behavior; extreme values tend to return toward the mean over time"
  :key-insight "Trees don't grow to the sky; what goes up tends to come down; extremes are temporary; the mean is an attractor"
  :application "Identify mean-reverting systems; measure deviation from mean; bet on reversion when deviation is extreme; define time horizon"
  :failure-modes
  [(failure "trending-as-reverting" "critical"
            "Applying mean reversion to trending systems"
            :signals ["Catching falling knives" "Averaging down losses" "Fighting the trend" "Catastrophic losses"]
            :safeguards ["Regime detection" "Trend identification" "Stop losses" "Adaptive strategies"])
   (failure "wrong-mean" "critical"
            "Reverting to outdated or incorrect mean"
            :signals ["Persistent losses" "Mean keeps shifting" "Structural changes ignored" "Regime blindness"]
            :safeguards ["Dynamic mean estimation" "Regime-conditional means" "Structural break detection" "Adaptive means"])
   (failure "premature-reversion" "high"
            "Betting on reversion before extreme reached"
            :signals ["Early entries" "Extended losses" "Trend continuation" "Opportunity cost"]
            :safeguards ["Extreme thresholds" "Confirmation signals" "Patience" "Statistical significance"])
   (failure "slow-reversion" "medium"
            "Underestimating time for reversion"
            :signals ["Holding costs" "Opportunity cost" "Margin calls" "Forced exits"]
            :safeguards ["Time horizon analysis" "Historical reversion speeds" "Position sizing" "Patience capital"])
   (failure "no-reversion" "critical"
            "Assuming reversion when structural change occurred"
            :signals ["Permanent losses" "Value traps" "Obsolete businesses" "Paradigm shifts"]
            :safeguards ["Fundamental analysis" "Structural change detection" "Scenario analysis" "Exit criteria"])]})

;; ============================================