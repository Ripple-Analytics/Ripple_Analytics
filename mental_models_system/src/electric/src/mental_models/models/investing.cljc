(ns mental-models.models.investing
  "Mental Models - Investing Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Investing
;; ============================================

(register-model
 {:name "mr-market"
  :category "investing"
  :originator "Benjamin Graham"
  :description "The market is like an emotional partner offering prices"
  :key-insight "Mr. Market is there to serve you, not guide you"
  :application "Take advantage of Mr. Market's mood swings"
  :failure-modes
  [(failure "market-following" "high"
            "Letting Mr. Market guide your decisions"
            :signals ["Buying high, selling low" "Emotional trading"]
            :safeguards ["Independent valuation" "Ignore daily prices"])
   (failure "market-timing" "high"
            "Trying to predict Mr. Market's moods"
            :signals ["Waiting for perfect entry" "Missing opportunities"]
            :safeguards ["Time in market" "Dollar cost averaging"])
   (failure "market-ignoring" "medium"
            "Completely ignoring market prices"
            :signals ["Missing obvious mispricings"]
            :safeguards ["Monitor for opportunities" "Be ready to act"])
   (failure "overtrading" "high"
            "Trading too much with Mr. Market"
            :signals ["High transaction costs" "Tax inefficiency"]
            :safeguards ["Trade rarely" "Long-term holding"])
   (failure "market-rationality-assumption" "medium"
            "Assuming Mr. Market is always right"
            :signals ["Efficient market worship" "No independent thought"]
            :safeguards ["Markets can be wrong" "Do your analysis"])]})

(register-model
 {:name "circle-of-competence-investing"
  :category "investing"
  :originator "Warren Buffett"
  :description "Only invest in what you understand"
  :key-insight "It's not about the size of your circle, but knowing its boundaries"
  :application "Stay within your circle; expand it deliberately"
  :failure-modes
  [(failure "circle-expansion-greed" "high"
            "Investing outside your circle for returns"
            :signals ["Chasing hot sectors" "FOMO investing"]
            :safeguards ["Stick to what you know" "Pass on unknowns"])
   (failure "false-understanding" "high"
            "Thinking you understand when you don't"
            :signals ["Can't explain simply" "Surprised by outcomes"]
            :safeguards ["Explain to others" "Test understanding"])
   (failure "circle-stagnation" "medium"
            "Not expanding circle over time"
            :signals ["Same investments forever" "Missing opportunities"]
            :safeguards ["Deliberate learning" "Adjacent expansion"])
   (failure "over-diversification" "medium"
            "Diversifying into areas you don't understand"
            :signals ["Owning what you can't evaluate"]
            :safeguards ["Concentrated in circle" "Index outside"])
   (failure "competence-arrogance" "high"
            "Overconfidence in your circle"
            :signals ["Not seeking contrary views" "Dismissing risks"]
            :safeguards ["Humility" "Seek disconfirmation"])]})

(register-model
 {:name "margin-of-safety-investing"
  :category "investing"
  :originator "Benjamin Graham"
  :description "Buy at a significant discount to intrinsic value"
  :key-insight "The margin of safety protects against errors and bad luck"
  :application "Only buy when price is well below value"
  :failure-modes
  [(failure "no-margin" "critical"
            "Buying without margin of safety"
            :signals ["Paying fair value" "No room for error"]
            :safeguards ["Require discount" "Walk away if not cheap"])
   (failure "value-trap" "high"
            "Buying cheap things that stay cheap"
            :signals ["Permanent impairment" "Deteriorating business"]
            :safeguards ["Quality matters" "Catalyst identification"])
   (failure "margin-erosion" "high"
            "Margin disappearing after purchase"
            :signals ["Business deterioration" "Competitive damage"]
            :safeguards ["Monitor continuously" "Sell if thesis breaks"])
   (failure "opportunity-cost" "medium"
            "Waiting for margin that never comes"
            :signals ["Missing good investments" "Cash drag"]
            :safeguards ["Reasonable thresholds" "Opportunity cost awareness"])
   (failure "false-margin" "high"
            "Margin based on wrong valuation"
            :signals ["Valuation errors" "Optimistic assumptions"]
            :safeguards ["Conservative valuation" "Multiple methods"])]})

;; ============================================