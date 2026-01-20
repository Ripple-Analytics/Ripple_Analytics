(ns mental-models.models.economics
  "Mental Models - Economics Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Economics
;; ============================================

(register-model
 {:name "supply-demand"
  :category "economics"
  :originator "Adam Smith"
  :description "Prices are determined by the interaction of supply and demand"
  :key-insight "Price is a signal that coordinates economic activity"
  :application "Analyze how supply and demand affect any market"
  :failure-modes
  [(failure "price-blindness" "high"
            "Ignoring price signals"
            :signals ["Not adjusting to price changes" "Ignoring market signals"]
            :safeguards ["Monitor prices" "Respond to signals" "Understand price formation"])
   (failure "supply-only-thinking" "medium"
            "Only considering supply side"
            :signals ["Ignoring demand" "Cost-plus pricing"]
            :safeguards ["Study demand curves" "Value-based pricing" "Customer research"])
   (failure "demand-only-thinking" "medium"
            "Only considering demand side"
            :signals ["Ignoring supply constraints" "Assuming infinite supply"]
            :safeguards ["Map supply chain" "Understand capacity" "Monitor inventory"])
   (failure "equilibrium-assumption" "high"
            "Assuming markets are always in equilibrium"
            :signals ["Ignoring disequilibrium" "Missing arbitrage"]
            :safeguards ["Look for imbalances" "Monitor adjustment speed"])
   (failure "elasticity-blindness" "high"
            "Not considering price elasticity"
            :signals ["Assuming fixed demand" "Ignoring substitutes"]
            :safeguards ["Measure elasticity" "Study substitutes" "Test price sensitivity"])]})

(register-model
 {:name "comparative-advantage"
  :category "economics"
  :originator "David Ricardo"
  :description "Specialize in what you do relatively better, even if not absolutely better"
  :key-insight "Trade benefits both parties even when one is better at everything"
  :application "Focus on your relative strengths, not absolute ones"
  :failure-modes
  [(failure "absolute-advantage-focus" "high"
            "Only considering absolute advantage"
            :signals ["Trying to do everything" "Not specializing"]
            :safeguards ["Calculate opportunity costs" "Compare relative efficiency"])
   (failure "static-advantage-thinking" "medium"
            "Assuming advantages don't change"
            :signals ["Complacency" "Not investing in capabilities"]
            :safeguards ["Monitor competitive landscape" "Invest in skills"])
   (failure "over-specialization" "medium"
            "Specializing too narrowly"
            :signals ["Single point of failure" "No diversification"]
            :safeguards ["Maintain core competencies" "Strategic diversification"])
   (failure "trade-barrier-blindness" "medium"
            "Ignoring barriers to trade/exchange"
            :signals ["Assuming frictionless exchange" "Ignoring transaction costs"]
            :safeguards ["Map barriers" "Reduce friction" "Account for costs"])
   (failure "advantage-erosion" "high"
            "Not protecting comparative advantage"
            :signals ["Competitors catching up" "Commoditization"]
            :safeguards ["Continuous improvement" "Build moats" "Innovate"])]})

(register-model
 {:name "compound-interest"
  :category "economics"
  :originator "Mathematics/Finance"
  :description "Returns on returns create exponential growth"
  :key-insight "Small differences in rate compound to huge differences over time"
  :application "Start early, be patient, optimize the rate"
  :failure-modes
  [(failure "linear-thinking" "critical"
            "Thinking in linear rather than exponential terms"
            :signals ["Underestimating long-term growth" "Impatience"]
            :safeguards ["Calculate compound effects" "Visualize exponentials" "Think long-term"])
   (failure "rate-neglect" "high"
            "Not optimizing the compound rate"
            :signals ["Accepting low returns" "Not shopping for better rates"]
            :safeguards ["Compare rates" "Optimize continuously" "Reduce fees"])
   (failure "time-neglect" "high"
            "Not starting early enough"
            :signals ["Procrastination" "Waiting for perfect conditions"]
            :safeguards ["Start now" "Time in market" "Automatic investing"])
   (failure "interruption-damage" "high"
            "Breaking the compound chain"
            :signals ["Withdrawing early" "Panic selling"]
            :safeguards ["Stay invested" "Emergency fund" "Long-term commitment"])
   (failure "negative-compounding-blindness" "high"
            "Not seeing negative compounding (debt, bad habits)"
            :signals ["Accumulating debt" "Ignoring small negatives"]
            :safeguards ["Eliminate negative compounds" "Address small problems early"])]})

;; ============================================