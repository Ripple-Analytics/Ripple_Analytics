(ns mental-models.models.negotiation
  "Mental Models - Negotiation Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Negotiation
;; ============================================

(register-model
 {:name "batna"
  :category "negotiation"
  :originator "Roger Fisher"
  :description "Best Alternative To Negotiated Agreement"
  :key-insight "Your power in negotiation comes from your alternatives"
  :application "Always know your BATNA before negotiating"
  :failure-modes
  [(failure "unknown-batna" "critical"
            "Not knowing your alternatives"
            :signals ["Accepting bad deals" "Desperation"]
            :safeguards ["Research alternatives" "Develop options"])
   (failure "weak-batna" "high"
            "Having poor alternatives"
            :signals ["No leverage" "Must accept terms"]
            :safeguards ["Improve alternatives" "Create options"])
   (failure "batna-overestimation" "high"
            "Overestimating your alternatives"
            :signals ["Walking away from good deals" "Overconfidence"]
            :safeguards ["Realistic assessment" "Test alternatives"])
   (failure "batna-revelation" "medium"
            "Revealing your BATNA inappropriately"
            :signals ["Losing leverage" "Opponent exploiting knowledge"]
            :safeguards ["Strategic disclosure" "Protect information"])
   (failure "static-batna" "medium"
            "Not improving BATNA during negotiation"
            :signals ["Weakening position" "No parallel development"]
            :safeguards ["Continuous improvement" "Parallel negotiations"])]})

(register-model
 {:name "anchoring-negotiation"
  :category "negotiation"
  :originator "Negotiation Theory"
  :description "First numbers strongly influence final outcomes"
  :key-insight "Anchor aggressively but credibly"
  :application "Make the first offer when you have information"
  :failure-modes
  [(failure "anchor-neglect" "high"
            "Not anchoring when you should"
            :signals ["Letting opponent anchor" "Reactive positioning"]
            :safeguards ["Anchor first when informed" "Set the frame"])
   (failure "extreme-anchor" "medium"
            "Anchoring so aggressively you lose credibility"
            :signals ["Opponent walks away" "Damaged relationship"]
            :safeguards ["Credible anchors" "Justifiable positions"])
   (failure "anchor-susceptibility" "high"
            "Being overly influenced by opponent's anchor"
            :signals ["Adjusting from their number" "Forgetting your analysis"]
            :safeguards ["Prepare independently" "Ignore their anchor"])
   (failure "single-issue-anchoring" "medium"
            "Only anchoring on one dimension"
            :signals ["Losing on other issues"]
            :safeguards ["Multi-issue anchoring" "Package deals"])
   (failure "anchor-rigidity" "medium"
            "Unable to move from anchor"
            :signals ["Stalemate" "No progress"]
            :safeguards ["Planned concessions" "Flexibility"])]})

(register-model
 {:name "win-win"
  :category "negotiation"
  :originator "Negotiation Theory"
  :description "Expand the pie before dividing it"
  :key-insight "Most negotiations have integrative potential"
  :application "Look for trades that create value for both parties"
  :failure-modes
  [(failure "zero-sum-assumption" "high"
            "Assuming fixed pie when it can expand"
            :signals ["Distributive only" "Missing value creation"]
            :safeguards ["Look for trades" "Explore interests"])
   (failure "naive-win-win" "medium"
            "Assuming win-win when it's zero-sum"
            :signals ["Being exploited" "Giving away value"]
            :safeguards ["Verify integrative potential" "Protect interests"])
   (failure "interest-blindness" "high"
            "Not understanding underlying interests"
            :signals ["Positional bargaining" "Missing creative solutions"]
            :safeguards ["Ask why" "Explore interests"])
   (failure "value-claiming-neglect" "medium"
            "Creating value but not claiming fair share"
            :signals ["Expanding pie but getting small slice"]
            :safeguards ["Balance creation and claiming"])
   (failure "relationship-sacrifice" "medium"
            "Sacrificing relationship for deal"
            :signals ["Winning battle, losing war"]
            :safeguards ["Long-term view" "Relationship maintenance"])]})

;; ============================================