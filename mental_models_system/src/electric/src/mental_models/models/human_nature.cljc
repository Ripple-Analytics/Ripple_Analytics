(ns mental-models.models.human-nature
  "Mental Models - Human Nature Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Human Nature
;; ============================================

(register-model
 {:name "reciprocity"
  :category "human_nature"
  :originator "Robert Cialdini"
  :description "People feel obligated to return favors"
  :key-insight "Give first to receive; the obligation to reciprocate is powerful"
  :application "Give value first; be aware of reciprocity manipulation"
  :failure-modes
  [(failure "reciprocity-blindness" "medium"
            "Not using reciprocity effectively"
            :signals ["Asking before giving" "Transactional approach"]
            :safeguards ["Give first" "Genuine generosity"])
   (failure "reciprocity-manipulation" "high"
            "Being manipulated through reciprocity"
            :signals ["Feeling obligated" "Unwanted gifts"]
            :safeguards ["Recognize the tactic" "Decline if manipulative"])
   (failure "over-reciprocation" "medium"
            "Reciprocating disproportionately"
            :signals ["Small favor, big return" "Exploitation"]
            :safeguards ["Proportional response" "Recognize imbalance"])
   (failure "reciprocity-expectation" "medium"
            "Giving with expectation of return"
            :signals ["Keeping score" "Resentment when not reciprocated"]
            :safeguards ["Give without expectation" "Genuine generosity"])
   (failure "negative-reciprocity" "high"
            "Escalating negative exchanges"
            :signals ["Tit for tat" "Revenge cycles"]
            :safeguards ["Break negative cycles" "Forgiveness"])]})

(register-model
 {:name "scarcity"
  :category "human_nature"
  :originator "Robert Cialdini"
  :description "People value things more when they're scarce"
  :key-insight "Limited availability increases perceived value"
  :application "Create genuine scarcity; be aware of artificial scarcity"
  :failure-modes
  [(failure "scarcity-manipulation" "high"
            "Being manipulated by artificial scarcity"
            :signals ["Limited time offers" "Only 3 left"]
            :safeguards ["Verify scarcity" "Decide on merits"])
   (failure "scarcity-blindness" "medium"
            "Not recognizing genuine scarcity"
            :signals ["Missing opportunities" "Assuming abundance"]
            :safeguards ["Assess true availability" "Act on genuine scarcity"])
   (failure "hoarding" "medium"
            "Overreacting to scarcity"
            :signals ["Stockpiling" "Panic buying"]
            :safeguards ["Rational assessment" "Avoid panic"])
   (failure "false-scarcity-creation" "medium"
            "Creating artificial scarcity unethically"
            :signals ["Manipulation" "Trust erosion"]
            :safeguards ["Genuine scarcity only" "Ethical marketing"])
   (failure "abundance-blindness" "medium"
            "Not seeing when scarcity has ended"
            :signals ["Hoarding when abundant" "Outdated scarcity mindset"]
            :safeguards ["Monitor conditions" "Update beliefs"])]})

(register-model
 {:name "authority"
  :category "human_nature"
  :originator "Stanley Milgram"
  :description "People tend to obey authority figures"
  :key-insight "Authority can override personal judgment"
  :application "Use authority responsibly; question authority appropriately"
  :failure-modes
  [(failure "blind-obedience" "critical"
            "Following authority without question"
            :signals ["Not questioning orders" "Deferring judgment"]
            :safeguards ["Question authority" "Independent thinking"])
   (failure "authority-rejection" "medium"
            "Rejecting all authority"
            :signals ["Contrarian for its own sake" "Missing valid expertise"]
            :safeguards ["Evaluate on merits" "Respect genuine expertise"])
   (failure "false-authority" "high"
            "Being fooled by false authority signals"
            :signals ["Titles without substance" "Credentials without competence"]
            :safeguards ["Verify expertise" "Look for track record"])
   (failure "authority-abuse" "high"
            "Misusing authority position"
            :signals ["Demanding compliance" "Not earning respect"]
            :safeguards ["Lead by example" "Earn authority"])
   (failure "authority-transfer" "medium"
            "Assuming authority in one domain transfers to another"
            :signals ["Expert in X opining on Y"]
            :safeguards ["Domain-specific expertise" "Stay in lane"])]})

;; ============================================