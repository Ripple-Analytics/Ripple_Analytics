(ns mental-models.models.organizational-design-ray-dalio
  "Mental Models - Organizational Design (Ray Dalio) Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Organizational Design (Ray Dalio)
;; ============================================

(register-model
 {:name "radical-transparency"
  :category "organizational_design"
  :originator "Ray Dalio"
  :description "Make almost everything open and accessible to everyone"
  :key-insight "Transparency creates trust and enables better decisions"
  :application "Record meetings, share feedback openly, make information accessible"
  :failure-modes
  [(failure "privacy-violation" "high"
            "Transparency without boundaries"
            :signals ["Personal issues public" "Legal problems"]
            :safeguards ["Clear boundaries" "Privacy protection" "Consent"])
   (failure "transparency-paralysis" "medium"
            "Fear of being recorded prevents action"
            :signals ["Reduced candor" "Performative behavior"]
            :safeguards ["Psychological safety" "Clear purpose" "Trust building"])
   (failure "information-overload" "medium"
            "Too much transparency, can't process"
            :signals ["Overwhelmed" "Can't find signal"]
            :safeguards ["Curation" "Relevance filtering" "Summaries"])
   (failure "weaponized-transparency" "high"
            "Using transparency to attack others"
            :signals ["Gotcha culture" "Fear-based environment"]
            :safeguards ["Constructive intent" "Learning focus" "No blame culture"])
   (failure "selective-transparency" "high"
            "Transparency only when convenient"
            :signals ["Trust erosion" "Cynicism"]
            :safeguards ["Consistent application" "Leadership modeling" "Accountability"])]})

(register-model
 {:name "idea-meritocracy"
  :category "organizational_design"
  :originator "Ray Dalio"
  :description "Best ideas win regardless of source"
  :key-insight "Truth and excellence come from rigorous evaluation, not hierarchy"
  :application "Weight ideas by quality and track record, not title"
  :failure-modes
  [(failure "false-meritocracy" "critical"
            "Claiming meritocracy while maintaining hierarchy"
            :signals ["Same people always win" "Lip service to ideas"]
            :safeguards ["Track decision outcomes" "Transparent weighting" "Actual power shifts"])
   (failure "credibility-calculation-error" "high"
            "Wrong weights for believability"
            :signals ["Poor decisions" "Ignored expertise"]
            :safeguards ["Track record data" "Domain specificity" "Regular recalibration"])
   (failure "analysis-paralysis" "medium"
            "Too much debate, no decisions"
            :signals ["Endless discussion" "Missed opportunities"]
            :safeguards ["Decision deadlines" "Escalation paths" "Minimum viable consensus"])
   (failure "expertise-tyranny" "medium"
            "Only experts can contribute"
            :signals ["Outsider ideas ignored" "Innovation stagnation"]
            :safeguards ["Beginner's mind" "Cross-pollination" "Diverse perspectives"])
   (failure "meritocracy-without-safety" "high"
            "Brutal honesty without psychological safety"
            :signals ["Fear" "Reduced participation"]
            :safeguards ["Respect" "Learning intent" "Supportive challenge"])]})

(register-model
 {:name "believability-weighted-decision-making"
  :category "decision_theory"
  :originator "Ray Dalio"
  :description "Weight opinions by track record and expertise"
  :key-insight "Not all opinions are equal; weight by demonstrated competence"
  :application "Calculate believability scores, weight votes accordingly"
  :failure-modes
  [(failure "track-record-overfitting" "high"
            "Past success doesn't guarantee future success"
            :signals ["Experts failing" "Context changed"]
            :safeguards ["Context awareness" "Decay old data" "Regime detection"])
   (failure "narrow-expertise" "medium"
            "Expert in one domain, weighted in all"
            :signals ["Poor cross-domain decisions" "Halo effect"]
            :safeguards ["Domain-specific weights" "Expertise boundaries" "Humility"])
   (failure "newcomer-discount" "medium"
            "New people have no weight"
            :signals ["Fresh perspectives ignored" "Innovation loss"]
            :safeguards ["Starter credibility" "Idea evaluation independent of source" "Fast credibility building"])
   (failure "weight-gaming" "high"
            "People optimize for weight, not truth"
            :signals ["Political behavior" "Safe predictions"]
            :safeguards ["Outcome tracking" "Calibration scoring" "Intellectual honesty rewards"])
   (failure "groupthink-by-weight" "high"
            "High-weight people converge, suppress dissent"
            :signals ["Uniform views" "Surprises"]
            :safeguards ["Devil's advocate" "Diversity" "Contrarian bonus"])]})

;; ============================================