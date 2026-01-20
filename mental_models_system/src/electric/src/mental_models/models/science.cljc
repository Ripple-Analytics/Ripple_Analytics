(ns mental-models.models.science
  "Mental Models - Science Category"
  (:require [mental-models.models.core :refer [register-model failure]]))

;; Category: Science
;; ============================================

(register-model
 {:name "falsifiability"
  :category "science"
  :originator "Karl Popper"
  :description "Scientific theories must be testable and potentially disprovable"
  :key-insight "If nothing could prove it wrong, it's not scientific"
  :application "Seek to disprove, not confirm, your hypotheses"
  :failure-modes
  [(failure "unfalsifiable-beliefs" "high"
            "Holding beliefs that can't be tested"
            :signals ["No possible disconfirmation" "Moving goalposts"]
            :safeguards ["Define what would disprove" "Testable predictions"])
   (failure "confirmation-seeking" "high"
            "Only looking for confirming evidence"
            :signals ["Cherry-picking data" "Ignoring disconfirmation"]
            :safeguards ["Seek disconfirmation" "Pre-register predictions"])
   (failure "ad-hoc-rescue" "medium"
            "Adding exceptions to save theory"
            :signals ["Increasingly complex explanations"]
            :safeguards ["Occam's razor" "Accept falsification"])
   (failure "over-falsification" "medium"
            "Abandoning theories too quickly"
            :signals ["No persistent beliefs" "Excessive skepticism"]
            :safeguards ["Accumulate evidence" "Bayesian updating"])
   (failure "practical-unfalsifiability" "medium"
            "Theories that are technically but not practically testable"
            :signals ["Tests too expensive or long"]
            :safeguards ["Practical test design" "Proxy measures"])]})

(register-model
 {:name "replication"
  :category "science"
  :originator "Scientific Method"
  :description "Results must be reproducible by others"
  :key-insight "If it can't be replicated, it might not be real"
  :application "Verify important findings through replication"
  :failure-modes
  [(failure "single-study-belief" "high"
            "Believing results from one study"
            :signals ["No replication" "Surprising findings accepted"]
            :safeguards ["Wait for replication" "Meta-analyses"])
   (failure "publication-bias" "high"
            "Only positive results published"
            :signals ["File drawer problem" "Inflated effect sizes"]
            :safeguards ["Seek null results" "Pre-registration"])
   (failure "replication-neglect" "medium"
            "Not attempting replication"
            :signals ["No verification" "Building on shaky foundations"]
            :safeguards ["Replicate key findings" "Verify before building"])
   (failure "exact-replication-fallacy" "medium"
            "Expecting exact replication"
            :signals ["Dismissing close replications"]
            :safeguards ["Conceptual replication" "Effect size focus"])
   (failure "replication-crisis-overreaction" "medium"
            "Dismissing all research"
            :signals ["Excessive skepticism" "Paralysis"]
            :safeguards ["Calibrated trust" "Evidence accumulation"])]})

(register-model
 {:name "survivorship-bias"
  :category "science"
  :originator "Abraham Wald"
  :description "Focusing on survivors while ignoring those who didn't make it"
  :key-insight "The dead can't tell their stories"
  :application "Always ask: What am I not seeing?"
  :failure-modes
  [(failure "success-only-analysis" "high"
            "Only studying successes"
            :signals ["Learning from winners only" "Ignoring failures"]
            :safeguards ["Study failures" "Include non-survivors"])
   (failure "visible-success-bias" "high"
            "Overweighting visible successes"
            :signals ["Media-driven beliefs" "Lottery winner focus"]
            :safeguards ["Base rates" "Full population analysis"])
   (failure "advice-from-survivors" "high"
            "Taking advice only from survivors"
            :signals ["Survivor tips" "Ignoring luck"]
            :safeguards ["Consider luck" "Failure analysis"])
   (failure "selection-blindness" "medium"
            "Not seeing selection effects"
            :signals ["Surprised by attrition" "Missing dropout patterns"]
            :safeguards ["Track full cohort" "Intention to treat"])
   (failure "reverse-survivorship" "medium"
            "Overweighting failures"
            :signals ["Excessive pessimism" "Missing success patterns"]
            :safeguards ["Balance success and failure analysis"])]})

;; ============================================