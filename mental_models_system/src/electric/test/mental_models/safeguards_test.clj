(ns mental-models.safeguards-test
  "Tests for the safeguards module.
   
   Tests failure mode detection, safeguard recommendations,
   and risk assessment."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.safeguards :as safeguards]))

;; ============================================
;; Failure Mode Categories Tests
;; ============================================

(deftest test-failure-mode-categories-defined
  (testing "Failure mode categories are defined"
    (is (map? safeguards/failure-mode-categories))
    (is (contains? safeguards/failure-mode-categories :cognitive))
    (is (contains? safeguards/failure-mode-categories :social))
    (is (contains? safeguards/failure-mode-categories :emotional))
    (is (contains? safeguards/failure-mode-categories :analytical))
    (is (contains? safeguards/failure-mode-categories :structural))))

(deftest test-failure-mode-categories-content
  (testing "Each category has failure modes"
    (is (vector? (:cognitive safeguards/failure-mode-categories)))
    (is (>= (count (:cognitive safeguards/failure-mode-categories)) 3))
    (is (some #(= "confirmation-bias" %) (:cognitive safeguards/failure-mode-categories)))
    (is (some #(= "social-proof" %) (:social safeguards/failure-mode-categories)))
    (is (some #(= "loss-aversion" %) (:emotional safeguards/failure-mode-categories)))))

;; ============================================
;; Severity Levels Tests
;; ============================================

(deftest test-severity-levels-defined
  (testing "Severity levels are defined"
    (is (map? safeguards/severity-levels))
    (is (contains? safeguards/severity-levels :critical))
    (is (contains? safeguards/severity-levels :high))
    (is (contains? safeguards/severity-levels :medium))
    (is (contains? safeguards/severity-levels :low))
    (is (contains? safeguards/severity-levels :minimal))))

(deftest test-severity-levels-ordering
  (testing "Severity levels have correct ordering"
    (is (> (get-in safeguards/severity-levels [:critical :level])
           (get-in safeguards/severity-levels [:high :level])))
    (is (> (get-in safeguards/severity-levels [:high :level])
           (get-in safeguards/severity-levels [:medium :level])))
    (is (> (get-in safeguards/severity-levels [:medium :level])
           (get-in safeguards/severity-levels [:low :level])))
    (is (> (get-in safeguards/severity-levels [:low :level])
           (get-in safeguards/severity-levels [:minimal :level])))))

;; ============================================
;; Failure Mode Signals Tests
;; ============================================

(deftest test-failure-mode-signals-defined
  (testing "Failure mode signals are defined"
    (is (map? safeguards/failure-mode-signals))
    (is (contains? safeguards/failure-mode-signals :confirmation-bias))
    (is (contains? safeguards/failure-mode-signals :overconfidence))
    (is (contains? safeguards/failure-mode-signals :sunk-cost-fallacy))))

(deftest test-failure-mode-signals-structure
  (testing "Each failure mode signal has required fields"
    (doseq [[mode config] safeguards/failure-mode-signals]
      (is (contains? config :signals) (str mode " missing :signals"))
      (is (contains? config :severity) (str mode " missing :severity"))
      (is (contains? config :mitigation) (str mode " missing :mitigation"))
      (is (vector? (:signals config)) (str mode " :signals not a vector"))
      (is (keyword? (:severity config)) (str mode " :severity not a keyword"))
      (is (string? (:mitigation config)) (str mode " :mitigation not a string")))))

;; ============================================
;; Failure Mode Detection Tests
;; ============================================

(deftest test-detect-failure-mode-confirmation-bias
  (testing "Detect confirmation bias"
    (let [result (safeguards/detect-failure-mode 
                   "This confirms my belief that I was right" 
                   :confirmation-bias)]
      (is (map? result))
      (is (= :confirmation-bias (:failure-mode result)))
      (is (>= (:match-count result) 1))
      (is (= :high (:severity result)))
      (is (string? (:mitigation result))))))

(deftest test-detect-failure-mode-overconfidence
  (testing "Detect overconfidence"
    (let [result (safeguards/detect-failure-mode 
                   "This is definitely going to work, guaranteed" 
                   :overconfidence)]
      (is (map? result))
      (is (= :overconfidence (:failure-mode result)))
      (is (= :critical (:severity result))))))

(deftest test-detect-failure-mode-no-match
  (testing "No detection when signals not present"
    (let [result (safeguards/detect-failure-mode 
                   "The weather is nice today" 
                   :confirmation-bias)]
      (is (nil? result)))))

(deftest test-detect-all-failure-modes
  (testing "Detect all failure modes in text"
    (let [result (safeguards/detect-all-failure-modes 
                   "This confirms my belief and everyone agrees it's definitely true")]
      (is (map? result))
      (is (contains? result :failure-modes-detected))
      (is (contains? result :total-detected))
      (is (contains? result :risk-level))
      (is (>= (:total-detected result) 1)))))

(deftest test-detect-all-failure-modes-empty
  (testing "Handle text with no failure modes"
    (let [result (safeguards/detect-all-failure-modes "Normal text")]
      (is (= 0 (:total-detected result)))
      (is (= :none (:risk-level result))))))

(deftest test-detect-all-failure-modes-risk-levels
  (testing "Risk levels based on severity"
    (let [critical (safeguards/detect-all-failure-modes "definitely guaranteed 100%")
          high (safeguards/detect-all-failure-modes "confirms my belief")
          none (safeguards/detect-all-failure-modes "neutral text")]
      (is (= :critical (:risk-level critical)))
      (is (#{:high :medium} (:risk-level high)))
      (is (= :none (:risk-level none))))))

;; ============================================
;; Safeguards Tests
;; ============================================

(deftest test-safeguards-defined
  (testing "Safeguards are defined for each category"
    (is (map? safeguards/safeguards))
    (is (contains? safeguards/safeguards :cognitive))
    (is (contains? safeguards/safeguards :social))
    (is (contains? safeguards/safeguards :emotional))
    (is (contains? safeguards/safeguards :analytical))
    (is (contains? safeguards/safeguards :structural))))

(deftest test-safeguards-structure
  (testing "Each safeguard has required fields"
    (doseq [[category safeguard-list] safeguards/safeguards]
      (is (vector? safeguard-list) (str category " safeguards not a vector"))
      (doseq [sg safeguard-list]
        (is (contains? sg :name) (str category " safeguard missing :name"))
        (is (contains? sg :description) (str category " safeguard missing :description"))
        (is (contains? sg :when-to-use) (str category " safeguard missing :when-to-use"))))))

(deftest test-get-safeguards-for-failure-mode
  (testing "Get safeguards for a failure mode"
    (let [result (safeguards/get-safeguards-for-failure-mode :confirmation-bias)]
      (is (map? result))
      (is (= :confirmation-bias (:failure-mode result)))
      (is (keyword? (:category result)))
      (is (vector? (:safeguards result)))
      (is (string? (:specific-mitigation result))))))

;; ============================================
;; Safeguard Report Tests
;; ============================================

(deftest test-generate-safeguard-report
  (testing "Generate safeguard report"
    (let [result (safeguards/generate-safeguard-report 
                   "This confirms my belief and is definitely true")]
      (is (map? result))
      (is (contains? result :detection))
      (is (contains? result :safeguard-recommendations))
      (is (contains? result :priority-actions))
      (is (contains? result :summary)))))

(deftest test-generate-safeguard-report-summary
  (testing "Safeguard report summary structure"
    (let [result (safeguards/generate-safeguard-report "test text")
          summary (:summary result)]
      (is (map? summary))
      (is (contains? summary :total-risks))
      (is (contains? summary :risk-level))
      (is (contains? summary :categories-affected)))))

;; ============================================
;; Risk Assessment Tests
;; ============================================

(deftest test-calculate-risk-score
  (testing "Calculate risk score from failure modes"
    (let [failure-modes [{:severity :critical}
                         {:severity :high}
                         {:severity :medium}]
          result (safeguards/calculate-risk-score failure-modes)]
      (is (map? result))
      (is (contains? result :raw-score))
      (is (contains? result :normalized-score))
      (is (contains? result :risk-level))
      (is (contains? result :failure-mode-count))
      (is (number? (:raw-score result)))
      (is (number? (:normalized-score result)))
      (is (= 3 (:failure-mode-count result))))))

(deftest test-calculate-risk-score-empty
  (testing "Calculate risk score with no failure modes"
    (let [result (safeguards/calculate-risk-score [])]
      (is (= 0 (:raw-score result)))
      (is (= 0 (:failure-mode-count result)))
      (is (= :none (:risk-level result))))))

(deftest test-assess-decision-risk
  (testing "Assess decision risk"
    (let [result (safeguards/assess-decision-risk 
                   "Invest everything" 
                   "This is definitely going to work")]
      (is (map? result))
      (is (contains? result :decision))
      (is (contains? result :context))
      (is (contains? result :detection))
      (is (contains? result :risk-score))
      (is (contains? result :recommendation)))))

(deftest test-assess-decision-risk-recommendations
  (testing "Decision risk recommendations"
    (let [critical (safeguards/assess-decision-risk 
                     "test" 
                     "definitely guaranteed 100% can't lose")
          safe (safeguards/assess-decision-risk 
                 "test" 
                 "normal decision context")]
      (is (clojure.string/includes? (:recommendation critical) "STOP"))
      (is (or (clojure.string/includes? (:recommendation safe) "PROCEED")
              (clojure.string/includes? (:recommendation safe) "REVIEW"))))))

;; ============================================
;; Model-Specific Failure Modes Tests
;; ============================================

(deftest test-get-model-failure-modes
  (testing "Get failure modes for a model"
    (let [result (safeguards/get-model-failure-modes "Circle of Competence")]
      (is (map? result))
      (is (or (contains? result :failure-modes)
              (contains? result :error))))))

(deftest test-get-model-failure-modes-not-found
  (testing "Handle model not found"
    (let [result (safeguards/get-model-failure-modes "NonexistentModel")]
      (is (contains? result :error)))))

(deftest test-check-model-failure-modes
  (testing "Check model failure modes in context"
    (let [result (safeguards/check-model-failure-modes 
                   "Circle of Competence" 
                   "I'm an expert in everything")]
      (is (map? result))
      (is (or (contains? result :active-failure-modes)
              (contains? result :error))))))

;; ============================================
;; Comprehensive Safeguard Analysis Tests
;; ============================================

(deftest test-comprehensive-safeguard-analysis
  (testing "Comprehensive safeguard analysis"
    (let [result (safeguards/comprehensive-safeguard-analysis 
                   "Major investment decision"
                   "This is definitely going to work"
                   ["Circle of Competence"])]
      (is (map? result))
      (is (contains? result :decision))
      (is (contains? result :context))
      (is (contains? result :models-analyzed))
      (is (contains? result :text-analysis))
      (is (contains? result :model-analyses))
      (is (contains? result :combined-risk))
      (is (contains? result :final-recommendation)))))

(deftest test-comprehensive-safeguard-analysis-recommendation
  (testing "Comprehensive analysis final recommendation"
    (let [result (safeguards/comprehensive-safeguard-analysis 
                   "test" "test" [])]
      (is (map? (:final-recommendation result)))
      (is (contains? (:final-recommendation result) :action))
      (is (contains? (:final-recommendation result) :message)))))

;; ============================================
;; Edge Cases
;; ============================================

(deftest test-empty-input-handling
  (testing "Handle empty inputs gracefully"
    (is (map? (safeguards/detect-all-failure-modes "")))
    (is (map? (safeguards/detect-all-failure-modes nil)))
    (is (map? (safeguards/generate-safeguard-report "")))
    (is (map? (safeguards/calculate-risk-score [])))))

(deftest test-long-input-handling
  (testing "Handle long inputs"
    (let [long-text (apply str (repeat 1000 "This is a test. "))
          result (safeguards/detect-all-failure-modes long-text)]
      (is (map? result))
      (is (<= (count (:text-analyzed result)) 200)))))

(deftest test-case-insensitivity
  (testing "Detection is case insensitive"
    (let [lower (safeguards/detect-failure-mode "confirms my belief" :confirmation-bias)
          upper (safeguards/detect-failure-mode "CONFIRMS MY BELIEF" :confirmation-bias)
          mixed (safeguards/detect-failure-mode "Confirms My Belief" :confirmation-bias)]
      (is (some? lower))
      (is (some? upper))
      (is (some? mixed)))))
