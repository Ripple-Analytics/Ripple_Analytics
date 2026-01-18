(ns mental-models.analysis-test
  "Tests for the analysis module.
   
   Tests latticework analysis, lollapalooza detection, inversion,
   two-track analysis, and bias detection."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.analysis :as analysis]))

;; ============================================
;; Latticework Analysis Tests
;; ============================================

(deftest test-analyze-with-model
  (testing "Analyze with a single model"
    (let [model {:name "Circle of Competence"
                 :category "Core"
                 :description "Stay within your area of expertise"
                 :key-insight "Know what you know and what you don't"
                 :failure-modes [{:name "Overreach"
                                  :signals ["expert" "know everything"]
                                  :severity "high"
                                  :description "Venturing outside competence"}]}
          context "I'm an expert in this field and know everything about it"
          result (analysis/analyze-with-model model context)]
      (is (= "Circle of Competence" (:model result)))
      (is (= "Core" (:category result)))
      (is (true? (:applicable result)))
      (is (number? (:confidence result)))
      (is (<= 0 (:confidence result) 1))
      (is (vector? (:insights result))))))

(deftest test-latticework-analyze
  (testing "Latticework analysis with multiple models"
    (let [result (analysis/latticework-analyze [] "Should I invest in this company?")]
      (is (map? result))
      (is (contains? result :context))
      (is (contains? result :models-applied))
      (is (contains? result :individual-analyses))
      (is (contains? result :combined-confidence))
      (is (contains? result :recommendation))
      (is (number? (:combined-confidence result)))
      (is (map? (:recommendation result)))
      (is (contains? (:recommendation result) :type))
      (is (contains? (:recommendation result) :message)))))

(deftest test-latticework-recommendation-types
  (testing "Recommendation types based on confidence"
    (let [result (analysis/latticework-analyze [] "test context")]
      (is (#{:strong :moderate :weak} (keyword (:type (:recommendation result))))))))

;; ============================================
;; Lollapalooza Detection Tests
;; ============================================

(deftest test-calculate-lollapalooza-multiplier
  (testing "Lollapalooza multiplier calculation"
    (is (= 1.0 (analysis/calculate-lollapalooza-multiplier 0)))
    (is (= 1.0 (analysis/calculate-lollapalooza-multiplier 1)))
    (is (= 1.5 (analysis/calculate-lollapalooza-multiplier 2)))
    (is (= 2.0 (analysis/calculate-lollapalooza-multiplier 3)))
    (is (= 2.5 (analysis/calculate-lollapalooza-multiplier 4)))
    (is (= 3.0 (analysis/calculate-lollapalooza-multiplier 5)))
    (is (= 3.0 (analysis/calculate-lollapalooza-multiplier 10)))))

(deftest test-detect-lollapalooza
  (testing "Lollapalooza detection"
    (let [result (analysis/detect-lollapalooza [] "test context")]
      (is (map? result))
      (is (contains? result :is-lollapalooza))
      (is (contains? result :models-aligned))
      (is (contains? result :strength))
      (is (contains? result :effect-multiplier))
      (is (contains? result :recommendation))
      (is (boolean? (:is-lollapalooza result)))
      (is (number? (:models-aligned result)))
      (is (number? (:strength result)))
      (is (number? (:effect-multiplier result)))
      (is (string? (:recommendation result))))))

;; ============================================
;; Inversion Tests
;; ============================================

(deftest test-invert
  (testing "Inversion technique"
    (let [problem "How to build a successful startup"
          result (analysis/invert problem)]
      (is (map? result))
      (is (= problem (:original-problem result)))
      (is (vector? (:inverted-questions result)))
      (is (= 5 (count (:inverted-questions result))))
      (is (every? string? (:inverted-questions result)))
      (is (vector? (:common-failure-categories result)))
      (is (string? (:approach result)))
      (is (string? (:munger-quote result)))
      (is (some #(clojure.string/includes? % "failure") (:inverted-questions result))))))

(deftest test-invert-question-content
  (testing "Inverted questions contain problem reference"
    (let [problem "launching a new product"
          result (analysis/invert problem)]
      (is (every? #(clojure.string/includes? % problem) (:inverted-questions result))))))

;; ============================================
;; Two-Track Analysis Tests
;; ============================================

(deftest test-two-track-analysis
  (testing "Two-track analysis structure"
    (let [situation "Considering a major acquisition"
          result (analysis/two-track-analysis situation)]
      (is (map? result))
      (is (= situation (:situation result)))
      (is (map? (:track-1-rational result)))
      (is (map? (:track-2-psychological result)))
      (is (vector? (:integration-questions result))))))

(deftest test-two-track-rational-factors
  (testing "Track 1 rational factors"
    (let [result (analysis/two-track-analysis "test")]
      (is (contains? (:track-1-rational result) :economic-incentives))
      (is (contains? (:track-1-rational result) :opportunity-costs))
      (is (contains? (:track-1-rational result) :second-order-effects))
      (is (contains? (:track-1-rational result) :margin-of-safety)))))

(deftest test-two-track-psychological-factors
  (testing "Track 2 psychological factors"
    (let [result (analysis/two-track-analysis "test")]
      (is (contains? (:track-2-psychological result) :social-proof))
      (is (contains? (:track-2-psychological result) :commitment-consistency))
      (is (contains? (:track-2-psychological result) :incentive-caused-bias))
      (is (contains? (:track-2-psychological result) :confirmation-bias))
      (is (contains? (:track-2-psychological result) :loss-aversion)))))

;; ============================================
;; Bias Detection Tests
;; ============================================

(deftest test-bias-patterns-defined
  (testing "Bias patterns are defined"
    (is (map? analysis/bias-patterns))
    (is (contains? analysis/bias-patterns :confirmation-bias))
    (is (contains? analysis/bias-patterns :availability-bias))
    (is (contains? analysis/bias-patterns :anchoring))
    (is (contains? analysis/bias-patterns :social-proof))
    (is (contains? analysis/bias-patterns :overconfidence))))

(deftest test-detect-biases-no-bias
  (testing "Detect biases - no bias present"
    (let [result (analysis/detect-biases "The weather is nice today")]
      (is (map? result))
      (is (contains? result :biases-detected))
      (is (contains? result :total-biases))
      (is (contains? result :risk-level))
      (is (= "low" (:risk-level result))))))

(deftest test-detect-biases-confirmation-bias
  (testing "Detect confirmation bias"
    (let [result (analysis/detect-biases "This confirms my belief that I was right all along")]
      (is (>= (:total-biases result) 1))
      (is (some #(= "confirmation-bias" (:bias %)) (:biases-detected result))))))

(deftest test-detect-biases-overconfidence
  (testing "Detect overconfidence bias"
    (let [result (analysis/detect-biases "This is definitely going to work, guaranteed success, no doubt about it")]
      (is (>= (:total-biases result) 1))
      (is (some #(= "overconfidence" (:bias %)) (:biases-detected result))))))

(deftest test-detect-biases-social-proof
  (testing "Detect social proof bias"
    (let [result (analysis/detect-biases "Everyone is doing it, it's very popular and trending")]
      (is (>= (:total-biases result) 1))
      (is (some #(= "social-proof" (:bias %)) (:biases-detected result))))))

(deftest test-detect-biases-risk-levels
  (testing "Risk level calculation"
    (let [low-risk (analysis/detect-biases "Normal text without bias")
          medium-risk (analysis/detect-biases "This confirms my belief")
          high-risk (analysis/detect-biases "Everyone knows this is definitely true, as expected, trending")]
      (is (= "low" (:risk-level low-risk)))
      (is (#{"medium" "high"} (:risk-level medium-risk)))
      (is (#{"medium" "high"} (:risk-level high-risk))))))

;; ============================================
;; Decision Checklist Tests
;; ============================================

(deftest test-decision-checklist
  (testing "Decision checklist generation"
    (let [result (analysis/decision-checklist "Buy company X" {:context "M&A decision"})]
      (is (map? result))
      (is (= "Buy company X" (:decision result)))
      (is (vector? (:checklist result)))
      (is (>= (count (:checklist result)) 5)))))

(deftest test-decision-checklist-categories
  (testing "Decision checklist has required categories"
    (let [result (analysis/decision-checklist "test" {})
          categories (set (map :category (:checklist result)))]
      (is (contains? categories "Circle of Competence"))
      (is (contains? categories "Inversion"))
      (is (contains? categories "Second-Order Effects"))
      (is (contains? categories "Incentives"))
      (is (contains? categories "Margin of Safety")))))

(deftest test-decision-checklist-questions
  (testing "Each category has questions"
    (let [result (analysis/decision-checklist "test" {})]
      (is (every? #(vector? (:questions %)) (:checklist result)))
      (is (every? #(>= (count (:questions %)) 2) (:checklist result))))))

;; ============================================
;; Comprehensive Analysis Tests
;; ============================================

(deftest test-analyze-comprehensive
  (testing "Comprehensive analysis combines all techniques"
    (let [context {:situation "Considering investment in tech startup"
                   :decision "Invest $1M"}
          result (analysis/analyze-comprehensive [] context)]
      (is (map? result))
      (is (contains? result :latticework-analysis))
      (is (contains? result :lollapalooza-detection))
      (is (contains? result :inversion-analysis))
      (is (contains? result :two-track-analysis))
      (is (contains? result :bias-detection))
      (is (contains? result :decision-checklist)))))

(deftest test-analyze-comprehensive-structure
  (testing "Comprehensive analysis has correct structure"
    (let [result (analysis/analyze-comprehensive [] {:situation "test" :decision "test"})]
      (is (map? (:latticework-analysis result)))
      (is (map? (:lollapalooza-detection result)))
      (is (map? (:inversion-analysis result)))
      (is (map? (:two-track-analysis result)))
      (is (map? (:bias-detection result)))
      (is (map? (:decision-checklist result))))))

;; ============================================
;; Edge Cases
;; ============================================

(deftest test-empty-input-handling
  (testing "Handle empty inputs gracefully"
    (is (map? (analysis/detect-biases "")))
    (is (map? (analysis/detect-biases nil)))
    (is (map? (analysis/invert "")))
    (is (map? (analysis/two-track-analysis "")))
    (is (map? (analysis/latticework-analyze [] "")))))

(deftest test-long-input-handling
  (testing "Handle long inputs"
    (let [long-text (apply str (repeat 1000 "This is a test sentence. "))
          result (analysis/detect-biases long-text)]
      (is (map? result))
      (is (<= (count (:text-analyzed result)) 200)))))
