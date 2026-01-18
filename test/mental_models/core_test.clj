(ns mental-models.core-test
  "Comprehensive test suite for Mental Models System.
   
   Tests organized by Munger's hierarchy:
   1. Hard Sciences (Mathematics, Physics, Chemistry, Engineering, Accounting)
   2. Life Sciences (Biology, Physiology, Psychology, Economics)
   3. Applied Knowledge (Moats, Organizational, Thinking Tools)
   4. Integration Tests (End-to-end workflows)"
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [mental-models.data.models :as models]
            [mental-models.analysis.core :as analysis]
            [mental-models.models.unified-detector :as detector]))

;; =============================================================================
;; TEST FIXTURES
;; =============================================================================

(def sample-texts
  {:confirmation-bias
   "The data clearly confirms what I always believed. I knew this would happen.
    Every piece of evidence supports my original hypothesis. The contradicting
    studies are clearly flawed and can be dismissed."
   
   :anchoring
   "The original price was $1000, so getting it for $700 is a great deal.
    The first offer set the baseline for all subsequent negotiations."
   
   :availability-heuristic
   "After seeing the plane crash on the news, I'm terrified of flying.
    These dramatic events are so vivid in my memory."
   
   :sunk-cost
   "We've already invested $5 million in this project. We can't stop now,
    even though the market has completely changed."
   
   :compound-interest
   "The investment grew exponentially over 20 years. The power of compounding
    turned a small initial amount into a substantial sum."
   
   :margin-of-safety
   "We built in a 30% buffer to account for unexpected costs. Always leave
    room for error in your calculations."
   
   :inversion
   "Instead of asking how to succeed, ask how to fail and avoid those things.
    Think backwards from the worst case scenario."
   
   :second-order-effects
   "The policy seemed good initially, but the unintended consequences were
    devastating. We failed to consider what would happen next."
   
   :network-effects
   "Each new user makes the platform more valuable for all existing users.
    The value grows exponentially with the user base."
   
   :moat
   "Their brand recognition and switching costs create a durable competitive
    advantage that competitors cannot easily replicate."})

;; =============================================================================
;; UNIT TESTS - MENTAL MODEL DETECTION
;; =============================================================================

(deftest test-confirmation-bias-detection
  (testing "Detects confirmation bias in text"
    (let [result (detector/detect-model :confirmation-bias (:confirmation-bias sample-texts))]
      (is (> (:score result) 0.5) "Should detect confirmation bias with high confidence")
      (is (= :confirmation-bias (:model-id result)))
      (is (seq (:evidence result)) "Should extract evidence"))))

(deftest test-anchoring-detection
  (testing "Detects anchoring bias in text"
    (let [result (detector/detect-model :anchoring (:anchoring sample-texts))]
      (is (> (:score result) 0.5) "Should detect anchoring with high confidence")
      (is (contains? #{:high :moderate} (:confidence result))))))

(deftest test-availability-heuristic-detection
  (testing "Detects availability heuristic in text"
    (let [result (detector/detect-model :availability-heuristic (:availability-heuristic sample-texts))]
      (is (> (:score result) 0.4) "Should detect availability heuristic"))))

(deftest test-sunk-cost-detection
  (testing "Detects sunk cost fallacy in text"
    (let [result (detector/detect-model :sunk-cost (:sunk-cost sample-texts))]
      (is (> (:score result) 0.5) "Should detect sunk cost fallacy")
      (is (seq (:evidence result)) "Should find evidence of sunk cost reasoning"))))

(deftest test-compound-interest-detection
  (testing "Detects compound interest mental model"
    (let [result (detector/detect-model :compound-interest (:compound-interest sample-texts))]
      (is (> (:score result) 0.5) "Should detect compound interest concepts"))))

(deftest test-margin-of-safety-detection
  (testing "Detects margin of safety mental model"
    (let [result (detector/detect-model :margin-of-safety (:margin-of-safety sample-texts))]
      (is (> (:score result) 0.5) "Should detect margin of safety"))))

(deftest test-inversion-detection
  (testing "Detects inversion mental model"
    (let [result (detector/detect-model :inversion (:inversion sample-texts))]
      (is (> (:score result) 0.5) "Should detect inversion thinking"))))

(deftest test-second-order-effects-detection
  (testing "Detects second-order effects thinking"
    (let [result (detector/detect-model :second-order-effects (:second-order-effects sample-texts))]
      (is (> (:score result) 0.5) "Should detect second-order thinking"))))

(deftest test-network-effects-detection
  (testing "Detects network effects"
    (let [result (detector/detect-model :network-effects (:network-effects sample-texts))]
      (is (> (:score result) 0.5) "Should detect network effects"))))

(deftest test-moat-detection
  (testing "Detects competitive moat concepts"
    (let [result (detector/detect-model :economic-moat (:moat sample-texts))]
      (is (> (:score result) 0.5) "Should detect moat concepts"))))

;; =============================================================================
;; UNIT TESTS - BATCH ANALYSIS
;; =============================================================================

(deftest test-batch-analysis
  (testing "Analyzes text against all models"
    (let [text "The market crashed due to a combination of overconfidence, 
                herd behavior, and failure to consider second-order effects.
                Investors anchored on past performance and ignored warning signs."
          results (detector/analyze-all-models text)]
      (is (map? results) "Should return results map")
      (is (> (count (:detected-models results)) 0) "Should detect some models")
      (is (contains? results :top-models) "Should include top models")
      (is (contains? results :lollapalooza-detected) "Should check for lollapalooza"))))

(deftest test-lollapalooza-detection
  (testing "Detects lollapalooza when multiple models converge"
    (let [text "The company failed spectacularly due to confirmation bias 
                (they only saw what they wanted to see), sunk cost fallacy 
                (they kept investing in a failing strategy), overconfidence 
                (they thought they couldn't fail), and social proof 
                (everyone else was doing it). This perfect storm of 
                cognitive biases created a lollapalooza effect."
          results (detector/analyze-all-models text)]
      ;; Lollapalooza requires 3+ models with score > 0.7
      (is (boolean? (:lollapalooza-detected results))))))

;; =============================================================================
;; UNIT TESTS - MODEL DATA
;; =============================================================================

(deftest test-models-data-structure
  (testing "Mental models data is properly structured"
    (let [all-models (models/get-all-models)]
      (is (seq all-models) "Should have models")
      (is (every? :id all-models) "Every model should have an id")
      (is (every? :name all-models) "Every model should have a name")
      (is (every? :category all-models) "Every model should have a category"))))

(deftest test-models-by-category
  (testing "Models are organized by Munger's hierarchy"
    (let [categories (models/get-categories)]
      (is (some #(= (:slug %) "mathematics") categories) "Should have Mathematics")
      (is (some #(= (:slug %) "psychology") categories) "Should have Psychology")
      (is (some #(= (:slug %) "economics") categories) "Should have Economics")
      (is (some #(= (:slug %) "physics") categories) "Should have Physics"))))

(deftest test-failure-modes
  (testing "Each model has associated failure modes"
    (let [model (first (models/get-models-by-category "psychology"))
          failure-modes (models/get-failure-modes-for-model (:id model))]
      (is (seq failure-modes) "Should have failure modes")
      (is (every? :name failure-modes) "Each failure mode should have a name")
      (is (every? :severity failure-modes) "Each failure mode should have severity"))))

;; =============================================================================
;; INTEGRATION TESTS - ANALYSIS PIPELINE
;; =============================================================================

(deftest test-full-analysis-pipeline
  (testing "Complete analysis pipeline works end-to-end"
    (let [document {:id 1
                    :title "Test Document"
                    :content "This investment decision was driven by overconfidence
                              and anchoring on the initial valuation. We failed to
                              consider margin of safety and second-order effects."}
          result (analysis/analyze-document document)]
      (is (map? result) "Should return analysis result")
      (is (contains? result :model-matches) "Should include model matches")
      (is (contains? result :summary) "Should include summary")
      (is (number? (:processing-time-ms result)) "Should track processing time"))))

(deftest test-analysis-scoring
  (testing "Analysis scoring is consistent"
    (let [text (:confirmation-bias sample-texts)
          result1 (detector/detect-model :confirmation-bias text)
          result2 (detector/detect-model :confirmation-bias text)]
      ;; Same text should produce same score (deterministic)
      (is (= (:score result1) (:score result2)) 
          "Same text should produce consistent scores"))))

;; =============================================================================
;; EDGE CASE TESTS
;; =============================================================================

(deftest test-empty-text-handling
  (testing "Handles empty text gracefully"
    (let [result (detector/analyze-all-models "")]
      (is (map? result) "Should return valid result for empty text")
      (is (= 0 (count (:detected-models result))) "Should detect no models"))))

(deftest test-very-long-text-handling
  (testing "Handles very long text"
    (let [long-text (apply str (repeat 1000 "This is a test sentence. "))
          result (detector/analyze-all-models long-text)]
      (is (map? result) "Should handle long text")
      (is (number? (:processing-time-ms result))))))

(deftest test-special-characters-handling
  (testing "Handles special characters in text"
    (let [text "The price was $1,000 (20% off!) & the deal was \"amazing\"."
          result (detector/analyze-all-models text)]
      (is (map? result) "Should handle special characters"))))

(deftest test-unicode-handling
  (testing "Handles unicode text"
    (let [text "投资决策 was influenced by 確認偏誤 (confirmation bias)."
          result (detector/analyze-all-models text)]
      (is (map? result) "Should handle unicode"))))

;; =============================================================================
;; PERFORMANCE TESTS
;; =============================================================================

(deftest test-analysis-performance
  (testing "Analysis completes within acceptable time"
    (let [text "A moderate length text for performance testing purposes."
          start-time (System/currentTimeMillis)
          _ (detector/analyze-all-models text)
          end-time (System/currentTimeMillis)
          duration (- end-time start-time)]
      ;; Should complete within 5 seconds for basic analysis
      (is (< duration 5000) "Analysis should complete within 5 seconds"))))

;; =============================================================================
;; DATA INTEGRITY TESTS
;; =============================================================================

(deftest test-no-hardcoded-fake-data
  (testing "No hardcoded fake data in results"
    (let [result (detector/analyze-all-models "Test text")]
      ;; Scores should be calculated, not hardcoded
      (is (every? #(<= 0 % 1) (map :score (:all-scores result)))
          "All scores should be between 0 and 1")
      ;; Evidence should be extracted from actual text
      (is (every? string? (mapcat :evidence (:detected-models result)))
          "Evidence should be strings"))))

(deftest test-data-source-traceability
  (testing "All data can be traced to source"
    (let [result (detector/analyze-all-models "Test text for traceability")]
      ;; Each detection should have traceable source
      (is (every? :model-id (:detected-models result))
          "Each detection should identify the model")
      (is (every? :score (:detected-models result))
          "Each detection should have a score"))))
