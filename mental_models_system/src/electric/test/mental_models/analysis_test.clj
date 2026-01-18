(ns mental-models.analysis-test
  "Tests for analysis functions"
  (:require [clojure.test :refer [deftest is testing]]
            [mental-models.analysis :as analysis]))

(deftest test-latticework-analyze
  (testing "Latticework analysis"
    (let [result (analysis/latticework-analyze 
                  ["circle-of-competence" "margin-of-safety"]
                  "Should I invest in a tech startup outside my expertise?")]
      (is (map? result) "Should return a map")
      (is (contains? result :models-applied) "Should have models-applied")
      (is (contains? result :individual-analyses) "Should have individual-analyses")
      (is (number? (:models-applied result)) "models-applied should be a number")
      (is (= 2 (:models-applied result)) "Should apply 2 models"))))

(deftest test-lollapalooza-detect
  (testing "Lollapalooza effect detection"
    (let [text "This company has strong network effects, high switching costs, and economies of scale."
          result (analysis/detect-lollapalooza [] text)]
      (is (map? result) "Should return a map")
      (is (contains? result :is-lollapalooza) "Should have is-lollapalooza")
      (is (contains? result :models-aligned) "Should have models-aligned")
      (is (number? (:models-aligned result)) "models-aligned should be a number")
      (is (>= (:models-aligned result) 0) "models-aligned should be non-negative"))))

(deftest test-inversion-analyze
  (testing "Inversion analysis"
    (let [result (analysis/invert "How can I build a successful business?")]
      (is (map? result) "Should return a map")
      (is (contains? result :original-problem) "Should have original-problem")
      (is (contains? result :inverted-questions) "Should have inverted-questions")
      (is (vector? (:inverted-questions result)) "inverted-questions should be vector")
      (is (> (count (:inverted-questions result)) 0) "Should have at least one inverted question"))))

(deftest test-two-track-analyze
  (testing "Two-track analysis (rational + psychological)"
    (let [result (analysis/two-track-analysis "Should we acquire this competitor?")]
      (is (map? result) "Should return a map")
      (is (contains? result :track-1-rational) "Should have track-1-rational")
      (is (contains? result :track-2-psychological) "Should have track-2-psychological")
      (is (contains? result :integration-questions) "Should have integration-questions"))))

(deftest test-detect-biases
  (testing "Cognitive bias detection"
    (let [text "Everyone is buying this stock, so it must be good. I knew this would happen all along."
          result (analysis/detect-biases text)]
      (is (map? result) "Should return a map")
      (is (contains? result :biases-detected) "Should have biases-detected")
      (is (sequential? (:biases-detected result)) "biases-detected should be sequential")
      (is (> (count (:biases-detected result)) 0) "Should detect at least one bias"))))

(deftest test-decision-checklist
  (testing "Decision checklist generation"
    (let [result (analysis/decision-checklist "Should I hire this executive?" "Hiring decision for VP of Engineering")]
      (is (map? result) "Should return a map")
      (is (contains? result :checklist) "Should have checklist")
      (is (vector? (:checklist result)) "checklist should be vector")
      (is (> (count (:checklist result)) 5) "Should have multiple checklist items"))))

(deftest test-analyze-comprehensive
  (testing "Comprehensive analysis"
    (let [result (analysis/analyze-comprehensive
                  ["circle-of-competence" "margin-of-safety"]
                  "Should I invest $1M in this AI startup?")]
      (is (map? result) "Should return a map")
      (is (contains? result :latticework-analysis) "Should have latticework-analysis")
      (is (contains? result :lollapalooza-detection) "Should have lollapalooza-detection"))))
