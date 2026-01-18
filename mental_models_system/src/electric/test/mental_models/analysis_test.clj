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
      (is (contains? result :analysis) "Should have analysis")
      (is (vector? (:models-applied result)) "models-applied should be vector")
      (is (= 2 (count (:models-applied result))) "Should apply 2 models"))))

(deftest test-lollapalooza-detect
  (testing "Lollapalooza effect detection"
    (let [text "This company has strong network effects, high switching costs, and economies of scale."
          result (analysis/lollapalooza-detect text)]
      (is (map? result) "Should return a map")
      (is (contains? result :models-detected) "Should have models-detected")
      (is (contains? result :lollapalooza-score) "Should have lollapalooza-score")
      (is (number? (:lollapalooza-score result)) "Score should be a number")
      (is (>= (:lollapalooza-score result) 0) "Score should be non-negative"))))

(deftest test-inversion-analyze
  (testing "Inversion analysis"
    (let [result (analysis/inversion-analyze "How can I build a successful business?")]
      (is (map? result) "Should return a map")
      (is (contains? result :original-question) "Should have original-question")
      (is (contains? result :inverted-question) "Should have inverted-question")
      (is (contains? result :failure-modes) "Should have failure-modes")
      (is (vector? (:failure-modes result)) "failure-modes should be vector"))))

(deftest test-two-track-analyze
  (testing "Two-track analysis (rational + psychological)"
    (let [result (analysis/two-track-analyze "Should we acquire this competitor?")]
      (is (map? result) "Should return a map")
      (is (contains? result :rational-track) "Should have rational-track")
      (is (contains? result :psychological-track) "Should have psychological-track")
      (is (contains? result :synthesis) "Should have synthesis"))))

(deftest test-detect-biases
  (testing "Cognitive bias detection"
    (let [text "Everyone is buying this stock, so it must be good. I knew this would happen all along."
          result (analysis/detect-biases text)]
      (is (map? result) "Should return a map")
      (is (contains? result :biases-detected) "Should have biases-detected")
      (is (vector? (:biases-detected result)) "biases-detected should be vector")
      (is (> (count (:biases-detected result)) 0) "Should detect at least one bias"))))

(deftest test-decision-checklist
  (testing "Decision checklist generation"
    (let [result (analysis/decision-checklist "Should I hire this executive?")]
      (is (map? result) "Should return a map")
      (is (contains? result :checklist) "Should have checklist")
      (is (vector? (:checklist result)) "checklist should be vector")
      (is (> (count (:checklist result)) 5) "Should have multiple checklist items"))))

(deftest test-analyze-with-context
  (testing "Analysis with context"
    (let [result (analysis/analyze-with-context
                  "Investment decision"
                  {:industry "technology"
                   :stage "growth"
                   :risk-tolerance "moderate"}
                  "Should I invest $1M in this AI startup?")]
      (is (map? result) "Should return a map")
      (is (contains? result :context) "Should include context")
      (is (contains? result :analysis) "Should have analysis"))))
