(ns mental-models.models-test
  "Tests for mental models library"
  (:require [clojure.test :refer [deftest is testing]]
            [mental-models.models :as models]))

(deftest test-model-registration
  (testing "Model registration and retrieval"
    (let [test-model {:name "test-model"
                      :category "test"
                      :description "A test model"
                      :key-insight "Testing is important"
                      :application "Use for testing"
                      :failure-modes []}]
      (models/register-model test-model)
      (is (= test-model (models/get-model "test-model"))
          "Should retrieve registered model")
      (is (contains? (set (models/get-all-categories)) "test")
          "Should include test category"))))

(deftest test-get-all-models
  (testing "Get all models"
    (let [all-models (models/get-all-models)]
      (is (> (count all-models) 100)
          "Should have more than 100 models")
      (is (every? #(contains? % :name) all-models)
          "All models should have names")
      (is (every? #(contains? % :category) all-models)
          "All models should have categories"))))

(deftest test-get-models-by-category
  (testing "Get models by category"
    (let [decision-models (models/get-models-by-category "decision_making")]
      (is (> (count decision-models) 0)
          "Should have decision making models")
      (is (every? #(= "decision_making" (:category %)) decision-models)
          "All models should be in decision_making category"))))

(deftest test-search-models
  (testing "Search models by query"
    (let [results (models/search-models "circle")]
      (is (> (count results) 0)
          "Should find models matching 'circle'")
      (is (some #(= "circle-of-competence" (:name %)) results)
          "Should find circle-of-competence model"))
    
    (let [empty-results (models/search-models "")]
      (is (= (count empty-results) (count (models/get-all-models)))
          "Empty search should return all models"))))

(deftest test-model-structure
  (testing "Model structure validation"
    (let [models (models/get-all-models)]
      (doseq [model models]
        (is (string? (:name model)) "Model name should be string")
        (is (string? (:category model)) "Model category should be string")
        (is (string? (:description model)) "Model description should be string")
        (is (string? (:key-insight model)) "Model key-insight should be string")
        (is (string? (:application model)) "Model application should be string")
        (is (vector? (:failure-modes model)) "Model failure-modes should be vector")
        (is (= 5 (count (:failure-modes model))) 
            (str "Model " (:name model) " should have exactly 5 failure modes"))))))

(deftest test-failure-mode-structure
  (testing "Failure mode structure validation"
    (let [model (models/get-model "circle-of-competence")
          failure-modes (:failure-modes model)]
      (doseq [fm failure-modes]
        (is (string? (:name fm)) "Failure mode name should be string")
        (is (string? (:severity fm)) "Failure mode severity should be string")
        (is (string? (:description fm)) "Failure mode description should be string")
        (is (vector? (:signals fm)) "Failure mode signals should be vector")
        (is (vector? (:safeguards fm)) "Failure mode safeguards should be vector")))))

(deftest test-all-categories
  (testing "All categories"
    (let [categories (models/get-all-categories)]
      (is (> (count categories) 30)
          "Should have more than 30 categories")
      (is (contains? (set categories) "decision_making")
          "Should include decision_making category")
      (is (contains? (set categories) "psychology")
          "Should include psychology category"))))
