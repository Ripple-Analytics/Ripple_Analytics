(ns mental-models.db-test
  "Tests for database operations and connection pooling."
  (:require [clojure.test :refer [deftest testing is are use-fixtures]]
            [mental-models.db.core :as db]))

;; =============================================================================
;; CONNECTION POOL TESTS
;; =============================================================================

(deftest test-connection-pool-initialization
  (testing "Connection pool initializes correctly"
    (let [pool (db/get-connection-pool)]
      (is (some? pool) "Pool should exist")
      (is (db/pool-healthy? pool) "Pool should be healthy"))))

(deftest test-connection-pool-config
  (testing "Connection pool has correct configuration"
    (let [config (db/get-pool-config)]
      (is (pos? (:max-pool-size config)) "Should have max pool size")
      (is (pos? (:min-idle config)) "Should have min idle connections")
      (is (pos? (:connection-timeout config)) "Should have connection timeout"))))

;; =============================================================================
;; CRUD OPERATION TESTS
;; =============================================================================

(deftest test-create-and-read-model
  (testing "Can create and read mental model"
    (let [model {:name "Test Model"
                 :slug "test-model"
                 :description "A test model"
                 :category-id 1}
          created (db/create-model! model)
          read (db/get-model-by-slug "test-model")]
      (is (some? created) "Should create model")
      (is (= "Test Model" (:name read)) "Should read back correctly")
      ;; Cleanup
      (db/delete-model! (:id created)))))

(deftest test-update-model
  (testing "Can update mental model"
    (let [model {:name "Update Test"
                 :slug "update-test"
                 :description "Original"
                 :category-id 1}
          created (db/create-model! model)
          _ (db/update-model! (:id created) {:description "Updated"})
          updated (db/get-model-by-id (:id created))]
      (is (= "Updated" (:description updated)) "Should update description")
      ;; Cleanup
      (db/delete-model! (:id created)))))

(deftest test-delete-model
  (testing "Can delete mental model"
    (let [model {:name "Delete Test"
                 :slug "delete-test"
                 :description "To be deleted"
                 :category-id 1}
          created (db/create-model! model)
          _ (db/delete-model! (:id created))
          deleted (db/get-model-by-id (:id created))]
      (is (nil? deleted) "Model should be deleted"))))

;; =============================================================================
;; QUERY TESTS
;; =============================================================================

(deftest test-get-all-models
  (testing "Can retrieve all models"
    (let [models (db/get-all-models)]
      (is (seq models) "Should have models")
      (is (every? :id models) "Each model should have id")
      (is (every? :name models) "Each model should have name"))))

(deftest test-get-models-by-category
  (testing "Can retrieve models by category"
    (let [models (db/get-models-by-category "psychology")]
      (is (seq models) "Should have psychology models")
      (is (every? #(= "psychology" (:category-slug %)) models)
          "All models should be in psychology category"))))

(deftest test-search-models
  (testing "Can search models"
    (let [results (db/search-models "bias")]
      (is (seq results) "Should find models with 'bias'"))))

(deftest test-get-failure-modes
  (testing "Can retrieve failure modes"
    (let [modes (db/get-all-failure-modes)]
      (is (seq modes) "Should have failure modes")
      (is (every? :model-id modes) "Each mode should reference a model"))))

;; =============================================================================
;; TRANSACTION TESTS
;; =============================================================================

(deftest test-transaction-commit
  (testing "Transaction commits correctly"
    (let [result (db/with-transaction [tx]
                   (db/create-model! tx {:name "TX Test"
                                         :slug "tx-test"
                                         :category-id 1})
                   :committed)]
      (is (= :committed result) "Transaction should commit")
      ;; Cleanup
      (when-let [model (db/get-model-by-slug "tx-test")]
        (db/delete-model! (:id model))))))

(deftest test-transaction-rollback
  (testing "Transaction rolls back on error"
    (try
      (db/with-transaction [tx]
        (db/create-model! tx {:name "Rollback Test"
                              :slug "rollback-test"
                              :category-id 1})
        (throw (ex-info "Force rollback" {})))
      (catch Exception _))
    (let [model (db/get-model-by-slug "rollback-test")]
      (is (nil? model) "Model should not exist after rollback"))))

;; =============================================================================
;; DATA INTEGRITY TESTS
;; =============================================================================

(deftest test-model-count-accuracy
  (testing "Model count is accurate"
    (let [count-result (db/count-models)
          all-models (db/get-all-models)]
      (is (= count-result (count all-models))
          "Count should match actual number of models"))))

(deftest test-failure-mode-count-accuracy
  (testing "Failure mode count is accurate"
    (let [count-result (db/count-failure-modes)
          all-modes (db/get-all-failure-modes)]
      (is (= count-result (count all-modes))
          "Count should match actual number of failure modes"))))

(deftest test-category-model-counts
  (testing "Category model counts are accurate"
    (let [categories (db/get-categories-with-counts)]
      (doseq [cat categories]
        (let [models (db/get-models-by-category (:slug cat))]
          (is (= (:model-count cat) (count models))
              (str "Count for " (:name cat) " should be accurate")))))))
