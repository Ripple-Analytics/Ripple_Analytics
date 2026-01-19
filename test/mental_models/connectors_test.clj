(ns mental-models.connectors-test
  "Tests for data connectors - news, financial, academic sources."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.connectors :as conn]))

;; =============================================================================
;; NEWS CONNECTOR TESTS
;; =============================================================================

(deftest test-news-connector-structure
  (testing "News connector has required functions"
    (is (fn? conn/fetch-news) "Should have fetch-news function")
    (is (fn? conn/search-news) "Should have search-news function")))

(deftest test-news-fetch-returns-valid-structure
  (testing "News fetch returns properly structured data"
    (let [result (conn/fetch-news {:source "test" :limit 5})]
      (is (map? result) "Should return a map")
      (is (contains? result :articles) "Should contain articles key")
      (is (contains? result :source) "Should identify source")
      (is (contains? result :fetched-at) "Should have timestamp"))))

(deftest test-news-article-structure
  (testing "News articles have required fields"
    (let [result (conn/fetch-news {:source "test" :limit 1})
          article (first (:articles result))]
      (when article
        (is (contains? article :title) "Article should have title")
        (is (contains? article :url) "Article should have URL")
        (is (contains? article :published-at) "Article should have publish date")))))

;; =============================================================================
;; FINANCIAL DATA CONNECTOR TESTS
;; =============================================================================

(deftest test-financial-connector-structure
  (testing "Financial connector has required functions"
    (is (fn? conn/fetch-stock-data) "Should have fetch-stock-data function")
    (is (fn? conn/fetch-market-data) "Should have fetch-market-data function")))

(deftest test-stock-data-returns-valid-structure
  (testing "Stock data returns properly structured data"
    (let [result (conn/fetch-stock-data {:symbol "AAPL"})]
      (is (map? result) "Should return a map")
      (is (contains? result :symbol) "Should contain symbol")
      (is (contains? result :data) "Should contain data"))))

;; =============================================================================
;; ACADEMIC CONNECTOR TESTS
;; =============================================================================

(deftest test-academic-connector-structure
  (testing "Academic connector has required functions"
    (is (fn? conn/search-papers) "Should have search-papers function")
    (is (fn? conn/fetch-paper) "Should have fetch-paper function")))

(deftest test-paper-search-returns-valid-structure
  (testing "Paper search returns properly structured data"
    (let [result (conn/search-papers {:query "mental models" :limit 5})]
      (is (map? result) "Should return a map")
      (is (contains? result :papers) "Should contain papers key")
      (is (contains? result :total-count) "Should have total count"))))

;; =============================================================================
;; CONNECTOR ERROR HANDLING TESTS
;; =============================================================================

(deftest test-connector-error-handling
  (testing "Connectors handle errors gracefully"
    (let [result (conn/fetch-news {:source "invalid-source"})]
      (is (map? result) "Should return a map even on error")
      (is (or (contains? result :error)
              (contains? result :articles))
          "Should contain error or valid data"))))

(deftest test-connector-timeout-handling
  (testing "Connectors handle timeouts"
    (let [result (conn/fetch-news {:source "test" :timeout 1})]
      (is (map? result) "Should return a map even on timeout"))))

;; =============================================================================
;; CONNECTOR CONFIGURATION TESTS
;; =============================================================================

(deftest test-connector-config-validation
  (testing "Connector validates configuration"
    (is (thrown? Exception (conn/fetch-news nil))
        "Should throw on nil config")
    (is (thrown? Exception (conn/fetch-news {}))
        "Should throw on empty config")))
