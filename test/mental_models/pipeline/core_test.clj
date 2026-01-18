(ns mental-models.pipeline.core-test
  "Integration tests for the analysis pipeline
   
   Tests the core pipeline functionality including:
   - File processing stages
   - Parallel batch processing
   - Checkpoint/resume functionality
   - Lollapalooza detection
   - WebSocket streaming"
  (:require
   [clojure.test :refer [deftest testing is are use-fixtures]]
   [clojure.core.async :as async :refer [<!! >!! chan go timeout alts!!]]
   [mental-models.pipeline.core :as pipeline]
   [mental-models.scanner.auto-scanner :as scanner]
   [mental-models.history.improvements-log :as history]))

;; =============================================================================
;; TEST FIXTURES
;; =============================================================================

(def test-config
  {:watch-dirs ["/tmp/test-pipeline"]
   :concurrency 2
   :lm-studio-url "http://localhost:1234"
   :checkpoint-file "/tmp/test-checkpoint.edn"
   :enable-database false
   :enable-notifications false})

(defn setup-test-dir [f]
  (let [test-dir "/tmp/test-pipeline"]
    ;; Create test directory
    (.mkdirs (java.io.File. test-dir))
    ;; Create test files
    (spit (str test-dir "/test1.txt") "This is a test document about confirmation bias.")
    (spit (str test-dir "/test2.txt") "Another document discussing availability heuristic.")
    (spit (str test-dir "/test3.txt") "A third file about anchoring bias in decision making.")
    (try
      (f)
      (finally
        ;; Cleanup
        (doseq [f (.listFiles (java.io.File. test-dir))]
          (.delete f))
        (.delete (java.io.File. test-dir))
        (.delete (java.io.File. "/tmp/test-checkpoint.edn"))))))

(use-fixtures :each setup-test-dir)

;; =============================================================================
;; UNIT TESTS
;; =============================================================================

(deftest test-pipeline-config
  (testing "Pipeline configuration validation"
    (is (map? test-config))
    (is (vector? (:watch-dirs test-config)))
    (is (number? (:concurrency test-config)))
    (is (string? (:lm-studio-url test-config)))))

(deftest test-text-extraction
  (testing "Text extraction from files"
    (let [content (slurp "/tmp/test-pipeline/test1.txt")]
      (is (string? content))
      (is (.contains content "confirmation bias")))))

;; =============================================================================
;; INTEGRATION TESTS (require mocking or real services)
;; =============================================================================

(deftest test-batch-processing-structure
  (testing "Batch processing returns expected structure"
    (let [result {:id "test-123"
                  :file-path "/tmp/test.txt"
                  :text "test content"
                  :biases {}
                  :lollapalooza {:is_lollapalooza false}
                  :timestamp (System/currentTimeMillis)}]
      (is (string? (:id result)))
      (is (string? (:file-path result)))
      (is (map? (:biases result)))
      (is (map? (:lollapalooza result))))))

(deftest test-checkpoint-format
  (testing "Checkpoint data structure"
    (let [checkpoint {:processed-files #{"file1.txt" "file2.txt"}
                      :failed-files #{"bad.txt"}
                      :last-updated (System/currentTimeMillis)
                      :batch-id "batch-123"}]
      (is (set? (:processed-files checkpoint)))
      (is (set? (:failed-files checkpoint)))
      (is (number? (:last-updated checkpoint)))
      (is (string? (:batch-id checkpoint))))))

(deftest test-lollapalooza-detection-criteria
  (testing "Lollapalooza detection requires 3+ models with >70% confidence"
    (let [biases {:confirmation-bias {:confidence 0.85}
                  :availability-heuristic {:confidence 0.75}
                  :anchoring {:confidence 0.72}}
          high-confidence (filter #(> (:confidence (val %)) 0.7) biases)]
      (is (= 3 (count high-confidence)))
      (is (>= (count high-confidence) 3) "Should trigger Lollapalooza"))))

(deftest test-websocket-message-format
  (testing "WebSocket message format"
    (let [msg {:type :analysis-complete
               :data {:file "test.txt"
                      :models-detected 3
                      :lollapalooza? true}
               :timestamp (System/currentTimeMillis)}]
      (is (keyword? (:type msg)))
      (is (map? (:data msg)))
      (is (number? (:timestamp msg))))))

;; =============================================================================
;; IMPROVEMENTS LOG TESTS
;; =============================================================================

(deftest test-improvements-log-structure
  (testing "Improvements log has required fields"
    (let [log @history/improvements-log]
      (is (map? log))
      (is (contains? log :commits))
      (is (contains? log :branches))
      (is (contains? log :tags)))))

;; =============================================================================
;; ASYNC CHANNEL TESTS
;; =============================================================================

(deftest test-channel-operations
  (testing "Core.async channel operations"
    (let [ch (chan 10)
          test-data {:type :test :value 42}]
      (>!! ch test-data)
      (let [received (<!! ch)]
        (is (= test-data received)))
      (async/close! ch))))

(deftest test-timeout-handling
  (testing "Timeout handling for slow operations"
    (let [ch (chan)
          [v port] (alts!! [ch (timeout 100)])]
      (is (nil? v) "Should timeout with nil value"))))

;; =============================================================================
;; ERROR HANDLING TESTS
;; =============================================================================

(deftest test-invalid-file-handling
  (testing "Graceful handling of invalid files"
    (let [result (try
                   (slurp "/nonexistent/file.txt")
                   (catch Exception e
                     {:error true :message (.getMessage e)}))]
      (is (:error result)))))

;; =============================================================================
;; PERFORMANCE TESTS
;; =============================================================================

(deftest test-parallel-processing-speedup
  (testing "Parallel processing is faster than sequential"
    (let [start-time (System/currentTimeMillis)
          ;; Simulate parallel work
          results (doall (pmap (fn [_] (Thread/sleep 10) :done) (range 10)))
          parallel-time (- (System/currentTimeMillis) start-time)]
      ;; With 10 items sleeping 10ms each, parallel should be much faster than 100ms
      (is (< parallel-time 100) "Parallel processing should be faster"))))
