(ns mental-models.llm-test
  "Tests for LM Studio and LLM integration."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.services.lm-studio :as lm]))

;; =============================================================================
;; LM STUDIO CONNECTION TESTS
;; =============================================================================

(deftest test-lm-studio-connection
  (testing "Can connect to LM Studio"
    (let [status (lm/check-connection)]
      (is (map? status) "Should return status map")
      (is (contains? status :connected) "Should indicate connection status"))))

(deftest test-lm-studio-model-info
  (testing "Can get loaded model info"
    (let [info (lm/get-model-info)]
      (is (map? info) "Should return model info")
      (when (:connected info)
        (is (contains? info :model-name) "Should have model name")))))

;; =============================================================================
;; COMPLETION TESTS
;; =============================================================================

(deftest test-basic-completion
  (testing "Can get basic completion"
    (let [result (lm/complete {:prompt "Hello, "
                               :max-tokens 10})]
      (is (map? result) "Should return result map")
      (is (or (contains? result :text)
              (contains? result :error))
          "Should have text or error"))))

(deftest test-chat-completion
  (testing "Can get chat completion"
    (let [result (lm/chat {:messages [{:role "user"
                                       :content "What is 2+2?"}]
                           :max-tokens 50})]
      (is (map? result) "Should return result map")
      (is (or (contains? result :response)
              (contains? result :error))
          "Should have response or error"))))

;; =============================================================================
;; MENTAL MODEL ANALYSIS TESTS
;; =============================================================================

(deftest test-model-detection-prompt
  (testing "Model detection prompt returns valid structure"
    (let [prompt (lm/build-detection-prompt :confirmation-bias 
                                            "Test text for analysis")]
      (is (string? prompt) "Should return string prompt")
      (is (.contains prompt "confirmation bias") "Should mention the model"))))

(deftest test-semantic-analysis
  (testing "Semantic analysis returns score"
    (let [result (lm/analyze-for-model 
                   :confirmation-bias
                   "I only see evidence that confirms what I already believe.")]
      (is (map? result) "Should return result map")
      (when-not (:error result)
        (is (number? (:score result)) "Should have numeric score")
        (is (<= 0 (:score result) 1) "Score should be 0-1")))))

(deftest test-batch-analysis
  (testing "Batch analysis processes multiple models"
    (let [text "The market crashed due to overconfidence and herd behavior."
          models [:overconfidence :social-proof :anchoring]
          results (lm/batch-analyze text models)]
      (is (map? results) "Should return results map")
      (is (= (count models) (count (:scores results)))
          "Should have score for each model"))))

;; =============================================================================
;; PROMPT TEMPLATE TESTS
;; =============================================================================

(deftest test-prompt-templates-exist
  (testing "Prompt templates exist for key models"
    (let [templates (lm/get-prompt-templates)]
      (is (contains? templates :confirmation-bias))
      (is (contains? templates :anchoring))
      (is (contains? templates :sunk-cost))
      (is (contains? templates :availability-heuristic)))))

(deftest test-prompt-template-structure
  (testing "Prompt templates have required fields"
    (let [template (lm/get-prompt-template :confirmation-bias)]
      (is (contains? template :system) "Should have system prompt")
      (is (contains? template :user-template) "Should have user template")
      (is (string? (:system template)))
      (is (string? (:user-template template))))))

;; =============================================================================
;; ERROR HANDLING TESTS
;; =============================================================================

(deftest test-handles-connection-failure
  (testing "Handles LM Studio not running"
    (let [result (lm/complete {:prompt "test" :timeout 100})]
      (is (map? result) "Should return map even on failure")
      (is (or (contains? result :text)
              (contains? result :error))
          "Should have text or error"))))

(deftest test-handles-invalid-prompt
  (testing "Handles invalid prompt gracefully"
    (let [result (lm/complete {:prompt nil})]
      (is (contains? result :error) "Should return error for nil prompt"))))

(deftest test-handles-timeout
  (testing "Handles timeout gracefully"
    (let [result (lm/complete {:prompt "Generate a very long response..."
                               :max-tokens 10000
                               :timeout 100})]
      (is (map? result) "Should return map even on timeout"))))

;; =============================================================================
;; CONFIGURATION TESTS
;; =============================================================================

(deftest test-lm-studio-config
  (testing "LM Studio configuration is valid"
    (let [config (lm/get-config)]
      (is (string? (:url config)) "Should have URL")
      (is (pos? (:default-timeout config)) "Should have timeout")
      (is (pos? (:max-tokens config)) "Should have max tokens"))))

(deftest test-temperature-settings
  (testing "Temperature settings are applied"
    (let [result1 (lm/complete {:prompt "Test" :temperature 0.0})
          result2 (lm/complete {:prompt "Test" :temperature 0.0})]
      ;; With temperature 0, responses should be deterministic
      (when (and (:text result1) (:text result2))
        (is (= (:text result1) (:text result2))
            "Zero temperature should give deterministic results")))))
