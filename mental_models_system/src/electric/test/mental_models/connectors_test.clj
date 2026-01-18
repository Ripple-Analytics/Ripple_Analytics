(ns mental-models.connectors-test
  "Tests for the connectors module.
   
   Tests connector creation, configuration, and basic functionality.
   Note: Full integration tests require actual API keys and services."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.connectors :as conn]))

;; ============================================
;; Connector Creation Tests
;; ============================================

(deftest test-create-zapier-connector
  (testing "Zapier connector creation"
    (let [connector (conn/create-zapier-connector "https://hooks.zapier.com/test")]
      (is (= :zapier (:type connector)))
      (is (= "https://hooks.zapier.com/test" (:webhook-url connector)))
      (is (= :disconnected (:status connector)))
      (is (nil? (:last-triggered connector))))))

(deftest test-create-huggingface-connector
  (testing "Huggingface connector creation"
    (let [connector (conn/create-huggingface-connector "hf_test_key")]
      (is (= :huggingface (:type connector)))
      (is (= "hf_test_key" (:api-key connector)))
      (is (= :disconnected (:status connector)))
      (is (map? (:available-models connector))))))

(deftest test-create-github-connector
  (testing "GitHub connector creation"
    (let [connector (conn/create-github-connector "ghp_test_token")]
      (is (= :github (:type connector)))
      (is (= "ghp_test_token" (:token connector)))
      (is (= :disconnected (:status connector)))
      (is (map? (:rate-limit connector)))
      (is (= 5000 (get-in connector [:rate-limit :remaining]))))))

(deftest test-create-slack-connector
  (testing "Slack connector creation"
    (let [connector (conn/create-slack-connector "xoxb-test-token")]
      (is (= :slack (:type connector)))
      (is (= "xoxb-test-token" (:bot-token connector)))
      (is (= :disconnected (:status connector)))
      (is (nil? (:workspace connector))))))

(deftest test-create-google-drive-connector
  (testing "Google Drive connector creation"
    (let [connector (conn/create-google-drive-connector {:client-id "test"})]
      (is (= :google-drive (:type connector)))
      (is (= {:client-id "test"} (:credentials connector)))
      (is (= :disconnected (:status connector))))))

(deftest test-create-lm-studio-connector
  (testing "LM Studio connector creation with defaults"
    (let [connector (conn/create-lm-studio-connector)]
      (is (= :lm-studio (:type connector)))
      (is (= "localhost" (:host connector)))
      (is (= 1234 (:port connector)))
      (is (= "http://localhost:1234/v1" (:base-url connector)))
      (is (= :disconnected (:status connector)))))
  
  (testing "LM Studio connector creation with custom host/port"
    (let [connector (conn/create-lm-studio-connector :host "192.168.1.100" :port 8080)]
      (is (= "192.168.1.100" (:host connector)))
      (is (= 8080 (:port connector)))
      (is (= "http://192.168.1.100:8080/v1" (:base-url connector))))))

(deftest test-create-database-connector
  (testing "Database connector creation"
    (let [db-spec {:dbtype "postgresql" :host "localhost" :dbname "test"}
          connector (conn/create-database-connector db-spec)]
      (is (= :database (:type connector)))
      (is (= db-spec (:db-spec connector)))
      (is (= :disconnected (:status connector)))
      (is (nil? (:pool connector))))))

(deftest test-create-file-connector
  (testing "File connector creation"
    (let [connector (conn/create-file-connector "/tmp/test")]
      (is (= :file (:type connector)))
      (is (= "/tmp/test" (:base-path connector)))
      (is (= :connected (:status connector))))))

(deftest test-create-web-scraper
  (testing "Web scraper creation with defaults"
    (let [connector (conn/create-web-scraper)]
      (is (= :web-scraper (:type connector)))
      (is (= "MentalModels-Bot/1.0" (:user-agent connector)))
      (is (= :ready (:status connector)))
      (is (= 1 (get-in connector [:rate-limit :requests-per-second])))))
  
  (testing "Web scraper creation with custom user-agent"
    (let [connector (conn/create-web-scraper :user-agent "CustomBot/2.0")]
      (is (= "CustomBot/2.0" (:user-agent connector))))))

;; ============================================
;; Connector Registry Tests
;; ============================================

(deftest test-list-connectors
  (testing "List all available connectors"
    (let [connectors (conn/list-connectors)]
      (is (vector? connectors))
      (is (>= (count connectors) 9))
      (is (every? #(contains? % :type) connectors))
      (is (every? #(contains? % :description) connectors))
      (is (some #(= :zapier (:type %)) connectors))
      (is (some #(= :github (:type %)) connectors))
      (is (some #(= :lm-studio (:type %)) connectors)))))

(deftest test-create-connector-by-type
  (testing "Create connector using registry"
    (let [zapier (conn/create-connector :zapier "https://test.zapier.com")
          github (conn/create-connector :github "test-token")
          lm-studio (conn/create-connector :lm-studio)]
      (is (= :zapier (:type zapier)))
      (is (= :github (:type github)))
      (is (= :lm-studio (:type lm-studio)))))
  
  (testing "Create connector with unknown type"
    (let [result (conn/create-connector :unknown)]
      (is (contains? result :error))
      (is (string? (:error result))))))

;; ============================================
;; Configuration Tests
;; ============================================

(deftest test-zapier-config
  (testing "Zapier configuration"
    (is (= "https://hooks.zapier.com" (:base-url conn/zapier-config)))
    (is (= 30000 (:timeout-ms conn/zapier-config)))
    (is (= 3 (:retry-count conn/zapier-config)))))

(deftest test-huggingface-config
  (testing "Huggingface configuration"
    (is (= "https://api-inference.huggingface.co" (:base-url conn/huggingface-config)))
    (is (map? (:models conn/huggingface-config)))
    (is (contains? (:models conn/huggingface-config) :text-generation))
    (is (contains? (:models conn/huggingface-config) :embeddings))))

(deftest test-github-config
  (testing "GitHub configuration"
    (is (= "https://api.github.com" (:base-url conn/github-config)))
    (is (= "2022-11-28" (:api-version conn/github-config)))))

(deftest test-slack-config
  (testing "Slack configuration"
    (is (= "https://slack.com/api" (:base-url conn/slack-config)))
    (is (vector? (:scopes conn/slack-config)))
    (is (some #(= "chat:write" %) (:scopes conn/slack-config)))))

(deftest test-lm-studio-config
  (testing "LM Studio configuration"
    (is (= "localhost" (:default-host conn/lm-studio-config)))
    (is (= 1234 (:default-port conn/lm-studio-config)))
    (is (= "/v1" (:api-path conn/lm-studio-config)))))

(deftest test-scraper-config
  (testing "Web scraper configuration"
    (is (= "MentalModels-Bot/1.0" (:user-agent conn/scraper-config)))
    (is (= 30000 (:timeout-ms conn/scraper-config)))
    (is (true? (:respect-robots-txt conn/scraper-config)))))

;; ============================================
;; Error Handling Tests (without actual API calls)
;; ============================================

(deftest test-error-response-structure
  (testing "Error responses have consistent structure"
    ;; Test that functions return proper error structures when called without valid credentials
    ;; These tests verify the error handling paths work correctly
    (let [github-connector (conn/create-github-connector "invalid-token")
          lm-studio-connector (conn/create-lm-studio-connector :host "nonexistent" :port 9999)]
      
      ;; GitHub functions should return error structure when API fails
      (let [result (conn/github-list-issues github-connector "owner" "repo")]
        (is (contains? result :status))
        (is (contains? result :timestamp)))
      
      ;; LM Studio functions should return error structure when server unavailable
      (let [result (conn/lm-studio-list-models lm-studio-connector)]
        (is (contains? result :status))
        (is (contains? result :models))))))

;; ============================================
;; File Connector Tests (can run without external services)
;; ============================================

(deftest test-file-connector-operations
  (testing "File connector read/write/list operations"
    (let [test-dir "/tmp/mental-models-test"
          connector (conn/create-file-connector test-dir)]
      
      ;; Create test directory
      (.mkdirs (java.io.File. test-dir))
      
      ;; Test write
      (let [write-result (conn/file-write connector "test.txt" "Hello, World!")]
        (is (= :success (:status write-result)))
        (is (= 13 (:bytes-written write-result))))
      
      ;; Test read
      (let [read-result (conn/file-read connector "test.txt")]
        (is (= :success (:status read-result)))
        (is (= "Hello, World!" (:content read-result))))
      
      ;; Test list
      (let [list-result (conn/file-list connector "")]
        (is (= :success (:status list-result)))
        (is (some #(= "test.txt" %) (:files list-result))))
      
      ;; Cleanup
      (.delete (java.io.File. (str test-dir "/test.txt")))
      (.delete (java.io.File. test-dir)))))

(deftest test-file-connector-error-handling
  (testing "File connector error handling"
    (let [connector (conn/create-file-connector "/nonexistent/path")]
      
      ;; Test read error
      (let [result (conn/file-read connector "missing.txt")]
        (is (= :error (:status result)))
        (is (contains? result :error)))
      
      ;; Test list error
      (let [result (conn/file-list connector "")]
        (is (= :error (:status result)))
        (is (contains? result :error))))))

;; ============================================
;; Integration Test Helpers
;; ============================================

(defn integration-test-available?
  "Check if integration tests can run (requires actual services)."
  [connector-type]
  (case connector-type
    :lm-studio (try
                 (let [connector (conn/create-lm-studio-connector)
                       result (conn/lm-studio-list-models connector)]
                   (= :success (:status result)))
                 (catch Exception _ false))
    :github (some? (System/getenv "GITHUB_TOKEN"))
    :slack (some? (System/getenv "SLACK_BOT_TOKEN"))
    :huggingface (some? (System/getenv "HUGGINGFACE_API_KEY"))
    false))

(deftest test-integration-helpers
  (testing "Integration test helpers work"
    (is (boolean? (integration-test-available? :lm-studio)))
    (is (boolean? (integration-test-available? :github)))
    (is (boolean? (integration-test-available? :slack)))
    (is (boolean? (integration-test-available? :huggingface)))))

;; ============================================
;; Retry Configuration Tests
;; ============================================

(deftest test-retry-config
  (testing "Retry configuration is defined"
    (is (map? conn/retry-config))
    (is (= 3 (:max-retries conn/retry-config)))
    (is (= 1000 (:initial-delay-ms conn/retry-config)))
    (is (= 30000 (:max-delay-ms conn/retry-config)))
    (is (= 2.0 (:backoff-multiplier conn/retry-config)))
    (is (set? (:retryable-status-codes conn/retry-config)))
    (is (contains? (:retryable-status-codes conn/retry-config) 429))
    (is (contains? (:retryable-status-codes conn/retry-config) 503))))

(deftest test-calculate-backoff-delay
  (testing "Backoff delay calculation"
    (let [config {:initial-delay-ms 1000 :max-delay-ms 30000 :backoff-multiplier 2.0}]
      ;; First attempt (attempt 0) should be around initial-delay-ms
      (let [delay (conn/calculate-backoff-delay 0 config)]
        (is (>= delay 1000))
        (is (<= delay 1300))) ;; With 30% jitter
      
      ;; Second attempt should be around 2x initial
      (let [delay (conn/calculate-backoff-delay 1 config)]
        (is (>= delay 2000))
        (is (<= delay 2600)))
      
      ;; Third attempt should be around 4x initial
      (let [delay (conn/calculate-backoff-delay 2 config)]
        (is (>= delay 4000))
        (is (<= delay 5200)))
      
      ;; Should cap at max-delay-ms
      (let [delay (conn/calculate-backoff-delay 10 config)]
        (is (<= delay 30000))))))

(deftest test-retryable-error-detection
  (testing "Retryable error detection"
    (let [config conn/retry-config]
      ;; Timeout exceptions should be retryable
      (is (true? (conn/retryable-error? (Exception. "Connection timeout") config)))
      (is (true? (conn/retryable-error? (Exception. "connection reset") config)))
      
      ;; Non-retryable exceptions
      (is (false? (conn/retryable-error? (Exception. "Invalid API key") config)))
      (is (false? (conn/retryable-error? (Exception. "Not found") config)))
      
      ;; Retryable status codes
      (is (true? (conn/retryable-error? {:status 429} config)))
      (is (true? (conn/retryable-error? {:status 503} config)))
      (is (true? (conn/retryable-error? {:status 500} config)))
      
      ;; Non-retryable status codes
      (is (false? (conn/retryable-error? {:status 200} config)))
      (is (false? (conn/retryable-error? {:status 404} config)))
      (is (false? (conn/retryable-error? {:status 401} config))))))

(deftest test-with-retry-success
  (testing "with-retry returns result on success"
    (let [call-count (atom 0)
          result (conn/with-retry #(do (swap! call-count inc) "success"))]
      (is (= "success" result))
      (is (= 1 @call-count)))))

(deftest test-with-retry-eventual-success
  (testing "with-retry retries on transient failure then succeeds"
    (let [call-count (atom 0)
          result (conn/with-retry 
                   #(do 
                      (swap! call-count inc)
                      (if (< @call-count 2)
                        (throw (Exception. "Connection timeout"))
                        "success"))
                   {:max-retries 3 :initial-delay-ms 10 :max-delay-ms 100 :backoff-multiplier 2.0
                    :retryable-status-codes #{}})]
      (is (= "success" result))
      (is (= 2 @call-count)))))

;; ============================================
;; Rate Limiting Tests
;; ============================================

(deftest test-rate-limit-config
  (testing "Rate limit configuration is defined"
    (is (map? conn/rate-limit-config))
    (is (= 60 (:github conn/rate-limit-config)))
    (is (= 60 (:slack conn/rate-limit-config)))
    (is (= 30 (:huggingface conn/rate-limit-config)))
    (is (= 100 (:zapier conn/rate-limit-config)))
    (is (= 10 (:web-scraper conn/rate-limit-config)))))

(deftest test-check-rate-limit
  (testing "Rate limit checking"
    ;; Reset rate limiters for clean test
    (reset! conn/rate-limiters {})
    
    ;; First request should always be allowed
    (is (true? (conn/check-rate-limit :github)))
    
    ;; Multiple requests within limit should be allowed
    (dotimes [_ 10]
      (is (true? (conn/check-rate-limit :github))))))

(deftest test-rate-limiters-atom
  (testing "Rate limiters atom is initialized"
    (is (instance? clojure.lang.Atom conn/rate-limiters))
    (is (map? @conn/rate-limiters))))

;; ============================================
;; Response Caching Tests
;; ============================================

(deftest test-cache-config
  (testing "Cache configuration is defined"
    (is (map? conn/cache-config))
    (is (= 300000 (:github conn/cache-config)))
    (is (= 60000 (:huggingface conn/cache-config)))
    (is (= 600000 (:web-scraper conn/cache-config)))
    (is (= 0 (:lm-studio conn/cache-config)))))

(deftest test-response-cache-atom
  (testing "Response cache atom is initialized"
    (is (instance? clojure.lang.Atom conn/response-cache))
    (is (map? @conn/response-cache))))

(deftest test-cache-key-generation
  (testing "Cache key generation"
    (let [key1 (conn/cache-key :github "owner" "repo")
          key2 (conn/cache-key :github "owner" "repo")
          key3 (conn/cache-key :github "other" "repo")]
      (is (string? key1))
      (is (= key1 key2))
      (is (not= key1 key3))
      (is (clojure.string/starts-with? key1 "github-")))))

(deftest test-set-and-get-cached
  (testing "Cache set and get operations"
    ;; Clear cache first
    (conn/clear-cache)
    
    ;; Set a cached value
    (conn/set-cached :github {:data "test"} "owner" "repo")
    
    ;; Get the cached value
    (let [cached (conn/get-cached :github "owner" "repo")]
      (is (= {:data "test"} cached)))
    
    ;; Different params should return nil
    (is (nil? (conn/get-cached :github "other" "repo")))
    
    ;; LM Studio should not cache (TTL = 0)
    (conn/set-cached :lm-studio {:data "test"} "prompt")
    (is (nil? (conn/get-cached :lm-studio "prompt")))))

(deftest test-clear-cache
  (testing "Cache clearing"
    ;; Set some cached values
    (conn/set-cached :github {:data "github"} "test1")
    (conn/set-cached :huggingface {:data "hf"} "test2")
    
    ;; Clear specific connector cache
    (conn/clear-cache :github)
    (is (nil? (conn/get-cached :github "test1")))
    (is (some? (conn/get-cached :huggingface "test2")))
    
    ;; Clear all cache
    (conn/clear-cache)
    (is (nil? (conn/get-cached :huggingface "test2")))))

;; ============================================
;; Connection Manager Tests
;; ============================================

(deftest test-connection-manager-defined
  (testing "Connection manager is defined"
    (is (delay? conn/connection-manager))))

(deftest test-default-http-opts-defined
  (testing "Default HTTP options are defined"
    (is (map? conn/default-http-opts))
    (is (contains? conn/default-http-opts :socket-timeout))
    (is (contains? conn/default-http-opts :connection-timeout))
    (is (= 30000 (:socket-timeout conn/default-http-opts)))
    (is (= 10000 (:connection-timeout conn/default-http-opts)))))
