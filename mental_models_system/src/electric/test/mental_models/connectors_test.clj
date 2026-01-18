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

;; ============================================
;; Metrics & Telemetry Tests
;; ============================================

(deftest test-metrics-atom-initialized
  (testing "Metrics atom is initialized with correct structure"
    (is (instance? clojure.lang.Atom conn/metrics))
    (is (map? @conn/metrics))
    (is (contains? @conn/metrics :requests))
    (is (contains? @conn/metrics :errors))
    (is (contains? @conn/metrics :latencies))
    (is (contains? @conn/metrics :cache-hits))
    (is (contains? @conn/metrics :cache-misses))
    (is (contains? @conn/metrics :rate-limit-waits))
    (is (contains? @conn/metrics :retries))))

(deftest test-record-request
  (testing "Record request increments counter"
    (conn/reset-metrics)
    (conn/record-request :github)
    (is (= 1 (get-in @conn/metrics [:requests :github])))
    (conn/record-request :github)
    (is (= 2 (get-in @conn/metrics [:requests :github])))
    (conn/record-request :slack)
    (is (= 1 (get-in @conn/metrics [:requests :slack])))))

(deftest test-record-error
  (testing "Record error increments error counter by type"
    (conn/reset-metrics)
    (conn/record-error :github java.net.SocketTimeoutException)
    (is (= 1 (get-in @conn/metrics [:errors :github java.net.SocketTimeoutException])))
    (conn/record-error :github java.net.SocketTimeoutException)
    (is (= 2 (get-in @conn/metrics [:errors :github java.net.SocketTimeoutException])))
    (conn/record-error :github java.io.IOException)
    (is (= 1 (get-in @conn/metrics [:errors :github java.io.IOException])))))

(deftest test-record-latency
  (testing "Record latency tracks min, max, total, count"
    (conn/reset-metrics)
    (conn/record-latency :github 100)
    (let [latencies (get-in @conn/metrics [:latencies :github])]
      (is (= 1 (:count latencies)))
      (is (= 100 (:total latencies)))
      (is (= 100 (:min latencies)))
      (is (= 100 (:max latencies))))
    
    (conn/record-latency :github 200)
    (let [latencies (get-in @conn/metrics [:latencies :github])]
      (is (= 2 (:count latencies)))
      (is (= 300 (:total latencies)))
      (is (= 100 (:min latencies)))
      (is (= 200 (:max latencies))))
    
    (conn/record-latency :github 50)
    (let [latencies (get-in @conn/metrics [:latencies :github])]
      (is (= 3 (:count latencies)))
      (is (= 350 (:total latencies)))
      (is (= 50 (:min latencies)))
      (is (= 200 (:max latencies))))))

(deftest test-record-cache-hit-miss
  (testing "Record cache hits and misses"
    (conn/reset-metrics)
    (conn/record-cache-hit :github)
    (conn/record-cache-hit :github)
    (conn/record-cache-miss :github)
    (is (= 2 (get-in @conn/metrics [:cache-hits :github])))
    (is (= 1 (get-in @conn/metrics [:cache-misses :github])))))

(deftest test-record-rate-limit-wait
  (testing "Record rate limit waits"
    (conn/reset-metrics)
    (conn/record-rate-limit-wait :github)
    (conn/record-rate-limit-wait :github)
    (is (= 2 (get-in @conn/metrics [:rate-limit-waits :github])))))

(deftest test-record-retry
  (testing "Record retry attempts"
    (conn/reset-metrics)
    (conn/record-retry :github 1)
    (conn/record-retry :github 1)
    (conn/record-retry :github 2)
    (is (= 2 (get-in @conn/metrics [:retries :github 1])))
    (is (= 1 (get-in @conn/metrics [:retries :github 2])))))

(deftest test-get-metrics
  (testing "Get metrics for all or specific connector"
    (conn/reset-metrics)
    (conn/record-request :github)
    (conn/record-latency :github 100)
    
    ;; Get all metrics
    (let [all-metrics (conn/get-metrics)]
      (is (map? all-metrics))
      (is (= 1 (get-in all-metrics [:requests :github]))))
    
    ;; Get specific connector metrics
    (let [github-metrics (conn/get-metrics :github)]
      (is (= 1 (:requests github-metrics)))
      (is (map? (:latencies github-metrics)))
      (is (= 1 (get-in github-metrics [:latencies :count]))))))

(deftest test-get-average-latency
  (testing "Calculate average latency"
    (conn/reset-metrics)
    (conn/record-latency :github 100)
    (conn/record-latency :github 200)
    (conn/record-latency :github 300)
    (is (= 200 (conn/get-average-latency :github)))
    
    ;; No data should return 0
    (is (= 0 (conn/get-average-latency :nonexistent)))))

(deftest test-get-error-rate
  (testing "Calculate error rate"
    (conn/reset-metrics)
    (conn/record-request :github)
    (conn/record-request :github)
    (conn/record-request :github)
    (conn/record-request :github)
    (conn/record-error :github Exception)
    (is (= 0.25 (conn/get-error-rate :github)))
    
    ;; No requests should return 0.0
    (is (= 0.0 (conn/get-error-rate :nonexistent)))))

(deftest test-get-cache-hit-rate
  (testing "Calculate cache hit rate"
    (conn/reset-metrics)
    (conn/record-cache-hit :github)
    (conn/record-cache-hit :github)
    (conn/record-cache-hit :github)
    (conn/record-cache-miss :github)
    (is (= 0.75 (conn/get-cache-hit-rate :github)))
    
    ;; No cache activity should return 0.0
    (is (= 0.0 (conn/get-cache-hit-rate :nonexistent)))))

(deftest test-reset-metrics
  (testing "Reset all metrics"
    (conn/record-request :github)
    (conn/record-latency :github 100)
    (conn/reset-metrics)
    (is (= {} (:requests @conn/metrics)))
    (is (= {} (:latencies @conn/metrics))))
  
  (testing "Reset specific connector metrics"
    (conn/record-request :github)
    (conn/record-request :slack)
    (conn/reset-metrics :github)
    (is (nil? (get-in @conn/metrics [:requests :github])))
    (is (= 1 (get-in @conn/metrics [:requests :slack])))))

(deftest test-get-metrics-summary
  (testing "Get metrics summary for all connectors"
    (conn/reset-metrics)
    (conn/record-request :github)
    (conn/record-request :github)
    (conn/record-latency :github 100)
    (conn/record-latency :github 200)
    (conn/record-cache-hit :github)
    (conn/record-cache-miss :github)
    
    (let [summary (conn/get-metrics-summary)]
      (is (map? summary))
      (is (contains? summary :github))
      (is (= 2 (get-in summary [:github :requests])))
      (is (= 150 (get-in summary [:github :avg-latency-ms])))
      (is (= 0.5 (get-in summary [:github :cache-hit-rate]))))))

;; ============================================
;; Circuit Breaker Tests
;; ============================================

(deftest test-circuit-breaker-config
  (testing "Circuit breaker configuration is defined"
    (is (map? conn/circuit-breaker-config))
    (is (= 5 (:failure-threshold conn/circuit-breaker-config)))
    (is (= 3 (:success-threshold conn/circuit-breaker-config)))
    (is (= 60000 (:timeout-ms conn/circuit-breaker-config)))
    (is (= 3 (:half-open-max-calls conn/circuit-breaker-config)))))

(deftest test-circuit-breakers-atom
  (testing "Circuit breakers atom is initialized"
    (is (instance? clojure.lang.Atom conn/circuit-breakers))
    (is (map? @conn/circuit-breakers))))

(deftest test-get-circuit-state
  (testing "Get circuit state returns default for unknown connector"
    (conn/reset-all-circuits)
    (let [state (conn/get-circuit-state :unknown)]
      (is (= :closed (:state state)))
      (is (= 0 (:failure-count state)))
      (is (= 0 (:success-count state))))))

(deftest test-circuit-closed-by-default
  (testing "Circuit is closed by default"
    (conn/reset-all-circuits)
    (is (false? (conn/circuit-open? :github)))
    (is (false? (conn/circuit-half-open? :github)))))

(deftest test-record-circuit-failure-opens-circuit
  (testing "Recording failures opens circuit after threshold"
    (conn/reset-all-circuits)
    ;; Record failures up to threshold
    (dotimes [_ 4]
      (conn/record-circuit-failure :github))
    (is (false? (conn/circuit-open? :github)))
    
    ;; One more failure should open the circuit
    (conn/record-circuit-failure :github)
    (is (true? (conn/circuit-open? :github)))))

(deftest test-record-circuit-success-in-half-open
  (testing "Recording successes in half-open closes circuit"
    (conn/reset-all-circuits)
    ;; Open the circuit
    (dotimes [_ 5]
      (conn/record-circuit-failure :github))
    (is (true? (conn/circuit-open? :github)))
    
    ;; Manually set to half-open for testing
    (swap! conn/circuit-breakers assoc :github
           {:state :half-open :failure-count 0 :success-count 0 :half-open-calls 0})
    
    ;; Record successes
    (conn/record-circuit-success :github)
    (conn/record-circuit-success :github)
    (is (= :half-open (:state (conn/get-circuit-state :github))))
    
    ;; Third success should close the circuit
    (conn/record-circuit-success :github)
    (is (= :closed (:state (conn/get-circuit-state :github))))))

(deftest test-reset-circuit
  (testing "Reset circuit clears state"
    (conn/record-circuit-failure :github)
    (conn/reset-circuit :github)
    (is (nil? (get @conn/circuit-breakers :github)))))

(deftest test-reset-all-circuits
  (testing "Reset all circuits clears all states"
    (conn/record-circuit-failure :github)
    (conn/record-circuit-failure :slack)
    (conn/reset-all-circuits)
    (is (= {} @conn/circuit-breakers))))

(deftest test-get-circuit-status
  (testing "Get circuit status returns summary"
    (conn/reset-all-circuits)
    (conn/record-circuit-failure :github)
    (conn/record-circuit-failure :github)
    (let [status (conn/get-circuit-status)]
      (is (map? status))
      (is (contains? status :github))
      (is (= :closed (get-in status [:github :state])))
      (is (= 2 (get-in status [:github :failure-count]))))))

(deftest test-with-circuit-breaker-success
  (testing "with-circuit-breaker returns success on successful call"
    (conn/reset-all-circuits)
    (let [result (conn/with-circuit-breaker :github #(+ 1 2))]
      (is (true? (:success result)))
      (is (= 3 (:value result)))
      (is (= :closed (:circuit-state result))))))

(deftest test-with-circuit-breaker-failure
  (testing "with-circuit-breaker returns failure on exception"
    (conn/reset-all-circuits)
    (let [result (conn/with-circuit-breaker :github #(throw (Exception. "test error")))]
      (is (false? (:success result)))
      (is (= "test error" (:error result)))
      (is (= :closed (:circuit-state result))))))

(deftest test-with-circuit-breaker-open-circuit
  (testing "with-circuit-breaker fails fast when circuit is open"
    (conn/reset-all-circuits)
    ;; Open the circuit
    (dotimes [_ 5]
      (conn/record-circuit-failure :github))
    
    (let [result (conn/with-circuit-breaker :github #(+ 1 2))]
      (is (false? (:success result)))
      (is (= "Circuit breaker is open" (:error result)))
      (is (= :open (:circuit-state result))))))

;; ============================================
;; Health Check Tests
;; ============================================

(deftest test-health-check-file-connector
  (testing "Health check for file connector"
    (let [test-dir "/tmp/mental-models-health-test"
          _ (.mkdirs (java.io.File. test-dir))
          connector (conn/create-file-connector test-dir)
          result (conn/health-check-connector :file connector)]
      (is (map? result))
      (is (contains? result :healthy))
      (is (contains? result :latency-ms))
      (is (contains? result :connector-type))
      (is (= :file (:connector-type result)))
      (is (number? (:latency-ms result)))
      ;; Cleanup
      (.delete (java.io.File. test-dir)))))

(deftest test-health-check-unknown-connector
  (testing "Health check for unknown connector type"
    (let [result (conn/health-check-connector :unknown {})]
      (is (false? (:healthy result)))
      (is (= :unknown (:connector-type result)))
      (is (= "Unknown connector type" (get-in result [:details :error]))))))

(deftest test-get-system-health
  (testing "Get system health returns comprehensive status"
    (conn/reset-metrics)
    (conn/reset-all-circuits)
    (conn/clear-cache)
    
    (let [health (conn/get-system-health)]
      (is (map? health))
      (is (contains? health :timestamp))
      (is (contains? health :metrics-summary))
      (is (contains? health :circuit-breakers))
      (is (contains? health :rate-limiters))
      (is (contains? health :cache-size))
      (is (map? (:metrics-summary health)))
      (is (map? (:circuit-breakers health)))
      (is (map? (:rate-limiters health)))
      (is (number? (:cache-size health))))))

;; ============================================
;; Bulk Operations Tests
;; ============================================

(deftest test-bulk-request-success
  (testing "Bulk request executes multiple functions in parallel"
    (conn/reset-all-circuits)
    (let [requests [{:connector-type :test
                     :connector {}
                     :fn (fn [_] {:result 1})}
                    {:connector-type :test
                     :connector {}
                     :fn (fn [_] {:result 2})}
                    {:connector-type :test
                     :connector {}
                     :fn (fn [_] {:result 3})}]
          results (conn/bulk-request requests)]
      (is (vector? results))
      (is (= 3 (count results)))
      (is (every? #(contains? % :success) results)))))

(deftest test-bulk-request-with-failures
  (testing "Bulk request handles failures gracefully"
    (conn/reset-all-circuits)
    (let [requests [{:connector-type :test
                     :connector {}
                     :fn (fn [_] {:result 1})}
                    {:connector-type :test
                     :connector {}
                     :fn (fn [_] (throw (Exception. "Test error")))}
                    {:connector-type :test
                     :connector {}
                     :fn (fn [_] {:result 3})}]
          results (conn/bulk-request requests)]
      (is (vector? results))
      (is (= 3 (count results)))
      ;; First and third should succeed
      (is (true? (:success (first results))))
      ;; Second should fail
      (is (false? (:success (second results)))))))

;; ============================================
;; Connector Lifecycle Management Tests
;; ============================================

(deftest test-active-connectors-atom
  (testing "Active connectors atom is initialized"
    (is (instance? clojure.lang.Atom conn/active-connectors))
    (is (map? @conn/active-connectors))))

(deftest test-register-connector
  (testing "Register connector adds to active connectors"
    (conn/shutdown-all-connectors)
    (let [connector (conn/create-file-connector "/tmp")]
      (conn/register-connector "test-file" :file connector)
      (is (contains? @conn/active-connectors "test-file"))
      (is (= :file (get-in @conn/active-connectors ["test-file" :type])))
      (is (= :active (get-in @conn/active-connectors ["test-file" :status]))))))

(deftest test-unregister-connector
  (testing "Unregister connector removes from active connectors"
    (conn/shutdown-all-connectors)
    (let [connector (conn/create-file-connector "/tmp")]
      (conn/register-connector "test-file" :file connector)
      (is (contains? @conn/active-connectors "test-file"))
      (conn/unregister-connector "test-file")
      (is (not (contains? @conn/active-connectors "test-file"))))))

(deftest test-get-connector
  (testing "Get connector retrieves registered connector"
    (conn/shutdown-all-connectors)
    (let [connector (conn/create-file-connector "/tmp")]
      (conn/register-connector "test-file" :file connector)
      (let [retrieved (conn/get-connector "test-file")]
        (is (= connector retrieved)))
      (is (nil? (conn/get-connector "nonexistent"))))))

(deftest test-list-active-connectors
  (testing "List active connectors returns all registered"
    (conn/shutdown-all-connectors)
    (conn/register-connector "file1" :file (conn/create-file-connector "/tmp"))
    (conn/register-connector "file2" :file (conn/create-file-connector "/var"))
    (let [connectors (conn/list-active-connectors)]
      (is (vector? connectors))
      (is (= 2 (count connectors)))
      (is (every? #(contains? % :name) connectors))
      (is (every? #(contains? % :type) connectors))
      (is (every? #(contains? % :status) connectors)))))

(deftest test-health-check-all
  (testing "Health check all runs checks on all registered connectors"
    (conn/shutdown-all-connectors)
    (let [test-dir "/tmp/health-check-all-test"
          _ (.mkdirs (java.io.File. test-dir))]
      (conn/register-connector "test-file" :file (conn/create-file-connector test-dir))
      (let [results (conn/health-check-all)]
        (is (map? results))
        (is (contains? results "test-file"))
        (is (contains? (get results "test-file") :healthy)))
      ;; Cleanup
      (.delete (java.io.File. test-dir)))))

(deftest test-shutdown-all-connectors
  (testing "Shutdown all connectors cleans up everything"
    (conn/register-connector "test" :file (conn/create-file-connector "/tmp"))
    (conn/record-request :github)
    (conn/record-circuit-failure :github)
    (conn/set-cached :github {:data "test"} "key")
    
    (let [result (conn/shutdown-all-connectors)]
      (is (= :shutdown (:status result)))
      (is (contains? result :timestamp))
      (is (= {} @conn/active-connectors))
      (is (= {} @conn/circuit-breakers))
      (is (= {} (:requests @conn/metrics)))
      (is (= {} @conn/response-cache)))))

;; ============================================
;; Request/Response Logging Tests
;; ============================================

(deftest test-request-log-atom
  (testing "Request log atom is initialized"
    (is (instance? clojure.lang.Atom conn/request-log))
    (is (vector? @conn/request-log))))

(deftest test-log-config
  (testing "Log config is properly defined"
    (is (map? conn/log-config))
    (is (= 1000 (:max-entries conn/log-config)))
    (is (true? (:log-request-body conn/log-config)))
    (is (true? (:log-response-body conn/log-config)))
    (is (set? (:redact-headers conn/log-config)))
    (is (contains? (:redact-headers conn/log-config) "authorization"))))

(deftest test-redact-sensitive
  (testing "Redact sensitive headers"
    (let [headers {"content-type" "application/json"
                   "authorization" "Bearer secret-token"
                   "x-api-key" "my-api-key"}
          redacted (conn/redact-sensitive headers)]
      (is (= "application/json" (get redacted "content-type")))
      (is (= "[REDACTED]" (get redacted "authorization")))
      (is (= "[REDACTED]" (get redacted "x-api-key"))))))

(deftest test-log-request
  (testing "Log request creates entry"
    (conn/clear-request-log)
    (let [request-id (conn/log-request :github :get "https://api.github.com/repos"
                                       :headers {"accept" "application/json"}
                                       :body nil)]
      (is (uuid? request-id))
      (is (= 1 (count @conn/request-log)))
      (let [entry (first @conn/request-log)]
        (is (= :request (:type entry)))
        (is (= :github (:connector-type entry)))
        (is (= :get (:method entry)))
        (is (= "https://api.github.com/repos" (:url entry)))))))

(deftest test-log-response
  (testing "Log response creates entry"
    (conn/clear-request-log)
    (let [request-id (java.util.UUID/randomUUID)
          response-id (conn/log-response request-id 200
                                         :headers {"content-type" "application/json"}
                                         :body "{\"data\": \"test\"}"
                                         :latency-ms 150)]
      (is (uuid? response-id))
      (is (= 1 (count @conn/request-log)))
      (let [entry (first @conn/request-log)]
        (is (= :response (:type entry)))
        (is (= request-id (:request-id entry)))
        (is (= 200 (:status entry)))
        (is (= 150 (:latency-ms entry)))))))

(deftest test-get-request-log
  (testing "Get request log with filtering"
    (conn/clear-request-log)
    (conn/log-request :github :get "https://api.github.com/repos")
    (conn/log-request :slack :post "https://slack.com/api/chat.postMessage")
    (conn/log-request :github :get "https://api.github.com/users")
    
    ;; Get all logs
    (is (= 3 (count (conn/get-request-log))))
    
    ;; Filter by connector type
    (is (= 2 (count (conn/get-request-log :connector-type :github))))
    (is (= 1 (count (conn/get-request-log :connector-type :slack))))
    
    ;; Limit results
    (is (= 2 (count (conn/get-request-log :limit 2))))))

(deftest test-get-request-by-id
  (testing "Get request by ID returns request and response"
    (conn/clear-request-log)
    (let [request-id (conn/log-request :github :get "https://api.github.com/repos")]
      (conn/log-response request-id 200 :latency-ms 100)
      (let [result (conn/get-request-by-id request-id)]
        (is (map? result))
        (is (contains? result :request))
        (is (contains? result :response))
        (is (= request-id (get-in result [:request :id])))
        (is (= request-id (get-in result [:response :request-id])))))))

(deftest test-clear-request-log
  (testing "Clear request log removes all entries"
    (conn/log-request :github :get "https://api.github.com/repos")
    (conn/log-request :slack :post "https://slack.com/api/chat.postMessage")
    (is (pos? (count @conn/request-log)))
    (conn/clear-request-log)
    (is (= 0 (count @conn/request-log)))))

(deftest test-get-log-stats
  (testing "Get log stats returns statistics"
    (conn/clear-request-log)
    (let [req1 (conn/log-request :github :get "https://api.github.com/repos")
          req2 (conn/log-request :slack :post "https://slack.com/api/chat.postMessage")]
      (conn/log-response req1 200 :latency-ms 100)
      (conn/log-response req2 500 :latency-ms 200 :error "Server error")
      
      (let [stats (conn/get-log-stats)]
        (is (map? stats))
        (is (= 4 (:total-entries stats)))
        (is (= 2 (:total-requests stats)))
        (is (= 2 (:total-responses stats)))
        (is (= 1 (:error-count stats)))
        (is (= 1 (get-in stats [:by-connector :github])))
        (is (= 1 (get-in stats [:by-connector :slack])))
        (is (= 150 (:avg-latency-ms stats)))))))

;; ============================================
;; Timeout Configuration Tests
;; ============================================

(deftest test-timeout-config-atom
  (testing "Timeout config atom is initialized with defaults"
    (is (instance? clojure.lang.Atom conn/timeout-config))
    (is (map? @conn/timeout-config))
    (is (contains? @conn/timeout-config :github))
    (is (contains? @conn/timeout-config :slack))
    (is (contains? @conn/timeout-config :huggingface))
    (is (contains? @conn/timeout-config :lm-studio))
    (is (contains? @conn/timeout-config :web-scraper))
    (is (contains? @conn/timeout-config :default))))

(deftest test-timeout-config-values
  (testing "Timeout config has correct default values"
    ;; GitHub: 30s socket, 10s connection
    (is (= 30000 (get-in @conn/timeout-config [:github :socket-timeout])))
    (is (= 10000 (get-in @conn/timeout-config [:github :connection-timeout])))
    
    ;; Huggingface: 60s socket, 15s connection
    (is (= 60000 (get-in @conn/timeout-config [:huggingface :socket-timeout])))
    (is (= 15000 (get-in @conn/timeout-config [:huggingface :connection-timeout])))
    
    ;; LM Studio: 120s socket, 10s connection
    (is (= 120000 (get-in @conn/timeout-config [:lm-studio :socket-timeout])))
    (is (= 10000 (get-in @conn/timeout-config [:lm-studio :connection-timeout])))
    
    ;; Web scraper: 45s socket, 15s connection
    (is (= 45000 (get-in @conn/timeout-config [:web-scraper :socket-timeout])))
    (is (= 15000 (get-in @conn/timeout-config [:web-scraper :connection-timeout])))))

(deftest test-get-timeout-config
  (testing "Get timeout config returns correct values"
    (let [github-config (conn/get-timeout-config :github)]
      (is (map? github-config))
      (is (= 30000 (:socket-timeout github-config)))
      (is (= 10000 (:connection-timeout github-config))))
    
    ;; Unknown connector type should return default
    (let [unknown-config (conn/get-timeout-config :unknown)]
      (is (map? unknown-config))
      (is (= 30000 (:socket-timeout unknown-config)))
      (is (= 10000 (:connection-timeout unknown-config))))))

(deftest test-set-timeout-config
  (testing "Set timeout config updates values"
    ;; Save original value
    (let [original (get @conn/timeout-config :github)]
      ;; Set new values
      (conn/set-timeout-config :github 60000 20000)
      (let [updated (conn/get-timeout-config :github)]
        (is (= 60000 (:socket-timeout updated)))
        (is (= 20000 (:connection-timeout updated))))
      
      ;; Restore original
      (conn/set-timeout-config :github (:socket-timeout original) (:connection-timeout original)))))

(deftest test-set-timeout-config-new-connector
  (testing "Set timeout config for new connector type"
    (conn/set-timeout-config :custom-connector 90000 30000)
    (let [config (conn/get-timeout-config :custom-connector)]
      (is (= 90000 (:socket-timeout config)))
      (is (= 30000 (:connection-timeout config))))
    ;; Clean up
    (swap! conn/timeout-config dissoc :custom-connector)))

(deftest test-get-http-opts-for-connector
  (testing "Get HTTP opts includes connector-specific timeouts"
    (let [opts (conn/get-http-opts-for-connector :github)]
      (is (map? opts))
      (is (= 30000 (:socket-timeout opts)))
      (is (= 10000 (:connection-timeout opts)))
      (is (false? (:throw-exceptions opts))))))

;; ============================================
;; Event Hooks Tests
;; ============================================

(deftest test-event-hooks-atom
  (testing "Event hooks atom is initialized with all event types"
    (is (instance? clojure.lang.Atom conn/event-hooks))
    (is (map? @conn/event-hooks))
    (is (contains? @conn/event-hooks :request-start))
    (is (contains? @conn/event-hooks :request-complete))
    (is (contains? @conn/event-hooks :request-error))
    (is (contains? @conn/event-hooks :circuit-open))
    (is (contains? @conn/event-hooks :circuit-close))
    (is (contains? @conn/event-hooks :rate-limit-hit))
    (is (contains? @conn/event-hooks :cache-hit))
    (is (contains? @conn/event-hooks :cache-miss))))

(deftest test-register-hook
  (testing "Register hook adds hook and returns ID"
    (conn/clear-hooks)
    (let [received-events (atom [])
          hook-fn (fn [event] (swap! received-events conj event))
          hook-id (conn/register-hook :request-start hook-fn)]
      (is (uuid? hook-id))
      (is (= 1 (count (conn/list-hooks :request-start))))
      ;; Clean up
      (conn/clear-hooks))))

(deftest test-unregister-hook
  (testing "Unregister hook removes hook by ID"
    (conn/clear-hooks)
    (let [hook-fn (fn [_] nil)
          hook-id (conn/register-hook :request-start hook-fn)]
      (is (= 1 (count (conn/list-hooks :request-start))))
      (conn/unregister-hook :request-start hook-id)
      (is (= 0 (count (conn/list-hooks :request-start)))))))

(deftest test-clear-hooks
  (testing "Clear hooks removes all hooks"
    (conn/register-hook :request-start (fn [_] nil))
    (conn/register-hook :request-complete (fn [_] nil))
    (conn/register-hook :request-error (fn [_] nil))
    
    ;; Clear specific event type
    (conn/clear-hooks :request-start)
    (is (= 0 (count (conn/list-hooks :request-start))))
    (is (= 1 (count (conn/list-hooks :request-complete))))
    
    ;; Clear all hooks
    (conn/clear-hooks)
    (is (= 0 (count (conn/list-hooks :request-complete))))
    (is (= 0 (count (conn/list-hooks :request-error))))))

(deftest test-emit-event
  (testing "Emit event calls all registered hooks"
    (conn/clear-hooks)
    (let [received-events (atom [])
          hook-fn1 (fn [event] (swap! received-events conj {:hook 1 :event event}))
          hook-fn2 (fn [event] (swap! received-events conj {:hook 2 :event event}))]
      (conn/register-hook :request-start hook-fn1)
      (conn/register-hook :request-start hook-fn2)
      
      (conn/emit-event :request-start {:connector-type :github :url "https://api.github.com"})
      
      ;; Both hooks should have been called
      (is (= 2 (count @received-events)))
      (is (some #(= 1 (:hook %)) @received-events))
      (is (some #(= 2 (:hook %)) @received-events))
      
      ;; Event data should include timestamp and event-type
      (let [event (:event (first @received-events))]
        (is (= :request-start (:event-type event)))
        (is (contains? event :timestamp))
        (is (= :github (:connector-type event))))
      
      ;; Clean up
      (conn/clear-hooks))))

(deftest test-emit-event-handles-hook-errors
  (testing "Emit event continues even if a hook throws"
    (conn/clear-hooks)
    (let [received-events (atom [])
          failing-hook (fn [_] (throw (Exception. "Hook error")))
          working-hook (fn [event] (swap! received-events conj event))]
      (conn/register-hook :request-start failing-hook)
      (conn/register-hook :request-start working-hook)
      
      ;; Should not throw, and working hook should still be called
      (conn/emit-event :request-start {:test true})
      (is (= 1 (count @received-events)))
      
      ;; Clean up
      (conn/clear-hooks))))

(deftest test-list-hooks
  (testing "List hooks returns registered hooks"
    (conn/clear-hooks)
    (conn/register-hook :request-start (fn [_] nil))
    (conn/register-hook :request-complete (fn [_] nil))
    
    ;; List all hooks
    (let [all-hooks (conn/list-hooks)]
      (is (map? all-hooks))
      (is (= 1 (count (:request-start all-hooks))))
      (is (= 1 (count (:request-complete all-hooks)))))
    
    ;; List hooks for specific event type
    (let [start-hooks (conn/list-hooks :request-start)]
      (is (vector? start-hooks))
      (is (= 1 (count start-hooks))))
    
    ;; Clean up
    (conn/clear-hooks)))

;; ============================================
;; Connector Factory Pattern Tests
;; ============================================

(deftest test-connector-registry-atom
  (testing "Connector registry atom is initialized"
    (is (instance? clojure.lang.Atom conn/connector-registry))
    (is (map? @conn/connector-registry))))

(deftest test-built-in-connector-types-registered
  (testing "Built-in connector types are registered"
    (is (contains? @conn/connector-registry :github))
    (is (contains? @conn/connector-registry :slack))
    (is (contains? @conn/connector-registry :huggingface))
    (is (contains? @conn/connector-registry :lm-studio))
    (is (contains? @conn/connector-registry :zapier))
    (is (contains? @conn/connector-registry :web-scraper))
    (is (contains? @conn/connector-registry :file))))

(deftest test-register-connector-type
  (testing "Register connector type adds to registry"
    (conn/register-connector-type :test-connector
      (fn [config] {:type :test :config config})
      :description "Test connector"
      :required-config [:test-key])
    
    (is (contains? @conn/connector-registry :test-connector))
    (let [type-info (get @conn/connector-registry :test-connector)]
      (is (fn? (:factory-fn type-info)))
      (is (= "Test connector" (:description type-info)))
      (is (= [:test-key] (:required-config type-info))))
    
    ;; Clean up
    (conn/unregister-connector-type :test-connector)))

(deftest test-unregister-connector-type
  (testing "Unregister connector type removes from registry"
    (conn/register-connector-type :temp-connector
      (fn [_] {:type :temp})
      :description "Temporary connector")
    
    (is (contains? @conn/connector-registry :temp-connector))
    (conn/unregister-connector-type :temp-connector)
    (is (not (contains? @conn/connector-registry :temp-connector)))))

(deftest test-list-connector-types
  (testing "List connector types returns all registered types"
    (let [types (conn/list-connector-types)]
      (is (vector? types))
      (is (pos? (count types)))
      (is (every? #(contains? % :type) types))
      (is (every? #(contains? % :description) types))
      (is (every? #(contains? % :required-config) types)))))

(deftest test-create-connector-success
  (testing "Create connector with valid config succeeds"
    (let [connector (conn/create-connector :file {:base-path "/tmp"})]
      (is (map? connector))
      (is (= :file (:type connector)))
      (is (not (contains? connector :error))))))

(deftest test-create-connector-missing-config
  (testing "Create connector with missing required config fails"
    (let [result (conn/create-connector :github {})]
      (is (map? result))
      (is (= :error (:status result)))
      (is (str/includes? (:error result) "Missing required config")))))

(deftest test-create-connector-unknown-type
  (testing "Create connector with unknown type fails"
    (let [result (conn/create-connector :nonexistent {:key "value"})]
      (is (map? result))
      (is (= :error (:status result)))
      (is (str/includes? (:error result) "Unknown connector type")))))

(deftest test-create-and-register
  (testing "Create and register connector in one step"
    (conn/shutdown-all-connectors)
    (let [result (conn/create-and-register "test-file" :file {:base-path "/tmp"})]
      (is (map? result))
      (is (= :success (:status result)))
      (is (= "test-file" (:name result)))
      (is (= :file (:connector-type result)))
      (is (contains? @conn/active-connectors "test-file")))
    ;; Clean up
    (conn/shutdown-all-connectors)))

(deftest test-create-and-register-with-error
  (testing "Create and register with missing config returns error"
    (let [result (conn/create-and-register "test-github" :github {})]
      (is (map? result))
      (is (= :error (:status result)))
      (is (not (contains? @conn/active-connectors "test-github"))))))
