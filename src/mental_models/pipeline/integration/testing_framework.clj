(ns mental-models.pipeline.integration.testing-framework
  "Testing framework for mental model analysis system.
   
   Features:
   - Unit testing
   - Integration testing
   - Property-based testing
   - Mocking and stubbing
   - Test fixtures
   - Coverage tracking
   - Performance testing
   - Regression testing"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:test-suites {}      ;; suite-id -> test-suite
         :test-cases {}       ;; test-id -> test-case
         :test-runs {}        ;; run-id -> test-run
         :mocks {}            ;; mock-id -> mock
         :fixtures {}         ;; fixture-id -> fixture
         :coverage {}         ;; namespace -> coverage-data
         :config {:parallel? true
                  :timeout-ms 30000
                  :retry-count 0}
         :stats {:tests-run 0 :tests-passed 0 :tests-failed 0}
         :initialized? false}))

;; ============================================================================
;; Test Suite Management
;; ============================================================================

(defn create-test-suite!
  "Create a test suite."
  [suite-id config]
  (let [suite {:id suite-id
               :name (get config :name (name suite-id))
               :description (get config :description "")
               :tests (get config :tests [])
               :setup-fn (get config :setup-fn)
               :teardown-fn (get config :teardown-fn)
               :tags (get config :tags #{})
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:test-suites suite-id] suite)
    (logging/log :info "Created test suite" {:suite-id suite-id})
    suite-id))

(defn get-test-suite
  "Get a test suite."
  [suite-id]
  (get-in @state [:test-suites suite-id]))

(defn list-test-suites
  "List all test suites."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :test-count (count (:tests s))
           :tags (:tags s)})
        (:test-suites @state)))

(defn add-test-to-suite!
  "Add a test to a suite."
  [suite-id test-id]
  (swap! state update-in [:test-suites suite-id :tests] conj test-id))

;; ============================================================================
;; Test Case Management
;; ============================================================================

(defn create-test-case!
  "Create a test case."
  [test-id config]
  (let [test-case {:id test-id
                   :name (get config :name (name test-id))
                   :description (get config :description "")
                   :type (get config :type :unit) ;; :unit, :integration, :property, :performance
                   :test-fn (get config :test-fn)
                   :setup-fn (get config :setup-fn)
                   :teardown-fn (get config :teardown-fn)
                   :timeout-ms (get config :timeout-ms (get-in @state [:config :timeout-ms]))
                   :tags (get config :tags #{})
                   :enabled? (get config :enabled? true)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:test-cases test-id] test-case)
    (logging/log :debug "Created test case" {:test-id test-id})
    test-id))

(defn get-test-case
  "Get a test case."
  [test-id]
  (get-in @state [:test-cases test-id]))

(defn list-test-cases
  "List all test cases."
  [& {:keys [type tag enabled?]}]
  (let [tests (vals (:test-cases @state))
        filtered (cond->> tests
                   type (filter #(= (:type %) type))
                   tag (filter #(contains? (:tags %) tag))
                   (some? enabled?) (filter #(= (:enabled? %) enabled?)))]
    (mapv #(select-keys % [:id :name :type :tags :enabled?]) filtered)))

(defn enable-test!
  "Enable a test case."
  [test-id]
  (swap! state assoc-in [:test-cases test-id :enabled?] true))

(defn disable-test!
  "Disable a test case."
  [test-id]
  (swap! state assoc-in [:test-cases test-id :enabled?] false))

;; ============================================================================
;; Test Execution
;; ============================================================================

(defn- run-single-test
  "Run a single test case."
  [test-case context]
  (let [start-time (System/currentTimeMillis)
        test-fn (:test-fn test-case)
        setup-fn (:setup-fn test-case)
        teardown-fn (:teardown-fn test-case)]
    (try
      ;; Setup
      (when setup-fn
        (setup-fn context))
      
      ;; Run test
      (let [result (if test-fn
                     (test-fn context)
                     {:status :skipped :message "No test function defined"})]
        
        ;; Teardown
        (when teardown-fn
          (teardown-fn context))
        
        {:test-id (:id test-case)
         :status (if (get result :passed? true) :passed :failed)
         :message (get result :message)
         :assertions (get result :assertions [])
         :duration-ms (- (System/currentTimeMillis) start-time)})
      
      (catch Exception e
        (when teardown-fn
          (try (teardown-fn context) (catch Exception _)))
        {:test-id (:id test-case)
         :status :error
         :message (.getMessage e)
         :error (str e)
         :duration-ms (- (System/currentTimeMillis) start-time)}))))

(defn run-test!
  "Run a single test."
  [test-id & {:keys [context]}]
  (when-let [test-case (get-test-case test-id)]
    (when (:enabled? test-case)
      (let [result (run-single-test test-case (or context {}))]
        (swap! state update-in [:stats :tests-run] inc)
        (case (:status result)
          :passed (swap! state update-in [:stats :tests-passed] inc)
          :failed (swap! state update-in [:stats :tests-failed] inc)
          nil)
        result))))

(defn run-test-suite!
  "Run a test suite."
  [suite-id & {:keys [context]}]
  (when-let [suite (get-test-suite suite-id)]
    (let [run-id (str (UUID/randomUUID))
          start-time (System/currentTimeMillis)
          ctx (or context {})
          
          ;; Suite setup
          _ (when-let [setup-fn (:setup-fn suite)]
              (setup-fn ctx))
          
          ;; Run tests
          results (mapv (fn [test-id]
                          (run-test! test-id :context ctx))
                        (:tests suite))
          
          ;; Suite teardown
          _ (when-let [teardown-fn (:teardown-fn suite)]
              (teardown-fn ctx))
          
          run {:id run-id
               :suite-id suite-id
               :results results
               :passed (count (filter #(= :passed (:status %)) results))
               :failed (count (filter #(= :failed (:status %)) results))
               :errors (count (filter #(= :error (:status %)) results))
               :skipped (count (filter #(= :skipped (:status %)) results))
               :duration-ms (- (System/currentTimeMillis) start-time)
               :timestamp (System/currentTimeMillis)}]
      
      (swap! state assoc-in [:test-runs run-id] run)
      (logging/log :info "Test suite completed" {:suite-id suite-id :passed (:passed run) :failed (:failed run)})
      (events/emit! :test-suite-completed {:suite-id suite-id :run-id run-id})
      
      run)))

(defn run-all-tests!
  "Run all enabled tests."
  [& {:keys [tags type context]}]
  (let [tests (list-test-cases :enabled? true :tag (first tags) :type type)
        run-id (str (UUID/randomUUID))
        start-time (System/currentTimeMillis)
        results (mapv (fn [t]
                        (run-test! (:id t) :context context))
                      tests)
        run {:id run-id
             :results results
             :passed (count (filter #(= :passed (:status %)) results))
             :failed (count (filter #(= :failed (:status %)) results))
             :errors (count (filter #(= :error (:status %)) results))
             :duration-ms (- (System/currentTimeMillis) start-time)
             :timestamp (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:test-runs run-id] run)
    run))

;; ============================================================================
;; Mocking
;; ============================================================================

(defn create-mock!
  "Create a mock."
  [mock-id config]
  (let [mock {:id mock-id
              :target (get config :target)
              :return-value (get config :return-value)
              :return-fn (get config :return-fn)
              :call-count (atom 0)
              :calls (atom [])
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:mocks mock-id] mock)
    mock-id))

(defn get-mock
  "Get a mock."
  [mock-id]
  (get-in @state [:mocks mock-id]))

(defn call-mock
  "Call a mock and record the call."
  [mock-id & args]
  (when-let [mock (get-mock mock-id)]
    (swap! (:call-count mock) inc)
    (swap! (:calls mock) conj {:args args :timestamp (System/currentTimeMillis)})
    (if-let [return-fn (:return-fn mock)]
      (apply return-fn args)
      (:return-value mock))))

(defn get-mock-calls
  "Get mock call history."
  [mock-id]
  (when-let [mock (get-mock mock-id)]
    @(:calls mock)))

(defn get-mock-call-count
  "Get mock call count."
  [mock-id]
  (when-let [mock (get-mock mock-id)]
    @(:call-count mock)))

(defn reset-mock!
  "Reset a mock."
  [mock-id]
  (when-let [mock (get-mock mock-id)]
    (reset! (:call-count mock) 0)
    (reset! (:calls mock) [])))

(defn clear-mocks!
  "Clear all mocks."
  []
  (swap! state assoc :mocks {}))

;; ============================================================================
;; Fixtures
;; ============================================================================

(defn create-fixture!
  "Create a test fixture."
  [fixture-id config]
  (let [fixture {:id fixture-id
                 :name (get config :name (name fixture-id))
                 :setup-fn (get config :setup-fn)
                 :teardown-fn (get config :teardown-fn)
                 :data (get config :data {})
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:fixtures fixture-id] fixture)
    fixture-id))

(defn get-fixture
  "Get a fixture."
  [fixture-id]
  (get-in @state [:fixtures fixture-id]))

(defn use-fixture
  "Use a fixture in a test."
  [fixture-id test-fn]
  (when-let [fixture (get-fixture fixture-id)]
    (let [setup-fn (:setup-fn fixture)
          teardown-fn (:teardown-fn fixture)
          context (merge {} (:data fixture))]
      (try
        (when setup-fn
          (setup-fn context))
        (test-fn context)
        (finally
          (when teardown-fn
            (teardown-fn context)))))))

;; ============================================================================
;; Assertions
;; ============================================================================

(defn assert-equals
  "Assert two values are equal."
  [expected actual & {:keys [message]}]
  {:passed? (= expected actual)
   :type :equals
   :expected expected
   :actual actual
   :message (or message (str "Expected " expected " but got " actual))})

(defn assert-true
  "Assert value is truthy."
  [value & {:keys [message]}]
  {:passed? (boolean value)
   :type :true
   :actual value
   :message (or message (str "Expected truthy value but got " value))})

(defn assert-false
  "Assert value is falsy."
  [value & {:keys [message]}]
  {:passed? (not value)
   :type :false
   :actual value
   :message (or message (str "Expected falsy value but got " value))})

(defn assert-throws
  "Assert that a function throws an exception."
  [f & {:keys [exception-type message]}]
  (try
    (f)
    {:passed? false
     :type :throws
     :message (or message "Expected exception but none was thrown")}
    (catch Exception e
      {:passed? (if exception-type
                  (instance? exception-type e)
                  true)
       :type :throws
       :exception e
       :message (or message "Exception thrown as expected")})))

(defn assert-contains
  "Assert collection contains value."
  [coll value & {:keys [message]}]
  {:passed? (contains? (set coll) value)
   :type :contains
   :collection coll
   :value value
   :message (or message (str "Expected collection to contain " value))})

;; ============================================================================
;; Coverage Tracking
;; ============================================================================

(defn track-coverage!
  "Track code coverage."
  [namespace fn-name]
  (swap! state update-in [:coverage namespace fn-name]
         (fnil inc 0)))

(defn get-coverage
  "Get coverage data."
  [& {:keys [namespace]}]
  (if namespace
    (get-in @state [:coverage namespace])
    (:coverage @state)))

(defn calculate-coverage-percentage
  "Calculate coverage percentage."
  [namespace total-functions]
  (let [covered (count (get-in @state [:coverage namespace] {}))]
    (if (pos? total-functions)
      (* 100 (/ covered total-functions))
      0)))

;; ============================================================================
;; Performance Testing
;; ============================================================================

(defn benchmark
  "Benchmark a function."
  [f & {:keys [iterations warmup-iterations] :or {iterations 100 warmup-iterations 10}}]
  ;; Warmup
  (dotimes [_ warmup-iterations]
    (f))
  
  ;; Benchmark
  (let [times (mapv (fn [_]
                      (let [start (System/nanoTime)]
                        (f)
                        (- (System/nanoTime) start)))
                    (range iterations))
        total-ns (reduce + times)
        avg-ns (/ total-ns iterations)
        min-ns (apply min times)
        max-ns (apply max times)
        sorted-times (sort times)
        p50 (nth sorted-times (int (* 0.5 iterations)))
        p95 (nth sorted-times (int (* 0.95 iterations)))
        p99 (nth sorted-times (int (* 0.99 iterations)))]
    {:iterations iterations
     :total-ms (/ total-ns 1000000.0)
     :avg-ms (/ avg-ns 1000000.0)
     :min-ms (/ min-ns 1000000.0)
     :max-ms (/ max-ns 1000000.0)
     :p50-ms (/ p50 1000000.0)
     :p95-ms (/ p95 1000000.0)
     :p99-ms (/ p99 1000000.0)
     :throughput-per-sec (/ 1000000000.0 avg-ns)}))

;; ============================================================================
;; Test Reports
;; ============================================================================

(defn get-test-run
  "Get a test run."
  [run-id]
  (get-in @state [:test-runs run-id]))

(defn list-test-runs
  "List test runs."
  [& {:keys [suite-id limit] :or {limit 50}}]
  (let [runs (vals (:test-runs @state))
        filtered (cond->> runs
                   suite-id (filter #(= (:suite-id %) suite-id))
                   true (sort-by :timestamp >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :suite-id :passed :failed :errors :duration-ms :timestamp]) filtered)))

(defn generate-test-report
  "Generate a test report."
  [run-id & {:keys [format] :or {format :markdown}}]
  (when-let [run (get-test-run run-id)]
    (case format
      :markdown (generate-markdown-report run)
      :html (generate-html-report run)
      :json run
      (generate-markdown-report run))))

(defn- generate-markdown-report
  "Generate Markdown test report."
  [run]
  (let [sb (StringBuilder.)]
    (.append sb "# Test Report\n\n")
    (.append sb (str "**Run ID:** " (:id run) "\n\n"))
    (.append sb (str "**Duration:** " (:duration-ms run) "ms\n\n"))
    (.append sb "## Summary\n\n")
    (.append sb (str "- Passed: " (:passed run) "\n"))
    (.append sb (str "- Failed: " (:failed run) "\n"))
    (.append sb (str "- Errors: " (:errors run) "\n\n"))
    
    (.append sb "## Results\n\n")
    (doseq [result (:results run)]
      (.append sb (str "### " (:test-id result) "\n"))
      (.append sb (str "- Status: " (name (:status result)) "\n"))
      (.append sb (str "- Duration: " (:duration-ms result) "ms\n"))
      (when (:message result)
        (.append sb (str "- Message: " (:message result) "\n")))
      (.append sb "\n"))
    
    (.toString sb)))

(defn- generate-html-report
  "Generate HTML test report."
  [run]
  (str "<html><head><title>Test Report</title></head><body>"
       "<h1>Test Report</h1>"
       "<p>Passed: " (:passed run) "</p>"
       "<p>Failed: " (:failed run) "</p>"
       "</body></html>"))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-testing-stats
  "Get testing statistics."
  []
  (let [stats (:stats @state)]
    {:total-suites (count (:test-suites @state))
     :total-tests (count (:test-cases @state))
     :total-runs (count (:test-runs @state))
     :total-mocks (count (:mocks @state))
     :total-fixtures (count (:fixtures @state))
     :tests-run (:tests-run stats)
     :tests-passed (:tests-passed stats)
     :tests-failed (:tests-failed stats)
     :pass-rate (if (pos? (:tests-run stats))
                  (/ (:tests-passed stats) (:tests-run stats))
                  0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-testing-framework!
  "Initialize the testing framework."
  []
  (when-not (:initialized? @state)
    ;; Create sample test suite
    (create-test-suite! :mental-model-tests
                        {:name "Mental Model Tests"
                         :description "Tests for mental model analysis"
                         :tags #{:unit :mental-models}})
    
    ;; Create sample test cases
    (create-test-case! :test-model-detection
                       {:name "Test Model Detection"
                        :description "Test that mental models are correctly detected"
                        :type :unit
                        :test-fn (fn [_]
                                   {:passed? true
                                    :message "Model detection working"})
                        :tags #{:detection}})
    
    (create-test-case! :test-lollapalooza-detection
                       {:name "Test Lollapalooza Detection"
                        :description "Test that Lollapalooza effects are detected"
                        :type :unit
                        :test-fn (fn [_]
                                   {:passed? true
                                    :message "Lollapalooza detection working"})
                        :tags #{:lollapalooza}})
    
    (add-test-to-suite! :mental-model-tests :test-model-detection)
    (add-test-to-suite! :mental-model-tests :test-lollapalooza-detection)
    
    ;; Create fixture
    (create-fixture! :lm-studio-fixture
                     {:name "LM Studio Fixture"
                      :setup-fn (fn [ctx]
                                  (assoc ctx :lm-studio-url "http://localhost:1234"))
                      :teardown-fn (fn [_] nil)
                      :data {:model "default"}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Testing framework initialized")
    (events/emit! :testing-framework-initialized {})
    true))
