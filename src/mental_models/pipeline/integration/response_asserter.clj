(ns mental-models.pipeline.integration.response-asserter
  "Response asserter for mental model analysis system.
   
   Features:
   - Response assertions
   - Schema assertions
   - Value assertions
   - Custom assertions
   - Assertion chains
   - Assertion reports
   - Soft assertions
   - Assertion metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
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
  (atom {:assertions {}       ;; assertion-id -> assertion
         :reports []          ;; assertion reports
         :config {:fail-fast? false
                  :max-reports 10000
                  :log-failures? true}
         :stats {:assertions-run 0
                 :assertions-passed 0
                 :assertions-failed 0
                 :reports-generated 0}
         :initialized? false}))

;; ============================================================================
;; Basic Assertions
;; ============================================================================

(defn assert-status
  "Assert response status code."
  [response expected]
  (let [actual (:status response)
        passed? (if (set? expected)
                  (contains? expected actual)
                  (= actual expected))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :status
     :passed? passed?
     :expected expected
     :actual actual
     :message (if passed?
                "Status assertion passed"
                (str "Expected status " expected " but got " actual))}))

(defn assert-status-success
  "Assert response is successful (2xx)."
  [response]
  (assert-status response #{200 201 202 203 204 205 206}))

(defn assert-status-redirect
  "Assert response is a redirect (3xx)."
  [response]
  (assert-status response #{300 301 302 303 304 305 307 308}))

(defn assert-status-client-error
  "Assert response is a client error (4xx)."
  [response]
  (let [actual (:status response)
        passed? (and (>= actual 400) (< actual 500))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :status-range
     :passed? passed?
     :expected "4xx"
     :actual actual}))

(defn assert-status-server-error
  "Assert response is a server error (5xx)."
  [response]
  (let [actual (:status response)
        passed? (and (>= actual 500) (< actual 600))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :status-range
     :passed? passed?
     :expected "5xx"
     :actual actual}))

;; ============================================================================
;; Header Assertions
;; ============================================================================

(defn assert-header
  "Assert a header value."
  [response header expected]
  (let [actual (get-in response [:headers header])
        passed? (if (fn? expected)
                  (expected actual)
                  (= actual expected))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :header
     :passed? passed?
     :header header
     :expected expected
     :actual actual}))

(defn assert-header-exists
  "Assert a header exists."
  [response header]
  (let [exists? (contains? (:headers response) header)]
    (swap! state update-in [:stats :assertions-run] inc)
    (if exists?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :header-exists
     :passed? exists?
     :header header}))

(defn assert-header-matches
  "Assert a header matches a pattern."
  [response header pattern]
  (let [actual (get-in response [:headers header])
        passed? (and actual (re-matches pattern actual))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :header-matches
     :passed? passed?
     :header header
     :pattern (str pattern)
     :actual actual}))

(defn assert-content-type
  "Assert content type header."
  [response expected]
  (assert-header response "Content-Type" #(str/starts-with? (or % "") expected)))

;; ============================================================================
;; Body Assertions
;; ============================================================================

(defn assert-body
  "Assert response body."
  [response expected]
  (let [actual (:body response)
        passed? (if (fn? expected)
                  (expected actual)
                  (= actual expected))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :body
     :passed? passed?
     :expected (if (fn? expected) "<predicate>" expected)
     :actual actual}))

(defn assert-body-contains
  "Assert body contains a value."
  [response value]
  (let [body (:body response)
        body-str (if (string? body) body (pr-str body))
        passed? (str/includes? body-str (str value))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :body-contains
     :passed? passed?
     :value value}))

(defn assert-body-not-contains
  "Assert body does not contain a value."
  [response value]
  (let [body (:body response)
        body-str (if (string? body) body (pr-str body))
        passed? (not (str/includes? body-str (str value)))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :body-not-contains
     :passed? passed?
     :value value}))

(defn assert-body-matches
  "Assert body matches a pattern."
  [response pattern]
  (let [body (:body response)
        body-str (if (string? body) body (pr-str body))
        passed? (boolean (re-find pattern body-str))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :body-matches
     :passed? passed?
     :pattern (str pattern)}))

;; ============================================================================
;; JSON Body Assertions
;; ============================================================================

(defn assert-json-path
  "Assert a JSON path value."
  [response path expected]
  (let [body (:body response)
        actual (get-in body path)]
    (let [passed? (if (fn? expected)
                    (expected actual)
                    (= actual expected))]
      (swap! state update-in [:stats :assertions-run] inc)
      (if passed?
        (swap! state update-in [:stats :assertions-passed] inc)
        (swap! state update-in [:stats :assertions-failed] inc))
      {:type :json-path
       :passed? passed?
       :path path
       :expected (if (fn? expected) "<predicate>" expected)
       :actual actual})))

(defn assert-json-path-exists
  "Assert a JSON path exists."
  [response path]
  (let [body (:body response)
        exists? (some? (get-in body path))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if exists?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :json-path-exists
     :passed? exists?
     :path path}))

(defn assert-json-array-length
  "Assert JSON array length."
  [response path expected]
  (let [body (:body response)
        array (get-in body path)
        actual (count array)
        passed? (if (fn? expected)
                  (expected actual)
                  (= actual expected))]
    (swap! state update-in [:stats :assertions-run] inc)
    (if passed?
      (swap! state update-in [:stats :assertions-passed] inc)
      (swap! state update-in [:stats :assertions-failed] inc))
    {:type :json-array-length
     :passed? passed?
     :path path
     :expected expected
     :actual actual}))

;; ============================================================================
;; Schema Assertions
;; ============================================================================

(defn assert-schema
  "Assert response matches a schema."
  [response schema]
  (let [body (:body response)
        errors (atom [])]
    
    (doseq [[field spec] schema]
      (let [value (get body field)
            type-check (case (:type spec)
                         :string (string? value)
                         :number (number? value)
                         :integer (integer? value)
                         :boolean (boolean? value)
                         :array (sequential? value)
                         :object (map? value)
                         :null (nil? value)
                         true)]
        (when-not type-check
          (swap! errors conj {:field field :expected (:type spec) :actual (type value)}))
        
        (when (and (:required spec) (nil? value))
          (swap! errors conj {:field field :error :required}))
        
        (when (and (:min-length spec) (string? value) (< (count value) (:min-length spec)))
          (swap! errors conj {:field field :error :min-length}))
        
        (when (and (:max-length spec) (string? value) (> (count value) (:max-length spec)))
          (swap! errors conj {:field field :error :max-length}))))
    
    (let [passed? (empty? @errors)]
      (swap! state update-in [:stats :assertions-run] inc)
      (if passed?
        (swap! state update-in [:stats :assertions-passed] inc)
        (swap! state update-in [:stats :assertions-failed] inc))
      {:type :schema
       :passed? passed?
       :errors @errors})))

;; ============================================================================
;; Custom Assertions
;; ============================================================================

(defn register-assertion!
  "Register a custom assertion."
  [assertion-id config]
  (let [assertion {:id assertion-id
                   :name (get config :name (name assertion-id))
                   :assert-fn (get config :assert-fn)
                   :enabled? (atom true)
                   :metrics {:runs (atom 0)
                             :passes (atom 0)
                             :failures (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:assertions assertion-id] assertion)
    (logging/log :info "Registered assertion" {:assertion-id assertion-id})
    assertion-id))

(defn run-assertion
  "Run a custom assertion."
  [assertion-id response & args]
  (when-let [assertion (get-in @state [:assertions assertion-id])]
    (when @(:enabled? assertion)
      (swap! (get-in assertion [:metrics :runs]) inc)
      (let [result (apply (:assert-fn assertion) response args)]
        (if (:passed? result)
          (swap! (get-in assertion [:metrics :passes]) inc)
          (swap! (get-in assertion [:metrics :failures]) inc))
        result))))

;; ============================================================================
;; Assertion Chains
;; ============================================================================

(defn assert-all
  "Run multiple assertions and collect results."
  [response assertions]
  (let [results (mapv (fn [[assert-fn & args]]
                        (apply assert-fn response args))
                      assertions)
        all-passed? (every? :passed? results)]
    (swap! state update-in [:stats :reports-generated] inc)
    {:passed? all-passed?
     :results results
     :passed-count (count (filter :passed? results))
     :failed-count (count (filter (complement :passed?) results))}))

(defn assert-any
  "Run assertions until one passes."
  [response assertions]
  (let [results (atom [])]
    (loop [remaining assertions]
      (if (empty? remaining)
        {:passed? false :results @results}
        (let [[assert-fn & args] (first remaining)
              result (apply assert-fn response args)]
          (swap! results conj result)
          (if (:passed? result)
            {:passed? true :results @results}
            (recur (rest remaining))))))))

;; ============================================================================
;; Soft Assertions
;; ============================================================================

(defn soft-assert
  "Run an assertion without failing."
  [assert-fn response & args]
  (try
    (apply assert-fn response args)
    (catch Exception e
      {:type :soft-assertion
       :passed? false
       :error (.getMessage e)})))

(defn collect-soft-assertions
  "Collect multiple soft assertions."
  [response assertions]
  (let [results (mapv (fn [[assert-fn & args]]
                        (soft-assert assert-fn response args))
                      assertions)]
    {:all-passed? (every? :passed? results)
     :results results
     :failures (filter (complement :passed?) results)}))

;; ============================================================================
;; Assertion Reports
;; ============================================================================

(defn generate-report
  "Generate an assertion report."
  [response assertions & {:keys [name]}]
  (let [results (assert-all response assertions)
        report {:id (str (UUID/randomUUID))
                :name name
                :timestamp (System/currentTimeMillis)
                :response {:status (:status response)}
                :results (:results results)
                :passed? (:passed? results)
                :passed-count (:passed-count results)
                :failed-count (:failed-count results)}
        max-reports (get-in @state [:config :max-reports])]
    
    (swap! state update :reports
           (fn [r]
             (let [new-reports (conj r report)]
               (if (> (count new-reports) max-reports)
                 (vec (drop 1 new-reports))
                 new-reports))))
    
    (when (and (not (:passed? results))
               (get-in @state [:config :log-failures?]))
      (logging/log :warn "Assertion failed" {:report-id (:id report)
                                              :failures (filter (complement :passed?) (:results results))}))
    
    report))

(defn get-reports
  "Get assertion reports."
  [& {:keys [limit passed?] :or {limit 100}}]
  (cond->> (:reports @state)
    (some? passed?) (filter #(= (:passed? %) passed?))
    true (take-last limit)
    true vec))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-assert
  "Ring middleware to assert responses."
  [handler assertions]
  (fn [request]
    (let [response (handler request)
          result (assert-all response assertions)]
      (when-not (:passed? result)
        (logging/log :warn "Response assertion failed" {:uri (:uri request)}))
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-fail-fast!
  "Enable/disable fail-fast mode."
  [enabled?]
  (swap! state assoc-in [:config :fail-fast?] enabled?))

(defn set-log-failures!
  "Enable/disable failure logging."
  [enabled?]
  (swap! state assoc-in [:config :log-failures?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-asserter-metrics
  "Get asserter metrics."
  []
  (let [stats (:stats @state)]
    {:assertions-run (:assertions-run stats)
     :assertions-passed (:assertions-passed stats)
     :assertions-failed (:assertions-failed stats)
     :reports-generated (:reports-generated stats)
     :custom-assertions-count (count (:assertions @state))
     :reports-count (count (:reports @state))
     :pass-rate (if (pos? (:assertions-run stats))
                  (/ (:assertions-passed stats) (:assertions-run stats))
                  1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-asserter-stats
  "Get asserter statistics."
  []
  (merge (get-asserter-metrics)
         {:fail-fast? (get-in @state [:config :fail-fast?])
          :log-failures? (get-in @state [:config :log-failures?])}))

(defn reset-stats!
  "Reset asserter statistics."
  []
  (swap! state assoc :stats {:assertions-run 0
                             :assertions-passed 0
                             :assertions-failed 0
                             :reports-generated 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-asserter!
  "Initialize the response asserter."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response asserter initialized")
    (events/emit! :response-asserter-initialized {})
    true))
