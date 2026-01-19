(ns mental-models.pipeline.integration.response-mocker
  "Response mocker for mental model analysis system.
   
   Features:
   - Response mocking
   - Mock definitions
   - Conditional mocking
   - Mock delays
   - Mock errors
   - Mock sequences
   - Mock recording
   - Mocking metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:mocks {}            ;; mock-id -> mock definition
         :sequences {}        ;; sequence-id -> sequence state
         :recordings []       ;; recorded mock calls
         :config {:enabled? true
                  :record-calls? true
                  :max-recordings 10000
                  :default-delay-ms 0}
         :stats {:mocks-served 0
                 :mocks-matched 0
                 :mocks-missed 0
                 :errors-simulated 0}
         :initialized? false}))

;; ============================================================================
;; Mock Definitions
;; ============================================================================

(defn define-mock!
  "Define a mock response."
  [mock-id config]
  (let [mock {:id mock-id
              :name (get config :name (name mock-id))
              :matcher (get config :matcher)  ;; fn or map
              :response (get config :response)
              :response-fn (get config :response-fn)
              :delay-ms (get config :delay-ms 0)
              :error (get config :error)
              :times (atom (get config :times nil))  ;; nil = unlimited
              :priority (get config :priority 100)
              :enabled? (atom true)
              :metrics {:calls (atom 0)}
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:mocks mock-id] mock)
    (logging/log :info "Defined mock" {:mock-id mock-id})
    mock-id))

(defn get-mock
  "Get a mock definition."
  [mock-id]
  (get-in @state [:mocks mock-id]))

(defn list-mocks
  "List all mocks."
  []
  (mapv (fn [[id m]]
          {:id id
           :name (:name m)
           :priority (:priority m)
           :enabled? @(:enabled? m)
           :calls @(get-in m [:metrics :calls])
           :times-remaining @(:times m)})
        (:mocks @state)))

(defn enable-mock!
  "Enable a mock."
  [mock-id]
  (when-let [mock (get-mock mock-id)]
    (reset! (:enabled? mock) true)))

(defn disable-mock!
  "Disable a mock."
  [mock-id]
  (when-let [mock (get-mock mock-id)]
    (reset! (:enabled? mock) false)))

(defn delete-mock!
  "Delete a mock."
  [mock-id]
  (swap! state update :mocks dissoc mock-id))

(defn clear-mocks!
  "Clear all mocks."
  []
  (swap! state assoc :mocks {}))

;; ============================================================================
;; Mock Matching
;; ============================================================================

(defn- matches-request?
  "Check if a mock matches a request."
  [mock request]
  (let [matcher (:matcher mock)]
    (cond
      (fn? matcher) (matcher request)
      (map? matcher) (every? (fn [[k v]]
                               (cond
                                 (= k :uri) (if (instance? java.util.regex.Pattern v)
                                              (re-matches v (:uri request))
                                              (= v (:uri request)))
                                 (= k :method) (= v (:request-method request))
                                 (= k :headers) (every? (fn [[hk hv]]
                                                          (= hv (get-in request [:headers hk])))
                                                        v)
                                 :else true))
                             matcher)
      :else true)))

(defn find-matching-mock
  "Find a mock that matches the request."
  [request]
  (let [mocks (->> (vals (:mocks @state))
                   (filter #@(:enabled? %))
                   (filter #(or (nil? @(:times %)) (pos? @(:times %))))
                   (filter #(matches-request? % request))
                   (sort-by :priority))]
    (first mocks)))

;; ============================================================================
;; Mock Response Generation
;; ============================================================================

(defn- generate-mock-response
  "Generate a mock response."
  [mock request]
  (cond
    ;; Error simulation
    (:error mock)
    (do
      (swap! state update-in [:stats :errors-simulated] inc)
      (throw (ex-info "Simulated error" {:error (:error mock)})))
    
    ;; Dynamic response
    (:response-fn mock)
    ((:response-fn mock) request)
    
    ;; Static response
    (:response mock)
    (:response mock)
    
    ;; Default
    :else
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body {:mocked true}}))

(defn serve-mock
  "Serve a mock response for a request."
  [request]
  (when (get-in @state [:config :enabled?])
    (if-let [mock (find-matching-mock request)]
      (do
        ;; Update metrics
        (swap! (get-in mock [:metrics :calls]) inc)
        (swap! state update-in [:stats :mocks-served] inc)
        (swap! state update-in [:stats :mocks-matched] inc)
        
        ;; Decrement times if limited
        (when @(:times mock)
          (swap! (:times mock) dec))
        
        ;; Record call
        (when (get-in @state [:config :record-calls?])
          (record-call! mock request))
        
        ;; Apply delay
        (when (pos? (:delay-ms mock))
          (Thread/sleep (:delay-ms mock)))
        
        ;; Generate response
        (generate-mock-response mock request))
      
      (do
        (swap! state update-in [:stats :mocks-missed] inc)
        nil))))

;; ============================================================================
;; Mock Sequences
;; ============================================================================

(defn define-sequence!
  "Define a mock sequence."
  [sequence-id responses]
  (let [sequence {:id sequence-id
                  :responses (vec responses)
                  :index (atom 0)
                  :cycle? false
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:sequences sequence-id] sequence)
    sequence-id))

(defn define-cycling-sequence!
  "Define a cycling mock sequence."
  [sequence-id responses]
  (let [sequence {:id sequence-id
                  :responses (vec responses)
                  :index (atom 0)
                  :cycle? true
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:sequences sequence-id] sequence)
    sequence-id))

(defn get-next-in-sequence
  "Get the next response in a sequence."
  [sequence-id]
  (when-let [sequence (get-in @state [:sequences sequence-id])]
    (let [idx @(:index sequence)
          responses (:responses sequence)
          response (get responses idx)]
      (if (:cycle? sequence)
        (swap! (:index sequence) #(mod (inc %) (count responses)))
        (when (< idx (dec (count responses)))
          (swap! (:index sequence) inc)))
      response)))

(defn reset-sequence!
  "Reset a sequence to the beginning."
  [sequence-id]
  (when-let [sequence (get-in @state [:sequences sequence-id])]
    (reset! (:index sequence) 0)))

;; ============================================================================
;; Mock Recording
;; ============================================================================

(defn- record-call!
  "Record a mock call."
  [mock request]
  (let [max-recordings (get-in @state [:config :max-recordings])
        recording {:mock-id (:id mock)
                   :request {:method (:request-method request)
                             :uri (:uri request)
                             :headers (:headers request)}
                   :timestamp (System/currentTimeMillis)}]
    (swap! state update :recordings
           (fn [r]
             (let [new-recordings (conj r recording)]
               (if (> (count new-recordings) max-recordings)
                 (vec (drop 1 new-recordings))
                 new-recordings))))))

(defn get-recordings
  "Get mock call recordings."
  [& {:keys [mock-id limit] :or {limit 100}}]
  (cond->> (:recordings @state)
    mock-id (filter #(= (:mock-id %) mock-id))
    true (take-last limit)
    true vec))

(defn clear-recordings!
  "Clear mock recordings."
  []
  (swap! state assoc :recordings []))

;; ============================================================================
;; Convenience Mocks
;; ============================================================================

(defn mock-success!
  "Define a simple success mock."
  [mock-id uri body]
  (define-mock! mock-id
    {:matcher {:uri uri}
     :response {:status 200
                :headers {"Content-Type" "application/json"}
                :body body}}))

(defn mock-error!
  "Define a simple error mock."
  [mock-id uri status message]
  (define-mock! mock-id
    {:matcher {:uri uri}
     :response {:status status
                :headers {"Content-Type" "application/json"}
                :body {:error message}}}))

(defn mock-delay!
  "Define a delayed mock."
  [mock-id uri delay-ms body]
  (define-mock! mock-id
    {:matcher {:uri uri}
     :delay-ms delay-ms
     :response {:status 200
                :headers {"Content-Type" "application/json"}
                :body body}}))

(defn mock-exception!
  "Define a mock that throws an exception."
  [mock-id uri error-message]
  (define-mock! mock-id
    {:matcher {:uri uri}
     :error {:message error-message}}))

(defn mock-sequence!
  "Define a mock that returns responses in sequence."
  [mock-id uri responses]
  (let [seq-id (keyword (str (name mock-id) "-seq"))]
    (define-sequence! seq-id responses)
    (define-mock! mock-id
      {:matcher {:uri uri}
       :response-fn (fn [_] (get-next-in-sequence seq-id))})))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-mock
  "Ring middleware to serve mocks."
  [handler]
  (fn [request]
    (if-let [mock-response (serve-mock request)]
      mock-response
      (handler request))))

(defn wrap-mock-only
  "Ring middleware that only serves mocks (no fallback)."
  []
  (fn [request]
    (or (serve-mock request)
        {:status 404
         :headers {"Content-Type" "application/json"}
         :body {:error "No mock found"}})))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable mocking."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-record-calls!
  "Enable/disable call recording."
  [enabled?]
  (swap! state assoc-in [:config :record-calls?] enabled?))

(defn set-default-delay!
  "Set default delay for mocks."
  [delay-ms]
  (swap! state assoc-in [:config :default-delay-ms] delay-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-mocker-metrics
  "Get mocker metrics."
  []
  (let [stats (:stats @state)]
    {:mocks-served (:mocks-served stats)
     :mocks-matched (:mocks-matched stats)
     :mocks-missed (:mocks-missed stats)
     :errors-simulated (:errors-simulated stats)
     :mocks-count (count (:mocks @state))
     :sequences-count (count (:sequences @state))
     :recordings-count (count (:recordings @state))
     :match-rate (if (pos? (+ (:mocks-matched stats) (:mocks-missed stats)))
                   (/ (:mocks-matched stats)
                      (+ (:mocks-matched stats) (:mocks-missed stats)))
                   1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-mocker-stats
  "Get mocker statistics."
  []
  (merge (get-mocker-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :record-calls? (get-in @state [:config :record-calls?])
          :default-delay-ms (get-in @state [:config :default-delay-ms])}))

(defn reset-stats!
  "Reset mocker statistics."
  []
  (swap! state assoc :stats {:mocks-served 0
                             :mocks-matched 0
                             :mocks-missed 0
                             :errors-simulated 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-mocker!
  "Initialize the response mocker."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response mocker initialized")
    (events/emit! :response-mocker-initialized {})
    true))
