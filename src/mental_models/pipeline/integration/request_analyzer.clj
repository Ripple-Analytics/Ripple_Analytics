(ns mental-models.pipeline.integration.request-analyzer
  "Request analyzer for mental model analysis system.
   
   Features:
   - Request pattern analysis
   - Traffic analysis
   - Anomaly detection
   - Request clustering
   - Trend analysis
   - Usage patterns
   - Analysis reports
   - Analysis metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZoneId]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:requests []         ;; recent requests for analysis
         :patterns {}         ;; detected patterns
         :anomalies []        ;; detected anomalies
         :config {:window-size 1000
                  :anomaly-threshold 2.0  ;; standard deviations
                  :pattern-min-count 10
                  :analysis-interval-ms 60000}
         :stats {:requests-analyzed 0
                 :patterns-detected 0
                 :anomalies-detected 0
                 :analyses-run 0}
         :initialized? false}))

;; ============================================================================
;; Request Recording
;; ============================================================================

(defn record-request!
  "Record a request for analysis."
  [request]
  (let [window-size (get-in @state [:config :window-size])
        record {:uri (:uri request)
                :method (:request-method request)
                :status (get-in request [:response :status])
                :duration-ms (get request :duration-ms 0)
                :timestamp (System/currentTimeMillis)
                :client-ip (get-in request [:headers "x-forwarded-for"]
                                   (get request :remote-addr))
                :user-agent (get-in request [:headers "user-agent"])
                :content-type (get-in request [:headers "content-type"])
                :content-length (get-in request [:headers "content-length"])}]
    
    (swap! state update :requests
           (fn [reqs]
             (let [new-reqs (conj reqs record)]
               (if (> (count new-reqs) window-size)
                 (vec (drop 1 new-reqs))
                 new-reqs))))
    (swap! state update-in [:stats :requests-analyzed] inc)
    record))

(defn get-recent-requests
  "Get recent requests."
  [& {:keys [limit since] :or {limit 100}}]
  (let [requests (:requests @state)]
    (cond->> requests
      since (filter #(> (:timestamp %) since))
      true (take-last limit)
      true vec)))

;; ============================================================================
;; Traffic Analysis
;; ============================================================================

(defn analyze-traffic
  "Analyze traffic patterns."
  []
  (let [requests (:requests @state)
        now (System/currentTimeMillis)
        last-minute (filter #(> (:timestamp %) (- now 60000)) requests)
        last-hour (filter #(> (:timestamp %) (- now 3600000)) requests)]
    
    {:requests-per-minute (count last-minute)
     :requests-per-hour (count last-hour)
     :by-method (frequencies (map :method requests))
     :by-status (frequencies (map :status requests))
     :by-uri (frequencies (map :uri requests))
     :avg-duration-ms (when (seq requests)
                        (/ (reduce + (map :duration-ms requests))
                           (count requests)))
     :p50-duration-ms (when (seq requests)
                        (let [sorted (sort (map :duration-ms requests))
                              idx (int (* 0.5 (count sorted)))]
                          (nth sorted idx)))
     :p95-duration-ms (when (seq requests)
                        (let [sorted (sort (map :duration-ms requests))
                              idx (int (* 0.95 (count sorted)))]
                          (nth sorted idx)))
     :p99-duration-ms (when (seq requests)
                        (let [sorted (sort (map :duration-ms requests))
                              idx (int (* 0.99 (count sorted)))]
                          (nth sorted idx)))}))

(defn analyze-by-endpoint
  "Analyze traffic by endpoint."
  []
  (let [requests (:requests @state)
        by-endpoint (group-by :uri requests)]
    (into {}
          (for [[uri reqs] by-endpoint]
            [uri {:count (count reqs)
                  :methods (frequencies (map :method reqs))
                  :statuses (frequencies (map :status reqs))
                  :avg-duration-ms (/ (reduce + (map :duration-ms reqs))
                                      (count reqs))
                  :error-rate (/ (count (filter #(>= (or (:status %) 0) 400) reqs))
                                 (count reqs))}]))))

(defn analyze-by-client
  "Analyze traffic by client."
  []
  (let [requests (:requests @state)
        by-client (group-by :client-ip requests)]
    (into {}
          (for [[ip reqs] by-client
                :when ip]
            [ip {:count (count reqs)
                 :endpoints (frequencies (map :uri reqs))
                 :user-agents (frequencies (map :user-agent reqs))
                 :first-seen (:timestamp (first (sort-by :timestamp reqs)))
                 :last-seen (:timestamp (last (sort-by :timestamp reqs)))}]))))

;; ============================================================================
;; Pattern Detection
;; ============================================================================

(defn- detect-sequential-pattern
  "Detect sequential request patterns."
  [requests]
  (let [uri-sequences (partition 3 1 (map :uri requests))
        sequence-counts (frequencies uri-sequences)
        min-count (get-in @state [:config :pattern-min-count])]
    (for [[seq count] sequence-counts
          :when (>= count min-count)]
      {:type :sequential
       :pattern seq
       :count count
       :confidence (/ count (count requests))})))

(defn- detect-temporal-pattern
  "Detect temporal patterns (e.g., hourly spikes)."
  [requests]
  (let [by-hour (group-by (fn [r]
                            (.getHour (LocalDateTime/ofInstant
                                       (Instant/ofEpochMilli (:timestamp r))
                                       (ZoneId/of "UTC"))))
                          requests)
        hour-counts (into {} (map (fn [[h rs]] [h (count rs)]) by-hour))
        avg-count (/ (reduce + (vals hour-counts)) (max 1 (count hour-counts)))
        threshold (* 1.5 avg-count)]
    (for [[hour count] hour-counts
          :when (> count threshold)]
      {:type :temporal
       :hour hour
       :count count
       :ratio (/ count avg-count)})))

(defn detect-patterns
  "Detect all patterns in requests."
  []
  (swap! state update-in [:stats :analyses-run] inc)
  
  (let [requests (:requests @state)
        sequential-patterns (detect-sequential-pattern requests)
        temporal-patterns (detect-temporal-pattern requests)
        all-patterns (concat sequential-patterns temporal-patterns)]
    
    (swap! state update-in [:stats :patterns-detected] + (count all-patterns))
    (swap! state assoc :patterns
           {:sequential (vec sequential-patterns)
            :temporal (vec temporal-patterns)
            :detected-at (System/currentTimeMillis)})
    
    all-patterns))

(defn get-patterns
  "Get detected patterns."
  []
  (:patterns @state))

;; ============================================================================
;; Anomaly Detection
;; ============================================================================

(defn- calculate-stats
  "Calculate mean and standard deviation."
  [values]
  (when (seq values)
    (let [n (count values)
          mean (/ (reduce + values) n)
          variance (/ (reduce + (map #(Math/pow (- % mean) 2) values)) n)
          std-dev (Math/sqrt variance)]
      {:mean mean :std-dev std-dev :count n})))

(defn- detect-duration-anomalies
  "Detect anomalies in request duration."
  [requests]
  (let [durations (map :duration-ms requests)
        stats (calculate-stats durations)
        threshold (get-in @state [:config :anomaly-threshold])]
    (when stats
      (for [req requests
            :let [z-score (/ (- (:duration-ms req) (:mean stats))
                             (max 1 (:std-dev stats)))]
            :when (> (Math/abs z-score) threshold)]
        {:type :duration
         :request req
         :value (:duration-ms req)
         :z-score z-score
         :mean (:mean stats)
         :std-dev (:std-dev stats)}))))

(defn- detect-rate-anomalies
  "Detect anomalies in request rate."
  [requests]
  (let [by-minute (group-by (fn [r]
                              (quot (:timestamp r) 60000))
                            requests)
        minute-counts (map (fn [[_ rs]] (count rs)) by-minute)
        stats (calculate-stats minute-counts)
        threshold (get-in @state [:config :anomaly-threshold])]
    (when stats
      (for [[minute reqs] by-minute
            :let [count (count reqs)
                  z-score (/ (- count (:mean stats))
                             (max 1 (:std-dev stats)))]
            :when (> (Math/abs z-score) threshold)]
        {:type :rate
         :minute minute
         :count count
         :z-score z-score
         :mean (:mean stats)
         :std-dev (:std-dev stats)}))))

(defn detect-anomalies
  "Detect all anomalies."
  []
  (let [requests (:requests @state)
        duration-anomalies (detect-duration-anomalies requests)
        rate-anomalies (detect-rate-anomalies requests)
        all-anomalies (concat duration-anomalies rate-anomalies)]
    
    (swap! state update-in [:stats :anomalies-detected] + (count all-anomalies))
    (swap! state assoc :anomalies (vec all-anomalies))
    
    (when (seq all-anomalies)
      (events/emit! :anomalies-detected {:count (count all-anomalies)}))
    
    all-anomalies))

(defn get-anomalies
  "Get detected anomalies."
  []
  (:anomalies @state))

;; ============================================================================
;; Trend Analysis
;; ============================================================================

(defn analyze-trends
  "Analyze request trends over time."
  []
  (let [requests (:requests @state)
        now (System/currentTimeMillis)
        intervals [{:name :last-5-min :start (- now 300000)}
                   {:name :last-15-min :start (- now 900000)}
                   {:name :last-hour :start (- now 3600000)}]]
    (into {}
          (for [{:keys [name start]} intervals]
            (let [interval-reqs (filter #(> (:timestamp %) start) requests)
                  count (count interval-reqs)]
              [name {:count count
                     :rate-per-minute (/ count (/ (- now start) 60000.0))
                     :avg-duration-ms (when (seq interval-reqs)
                                        (/ (reduce + (map :duration-ms interval-reqs))
                                           count))
                     :error-rate (when (seq interval-reqs)
                                   (/ (count (filter #(>= (or (:status %) 0) 400)
                                                     interval-reqs))
                                      count))}])))))

;; ============================================================================
;; Analysis Reports
;; ============================================================================

(defn generate-report
  "Generate a comprehensive analysis report."
  []
  {:generated-at (System/currentTimeMillis)
   :traffic (analyze-traffic)
   :by-endpoint (analyze-by-endpoint)
   :by-client (analyze-by-client)
   :patterns (detect-patterns)
   :anomalies (detect-anomalies)
   :trends (analyze-trends)
   :stats (:stats @state)})

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-analyze
  "Ring middleware to record requests for analysis."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)]
      (record-request! (assoc request
                              :duration-ms duration-ms
                              :response {:status (:status response)}))
      response)))

;; ============================================================================
;; Background Analysis
;; ============================================================================

(defn start-background-analysis!
  "Start background analysis task."
  []
  (go-loop []
    (when (:initialized? @state)
      (<! (timeout (get-in @state [:config :analysis-interval-ms])))
      (detect-patterns)
      (detect-anomalies)
      (recur))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-window-size!
  "Set the analysis window size."
  [size]
  (swap! state assoc-in [:config :window-size] size))

(defn set-anomaly-threshold!
  "Set the anomaly detection threshold."
  [threshold]
  (swap! state assoc-in [:config :anomaly-threshold] threshold))

(defn set-pattern-min-count!
  "Set the minimum count for pattern detection."
  [count]
  (swap! state assoc-in [:config :pattern-min-count] count))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-analyzer-metrics
  "Get analyzer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-analyzed (:requests-analyzed stats)
     :patterns-detected (:patterns-detected stats)
     :anomalies-detected (:anomalies-detected stats)
     :analyses-run (:analyses-run stats)
     :requests-in-window (count (:requests @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-analyzer-stats
  "Get analyzer statistics."
  []
  (merge (get-analyzer-metrics)
         {:window-size (get-in @state [:config :window-size])
          :anomaly-threshold (get-in @state [:config :anomaly-threshold])
          :pattern-min-count (get-in @state [:config :pattern-min-count])}))

(defn reset-stats!
  "Reset analyzer statistics."
  []
  (swap! state assoc :stats {:requests-analyzed 0
                             :patterns-detected 0
                             :anomalies-detected 0
                             :analyses-run 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-analyzer!
  "Initialize the request analyzer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (start-background-analysis!)
    (logging/log :info "Request analyzer initialized")
    (events/emit! :request-analyzer-initialized {})
    true))
