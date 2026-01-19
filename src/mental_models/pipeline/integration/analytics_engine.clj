(ns mental-models.pipeline.integration.analytics-engine
  "Analytics engine for mental model analysis insights.
   
   Features:
   - Time series analysis
   - Trend detection
   - Correlation analysis
   - Cohort analysis
   - Funnel analysis
   - Retention analysis
   - Statistical testing
   - Custom metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate ZoneId]
           [java.time.temporal ChronoUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:events []           ;; raw events
         :metrics {}          ;; metric-id -> metric-definition
         :cohorts {}          ;; cohort-id -> cohort
         :funnels {}          ;; funnel-id -> funnel
         :reports {}          ;; report-id -> report
         :cache {}            ;; query-hash -> cached-result
         :stats {:queries-executed 0 :events-processed 0}
         :initialized? false}))

;; ============================================================================
;; Event Tracking
;; ============================================================================

(defn track-event!
  "Track an analytics event."
  [event-type properties]
  (when (flags/enabled? :analytics-engine)
    (let [event {:id (str (UUID/randomUUID))
                 :type event-type
                 :properties (or properties {})
                 :user-id (get properties :user-id)
                 :session-id (get properties :session-id)
                 :timestamp (System/currentTimeMillis)}]
      (swap! state update :events
             (fn [events]
               (take-last 100000 (conj events event))))
      (swap! state update-in [:stats :events-processed] inc)
      (:id event))))

(defn query-events
  "Query events."
  [& {:keys [event-type user-id since until limit] :or {limit 1000}}]
  (let [events (:events @state)
        filtered (cond->> events
                   event-type (filter #(= (:type %) event-type))
                   user-id (filter #(= (:user-id %) user-id))
                   since (filter #(>= (:timestamp %) since))
                   until (filter #(<= (:timestamp %) until))
                   true (take-last limit))]
    (vec filtered)))

;; ============================================================================
;; Time Series Analysis
;; ============================================================================

(defn- bucket-by-time
  "Bucket events by time interval."
  [events interval-ms]
  (let [bucketed (group-by (fn [e]
                             (* (quot (:timestamp e) interval-ms) interval-ms))
                           events)]
    (sort-by first bucketed)))

(defn time-series
  "Generate time series data."
  [event-type & {:keys [interval since until aggregation]
                 :or {interval :day aggregation :count}}]
  (let [interval-ms (case interval
                      :minute 60000
                      :hour 3600000
                      :day 86400000
                      :week 604800000
                      86400000)
        events (query-events :event-type event-type :since since :until until :limit 100000)
        bucketed (bucket-by-time events interval-ms)]
    (mapv (fn [[timestamp events]]
            {:timestamp timestamp
             :value (case aggregation
                      :count (count events)
                      :sum (reduce + (map #(get-in % [:properties :value] 0) events))
                      :avg (let [vals (map #(get-in % [:properties :value] 0) events)]
                             (if (seq vals) (/ (reduce + vals) (count vals)) 0))
                      :unique (count (distinct (map :user-id events)))
                      (count events))})
          bucketed)))

(defn detect-trend
  "Detect trend in time series data."
  [time-series-data]
  (when (>= (count time-series-data) 2)
    (let [values (map :value time-series-data)
          n (count values)
          x-mean (/ (dec n) 2.0)
          y-mean (/ (reduce + values) n)
          ;; Calculate slope using least squares
          numerator (reduce + (map-indexed (fn [i v]
                                             (* (- i x-mean) (- v y-mean)))
                                           values))
          denominator (reduce + (map-indexed (fn [i _]
                                               (Math/pow (- i x-mean) 2))
                                             values))
          slope (if (zero? denominator) 0 (/ numerator denominator))
          ;; Determine trend direction
          trend (cond
                  (> slope 0.1) :increasing
                  (< slope -0.1) :decreasing
                  :else :stable)]
      {:trend trend
       :slope slope
       :start-value (first values)
       :end-value (last values)
       :change-percent (if (zero? (first values))
                         0
                         (* 100 (/ (- (last values) (first values)) (first values))))})))

;; ============================================================================
;; Correlation Analysis
;; ============================================================================

(defn- pearson-correlation
  "Calculate Pearson correlation coefficient."
  [xs ys]
  (when (and (seq xs) (seq ys) (= (count xs) (count ys)))
    (let [n (count xs)
          x-mean (/ (reduce + xs) n)
          y-mean (/ (reduce + ys) n)
          numerator (reduce + (map (fn [x y]
                                     (* (- x x-mean) (- y y-mean)))
                                   xs ys))
          x-std (Math/sqrt (/ (reduce + (map #(Math/pow (- % x-mean) 2) xs)) n))
          y-std (Math/sqrt (/ (reduce + (map #(Math/pow (- % y-mean) 2) ys)) n))
          denominator (* n x-std y-std)]
      (if (zero? denominator) 0 (/ numerator denominator)))))

(defn correlate-events
  "Calculate correlation between two event types."
  [event-type-1 event-type-2 & {:keys [interval since until]
                                :or {interval :day}}]
  (let [ts1 (time-series event-type-1 :interval interval :since since :until until)
        ts2 (time-series event-type-2 :interval interval :since since :until until)
        ;; Align time series
        timestamps (set (concat (map :timestamp ts1) (map :timestamp ts2)))
        ts1-map (into {} (map (juxt :timestamp :value) ts1))
        ts2-map (into {} (map (juxt :timestamp :value) ts2))
        aligned-ts (sort timestamps)
        values1 (map #(get ts1-map % 0) aligned-ts)
        values2 (map #(get ts2-map % 0) aligned-ts)
        correlation (pearson-correlation values1 values2)]
    {:event-type-1 event-type-1
     :event-type-2 event-type-2
     :correlation correlation
     :strength (cond
                 (>= (Math/abs correlation) 0.7) :strong
                 (>= (Math/abs correlation) 0.4) :moderate
                 :else :weak)
     :direction (cond
                  (pos? correlation) :positive
                  (neg? correlation) :negative
                  :else :none)}))

;; ============================================================================
;; Cohort Analysis
;; ============================================================================

(defn define-cohort!
  "Define a cohort."
  [cohort-id config]
  (let [cohort {:id cohort-id
                :name (get config :name (name cohort-id))
                :description (get config :description "")
                :criteria (get config :criteria {})
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:cohorts cohort-id] cohort)
    cohort-id))

(defn get-cohort-users
  "Get users in a cohort."
  [cohort-id]
  (when-let [cohort (get-in @state [:cohorts cohort-id])]
    (let [criteria (:criteria cohort)
          events (:events @state)
          ;; Filter users based on criteria
          matching-users (distinct
                          (map :user-id
                               (filter (fn [e]
                                         (every? (fn [[k v]]
                                                   (= (get-in e [:properties k]) v))
                                                 criteria))
                                       events)))]
      (vec (remove nil? matching-users)))))

(defn cohort-retention
  "Calculate cohort retention."
  [cohort-id & {:keys [periods period-type] :or {periods 7 period-type :day}}]
  (let [users (get-cohort-users cohort-id)
        period-ms (case period-type
                    :day 86400000
                    :week 604800000
                    :month 2592000000
                    86400000)
        events (:events @state)
        user-events (group-by :user-id events)
        
        ;; Calculate retention for each period
        retention (mapv (fn [period]
                          (let [period-start (* period period-ms)
                                period-end (* (inc period) period-ms)
                                active-users (count
                                              (filter (fn [user-id]
                                                        (some (fn [e]
                                                                (let [age (- (:timestamp e) (System/currentTimeMillis))]
                                                                  (and (>= age period-start)
                                                                       (< age period-end))))
                                                              (get user-events user-id [])))
                                                      users))]
                            {:period period
                             :active-users active-users
                             :retention-rate (if (pos? (count users))
                                               (/ active-users (count users))
                                               0)}))
                        (range periods))]
    {:cohort-id cohort-id
     :total-users (count users)
     :retention retention}))

;; ============================================================================
;; Funnel Analysis
;; ============================================================================

(defn define-funnel!
  "Define a conversion funnel."
  [funnel-id config]
  (let [funnel {:id funnel-id
                :name (get config :name (name funnel-id))
                :description (get config :description "")
                :steps (get config :steps [])
                :window-ms (get config :window-ms 86400000) ;; 24 hours
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:funnels funnel-id] funnel)
    funnel-id))

(defn analyze-funnel
  "Analyze funnel conversion."
  [funnel-id & {:keys [since until]}]
  (when-let [funnel (get-in @state [:funnels funnel-id])]
    (let [steps (:steps funnel)
          window-ms (:window-ms funnel)
          events (query-events :since since :until until :limit 100000)
          user-events (group-by :user-id events)
          
          ;; Calculate conversion for each step
          step-results (reduce
                        (fn [results step]
                          (let [prev-users (if (empty? results)
                                             (set (keys user-events))
                                             (:users (last results)))
                                step-users (set
                                            (filter (fn [user-id]
                                                      (some #(= (:type %) (:event step))
                                                            (get user-events user-id [])))
                                                    prev-users))]
                            (conj results {:step (:name step)
                                           :event (:event step)
                                           :users step-users
                                           :count (count step-users)
                                           :conversion-rate (if (pos? (count prev-users))
                                                              (/ (count step-users) (count prev-users))
                                                              0)})))
                        []
                        steps)]
      {:funnel-id funnel-id
       :total-users (count (keys user-events))
       :steps (mapv #(dissoc % :users) step-results)
       :overall-conversion (if (and (seq step-results) (pos? (count (keys user-events))))
                             (/ (:count (last step-results)) (count (keys user-events)))
                             0)})))

;; ============================================================================
;; Custom Metrics
;; ============================================================================

(defn define-metric!
  "Define a custom metric."
  [metric-id config]
  (let [metric {:id metric-id
                :name (get config :name (name metric-id))
                :description (get config :description "")
                :formula (get config :formula)
                :event-types (get config :event-types [])
                :aggregation (get config :aggregation :count)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:metrics metric-id] metric)
    metric-id))

(defn calculate-metric
  "Calculate a custom metric."
  [metric-id & {:keys [since until group-by]}]
  (when-let [metric (get-in @state [:metrics metric-id])]
    (let [events (reduce concat
                         (map #(query-events :event-type % :since since :until until)
                              (:event-types metric)))
          grouped (if group-by
                    (clojure.core/group-by #(get-in % [:properties group-by]) events)
                    {:all events})
          results (into {}
                        (map (fn [[k v]]
                               [k (case (:aggregation metric)
                                    :count (count v)
                                    :sum (reduce + (map #(get-in % [:properties :value] 0) v))
                                    :avg (let [vals (map #(get-in % [:properties :value] 0) v)]
                                           (if (seq vals) (/ (reduce + vals) (count vals)) 0))
                                    :unique (count (distinct (map :user-id v)))
                                    (count v))])
                             grouped))]
      {:metric-id metric-id
       :results results
       :total (reduce + (vals results))})))

;; ============================================================================
;; Statistical Testing
;; ============================================================================

(defn- z-score
  "Calculate z-score for A/B test."
  [control-conversions control-total treatment-conversions treatment-total]
  (let [p1 (/ control-conversions control-total)
        p2 (/ treatment-conversions treatment-total)
        p-pooled (/ (+ control-conversions treatment-conversions)
                    (+ control-total treatment-total))
        se (Math/sqrt (* p-pooled (- 1 p-pooled)
                         (+ (/ 1 control-total) (/ 1 treatment-total))))]
    (if (zero? se) 0 (/ (- p2 p1) se))))

(defn ab-test-significance
  "Calculate A/B test statistical significance."
  [control-data treatment-data]
  (let [control-conversions (:conversions control-data)
        control-total (:total control-data)
        treatment-conversions (:conversions treatment-data)
        treatment-total (:total treatment-data)
        z (z-score control-conversions control-total treatment-conversions treatment-total)
        ;; Approximate p-value from z-score
        p-value (if (>= (Math/abs z) 1.96) 0.05 0.1)]
    {:z-score z
     :p-value p-value
     :significant? (< p-value 0.05)
     :control-rate (/ control-conversions control-total)
     :treatment-rate (/ treatment-conversions treatment-total)
     :lift (if (zero? (/ control-conversions control-total))
             0
             (/ (- (/ treatment-conversions treatment-total)
                   (/ control-conversions control-total))
                (/ control-conversions control-total)))}))

;; ============================================================================
;; Reports
;; ============================================================================

(defn create-report!
  "Create an analytics report."
  [report-id config]
  (let [report {:id report-id
                :name (get config :name (name report-id))
                :description (get config :description "")
                :sections (get config :sections [])
                :schedule (get config :schedule nil)
                :recipients (get config :recipients [])
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:reports report-id] report)
    report-id))

(defn generate-report
  "Generate a report."
  [report-id & {:keys [since until]}]
  (when-let [report (get-in @state [:reports report-id])]
    (let [section-data (mapv (fn [section]
                               (case (:type section)
                                 :time-series (assoc section :data
                                                     (time-series (:event-type section)
                                                                  :interval (:interval section :day)
                                                                  :since since :until until))
                                 :funnel (assoc section :data
                                                (analyze-funnel (:funnel-id section)
                                                                :since since :until until))
                                 :metric (assoc section :data
                                                (calculate-metric (:metric-id section)
                                                                  :since since :until until))
                                 section))
                             (:sections report))]
      {:report-id report-id
       :name (:name report)
       :generated-at (System/currentTimeMillis)
       :period {:since since :until until}
       :sections section-data})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-analytics-stats
  "Get analytics engine statistics."
  []
  (let [stats (:stats @state)]
    {:total-events (count (:events @state))
     :total-metrics (count (:metrics @state))
     :total-cohorts (count (:cohorts @state))
     :total-funnels (count (:funnels @state))
     :total-reports (count (:reports @state))
     :queries-executed (:queries-executed stats)
     :events-processed (:events-processed stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-analytics-engine!
  "Initialize the analytics engine."
  []
  (when-not (:initialized? @state)
    ;; Define default metrics
    (define-metric! :daily-active-users
                    {:name "Daily Active Users"
                     :event-types [:page-view :analysis-run]
                     :aggregation :unique})
    
    (define-metric! :analyses-per-user
                    {:name "Analyses Per User"
                     :event-types [:analysis-run]
                     :aggregation :avg})
    
    ;; Define default funnel
    (define-funnel! :analysis-funnel
                    {:name "Analysis Funnel"
                     :steps [{:name "Visit" :event :page-view}
                             {:name "Upload Document" :event :document-upload}
                             {:name "Run Analysis" :event :analysis-run}
                             {:name "View Results" :event :results-view}]})
    
    ;; Define default cohort
    (define-cohort! :power-users
                    {:name "Power Users"
                     :criteria {:user-type "power"}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Analytics engine initialized")
    (events/emit! :analytics-engine-initialized {})
    true))
