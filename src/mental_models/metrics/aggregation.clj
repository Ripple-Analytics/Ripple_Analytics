(ns mental-models.metrics.aggregation
  "Metrics Aggregation Module for Mental Models Pipeline
   
   Provides metrics aggregation with:
   - Time-series aggregation
   - Statistical calculations
   - Rolling windows
   - Percentile calculations
   - Histogram buckets"
  (:require
   [clojure.string :as str])
  (:import
   [java.time Instant Duration]
   [java.util.concurrent ConcurrentHashMap]))

;; =============================================================================
;; METRIC TYPES
;; =============================================================================

(defrecord Counter [name value labels])
(defrecord Gauge [name value labels])
(defrecord Histogram [name buckets sum count labels])
(defrecord Summary [name quantiles sum count labels])

;; =============================================================================
;; METRIC STORAGE
;; =============================================================================

(def ^:private metrics (ConcurrentHashMap.))
(def ^:private time-series (atom {}))

(defn metric-key [name labels]
  (str name (when labels (str ":" (hash labels)))))

;; =============================================================================
;; COUNTER OPERATIONS
;; =============================================================================

(defn counter-inc!
  "Increment a counter by 1 or a specified amount."
  ([name] (counter-inc! name 1 nil))
  ([name amount] (counter-inc! name amount nil))
  ([name amount labels]
   (let [key (metric-key name labels)]
     (.compute metrics key
               (fn [_ existing]
                 (if existing
                   (update existing :value + amount)
                   (->Counter name amount labels))))
     (get-counter name labels))))

(defn get-counter [name labels]
  (.get metrics (metric-key name labels)))

(defn reset-counter! [name labels]
  (let [key (metric-key name labels)]
    (.put metrics key (->Counter name 0 labels))))

;; =============================================================================
;; GAUGE OPERATIONS
;; =============================================================================

(defn gauge-set!
  "Set a gauge to a specific value."
  ([name value] (gauge-set! name value nil))
  ([name value labels]
   (let [key (metric-key name labels)]
     (.put metrics key (->Gauge name value labels))
     (->Gauge name value labels))))

(defn gauge-inc!
  "Increment a gauge."
  ([name] (gauge-inc! name 1 nil))
  ([name amount] (gauge-inc! name amount nil))
  ([name amount labels]
   (let [key (metric-key name labels)]
     (.compute metrics key
               (fn [_ existing]
                 (if existing
                   (update existing :value + amount)
                   (->Gauge name amount labels)))))))

(defn gauge-dec!
  "Decrement a gauge."
  ([name] (gauge-dec! name 1 nil))
  ([name amount] (gauge-dec! name amount nil))
  ([name amount labels]
   (gauge-inc! name (- amount) labels)))

(defn get-gauge [name labels]
  (.get metrics (metric-key name labels)))

;; =============================================================================
;; HISTOGRAM OPERATIONS
;; =============================================================================

(def default-buckets [0.005 0.01 0.025 0.05 0.1 0.25 0.5 1 2.5 5 10])

(defn histogram-observe!
  "Record an observation in a histogram."
  ([name value] (histogram-observe! name value nil default-buckets))
  ([name value labels] (histogram-observe! name value labels default-buckets))
  ([name value labels buckets]
   (let [key (metric-key name labels)]
     (.compute metrics key
               (fn [_ existing]
                 (let [hist (or existing (->Histogram name (vec (repeat (count buckets) 0)) 0 0 labels))
                       new-buckets (vec (map-indexed
                                         (fn [i bucket-bound]
                                           (if (<= value bucket-bound)
                                             (inc (nth (:buckets hist) i))
                                             (nth (:buckets hist) i)))
                                         buckets))]
                   (assoc hist
                          :buckets new-buckets
                          :sum (+ (:sum hist) value)
                          :count (inc (:count hist)))))))))

(defn get-histogram [name labels]
  (.get metrics (metric-key name labels)))

;; =============================================================================
;; SUMMARY OPERATIONS
;; =============================================================================

(def ^:private summary-observations (ConcurrentHashMap.))

(defn summary-observe!
  "Record an observation in a summary."
  ([name value] (summary-observe! name value nil))
  ([name value labels]
   (let [key (metric-key name labels)]
     (.compute summary-observations key
               (fn [_ existing]
                 (conj (or existing []) value))))))

(defn calculate-quantile [observations quantile]
  (let [sorted (sort observations)
        n (count sorted)
        idx (int (* quantile (dec n)))]
    (nth sorted idx)))

(defn get-summary [name labels quantiles]
  (let [key (metric-key name labels)
        observations (.get summary-observations key)]
    (when observations
      (->Summary name
                 (into {} (map (fn [q] [q (calculate-quantile observations q)]) quantiles))
                 (reduce + observations)
                 (count observations)
                 labels))))

;; =============================================================================
;; TIME SERIES
;; =============================================================================

(defn record-time-series!
  "Record a value in a time series."
  [name value & {:keys [labels timestamp] :or {timestamp (Instant/now)}}]
  (swap! time-series update name (fnil conj [])
         {:value value :timestamp timestamp :labels labels}))

(defn get-time-series
  "Get time series data."
  ([name] (get @time-series name))
  ([name start-time end-time]
   (filter #(and (.isAfter (:timestamp %) start-time)
                 (.isBefore (:timestamp %) end-time))
           (get @time-series name))))

;; =============================================================================
;; AGGREGATION FUNCTIONS
;; =============================================================================

(defn aggregate-sum [values]
  (reduce + values))

(defn aggregate-avg [values]
  (when (seq values)
    (/ (reduce + values) (count values))))

(defn aggregate-min [values]
  (when (seq values)
    (apply min values)))

(defn aggregate-max [values]
  (when (seq values)
    (apply max values)))

(defn aggregate-count [values]
  (count values))

(defn aggregate-percentile [values p]
  (when (seq values)
    (let [sorted (sort values)
          idx (int (* p (dec (count sorted))))]
      (nth sorted idx))))

;; =============================================================================
;; ROLLING WINDOW
;; =============================================================================

(defn rolling-window
  "Get values within a rolling time window."
  [name window-duration]
  (let [now (Instant/now)
        start (.minus now window-duration)]
    (get-time-series name start now)))

(defn rolling-avg [name window-duration]
  (aggregate-avg (map :value (rolling-window name window-duration))))

(defn rolling-sum [name window-duration]
  (aggregate-sum (map :value (rolling-window name window-duration))))

(defn rolling-rate [name window-duration]
  (let [values (rolling-window name window-duration)]
    (when (>= (count values) 2)
      (let [first-val (:value (first values))
            last-val (:value (last values))
            time-diff (.getSeconds (Duration/between
                                    (:timestamp (first values))
                                    (:timestamp (last values))))]
        (when (pos? time-diff)
          (/ (- last-val first-val) time-diff))))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-all-metrics []
  (into {} metrics))

(defn get-metrics-by-prefix [prefix]
  (into {} (filter #(str/starts-with? (key %) prefix) metrics)))

(defn clear-all-metrics! []
  (.clear metrics)
  (.clear summary-observations)
  (reset! time-series {}))

(defn get-stats []
  {:total-metrics (.size metrics)
   :time-series-count (count @time-series)
   :summary-observations (.size summary-observations)})

;; =============================================================================
;; PROMETHEUS FORMAT
;; =============================================================================

(defn format-prometheus-counter [{:keys [name value labels]}]
  (let [label-str (when labels
                    (str "{" (str/join "," (map (fn [[k v]] (str (clojure.core/name k) "=\"" v "\"")) labels)) "}"))]
    (str name (or label-str "") " " value)))

(defn format-prometheus-gauge [{:keys [name value labels]}]
  (format-prometheus-counter {:name name :value value :labels labels}))

(defn export-prometheus []
  (str/join "\n"
            (for [[_ metric] metrics]
              (cond
                (instance? Counter metric) (format-prometheus-counter metric)
                (instance? Gauge metric) (format-prometheus-gauge metric)
                :else ""))))
