(ns mental-models.services.anomaly
  "Anomaly Detection - Identify unusual patterns in decision-making and model usage
   Uses statistical methods and machine learning for pattern detection"
  (:require [mental-models.services.statistics :as stats]
            [taoensso.timbre :as log]))

;; -- Anomaly Types -----------------------------------------------------------

(def anomaly-types
  {:usage-spike "Unusual spike in model usage"
   :effectiveness-drop "Sudden drop in model effectiveness"
   :pattern-break "Break from established decision patterns"
   :correlation-shift "Shift in model correlation patterns"
   :time-anomaly "Unusual timing in decision-making"
   :outcome-outlier "Extreme outcome deviation"})

;; -- Statistical Anomaly Detection -------------------------------------------

(defn detect-zscore-anomalies
  "Detect anomalies using z-score method"
  [values & {:keys [threshold] :or {threshold 2.5}}]
  (when (seq values)
    (let [mean (stats/mean values)
          std-dev (stats/std-dev values)]
      (when (and mean std-dev (pos? std-dev))
        (map-indexed
         (fn [idx val]
           (let [z (/ (- val mean) std-dev)]
             {:index idx
              :value val
              :z-score z
              :is-anomaly (> (Math/abs z) threshold)
              :direction (cond (> z threshold) :high
                               (< z (- threshold)) :low
                               :else :normal)}))
         values)))))

(defn detect-iqr-anomalies
  "Detect anomalies using Interquartile Range method"
  [values & {:keys [multiplier] :or {multiplier 1.5}}]
  (when (seq values)
    (let [sorted (sort values)
          n (count sorted)
          q1 (nth sorted (int (* 0.25 n)))
          q3 (nth sorted (int (* 0.75 n)))
          iqr (- q3 q1)
          lower-bound (- q1 (* multiplier iqr))
          upper-bound (+ q3 (* multiplier iqr))]
      (map-indexed
       (fn [idx val]
         {:index idx
          :value val
          :is-anomaly (or (< val lower-bound) (> val upper-bound))
          :direction (cond (< val lower-bound) :low
                           (> val upper-bound) :high
                           :else :normal)})
       values))))

;; -- Time Series Anomaly Detection -------------------------------------------

(defn detect-trend-anomalies
  "Detect anomalies in time series trends"
  [time-series & {:keys [window-size] :or {window-size 7}}]
  (when (> (count time-series) window-size)
    (let [windows (partition window-size 1 time-series)
          trends (map (fn [window]
                        (let [{:keys [slope]} (stats/linear-regression
                                               (range (count window))
                                               window)]
                          slope))
                      windows)
          trend-anomalies (detect-zscore-anomalies trends :threshold 2.0)]
      (map (fn [anomaly window-start]
             (assoc anomaly
                    :window-start window-start
                    :window-end (+ window-start window-size)))
           trend-anomalies
           (range)))))

(defn detect-seasonality-anomalies
  "Detect anomalies that deviate from seasonal patterns"
  [time-series period]
  (when (> (count time-series) (* 2 period))
    (let [;; Calculate seasonal averages
          seasonal-avg (vec (for [i (range period)]
                              (stats/mean (take-nth period (drop i time-series)))))
          ;; Calculate deviations from seasonal pattern
          deviations (map-indexed
                      (fn [idx val]
                        (let [expected (nth seasonal-avg (mod idx period))]
                          (if (zero? expected)
                            0
                            (/ (- val expected) expected))))
                      time-series)]
      (detect-zscore-anomalies deviations :threshold 2.0))))

;; -- Pattern-Based Anomaly Detection -----------------------------------------

(defn detect-sequence-anomalies
  "Detect anomalies in sequences of categorical data"
  [sequences]
  (when (seq sequences)
    (let [;; Build transition probability matrix
          transitions (frequencies (partition 2 1 sequences))
          from-counts (frequencies (butlast sequences))
          transition-probs (into {}
                                 (map (fn [[[from to] count]]
                                        [[from to] (/ count (get from-counts from 1))])
                                      transitions))
          ;; Find low-probability transitions
          threshold 0.05]
      (map-indexed
       (fn [idx [from to]]
         (let [prob (get transition-probs [from to] 0)]
           {:index idx
            :from from
            :to to
            :probability prob
            :is-anomaly (< prob threshold)}))
       (partition 2 1 sequences)))))

;; -- Decision Pattern Anomalies ----------------------------------------------

(defn analyze-decision-patterns
  "Analyze decision-making patterns for anomalies"
  [decisions]
  (when (seq decisions)
    (let [;; Extract features
          outcomes (map :outcome-rating decisions)
          model-counts (map #(count (:models-applied %)) decisions)
          time-gaps (map #(- (:created-at %2) (:created-at %1))
                         decisions (rest decisions))
          
          ;; Detect anomalies in each dimension
          outcome-anomalies (detect-zscore-anomalies outcomes)
          model-count-anomalies (detect-iqr-anomalies model-counts)
          timing-anomalies (when (seq time-gaps)
                            (detect-zscore-anomalies time-gaps))]
      
      {:outcome-anomalies (filter :is-anomaly outcome-anomalies)
       :model-count-anomalies (filter :is-anomaly model-count-anomalies)
       :timing-anomalies (filter :is-anomaly timing-anomalies)
       :total-anomalies (+ (count (filter :is-anomaly outcome-anomalies))
                           (count (filter :is-anomaly model-count-anomalies))
                           (count (filter :is-anomaly timing-anomalies)))})))

;; -- Model Effectiveness Anomalies -------------------------------------------

(defn analyze-model-effectiveness-anomalies
  "Detect anomalies in model effectiveness over time"
  [usage-data]
  (when (seq usage-data)
    (let [by-model (group-by :model-id usage-data)
          model-anomalies (for [[model-id usages] by-model
                                :let [outcomes (map :outcome-rating usages)
                                      anomalies (detect-zscore-anomalies outcomes)]
                                :when (some :is-anomaly anomalies)]
                            {:model-id model-id
                             :anomalies (filter :is-anomaly anomalies)
                             :usage-count (count usages)})]
      {:models-with-anomalies (count model-anomalies)
       :details model-anomalies})))

;; -- Automated Insights ------------------------------------------------------

(defn generate-anomaly-insights
  "Generate human-readable insights from detected anomalies"
  [anomaly-results]
  (let [insights (atom [])]
    ;; Outcome anomalies
    (when-let [outcome-anomalies (:outcome-anomalies anomaly-results)]
      (when (seq outcome-anomalies)
        (let [high-count (count (filter #(= :high (:direction %)) outcome-anomalies))
              low-count (count (filter #(= :low (:direction %)) outcome-anomalies))]
          (when (pos? high-count)
            (swap! insights conj
                   {:type :positive
                    :message (str high-count " decisions had unusually positive outcomes")
                    :recommendation "Analyze what made these decisions successful"}))
          (when (pos? low-count)
            (swap! insights conj
                   {:type :warning
                    :message (str low-count " decisions had unusually negative outcomes")
                    :recommendation "Review these decisions for potential bias or errors"})))))
    
    ;; Timing anomalies
    (when-let [timing-anomalies (:timing-anomalies anomaly-results)]
      (when (seq timing-anomalies)
        (swap! insights conj
               {:type :info
                :message "Unusual gaps detected in decision-making timing"
                :recommendation "Consider if external factors affected decision frequency"})))
    
    @insights))

;; -- Alert Generation --------------------------------------------------------

(def alert-thresholds
  {:critical {:anomaly-count 10 :z-score 3.5}
   :warning {:anomaly-count 5 :z-score 2.5}
   :info {:anomaly-count 2 :z-score 2.0}})

(defn generate-alerts
  "Generate alerts based on anomaly severity"
  [anomaly-results]
  (let [total-anomalies (:total-anomalies anomaly-results 0)
        max-z-score (apply max 0 (map :z-score
                                      (concat (:outcome-anomalies anomaly-results)
                                              (:timing-anomalies anomaly-results))))]
    (cond
      (or (>= total-anomalies (get-in alert-thresholds [:critical :anomaly-count]))
          (>= max-z-score (get-in alert-thresholds [:critical :z-score])))
      {:level :critical
       :message "Critical number of anomalies detected"
       :action-required true}
      
      (or (>= total-anomalies (get-in alert-thresholds [:warning :anomaly-count]))
          (>= max-z-score (get-in alert-thresholds [:warning :z-score])))
      {:level :warning
       :message "Elevated anomaly activity detected"
       :action-required false}
      
      (or (>= total-anomalies (get-in alert-thresholds [:info :anomaly-count]))
          (>= max-z-score (get-in alert-thresholds [:info :z-score])))
      {:level :info
       :message "Minor anomalies detected"
       :action-required false}
      
      :else nil)))

;; -- Main Analysis Function --------------------------------------------------

(defn run-full-anomaly-analysis
  "Run comprehensive anomaly analysis on all data"
  [decisions model-usage]
  (let [decision-anomalies (analyze-decision-patterns decisions)
        effectiveness-anomalies (analyze-model-effectiveness-anomalies model-usage)
        insights (generate-anomaly-insights decision-anomalies)
        alerts (generate-alerts decision-anomalies)]
    {:decision-anomalies decision-anomalies
     :effectiveness-anomalies effectiveness-anomalies
     :insights insights
     :alerts alerts
     :analyzed-at (System/currentTimeMillis)}))
