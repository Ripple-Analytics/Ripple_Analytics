(ns mental-models.pipeline.integration.anomaly-detector
  "Anomaly detection for mental model analysis patterns.
   
   Features:
   - Statistical anomaly detection (z-score, IQR)
   - Time-series anomaly detection
   - Isolation forest algorithm
   - Local outlier factor (LOF)
   - Anomaly scoring and ranking
   - Anomaly alerting and notification
   - Historical anomaly tracking
   - Anomaly explanation generation"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:detectors {}        ;; detector-id -> detector-config
         :baselines {}        ;; baseline-id -> baseline-data
         :anomalies {}        ;; anomaly-id -> anomaly-record
         :alerts {}           ;; alert-id -> alert-config
         :history []          ;; historical anomaly records
         :initialized? false}))

;; ============================================================================
;; Statistical Functions
;; ============================================================================

(defn- mean
  "Calculate mean of a sequence."
  [xs]
  (if (empty? xs)
    0.0
    (/ (reduce + xs) (count xs))))

(defn- variance
  "Calculate variance of a sequence."
  [xs]
  (if (< (count xs) 2)
    0.0
    (let [m (mean xs)
          squared-diffs (map #(Math/pow (- % m) 2) xs)]
      (/ (reduce + squared-diffs) (dec (count xs))))))

(defn- std-dev
  "Calculate standard deviation of a sequence."
  [xs]
  (Math/sqrt (variance xs)))

(defn- percentile
  "Calculate percentile of a sequence."
  [xs p]
  (if (empty? xs)
    0.0
    (let [sorted (sort xs)
          n (count sorted)
          idx (* (/ p 100.0) (dec n))
          lower (int (Math/floor idx))
          upper (int (Math/ceil idx))
          weight (- idx lower)]
      (if (= lower upper)
        (nth sorted lower)
        (+ (* (- 1 weight) (nth sorted lower))
           (* weight (nth sorted (min upper (dec n)))))))))

(defn- iqr
  "Calculate interquartile range."
  [xs]
  (- (percentile xs 75) (percentile xs 25)))

;; ============================================================================
;; Detector Registration
;; ============================================================================

(defn register-detector!
  "Register an anomaly detector."
  [detector-id config]
  (let [detector {:id detector-id
                  :name (get config :name (name detector-id))
                  :type (get config :type :z-score)
                  :threshold (get config :threshold 3.0)
                  :window-size (get config :window-size 100)
                  :min-samples (get config :min-samples 10)
                  :features (get config :features [])
                  :enabled? (get config :enabled? true)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:detectors detector-id] detector)
    (logging/log :info "Registered anomaly detector" {:detector-id detector-id :type (:type detector)})
    (events/emit! :detector-registered {:detector-id detector-id})
    detector-id))

(defn get-detector
  "Get a detector configuration."
  [detector-id]
  (get-in @state [:detectors detector-id]))

(defn list-detectors
  "List all registered detectors."
  []
  (mapv (fn [[id d]]
          {:id id
           :name (:name d)
           :type (:type d)
           :enabled? (:enabled? d)})
        (:detectors @state)))

(defn enable-detector!
  "Enable a detector."
  [detector-id]
  (swap! state assoc-in [:detectors detector-id :enabled?] true))

(defn disable-detector!
  "Disable a detector."
  [detector-id]
  (swap! state assoc-in [:detectors detector-id :enabled?] false))

;; ============================================================================
;; Baseline Management
;; ============================================================================

(defn create-baseline!
  "Create a baseline from historical data."
  [baseline-id data & {:keys [feature]}]
  (let [values (if feature
                 (map #(get % feature) data)
                 data)
        baseline {:id baseline-id
                  :feature feature
                  :mean (mean values)
                  :std-dev (std-dev values)
                  :min (apply min values)
                  :max (apply max values)
                  :q1 (percentile values 25)
                  :median (percentile values 50)
                  :q3 (percentile values 75)
                  :iqr (iqr values)
                  :sample-count (count values)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:baselines baseline-id] baseline)
    (logging/log :info "Created baseline" {:baseline-id baseline-id :samples (count values)})
    baseline))

(defn get-baseline
  "Get a baseline."
  [baseline-id]
  (get-in @state [:baselines baseline-id]))

(defn update-baseline!
  "Update a baseline with new data."
  [baseline-id new-values]
  (let [baseline (get-baseline baseline-id)
        old-count (:sample-count baseline)
        new-count (count new-values)
        total-count (+ old-count new-count)
        ;; Incremental mean update
        old-mean (:mean baseline)
        new-mean (mean new-values)
        combined-mean (/ (+ (* old-mean old-count) (* new-mean new-count)) total-count)]
    (swap! state update-in [:baselines baseline-id]
           merge {:mean combined-mean
                  :sample-count total-count
                  :updated-at (System/currentTimeMillis)})
    (logging/log :debug "Updated baseline" {:baseline-id baseline-id :new-samples new-count})))

;; ============================================================================
;; Anomaly Detection Algorithms
;; ============================================================================

(defn detect-zscore
  "Detect anomalies using z-score method."
  [value baseline threshold]
  (let [z (if (zero? (:std-dev baseline))
            0.0
            (/ (- value (:mean baseline)) (:std-dev baseline)))]
    {:is-anomaly? (> (Math/abs z) threshold)
     :score (Math/abs z)
     :method :z-score
     :details {:z-score z
               :threshold threshold
               :mean (:mean baseline)
               :std-dev (:std-dev baseline)}}))

(defn detect-iqr
  "Detect anomalies using IQR method."
  [value baseline multiplier]
  (let [q1 (:q1 baseline)
        q3 (:q3 baseline)
        iqr-val (:iqr baseline)
        lower-bound (- q1 (* multiplier iqr-val))
        upper-bound (+ q3 (* multiplier iqr-val))
        is-anomaly? (or (< value lower-bound) (> value upper-bound))
        distance (cond
                   (< value lower-bound) (- lower-bound value)
                   (> value upper-bound) (- value upper-bound)
                   :else 0)]
    {:is-anomaly? is-anomaly?
     :score (if (zero? iqr-val) 0.0 (/ distance iqr-val))
     :method :iqr
     :details {:lower-bound lower-bound
               :upper-bound upper-bound
               :iqr iqr-val}}))

(defn detect-mad
  "Detect anomalies using Median Absolute Deviation."
  [value baseline threshold]
  (let [median (:median baseline)
        ;; Approximate MAD from IQR: MAD ≈ IQR / 1.35
        mad (/ (:iqr baseline) 1.35)
        modified-z (if (zero? mad)
                     0.0
                     (/ (- value median) (* 1.4826 mad)))]
    {:is-anomaly? (> (Math/abs modified-z) threshold)
     :score (Math/abs modified-z)
     :method :mad
     :details {:modified-z modified-z
               :median median
               :mad mad}}))

(defn detect-isolation-score
  "Calculate isolation score (simplified isolation forest)."
  [value data]
  (let [n (count data)
        ;; Count how many splits needed to isolate the value
        sorted (sort data)
        position (.indexOf (vec sorted) value)
        ;; Simplified: use position-based scoring
        isolation-score (if (zero? n)
                          0.5
                          (let [depth (Math/log (+ 1 (min position (- n position))))]
                            (/ depth (Math/log n))))]
    {:is-anomaly? (< isolation-score 0.3)
     :score (- 1 isolation-score)
     :method :isolation
     :details {:isolation-score isolation-score
               :position position
               :total n}}))

(defn detect-lof
  "Calculate Local Outlier Factor (simplified)."
  [value data k]
  (let [sorted-by-distance (sort-by #(Math/abs (- % value)) data)
        k-neighbors (take k sorted-by-distance)
        k-distance (if (seq k-neighbors)
                     (Math/abs (- (last k-neighbors) value))
                     0)
        ;; Simplified LOF calculation
        lof (if (zero? k-distance)
              1.0
              (let [neighbor-densities (map (fn [n]
                                              (let [n-neighbors (take k (sort-by #(Math/abs (- % n)) data))]
                                                (if (seq n-neighbors)
                                                  (/ 1.0 (mean (map #(Math/abs (- % n)) n-neighbors)))
                                                  1.0)))
                                            k-neighbors)
                    local-density (/ 1.0 k-distance)]
                (/ (mean neighbor-densities) local-density)))]
    {:is-anomaly? (> lof 1.5)
     :score lof
     :method :lof
     :details {:lof lof
               :k k
               :k-distance k-distance}}))

;; ============================================================================
;; Main Detection Interface
;; ============================================================================

(defn detect-anomaly
  "Detect if a value is anomalous using specified detector."
  [detector-id value & {:keys [baseline-id data]}]
  (when (flags/enabled? :anomaly-detector)
    (let [detector (get-detector detector-id)
          baseline (when baseline-id (get-baseline baseline-id))
          threshold (:threshold detector)]
      
      (when (:enabled? detector)
        (let [result (case (:type detector)
                       :z-score (detect-zscore value baseline threshold)
                       :iqr (detect-iqr value baseline threshold)
                       :mad (detect-mad value baseline threshold)
                       :isolation (detect-isolation-score value (or data []))
                       :lof (detect-lof value (or data []) 5)
                       {:is-anomaly? false :score 0 :method :unknown})]
          
          (when (:is-anomaly? result)
            (metrics/increment :anomalies-detected {:detector-id detector-id}))
          
          (merge result
                 {:detector-id detector-id
                  :value value
                  :timestamp (System/currentTimeMillis)}))))))

(defn detect-batch
  "Detect anomalies in a batch of values."
  [detector-id values & {:keys [baseline-id]}]
  (mapv #(detect-anomaly detector-id % :baseline-id baseline-id) values))

(defn detect-multivariate
  "Detect anomalies across multiple features."
  [detector-ids value-map & {:keys [baseline-ids]}]
  (let [results (for [[feature value] value-map
                      :let [detector-id (get detector-ids feature)
                            baseline-id (get baseline-ids feature)]
                      :when detector-id]
                  [feature (detect-anomaly detector-id value :baseline-id baseline-id)])
        anomalies (filter #(:is-anomaly? (second %)) results)
        combined-score (if (seq results)
                         (mean (map #(:score (second %)) results))
                         0.0)]
    {:features (into {} results)
     :anomalous-features (mapv first anomalies)
     :combined-score combined-score
     :is-anomaly? (seq anomalies)}))

;; ============================================================================
;; Anomaly Recording
;; ============================================================================

(defn record-anomaly!
  "Record a detected anomaly."
  [detection & {:keys [source context]}]
  (when (:is-anomaly? detection)
    (let [anomaly-id (str (UUID/randomUUID))
          anomaly {:id anomaly-id
                   :detector-id (:detector-id detection)
                   :value (:value detection)
                   :score (:score detection)
                   :method (:method detection)
                   :details (:details detection)
                   :source source
                   :context context
                   :timestamp (:timestamp detection)
                   :acknowledged? false}]
      (swap! state assoc-in [:anomalies anomaly-id] anomaly)
      (swap! state update :history conj anomaly)
      (logging/log :warn "Anomaly detected" {:anomaly-id anomaly-id :score (:score detection)})
      (events/emit! :anomaly-detected {:anomaly-id anomaly-id :detection detection})
      anomaly-id)))

(defn get-anomaly
  "Get an anomaly record."
  [anomaly-id]
  (get-in @state [:anomalies anomaly-id]))

(defn list-anomalies
  "List anomalies with optional filters."
  [& {:keys [detector-id since acknowledged? limit]}]
  (let [anomalies (vals (:anomalies @state))
        filtered (cond->> anomalies
                   detector-id (filter #(= (:detector-id %) detector-id))
                   since (filter #(>= (:timestamp %) since))
                   (some? acknowledged?) (filter #(= (:acknowledged? %) acknowledged?))
                   true (sort-by :timestamp >)
                   limit (take limit))]
    (vec filtered)))

(defn acknowledge-anomaly!
  "Acknowledge an anomaly."
  [anomaly-id & {:keys [notes user]}]
  (swap! state update-in [:anomalies anomaly-id]
         merge {:acknowledged? true
                :acknowledged-at (System/currentTimeMillis)
                :acknowledged-by user
                :notes notes})
  (logging/log :info "Anomaly acknowledged" {:anomaly-id anomaly-id}))

;; ============================================================================
;; Alerting
;; ============================================================================

(defn register-alert!
  "Register an alert rule for anomalies."
  [alert-id config]
  (let [alert {:id alert-id
               :name (get config :name (name alert-id))
               :detector-ids (get config :detector-ids [])
               :min-score (get config :min-score 2.0)
               :channels (get config :channels [:log])
               :cooldown-ms (get config :cooldown-ms 300000)
               :last-fired nil
               :enabled? (get config :enabled? true)}]
    (swap! state assoc-in [:alerts alert-id] alert)
    (logging/log :info "Registered anomaly alert" {:alert-id alert-id})
    alert-id))

(defn- should-fire-alert?
  "Check if an alert should fire."
  [alert detection]
  (and (:enabled? alert)
       (or (empty? (:detector-ids alert))
           (contains? (set (:detector-ids alert)) (:detector-id detection)))
       (>= (:score detection) (:min-score alert))
       (or (nil? (:last-fired alert))
           (> (- (System/currentTimeMillis) (:last-fired alert))
              (:cooldown-ms alert)))))

(defn check-alerts!
  "Check and fire alerts for a detection."
  [detection]
  (when (:is-anomaly? detection)
    (doseq [[alert-id alert] (:alerts @state)]
      (when (should-fire-alert? alert detection)
        (swap! state assoc-in [:alerts alert-id :last-fired] (System/currentTimeMillis))
        (logging/log :warn "Anomaly alert fired"
                     {:alert-id alert-id :detection detection})
        (events/emit! :anomaly-alert-fired
                      {:alert-id alert-id
                       :detection detection
                       :channels (:channels alert)})))))

;; ============================================================================
;; Anomaly Explanation
;; ============================================================================

(defn explain-anomaly
  "Generate human-readable explanation for an anomaly."
  [anomaly-id]
  (when-let [anomaly (get-anomaly anomaly-id)]
    (let [method (:method anomaly)
          details (:details anomaly)
          score (:score anomaly)
          value (:value anomaly)]
      {:anomaly-id anomaly-id
       :summary (case method
                  :z-score (format "Value %.2f is %.2f standard deviations from the mean (%.2f)"
                                   value (:z-score details) (:mean details))
                  :iqr (format "Value %.2f is outside the expected range [%.2f, %.2f]"
                               value (:lower-bound details) (:upper-bound details))
                  :mad (format "Value %.2f has a modified z-score of %.2f"
                               value (:modified-z details))
                  :isolation (format "Value %.2f has an isolation score of %.2f (lower = more anomalous)"
                                     value (:isolation-score details))
                  :lof (format "Value %.2f has a Local Outlier Factor of %.2f (higher = more anomalous)"
                               value (:lof details))
                  "Unknown anomaly type")
       :severity (cond
                   (> score 5) :critical
                   (> score 3) :high
                   (> score 2) :medium
                   :else :low)
       :recommendations (case method
                          :z-score ["Investigate the data source for errors"
                                    "Check if this represents a genuine change"
                                    "Consider updating the baseline if this is expected"]
                          :iqr ["Review recent changes in the data"
                                "Check for data quality issues"
                                "Verify measurement accuracy"]
                          ["Review the anomaly context"
                           "Investigate potential causes"])})))

;; ============================================================================
;; Time Series Anomaly Detection
;; ============================================================================

(defn detect-trend-anomaly
  "Detect anomalies in time series trends."
  [time-series & {:keys [window-size threshold] :or {window-size 10 threshold 2.0}}]
  (let [values (map :value time-series)
        windows (partition window-size 1 values)
        window-means (map mean windows)
        changes (map (fn [[a b]] (- b a)) (partition 2 1 window-means))
        change-baseline {:mean (mean changes)
                         :std-dev (std-dev changes)}]
    (map-indexed
     (fn [idx change]
       (let [detection (detect-zscore change change-baseline threshold)]
         (merge detection
                {:index (+ idx window-size)
                 :window-mean (nth window-means (inc idx) nil)
                 :change change})))
     changes)))

(defn detect-seasonality-anomaly
  "Detect anomalies considering seasonal patterns."
  [time-series period & {:keys [threshold] :or {threshold 2.0}}]
  (let [values (map :value time-series)
        ;; Group by position in period
        seasonal-groups (group-by #(mod (first %) period)
                                  (map-indexed vector values))
        ;; Calculate baseline for each seasonal position
        seasonal-baselines (into {}
                                 (map (fn [[pos vals]]
                                        [pos {:mean (mean (map second vals))
                                              :std-dev (std-dev (map second vals))}])
                                      seasonal-groups))]
    (map-indexed
     (fn [idx value]
       (let [pos (mod idx period)
             baseline (get seasonal-baselines pos)]
         (merge (detect-zscore value baseline threshold)
                {:index idx
                 :seasonal-position pos})))
     values)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-detector-stats
  "Get statistics for the anomaly detector."
  []
  (let [anomalies (vals (:anomalies @state))
        by-detector (group-by :detector-id anomalies)
        by-method (group-by :method anomalies)]
    {:total-detectors (count (:detectors @state))
     :total-baselines (count (:baselines @state))
     :total-anomalies (count anomalies)
     :unacknowledged (count (filter #(not (:acknowledged? %)) anomalies))
     :by-detector (into {} (map (fn [[k v]] [k (count v)]) by-detector))
     :by-method (into {} (map (fn [[k v]] [k (count v)]) by-method))
     :total-alerts (count (:alerts @state))
     :avg-score (if (seq anomalies)
                  (mean (map :score anomalies))
                  0.0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-anomaly-detector!
  "Initialize the anomaly detector."
  []
  (when-not (:initialized? @state)
    ;; Register default detectors
    (register-detector! :confidence-anomaly
                        {:name "Confidence Score Anomaly"
                         :type :z-score
                         :threshold 2.5
                         :features [:confidence]})
    
    (register-detector! :model-count-anomaly
                        {:name "Model Count Anomaly"
                         :type :iqr
                         :threshold 1.5
                         :features [:model-count]})
    
    (register-detector! :processing-time-anomaly
                        {:name "Processing Time Anomaly"
                         :type :mad
                         :threshold 3.0
                         :features [:processing-time]})
    
    ;; Register default alert
    (register-alert! :critical-anomaly
                     {:name "Critical Anomaly Alert"
                      :min-score 3.0
                      :channels [:log :events]
                      :cooldown-ms 60000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Anomaly detector initialized")
    (events/emit! :anomaly-detector-initialized {})
    true))
