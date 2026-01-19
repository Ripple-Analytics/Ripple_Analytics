(ns mental-models.analytics.anomaly
  "Anomaly detection for mental model scan patterns.
   
   Features:
   - Z-score anomaly detection
   - IQR-based outlier detection
   - Trend anomalies
   - Alert generation"
  (:require [clojure.string :as str])
  (:import [java.util UUID]))

;; ============================================================================
;; State
;; ============================================================================

(defonce ^:private anomaly-state
  (atom {:baselines {}      ;; metric -> baseline stats
         :anomalies []      ;; detected anomalies
         :alerts []         ;; active alerts
         :thresholds {:z-score 2.5
                      :iqr-multiplier 1.5
                      :min-samples 10}}))

;; ============================================================================
;; Statistical Functions
;; ============================================================================

(defn- mean [xs]
  (if (empty? xs) 0.0 (double (/ (reduce + xs) (count xs)))))

(defn- std-dev [xs]
  (if (< (count xs) 2)
    0.0
    (let [m (mean xs)
          variance (/ (reduce + (map #(Math/pow (- % m) 2) xs)) (dec (count xs)))]
      (Math/sqrt variance))))

(defn- percentile [xs p]
  (if (empty? xs)
    0.0
    (let [sorted (vec (sort xs))
          idx (int (* (/ p 100.0) (dec (count sorted))))]
      (double (nth sorted (min idx (dec (count sorted))))))))

(defn- iqr [xs]
  (- (percentile xs 75) (percentile xs 25)))

;; ============================================================================
;; Baseline Management
;; ============================================================================

(defn update-baseline!
  "Update baseline statistics for a metric."
  [metric-name values]
  (when (seq values)
    (let [baseline {:mean (mean values)
                    :std-dev (std-dev values)
                    :median (percentile values 50)
                    :q1 (percentile values 25)
                    :q3 (percentile values 75)
                    :iqr (iqr values)
                    :min (apply min values)
                    :max (apply max values)
                    :count (count values)
                    :updated-at (System/currentTimeMillis)}]
      (swap! anomaly-state assoc-in [:baselines metric-name] baseline)
      baseline)))

(defn get-baseline
  "Get baseline for a metric."
  [metric-name]
  (get-in @anomaly-state [:baselines metric-name]))

;; ============================================================================
;; Anomaly Detection
;; ============================================================================

(defn z-score-anomaly?
  "Check if value is anomalous using z-score method."
  [value baseline]
  (when (and baseline (pos? (:std-dev baseline)))
    (let [z (/ (- value (:mean baseline)) (:std-dev baseline))
          threshold (get-in @anomaly-state [:thresholds :z-score])]
      (when (> (Math/abs z) threshold)
        {:type :z-score
         :z-score z
         :threshold threshold
         :severity (cond
                     (> (Math/abs z) 4) :critical
                     (> (Math/abs z) 3) :high
                     :else :medium)}))))

(defn iqr-anomaly?
  "Check if value is anomalous using IQR method."
  [value baseline]
  (when (and baseline (pos? (:iqr baseline)))
    (let [multiplier (get-in @anomaly-state [:thresholds :iqr-multiplier])
          lower-bound (- (:q1 baseline) (* multiplier (:iqr baseline)))
          upper-bound (+ (:q3 baseline) (* multiplier (:iqr baseline)))]
      (when (or (< value lower-bound) (> value upper-bound))
        {:type :iqr
         :lower-bound lower-bound
         :upper-bound upper-bound
         :severity (cond
                     (or (< value (- lower-bound (:iqr baseline)))
                         (> value (+ upper-bound (:iqr baseline)))) :critical
                     :else :high)}))))

(defn detect-anomaly
  "Detect if a value is anomalous for a given metric."
  [metric-name value]
  (let [baseline (get-baseline metric-name)]
    (when baseline
      (or (z-score-anomaly? value baseline)
          (iqr-anomaly? value baseline)))))

(defn record-anomaly!
  "Record a detected anomaly."
  [metric-name value anomaly-info context]
  (let [anomaly {:id (str (UUID/randomUUID))
                 :metric metric-name
                 :value value
                 :anomaly anomaly-info
                 :context context
                 :timestamp (System/currentTimeMillis)}]
    (swap! anomaly-state update :anomalies
           (fn [anomalies] (vec (take-last 100 (conj anomalies anomaly)))))
    anomaly))

;; ============================================================================
;; Scan Anomaly Detection
;; ============================================================================

(defn check-scan-anomalies
  "Check a scan result for anomalies."
  [scan-result]
  (let [anomalies []]
    ;; Check duration anomaly
    (when-let [duration-anomaly (detect-anomaly :scan-duration (:duration-ms scan-result))]
      (record-anomaly! :scan-duration (:duration-ms scan-result) duration-anomaly
                       {:folder (:folder scan-result)}))
    ;; Check files scanned anomaly
    (when-let [files-anomaly (detect-anomaly :files-scanned (:files scan-result))]
      (record-anomaly! :files-scanned (:files scan-result) files-anomaly
                       {:folder (:folder scan-result)}))
    ;; Check models found anomaly
    (when-let [models-anomaly (detect-anomaly :models-found (:model-count scan-result))]
      (record-anomaly! :models-found (:model-count scan-result) models-anomaly
                       {:folder (:folder scan-result)}))))

(defn update-scan-baselines!
  "Update baselines from scan history."
  [scans]
  (when (>= (count scans) (get-in @anomaly-state [:thresholds :min-samples]))
    (update-baseline! :scan-duration (map :duration-ms scans))
    (update-baseline! :files-scanned (map :files scans))
    (update-baseline! :models-found (map :model-count scans))))

;; ============================================================================
;; Alerts
;; ============================================================================

(defn create-alert!
  "Create an alert for an anomaly."
  [anomaly-record]
  (let [alert {:id (str (UUID/randomUUID))
               :anomaly-id (:id anomaly-record)
               :metric (:metric anomaly-record)
               :severity (get-in anomaly-record [:anomaly :severity])
               :message (str "Anomaly detected in " (name (:metric anomaly-record))
                            ": value " (:value anomaly-record)
                            " (" (name (get-in anomaly-record [:anomaly :type])) ")")
               :acknowledged? false
               :created-at (System/currentTimeMillis)}]
    (swap! anomaly-state update :alerts conj alert)
    alert))

(defn get-active-alerts
  "Get unacknowledged alerts."
  []
  (filter (complement :acknowledged?) (:alerts @anomaly-state)))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id]
  (swap! anomaly-state update :alerts
         (fn [alerts]
           (mapv (fn [a]
                   (if (= (:id a) alert-id)
                     (assoc a :acknowledged? true :acknowledged-at (System/currentTimeMillis))
                     a))
                 alerts))))

(defn get-recent-anomalies
  "Get recent anomalies."
  [& {:keys [limit] :or {limit 20}}]
  (vec (take-last limit (:anomalies @anomaly-state))))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn anomaly-summary
  "Get anomaly summary for dashboard."
  []
  {:total-anomalies (count (:anomalies @anomaly-state))
   :active-alerts (count (get-active-alerts))
   :recent-anomalies (get-recent-anomalies :limit 5)
   :baselines (into {} (map (fn [[k v]] [k (select-keys v [:mean :std-dev :count])])
                            (:baselines @anomaly-state)))})

(defn reset-anomalies!
  "Reset anomaly state."
  []
  (reset! anomaly-state
          {:baselines {}
           :anomalies []
           :alerts []
           :thresholds {:z-score 2.5
                        :iqr-multiplier 1.5
                        :min-samples 10}}))
