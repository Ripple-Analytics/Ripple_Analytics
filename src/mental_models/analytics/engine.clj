(ns mental-models.analytics.engine
  "Analytics engine for mental model analysis - standalone for desktop app.
   
   Features:
   - Scan tracking and statistics
   - Model detection frequency analysis
   - Time series for dashboard display
   - Performance metrics"
  (:require [clojure.string :as str])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private analytics-state
  (atom {:scans []            ;; scan history
         :models-detected {}  ;; model-name -> count
         :files-by-type {}    ;; extension -> count
         :hourly-activity {}  ;; hour -> scan count
         :stats {:total-scans 0
                 :total-files-scanned 0
                 :total-models-found 0
                 :session-start (System/currentTimeMillis)}
         :initialized? false}))

;; ============================================================================
;; Statistical Functions
;; ============================================================================

(defn- mean [xs]
  (if (empty? xs) 0.0 (/ (reduce + xs) (count xs))))

(defn- std-dev [xs]
  (if (< (count xs) 2)
    0.0
    (let [m (mean xs)
          squared-diffs (map #(Math/pow (- % m) 2) xs)]
      (Math/sqrt (/ (reduce + squared-diffs) (dec (count xs)))))))

(defn- percentile [xs p]
  (if (empty? xs)
    0.0
    (let [sorted (vec (sort xs))
          idx (int (* (/ p 100.0) (dec (count sorted))))]
      (nth sorted (min idx (dec (count sorted)))))))

;; ============================================================================
;; Scan Tracking
;; ============================================================================

(defn track-scan!
  "Track a folder scan with results."
  [folder-path files-scanned models-found duration-ms]
  (let [scan {:id (str (UUID/randomUUID))
              :folder folder-path
              :files files-scanned
              :models (vec models-found)
              :model-count (count models-found)
              :duration-ms duration-ms
              :timestamp (System/currentTimeMillis)
              :hour (mod (quot (System/currentTimeMillis) 3600000) 24)}]
    ;; Add to scan history (keep last 1000)
    (swap! analytics-state update :scans
           (fn [scans] (vec (take-last 1000 (conj scans scan)))))
    ;; Update totals
    (swap! analytics-state update-in [:stats :total-scans] inc)
    (swap! analytics-state update-in [:stats :total-files-scanned] + files-scanned)
    (swap! analytics-state update-in [:stats :total-models-found] + (count models-found))
    ;; Track model frequency
    (doseq [model models-found]
      (swap! analytics-state update-in [:models-detected model] (fnil inc 0)))
    ;; Track hourly activity
    (swap! analytics-state update-in [:hourly-activity (:hour scan)] (fnil inc 0))
    scan))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn get-stats
  "Get current statistics for dashboard."
  []
  (let [state @analytics-state
        scans (:scans state)
        durations (map :duration-ms scans)
        files (map :files scans)
        uptime-ms (- (System/currentTimeMillis) 
                     (get-in state [:stats :session-start] (System/currentTimeMillis)))]
    (merge (:stats state)
           {:unique-models (count (:models-detected state))
            :avg-scan-duration-ms (mean durations)
            :avg-files-per-scan (mean files)
            :p95-duration-ms (percentile durations 95)
            :files-per-second (if (pos? (mean durations))
                                (* 1000 (/ (mean files) (mean durations)))
                                0)
            :uptime-ms uptime-ms
            :uptime-hours (/ uptime-ms 3600000.0)
            :scans-per-hour (if (pos? uptime-ms)
                              (* 3600000 (/ (count scans) uptime-ms))
                              0)})))

(defn top-models
  "Get most frequently detected models."
  [& {:keys [limit] :or {limit 20}}]
  (->> (:models-detected @analytics-state)
       (sort-by val >)
       (take limit)
       (mapv (fn [[model cnt]] {:model model :count cnt :pct 0}))))

(defn recent-scans
  "Get recent scan history."
  [& {:keys [limit] :or {limit 10}}]
  (vec (take-last limit (:scans @analytics-state))))

(defn hourly-activity
  "Get hourly activity distribution for chart."
  []
  (let [activity (:hourly-activity @analytics-state)]
    (mapv (fn [hour]
            {:hour hour
             :count (get activity hour 0)
             :label (str hour ":00")})
          (range 24))))

(defn model-category-breakdown
  "Get model counts by category."
  []
  (let [models (:models-detected @analytics-state)
        categories {"Psychology" ["Confirmation Bias" "Anchoring" "Availability Heuristic" 
                                  "Social Proof" "Loss Aversion" "Sunk Cost" "Reciprocity"
                                  "Commitment and Consistency" "Authority" "Liking" "Scarcity"
                                  "Contrast Effect" "Reason-Respecting" "Stress Influence"
                                  "Deprival Super-Reaction" "Incentive-Caused Bias"]
                   "Economics" ["Supply and Demand" "Opportunity Cost" "Comparative Advantage"
                               "Marginal Utility" "Economies of Scale" "Network Effects"
                               "Creative Destruction" "Tragedy of the Commons"]
                   "Physics" ["Critical Mass" "Momentum" "Leverage" "Inertia" "Equilibrium"
                             "Feedback Loops" "Entropy" "Activation Energy"]
                   "Biology" ["Evolution" "Natural Selection" "Adaptation" "Ecosystems"
                             "Red Queen Effect" "Symbiosis" "Carrying Capacity"]
                   "Mathematics" ["Compounding" "Probability" "Permutations" "Combinations"
                                 "Law of Large Numbers" "Regression to Mean" "Power Laws"
                                 "Pareto Principle" "Bayes Theorem"]
                   "Engineering" ["Redundancy" "Margin of Safety" "Breakpoints" 
                                 "Feedback Systems" "Bottlenecks" "Constraints"]}]
    (mapv (fn [[category model-list]]
            {:category category
             :count (reduce + (map #(get models % 0) model-list))
             :models (count (filter #(pos? (get models % 0)) model-list))})
          categories)))

(defn dashboard-summary
  "Get complete dashboard summary data."
  []
  {:stats (get-stats)
   :top-models (top-models :limit 15)
   :recent-scans (recent-scans :limit 5)
   :hourly-activity (hourly-activity)
   :categories (model-category-breakdown)
   :timestamp (System/currentTimeMillis)})

;; ============================================================================
;; Persistence
;; ============================================================================

(defn export-analytics
  "Export analytics data for persistence."
  []
  (select-keys @analytics-state [:scans :models-detected :files-by-type 
                                  :hourly-activity :stats]))

(defn import-analytics!
  "Import previously saved analytics data."
  [data]
  (when (map? data)
    (swap! analytics-state merge data)
    (swap! analytics-state assoc :initialized? true)))

(defn reset-analytics!
  "Reset all analytics data."
  []
  (reset! analytics-state
          {:scans []
           :models-detected {}
           :files-by-type {}
           :hourly-activity {}
           :stats {:total-scans 0
                   :total-files-scanned 0
                   :total-models-found 0
                   :session-start (System/currentTimeMillis)}
           :initialized? true}))

;; Initialize
(when-not (:initialized? @analytics-state)
  (swap! analytics-state assoc :initialized? true))
