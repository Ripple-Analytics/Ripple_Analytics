(ns mental-models.metrics-collector
  "Metrics Collector - Comprehensive system health and performance metrics
   
   Tracks code quality, usage patterns, performance, and system health.
   Enables metric-driven development and continuous improvement.
   
   This is a .cljc file - runs on both client and server!"
  #?(:clj (:require [clojure.string :as str]
                    [clojure.java.io :as io]
                    [mental-models.models :as models])
     :cljs (:require [clojure.string :as str]
                     [mental-models.models :as models])))

;; ============================================
;; State Management
;; ============================================

(defonce !metrics (atom {}))
(defonce !metrics-history (atom []))

;; ============================================
;; Code Metrics
;; ============================================

(defn count-lines-of-code
  "Count total lines of code in the system.
   
   Returns:
     Map with LOC breakdown"
  []
  #?(:clj
     (let [src-dir (io/file "src")
           clj-files (filter #(or (.endsWith (.getName %) ".clj")
                                  (.endsWith (.getName %) ".cljc"))
                            (file-seq src-dir))
           total-lines (reduce + (map #(count (line-seq (io/reader %))) clj-files))
           file-count (count clj-files)]
       {:total-lines total-lines
        :total-files file-count
        :avg-lines-per-file (if (pos? file-count) (/ total-lines file-count) 0)})
     :cljs
     {:total-lines 0
      :total-files 0
      :avg-lines-per-file 0
      :note "LOC counting not available in ClojureScript"}))

(defn calculate-code-complexity
  "Calculate code complexity metrics.
   
   Returns:
     Map with complexity metrics"
  []
  (let [all-models (models/get-all-models)
        total-models (count all-models)
        total-failure-modes (reduce + (map #(count (:failure-modes %)) all-models))
        avg-failure-modes (if (pos? total-models) (/ total-failure-modes total-models) 0)
        categories (count (models/get-all-categories))]
    {:total-models total-models
     :total-failure-modes total-failure-modes
     :avg-failure-modes-per-model avg-failure-modes
     :total-categories categories
     :models-per-category (if (pos? categories) (/ total-models categories) 0)}))

;; ============================================
;; Usage Metrics
;; ============================================

(defonce !usage-stats (atom {:model-queries 0
                             :analysis-runs 0
                             :searches 0
                             :model-access {}
                             :category-access {}}))

(defn track-model-access
  "Track when a model is accessed.
   
   Args:
     model-name: Name of the accessed model"
  [model-name]
  (swap! !usage-stats update :model-queries inc)
  (swap! !usage-stats update-in [:model-access model-name] (fnil inc 0)))

(defn track-analysis-run
  "Track when an analysis is run.
   
   Args:
     analysis-type: Type of analysis (e.g., 'latticework', 'lollapalooza')"
  [analysis-type]
  (swap! !usage-stats update :analysis-runs inc)
  (swap! !usage-stats update-in [:analysis-types analysis-type] (fnil inc 0)))

(defn track-search
  "Track when a search is performed.
   
   Args:
     query: Search query string"
  [query]
  (swap! !usage-stats update :searches inc))

(defn get-usage-statistics
  "Get current usage statistics.
   
   Returns:
     Map with usage stats"
  []
  @!usage-stats)

(defn get-most-used-models
  "Get most frequently accessed models.
   
   Args:
     n: Number of top models to return (default 10)
   
   Returns:
     Vector of [model-name access-count] tuples"
  ([] (get-most-used-models 10))
  ([n]
   (let [model-access (:model-access @!usage-stats)]
     (take n (sort-by second > model-access)))))

;; ============================================
;; Performance Metrics
;; ============================================

(defonce !performance-stats (atom {:analysis-times []
                                   :query-times []
                                   :load-times []}))

(defn track-performance
  "Track performance of an operation.
   
   Args:
     operation-type: Type of operation (:analysis, :query, :load)
     duration-ms: Duration in milliseconds"
  [operation-type duration-ms]
  (swap! !performance-stats update operation-type (fnil conj []) duration-ms))

(defn get-performance-statistics
  "Get performance statistics.
   
   Returns:
     Map with performance stats"
  []
  (let [stats @!performance-stats]
    (into {}
          (map (fn [[op-type times]]
                 [op-type (if (empty? times)
                           {:count 0
                            :avg 0
                            :min 0
                            :max 0}
                           {:count (count times)
                            :avg (/ (reduce + times) (count times))
                            :min (apply min times)
                            :max (apply max times)
                            :p50 (nth (sort times) (quot (count times) 2))
                            :p95 (nth (sort times) (int (* 0.95 (count times))))
                            :p99 (nth (sort times) (int (* 0.99 (count times))))})])
               stats))))

;; ============================================
;; System Health Metrics
;; ============================================

(defn calculate-system-health
  "Calculate overall system health score.
   
   Returns:
     Health score (0.0-100.0) and breakdown"
  []
  (let [complexity (calculate-code-complexity)
        usage (get-usage-statistics)
        performance (get-performance-statistics)
        
        ;; Health components (0-100 each)
        model-coverage-score (* 100 (min 1.0 (/ (:total-models complexity) 150)))
        failure-mode-score (* 100 (min 1.0 (/ (:total-failure-modes complexity) 750)))
        usage-score (if (pos? (:model-queries usage))
                     (min 100 (* 10 (Math/log (+ 1 (:model-queries usage)))))
                     0)
        performance-score (if-let [avg-analysis-time (get-in performance [:analysis-times :avg])]
                           (max 0 (- 100 (* 2 avg-analysis-time)))
                           100)
        
        ;; Overall health (weighted average)
        overall-health (/ (+ (* 0.3 model-coverage-score)
                            (* 0.3 failure-mode-score)
                            (* 0.2 usage-score)
                            (* 0.2 performance-score))
                         1.0)]
    {:overall-health overall-health
     :components {:model-coverage model-coverage-score
                  :failure-mode-coverage failure-mode-score
                  :usage-activity usage-score
                  :performance performance-score}
     :status (cond
              (>= overall-health 80) "Excellent"
              (>= overall-health 60) "Good"
              (>= overall-health 40) "Fair"
              :else "Needs Improvement")}))

;; ============================================
;; Development Velocity Metrics
;; ============================================

(defonce !development-history (atom []))

(defn record-development-milestone
  "Record a development milestone.
   
   Args:
     milestone: Map with :date, :type, :description, :metrics"
  [milestone]
  (swap! !development-history conj (assoc milestone :timestamp #?(:clj (java.time.Instant/now)
                                                                   :cljs (js/Date.)))))

(defn calculate-development-velocity
  "Calculate development velocity metrics.
   
   Returns:
     Map with velocity metrics"
  []
  (let [history @!development-history
        total-milestones (count history)
        recent-milestones (take 10 (reverse history))
        milestone-types (frequencies (map :type history))]
    {:total-milestones total-milestones
     :recent-milestones (count recent-milestones)
     :milestone-breakdown milestone-types
     :velocity-trend (if (>= total-milestones 2)
                      (let [recent-rate (count recent-milestones)
                            older-rate (count (take 10 (drop 10 (reverse history))))]
                        (if (pos? older-rate)
                          (/ recent-rate older-rate)
                          1.0))
                      1.0)}))

;; ============================================
;; Comprehensive Metrics Collection
;; ============================================

(defn collect-all-metrics
  "Collect all system metrics.
   
   Returns:
     Comprehensive metrics map"
  []
  (let [timestamp #?(:clj (java.time.Instant/now)
                    :cljs (js/Date.))
        loc (count-lines-of-code)
        complexity (calculate-code-complexity)
        usage (get-usage-statistics)
        performance (get-performance-statistics)
        health (calculate-system-health)
        velocity (calculate-development-velocity)
        
        metrics {:timestamp timestamp
                 :code {:lines-of-code loc
                        :complexity complexity}
                 :usage usage
                 :performance performance
                 :health health
                 :development-velocity velocity}]
    
    ;; Store current metrics
    (reset! !metrics metrics)
    
    ;; Add to history
    (swap! !metrics-history conj metrics)
    
    metrics))

(defn get-current-metrics
  "Get current system metrics.
   
   Returns:
     Latest metrics map"
  []
  (if (empty? @!metrics)
    (collect-all-metrics)
    @!metrics))

(defn get-metrics-trend
  "Get metrics trend over time.
   
   Args:
     metric-path: Path to metric (e.g., [:health :overall-health])
     n: Number of historical points (default 10)
   
   Returns:
     Vector of metric values over time"
  ([metric-path] (get-metrics-trend metric-path 10))
  ([metric-path n]
   (let [history (take n (reverse @!metrics-history))]
     (map #(get-in % metric-path) history))))

;; ============================================
;; Reporting
;; ============================================

(defn generate-metrics-report
  "Generate comprehensive metrics report.
   
   Returns:
     Formatted metrics report string"
  []
  (let [metrics (get-current-metrics)
        health (:health metrics)
        complexity (get-in metrics [:code :complexity])
        usage (:usage metrics)
        performance (:performance metrics)]
    (str "=== Mental Models System Metrics Report ===\n\n"
         "System Health: " (format "%.1f" (:overall-health health)) "/100 - " (:status health) "\n\n"
         "Code Metrics:\n"
         "  Total Models: " (:total-models complexity) "\n"
         "  Total Failure Modes: " (:total-failure-modes complexity) "\n"
         "  Categories: " (:total-categories complexity) "\n"
         "  Avg Failure Modes/Model: " (format "%.1f" (:avg-failure-modes-per-model complexity)) "\n\n"
         "Usage Statistics:\n"
         "  Model Queries: " (:model-queries usage) "\n"
         "  Analysis Runs: " (:analysis-runs usage) "\n"
         "  Searches: " (:searches usage) "\n\n"
         "Performance:\n"
         "  Analysis Times: " (get-in performance [:analysis-times :count] 0) " runs\n"
         "  Avg Analysis Time: " (format "%.2f" (get-in performance [:analysis-times :avg] 0.0)) "ms\n\n"
         "Health Components:\n"
         "  Model Coverage: " (format "%.1f" (get-in health [:components :model-coverage])) "/100\n"
         "  Failure Mode Coverage: " (format "%.1f" (get-in health [:components :failure-mode-coverage])) "/100\n"
         "  Usage Activity: " (format "%.1f" (get-in health [:components :usage-activity])) "/100\n"
         "  Performance: " (format "%.1f" (get-in health [:components :performance])) "/100\n")))

(defn save-metrics-to-file
  "Save metrics to file.
   
   Args:
     filepath: Path to save file
   
   Returns:
     Success boolean"
  [filepath]
  #?(:clj
     (try
       (spit filepath (pr-str {:current-metrics @!metrics
                               :metrics-history @!metrics-history
                               :usage-stats @!usage-stats
                               :performance-stats @!performance-stats}))
       true
       (catch Exception e
         (println "Error saving metrics:" (.getMessage e))
         false))
     :cljs
     (println "File saving not supported in ClojureScript")))

(defn load-metrics-from-file
  "Load metrics from file.
   
   Args:
     filepath: Path to load file
   
   Returns:
     Success boolean"
  [filepath]
  #?(:clj
     (try
       (let [data (read-string (slurp filepath))]
         (reset! !metrics (:current-metrics data))
         (reset! !metrics-history (:metrics-history data))
         (reset! !usage-stats (:usage-stats data))
         (reset! !performance-stats (:performance-stats data))
         true)
       (catch Exception e
         (println "Error loading metrics:" (.getMessage e))
         false))
     :cljs
     (println "File loading not supported in ClojureScript")))

;; ============================================
;; Usage Examples
;; ============================================

(comment
  ;; Collect all metrics
  (collect-all-metrics)
  
  ;; Get current metrics
  (get-current-metrics)
  
  ;; Generate report
  (println (generate-metrics-report))
  
  ;; Track usage
  (track-model-access "circle-of-competence")
  (track-analysis-run "latticework")
  (track-search "margin of safety")
  
  ;; Track performance
  (track-performance :analysis 150)
  (track-performance :query 25)
  
  ;; Get most used models
  (get-most-used-models 10)
  
  ;; Get metrics trend
  (get-metrics-trend [:health :overall-health] 20)
  
  ;; Save/load metrics
  (save-metrics-to-file "data/metrics.edn")
  (load-metrics-from-file "data/metrics.edn")
  
  ;; Record development milestone
  (record-development-milestone {:date "2026-01-18"
                                 :type "feature"
                                 :description "Added effectiveness tracker"
                                 :metrics {:models-added 10
                                          :loc-added 500}}))
