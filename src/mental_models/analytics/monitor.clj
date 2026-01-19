(ns mental-models.analytics.monitor
  "System monitor for desktop app health metrics.
   
   Features:
   - Memory usage tracking
   - CPU monitoring
   - Disk space checks
   - Network status
   - Health scoring"
  (:require [clojure.string :as str])
  (:import [java.lang.management ManagementFactory]
           [java.io File]))

;; ============================================================================
;; State
;; ============================================================================

(defonce ^:private monitor-state
  (atom {:metrics-history []
         :health-score 100
         :status :healthy
         :last-check nil
         :alerts []}))

;; ============================================================================
;; System Metrics Collection
;; ============================================================================

(defn get-memory-metrics
  "Get JVM memory metrics."
  []
  (let [runtime (Runtime/getRuntime)
        max-mem (.maxMemory runtime)
        total-mem (.totalMemory runtime)
        free-mem (.freeMemory runtime)
        used-mem (- total-mem free-mem)]
    {:max-mb (/ max-mem 1048576.0)
     :total-mb (/ total-mem 1048576.0)
     :used-mb (/ used-mem 1048576.0)
     :free-mb (/ free-mem 1048576.0)
     :usage-pct (* 100.0 (/ used-mem max-mem))}))

(defn get-cpu-metrics
  "Get CPU metrics."
  []
  (let [os-bean (ManagementFactory/getOperatingSystemMXBean)
        processors (.getAvailableProcessors os-bean)
        load-avg (.getSystemLoadAverage os-bean)]
    {:processors processors
     :load-average (if (neg? load-avg) 0.0 load-avg)
     :load-per-cpu (if (neg? load-avg) 0.0 (/ load-avg processors))}))

(defn get-disk-metrics
  "Get disk space metrics for home directory."
  []
  (let [home (File. (System/getProperty "user.home"))
        total (.getTotalSpace home)
        free (.getFreeSpace home)
        usable (.getUsableSpace home)]
    {:total-gb (/ total 1073741824.0)
     :free-gb (/ free 1073741824.0)
     :usable-gb (/ usable 1073741824.0)
     :usage-pct (* 100.0 (/ (- total free) total))}))

(defn get-thread-metrics
  "Get thread metrics."
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:thread-count (.getThreadCount thread-bean)
     :peak-thread-count (.getPeakThreadCount thread-bean)
     :daemon-thread-count (.getDaemonThreadCount thread-bean)}))

(defn get-gc-metrics
  "Get garbage collection metrics."
  []
  (let [gc-beans (ManagementFactory/getGarbageCollectorMXBeans)]
    {:gc-count (reduce + (map #(.getCollectionCount %) gc-beans))
     :gc-time-ms (reduce + (map #(.getCollectionTime %) gc-beans))}))

(defn collect-all-metrics
  "Collect all system metrics."
  []
  {:memory (get-memory-metrics)
   :cpu (get-cpu-metrics)
   :disk (get-disk-metrics)
   :threads (get-thread-metrics)
   :gc (get-gc-metrics)
   :timestamp (System/currentTimeMillis)})

;; ============================================================================
;; Health Scoring
;; ============================================================================

(defn calculate-health-score
  "Calculate overall health score (0-100)."
  [metrics]
  (let [memory-score (max 0 (- 100 (* 1.5 (get-in metrics [:memory :usage-pct] 0))))
        cpu-score (max 0 (- 100 (* 20 (get-in metrics [:cpu :load-per-cpu] 0))))
        disk-score (max 0 (- 100 (get-in metrics [:disk :usage-pct] 0)))
        ;; Weight: memory 40%, CPU 30%, disk 30%
        weighted-score (+ (* 0.4 memory-score)
                          (* 0.3 cpu-score)
                          (* 0.3 disk-score))]
    (int (max 0 (min 100 weighted-score)))))

(defn determine-status
  "Determine health status from score."
  [score]
  (cond
    (>= score 80) :healthy
    (>= score 60) :degraded
    (>= score 40) :warning
    :else :critical))

;; ============================================================================
;; Monitoring
;; ============================================================================

(defn check-health!
  "Perform health check and update state."
  []
  (let [metrics (collect-all-metrics)
        score (calculate-health-score metrics)
        status (determine-status score)]
    ;; Update state
    (swap! monitor-state assoc
           :health-score score
           :status status
           :last-check (System/currentTimeMillis))
    ;; Keep metrics history (last 100 samples)
    (swap! monitor-state update :metrics-history
           (fn [history] (vec (take-last 100 (conj history metrics)))))
    ;; Generate alerts for critical conditions
    (when (< score 40)
      (swap! monitor-state update :alerts conj
             {:type :health-critical
              :score score
              :timestamp (System/currentTimeMillis)}))
    {:metrics metrics
     :score score
     :status status}))

(defn get-health-status
  "Get current health status."
  []
  (let [state @monitor-state]
    {:score (:health-score state)
     :status (:status state)
     :last-check (:last-check state)
     :alerts-count (count (:alerts state))}))

(defn get-metrics-history
  "Get metrics history."
  [& {:keys [limit] :or {limit 50}}]
  (vec (take-last limit (:metrics-history @monitor-state))))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn monitor-dashboard
  "Get complete monitor data for dashboard."
  []
  (let [current (collect-all-metrics)
        score (calculate-health-score current)
        history (:metrics-history @monitor-state)]
    {:current current
     :health-score score
     :status (determine-status score)
     :trends {:memory-trend (when (>= (count history) 2)
                              (let [recent (take-last 10 history)
                                    usages (map #(get-in % [:memory :usage-pct]) recent)]
                                (if (> (last usages) (first usages)) :increasing :stable)))
              :cpu-trend (when (>= (count history) 2)
                           (let [recent (take-last 10 history)
                                 loads (map #(get-in % [:cpu :load-per-cpu]) recent)]
                             (if (> (last loads) (first loads)) :increasing :stable)))}
     :history-count (count history)
     :last-check (:last-check @monitor-state)}))

(defn format-for-display
  "Format metrics for UI display."
  [metrics]
  {:memory (str (int (get-in metrics [:memory :used-mb])) " / "
                (int (get-in metrics [:memory :max-mb])) " MB ("
                (int (get-in metrics [:memory :usage-pct])) "%)")
   :cpu (str "Load: " (format "%.2f" (get-in metrics [:cpu :load-average]))
             " (" (get-in metrics [:cpu :processors]) " cores)")
   :disk (str (format "%.1f" (get-in metrics [:disk :free-gb])) " GB free ("
              (int (get-in metrics [:disk :usage-pct])) "% used)")
   :threads (str (get-in metrics [:threads :thread-count]) " threads")
   :gc (str (get-in metrics [:gc :gc-count]) " collections, "
            (get-in metrics [:gc :gc-time-ms]) " ms total")})

(defn reset-monitor!
  "Reset monitor state."
  []
  (reset! monitor-state
          {:metrics-history []
           :health-score 100
           :status :healthy
           :last-check nil
           :alerts []}))
