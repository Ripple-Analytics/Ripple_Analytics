(ns mental-models.pipeline.integration.performance-monitor
  "Real-time performance monitoring for the pipeline.
   
   Features:
   - CPU and memory monitoring
   - Request latency tracking
   - Throughput measurement
   - Resource utilization alerts
   - Performance trend analysis
   - Bottleneck detection
   - SLA compliance tracking
   - Performance reporting"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.lang.management ManagementFactory]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:metrics-history []   ;; historical metrics
         :latency-samples {}   ;; endpoint -> latency samples
         :throughput {}        ;; endpoint -> throughput data
         :alerts {}            ;; alert-id -> alert-config
         :active-alerts {}     ;; alert-id -> active-alert
         :slas {}              ;; sla-id -> sla-config
         :baselines {}         ;; metric -> baseline
         :monitoring? false
         :initialized? false}))

;; ============================================================================
;; System Metrics Collection
;; ============================================================================

(defn get-cpu-usage
  "Get current CPU usage."
  []
  (let [os-bean (ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? com.sun.management.OperatingSystemMXBean os-bean)
      (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean os-bean)
      -1.0)))

(defn get-memory-usage
  "Get current memory usage."
  []
  (let [runtime (Runtime/getRuntime)
        total (.totalMemory runtime)
        free (.freeMemory runtime)
        used (- total free)
        max (.maxMemory runtime)]
    {:total-mb (/ total 1048576.0)
     :used-mb (/ used 1048576.0)
     :free-mb (/ free 1048576.0)
     :max-mb (/ max 1048576.0)
     :usage-percent (* 100.0 (/ used max))}))

(defn get-thread-count
  "Get current thread count."
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:total (.getThreadCount thread-bean)
     :daemon (.getDaemonThreadCount thread-bean)
     :peak (.getPeakThreadCount thread-bean)}))

(defn get-gc-stats
  "Get garbage collection statistics."
  []
  (let [gc-beans (ManagementFactory/getGarbageCollectorMXBeans)]
    (mapv (fn [gc]
            {:name (.getName gc)
             :collection-count (.getCollectionCount gc)
             :collection-time-ms (.getCollectionTime gc)})
          gc-beans)))

(defn collect-system-metrics
  "Collect all system metrics."
  []
  {:timestamp (System/currentTimeMillis)
   :cpu-usage (get-cpu-usage)
   :memory (get-memory-usage)
   :threads (get-thread-count)
   :gc (get-gc-stats)})

;; ============================================================================
;; Latency Tracking
;; ============================================================================

(defn record-latency!
  "Record a latency sample."
  [endpoint latency-ms & {:keys [status]}]
  (let [sample {:latency-ms latency-ms
                :status status
                :timestamp (System/currentTimeMillis)}]
    (swap! state update-in [:latency-samples endpoint]
           (fn [samples]
             (let [updated (conj (or samples []) sample)]
               ;; Keep last 1000 samples
               (vec (take-last 1000 updated)))))
    (metrics/histogram :request-latency {:endpoint endpoint} latency-ms)))

(defn get-latency-stats
  "Get latency statistics for an endpoint."
  [endpoint]
  (let [samples (get-in @state [:latency-samples endpoint] [])
        latencies (map :latency-ms samples)]
    (when (seq latencies)
      (let [sorted (sort latencies)
            n (count sorted)
            sum (reduce + latencies)]
        {:count n
         :min (first sorted)
         :max (last sorted)
         :mean (/ sum n)
         :p50 (nth sorted (int (* 0.5 n)))
         :p90 (nth sorted (int (* 0.9 n)))
         :p95 (nth sorted (int (* 0.95 n)))
         :p99 (nth sorted (int (* 0.99 n)))}))))

(defn get-all-latency-stats
  "Get latency statistics for all endpoints."
  []
  (into {}
        (map (fn [[endpoint _]]
               [endpoint (get-latency-stats endpoint)])
             (:latency-samples @state))))

;; ============================================================================
;; Throughput Tracking
;; ============================================================================

(defn record-request!
  "Record a request for throughput tracking."
  [endpoint & {:keys [bytes]}]
  (let [now (System/currentTimeMillis)
        window-start (- now 60000)] ;; 1 minute window
    (swap! state update-in [:throughput endpoint]
           (fn [data]
             (let [requests (or (:requests data) [])
                   ;; Filter to keep only requests in window
                   recent (filterv #(>= (:timestamp %) window-start) requests)
                   updated (conj recent {:timestamp now :bytes (or bytes 0)})]
               {:requests updated
                :last-updated now})))))

(defn get-throughput
  "Get throughput for an endpoint."
  [endpoint]
  (let [data (get-in @state [:throughput endpoint])
        requests (:requests data [])
        now (System/currentTimeMillis)
        window-start (- now 60000)
        recent (filter #(>= (:timestamp %) window-start) requests)
        count (count recent)
        bytes (reduce + (map :bytes recent))]
    {:requests-per-minute count
     :requests-per-second (/ count 60.0)
     :bytes-per-minute bytes
     :bytes-per-second (/ bytes 60.0)}))

(defn get-all-throughput
  "Get throughput for all endpoints."
  []
  (into {}
        (map (fn [[endpoint _]]
               [endpoint (get-throughput endpoint)])
             (:throughput @state))))

;; ============================================================================
;; Alert Management
;; ============================================================================

(defn register-alert!
  "Register a performance alert."
  [alert-id config]
  (let [alert {:id alert-id
               :name (get config :name (name alert-id))
               :metric (get config :metric)
               :condition (get config :condition :gt)
               :threshold (get config :threshold)
               :duration-ms (get config :duration-ms 60000)
               :severity (get config :severity :warning)
               :channels (get config :channels [:log])
               :cooldown-ms (get config :cooldown-ms 300000)
               :enabled? (get config :enabled? true)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:alerts alert-id] alert)
    (logging/log :info "Registered performance alert" {:alert-id alert-id})
    alert-id))

(defn- check-condition
  "Check if a condition is met."
  [value condition threshold]
  (case condition
    :gt (> value threshold)
    :gte (>= value threshold)
    :lt (< value threshold)
    :lte (<= value threshold)
    :eq (= value threshold)
    :neq (not= value threshold)
    false))

(defn- get-metric-value
  "Get current value for a metric."
  [metric]
  (case metric
    :cpu-usage (get-cpu-usage)
    :memory-usage (get-in (get-memory-usage) [:usage-percent])
    :thread-count (get-in (get-thread-count) [:total])
    nil))

(defn check-alerts!
  "Check all alerts and fire if conditions are met."
  []
  (doseq [[alert-id alert] (:alerts @state)]
    (when (:enabled? alert)
      (let [value (get-metric-value (:metric alert))
            triggered? (and value (check-condition value (:condition alert) (:threshold alert)))
            active (get-in @state [:active-alerts alert-id])
            now (System/currentTimeMillis)]
        (cond
          ;; New alert
          (and triggered? (nil? active))
          (do
            (swap! state assoc-in [:active-alerts alert-id]
                   {:triggered-at now
                    :value value
                    :notified? false})
            (logging/log :warn "Performance alert triggered"
                         {:alert-id alert-id :metric (:metric alert) :value value})
            (events/emit! :performance-alert-triggered
                          {:alert-id alert-id :alert alert :value value}))
          
          ;; Alert resolved
          (and (not triggered?) active)
          (do
            (swap! state update :active-alerts dissoc alert-id)
            (logging/log :info "Performance alert resolved"
                         {:alert-id alert-id :duration-ms (- now (:triggered-at active))})
            (events/emit! :performance-alert-resolved {:alert-id alert-id})))))))

(defn list-active-alerts
  "List all active alerts."
  []
  (mapv (fn [[id data]]
          (merge {:alert-id id} data (get-in @state [:alerts id])))
        (:active-alerts @state)))

;; ============================================================================
;; SLA Tracking
;; ============================================================================

(defn register-sla!
  "Register an SLA."
  [sla-id config]
  (let [sla {:id sla-id
             :name (get config :name (name sla-id))
             :endpoint (get config :endpoint)
             :latency-p99-ms (get config :latency-p99-ms 1000)
             :availability-percent (get config :availability-percent 99.9)
             :error-rate-percent (get config :error-rate-percent 0.1)
             :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:slas sla-id] sla)
    (logging/log :info "Registered SLA" {:sla-id sla-id})
    sla-id))

(defn check-sla-compliance
  "Check SLA compliance for an endpoint."
  [sla-id]
  (when-let [sla (get-in @state [:slas sla-id])]
    (let [endpoint (:endpoint sla)
          latency-stats (get-latency-stats endpoint)
          samples (get-in @state [:latency-samples endpoint] [])
          total (count samples)
          errors (count (filter #(>= (:status % 200) 400) samples))
          error-rate (if (pos? total) (* 100.0 (/ errors total)) 0.0)
          
          latency-ok? (or (nil? latency-stats)
                          (<= (:p99 latency-stats) (:latency-p99-ms sla)))
          error-rate-ok? (<= error-rate (:error-rate-percent sla))]
      {:sla-id sla-id
       :endpoint endpoint
       :compliant? (and latency-ok? error-rate-ok?)
       :latency-p99 (:p99 latency-stats)
       :latency-target (:latency-p99-ms sla)
       :latency-ok? latency-ok?
       :error-rate error-rate
       :error-rate-target (:error-rate-percent sla)
       :error-rate-ok? error-rate-ok?
       :checked-at (System/currentTimeMillis)})))

(defn get-all-sla-compliance
  "Get SLA compliance for all registered SLAs."
  []
  (mapv (fn [[sla-id _]]
          (check-sla-compliance sla-id))
        (:slas @state)))

;; ============================================================================
;; Baseline Management
;; ============================================================================

(defn set-baseline!
  "Set a performance baseline."
  [metric baseline-value]
  (swap! state assoc-in [:baselines metric]
         {:value baseline-value
          :set-at (System/currentTimeMillis)})
  (logging/log :info "Set performance baseline" {:metric metric :value baseline-value}))

(defn get-baseline
  "Get a performance baseline."
  [metric]
  (get-in @state [:baselines metric]))

(defn compare-to-baseline
  "Compare current value to baseline."
  [metric current-value]
  (when-let [baseline (get-baseline metric)]
    (let [baseline-value (:value baseline)
          diff (- current-value baseline-value)
          percent-change (if (zero? baseline-value)
                           0.0
                           (* 100.0 (/ diff baseline-value)))]
      {:metric metric
       :current current-value
       :baseline baseline-value
       :difference diff
       :percent-change percent-change
       :status (cond
                 (> percent-change 20) :degraded
                 (< percent-change -20) :improved
                 :else :stable)})))

;; ============================================================================
;; Trend Analysis
;; ============================================================================

(defn analyze-trends
  "Analyze performance trends."
  []
  (let [history (:metrics-history @state)
        recent (take-last 60 history)] ;; Last 60 samples
    (when (>= (count recent) 10)
      (let [cpu-values (map :cpu-usage recent)
            memory-values (map #(get-in % [:memory :usage-percent]) recent)
            
            cpu-trend (let [first-half (take (quot (count cpu-values) 2) cpu-values)
                            second-half (drop (quot (count cpu-values) 2) cpu-values)]
                        (- (/ (reduce + second-half) (count second-half))
                           (/ (reduce + first-half) (count first-half))))
            
            memory-trend (let [first-half (take (quot (count memory-values) 2) memory-values)
                               second-half (drop (quot (count memory-values) 2) memory-values)]
                           (- (/ (reduce + second-half) (count second-half))
                              (/ (reduce + first-half) (count first-half))))]
        {:cpu {:current (last cpu-values)
               :trend cpu-trend
               :direction (cond (> cpu-trend 5) :increasing
                                (< cpu-trend -5) :decreasing
                                :else :stable)}
         :memory {:current (last memory-values)
                  :trend memory-trend
                  :direction (cond (> memory-trend 5) :increasing
                                   (< memory-trend -5) :decreasing
                                   :else :stable)}
         :analyzed-at (System/currentTimeMillis)}))))

;; ============================================================================
;; Bottleneck Detection
;; ============================================================================

(defn detect-bottlenecks
  "Detect performance bottlenecks."
  []
  (let [system-metrics (collect-system-metrics)
        latency-stats (get-all-latency-stats)
        bottlenecks (atom [])]
    
    ;; Check CPU
    (when (> (:cpu-usage system-metrics) 0.8)
      (swap! bottlenecks conj {:type :cpu
                               :severity :high
                               :value (:cpu-usage system-metrics)
                               :message "CPU usage above 80%"}))
    
    ;; Check memory
    (when (> (get-in system-metrics [:memory :usage-percent]) 85)
      (swap! bottlenecks conj {:type :memory
                               :severity :high
                               :value (get-in system-metrics [:memory :usage-percent])
                               :message "Memory usage above 85%"}))
    
    ;; Check latency
    (doseq [[endpoint stats] latency-stats]
      (when (and stats (> (:p99 stats) 5000))
        (swap! bottlenecks conj {:type :latency
                                 :severity :medium
                                 :endpoint endpoint
                                 :value (:p99 stats)
                                 :message (str "P99 latency above 5s for " endpoint)})))
    
    {:bottlenecks @bottlenecks
     :has-bottlenecks? (seq @bottlenecks)
     :detected-at (System/currentTimeMillis)}))

;; ============================================================================
;; Monitoring Loop
;; ============================================================================

(defn- start-monitoring-loop!
  "Start the background monitoring loop."
  []
  (when-not (:monitoring? @state)
    (swap! state assoc :monitoring? true)
    (go-loop []
      (when (:monitoring? @state)
        ;; Collect metrics
        (let [metrics (collect-system-metrics)]
          (swap! state update :metrics-history
                 (fn [h]
                   (vec (take-last 1000 (conj h metrics))))))
        
        ;; Check alerts
        (check-alerts!)
        
        ;; Wait before next collection
        (<! (timeout 10000)) ;; Every 10 seconds
        (recur)))))

(defn stop-monitoring!
  "Stop the monitoring loop."
  []
  (swap! state assoc :monitoring? false)
  (logging/log :info "Stopped performance monitoring"))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-monitor-stats
  "Get performance monitor statistics."
  []
  (let [system-metrics (collect-system-metrics)]
    {:current-metrics system-metrics
     :latency-stats (get-all-latency-stats)
     :throughput (get-all-throughput)
     :active-alerts (count (:active-alerts @state))
     :registered-alerts (count (:alerts @state))
     :registered-slas (count (:slas @state))
     :sla-compliance (get-all-sla-compliance)
     :trends (analyze-trends)
     :bottlenecks (detect-bottlenecks)
     :metrics-history-size (count (:metrics-history @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-performance-monitor!
  "Initialize the performance monitor."
  []
  (when-not (:initialized? @state)
    ;; Register default alerts
    (register-alert! :high-cpu
                     {:name "High CPU Usage"
                      :metric :cpu-usage
                      :condition :gt
                      :threshold 0.8
                      :severity :warning})
    
    (register-alert! :high-memory
                     {:name "High Memory Usage"
                      :metric :memory-usage
                      :condition :gt
                      :threshold 85
                      :severity :warning})
    
    (register-alert! :critical-memory
                     {:name "Critical Memory Usage"
                      :metric :memory-usage
                      :condition :gt
                      :threshold 95
                      :severity :critical})
    
    ;; Register default SLA
    (register-sla! :default-api
                   {:name "Default API SLA"
                    :endpoint "/api"
                    :latency-p99-ms 2000
                    :availability-percent 99.5
                    :error-rate-percent 1.0})
    
    ;; Set initial baselines
    (let [initial-metrics (collect-system-metrics)]
      (set-baseline! :cpu-usage (:cpu-usage initial-metrics))
      (set-baseline! :memory-usage (get-in initial-metrics [:memory :usage-percent])))
    
    ;; Start monitoring loop
    (start-monitoring-loop!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Performance monitor initialized")
    (events/emit! :performance-monitor-initialized {})
    true))
