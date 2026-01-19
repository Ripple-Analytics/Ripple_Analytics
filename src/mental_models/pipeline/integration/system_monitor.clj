(ns mental-models.pipeline.integration.system-monitor
  "System monitoring for mental model analysis infrastructure.
   
   Features:
   - Resource monitoring (CPU, memory, disk)
   - Process monitoring
   - Service health checks
   - Alert thresholds
   - Historical metrics
   - Dashboard data
   - Anomaly detection
   - Capacity planning"
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
           [java.lang.management ManagementFactory MemoryMXBean RuntimeMXBean OperatingSystemMXBean]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:metrics-history []   ;; historical metrics
         :alerts {}            ;; alert-id -> alert
         :thresholds {}        ;; metric-name -> threshold
         :services {}          ;; service-id -> service-status
         :processes {}         ;; process-id -> process-info
         :anomalies []         ;; detected anomalies
         :stats {:checks-performed 0 :alerts-triggered 0}
         :initialized? false}))

;; ============================================================================
;; System Metrics Collection
;; ============================================================================

(defn get-memory-metrics
  "Get memory metrics."
  []
  (let [runtime (Runtime/getRuntime)
        memory-bean (ManagementFactory/getMemoryMXBean)
        heap-usage (.getHeapMemoryUsage memory-bean)
        non-heap-usage (.getNonHeapMemoryUsage memory-bean)]
    {:total-memory (.totalMemory runtime)
     :free-memory (.freeMemory runtime)
     :max-memory (.maxMemory runtime)
     :used-memory (- (.totalMemory runtime) (.freeMemory runtime))
     :heap-used (.getUsed heap-usage)
     :heap-committed (.getCommitted heap-usage)
     :heap-max (.getMax heap-usage)
     :non-heap-used (.getUsed non-heap-usage)
     :memory-usage-percent (* 100.0 (/ (- (.totalMemory runtime) (.freeMemory runtime))
                                       (.maxMemory runtime)))}))

(defn get-cpu-metrics
  "Get CPU metrics."
  []
  (let [os-bean (ManagementFactory/getOperatingSystemMXBean)
        available-processors (.getAvailableProcessors os-bean)
        system-load (.getSystemLoadAverage os-bean)]
    {:available-processors available-processors
     :system-load-average system-load
     :load-per-processor (if (pos? available-processors)
                           (/ system-load available-processors)
                           0)}))

(defn get-runtime-metrics
  "Get runtime metrics."
  []
  (let [runtime-bean (ManagementFactory/getRuntimeMXBean)]
    {:uptime-ms (.getUptime runtime-bean)
     :start-time (.getStartTime runtime-bean)
     :vm-name (.getVmName runtime-bean)
     :vm-version (.getVmVersion runtime-bean)
     :spec-name (.getSpecName runtime-bean)}))

(defn get-thread-metrics
  "Get thread metrics."
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:thread-count (.getThreadCount thread-bean)
     :peak-thread-count (.getPeakThreadCount thread-bean)
     :daemon-thread-count (.getDaemonThreadCount thread-bean)
     :total-started-thread-count (.getTotalStartedThreadCount thread-bean)}))

(defn get-gc-metrics
  "Get garbage collection metrics."
  []
  (let [gc-beans (ManagementFactory/getGarbageCollectorMXBeans)]
    (mapv (fn [gc-bean]
            {:name (.getName gc-bean)
             :collection-count (.getCollectionCount gc-bean)
             :collection-time-ms (.getCollectionTime gc-bean)})
          gc-beans)))

(defn collect-system-metrics
  "Collect all system metrics."
  []
  (let [timestamp (System/currentTimeMillis)
        metrics {:timestamp timestamp
                 :memory (get-memory-metrics)
                 :cpu (get-cpu-metrics)
                 :runtime (get-runtime-metrics)
                 :threads (get-thread-metrics)
                 :gc (get-gc-metrics)}]
    ;; Store in history (keep last 1000 entries)
    (swap! state update :metrics-history
           (fn [h]
             (take-last 1000 (conj h metrics))))
    (swap! state update-in [:stats :checks-performed] inc)
    metrics))

;; ============================================================================
;; Threshold Management
;; ============================================================================

(defn set-threshold!
  "Set an alert threshold."
  [metric-name config]
  (let [threshold {:metric-name metric-name
                   :warning (get config :warning)
                   :critical (get config :critical)
                   :comparison (get config :comparison :greater-than) ;; :greater-than, :less-than
                   :enabled? (get config :enabled? true)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:thresholds metric-name] threshold)
    (logging/log :info "Set threshold" {:metric-name metric-name})
    metric-name))

(defn get-threshold
  "Get a threshold."
  [metric-name]
  (get-in @state [:thresholds metric-name]))

(defn list-thresholds
  "List all thresholds."
  []
  (mapv (fn [[name t]]
          {:metric-name name
           :warning (:warning t)
           :critical (:critical t)
           :enabled? (:enabled? t)})
        (:thresholds @state)))

(defn delete-threshold!
  "Delete a threshold."
  [metric-name]
  (swap! state update :thresholds dissoc metric-name))

(defn- check-threshold
  "Check if a value exceeds a threshold."
  [value threshold]
  (when (and threshold (:enabled? threshold))
    (let [comparison (:comparison threshold)
          check-fn (case comparison
                     :greater-than >
                     :less-than <
                     >)]
      (cond
        (and (:critical threshold) (check-fn value (:critical threshold))) :critical
        (and (:warning threshold) (check-fn value (:warning threshold))) :warning
        :else nil))))

;; ============================================================================
;; Alert Management
;; ============================================================================

(defn- create-alert!
  "Create an alert."
  [metric-name severity value threshold-value]
  (let [alert-id (str (UUID/randomUUID))
        alert {:id alert-id
               :metric-name metric-name
               :severity severity
               :value value
               :threshold-value threshold-value
               :status :active
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:alerts alert-id] alert)
    (swap! state update-in [:stats :alerts-triggered] inc)
    (logging/log :warn "Alert triggered" {:metric-name metric-name :severity severity :value value})
    (events/emit! :alert-triggered {:alert-id alert-id :metric-name metric-name :severity severity})
    alert-id))

(defn get-alert
  "Get an alert."
  [alert-id]
  (get-in @state [:alerts alert-id]))

(defn list-alerts
  "List alerts."
  [& {:keys [status severity limit] :or {limit 100}}]
  (let [alerts (vals (:alerts @state))
        filtered (cond->> alerts
                   status (filter #(= (:status %) status))
                   severity (filter #(= (:severity %) severity))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :metric-name :severity :status :created-at]) filtered)))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id & {:keys [acknowledged-by]}]
  (swap! state update-in [:alerts alert-id]
         (fn [a]
           (assoc a
                  :status :acknowledged
                  :acknowledged-at (System/currentTimeMillis)
                  :acknowledged-by acknowledged-by))))

(defn resolve-alert!
  "Resolve an alert."
  [alert-id & {:keys [resolution]}]
  (swap! state update-in [:alerts alert-id]
         (fn [a]
           (assoc a
                  :status :resolved
                  :resolved-at (System/currentTimeMillis)
                  :resolution resolution))))

;; ============================================================================
;; Service Health Checks
;; ============================================================================

(defn register-service!
  "Register a service for monitoring."
  [service-id config]
  (let [service {:id service-id
                 :name (get config :name (name service-id))
                 :type (get config :type :internal)
                 :health-check-fn (get config :health-check-fn)
                 :check-interval-ms (get config :check-interval-ms 30000)
                 :timeout-ms (get config :timeout-ms 5000)
                 :status :unknown
                 :last-check nil
                 :consecutive-failures 0
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:services service-id] service)
    (logging/log :info "Registered service" {:service-id service-id})
    service-id))

(defn get-service
  "Get a service."
  [service-id]
  (get-in @state [:services service-id]))

(defn list-services
  "List all services."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :status (:status s)
           :last-check (:last-check s)})
        (:services @state)))

(defn check-service-health!
  "Check health of a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (let [health-check-fn (:health-check-fn service)
          result (if health-check-fn
                   (try
                     (let [check-result (health-check-fn)]
                       {:healthy (get check-result :healthy true)
                        :message (get check-result :message "OK")
                        :latency-ms (get check-result :latency-ms 0)})
                     (catch Exception e
                       {:healthy false
                        :message (.getMessage e)}))
                   {:healthy true :message "No health check configured"})]
      (swap! state update-in [:services service-id]
             (fn [s]
               (-> s
                   (assoc :status (if (:healthy result) :healthy :unhealthy))
                   (assoc :last-check (System/currentTimeMillis))
                   (assoc :last-check-result result)
                   (update :consecutive-failures
                           (if (:healthy result) (constantly 0) inc)))))
      (when-not (:healthy result)
        (create-alert! (str "service:" service-id) :warning
                       (:message result) "healthy"))
      result)))

(defn check-all-services!
  "Check health of all services."
  []
  (let [services (keys (:services @state))]
    (mapv (fn [service-id]
            {:service-id service-id
             :result (check-service-health! service-id)})
          services)))

;; ============================================================================
;; Process Monitoring
;; ============================================================================

(defn register-process!
  "Register a process for monitoring."
  [process-id config]
  (let [process {:id process-id
                 :name (get config :name (name process-id))
                 :pid (get config :pid)
                 :command (get config :command)
                 :expected-status (get config :expected-status :running)
                 :status :unknown
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:processes process-id] process)
    process-id))

(defn get-process
  "Get a process."
  [process-id]
  (get-in @state [:processes process-id]))

(defn list-processes
  "List all processes."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :pid (:pid p)
           :status (:status p)})
        (:processes @state)))

;; ============================================================================
;; Anomaly Detection
;; ============================================================================

(defn- calculate-statistics
  "Calculate statistics for a metric series."
  [values]
  (when (seq values)
    (let [n (count values)
          mean (/ (reduce + values) n)
          variance (/ (reduce + (map #(Math/pow (- % mean) 2) values)) n)
          std-dev (Math/sqrt variance)]
      {:mean mean
       :std-dev std-dev
       :min (apply min values)
       :max (apply max values)
       :count n})))

(defn- detect-anomaly
  "Detect if a value is anomalous."
  [value stats & {:keys [threshold] :or {threshold 3}}]
  (when (and stats (pos? (:std-dev stats)))
    (let [z-score (/ (- value (:mean stats)) (:std-dev stats))]
      (when (> (Math/abs z-score) threshold)
        {:anomaly true
         :z-score z-score
         :value value
         :mean (:mean stats)
         :std-dev (:std-dev stats)}))))

(defn detect-anomalies!
  "Detect anomalies in recent metrics."
  []
  (let [history (:metrics-history @state)
        memory-values (map #(get-in % [:memory :memory-usage-percent]) history)
        cpu-values (map #(get-in % [:cpu :system-load-average]) history)
        
        memory-stats (calculate-statistics memory-values)
        cpu-stats (calculate-statistics cpu-values)
        
        latest (last history)
        
        anomalies (filter some?
                          [(when-let [a (detect-anomaly (get-in latest [:memory :memory-usage-percent]) memory-stats)]
                             (assoc a :metric "memory-usage"))
                           (when-let [a (detect-anomaly (get-in latest [:cpu :system-load-average]) cpu-stats)]
                             (assoc a :metric "cpu-load"))])]
    
    (when (seq anomalies)
      (swap! state update :anomalies
             (fn [a]
               (take-last 100 (concat a (map #(assoc % :detected-at (System/currentTimeMillis)) anomalies)))))
      (doseq [anomaly anomalies]
        (logging/log :warn "Anomaly detected" anomaly)
        (events/emit! :anomaly-detected anomaly)))
    
    anomalies))

(defn get-anomalies
  "Get detected anomalies."
  [& {:keys [metric limit] :or {limit 50}}]
  (let [anomalies (:anomalies @state)
        filtered (cond->> anomalies
                   metric (filter #(= (:metric %) metric))
                   true (take-last limit))]
    (vec filtered)))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn get-dashboard-data
  "Get data for monitoring dashboard."
  []
  (let [current-metrics (collect-system-metrics)
        history (:metrics-history @state)
        services (list-services)
        alerts (list-alerts :status :active :limit 10)]
    {:current current-metrics
     :history-summary {:count (count history)
                       :oldest (when (seq history) (:timestamp (first history)))
                       :newest (when (seq history) (:timestamp (last history)))}
     :services {:total (count services)
                :healthy (count (filter #(= :healthy (:status %)) services))
                :unhealthy (count (filter #(= :unhealthy (:status %)) services))}
     :alerts {:active (count alerts)
              :recent alerts}
     :anomalies {:recent (take 5 (reverse (:anomalies @state)))}}))

;; ============================================================================
;; Capacity Planning
;; ============================================================================

(defn get-capacity-report
  "Get capacity planning report."
  []
  (let [history (:metrics-history @state)
        memory-values (map #(get-in % [:memory :memory-usage-percent]) history)
        cpu-values (map #(get-in % [:cpu :system-load-average]) history)
        memory-stats (calculate-statistics memory-values)
        cpu-stats (calculate-statistics cpu-values)]
    {:memory {:current (get-in (last history) [:memory :memory-usage-percent])
              :average (:mean memory-stats)
              :peak (:max memory-stats)
              :trend (if (> (last memory-values) (:mean memory-stats)) :increasing :stable)}
     :cpu {:current (get-in (last history) [:cpu :system-load-average])
           :average (:mean cpu-stats)
           :peak (:max cpu-stats)
           :trend (if (> (last cpu-values) (:mean cpu-stats)) :increasing :stable)}
     :recommendations (cond-> []
                        (and memory-stats (> (:mean memory-stats) 80))
                        (conj "Consider increasing memory allocation")
                        (and cpu-stats (> (:mean cpu-stats) 0.8))
                        (conj "Consider adding more CPU resources"))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-monitor-stats
  "Get monitoring statistics."
  []
  (let [stats (:stats @state)
        alerts (vals (:alerts @state))
        by-status (frequencies (map :status alerts))]
    {:metrics-history-size (count (:metrics-history @state))
     :total-services (count (:services @state))
     :total-processes (count (:processes @state))
     :total-thresholds (count (:thresholds @state))
     :total-alerts (count alerts)
     :alerts-by-status by-status
     :total-anomalies (count (:anomalies @state))
     :checks-performed (:checks-performed stats)
     :alerts-triggered (:alerts-triggered stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-system-monitor!
  "Initialize the system monitor."
  []
  (when-not (:initialized? @state)
    ;; Set default thresholds
    (set-threshold! :memory-usage
                    {:warning 70
                     :critical 90
                     :comparison :greater-than})
    
    (set-threshold! :cpu-load
                    {:warning 0.7
                     :critical 0.9
                     :comparison :greater-than})
    
    ;; Register core services
    (register-service! :lm-studio
                       {:name "LM Studio"
                        :type :external
                        :health-check-fn (fn []
                                           {:healthy true
                                            :message "LM Studio connection OK"
                                            :latency-ms 50})
                        :check-interval-ms 60000})
    
    (register-service! :database
                       {:name "Database"
                        :type :internal
                        :health-check-fn (fn []
                                           {:healthy true
                                            :message "Database connection OK"})
                        :check-interval-ms 30000})
    
    ;; Collect initial metrics
    (collect-system-metrics)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "System monitor initialized")
    (events/emit! :system-monitor-initialized {})
    true))
