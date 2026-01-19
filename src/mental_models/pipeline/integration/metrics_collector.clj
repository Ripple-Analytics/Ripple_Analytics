(ns mental-models.pipeline.integration.metrics-collector
  "Metrics Collector Module
   
   Comprehensive metrics gathering:
   - System metrics (CPU, memory, disk)
   - Application metrics
   - Custom metrics
   - Metric exporters
   - Alerting thresholds"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.lang.management ManagementFactory MemoryMXBean ThreadMXBean RuntimeMXBean OperatingSystemMXBean]
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]
   [java.util.concurrent.atomic AtomicLong AtomicReference]))

;; =============================================================================
;; METRICS COLLECTOR STATE
;; =============================================================================

(defonce collector-state (atom {:collectors {}
                                :exporters {}
                                :alerts {}
                                :snapshots []
                                :scheduler nil
                                :config {:collection-interval-ms 10000
                                         :max-snapshots 1000
                                         :default-alert-window 5}}))

;; =============================================================================
;; SYSTEM METRICS
;; =============================================================================

(defn get-memory-metrics
  "Get JVM memory metrics."
  []
  (let [^MemoryMXBean memory-bean (ManagementFactory/getMemoryMXBean)
        heap (.getHeapMemoryUsage memory-bean)
        non-heap (.getNonHeapMemoryUsage memory-bean)]
    {:heap-used (.getUsed heap)
     :heap-max (.getMax heap)
     :heap-committed (.getCommitted heap)
     :heap-utilization (if (pos? (.getMax heap))
                         (* 100.0 (/ (.getUsed heap) (.getMax heap)))
                         0.0)
     :non-heap-used (.getUsed non-heap)
     :non-heap-committed (.getCommitted non-heap)}))

(defn get-thread-metrics
  "Get JVM thread metrics."
  []
  (let [^ThreadMXBean thread-bean (ManagementFactory/getThreadMXBean)]
    {:thread-count (.getThreadCount thread-bean)
     :peak-thread-count (.getPeakThreadCount thread-bean)
     :daemon-thread-count (.getDaemonThreadCount thread-bean)
     :total-started-thread-count (.getTotalStartedThreadCount thread-bean)}))

(defn get-runtime-metrics
  "Get JVM runtime metrics."
  []
  (let [^RuntimeMXBean runtime-bean (ManagementFactory/getRuntimeMXBean)]
    {:uptime-ms (.getUptime runtime-bean)
     :start-time (.getStartTime runtime-bean)
     :vm-name (.getVmName runtime-bean)
     :vm-version (.getVmVersion runtime-bean)}))

(defn get-os-metrics
  "Get operating system metrics."
  []
  (let [^OperatingSystemMXBean os-bean (ManagementFactory/getOperatingSystemMXBean)]
    {:available-processors (.getAvailableProcessors os-bean)
     :system-load-average (.getSystemLoadAverage os-bean)
     :os-name (.getName os-bean)
     :os-version (.getVersion os-bean)
     :os-arch (.getArch os-bean)}))

(defn get-gc-metrics
  "Get garbage collection metrics."
  []
  (let [gc-beans (ManagementFactory/getGarbageCollectorMXBeans)]
    (into {} (for [gc-bean gc-beans]
               [(.getName gc-bean) {:collection-count (.getCollectionCount gc-bean)
                                    :collection-time-ms (.getCollectionTime gc-bean)}]))))

(defn get-system-metrics
  "Get all system metrics."
  []
  {:memory (get-memory-metrics)
   :threads (get-thread-metrics)
   :runtime (get-runtime-metrics)
   :os (get-os-metrics)
   :gc (get-gc-metrics)
   :timestamp (System/currentTimeMillis)})

;; =============================================================================
;; CUSTOM COLLECTORS
;; =============================================================================

(defn create-collector
  "Create a custom metrics collector."
  [collector-id {:keys [name collect-fn interval-ms tags]}]
  {:id collector-id
   :name (or name (str collector-id))
   :collect-fn collect-fn
   :interval-ms (or interval-ms (get-in @collector-state [:config :collection-interval-ms]))
   :tags (or tags #{})
   :last-value (AtomicReference. nil)
   :last-collected (AtomicLong. 0)
   :collection-count (AtomicLong. 0)
   :error-count (AtomicLong. 0)
   :created-at (System/currentTimeMillis)})

(defn register-collector!
  "Register a custom metrics collector."
  [collector-id opts]
  (log/info "Registering collector" {:id collector-id})
  (let [collector (create-collector collector-id opts)]
    (swap! collector-state assoc-in [:collectors collector-id] collector)
    (metrics/inc-counter! :metricscollector/collectors-registered)
    collector-id))

(defn unregister-collector!
  "Unregister a custom metrics collector."
  [collector-id]
  (log/info "Unregistering collector" {:id collector-id})
  (swap! collector-state update :collectors dissoc collector-id))

(defn get-collector
  "Get a custom metrics collector."
  [collector-id]
  (get-in @collector-state [:collectors collector-id]))

(defn list-collectors
  "List all custom metrics collectors."
  []
  (keys (:collectors @collector-state)))

(defn collect!
  "Collect metrics from a collector."
  [collector-id]
  (when-let [collector (get-collector collector-id)]
    (try
      (let [value ((:collect-fn collector))]
        (.set ^AtomicReference (:last-value collector) value)
        (.set ^AtomicLong (:last-collected collector) (System/currentTimeMillis))
        (.incrementAndGet ^AtomicLong (:collection-count collector))
        value)
      (catch Exception e
        (.incrementAndGet ^AtomicLong (:error-count collector))
        (log/error "Collection failed" {:collector collector-id :error (.getMessage e)})
        nil))))

(defn collect-all!
  "Collect metrics from all collectors."
  []
  (into {} (for [collector-id (list-collectors)]
             [collector-id (collect! collector-id)])))

;; =============================================================================
;; METRIC EXPORTERS
;; =============================================================================

(defn create-exporter
  "Create a metrics exporter."
  [exporter-id {:keys [name export-fn format interval-ms]}]
  {:id exporter-id
   :name (or name (str exporter-id))
   :export-fn export-fn
   :format (or format :json)
   :interval-ms (or interval-ms 60000)
   :last-exported (AtomicLong. 0)
   :export-count (AtomicLong. 0)
   :error-count (AtomicLong. 0)
   :created-at (System/currentTimeMillis)})

(defn register-exporter!
  "Register a metrics exporter."
  [exporter-id opts]
  (log/info "Registering exporter" {:id exporter-id})
  (let [exporter (create-exporter exporter-id opts)]
    (swap! collector-state assoc-in [:exporters exporter-id] exporter)
    exporter-id))

(defn unregister-exporter!
  "Unregister a metrics exporter."
  [exporter-id]
  (log/info "Unregistering exporter" {:id exporter-id})
  (swap! collector-state update :exporters dissoc exporter-id))

(defn get-exporter
  "Get a metrics exporter."
  [exporter-id]
  (get-in @collector-state [:exporters exporter-id]))

(defn list-exporters
  "List all metrics exporters."
  []
  (keys (:exporters @collector-state)))

(defn export!
  "Export metrics using an exporter."
  [exporter-id metrics]
  (when-let [exporter (get-exporter exporter-id)]
    (try
      ((:export-fn exporter) metrics)
      (.set ^AtomicLong (:last-exported exporter) (System/currentTimeMillis))
      (.incrementAndGet ^AtomicLong (:export-count exporter))
      true
      (catch Exception e
        (.incrementAndGet ^AtomicLong (:error-count exporter))
        (log/error "Export failed" {:exporter exporter-id :error (.getMessage e)})
        false))))

(defn export-all!
  "Export metrics to all exporters."
  [metrics]
  (doseq [exporter-id (list-exporters)]
    (export! exporter-id metrics)))

;; =============================================================================
;; ALERTING
;; =============================================================================

(defn create-alert
  "Create an alert rule."
  [alert-id {:keys [name metric-path condition threshold severity callback window]}]
  {:id alert-id
   :name (or name (str alert-id))
   :metric-path metric-path
   :condition condition
   :threshold threshold
   :severity (or severity :warning)
   :callback callback
   :window (or window (get-in @collector-state [:config :default-alert-window]))
   :triggered? (AtomicReference. false)
   :trigger-count (AtomicLong. 0)
   :last-triggered (AtomicLong. 0)
   :created-at (System/currentTimeMillis)})

(defn register-alert!
  "Register an alert rule."
  [alert-id opts]
  (log/info "Registering alert" {:id alert-id})
  (let [alert (create-alert alert-id opts)]
    (swap! collector-state assoc-in [:alerts alert-id] alert)
    alert-id))

(defn unregister-alert!
  "Unregister an alert rule."
  [alert-id]
  (log/info "Unregistering alert" {:id alert-id})
  (swap! collector-state update :alerts dissoc alert-id))

(defn get-alert
  "Get an alert rule."
  [alert-id]
  (get-in @collector-state [:alerts alert-id]))

(defn list-alerts
  "List all alert rules."
  []
  (keys (:alerts @collector-state)))

(defn get-metric-value
  "Get a metric value by path."
  [metrics path]
  (get-in metrics (if (vector? path) path [path])))

(defn check-condition
  "Check if an alert condition is met."
  [value condition threshold]
  (case condition
    :gt (> value threshold)
    :gte (>= value threshold)
    :lt (< value threshold)
    :lte (<= value threshold)
    :eq (= value threshold)
    :neq (not= value threshold)
    false))

(defn check-alert!
  "Check an alert against current metrics."
  [alert-id metrics]
  (when-let [alert (get-alert alert-id)]
    (let [value (get-metric-value metrics (:metric-path alert))
          triggered? (and value (check-condition value (:condition alert) (:threshold alert)))
          was-triggered? (.get ^AtomicReference (:triggered? alert))]
      (when (and triggered? (not was-triggered?))
        ;; Alert triggered
        (.set ^AtomicReference (:triggered? alert) true)
        (.set ^AtomicLong (:last-triggered alert) (System/currentTimeMillis))
        (.incrementAndGet ^AtomicLong (:trigger-count alert))
        (log/warn "Alert triggered" {:alert alert-id :value value :threshold (:threshold alert)})
        (events/publish! :metricscollector/alert-triggered {:alert-id alert-id
                                                            :value value
                                                            :threshold (:threshold alert)
                                                            :severity (:severity alert)})
        (when-let [callback (:callback alert)]
          (try
            (callback {:alert-id alert-id :value value :threshold (:threshold alert)})
            (catch Exception e
              (log/error "Alert callback failed" {:alert alert-id :error (.getMessage e)})))))
      (when (and (not triggered?) was-triggered?)
        ;; Alert resolved
        (.set ^AtomicReference (:triggered? alert) false)
        (log/info "Alert resolved" {:alert alert-id})
        (events/publish! :metricscollector/alert-resolved {:alert-id alert-id}))
      triggered?)))

(defn check-all-alerts!
  "Check all alerts against current metrics."
  [metrics]
  (into {} (for [alert-id (list-alerts)]
             [alert-id (check-alert! alert-id metrics)])))

;; =============================================================================
;; SNAPSHOTS
;; =============================================================================

(defn take-snapshot!
  "Take a metrics snapshot."
  []
  (let [system-metrics (get-system-metrics)
        custom-metrics (collect-all!)
        snapshot {:timestamp (System/currentTimeMillis)
                  :system system-metrics
                  :custom custom-metrics}]
    (swap! collector-state update :snapshots
           (fn [snaps]
             (let [max-snaps (get-in @collector-state [:config :max-snapshots])
                   new-snaps (conj snaps snapshot)]
               (if (> (count new-snaps) max-snaps)
                 (vec (drop (- (count new-snaps) max-snaps) new-snaps))
                 new-snaps))))
    ;; Check alerts
    (check-all-alerts! (merge system-metrics custom-metrics))
    ;; Export metrics
    (export-all! snapshot)
    snapshot))

(defn get-snapshots
  "Get metrics snapshots."
  [& {:keys [limit since]}]
  (let [snapshots (:snapshots @collector-state)]
    (cond->> snapshots
      since (filter #(>= (:timestamp %) since))
      limit (take-last limit))))

(defn get-latest-snapshot
  "Get the latest metrics snapshot."
  []
  (last (:snapshots @collector-state)))

(defn clear-snapshots!
  "Clear metrics snapshots."
  []
  (swap! collector-state assoc :snapshots []))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-collection-scheduler!
  "Start the metrics collection scheduler."
  []
  (when (and (flags/is-enabled? "metrics-collector")
             (nil? (:scheduler @collector-state)))
    (log/info "Starting metrics collection scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @collector-state [:config :collection-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (take-snapshot!)
                               (catch Exception e
                                 (log/error "Metrics collection error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! collector-state assoc :scheduler executor))))

(defn stop-collection-scheduler!
  "Stop the metrics collection scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @collector-state)]
    (log/info "Stopping metrics collection scheduler")
    (.shutdown executor)
    (swap! collector-state assoc :scheduler nil)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-collector-stats
  "Get statistics for a collector."
  [collector-id]
  (when-let [collector (get-collector collector-id)]
    {:id collector-id
     :name (:name collector)
     :last-value (.get ^AtomicReference (:last-value collector))
     :last-collected (.get ^AtomicLong (:last-collected collector))
     :collection-count (.get ^AtomicLong (:collection-count collector))
     :error-count (.get ^AtomicLong (:error-count collector))}))

(defn get-all-collector-stats
  "Get statistics for all collectors."
  []
  (into {} (for [collector-id (list-collectors)]
             [collector-id (get-collector-stats collector-id)])))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-metrics-collector!
  "Initialize metrics collector."
  []
  (log/info "Initializing metrics collector")
  ;; Register feature flag
  (flags/register-flag! "metrics-collector" "Enable metrics collector" true)
  ;; Create metrics
  (metrics/create-counter! :metricscollector/collectors-registered "Collectors registered")
  (metrics/create-gauge! :metricscollector/total-collectors "Total collectors"
                         #(count (:collectors @collector-state)))
  (metrics/create-gauge! :metricscollector/total-exporters "Total exporters"
                         #(count (:exporters @collector-state)))
  (metrics/create-gauge! :metricscollector/total-alerts "Total alerts"
                         #(count (:alerts @collector-state)))
  (metrics/create-gauge! :metricscollector/snapshot-count "Snapshot count"
                         #(count (:snapshots @collector-state)))
  (log/info "Metrics collector initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-collector-status []
  {:enabled (flags/is-enabled? "metrics-collector")
   :collectors (count (:collectors @collector-state))
   :exporters (count (:exporters @collector-state))
   :alerts (count (:alerts @collector-state))
   :snapshots (count (:snapshots @collector-state))
   :scheduler-active (some? (:scheduler @collector-state))
   :latest-snapshot (get-latest-snapshot)
   :collector-stats (get-all-collector-stats)
   :config (:config @collector-state)})
