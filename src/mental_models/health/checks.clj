(ns mental-models.health.checks
  "Health Check Module for Pipeline Components
   
   Provides comprehensive health monitoring:
   - Component health status
   - Dependency checks (LM Studio, Database, etc.)
   - Resource utilization
   - Readiness and liveness probes
   - Health history and trends"
  (:require
   [clojure.core.async :as async :refer [go chan put! <! timeout]]
   [mental-models.config.pipeline-config :as config]
   [mental-models.db.pipeline-storage :as db-storage]
   #?(:clj [clj-http.client :as http]))
  (:import
   [java.lang.management ManagementFactory]))

;; =============================================================================
;; HEALTH STATUS DEFINITIONS
;; =============================================================================

(def health-statuses
  {:healthy {:code 200 :message "OK"}
   :degraded {:code 200 :message "Degraded but operational"}
   :unhealthy {:code 503 :message "Service unavailable"}
   :unknown {:code 500 :message "Unable to determine health"}})

;; =============================================================================
;; HEALTH STATE
;; =============================================================================

(defonce health-state
  (atom {:components {}
         :last-check nil
         :overall-status :unknown
         :history []}))

;; =============================================================================
;; COMPONENT HEALTH CHECKS
;; =============================================================================

#?(:clj
   (defn check-lm-studio-health
     "Check LM Studio server health"
     []
     (let [url (str (config/get-config :lm-studio-url) "/v1/models")]
       (try
         (let [response (http/get url {:socket-timeout 5000
                                       :connection-timeout 5000
                                       :throw-exceptions false})]
           (if (= 200 (:status response))
             {:status :healthy
              :message "LM Studio responding"
              :latency-ms (:request-time response)}
             {:status :unhealthy
              :message (str "LM Studio returned " (:status response))}))
         (catch Exception e
           {:status :unhealthy
            :message (str "LM Studio unreachable: " (.getMessage e))})))))

(defn check-database-health
  "Check database connection health"
  []
  (try
    (let [result (db-storage/check-db-connection)]
      (if (= :healthy (:status result))
        {:status :healthy
         :message "Database connection OK"}
        {:status :unhealthy
         :message (:message result)}))
    (catch Exception e
      {:status :unhealthy
       :message (str "Database error: " (.getMessage e))})))

(defn check-memory-health
  "Check JVM memory health"
  []
  (let [runtime (Runtime/getRuntime)
        used (- (.totalMemory runtime) (.freeMemory runtime))
        max (.maxMemory runtime)
        percent (* 100.0 (/ used max))]
    (cond
      (< percent 70) {:status :healthy
                      :message (str "Memory usage: " (int percent) "%")
                      :used-mb (/ used 1048576.0)
                      :max-mb (/ max 1048576.0)}
      (< percent 90) {:status :degraded
                      :message (str "High memory usage: " (int percent) "%")
                      :used-mb (/ used 1048576.0)
                      :max-mb (/ max 1048576.0)}
      :else {:status :unhealthy
             :message (str "Critical memory usage: " (int percent) "%")
             :used-mb (/ used 1048576.0)
             :max-mb (/ max 1048576.0)})))

(defn check-disk-health
  "Check disk space health"
  []
  (try
    (let [file (java.io.File. ".")
          free (.getFreeSpace file)
          total (.getTotalSpace file)
          percent (* 100.0 (/ (- total free) total))]
      (cond
        (< percent 80) {:status :healthy
                        :message (str "Disk usage: " (int percent) "%")
                        :free-gb (/ free 1073741824.0)
                        :total-gb (/ total 1073741824.0)}
        (< percent 95) {:status :degraded
                        :message (str "High disk usage: " (int percent) "%")}
        :else {:status :unhealthy
               :message (str "Critical disk usage: " (int percent) "%")}))
    (catch Exception e
      {:status :unknown
       :message (str "Disk check failed: " (.getMessage e))})))

(defn check-thread-health
  "Check thread pool health"
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)
        active (.getThreadCount thread-bean)
        peak (.getPeakThreadCount thread-bean)]
    (cond
      (< active 100) {:status :healthy
                      :message (str "Active threads: " active)
                      :active active
                      :peak peak}
      (< active 500) {:status :degraded
                      :message (str "High thread count: " active)}
      :else {:status :unhealthy
             :message (str "Critical thread count: " active)})))

;; =============================================================================
;; AGGREGATE HEALTH CHECK
;; =============================================================================

(defn run-all-checks
  "Run all health checks and aggregate results"
  []
  (let [checks {:lm-studio #?(:clj (check-lm-studio-health) :cljs {:status :unknown})
                :database (check-database-health)
                :memory (check-memory-health)
                :disk (check-disk-health)
                :threads (check-thread-health)}
        statuses (map :status (vals checks))
        overall (cond
                  (every? #(= :healthy %) statuses) :healthy
                  (some #(= :unhealthy %) statuses) :unhealthy
                  (some #(= :degraded %) statuses) :degraded
                  :else :unknown)
        result {:components checks
                :overall-status overall
                :timestamp (System/currentTimeMillis)}]
    (swap! health-state merge result)
    (swap! health-state update :history #(take-last 100 (conj % result)))
    result))

;; =============================================================================
;; KUBERNETES-STYLE PROBES
;; =============================================================================

(defn liveness-probe
  "Check if the service is alive (basic health)"
  []
  (let [memory (check-memory-health)
        threads (check-thread-health)]
    (if (and (not= :unhealthy (:status memory))
             (not= :unhealthy (:status threads)))
      {:alive true :status 200}
      {:alive false :status 503})))

(defn readiness-probe
  "Check if the service is ready to accept traffic"
  []
  (let [lm-studio #?(:clj (check-lm-studio-health) :cljs {:status :unknown})
        database (check-database-health)]
    (if (and (= :healthy (:status lm-studio))
             (= :healthy (:status database)))
      {:ready true :status 200}
      {:ready false :status 503
       :reasons (cond-> []
                  (not= :healthy (:status lm-studio)) (conj "LM Studio not ready")
                  (not= :healthy (:status database)) (conj "Database not ready"))})))

(defn startup-probe
  "Check if the service has completed startup"
  []
  (let [config-loaded (some? @config/current-config)]
    (if config-loaded
      {:started true :status 200}
      {:started false :status 503})))

;; =============================================================================
;; HEALTH ENDPOINTS
;; =============================================================================

(defn health-summary
  "Get a summary of current health status"
  []
  (let [{:keys [components overall-status timestamp]} @health-state]
    {:status overall-status
     :timestamp timestamp
     :components (into {} (map (fn [[k v]] [k (:status v)]) components))}))

(defn health-details
  "Get detailed health information"
  []
  @health-state)

(defn health-history
  "Get health check history"
  [& {:keys [limit] :or {limit 20}}]
  (take-last limit (:history @health-state)))

;; =============================================================================
;; BACKGROUND HEALTH MONITORING
;; =============================================================================

(defonce health-monitor-channel (atom nil))

(defn start-health-monitoring!
  "Start background health monitoring"
  [interval-ms]
  (let [ch (chan)]
    (reset! health-monitor-channel ch)
    (go
      (loop []
        (let [[_ port] (async/alts! [ch (timeout interval-ms)])]
          (when-not (= port ch)
            (run-all-checks)
            (recur)))))
    (println "[HEALTH] Monitoring started with interval" interval-ms "ms")
    ch))

(defn stop-health-monitoring!
  "Stop background health monitoring"
  []
  (when-let [ch @health-monitor-channel]
    (async/close! ch)
    (reset! health-monitor-channel nil)
    (println "[HEALTH] Monitoring stopped")))

;; =============================================================================
;; HEALTH ALERTS
;; =============================================================================

(defn check-and-alert!
  "Run health checks and alert on issues"
  [alert-fn]
  (let [result (run-all-checks)
        unhealthy (filter (fn [[_ v]] (= :unhealthy (:status v)))
                          (:components result))]
    (when (seq unhealthy)
      (alert-fn {:type :health-alert
                 :severity :critical
                 :message (str "Unhealthy components: " (keys unhealthy))
                 :details unhealthy}))
    result))
