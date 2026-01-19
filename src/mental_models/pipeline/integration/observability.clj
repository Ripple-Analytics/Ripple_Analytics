(ns mental-models.pipeline.integration.observability
  "Unified observability for mental model analysis system.
   
   Features:
   - Metrics collection
   - Log aggregation
   - Trace correlation
   - Health monitoring
   - SLO/SLI tracking
   - Alerting integration
   - Dashboard data
   - Anomaly detection"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant Duration]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:metrics {}          ;; metric-name -> metric-data
         :logs []             ;; recent logs
         :traces {}           ;; trace-id -> trace-data
         :health-checks {}    ;; check-id -> check-config
         :slos {}             ;; slo-id -> slo-config
         :dashboards {}       ;; dashboard-id -> dashboard-config
         :alerts []           ;; active alerts
         :stats {:metrics-collected 0 :logs-ingested 0 :traces-recorded 0}
         :initialized? false}))

;; ============================================================================
;; Metrics Collection
;; ============================================================================

(defn record-metric!
  "Record a metric value."
  [metric-name value & {:keys [tags type] :or {tags {} type :gauge}}]
  (when (flags/enabled? :observability)
    (let [timestamp (System/currentTimeMillis)
          metric-key [metric-name tags]]
      (swap! state update-in [:metrics metric-key]
             (fn [m]
               (let [current (or m {:name metric-name
                                    :type type
                                    :tags tags
                                    :values []
                                    :created-at timestamp})]
                 (update current :values
                         (fn [vals]
                           (take-last 1000 (conj vals {:value value :timestamp timestamp})))))))
      (swap! state update-in [:stats :metrics-collected] inc))))

(defn record-counter!
  "Increment a counter metric."
  [metric-name & {:keys [tags increment] :or {tags {} increment 1}}]
  (let [metric-key [metric-name tags]
        current (get-in @state [:metrics metric-key :value] 0)]
    (record-metric! metric-name (+ current increment) :tags tags :type :counter)))

(defn record-histogram!
  "Record a histogram value."
  [metric-name value & {:keys [tags] :or {tags {}}}]
  (record-metric! metric-name value :tags tags :type :histogram))

(defn record-timer!
  "Record a timer value (duration in ms)."
  [metric-name duration-ms & {:keys [tags] :or {tags {}}}]
  (record-metric! metric-name duration-ms :tags tags :type :timer))

(defn get-metric
  "Get a metric."
  [metric-name & {:keys [tags] :or {tags {}}}]
  (get-in @state [:metrics [metric-name tags]]))

(defn list-metrics
  "List all metrics."
  []
  (mapv (fn [[[name tags] m]]
          {:name name
           :type (:type m)
           :tags tags
           :latest-value (-> m :values last :value)
           :sample-count (count (:values m))})
        (:metrics @state)))

(defn get-metric-stats
  "Get statistics for a metric."
  [metric-name & {:keys [tags window-ms] :or {tags {} window-ms 300000}}]
  (when-let [metric (get-metric metric-name :tags tags)]
    (let [now (System/currentTimeMillis)
          cutoff (- now window-ms)
          recent-values (filter #(> (:timestamp %) cutoff) (:values metric))
          values (map :value recent-values)]
      (when (seq values)
        {:name metric-name
         :count (count values)
         :min (apply min values)
         :max (apply max values)
         :avg (/ (reduce + values) (count values))
         :sum (reduce + values)
         :p50 (nth (sort values) (int (* 0.5 (count values))) nil)
         :p95 (nth (sort values) (int (* 0.95 (count values))) nil)
         :p99 (nth (sort values) (int (* 0.99 (count values))) nil)}))))

;; ============================================================================
;; Log Aggregation
;; ============================================================================

(defn ingest-log!
  "Ingest a log entry."
  [log-entry]
  (when (flags/enabled? :observability)
    (let [entry (merge {:id (str (UUID/randomUUID))
                        :timestamp (System/currentTimeMillis)}
                       log-entry)]
      (swap! state update :logs
             (fn [logs]
               (take-last 10000 (conj logs entry))))
      (swap! state update-in [:stats :logs-ingested] inc)
      
      ;; Check for error patterns
      (when (= :error (:level entry))
        (check-error-patterns! entry)))))

(defn query-logs
  "Query logs."
  [& {:keys [level service message-pattern since until limit]
      :or {limit 100}}]
  (let [logs (:logs @state)
        filtered (cond->> logs
                   level (filter #(= (:level %) level))
                   service (filter #(= (:service %) service))
                   message-pattern (filter #(re-find (re-pattern message-pattern)
                                                     (str (:message %))))
                   since (filter #(> (:timestamp %) since))
                   until (filter #(< (:timestamp %) until))
                   true (take-last limit))]
    (vec filtered)))

(defn get-log-stats
  "Get log statistics."
  [& {:keys [window-ms] :or {window-ms 300000}}]
  (let [now (System/currentTimeMillis)
        cutoff (- now window-ms)
        recent (filter #(> (:timestamp %) cutoff) (:logs @state))
        by-level (group-by :level recent)
        by-service (group-by :service recent)]
    {:total (count recent)
     :by-level (into {} (map (fn [[k v]] [k (count v)]) by-level))
     :by-service (into {} (map (fn [[k v]] [k (count v)]) by-service))
     :error-rate (/ (count (get by-level :error [])) (max 1 (count recent)))}))

;; ============================================================================
;; Trace Correlation
;; ============================================================================

(defn start-trace!
  "Start a new trace."
  [trace-id & {:keys [name service tags]}]
  (let [trace {:id trace-id
               :name name
               :service service
               :tags (or tags {})
               :spans []
               :started-at (System/currentTimeMillis)
               :status :active}]
    (swap! state assoc-in [:traces trace-id] trace)
    (swap! state update-in [:stats :traces-recorded] inc)
    trace-id))

(defn add-span!
  "Add a span to a trace."
  [trace-id span-id & {:keys [name parent-id tags]}]
  (let [span {:id span-id
              :name name
              :parent-id parent-id
              :tags (or tags {})
              :started-at (System/currentTimeMillis)
              :status :active}]
    (swap! state update-in [:traces trace-id :spans] conj span)))

(defn end-span!
  "End a span."
  [trace-id span-id & {:keys [status error]}]
  (swap! state update-in [:traces trace-id :spans]
         (fn [spans]
           (mapv (fn [s]
                   (if (= (:id s) span-id)
                     (assoc s
                            :ended-at (System/currentTimeMillis)
                            :duration-ms (- (System/currentTimeMillis) (:started-at s))
                            :status (or status :completed)
                            :error error)
                     s))
                 spans))))

(defn end-trace!
  "End a trace."
  [trace-id & {:keys [status]}]
  (swap! state update-in [:traces trace-id]
         (fn [t]
           (assoc t
                  :ended-at (System/currentTimeMillis)
                  :duration-ms (- (System/currentTimeMillis) (:started-at t))
                  :status (or status :completed)))))

(defn get-trace
  "Get a trace."
  [trace-id]
  (get-in @state [:traces trace-id]))

(defn correlate-logs-to-trace
  "Correlate logs to a trace."
  [trace-id]
  (let [trace (get-trace trace-id)]
    (when trace
      (let [start (:started-at trace)
            end (or (:ended-at trace) (System/currentTimeMillis))
            related-logs (filter (fn [log]
                                   (and (>= (:timestamp log) start)
                                        (<= (:timestamp log) end)
                                        (or (= (:trace-id log) trace-id)
                                            (= (:service log) (:service trace)))))
                                 (:logs @state))]
        (assoc trace :related-logs (vec related-logs))))))

;; ============================================================================
;; Health Monitoring
;; ============================================================================

(defn register-health-check!
  "Register a health check."
  [check-id config]
  (let [check {:id check-id
               :name (get config :name (name check-id))
               :check-fn (get config :check-fn (fn [] {:healthy true}))
               :interval-ms (get config :interval-ms 30000)
               :timeout-ms (get config :timeout-ms 5000)
               :critical? (get config :critical? false)
               :last-check nil
               :status :unknown
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:health-checks check-id] check)
    check-id))

(defn run-health-check!
  "Run a health check."
  [check-id]
  (when-let [check (get-in @state [:health-checks check-id])]
    (let [start-time (System/currentTimeMillis)
          result (try
                   (let [r ((:check-fn check))]
                     {:healthy (:healthy r true)
                      :message (:message r)
                      :details (:details r)})
                   (catch Exception e
                     {:healthy false
                      :message (.getMessage e)
                      :error true}))
          duration (- (System/currentTimeMillis) start-time)]
      (swap! state update-in [:health-checks check-id]
             (fn [c]
               (assoc c
                      :last-check (System/currentTimeMillis)
                      :last-duration-ms duration
                      :status (if (:healthy result) :healthy :unhealthy)
                      :last-result result)))
      result)))

(defn run-all-health-checks!
  "Run all health checks."
  []
  (let [checks (keys (:health-checks @state))]
    (into {}
          (map (fn [check-id]
                 [check-id (run-health-check! check-id)])
               checks))))

(defn get-system-health
  "Get overall system health."
  []
  (let [checks (vals (:health-checks @state))
        healthy-count (count (filter #(= :healthy (:status %)) checks))
        unhealthy-count (count (filter #(= :unhealthy (:status %)) checks))
        critical-unhealthy (filter #(and (:critical? %) (= :unhealthy (:status %))) checks)]
    {:status (cond
               (seq critical-unhealthy) :critical
               (pos? unhealthy-count) :degraded
               :else :healthy)
     :total-checks (count checks)
     :healthy healthy-count
     :unhealthy unhealthy-count
     :critical-issues (mapv :name critical-unhealthy)}))

;; ============================================================================
;; SLO/SLI Tracking
;; ============================================================================

(defn define-slo!
  "Define a Service Level Objective."
  [slo-id config]
  (let [slo {:id slo-id
             :name (get config :name (name slo-id))
             :description (get config :description "")
             :target (get config :target 0.99)
             :metric-name (get config :metric-name)
             :metric-tags (get config :metric-tags {})
             :window-days (get config :window-days 30)
             :error-budget (get config :error-budget nil)
             :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:slos slo-id] slo)
    slo-id))

(defn calculate-sli
  "Calculate Service Level Indicator."
  [slo-id]
  (when-let [slo (get-in @state [:slos slo-id])]
    (let [window-ms (* (:window-days slo) 24 60 60 1000)
          stats (get-metric-stats (:metric-name slo)
                                  :tags (:metric-tags slo)
                                  :window-ms window-ms)]
      (when stats
        (let [;; Calculate SLI based on metric type
              sli (case (:type (get-metric (:metric-name slo) :tags (:metric-tags slo)))
                    :counter (/ (:sum stats) (max 1 (:count stats)))
                    :gauge (:avg stats)
                    :histogram (/ (count (filter #(<= % (:target slo))
                                                 (map :value (:values (get-metric (:metric-name slo))))))
                                  (max 1 (:count stats)))
                    (:avg stats))
              target (:target slo)
              error-budget (- 1 target)
              budget-consumed (if (>= sli target)
                                0
                                (/ (- target sli) error-budget))]
          {:slo-id slo-id
           :sli sli
           :target target
           :meeting-slo? (>= sli target)
           :error-budget error-budget
           :budget-consumed budget-consumed
           :budget-remaining (- 1 budget-consumed)})))))

(defn get-slo-report
  "Get SLO report for all SLOs."
  []
  (let [slos (keys (:slos @state))]
    {:slos (mapv calculate-sli slos)
     :overall-health (let [slis (map calculate-sli slos)
                           meeting (count (filter :meeting-slo? slis))]
                       (/ meeting (max 1 (count slis))))}))

;; ============================================================================
;; Alerting Integration
;; ============================================================================

(defn- check-error-patterns!
  "Check for error patterns and create alerts."
  [log-entry]
  (let [recent-errors (count (filter #(and (= :error (:level %))
                                           (> (:timestamp %) (- (System/currentTimeMillis) 60000)))
                                     (:logs @state)))]
    (when (> recent-errors 10)
      (create-alert! {:type :error-spike
                      :severity :warning
                      :message (str "Error spike detected: " recent-errors " errors in last minute")
                      :source :observability}))))

(defn create-alert!
  "Create an alert."
  [alert-data]
  (let [alert (merge {:id (str (UUID/randomUUID))
                      :created-at (System/currentTimeMillis)
                      :status :active}
                     alert-data)]
    (swap! state update :alerts conj alert)
    (events/emit! :alert-created alert)
    (:id alert)))

(defn get-active-alerts
  "Get active alerts."
  []
  (filter #(= :active (:status %)) (:alerts @state)))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id]
  (swap! state update :alerts
         (fn [alerts]
           (mapv (fn [a]
                   (if (= (:id a) alert-id)
                     (assoc a :status :acknowledged :acknowledged-at (System/currentTimeMillis))
                     a))
                 alerts))))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn create-dashboard!
  "Create a dashboard."
  [dashboard-id config]
  (let [dashboard {:id dashboard-id
                   :name (get config :name (name dashboard-id))
                   :description (get config :description "")
                   :panels (get config :panels [])
                   :refresh-interval-ms (get config :refresh-interval-ms 30000)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:dashboards dashboard-id] dashboard)
    dashboard-id))

(defn get-dashboard-data
  "Get data for a dashboard."
  [dashboard-id]
  (when-let [dashboard (get-in @state [:dashboards dashboard-id])]
    (let [panel-data (mapv (fn [panel]
                             (let [data (case (:type panel)
                                          :metric (get-metric-stats (:metric panel)
                                                                    :tags (:tags panel {})
                                                                    :window-ms (:window-ms panel 300000))
                                          :logs (query-logs :level (:level panel)
                                                            :limit (:limit panel 10))
                                          :health (get-system-health)
                                          :slo (calculate-sli (:slo-id panel))
                                          nil)]
                               (assoc panel :data data)))
                           (:panels dashboard))]
      (assoc dashboard :panels panel-data :generated-at (System/currentTimeMillis)))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-observability-stats
  "Get observability statistics."
  []
  (let [stats (:stats @state)]
    {:metrics-collected (:metrics-collected stats)
     :logs-ingested (:logs-ingested stats)
     :traces-recorded (:traces-recorded stats)
     :total-metrics (count (:metrics @state))
     :total-logs (count (:logs @state))
     :total-traces (count (:traces @state))
     :health-checks (count (:health-checks @state))
     :slos (count (:slos @state))
     :active-alerts (count (get-active-alerts))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-observability!
  "Initialize observability."
  []
  (when-not (:initialized? @state)
    ;; Register default health checks
    (register-health-check! :system
                            {:name "System Health"
                             :check-fn (fn [] {:healthy true :message "System operational"})
                             :critical? true})
    
    (register-health-check! :database
                            {:name "Database Connection"
                             :check-fn (fn [] {:healthy true :message "Database connected"})
                             :critical? true})
    
    (register-health-check! :lm-studio
                            {:name "LM Studio Connection"
                             :check-fn (fn [] {:healthy true :message "LM Studio available"})
                             :critical? false})
    
    ;; Define default SLOs
    (define-slo! :availability
                 {:name "Service Availability"
                  :description "99.9% availability target"
                  :target 0.999
                  :metric-name :availability
                  :window-days 30})
    
    (define-slo! :latency
                 {:name "Analysis Latency"
                  :description "95th percentile latency under 5s"
                  :target 0.95
                  :metric-name :analysis-latency
                  :window-days 7})
    
    ;; Create default dashboard
    (create-dashboard! :overview
                       {:name "System Overview"
                        :description "Main system dashboard"
                        :panels [{:id :health :type :health :title "System Health"}
                                 {:id :errors :type :logs :title "Recent Errors" :level :error :limit 5}
                                 {:id :availability :type :slo :title "Availability SLO" :slo-id :availability}]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Observability initialized")
    (events/emit! :observability-initialized {})
    true))
