(ns mental-models.pipeline.integration.dashboard-provider
  "Dashboard Data Provider Module
   
   Provides data for the web dashboard:
   - Real-time metrics
   - Analysis summaries
   - Model statistics
   - System health data
   - Chart data formatting"
  (:require
   [mental-models.pipeline.integration.history-tracker :as history]
   [mental-models.pipeline.integration.alert-manager :as alerts]
   [mental-models.pipeline.integration.search-index :as search]
   [mental-models.registry.models :as registry]
   [mental-models.health.checks :as health]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; DASHBOARD CONFIGURATION
;; =============================================================================

(def refresh-intervals
  {:real-time 1000
   :fast 5000
   :normal 30000
   :slow 60000})

;; =============================================================================
;; OVERVIEW DATA
;; =============================================================================

(defn get-overview-data
  "Get overview data for the main dashboard."
  []
  (log/debug "Fetching overview data")
  (let [stats (history/get-summary-stats)
        active-alerts (alerts/get-active-alerts)
        health-status (health/get-health-status)]
    {:timestamp (System/currentTimeMillis)
     :summary {:total-analyses (:total-analyses stats)
               :total-lollapaloozas (:total-lollapaloozas stats)
               :lollapalooza-rate (:lollapalooza-rate stats)
               :unique-models (:unique-models stats)}
     :alerts {:active (count active-alerts)
              :critical (count (filter #(= :critical (:severity %)) active-alerts))
              :unacknowledged (count (alerts/get-unacknowledged-alerts))}
     :health {:status (if (:healthy health-status) "healthy" "unhealthy")
              :components (:components health-status)}
     :search {:indexed-documents (get-in (search/get-index-status) [:total-documents] 0)}}))

;; =============================================================================
;; MODEL STATISTICS
;; =============================================================================

(defn get-model-statistics
  "Get statistics for all mental models."
  []
  (log/debug "Fetching model statistics")
  (let [frequencies (:model-frequencies @history/history-state)
        all-models (registry/get-all-models)]
    {:timestamp (System/currentTimeMillis)
     :models (map (fn [model]
                    {:id (:model-id model)
                     :name (:name model)
                     :category (:category model)
                     :detection-count (get frequencies (:model-id model) 0)
                     :tags (:tags model)})
                  all-models)
     :top-models (take 10 (sort-by val > frequencies))
     :total-detections (reduce + (vals frequencies))}))

(defn get-model-detail
  "Get detailed data for a specific model."
  [model-id]
  (log/debug "Fetching model detail" {:model-id model-id})
  (let [model (registry/get-model model-id)
        recent-analyses (history/get-analyses-by-model model-id :limit 50)
        frequencies (:model-frequencies @history/history-state)]
    {:timestamp (System/currentTimeMillis)
     :model model
     :statistics {:total-detections (get frequencies model-id 0)
                  :recent-analyses (count recent-analyses)}
     :recent-analyses recent-analyses
     :co-occurring-models (frequencies
                           (mapcat #(map :model-id (:detections %)) recent-analyses))}))

;; =============================================================================
;; TREND DATA
;; =============================================================================

(defn get-trend-data
  "Get trend data for charts."
  [& {:keys [days] :or {days 7}}]
  (log/debug "Fetching trend data" {:days days})
  (let [daily-stats (history/get-recent-daily-stats :days days)
        model-trends (history/get-model-trends :days days)
        confidence-trends (history/get-confidence-trends :days days)]
    {:timestamp (System/currentTimeMillis)
     :period-days days
     :daily-stats (map (fn [{:keys [day date stats]}]
                         {:date date
                          :analyses (get stats :analyses 0)
                          :models (get stats :models 0)
                          :lollapaloozas (get stats :lollapaloozas 0)})
                       daily-stats)
     :model-trends {:top-models (:top-models model-trends)
                    :total-analyses (:total-analyses model-trends)}
     :confidence-trends confidence-trends}))

;; =============================================================================
;; CHART DATA FORMATTING
;; =============================================================================

(defn format-time-series
  "Format data for time series charts."
  [data & {:keys [x-key y-key label]
           :or {x-key :date y-key :value label "Value"}}]
  {:labels (map x-key data)
   :datasets [{:label label
               :data (map y-key data)}]})

(defn format-pie-chart
  "Format data for pie charts."
  [data]
  {:labels (map first data)
   :datasets [{:data (map second data)}]})

(defn format-bar-chart
  "Format data for bar charts."
  [data & {:keys [label] :or {label "Count"}}]
  {:labels (map (comp name first) data)
   :datasets [{:label label
               :data (map second data)}]})

(defn get-analysis-chart-data
  "Get formatted chart data for analyses."
  [& {:keys [days] :or {days 7}}]
  (let [trend-data (get-trend-data :days days)
        daily-stats (:daily-stats trend-data)]
    {:analyses-over-time (format-time-series daily-stats
                                             :x-key :date
                                             :y-key :analyses
                                             :label "Analyses")
     :lollapaloozas-over-time (format-time-series daily-stats
                                                  :x-key :date
                                                  :y-key :lollapaloozas
                                                  :label "Lollapaloozas")
     :top-models (format-bar-chart (get-in trend-data [:model-trends :top-models])
                                   :label "Detections")}))

;; =============================================================================
;; ALERT DATA
;; =============================================================================

(defn get-alert-data
  "Get alert data for the dashboard."
  []
  (log/debug "Fetching alert data")
  (let [active (alerts/get-active-alerts)
        history (alerts/get-alert-history :limit 50)]
    {:timestamp (System/currentTimeMillis)
     :active-alerts (map (fn [alert]
                           {:id (:id alert)
                            :type (:type alert)
                            :severity (:severity alert)
                            :message (:message alert)
                            :created-at (:created-at alert)
                            :status (:status alert)})
                         active)
     :alert-history history
     :by-severity {:critical (count (filter #(= :critical (:severity %)) active))
                   :high (count (filter #(= :high (:severity %)) active))
                   :medium (count (filter #(= :medium (:severity %)) active))
                   :low (count (filter #(= :low (:severity %)) active))}}))

;; =============================================================================
;; RECENT ACTIVITY
;; =============================================================================

(defn get-recent-activity
  "Get recent activity feed."
  [& {:keys [limit] :or {limit 20}}]
  (log/debug "Fetching recent activity" {:limit limit})
  (let [recent-analyses (history/get-recent-analyses :limit limit)
        recent-alerts (alerts/get-alert-history :limit limit)]
    {:timestamp (System/currentTimeMillis)
     :activities (take limit
                       (sort-by :timestamp >
                                (concat
                                 (map (fn [a]
                                        {:type :analysis
                                         :timestamp (:timestamp a)
                                         :description (str "Analyzed: " (or (:file-path a) "document"))
                                         :models (count (:detections a))
                                         :lollapalooza (:lollapalooza a)})
                                      recent-analyses)
                                 (map (fn [a]
                                        {:type :alert
                                         :timestamp (:created-at a)
                                         :description (:message a)
                                         :severity (:severity a)})
                                      recent-alerts))))}))

;; =============================================================================
;; SYSTEM STATUS
;; =============================================================================

(defn get-system-status
  "Get system status for the dashboard."
  []
  (log/debug "Fetching system status")
  {:timestamp (System/currentTimeMillis)
   :health (health/get-health-status)
   :feature-flags (flags/get-stats)
   :search-index (search/get-index-status)
   :alerts (alerts/get-manager-status)
   :history (history/get-tracker-status)})

;; =============================================================================
;; DASHBOARD API
;; =============================================================================

(defn get-dashboard-data
  "Get all dashboard data in one call."
  [& {:keys [sections] :or {sections #{:overview :models :trends :alerts :activity}}}]
  (log/debug "Fetching dashboard data" {:sections sections})
  (let [start-time (System/currentTimeMillis)]
    (cond-> {:timestamp (System/currentTimeMillis)}
      (contains? sections :overview)
      (assoc :overview (get-overview-data))
      
      (contains? sections :models)
      (assoc :models (get-model-statistics))
      
      (contains? sections :trends)
      (assoc :trends (get-trend-data))
      
      (contains? sections :alerts)
      (assoc :alerts (get-alert-data))
      
      (contains? sections :activity)
      (assoc :activity (get-recent-activity))
      
      (contains? sections :charts)
      (assoc :charts (get-analysis-chart-data))
      
      (contains? sections :system)
      (assoc :system (get-system-status))
      
      true
      (assoc :fetch-time-ms (- (System/currentTimeMillis) start-time)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-dashboard-provider!
  "Initialize dashboard data provider."
  []
  (log/info "Initializing dashboard provider")
  ;; Register feature flag
  (flags/register-flag! "dashboard-provider" "Enable dashboard provider" true)
  ;; Create metrics
  (metrics/create-counter! :dashboard/data-fetches "Dashboard data fetches")
  (metrics/create-histogram! :dashboard/fetch-time "Dashboard fetch time" [10 50 100 500])
  (log/info "Dashboard provider initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-provider-status []
  {:enabled (flags/is-enabled? "dashboard-provider")
   :refresh-intervals refresh-intervals})
