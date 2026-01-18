(ns mental-models.ui.metrics-dashboard
  "Electric Clojure UI Components for Metrics Dashboard
   
   Real-time metrics visualization:
   - Pipeline throughput and latency
   - Memory and CPU usage
   - Document processing stats
   - Lollapalooza event tracking
   - Historical trends"
  (:require
   [clojure.string :as str]
   [mental-models.benchmark.performance :as perf]
   [mental-models.services.pipeline-notifications :as notify]
   #?(:clj [hyperfiddle.electric :as e])))

;; =============================================================================
;; FORMATTING HELPERS
;; =============================================================================

(defn format-number [n decimals]
  #?(:clj (format (str "%." decimals "f") (double n))
     :cljs (.toFixed n decimals)))

(defn format-percent [n]
  (str (format-number n 1) "%"))

(defn format-duration [ms]
  (cond
    (< ms 1000) (str (format-number ms 0) "ms")
    (< ms 60000) (str (format-number (/ ms 1000) 1) "s")
    :else (str (format-number (/ ms 60000) 1) "m")))

;; =============================================================================
;; METRIC CARDS
;; =============================================================================

#?(:clj
   (e/defn MetricCard [title value unit & {:keys [trend color]}]
     (e/client
      [:div.metric-card {:class (or color "bg-white")}
       [:div.metric-title title]
       [:div.metric-value
        [:span.value value]
        [:span.unit unit]]
       (when trend
         [:div.metric-trend {:class (if (pos? trend) "positive" "negative")}
          (if (pos? trend) "↑" "↓") (format-number (abs trend) 1) "%"])])))

#?(:clj
   (e/defn ThroughputCard []
     (e/server
      (let [results (e/watch perf/metrics-state)
            throughput (perf/calculate-throughput)]
        (MetricCard. "Throughput" (format-number throughput 2) "docs/sec")))))

#?(:clj
   (e/defn LatencyCard []
     (e/server
      (let [latencies @perf/latency-histogram
            percentiles (perf/calculate-percentiles latencies)]
        (e/client
         [:div.latency-card
          [:div.metric-title "Latency"]
          [:div.latency-values
           [:div.p50 "p50: " (format-duration (or (:p50 percentiles) 0))]
           [:div.p95 "p95: " (format-duration (or (:p95 percentiles) 0))]
           [:div.p99 "p99: " (format-duration (or (:p99 percentiles) 0))]]])))))

#?(:clj
   (e/defn MemoryCard []
     (e/server
      (let [memory (perf/get-memory-usage)]
        (e/client
         [:div.memory-card
          [:div.metric-title "Memory"]
          [:div.memory-bar
           [:div.used {:style {:width (str (:percent memory) "%")}}]]
          [:div.memory-text
           (format-number (:used-mb memory) 0) " / "
           (format-number (:max-mb memory) 0) " MB"]])))))

;; =============================================================================
;; PROCESSING STATS
;; =============================================================================

#?(:clj
   (e/defn ProcessingStats []
     (e/server
      (let [{:keys [total-documents processed-documents failed-documents]} 
            (e/watch perf/metrics-state)
            success-rate (if (pos? total-documents)
                           (* 100.0 (/ processed-documents total-documents))
                           0)]
        (e/client
         [:div.processing-stats
          [:h3 "Processing"]
          [:div.stat-row
           [:span.label "Total:"]
           [:span.value total-documents]]
          [:div.stat-row
           [:span.label "Processed:"]
           [:span.value processed-documents]]
          [:div.stat-row
           [:span.label "Failed:"]
           [:span.value {:class (when (pos? failed-documents) "error")} failed-documents]]
          [:div.stat-row
           [:span.label "Success Rate:"]
           [:span.value (format-percent success-rate)]]])))))

;; =============================================================================
;; NOTIFICATION FEED
;; =============================================================================

#?(:clj
   (e/defn NotificationFeed []
     (e/server
      (let [notifications (e/watch notify/notification-history)
            recent (take 10 (reverse notifications))]
        (e/client
         [:div.notification-feed
          [:h3 "Recent Activity"]
          [:div.feed-list
           (e/for [n recent]
             [:div.feed-item {:class (name (:type n))}
              [:span.icon (:icon n)]
              [:span.title (:title n)]
              [:span.time (format-duration (- (System/currentTimeMillis) (:timestamp n)))]])]])))))

;; =============================================================================
;; LOLLAPALOOZA TRACKER
;; =============================================================================

#?(:clj
   (e/defn LollapaloozaTracker []
     (e/server
      (let [stats (notify/get-notification-stats)
            lollapalooza-count (get-in stats [:by-type :lollapalooza] 0)
            critical-count (get-in stats [:by-type :critical-lollapalooza] 0)]
        (e/client
         [:div.lollapalooza-tracker
          [:h3 "Lollapalooza Events"]
          [:div.event-counts
           [:div.normal
            [:span.count lollapalooza-count]
            [:span.label "Standard"]]
           [:div.critical
            [:span.count critical-count]
            [:span.label "Critical"]]]
          [:div.description
           "3+ mental models converging with >70% confidence"]])))))

;; =============================================================================
;; MAIN DASHBOARD
;; =============================================================================

#?(:clj
   (e/defn MetricsDashboard []
     (e/client
      [:div.metrics-dashboard
       [:h2 "Pipeline Metrics"]
       
       [:div.metrics-grid
        (e/server (ThroughputCard.))
        (e/server (LatencyCard.))
        (e/server (MemoryCard.))]
       
       [:div.dashboard-row
        [:div.left-panel
         (e/server (ProcessingStats.))
         (e/server (LollapaloozaTracker.))]
        [:div.right-panel
         (e/server (NotificationFeed.))]]])))

;; =============================================================================
;; COMPACT WIDGET
;; =============================================================================

#?(:clj
   (e/defn MetricsWidget []
     (e/server
      (let [throughput (perf/calculate-throughput)
            {:keys [processed-documents failed-documents]} (e/watch perf/metrics-state)
            memory (perf/get-memory-usage)]
        (e/client
         [:div.metrics-widget
          [:span.throughput (format-number throughput 1) " docs/s"]
          [:span.processed processed-documents " processed"]
          [:span.memory (format-number (:percent memory) 0) "% mem"]])))))
