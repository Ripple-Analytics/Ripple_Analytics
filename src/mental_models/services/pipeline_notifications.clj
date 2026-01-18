(ns mental-models.services.pipeline-notifications
  "Unified Notification Service for Pipeline Events
   
   Provides a single interface for all pipeline notifications:
   - Analysis completion
   - Lollapalooza alerts
   - Error notifications
   - Progress updates
   
   Integrates with: Slack, email, desktop, web app"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go chan put! <! close!]]
   [mental-models.alerts.lollapalooza :as lollapalooza]
   #?(:clj [clj-http.client :as http])))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def ^:dynamic *notification-config*
  {:enabled true
   :channels #{:web :slack}
   :slack-webhook-url (System/getenv "SLACK_WEBHOOK_URL")
   :email-recipients []
   :min-severity :info  ; :debug :info :warning :error :critical
   :batch-notifications true
   :batch-interval-ms 5000})

;; =============================================================================
;; NOTIFICATION TYPES
;; =============================================================================

(def notification-types
  {:analysis-complete {:icon "📊" :color "#22c55e" :priority :info}
   :batch-complete {:icon "📦" :color "#3b82f6" :priority :info}
   :lollapalooza {:icon "⚠️" :color "#f59e0b" :priority :warning}
   :critical-lollapalooza {:icon "🚨" :color "#dc2626" :priority :critical}
   :error {:icon "❌" :color "#ef4444" :priority :error}
   :pipeline-started {:icon "▶️" :color "#8b5cf6" :priority :info}
   :pipeline-stopped {:icon "⏹️" :color "#6b7280" :priority :info}})

;; =============================================================================
;; NOTIFICATION STATE
;; =============================================================================

(defonce notification-queue (chan 1000))
(defonce notification-history (atom []))

;; =============================================================================
;; NOTIFICATION FORMATTING
;; =============================================================================

(defn format-notification
  "Format notification for display"
  [{:keys [type title message data timestamp]}]
  (let [type-info (get notification-types type {:icon "📌" :color "#6b7280"})]
    {:type type
     :icon (:icon type-info)
     :color (:color type-info)
     :priority (:priority type-info)
     :title title
     :message message
     :data data
     :timestamp (or timestamp (System/currentTimeMillis))}))

(defn format-slack-message
  "Format notification for Slack"
  [{:keys [icon title message data]}]
  (str icon " *" title "*
" message
       (when data (str "
"))))

;; =============================================================================
;; NOTIFICATION DISPATCH
;; =============================================================================

#?(:clj
   (defn send-slack-notification!
     "Send notification to Slack"
     [notification]
     (when-let [webhook-url (:slack-webhook-url *notification-config*)]
       (try
         (http/post webhook-url
                    {:content-type :json
                     :body (str "{"text": ""
                                (str/escape (format-slack-message notification)
                                            {" "\"" 
ewline "\n"})
                                ""}")})
         (catch Exception e
           (println "[NOTIFY] Slack error:" (.getMessage e)))))))

(defn push-web-notification!
  "Push notification to web app"
  [notification]
  (swap! notification-history conj notification)
  (swap! notification-history #(vec (take-last 100 %))))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn notify!
  "Send a notification through all configured channels"
  [type title message & {:keys [data]}]
  (when (:enabled *notification-config*)
    (let [notification (format-notification {:type type
                                             :title title
                                             :message message
                                             :data data})]
      (put! notification-queue notification)
      (when (contains? (:channels *notification-config*) :web)
        (push-web-notification! notification))
      (when (contains? (:channels *notification-config*) :slack)
        #?(:clj (send-slack-notification! notification)))
      notification)))

(defn notify-analysis-complete!
  "Notify when analysis completes"
  [file-path result]
  (notify! :analysis-complete
           "Analysis Complete"
           (str "Processed: " file-path)
           :data {:file file-path
                  :models-detected (count (:biases result))
                  :lollapalooza? (get-in result [:lollapalooza :is_lollapalooza])}))

(defn notify-batch-complete!
  "Notify when batch processing completes"
  [total successful failed]
  (notify! :batch-complete
           "Batch Processing Complete"
           (str "Processed " total " files: " successful " successful, " failed " failed")
           :data {:total total :successful successful :failed failed}))

(defn notify-lollapalooza!
  "Notify when Lollapalooza detected"
  [lollapalooza-result source-info]
  (let [type (if (:critical? lollapalooza-result) :critical-lollapalooza :lollapalooza)]
    (notify! type
             (if (:critical? lollapalooza-result)
               "CRITICAL Lollapalooza Detected"
               "Lollapalooza Detected")
             (str (count (:converging-models lollapalooza-result)) " models converging in " (:name source-info))
             :data {:source source-info
                    :models (map :name (:converging-models lollapalooza-result))
                    :severity (:severity lollapalooza-result)})))

(defn notify-error!
  "Notify when error occurs"
  [error-message & {:keys [file-path exception]}]
  (notify! :error
           "Pipeline Error"
           error-message
           :data {:file file-path
                  :exception (when exception (.getMessage exception))}))

(defn notify-pipeline-started!
  "Notify when pipeline starts"
  [config]
  (notify! :pipeline-started
           "Pipeline Started"
           (str "Watching " (count (:watch-dirs config)) " directories")
           :data {:watch-dirs (:watch-dirs config)
                  :concurrency (:concurrency config)}))

(defn notify-pipeline-stopped!
  "Notify when pipeline stops"
  []
  (notify! :pipeline-stopped
           "Pipeline Stopped"
           "Analysis pipeline has been stopped"))

;; =============================================================================
;; QUERIES
;; =============================================================================

(defn get-recent-notifications
  "Get recent notifications"
  [& {:keys [limit type] :or {limit 50}}]
  (let [notifications @notification-history
        filtered (if type
                   (filter #(= type (:type %)) notifications)
                   notifications)]
    (take-last limit filtered)))

(defn get-notification-stats
  "Get notification statistics"
  []
  (let [notifications @notification-history
        now (System/currentTimeMillis)
        hour-ms (* 60 60 1000)]
    {:total (count notifications)
     :last-hour (count (filter #(> (:timestamp %) (- now hour-ms)) notifications))
     :by-type (frequencies (map :type notifications))
     :by-priority (frequencies (map :priority notifications))}))
