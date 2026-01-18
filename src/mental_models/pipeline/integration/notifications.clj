(ns mental-models.pipeline.integration.notifications
  "Notification Integration for Pipeline
   
   Connects the pipeline to notification services:
   - Slack notifications for Lollapalooza alerts
   - Email notifications for batch completions
   - Desktop notifications for real-time updates
   - WebSocket streaming for web app"
  (:require
   [mental-models.services.pipeline-notifications :as notif]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.core.async :as async :refer [go chan <! >! close! mult tap]]))

;; =============================================================================
;; NOTIFICATION CHANNELS
;; =============================================================================

(def notification-channels
  {:slack {:enabled true :priority :high}
   :email {:enabled true :priority :medium}
   :desktop {:enabled true :priority :low}
   :websocket {:enabled true :priority :high}})

(defonce websocket-clients (atom #{}))
(defonce notification-mult (atom nil))

;; =============================================================================
;; WEBSOCKET STREAMING
;; =============================================================================

(defn create-notification-stream []
  (let [ch (chan 100)]
    (reset! notification-mult (mult ch))
    ch))

(defn subscribe-websocket [client-id]
  (let [client-chan (chan 100)]
    (when @notification-mult
      (tap @notification-mult client-chan))
    (swap! websocket-clients conj {:id client-id :chan client-chan})
    (log/info "WebSocket client subscribed" {:client-id client-id})
    (metrics/inc-counter! :notifications/websocket-subscriptions)
    client-chan))

(defn unsubscribe-websocket [client-id]
  (when-let [client (first (filter #(= client-id (:id %)) @websocket-clients))]
    (close! (:chan client))
    (swap! websocket-clients disj client)
    (log/info "WebSocket client unsubscribed" {:client-id client-id})))

(defn broadcast-to-websockets [message]
  (when (flags/is-enabled? "websocket-notifications")
    (doseq [{:keys [id chan]} @websocket-clients]
      (go
        (try
          (>! chan message)
          (catch Exception e
            (log/warn "Failed to send to WebSocket" {:client-id id :error (.getMessage e)})
            (unsubscribe-websocket id)))))))

;; =============================================================================
;; NOTIFICATION SENDING
;; =============================================================================

(defn send-slack-notification!
  "Send notification to Slack with circuit breaker."
  [message & {:keys [channel] :or {channel "#mental-models"}}]
  (when (flags/is-enabled? "slack-notifications")
    (let [cb-result (cb/execute (cb/get-circuit-breaker "notification")
                                #(notif/send-slack-notification! message channel))]
      (if (= :success (:status cb-result))
        (do
          (metrics/inc-counter! :notifications/slack-sent)
          (audit/log-operation! {:operation :slack-notification
                                 :channel channel
                                 :message-length (count message)})
          true)
        (do
          (log/warn "Slack notification failed" {:status (:status cb-result)})
          (metrics/inc-counter! :notifications/slack-failures)
          false)))))

(defn send-email-notification!
  "Send email notification."
  [to subject body]
  (when (flags/is-enabled? "email-notifications")
    (let [cb-result (cb/execute (cb/get-circuit-breaker "notification")
                                #(notif/send-email-notification! to subject body))]
      (if (= :success (:status cb-result))
        (do
          (metrics/inc-counter! :notifications/email-sent)
          (audit/log-operation! {:operation :email-notification
                                 :to to
                                 :subject subject})
          true)
        (do
          (log/warn "Email notification failed" {:status (:status cb-result)})
          (metrics/inc-counter! :notifications/email-failures)
          false)))))

(defn send-desktop-notification!
  "Send desktop notification."
  [title message]
  (when (flags/is-enabled? "desktop-notifications")
    (metrics/inc-counter! :notifications/desktop-sent)
    (notif/send-desktop-notification! title message)))

;; =============================================================================
;; LOLLAPALOOZA ALERTS
;; =============================================================================

(defn send-lollapalooza-alert!
  "Send Lollapalooza alert to all channels."
  [alert]
  (let [model-names (map name (:models alert))
        message (str "LOLLAPALOOZA DETECTED!\n"
                     "Models: " (clojure.string/join ", " model-names) "\n"
                     "Confidence: " (format "%.1f%%" (* 100 (:avg-confidence alert))) "\n"
                     "Time: " (java.time.Instant/ofEpochMilli (:timestamp alert)))]
    (send-slack-notification! message :channel "#lollapalooza-alerts")
    (send-desktop-notification! "Lollapalooza Alert!" message)
    (broadcast-to-websockets {:type :lollapalooza
                              :alert alert
                              :message message
                              :timestamp (System/currentTimeMillis)})
    (log/warn "Lollapalooza alert sent to all channels" {:models model-names})))

;; =============================================================================
;; BATCH COMPLETION NOTIFICATIONS
;; =============================================================================

(defn send-batch-completion-notification!
  "Send batch completion notification."
  [batch-result]
  (let [{:keys [total successful failed duration-ms lollapaloozas]} batch-result
        message (str "Batch Analysis Complete\n"
                     "Total: " total " documents\n"
                     "Successful: " successful "\n"
                     "Failed: " failed "\n"
                     "Lollapaloozas: " (or lollapaloozas 0) "\n"
                     "Duration: " (format "%.1f" (/ duration-ms 1000.0)) "s")]
    (send-slack-notification! message)
    (broadcast-to-websockets {:type :batch-complete
                              :result batch-result
                              :message message
                              :timestamp (System/currentTimeMillis)})))

;; =============================================================================
;; ANALYSIS PROGRESS NOTIFICATIONS
;; =============================================================================

(defn send-analysis-progress!
  "Send analysis progress update via WebSocket."
  [progress]
  (broadcast-to-websockets {:type :analysis-progress
                            :progress progress
                            :timestamp (System/currentTimeMillis)}))

(defn send-analysis-complete!
  "Send analysis completion notification."
  [result]
  (broadcast-to-websockets {:type :analysis-complete
                            :result result
                            :timestamp (System/currentTimeMillis)}))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-notification-handlers!
  "Set up event handlers for automatic notifications."
  []
  (events/subscribe! :lollapalooza/detected
                     (fn [alert]
                       (send-lollapalooza-alert! alert)))
  (events/subscribe! :batch/completed
                     (fn [result]
                       (send-batch-completion-notification! result)))
  (events/subscribe! :analysis/progress
                     (fn [progress]
                       (send-analysis-progress! progress)))
  (events/subscribe! :analysis/completed
                     (fn [event]
                       (send-analysis-complete! (:result event))))
  (log/info "Notification event handlers initialized"))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-notifications!
  "Initialize notification integration."
  []
  (log/info "Initializing notification integration")
  (create-notification-stream)
  (flags/register-flag! "slack-notifications" "Enable Slack notifications" true)
  (flags/register-flag! "email-notifications" "Enable email notifications" true)
  (flags/register-flag! "desktop-notifications" "Enable desktop notifications" true)
  (flags/register-flag! "websocket-notifications" "Enable WebSocket notifications" true)
  (metrics/create-counter! :notifications/slack-sent "Slack notifications sent")
  (metrics/create-counter! :notifications/slack-failures "Slack notification failures")
  (metrics/create-counter! :notifications/email-sent "Email notifications sent")
  (metrics/create-counter! :notifications/email-failures "Email notification failures")
  (metrics/create-counter! :notifications/desktop-sent "Desktop notifications sent")
  (metrics/create-counter! :notifications/websocket-subscriptions "WebSocket subscriptions")
  (setup-notification-handlers!)
  (log/info "Notification integration initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-notification-status []
  {:websocket-clients (count @websocket-clients)
   :channels notification-channels
   :flags {:slack (flags/is-enabled? "slack-notifications")
           :email (flags/is-enabled? "email-notifications")
           :desktop (flags/is-enabled? "desktop-notifications")
           :websocket (flags/is-enabled? "websocket-notifications")}})
