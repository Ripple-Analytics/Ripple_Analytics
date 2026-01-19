(ns mental-models.pipeline.integration.notification-dispatcher
  "Notification Dispatcher Module
   
   Unified notification delivery:
   - Multi-channel dispatch
   - Template-based messages
   - Delivery tracking
   - Retry handling
   - User preferences"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; DISPATCHER STATE
;; =============================================================================

(defonce dispatcher-state (atom {:channels {}
                                 :templates {}
                                 :preferences {}
                                 :queue (chan 10000)
                                 :deliveries {}
                                 :config {:max-retries 3
                                          :retry-delay-ms 5000
                                          :batch-size 100}}))

;; =============================================================================
;; CHANNEL REGISTRATION
;; =============================================================================

(defn register-channel!
  "Register a notification channel."
  [channel-id {:keys [send-fn validate-fn description]}]
  (log/info "Registering notification channel" {:id channel-id})
  (swap! dispatcher-state assoc-in [:channels channel-id]
         {:id channel-id
          :send-fn send-fn
          :validate-fn validate-fn
          :description description
          :enabled true})
  (metrics/inc-counter! :notification/channels-registered)
  channel-id)

(defn unregister-channel!
  "Unregister a notification channel."
  [channel-id]
  (log/info "Unregistering notification channel" {:id channel-id})
  (swap! dispatcher-state update :channels dissoc channel-id))

(defn get-channel
  "Get a notification channel."
  [channel-id]
  (get-in @dispatcher-state [:channels channel-id]))

(defn list-channels
  "List all registered channels."
  []
  (keys (:channels @dispatcher-state)))

(defn enable-channel!
  "Enable a notification channel."
  [channel-id]
  (swap! dispatcher-state assoc-in [:channels channel-id :enabled] true))

(defn disable-channel!
  "Disable a notification channel."
  [channel-id]
  (swap! dispatcher-state assoc-in [:channels channel-id :enabled] false))

;; =============================================================================
;; MESSAGE TEMPLATES
;; =============================================================================

(defn register-template!
  "Register a message template."
  [template-id {:keys [subject body channels]}]
  (log/info "Registering notification template" {:id template-id})
  (swap! dispatcher-state assoc-in [:templates template-id]
         {:id template-id
          :subject subject
          :body body
          :channels channels}))

(defn get-template
  "Get a message template."
  [template-id]
  (get-in @dispatcher-state [:templates template-id]))

(defn render-template
  "Render a template with variables."
  [template vars]
  (reduce (fn [s [k v]]
            (str/replace s (str "{{" (name k) "}}") (str v)))
          template
          vars))

;; =============================================================================
;; USER PREFERENCES
;; =============================================================================

(defn set-user-preferences!
  "Set notification preferences for a user."
  [user-id preferences]
  (log/info "Setting user preferences" {:user user-id})
  (swap! dispatcher-state assoc-in [:preferences user-id] preferences))

(defn get-user-preferences
  "Get notification preferences for a user."
  [user-id]
  (get-in @dispatcher-state [:preferences user-id]
          {:channels #{:email :in-app}
           :quiet-hours nil
           :frequency :immediate}))

(defn user-accepts-channel?
  "Check if user accepts notifications on a channel."
  [user-id channel-id]
  (let [prefs (get-user-preferences user-id)]
    (contains? (:channels prefs) channel-id)))

(defn in-quiet-hours?
  "Check if current time is in user's quiet hours."
  [user-id]
  (let [prefs (get-user-preferences user-id)
        quiet-hours (:quiet-hours prefs)]
    (when quiet-hours
      (let [now (java.time.LocalTime/now)
            start (java.time.LocalTime/parse (:start quiet-hours))
            end (java.time.LocalTime/parse (:end quiet-hours))]
        (and (.isAfter now start) (.isBefore now end))))))

;; =============================================================================
;; NOTIFICATION CREATION
;; =============================================================================

(defn generate-notification-id
  "Generate a unique notification ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-notification
  "Create a notification."
  [notification-type recipient & {:keys [subject body data channels priority template]}]
  {:id (generate-notification-id)
   :type notification-type
   :recipient recipient
   :subject subject
   :body body
   :data data
   :channels (or channels [:email :in-app])
   :priority (or priority :normal)
   :template template
   :status :pending
   :created-at (System/currentTimeMillis)
   :attempts 0})

;; =============================================================================
;; NOTIFICATION DISPATCH
;; =============================================================================

(defn dispatch-to-channel!
  "Dispatch a notification to a specific channel."
  [notification channel-id]
  (when-let [channel (get-channel channel-id)]
    (when (:enabled channel)
      (log/debug "Dispatching to channel" {:notification (:id notification) :channel channel-id})
      (try
        (let [send-fn (:send-fn channel)
              result (send-fn notification)]
          (metrics/inc-counter! :notification/sent)
          {:success true :channel channel-id :result result})
        (catch Exception e
          (log/error "Channel dispatch failed" {:channel channel-id :error (.getMessage e)})
          (metrics/inc-counter! :notification/failed)
          {:success false :channel channel-id :error (.getMessage e)})))))

(defn dispatch!
  "Dispatch a notification to all configured channels."
  [notification]
  (when (flags/is-enabled? "notification-dispatcher")
    (log/info "Dispatching notification" {:id (:id notification) :type (:type notification)})
    (let [recipient (:recipient notification)
          channels (:channels notification)
          ;; Filter by user preferences
          accepted-channels (filter #(user-accepts-channel? recipient %) channels)
          ;; Check quiet hours
          should-dispatch (not (in-quiet-hours? recipient))]
      (if should-dispatch
        (let [results (doall (map #(dispatch-to-channel! notification %) accepted-channels))
              all-success (every? :success results)]
          ;; Track delivery
          (swap! dispatcher-state assoc-in [:deliveries (:id notification)]
                 {:notification notification
                  :results results
                  :dispatched-at (System/currentTimeMillis)
                  :success all-success})
          (events/publish! :notification/dispatched {:id (:id notification) :success all-success})
          {:success all-success :results results})
        (do
          (log/debug "Notification deferred (quiet hours)" {:id (:id notification)})
          {:success false :reason :quiet-hours})))))

(defn queue-notification!
  "Queue a notification for async dispatch."
  [notification]
  (go (>! (:queue @dispatcher-state) notification))
  (:id notification))

(defn send!
  "Convenience function to create and dispatch a notification."
  [notification-type recipient & opts]
  (let [notification (apply create-notification notification-type recipient opts)]
    (dispatch! notification)))

(defn send-async!
  "Convenience function to create and queue a notification."
  [notification-type recipient & opts]
  (let [notification (apply create-notification notification-type recipient opts)]
    (queue-notification! notification)))

;; =============================================================================
;; TEMPLATE-BASED DISPATCH
;; =============================================================================

(defn send-from-template!
  "Send a notification using a template."
  [template-id recipient vars]
  (when-let [template (get-template template-id)]
    (let [subject (render-template (:subject template) vars)
          body (render-template (:body template) vars)
          notification (create-notification template-id recipient
                                            :subject subject
                                            :body body
                                            :channels (:channels template)
                                            :template template-id)]
      (dispatch! notification))))

;; =============================================================================
;; BATCH DISPATCH
;; =============================================================================

(defn dispatch-batch!
  "Dispatch multiple notifications."
  [notifications]
  (log/info "Dispatching batch" {:count (count notifications)})
  (let [results (doall (pmap dispatch! notifications))]
    {:total (count notifications)
     :success (count (filter :success results))
     :failed (count (filter (complement :success) results))}))

;; =============================================================================
;; QUEUE PROCESSOR
;; =============================================================================

(defonce queue-processor (atom nil))

(defn start-queue-processor!
  "Start the notification queue processor."
  []
  (log/info "Starting notification queue processor")
  (reset! queue-processor
          (go-loop []
            (when-let [notification (<! (:queue @dispatcher-state))]
              (try
                (dispatch! notification)
                (catch Exception e
                  (log/error "Queue processor error" {:error (.getMessage e)})))
              (recur)))))

(defn stop-queue-processor!
  "Stop the notification queue processor."
  []
  (when @queue-processor
    (close! (:queue @dispatcher-state))
    (reset! queue-processor nil)
    (log/info "Notification queue processor stopped")))

;; =============================================================================
;; DELIVERY TRACKING
;; =============================================================================

(defn get-delivery
  "Get delivery status for a notification."
  [notification-id]
  (get-in @dispatcher-state [:deliveries notification-id]))

(defn list-deliveries
  "List recent deliveries."
  [& {:keys [limit success-only]}]
  (let [deliveries (vals (:deliveries @dispatcher-state))
        filtered (if success-only
                   (filter :success deliveries)
                   deliveries)
        sorted (sort-by :dispatched-at > filtered)]
    (if limit
      (take limit sorted)
      sorted)))

(defn cleanup-old-deliveries!
  "Clean up old delivery records."
  [max-age-ms]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        old-ids (for [[id delivery] (:deliveries @dispatcher-state)
                      :when (< (:dispatched-at delivery) cutoff)]
                  id)]
    (doseq [id old-ids]
      (swap! dispatcher-state update :deliveries dissoc id))
    (count old-ids)))

;; =============================================================================
;; DEFAULT CHANNELS
;; =============================================================================

(def email-channel
  {:send-fn (fn [notification]
              (log/info "Sending email" {:to (:recipient notification)
                                         :subject (:subject notification)})
              {:sent true})
   :validate-fn (fn [recipient] (re-matches #".+@.+\..+" recipient))
   :description "Email notifications"})

(def slack-channel
  {:send-fn (fn [notification]
              (log/info "Sending Slack message" {:to (:recipient notification)})
              {:sent true})
   :validate-fn (fn [_] true)
   :description "Slack notifications"})

(def in-app-channel
  {:send-fn (fn [notification]
              (log/info "Sending in-app notification" {:to (:recipient notification)})
              (events/publish! :notification/in-app notification)
              {:sent true})
   :validate-fn (fn [_] true)
   :description "In-app notifications"})

(def sms-channel
  {:send-fn (fn [notification]
              (log/info "Sending SMS" {:to (:recipient notification)})
              {:sent true})
   :validate-fn (fn [recipient] (re-matches #"\+?[0-9]{10,15}" recipient))
   :description "SMS notifications"})

;; =============================================================================
;; DEFAULT TEMPLATES
;; =============================================================================

(def default-templates
  {:analysis-complete {:subject "Analysis Complete: {{document}}"
                       :body "Your analysis of {{document}} is complete. {{model_count}} mental models were detected."
                       :channels [:email :in-app]}
   :lollapalooza-alert {:subject "Lollapalooza Effect Detected!"
                        :body "A Lollapalooza effect was detected in {{document}} with {{model_count}} converging models."
                        :channels [:email :slack :in-app]}
   :system-alert {:subject "System Alert: {{alert_type}}"
                  :body "{{message}}"
                  :channels [:email :slack]}})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-notification-dispatcher!
  "Initialize notification dispatcher."
  []
  (log/info "Initializing notification dispatcher")
  ;; Register feature flag
  (flags/register-flag! "notification-dispatcher" "Enable notification dispatch" true)
  ;; Create metrics
  (metrics/create-counter! :notification/channels-registered "Channels registered")
  (metrics/create-counter! :notification/sent "Notifications sent")
  (metrics/create-counter! :notification/failed "Notifications failed")
  (metrics/create-gauge! :notification/queue-size "Queue size"
                         #(count (:deliveries @dispatcher-state)))
  ;; Register default channels
  (register-channel! :email email-channel)
  (register-channel! :slack slack-channel)
  (register-channel! :in-app in-app-channel)
  (register-channel! :sms sms-channel)
  ;; Register default templates
  (doseq [[id template] default-templates]
    (register-template! id template))
  ;; Start queue processor
  (start-queue-processor!)
  (log/info "Notification dispatcher initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-dispatcher-status []
  {:enabled (flags/is-enabled? "notification-dispatcher")
   :channels (into {} (map (fn [[k v]] [k {:enabled (:enabled v)}])
                           (:channels @dispatcher-state)))
   :templates (count (:templates @dispatcher-state))
   :deliveries (count (:deliveries @dispatcher-state))
   :queue-processor-running (some? @queue-processor)})
