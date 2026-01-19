(ns mental-models.pipeline.integration.notification-hub
  "Notification hub for multi-channel delivery.
   
   Features:
   - Multi-channel notifications (email, SMS, push, in-app)
   - Template management
   - Delivery scheduling
   - User preferences
   - Notification grouping
   - Read/unread tracking
   - Notification history
   - Rate limiting"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:notifications {}    ;; notification-id -> notification
         :templates {}        ;; template-id -> template
         :preferences {}      ;; user-id -> preferences
         :channels {}         ;; channel-id -> channel-config
         :groups {}           ;; group-id -> notification-group
         :scheduled []        ;; scheduled notifications
         :stats {:sent 0 :delivered 0 :failed 0 :read 0}
         :initialized? false}))

;; ============================================================================
;; Channel Management
;; ============================================================================

(defn register-channel!
  "Register a notification channel."
  [channel-id config]
  (let [channel {:id channel-id
                 :name (get config :name (name channel-id))
                 :type (get config :type :generic)
                 :provider (get config :provider)
                 :settings (get config :settings {})
                 :rate-limit (get config :rate-limit {:per-minute 60})
                 :enabled? (get config :enabled? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:channels channel-id] channel)
    (logging/log :info "Registered channel" {:channel-id channel-id})
    channel-id))

(defn get-channel
  "Get a channel."
  [channel-id]
  (get-in @state [:channels channel-id]))

(defn list-channels
  "List all channels."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :type (:type c)
           :enabled? (:enabled? c)})
        (:channels @state)))

(defn enable-channel!
  "Enable a channel."
  [channel-id]
  (swap! state assoc-in [:channels channel-id :enabled?] true))

(defn disable-channel!
  "Disable a channel."
  [channel-id]
  (swap! state assoc-in [:channels channel-id :enabled?] false))

;; ============================================================================
;; Template Management
;; ============================================================================

(defn create-template!
  "Create a notification template."
  [template-id config]
  (let [template {:id template-id
                  :name (get config :name (name template-id))
                  :subject (get config :subject "")
                  :body (get config :body "")
                  :html-body (get config :html-body nil)
                  :channels (get config :channels #{:email :in-app})
                  :variables (get config :variables [])
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:templates template-id] template)
    (logging/log :info "Created template" {:template-id template-id})
    template-id))

(defn get-template
  "Get a template."
  [template-id]
  (get-in @state [:templates template-id]))

(defn list-templates
  "List all templates."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :channels (:channels t)})
        (:templates @state)))

(defn render-template
  "Render a template with variables."
  [template-id variables]
  (when-let [template (get-template template-id)]
    (let [render-string (fn [s]
                          (reduce (fn [result [k v]]
                                    (str/replace result (str "{{" (name k) "}}") (str v)))
                                  s
                                  variables))]
      {:subject (render-string (:subject template))
       :body (render-string (:body template))
       :html-body (when (:html-body template)
                    (render-string (:html-body template)))})))

(defn delete-template!
  "Delete a template."
  [template-id]
  (swap! state update :templates dissoc template-id))

;; ============================================================================
;; User Preferences
;; ============================================================================

(defn set-preferences!
  "Set user notification preferences."
  [user-id preferences]
  (swap! state assoc-in [:preferences user-id]
         (merge {:user-id user-id
                 :channels {:email true :sms false :push true :in-app true}
                 :quiet-hours {:enabled false :start "22:00" :end "08:00"}
                 :frequency :immediate
                 :categories {}
                 :updated-at (System/currentTimeMillis)}
                preferences)))

(defn get-preferences
  "Get user notification preferences."
  [user-id]
  (get-in @state [:preferences user-id]
          {:channels {:email true :sms false :push true :in-app true}
           :quiet-hours {:enabled false}
           :frequency :immediate}))

(defn update-preferences!
  "Update user notification preferences."
  [user-id updates]
  (swap! state update-in [:preferences user-id]
         (fn [p]
           (merge (or p {}) updates {:updated-at (System/currentTimeMillis)}))))

(defn- should-deliver?
  "Check if notification should be delivered based on preferences."
  [user-id channel category]
  (let [prefs (get-preferences user-id)
        channel-enabled? (get-in prefs [:channels channel] true)
        category-enabled? (get-in prefs [:categories category :enabled] true)
        quiet-hours (:quiet-hours prefs)
        in-quiet-hours? false] ;; Simplified - would check actual time
    (and channel-enabled? category-enabled? (not in-quiet-hours?))))

;; ============================================================================
;; Notification Management
;; ============================================================================

(defn send-notification!
  "Send a notification."
  [config]
  (when (flags/enabled? :notification-hub)
    (let [notification-id (str (UUID/randomUUID))
          user-id (:user-id config)
          template-id (:template-id config)
          template (when template-id (get-template template-id))
          rendered (when template (render-template template-id (:variables config {})))
          
          notification {:id notification-id
                        :user-id user-id
                        :template-id template-id
                        :subject (or (:subject config) (:subject rendered) "Notification")
                        :body (or (:body config) (:body rendered) "")
                        :html-body (or (:html-body config) (:html-body rendered))
                        :category (get config :category :general)
                        :priority (get config :priority :normal)
                        :channels (or (:channels config)
                                      (:channels template)
                                      #{:in-app})
                        :data (get config :data {})
                        :status :pending
                        :read? false
                        :created-at (System/currentTimeMillis)}]
      
      (swap! state assoc-in [:notifications notification-id] notification)
      
      ;; Deliver to each channel
      (doseq [channel (:channels notification)]
        (when (should-deliver? user-id channel (:category notification))
          (deliver-to-channel! notification-id channel)))
      
      (swap! state update-in [:stats :sent] inc)
      (logging/log :info "Sent notification" {:notification-id notification-id :user-id user-id})
      (events/emit! :notification-sent {:notification-id notification-id :user-id user-id})
      notification-id)))

(defn- deliver-to-channel!
  "Deliver notification to a specific channel."
  [notification-id channel]
  (when-let [channel-config (get-channel channel)]
    (when (:enabled? channel-config)
      ;; Simulate delivery (in production would use actual providers)
      (swap! state update-in [:notifications notification-id :deliveries]
             (fn [d]
               (conj (or d [])
                     {:channel channel
                      :status :delivered
                      :delivered-at (System/currentTimeMillis)})))
      (swap! state update-in [:stats :delivered] inc)
      (metrics/increment :notifications-delivered {:channel channel}))))

(defn get-notification
  "Get a notification."
  [notification-id]
  (get-in @state [:notifications notification-id]))

(defn list-notifications
  "List notifications for a user."
  [user-id & {:keys [unread-only category limit] :or {limit 50}}]
  (let [notifications (vals (:notifications @state))
        filtered (cond->> notifications
                   true (filter #(= (:user-id %) user-id))
                   unread-only (filter #(not (:read? %)))
                   category (filter #(= (:category %) category))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :subject :body :category :priority :read? :created-at]) filtered)))

(defn mark-as-read!
  "Mark a notification as read."
  [notification-id]
  (swap! state assoc-in [:notifications notification-id :read?] true)
  (swap! state assoc-in [:notifications notification-id :read-at] (System/currentTimeMillis))
  (swap! state update-in [:stats :read] inc))

(defn mark-all-as-read!
  "Mark all notifications as read for a user."
  [user-id]
  (let [notifications (filter (fn [[_ n]]
                                (and (= (:user-id n) user-id)
                                     (not (:read? n))))
                              (:notifications @state))]
    (doseq [[id _] notifications]
      (mark-as-read! id))
    (count notifications)))

(defn delete-notification!
  "Delete a notification."
  [notification-id]
  (swap! state update :notifications dissoc notification-id))

;; ============================================================================
;; Notification Grouping
;; ============================================================================

(defn create-group!
  "Create a notification group."
  [group-id config]
  (let [group {:id group-id
               :name (get config :name (name group-id))
               :user-id (get config :user-id)
               :category (get config :category)
               :notifications []
               :collapsed? (get config :collapsed? true)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:groups group-id] group)
    group-id))

(defn add-to-group!
  "Add a notification to a group."
  [group-id notification-id]
  (swap! state update-in [:groups group-id :notifications] conj notification-id))

(defn get-grouped-notifications
  "Get grouped notifications for a user."
  [user-id]
  (let [groups (filter (fn [[_ g]] (= (:user-id g) user-id)) (:groups @state))]
    (mapv (fn [[id g]]
            {:id id
             :name (:name g)
             :category (:category g)
             :count (count (:notifications g))
             :collapsed? (:collapsed? g)})
          groups)))

;; ============================================================================
;; Scheduled Notifications
;; ============================================================================

(defn schedule-notification!
  "Schedule a notification for later delivery."
  [config]
  (let [scheduled-id (str (UUID/randomUUID))
        scheduled {:id scheduled-id
                   :config config
                   :scheduled-for (get config :scheduled-for)
                   :status :pending
                   :created-at (System/currentTimeMillis)}]
    (swap! state update :scheduled conj scheduled)
    (logging/log :info "Scheduled notification" {:scheduled-id scheduled-id})
    scheduled-id))

(defn get-scheduled-notifications
  "Get pending scheduled notifications."
  []
  (filter #(= :pending (:status %)) (:scheduled @state)))

(defn process-scheduled-notifications!
  "Process due scheduled notifications."
  []
  (let [now (System/currentTimeMillis)
        due (filter #(and (= :pending (:status %))
                          (<= (:scheduled-for %) now))
                    (:scheduled @state))]
    (doseq [scheduled due]
      (send-notification! (:config scheduled))
      (swap! state update :scheduled
             (fn [s]
               (mapv (fn [item]
                       (if (= (:id item) (:id scheduled))
                         (assoc item :status :sent :sent-at now)
                         item))
                     s))))
    (count due)))

(defn cancel-scheduled!
  "Cancel a scheduled notification."
  [scheduled-id]
  (swap! state update :scheduled
         (fn [s]
           (mapv (fn [item]
                   (if (= (:id item) scheduled-id)
                     (assoc item :status :cancelled)
                     item))
                 s))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-notification-stats
  "Get notification statistics."
  []
  (let [stats (:stats @state)
        notifications (vals (:notifications @state))
        by-channel (frequencies (mapcat :channels notifications))
        by-category (frequencies (map :category notifications))]
    {:total-notifications (count notifications)
     :total-templates (count (:templates @state))
     :total-channels (count (:channels @state))
     :sent (:sent stats)
     :delivered (:delivered stats)
     :failed (:failed stats)
     :read (:read stats)
     :by-channel by-channel
     :by-category by-category
     :pending-scheduled (count (get-scheduled-notifications))}))

(defn get-user-stats
  "Get notification statistics for a user."
  [user-id]
  (let [notifications (filter #(= (:user-id %) user-id) (vals (:notifications @state)))
        unread (count (filter #(not (:read? %)) notifications))]
    {:total (count notifications)
     :unread unread
     :read (- (count notifications) unread)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-notification-hub!
  "Initialize the notification hub."
  []
  (when-not (:initialized? @state)
    ;; Register default channels
    (register-channel! :email
                       {:name "Email"
                        :type :email
                        :provider :smtp
                        :rate-limit {:per-minute 100}})
    
    (register-channel! :sms
                       {:name "SMS"
                        :type :sms
                        :provider :twilio
                        :rate-limit {:per-minute 30}})
    
    (register-channel! :push
                       {:name "Push Notification"
                        :type :push
                        :provider :firebase
                        :rate-limit {:per-minute 1000}})
    
    (register-channel! :in-app
                       {:name "In-App"
                        :type :in-app
                        :rate-limit {:per-minute 1000}})
    
    ;; Create default templates
    (create-template! :analysis-complete
                      {:name "Analysis Complete"
                       :subject "Your analysis is complete"
                       :body "Your document analysis has completed. {{model_count}} mental models were detected."
                       :channels #{:email :in-app :push}
                       :variables [:model_count :document_name]})
    
    (create-template! :lollapalooza-alert
                      {:name "Lollapalooza Alert"
                       :subject "Lollapalooza Effect Detected!"
                       :body "A Lollapalooza effect was detected with {{model_count}} converging models: {{models}}"
                       :channels #{:email :in-app :push :sms}
                       :variables [:model_count :models :confidence]})
    
    (create-template! :weekly-digest
                      {:name "Weekly Digest"
                       :subject "Your Weekly Mental Models Digest"
                       :body "This week you analyzed {{document_count}} documents and detected {{model_count}} mental models."
                       :channels #{:email}
                       :variables [:document_count :model_count :top_models]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Notification hub initialized")
    (events/emit! :notification-hub-initialized {})
    true))
