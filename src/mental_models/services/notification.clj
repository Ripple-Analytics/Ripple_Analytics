(ns mental-models.services.notification
  "Notification Service - Multi-channel alerts and updates
   Supports push notifications, email, webhooks, and in-app notifications"
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]])
  (:import [java.time Instant]
           [java.util UUID]))

;; -- Configuration -----------------------------------------------------------

(def config
  {:webhook-url (System/getenv "NOTIFICATION_WEBHOOK_URL")
   :email-api-key (System/getenv "EMAIL_API_KEY")
   :push-api-key (System/getenv "PUSH_API_KEY")
   :owner-email (System/getenv "OWNER_EMAIL")
   :rate-limit-per-minute 60
   :batch-delay-ms 5000})

;; -- State -------------------------------------------------------------------

(defonce notification-queue (chan 1000))
(defonce notification-history (atom []))
(defonce rate-limiter (atom {:count 0 :reset-at (Instant/now)}))

;; -- Notification Types ------------------------------------------------------

(def notification-types
  {:alert {:priority :high :channels [:push :email :webhook]}
   :warning {:priority :medium :channels [:push :in-app]}
   :info {:priority :low :channels [:in-app]}
   :digest {:priority :low :channels [:email]}
   :system {:priority :high :channels [:webhook :email]}
   :anomaly {:priority :high :channels [:push :email :webhook]}
   :refactor-complete {:priority :medium :channels [:in-app :email]}
   :health-degraded {:priority :high :channels [:push :webhook]}})

;; -- Rate Limiting -----------------------------------------------------------

(defn check-rate-limit!
  "Check and update rate limit, returns true if allowed"
  []
  (let [now (Instant/now)
        state @rate-limiter]
    (if (.isAfter now (:reset-at state))
      (do (reset! rate-limiter {:count 1 :reset-at (.plusSeconds now 60)})
          true)
      (if (< (:count state) (:rate-limit-per-minute config))
        (do (swap! rate-limiter update :count inc)
            true)
        false))))

;; -- Channel Implementations -------------------------------------------------

(defn send-webhook!
  "Send notification via webhook"
  [{:keys [title message type data]}]
  (when-let [url (:webhook-url config)]
    (try
      (http/post url
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string
                         {:event "notification"
                          :type type
                          :title title
                          :message message
                          :data data
                          :timestamp (str (Instant/now))})
                  :timeout 5000})
      {:success true :channel :webhook}
      (catch Exception e
        (log/error "Webhook failed:" (.getMessage e))
        {:success false :channel :webhook :error (.getMessage e)}))))

(defn send-email!
  "Send notification via email (using generic HTTP API)"
  [{:keys [title message type recipient]}]
  (let [to (or recipient (:owner-email config))]
    (when (and (:email-api-key config) to)
      (try
        ;; Generic email API call - would be configured for actual provider
        (log/info "Email notification:" title "to" to)
        {:success true :channel :email :to to}
        (catch Exception e
          (log/error "Email failed:" (.getMessage e))
          {:success false :channel :email :error (.getMessage e)})))))

(defn send-push!
  "Send push notification"
  [{:keys [title message type]}]
  (when (:push-api-key config)
    (try
      ;; Generic push notification - would integrate with FCM/APNS
      (log/info "Push notification:" title)
      {:success true :channel :push}
      (catch Exception e
        (log/error "Push failed:" (.getMessage e))
        {:success false :channel :push :error (.getMessage e)}))))

(defn store-in-app!
  "Store notification for in-app display"
  [{:keys [id title message type data created-at] :as notification}]
  (swap! notification-history conj
         {:id id
          :title title
          :message message
          :type type
          :data data
          :created-at created-at
          :read false})
  {:success true :channel :in-app})

;; -- Notification Processing -------------------------------------------------

(defn process-notification!
  "Process a single notification through all configured channels"
  [{:keys [type] :as notification}]
  (let [type-config (get notification-types type {:priority :low :channels [:in-app]})
        channels (:channels type-config)
        results (for [channel channels]
                  (case channel
                    :webhook (send-webhook! notification)
                    :email (send-email! notification)
                    :push (send-push! notification)
                    :in-app (store-in-app! notification)
                    nil))]
    {:notification notification
     :results (remove nil? results)
     :success (some :success results)}))

;; -- Queue Processing --------------------------------------------------------

(defn start-notification-processor!
  "Start background processor for notification queue"
  []
  (go-loop []
    (when-let [notification (<! notification-queue)]
      (when (check-rate-limit!)
        (try
          (process-notification! notification)
          (catch Exception e
            (log/error "Notification processing error:" (.getMessage e)))))
      (recur))))

;; -- Public API --------------------------------------------------------------

(defn notify!
  "Send a notification (async)"
  [type title message & {:keys [data recipient]}]
  (let [notification {:id (str (UUID/randomUUID))
                      :type type
                      :title title
                      :message message
                      :data data
                      :recipient recipient
                      :created-at (Instant/now)}]
    (go (>! notification-queue notification))
    notification))

(defn notify-sync!
  "Send a notification (sync)"
  [type title message & {:keys [data recipient]}]
  (let [notification {:id (str (UUID/randomUUID))
                      :type type
                      :title title
                      :message message
                      :data data
                      :recipient recipient
                      :created-at (Instant/now)}]
    (when (check-rate-limit!)
      (process-notification! notification))))

;; -- Convenience Functions ---------------------------------------------------

(defn alert!
  "Send high-priority alert"
  [title message & {:keys [data]}]
  (notify! :alert title message :data data))

(defn warn!
  "Send warning notification"
  [title message & {:keys [data]}]
  (notify! :warning title message :data data))

(defn info!
  "Send info notification"
  [title message & {:keys [data]}]
  (notify! :info title message :data data))

(defn anomaly-detected!
  "Send anomaly detection alert"
  [anomaly-type details]
  (notify! :anomaly
           (str "Anomaly Detected: " anomaly-type)
           (str "An anomaly has been detected in the system.")
           :data details))

(defn refactor-complete!
  "Send refactoring completion notification"
  [summary]
  (notify! :refactor-complete
           "Refactoring Complete"
           (str "Autonomous refactoring completed. "
                (:tangles-fixed summary 0) " tangles fixed.")
           :data summary))

(defn health-degraded!
  "Send health degradation alert"
  [health-score previous-score]
  (notify! :health-degraded
           "Codebase Health Degraded"
           (str "Health score dropped from " previous-score " to " health-score)
           :data {:current health-score :previous previous-score}))

;; -- In-App Notification Management ------------------------------------------

(defn get-unread-notifications
  "Get all unread in-app notifications"
  []
  (filter (complement :read) @notification-history))

(defn get-notifications
  "Get notifications with optional filters"
  [& {:keys [limit type unread-only]}]
  (cond->> @notification-history
    type (filter #(= type (:type %)))
    unread-only (filter (complement :read))
    limit (take limit)))

(defn mark-read!
  "Mark a notification as read"
  [notification-id]
  (swap! notification-history
         (fn [history]
           (mapv (fn [n]
                   (if (= notification-id (:id n))
                     (assoc n :read true)
                     n))
                 history))))

(defn mark-all-read!
  "Mark all notifications as read"
  []
  (swap! notification-history
         (fn [history]
           (mapv #(assoc % :read true) history))))

(defn clear-old-notifications!
  "Clear notifications older than specified hours"
  [hours]
  (let [cutoff (.minusSeconds (Instant/now) (* hours 3600))]
    (swap! notification-history
           (fn [history]
             (filterv #(.isAfter (:created-at %) cutoff) history)))))

;; -- Digest Generation -------------------------------------------------------

(defn generate-daily-digest
  "Generate a daily digest of notifications"
  []
  (let [yesterday (.minusSeconds (Instant/now) 86400)
        recent (filter #(.isAfter (:created-at %) yesterday) @notification-history)
        by-type (group-by :type recent)]
    {:period "daily"
     :generated-at (Instant/now)
     :total-count (count recent)
     :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :highlights (take 5 (sort-by :created-at > recent))}))

(defn send-daily-digest!
  "Send daily digest email"
  []
  (let [digest (generate-daily-digest)]
    (notify! :digest
             "Daily Notification Digest"
             (str "You had " (:total-count digest) " notifications in the last 24 hours.")
             :data digest)))

;; -- Initialization ----------------------------------------------------------

(defn init!
  "Initialize notification service"
  []
  (log/info "Initializing notification service...")
  (start-notification-processor!)
  (log/info "Notification service initialized"))
