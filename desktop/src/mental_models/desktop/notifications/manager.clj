(ns mental-models.desktop.notifications.manager
  "Desktop notifications management for Mental Models app.
   Handles desktop notifications, notification preferences,
   history tracking, and integration with system tray."
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [mental-models.desktop.api.web-client :as api]
            [mental-models.desktop.settings.sync :as settings])
  (:import [java.awt SystemTray TrayIcon TrayIcon$MessageType Toolkit]
           [java.time Instant Duration]
           [javax.sound.sampled AudioSystem Clip]))

;; =============================================================================
;; Notification Storage
;; =============================================================================

(def notifications-dir (str (System/getProperty "user.home") "/.mental-models"))
(def notifications-file (str notifications-dir "/notifications.json"))
(def max-notification-history 500)

(defn ensure-notifications-dir! []
  (let [dir (io/file notifications-dir)]
    (when-not (.exists dir)
      (.mkdirs dir))))

;; =============================================================================
;; Notification State
;; =============================================================================

(def notification-state (atom {:notifications []
                               :unread-count 0
                               :do-not-disturb false
                               :dnd-until nil
                               :snoozed {}
                               :last-shown nil}))

;; =============================================================================
;; Notification Types
;; =============================================================================

(def notification-types
  {:high-risk {:title "High Risk Alert"
               :icon "warning"
               :priority :high
               :sound true
               :default-enabled true}
   :lollapalooza {:title "Lollapalooza Detected"
                  :icon "alert"
                  :priority :high
                  :sound true
                  :default-enabled true}
   :sync-complete {:title "Sync Complete"
                   :icon "sync"
                   :priority :low
                   :sound false
                   :default-enabled true}
   :scan-complete {:title "Scan Complete"
                   :icon "check"
                   :priority :normal
                   :sound false
                   :default-enabled true}
   :new-signal {:title "New Signal"
                :icon "signal"
                :priority :normal
                :sound true
                :default-enabled true}
   :decision-reminder {:title "Decision Reminder"
                       :icon "clock"
                       :priority :normal
                       :sound true
                       :default-enabled true}
   :update-available {:title "Update Available"
                      :icon "download"
                      :priority :low
                      :sound false
                      :default-enabled true}
   :connection-lost {:title "Connection Lost"
                     :icon "disconnect"
                     :priority :high
                     :sound true
                     :default-enabled true}
   :connection-restored {:title "Connection Restored"
                         :icon "connect"
                         :priority :normal
                         :sound false
                         :default-enabled true}})

;; =============================================================================
;; Notification History Persistence
;; =============================================================================

(defn load-notifications!
  "Load notification history from disk"
  []
  (ensure-notifications-dir!)
  (try
    (when (.exists (io/file notifications-file))
      (let [data (json/read-str (slurp notifications-file) :key-fn keyword)]
        (swap! notification-state merge data)))
    (catch Exception e
      (println "Error loading notifications:" (.getMessage e)))))

(defn save-notifications!
  "Save notification history to disk"
  []
  (ensure-notifications-dir!)
  (try
    (let [state @notification-state
          ;; Keep only recent notifications
          trimmed (update state :notifications #(take max-notification-history %))]
      (spit notifications-file (json/write-str trimmed)))
    (catch Exception e
      (println "Error saving notifications:" (.getMessage e)))))

;; =============================================================================
;; Do Not Disturb
;; =============================================================================

(defn enable-dnd!
  "Enable Do Not Disturb mode"
  [& {:keys [duration-minutes]}]
  (let [until (when duration-minutes
                (str (.plus (Instant/now) (Duration/ofMinutes duration-minutes))))]
    (swap! notification-state assoc
           :do-not-disturb true
           :dnd-until until)
    (save-notifications!)))

(defn disable-dnd!
  "Disable Do Not Disturb mode"
  []
  (swap! notification-state assoc
         :do-not-disturb false
         :dnd-until nil)
  (save-notifications!))

(defn dnd-active?
  "Check if Do Not Disturb is currently active"
  []
  (let [{:keys [do-not-disturb dnd-until]} @notification-state]
    (cond
      (not do-not-disturb) false
      (nil? dnd-until) true
      :else (let [until (Instant/parse dnd-until)]
              (if (.isAfter (Instant/now) until)
                (do (disable-dnd!) false)
                true)))))

;; =============================================================================
;; Snooze Management
;; =============================================================================

(defn snooze-notification!
  "Snooze a notification for a specified duration"
  [notification-id duration-minutes]
  (let [until (str (.plus (Instant/now) (Duration/ofMinutes duration-minutes)))]
    (swap! notification-state update :snoozed assoc notification-id until)
    (save-notifications!)))

(defn unsnooze-notification!
  "Remove snooze from a notification"
  [notification-id]
  (swap! notification-state update :snoozed dissoc notification-id)
  (save-notifications!))

(defn notification-snoozed?
  "Check if a notification is currently snoozed"
  [notification-id]
  (when-let [until (get-in @notification-state [:snoozed notification-id])]
    (let [until-instant (Instant/parse until)]
      (if (.isAfter (Instant/now) until-instant)
        (do (unsnooze-notification! notification-id) false)
        true))))

;; =============================================================================
;; Sound Alerts
;; =============================================================================

(defn play-notification-sound!
  "Play a notification sound"
  [sound-type]
  (try
    ;; Use system beep as fallback
    (.beep (Toolkit/getDefaultToolkit))
    (catch Exception e
      (println "Error playing notification sound:" (.getMessage e)))))

;; =============================================================================
;; Desktop Notifications (System Tray)
;; =============================================================================

(def tray-icon (atom nil))

(defn init-system-tray!
  "Initialize system tray for notifications"
  []
  (when (SystemTray/isSupported)
    (try
      (let [tray (SystemTray/getSystemTray)
            image (.getImage (Toolkit/getDefaultToolkit) 
                             (io/resource "icons/mental-models.png"))
            icon (TrayIcon. image "Mental Models Desktop")]
        (.setImageAutoSize icon true)
        (.add tray icon)
        (reset! tray-icon icon)
        true)
      (catch Exception e
        (println "Error initializing system tray:" (.getMessage e))
        false))))

(defn show-desktop-notification!
  "Show a desktop notification via system tray"
  [{:keys [title message type priority]}]
  (when-let [icon @tray-icon]
    (try
      (let [msg-type (case priority
                       :high TrayIcon$MessageType/WARNING
                       :low TrayIcon$MessageType/INFO
                       TrayIcon$MessageType/NONE)]
        (.displayMessage icon title message msg-type))
      (catch Exception e
        (println "Error showing desktop notification:" (.getMessage e))))))

;; =============================================================================
;; Notification Creation and Display
;; =============================================================================

(defn should-show-notification?
  "Check if a notification should be shown based on settings and state"
  [notification-type]
  (let [settings (settings/load-settings)
        type-config (get notification-types notification-type)
        enabled-types (or (:notification-types settings) #{})]
    (and (:notifications-enabled settings)
         (not (dnd-active?))
         (or (contains? enabled-types notification-type)
             (:default-enabled type-config)))))

(defn create-notification
  "Create a notification record"
  [type title message & {:keys [data link action]}]
  {:id (str (java.util.UUID/randomUUID))
   :type type
   :title title
   :message message
   :data data
   :link link
   :action action
   :created-at (str (Instant/now))
   :read false
   :dismissed false})

(defn add-notification!
  "Add a notification to history and optionally show it"
  [notification & {:keys [show-desktop play-sound] :or {show-desktop true play-sound true}}]
  (let [type-config (get notification-types (:type notification))
        settings (settings/load-settings)]
    ;; Add to history
    (swap! notification-state update :notifications #(cons notification %))
    (swap! notification-state update :unread-count inc)
    (save-notifications!)
    
    ;; Show desktop notification if enabled
    (when (and show-desktop (should-show-notification? (:type notification)))
      (show-desktop-notification! {:title (:title notification)
                                   :message (:message notification)
                                   :type (:type notification)
                                   :priority (:priority type-config)})
      (swap! notification-state assoc :last-shown (str (Instant/now))))
    
    ;; Play sound if enabled
    (when (and play-sound
               (:notification-sound settings)
               (:sound type-config)
               (should-show-notification? (:type notification)))
      (play-notification-sound! (:type notification)))
    
    notification))

;; =============================================================================
;; Notification Actions
;; =============================================================================

(defn mark-as-read!
  "Mark a notification as read"
  [notification-id]
  (swap! notification-state
         (fn [state]
           (let [notifications (:notifications state)
                 updated (mapv #(if (= (:id %) notification-id)
                                  (assoc % :read true)
                                  %)
                               notifications)
                 unread (count (filter #(not (:read %)) updated))]
             (assoc state
                    :notifications updated
                    :unread-count unread))))
  (save-notifications!))

(defn mark-all-as-read!
  "Mark all notifications as read"
  []
  (swap! notification-state
         (fn [state]
           (let [notifications (:notifications state)
                 updated (mapv #(assoc % :read true) notifications)]
             (assoc state
                    :notifications updated
                    :unread-count 0))))
  (save-notifications!))

(defn dismiss-notification!
  "Dismiss a notification"
  [notification-id]
  (swap! notification-state
         (fn [state]
           (let [notifications (:notifications state)
                 updated (mapv #(if (= (:id %) notification-id)
                                  (assoc % :dismissed true :read true)
                                  %)
                               notifications)
                 unread (count (filter #(and (not (:read %)) (not (:dismissed %))) updated))]
             (assoc state
                    :notifications updated
                    :unread-count unread))))
  (save-notifications!))

(defn clear-all-notifications!
  "Clear all notifications"
  []
  (swap! notification-state assoc
         :notifications []
         :unread-count 0)
  (save-notifications!))

;; =============================================================================
;; Notification Queries
;; =============================================================================

(defn get-notifications
  "Get notifications with optional filters"
  [& {:keys [unread-only type limit] :or {limit 50}}]
  (let [notifications (:notifications @notification-state)]
    (cond->> notifications
      unread-only (filter #(not (:read %)))
      type (filter #(= (:type %) type))
      true (filter #(not (:dismissed %)))
      limit (take limit)
      true vec)))

(defn get-unread-count
  "Get count of unread notifications"
  []
  (:unread-count @notification-state))

(defn get-notification-by-id
  "Get a specific notification by ID"
  [notification-id]
  (first (filter #(= (:id %) notification-id) (:notifications @notification-state))))

;; =============================================================================
;; Convenience Functions for Common Notifications
;; =============================================================================

(defn notify-high-risk!
  "Send a high-risk alert notification"
  [title message & {:keys [data link]}]
  (add-notification! (create-notification :high-risk title message :data data :link link)))

(defn notify-lollapalooza!
  "Send a lollapalooza detection notification"
  [file-path models]
  (add-notification! (create-notification :lollapalooza
                                          "Lollapalooza Effect Detected!"
                                          (str "Found " (count models) " converging mental models in: " 
                                               (.getName (io/file file-path)))
                                          :data {:file-path file-path :models models})))

(defn notify-sync-complete!
  "Send a sync complete notification"
  [synced-count]
  (add-notification! (create-notification :sync-complete
                                          "Sync Complete"
                                          (str "Successfully synced " synced-count " items")
                                          :show-desktop false)))

(defn notify-scan-complete!
  "Send a scan complete notification"
  [files-scanned models-found]
  (add-notification! (create-notification :scan-complete
                                          "Scan Complete"
                                          (str "Scanned " files-scanned " files, found " models-found " mental models"))))

(defn notify-new-signal!
  "Send a new signal notification"
  [signal]
  (add-notification! (create-notification :new-signal
                                          "New Signal Detected"
                                          (:title signal)
                                          :data signal
                                          :link (:url signal))))

(defn notify-decision-reminder!
  "Send a decision reminder notification"
  [decision]
  (add-notification! (create-notification :decision-reminder
                                          "Decision Review Reminder"
                                          (str "Time to review your decision: " (:title decision))
                                          :data decision)))

(defn notify-update-available!
  "Send an update available notification"
  [version]
  (add-notification! (create-notification :update-available
                                          "Update Available"
                                          (str "Version " version " is available for download")
                                          :data {:version version})))

(defn notify-connection-lost!
  "Send a connection lost notification"
  []
  (add-notification! (create-notification :connection-lost
                                          "Connection Lost"
                                          "Lost connection to web app. Working in offline mode.")))

(defn notify-connection-restored!
  "Send a connection restored notification"
  []
  (add-notification! (create-notification :connection-restored
                                          "Connection Restored"
                                          "Reconnected to web app. Syncing pending changes...")))

;; =============================================================================
;; Web App Sync
;; =============================================================================

(defn sync-notification-preferences!
  "Sync notification preferences with web app"
  []
  (when (api/is-online?)
    (try
      (let [settings (settings/load-settings)
            prefs {:notifications-enabled (:notifications-enabled settings)
                   :notification-sound (:notification-sound settings)
                   :notification-types (:notification-types settings)}]
        (api/sync-notification-preferences prefs))
      (catch Exception e
        (println "Error syncing notification preferences:" (.getMessage e))))))

(defn fetch-notification-preferences!
  "Fetch notification preferences from web app"
  []
  (when (api/is-online?)
    (try
      (when-let [prefs (api/get-notification-preferences)]
        (settings/update-setting! :notifications-enabled (:notifications-enabled prefs))
        (settings/update-setting! :notification-sound (:notification-sound prefs))
        (settings/update-setting! :notification-types (set (:notification-types prefs))))
      (catch Exception e
        (println "Error fetching notification preferences:" (.getMessage e))))))

;; =============================================================================
;; Badge Icon
;; =============================================================================

(defn update-badge-icon!
  "Update the system tray badge with unread count"
  []
  (when-let [icon @tray-icon]
    (let [unread (get-unread-count)]
      (try
        (.setToolTip icon (if (pos? unread)
                           (str "Mental Models Desktop (" unread " unread)")
                           "Mental Models Desktop"))
        (catch Exception e
          (println "Error updating badge icon:" (.getMessage e)))))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize the notification system"
  []
  (load-notifications!)
  (init-system-tray!)
  (update-badge-icon!)
  ;; Start a background thread to check for expired DND/snooze
  (future
    (loop []
      (Thread/sleep 60000) ;; Check every minute
      (try
        (dnd-active?) ;; This will auto-disable if expired
        (doseq [[id _] (:snoozed @notification-state)]
          (notification-snoozed? id)) ;; This will auto-unsnooze if expired
        (catch Exception _))
      (recur))))
