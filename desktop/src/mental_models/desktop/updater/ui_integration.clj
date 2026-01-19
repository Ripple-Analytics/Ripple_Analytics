(ns mental-models.desktop.updater.ui-integration
  "UI Integration Module - User interface for update management
   
   Covers Category 16: UI Integration
   - 'Check for Updates' button functionality
   - Progress bar with accurate percentage
   - Desktop notifications
   - 'Restart to Apply' prompt
   - Update status panel
   - Last update time display
   - Update history view"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util UUID]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ui-config
  (atom {:show-notifications true
         :auto-check-on-startup true
         :show-progress-in-taskbar true
         :minimize-to-tray-during-update false
         :notification-duration-ms 5000
         :progress-update-interval-ms 100}))

;; =============================================================================
;; UI State
;; =============================================================================

(defonce ui-state
  (atom {:update-status :idle          ;; :idle, :checking, :available, :downloading, :extracting, :ready, :applying, :error
         :current-version nil
         :available-version nil
         :download-progress 0          ;; 0-100
         :download-speed-bps 0
         :estimated-time-remaining-ms nil
         :last-check-time nil
         :last-update-time nil
         :error-message nil
         :user-friendly-error nil
         :restart-required false
         :update-history []
         :notifications-queue []}))

(defonce ui-event-channel (chan 100))
(defonce ui-listeners (atom #{}))

;; =============================================================================
;; Event System
;; =============================================================================

(defn register-ui-listener!
  "Register a listener for UI events"
  [listener-fn]
  (swap! ui-listeners conj listener-fn)
  (log/debug "Registered UI listener"))

(defn unregister-ui-listener!
  "Unregister a UI listener"
  [listener-fn]
  (swap! ui-listeners disj listener-fn))

(defn emit-ui-event!
  "Emit a UI event to all listeners"
  [event-type data]
  (let [event {:type event-type
               :data data
               :timestamp (str (Instant/now))}]
    (async/put! ui-event-channel event)
    (doseq [listener @ui-listeners]
      (try
        (listener event)
        (catch Exception e
          (log/warn "UI listener error:" (.getMessage e)))))))

(defn start-event-dispatcher!
  "Start the UI event dispatcher"
  []
  (go-loop []
    (when-let [event (<! ui-event-channel)]
      (log/debug "UI event:" (:type event))
      (recur))))

;; =============================================================================
;; Status Updates
;; =============================================================================

(defn set-status!
  "Update the UI status"
  [status & {:keys [message error]}]
  (swap! ui-state assoc :update-status status)
  (when message
    (swap! ui-state assoc :error-message message))
  (when error
    (swap! ui-state assoc :user-friendly-error error))
  (emit-ui-event! :status-changed {:status status :message message}))

(defn set-progress!
  "Update download/extraction progress"
  [progress & {:keys [speed eta]}]
  (swap! ui-state assoc 
         :download-progress (min 100 (max 0 progress))
         :download-speed-bps (or speed 0)
         :estimated-time-remaining-ms eta)
  (emit-ui-event! :progress-updated {:progress progress :speed speed :eta eta}))

(defn set-versions!
  "Update version information"
  [current available]
  (swap! ui-state assoc 
         :current-version current
         :available-version available)
  (emit-ui-event! :versions-updated {:current current :available available}))

(defn set-restart-required!
  "Mark that a restart is required to apply update"
  [required]
  (swap! ui-state assoc :restart-required required)
  (when required
    (emit-ui-event! :restart-required {})))

;; =============================================================================
;; Check for Updates Button
;; =============================================================================

(defn check-for-updates!
  "Handle 'Check for Updates' button click
   
   Returns a channel that will receive the result"
  [check-fn]
  (let [result-chan (chan 1)]
    (go
      (set-status! :checking)
      (swap! ui-state assoc :last-check-time (str (Instant/now)))
      (emit-ui-event! :check-started {})
      
      (try
        (let [result (<! (check-fn))]
          (if (:update-available result)
            (do
              (set-status! :available)
              (set-versions! (:current-version result) (:available-version result))
              (emit-ui-event! :update-available {:version (:available-version result)})
              (>! result-chan {:success true :update-available true :version (:available-version result)}))
            (do
              (set-status! :idle)
              (emit-ui-event! :up-to-date {})
              (>! result-chan {:success true :update-available false}))))
        (catch Exception e
          (set-status! :error :message (.getMessage e) :error "Failed to check for updates")
          (emit-ui-event! :check-failed {:error (.getMessage e)})
          (>! result-chan {:success false :error (.getMessage e)})))
      
      (close! result-chan))
    result-chan))

;; =============================================================================
;; Progress Bar
;; =============================================================================

(defn format-speed
  "Format download speed for display"
  [bytes-per-second]
  (cond
    (nil? bytes-per-second) "-- KB/s"
    (< bytes-per-second 1024) (str bytes-per-second " B/s")
    (< bytes-per-second (* 1024 1024)) (format "%.1f KB/s" (/ bytes-per-second 1024.0))
    :else (format "%.1f MB/s" (/ bytes-per-second 1024.0 1024.0))))

(defn format-eta
  "Format estimated time remaining for display"
  [ms]
  (cond
    (nil? ms) "calculating..."
    (< ms 60000) (str (int (/ ms 1000)) " seconds")
    (< ms 3600000) (str (int (/ ms 60000)) " minutes")
    :else (str (int (/ ms 3600000)) " hours")))

(defn format-size
  "Format file size for display"
  [bytes]
  (cond
    (nil? bytes) "-- MB"
    (< bytes 1024) (str bytes " B")
    (< bytes (* 1024 1024)) (format "%.1f KB" (/ bytes 1024.0))
    (< bytes (* 1024 1024 1024)) (format "%.1f MB" (/ bytes 1024.0 1024.0))
    :else (format "%.2f GB" (/ bytes 1024.0 1024.0 1024.0))))

(defn get-progress-display
  "Get formatted progress information for UI"
  []
  (let [state @ui-state]
    {:progress (:download-progress state)
     :progress-text (str (int (:download-progress state)) "%")
     :speed-text (format-speed (:download-speed-bps state))
     :eta-text (format-eta (:estimated-time-remaining-ms state))
     :status (:update-status state)
     :status-text (case (:update-status state)
                    :idle "Ready"
                    :checking "Checking for updates..."
                    :available "Update available"
                    :downloading "Downloading update..."
                    :extracting "Extracting update..."
                    :ready "Ready to install"
                    :applying "Applying update..."
                    :error "Error"
                    "Unknown")}))

;; =============================================================================
;; Desktop Notifications
;; =============================================================================

(defn create-notification
  "Create a notification object"
  [type title message & {:keys [actions icon]}]
  {:id (str (UUID/randomUUID))
   :type type
   :title title
   :message message
   :actions (or actions [])
   :icon icon
   :timestamp (str (Instant/now))
   :read false})

(defn queue-notification!
  "Queue a notification for display"
  [notification]
  (when (:show-notifications @ui-config)
    (swap! ui-state update :notifications-queue conj notification)
    (emit-ui-event! :notification-queued notification)))

(defn show-update-available-notification!
  "Show notification that an update is available"
  [version]
  (queue-notification!
    (create-notification
      :update-available
      "Update Available"
      (str "Version " version " is available. Click to download.")
      :actions [{:id :download :label "Download Now"}
                {:id :later :label "Later"}])))

(defn show-update-ready-notification!
  "Show notification that update is ready to install"
  []
  (queue-notification!
    (create-notification
      :update-ready
      "Update Ready"
      "The update has been downloaded and is ready to install."
      :actions [{:id :restart :label "Restart Now"}
                {:id :later :label "Later"}])))

(defn show-update-complete-notification!
  "Show notification that update was applied successfully"
  [version]
  (queue-notification!
    (create-notification
      :update-complete
      "Update Complete"
      (str "Successfully updated to version " version "."))))

(defn show-update-error-notification!
  "Show notification about update error"
  [error-message]
  (queue-notification!
    (create-notification
      :update-error
      "Update Failed"
      error-message
      :actions [{:id :retry :label "Retry"}
                {:id :dismiss :label "Dismiss"}])))

(defn dismiss-notification!
  "Dismiss a notification"
  [notification-id]
  (swap! ui-state update :notifications-queue
         (fn [q] (remove #(= (:id %) notification-id) q))))

(defn get-pending-notifications
  "Get all pending notifications"
  []
  (:notifications-queue @ui-state))

;; =============================================================================
;; Restart to Apply Prompt
;; =============================================================================

(defonce restart-callbacks (atom {:on-restart nil :on-later nil}))

(defn prompt-restart!
  "Show 'Restart to Apply' prompt
   
   Returns a channel that will receive :restart or :later"
  [& {:keys [on-restart on-later]}]
  (let [result-chan (chan 1)]
    (reset! restart-callbacks {:on-restart on-restart :on-later on-later})
    (set-restart-required! true)
    (emit-ui-event! :restart-prompt-shown {})
    result-chan))

(defn handle-restart-response!
  "Handle user response to restart prompt"
  [response]
  (case response
    :restart (do
               (emit-ui-event! :restart-accepted {})
               (when-let [cb (:on-restart @restart-callbacks)]
                 (cb)))
    :later (do
             (emit-ui-event! :restart-postponed {})
             (when-let [cb (:on-later @restart-callbacks)]
               (cb))))
  (reset! restart-callbacks {:on-restart nil :on-later nil}))

;; =============================================================================
;; Update Status Panel
;; =============================================================================

(defn format-timestamp
  "Format a timestamp for display"
  [timestamp]
  (when timestamp
    (try
      (let [instant (Instant/parse timestamp)
            local-dt (LocalDateTime/ofInstant instant (ZoneId/systemDefault))
            formatter (DateTimeFormatter/ofPattern "MMM d, yyyy 'at' h:mm a")]
        (.format local-dt formatter))
      (catch Exception _
        timestamp))))

(defn get-status-panel-data
  "Get data for the update status panel"
  []
  (let [state @ui-state]
    {:current-version (or (:current-version state) "Unknown")
     :available-version (:available-version state)
     :status (:update-status state)
     :status-display (case (:update-status state)
                       :idle "Up to date"
                       :checking "Checking..."
                       :available "Update available"
                       :downloading "Downloading..."
                       :extracting "Installing..."
                       :ready "Ready to restart"
                       :applying "Applying..."
                       :error "Error occurred"
                       "Unknown")
     :last-check (format-timestamp (:last-check-time state))
     :last-update (format-timestamp (:last-update-time state))
     :restart-required (:restart-required state)
     :error-message (:user-friendly-error state)
     :progress (when (#{:downloading :extracting} (:update-status state))
                 (get-progress-display))}))

;; =============================================================================
;; Update History
;; =============================================================================

(defn add-to-history!
  "Add an update event to history"
  [event-type version & {:keys [success error]}]
  (let [entry {:id (str (UUID/randomUUID))
               :type event-type
               :version version
               :timestamp (str (Instant/now))
               :success (if (nil? success) true success)
               :error error}]
    (swap! ui-state update :update-history
           (fn [history]
             (take 50 (cons entry history))))
    entry))

(defn get-update-history
  "Get update history for display"
  [& {:keys [limit] :or {limit 10}}]
  (let [history (:update-history @ui-state)]
    (->> history
         (take limit)
         (map (fn [entry]
                (assoc entry
                       :timestamp-display (format-timestamp (:timestamp entry))
                       :type-display (case (:type entry)
                                       :check "Checked for updates"
                                       :download "Downloaded update"
                                       :install "Installed update"
                                       :rollback "Rolled back"
                                       "Unknown")))))))

;; =============================================================================
;; Download Flow Integration
;; =============================================================================

(defn start-download-ui!
  "Start download with UI updates"
  [download-fn version]
  (let [result-chan (chan 1)
        start-time (System/currentTimeMillis)
        last-progress (atom 0)
        last-time (atom start-time)]
    
    (set-status! :downloading)
    (set-progress! 0)
    (emit-ui-event! :download-started {:version version})
    
    (go
      (try
        ;; Start download with progress callback
        (let [result (<! (download-fn
                           (fn [progress total]
                             (let [now (System/currentTimeMillis)
                                   elapsed (- now @last-time)
                                   bytes-delta (- progress @last-progress)
                                   speed (if (> elapsed 0)
                                           (int (* 1000 (/ bytes-delta elapsed)))
                                           0)
                                   percent (if (> total 0)
                                             (* 100 (/ progress total))
                                             0)
                                   remaining-bytes (- total progress)
                                   eta (if (> speed 0)
                                         (* 1000 (/ remaining-bytes speed))
                                         nil)]
                               (reset! last-progress progress)
                               (reset! last-time now)
                               (set-progress! percent :speed speed :eta eta)))))]
          
          (if (:success result)
            (do
              (set-status! :ready)
              (set-progress! 100)
              (add-to-history! :download version :success true)
              (show-update-ready-notification!)
              (emit-ui-event! :download-complete {:version version})
              (>! result-chan {:success true}))
            (do
              (set-status! :error :error (:error result))
              (add-to-history! :download version :success false :error (:error result))
              (show-update-error-notification! (or (:user-error result) "Download failed"))
              (emit-ui-event! :download-failed {:error (:error result)})
              (>! result-chan {:success false :error (:error result)}))))
        
        (catch Exception e
          (set-status! :error :message (.getMessage e) :error "Download failed")
          (emit-ui-event! :download-failed {:error (.getMessage e)})
          (>! result-chan {:success false :error (.getMessage e)})))
      
      (close! result-chan))
    
    result-chan))

;; =============================================================================
;; Installation Flow Integration
;; =============================================================================

(defn start-install-ui!
  "Start installation with UI updates"
  [install-fn version]
  (let [result-chan (chan 1)]
    
    (set-status! :applying)
    (emit-ui-event! :install-started {:version version})
    
    (go
      (try
        (let [result (<! (install-fn))]
          (if (:success result)
            (do
              (set-status! :idle)
              (swap! ui-state assoc 
                     :current-version version
                     :available-version nil
                     :last-update-time (str (Instant/now)))
              (add-to-history! :install version :success true)
              (show-update-complete-notification! version)
              (emit-ui-event! :install-complete {:version version})
              (>! result-chan {:success true}))
            (do
              (set-status! :error :error (:error result))
              (add-to-history! :install version :success false :error (:error result))
              (show-update-error-notification! (or (:user-error result) "Installation failed"))
              (emit-ui-event! :install-failed {:error (:error result)})
              (>! result-chan {:success false :error (:error result)}))))
        
        (catch Exception e
          (set-status! :error :message (.getMessage e) :error "Installation failed")
          (emit-ui-event! :install-failed {:error (.getMessage e)})
          (>! result-chan {:success false :error (.getMessage e)})))
      
      (close! result-chan))
    
    result-chan))

;; =============================================================================
;; UI State Getters
;; =============================================================================

(defn get-ui-state
  "Get the full UI state"
  []
  @ui-state)

(defn get-update-status
  "Get current update status"
  []
  (:update-status @ui-state))

(defn is-update-available?
  "Check if an update is available"
  []
  (= :available (:update-status @ui-state)))

(defn is-restart-required?
  "Check if restart is required"
  []
  (:restart-required @ui-state))

(defn is-busy?
  "Check if update system is busy"
  []
  (#{:checking :downloading :extracting :applying} (:update-status @ui-state)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-ui!
  "Initialize the UI integration module"
  [& {:keys [current-version]}]
  (log/info "Initializing update UI integration")
  (when current-version
    (swap! ui-state assoc :current-version current-version))
  (start-event-dispatcher!)
  (emit-ui-event! :ui-initialized {:version current-version})
  {:success true})

(defn reset-ui-state!
  "Reset UI state to defaults"
  []
  (reset! ui-state
          {:update-status :idle
           :current-version (:current-version @ui-state)
           :available-version nil
           :download-progress 0
           :download-speed-bps 0
           :estimated-time-remaining-ms nil
           :last-check-time (:last-check-time @ui-state)
           :last-update-time (:last-update-time @ui-state)
           :error-message nil
           :user-friendly-error nil
           :restart-required false
           :update-history (:update-history @ui-state)
           :notifications-queue []})
  (emit-ui-event! :ui-reset {}))
