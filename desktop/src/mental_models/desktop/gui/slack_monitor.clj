(ns mental-models.desktop.gui.slack-monitor
  "Monitor Slack channel for remote commands (update, scan, status)"
  (:require [mental-models.desktop.gui.app :as app]
            [mental-models.desktop.updater.continuous-deploy :as deploy]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as async :refer [go-loop <! timeout chan close!]])
  (:import [java.time Instant]))

;; =============================================================================
;; Slack API
;; =============================================================================

(defn get-channel-messages [token channel-id & {:keys [limit oldest] :or {limit 10}}]
  "Fetch recent messages from a Slack channel"
  (try
    (let [params (cond-> {:channel channel-id
                          :limit limit}
                   oldest (assoc :oldest oldest))
          response (http/get "https://slack.com/api/conversations.history"
                             {:headers {"Authorization" (str "Bearer " token)}
                              :query-params params
                              :as :json})]
      (when (get-in response [:body :ok])
        (get-in response [:body :messages])))
    (catch Exception e
      (app/add-log! :error (str "Slack API error: " (.getMessage e)))
      nil)))

(defn post-message [webhook-url text]
  "Post a message to Slack via webhook"
  (try
    (http/post webhook-url
               {:content-type :json
                :body (json/generate-string {:text text})})
    (catch Exception _)))

;; =============================================================================
;; Command Parsing
;; =============================================================================

(def command-patterns
  {:update #"(?i)@?mental.?models?\s+(update|upgrade|pull)"
   :scan #"(?i)@?mental.?models?\s+scan\s+(.+)"
   :status #"(?i)@?mental.?models?\s+status"
   :stop #"(?i)@?mental.?models?\s+stop"
   :restart #"(?i)@?mental.?models?\s+restart"})

(defn parse-command [text]
  "Parse a Slack message for commands"
  (some (fn [[cmd pattern]]
          (when-let [match (re-find pattern text)]
            {:command cmd
             :args (when (vector? match) (rest match))}))
        command-patterns))

;; =============================================================================
;; Command Handlers
;; =============================================================================

(defmulti handle-command :command)

(defmethod handle-command :update [{:keys [webhook-url]}]
  (app/add-log! :info "Received remote UPDATE command from Slack")
  (post-message webhook-url "🔄 Starting update check...")
  
  (let [result (deploy/check-and-update!)]
    (if (:updated result)
      (post-message webhook-url (str "✅ Updated to version " (:version result)))
      (post-message webhook-url "ℹ️ Already on latest version"))))

(defmethod handle-command :scan [{:keys [args webhook-url]}]
  (let [folder (first args)]
    (app/add-log! :info (str "Received remote SCAN command for: " folder))
    (post-message webhook-url (str "🔍 Starting scan of: " folder))
    
    ;; Add folder and start scan
    (swap! app/*state update :watched-folders conj folder)
    ;; Trigger scan via event
    (app/event-handler {:event/type ::app/start-scan})))

(defmethod handle-command :status [{:keys [webhook-url]}]
  (app/add-log! :info "Received remote STATUS command")
  (let [stats (:stats @app/*state)
        scan (:scan @app/*state)
        connections (:connections @app/*state)]
    (post-message webhook-url
                  (str "📊 *Mental Models Desktop Status*\n"
                       "• Files scanned: " (:files-scanned stats) "\n"
                       "• Models found: " (:models-found stats) "\n"
                       "• Lollapalooza events: " (:lollapalooza-events stats) "\n"
                       "• Status: " (name (:status scan)) "\n"
                       "• LM Studio: " (name (:lm-studio connections)) "\n"
                       "• Last sync: " (or (:last-sync stats) "Never")))))

(defmethod handle-command :stop [{:keys [webhook-url]}]
  (app/add-log! :info "Received remote STOP command")
  (app/event-handler {:event/type ::app/stop-scan})
  (app/event-handler {:event/type ::app/stop-watch})
  (post-message webhook-url "⏹️ Stopped all operations"))

(defmethod handle-command :restart [{:keys [webhook-url]}]
  (app/add-log! :info "Received remote RESTART command")
  (post-message webhook-url "🔄 Restarting application...")
  ;; In a real implementation, this would trigger a graceful restart
  (deploy/restart-application!))

(defmethod handle-command :default [_]
  nil)

;; =============================================================================
;; Monitor Loop
;; =============================================================================

(def monitor-channel (atom nil))
(def last-message-ts (atom nil))

(defn start-monitor! []
  "Start monitoring Slack for commands"
  (let [settings (:settings @app/*state)
        token (System/getenv "SLACK_BOT_TOKEN")
        channel-id (:slack-channel settings)
        webhook-url (:slack-webhook settings)]
    
    (when (and token (not (clojure.string/blank? channel-id)))
      (app/add-log! :info "Starting Slack command monitor...")
      (app/set-connection! :slack :connected)
      
      (let [ch (chan)]
        (reset! monitor-channel ch)
        
        ;; Poll every 10 seconds
        (go-loop []
          (let [_ (<! (timeout 10000))]
            (when-not (async/poll! ch)  ;; Check if channel is closed
              (when-let [messages (get-channel-messages token channel-id
                                                        :limit 5
                                                        :oldest @last-message-ts)]
                (doseq [msg (reverse messages)]
                  (when-let [cmd (parse-command (:text msg))]
                    (handle-command (assoc cmd :webhook-url webhook-url))))
                
                ;; Update last message timestamp
                (when-let [latest (first messages)]
                  (reset! last-message-ts (:ts latest))))
              (recur))))
        
        (app/add-log! :success "Slack monitor started - listening for commands")))))

(defn stop-monitor! []
  "Stop monitoring Slack"
  (when-let [ch @monitor-channel]
    (close! ch)
    (reset! monitor-channel nil))
  (app/set-connection! :slack :disconnected)
  (app/add-log! :info "Slack monitor stopped"))

;; =============================================================================
;; Notification Helpers
;; =============================================================================

(defn notify-lollapalooza! [file-path models]
  "Send Slack notification when Lollapalooza is detected"
  (let [webhook-url (get-in @app/*state [:settings :slack-webhook])]
    (when-not (clojure.string/blank? webhook-url)
      (post-message webhook-url
                    (str "🎯 *LOLLAPALOOZA DETECTED*\n"
                         "File: `" file-path "`\n"
                         "Models found: " (count models) "\n"
                         (clojure.string/join "\n"
                                              (map #(str "• " (:name %) " (" (int (* 100 (:confidence %))) "%)") 
                                                   models)))))))

(defn notify-scan-complete! [stats]
  "Send Slack notification when scan completes"
  (let [webhook-url (get-in @app/*state [:settings :slack-webhook])]
    (when-not (clojure.string/blank? webhook-url)
      (post-message webhook-url
                    (str "✅ *Scan Complete*\n"
                         "• Files scanned: " (:files-scanned stats) "\n"
                         "• Models found: " (:models-found stats) "\n"
                         "• Lollapalooza events: " (:lollapalooza-events stats))))))
