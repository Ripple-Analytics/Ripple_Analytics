(ns mental-models.alerts.lollapalooza
  "Real-time Lollapalooza Alert System
   
   Detects when 3+ mental models converge with high confidence (>70%)
   This indicates a potential 'Lollapalooza' effect - multiple psychological
   forces combining to create extreme outcomes.
   
   Alert channels:
   - Desktop notifications (CLJFX)
   - Email alerts
   - Slack notifications
   - Web app real-time updates (Electric)
   - SMS for critical alerts"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop chan >! <! put! close!]]
   #?(:clj [clj-http.client :as http])
   #?(:clj [clojure.java.io :as io])))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def ^:dynamic *alert-config*
  {:lollapalooza-threshold 0.7
   :min-converging-models 3
   :critical-threshold 0.85
   :critical-min-models 5
   :alert-channels #{:desktop :web :slack}
   :cooldown-minutes 5  ; Don't spam same alert
   :slack-webhook-url nil
   :email-smtp nil
   :sms-api nil})

;; =============================================================================
;; ALERT STATE
;; =============================================================================

(def alert-history (atom []))
(def recent-alerts (atom {}))  ; Track to prevent spam
(def alert-channel (chan 100))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn calculate-lollapalooza-score
  "Calculate overall Lollapalooza severity score"
  [converging-models]
  (let [scores (map :score converging-models)
        count-factor (min 2.0 (/ (count converging-models) 3))
        avg-score (/ (reduce + scores) (count scores))
        max-score (apply max scores)]
    {:severity (* count-factor avg-score)
     :avg-confidence avg-score
     :max-confidence max-score
     :model-count (count converging-models)
     :categories (distinct (map :category converging-models))}))

(defn detect-lollapalooza
  "Detect Lollapalooza effect in classification results"
  [classification-result]
  (let [threshold (:lollapalooza-threshold *alert-config*)
        min-models (:min-converging-models *alert-config*)
        high-scoring (filter #(>= (:score %) threshold) 
                             (:detected-models classification-result))
        is-lollapalooza (>= (count high-scoring) min-models)
        is-critical (and is-lollapalooza
                         (>= (count high-scoring) (:critical-min-models *alert-config*))
                         (some #(>= (:score %) (:critical-threshold *alert-config*)) 
                               high-scoring))]
    (when is-lollapalooza
      (let [score-info (calculate-lollapalooza-score high-scoring)]
        {:lollapalooza? true
         :critical? is-critical
         :converging-models high-scoring
         :severity (:severity score-info)
         :avg-confidence (:avg-confidence score-info)
         :max-confidence (:max-confidence score-info)
         :model-count (:model-count score-info)
         :categories (:categories score-info)
         :timestamp (System/currentTimeMillis)}))))

;; =============================================================================
;; ALERT FORMATTING
;; =============================================================================

(defn format-alert-text
  "Format alert for text-based channels (Slack, email, SMS)"
  [lollapalooza-result source-info]
  (let [{:keys [critical? converging-models severity categories]} lollapalooza-result
        level (if critical? "🚨 CRITICAL" "⚠️ WARNING")]
    (str level " LOLLAPALOOZA DETECTED\n"
         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
         "Source: " (:name source-info) "\n"
         "Severity: " (format "%.1f" (* 100 severity)) "%\n"
         "Models Converging: " (count converging-models) "\n"
         "Categories: " (str/join ", " categories) "\n"
         "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
         "TOP CONVERGING MODELS:\n"
         (str/join "\n" 
                   (for [m (take 5 (sort-by :score > converging-models))]
                     (str "• " (:name m) " (" (format "%.0f%%" (* 100 (:score m))) ")")))
         "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
         "RECOMMENDED ACTIONS:\n"
         "1. Review source document carefully\n"
         "2. Apply inversion - what could go wrong?\n"
         "3. Seek disconfirming evidence\n"
         "4. Consult checklist for each model\n"
         "5. Consider waiting before acting")))

(defn format-alert-html
  "Format alert for HTML channels (email, web)"
  [lollapalooza-result source-info]
  (let [{:keys [critical? converging-models severity categories]} lollapalooza-result
        color (if critical? "#dc2626" "#f59e0b")]
    (str "<div style='border-left: 4px solid " color "; padding: 16px; margin: 16px 0;'>"
         "<h2 style='color: " color "; margin: 0;'>"
         (if critical? "🚨 CRITICAL LOLLAPALOOZA" "⚠️ LOLLAPALOOZA WARNING")
         "</h2>"
         "<p><strong>Source:</strong> " (:name source-info) "</p>"
         "<p><strong>Severity:</strong> " (format "%.1f%%" (* 100 severity)) "</p>"
         "<p><strong>Models:</strong> " (count converging-models) " converging</p>"
         "<p><strong>Categories:</strong> " (str/join ", " categories) "</p>"
         "<h3>Converging Models:</h3>"
         "<ul>"
         (str/join "" 
                   (for [m (take 5 (sort-by :score > converging-models))]
                     (str "<li>" (:name m) " - " (format "%.0f%%" (* 100 (:score m))) "</li>")))
         "</ul>"
         "</div>")))

;; =============================================================================
;; ALERT CHANNELS
;; =============================================================================

#?(:clj
   (defn send-slack-alert
     "Send alert to Slack webhook"
     [lollapalooza-result source-info]
     (when-let [webhook-url (:slack-webhook-url *alert-config*)]
       (try
         (http/post webhook-url
                    {:content-type :json
                     :body (str "{\"text\": \"" 
                                (str/escape (format-alert-text lollapalooza-result source-info)
                                            {\" "\\\"" \newline "\\n"})
                                "\"}")})
         (catch Exception e
           (println "[ALERT] Slack error:" (.getMessage e)))))))

#?(:clj
   (defn send-email-alert
     "Send alert via email"
     [lollapalooza-result source-info recipients]
     (when-let [smtp-config (:email-smtp *alert-config*)]
       ;; Email sending implementation
       (println "[ALERT] Email would be sent to:" recipients))))

#?(:clj
   (defn send-sms-alert
     "Send SMS for critical alerts"
     [lollapalooza-result source-info phone-numbers]
     (when (and (:critical? lollapalooza-result)
                (:sms-api *alert-config*))
       ;; SMS sending implementation
       (println "[ALERT] SMS would be sent to:" phone-numbers))))

;; =============================================================================
;; DESKTOP NOTIFICATIONS (CLJFX)
;; =============================================================================

#?(:clj
   (defn show-desktop-notification
     "Show desktop notification using Java AWT"
     [lollapalooza-result source-info]
     (try
       (when (java.awt.SystemTray/isSupported)
         (let [tray (java.awt.SystemTray/getSystemTray)
               image (java.awt.Toolkit/getDefaultToolkit)
               icon (java.awt.TrayIcon. 
                     (.createImage image "icon.png")
                     "Mental Models Alert")]
           (.add tray icon)
           (.displayMessage icon
                            (if (:critical? lollapalooza-result)
                              "🚨 CRITICAL LOLLAPALOOZA"
                              "⚠️ Lollapalooza Detected")
                            (str (:name source-info) "\n"
                                 (count (:converging-models lollapalooza-result))
                                 " models converging")
                            (if (:critical? lollapalooza-result)
                              java.awt.TrayIcon$MessageType/ERROR
                              java.awt.TrayIcon$MessageType/WARNING))))
       (catch Exception e
         (println "[ALERT] Desktop notification error:" (.getMessage e))))))

;; =============================================================================
;; ELECTRIC REACTIVE ALERTS (Web App)
;; =============================================================================

(def !alerts (atom []))  ; Reactive atom for Electric

(defn push-web-alert
  "Push alert to web app via Electric reactive atom"
  [lollapalooza-result source-info]
  (let [alert {:id (str (java.util.UUID/randomUUID))
               :type (if (:critical? lollapalooza-result) :critical :warning)
               :lollapalooza lollapalooza-result
               :source source-info
               :timestamp (System/currentTimeMillis)
               :read? false}]
    (swap! !alerts conj alert)
    ;; Keep only last 100 alerts
    (swap! !alerts #(vec (take-last 100 %)))))

;; =============================================================================
;; ALERT DEDUPLICATION
;; =============================================================================

(defn should-alert?
  "Check if we should send this alert (avoid spam)"
  [lollapalooza-result source-info]
  (let [alert-key (str (:name source-info) "-" 
                       (str/join "-" (map :id (:converging-models lollapalooza-result))))
        cooldown-ms (* 60 1000 (:cooldown-minutes *alert-config*))
        last-alert-time (get @recent-alerts alert-key 0)
        now (System/currentTimeMillis)]
    (when (> (- now last-alert-time) cooldown-ms)
      (swap! recent-alerts assoc alert-key now)
      true)))

;; =============================================================================
;; MAIN ALERT DISPATCHER
;; =============================================================================

(defn dispatch-alert!
  "Dispatch alert to all configured channels"
  [lollapalooza-result source-info]
  (when (should-alert? lollapalooza-result source-info)
    ;; Log to history
    (swap! alert-history conj 
           {:lollapalooza lollapalooza-result
            :source source-info
            :timestamp (System/currentTimeMillis)})
    
    ;; Dispatch to channels
    (let [channels (:alert-channels *alert-config*)]
      (when (:desktop channels)
        #?(:clj (show-desktop-notification lollapalooza-result source-info)))
      
      (when (:web channels)
        (push-web-alert lollapalooza-result source-info))
      
      (when (:slack channels)
        #?(:clj (send-slack-alert lollapalooza-result source-info)))
      
      (when (and (:email channels) (:critical? lollapalooza-result))
        #?(:clj (send-email-alert lollapalooza-result source-info ["alerts@example.com"])))
      
      (when (and (:sms channels) (:critical? lollapalooza-result))
        #?(:clj (send-sms-alert lollapalooza-result source-info ["+1234567890"]))))
    
    ;; Put on async channel for processing
    (put! alert-channel {:lollapalooza lollapalooza-result :source source-info})))

;; =============================================================================
;; ALERT PROCESSING LOOP
;; =============================================================================

(defn start-alert-processor!
  "Start background alert processing loop"
  []
  (go-loop []
    (when-let [alert (<! alert-channel)]
      (println "[ALERT] Processing:" (:name (:source alert)))
      ;; Additional processing could happen here
      ;; - Store to database
      ;; - Trigger workflows
      ;; - Update dashboards
      (recur))))

;; =============================================================================
;; ALERT QUERIES
;; =============================================================================

(defn get-recent-alerts
  "Get recent alerts, optionally filtered"
  [& {:keys [limit critical-only?]
      :or {limit 50 critical-only? false}}]
  (let [alerts @alert-history
        filtered (if critical-only?
                   (filter #(:critical? (:lollapalooza %)) alerts)
                   alerts)]
    (take-last limit filtered)))

(defn get-alert-stats
  "Get alert statistics"
  []
  (let [alerts @alert-history
        now (System/currentTimeMillis)
        day-ms (* 24 60 60 1000)
        week-ms (* 7 day-ms)]
    {:total (count alerts)
     :last-24h (count (filter #(> (:timestamp %) (- now day-ms)) alerts))
     :last-week (count (filter #(> (:timestamp %) (- now week-ms)) alerts))
     :critical (count (filter #(:critical? (:lollapalooza %)) alerts))
     :by-category (frequencies (mapcat #(-> % :lollapalooza :categories) alerts))}))

;; =============================================================================
;; INTEGRATION WITH SCANNER
;; =============================================================================

(defn check-and-alert!
  "Check classification result and dispatch alert if Lollapalooza detected"
  [classification-result source-info]
  (when-let [lollapalooza (detect-lollapalooza classification-result)]
    (dispatch-alert! lollapalooza source-info)
    lollapalooza))
