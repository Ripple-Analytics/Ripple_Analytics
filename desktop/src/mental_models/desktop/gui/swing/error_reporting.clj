(ns mental-models.desktop.gui.swing.error-reporting
  "Swing App - Error Reporting"
  (:require [mental-models.desktop.gui.swing.state :refer [*state log!]]
            [mental-models.desktop.gui.swing.theme :refer [colors fonts]]
            [mental-models.desktop.gui.swing.config :refer [config]])
  (:import [javax.swing JFrame JPanel JLabel JButton JTextField JTextArea JScrollPane
                        JFileChooser JProgressBar JOptionPane SwingUtilities UIManager
                        BorderFactory JList DefaultListModel JMenuBar JMenu JMenuItem
                        JDialog Timer Box BoxLayout JComboBox ListSelectionModel JTable JSplitPane]
           [javax.swing.table DefaultTableModel]
           [javax.swing.border TitledBorder EmptyBorder]
           [java.awt BorderLayout GridLayout FlowLayout Color Font Dimension CardLayout
                     Desktop GridBagLayout GridBagConstraints Insets Cursor Graphics2D RenderingHints BasicStroke]
           [java.awt.event ActionListener WindowAdapter]
           [java.io File]
           [java.net URL HttpURLConnection URI]))

;; Error Reporting
;; =============================================================================

(defn format-stack-trace [^Throwable ex]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.printStackTrace ex pw)
    (.toString sw)))

(defn report-error-to-slack! [title message]
  (when-let [webhook (get-in @*state [:settings :slack-webhook])]
    (when-not (str/blank? webhook)
      (future
        (try
          (let [payload (str "{\"text\":\"*" title "*\\n" 
                            (str/replace message "\"" "\\\"") 
                            "\"}")
                result (http-post webhook payload)]
            (when (:success result)
              (swap! *state assoc-in [:connections :slack] :connected)))
          (catch Exception _))))))

(defn report-error-to-github! [title body]
  (let [token (or (get-in @*state [:settings :github-token]) (:github-token config))]
    (when-not (str/blank? token)
      (future
        (try
          (let [url (str (:github-api config) "/repos/" (:github-repo config) "/issues")
                payload (str "{\"title\":\"" (str/replace title "\"" "\\\"")
                            "\",\"body\":\"" (str/replace body "\"" "\\\"")
                            "\",\"labels\":[\"bug\",\"auto-reported\"]}")
                result (http-post url payload 
                         :headers {"Authorization" (str "token " token)
                                  "Accept" "application/vnd.github.v3+json"})]
            (when (:success result)
              (log! "GitHub issue created")))
          (catch Exception _))))))

(defn report-error-to-devin! [error-type message stack-trace]
  "Report error to Devin via Slack for automatic fixing"
  (let [webhook "https://hooks.slack.com/services/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"]
    (future
      (try
        (let [payload (str "{\"text\":\"<@U08Q3V7TNKS> *Desktop App Error Report*\\n"
                          "*Type:* " error-type "\\n"
                          "*Version:* " (:version config) "\\n"
                          "*Message:* " (str/replace (str message) "\"" "'") "\\n"
                          "*Stack:* ```" (subs (str/replace (str stack-trace) "\"" "'") 0 (min 500 (count stack-trace))) "```\"}")
              result (http-post webhook payload)]
          (when (:success result)
            (log! "[DEVIN] Error reported to Devin")))
        (catch Exception e
          (log! (str "[DEVIN] Failed to report: " (.getMessage e))))))))

(defn handle-error! [^Throwable ex context]
  (let [timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
        title (str "Desktop App Error: " (.getSimpleName (class ex)))
        stack (format-stack-trace ex)
        body (str "**Error Report**\n\n"
                  "- **Time:** " timestamp "\n"
                  "- **Version:** " (:version config) "\n"
                  "- **Context:** " context "\n"
                  "- **Error:** " (.getMessage ex) "\n\n"
                  "**Stack Trace:**\n```\n" stack "\n```")]
    (log! (str "[ERROR] " title ": " (.getMessage ex)))
    (report-error-to-slack! title body)
    (report-error-to-github! title body)
    (report-error-to-devin! (.getSimpleName (class ex)) (.getMessage ex) stack)))

(defn request-feature-from-devin! [feature-description]
  "Send feature request to Devin via Slack"
  (let [webhook "https://hooks.slack.com/services/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"]
    (future
      (try
        (let [payload (str "{\"text\":\"<@U08Q3V7TNKS> *Feature Request from Desktop App*\\n"
                          "*Version:* " (:version config) "\\n"
                          "*Request:* " (str/replace feature-description "\"" "'") "\"}")
              result (http-post webhook payload)]
          (when (:success result)
            (log! "[DEVIN] Feature request sent")))
        (catch Exception e
          (log! (str "[DEVIN] Failed to send request: " (.getMessage e))))))))

(defn notify-startup! []
  "Notify Slack channel of app startup (for usage tracking)"
  (let [webhook (get-in @*state [:settings :slack-webhook])]
    (when (and webhook (not (str/blank? webhook)))
      (future
        (try
          (let [payload (str "{\"text\":\"Mental Models Desktop v" (:version config) " started\"}")
                _ (http-post webhook payload)]
            nil)
          (catch Exception _))))))

;; =============================================================================