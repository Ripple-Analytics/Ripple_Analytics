(ns mental-models.desktop.gui.swing.connection-checks
  "Swing App - Connection Checks"
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

;; Connection Checks
;; =============================================================================

(defn check-lm-studio! []
  (future
    (try
      (let [url (str (get-in @*state [:settings :lm-studio-url]) "/v1/models")
            result (http-get url)]
        (swap! *state assoc-in [:connections :lm-studio] 
               (if (:success result) :connected :disconnected)))
      (catch Exception _
        (swap! *state assoc-in [:connections :lm-studio] :disconnected)))))

(defn check-web-app! []
  (future
    (try
      (let [url (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))]
        (if (and url (not (str/blank? url)))
          (let [result (http-get (str url "/api/trpc/desktop.config") 
                                 :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
            (swap! *state assoc-in [:connections :web-app]
                   (if (:success result) :connected :disconnected))
            (when (:success result)
              (log! "[WEB-APP] Connected successfully")))
          (swap! *state assoc-in [:connections :web-app] :disconnected)))
      (catch Exception e
        (log! (str "[WEB-APP] Connection error: " (.getMessage e)))
        (swap! *state assoc-in [:connections :web-app] :disconnected)))))

(defn sync-to-web-app! [scan-results]
  "Push scan results to web app for dashboard sync"
  (future
    (try
      (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                     "/api/trpc/desktop.syncResults")
            payload (str "{\"json\":{\"results\":"
                        (str "[" (str/join ","
                               (map (fn [r]
                                     (str "{\"file\":\"" (str/replace (:file r "") "\"" "\\\"") "\","
                                          "\"models\":[" (str/join "," (map (fn [m] (str "{\"name\":\"" (:name m) "\"}")) (:models r []))) "],"
                                          "\"lollapalooza\":" (if (:lollapalooza r) "true" "false") "}"))
                                    scan-results)) "]")
                        ",\"timestamp\":" (System/currentTimeMillis) "}}")]
        (log! (str "[SYNC] Pushing " (count scan-results) " results to web app"))
        (let [result (http-post url payload)]
          (if (:success result)
            (log! "[SYNC] Successfully synced to web app")
            (log! (str "[SYNC] Failed to sync: " (:body result))))))
      (catch Exception e
        (log! (str "[SYNC] Error syncing: " (.getMessage e)))))))

;; =============================================================================