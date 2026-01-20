(ns mental-models.desktop.gui.swing.blue-green-deployment-part2
  "Swing App - Blue Green Deployment (Part 2)"
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

                           (keyword (str (name inactive-slot) "-ready")) true)
                    (swap! *state assoc-in [:update :status] "✓ Delta ready")
                    ;; Auto-swap after brief delay
                    (Thread/sleep 2000)
                    (swap-slots!))
                  (log! "[BLUE-GREEN] Some deltas failed")))))
          (log! "[BLUE-GREEN] No changes detected")))
      (catch Exception e
        (log! (str "[BLUE-GREEN] Delta update failed: " (.getMessage e)))
        (swap! *blue-green-state assoc :pending-download nil)))))

(defn fetch-code-manifest []
  "Fetch manifest of current code files and hashes from web app"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/desktop.codeManifest")
          result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
      (when (:success result)
        (let [body (:body result)]
          ;; Parse JSON response
          (when-let [data (re-find #"\"files\":\s*\[([^\]]+)\]" body)]
            {:success true :manifest (second data)}))))
    (catch Exception e
      (log! (str "[DELTA] Failed to fetch manifest: " (.getMessage e)))
      nil)))

(defn download-delta-file! [file-path file-url]
  "Download a single changed file"
  (try
    (let [app-dir (File. (or (System/getProperty "app.dir") "."))
          dest-file (File. app-dir file-path)
          result (http-get file-url)]
      (when (:success result)
        (.mkdirs (.getParentFile dest-file))
        (spit dest-file (:body result))
        (log! (str "[DELTA] Updated: " file-path))
        true))
    (catch Exception e
      (log! (str "[DELTA] Failed to update " file-path ": " (.getMessage e)))
      false)))

(defn apply-delta-updates! [changed-files]
  "Apply delta updates for changed files only"
  (log! (str "[DELTA] Applying " (count changed-files) " file updates..."))
  (let [results (doall (map (fn [{:keys [path url]}]
                              (download-delta-file! path url))
                            changed-files))]
    (if (every? true? results)
      (do
        (log! "[DELTA] All updates applied successfully")
        (swap! *state assoc-in [:update :status] "✓ Live updated")
        ;; Reload changed namespaces if possible
        (try
          (require 'mental-models.desktop.gui.swing-app :reload)
          (log! "[DELTA] Hot-reloaded code")
          (catch Exception e
            (log! (str "[DELTA] Hot reload not possible: " (.getMessage e)))))
        true)
      (do
        (log! "[DELTA] Some updates failed")
        false))))

(defn check-for-delta-updates! []
  "Check web app for code changes and apply blue-green delta updates"
  (future
    (try
      (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                     "/api/trpc/desktop.checkUpdates?input=" 
                     (java.net.URLEncoder/encode 
                       (str "{\"json\":{\"version\":\"" (:version config) 
                            "\",\"codeHash\":\"" @*last-code-hash "\"}}")
                       "UTF-8"))
            result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
        (when (:success result)
          (let [body (:body result)]
            (when (str/includes? body "\"hasUpdates\":true")
              (log! "[POLLER] Updates detected - starting blue-green delta update...")
              ;; Use blue-green delta update (downloads only changed files)
              (blue-green-delta-update!)))))
      (catch Exception e
        (log! (str "[POLLER] Update check failed: " (.getMessage e)))))))

(defn start-update-poller! []
  "Start background polling for updates"
  (when-not @*update-poller
    (reset! *update-poller
      (future
        (log! "[POLLER] Started continuous update polling")
        (while true
          (try
            (Thread/sleep @*update-poll-interval)
            (check-for-delta-updates!)
            (catch InterruptedException _
              (log! "[POLLER] Stopped")
              (throw (InterruptedException.)))
            (catch Exception e
              (log! (str "[POLLER] Error: " (.getMessage e))))))))))

(defn stop-update-poller! []
  "Stop the update poller"
  (when-let [p @*update-poller]
    (future-cancel p)
    (reset! *update-poller nil)
    (log! "[POLLER] Stopped")))

;; =============================================================================