(ns mental-models.desktop.gui.swing.gdrive-updates
  "Swing App - Gdrive Updates"
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

;; Google Drive Update Functions
;; =============================================================================

(defn check-gdrive-manifest []
  "Check Google Drive manifest for latest version - PRIMARY update method"
  (try
    (println "[GDRIVE] Checking manifest...")
    (let [manifest-url (str (:gdrive-releases-url config) (:gdrive-manifest-id config))
          result (http-get manifest-url)]
      (if (:success result)
        (let [body (:body result)]
          (try
            ;; Parse JSON manifest
            (let [latest (second (re-find #"\"latest\"\s*:\s*\"([^\"]+)\"" body))
                  url (second (re-find #"\"url\"\s*:\s*\"([^\"]+)\"" body))]
              (println "[GDRIVE] Manifest parsed - latest:" latest "url:" url)
              {:success true :version latest :download-url url})
            (catch Exception e
              (println "[GDRIVE] Failed to parse manifest:" (.getMessage e))
              {:success false :error "Parse failed"})))
        (do
          (println "[GDRIVE] Failed to fetch manifest:" (:error result))
          {:success false :error (:error result)})))
    (catch Exception e
      (println "[GDRIVE] Exception:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn is-gdrive-url? [url]
  "Check if URL is a Google Drive URL"
  (and url (or (str/includes? url "drive.google.com")
               (str/includes? url "docs.google.com"))))

(defn check-updates-silently! [frame]
  "Check for updates - tries Google Drive first (no auth), then web app, then GitHub"
  (future
    (try
      (Thread/sleep 3000) ;; Wait for UI to load
      ;; Start continuous polling
      (start-update-poller!)
      (println "[AUTO-UPDATE] Checking for updates...")
      
      ;; TRY GOOGLE DRIVE FIRST (PRIMARY - no auth needed, always works)
      (let [gdrive-result (check-gdrive-manifest)]
        (if (:success gdrive-result)
          (let [tag (:version gdrive-result)
                download-url (:download-url gdrive-result)
                newer? (version-newer? tag (:version config))]
            (println "[AUTO-UPDATE] Google Drive - Current:" (:version config) "Latest:" tag "Newer?" newer?)
            (if (and newer? download-url)
              (do
                (println "[AUTO-UPDATE] *** NEW VERSION DETECTED VIA GOOGLE DRIVE ***")
                (log! (str "[AUTO-UPDATE] New version " tag " found - downloading from Google Drive..."))
                (swap! *state assoc-in [:update :available] true)
                (swap! *state assoc-in [:update :latest-version] tag)
                (swap! *state assoc-in [:update :download-url] download-url)
                (perform-auto-update! frame download-url tag))
              (println "[AUTO-UPDATE] Already on latest version")))
          
          ;; FALLBACK 1: Web app config
          (do
            (println "[AUTO-UPDATE] Google Drive unavailable, trying web app...")
            (let [web-config (fetch-remote-config)]
              (if (and web-config (:version web-config))
                (let [tag (:version web-config)
                      sources (:sources web-config)
                      newer? (version-newer? tag (:version config))]
                  (println "[AUTO-UPDATE] Web app - Current:" (:version config) "Latest:" tag "Newer?" newer?)
                  (if (and newer? (seq sources))
                    (do
                      (println "[AUTO-UPDATE] *** NEW VERSION DETECTED VIA WEB APP ***")
                      (log! (str "[AUTO-UPDATE] New version " tag " found - downloading..."))
                      (swap! *state assoc-in [:update :available] true)
                      (swap! *state assoc-in [:update :latest-version] tag)
                      (let [download-url (:url (first sources))]
                        (swap! *state assoc-in [:update :download-url] download-url)
                        (perform-auto-update! frame download-url tag)))
                    (println "[AUTO-UPDATE] Already on latest version")))
                
                ;; FALLBACK 2: GitHub API (public releases only)
                (do
                  (println "[AUTO-UPDATE] Web app unavailable, trying GitHub public API...")
                  (let [url (str (:github-api config) "/repos/" (:github-repo config) "/releases/latest")
                        headers {"Accept" "application/vnd.github.v3+json"}
                        result (http-get url :headers headers)]
                    (if (:success result)
                      (let [body (:body result)
                            tag (second (re-find #"\"tag_name\"\s*:\s*\"([^\"]+)\"" body))
                            download-url (second (re-find #"\"browser_download_url\"\s*:\s*\"([^\"]+\.zip)\"" body))
                            newer? (version-newer? tag (:version config))]
                        (println "[AUTO-UPDATE] GitHub - Current:" (:version config) "Latest:" tag "Newer?" newer?)
                        (if (and newer? download-url)
                          (do
                            (println "[AUTO-UPDATE] *** NEW VERSION DETECTED VIA GITHUB ***")
                            (log! (str "[AUTO-UPDATE] New version " tag " found - downloading..."))
                            (swap! *state assoc-in [:update :available] true)
                            (swap! *state assoc-in [:update :latest-version] tag)
                            (swap! *state assoc-in [:update :download-url] download-url)
                            (perform-auto-update! frame download-url tag))
                          (println "[AUTO-UPDATE] Already on latest version or no download URL")))
                      (println "[AUTO-UPDATE] All update sources failed")))))))))
      (catch Exception e
        (println "[AUTO-UPDATE] Silent check failed:" (.getMessage e))
        (.printStackTrace e)))))

;; =============================================================================