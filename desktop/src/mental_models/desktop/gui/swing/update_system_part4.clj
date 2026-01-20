(ns mental-models.desktop.gui.swing.update-system-part4
  "Swing App - Update System (Part 4)"
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

                          (log! "[UPDATE] Backup complete")
                          
                          ;; STEP 6: Create update script
                          (let [update-script (File. temp-dir "mental-models-update.bat")
                                script-content (str "@echo off\r\n"
                                                   "echo [UPDATE] SAFE UPDATE - Version verified before switching\r\n"
                                                   "timeout /t 2 /nobreak >nul\r\n"
                                                   "echo [UPDATE] Copying verified new files...\r\n"
                                                   "xcopy /E /Y /Q \"" (.getAbsolutePath source-dir) "\\*\" \"" (.getAbsolutePath app-dir) "\\\"\r\n"
                                                   "echo [UPDATE] Preserving state...\r\n"
                                                   "copy /Y \"" (.getAbsolutePath state-backup-file) "\" \"" (.getAbsolutePath app-dir) "\\state-restore.edn\"\r\n"
                                                   "rmdir /S /Q \"" (.getAbsolutePath temp-extract) "\"\r\n"
                                                   "echo [UPDATE] Starting verified new version...\r\n"
                                                   "start \"\" \"" (.getAbsolutePath bat-file) "\"\r\n"
                                                   "del \"%~f0\"\r\n")]
                            
                            (spit update-script script-content)
                            (log! "[UPDATE] Created update script for verified version")
                            
                            (swap! *state assoc-in [:update :status] "Restarting (verified safe)...")
                            (Thread/sleep 1000)
                            
                            ;; Launch update script and exit
                            (.exec (Runtime/getRuntime) 
                                   (str "cmd /c start /min \"\" \"" (.getAbsolutePath update-script) "\""))
                            (Thread/sleep 500)
                            (log! "[UPDATE] Exiting for VERIFIED update...")
                            (System/exit 0)))
                        
                        ;; NEW VERSION FAILED TESTING - DO NOT SWITCH
                        (do
                          (log! "[UPDATE] New version FAILED testing - NOT switching, keeping current version")
                          (swap! *state assoc-in [:update :status] "Update REJECTED - bad version")
                          ;; Clean up the bad download
                          (doseq [f (file-seq temp-extract)]
                            (when (.isFile f) (.delete f)))
                          (.delete temp-extract)
                          ;; Report the failure
                          (report-error-to-devin! "UpdateRejected" 
                                                  (str "Version " tag " failed validation") 
                                                  "New version did not pass pre-switch testing")))))
                  
                  ;; Extraction failed
                  (do
                    (log! (str "[UPDATE] Extraction failed: " (:error extract-result)))
                    (.delete temp-zip)
                    (swap! *state assoc-in [:update :status] "Extract failed")))))
            
            ;; Download failed
            (do
              (log! (str "[UPDATE] Download failed: " (:error download-result)))
              (swap! *state assoc-in [:update :status] "Download failed")))))
      
      (catch Exception e
        (log! (str "[UPDATE] Error: " (.getMessage e)))
        (.printStackTrace e)
        (swap! *state assoc-in [:update :status] "Update error")))))

(defn check-for-updates! [parent]
  "Silent update check - uses GitHub API for private repo downloads"
  (swap! *state assoc-in [:update :checking] true)
  (swap! *state assoc-in [:update :status] "Checking...")
  (log! "[UPDATE] Checking for updates...")
  (future
    (try
      (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
            url (str (:github-api config) "/repos/" (:github-repo config) "/releases/latest")
            headers {"Accept" "application/vnd.github.v3+json"
                     "Authorization" (str "token " github-token)}
            result (http-get url :headers headers)]
        (swap! *state assoc-in [:update :checking] false)
        (if (:success result)
          (let [body (:body result)
                tag (second (re-find #"\"tag_name\"\s*:\s*\"([^\"]+)\"" body))
                ;; Extract asset API URL (not browser URL) for private repos
                ;; Format: https://api.github.com/repos/owner/repo/releases/assets/{id}
                asset-api-url (second (re-find #"\"url\"\s*:\s*\"(https://api\.github\.com/repos/[^/]+/[^/]+/releases/assets/\d+)\"" body))
                newer? (version-newer? tag (:version config))]
            (log! (str "[UPDATE] Current: " (:version config) " Latest: " tag " Newer? " newer?))
            (log! (str "[UPDATE] Asset API URL: " asset-api-url))
            (if newer?
              (do
                (log! (str "[UPDATE] New version available: " tag))
                (swap! *state assoc-in [:update :status] (str "Downloading " tag "..."))
                (swap! *state assoc-in [:update :available] true)
                (swap! *state assoc-in [:update :latest-version] tag)
                ;; Use API URL for private repo download
                (when asset-api-url
                  (perform-auto-update! parent asset-api-url tag)))
              (do
                (log! "[UPDATE] Already on latest version")
                (swap! *state assoc-in [:update :status] "Up to date ✓")
                (swap! *state assoc-in [:update :available] false))))
          (do
            (log! (str "[UPDATE] Check failed: " (:error result)))
            (swap! *state assoc-in [:update :status] "Check failed"))))
      (catch Exception e
        (swap! *state assoc-in [:update :checking] false)
        (swap! *state assoc-in [:update :status] "Error")
        (log! (str "[UPDATE] Error: " (.getMessage e)))))))

;; =============================================================================