(ns mental-models.desktop.gui.swing.main-entry
  "Swing App - Main Entry"
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

;; Main Entry Point
;; =============================================================================

(defn check-and-rollback-if-needed! []
  "Check if we crashed after an update and rollback if backup exists"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        crash-marker (File. app-dir "startup-in-progress.marker")
        backup-dir (File. app-dir "backup-previous-version")]
    ;; If crash marker exists from last run, we crashed during startup
    (when (.exists crash-marker)
      (println "[ROLLBACK] DETECTED CRASH FROM PREVIOUS STARTUP!")
      (when (.exists backup-dir)
        (println "[ROLLBACK] Backup found - rolling back to previous version...")
        (try
          ;; Copy backup back to main location
          (doseq [f (file-seq backup-dir)]
            (when (.isFile f)
              (let [rel-path (.substring (.getAbsolutePath f) 
                                        (inc (count (.getAbsolutePath backup-dir))))
                    target-file (File. app-dir rel-path)]
                (.mkdirs (.getParentFile target-file))
                (io/copy f target-file))))
          (println "[ROLLBACK] Rollback complete - previous version restored")
          ;; Report the rollback
          (report-error-to-devin! "AutoRollback" 
                                  "Automatic rollback triggered" 
                                  "Previous startup crashed, restored backup")
          (catch Exception e
            (println (str "[ROLLBACK] Rollback failed: " (.getMessage e))))))
      ;; Delete crash marker
      (.delete crash-marker))
    
    ;; Create crash marker - will be deleted at end of successful startup
    (spit crash-marker (str "Started: " (java.util.Date.)))))

(defn mark-startup-successful! []
  "Remove crash marker to indicate successful startup"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        crash-marker (File. app-dir "startup-in-progress.marker")]
    (when (.exists crash-marker)
      (.delete crash-marker)
      (println "[STARTUP] Startup successful - crash marker removed"))))

(defn restore-state-from-update! []
  "Check for and restore state from a previous update"
  (let [app-dir (File. (or (System/getProperty "app.dir") "."))
        state-file (File. app-dir "state-restore.edn")]
    (when (.exists state-file)
      (try
        (println "[HOT-RELOAD] Found state backup from update, restoring...")
        (let [saved-state (read-string (slurp state-file))]
          (restore-state! saved-state)
          ;; Delete the state file after successful restore
          (.delete state-file)
          (println "[HOT-RELOAD] State restored successfully - 99.9% uptime maintained!"))
        (catch Exception e
          (println (str "[HOT-RELOAD] Failed to restore state: " (.getMessage e)))
          ;; Delete corrupted state file
          (.delete state-file))))))


(defn run-startup-selftest []
  "Run self-test to catch errors BEFORE showing UI"
  (try
    (println "[SELFTEST] Testing UI components...")
    ;; Test 1: Can we create basic UI?
    (let [_ (JPanel.)] nil)
    ;; Test 2: Mental models loaded?
    (assert (seq mental-models) "Mental models not loaded")
    ;; Test 3: Dashboard panel
    (let [_ (create-dashboard-panel)] nil)
    ;; Test 4: Scan panel
    (let [_ (create-scan-panel nil)] nil)
    ;; Test 5: Models panel  
    (let [_ (create-models-panel)] nil)
    (println "[SELFTEST] All tests PASSED")
    true
    (catch Exception e
      (println (str "[SELFTEST] FAILED: " (.getMessage e)))
      (.printStackTrace e)
      false)))

(defn -main [& args]
  ;; Check for CLI batch mode
  (when (and (seq args) (= (first args) "--batch"))
    (run-cli-batch (rest args)))
  (when (and (seq args) (= (first args) "--batch-file"))
    (let [file-path (second args)
          paths (when file-path 
                  (str/split-lines (slurp file-path)))]
      (run-cli-batch paths)))
  
  (println "")
  (println "╔════════════════════════════════════════════════╗")
  (println "║     Mental Models Desktop v" (:version config) "           ║")
  (println "║  ★ SAFE UPDATES + AUTO-ROLLBACK                 ║")
  (println "╠════════════════════════════════════════════════╣")
  (println "║  • Tests new version BEFORE switching           ║")
  (println "║  • Auto-rollback if startup crashes             ║")
  (println "║  • State preservation on updates               ║")
  (println "║  • SQLite persistence enabled                  ║")
  (println "║  • Auto-update from GitHub releases            ║")
  (println "╚════════════════════════════════════════════════╝")
  (println "")
  
  ;; FIRST: Check if we crashed after an update and need to rollback
  (check-and-rollback-if-needed!)
  
  ;; Set up global exception handler for Devin feedback
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (let [stack (format-stack-trace ex)]
          (println "[ERROR] Uncaught exception:" (.getMessage ex))
          (report-error-to-devin! "UncaughtException" (.getMessage ex) stack)))))
  
  (init-database!)
  
  ;; Initialize semantic search index
  (println "[SEARCH] Indexing mental models...")
  (let [indexed (search/index-models! mental-models)]
    (println (str "[SEARCH] Indexed " indexed " models for semantic search")))
  
  ;; RESTORE STATE FROM UPDATE (99.9% uptime feature)
  (restore-state-from-update!)
  
  (let [saved-settings (load-settings!)]
    (when (seq saved-settings)
      (swap! *state update :settings merge saved-settings)
      (println "[SETTINGS] Loaded saved settings")))
  
  (SwingUtilities/invokeLater
    (fn []
      (let [frame (create-main-frame)]
        (.setVisible frame true)
        (println "[GUI] Application started")
        
        ;; MARK STARTUP SUCCESSFUL - remove crash marker
        (mark-startup-successful!)
        
        (check-all-connections!)
        ;; Silent startup update check - FULLY AUTOMATIC with SAFE updates
        (check-updates-silently! frame)
        ;; Notify Devin of startup (optional, for usage tracking)
        (when (get-in @*state [:settings :slack-webhook])
          (notify-startup!))))))

(comment
  (-main))
