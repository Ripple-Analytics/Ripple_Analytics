(ns mental-models.desktop.gui.swing.cli-batch
  "Swing App - Cli Batch"
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

;; CLI Batch Mode (Headless)
;; =============================================================================
(defn run-cli-batch [args]
  "Run in headless CLI mode for batch processing"
  (println "Mental Models Desktop - CLI Batch Mode")
  (println "======================================")
  (let [folders (filter #(.isDirectory (File. %)) args)]
    (if (empty? folders)
      (do
        (println "Usage: MentalModels.bat --batch <folder1> <folder2> ...")
        (println "       MentalModels.bat --batch-file <paths.txt>")
        (System/exit 1))
      (do
        (println (str "Processing " (count folders) " folders..."))
        (add-to-batch-queue! folders)
        (let [done (promise)]
          (process-batch-queue!
            (fn [result]
              (when (:file result)
                (println (str "  " (:file result) " -> " (count (:models result)) " models"))))
            (fn [stats]
              (println "")
              (println "=== BATCH COMPLETE ===")
              ;; Auto-sync batch results
              (when (get-in @*state [:settings :auto-sync] true)
                (try
                  (let [results (get-in @*state [:scan-results])]
                    (when (seq results)
                      (println "[SYNC] Auto-syncing batch results...")
                      (sync-to-web-app! results)
                      (println "[SYNC] Batch sync complete")))
                  (catch Exception e
                    (println (str "[SYNC] Batch sync failed: " (.getMessage e))))))
              (println (str "Folders: " (:completed-folders stats)))
              (println (str "Files:   " (:total-files stats)))
              (println (str "Models:  " (:total-models stats)))
              (deliver done true)))
          @done)
        (System/exit 0)))))


;; =============================================================================