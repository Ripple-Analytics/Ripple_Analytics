(ns mental-models.desktop.gui.swing.batch-queue
  "Swing App - Batch Queue"
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

;; Batch Queue Processing (Wholesale Mode)
;; =============================================================================
(def *batch-queue (atom []))
(def *batch-running (atom false))
(def *batch-stats (atom {:total-folders 0 :completed-folders 0 :total-files 0 :total-models 0}))

(defn add-to-batch-queue! [folder-paths]
  "Add multiple folder paths to the batch queue"
  (swap! *batch-queue concat folder-paths)
  (swap! *batch-stats update :total-folders + (count folder-paths))
  (log! (str "[BATCH] Added " (count folder-paths) " folders to queue. Total: " (count @*batch-queue))))

(defn process-batch-queue! [progress-callback completion-callback]
  "Process all folders in the batch queue sequentially"
  (when-not @*batch-running
    (reset! *batch-running true)
    (future
      (try
        (log! (str "[BATCH] Starting batch processing of " (count @*batch-queue) " folders"))
        (let [start-time (System/currentTimeMillis)]
          (doseq [[idx folder] (map-indexed vector @*batch-queue)]
            (when @*batch-running
              (log! (str "[BATCH] Processing folder " (inc idx) "/" (count @*batch-queue) ": " folder))
              (let [folder-files (atom 0)
                    folder-models (atom 0)]
                (scan-folder folder
                  (fn [result]
                    (when (:models result)
                      (swap! folder-files inc)
                      (swap! folder-models + (count (:models result)))
                      (swap! *batch-stats update :total-files inc)
                      (swap! *batch-stats update :total-models + (count (:models result))))
                    (when progress-callback
                      (progress-callback {:folder folder
                                         :folder-progress (/ (* 100 (inc idx)) (count @*batch-queue))
                                         :file (:file result)
                                         :models (:models result)})))
                  nil)
                ;; Wait for folder scan to complete
                (Thread/sleep 100)
                (while (not= @folder-files @folder-files) (Thread/sleep 50)))
              (swap! *batch-stats update :completed-folders inc)))
          (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
            (log! (str "[BATCH] Complete. " (:total-files @*batch-stats) " files, " 
                      (:total-models @*batch-stats) " models in " (format "%.1f" elapsed) "s"))
            (when completion-callback
              (completion-callback @*batch-stats))))
        (catch Exception e
          (log! (str "[BATCH] Error: " (.getMessage e))))
        (finally
          (reset! *batch-running false)
          (reset! *batch-queue []))))))

(defn stop-batch! []
  "Stop batch processing"
  (reset! *batch-running false)
  (log! "[BATCH] Stopped"))

(defn clear-batch-queue! []
  "Clear the batch queue"
  (reset! *batch-queue [])
  (reset! *batch-stats {:total-folders 0 :completed-folders 0 :total-files 0 :total-models 0})
  (log! "[BATCH] Queue cleared"))

;; =============================================================================