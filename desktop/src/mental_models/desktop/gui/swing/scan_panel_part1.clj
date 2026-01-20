(ns mental-models.desktop.gui.swing.scan-panel-part1
  "Swing App - Scan Panel (Part 1)"
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

;; Scan Panel
;; =============================================================================

(defn create-scan-panel [frame]
  "VALUE LINE STYLE - Compact scan interface with table results"
  (let [panel (JPanel. (BorderLayout. 2 2))
        ;; Top: Path + buttons in single row
        top-row (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))
        path-field (JTextField. 50)
        browse-btn (JButton. "...")
        scan-btn (JButton. "Scan")
        ai-scan-btn (JButton. "AI Scan")
        add-queue-btn (JButton. "+ Queue")
        run-queue-btn (JButton. "Run Queue")
        progress (JProgressBar. 0 100)
        ;; Results as table instead of text area
        results-model (javax.swing.table.DefaultTableModel.
                        (into-array ["File" "Models Found" "Count" "Lollapalooza" "Confidence"])
                        0)
        results-table (javax.swing.JTable. results-model)
        results-scroll (JScrollPane. results-table)
        ;; Stats strip at bottom
        stats-strip (JPanel. (FlowLayout. FlowLayout/LEFT 3 0))
        files-lbl (JLabel. "Files:0")
        models-lbl (JLabel. "Models:0")
        lolla-lbl (JLabel. "Lollapalooza: 0")
        rate-lbl (JLabel. "Rate:0/s")]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 2 2 2 2))
    
    ;; TOP ROW - Path + controls
    (.setOpaque top-row false)
    (let [path-lbl (JLabel. "Path:")]
      (.setFont path-lbl (:micro fonts))
      (.add top-row path-lbl))
    (.setFont path-field (:data fonts))
    (.setPreferredSize path-field (Dimension. 300 18))
    (.add top-row path-field)
    
    (.setFont browse-btn (:micro fonts))
    (.setMargin browse-btn (Insets. 0 2 0 2))
    (.addActionListener browse-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [chooser (JFileChooser.)]
            (.setFileSelectionMode chooser JFileChooser/DIRECTORIES_ONLY)
            (when (= (.showOpenDialog chooser frame) JFileChooser/APPROVE_OPTION)
              (.setText path-field (.getAbsolutePath (.getSelectedFile chooser))))))))
    (.add top-row browse-btn)
    
    (.setFont scan-btn (:small fonts))
    (.setMargin scan-btn (Insets. 0 4 0 4))
    (.setBackground scan-btn (:primary colors))
    (.setForeground scan-btn Color/WHITE)
    (.addActionListener scan-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [path (.getText path-field)]
            (when-not (str/blank? path)
              (.setRowCount results-model 0)
              (.setValue progress 0)
              (let [start-time (System/currentTimeMillis)
                    file-count (atom 0)
                    model-count (atom 0)
                    lolla-count (atom 0)]
                (scan-folder path
                  (fn [result]
                    (SwingUtilities/invokeLater
                      #(do
                         (when (:progress result)
                           (.setValue progress (int (:progress result))))
                         (when (:models result)
                           (swap! file-count inc)
                           (swap! model-count + (count (:models result)))
                           (when (:lollapalooza result) (swap! lolla-count inc))
                           (.addRow results-model
                             (into-array Object
                               [(subs (:file result) 0 (min 30 (count (:file result))))
                                (str/join "," (map (fn [m] (subs (:name m) 0 (min 10 (count (:name m))))) (take 3 (:models result))))
                                (count (:models result))
                                (if (:lollapalooza result) "⚡" "-")
                                "-"]))
                           (.setText files-lbl (str "Files Scanned: " @file-count))
                           (.setText models-lbl (str "Models Found: " @model-count))
                           (.setText lolla-lbl (str "Lollapalooza: " @lolla-count))))))
                  (fn [summary]
                    (SwingUtilities/invokeLater
                      #(do
                         (.setValue progress 100)
                         (let [elapsed (/ (- (System/currentTimeMillis) start-time) 1000.0)]
                           (.setText rate-lbl (str "Scan Rate: " (format "%.1f" (/ @file-count (max 0.1 elapsed))) "/s")))
                         (refresh-dashboard!)))))))))))
    (.add top-row scan-btn)    
    ;; AI Scan button
    (.setFont ai-scan-btn (:small fonts))
    (.setMargin ai-scan-btn (Insets. 0 4 0 4))
    (.setToolTipText ai-scan-btn "Use LM Studio AI for deeper analysis")
    (.add top-row ai-scan-btn)
    
    ;; Batch queue buttons
    (.setFont add-queue-btn (:small fonts))
    (.setMargin add-queue-btn (Insets. 0 4 0 4))
    (.setToolTipText add-queue-btn "Add current path to batch queue")
    (.addActionListener add-queue-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [path (.getText path-field)]
            (when-not (str/blank? path)
              (add-to-batch-queue! [path])
              (javax.swing.JOptionPane/showMessageDialog 
                frame 
                (str "Added to queue. Total folders: " (count @*batch-queue))
                "Queue Updated"
                javax.swing.JOptionPane/INFORMATION_MESSAGE))))))
    (.add top-row add-queue-btn)
    
    (.setFont run-queue-btn (:small fonts))
    (.setMargin run-queue-btn (Insets. 0 4 0 4))
    (.setBackground run-queue-btn (:success colors))
    (.setForeground run-queue-btn Color/WHITE)
    (.setToolTipText run-queue-btn "Process all folders in queue")
    (.addActionListener run-queue-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (if (empty? @*batch-queue)
            (javax.swing.JOptionPane/showMessageDialog 
              frame "Queue is empty. Add folders first." "Empty Queue" javax.swing.JOptionPane/WARNING_MESSAGE)
            (do
              (.setRowCount results-model 0)
              (.setValue progress 0)
              (process-batch-queue!
                (fn [result]
                  (SwingUtilities/invokeLater
                    #(do
                       (when (:folder-progress result)
                         (.setValue progress (int (:folder-progress result))))
                       (when (:models result)
                         (.addRow results-model
                           (into-array Object
                             [(or (:file result) "")
                              (str/join ", " (map :name (take 3 (:models result))))
                              (count (:models result))
                              (if (:lollapalooza result) "Yes" "-")
                              "-"]))))))
                (fn [stats]
                  (SwingUtilities/invokeLater
                    #(do
                       (.setValue progress 100)
                       (javax.swing.JOptionPane/showMessageDialog 