(ns mental-models.desktop.gui.swing.scan-panel-part2
  "Swing App - Scan Panel (Part 2)"
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

                         frame 
                         (str "Batch complete!\n"
                              "Folders: " (:completed-folders stats) "\n"
                              "Files: " (:total-files stats) "\n"
                              "Models: " (:total-models stats))
                         "Batch Complete"
                         javax.swing.JOptionPane/INFORMATION_MESSAGE)
                       (refresh-dashboard!))))))))))
    (.add top-row run-queue-btn)
    
    ;; Sync to Web button
    (let [sync-btn (JButton. "↗ Sync to Web")]
      (.setFont sync-btn (:small fonts))
      (.setMargin sync-btn (Insets. 0 6 0 6))
      (.setForeground sync-btn (:accent colors))
      (.setToolTipText sync-btn "Push scan results to web dashboard")
      (.addActionListener sync-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [results (get-in @*state [:scan-results])]
              (if (empty? results)
                (javax.swing.JOptionPane/showMessageDialog 
                  frame "No scan results to sync. Run a scan first." "Nothing to Sync" javax.swing.JOptionPane/WARNING_MESSAGE)
                (do
                  (.setEnabled sync-btn false)
                  (.setText sync-btn "Syncing...")
                  (future
                    (try
                      (sync-to-web-app! results)
                      (SwingUtilities/invokeLater
                        #(do
                           (.setText sync-btn "✓ Synced!")
                           (javax.swing.JOptionPane/showMessageDialog 
                             frame 
                             (str "Synced " (count results) " results to web dashboard.\n\nView at: " (:web-app-url config) "/dashboard")
                             "Sync Complete"
                             javax.swing.JOptionPane/INFORMATION_MESSAGE)
                           (Thread/sleep 2000)
                           (.setText sync-btn "↗ Sync to Web")
                           (.setEnabled sync-btn true)))
                      (catch Exception e
                        (SwingUtilities/invokeLater
                          #(do
                             (.setText sync-btn "↗ Sync to Web")
                             (.setEnabled sync-btn true)
                             (javax.swing.JOptionPane/showMessageDialog 
                               frame (str "Sync failed: " (.getMessage e)) "Sync Error" javax.swing.JOptionPane/ERROR_MESSAGE))))))))))))
      (.add top-row sync-btn))
    
    ;; Progress bar - compact
    (.setPreferredSize progress (Dimension. 80 14))
    (.setFont progress (:micro fonts))
    (.setStringPainted progress true)
    (.add top-row progress)
    
    (.add panel top-row BorderLayout/NORTH)
    
    ;; RESULTS TABLE - Value Line dense
    (.setFont results-table (:data fonts))
    (.setRowHeight results-table 12)
    (.setShowGrid results-table true)
    (.setGridColor results-table (:border colors))
    (.setAutoResizeMode results-table javax.swing.JTable/AUTO_RESIZE_OFF)
    ;; Set column widths
    (let [cm (.getColumnModel results-table)]
      (.setPreferredWidth (.getColumn cm 0) 180)  ;; File
      (.setPreferredWidth (.getColumn cm 1) 150)  ;; Models
      (.setPreferredWidth (.getColumn cm 2) 25)   ;; #
      (.setPreferredWidth (.getColumn cm 3) 20)   ;; L
      (.setPreferredWidth (.getColumn cm 4) 35))  ;; Conf
    (.setBorder results-scroll (BorderFactory/createTitledBorder
                                 (BorderFactory/createLineBorder (:border colors) 1)
                                 "SCAN RESULTS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.add panel results-scroll BorderLayout/CENTER)
    
    ;; STATS STRIP - bottom
    (.setOpaque stats-strip false)
    (doseq [lbl [files-lbl models-lbl lolla-lbl rate-lbl]]
      (.setFont lbl (:micro fonts))
      (.setForeground lbl (:text-muted colors))
      (.add stats-strip lbl))
    (.add panel stats-strip BorderLayout/SOUTH)
    
    panel))

;; =============================================================================