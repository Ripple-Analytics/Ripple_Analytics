(ns mental-models.desktop.gui.swing.sync-status-panel
  "Swing App - Sync Status Panel"
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

;; Sync Status Panel
;; =============================================================================

(defn create-sync-panel []
  "Panel showing sync status with web app"
  (let [panel (JPanel. (BorderLayout. 20 20))
        status-area (JTextArea. 15 50)
        sync-btn (JButton. "🔄 Sync Now")
        clear-btn (JButton. "🗑️ Clear Queue")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "☁️ Sync Status")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Status display
    (.setEditable status-area false)
    (.setFont status-area (:mono fonts))
    (.setText status-area 
      (str "=== Sync Status ===\n\n"
           "Web App: " (or (get-in @*state [:settings :web-app-url]) "Not configured") "\n"
           "Connection: " (if (= (get-in @*state [:connections :web-app]) :connected) "Connected" "Disconnected") "\n\n"
           "=== Pending Sync Items ===\n"
           "Scan Results: " (count (get-in @*state [:sync-queue :scans] [])) "\n"
           "Decisions: " (count (get-in @*state [:sync-queue :decisions] [])) "\n"
           "Model Updates: " (count (get-in @*state [:sync-queue :models] [])) "\n\n"
           "=== Last Sync ===\n"
           "Time: " (or (get-in @*state [:last-sync]) "Never") "\n"
           "Status: " (or (get-in @*state [:last-sync-status]) "Unknown")))
    
    (.add panel (JScrollPane. status-area) BorderLayout/CENTER)
    
    ;; Buttons
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT))]
      (.setOpaque btn-panel false)
      (.add btn-panel clear-btn)
      (.add btn-panel sync-btn)
      (.add panel btn-panel BorderLayout/SOUTH))
    
    ;; Sync action
    (.addActionListener sync-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (future
            (let [results (get-in @*state [:recent-scans] [])]
              (when (seq results)
                (sync-to-web-app! results)
                (swap! *state assoc :last-sync (str (java.time.LocalDateTime/now))
                                   :last-sync-status "Success")))))))
    
    panel))

;; =============================================================================