(ns mental-models.desktop.gui.swing.logs-panel
  "Swing App - Logs Panel"
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

;; Logs Panel
;; =============================================================================

(defn create-logs-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        logs-area (JTextArea. 25 80)
        scroll (JScrollPane. logs-area)
        clear-btn (JButton. "Clear Logs")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Live Logs")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Logs area
    (.setEditable logs-area false)
    (.setFont logs-area (:mono fonts))
    (.setBackground logs-area (Color. 30 30 30))
    (.setForeground logs-area (Color. 200 200 200))
    (.add panel scroll BorderLayout/CENTER)
    
    ;; Update logs periodically
    (let [timer (Timer. 1000
                  (reify ActionListener
                    (actionPerformed [_ _]
                      (let [logs (:logs @*state)]
                        (.setText logs-area (str/join "\n" (reverse (take 100 logs))))))))]
      (.start timer))
    
    ;; Clear button
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/RIGHT))]
      (.setOpaque btn-panel false)
      (.addActionListener clear-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (swap! *state assoc :logs [])
            (.setText logs-area ""))))
      (.add btn-panel clear-btn)
      (.add panel btn-panel BorderLayout/SOUTH))
    
    panel))

;; =============================================================================