(ns mental-models.desktop.gui.swing.signals-panel
  "Swing App - Signals Panel"
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

;; Signals Panel (News/SEC filings)
;; =============================================================================

(defn create-signals-panel []
  "Panel showing signals from news and SEC filings"
  (let [panel (JPanel. (BorderLayout. 20 20))
        table-model (DefaultTableModel.
                     (into-array ["Date" "Type" "Title" "Models Detected" "Lollapalooza"])
                     0)
        table (JTable. table-model)
        refresh-btn (JButton. "🔄 Fetch Signals")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [header (JPanel. (BorderLayout.))
          title (JLabel. "📡 Signals")]
      (.setOpaque header false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add header title BorderLayout/WEST)
      (.add header refresh-btn BorderLayout/EAST)
      (.add panel header BorderLayout/NORTH))
    
    ;; Sample signals
    (doseq [[date type title models lolla]
            [["2026-01-19" "News" "Tech Giant Reports Record Earnings" "Social Proof, FOMO" "No"]
             ["2026-01-18" "SEC" "Form 10-K: XYZ Corp" "Incentive Bias" "No"]
             ["2026-01-17" "News" "Startup Raises $500M at $10B Valuation" "Overconfidence, Greater Fool" "Yes"]
             ["2026-01-16" "SEC" "Form 8-K: Management Change" "Authority Bias" "No"]
             ["2026-01-15" "News" "Market Hits All-Time High" "Social Proof, FOMO, Denial" "Yes"]]]
      (.addRow table-model (into-array Object [date type title models lolla])))
    
    ;; Table setup
    (.setFont table (:body fonts))
    (.setRowHeight table 28)
    
    ;; Color Lollapalooza rows
    (.setDefaultRenderer table Object
      (proxy [javax.swing.table.DefaultTableCellRenderer] []
        (getTableCellRendererComponent [tbl value isSelected hasFocus row col]
          (let [comp (proxy-super getTableCellRendererComponent tbl value isSelected hasFocus row col)
                lolla-val (.getValueAt tbl row 4)]
            (when (and (not isSelected) (= lolla-val "Yes"))
              (.setBackground comp (Color. 254 243 199)))
            comp))))
    
    (.add panel (JScrollPane. table) BorderLayout/CENTER)
    
    ;; Refresh action
    (.addActionListener refresh-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (JOptionPane/showMessageDialog panel 
            "Fetching signals from web app...\nConnect your web app in Settings to enable."))))
    
    panel))

;; =============================================================================