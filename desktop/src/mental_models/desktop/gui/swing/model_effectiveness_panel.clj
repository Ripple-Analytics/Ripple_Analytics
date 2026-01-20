(ns mental-models.desktop.gui.swing.model-effectiveness-panel
  "Swing App - Model Effectiveness Panel"
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

;; Model Effectiveness Panel
;; =============================================================================

(defn create-effectiveness-panel []
  "Panel showing Bayesian effectiveness tracking for mental models"
  (let [panel (JPanel. (BorderLayout. 20 20))
        table-model (DefaultTableModel.
                     (into-array ["Model" "Domain" "Success" "Failure" "Bayesian Score"])
                     0)
        table (JTable. table-model)
        refresh-btn (JButton. "🔄 Refresh")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [header (JPanel. (BorderLayout.))
          title (JLabel. "📊 Model Effectiveness")]
      (.setOpaque header false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add header title BorderLayout/WEST)
      (.add header refresh-btn BorderLayout/EAST)
      (.add panel header BorderLayout/NORTH))
    
    ;; Sample data - will be replaced by web app data
    (doseq [[model domain success fail score] 
            [["Margin of Safety" "Investing" 15 3 "0.83"]
             ["Circle of Competence" "Business" 12 2 "0.86"]
             ["Incentive Bias" "Management" 8 4 "0.67"]
             ["Social Proof" "Marketing" 10 5 "0.67"]
             ["Sunk Cost" "Decisions" 6 8 "0.43"]
             ["Confirmation Bias" "Research" 4 6 "0.40"]]]
      (.addRow table-model (into-array Object [model domain success fail score])))
    
    ;; Table setup
    (.setFont table (:body fonts))
    (.setRowHeight table 28)
    
    ;; Explanation
    (let [info (JLabel. "<html>Bayesian scores update based on decision outcomes. Higher = more effective.</html>")]
      (.setFont info (:small fonts))
      (.add panel info BorderLayout/SOUTH))
    
    (.add panel (JScrollPane. table) BorderLayout/CENTER)
    
    panel))

;; =============================================================================