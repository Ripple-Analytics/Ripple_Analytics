(ns mental-models.desktop.gui.swing.web-app-data-part2
  "Swing App - Web App Data (Part 2)"
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

    ;; Table setup
    (.setFont table (:body fonts))
    (.setRowHeight table 28)
    (.setSelectionMode (.getSelectionModel table) ListSelectionModel/SINGLE_SELECTION)
    
    ;; Selection listener for detail view
    (.addListSelectionListener (.getSelectionModel table)
      (reify javax.swing.event.ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (let [row (.getSelectedRow table)]
              (when (>= row 0)
                (let [name (.getValueAt table-model row 0)]
                  (.setText detail-area (str "Selected: " name "\n\nClick 'Refresh' to load full details from web app.")))))))))
    
    ;; Detail area
    (.setEditable detail-area false)
    (.setLineWrap detail-area true)
    (.setFont detail-area (:body fonts))
    
    ;; Layout
    (let [split (JSplitPane. JSplitPane/VERTICAL_SPLIT
                            (JScrollPane. table)
                            (JScrollPane. detail-area))]
      (.setDividerLocation split 250)
      (.add panel split BorderLayout/CENTER))
    
    ;; Refresh action
    (.addActionListener refresh-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (future
            (let [studies (fetch-case-studies)]
              (SwingUtilities/invokeLater
                #(do
                   (.setRowCount table-model 0)
                   (if (seq studies)
                     (doseq [s studies]
                       (.addRow table-model (into-array Object 
                         [(:title s) (:category s) 
                          (str (count (:primaryModels s [])) " models")
                          (if (:lollapaloozaPresent s) "Yes" "No")])))
                     (.setText detail-area "No case studies found or web app unavailable.")))))))))
    
    panel))

;; =============================================================================