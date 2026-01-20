(ns mental-models.desktop.gui.swing.dashboard-panel-part2
  "Swing App - Dashboard Panel (Part 2)"
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

    
    ;; Panel 2: Recent Scans
    (.add main-panel (create-recent-scans-table))
    
    ;; Panel 3: Top Models Found (frequency table)
    (let [models-panel (JPanel. (BorderLayout. 0 0))
          model-counts (frequencies (mapcat :models (:scan-results @*state)))
          top-models (take 8 (sort-by val > model-counts))
          ;; model-counts keys are model names (strings), not maps
          table-data (to-array-2d (map (fn [[model-name cnt]] 
                                         [(let [name-str (str model-name)]
                                            (subs name-str 0 (min 20 (count name-str))))
                                          cnt]) 
                                       top-models))
          columns (into-array ["Mental Model" "Count"])
          table (javax.swing.JTable. table-data columns)]
      (.setFont table (:data fonts))
      (.setRowHeight table 11)
      (.setShowGrid table true)
      (.setGridColor table (:border colors))
      (.setBorder models-panel (BorderFactory/createTitledBorder 
                                 (BorderFactory/createLineBorder (:border colors) 1)
                                 "TOP MODELS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
      (.add models-panel (JScrollPane. table) BorderLayout/CENTER)
      (.add main-panel models-panel))
    
    ;; Panel 4: Category Distribution
    (let [cat-panel (JPanel. (GridLayout. 0 2 2 1))
          categories ["Psychology" "Economics" "Biology" "Physics" "Mathematics" "Engineering" "Moats" "Organizational"]]
      (.setBorder cat-panel (BorderFactory/createTitledBorder 
                              (BorderFactory/createLineBorder (:border colors) 1)
                              "CATEGORIES" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
      (doseq [cat categories]
        (let [cat-count (count (filter #(= cat (:category %)) mental-models))]
          (.add cat-panel (create-stat-row cat cat-count))))
      (.add main-panel cat-panel))
    
    (.add panel main-panel BorderLayout/CENTER)
    
    ;; BOTTOM STRIP - Compact buttons
    (.setOpaque bottom-strip false)
    (doseq [[label action-fn] [["Web" #(try (.browse (Desktop/getDesktop) (URI. (or (get-in @*state [:settings :web-app-url]) (:web-app-url config)))) (catch Exception _))]
                               ["Refresh" #(refresh-dashboard!)]
                               ["Update" #(check-for-updates! panel)]]]
      (let [btn (JButton. label)]
        (.setFont btn (:micro fonts))
        (.setMargin btn (Insets. 1 4 1 4))
        (.addActionListener btn (reify ActionListener (actionPerformed [_ _] (action-fn))))
        (.add bottom-strip btn)))
    (.add panel bottom-strip BorderLayout/SOUTH)
    
    panel))

;; =============================================================================