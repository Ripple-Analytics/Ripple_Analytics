(ns mental-models.desktop.gui.swing.models-browser-panel
  "Swing App - Models Browser Panel"
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

;; Models Browser Panel
;; =============================================================================

(defn create-models-panel []
  "VALUE LINE STYLE - Dense multi-column models browser"
  (let [panel (JPanel. (BorderLayout. 2 2))
        ;; Top row: Search + Category filter
        top-row (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))
        search-field (JTextField. 20)
        category-combo (JComboBox. (into-array String ["All" "Psych" "Econ" "Bio" "Phys" "Math" "Eng" "Moat" "Org" "Think"]))
        ;; Main: Table of all models
        table-model (javax.swing.table.DefaultTableModel.
                      (into-array ["#" "Model Name" "Category" "Keywords"])
                      0)
        models-table (javax.swing.JTable. table-model)
        table-scroll (JScrollPane. models-table)
        ;; Right: Detail panel (compact)
        detail-area (JTextArea. 8 25)
        detail-scroll (JScrollPane. detail-area)
        ;; Split pane
        split-pane (javax.swing.JSplitPane. javax.swing.JSplitPane/HORIZONTAL_SPLIT)]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 2 2 2 2))
    
    ;; TOP ROW - Search + filter
    (.setOpaque top-row false)
    (let [lbl (JLabel. "Find:")]
      (.setFont lbl (:micro fonts))
      (.add top-row lbl))
    (.setFont search-field (:data fonts))
    (.setPreferredSize search-field (Dimension. 120 16))
    (.add top-row search-field)
    (let [lbl (JLabel. "Cat:")]
      (.setFont lbl (:micro fonts))
      (.add top-row lbl))
    (.setFont category-combo (:micro fonts))
    (.add top-row category-combo)
    ;; Count label
    (let [count-lbl (JLabel. (str "Total:" (count mental-models)))]
      (.setFont count-lbl (:micro fonts))
      (.setForeground count-lbl (:text-muted colors))
      (.add top-row count-lbl))
    (.add panel top-row BorderLayout/NORTH)
    
    ;; Populate table with all models
    (doseq [model mental-models]
      (.addRow table-model
        (into-array Object
          [(:id model)
           (subs (:name model) 0 (min 25 (count (:name model))))
           (subs (:category model) 0 (min 5 (count (:category model))))
           (str/join "," (take 3 (:keywords model)))])))
    
    ;; MODELS TABLE - Value Line dense
    (.setFont models-table (:data fonts))
    (.setRowHeight models-table 11)
    (.setShowGrid models-table true)
    (.setGridColor models-table (:border colors))
    (.setAutoResizeMode models-table javax.swing.JTable/AUTO_RESIZE_OFF)
    (let [cm (.getColumnModel models-table)]
      (.setPreferredWidth (.getColumn cm 0) 25)   ;; #
      (.setPreferredWidth (.getColumn cm 1) 150)  ;; Name
      (.setPreferredWidth (.getColumn cm 2) 40)   ;; Cat
      (.setPreferredWidth (.getColumn cm 3) 120)) ;; Keywords
    (.setBorder table-scroll (BorderFactory/createTitledBorder
                               (BorderFactory/createLineBorder (:border colors) 1)
                               "MODELS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.setLeftComponent split-pane table-scroll)
    
    ;; DETAIL AREA - compact
    (.setEditable detail-area false)
    (.setLineWrap detail-area true)
    (.setWrapStyleWord detail-area true)
    (.setFont detail-area (:data fonts))
    (.setBorder detail-scroll (BorderFactory/createTitledBorder
                                (BorderFactory/createLineBorder (:border colors) 1)
                                "DETAIL" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.setRightComponent split-pane detail-scroll)
    (.setDividerLocation split-pane 350)
    
    ;; Selection listener - show detail
    (.addListSelectionListener (.getSelectionModel models-table)
      (reify javax.swing.event.ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (let [row (.getSelectedRow models-table)]
              (when (>= row 0)
                (let [model-id (Integer/parseInt (str (.getValueAt table-model row 0)))
                      model (first (filter #(= (:id %) model-id) mental-models))]
                  (when model
                    (.setText detail-area
                      (str (:name model) "\n"
                           "Cat: " (:category model) "\n"
                           (:description model) "\n\n"
                           "Ex: " (:example model)))))))))))
    
    ;; Search filter - simple keyword search
    (let [do-filter (fn []
                      (let [query (.getText search-field)
                            query-lower (str/lower-case query)
                            cat-full {"All" nil "Psych" "Psychology" "Econ" "Economics" "Bio" "Biology"
                                      "Phys" "Physics" "Math" "Mathematics" "Eng" "Engineering"
                                      "Moat" "Moats" "Org" "Organizational" "Think" "Thinking Tools"}
                            cat (get cat-full (str (.getSelectedItem category-combo)))]
                        (.setRowCount table-model 0)
                        (doseq [model mental-models]
                          (when (and (or (str/blank? query)
                                        (str/includes? (str/lower-case (:name model)) query-lower)
                                        (str/includes? (str/lower-case (str (:keywords model))) query-lower))
                                    (or (nil? cat)
                                        (str/includes? (:category model) (or cat ""))))
                            (.addRow table-model
                              (into-array Object
                                [(:id model)
                                 (subs (:name model) 0 (min 25 (count (:name model))))
                                 (subs (:category model) 0 (min 5 (count (:category model))))
                                 (str/join "," (take 3 (:keywords model)))]))))))]
      (.addActionListener search-field
        (reify ActionListener
          (actionPerformed [_ _] (do-filter))))
      (.addActionListener category-combo
        (reify ActionListener
          (actionPerformed [_ _] (do-filter)))))
    
    (.add panel split-pane BorderLayout/CENTER)
    
    ;; Bottom strip with web link - compact
    (let [bottom-strip (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))
          web-btn (JButton. "Web")]
      (.setOpaque bottom-strip false)
      (.setFont web-btn (:micro fonts))
      (.setMargin web-btn (Insets. 0 3 0 3))
      (.addActionListener web-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (let [web-url (or (get-in @*state [:settings :web-app-url])
                               (:web-app-url config)
                               "https://mental-models-web.manus.space")
                    row (.getSelectedRow models-table)
                    model-id (when (>= row 0) (Integer/parseInt (str (.getValueAt table-model row 0))))
                    url (if model-id
                          (str web-url "/models/" model-id)
                          web-url)]
                (.browse (Desktop/getDesktop) (URI. url)))
              (catch Exception _)))))
      (.add bottom-strip web-btn)
      (.add panel bottom-strip BorderLayout/SOUTH))
    
    panel))
;; =============================================================================