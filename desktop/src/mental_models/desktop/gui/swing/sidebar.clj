(ns mental-models.desktop.gui.swing.sidebar
  "Swing App - Sidebar"
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

;; Sidebar
;; =============================================================================

(defn create-sidebar [card-layout content-panel frame]
  "Clean modern sidebar matching web app design"
  (let [sidebar (JPanel.)
        layout (BoxLayout. sidebar BoxLayout/Y_AXIS)]
    
    (.setLayout sidebar layout)
    (.setBackground sidebar (:bg-secondary colors))
    (.setPreferredSize sidebar (Dimension. 180 0))  ;; Wider for readability
    (.setBorder sidebar (EmptyBorder. 12 12 12 12))  ;; More breathing room
    
    ;; Logo/Title - full name
    (let [logo (JLabel. "Mental Models")]
      (.setFont logo (:header fonts))
      (.setForeground logo (:accent colors))
      (.setAlignmentX logo 0.0)
      (.add sidebar logo))
    
    (.add sidebar (Box/createVerticalStrut 16))
    
    ;; Nav buttons - full labels, clean styling
    (doseq [[label card-name] [["Dashboard" "dashboard"]
                               ["Scan Files" "scan"]
                               ["All Models" "models"]
                               ["News Analyzer" "news"]
                               ["Decisions" "decisions"]
                               ["Case Studies" "cases"]
                               ["Effectiveness" "effectiveness"]
                               ["Signals" "signals"]
                               ["Knowledge Graph" "graph"]
                               ["Sync Status" "sync"]
                               ["Watch Folders" "watch"]
                               ["Activity Log" "logs"]
                               ["Settings" "settings"]]]
      (let [btn (JButton. label)]
        (.setFont btn (:body fonts))
        (.setForeground btn (:text-secondary colors))
        (.setBackground btn (:bg-primary colors))
        (.setBorderPainted btn false)
        (.setFocusPainted btn false)
        (.setHorizontalAlignment btn JButton/LEFT)
        (.setMaximumSize btn (Dimension. 160 32))   ;; Comfortable height
        (.setPreferredSize btn (Dimension. 160 32))
        (.setAlignmentX btn 0.0)
        (.setMargin btn (Insets. 4 8 4 8))
        (.setCursor btn (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
        (.addActionListener btn
          (reify ActionListener
            (actionPerformed [_ _]
              (.show card-layout content-panel card-name)
              (when (= card-name "dashboard")
                (refresh-dashboard!)))))
        (.add sidebar btn)
        (.add sidebar (Box/createVerticalStrut 4))))  ;; Minimal gap
    
    (.add sidebar (Box/createVerticalStrut 16))
    
    ;; Web App Quick Links section
    (let [web-section (JPanel.)
          web-layout (BoxLayout. web-section BoxLayout/Y_AXIS)]
      (.setLayout web-section web-layout)
      (.setOpaque web-section false)
      (.setAlignmentX web-section 0.0)
      
      (let [section-lbl (JLabel. "WEB APP")]
        (.setFont section-lbl (:label fonts))
        (.setForeground section-lbl (:text-muted colors))
        (.setAlignmentX section-lbl 0.0)
        (.add web-section section-lbl))
      
      (.add web-section (Box/createVerticalStrut 4))
      
      (doseq [[label path] [["📊 Dashboard" "/dashboard"]
                            ["🧠 All Models" "/models"]
                            ["📈 Analytics" "/analytics"]
                            ["⚙️ Settings" "/settings"]]]
        (let [btn (JButton. label)]
          (.setFont btn (:small fonts))
          (.setForeground btn (:accent colors))
          (.setBackground btn (:bg-primary colors))
          (.setBorderPainted btn false)
          (.setFocusPainted btn false)
          (.setHorizontalAlignment btn JButton/LEFT)
          (.setMaximumSize btn (Dimension. 160 28))
          (.setPreferredSize btn (Dimension. 160 28))
          (.setAlignmentX btn 0.0)
          (.setMargin btn (Insets. 2 8 2 8))
          (.setCursor btn (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
          (.addActionListener btn
            (reify ActionListener
              (actionPerformed [_ _]
                (try
                  (let [base-url (or (get-in @*state [:settings :web-app-url])
                                    (:web-app-url config))
                        full-url (str base-url path)]
                    (.browse (Desktop/getDesktop) (URI. full-url)))
                  (catch Exception e
                    (log! :error (str "Failed to open web: " (.getMessage e))))))))
          (.add web-section btn)
          (.add web-section (Box/createVerticalStrut 2))))
      
      (.add sidebar web-section))
    
    (.add sidebar (Box/createVerticalGlue))
    
    ;; Connection status - ultra compact single row
    (let [conn-row (JPanel. (FlowLayout. FlowLayout/LEFT 1 0))]
      (.setOpaque conn-row false)
      (.setAlignmentX conn-row 0.0)
      (doseq [[name key] [["LM Studio" :lm-studio] ["Web App" :web-app] ["Slack" :slack] ["GitHub" :github]]]
        (let [lbl (JLabel. (str "● " name))]
          (.setFont lbl (:micro fonts))
          (add-watch *state (keyword (str "sb-" name))
            (fn [_ _ _ new-state]
              (SwingUtilities/invokeLater
                #(.setForeground lbl
                   (if (= :connected (get-in new-state [:connections key]))
                     (:success colors)
                     (:danger colors))))))
          (.setForeground lbl (:danger colors))
          (.add conn-row lbl)))
      (.add sidebar conn-row))
    
    ;; Web link - compact
    (.add sidebar (Box/createVerticalStrut 3))
    (let [web-btn (JButton. "Web")]
      (.setFont web-btn (:micro fonts))
      (.setForeground web-btn (:primary colors))
      (.setBackground web-btn (:sidebar-bg colors))
      (.setBorderPainted web-btn false)
      (.setMaximumSize web-btn (Dimension. 85 14))
      (.setAlignmentX web-btn 0.0)
      (.setMargin web-btn (Insets. 0 2 0 2))
      (.addActionListener web-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (.browse (Desktop/getDesktop) (URI. (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))))
              (catch Exception _)))))
      (.add sidebar web-btn))
    
    sidebar))

;; =============================================================================