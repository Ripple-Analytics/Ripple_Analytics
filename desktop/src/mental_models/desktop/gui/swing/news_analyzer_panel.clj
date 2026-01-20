(ns mental-models.desktop.gui.swing.news-analyzer-panel
  "Swing App - News Analyzer Panel"
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

;; News Analyzer Panel - Predictive Mental Models
;; =============================================================================

(defn create-news-panel []
  "News analyzer with pattern overlap and outcome prediction"
  (let [panel (JPanel. (BorderLayout. 15 15))
        input-panel (JPanel. (BorderLayout. 10 10))
        results-panel (JPanel. (BorderLayout. 10 10))
        
        headline-field (JTextField. 60)
        content-area (JTextArea. 8 60)
        analyze-btn (JButton. "🔍 Analyze Story")
        clear-btn (JButton. "Clear")
        
        results-area (JTextArea. 20 60)
        
        ;; Stats labels
        danger-label (JLabel. "Danger: 0%")
        success-label (JLabel. "Success: 0%")
        overlap-label (JLabel. "Pattern Overlap: 0%")
        risk-label (JLabel. "Risk: --")
        models-label (JLabel. "Models: 0")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 20 20 20 20))
    
    ;; Title
    (let [title-panel (JPanel. (BorderLayout.))
          title (JLabel. "📰 News Story Analyzer")
          subtitle (JLabel. "Detect mental models, predict outcomes, show pattern overlap")]
      (.setOpaque title-panel false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.setFont subtitle (:small fonts))
      (.setForeground subtitle (:text-muted colors))
      (.add title-panel title BorderLayout/NORTH)
      (.add title-panel subtitle BorderLayout/SOUTH)
      (.add panel title-panel BorderLayout/NORTH))
    
    ;; Input section
    (.setOpaque input-panel false)
    (.setBorder input-panel (BorderFactory/createTitledBorder "Enter News Story"))
    
    (let [headline-panel (JPanel. (BorderLayout. 5 5))
          headline-lbl (JLabel. "Headline:")]
      (.setOpaque headline-panel false)
      (.setFont headline-lbl (:body fonts))
      (.add headline-panel headline-lbl BorderLayout/WEST)
      (.add headline-panel headline-field BorderLayout/CENTER)
      (.add input-panel headline-panel BorderLayout/NORTH))
    
    (.setLineWrap content-area true)
    (.setWrapStyleWord content-area true)
    (.setBorder content-area (BorderFactory/createTitledBorder "Content/Body"))
    (.add input-panel (JScrollPane. content-area) BorderLayout/CENTER)
    
    ;; Buttons
    (let [btn-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 5))]
      (.setOpaque btn-panel false)
      (.setBackground analyze-btn (:primary colors))
      (.setForeground analyze-btn Color/WHITE)
      (.setFont analyze-btn (:body fonts))
      (.add btn-panel analyze-btn)
      (.add btn-panel clear-btn)
      (.add input-panel btn-panel BorderLayout/SOUTH))
    
    ;; Results section
    (.setOpaque results-panel false)
    (.setBorder results-panel (BorderFactory/createTitledBorder "Analysis Results"))
    
    ;; Stats bar at top of results
    (let [stats-panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 5))]
      (.setOpaque stats-panel false)
      (doseq [lbl [danger-label success-label overlap-label risk-label models-label]]
        (.setFont lbl (:body fonts))
        (.add stats-panel lbl))
      (.setForeground danger-label (Color. 220 50 50))
      (.setForeground success-label (Color. 50 180 50))
      (.setForeground risk-label (:accent colors))
      (.add results-panel stats-panel BorderLayout/NORTH))
    
    (.setEditable results-area false)
    (.setFont results-area (Font. "Monospaced" Font/PLAIN 11))
    (.setBackground results-area (:bg-primary colors))
    (.setForeground results-area (:text-primary colors))
    (.add results-panel (JScrollPane. results-area) BorderLayout/CENTER)
    
    ;; Analyze button action
    (.addActionListener analyze-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [headline (.getText headline-field)
                content (.getText content-area)]
            (when (and (not (str/blank? headline)) (not (str/blank? content)))
              (try
                (let [analysis (news/analyze-story headline content)
                      report (news/format-analysis-report analysis)]
                  ;; Update stats
                  (.setText danger-label (format "Danger: %.0f%%" (* 100 (:danger-score analysis))))
                  (.setText success-label (format "Success: %.0f%%" (* 100 (:success-score analysis))))
                  (.setText overlap-label (format "Pattern Overlap: %.1f%%" (double (:pattern-overlap-pct analysis))))
                  (.setText risk-label (str "Risk: " (name (:overall-risk analysis))))
                  (.setText models-label (str "Models: " (:model-count analysis)))
                  ;; Update results
                  (.setText results-area report)
                  (.setCaretPosition results-area 0)
                  (log! (str "[NEWS] Analyzed: " headline " - " (:model-count analysis) " models detected")))
                (catch Exception e
                  (.setText results-area (str "Error analyzing story: " (.getMessage e)))
                  (log! :error (str "[NEWS] Analysis error: " (.getMessage e))))))))))
    
    ;; Clear button action
    (.addActionListener clear-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (.setText headline-field "")
          (.setText content-area "")
          (.setText results-area "")
          (.setText danger-label "Danger: 0%")
          (.setText success-label "Success: 0%")
          (.setText overlap-label "Pattern Overlap: 0%")
          (.setText risk-label "Risk: --")
          (.setText models-label "Models: 0"))))
    
    ;; Layout - split pane
    (let [split (javax.swing.JSplitPane. javax.swing.JSplitPane/VERTICAL_SPLIT input-panel results-panel)]
      (.setDividerLocation split 200)
      (.setResizeWeight split 0.3)
      (.add panel split BorderLayout/CENTER))
    
    panel))

;; =============================================================================