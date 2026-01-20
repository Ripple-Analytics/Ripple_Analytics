(ns mental-models.desktop.gui.swing.knowledge-graph-panel
  "Swing App - Knowledge Graph Panel"
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

;; Knowledge Graph Panel
;; =============================================================================

(defn create-knowledge-graph-panel []
  "Panel showing knowledge graph visualization of mental model connections"
  (let [panel (JPanel. (BorderLayout. 20 20))
        canvas (JPanel.)
        refresh-btn (JButton. "🔄 Refresh Graph")
        zoom-in-btn (JButton. "+")
        zoom-out-btn (JButton. "-")
        zoom-level (atom 1.0)
        
        ;; Sample graph data - nodes and edges
        nodes (atom [{:id 1 :label "Incentive Bias" :x 200 :y 150 :category "Psychology"}
                     {:id 2 :label "Social Proof" :x 350 :y 100 :category "Psychology"}
                     {:id 3 :label "Sunk Cost" :x 300 :y 250 :category "Economics"}
                     {:id 4 :label "Confirmation Bias" :x 450 :y 200 :category "Psychology"}
                     {:id 5 :label "Margin of Safety" :x 150 :y 300 :category "Investing"}
                     {:id 6 :label "Circle of Competence" :x 500 :y 350 :category "Investing"}
                     {:id 7 :label "Leverage" :x 400 :y 400 :category "Economics"}
                     {:id 8 :label "Network Effects" :x 250 :y 400 :category "Systems"}])
        edges (atom [[1 2] [1 3] [2 4] [3 5] [4 6] [5 7] [6 7] [7 8] [1 8]])]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title and controls
    (let [header (JPanel. (BorderLayout.))
          title (JLabel. "🕸️ Knowledge Graph")
          controls (JPanel. (FlowLayout. FlowLayout/RIGHT))]
      (.setOpaque header false)
      (.setOpaque controls false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add controls zoom-out-btn)
      (.add controls zoom-in-btn)
      (.add controls refresh-btn)
      (.add header title BorderLayout/WEST)
      (.add header controls BorderLayout/EAST)
      (.add panel header BorderLayout/NORTH))
    
    ;; Canvas for drawing graph
    (let [graph-canvas (proxy [JPanel] []
                        (paintComponent [g]
                          (proxy-super paintComponent g)
                          (let [g2d (cast Graphics2D g)
                                zoom @zoom-level]
                            ;; Enable anti-aliasing
                            (.setRenderingHint g2d RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                            
                            ;; Draw edges
                            (.setColor g2d (Color. 200 200 200))
                            (.setStroke g2d (BasicStroke. 2))
                            (doseq [[from to] @edges]
                              (let [n1 (first (filter #(= (:id %) from) @nodes))
                                    n2 (first (filter #(= (:id %) to) @nodes))]
                                (when (and n1 n2)
                                  (.drawLine g2d 
                                    (int (* zoom (:x n1))) (int (* zoom (:y n1)))
                                    (int (* zoom (:x n2))) (int (* zoom (:y n2)))))))
                            
                            ;; Draw nodes
                            (doseq [node @nodes]
                              (let [x (int (* zoom (:x node)))
                                    y (int (* zoom (:y node)))
                                    color (case (:category node)
                                           "Psychology" (Color. 99 102 241)
                                           "Economics" (Color. 34 197 94)
                                           "Investing" (Color. 234 179 8)
                                           "Systems" (Color. 239 68 68)
                                           (Color. 148 163 184))]
                                ;; Node circle
                                (.setColor g2d color)
                                (.fillOval g2d (- x 20) (- y 20) 40 40)
                                (.setColor g2d Color/WHITE)
                                (.setStroke g2d (BasicStroke. 2))
                                (.drawOval g2d (- x 20) (- y 20) 40 40)
                                ;; Label
                                (.setColor g2d (:text-primary colors))
                                (.setFont g2d (:small fonts))
                                (let [fm (.getFontMetrics g2d)
                                      label (:label node)
                                      lw (.stringWidth fm label)]
                                  (.drawString g2d label (- x (/ lw 2)) (+ y 35))))))))]
      (.setBackground graph-canvas (:bg-primary colors))
      (.setPreferredSize graph-canvas (Dimension. 800 600))
      (.add panel (JScrollPane. graph-canvas) BorderLayout/CENTER)
      
      ;; Zoom controls
      (.addActionListener zoom-in-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (swap! zoom-level #(min 2.0 (+ % 0.1)))
            (.repaint graph-canvas))))
      
      (.addActionListener zoom-out-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (swap! zoom-level #(max 0.5 (- % 0.1)))
            (.repaint graph-canvas)))))
    
    ;; Legend
    (let [legend (JPanel. (FlowLayout. FlowLayout/LEFT 15 5))]
      (.setOpaque legend false)
      (doseq [[cat color] [["Psychology" "#6366f1"] ["Economics" "#22c55e"] 
                           ["Investing" "#eab308"] ["Systems" "#ef4444"]]]
        (let [lbl (JLabel. (str "● " cat))]
          (.setFont lbl (:small fonts))
          (.add legend lbl)))
      (.add panel legend BorderLayout/SOUTH))
    
    panel))

;; =============================================================================