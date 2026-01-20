(ns mental-models.desktop.gui.swing.decision-journal-panel
  "Swing App - Decision Journal Panel"
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

;; Decision Journal Panel
;; =============================================================================

(defn create-decisions-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        form-panel (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        decisions-model (DefaultListModel.)
        decisions-list (JList. decisions-model)
        
        title-field (JTextField. 40)
        context-area (JTextArea. 4 40)
        models-field (JTextField. 40)
        outcome-combo (JComboBox. (into-array String ["Pending" "Success" "Partial" "Failure"]))
        notes-area (JTextArea. 4 40)
        save-btn (JButton. "💾 Save Decision")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Decision Journal")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    ;; Form setup
    (.setOpaque form-panel false)
    (set! (. gbc insets) (Insets. 5 5 5 5))
    (set! (. gbc anchor) GridBagConstraints/WEST)
    
    ;; Decision Title
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 0)
    (.add form-panel (JLabel. "Decision Title:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.add form-panel title-field gbc)
    
    ;; Context
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 1) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Context:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.setLineWrap context-area true)
    (.add form-panel (JScrollPane. context-area) gbc)
    
    ;; Models Used
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 2) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Models Used (comma-sep):") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.add form-panel models-field gbc)
    
    ;; Outcome
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 3) (set! (. gbc fill) GridBagConstraints/NONE)
    (.add form-panel (JLabel. "Outcome:") gbc)
    (set! (. gbc gridx) 1)
    (.add form-panel outcome-combo gbc)
    
    ;; Notes
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 4)
    (.add form-panel (JLabel. "Notes/Learnings:") gbc)
    (set! (. gbc gridx) 1) (set! (. gbc fill) GridBagConstraints/HORIZONTAL)
    (.setLineWrap notes-area true)
    (.add form-panel (JScrollPane. notes-area) gbc)
    
    ;; Save button
    (set! (. gbc gridx) 1) (set! (. gbc gridy) 5) (set! (. gbc fill) GridBagConstraints/NONE)
    (.setBackground save-btn (:primary colors))
    (.setForeground save-btn Color/WHITE)
    (.addActionListener save-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (let [decision {:title (.getText title-field)
                         :context (.getText context-area)
                         :models (str/split (.getText models-field) #",\s*")
                         :outcome (.getSelectedItem outcome-combo)
                         :notes (.getText notes-area)
                         :timestamp (str (LocalDateTime/now))}]
            (swap! *state update :decisions (fnil conj []) decision)
            (.addElement decisions-model (str (:title decision) " - " (:outcome decision)))
            ;; Clear form
            (.setText title-field "")
            (.setText context-area "")
            (.setText models-field "")
            (.setSelectedIndex outcome-combo 0)
            (.setText notes-area "")
            (log! (str "[DECISION] Saved: " (:title decision)))))))
    (.add form-panel save-btn gbc)
    
    ;; Layout
    (let [center-panel (JPanel. (BorderLayout. 10 10))]
      (.setOpaque center-panel false)
      (.add center-panel form-panel BorderLayout/NORTH)
      
      ;; Decisions list
      (let [list-panel (JPanel. (BorderLayout.))
            list-label (JLabel. "Previous Decisions:")]
        (.setOpaque list-panel false)
        (.setFont list-label (:subtitle fonts))
        (.add list-panel list-label BorderLayout/NORTH)
        (.add list-panel (JScrollPane. decisions-list) BorderLayout/CENTER)
        (.add center-panel list-panel BorderLayout/CENTER))
      
      (.add panel center-panel BorderLayout/CENTER))
    
    panel))

;; =============================================================================