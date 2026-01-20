(ns mental-models.desktop.gui.swing.watch-panel
  "Swing App - Watch Panel"
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

;; Watch Panel
;; =============================================================================

(defn create-watch-panel []
  (let [panel (JPanel. (BorderLayout. 20 20))
        content (JPanel. (BorderLayout. 15 15))
        folder-list-model (DefaultListModel.)
        folder-list (JList. folder-list-model)
        add-btn (JButton. "+ Add Folder")
        start-btn (JButton. "▶ Start Watching")
        stop-btn (JButton. "⏹ Stop")
        status-label (JLabel. "Status: Idle")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Watch Mode")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    (.setOpaque content false)
    
    ;; Description
    (let [desc (JLabel. "<html>Watch mode monitors folders and automatically scans new files for mental models.<br>Add folders below and click Start to begin continuous monitoring.</html>")]
      (.setFont desc (:body fonts))
      (.add content desc BorderLayout/NORTH))
    
    ;; Folder list
    (let [list-panel (JPanel. (BorderLayout. 0 8))]
      (.setOpaque list-panel false)
      (.setBorder list-panel (TitledBorder. "Watched Folders"))
      (.setVisibleRowCount folder-list 5)
      (.add list-panel (JScrollPane. folder-list) BorderLayout/CENTER)
      
      ;; Add/Remove buttons
      (let [btn-panel (JPanel. (FlowLayout. FlowLayout/LEFT))]
        (.setOpaque btn-panel false)
        (.addActionListener add-btn
          (reify ActionListener
            (actionPerformed [_ _]
              (let [chooser (JFileChooser.)]
                (.setFileSelectionMode chooser JFileChooser/DIRECTORIES_ONLY)
                (when (= (.showOpenDialog chooser panel) JFileChooser/APPROVE_OPTION)
                  (.addElement folder-list-model (.getAbsolutePath (.getSelectedFile chooser))))))))
        (.add btn-panel add-btn)
        (.add list-panel btn-panel BorderLayout/SOUTH))
      (.add content list-panel BorderLayout/CENTER))
    
    ;; Control panel
    (let [control-panel (JPanel. (FlowLayout. FlowLayout/LEFT 10 0))]
      (.setOpaque control-panel false)
      (.setFont status-label (:body fonts))
      (.setEnabled stop-btn false)
      
      (.addActionListener start-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (let [folders (for [i (range (.getSize folder-list-model))]
                           (.getElementAt folder-list-model i))]
              (when (seq folders)
                (start-watch-mode! (vec folders) log!)
                (.setText status-label "Status: Watching...")
                (.setEnabled start-btn false)
                (.setEnabled stop-btn true))))))
      
      (.addActionListener stop-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (stop-watch-mode! log!)
            (.setText status-label "Status: Stopped")
            (.setEnabled start-btn true)
            (.setEnabled stop-btn false))))
      
      (.setBackground start-btn (:success colors))
      (.setForeground start-btn Color/WHITE)
      (.add control-panel start-btn)
      (.add control-panel stop-btn)
      (.add control-panel (Box/createHorizontalStrut 20))
      (.add control-panel status-label)
      (.add content control-panel BorderLayout/SOUTH))
    
    (.add panel content BorderLayout/CENTER)
    panel))

;; =============================================================================