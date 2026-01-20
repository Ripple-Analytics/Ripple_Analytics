(ns mental-models.desktop.gui.swing.main-frame
  "Swing App - Main Frame"
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

;; Main Frame
;; =============================================================================

(defn create-main-frame []
  (let [frame (JFrame. (str (:app-name config) " v" (:version config)))
        content-panel (JPanel. (CardLayout.))
        card-layout (.getLayout content-panel)]
    
    (try
      (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
      (catch Exception _))
    
    (.add content-panel (create-dashboard-panel) "dashboard")
    (.add content-panel (create-scan-panel frame) "scan")
    (.add content-panel (create-models-panel) "models")
    (.add content-panel (create-news-panel) "news")
    (.add content-panel (create-decisions-panel) "decisions")
    (.add content-panel (create-case-studies-panel) "cases")
    (.add content-panel (create-effectiveness-panel) "effectiveness")
    (.add content-panel (create-signals-panel) "signals")
    (.add content-panel (create-knowledge-graph-panel) "graph")
    (.add content-panel (create-sync-panel) "sync")
    (.add content-panel (create-watch-panel) "watch")
    (.add content-panel (create-logs-panel) "logs")
    (.add content-panel (create-settings-panel frame) "settings")
    
    (let [sidebar (create-sidebar card-layout content-panel frame)]
      (.add (.getContentPane frame) sidebar BorderLayout/WEST)
      (.add (.getContentPane frame) content-panel BorderLayout/CENTER))
    
    ;; Menu bar
    (let [menubar (JMenuBar.)
          file-menu (JMenu. "File")
          view-menu (JMenu. "View")
          help-menu (JMenu. "Help")
          
          exit-item (JMenuItem. "Exit")
          updates-item (JMenuItem. "Check for Updates...")
          about-item (JMenuItem. "About")]
      
      (.addActionListener exit-item
        (reify ActionListener
          (actionPerformed [_ _] (System/exit 0))))
      
      (.addActionListener updates-item
        (reify ActionListener
          (actionPerformed [_ _]
            (check-for-updates! frame))))
      
      (let [feature-item (JMenuItem. "Request a Feature...")]
        (.addActionListener feature-item
          (reify ActionListener
            (actionPerformed [_ _]
              (show-feature-request-dialog! frame))))
        (.add help-menu feature-item))
      
      (.addActionListener about-item
        (reify ActionListener
          (actionPerformed [_ _]
            (JOptionPane/showMessageDialog
              frame
              (str (:app-name config) "\n"
                   "Version: " (:version config) "\n\n"
                   "One-click mental model detection.\n"
                   "Scan documents, find patterns, track insights.\n\n"
                   "© 2025 Ripple Analytics")
              "About"
              JOptionPane/INFORMATION_MESSAGE))))
      
      (.add file-menu exit-item)
      (.add help-menu updates-item)
      (.addSeparator help-menu)
      (.add help-menu about-item)
      
      (.add menubar file-menu)
      (.add menubar help-menu)
      (let [theme-item (JMenuItem. "Toggle Dark/Light Mode")
            fullscreen-item (JMenuItem. "Toggle Fullscreen")]
        (.addActionListener theme-item
          (reify ActionListener
            (actionPerformed [_ _]
              (toggle-theme!)
              ;; Refresh the UI - recreate panels
              (javax.swing.JOptionPane/showMessageDialog 
                frame 
                "Theme changed. Restart app to apply fully."
                "Theme Toggle"
                javax.swing.JOptionPane/INFORMATION_MESSAGE))))
        (.addActionListener fullscreen-item
          (reify ActionListener
            (actionPerformed [_ _]
              (let [device (.getDefaultScreenDevice (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))]
                (if (.getFullScreenWindow device)
                  (.setFullScreenWindow device nil)
                  (.setFullScreenWindow device frame))))))
        (.add view-menu theme-item)
        (.add view-menu fullscreen-item))
      (.add menubar view-menu)
      (.setJMenuBar frame menubar))
    
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setSize frame 1100 750)
    (.setMinimumSize frame (Dimension. 900 600))
    (.setLocationRelativeTo frame nil)
    
    frame))

;; =============================================================================