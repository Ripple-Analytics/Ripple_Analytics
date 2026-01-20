(ns mental-models.desktop.gui.swing.settings-panel
  "Swing App - Settings Panel"
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

;; Settings Panel
;; =============================================================================

(defn create-settings-panel [frame]
  (let [panel (JPanel. (BorderLayout. 20 20))
        form (JPanel. (GridBagLayout.))
        gbc (GridBagConstraints.)
        
        lm-field (JTextField. (get-in @*state [:settings :lm-studio-url]) 30)
        web-field (JTextField. (or (get-in @*state [:settings :web-app-url]) "") 30)
        slack-field (JTextField. (or (get-in @*state [:settings :slack-webhook]) "") 30)
        github-field (JTextField. (or (get-in @*state [:settings :github-token]) "") 30)
        save-btn (JButton. "💾 Save Settings")]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [title (JLabel. "Settings")]
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add panel title BorderLayout/NORTH))
    
    (.setOpaque form false)
    (set! (. gbc insets) (Insets. 8 8 8 8))
    (set! (. gbc anchor) GridBagConstraints/WEST)
    
    ;; LM Studio URL
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 0)
    (.add form (JLabel. "LM Studio URL:") gbc)
    (set! (. gbc gridx) 1)
    (.add form lm-field gbc)
    
    ;; Web App URL
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 1)
    (.add form (JLabel. "Web App URL:") gbc)
    (set! (. gbc gridx) 1)
    (.add form web-field gbc)
    
    ;; Slack Webhook
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 2)
    (.add form (JLabel. "Slack Webhook:") gbc)
    (set! (. gbc gridx) 1)
    (.add form slack-field gbc)
    
    ;; GitHub Token
    (set! (. gbc gridx) 0) (set! (. gbc gridy) 3)
    (.add form (JLabel. "GitHub Token:") gbc)
    (set! (. gbc gridx) 1)
    (.add form github-field gbc)
    
    ;; Save button
    (set! (. gbc gridx) 1) (set! (. gbc gridy) 4)
    (.setBackground save-btn (:primary colors))
    (.setForeground save-btn Color/WHITE)
    (.addActionListener save-btn
      (reify ActionListener
        (actionPerformed [_ _]
          (swap! *state assoc :settings
            {:lm-studio-url (.getText lm-field)
             :web-app-url (.getText web-field)
             :slack-webhook (.getText slack-field)
             :github-token (.getText github-field)})
          (save-setting! :lm-studio-url (.getText lm-field))
          (save-setting! :web-app-url (.getText web-field))
          (save-setting! :slack-webhook (.getText slack-field))
          (save-setting! :github-token (.getText github-field))
          (check-all-connections!)
          (JOptionPane/showMessageDialog frame "Settings saved!" "Success" JOptionPane/INFORMATION_MESSAGE))))
    (.add form save-btn gbc)
    
    ;; Open Web Settings button
    (let [web-settings-btn (JButton. "🌐 Open Web App Settings")]
      (set! (. gbc gridx) 1) (set! (. gbc gridy) 5)
      (.addActionListener web-settings-btn
        (reify ActionListener
          (actionPerformed [_ _]
            (try
              (let [web-url (or (.getText web-field)
                               (get-in @*state [:settings :web-app-url])
                               (:web-app-url config)
                               "https://mental-models-web.manus.space")]
                (log! (str "[WEB] Opening web settings: " web-url))
                (.browse (Desktop/getDesktop) (URI. web-url)))
              (catch Exception e
                (log! (str "[WEB] Error: " (.getMessage e))))))))
      (.add form web-settings-btn gbc))
    
    (.add panel form BorderLayout/CENTER)
    
    ;; Connection status
    (let [status-panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 5))
          make-status (fn [name key]
                       (let [label (JLabel. (str "● " name))]
                         (.setFont label (:body fonts))
                         (add-watch *state (keyword (str "conn-" name))
                           (fn [_ _ _ new-state]
                             (let [status (get-in new-state [:connections key])]
                               (SwingUtilities/invokeLater
                                 #(.setForeground label
                                    (if (= status :connected)
                                      (:success colors)
                                      (:danger colors)))))))
                         label))]
      (.setOpaque status-panel false)
      (.setBorder status-panel (TitledBorder. "Connections"))
      (.add status-panel (make-status "LM Studio" :lm-studio))
      (.add status-panel (make-status "Web App" :web-app))
      (.add status-panel (make-status "Slack" :slack))
      (.add status-panel (make-status "GitHub" :github))
      (.add panel status-panel BorderLayout/SOUTH))
    
    panel))

;; =============================================================================