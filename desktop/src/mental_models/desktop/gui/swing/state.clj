(ns mental-models.desktop.gui.swing.state
  "Swing App - State"
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

;; State Management
;; =============================================================================

(def *state (atom {:logs []
                   :scan-results []
                   :watch-active false
                   :settings {:github-token nil  ;; Not needed with Google Drive
                              :slack-webhook nil
                              :lm-studio-url "http://localhost:1234"
                              :web-app-url nil}
                   :connections {:lm-studio :disconnected
                                 :web-app :disconnected
                                 :slack :disconnected
                                 :github :disconnected}
                   :update {:available false
                            :latest-version nil
                            :download-url nil
                            :checking false}
                   :stats {:files-scanned 0
                           :models-found 0
                           :documents 0
                           :lollapalooza 0}}))

(defn log! [message]
  (let [timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "HH:mm:ss"))
        entry (str "[" timestamp "] " message)]
    (println entry)
    (swap! *state update :logs #(take 1000 (conj % entry)))))

;; =============================================================================