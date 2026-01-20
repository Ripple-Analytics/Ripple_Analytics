(ns mental-models.desktop.gui.swing.watch-mode
  "Swing App - Watch Mode"
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

;; Watch Mode
;; =============================================================================

(def *watch-thread (atom nil))

(defn start-watch-mode! [folders log-fn]
  (log-fn "[WATCH] Starting watch mode...")
  (swap! *state assoc :watch-active true)
  (reset! *watch-thread
    (future
      (while (:watch-active @*state)
        (doseq [folder folders]
          (scan-folder folder
            (fn [result]
              (when (:models result)
                (log-fn (str "[WATCH] Found " (count (:models result)) " models in " (:file result)))))
            nil))
        (Thread/sleep 30000)))))

(defn stop-watch-mode! [log-fn]
  (log-fn "[WATCH] Stopping watch mode...")
  (swap! *state assoc :watch-active false)
  (when-let [t @*watch-thread]
    (future-cancel t))
  (reset! *watch-thread nil))

;; =============================================================================