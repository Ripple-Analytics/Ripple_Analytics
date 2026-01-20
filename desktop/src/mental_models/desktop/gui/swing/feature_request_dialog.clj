(ns mental-models.desktop.gui.swing.feature-request-dialog
  "Swing App - Feature Request Dialog"
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

;; Feature Request Dialog
;; =============================================================================

(defn show-feature-request-dialog! [parent]
  (let [text-area (JTextArea. 5 40)
        scroll (JScrollPane. text-area)
        result (JOptionPane/showConfirmDialog
                 parent
                 scroll
                 "Request a Feature (sent to dev team)"
                 JOptionPane/OK_CANCEL_OPTION
                 JOptionPane/PLAIN_MESSAGE)]
    (when (= result JOptionPane/OK_OPTION)
      (let [feature (.getText text-area)]
        (when-not (str/blank? feature)
          (request-feature-from-devin! feature)
          (JOptionPane/showMessageDialog
            parent
            "Feature request sent! The dev team will review it."
            "Request Sent"
            JOptionPane/INFORMATION_MESSAGE))))))

;; =============================================================================