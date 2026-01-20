(ns mental-models.desktop.gui.swing.theme
  "Swing App - Theme"
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

;; UI Theme
;; =============================================================================

(def colors
  {:bg-primary (Color. 255 255 255)
   :bg-secondary (Color. 248 250 252)
   :bg-tertiary (Color. 241 245 249)
   :text-primary (Color. 15 23 42)
   :text-secondary (Color. 71 85 105)
   :text-muted (Color. 148 163 184)
   :border (Color. 226 232 240)
   :primary (Color. 99 102 241)
   :success (Color. 34 197 94)
   :warning (Color. 245 158 11)
   :danger (Color. 239 68 68)
   :sidebar-bg (Color. 248 250 252)
   :sidebar-text (Color. 15 23 42)})

;; VALUE LINE STYLE - Information Dense Fonts

;; Theme toggle - switch between Gitmos dark and light mode
(def *dark-mode (atom true))

(defn toggle-theme! []
  "Toggle between dark and light mode"
  (swap! *dark-mode not))

(def fonts
  {:title (Font. "Segoe UI" Font/BOLD 22)      ;; Large title
   :subtitle (Font. "Segoe UI" Font/BOLD 18)   ;; Large subtitle
   :heading (Font. "Segoe UI" Font/BOLD 16)    ;; Clear heading
   :body (Font. "Segoe UI" Font/PLAIN 14)      ;; Readable body text
   :small (Font. "Segoe UI" Font/PLAIN 13)     ;; Small but clear
   :mono (Font. "Consolas" Font/PLAIN 14)      ;; Readable monospace
   :data (Font. "Consolas" Font/PLAIN 14)      ;; Readable data tables
   :micro (Font. "Segoe UI" Font/PLAIN 12)})   ;; Labels - readable

;; =============================================================================