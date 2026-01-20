(ns mental-models.desktop.gui.swing.config
  "Swing App - Config"
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

;; Configuration
;; =============================================================================

(def config
  {:version "v2.3.0"
   :blue-green true
   :app-name "Mental Models Desktop"
   :github-repo "Ripple-Analytics/Ripple_Analytics"
   :github-api "https://api.github.com"
   :github-token nil  ;; Not needed - using Google Drive for downloads
   ;; Google Drive update system (PRIMARY - no auth needed)
   :gdrive-releases-url "https://drive.google.com/uc?export=download&id="
   :gdrive-manifest-id "1s5TF72m8QbrMAx3eKIxnkpTps6y38V3X"
   :slack-webhook nil
   :lm-studio-url "http://localhost:1234"
   ;; Remote config for bulletproof updates (FALLBACK)
   :web-app-url "https://mental-models-web.manus.space"
   :desktop-api-key "mm-desktop-2026-ripple"
   :cache-dir (str (System/getProperty "user.home") "/.mental-models/cache")})

;; =============================================================================