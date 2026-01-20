(ns mental-models.desktop.gui.swing.version-utils
  "Swing App - Version Utils"
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

;; Version Comparison
;; =============================================================================

(defn parse-version [v]
  (when v
    (let [cleaned (str/replace (str v) #"^v" "")]
      (mapv #(Integer/parseInt %) (str/split cleaned #"\.")))))

(defn version-newer? [remote local]
  (try
    (let [r (parse-version remote)
          l (parse-version local)]
      (when (and r l)
        (loop [r r l l]
          (cond
            (and (empty? r) (empty? l)) false
            (empty? r) false
            (empty? l) true
            (> (first r) (first l)) true
            (< (first r) (first l)) false
            :else (recur (rest r) (rest l))))))
    (catch Exception _ false)))

;; =============================================================================