(ns mental-models.desktop.gui.swing.models-data-part7
  "Swing App - Models Data (Part 7)"
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

    :safeguards ["Scalable trust mechanisms" "Trust infrastructure" "Overcome distrust" "Trust maintenance" "Trust but verify"]}
   {:id 129
    :name "Avoiding Madness in Crowds"
    :category "Organizational & Institutional"
    :description "Resist groupthink and maintain independent judgment."
    :keywords ["independent" "avoiding" "maintain" "groupthink" "crowds" "in" "resist" "madness"]
    :failure-modes []
    :safeguards []}
  ])
(defn detect-models [text]
  "Detect mental models in text using keyword matching"
  (let [text-lower (str/lower-case (or text ""))]
    (filter
      (fn [model]
        (some #(str/includes? text-lower (str/lower-case %))
              (:keywords model)))
      mental-models)))

(defn detect-lollapalooza [models]
  "Detect if multiple models combine for Lollapalooza effect"
  (when (>= (count models) 3)
    (let [categories (set (map :category models))]
      (when (>= (count categories) 2)
        {:detected true
         :models (map :name models)
         :categories categories
         :strength (cond
                     (>= (count models) 5) :strong
                     (>= (count models) 4) :moderate
                     :else :mild)}))))

;; =============================================================================