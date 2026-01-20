(ns mental-models.desktop.gui.swing.http-client
  "Swing App - Http Client"
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

;; HTTP Client
;; =============================================================================

(defn http-get [url & {:keys [headers]}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)
            body (when (< status 400)
                   (slurp (.getInputStream conn)))]
        (.disconnect conn)
        {:success (< status 400) :status status :body body}))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn http-post [url body & {:keys [headers]}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 60000)
                 (.setRequestProperty "Content-Type" "application/json"))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (with-open [out (.getOutputStream conn)]
        (.write out (.getBytes body "UTF-8")))
      (let [status (.getResponseCode conn)
            response-body (try (slurp (.getInputStream conn)) (catch Exception _ nil))]
        (.disconnect conn)
        {:success (< status 400) :status status :body response-body}))
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================