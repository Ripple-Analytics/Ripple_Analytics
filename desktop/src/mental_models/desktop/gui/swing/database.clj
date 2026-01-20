(ns mental-models.desktop.gui.swing.database
  "Swing App - Database"
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

;; Database (SQLite)
;; =============================================================================

(def db-path (str (System/getProperty "user.home") "/.mental-models/data.db"))

(defn get-connection []
  (let [db-dir (File. (str (System/getProperty "user.home") "/.mental-models"))]
    (when-not (.exists db-dir)
      (.mkdirs db-dir))
    (Class/forName "org.sqlite.JDBC")
    (DriverManager/getConnection (str "jdbc:sqlite:" db-path))))

(defn init-database! []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)]
      (.execute stmt "CREATE TABLE IF NOT EXISTS scan_results (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        file_path TEXT NOT NULL,
                        models_found TEXT,
                        scan_date TEXT,
                        confidence REAL)")
      (.execute stmt "CREATE TABLE IF NOT EXISTS settings (
                        key TEXT PRIMARY KEY,
                        value TEXT)")
      (log! "[DB] Database initialized"))
    (catch Exception e
      (log! (str "[DB] Init error: " (.getMessage e))))))

(defn save-setting! [key value]
  (try
    (with-open [conn (get-connection)
                stmt (.prepareStatement conn 
                       "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)")]
      (.setString stmt 1 (name key))
      (.setString stmt 2 (str value))
      (.executeUpdate stmt))
    (catch Exception e
      (log! (str "[DB] Save setting error: " (.getMessage e))))))

(defn load-settings! []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)
                rs (.executeQuery stmt "SELECT key, value FROM settings")]
      (loop [settings {}]
        (if (.next rs)
          (recur (assoc settings (keyword (.getString rs "key")) (.getString rs "value")))
          settings)))
    (catch Exception e
      (log! (str "[DB] Load settings error: " (.getMessage e)))
      {})))

(defn save-scan-result! [file-path models confidence]
  (try
    (with-open [conn (get-connection)
                stmt (.prepareStatement conn
                       "INSERT INTO scan_results (file_path, models_found, scan_date, confidence) VALUES (?, ?, ?, ?)")]
      (.setString stmt 1 file-path)
      (.setString stmt 2 (str/join ", " models))
      (.setString stmt 3 (.format (LocalDateTime/now) (DateTimeFormatter/ISO_LOCAL_DATE_TIME)))
      (.setDouble stmt 4 confidence)
      (.executeUpdate stmt))
    (catch Exception e
      (log! (str "[DB] Save result error: " (.getMessage e))))))

(defn get-stats-from-db []
  (try
    (with-open [conn (get-connection)
                stmt (.createStatement conn)]
      (let [rs (.executeQuery stmt "SELECT COUNT(*) as count, 
                                           COUNT(DISTINCT file_path) as files,
                                           SUM(LENGTH(models_found) - LENGTH(REPLACE(models_found, ',', '')) + 1) as models
                                    FROM scan_results")]
        (when (.next rs)
          {:files-scanned (.getInt rs "files")
           :models-found (or (.getInt rs "models") 0)
           :documents (.getInt rs "count")
           :lollapalooza 0})))
    (catch Exception e
      (log! (str "[DB] Stats error: " (.getMessage e)))
      {:files-scanned 0 :models-found 0 :documents 0 :lollapalooza 0})))


;; =============================================================================