(ns mental-models.desktop.gui.swing.dashboard-panel-part1
  "Swing App - Dashboard Panel (Part 1)"
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

;; Dashboard Panel
;; =============================================================================

(defn refresh-dashboard! []
  (let [stats (get-stats-from-db)]
    (swap! *state assoc :stats stats)))

;; VALUE LINE STYLE - Compact stat display (no cards, just dense data)
(defn create-stat-row [label value]
  "Create a single compact stat row like Value Line"
  (let [row (JPanel. (FlowLayout. FlowLayout/LEFT 2 0))]
    (.setOpaque row false)
    (let [lbl (JLabel. (str label ":"))
          val (JLabel. (str value))]
      (.setFont lbl (:micro fonts))
      (.setForeground lbl (:text-muted colors))
      (.setFont val (:data fonts))
      (.setForeground val (:text-primary colors))
      (.add row lbl)
      (.add row val))
    row))

(defn create-dense-stats-table []
  "Create Value Line style dense statistics table with analytics"
  (let [table-panel (JPanel. (GridLayout. 0 4 3 1))  ;; 4 columns, tight spacing
        stats (:stats @*state)
        analytics-stats (analytics/get-stats)
        health (monitor/get-health-status)
        anomalies (anomaly/anomaly-summary)]
    (.setOpaque table-panel false)
    (.setBorder table-panel (BorderFactory/createTitledBorder 
                              (BorderFactory/createLineBorder (:border colors) 1)
                              "STATISTICS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    
    ;; Row 1: Core counts
    (.add table-panel (create-stat-row "Files" (:total-files-scanned analytics-stats 0)))
    (.add table-panel (create-stat-row "Models" (:total-models-found analytics-stats 0)))
    (.add table-panel (create-stat-row "Unique" (:unique-models analytics-stats 0)))
    (.add table-panel (create-stat-row "Scans" (:total-scans analytics-stats 0)))
    
    ;; Row 2: Performance metrics
    (.add table-panel (create-stat-row "Files/sec" (format "%.1f" (double (:files-per-second analytics-stats 0)))))
    (.add table-panel (create-stat-row "Avg Duration" (str (int (:avg-scan-duration-ms analytics-stats 0)) "ms")))
    (.add table-panel (create-stat-row "Scans/hr" (format "%.1f" (double (:scans-per-hour analytics-stats 0)))))
    (.add table-panel (create-stat-row "Uptime" (format "%.1fh" (double (:uptime-hours analytics-stats 0)))))
    
    ;; Row 3: Health and anomalies
    (.add table-panel (create-stat-row "Health" (str (:score health 100) "%")))
    (.add table-panel (create-stat-row "Status" (name (:status health :healthy))))
    (.add table-panel (create-stat-row "Anomalies" (:total-anomalies anomalies 0)))
    (.add table-panel (create-stat-row "Alerts" (:active-alerts anomalies 0)))
    
    ;; Row 4: Version info
    (.add table-panel (create-stat-row "Version" (:version config)))
    (.add table-panel (create-stat-row "P95 Duration" (str (int (:p95-duration-ms analytics-stats 0)) "ms")))
    (.add table-panel (create-stat-row "Lollapalooza" (:lollapalooza stats 0)))
    (.add table-panel (create-stat-row "Detection %" (str (int (* 100 (/ (double (:total-models-found analytics-stats 1)) (max 1 (:total-files-scanned analytics-stats 1))))) "%")))
    
    table-panel))

(defn create-recent-scans-table []
  "Create compact recent scans list"
  (let [panel (JPanel. (BorderLayout. 0 0))
        table-data (to-array-2d (take 10 (map (fn [r] 
                                                [(subs (:file r "") 0 (min 25 (count (:file r ""))))
                                                 (count (:models r []))
                                                 (if (:lollapalooza r) "⚡" "-")])
                                              (:scan-results @*state))))
        columns (into-array ["File" "Models" "Lollapalooza"])
        table (javax.swing.JTable. table-data columns)]
    (.setFont table (:data fonts))
    (.setRowHeight table 12)
    (.setShowGrid table true)
    (.setGridColor table (:border colors))
    (.setPreferredWidth (.getColumn (.getColumnModel table) 0) 150)
    (.setPreferredWidth (.getColumn (.getColumnModel table) 1) 25)
    (.setPreferredWidth (.getColumn (.getColumnModel table) 2) 20)
    (.setBorder panel (BorderFactory/createTitledBorder 
                        (BorderFactory/createLineBorder (:border colors) 1)
                        "RECENT SCANS" TitledBorder/LEFT TitledBorder/TOP (:micro fonts) (:text-muted colors)))
    (.add panel (JScrollPane. table) BorderLayout/CENTER)
    panel))

(defn create-connections-strip []
  "Single-line connection status strip"
  (let [strip (JPanel. (FlowLayout. FlowLayout/LEFT 5 0))]
    (.setOpaque strip false)
    (doseq [[name key color-key] [["LM Studio" :lm-studio] ["Web App" :web-app] ["GitHub" :github] ["Slack" :slack]]]
      (let [lbl (JLabel. (str "●" name))]
        (.setFont lbl (:micro fonts))
        (add-watch *state (keyword (str "strip-" name))
          (fn [_ _ _ new-state]
            (SwingUtilities/invokeLater
              #(.setForeground lbl
                 (if (= :connected (get-in new-state [:connections key]))
                   (:success colors)
                   (:danger colors))))))
        (.setForeground lbl (:danger colors))
        (.add strip lbl)))
    strip))

(defn create-dashboard-panel []
  "VALUE LINE STYLE - Maximum information density dashboard"
  (let [panel (JPanel. (BorderLayout. 2 2))  ;; Minimal gaps
        ;; Top strip: Title + Version + Status + Connections
        top-strip (JPanel. (FlowLayout. FlowLayout/LEFT 3 1))
        ;; Main content: Split into data grids
        main-panel (JPanel. (GridLayout. 2 2 2 2))  ;; 2x2 grid of data panels
        ;; Bottom: Compact action buttons
        bottom-strip (JPanel. (FlowLayout. FlowLayout/LEFT 2 1))]
    
    (.setBackground panel (:bg-primary colors))
    (.setBorder panel (EmptyBorder. 3 3 3 3))  ;; Minimal border
    
    ;; TOP STRIP - Single line header
    (.setOpaque top-strip false)
    (let [title (JLabel. "MENTAL MODELS DASHBOARD")
          version (JLabel. (str "v" (:version config)))
          update-status (JLabel. "")]
      (.setFont title (:heading fonts))
      (.setForeground title (:text-primary colors))
      (.setFont version (:micro fonts))
      (.setForeground version (:text-muted colors))
      (.setFont update-status (:micro fonts))
      
      (add-watch *state :dash-update-status
        (fn [_ _ _ new-state]
          (SwingUtilities/invokeLater
            #(let [status (get-in new-state [:update :status] "")]
               (.setText update-status status)
               (.setForeground update-status
                 (cond
                   (str/includes? status "✓") (:success colors)
                   (str/includes? status "Downloading") (:primary colors)
                   :else (:text-muted colors)))))))
      
      (.add top-strip title)
      (.add top-strip (JLabel. " | "))
      (.add top-strip version)
      (.add top-strip (JLabel. " | "))
      (.add top-strip update-status)
      (.add top-strip (JLabel. " | "))
      (.add top-strip (create-connections-strip)))
    (.add panel top-strip BorderLayout/NORTH)
    
    ;; MAIN CONTENT - 4 dense data panels
    (.setOpaque main-panel false)
    
    ;; Panel 1: Statistics Table
    (.add main-panel (create-dense-stats-table))