(ns mental-models.desktop.gui.swing.web-app-data-part1
  "Swing App - Web App Data (Part 1)"
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

;; Case Studies Panel (fetches from web app)
;; =============================================================================

;; =============================================================================
;; Web App Data Fetching Functions
;; =============================================================================

(defn fetch-case-studies []
  "Fetch case studies from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/caseStudies.list")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (log! "[CASE-STUDIES] Fetching from web app...")
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] [])
        []))
    (catch Exception e
      (log! (str "[CASE-STUDIES] Error fetching: " (.getMessage e)))
      [])))

(defn fetch-signals []
  "Fetch signals from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/signals.list?input=%7B%22limit%22%3A100%7D")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (log! "[SIGNALS] Fetching from web app...")
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] [])
        []))
    (catch Exception e
      (log! (str "[SIGNALS] Error fetching: " (.getMessage e)))
      [])))

(defn fetch-effectiveness []
  "Fetch model effectiveness data from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/effectiveness.stats")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (log! "[EFFECTIVENESS] Fetching from web app...")
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] {})
        {}))
    (catch Exception e
      (log! (str "[EFFECTIVENESS] Error fetching: " (.getMessage e)))
      {})))

(defn fetch-top-combinations []
  "Fetch top model combinations from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/effectiveness.topCombinations?input=%7B%22limit%22%3A10%7D")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] [])
        []))
    (catch Exception e
      (log! (str "[COMBINATIONS] Error fetching: " (.getMessage e)))
      [])))

(defn fetch-knowledge-graph []
  "Fetch knowledge graph from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/knowledgeGraph.get")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (log! "[KNOWLEDGE-GRAPH] Fetching from web app...")
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] {:nodes [] :edges []})
        {:nodes [] :edges []}))
    (catch Exception e
      (log! (str "[KNOWLEDGE-GRAPH] Error fetching: " (.getMessage e)))
      {:nodes [] :edges []})))

(defn fetch-dashboard-stats []
  "Fetch dashboard stats from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/dashboard.stats")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] {})
        {}))
    (catch Exception e
      (log! (str "[DASHBOARD] Error fetching: " (.getMessage e)))
      {})))

(defn fetch-lollapalooza-events []
  "Fetch lollapalooza events from web app API"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/lollapalooza.recent")
          result (http-get url {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (if (:success result)
        (get-in (json/parse-string (:body result) :key-fn keyword) [:result :data :json] [])
        []))
    (catch Exception e
      (log! (str "[LOLLAPALOOZA] Error fetching: " (.getMessage e)))
      [])))

(defn sync-to-web-app-enhanced [scan-result]
  "Enhanced sync to web app with full data"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/desktop.syncResults")
          body (json/generate-string {:results scan-result})
          result (http-post url body {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"
                                                "Content-Type" "application/json"}})]
      (log! (str "[SYNC] Result: " (:success result)))
      (:success result))
    (catch Exception e
      (log! (str "[SYNC] Error: " (.getMessage e)))
      false)))

(defn create-case-studies-panel []
  "Panel showing historical case studies from web app"
  (let [panel (JPanel. (BorderLayout. 20 20))
        table-model (DefaultTableModel.
                     (into-array ["Case Study" "Category" "Models" "Lollapalooza"])
                     0)
        table (JTable. table-model)
        refresh-btn (JButton. "🔄 Refresh from Web App")
        detail-area (JTextArea. 10 50)]
    
    (.setBackground panel (:bg-secondary colors))
    (.setBorder panel (EmptyBorder. 30 30 30 30))
    
    ;; Title
    (let [header (JPanel. (BorderLayout.))
          title (JLabel. "📚 Case Studies")]
      (.setOpaque header false)
      (.setFont title (:title fonts))
      (.setForeground title (:text-primary colors))
      (.add header title BorderLayout/WEST)
      (.add header refresh-btn BorderLayout/EAST)
      (.add panel header BorderLayout/NORTH))
    
    ;; Built-in case studies
    (doseq [[name cat models lolla] [["Enron Collapse (2001)" "Corporate Fraud" "Incentive Bias, Denial, Authority" "Yes"]
                                     ["2008 Financial Crisis" "Market Bubble" "Social Proof, FOMO, Leverage" "Yes"]
                                     ["Theranos Fraud" "Corporate Fraud" "Authority Bias, Denial, Overconfidence" "Yes"]
                                     ["Berkshire Hathaway" "Success Story" "Margin of Safety, Circle of Competence" "No"]
                                     ["Amazon Growth" "Success Story" "Network Effects, Scale Economics" "Yes"]
                                     ["WeWork Collapse" "Corporate Fraud" "Overconfidence, Incentive Bias" "Yes"]
                                     ["Dot-com Bubble" "Market Bubble" "Social Proof, FOMO, Greater Fool" "Yes"]
                                     ["Long-Term Capital" "Market Bubble" "Overconfidence, Leverage Risk" "Yes"]]]
      (.addRow table-model (into-array Object [name cat models lolla])))
    