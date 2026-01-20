(ns mental-models.desktop.gui.swing.blue-green-deployment-part1
  "Swing App - Blue Green Deployment (Part 1)"
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

;; Startup Update Check (Fully Automatic)
;; =============================================================================

;; =============================================================================
;; Continuous Update System - Delta Updates + Auto-Polling
;; =============================================================================

(def *update-poll-interval (atom 3600000)) ;; 1 hour default (less intrusive)
(def *last-code-hash (atom nil))
(def *update-poller (atom nil))

;; Blue-Green Deployment State
(def *blue-green-state (atom {:active-slot :blue
                               :blue-version nil
                               :green-version nil
                               :blue-ready true
                               :green-ready false
                               :pending-download nil
                               :last-swap nil}))

(defn get-inactive-slot []
  "Get the slot that's not currently active"
  (if (= :blue (:active-slot @*blue-green-state)) :green :blue))

(defn get-slot-dir [slot]
  "Get directory for a deployment slot"
  (let [base-dir (or (System/getProperty "app.dir") ".")
        slot-name (name slot)]
    (File. base-dir (str "slots/" slot-name))))

(defn prepare-inactive-slot! [new-version-dir]
  "Prepare the inactive slot with new version (background)"
  (future
    (try
      (let [inactive-slot (get-inactive-slot)
            slot-dir (get-slot-dir inactive-slot)]
        (log! (str "[BLUE-GREEN] Preparing " (name inactive-slot) " slot..."))
        ;; Create slot directory
        (.mkdirs slot-dir)
        ;; Copy new version to inactive slot
        (doseq [f (file-seq (File. new-version-dir))]
          (when (.isFile f)
            (let [rel-path (.substring (.getAbsolutePath f) 
                                       (inc (count (.getAbsolutePath (File. new-version-dir)))))
                  dest-file (File. slot-dir rel-path)]
              (.mkdirs (.getParentFile dest-file))
              (io/copy f dest-file))))
        ;; Mark slot as ready
        (swap! *blue-green-state assoc 
               (keyword (str (name inactive-slot) "-ready")) true
               (keyword (str (name inactive-slot) "-version")) (:version config))
        (log! (str "[BLUE-GREEN] " (name inactive-slot) " slot ready"))
        {:success true :slot inactive-slot})
      (catch Exception e
        (log! (str "[BLUE-GREEN] Slot preparation failed: " (.getMessage e)))
        {:success false :error (.getMessage e)}))))

(defn swap-slots! []
  "Instantly swap active slot (zero-downtime switch)"
  (let [current (:active-slot @*blue-green-state)
        new-slot (get-inactive-slot)]
    (when (get @*blue-green-state (keyword (str (name new-slot) "-ready")))
      (log! (str "[BLUE-GREEN] Swapping " (name current) " -> " (name new-slot)))
      (swap! *blue-green-state assoc 
             :active-slot new-slot
             :last-swap (System/currentTimeMillis))
      ;; Update classpath to point to new slot (Clojure dynamic loading)
      (try
        (let [slot-dir (get-slot-dir new-slot)
              src-dir (File. slot-dir "src")]
          (when (.exists src-dir)
            ;; Reload the main namespace from new location
            (require 'mental-models.desktop.gui.swing-app :reload)
            (log! "[BLUE-GREEN] Code swapped successfully - no restart needed")))
        (catch Exception e
          (log! (str "[BLUE-GREEN] Hot swap failed, will use on next restart: " (.getMessage e)))))
      (swap! *state assoc-in [:update :status] (str "✓ Swapped to " (name new-slot)))
      {:success true :active new-slot})))

(defn fetch-delta-manifest! []
  "Fetch list of changed files from web app"
  (try
    (let [url (str (or (get-in @*state [:settings :web-app-url]) (:web-app-url config))
                   "/api/trpc/desktop.deltaManifest?input="
                   (java.net.URLEncoder/encode 
                     (str "{\"json\":{\"version\":\"" (:version config) "\"}}")
                     "UTF-8"))
          result (http-get url :headers {"X-Desktop-API-Key" (:desktop-api-key config)})]
      (when (:success result)
        (let [body (:body result)]
          ;; Parse changed files from response
          (when-let [files-match (re-find #"\"changedFiles\":\s*\[([^\]]+)\]" body)]
            (let [files-str (second files-match)
                  ;; Extract file objects
                  file-matches (re-seq #"\{\"path\":\"([^\"]+)\",\"url\":\"([^\"]+)\",\"hash\":\"([^\"]+)\"\}" files-str)]
              (map (fn [[_ path url hash]]
                     {:path path :url url :hash hash})
                   file-matches))))))
    (catch Exception e
      (log! (str "[DELTA] Failed to fetch manifest: " (.getMessage e)))
      nil)))

(defn blue-green-delta-update! []
  "Delta update to inactive slot, then swap when ready - NO full downloads"
  (future
    (try
      (log! "[BLUE-GREEN] Checking for delta updates...")
      (let [changed-files (fetch-delta-manifest!)]
        (if (and changed-files (seq changed-files))
          (do
            (log! (str "[BLUE-GREEN] " (count changed-files) " files changed - downloading deltas only"))
            (swap! *blue-green-state assoc :pending-download "delta")
            
            ;; Prepare inactive slot by copying current version first
            (let [inactive-slot (get-inactive-slot)
                  slot-dir (get-slot-dir inactive-slot)
                  current-dir (File. (or (System/getProperty "app.dir") "."))]
              
              ;; Only copy if slot doesn't exist yet
              (when-not (.exists slot-dir)
                (log! "[BLUE-GREEN] Initializing inactive slot from current...")
                (.mkdirs slot-dir)
                (doseq [f (file-seq current-dir)]
                  (when (and (.isFile f)
                             (not (str/includes? (.getAbsolutePath f) "slots/"))
                             (not (str/includes? (.getAbsolutePath f) ".git")))
                    (let [rel-path (.substring (.getAbsolutePath f) 
                                               (inc (count (.getAbsolutePath current-dir))))
                          dest-file (File. slot-dir rel-path)]
                      (.mkdirs (.getParentFile dest-file))
                      (io/copy f dest-file)))))
              
              ;; Now apply only the changed files
              (let [success-count (atom 0)]
                (doseq [{:keys [path url]} changed-files]
                  (let [dest-file (File. slot-dir path)]
                    (.mkdirs (.getParentFile dest-file))
                    (try
                      (let [result (http-get url)]
                        (when (:success result)
                          (spit dest-file (:body result))
                          (swap! success-count inc)
                          (log! (str "[DELTA] Updated: " path))))
                      (catch Exception e
                        (log! (str "[DELTA] Failed: " path " - " (.getMessage e)))))))
                
                (if (= @success-count (count changed-files))
                  (do
                    (log! "[BLUE-GREEN] All deltas applied - ready to swap")
                    (swap! *blue-green-state assoc 