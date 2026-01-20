(ns mental-models.desktop.gui.swing.update-system-part1
  "Swing App - Update System (Part 1)"
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

;; BULLETPROOF UPDATE SYSTEM - Remotely Fixable
;; =============================================================================
;;
;; This system can be fixed REMOTELY without updating the app:
;; 1. Fetches download config from web app (URLs, fallbacks, retry settings)
;; 2. Tries multiple download sources in order
;; 3. Caches downloads locally to avoid re-downloading
;; 4. Reports failures to web app for debugging
;;
;; If downloads break, update the web app config - no app update needed!
;; =============================================================================

(defn ensure-cache-dir! []
  "Ensure the cache directory exists"
  (let [dir (File. (:cache-dir config))]
    (when-not (.exists dir)
      (.mkdirs dir))
    dir))

(defn cache-path [version]
  "Get the cache file path for a version"
  (File. (:cache-dir config) (str "MentalModels-" version ".zip")))

(defn cached? [version]
  "Check if a version is already cached"
  (let [f (cache-path version)]
    (and (.exists f) (> (.length f) 1000000)))) ;; Must be > 1MB to be valid

(defn fetch-secure-download []
  "Fetch secure, time-limited download URL from web app. Returns nil on failure."
  (log! "[UPDATE] Fetching secure download URL from web app...")
  (try
    (let [url (str (:web-app-url config) "/api/trpc/desktop.getSecureDownload")
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "X-Desktop-API-Key" (:desktop-api-key config))
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 30000))
          status (.getResponseCode conn)]
      (if (= status 200)
        (let [body (slurp (.getInputStream conn))]
          (.disconnect conn)
          (log! "[UPDATE] Secure download URL fetched successfully")
          ;; Parse the response
          (let [version (second (re-find #"\"version\":\s*\"([^\"]+)\"" body))
                download-url (second (re-find #"\"downloadUrl\":\s*\"([^\"]+)\"" body))
                fallback-url (second (re-find #"\"fallbackUrl\":\s*\"([^\"]+)\"" body))
                auth-header (second (re-find #"\"authHeader\":\s*\"([^\"]+)\"" body))
                sha256 (second (re-find #"\"sha256\":\s*\"([^\"]+)\"" body))]
            {:version version
             :download-url download-url
             :fallback-url fallback-url
             :auth-header auth-header
             :sha256 sha256}))
        (do
          (.disconnect conn)
          (log! (str "[UPDATE] Secure download failed: HTTP " status))
          nil)))
    (catch Exception e
      (log! (str "[UPDATE] Secure download error: " (.getMessage e)))
      nil)))

(defn fetch-remote-config []
  "Fetch download configuration from web app. Returns nil on failure."
  (log! "[UPDATE] Fetching remote config from web app...")
  (try
    (let [url (str (:web-app-url config) "/api/trpc/desktop.config")
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "X-Desktop-API-Key" (:desktop-api-key config))
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 30000))
          status (.getResponseCode conn)]
      (if (= status 200)
        (let [body (slurp (.getInputStream conn))]
          (.disconnect conn)
          (log! "[UPDATE] Remote config fetched successfully")
          ;; Parse the tRPC response to extract version
          (let [version (second (re-find #"\"currentVersion\":\s*\"([^\"]+)\"" body))]
            ;; Now fetch the secure download URL
            (if-let [secure-download (fetch-secure-download)]
              {:version version
               :sources [{:name "secure"
                          :url (:download-url secure-download)
                          :fallback-url (:fallback-url secure-download)
                          :auth-header (:auth-header secure-download)
                          :sha256 (:sha256 secure-download)
                          :requires-auth true}]
               :raw body}
              ;; Fallback to GitHub if secure download fails
              {:version version
               :sources [{:name "github"
                          :url (str "https://github.com/Ripple-Analytics/Ripple_Analytics/releases/download/" version "/MentalModels-" version "-Desktop.zip")
                          :requires-auth true}]
               :raw body})))
        (do
          (.disconnect conn)
          (log! (str "[UPDATE] Remote config failed: HTTP " status))
          nil)))
    (catch Exception e
      (log! (str "[UPDATE] Remote config error: " (.getMessage e)))
      nil)))

(defn download-file-simple! [url dest-file progress-callback use-auth?]
  "Download a file - use-auth? controls whether to send GitHub token"
  (try
    (let [github-token (or (get-in @*state [:settings :github-token]) (:github-token config))
          conn (doto ^HttpURLConnection (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setRequestProperty "User-Agent" "MentalModels-Desktop/1.4.2")
                 (.setRequestProperty "Accept" "application/octet-stream")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 300000)
                 (.setInstanceFollowRedirects false))]
      ;; Only add auth for GitHub API calls, not for S3 signed URLs
      (when use-auth?
        (.setRequestProperty conn "Authorization" (str "token " github-token)))
      
      (let [status (.getResponseCode conn)]
        (log! (str "[DOWNLOAD] URL: " (subs url 0 (min 80 (count url))) "..."))
        (log! (str "[DOWNLOAD] Status: " status " use-auth: " use-auth?))
        
        (cond
          ;; Success - download the file
          (= status 200)
          (let [content-length (.getContentLengthLong conn)
                input (BufferedInputStream. (.getInputStream conn))
                output (FileOutputStream. dest-file)
                buffer (byte-array 65536)] ;; 64KB buffer for speed
            (log! (str "[DOWNLOAD] Starting download, size: " 
                       (if (pos? content-length) 
                         (format "%.1f MB" (/ content-length 1048576.0))
                         "unknown")))
            (loop [total 0]
              (let [n (.read input buffer)]
                (if (pos? n)
                  (do
                    (.write output buffer 0 n)
                    (let [new-total (+ total n)
                          percent (if (pos? content-length)
                                    (int (* 100 (/ new-total content-length)))
                                    -1)]
                      (when (and progress-callback (zero? (mod new-total 524288))) ;; Update every 512KB
                        (progress-callback percent new-total content-length))
                      (recur new-total)))
                  (do
                    (.close input)
                    (.close output)
                    (.disconnect conn)
                    (log! (str "[DOWNLOAD] Complete: " total " bytes"))