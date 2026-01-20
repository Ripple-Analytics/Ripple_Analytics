(ns mental-models.desktop.gui.swing.file-scanning
  "Swing App - File Scanning"
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

;; File Scanning
;; =============================================================================

(defn read-text-file [file]
  (try
    (slurp file)
    (catch Exception e
      (log! (str "[SCAN] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn read-pdf-file [file]
  "Extract text from PDF using Apache PDFBox"
  (try
    (let [pdf-class (Class/forName "org.apache.pdfbox.Loader")
          load-method (.getMethod pdf-class "loadPDF" (into-array Class [File]))
          document (. load-method invoke nil (into-array Object [file]))
          stripper-class (Class/forName "org.apache.pdfbox.text.PDFTextStripper")
          stripper (.newInstance stripper-class)
          text (.getText stripper document)]
      (.close document)
      text)
    (catch ClassNotFoundException _
      (log! "[PDF] PDFBox not available - PDF support disabled")
      nil)
    (catch Exception e
      (log! (str "[PDF] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn read-docx-file [file]
  "Extract text from DOCX files (basic XML extraction)"
  (try
    (with-open [zis (java.util.zip.ZipInputStream. (FileInputStream. file))]
      (loop [text-parts []]
        (if-let [entry (.getNextEntry zis)]
          (if (= "word/document.xml" (.getName entry))
            (let [content (slurp zis)
                  ;; Extract text between <w:t> tags
                  text-matches (re-seq #"<w:t[^>]*>([^<]*)</w:t>" content)
                  text (str/join " " (map second text-matches))]
              text)
            (recur text-parts))
          (str/join " " text-parts))))
    (catch Exception e
      (log! (str "[DOCX] Error reading " (.getName file) ": " (.getMessage e)))
      nil)))

(defn scan-file [file progress-callback]
  "Scan a single file for mental models with semantic indexing"
  (let [name (.getName file)
        ext (str/lower-case (or (last (str/split name #"\.")) ""))
        text (cond
               (#{"txt" "md" "markdown"} ext) (read-text-file file)
               (= "pdf" ext) (read-pdf-file file)
               (#{"docx" "doc"} ext) (read-docx-file file)
               :else nil)]
    (when text
      (let [models (detect-models text)
            lollapalooza (detect-lollapalooza models)]
        (when (seq models)
          (save-scan-result! (.getAbsolutePath file) (map :name models) 
                            (/ (count models) (double (count mental-models))))
          ;; Index document for semantic search
          (try
            (search/index-document! (.getAbsolutePath file) text
                                   {:name name
                                    :models (map :name models)
                                    :lollapalooza (boolean lollapalooza)})
            (catch Exception _ nil))
          (when progress-callback
            (progress-callback {:file name
                               :models models
                               :lollapalooza lollapalooza})))
        {:file name
         :models models
         :lollapalooza lollapalooza}))))

(defn scan-folder [folder-path progress-callback completion-callback]
  "Scan all files in a folder with analytics tracking"
  (future
    (try
      (let [start-time (System/currentTimeMillis)
            folder (File. folder-path)
            files (->> (file-seq folder)
                      (filter #(.isFile %))
                      (filter #(let [ext (str/lower-case (or (last (str/split (.getName %) #"\.")) ""))]
                                (#{"txt" "md" "markdown" "pdf" "docx" "doc"} ext))))
            models-found (atom [])]
        (log! (str "[SCAN] Starting scan of " (count files) " files in " folder-path))
        (doseq [[idx file] (map-indexed vector files)]
          (when progress-callback
            (progress-callback {:progress (/ (* 100 (inc idx)) (count files))
                               :current (.getName file)}))
          (when-let [result (scan-file file progress-callback)]
            (swap! models-found concat (map :name (:models result)))))
        (let [duration-ms (- (System/currentTimeMillis) start-time)]
          (log! (str "[SCAN] Complete in " duration-ms "ms"))
          ;; Track scan in analytics
          (analytics/track-scan! folder-path (count files) (distinct @models-found) duration-ms)
          ;; Update anomaly baselines
          (anomaly/update-scan-baselines! (:scans (analytics/get-stats)))
          ;; Check for anomalies
          (anomaly/check-scan-anomalies {:folder folder-path 
                                          :files (count files)
                                          :model-count (count (distinct @models-found))
                                          :duration-ms duration-ms}))
        ;; Auto-sync results to web app
        (when (get-in @*state [:settings :auto-sync] true)
          (try
            (let [results (get-in @*state [:scan-results])]
              (when (seq results)
                (log! "[SYNC] Auto-syncing to web app...")
                (sync-to-web-app! results)
                (log! "[SYNC] Auto-sync complete")))
            (catch Exception e
              (log! (str "[SYNC] Auto-sync failed: " (.getMessage e))))))
        (when completion-callback
          (completion-callback {:total (count files)})))
      (catch Exception e
        (log! (str "[SCAN] Error: " (.getMessage e)))))))


;; =============================================================================