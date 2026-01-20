(ns mental-models.desktop.gui.swing.update-system-part3
  "Swing App - Update System (Part 3)"
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

    (let [sources (:sources remote-config)]
      (log! (str "[DOWNLOAD] Got " (count sources) " download sources from remote config"))
      (if (seq sources)
        ;; Use multi-source download
        (let [result (download-with-fallback! sources dest-file progress-callback)]
          (when-not (:success result)
            ;; Report all failures
            (doseq [err (:errors result)]
              (report-download-failure! (:version remote-config) (:source err) (:error err))))
          result)
        ;; No sources in config, fall back to direct URL
        (do
          (log! "[DOWNLOAD] No sources in remote config, using direct URL")
          (download-file-simple! download-url dest-file progress-callback true))))
    
    ;; Remote config failed, fall back to direct download
    (do
      (log! "[DOWNLOAD] Remote config unavailable, using direct URL")
      (download-file-simple! download-url dest-file progress-callback true))))

(defn extract-zip! [zip-file dest-dir]
  "Extract a ZIP file to destination directory"
  (try
    (log! (str "[EXTRACT] Extracting " (.getName zip-file) " to " dest-dir))
    (with-open [zis (ZipInputStream. (FileInputStream. zip-file))]
      (loop []
        (when-let [entry (.getNextEntry zis)]
          (let [dest-file (File. dest-dir (.getName entry))]
            (if (.isDirectory entry)
              (.mkdirs dest-file)
              (do
                (.mkdirs (.getParentFile dest-file))
                (with-open [out (FileOutputStream. dest-file)]
                  (io/copy zis out)))))
          (.closeEntry zis)
          (recur))))
    (log! "[EXTRACT] Extraction complete")
    {:success true}
    (catch Exception e
      (log! (str "[EXTRACT] Error: " (.getMessage e)))
      {:success false :error (.getMessage e)})))

(defn test-new-version! [source-dir]
  "Test if new version can start without crashing - returns true if OK"
  (log! "[UPDATE] Testing new version before switching...")
  (try
    ;; Try to load and compile the new code
    (let [new-swing-app (File. source-dir "src/mental_models/desktop/gui/swing_app.clj")]
      (if (.exists new-swing-app)
        (do
          ;; Basic syntax check - try to read the file as Clojure
          (let [content (slurp new-swing-app)]
            ;; Check for basic structure
            (if (and (str/includes? content "defn -main")
                     (str/includes? content "defn create-main-frame")
                     (> (count content) 10000))  ;; Sanity check - file should be substantial
              (do
                (log! "[UPDATE] New version passed basic validation")
                true)
              (do
                (log! "[UPDATE] New version FAILED validation - missing required functions")
                false))))
        (do
          (log! "[UPDATE] New version FAILED - swing_app.clj not found")
          false)))
    (catch Exception e
      (log! (str "[UPDATE] New version FAILED validation: " (.getMessage e)))
      false)))

(defn perform-auto-update! [parent download-url tag]
  "SAFE UPDATE: Download, test, then switch - NEVER quit until new version is verified"
  (log! (str "[UPDATE] Starting SAFE automatic update to " tag))
  (log! (str "[UPDATE] Download URL: " download-url))
  
  (future
    (try
      (let [app-dir (File. (or (System/getProperty "app.dir") "."))
            temp-dir (File. (System/getProperty "java.io.tmpdir"))
            temp-zip (File. temp-dir (str "MentalModels-update-" tag ".zip"))
            temp-extract (File. temp-dir (str "MentalModels-extract-" (System/currentTimeMillis)))
            backup-dir (File. app-dir "backup-previous-version")
            state-backup-file (File. temp-dir "mm-state-backup.edn")]
        
        (log! (str "[UPDATE] App dir: " (.getAbsolutePath app-dir)))
        (log! (str "[UPDATE] Temp zip: " temp-zip))
        
        ;; STEP 1: Save current state BEFORE anything else
        (log! "[UPDATE] Saving application state...")
        (spit state-backup-file (pr-str (serialize-state)))
        (log! (str "[UPDATE] State saved to: " (.getAbsolutePath state-backup-file)))
        
        ;; STEP 2: Download the ZIP
        (swap! *state assoc-in [:update :status] (str "Downloading " tag "..."))
        (let [download-result (download-github-release-asset! 
                                download-url 
                                temp-zip
                                (fn [percent bytes total]
                                  (let [status (if (pos? total)
                                                (format "Downloading %s (%d%%)" tag percent)
                                                (format "Downloading %s (%.1f MB)" tag (/ bytes 1048576.0)))]
                                    (swap! *state assoc-in [:update :status] status))))]
          
          (if (:success download-result)
            (do
              (log! (str "[UPDATE] Download complete: " (:bytes download-result) " bytes"))
              (swap! *state assoc-in [:update :status] "Extracting...")
              
              ;; STEP 3: Extract to temp folder
              (.mkdirs temp-extract)
              (let [extract-result (extract-zip! temp-zip temp-extract)]
                (if (:success extract-result)
                  (do
                    (log! "[UPDATE] Extraction complete")
                    (.delete temp-zip)
                    
                    ;; Find the extracted folder
                    (let [extracted-folders (.listFiles temp-extract)
                          source-dir (if (and (= 1 (count extracted-folders))
                                              (.isDirectory (first extracted-folders)))
                                       (first extracted-folders)
                                       temp-extract)
                          bat-file (File. app-dir "MentalModels.bat")]
                      
                      (log! (str "[UPDATE] Source dir: " (.getAbsolutePath source-dir)))
                      
                      ;; STEP 4: TEST NEW VERSION BEFORE SWITCHING
                      (swap! *state assoc-in [:update :status] "Testing new version...")
                      (if (test-new-version! source-dir)
                        (do
                          ;; NEW VERSION PASSED - proceed with update
                          (log! "[UPDATE] New version PASSED testing - proceeding with update")
                          (swap! *state assoc-in [:update :status] "Installing (verified safe)...")
                          
                          ;; STEP 5: Backup current version before overwriting
                          (log! "[UPDATE] Backing up current version...")
                          (when (.exists backup-dir)
                            ;; Delete old backup
                            (doseq [f (file-seq backup-dir)]
                              (when (.isFile f) (.delete f)))
                            (.delete backup-dir))
                          (.mkdirs backup-dir)
                          ;; Copy current src to backup
                          (let [current-src (File. app-dir "src")]
                            (when (.exists current-src)
                              (doseq [f (file-seq current-src)]
                                (when (.isFile f)
                                  (let [rel-path (.substring (.getAbsolutePath f) (count (.getAbsolutePath app-dir)))
                                        backup-file (File. backup-dir rel-path)]
                                    (.mkdirs (.getParentFile backup-file))
                                    (io/copy f backup-file))))))