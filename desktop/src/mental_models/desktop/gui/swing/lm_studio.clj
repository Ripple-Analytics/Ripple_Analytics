(ns mental-models.desktop.gui.swing.lm-studio
  "Swing App - Lm Studio"
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

;; LM Studio AI Integration
;; =============================================================================

(def lm-studio-paths
  "Common LM Studio installation paths"
  [(str (System/getenv "LOCALAPPDATA") "\\LM Studio\\LM Studio.exe")
   (str (System/getenv "PROGRAMFILES") "\\LM Studio\\LM Studio.exe")
   "/Applications/LM Studio.app/Contents/MacOS/LM Studio"
   (str (System/getProperty "user.home") "/Applications/LM Studio.app/Contents/MacOS/LM Studio")])

(defn find-lm-studio-path []
  "Find LM Studio executable path"
  (first (filter #(and % (.exists (File. %))) lm-studio-paths)))

(defn launch-lm-studio! []
  "Launch LM Studio if not running"
  (when-let [path (find-lm-studio-path)]
    (log! (str "[LM-STUDIO] Launching: " path))
    (try
      (.exec (Runtime/getRuntime) (into-array String [path]))
      (Thread/sleep 3000)
      true
      (catch Exception e
        (log! (str "[LM-STUDIO] Launch error: " (.getMessage e)))
        false))))

(defn ensure-lm-studio-running! []
  "Ensure LM Studio is running, launch if needed"
  (if (= :connected (get-in @*state [:connections :lm-studio]))
    true
    (do
      (log! "[LM-STUDIO] Not connected, attempting to launch...")
      (when (launch-lm-studio!)
        (Thread/sleep 5000)
        @(check-lm-studio!)
        (Thread/sleep 1000)
        (= :connected (get-in @*state [:connections :lm-studio]))))))

(defn lm-studio-chat [prompt]
  "Send a chat request to LM Studio"
  (try
    (let [url (str (get-in @*state [:settings :lm-studio-url]) "/v1/chat/completions")
          escaped (-> prompt
                     (str/replace "\\" "\\\\")
                     (str/replace "\"" "\\\"")
                     (str/replace "\n" "\\n")
                     (str/replace "\r" "")
                     (str/replace "\t" "\\t"))
          body (str "{\"messages\": [{\"role\": \"user\", \"content\": \"" escaped "\"}], \"temperature\": 0.3, \"max_tokens\": 800}")
          result (http-post url body)]
      (when (:success result)
        (when-let [match (re-find #"\"content\"\s*:\s*\"((?:[^\"\\]|\\.)*)\"" (:body result))]
          (-> (second match)
              (str/replace "\\n" "\n")
              (str/replace "\\\"" "\"")))))
    (catch Exception e
      (log! (str "[LM-STUDIO] Chat error: " (.getMessage e)))
      nil)))

(defn ai-analyze-text [text]
  "Use AI to analyze text for mental models with structured output"
  (let [truncated (subs text 0 (min 3000 (count text)))
        model-names (str/join ", " (map :name (take 30 mental-models)))
        prompt (str "You are a mental model expert. Analyze this text and identify which mental models apply.\n\n"
                   "Available mental models include: " model-names " and many more.\n\n"
                   "For each mental model you detect, respond in this exact format:\n"
                   "MODEL: [model name] | CONFIDENCE: [high/medium/low] | REASON: [brief explanation]\n\n"
                   "Text to analyze:\n" truncated)]
    (lm-studio-chat prompt)))

(defn parse-ai-response [response]
  "Parse AI response into structured mental model detections"
  (when response
    (let [lines (str/split-lines response)
          model-lines (filter #(str/starts-with? % "MODEL:") lines)]
      (for [line model-lines]
        (let [parts (str/split line #"\|")
              model-part (first parts)
              confidence-part (second parts)
              reason-part (nth parts 2 nil)]
          {:name (str/trim (str/replace (or model-part "") #"MODEL:\s*" ""))
           :confidence (str/lower-case (str/trim (str/replace (or confidence-part "") #"CONFIDENCE:\s*" "")))
           :reason (str/trim (str/replace (or reason-part "") #"REASON:\s*" ""))})))))

(defn ai-scan-file [file progress-callback]
  "Scan a file using AI for deeper mental model detection"
  (let [name (.getName file)
        ext (str/lower-case (or (last (str/split name #"\.")) ""))
        text (cond
               (#{"txt" "md" "markdown"} ext) (read-text-file file)
               (= "pdf" ext) (read-pdf-file file)
               (#{"docx" "doc"} ext) (read-docx-file file)
               :else nil)]
    (when text
      (log! (str "[AI-SCAN] Analyzing: " name))
      (let [ai-response (ai-analyze-text text)
            ai-models (parse-ai-response ai-response)
            ;; Also run keyword detection for comparison
            keyword-models (detect-models text)
            lollapalooza (detect-lollapalooza (concat ai-models keyword-models))]
        (when progress-callback
          (progress-callback {:file name
                             :ai-models ai-models
                             :keyword-models keyword-models
                             :lollapalooza lollapalooza}))
        {:file name
         :ai-models ai-models
         :keyword-models keyword-models
         :lollapalooza lollapalooza}))))

(defn check-github! []
  (future
    (try
      (let [token (get-in @*state [:settings :github-token])
            headers (when (and token (not (str/blank? token)))
                     {"Authorization" (str "token " token)
                      "Accept" "application/vnd.github.v3+json"})
            result (http-get (str (:github-api config) "/user") :headers headers)]
        (swap! *state assoc-in [:connections :github]
               (if (:success result) :connected :disconnected)))
      (catch Exception _
        (swap! *state assoc-in [:connections :github] :disconnected)))))

(defn check-all-connections! []
  (check-lm-studio!)
  (check-web-app!)
  (check-github!))

;; =============================================================================