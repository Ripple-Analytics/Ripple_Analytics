(ns mental-models.desktop.gui.scanner
  "Scanner integration for GUI - connects UI to actual file scanning"
  (:require [mental-models.desktop.gui.app :as app]
            [mental-models.desktop.extractor :as extractor]
            [mental-models.desktop.db :as db]
            [mental-models.desktop.watcher :as watcher]
            [mental-models.services.lm-studio :as lm]
            [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]])
  (:import [java.io File]))

;; =============================================================================
;; File Discovery
;; =============================================================================

(def supported-extensions
  #{".txt" ".md" ".pdf" ".docx" ".doc" ".rtf"})

(defn supported-file? [^File file]
  (and (.isFile file)
       (some #(.endsWith (.toLowerCase (.getName file)) %) supported-extensions)))

(defn discover-files [folder-path]
  "Recursively find all supported files in a folder"
  (let [folder (io/file folder-path)]
    (->> (file-seq folder)
         (filter supported-file?)
         (map #(.getAbsolutePath %))
         (vec))))

;; =============================================================================
;; LM Studio Analysis
;; =============================================================================

(def mental-model-prompt
  "Analyze the following text and identify any mental models, cognitive biases, or psychological tendencies present.

For each mental model found, provide:
1. Name of the mental model
2. Category (e.g., Psychology, Economics, Mathematics, etc.)
3. Confidence score (0.0 to 1.0)
4. Brief explanation of how it appears in the text
5. Relevant quote from the text

If 3 or more mental models are found with confidence > 0.7, flag this as a potential 'Lollapalooza' effect.

Respond in JSON format:
{
  \"models\": [
    {
      \"name\": \"Model Name\",
      \"category\": \"Category\",
      \"confidence\": 0.85,
      \"explanation\": \"How it appears...\",
      \"quote\": \"Relevant text...\"
    }
  ],
  \"lollapalooza\": true/false,
  \"summary\": \"Brief summary of the analysis\"
}

TEXT TO ANALYZE:
")

(defn analyze-with-lm-studio [text lm-url]
  "Send text to LM Studio for mental model analysis"
  (try
    (let [response (lm/chat lm-url
                           [{:role "system"
                             :content "You are an expert in mental models, cognitive biases, and psychological tendencies. Analyze text to identify these patterns."}
                            {:role "user"
                             :content (str mental-model-prompt text)}]
                           {:temperature 0.3
                            :max_tokens 2000})]
      (when-let [content (get-in response [:choices 0 :message :content])]
        (try
          (cheshire.core/parse-string content true)
          (catch Exception _
            {:models [] :error "Failed to parse LM response"}))))
    (catch Exception e
      {:models [] :error (.getMessage e)})))

;; =============================================================================
;; Scan Pipeline
;; =============================================================================

(defn scan-file! [file-path lm-url]
  "Scan a single file and update GUI state"
  (app/add-log! :info (str "Scanning: " file-path))
  (swap! app/*state assoc-in [:scan :current-file] file-path)
  
  (try
    ;; Extract text
    (let [text (extractor/extract-text file-path)
          _ (app/add-log! :info (str "Extracted " (count text) " characters"))
          
          ;; Analyze with LM Studio
          analysis (when (and text (> (count text) 100))
                     (analyze-with-lm-studio (subs text 0 (min 8000 (count text))) lm-url))
          
          models (get analysis :models [])
          lollapalooza? (get analysis :lollapalooza false)]
      
      ;; Update stats
      (app/update-stat! :files-scanned 1)
      (app/update-stat! :documents-analyzed 1)
      (app/update-stat! :models-found (count models))
      
      (when lollapalooza?
        (app/update-stat! :lollapalooza-events 1)
        (app/add-log! :warning (str "🎯 LOLLAPALOOZA detected in: " file-path)))
      
      ;; Log found models
      (doseq [model models]
        (app/add-log! :model (str "Found: " (:name model) " (" (:category model) ") - " 
                                  (int (* 100 (:confidence model))) "% confidence"))
        (swap! app/*state update-in [:scan :found-models] conj model))
      
      ;; Save to local database
      (db/save-analysis! {:file-path file-path
                          :models models
                          :lollapalooza lollapalooza?
                          :analyzed-at (java.time.Instant/now)})
      
      {:success true :models models :lollapalooza lollapalooza?})
    
    (catch Exception e
      (app/add-log! :error (str "Error scanning " file-path ": " (.getMessage e)))
      {:success false :error (.getMessage e)})))

(defn start-scan! []
  "Start scanning all watched folders"
  (let [folders (:watched-folders @app/*state)
        lm-url (get-in @app/*state [:settings :lm-studio-url])]
    
    (when (empty? folders)
      (app/add-log! :warning "No folders selected to scan")
      (return))
    
    ;; Discover all files
    (app/add-log! :info "Discovering files...")
    (let [all-files (mapcat discover-files folders)]
      (app/add-log! :info (str "Found " (count all-files) " files to scan"))
      
      ;; Update state
      (swap! app/*state assoc-in [:scan :status] :scanning)
      (swap! app/*state assoc-in [:scan :total-files] (count all-files))
      (swap! app/*state assoc-in [:scan :progress] 0)
      
      ;; Process files in background
      (future
        (doseq [[idx file] (map-indexed vector all-files)]
          (when (= :scanning (get-in @app/*state [:scan :status]))
            (scan-file! file lm-url)
            (swap! app/*state assoc-in [:scan :progress] (inc idx))))
        
        ;; Done
        (swap! app/*state assoc-in [:scan :status] :idle)
        (swap! app/*state assoc-in [:scan :current-file] nil)
        (swap! app/*state assoc-in [:stats :last-sync] (str (java.time.LocalDateTime/now)))
        (app/add-log! :success "Scan complete!")))))

(defn stop-scan! []
  "Stop the current scan"
  (swap! app/*state assoc-in [:scan :status] :idle)
  (app/add-log! :info "Scan stopped by user"))

;; =============================================================================
;; Watch Mode
;; =============================================================================

(def watch-channel (atom nil))

(defn start-watch! []
  "Start watching folders for changes"
  (let [folders (:watched-folders @app/*state)
        lm-url (get-in @app/*state [:settings :lm-studio-url])]
    
    (when (empty? folders)
      (app/add-log! :warning "No folders selected to watch")
      (return))
    
    (swap! app/*state assoc-in [:scan :status] :watching)
    (app/add-log! :info "Watch mode started")
    
    ;; Create watch channel
    (let [ch (chan 100)]
      (reset! watch-channel ch)
      
      ;; Start watchers for each folder
      (doseq [folder folders]
        (watcher/watch-folder folder
                              (fn [event file]
                                (when (and (#{:create :modify} event)
                                           (supported-file? (io/file file)))
                                  (async/put! ch file)))))
      
      ;; Process changes
      (go-loop []
        (when-let [file (<! ch)]
          (app/add-log! :info (str "File changed: " file))
          (scan-file! file lm-url)
          (recur))))))

(defn stop-watch! []
  "Stop watching folders"
  (when-let [ch @watch-channel]
    (close! ch)
    (reset! watch-channel nil))
  (watcher/stop-all!)
  (swap! app/*state assoc-in [:scan :status] :idle)
  (app/add-log! :info "Watch mode stopped"))

;; =============================================================================
;; Connection Testing
;; =============================================================================

(defn test-lm-studio-connection! []
  "Test connection to LM Studio"
  (let [url (get-in @app/*state [:settings :lm-studio-url])]
    (app/add-log! :info (str "Testing connection to " url "..."))
    (app/set-connection! :lm-studio :connecting)
    
    (future
      (try
        (let [response (lm/health-check url)]
          (if (:ok response)
            (do
              (app/set-connection! :lm-studio :connected)
              (app/add-log! :success "LM Studio connected!"))
            (do
              (app/set-connection! :lm-studio :disconnected)
              (app/add-log! :error "LM Studio not responding"))))
        (catch Exception e
          (app/set-connection! :lm-studio :disconnected)
          (app/add-log! :error (str "Connection failed: " (.getMessage e))))))))

(defn test-web-app-connection! []
  "Test connection to web app"
  (let [url (get-in @app/*state [:settings :web-app-url])]
    (when-not (clojure.string/blank? url)
      (app/add-log! :info (str "Testing connection to " url "..."))
      (app/set-connection! :web-app :connecting)
      
      (future
        (try
          (let [response (clj-http.client/get (str url "/api/health")
                                               {:throw-exceptions false
                                                :socket-timeout 5000
                                                :connection-timeout 5000})]
            (if (= 200 (:status response))
              (do
                (app/set-connection! :web-app :connected)
                (app/add-log! :success "Web app connected!"))
              (do
                (app/set-connection! :web-app :disconnected)
                (app/add-log! :error "Web app not responding"))))
          (catch Exception e
            (app/set-connection! :web-app :disconnected)
            (app/add-log! :error (str "Connection failed: " (.getMessage e)))))))))

;; =============================================================================
;; Update Event Handlers
;; =============================================================================

;; Override event handlers to use actual scanning
(defmethod app/event-handler ::app/start-scan [_]
  (start-scan!))

(defmethod app/event-handler ::app/stop-scan [_]
  (stop-scan!))

(defmethod app/event-handler ::app/start-watch [_]
  (start-watch!))

(defmethod app/event-handler ::app/stop-watch [_]
  (stop-watch!))

(defmethod app/event-handler ::app/test-lm-studio [_]
  (test-lm-studio-connection!))
