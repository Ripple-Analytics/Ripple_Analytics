(ns mental-models.desktop.gui.scanner
  "Scanner integration for GUI - connects UI to actual file scanning.
   Includes web app sync, LM Studio integration with retry/fallback,
   and offline mode support."
  (:require [mental-models.desktop.gui.app :as app]
            [mental-models.desktop.extractor :as extractor]
            [mental-models.desktop.db :as db]
            [mental-models.desktop.watcher :as watcher]
            [mental-models.desktop.api.web-client :as api]
            [mental-models.services.lm-studio :as lm]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]])
  (:import [java.io File]
           [java.security MessageDigest]
           [java.time Instant]))

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
;; File Hashing (for duplicate detection)
;; =============================================================================

(defn file-hash
  "Calculate SHA-256 hash of file content for duplicate detection"
  [file-path]
  (try
    (let [digest (MessageDigest/getInstance "SHA-256")
          file (io/file file-path)
          buffer (byte-array 8192)]
      (with-open [is (io/input-stream file)]
        (loop []
          (let [bytes-read (.read is buffer)]
            (when (pos? bytes-read)
              (.update digest buffer 0 bytes-read)
              (recur)))))
      (apply str (map #(format "%02x" %) (.digest digest))))
    (catch Exception e
      (println "Error hashing file:" (.getMessage e))
      nil)))

;; =============================================================================
;; Sync State Management
;; =============================================================================

(def sync-state (atom {:pending []
                       :synced []
                       :failed []
                       :last-sync nil}))

(defn add-to-sync-queue!
  "Add scan result to sync queue"
  [result]
  (swap! sync-state update :pending conj result))

(defn mark-synced!
  "Mark a result as successfully synced"
  [result-id]
  (swap! sync-state
         (fn [state]
           (let [result (first (filter #(= (:id %) result-id) (:pending state)))]
             (-> state
                 (update :pending #(vec (remove (fn [r] (= (:id r) result-id)) %)))
                 (update :synced conj (assoc result :synced-at (str (Instant/now)))))))))

(defn mark-sync-failed!
  "Mark a result as failed to sync"
  [result-id error]
  (swap! sync-state
         (fn [state]
           (let [result (first (filter #(= (:id %) result-id) (:pending state)))]
             (-> state
                 (update :pending #(vec (remove (fn [r] (= (:id r) result-id)) %)))
                 (update :failed conj (assoc result :error error :failed-at (str (Instant/now)))))))))

(defn get-sync-status
  "Get current sync status"
  []
  {:pending-count (count (:pending @sync-state))
   :synced-count (count (:synced @sync-state))
   :failed-count (count (:failed @sync-state))
   :last-sync (:last-sync @sync-state)
   :online (api/is-online?)})

(defn retry-failed-syncs!
  "Retry all failed syncs"
  []
  (let [failed (:failed @sync-state)]
    (swap! sync-state assoc :failed [])
    (doseq [result failed]
      (add-to-sync-queue! (dissoc result :error :failed-at)))))

;; =============================================================================
;; Web App Sync
;; =============================================================================

(defn sync-scan-result!
  "Sync a single scan result to the web app"
  [result]
  (try
    (when (api/is-online?)
      (let [sync-data {:file-path (:file-path result)
                       :file-name (.getName (io/file (:file-path result)))
                       :file-hash (:file-hash result)
                       :models (:models result)
                       :lollapalooza (:lollapalooza result)
                       :analyzed-at (str (:analyzed-at result))
                       :ai-analysis (:ai-analysis result)}
            success (api/sync-scan-results [sync-data])]
        (if success
          (do
            (mark-synced! (:id result))
            (app/add-log! :info (str "Synced: " (:file-path result)))
            true)
          (do
            (mark-sync-failed! (:id result) "API returned failure")
            false))))
    (catch Exception e
      (mark-sync-failed! (:id result) (.getMessage e))
      false)))

(defn process-sync-queue!
  "Process all pending sync items"
  []
  (when (and (api/is-online?) (pos? (count (:pending @sync-state))))
    (app/add-log! :info (str "Syncing " (count (:pending @sync-state)) " results to web app..."))
    (doseq [result (:pending @sync-state)]
      (sync-scan-result! result))
    (swap! sync-state assoc :last-sync (str (Instant/now)))
    (app/add-log! :success "Sync complete")))

(defn start-sync-processor!
  "Start background sync processor"
  []
  (future
    (loop []
      (Thread/sleep 30000) ;; Check every 30 seconds
      (try
        (process-sync-queue!)
        (catch Exception e
          (println "Sync processor error:" (.getMessage e))))
      (recur))))

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
;; LM Studio Integration with Retry and Fallback
;; =============================================================================

(def lm-studio-state (atom {:connected false
                            :last-check nil
                            :retry-count 0
                            :models-available []}))

(defn check-lm-studio-available
  "Check if LM Studio is available with retry logic"
  [lm-url]
  (loop [attempt 0]
    (if (>= attempt 3)
      (do
        (swap! lm-studio-state assoc :connected false)
        false)
      (try
        (let [response (lm/health-check lm-url)]
          (if (:ok response)
            (do
              (swap! lm-studio-state assoc
                     :connected true
                     :last-check (str (Instant/now))
                     :retry-count 0)
              true)
            (do
              (Thread/sleep (* 1000 (Math/pow 2 attempt)))
              (recur (inc attempt)))))
        (catch Exception _
          (Thread/sleep (* 1000 (Math/pow 2 attempt)))
          (recur (inc attempt)))))))

(defn keyword-fallback-analysis
  "Fallback to keyword-based detection when LM Studio is unavailable"
  [text]
  (let [keyword-patterns
        {"Confirmation Bias" #"(?i)confirm|bias|believe|evidence"
         "Sunk Cost Fallacy" #"(?i)sunk cost|already invested|too late"
         "Availability Heuristic" #"(?i)recent|remember|vivid|example"
         "Anchoring" #"(?i)anchor|initial|first impression|starting point"
         "Loss Aversion" #"(?i)loss|lose|risk|avoid"
         "Survivorship Bias" #"(?i)survivor|success|fail|selection"
         "Dunning-Kruger" #"(?i)overconfident|incompetent|expert"
         "Hindsight Bias" #"(?i)knew it|obvious|predictable"
         "Bandwagon Effect" #"(?i)everyone|popular|trend|follow"
         "Recency Bias" #"(?i)recent|latest|new|current"}
        detected (for [[model pattern] keyword-patterns
                       :when (re-find pattern text)]
                   {:name model
                    :category "Psychology"
                    :confidence 0.5
                    :explanation "Detected via keyword matching (LM Studio unavailable)"
                    :detection-method :keyword})]
    {:models (vec detected)
     :lollapalooza (>= (count detected) 3)
     :summary "Keyword-based analysis (AI unavailable)"
     :detection-method :keyword}))

(defn analyze-with-retry
  "Analyze text with LM Studio, with retry logic and fallback"
  [text lm-url]
  (if (check-lm-studio-available lm-url)
    (loop [attempt 0]
      (if (>= attempt 3)
        (do
          (app/add-log! :warning "LM Studio failed after 3 attempts, using keyword fallback")
          (keyword-fallback-analysis text))
        (let [result (analyze-with-lm-studio text lm-url)]
          (if (:error result)
            (do
              (Thread/sleep (* 1000 (Math/pow 2 attempt)))
              (recur (inc attempt)))
            (assoc result :detection-method :ai)))))
    (do
      (app/add-log! :warning "LM Studio unavailable, using keyword fallback")
      (keyword-fallback-analysis text))))

;; =============================================================================
;; Scan Pipeline
;; =============================================================================

(defn scan-file! [file-path lm-url]
  "Scan a single file and update GUI state, with sync to web app"
  (app/add-log! :info (str "Scanning: " file-path))
  (swap! app/*state assoc-in [:scan :current-file] file-path)
  
  (try
    ;; Calculate file hash for duplicate detection
    (let [hash (file-hash file-path)
          _ (app/add-log! :info (str "File hash: " (subs (or hash "unknown") 0 (min 16 (count (or hash ""))))))
          
          ;; Extract text
          text (extractor/extract-text file-path)
          _ (app/add-log! :info (str "Extracted " (count text) " characters"))
          
          ;; Analyze with LM Studio (with retry and fallback)
          analysis (when (and text (> (count text) 100))
                     (analyze-with-retry (subs text 0 (min 8000 (count text))) lm-url))
          
          models (get analysis :models [])
          lollapalooza? (get analysis :lollapalooza false)
          detection-method (get analysis :detection-method :unknown)
          
          ;; Create result with unique ID
          result-id (str (java.util.UUID/randomUUID))
          result {:id result-id
                  :file-path file-path
                  :file-hash hash
                  :models models
                  :lollapalooza lollapalooza?
                  :analyzed-at (Instant/now)
                  :detection-method detection-method
                  :ai-analysis (:summary analysis)}]
      
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
                                  (int (* 100 (or (:confidence model) 0))) "% confidence"
                                  (when (= detection-method :keyword) " [keyword]")))
        (swap! app/*state update-in [:scan :found-models] conj model))
      
      ;; Save to local database
      (db/save-analysis! {:file-path file-path
                          :file-name (.getName (io/file file-path))
                          :file-hash hash
                          :mental-models models
                          :lollapalooza lollapalooza?
                          :analyzed-at (str (Instant/now))})
      
      ;; Add to sync queue for web app
      (add-to-sync-queue! result)
      (app/add-log! :info (str "Queued for sync: " file-path))
      
      {:success true :models models :lollapalooza lollapalooza? :id result-id})
    
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
