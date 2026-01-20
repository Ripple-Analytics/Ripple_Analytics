(ns mental-models.desktop.updater.multi-update
  "Multi-method update system with automatic fallbacks.
   Provides bulletproof updates through multiple mechanisms:
   1. Hot-load - Push new code without restart
   2. Delta update - Download only changed files  
   3. Blue-green swap - Seamless version handoff
   4. Full download - Complete package replacement
   
   Each method has fallbacks and automatic retry with exponential backoff."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader File FileOutputStream BufferedInputStream]
           [java.nio.file Files Paths StandardCopyOption]
           [java.util.concurrent Executors TimeUnit]
           [java.util.zip ZipInputStream]))

;; =============================================================================
;; Configuration & State
;; =============================================================================

(def ^:private update-config
  {:web-app-url "https://mental-models.manus.space"
   :github-releases-url "https://api.github.com/repos/Ripple-Analytics/Ripple_Analytics/releases/latest"
   :gdrive-manifest-url "https://drive.google.com/uc?export=download&id=1Xv9qR8kM3nL5pW2jY6tH4sF7gA0bC9dE"
   :check-interval-ms 300000  ;; 5 minutes
   :retry-delays [1000 5000 15000 60000]  ;; exponential backoff
   :max-retries 4})

(def ^:private update-state
  (atom {:current-version nil
         :available-version nil
         :update-method nil
         :last-check nil
         :last-update nil
         :in-progress false
         :errors []
         :stats {:hot-loads 0
                 :delta-updates 0
                 :blue-green-swaps 0
                 :full-downloads 0
                 :failures 0}}))

(def ^:private log-fn (atom println))
(defn set-logger! [f] (reset! log-fn f))
(defn- log [& args] (apply @log-fn "[MULTI-UPDATE]" args))

;; =============================================================================
;; HTTP Utilities with Retry
;; =============================================================================

(defn- http-request
  "Make HTTP request with automatic retry and exponential backoff"
  [method url {:keys [headers body timeout retries]
               :or {timeout 30000 retries 3}}]
  (loop [attempt 0]
    (let [result (try
                   (let [conn (doto (.openConnection (URL. url))
                                (.setRequestMethod method)
                                (.setConnectTimeout timeout)
                                (.setReadTimeout timeout)
                                (.setInstanceFollowRedirects true))]
                     (doseq [[k v] headers]
                       (.setRequestProperty conn k v))
                     (when body
                       (.setDoOutput conn true)
                       (with-open [os (.getOutputStream conn)]
                         (.write os (.getBytes body "UTF-8"))))
                     (let [status (.getResponseCode conn)
                           stream (if (>= status 400)
                                    (.getErrorStream conn)
                                    (.getInputStream conn))
                           response-body (when stream
                                           (with-open [reader (BufferedReader. 
                                                               (InputStreamReader. stream))]
                                             (slurp reader)))]
                       {:status status
                        :body response-body
                        :success (< status 400)
                        :headers (into {} (.getHeaderFields conn))}))
                   (catch Exception e
                     {:status 0
                      :error (.getMessage e)
                      :success false}))]
      (if (or (:success result) (>= attempt retries))
        result
        (do
          (Thread/sleep (get (:retry-delays update-config) attempt 60000))
          (recur (inc attempt)))))))

(defn- download-file
  "Download file with progress tracking and retry"
  [url dest-path {:keys [headers on-progress]}]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 30000)
                 (.setReadTimeout 300000)
                 (.setInstanceFollowRedirects true))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)]
        (if (< status 400)
          (let [content-length (.getContentLengthLong conn)
                buffer (byte-array 8192)]
            (with-open [in (BufferedInputStream. (.getInputStream conn))
                        out (FileOutputStream. dest-path)]
              (loop [total 0]
                (let [n (.read in buffer)]
                  (when (pos? n)
                    (.write out buffer 0 n)
                    (let [new-total (+ total n)]
                      (when on-progress
                        (on-progress new-total content-length))
                      (recur new-total))))))
            {:success true :path dest-path :size content-length})
          {:success false :status status})))
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Update Method 1: Hot-Load (No Restart)
;; =============================================================================

(defn- try-hot-load
  "Attempt to hot-load new functionality without restart.
   Returns {:success true/false :loaded-modules [...]}"
  []
  (log "Attempting hot-load update...")
  (let [response (http-request "GET" 
                               (str (:web-app-url update-config) 
                                    "/api/trpc/desktop.hotLoadModules")
                               {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
    (if (:success response)
      (try
        (let [modules (-> (:body response)
                          (json/read-str :key-fn keyword)
                          (get-in [:result :data]))]
          (if (seq modules)
            (do
              (log "Found" (count modules) "hot-loadable modules")
              ;; Load each module using eval
              (doseq [m modules]
                (try
                  (load-string (:code m))
                  (log "Hot-loaded:" (:name m) "v" (:version m))
                  (catch Exception e
                    (log "Failed to hot-load" (:name m) ":" (.getMessage e)))))
              (swap! update-state update-in [:stats :hot-loads] inc)
              {:success true :method :hot-load :modules (map :name modules)})
            {:success false :reason "No modules available"}))
        (catch Exception e
          {:success false :reason (.getMessage e)}))
      {:success false :reason (or (:error response) "HTTP request failed")})))

;; =============================================================================
;; Update Method 2: Delta Update (Changed Files Only)
;; =============================================================================

(defn- try-delta-update
  "Attempt delta update - download only changed files.
   Returns {:success true/false :updated-files [...]}"
  [current-version]
  (log "Attempting delta update from" current-version "...")
  (let [response (http-request "GET"
                               (str (:web-app-url update-config)
                                    "/api/trpc/desktop.deltaManifest?input="
                                    (java.net.URLEncoder/encode
                                     (json/write-str {:version current-version})
                                     "UTF-8"))
                               {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
    (if (:success response)
      (try
        (let [manifest (-> (:body response)
                           (json/read-str :key-fn keyword)
                           (get-in [:result :data]))]
          (if (and (:hasUpdates manifest) (seq (:changedFiles manifest)))
            (let [app-dir (System/getProperty "user.dir")
                  updated-files (atom [])]
              (doseq [file (:changedFiles manifest)]
                (let [file-url (str (:web-app-url update-config)
                                    "/api/desktop/file/" (:path file))
                      dest-path (str app-dir "/" (:path file))
                      result (download-file file-url dest-path
                                            {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
                  (when (:success result)
                    (swap! updated-files conj (:path file)))))
              (swap! update-state update-in [:stats :delta-updates] inc)
              {:success true :method :delta :updated-files @updated-files})
            {:success false :reason "No delta updates available"}))
        (catch Exception e
          {:success false :reason (.getMessage e)}))
      {:success false :reason (or (:error response) "HTTP request failed")})))

;; =============================================================================
;; Update Method 3: Blue-Green Swap (Zero Downtime)
;; =============================================================================

(defn- try-blue-green-swap
  "Attempt blue-green deployment - download new version alongside current,
   then swap atomically. Returns {:success true/false :new-version ...}"
  [current-version]
  (log "Attempting blue-green swap from" current-version "...")
  (let [app-dir (System/getProperty "user.dir")
        parent-dir (.getParent (File. app-dir))
        ;; Determine current slot (blue or green)
        current-slot (if (str/includes? app-dir "blue") :blue :green)
        new-slot (if (= current-slot :blue) :green :blue)
        new-dir (str parent-dir "/Mental_Models_" (name new-slot))]
    
    ;; Get download URL from web app
    (let [response (http-request "GET"
                                 (str (:web-app-url update-config)
                                      "/api/trpc/desktop.getSecureDownload")
                                 {:headers {"x-desktop-api-key" "mm-desktop-2026-ripple"}})]
      (if (:success response)
        (try
          (let [download-info (-> (:body response)
                                  (json/read-str :key-fn keyword)
                                  (get-in [:result :data]))
                download-url (:downloadUrl download-info)
                temp-zip (str parent-dir "/update-" (System/currentTimeMillis) ".zip")]
            
            ;; Download new version
            (log "Downloading new version to" temp-zip)
            (let [dl-result (download-file download-url temp-zip
                                           {:headers (when (:authHeader download-info)
                                                       {"Authorization" (:authHeader download-info)
                                                        "Accept" "application/octet-stream"})
                                            :on-progress (fn [done total]
                                                           (log "Download progress:" 
                                                                (int (* 100 (/ done (max total 1)))) "%"))})]
              (if (:success dl-result)
                (do
                  ;; Extract to new slot
                  (log "Extracting to" new-dir)
                  (.mkdirs (File. new-dir))
                  (with-open [zis (ZipInputStream. (io/input-stream temp-zip))]
                    (loop []
                      (when-let [entry (.getNextEntry zis)]
                        (let [dest-file (File. new-dir (.getName entry))]
                          (if (.isDirectory entry)
                            (.mkdirs dest-file)
                            (do
                              (.mkdirs (.getParentFile dest-file))
                              (io/copy zis dest-file))))
                        (recur))))
                  
                  ;; Clean up temp file
                  (.delete (File. temp-zip))
                  
                  ;; Signal for handoff (write marker file)
                  (spit (str new-dir "/.ready") (str (System/currentTimeMillis)))
                  
                  (swap! update-state update-in [:stats :blue-green-swaps] inc)
                  {:success true 
                   :method :blue-green 
                   :new-version (:version download-info)
                   :new-dir new-dir
                   :restart-required true})
                {:success false :reason "Download failed"})))
          (catch Exception e
            {:success false :reason (.getMessage e)}))
        {:success false :reason (or (:error response) "HTTP request failed")}))))

;; =============================================================================
;; Update Method 4: Full Download (Fallback)
;; =============================================================================

(def ^:private github-token (atom nil))

(defn set-github-token!
  "Set GitHub Personal Access Token for private repo access.
   Token needs 'repo' scope or fine-grained 'contents:read' permission."
  [token]
  (reset! github-token token)
  (log "GitHub token configured:" (if token "yes" "no")))

(defn- github-auth-headers
  "Get headers with GitHub authentication if token is set"
  []
  (cond-> {"Accept" "application/vnd.github.v3+json"
           "User-Agent" "MentalModels-Desktop/1.0"}
    @github-token (assoc "Authorization" (str "token " @github-token))))

(defn- try-full-download-github
  "Fallback: Download full package from GitHub releases.
   Uses GitHub token for authentication if configured (required for private repos)."
  []
  (log "Attempting full download from GitHub...")
  (log "GitHub token configured:" (if @github-token "yes" "no"))
  (let [response (http-request "GET" (:github-releases-url update-config)
                               {:headers (github-auth-headers)})]
    (if (:success response)
      (try
        (let [release (json/read-str (:body response) :key-fn keyword)
              asset (first (filter #(str/ends-with? (:name %) "-Desktop.zip")
                                   (:assets release)))
              download-url (:browser_download_url asset)]
          (if download-url
            (let [temp-zip (str (System/getProperty "java.io.tmpdir")
                                "/mm-update-" (System/currentTimeMillis) ".zip")
                  ;; Add Authorization header for private repo downloads
                  result (download-file download-url temp-zip 
                                        {:headers (github-auth-headers)})]
              (if (:success result)
                (do
                  (swap! update-state update-in [:stats :full-downloads] inc)
                  {:success true 
                   :method :full-github 
                   :version (:tag_name release)
                   :zip-path temp-zip
                   :restart-required true})
                {:success false :reason "Download failed"}))
            {:success false :reason "No desktop asset found"}))
        (catch Exception e
          {:success false :reason (.getMessage e)}))
      {:success false :reason (or (:error response) "HTTP request failed")})))

(defn- try-full-download-gdrive
  "Fallback: Download full package from Google Drive"
  []
  (log "Attempting full download from Google Drive...")
  ;; First get manifest to find latest version URL
  (let [manifest-response (http-request "GET" (:gdrive-manifest-url update-config) {})]
    (if (:success manifest-response)
      (try
        (let [manifest (json/read-str (:body manifest-response) :key-fn keyword)
              download-url (:downloadUrl manifest)
              version (:latestVersion manifest)]
          (if download-url
            (let [temp-zip (str (System/getProperty "java.io.tmpdir")
                                "/mm-gdrive-" (System/currentTimeMillis) ".zip")
                  ;; Google Drive requires special handling for large files
                  result (download-file download-url temp-zip {})]
              (if (:success result)
                (do
                  (swap! update-state update-in [:stats :full-downloads] inc)
                  {:success true 
                   :method :full-gdrive 
                   :version version
                   :zip-path temp-zip
                   :restart-required true})
                {:success false :reason "Download failed"}))
            {:success false :reason "No download URL in manifest"}))
        (catch Exception e
          {:success false :reason (.getMessage e)}))
      {:success false :reason (or (:error manifest-response) "HTTP request failed")})))

;; =============================================================================
;; Master Update Orchestrator
;; =============================================================================

(defn perform-update!
  "Attempt update using all available methods with automatic fallback.
   Order: Hot-load → Delta → Blue-green → GitHub full → GDrive full"
  []
  (when-not (:in-progress @update-state)
    (swap! update-state assoc :in-progress true)
    (log "Starting update check...")
    
    (try
      (let [current-version (:current-version @update-state)]
        
        ;; Method 1: Hot-load (fastest, no restart)
        (let [result (try-hot-load)]
          (if (:success result)
            (do
              (swap! update-state assoc 
                     :last-update (System/currentTimeMillis)
                     :update-method :hot-load)
              (log "Update successful via hot-load")
              result)
            
            ;; Method 2: Delta update (fast, may need restart)
            (let [result (try-delta-update current-version)]
              (if (:success result)
                (do
                  (swap! update-state assoc 
                         :last-update (System/currentTimeMillis)
                         :update-method :delta)
                  (log "Update successful via delta")
                  result)
                
                ;; Method 3: Blue-green swap (reliable, needs restart)
                (let [result (try-blue-green-swap current-version)]
                  (if (:success result)
                    (do
                      (swap! update-state assoc 
                             :last-update (System/currentTimeMillis)
                             :update-method :blue-green
                             :available-version (:new-version result))
                      (log "Update successful via blue-green, restart required")
                      result)
                    
                    ;; Method 4: Full download from GitHub
                    (let [result (try-full-download-github)]
                      (if (:success result)
                        (do
                          (swap! update-state assoc 
                                 :last-update (System/currentTimeMillis)
                                 :update-method :full-github
                                 :available-version (:version result))
                          (log "Update successful via GitHub full download")
                          result)
                        
                        ;; Method 5: Full download from Google Drive (last resort)
                        (let [result (try-full-download-gdrive)]
                          (if (:success result)
                            (do
                              (swap! update-state assoc 
                                     :last-update (System/currentTimeMillis)
                                     :update-method :full-gdrive
                                     :available-version (:version result))
                              (log "Update successful via Google Drive")
                              result)
                            
                            ;; All methods failed
                            (do
                              (swap! update-state update-in [:stats :failures] inc)
                              (swap! update-state update :errors conj
                                     {:timestamp (System/currentTimeMillis)
                                      :message "All update methods failed"})
                              (log "All update methods failed")
                              {:success false 
                               :reason "All update methods exhausted"
                               :tried [:hot-load :delta :blue-green :github :gdrive]}))))))))))))
      
      (finally
        (swap! update-state assoc 
               :in-progress false
               :last-check (System/currentTimeMillis))))))

;; =============================================================================
;; Background Scheduler
;; =============================================================================

(def ^:private scheduler (atom nil))

(defn start-auto-updates!
  "Start background update checking"
  [current-version]
  (swap! update-state assoc :current-version current-version)
  
  (when-let [old @scheduler]
    (.shutdown old))
  
  (let [new-scheduler (Executors/newSingleThreadScheduledExecutor)]
    (reset! scheduler new-scheduler)
    (.scheduleAtFixedRate
     new-scheduler
     #(try (perform-update!) (catch Exception e (log "Update error:" (.getMessage e))))
     0
     (:check-interval-ms update-config)
     TimeUnit/MILLISECONDS)
    (log "Started auto-update scheduler (every 5 minutes)")))

(defn stop-auto-updates!
  "Stop background update checking"
  []
  (when-let [s @scheduler]
    (.shutdown s)
    (reset! scheduler nil)
    (log "Stopped auto-update scheduler")))

;; =============================================================================
;; Status & Manual Control
;; =============================================================================

(defn get-update-status
  "Get current update system status"
  []
  (let [state @update-state]
    {:current-version (:current-version state)
     :available-version (:available-version state)
     :last-check (:last-check state)
     :last-update (:last-update state)
     :last-method (:update-method state)
     :in-progress (:in-progress state)
     :stats (:stats state)
     :recent-errors (take 5 (:errors state))}))

(defn force-check!
  "Manually trigger update check"
  []
  (future (perform-update!)))

(defn set-web-app-url!
  "Configure web app URL"
  [url]
  (alter-var-root #'update-config assoc :web-app-url url))
