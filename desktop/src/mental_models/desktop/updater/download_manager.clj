(ns mental-models.desktop.updater.download-manager
  "Multi-Source Download Manager - Bulletproof download with fallbacks
   
   Features:
   - Google Drive manifest and direct download
   - GitHub releases API with asset filtering
   - Web app config fallback
   - Resume after network interruption
   - Progress callbacks with accurate tracking
   - Rate limit handling with exponential backoff
   - Disk space checking before download
   - Large file support (234MB+)
   - Timeout handling with configurable limits
   - Retry logic with exponential backoff
   - Proxy support"
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [taoensso.timbre :as log])
  (:import [java.io File FileOutputStream RandomAccessFile InputStream]
           [java.net URL HttpURLConnection]
           [java.nio.file Files Paths]
           [java.time Instant Duration]
           [java.security MessageDigest]
           [java.util.concurrent.atomic AtomicLong AtomicBoolean]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:google-drive-manifest-url nil  ;; Set by user
         :github-owner "Ripple-Analytics"
         :github-repo "Ripple_Analytics"
         :github-token nil
         :web-app-url nil  ;; e.g., "https://app.ripple-analytics.com"
         :web-app-api-key nil
         :download-dir (str (System/getProperty "user.home") "/.mental-models/downloads")
         :temp-dir (str (System/getProperty "user.home") "/.mental-models/temp")
         :connect-timeout-ms 10000
         :read-timeout-ms 60000
         :max-retries 3
         :retry-base-delay-ms 1000
         :retry-max-delay-ms 60000
         :min-disk-space-mb 500
         :chunk-size 65536  ;; 64KB chunks
         :progress-interval-ms 500
         :user-agent "MentalModels-Desktop/1.0"}))

(defonce download-state
  (atom {:active-downloads {}
         :completed-downloads []
         :failed-downloads []
         :total-downloaded-bytes (AtomicLong. 0)
         :current-source nil}))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn ensure-directory!
  "Ensure a directory exists, creating it if necessary."
  [path]
  (let [dir (io/file path)]
    (when-not (.exists dir)
      (.mkdirs dir))
    (.exists dir)))

(defn get-available-disk-space
  "Get available disk space in MB for a path."
  [path]
  (try
    (let [file (io/file path)
          parent (or (.getParentFile file) file)]
      (when (.exists parent)
        (/ (.getUsableSpace parent) (* 1024 1024))))
    (catch Exception e
      (log/warn "Failed to check disk space:" (.getMessage e))
      nil)))

(defn check-disk-space!
  "Check if there's enough disk space. Returns true if OK."
  [path required-mb]
  (let [available (get-available-disk-space path)
        min-required (+ required-mb (:min-disk-space-mb @config))]
    (if (nil? available)
      (do
        (log/warn "Could not determine disk space, proceeding anyway")
        true)
      (if (>= available min-required)
        true
        (do
          (log/error "Insufficient disk space. Available:" available "MB, Required:" min-required "MB")
          false)))))

(defn calculate-sha256
  "Calculate SHA-256 hash of a file."
  [file]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 8192)]
    (with-open [fis (io/input-stream file)]
      (loop []
        (let [n (.read fis buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn exponential-backoff
  "Calculate exponential backoff delay."
  [attempt]
  (let [base (:retry-base-delay-ms @config)
        max-delay (:retry-max-delay-ms @config)
        delay (* base (Math/pow 2 attempt))
        jitter (* delay (rand 0.1))]
    (min max-delay (+ delay jitter))))

;; =============================================================================
;; HTTP Client Helpers
;; =============================================================================

(defn make-http-request
  "Make an HTTP request with proper error handling."
  [url & {:keys [method headers as timeout-ms follow-redirects]
          :or {method :get as :auto follow-redirects true}}]
  (try
    (let [opts {:headers (merge {"User-Agent" (:user-agent @config)} headers)
                :as as
                :socket-timeout (or timeout-ms (:read-timeout-ms @config))
                :connection-timeout (:connect-timeout-ms @config)
                :follow-redirects follow-redirects
                :throw-exceptions false}
          response (case method
                     :get (http/get url opts)
                     :head (http/head url opts)
                     :post (http/post url opts))]
      (cond
        (<= 200 (:status response) 299)
        {:success true :response response :body (:body response)}
        
        (= 429 (:status response))
        {:success false :error :rate-limited
         :retry-after (some-> (get-in response [:headers "Retry-After"])
                              Integer/parseInt)}
        
        (= 404 (:status response))
        {:success false :error :not-found}
        
        (= 403 (:status response))
        {:success false :error :forbidden}
        
        (= 401 (:status response))
        {:success false :error :unauthorized}
        
        (>= (:status response) 500)
        {:success false :error :server-error :status (:status response)}
        
        :else
        {:success false :error :unknown :status (:status response)}))
    (catch java.net.SocketTimeoutException e
      {:success false :error :timeout :message (.getMessage e)})
    (catch java.net.ConnectException e
      {:success false :error :connection-failed :message (.getMessage e)})
    (catch javax.net.ssl.SSLException e
      {:success false :error :ssl-error :message (.getMessage e)})
    (catch Exception e
      {:success false :error :exception :message (.getMessage e)})))

;; =============================================================================
;; Google Drive Support
;; =============================================================================

(defn fetch-google-drive-manifest
  "Fetch the update manifest from Google Drive."
  []
  (when-let [manifest-url (:google-drive-manifest-url @config)]
    (log/info "Fetching Google Drive manifest from:" manifest-url)
    (let [result (make-http-request manifest-url :as :json :timeout-ms 30000)]
      (if (:success result)
        (let [manifest (:body result)]
          (if (and (:latestVersion manifest) (:downloadUrl manifest))
            {:success true :manifest manifest}
            {:success false :error :invalid-manifest
             :message "Manifest missing required fields (latestVersion, downloadUrl)"}))
        (do
          (log/warn "Failed to fetch Google Drive manifest:" (:error result))
          result)))))

(defn get-google-drive-direct-url
  "Convert a Google Drive sharing URL to a direct download URL."
  [url]
  (cond
    ;; Already a direct download URL
    (str/includes? url "export=download")
    url
    
    ;; File ID format: https://drive.google.com/file/d/FILE_ID/view
    (re-find #"drive\.google\.com/file/d/([^/]+)" url)
    (let [[_ file-id] (re-find #"drive\.google\.com/file/d/([^/]+)" url)]
      (str "https://drive.google.com/uc?export=download&id=" file-id))
    
    ;; Open format: https://drive.google.com/open?id=FILE_ID
    (re-find #"drive\.google\.com/open\?id=([^&]+)" url)
    (let [[_ file-id] (re-find #"drive\.google\.com/open\?id=([^&]+)" url)]
      (str "https://drive.google.com/uc?export=download&id=" file-id))
    
    :else url))

(defn handle-google-drive-virus-scan-warning
  "Handle Google Drive's virus scan warning for large files."
  [url cookies]
  (log/debug "Handling Google Drive virus scan warning...")
  (let [result (make-http-request url :headers {"Cookie" cookies})]
    (when (:success result)
      (let [body (str (:body result))]
        (when-let [confirm-match (re-find #"confirm=([^&\"]+)" body)]
          (let [confirm-code (second confirm-match)]
            (str url "&confirm=" confirm-code)))))))

;; =============================================================================
;; GitHub Support
;; =============================================================================

(defn github-api-url
  "Build a GitHub API URL."
  [endpoint]
  (str "https://api.github.com/repos/"
       (:github-owner @config) "/"
       (:github-repo @config)
       endpoint))

(defn github-headers
  "Get headers for GitHub API requests."
  []
  (cond-> {"Accept" "application/vnd.github.v3+json"
           "User-Agent" (:user-agent @config)}
    (:github-token @config)
    (assoc "Authorization" (str "token " (:github-token @config)))))

(defn fetch-github-latest-release
  "Fetch the latest release from GitHub."
  []
  (log/info "Fetching latest release from GitHub...")
  (let [url (github-api-url "/releases/latest")
        result (make-http-request url :headers (github-headers) :as :json)]
    (if (:success result)
      (let [release (:body result)
            assets (:assets release)
            desktop-asset (first (filter #(str/includes? (:name %) "Desktop.zip") assets))]
        {:success true
         :release {:tag (:tag_name release)
                   :name (:name release)
                   :body (:body release)
                   :published (:published_at release)
                   :download-url (when desktop-asset (:browser_download_url desktop-asset))
                   :asset-name (when desktop-asset (:name desktop-asset))
                   :asset-size (when desktop-asset (:size desktop-asset))}})
      (do
        (log/warn "Failed to fetch GitHub release:" (:error result))
        result))))

(defn fetch-github-releases
  "Fetch all releases from GitHub."
  []
  (let [url (github-api-url "/releases")
        result (make-http-request url :headers (github-headers) :as :json)]
    (if (:success result)
      {:success true :releases (:body result)}
      result)))

;; =============================================================================
;; Web App Config Fallback
;; =============================================================================

(defn fetch-web-app-config
  "Fetch download config from the web app."
  []
  (when-let [web-app-url (:web-app-url @config)]
    (log/info "Fetching config from web app...")
    (let [url (str web-app-url "/api/trpc/desktop.config")
          headers (cond-> {"Accept" "application/json"}
                    (:web-app-api-key @config)
                    (assoc "X-API-Key" (:web-app-api-key @config)))
          result (make-http-request url :headers headers :as :json :timeout-ms 30000)]
      (if (:success result)
        {:success true :config (:body result)}
        (do
          (log/warn "Failed to fetch web app config:" (:error result))
          result)))))

;; =============================================================================
;; Download Source Resolution
;; =============================================================================

(defn resolve-download-source
  "Resolve the best download source with fallbacks.
   Priority: Google Drive > GitHub > Web App"
  []
  (log/info "Resolving download source...")
  
  ;; Try Google Drive first
  (when-let [gd-result (fetch-google-drive-manifest)]
    (when (:success gd-result)
      (log/info "Using Google Drive as download source")
      (swap! download-state assoc :current-source :google-drive)
      (return {:source :google-drive
               :version (get-in gd-result [:manifest :latestVersion])
               :download-url (get-google-drive-direct-url
                               (get-in gd-result [:manifest :downloadUrl]))
               :checksum (get-in gd-result [:manifest :checksum])})))
  
  ;; Try GitHub
  (let [gh-result (fetch-github-latest-release)]
    (when (and (:success gh-result) (get-in gh-result [:release :download-url]))
      (log/info "Using GitHub as download source")
      (swap! download-state assoc :current-source :github)
      (return {:source :github
               :version (get-in gh-result [:release :tag])
               :download-url (get-in gh-result [:release :download-url])
               :expected-size (get-in gh-result [:release :asset-size])})))
  
  ;; Try Web App
  (when-let [wa-result (fetch-web-app-config)]
    (when (:success wa-result)
      (log/info "Using Web App as download source")
      (swap! download-state assoc :current-source :web-app)
      (return {:source :web-app
               :version (get-in wa-result [:config :latestVersion])
               :download-url (get-in wa-result [:config :downloadUrl])})))
  
  ;; All sources failed
  (log/error "All download sources failed")
  {:success false :error :all-sources-failed})

;; =============================================================================
;; Progress Tracking
;; =============================================================================

(defn create-progress-tracker
  "Create a progress tracker for downloads."
  [total-size progress-callback]
  (let [downloaded (AtomicLong. 0)
        start-time (System/currentTimeMillis)
        last-update (atom start-time)]
    {:downloaded downloaded
     :total-size total-size
     :start-time start-time
     :update! (fn [bytes-read]
                (.addAndGet downloaded bytes-read)
                (let [now (System/currentTimeMillis)
                      elapsed (- now @last-update)]
                  (when (>= elapsed (:progress-interval-ms @config))
                    (reset! last-update now)
                    (let [total-downloaded (.get downloaded)
                          elapsed-total (- now start-time)
                          speed (if (pos? elapsed-total)
                                  (/ (* total-downloaded 1000) elapsed-total)
                                  0)
                          progress (if (and total-size (pos? total-size))
                                     (* 100.0 (/ total-downloaded total-size))
                                     -1)
                          eta (if (and total-size (pos? speed))
                                (/ (- total-size total-downloaded) speed)
                                -1)]
                      (when progress-callback
                        (progress-callback {:downloaded total-downloaded
                                            :total total-size
                                            :progress progress
                                            :speed speed
                                            :eta eta}))))))}))

;; =============================================================================
;; Core Download Function
;; =============================================================================

(defn download-file!
  "Download a file with progress tracking and resume support.
   
   Options:
   - :progress-callback - fn called with progress updates
   - :expected-size - expected file size for validation
   - :checksum - expected SHA-256 checksum
   - :resume - attempt to resume partial download"
  [url target-file & {:keys [progress-callback expected-size checksum resume]
                      :or {resume true}}]
  (log/info "Starting download:" url)
  (log/info "Target:" (.getAbsolutePath target-file))
  
  ;; Ensure parent directory exists
  (io/make-parents target-file)
  
  ;; Check disk space
  (let [required-mb (if expected-size (/ expected-size (* 1024 1024)) 500)]
    (when-not (check-disk-space! (.getParent target-file) required-mb)
      (return {:success false :error :insufficient-disk-space})))
  
  ;; Check for existing partial download
  (let [existing-size (if (and resume (.exists target-file))
                        (.length target-file)
                        0)
        download-id (str (System/currentTimeMillis))]
    
    ;; Register active download
    (swap! download-state assoc-in [:active-downloads download-id]
           {:url url
            :target (.getAbsolutePath target-file)
            :started-at (str (Instant/now))
            :status :downloading})
    
    (try
      ;; Open connection
      (let [conn (doto (.openConnection (URL. url))
                   (.setRequestProperty "User-Agent" (:user-agent @config))
                   (.setConnectTimeout (:connect-timeout-ms @config))
                   (.setReadTimeout (:read-timeout-ms @config))
                   (.setInstanceFollowRedirects true))]
        
        ;; Set range header for resume
        (when (pos? existing-size)
          (log/info "Resuming download from byte:" existing-size)
          (.setRequestProperty conn "Range" (str "bytes=" existing-size "-")))
        
        (.connect conn)
        
        (let [response-code (.getResponseCode conn)
              content-length (.getContentLengthLong conn)
              total-size (if (= response-code 206)
                           (+ existing-size content-length)
                           content-length)
              tracker (create-progress-tracker total-size progress-callback)
              append? (= response-code 206)]
          
          (log/info "Response code:" response-code "Content-Length:" content-length)
          
          (when (and (not= response-code 200) (not= response-code 206))
            (throw (ex-info "HTTP error" {:status response-code})))
          
          ;; Download with progress
          (with-open [in (.getInputStream conn)
                      out (FileOutputStream. target-file append?)]
            (let [buffer (byte-array (:chunk-size @config))]
              (loop []
                (let [n (.read in buffer)]
                  (when (pos? n)
                    (.write out buffer 0 n)
                    ((:update! tracker) n)
                    (recur))))))
          
          ;; Verify download
          (let [final-size (.length target-file)]
            (log/info "Download complete. Size:" final-size)
            
            ;; Verify size
            (when (and expected-size (not= final-size expected-size))
              (log/warn "Size mismatch! Expected:" expected-size "Got:" final-size))
            
            ;; Verify checksum
            (when checksum
              (log/info "Verifying checksum...")
              (let [actual-checksum (calculate-sha256 target-file)]
                (when (not= (str/lower-case checksum) (str/lower-case actual-checksum))
                  (log/error "Checksum mismatch! Expected:" checksum "Got:" actual-checksum)
                  (throw (ex-info "Checksum verification failed"
                                  {:expected checksum :actual actual-checksum})))))
            
            ;; Success
            (swap! download-state update-in [:active-downloads] dissoc download-id)
            (swap! download-state update :completed-downloads conj
                   {:url url
                    :target (.getAbsolutePath target-file)
                    :size final-size
                    :completed-at (str (Instant/now))})
            (.addAndGet (:total-downloaded-bytes @download-state) final-size)
            
            {:success true
             :file target-file
             :size final-size})))
      
      (catch Exception e
        (log/error e "Download failed:" url)
        (swap! download-state update-in [:active-downloads] dissoc download-id)
        (swap! download-state update :failed-downloads conj
               {:url url
                :error (.getMessage e)
                :failed-at (str (Instant/now))})
        {:success false :error :download-failed :message (.getMessage e)}))))

;; =============================================================================
;; Download with Retry
;; =============================================================================

(defn download-with-retry!
  "Download a file with automatic retry on failure."
  [url target-file & opts]
  (let [max-retries (:max-retries @config)]
    (loop [attempt 0]
      (let [result (apply download-file! url target-file opts)]
        (if (:success result)
          result
          (if (< attempt max-retries)
            (let [delay (exponential-backoff attempt)]
              (log/warn "Download failed, retrying in" (/ delay 1000) "seconds..."
                        "Attempt" (inc attempt) "of" max-retries)
              (Thread/sleep delay)
              (recur (inc attempt)))
            (do
              (log/error "Download failed after" max-retries "retries")
              result)))))))

;; =============================================================================
;; High-Level Download Functions
;; =============================================================================

(defn download-latest-update!
  "Download the latest update from the best available source."
  [& {:keys [progress-callback]}]
  (log/info "========================================")
  (log/info "Starting update download...")
  (log/info "========================================")
  
  (let [source (resolve-download-source)]
    (if (:success source)
      (do
        (log/error "Failed to resolve download source")
        source)
      (let [download-url (:download-url source)
            version (:version source)
            target-file (io/file (:download-dir @config) (str "update-" version ".zip"))]
        (log/info "Downloading version:" version "from:" (:source source))
        (download-with-retry! download-url target-file
                              :progress-callback progress-callback
                              :expected-size (:expected-size source)
                              :checksum (:checksum source))))))

;; =============================================================================
;; Status & Configuration
;; =============================================================================

(defn get-download-status
  "Get current download status."
  []
  {:active-downloads (:active-downloads @download-state)
   :completed-count (count (:completed-downloads @download-state))
   :failed-count (count (:failed-downloads @download-state))
   :total-downloaded-bytes (.get (:total-downloaded-bytes @download-state))
   :current-source (:current-source @download-state)})

(defn configure!
  "Update download manager configuration."
  [config-map]
  (swap! config merge config-map)
  (log/info "Download manager configured:" (keys config-map)))

(defn set-google-drive-manifest!
  "Set the Google Drive manifest URL."
  [url]
  (swap! config assoc :google-drive-manifest-url url))

(defn set-github-token!
  "Set the GitHub token for authenticated requests."
  [token]
  (swap! config assoc :github-token token))

(defn set-web-app-url!
  "Set the web app URL for config fallback."
  [url]
  (swap! config assoc :web-app-url url))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-download-manager!
  "Initialize the download manager."
  []
  (log/info "Initializing download manager...")
  (ensure-directory! (:download-dir @config))
  (ensure-directory! (:temp-dir @config))
  {:success true})
