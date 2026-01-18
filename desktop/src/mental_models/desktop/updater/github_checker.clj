(ns mental-models.desktop.updater.github-checker
  "GitHub Update Checker - Polls for new commits/releases every 5 minutes
   
   Features:
   - Checks GitHub API for latest commit SHA
   - Compares against local version
   - Downloads updates when available
   - Supports both release tags and branch commits"
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.time Instant]
           [java.util.concurrent ScheduledExecutorService Executors TimeUnit]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:github-owner "Ripple-Analytics"
         :github-repo "Ripple_Analytics"
         :branch "main"
         :check-interval-ms (* 5 60 1000)  ;; 5 minutes
         :local-version-file (str (System/getProperty "user.home") "/.mental-models/version.txt")
         :staging-dir (str (System/getProperty "user.home") "/.mental-models/staging")
         :github-token nil  ;; Optional: for private repos or higher rate limits
         :auto-update true}))

(defonce update-state
  (atom {:current-sha nil
         :latest-sha nil
         :last-check nil
         :last-updated nil      ;; Timestamp when code was last updated
         :outdated-days 0       ;; Days since last update
         :update-available false
         :downloading false
         :download-progress 0
         :last-error nil
         :check-count 0}))

(defonce scheduler (atom nil))
(defonce update-channel (chan 100))

;; =============================================================================
;; GitHub API
;; =============================================================================

(defn github-api-url [endpoint]
  (str "https://api.github.com/repos/" 
       (:github-owner @config) "/" 
       (:github-repo @config) 
       endpoint))

(defn make-github-request [url]
  (try
    (let [headers (cond-> {"Accept" "application/vnd.github.v3+json"
                           "User-Agent" "MentalModels-Desktop"}
                    (:github-token @config)
                    (assoc "Authorization" (str "token " (:github-token @config))))
          response (http/get url {:headers headers
                                  :as :json
                                  :socket-timeout 10000
                                  :connection-timeout 5000})]
      {:success true
       :data (:body response)
       :rate-limit-remaining (get-in response [:headers "X-RateLimit-Remaining"])})
    (catch Exception e
      (log/error e "GitHub API request failed:" url)
      {:success false
       :error (.getMessage e)})))

(defn get-latest-commit []
  "Get the latest commit SHA from the configured branch"
  (let [url (github-api-url (str "/commits/" (:branch @config)))
        result (make-github-request url)]
    (when (:success result)
      {:sha (get-in result [:data :sha])
       :message (get-in result [:data :commit :message])
       :author (get-in result [:data :commit :author :name])
       :date (get-in result [:data :commit :author :date])})))

(defn get-latest-release []
  "Get the latest release (if using release-based updates)"
  (let [url (github-api-url "/releases/latest")
        result (make-github-request url)]
    (when (:success result)
      {:tag (get-in result [:data :tag_name])
       :name (get-in result [:data :name])
       :body (get-in result [:data :body])
       :published (get-in result [:data :published_at])
       :assets (get-in result [:data :assets])})))

(defn get-commit-diff [old-sha new-sha]
  "Get the list of changed files between two commits"
  (let [url (github-api-url (str "/compare/" old-sha "..." new-sha))
        result (make-github-request url)]
    (when (:success result)
      {:ahead-by (get-in result [:data :ahead_by])
       :commits (map #(select-keys % [:sha :commit]) 
                     (get-in result [:data :commits]))
       :files (map #(select-keys % [:filename :status :additions :deletions])
                   (get-in result [:data :files]))})))

;; =============================================================================
;; Version Management
;; =============================================================================

(defn read-local-version []
  "Read the current local version SHA"
  (let [version-file (io/file (:local-version-file @config))]
    (when (.exists version-file)
      (try
        (-> (slurp version-file)
            clojure.string/trim)
        (catch Exception e
          (log/warn "Failed to read version file:" (.getMessage e))
          nil)))))

(defn write-local-version [sha]
  "Write the current version SHA to disk and track update timestamp"
  (let [version-file (io/file (:local-version-file @config))
        timestamp-file (io/file (str (:local-version-file @config) ".timestamp"))
        now (str (Instant/now))]
    (io/make-parents version-file)
    (spit version-file sha)
    (spit timestamp-file now)
    (swap! update-state assoc :last-updated now :outdated-days 0)
    (log/info "Updated local version to:" sha)))

(defn read-last-updated []
  "Read when the code was last updated"
  (let [timestamp-file (io/file (str (:local-version-file @config) ".timestamp"))]
    (when (.exists timestamp-file)
      (try
        (clojure.string/trim (slurp timestamp-file))
        (catch Exception _ nil)))))

(defn calculate-outdated-days []
  "Calculate how many days since last update"
  (if-let [last-updated (read-last-updated)]
    (try
      (let [updated-instant (Instant/parse last-updated)
            now (Instant/now)
            duration (java.time.Duration/between updated-instant now)]
        (.toDays duration))
      (catch Exception _ 0))
    0))

(defn version-changed? [new-sha]
  "Check if the version has changed"
  (let [current (or (:current-sha @update-state) (read-local-version))]
    (and new-sha current (not= new-sha current))))

;; =============================================================================
;; Download & Staging
;; =============================================================================

(defn download-archive [sha]
  "Download the repository archive for a specific commit"
  (let [url (str "https://github.com/" 
                 (:github-owner @config) "/" 
                 (:github-repo @config) 
                 "/archive/" sha ".zip")
        staging-dir (io/file (:staging-dir @config))
        archive-file (io/file staging-dir (str sha ".zip"))]
    (io/make-parents archive-file)
    (swap! update-state assoc :downloading true :download-progress 0)
    (try
      (log/info "Downloading update:" sha)
      (with-open [in (io/input-stream url)
                  out (io/output-stream archive-file)]
        (io/copy in out))
      (swap! update-state assoc :downloading false :download-progress 100)
      (log/info "Download complete:" (.getAbsolutePath archive-file))
      {:success true :file archive-file}
      (catch Exception e
        (swap! update-state assoc :downloading false :last-error (.getMessage e))
        (log/error e "Failed to download update")
        {:success false :error (.getMessage e)}))))

(defn extract-archive [archive-file target-dir]
  "Extract the downloaded archive"
  (try
    (log/info "Extracting:" (.getName archive-file) "to" target-dir)
    ;; Use Java's built-in zip support
    (let [zip-file (java.util.zip.ZipFile. archive-file)]
      (doseq [entry (enumeration-seq (.entries zip-file))]
        (let [entry-name (.getName entry)
              target-file (io/file target-dir entry-name)]
          (if (.isDirectory entry)
            (.mkdirs target-file)
            (do
              (io/make-parents target-file)
              (with-open [in (.getInputStream zip-file entry)
                          out (io/output-stream target-file)]
                (io/copy in out))))))
      (.close zip-file))
    {:success true}
    (catch Exception e
      (log/error e "Failed to extract archive")
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Update Check Loop
;; =============================================================================

(defn check-for-updates []
  "Check GitHub for updates and notify if available"
  (log/debug "Checking for updates...")
  (swap! update-state update :check-count inc)
  (swap! update-state assoc :last-check (str (Instant/now)))
  
  (if-let [latest (get-latest-commit)]
    (do
      (swap! update-state assoc :latest-sha (:sha latest) :last-error nil)
      (when (nil? (:current-sha @update-state))
        (swap! update-state assoc :current-sha (or (read-local-version) (:sha latest))))
      
      (if (version-changed? (:sha latest))
        (do
          (log/info "🔄 Update available!" 
                    "\n  Current:" (:current-sha @update-state)
                    "\n  Latest:" (:sha latest)
                    "\n  Message:" (:message latest)
                    "\n  Author:" (:author latest))
          (swap! update-state assoc :update-available true)
          ;; Send update notification to channel
          (async/put! update-channel {:type :update-available
                                      :current (:current-sha @update-state)
                                      :latest latest}))
        (log/debug "Already on latest version:" (:sha latest))))
    (do
      (log/warn "Failed to check for updates")
      (swap! update-state assoc :last-error "Failed to fetch latest commit"))))

(defn start-update-checker! []
  "Start the background update checker (runs every 5 minutes)"
  (when @scheduler
    (log/warn "Update checker already running"))
  
  (log/info "Starting update checker (interval:" (/ (:check-interval-ms @config) 60000) "minutes)")
  
  ;; Initialize current version and last-updated tracking
  (when-let [local-version (read-local-version)]
    (swap! update-state assoc :current-sha local-version))
  (when-let [last-updated (read-last-updated)]
    (swap! update-state assoc 
           :last-updated last-updated
           :outdated-days (calculate-outdated-days)))
  
  ;; Run initial check
  (check-for-updates)
  
  ;; Schedule periodic checks
  (let [executor (Executors/newSingleThreadScheduledExecutor)]
    (reset! scheduler executor)
    (.scheduleAtFixedRate executor
                          #(try (check-for-updates)
                                (catch Exception e
                                  (log/error e "Update check failed")))
                          (:check-interval-ms @config)
                          (:check-interval-ms @config)
                          TimeUnit/MILLISECONDS)
    (log/info "Update checker started")))

(defn stop-update-checker! []
  "Stop the background update checker"
  (when @scheduler
    (log/info "Stopping update checker")
    (.shutdown @scheduler)
    (reset! scheduler nil)))

;; =============================================================================
;; Manual Update Commands
;; =============================================================================

(defn force-check! []
  "Force an immediate update check"
  (check-for-updates))

(defn get-update-status []
  "Get the current update status"
  @update-state)

(defn set-github-token! [token]
  "Set GitHub token for private repos or higher rate limits"
  (swap! config assoc :github-token token))

(defn set-check-interval! [minutes]
  "Change the update check interval (in minutes)"
  (swap! config assoc :check-interval-ms (* minutes 60 1000))
  (when @scheduler
    (stop-update-checker!)
    (start-update-checker!)))
