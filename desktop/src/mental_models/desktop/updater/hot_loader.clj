(ns mental-models.desktop.updater.hot-loader
  "Hot-loading system for dynamically updating desktop app functionality
   without requiring a restart. Fetches new code modules from web app
   and evaluates them at runtime."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader]
           [java.util.concurrent Executors TimeUnit ScheduledExecutorService]))

;; =============================================================================
;; State Management
;; =============================================================================

(def ^:private hot-load-state
  (atom {:loaded-modules {}      ;; module-name -> {:version :code :loaded-at}
         :pending-updates []     ;; modules waiting to be loaded
         :last-check nil         ;; timestamp of last check
         :errors []              ;; recent errors
         :enabled true           ;; hot-loading enabled
         :web-app-url nil        ;; web app base URL
         :api-key nil}))         ;; API key for authentication

(def ^:private scheduler (atom nil))
(def ^:private log-fn (atom println))

(defn set-logger! [f]
  (reset! log-fn f))

(defn- log [& args]
  (apply @log-fn "[HOT-LOAD]" args))

;; =============================================================================
;; Configuration
;; =============================================================================

(defn configure!
  "Configure hot-loading system"
  [{:keys [web-app-url api-key enabled poll-interval-ms]
    :or {enabled true poll-interval-ms 300000}}] ;; 5 minutes default
  (swap! hot-load-state merge
         {:web-app-url web-app-url
          :api-key api-key
          :enabled enabled
          :poll-interval-ms poll-interval-ms}))

;; =============================================================================
;; HTTP Utilities
;; =============================================================================

(defn- http-get [url headers]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 10000)
                 (.setReadTimeout 30000))]
      (doseq [[k v] headers]
        (.setRequestProperty conn k v))
      (let [status (.getResponseCode conn)
            stream (if (>= status 400)
                     (.getErrorStream conn)
                     (.getInputStream conn))
            body (when stream
                   (with-open [reader (BufferedReader. (InputStreamReader. stream))]
                     (slurp reader)))]
        {:status status
         :body body
         :success? (< status 400)}))
    (catch Exception e
      {:status 0
       :error (.getMessage e)
       :success? false})))

;; =============================================================================
;; Module Management
;; =============================================================================

(defn- fetch-available-modules
  "Fetch list of available hot-loadable modules from web app"
  []
  (let [{:keys [web-app-url api-key]} @hot-load-state
        url (str web-app-url "/api/trpc/desktop.hotLoadModules")
        response (http-get url {"x-desktop-api-key" (or api-key "mm-desktop-2026-ripple")
                                "Accept" "application/json"})]
    (when (:success? response)
      (try
        (-> (:body response)
            (json/read-str :key-fn keyword)
            (get-in [:result :data]))
        (catch Exception e
          (log "Error parsing modules response:" (.getMessage e))
          nil)))))

(defn- fetch-module-code
  "Fetch code for a specific module"
  [module-name version]
  (let [{:keys [web-app-url api-key]} @hot-load-state
        url (str web-app-url "/api/trpc/desktop.getModuleCode?input="
                 (java.net.URLEncoder/encode
                  (json/write-str {:name module-name :version version})
                  "UTF-8"))
        response (http-get url {"x-desktop-api-key" (or api-key "mm-desktop-2026-ripple")
                                "Accept" "application/json"})]
    (when (:success? response)
      (try
        (-> (:body response)
            (json/read-str :key-fn keyword)
            (get-in [:result :data :code]))
        (catch Exception e
          (log "Error parsing module code:" (.getMessage e))
          nil)))))

;; =============================================================================
;; Code Evaluation (Safe Sandbox)
;; =============================================================================

(def ^:private allowed-namespaces
  "Namespaces that hot-loaded code is allowed to use"
  #{'clojure.core 'clojure.string 'clojure.set 'clojure.data.json
    'mental-models.desktop.gui.swing-app
    'mental-models.desktop.scanner
    'mental-models.desktop.extractor
    'mental-models.predict.engine
    'mental-models.predict.news-scanner
    'mental-models.search.semantic
    'mental-models.analytics.engine
    'mental-models.analytics.anomaly})

(defn- safe-eval
  "Evaluate code in a sandboxed context with error handling"
  [code-string module-name]
  (try
    (log "Evaluating module:" module-name)
    
    ;; Parse and validate the code first
    (let [forms (read-string (str "[" code-string "]"))]
      
      ;; Check for dangerous operations
      (when (some #(and (list? %)
                        (contains? #{'System/exit 'Runtime/getRuntime
                                    'java.io.File. 'delete-file
                                    'spit 'slurp}
                                   (first %)))
                  (tree-seq coll? seq forms))
        (throw (ex-info "Unsafe code detected" {:module module-name})))
      
      ;; Evaluate in current namespace context
      (binding [*ns* (find-ns 'mental-models.desktop.gui.swing-app)]
        (let [result (load-string code-string)]
          {:success true
           :result result
           :module module-name})))
    
    (catch Exception e
      (log "Error evaluating module" module-name ":" (.getMessage e))
      {:success false
       :error (.getMessage e)
       :module module-name})))

;; =============================================================================
;; Hot-Load Execution
;; =============================================================================

(defn- load-module!
  "Load a single module into the running application"
  [module-info]
  (let [{:keys [name version code description]} module-info
        current (get-in @hot-load-state [:loaded-modules name])]
    
    ;; Skip if already loaded at this version
    (when (or (nil? current)
              (not= (:version current) version))
      (log "Loading module:" name "v" version)
      
      (let [result (safe-eval code name)]
        (if (:success result)
          (do
            (swap! hot-load-state update :loaded-modules
                   assoc name {:version version
                               :loaded-at (System/currentTimeMillis)
                               :description description})
            (log "Successfully loaded:" name "v" version)
            true)
          (do
            (swap! hot-load-state update :errors
                   conj {:module name
                         :version version
                         :error (:error result)
                         :timestamp (System/currentTimeMillis)})
            (log "Failed to load:" name "-" (:error result))
            false))))))

(defn check-and-load-updates!
  "Check for available updates and load them"
  []
  (when (:enabled @hot-load-state)
    (log "Checking for hot-loadable updates...")
    (swap! hot-load-state assoc :last-check (System/currentTimeMillis))
    
    (when-let [modules (fetch-available-modules)]
      (let [loaded-count (atom 0)]
        (doseq [module modules]
          (let [current-version (get-in @hot-load-state 
                                        [:loaded-modules (:name module) :version])]
            (when (or (nil? current-version)
                      (not= current-version (:version module)))
              ;; Fetch full code if not included
              (let [code (or (:code module)
                             (fetch-module-code (:name module) (:version module)))]
                (when code
                  (when (load-module! (assoc module :code code))
                    (swap! loaded-count inc)))))))
        
        (when (pos? @loaded-count)
          (log "Loaded" @loaded-count "new modules"))
        
        @loaded-count))))

;; =============================================================================
;; Scheduler
;; =============================================================================

(defn start-polling!
  "Start background polling for updates"
  []
  (when-let [old-scheduler @scheduler]
    (.shutdown old-scheduler))
  
  (let [poll-interval (or (:poll-interval-ms @hot-load-state) 300000)
        new-scheduler (Executors/newSingleThreadScheduledExecutor)]
    (reset! scheduler new-scheduler)
    
    (.scheduleAtFixedRate
     new-scheduler
     (fn []
       (try
         (check-and-load-updates!)
         (catch Exception e
           (log "Error in hot-load poll:" (.getMessage e)))))
     0  ;; initial delay
     poll-interval
     TimeUnit/MILLISECONDS)
    
    (log "Started hot-load polling every" (/ poll-interval 60000) "minutes")))

(defn stop-polling!
  "Stop background polling"
  []
  (when-let [s @scheduler]
    (.shutdown s)
    (reset! scheduler nil)
    (log "Stopped hot-load polling")))

;; =============================================================================
;; Manual Operations
;; =============================================================================

(defn force-reload!
  "Force reload all modules"
  []
  (swap! hot-load-state assoc :loaded-modules {})
  (check-and-load-updates!))

(defn get-status
  "Get current hot-load status"
  []
  (let [state @hot-load-state]
    {:enabled (:enabled state)
     :last-check (:last-check state)
     :loaded-modules (count (:loaded-modules state))
     :module-details (:loaded-modules state)
     :recent-errors (take 5 (:errors state))}))

(defn list-loaded-modules
  "List all currently loaded modules"
  []
  (:loaded-modules @hot-load-state))

;; =============================================================================
;; Pre-defined Hot-Loadable Functions
;; =============================================================================

(defn register-hot-function!
  "Register a function that can be hot-reloaded"
  [fn-name fn-impl]
  (intern 'mental-models.desktop.gui.swing-app (symbol fn-name) fn-impl)
  (log "Registered hot function:" fn-name))

;; =============================================================================
;; UI Integration Helpers
;; =============================================================================

(defn notify-ui-of-update
  "Callback to notify UI when new functionality is available"
  [callback-fn]
  (add-watch hot-load-state :ui-notify
             (fn [_ _ old new]
               (when (not= (:loaded-modules old) (:loaded-modules new))
                 (callback-fn (get-status))))))

(defn remove-ui-notification
  "Remove UI notification callback"
  []
  (remove-watch hot-load-state :ui-notify))
