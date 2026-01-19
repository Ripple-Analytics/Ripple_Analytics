(ns mental-models.pipeline.integration.config-watcher
  "Configuration Watcher Module
   
   Dynamic configuration updates:
   - File watching
   - Environment variable monitoring
   - Configuration change detection
   - Hot reload support
   - Change notifications"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.nio.file FileSystems Paths StandardWatchEventKinds WatchService WatchKey]
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; CONFIG WATCHER STATE
;; =============================================================================

(defonce watcher-state (atom {:configs {}
                              :watchers {}
                              :listeners {}
                              :env-cache {}
                              :scheduler nil
                              :config {:poll-interval-ms 5000
                                       :debounce-ms 500}}))

;; =============================================================================
;; CONFIGURATION SOURCES
;; =============================================================================

(defn register-config!
  "Register a configuration source."
  [config-id {:keys [type path env-vars default-value parser]}]
  (log/info "Registering config" {:id config-id :type type})
  (swap! watcher-state assoc-in [:configs config-id]
         {:id config-id
          :type (or type :file)
          :path path
          :env-vars env-vars
          :default-value default-value
          :parser (or parser identity)
          :value nil
          :last-modified nil
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :configwatcher/configs-registered)
  config-id)

(defn unregister-config!
  "Unregister a configuration source."
  [config-id]
  (log/info "Unregistering config" {:id config-id})
  (swap! watcher-state update :configs dissoc config-id))

(defn get-config
  "Get a configuration source."
  [config-id]
  (get-in @watcher-state [:configs config-id]))

(defn list-configs
  "List all configuration sources."
  []
  (keys (:configs @watcher-state)))

;; =============================================================================
;; FILE CONFIGURATION
;; =============================================================================

(defn read-file-config
  "Read configuration from a file."
  [path parser]
  (try
    (when (.exists (io/file path))
      (let [content (slurp path)]
        (parser content)))
    (catch Exception e
      (log/error "Failed to read config file" {:path path :error (.getMessage e)})
      nil)))

(defn get-file-modified-time
  "Get the last modified time of a file."
  [path]
  (try
    (when (.exists (io/file path))
      (.lastModified (io/file path)))
    (catch Exception _
      nil)))

(defn file-changed?
  "Check if a file has changed."
  [config]
  (when (= (:type config) :file)
    (let [current-modified (get-file-modified-time (:path config))
          last-modified (:last-modified config)]
      (and current-modified
           (or (nil? last-modified)
               (> current-modified last-modified))))))

;; =============================================================================
;; ENVIRONMENT CONFIGURATION
;; =============================================================================

(defn read-env-config
  "Read configuration from environment variables."
  [env-vars]
  (into {} (for [var-name env-vars
                 :let [value (System/getenv var-name)]
                 :when value]
             [var-name value])))

(defn env-changed?
  "Check if environment variables have changed."
  [config]
  (when (= (:type config) :env)
    (let [current-env (read-env-config (:env-vars config))
          cached-env (get-in @watcher-state [:env-cache (:id config)])]
      (not= current-env cached-env))))

;; =============================================================================
;; CONFIGURATION LOADING
;; =============================================================================

(defn load-config!
  "Load configuration from a source."
  [config-id]
  (when-let [config (get-config config-id)]
    (let [value (case (:type config)
                  :file (read-file-config (:path config) (:parser config))
                  :env (read-env-config (:env-vars config))
                  :edn (when-let [path (:path config)]
                         (read-file-config path edn/read-string))
                  :json (when-let [path (:path config)]
                          (read-file-config path #(clojure.data.json/read-str % :key-fn keyword)))
                  (:default-value config))
          prev-value (:value config)]
      ;; Update config value
      (swap! watcher-state update-in [:configs config-id]
             merge {:value (or value (:default-value config))
                    :last-modified (when (= (:type config) :file)
                                     (get-file-modified-time (:path config)))
                    :last-loaded (System/currentTimeMillis)})
      ;; Update env cache
      (when (= (:type config) :env)
        (swap! watcher-state assoc-in [:env-cache config-id] value))
      ;; Notify if changed
      (when (and prev-value (not= prev-value value))
        (log/info "Config changed" {:id config-id})
        (metrics/inc-counter! :configwatcher/config-changes)
        (events/publish! :configwatcher/changed {:config-id config-id
                                                  :old-value prev-value
                                                  :new-value value})
        (notify-listeners! config-id prev-value value))
      value)))

(defn get-config-value
  "Get the current value of a configuration."
  [config-id]
  (or (get-in @watcher-state [:configs config-id :value])
      (load-config! config-id)))

(defn reload-config!
  "Force reload a configuration."
  [config-id]
  (log/info "Reloading config" {:id config-id})
  (load-config! config-id))

(defn reload-all-configs!
  "Reload all configurations."
  []
  (doseq [config-id (list-configs)]
    (reload-config! config-id)))

;; =============================================================================
;; CHANGE LISTENERS
;; =============================================================================

(defn register-listener!
  "Register a configuration change listener."
  [listener-id config-id callback]
  (log/info "Registering config listener" {:listener listener-id :config config-id})
  (swap! watcher-state update-in [:listeners config-id] (fnil conj [])
         {:id listener-id :callback callback}))

(defn unregister-listener!
  "Unregister a configuration change listener."
  [listener-id config-id]
  (swap! watcher-state update-in [:listeners config-id]
         (fn [listeners]
           (vec (remove #(= (:id %) listener-id) listeners)))))

(defn notify-listeners!
  "Notify listeners of a configuration change."
  [config-id old-value new-value]
  (doseq [{:keys [callback]} (get-in @watcher-state [:listeners config-id])]
    (try
      (callback {:config-id config-id
                 :old-value old-value
                 :new-value new-value})
      (catch Exception e
        (log/error "Listener callback failed" {:config config-id :error (.getMessage e)})))))

;; =============================================================================
;; FILE WATCHER
;; =============================================================================

(defn start-file-watcher!
  "Start watching a file for changes."
  [config-id]
  (when-let [config (get-config config-id)]
    (when (and (= (:type config) :file)
               (:path config)
               (not (get-in @watcher-state [:watchers config-id])))
      (log/info "Starting file watcher" {:config config-id :path (:path config)})
      (let [path (Paths/get (:path config) (into-array String []))
            dir (.getParent path)
            ^WatchService watch-service (.newWatchService (FileSystems/getDefault))]
        (.register dir watch-service
                   (into-array [StandardWatchEventKinds/ENTRY_MODIFY]))
        (swap! watcher-state assoc-in [:watchers config-id]
               {:watch-service watch-service
                :running (atom true)})
        ;; Start watcher thread
        (future
          (while @(get-in @watcher-state [:watchers config-id :running])
            (try
              (when-let [^WatchKey key (.poll watch-service 1 TimeUnit/SECONDS)]
                (doseq [event (.pollEvents key)]
                  (let [changed-path (.context event)]
                    (when (= (str changed-path) (.getFileName path))
                      (Thread/sleep (get-in @watcher-state [:config :debounce-ms]))
                      (load-config! config-id))))
                (.reset key))
              (catch Exception e
                (log/error "File watcher error" {:config config-id :error (.getMessage e)})))))))))

(defn stop-file-watcher!
  "Stop watching a file."
  [config-id]
  (when-let [watcher (get-in @watcher-state [:watchers config-id])]
    (log/info "Stopping file watcher" {:config config-id})
    (reset! (:running watcher) false)
    (.close ^WatchService (:watch-service watcher))
    (swap! watcher-state update :watchers dissoc config-id)))

;; =============================================================================
;; POLLING WATCHER
;; =============================================================================

(defn check-for-changes!
  "Check all configurations for changes."
  []
  (doseq [config-id (list-configs)]
    (let [config (get-config config-id)]
      (when (or (file-changed? config)
                (env-changed? config))
        (load-config! config-id)))))

(defn start-polling-watcher!
  "Start polling for configuration changes."
  []
  (when (and (flags/is-enabled? "config-watcher")
             (nil? (:scheduler @watcher-state)))
    (log/info "Starting config polling watcher")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @watcher-state [:config :poll-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try (check-for-changes!)
                                  (catch Exception e
                                    (log/error "Config poll error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! watcher-state assoc :scheduler executor))))

(defn stop-polling-watcher!
  "Stop polling for configuration changes."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @watcher-state)]
    (log/info "Stopping config polling watcher")
    (.shutdown executor)
    (swap! watcher-state assoc :scheduler nil)))

;; =============================================================================
;; CONVENIENCE FUNCTIONS
;; =============================================================================

(defn watch-file!
  "Watch a file for configuration changes."
  [config-id path & {:keys [parser on-change]}]
  (register-config! config-id {:type :file :path path :parser (or parser identity)})
  (when on-change
    (register-listener! (keyword (str config-id "-listener")) config-id on-change))
  (load-config! config-id)
  (start-file-watcher! config-id))

(defn watch-env!
  "Watch environment variables for changes."
  [config-id env-vars & {:keys [on-change]}]
  (register-config! config-id {:type :env :env-vars env-vars})
  (when on-change
    (register-listener! (keyword (str config-id "-listener")) config-id on-change))
  (load-config! config-id))

(defn watch-edn!
  "Watch an EDN file for configuration changes."
  [config-id path & {:keys [on-change]}]
  (watch-file! config-id path :parser edn/read-string :on-change on-change))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-config-watcher!
  "Initialize configuration watcher."
  []
  (log/info "Initializing config watcher")
  ;; Register feature flag
  (flags/register-flag! "config-watcher" "Enable config watcher" true)
  ;; Create metrics
  (metrics/create-counter! :configwatcher/configs-registered "Configs registered")
  (metrics/create-counter! :configwatcher/config-changes "Config changes")
  (metrics/create-gauge! :configwatcher/watched-configs "Watched configs"
                         #(count (:configs @watcher-state)))
  (metrics/create-gauge! :configwatcher/active-watchers "Active watchers"
                         #(count (:watchers @watcher-state)))
  (log/info "Config watcher initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-watcher-status []
  {:enabled (flags/is-enabled? "config-watcher")
   :configs (count (:configs @watcher-state))
   :watchers (count (:watchers @watcher-state))
   :listeners (reduce + (map count (vals (:listeners @watcher-state))))
   :polling-active (some? (:scheduler @watcher-state))
   :config (:config @watcher-state)})
