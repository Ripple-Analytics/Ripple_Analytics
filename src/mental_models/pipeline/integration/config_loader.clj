(ns mental-models.pipeline.integration.config-loader
  "Configuration Loader Module
   
   Centralized configuration management:
   - Load from multiple sources (EDN, JSON, env vars)
   - Configuration validation
   - Hot reload support
   - Environment-specific configs
   - Secret management"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; CONFIGURATION STATE
;; =============================================================================

(defonce config-state (atom {:config {}
                             :sources []
                             :last-loaded nil
                             :watchers {}}))

;; =============================================================================
;; DEFAULT CONFIGURATION
;; =============================================================================

(def default-config
  {:app {:name "Mental Models System"
         :version "1.0.0"
         :environment :development}
   :lm-studio {:url "http://localhost:1234"
               :timeout-ms 30000
               :max-retries 3
               :retry-delay-ms 1000}
   :database {:host "localhost"
              :port 5432
              :name "mental_models"
              :pool-size 10}
   :websocket {:port 8765
               :heartbeat-interval-ms 30000}
   :file-watcher {:poll-interval-ms 1000
                  :batch-size 10
                  :supported-extensions #{".txt" ".md" ".json" ".csv" ".pdf" ".docx" ".html"}}
   :notifications {:slack {:enabled false}
                   :email {:enabled false}
                   :desktop {:enabled true}}
   :search {:max-results 100
            :min-term-length 3}
   :alerts {:escalation-interval-ms 60000
            :max-active-alerts 1000}
   :logging {:level :info
             :format :json}})

;; =============================================================================
;; CONFIGURATION LOADING
;; =============================================================================

(defn load-edn-file
  "Load configuration from an EDN file."
  [path]
  (try
    (when (.exists (io/file path))
      (log/debug "Loading EDN config" {:path path})
      (edn/read-string (slurp path)))
    (catch Exception e
      (log/error "Failed to load EDN config" {:path path :error (.getMessage e)})
      nil)))

(defn load-json-file
  "Load configuration from a JSON file."
  [path]
  (try
    (when (.exists (io/file path))
      (log/debug "Loading JSON config" {:path path})
      ;; Simple JSON parsing without external deps
      (let [content (slurp path)]
        (edn/read-string (str/replace content #":" ""))))
    (catch Exception e
      (log/error "Failed to load JSON config" {:path path :error (.getMessage e)})
      nil)))

(defn env-var-to-key
  "Convert environment variable name to config key path."
  [env-name]
  (let [parts (-> env-name
                  str/lower-case
                  (str/replace #"^mm_" "")
                  (str/split #"_"))]
    (mapv keyword parts)))

(defn load-env-vars
  "Load configuration from environment variables prefixed with MM_."
  []
  (log/debug "Loading environment variables")
  (reduce (fn [config [k v]]
            (when (str/starts-with? k "MM_")
              (let [path (env-var-to-key k)]
                (assoc-in config path v))))
          {}
          (System/getenv)))

;; =============================================================================
;; CONFIGURATION MERGING
;; =============================================================================

(defn deep-merge
  "Deep merge two maps."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn merge-configs
  "Merge multiple configuration sources."
  [& configs]
  (reduce deep-merge {} (filter some? configs)))

;; =============================================================================
;; CONFIGURATION VALIDATION
;; =============================================================================

(def config-schema
  {:app {:name string?
         :version string?
         :environment keyword?}
   :lm-studio {:url string?
               :timeout-ms pos-int?
               :max-retries pos-int?}
   :database {:host string?
              :port pos-int?
              :name string?}
   :websocket {:port pos-int?}})

(defn validate-config
  "Validate configuration against schema."
  [config]
  (let [errors (atom [])]
    (doseq [[section schema] config-schema]
      (doseq [[key pred] schema]
        (let [value (get-in config [section key])]
          (when (and value (not (pred value)))
            (swap! errors conj {:section section
                                :key key
                                :value value
                                :expected (str pred)})))))
    (if (empty? @errors)
      {:valid true :config config}
      {:valid false :errors @errors})))

;; =============================================================================
;; CONFIGURATION ACCESS
;; =============================================================================

(defn get-config
  "Get configuration value at path."
  [& path]
  (get-in (:config @config-state) path))

(defn get-config-or
  "Get configuration value at path with default."
  [default & path]
  (or (apply get-config path) default))

(defn set-config!
  "Set configuration value at path."
  [path value]
  (log/debug "Setting config" {:path path :value value})
  (swap! config-state update :config assoc-in path value)
  (events/publish! :config/changed {:path path :value value}))

;; =============================================================================
;; HOT RELOAD
;; =============================================================================

(defn watch-config-file!
  "Watch a configuration file for changes."
  [path]
  (log/info "Watching config file" {:path path})
  (let [file (io/file path)
        last-modified (atom (.lastModified file))
        watcher-fn (fn []
                     (let [current-modified (.lastModified file)]
                       (when (> current-modified @last-modified)
                         (log/info "Config file changed, reloading" {:path path})
                         (reset! last-modified current-modified)
                         (when-let [new-config (load-edn-file path)]
                           (swap! config-state update :config deep-merge new-config)
                           (events/publish! :config/reloaded {:path path})))))]
    (swap! config-state assoc-in [:watchers path] watcher-fn)
    watcher-fn))

(defn unwatch-config-file!
  "Stop watching a configuration file."
  [path]
  (log/info "Unwatching config file" {:path path})
  (swap! config-state update :watchers dissoc path))

;; =============================================================================
;; ENVIRONMENT PROFILES
;; =============================================================================

(def environment-overrides
  {:development {:logging {:level :debug}
                 :lm-studio {:timeout-ms 60000}}
   :staging {:logging {:level :info}
             :database {:pool-size 5}}
   :production {:logging {:level :warn}
                :database {:pool-size 20}
                :notifications {:slack {:enabled true}
                                :email {:enabled true}}}})

(defn apply-environment-profile
  "Apply environment-specific configuration overrides."
  [config]
  (let [env (get-in config [:app :environment] :development)
        overrides (get environment-overrides env {})]
    (deep-merge config overrides)))

;; =============================================================================
;; SECRET MANAGEMENT
;; =============================================================================

(defonce secrets (atom {}))

(defn load-secret
  "Load a secret from environment variable."
  [key env-var]
  (when-let [value (System/getenv env-var)]
    (swap! secrets assoc key value)
    (log/debug "Loaded secret" {:key key})
    true))

(defn get-secret
  "Get a secret value."
  [key]
  (get @secrets key))

(defn clear-secrets!
  "Clear all secrets from memory."
  []
  (reset! secrets {})
  (log/info "Secrets cleared"))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn load-config!
  "Load configuration from all sources."
  [& {:keys [config-paths env-prefix]
      :or {config-paths ["config.edn" "config.local.edn"]
           env-prefix "MM_"}}]
  (log/info "Loading configuration" {:paths config-paths})
  (let [file-configs (map load-edn-file config-paths)
        env-config (load-env-vars)
        merged (apply merge-configs default-config (concat file-configs [env-config]))
        with-env (apply-environment-profile merged)
        validation (validate-config with-env)]
    (if (:valid validation)
      (do
        (swap! config-state assoc
               :config with-env
               :sources (concat config-paths [:env-vars])
               :last-loaded (System/currentTimeMillis))
        (events/publish! :config/loaded {:sources (:sources @config-state)})
        (log/info "Configuration loaded successfully")
        with-env)
      (do
        (log/error "Configuration validation failed" {:errors (:errors validation)})
        (throw (ex-info "Invalid configuration" validation))))))

(defn init-config-loader!
  "Initialize configuration loader."
  []
  (log/info "Initializing configuration loader")
  ;; Register feature flag
  (flags/register-flag! "config-hot-reload" "Enable configuration hot reload" true)
  ;; Load default secrets
  (load-secret :database-password "MM_DATABASE_PASSWORD")
  (load-secret :slack-token "MM_SLACK_TOKEN")
  (load-secret :email-api-key "MM_EMAIL_API_KEY")
  (log/info "Configuration loader initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-loader-status []
  {:last-loaded (:last-loaded @config-state)
   :sources (:sources @config-state)
   :watched-files (keys (:watchers @config-state))
   :environment (get-config :app :environment)
   :secrets-loaded (count @secrets)})
