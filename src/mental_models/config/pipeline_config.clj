(ns mental-models.config.pipeline-config
  "Pipeline Configuration Management
   
   Centralized configuration for the analysis pipeline:
   - Environment-based configuration
   - Runtime configuration updates
   - Configuration validation
   - Default values with overrides
   - Configuration persistence"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]))

;; =============================================================================
;; CONFIGURATION SPECS
;; =============================================================================

(s/def ::watch-dirs (s/coll-of string?))
(s/def ::concurrency (s/and int? pos?))
(s/def ::lm-studio-url string?)
(s/def ::lm-studio-model string?)
(s/def ::checkpoint-file string?)
(s/def ::enable-database boolean?)
(s/def ::enable-notifications boolean?)
(s/def ::lollapalooza-threshold (s/and number? #(<= 0 % 1)))
(s/def ::min-converging-models (s/and int? pos?))
(s/def ::batch-size (s/and int? pos?))
(s/def ::retry-max-attempts (s/and int? pos?))
(s/def ::retry-initial-delay-ms (s/and int? pos?))
(s/def ::circuit-breaker-threshold (s/and int? pos?))

(s/def ::pipeline-config
  (s/keys :opt-un [::watch-dirs
                   ::concurrency
                   ::lm-studio-url
                   ::lm-studio-model
                   ::checkpoint-file
                   ::enable-database
                   ::enable-notifications
                   ::lollapalooza-threshold
                   ::min-converging-models
                   ::batch-size
                   ::retry-max-attempts
                   ::retry-initial-delay-ms
                   ::circuit-breaker-threshold]))

;; =============================================================================
;; DEFAULT CONFIGURATION
;; =============================================================================

(def default-config
  {:watch-dirs []
   :concurrency 4
   :lm-studio-url "http://localhost:1234"
   :lm-studio-model "default"
   :checkpoint-file ".scanner-checkpoint.edn"
   :enable-database true
   :enable-notifications true
   :lollapalooza-threshold 0.7
   :min-converging-models 3
   :batch-size 100
   :retry-max-attempts 3
   :retry-initial-delay-ms 1000
   :retry-multiplier 2.0
   :retry-max-delay-ms 30000
   :circuit-breaker-threshold 5
   :circuit-breaker-recovery-threshold 3
   :circuit-breaker-timeout-ms 30000
   :websocket-enabled true
   :websocket-port 8080
   :metrics-enabled true
   :metrics-interval-ms 5000
   :log-level :info
   :file-extensions #{"txt" "md" "pdf" "docx" "html" "json" "csv" "rtf"}})

;; =============================================================================
;; CONFIGURATION STATE
;; =============================================================================

(defonce current-config (atom default-config))
(defonce config-history (atom []))

;; =============================================================================
;; ENVIRONMENT LOADING
;; =============================================================================

(defn env-or-default
  "Get environment variable or default value"
  [env-var default-val]
  (or (System/getenv env-var) default-val))

(defn load-env-config
  "Load configuration from environment variables"
  []
  {:lm-studio-url (env-or-default "LM_STUDIO_URL" (:lm-studio-url default-config))
   :lm-studio-model (env-or-default "LM_STUDIO_MODEL" (:lm-studio-model default-config))
   :concurrency (Integer/parseInt (env-or-default "PIPELINE_CONCURRENCY" (str (:concurrency default-config))))
   :enable-database (Boolean/parseBoolean (env-or-default "ENABLE_DATABASE" "true"))
   :enable-notifications (Boolean/parseBoolean (env-or-default "ENABLE_NOTIFICATIONS" "true"))
   :log-level (keyword (env-or-default "LOG_LEVEL" "info"))})

;; =============================================================================
;; FILE LOADING
;; =============================================================================

(defn load-config-file
  "Load configuration from EDN file"
  [filepath]
  (when (.exists (io/file filepath))
    (try
      (edn/read-string (slurp filepath))
      (catch Exception e
        (println "[CONFIG] Error loading config file:" (.getMessage e))
        nil))))

(defn save-config-file!
  "Save configuration to EDN file"
  [filepath config]
  (try
    (spit filepath (pr-str config))
    (println "[CONFIG] Saved to" filepath)
    true
    (catch Exception e
      (println "[CONFIG] Error saving config:" (.getMessage e))
      false)))

;; =============================================================================
;; CONFIGURATION MANAGEMENT
;; =============================================================================

(defn validate-config
  "Validate configuration against spec"
  [config]
  (if (s/valid? ::pipeline-config config)
    {:valid true :config config}
    {:valid false :errors (s/explain-str ::pipeline-config config)}))

(defn merge-configs
  "Merge multiple configurations with later ones taking precedence"
  [& configs]
  (reduce (fn [acc cfg]
            (merge acc (into {} (filter (comp some? val) cfg))))
          {}
          configs))

(defn init-config!
  "Initialize configuration from defaults, file, and environment"
  [& {:keys [config-file]}]
  (let [file-config (when config-file (load-config-file config-file))
        env-config (load-env-config)
        merged (merge-configs default-config file-config env-config)
        validation (validate-config merged)]
    (if (:valid validation)
      (do
        (reset! current-config merged)
        (swap! config-history conj {:timestamp (System/currentTimeMillis)
                                    :config merged
                                    :source :init})
        (println "[CONFIG] Initialized successfully")
        merged)
      (do
        (println "[CONFIG] Validation failed:" (:errors validation))
        (reset! current-config default-config)
        default-config))))

(defn update-config!
  "Update configuration at runtime"
  [updates]
  (let [new-config (merge @current-config updates)
        validation (validate-config new-config)]
    (if (:valid validation)
      (do
        (swap! config-history conj {:timestamp (System/currentTimeMillis)
                                    :config new-config
                                    :changes updates
                                    :source :runtime})
        (reset! current-config new-config)
        (println "[CONFIG] Updated:" (keys updates))
        new-config)
      (do
        (println "[CONFIG] Invalid update:" (:errors validation))
        @current-config))))

(defn get-config
  "Get current configuration or specific key"
  ([] @current-config)
  ([key] (get @current-config key))
  ([key default] (get @current-config key default)))

;; =============================================================================
;; CONFIGURATION PROFILES
;; =============================================================================

(def profiles
  {:development
   {:concurrency 2
    :enable-database false
    :enable-notifications false
    :log-level :debug
    :metrics-interval-ms 1000}
   
   :testing
   {:concurrency 1
    :enable-database false
    :enable-notifications false
    :log-level :debug
    :batch-size 10}
   
   :production
   {:concurrency 8
    :enable-database true
    :enable-notifications true
    :log-level :info
    :batch-size 500
    :metrics-interval-ms 10000}
   
   :high-throughput
   {:concurrency 16
    :batch-size 1000
    :retry-max-attempts 5
    :circuit-breaker-threshold 10}})

(defn apply-profile!
  "Apply a configuration profile"
  [profile-name]
  (if-let [profile (get profiles profile-name)]
    (do
      (update-config! profile)
      (println "[CONFIG] Applied profile:" profile-name))
    (println "[CONFIG] Unknown profile:" profile-name)))

;; =============================================================================
;; CONFIGURATION QUERIES
;; =============================================================================

(defn get-config-history
  "Get configuration change history"
  [& {:keys [limit] :or {limit 10}}]
  (take-last limit @config-history))

(defn diff-configs
  "Show differences between two configurations"
  [config1 config2]
  (let [all-keys (set (concat (keys config1) (keys config2)))]
    (into {}
          (filter (fn [[k _]]
                    (not= (get config1 k) (get config2 k)))
                  (map (fn [k]
                         [k {:old (get config1 k)
                             :new (get config2 k)}])
                       all-keys)))))

(defn export-config
  "Export current configuration as EDN string"
  []
  (pr-str @current-config))

(defn import-config!
  "Import configuration from EDN string"
  [edn-str]
  (try
    (let [config (edn/read-string edn-str)]
      (update-config! config))
    (catch Exception e
      (println "[CONFIG] Import error:" (.getMessage e))
      nil)))
