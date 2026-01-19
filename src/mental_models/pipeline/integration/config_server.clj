(ns mental-models.pipeline.integration.config-server
  "Centralized configuration server for distributed mental model analysis.
   
   Features:
   - Centralized configuration management
   - Environment-specific configs
   - Dynamic configuration updates
   - Configuration versioning
   - Secret management
   - Configuration validation
   - Change notifications
   - Configuration inheritance"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:configs {}          ;; config-key -> config-value
         :environments {}     ;; env-id -> environment-config
         :versions {}         ;; config-key -> [versions]
         :secrets {}          ;; secret-key -> encrypted-value
         :schemas {}          ;; config-key -> validation-schema
         :watchers {}         ;; watcher-id -> watcher-config
         :current-env :development
         :initialized? false}))

;; ============================================================================
;; Environment Management
;; ============================================================================

(defn register-environment!
  "Register an environment."
  [env-id config]
  (let [environment {:id env-id
                     :name (get config :name (name env-id))
                     :description (get config :description "")
                     :parent (get config :parent nil)
                     :overrides (get config :overrides {})
                     :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:environments env-id] environment)
    (logging/log :info "Registered environment" {:env-id env-id})
    env-id))

(defn get-environment
  "Get an environment."
  [env-id]
  (get-in @state [:environments env-id]))

(defn list-environments
  "List all environments."
  []
  (mapv (fn [[id e]]
          {:id id
           :name (:name e)
           :parent (:parent e)})
        (:environments @state)))

(defn set-current-environment!
  "Set the current environment."
  [env-id]
  (swap! state assoc :current-env env-id)
  (logging/log :info "Set current environment" {:env-id env-id})
  (events/emit! :environment-changed {:env-id env-id}))

(defn get-current-environment
  "Get the current environment."
  []
  (:current-env @state))

;; ============================================================================
;; Configuration Management
;; ============================================================================

(defn- resolve-config-value
  "Resolve a configuration value with environment inheritance."
  [key env-id]
  (let [env (get-environment env-id)
        ;; Check environment overrides first
        override (get-in env [:overrides key])
        ;; Then check parent environment
        parent-value (when (:parent env)
                       (resolve-config-value key (:parent env)))
        ;; Finally check base config
        base-value (get-in @state [:configs key :value])]
    (or override parent-value base-value)))

(defn set-config!
  "Set a configuration value."
  [key value & {:keys [description schema env-id]}]
  (let [current-version (get-in @state [:configs key :version] 0)
        new-version (inc current-version)
        config {:key key
                :value value
                :description (or description "")
                :version new-version
                :updated-at (System/currentTimeMillis)
                :updated-by "system"}]
    ;; Validate if schema exists
    (when-let [schema-fn (get-in @state [:schemas key])]
      (when-not (schema-fn value)
        (throw (ex-info "Configuration validation failed" {:key key :value value}))))
    
    ;; Store config
    (if env-id
      ;; Environment-specific override
      (swap! state assoc-in [:environments env-id :overrides key] value)
      ;; Base config
      (swap! state assoc-in [:configs key] config))
    
    ;; Store version history
    (swap! state update-in [:versions key]
           (fn [versions]
             (conj (or versions [])
                   {:version new-version
                    :value value
                    :timestamp (System/currentTimeMillis)})))
    
    (logging/log :info "Set configuration" {:key key :version new-version})
    (events/emit! :config-changed {:key key :value value :version new-version})
    (metrics/increment :config-updates {})
    
    ;; Notify watchers
    (notify-watchers! key value)
    
    config))

(defn get-config
  "Get a configuration value."
  [key & {:keys [env-id default]}]
  (let [env (or env-id (get-current-environment))
        value (resolve-config-value key env)]
    (or value default)))

(defn get-config-with-metadata
  "Get a configuration value with metadata."
  [key]
  (get-in @state [:configs key]))

(defn list-configs
  "List all configurations."
  []
  (mapv (fn [[k v]]
          {:key k
           :value (:value v)
           :version (:version v)
           :updated-at (:updated-at v)})
        (:configs @state)))

(defn delete-config!
  "Delete a configuration."
  [key]
  (swap! state update :configs dissoc key)
  (logging/log :info "Deleted configuration" {:key key})
  (events/emit! :config-deleted {:key key}))

;; ============================================================================
;; Configuration Versioning
;; ============================================================================

(defn get-config-history
  "Get version history for a configuration."
  [key & {:keys [limit]}]
  (let [versions (get-in @state [:versions key] [])
        sorted (sort-by :version > versions)]
    (if limit
      (take limit sorted)
      sorted)))

(defn rollback-config!
  "Rollback a configuration to a previous version."
  [key version]
  (let [history (get-config-history key)
        target (first (filter #(= (:version %) version) history))]
    (when target
      (set-config! key (:value target))
      (logging/log :info "Rolled back configuration" {:key key :to-version version}))))

;; ============================================================================
;; Secret Management
;; ============================================================================

(defn- simple-encrypt
  "Simple encryption (placeholder - use proper encryption in production)."
  [value]
  (let [encoded (java.util.Base64/getEncoder)]
    (.encodeToString encoded (.getBytes (str value)))))

(defn- simple-decrypt
  "Simple decryption (placeholder - use proper decryption in production)."
  [encrypted]
  (let [decoded (java.util.Base64/getDecoder)]
    (String. (.decode decoded encrypted))))

(defn set-secret!
  "Set a secret value (encrypted)."
  [key value & {:keys [description]}]
  (let [encrypted (simple-encrypt value)
        secret {:key key
                :value encrypted
                :description (or description "")
                :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:secrets key] secret)
    (logging/log :info "Set secret" {:key key})
    (audit/log! {:action :secret-updated :key key})
    key))

(defn get-secret
  "Get a secret value (decrypted)."
  [key]
  (when-let [secret (get-in @state [:secrets key])]
    (simple-decrypt (:value secret))))

(defn list-secrets
  "List all secrets (keys only, not values)."
  []
  (mapv (fn [[k v]]
          {:key k
           :description (:description v)
           :updated-at (:updated-at v)})
        (:secrets @state)))

(defn delete-secret!
  "Delete a secret."
  [key]
  (swap! state update :secrets dissoc key)
  (logging/log :info "Deleted secret" {:key key})
  (audit/log! {:action :secret-deleted :key key}))

;; ============================================================================
;; Configuration Validation
;; ============================================================================

(defn register-schema!
  "Register a validation schema for a configuration key."
  [key schema-fn]
  (swap! state assoc-in [:schemas key] schema-fn)
  (logging/log :info "Registered schema" {:key key}))

(defn validate-config
  "Validate a configuration value against its schema."
  [key value]
  (if-let [schema-fn (get-in @state [:schemas key])]
    {:valid? (schema-fn value)
     :key key
     :value value}
    {:valid? true
     :key key
     :value value
     :note "No schema registered"}))

(defn validate-all-configs
  "Validate all configurations against their schemas."
  []
  (let [configs (:configs @state)
        results (for [[k v] configs]
                  (validate-config k (:value v)))]
    {:total (count results)
     :valid (count (filter :valid? results))
     :invalid (filterv #(not (:valid? %)) results)}))

;; ============================================================================
;; Configuration Watchers
;; ============================================================================

(defn register-watcher!
  "Register a configuration change watcher."
  [watcher-id config]
  (let [watcher {:id watcher-id
                 :keys (get config :keys [])
                 :callback (get config :callback (fn [_ _] nil))
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:watchers watcher-id] watcher)
    (logging/log :info "Registered config watcher" {:watcher-id watcher-id})
    watcher-id))

(defn unregister-watcher!
  "Unregister a configuration change watcher."
  [watcher-id]
  (swap! state update :watchers dissoc watcher-id))

(defn- notify-watchers!
  "Notify watchers of a configuration change."
  [key value]
  (doseq [[_ watcher] (:watchers @state)]
    (when (or (empty? (:keys watcher))
              (contains? (set (:keys watcher)) key))
      (try
        ((:callback watcher) key value)
        (catch Exception e
          (logging/log :error "Watcher callback failed" {:watcher-id (:id watcher) :error (.getMessage e)}))))))

;; ============================================================================
;; Configuration Import/Export
;; ============================================================================

(defn export-configs
  "Export all configurations."
  [& {:keys [format include-secrets?] :or {format :edn include-secrets? false}}]
  (let [data {:configs (into {} (map (fn [[k v]] [k (:value v)]) (:configs @state)))
              :environments (:environments @state)
              :secrets (when include-secrets?
                         (into {} (map (fn [[k v]] [k (get-secret k)]) (:secrets @state))))
              :exported-at (System/currentTimeMillis)}]
    (case format
      :edn (pr-str data)
      :json data
      data)))

(defn import-configs!
  "Import configurations."
  [data & {:keys [merge?] :or {merge? true}}]
  (let [parsed (if (string? data) (edn/read-string data) data)]
    ;; Import configs
    (doseq [[k v] (:configs parsed)]
      (set-config! k v))
    ;; Import environments
    (doseq [[id env] (:environments parsed)]
      (register-environment! id env))
    ;; Import secrets if present
    (when (:secrets parsed)
      (doseq [[k v] (:secrets parsed)]
        (set-secret! k v)))
    (logging/log :info "Imported configurations" {:configs (count (:configs parsed))})))

;; ============================================================================
;; Configuration Profiles
;; ============================================================================

(defn create-profile!
  "Create a configuration profile (snapshot)."
  [profile-id & {:keys [description]}]
  (let [profile {:id profile-id
                 :configs (into {} (map (fn [[k v]] [k (:value v)]) (:configs @state)))
                 :description (or description "")
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:profiles profile-id] profile)
    (logging/log :info "Created config profile" {:profile-id profile-id})
    profile-id))

(defn apply-profile!
  "Apply a configuration profile."
  [profile-id]
  (when-let [profile (get-in @state [:profiles profile-id])]
    (doseq [[k v] (:configs profile)]
      (set-config! k v))
    (logging/log :info "Applied config profile" {:profile-id profile-id})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-server-stats
  "Get configuration server statistics."
  []
  {:total-configs (count (:configs @state))
   :total-environments (count (:environments @state))
   :total-secrets (count (:secrets @state))
   :total-schemas (count (:schemas @state))
   :total-watchers (count (:watchers @state))
   :current-environment (get-current-environment)
   :config-versions (reduce + (map count (vals (:versions @state))))})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-config-server!
  "Initialize the configuration server."
  []
  (when-not (:initialized? @state)
    ;; Register default environments
    (register-environment! :development
                           {:name "Development"
                            :description "Local development environment"})
    
    (register-environment! :staging
                           {:name "Staging"
                            :description "Pre-production testing"
                            :parent :development})
    
    (register-environment! :production
                           {:name "Production"
                            :description "Production environment"
                            :parent :staging})
    
    ;; Set default configs
    (set-config! :lm-studio-url "http://localhost:1234"
                 :description "LM Studio API URL")
    
    (set-config! :analysis-batch-size 10
                 :description "Batch size for document analysis")
    
    (set-config! :cache-ttl-ms 3600000
                 :description "Default cache TTL in milliseconds")
    
    (set-config! :max-concurrent-analyses 5
                 :description "Maximum concurrent analysis jobs")
    
    ;; Register validation schemas
    (register-schema! :analysis-batch-size
                      (fn [v] (and (integer? v) (pos? v) (<= v 100))))
    
    (register-schema! :cache-ttl-ms
                      (fn [v] (and (integer? v) (pos? v))))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Configuration server initialized")
    (events/emit! :config-server-initialized {})
    true))
