(ns mental-models.desktop.settings.sync
  "Settings synchronization between desktop app and web app.
   Handles syncing user preferences, custom model definitions,
   alert rules, watched folders, and LM Studio settings."
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [mental-models.desktop.api.web-client :as api])
  (:import [java.io File]
           [java.time Instant]))

;; =============================================================================
;; Settings Storage Paths
;; =============================================================================

(def settings-dir (str (System/getProperty "user.home") "/.mental-models"))
(def settings-file (str settings-dir "/settings.json"))
(def custom-models-file (str settings-dir "/custom-models.json"))
(def alert-rules-file (str settings-dir "/alert-rules.json"))
(def settings-backup-dir (str settings-dir "/backups"))

(defn ensure-settings-dir! []
  (let [dir (io/file settings-dir)
        backup-dir (io/file settings-backup-dir)]
    (when-not (.exists dir)
      (.mkdirs dir))
    (when-not (.exists backup-dir)
      (.mkdirs backup-dir))))

;; =============================================================================
;; Local Settings Management
;; =============================================================================

(def default-settings
  {:lm-studio-url "http://localhost:1234"
   :web-app-url "https://mental-models.manus.space"
   :theme :dark
   :auto-sync true
   :sync-interval-minutes 5
   :notifications-enabled true
   :notification-sound true
   :notification-types #{:high-risk :lollapalooza :sync-complete}
   :auto-update true
   :watched-folders []
   :scan-on-startup false
   :max-file-size-mb 50
   :supported-extensions #{".txt" ".md" ".pdf" ".docx" ".doc" ".rtf"}
   :ai-temperature 0.3
   :ai-max-tokens 2000
   :offline-mode-enabled true
   :device-name (or (System/getenv "COMPUTERNAME") 
                    (System/getenv "HOSTNAME")
                    "Desktop")
   :device-id nil  ;; Generated on first run
   :last-sync nil
   :created-at nil
   :updated-at nil})

(defn generate-device-id []
  (str (java.util.UUID/randomUUID)))

(defn load-settings
  "Load settings from local storage"
  []
  (ensure-settings-dir!)
  (try
    (if (.exists (io/file settings-file))
      (let [saved (json/read-str (slurp settings-file) :key-fn keyword)]
        (merge default-settings saved))
      (let [new-settings (assoc default-settings
                                :device-id (generate-device-id)
                                :created-at (str (Instant/now)))]
        (save-settings! new-settings)
        new-settings))
    (catch Exception e
      (println "Error loading settings:" (.getMessage e))
      default-settings)))

(defn save-settings!
  "Save settings to local storage"
  [settings]
  (ensure-settings-dir!)
  (try
    (let [updated-settings (assoc settings :updated-at (str (Instant/now)))]
      (spit settings-file (json/write-str updated-settings))
      updated-settings)
    (catch Exception e
      (println "Error saving settings:" (.getMessage e))
      settings)))

(defn update-setting!
  "Update a single setting"
  [key value]
  (let [current (load-settings)
        updated (assoc current key value)]
    (save-settings! updated)))

(defn reset-to-defaults!
  "Reset settings to defaults (preserves device-id)"
  []
  (let [current (load-settings)
        device-id (:device-id current)
        reset-settings (assoc default-settings
                              :device-id device-id
                              :created-at (:created-at current)
                              :updated-at (str (Instant/now)))]
    (save-settings! reset-settings)))

;; =============================================================================
;; Custom Models Management
;; =============================================================================

(defn load-custom-models
  "Load custom model definitions"
  []
  (ensure-settings-dir!)
  (try
    (if (.exists (io/file custom-models-file))
      (json/read-str (slurp custom-models-file) :key-fn keyword)
      [])
    (catch Exception e
      (println "Error loading custom models:" (.getMessage e))
      [])))

(defn save-custom-models!
  "Save custom model definitions"
  [models]
  (ensure-settings-dir!)
  (try
    (spit custom-models-file (json/write-str models))
    models
    (catch Exception e
      (println "Error saving custom models:" (.getMessage e))
      models)))

(defn add-custom-model!
  "Add a new custom model"
  [model]
  (let [models (load-custom-models)
        new-model (assoc model
                         :id (str (java.util.UUID/randomUUID))
                         :created-at (str (Instant/now))
                         :is-custom true)]
    (save-custom-models! (conj models new-model))))

(defn update-custom-model!
  "Update an existing custom model"
  [id updates]
  (let [models (load-custom-models)
        updated (mapv #(if (= (:id %) id)
                         (merge % updates {:updated-at (str (Instant/now))})
                         %)
                      models)]
    (save-custom-models! updated)))

(defn delete-custom-model!
  "Delete a custom model"
  [id]
  (let [models (load-custom-models)
        filtered (vec (remove #(= (:id %) id) models))]
    (save-custom-models! filtered)))

;; =============================================================================
;; Alert Rules Management
;; =============================================================================

(defn load-alert-rules
  "Load alert rule configurations"
  []
  (ensure-settings-dir!)
  (try
    (if (.exists (io/file alert-rules-file))
      (json/read-str (slurp alert-rules-file) :key-fn keyword)
      [])
    (catch Exception e
      (println "Error loading alert rules:" (.getMessage e))
      [])))

(defn save-alert-rules!
  "Save alert rule configurations"
  [rules]
  (ensure-settings-dir!)
  (try
    (spit alert-rules-file (json/write-str rules))
    rules
    (catch Exception e
      (println "Error saving alert rules:" (.getMessage e))
      rules)))

(defn add-alert-rule!
  "Add a new alert rule"
  [rule]
  (let [rules (load-alert-rules)
        new-rule (assoc rule
                        :id (str (java.util.UUID/randomUUID))
                        :created-at (str (Instant/now))
                        :enabled true)]
    (save-alert-rules! (conj rules new-rule))))

(defn update-alert-rule!
  "Update an existing alert rule"
  [id updates]
  (let [rules (load-alert-rules)
        updated (mapv #(if (= (:id %) id)
                         (merge % updates {:updated-at (str (Instant/now))})
                         %)
                      rules)]
    (save-alert-rules! updated)))

(defn delete-alert-rule!
  "Delete an alert rule"
  [id]
  (let [rules (load-alert-rules)
        filtered (vec (remove #(= (:id %) id) rules))]
    (save-alert-rules! filtered)))

(defn toggle-alert-rule!
  "Enable/disable an alert rule"
  [id]
  (let [rules (load-alert-rules)
        updated (mapv #(if (= (:id %) id)
                         (update % :enabled not)
                         %)
                      rules)]
    (save-alert-rules! updated)))

;; =============================================================================
;; Settings Backup/Restore
;; =============================================================================

(defn create-backup!
  "Create a backup of all settings"
  []
  (ensure-settings-dir!)
  (let [timestamp (str (Instant/now))
        backup-name (str "backup-" (str/replace timestamp #"[:.]" "-") ".json")
        backup-path (str settings-backup-dir "/" backup-name)
        backup-data {:settings (load-settings)
                     :custom-models (load-custom-models)
                     :alert-rules (load-alert-rules)
                     :backup-at timestamp}]
    (try
      (spit backup-path (json/write-str backup-data))
      {:success true :path backup-path :timestamp timestamp}
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn list-backups
  "List all available backups"
  []
  (ensure-settings-dir!)
  (let [backup-dir (io/file settings-backup-dir)]
    (->> (.listFiles backup-dir)
         (filter #(.endsWith (.getName %) ".json"))
         (map (fn [f]
                {:name (.getName f)
                 :path (.getAbsolutePath f)
                 :size (.length f)
                 :modified (.lastModified f)}))
         (sort-by :modified >)
         vec)))

(defn restore-backup!
  "Restore settings from a backup file"
  [backup-path]
  (try
    (let [backup-data (json/read-str (slurp backup-path) :key-fn keyword)]
      (when (:settings backup-data)
        (save-settings! (:settings backup-data)))
      (when (:custom-models backup-data)
        (save-custom-models! (:custom-models backup-data)))
      (when (:alert-rules backup-data)
        (save-alert-rules! (:alert-rules backup-data)))
      {:success true :restored-at (str (Instant/now))})
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Web App Sync
;; =============================================================================

(defn sync-settings-to-web!
  "Sync local settings to web app"
  []
  (when (api/is-online?)
    (try
      (let [settings (load-settings)
            custom-models (load-custom-models)
            alert-rules (load-alert-rules)
            sync-data {:settings (dissoc settings :device-id :created-at)
                       :custom-models custom-models
                       :alert-rules alert-rules
                       :device-id (:device-id settings)
                       :device-name (:device-name settings)
                       :synced-at (str (Instant/now))}
            response (api/sync-settings sync-data)]
        (when response
          (update-setting! :last-sync (str (Instant/now)))
          {:success true :synced-at (:last-sync (load-settings))}))
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn sync-settings-from-web!
  "Sync settings from web app to local"
  []
  (when (api/is-online?)
    (try
      (let [remote-settings (api/get-settings)]
        (when remote-settings
          ;; Merge remote settings with local, preferring remote for most fields
          (let [local (load-settings)
                merged (merge local
                              (dissoc (:settings remote-settings) :device-id :device-name)
                              {:last-sync (str (Instant/now))})]
            (save-settings! merged)
            
            ;; Sync custom models if present
            (when-let [remote-models (:custom-models remote-settings)]
              (let [local-models (load-custom-models)
                    ;; Merge by ID, preferring newer versions
                    merged-models (merge-by-id local-models remote-models)]
                (save-custom-models! merged-models)))
            
            ;; Sync alert rules if present
            (when-let [remote-rules (:alert-rules remote-settings)]
              (let [local-rules (load-alert-rules)
                    merged-rules (merge-by-id local-rules remote-rules)]
                (save-alert-rules! merged-rules)))
            
            {:success true :synced-at (:last-sync (load-settings))})))
      (catch Exception e
        {:success false :error (.getMessage e)}))))

(defn- merge-by-id
  "Merge two collections by ID, preferring items with newer updated-at"
  [local remote]
  (let [local-map (into {} (map (juxt :id identity) local))
        remote-map (into {} (map (juxt :id identity) remote))
        all-ids (set (concat (keys local-map) (keys remote-map)))]
    (vec (for [id all-ids]
           (let [local-item (get local-map id)
                 remote-item (get remote-map id)]
             (cond
               (nil? local-item) remote-item
               (nil? remote-item) local-item
               :else (let [local-time (or (:updated-at local-item) (:created-at local-item) "")
                           remote-time (or (:updated-at remote-item) (:created-at remote-item) "")]
                       (if (pos? (compare remote-time local-time))
                         remote-item
                         local-item))))))))

(defn handle-settings-conflict
  "Handle conflicts when syncing settings"
  [local-settings remote-settings]
  ;; For now, use a simple strategy: remote wins for most fields,
  ;; but device-specific settings stay local
  (let [device-specific #{:device-id :device-name :lm-studio-url :watched-folders}
        local-device (select-keys local-settings device-specific)
        merged (merge remote-settings local-device)]
    merged))

;; =============================================================================
;; Export/Import
;; =============================================================================

(defn export-settings
  "Export all settings to a portable format"
  []
  {:settings (load-settings)
   :custom-models (load-custom-models)
   :alert-rules (load-alert-rules)
   :exported-at (str (Instant/now))
   :version "1.0"})

(defn export-settings-to-file!
  "Export settings to a file"
  [file-path]
  (try
    (spit file-path (json/write-str (export-settings)))
    {:success true :path file-path}
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn import-settings-from-file!
  "Import settings from a file"
  [file-path]
  (try
    (let [data (json/read-str (slurp file-path) :key-fn keyword)]
      ;; Create backup before importing
      (create-backup!)
      
      (when (:settings data)
        (let [current (load-settings)
              ;; Preserve device-specific settings
              imported (merge (:settings data)
                              {:device-id (:device-id current)
                               :device-name (:device-name current)})]
          (save-settings! imported)))
      
      (when (:custom-models data)
        (save-custom-models! (:custom-models data)))
      
      (when (:alert-rules data)
        (save-alert-rules! (:alert-rules data)))
      
      {:success true :imported-at (str (Instant/now))})
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Per-Device Settings Override
;; =============================================================================

(defn get-device-override
  "Get device-specific setting override"
  [setting-key]
  (let [settings (load-settings)
        overrides (:device-overrides settings {})]
    (get overrides setting-key)))

(defn set-device-override!
  "Set a device-specific setting override"
  [setting-key value]
  (let [settings (load-settings)
        overrides (assoc (:device-overrides settings {}) setting-key value)]
    (save-settings! (assoc settings :device-overrides overrides))))

(defn clear-device-override!
  "Clear a device-specific setting override"
  [setting-key]
  (let [settings (load-settings)
        overrides (dissoc (:device-overrides settings {}) setting-key)]
    (save-settings! (assoc settings :device-overrides overrides))))

(defn get-effective-setting
  "Get the effective value of a setting (considering device overrides)"
  [setting-key]
  (let [settings (load-settings)
        override (get-device-override setting-key)]
    (if (some? override)
      override
      (get settings setting-key))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize settings system"
  []
  (ensure-settings-dir!)
  (let [settings (load-settings)]
    ;; Generate device ID if not present
    (when-not (:device-id settings)
      (update-setting! :device-id (generate-device-id)))
    ;; Auto-sync if enabled
    (when (and (:auto-sync settings) (api/is-online?))
      (future
        (Thread/sleep 5000) ;; Wait for app to fully initialize
        (sync-settings-from-web!)))
    settings))
