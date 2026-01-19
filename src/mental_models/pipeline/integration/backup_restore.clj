(ns mental-models.pipeline.integration.backup-restore
  "Backup and Restore Module
   
   Data persistence and recovery:
   - Scheduled backups
   - Point-in-time recovery
   - Incremental backups
   - Compression
   - Encryption support"
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.io File]
   [java.util.zip GZIPOutputStream GZIPInputStream]
   [java.time LocalDateTime]
   [java.time.format DateTimeFormatter]))

;; =============================================================================
;; BACKUP STATE
;; =============================================================================

(defonce backup-state (atom {:backups []
                             :last-backup nil
                             :last-restore nil
                             :config {:backup-dir "backups"
                                      :max-backups 10
                                      :compression true
                                      :schedule-interval-ms 3600000}}))

;; =============================================================================
;; UTILITIES
;; =============================================================================

(defn timestamp-str []
  (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")))

(defn ensure-backup-dir! []
  (let [dir (io/file (get-in @backup-state [:config :backup-dir]))]
    (when-not (.exists dir)
      (.mkdirs dir))
    dir))

(defn backup-filename [name]
  (let [compression (get-in @backup-state [:config :compression])]
    (str name "-" (timestamp-str) (if compression ".edn.gz" ".edn"))))

;; =============================================================================
;; COMPRESSION
;; =============================================================================

(defn compress-data [data]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [gzip (GZIPOutputStream. baos)]
      (.write gzip (.getBytes (pr-str data) "UTF-8")))
    (.toByteArray baos)))

(defn decompress-data [bytes]
  (with-open [gzip (GZIPInputStream. (java.io.ByteArrayInputStream. bytes))]
    (edn/read-string (slurp gzip))))

;; =============================================================================
;; BACKUP OPERATIONS
;; =============================================================================

(defn create-backup!
  "Create a backup of the specified data."
  [name data & {:keys [metadata] :or {metadata {}}}]
  (when (flags/is-enabled? "backup-restore")
    (log/info "Creating backup" {:name name})
    (let [start-time (System/currentTimeMillis)
          backup-dir (ensure-backup-dir!)
          filename (backup-filename name)
          filepath (io/file backup-dir filename)
          compression (get-in @backup-state [:config :compression])
          backup-data {:name name
                       :timestamp (System/currentTimeMillis)
                       :metadata metadata
                       :data data}]
      (try
        (if compression
          (with-open [out (io/output-stream filepath)]
            (.write out (compress-data backup-data)))
          (spit filepath (pr-str backup-data)))
        (let [backup-record {:id (str (java.util.UUID/randomUUID))
                             :name name
                             :filename filename
                             :filepath (.getAbsolutePath filepath)
                             :size (.length filepath)
                             :compressed compression
                             :created-at (System/currentTimeMillis)
                             :metadata metadata}]
          ;; Update state
          (swap! backup-state update :backups conj backup-record)
          (swap! backup-state assoc :last-backup backup-record)
          ;; Cleanup old backups
          (cleanup-old-backups! name)
          ;; Record metrics
          (metrics/inc-counter! :backup/created)
          (metrics/observe-histogram! :backup/size (:size backup-record))
          ;; Publish event
          (events/publish! :backup/created backup-record)
          (log/info "Backup created" {:name name :size (:size backup-record)
                                      :duration-ms (- (System/currentTimeMillis) start-time)})
          backup-record)
        (catch Exception e
          (log/error "Backup failed" {:name name :error (.getMessage e)})
          (metrics/inc-counter! :backup/failed)
          nil)))))

(defn restore-backup!
  "Restore data from a backup."
  [backup-id]
  (when (flags/is-enabled? "backup-restore")
    (log/info "Restoring backup" {:id backup-id})
    (let [backup (first (filter #(= (:id %) backup-id) (:backups @backup-state)))]
      (if-not backup
        (do
          (log/error "Backup not found" {:id backup-id})
          nil)
        (try
          (let [filepath (io/file (:filepath backup))
                data (if (:compressed backup)
                       (decompress-data (byte-array (map byte (slurp filepath :encoding "ISO-8859-1"))))
                       (edn/read-string (slurp filepath)))]
            (swap! backup-state assoc :last-restore
                   {:backup-id backup-id
                    :restored-at (System/currentTimeMillis)})
            (metrics/inc-counter! :backup/restored)
            (events/publish! :backup/restored {:backup-id backup-id})
            (log/info "Backup restored" {:id backup-id :name (:name backup)})
            (:data data))
          (catch Exception e
            (log/error "Restore failed" {:id backup-id :error (.getMessage e)})
            (metrics/inc-counter! :backup/restore-failed)
            nil))))))

(defn restore-latest!
  "Restore the latest backup for a given name."
  [name]
  (let [backups (filter #(= (:name %) name) (:backups @backup-state))
        latest (first (sort-by :created-at > backups))]
    (when latest
      (restore-backup! (:id latest)))))

;; =============================================================================
;; INCREMENTAL BACKUPS
;; =============================================================================

(defonce incremental-state (atom {}))

(defn create-incremental-backup!
  "Create an incremental backup with only changed data."
  [name data & {:keys [diff-fn] :or {diff-fn (fn [old new] new)}}]
  (when (flags/is-enabled? "backup-restore")
    (let [previous (get @incremental-state name)
          changes (if previous
                    (diff-fn previous data)
                    data)]
      (swap! incremental-state assoc name data)
      (create-backup! (str name "-incremental") changes
                      :metadata {:type :incremental
                                 :base-timestamp (:timestamp previous)}))))

;; =============================================================================
;; BACKUP MANAGEMENT
;; =============================================================================

(defn list-backups
  "List all backups, optionally filtered by name."
  [& {:keys [name]}]
  (let [backups (:backups @backup-state)]
    (if name
      (filter #(= (:name %) name) backups)
      backups)))

(defn get-backup
  "Get a backup by ID."
  [backup-id]
  (first (filter #(= (:id %) backup-id) (:backups @backup-state))))

(defn delete-backup!
  "Delete a backup."
  [backup-id]
  (when-let [backup (get-backup backup-id)]
    (log/info "Deleting backup" {:id backup-id})
    (try
      (io/delete-file (:filepath backup))
      (swap! backup-state update :backups
             (fn [bs] (remove #(= (:id %) backup-id) bs)))
      (metrics/inc-counter! :backup/deleted)
      true
      (catch Exception e
        (log/error "Delete failed" {:id backup-id :error (.getMessage e)})
        false))))

(defn cleanup-old-backups!
  "Remove old backups exceeding max-backups limit."
  [name]
  (let [max-backups (get-in @backup-state [:config :max-backups])
        backups (sort-by :created-at > (filter #(= (:name %) name) (:backups @backup-state)))]
    (when (> (count backups) max-backups)
      (doseq [backup (drop max-backups backups)]
        (delete-backup! (:id backup))))))

;; =============================================================================
;; SCHEDULED BACKUPS
;; =============================================================================

(defonce scheduler (atom nil))

(defn schedule-backup!
  "Schedule periodic backups."
  [name data-fn & {:keys [interval-ms] :or {interval-ms 3600000}}]
  (log/info "Scheduling backup" {:name name :interval-ms interval-ms})
  (let [control-chan (clojure.core.async/chan)]
    (clojure.core.async/go-loop []
      (let [[v ch] (clojure.core.async/alts!
                    [(clojure.core.async/timeout interval-ms) control-chan])]
        (when-not (= ch control-chan)
          (try
            (create-backup! name (data-fn))
            (catch Exception e
              (log/error "Scheduled backup failed" {:name name :error (.getMessage e)})))
          (recur))))
    (swap! scheduler assoc name control-chan)
    control-chan))

(defn cancel-scheduled-backup!
  "Cancel a scheduled backup."
  [name]
  (when-let [chan (get @scheduler name)]
    (log/info "Cancelling scheduled backup" {:name name})
    (clojure.core.async/close! chan)
    (swap! scheduler dissoc name)))

;; =============================================================================
;; EXPORT/IMPORT
;; =============================================================================

(defn export-backup!
  "Export a backup to a specified path."
  [backup-id export-path]
  (when-let [backup (get-backup backup-id)]
    (log/info "Exporting backup" {:id backup-id :path export-path})
    (io/copy (io/file (:filepath backup)) (io/file export-path))
    true))

(defn import-backup!
  "Import a backup from a specified path."
  [import-path name]
  (log/info "Importing backup" {:path import-path :name name})
  (let [file (io/file import-path)]
    (when (.exists file)
      (let [backup-dir (ensure-backup-dir!)
            filename (str name "-imported-" (timestamp-str) ".edn")
            dest-path (io/file backup-dir filename)]
        (io/copy file dest-path)
        (let [backup-record {:id (str (java.util.UUID/randomUUID))
                             :name name
                             :filename filename
                             :filepath (.getAbsolutePath dest-path)
                             :size (.length dest-path)
                             :compressed (.endsWith import-path ".gz")
                             :created-at (System/currentTimeMillis)
                             :metadata {:imported true :source import-path}}]
          (swap! backup-state update :backups conj backup-record)
          backup-record)))))

;; =============================================================================
;; VERIFICATION
;; =============================================================================

(defn verify-backup
  "Verify a backup is valid and can be restored."
  [backup-id]
  (when-let [backup (get-backup backup-id)]
    (log/info "Verifying backup" {:id backup-id})
    (try
      (let [filepath (io/file (:filepath backup))]
        (if-not (.exists filepath)
          {:valid false :error "File not found"}
          (let [data (if (:compressed backup)
                       (decompress-data (byte-array (map byte (slurp filepath :encoding "ISO-8859-1"))))
                       (edn/read-string (slurp filepath)))]
            {:valid true
             :name (:name data)
             :timestamp (:timestamp data)
             :data-keys (keys (:data data))})))
      (catch Exception e
        {:valid false :error (.getMessage e)}))))

(defn verify-all-backups
  "Verify all backups."
  []
  (map (fn [backup]
         (assoc (verify-backup (:id backup)) :backup-id (:id backup)))
       (:backups @backup-state)))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defn configure!
  "Configure backup settings."
  [config]
  (log/info "Configuring backup" config)
  (swap! backup-state update :config merge config))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-backup-restore!
  "Initialize backup/restore system."
  []
  (log/info "Initializing backup/restore")
  ;; Register feature flag
  (flags/register-flag! "backup-restore" "Enable backup/restore" true)
  ;; Create metrics
  (metrics/create-counter! :backup/created "Backups created")
  (metrics/create-counter! :backup/restored "Backups restored")
  (metrics/create-counter! :backup/deleted "Backups deleted")
  (metrics/create-counter! :backup/failed "Backup failures")
  (metrics/create-counter! :backup/restore-failed "Restore failures")
  (metrics/create-histogram! :backup/size "Backup size" [1000 10000 100000 1000000])
  (metrics/create-gauge! :backup/total-count "Total backups"
                         #(count (:backups @backup-state)))
  ;; Ensure backup directory exists
  (ensure-backup-dir!)
  (log/info "Backup/restore initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-backup-status []
  {:enabled (flags/is-enabled? "backup-restore")
   :total-backups (count (:backups @backup-state))
   :last-backup (:last-backup @backup-state)
   :last-restore (:last-restore @backup-state)
   :scheduled-backups (keys @scheduler)
   :config (:config @backup-state)})
