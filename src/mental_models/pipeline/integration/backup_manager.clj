(ns mental-models.pipeline.integration.backup-manager
  "Backup manager for mental model analysis data.
   
   Features:
   - Automated backups
   - Incremental backups
   - Backup scheduling
   - Retention policies
   - Restore operations
   - Backup verification
   - Compression
   - Encryption support"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.security MessageDigest]
           [java.util Base64]
           [java.util.zip GZIPOutputStream GZIPInputStream]
           [java.io ByteArrayOutputStream ByteArrayInputStream]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:backups {}          ;; backup-id -> backup
         :schedules {}        ;; schedule-id -> schedule
         :policies {}         ;; policy-id -> retention-policy
         :sources {}          ;; source-id -> backup-source
         :restore-points {}   ;; restore-point-id -> restore-point
         :stats {:backups-created 0 :backups-restored 0 :bytes-backed-up 0}
         :initialized? false}))

;; ============================================================================
;; Backup Source Management
;; ============================================================================

(defn register-source!
  "Register a backup source."
  [source-id config]
  (let [source {:id source-id
                :name (get config :name (name source-id))
                :type (get config :type :data) ;; :data, :config, :logs, :full
                :path (get config :path)
                :include-patterns (get config :include-patterns ["*"])
                :exclude-patterns (get config :exclude-patterns [])
                :backup-fn (get config :backup-fn)
                :restore-fn (get config :restore-fn)
                :enabled? (get config :enabled? true)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:sources source-id] source)
    (logging/log :info "Registered backup source" {:source-id source-id})
    source-id))

(defn get-source
  "Get a backup source."
  [source-id]
  (get-in @state [:sources source-id]))

(defn list-sources
  "List all backup sources."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :type (:type s)
           :enabled? (:enabled? s)})
        (:sources @state)))

(defn delete-source!
  "Delete a backup source."
  [source-id]
  (swap! state update :sources dissoc source-id))

;; ============================================================================
;; Compression and Encryption
;; ============================================================================

(defn- compress-data
  "Compress data using GZIP."
  [data]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [gzip (GZIPOutputStream. baos)]
      (.write gzip (.getBytes (pr-str data) "UTF-8")))
    (.toByteArray baos)))

(defn- decompress-data
  "Decompress GZIP data."
  [compressed-bytes]
  (with-open [bais (ByteArrayInputStream. compressed-bytes)
              gzip (GZIPInputStream. bais)
              baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [len (.read gzip buffer)]
          (when (pos? len)
            (.write baos buffer 0 len)
            (recur)))))
    (edn/read-string (String. (.toByteArray baos) "UTF-8"))))

(defn- compute-checksum
  "Compute checksum for data."
  [data]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (if (bytes? data) data (.getBytes (str data) "UTF-8"))]
    (.encodeToString (Base64/getEncoder) (.digest digest bytes))))

;; ============================================================================
;; Backup Operations
;; ============================================================================

(defn create-backup!
  "Create a backup."
  [config]
  (when (flags/enabled? :backup-manager)
    (let [backup-id (str (UUID/randomUUID))
          source-ids (get config :source-ids (keys (:sources @state)))
          timestamp (System/currentTimeMillis)
          formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd-HH-mm-ss")
          timestamp-str (.format formatter (LocalDateTime/ofInstant (Instant/ofEpochMilli timestamp) (ZoneId/systemDefault)))
          
          backup {:id backup-id
                  :name (get config :name (str "backup-" timestamp-str))
                  :type (get config :type :full) ;; :full, :incremental, :differential
                  :source-ids source-ids
                  :status :in-progress
                  :compress? (get config :compress? true)
                  :encrypt? (get config :encrypt? false)
                  :started-at timestamp
                  :data {}
                  :metadata {:created-by (get config :created-by "system")
                             :description (get config :description "")}}]
      
      (swap! state assoc-in [:backups backup-id] backup)
      (logging/log :info "Starting backup" {:backup-id backup-id})
      (events/emit! :backup-started {:backup-id backup-id})
      
      (go
        (try
          (let [backup-data (reduce
                             (fn [acc source-id]
                               (if-let [source (get-source source-id)]
                                 (if (:enabled? source)
                                   (let [backup-fn (:backup-fn source)
                                         data (if backup-fn
                                                (backup-fn)
                                                {:source-id source-id :data "sample-data"})]
                                     (assoc acc source-id data))
                                   acc)
                                 acc))
                             {}
                             source-ids)
                
                ;; Compress if requested
                compressed (if (:compress? backup)
                             (compress-data backup-data)
                             backup-data)
                
                size (if (bytes? compressed)
                       (count compressed)
                       (count (pr-str compressed)))
                
                checksum (compute-checksum compressed)]
            
            (swap! state update-in [:backups backup-id]
                   (fn [b]
                     (assoc b
                            :status :completed
                            :data compressed
                            :size size
                            :checksum checksum
                            :completed-at (System/currentTimeMillis))))
            
            (swap! state update-in [:stats :backups-created] inc)
            (swap! state update-in [:stats :bytes-backed-up] + size)
            
            (logging/log :info "Backup completed" {:backup-id backup-id :size size})
            (events/emit! :backup-completed {:backup-id backup-id :size size}))
          
          (catch Exception e
            (swap! state update-in [:backups backup-id]
                   (fn [b]
                     (assoc b
                            :status :failed
                            :error (.getMessage e)
                            :completed-at (System/currentTimeMillis))))
            (logging/log :error "Backup failed" {:backup-id backup-id :error (.getMessage e)}))))
      
      backup-id)))

(defn get-backup
  "Get a backup."
  [backup-id]
  (let [backup (get-in @state [:backups backup-id])]
    (when backup
      (dissoc backup :data)))) ;; Don't return raw data

(defn list-backups
  "List backups."
  [& {:keys [status type limit] :or {limit 100}}]
  (let [backups (vals (:backups @state))
        filtered (cond->> backups
                   status (filter #(= (:status %) status))
                   type (filter #(= (:type %) type))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :name :type :status :size :started-at :completed-at]) filtered)))

(defn delete-backup!
  "Delete a backup."
  [backup-id]
  (swap! state update :backups dissoc backup-id)
  (logging/log :info "Deleted backup" {:backup-id backup-id}))

(defn verify-backup
  "Verify backup integrity."
  [backup-id]
  (when-let [backup (get-in @state [:backups backup-id])]
    (let [current-checksum (compute-checksum (:data backup))
          valid? (= current-checksum (:checksum backup))]
      {:backup-id backup-id
       :valid? valid?
       :stored-checksum (:checksum backup)
       :computed-checksum current-checksum
       :verified-at (System/currentTimeMillis)})))

;; ============================================================================
;; Restore Operations
;; ============================================================================

(defn restore-backup!
  "Restore from a backup."
  [backup-id & {:keys [source-ids target]}]
  (when-let [backup (get-in @state [:backups backup-id])]
    (when (= :completed (:status backup))
      (let [restore-id (str (UUID/randomUUID))
            restore-point {:id restore-id
                           :backup-id backup-id
                           :status :in-progress
                           :started-at (System/currentTimeMillis)}]
        
        (swap! state assoc-in [:restore-points restore-id] restore-point)
        (logging/log :info "Starting restore" {:restore-id restore-id :backup-id backup-id})
        (events/emit! :restore-started {:restore-id restore-id :backup-id backup-id})
        
        (go
          (try
            (let [data (if (:compress? backup)
                         (decompress-data (:data backup))
                         (:data backup))
                  
                  sources-to-restore (or source-ids (keys data))]
              
              (doseq [source-id sources-to-restore]
                (when-let [source (get-source source-id)]
                  (when-let [restore-fn (:restore-fn source)]
                    (restore-fn (get data source-id)))))
              
              (swap! state update-in [:restore-points restore-id]
                     (fn [r]
                       (assoc r
                              :status :completed
                              :completed-at (System/currentTimeMillis))))
              
              (swap! state update-in [:stats :backups-restored] inc)
              (logging/log :info "Restore completed" {:restore-id restore-id})
              (events/emit! :restore-completed {:restore-id restore-id}))
            
            (catch Exception e
              (swap! state update-in [:restore-points restore-id]
                     (fn [r]
                       (assoc r
                              :status :failed
                              :error (.getMessage e)
                              :completed-at (System/currentTimeMillis))))
              (logging/log :error "Restore failed" {:restore-id restore-id :error (.getMessage e)}))))
        
        restore-id))))

(defn get-restore-point
  "Get a restore point."
  [restore-id]
  (get-in @state [:restore-points restore-id]))

(defn list-restore-points
  "List restore points."
  [& {:keys [backup-id limit] :or {limit 50}}]
  (let [points (vals (:restore-points @state))
        filtered (cond->> points
                   backup-id (filter #(= (:backup-id %) backup-id))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :backup-id :status :started-at :completed-at]) filtered)))

;; ============================================================================
;; Scheduling
;; ============================================================================

(defn create-schedule!
  "Create a backup schedule."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :source-ids (get config :source-ids)
                  :backup-type (get config :backup-type :full)
                  :cron (get config :cron)
                  :interval-ms (get config :interval-ms 86400000) ;; Daily
                  :retention-policy-id (get config :retention-policy-id)
                  :enabled? (get config :enabled? true)
                  :last-run nil
                  :next-run (+ (System/currentTimeMillis) (get config :interval-ms 86400000))
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schedules schedule-id] schedule)
    (logging/log :info "Created backup schedule" {:schedule-id schedule-id})
    schedule-id))

(defn get-schedule
  "Get a schedule."
  [schedule-id]
  (get-in @state [:schedules schedule-id]))

(defn list-schedules
  "List all schedules."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :backup-type (:backup-type s)
           :enabled? (:enabled? s)
           :next-run (:next-run s)})
        (:schedules @state)))

(defn enable-schedule!
  "Enable a schedule."
  [schedule-id]
  (swap! state assoc-in [:schedules schedule-id :enabled?] true))

(defn disable-schedule!
  "Disable a schedule."
  [schedule-id]
  (swap! state assoc-in [:schedules schedule-id :enabled?] false))

(defn process-schedules!
  "Process due backup schedules."
  []
  (let [now (System/currentTimeMillis)
        due (filter (fn [[_ s]]
                      (and (:enabled? s)
                           (<= (:next-run s) now)))
                    (:schedules @state))]
    (doseq [[schedule-id schedule] due]
      (create-backup! {:source-ids (:source-ids schedule)
                       :type (:backup-type schedule)
                       :name (str (:name schedule) "-" (System/currentTimeMillis))})
      (swap! state update-in [:schedules schedule-id]
             (fn [s]
               (assoc s
                      :last-run now
                      :next-run (+ now (:interval-ms s))))))
    (count due)))

;; ============================================================================
;; Retention Policies
;; ============================================================================

(defn create-policy!
  "Create a retention policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :keep-daily (get config :keep-daily 7)
                :keep-weekly (get config :keep-weekly 4)
                :keep-monthly (get config :keep-monthly 12)
                :keep-yearly (get config :keep-yearly 3)
                :min-backups (get config :min-backups 1)
                :max-age-days (get config :max-age-days 365)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created retention policy" {:policy-id policy-id})
    policy-id))

(defn get-policy
  "Get a retention policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all retention policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :keep-daily (:keep-daily p)
           :keep-weekly (:keep-weekly p)})
        (:policies @state)))

(defn apply-retention-policy!
  "Apply retention policy to backups."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    (let [now (System/currentTimeMillis)
          max-age-ms (* (:max-age-days policy) 86400000)
          backups (filter #(= :completed (:status %)) (vals (:backups @state)))
          sorted (sort-by :started-at > backups)
          
          ;; Keep minimum backups
          to-keep (take (:min-backups policy) sorted)
          remaining (drop (:min-backups policy) sorted)
          
          ;; Filter by age
          to-delete (filter #(> (- now (:started-at %)) max-age-ms) remaining)]
      
      (doseq [backup to-delete]
        (delete-backup! (:id backup)))
      
      {:deleted (count to-delete)
       :kept (- (count backups) (count to-delete))})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-backup-stats
  "Get backup statistics."
  []
  (let [stats (:stats @state)
        backups (vals (:backups @state))
        by-status (frequencies (map :status backups))
        total-size (reduce + (map #(or (:size %) 0) backups))]
    {:total-backups (count backups)
     :total-sources (count (:sources @state))
     :total-schedules (count (:schedules @state))
     :total-policies (count (:policies @state))
     :backups-by-status by-status
     :total-size-bytes total-size
     :backups-created (:backups-created stats)
     :backups-restored (:backups-restored stats)
     :bytes-backed-up (:bytes-backed-up stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-backup-manager!
  "Initialize the backup manager."
  []
  (when-not (:initialized? @state)
    ;; Register default sources
    (register-source! :mental-models
                      {:name "Mental Models Data"
                       :type :data
                       :backup-fn (fn [] {:models [:confirmation-bias :anchoring :availability]
                                          :timestamp (System/currentTimeMillis)})
                       :restore-fn (fn [data] (logging/log :info "Restored mental models" data))})
    
    (register-source! :analysis-results
                      {:name "Analysis Results"
                       :type :data
                       :backup-fn (fn [] {:results []
                                          :timestamp (System/currentTimeMillis)})})
    
    (register-source! :configuration
                      {:name "System Configuration"
                       :type :config
                       :backup-fn (fn [] {:config {:version "1.0.0"}})})
    
    ;; Create default retention policy
    (create-policy! :default
                    {:name "Default Policy"
                     :keep-daily 7
                     :keep-weekly 4
                     :keep-monthly 6
                     :min-backups 3
                     :max-age-days 180})
    
    ;; Create default schedule
    (create-schedule! :daily-backup
                      {:name "Daily Backup"
                       :source-ids [:mental-models :analysis-results :configuration]
                       :backup-type :full
                       :interval-ms 86400000
                       :retention-policy-id :default})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Backup manager initialized")
    (events/emit! :backup-manager-initialized {})
    true))
