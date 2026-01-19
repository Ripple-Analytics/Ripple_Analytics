(ns mental-models.pipeline.integration.data-sync
  "Data synchronization service for distributed mental model analysis.
   
   Features:
   - Multi-source data synchronization
   - Conflict resolution strategies
   - Incremental sync support
   - Sync scheduling
   - Change detection
   - Bidirectional sync
   - Sync history tracking
   - Offline support with queue"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
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
  (atom {:sources {}          ;; source-id -> source-config
         :sync-jobs {}        ;; job-id -> sync-job
         :sync-history []     ;; historical sync records
         :pending-changes {}  ;; source-id -> pending changes
         :conflict-queue []   ;; unresolved conflicts
         :schedules {}        ;; schedule-id -> schedule
         :initialized? false}))

;; ============================================================================
;; Source Management
;; ============================================================================

(defn register-source!
  "Register a data source for synchronization."
  [source-id config]
  (let [source {:id source-id
                :name (get config :name (name source-id))
                :type (get config :type :database)
                :connection (get config :connection {})
                :sync-direction (get config :sync-direction :bidirectional)
                :conflict-strategy (get config :conflict-strategy :last-write-wins)
                :batch-size (get config :batch-size 100)
                :enabled? (get config :enabled? true)
                :last-sync nil
                :sync-token nil
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:sources source-id] source)
    (logging/log :info "Registered sync source" {:source-id source-id :type (:type source)})
    (events/emit! :sync-source-registered {:source-id source-id})
    source-id))

(defn get-source
  "Get a sync source."
  [source-id]
  (get-in @state [:sources source-id]))

(defn list-sources
  "List all sync sources."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :type (:type s)
           :sync-direction (:sync-direction s)
           :last-sync (:last-sync s)
           :enabled? (:enabled? s)})
        (:sources @state)))

(defn update-source!
  "Update a sync source."
  [source-id updates]
  (swap! state update-in [:sources source-id] merge updates))

;; ============================================================================
;; Change Detection
;; ============================================================================

(defn- compute-hash
  "Compute a hash for change detection."
  [data]
  (hash data))

(defn detect-changes
  "Detect changes between local and remote data."
  [local-data remote-data & {:keys [key-fn] :or {key-fn :id}}]
  (let [local-map (into {} (map (fn [item] [(key-fn item) item]) local-data))
        remote-map (into {} (map (fn [item] [(key-fn item) item]) remote-data))
        local-keys (set (keys local-map))
        remote-keys (set (keys remote-map))
        
        added (clojure.set/difference remote-keys local-keys)
        deleted (clojure.set/difference local-keys remote-keys)
        common (clojure.set/intersection local-keys remote-keys)
        
        modified (filter (fn [k]
                           (not= (compute-hash (get local-map k))
                                 (compute-hash (get remote-map k))))
                         common)]
    {:added (mapv #(get remote-map %) added)
     :deleted (mapv #(get local-map %) deleted)
     :modified (mapv (fn [k]
                       {:key k
                        :local (get local-map k)
                        :remote (get remote-map k)})
                     modified)
     :unchanged (- (count common) (count modified))
     :total-changes (+ (count added) (count deleted) (count modified))}))

(defn record-change!
  "Record a pending change for sync."
  [source-id change]
  (let [change-record {:id (str (UUID/randomUUID))
                       :source-id source-id
                       :type (get change :type :update)
                       :entity-type (get change :entity-type)
                       :entity-id (get change :entity-id)
                       :data (get change :data)
                       :timestamp (System/currentTimeMillis)
                       :synced? false}]
    (swap! state update-in [:pending-changes source-id]
           (fn [changes] (conj (or changes []) change-record)))
    change-record))

(defn get-pending-changes
  "Get pending changes for a source."
  [source-id]
  (get-in @state [:pending-changes source-id] []))

(defn clear-pending-changes!
  "Clear pending changes for a source."
  [source-id]
  (swap! state assoc-in [:pending-changes source-id] []))

;; ============================================================================
;; Conflict Resolution
;; ============================================================================

(defn- resolve-conflict-last-write-wins
  "Resolve conflict using last-write-wins strategy."
  [local remote]
  (if (> (get local :updated-at 0) (get remote :updated-at 0))
    {:winner :local :data local}
    {:winner :remote :data remote}))

(defn- resolve-conflict-first-write-wins
  "Resolve conflict using first-write-wins strategy."
  [local remote]
  (if (< (get local :updated-at 0) (get remote :updated-at 0))
    {:winner :local :data local}
    {:winner :remote :data remote}))

(defn- resolve-conflict-merge
  "Resolve conflict by merging data."
  [local remote]
  {:winner :merged
   :data (merge local remote)})

(defn resolve-conflict
  "Resolve a sync conflict."
  [conflict strategy]
  (let [{:keys [local remote]} conflict]
    (case strategy
      :last-write-wins (resolve-conflict-last-write-wins local remote)
      :first-write-wins (resolve-conflict-first-write-wins local remote)
      :merge (resolve-conflict-merge local remote)
      :manual {:winner :manual :data nil :requires-resolution true}
      (resolve-conflict-last-write-wins local remote))))

(defn queue-conflict!
  "Queue a conflict for manual resolution."
  [conflict]
  (let [conflict-record (assoc conflict
                               :id (str (UUID/randomUUID))
                               :queued-at (System/currentTimeMillis)
                               :resolved? false)]
    (swap! state update :conflict-queue conj conflict-record)
    (logging/log :warn "Queued sync conflict" {:conflict-id (:id conflict-record)})
    (events/emit! :sync-conflict-queued {:conflict-id (:id conflict-record)})
    conflict-record))

(defn resolve-queued-conflict!
  "Resolve a queued conflict."
  [conflict-id resolution]
  (swap! state update :conflict-queue
         (fn [queue]
           (mapv (fn [c]
                   (if (= (:id c) conflict-id)
                     (assoc c :resolved? true :resolution resolution :resolved-at (System/currentTimeMillis))
                     c))
                 queue)))
  (logging/log :info "Resolved sync conflict" {:conflict-id conflict-id}))

(defn get-unresolved-conflicts
  "Get unresolved conflicts."
  []
  (filterv #(not (:resolved? %)) (:conflict-queue @state)))

;; ============================================================================
;; Sync Operations
;; ============================================================================

(defn- create-sync-job
  "Create a sync job."
  [source-id direction]
  (let [job-id (str (UUID/randomUUID))]
    {:id job-id
     :source-id source-id
     :direction direction
     :status :pending
     :started-at nil
     :completed-at nil
     :items-synced 0
     :items-failed 0
     :conflicts 0
     :error nil}))

(defn- update-sync-job!
  "Update a sync job."
  [job-id updates]
  (swap! state update-in [:sync-jobs job-id] merge updates))

(defn sync-source!
  "Synchronize a data source."
  [source-id & {:keys [direction force?]}]
  (when (flags/enabled? :data-sync)
    (let [source (get-source source-id)
          sync-direction (or direction (:sync-direction source) :bidirectional)
          job (create-sync-job source-id sync-direction)
          job-id (:id job)]
      
      (swap! state assoc-in [:sync-jobs job-id] job)
      (update-sync-job! job-id {:status :running :started-at (System/currentTimeMillis)})
      
      (logging/log :info "Starting sync" {:source-id source-id :job-id job-id :direction sync-direction})
      (events/emit! :sync-started {:source-id source-id :job-id job-id})
      (metrics/increment :sync-operations {:source-id source-id})
      
      (try
        ;; Simulate sync operation
        (let [pending (get-pending-changes source-id)
              items-synced (count pending)
              conflicts 0]
          
          ;; Mark changes as synced
          (clear-pending-changes! source-id)
          
          ;; Update source last sync time
          (update-source! source-id {:last-sync (System/currentTimeMillis)})
          
          ;; Complete job
          (update-sync-job! job-id
                            {:status :completed
                             :completed-at (System/currentTimeMillis)
                             :items-synced items-synced
                             :conflicts conflicts})
          
          ;; Record in history
          (swap! state update :sync-history conj
                 {:job-id job-id
                  :source-id source-id
                  :direction sync-direction
                  :items-synced items-synced
                  :completed-at (System/currentTimeMillis)})
          
          (logging/log :info "Sync completed" {:source-id source-id :job-id job-id :items-synced items-synced})
          (events/emit! :sync-completed {:source-id source-id :job-id job-id})
          
          (get-in @state [:sync-jobs job-id]))
        
        (catch Exception e
          (update-sync-job! job-id
                            {:status :failed
                             :completed-at (System/currentTimeMillis)
                             :error (.getMessage e)})
          (logging/log :error "Sync failed" {:source-id source-id :job-id job-id :error (.getMessage e)})
          (events/emit! :sync-failed {:source-id source-id :job-id job-id :error (.getMessage e)})
          (get-in @state [:sync-jobs job-id]))))))

(defn get-sync-job
  "Get a sync job."
  [job-id]
  (get-in @state [:sync-jobs job-id]))

(defn list-sync-jobs
  "List sync jobs."
  [& {:keys [source-id status since limit]}]
  (let [jobs (vals (:sync-jobs @state))
        filtered (cond->> jobs
                   source-id (filter #(= (:source-id %) source-id))
                   status (filter #(= (:status %) status))
                   since (filter #(>= (:started-at %) since))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Sync Scheduling
;; ============================================================================

(defn schedule-sync!
  "Schedule a recurring sync."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :source-id (get config :source-id)
                  :interval-ms (get config :interval-ms 3600000) ;; 1 hour default
                  :direction (get config :direction :bidirectional)
                  :enabled? (get config :enabled? true)
                  :last-run nil
                  :next-run (+ (System/currentTimeMillis) (get config :interval-ms 3600000))
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schedules schedule-id] schedule)
    (logging/log :info "Scheduled sync" {:schedule-id schedule-id :source-id (:source-id config)})
    schedule-id))

(defn run-scheduled-sync!
  "Run a scheduled sync."
  [schedule-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (when (:enabled? schedule)
      (let [result (sync-source! (:source-id schedule) :direction (:direction schedule))]
        (swap! state update-in [:schedules schedule-id]
               (fn [s]
                 (assoc s
                        :last-run (System/currentTimeMillis)
                        :next-run (+ (System/currentTimeMillis) (:interval-ms s)))))
        result))))

(defn list-schedules
  "List all sync schedules."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :source-id (:source-id s)
           :interval-ms (:interval-ms s)
           :enabled? (:enabled? s)
           :last-run (:last-run s)
           :next-run (:next-run s)})
        (:schedules @state)))

;; ============================================================================
;; Offline Support
;; ============================================================================

(defn queue-offline-change!
  "Queue a change for later sync when offline."
  [source-id change]
  (let [queued-change (assoc change
                             :queued-at (System/currentTimeMillis)
                             :offline? true)]
    (record-change! source-id queued-change)))

(defn sync-offline-changes!
  "Sync all queued offline changes."
  [source-id]
  (let [pending (get-pending-changes source-id)
        offline-changes (filter :offline? pending)]
    (when (seq offline-changes)
      (logging/log :info "Syncing offline changes" {:source-id source-id :count (count offline-changes)})
      (sync-source! source-id :force? true))))

;; ============================================================================
;; Sync History
;; ============================================================================

(defn get-sync-history
  "Get sync history."
  [& {:keys [source-id since limit]}]
  (let [history (:sync-history @state)
        filtered (cond->> history
                   source-id (filter #(= (:source-id %) source-id))
                   since (filter #(>= (:completed-at %) since))
                   true (sort-by :completed-at >)
                   limit (take limit))]
    (vec filtered)))

(defn get-sync-stats
  "Get sync statistics for a source."
  [source-id]
  (let [history (get-sync-history :source-id source-id)
        total-syncs (count history)
        total-items (reduce + (map :items-synced history))]
    {:source-id source-id
     :total-syncs total-syncs
     :total-items-synced total-items
     :avg-items-per-sync (if (pos? total-syncs) (/ total-items total-syncs) 0)
     :last-sync (:completed-at (first history))
     :pending-changes (count (get-pending-changes source-id))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-service-stats
  "Get data sync service statistics."
  []
  (let [jobs (vals (:sync-jobs @state))
        by-status (group-by :status jobs)]
    {:total-sources (count (:sources @state))
     :total-jobs (count jobs)
     :total-schedules (count (:schedules @state))
     :jobs-by-status (into {} (map (fn [[k v]] [k (count v)]) by-status))
     :pending-changes (reduce + (map count (vals (:pending-changes @state))))
     :unresolved-conflicts (count (get-unresolved-conflicts))
     :sync-history-size (count (:sync-history @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-data-sync!
  "Initialize the data sync service."
  []
  (when-not (:initialized? @state)
    ;; Register default sources
    (register-source! :local-db
                      {:name "Local Database"
                       :type :database
                       :sync-direction :bidirectional
                       :conflict-strategy :last-write-wins})
    
    (register-source! :cloud-storage
                      {:name "Cloud Storage"
                       :type :cloud
                       :sync-direction :push
                       :conflict-strategy :last-write-wins})
    
    ;; Schedule default sync
    (schedule-sync! :hourly-sync
                    {:name "Hourly Database Sync"
                     :source-id :local-db
                     :interval-ms 3600000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Data sync service initialized")
    (events/emit! :data-sync-initialized {})
    true))
