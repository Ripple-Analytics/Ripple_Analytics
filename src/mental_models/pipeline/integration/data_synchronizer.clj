(ns mental-models.pipeline.integration.data-synchronizer
  "Data Synchronizer Module
   
   Data synchronization across systems:
   - Bi-directional sync
   - Conflict resolution
   - Change detection
   - Incremental sync
   - Sync scheduling"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]
   [java.util.concurrent.atomic AtomicLong]
   [java.security MessageDigest]))

;; =============================================================================
;; DATA SYNCHRONIZER STATE
;; =============================================================================

(defonce sync-state (atom {:sources (ConcurrentHashMap.)
                           :targets (ConcurrentHashMap.)
                           :mappings (ConcurrentHashMap.)
                           :sync-jobs (ConcurrentHashMap.)
                           :sync-history (ConcurrentHashMap.)
                           :conflict-handlers {}
                           :scheduler nil
                           :sync-count (AtomicLong. 0)
                           :config {:default-strategy :last-write-wins
                                    :max-batch-size 1000
                                    :sync-interval-ms 60000}}))

;; =============================================================================
;; HASH UTILITIES
;; =============================================================================

(defn compute-hash
  "Compute hash of data for change detection."
  [data]
  (let [md (MessageDigest/getInstance "SHA-256")
        bytes (.getBytes (pr-str data) "UTF-8")]
    (.update md bytes)
    (apply str (map #(format "%02x" %) (.digest md)))))

;; =============================================================================
;; SOURCE/TARGET MANAGEMENT
;; =============================================================================

(defn register-source!
  "Register a data source."
  [source-id {:keys [name type fetch-fn list-fn get-hash-fn]}]
  (log/info "Registering data source" {:id source-id :type type})
  (.put ^ConcurrentHashMap (:sources @sync-state) source-id
        {:id source-id
         :name name
         :type type
         :fetch-fn fetch-fn
         :list-fn list-fn
         :get-hash-fn (or get-hash-fn compute-hash)
         :registered-at (System/currentTimeMillis)}))

(defn unregister-source!
  "Unregister a data source."
  [source-id]
  (.remove ^ConcurrentHashMap (:sources @sync-state) source-id))

(defn get-source
  "Get a source by ID."
  [source-id]
  (.get ^ConcurrentHashMap (:sources @sync-state) source-id))

(defn register-target!
  "Register a data target."
  [target-id {:keys [name type write-fn delete-fn list-fn get-hash-fn]}]
  (log/info "Registering data target" {:id target-id :type type})
  (.put ^ConcurrentHashMap (:targets @sync-state) target-id
        {:id target-id
         :name name
         :type type
         :write-fn write-fn
         :delete-fn delete-fn
         :list-fn list-fn
         :get-hash-fn (or get-hash-fn compute-hash)
         :registered-at (System/currentTimeMillis)}))

(defn unregister-target!
  "Unregister a data target."
  [target-id]
  (.remove ^ConcurrentHashMap (:targets @sync-state) target-id))

(defn get-target
  "Get a target by ID."
  [target-id]
  (.get ^ConcurrentHashMap (:targets @sync-state) target-id))

;; =============================================================================
;; MAPPING MANAGEMENT
;; =============================================================================

(defn create-mapping!
  "Create a sync mapping between source and target."
  [mapping-id {:keys [source-id target-id transform-fn filter-fn key-fn]}]
  (log/info "Creating sync mapping" {:id mapping-id :source source-id :target target-id})
  (.put ^ConcurrentHashMap (:mappings @sync-state) mapping-id
        {:id mapping-id
         :source-id source-id
         :target-id target-id
         :transform-fn (or transform-fn identity)
         :filter-fn (or filter-fn (constantly true))
         :key-fn (or key-fn :id)
         :created-at (System/currentTimeMillis)}))

(defn delete-mapping!
  "Delete a sync mapping."
  [mapping-id]
  (.remove ^ConcurrentHashMap (:mappings @sync-state) mapping-id))

(defn get-mapping
  "Get a mapping by ID."
  [mapping-id]
  (.get ^ConcurrentHashMap (:mappings @sync-state) mapping-id))

;; =============================================================================
;; CONFLICT RESOLUTION
;; =============================================================================

(defn register-conflict-handler!
  "Register a conflict resolution handler."
  [strategy-id handler-fn]
  (swap! sync-state assoc-in [:conflict-handlers strategy-id] handler-fn))

(defn resolve-conflict
  "Resolve a sync conflict."
  [strategy source-record target-record]
  (let [handler (get-in @sync-state [:conflict-handlers strategy])]
    (if handler
      (handler source-record target-record)
      ;; Default: last-write-wins
      (if (> (or (:updated-at source-record) 0)
             (or (:updated-at target-record) 0))
        source-record
        target-record))))

;; Built-in conflict handlers
(defn init-conflict-handlers!
  "Initialize built-in conflict handlers."
  []
  ;; Last write wins
  (register-conflict-handler! :last-write-wins
                              (fn [source target]
                                (if (> (or (:updated-at source) 0)
                                       (or (:updated-at target) 0))
                                  source
                                  target)))
  ;; Source wins
  (register-conflict-handler! :source-wins
                              (fn [source _target] source))
  ;; Target wins
  (register-conflict-handler! :target-wins
                              (fn [_source target] target))
  ;; Merge (combine fields)
  (register-conflict-handler! :merge
                              (fn [source target]
                                (merge target source))))

;; =============================================================================
;; CHANGE DETECTION
;; =============================================================================

(defn detect-changes
  "Detect changes between source and target."
  [source-items target-items key-fn hash-fn]
  (let [source-map (into {} (map (fn [item] [(key-fn item) item]) source-items))
        target-map (into {} (map (fn [item] [(key-fn item) item]) target-items))
        source-keys (set (keys source-map))
        target-keys (set (keys target-map))
        added (clojure.set/difference source-keys target-keys)
        deleted (clojure.set/difference target-keys source-keys)
        common (clojure.set/intersection source-keys target-keys)
        modified (filter (fn [k]
                           (not= (hash-fn (get source-map k))
                                 (hash-fn (get target-map k))))
                         common)]
    {:added (map #(get source-map %) added)
     :deleted (map #(get target-map %) deleted)
     :modified (map (fn [k] {:source (get source-map k)
                             :target (get target-map k)})
                    modified)
     :unchanged (filter (fn [k]
                          (= (hash-fn (get source-map k))
                             (hash-fn (get target-map k))))
                        common)}))

;; =============================================================================
;; SYNC EXECUTION
;; =============================================================================

(defn sync-mapping!
  "Execute sync for a mapping."
  [mapping-id & {:keys [strategy dry-run]}]
  (.incrementAndGet ^AtomicLong (:sync-count @sync-state))
  (metrics/inc-counter! :datasync/syncs-executed)
  (let [mapping (get-mapping mapping-id)
        source (get-source (:source-id mapping))
        target (get-target (:target-id mapping))
        strategy (or strategy (get-in @sync-state [:config :default-strategy]))
        start-time (System/currentTimeMillis)]
    (if (and mapping source target)
      (try
        (log/info "Starting sync" {:mapping mapping-id :source (:source-id mapping) :target (:target-id mapping)})
        ;; Fetch data from source and target
        (let [source-items (filter (:filter-fn mapping) ((:list-fn source)))
              target-items ((:list-fn target))
              key-fn (:key-fn mapping)
              hash-fn (:get-hash-fn source)
              changes (detect-changes source-items target-items key-fn hash-fn)
              transform-fn (:transform-fn mapping)]
          (if dry-run
            ;; Dry run - just return changes
            {:mapping-id mapping-id
             :dry-run true
             :changes changes
             :stats {:added (count (:added changes))
                     :deleted (count (:deleted changes))
                     :modified (count (:modified changes))
                     :unchanged (count (:unchanged changes))}}
            ;; Execute sync
            (let [;; Add new items
                  added-results (doall (for [item (:added changes)]
                                         (try
                                           ((:write-fn target) (transform-fn item))
                                           {:status :success :item item}
                                           (catch Exception e
                                             {:status :error :item item :error (.getMessage e)}))))
                  ;; Update modified items
                  modified-results (doall (for [{:keys [source target]} (:modified changes)]
                                            (try
                                              (let [resolved (resolve-conflict strategy source target)]
                                                ((:write-fn target) (transform-fn resolved))
                                                {:status :success :item resolved})
                                              (catch Exception e
                                                {:status :error :item source :error (.getMessage e)}))))
                  ;; Delete removed items (if target supports it)
                  deleted-results (when (:delete-fn target)
                                    (doall (for [item (:deleted changes)]
                                             (try
                                               ((:delete-fn target) (key-fn item))
                                               {:status :success :item item}
                                               (catch Exception e
                                                 {:status :error :item item :error (.getMessage e)})))))
                  duration-ms (- (System/currentTimeMillis) start-time)
                  result {:mapping-id mapping-id
                          :status :completed
                          :duration-ms duration-ms
                          :stats {:added (count (filter #(= :success (:status %)) added-results))
                                  :modified (count (filter #(= :success (:status %)) modified-results))
                                  :deleted (count (filter #(= :success (:status %)) deleted-results))
                                  :errors (+ (count (filter #(= :error (:status %)) added-results))
                                             (count (filter #(= :error (:status %)) modified-results))
                                             (count (filter #(= :error (:status %)) deleted-results)))}}]
              ;; Record history
              (.put ^ConcurrentHashMap (:sync-history @sync-state)
                    (str mapping-id "-" (System/currentTimeMillis))
                    (assoc result :timestamp (System/currentTimeMillis)))
              ;; Publish event
              (events/publish! :datasync/sync-completed result)
              (log/info "Sync completed" result)
              result)))
        (catch Exception e
          (log/error "Sync failed" {:mapping mapping-id :error (.getMessage e)})
          {:mapping-id mapping-id
           :status :error
           :error (.getMessage e)}))
      {:mapping-id mapping-id
       :status :error
       :error "Invalid mapping, source, or target"})))

;; =============================================================================
;; SYNC JOBS
;; =============================================================================

(defn create-sync-job!
  "Create a scheduled sync job."
  [job-id {:keys [mapping-id interval-ms strategy enabled]}]
  (log/info "Creating sync job" {:id job-id :mapping mapping-id :interval-ms interval-ms})
  (.put ^ConcurrentHashMap (:sync-jobs @sync-state) job-id
        {:id job-id
         :mapping-id mapping-id
         :interval-ms interval-ms
         :strategy strategy
         :enabled (if (nil? enabled) true enabled)
         :last-run nil
         :next-run (+ (System/currentTimeMillis) interval-ms)
         :created-at (System/currentTimeMillis)}))

(defn delete-sync-job!
  "Delete a sync job."
  [job-id]
  (.remove ^ConcurrentHashMap (:sync-jobs @sync-state) job-id))

(defn enable-sync-job!
  "Enable a sync job."
  [job-id]
  (when-let [job (.get ^ConcurrentHashMap (:sync-jobs @sync-state) job-id)]
    (.put ^ConcurrentHashMap (:sync-jobs @sync-state) job-id
          (assoc job :enabled true))))

(defn disable-sync-job!
  "Disable a sync job."
  [job-id]
  (when-let [job (.get ^ConcurrentHashMap (:sync-jobs @sync-state) job-id)]
    (.put ^ConcurrentHashMap (:sync-jobs @sync-state) job-id
          (assoc job :enabled false))))

(defn run-due-jobs!
  "Run due sync jobs."
  []
  (let [now (System/currentTimeMillis)]
    (doseq [[job-id job] (:sync-jobs @sync-state)]
      (when (and (:enabled job)
                 (<= (:next-run job) now))
        (try
          (sync-mapping! (:mapping-id job) :strategy (:strategy job))
          (.put ^ConcurrentHashMap (:sync-jobs @sync-state) job-id
                (assoc job
                       :last-run now
                       :next-run (+ now (:interval-ms job))))
          (catch Exception e
            (log/error "Sync job failed" {:job job-id :error (.getMessage e)})))))))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-scheduler!
  "Start the sync scheduler."
  []
  (log/info "Starting sync scheduler")
  (let [scheduler (ScheduledThreadPoolExecutor. 1)]
    (.scheduleAtFixedRate scheduler
                          #(run-due-jobs!)
                          60000
                          60000
                          TimeUnit/MILLISECONDS)
    (swap! sync-state assoc :scheduler scheduler)))

(defn stop-scheduler!
  "Stop the sync scheduler."
  []
  (when-let [scheduler (:scheduler @sync-state)]
    (log/info "Stopping sync scheduler")
    (.shutdown ^ScheduledThreadPoolExecutor scheduler)
    (swap! sync-state assoc :scheduler nil)))

;; =============================================================================
;; SYNC HISTORY
;; =============================================================================

(defn get-sync-history
  "Get sync history."
  [& {:keys [mapping-id limit since]}]
  (let [history (vals (:sync-history @sync-state))]
    (cond->> history
      mapping-id (filter #(= (:mapping-id %) mapping-id))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

(defn clear-sync-history!
  "Clear sync history."
  [& {:keys [mapping-id before]}]
  (doseq [[k v] (:sync-history @sync-state)]
    (when (and (or (nil? mapping-id) (= (:mapping-id v) mapping-id))
               (or (nil? before) (< (:timestamp v) before)))
      (.remove ^ConcurrentHashMap (:sync-history @sync-state) k))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-sync-stats
  "Get synchronizer statistics."
  []
  {:sources (.size ^ConcurrentHashMap (:sources @sync-state))
   :targets (.size ^ConcurrentHashMap (:targets @sync-state))
   :mappings (.size ^ConcurrentHashMap (:mappings @sync-state))
   :jobs (.size ^ConcurrentHashMap (:sync-jobs @sync-state))
   :history-entries (.size ^ConcurrentHashMap (:sync-history @sync-state))
   :sync-count (.get ^AtomicLong (:sync-count @sync-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-synchronizer!
  "Initialize data synchronizer."
  []
  (log/info "Initializing data synchronizer")
  ;; Register feature flag
  (flags/register-flag! "data-synchronizer" "Enable data synchronizer" true)
  ;; Create metrics
  (metrics/create-counter! :datasync/syncs-executed "Syncs executed")
  (metrics/create-gauge! :datasync/active-jobs "Active sync jobs"
                         #(count (filter :enabled (vals (:sync-jobs @sync-state)))))
  ;; Initialize conflict handlers
  (init-conflict-handlers!)
  ;; Start scheduler
  (start-scheduler!)
  (log/info "Data synchronizer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-data-synchronizer-status []
  {:enabled (flags/is-enabled? "data-synchronizer")
   :sources (.size ^ConcurrentHashMap (:sources @sync-state))
   :targets (.size ^ConcurrentHashMap (:targets @sync-state))
   :mappings (.size ^ConcurrentHashMap (:mappings @sync-state))
   :jobs (.size ^ConcurrentHashMap (:sync-jobs @sync-state))
   :scheduler-running (some? (:scheduler @sync-state))
   :sync-count (.get ^AtomicLong (:sync-count @sync-state))
   :stats (get-sync-stats)
   :config (:config @sync-state)})
