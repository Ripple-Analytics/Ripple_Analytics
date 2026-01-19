(ns mental-models.pipeline.integration.migration-manager
  "Migration Manager Module
   
   Database schema migration management:
   - Version tracking
   - Up/down migrations
   - Migration history
   - Rollback support
   - Dry run mode"
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; MIGRATION STATE
;; =============================================================================

(defonce migration-state (atom {:migrations {}
                                :applied []
                                :current-version nil
                                :history []
                                :config {:migrations-dir "migrations"
                                         :history-file "migration_history.edn"}}))

;; =============================================================================
;; MIGRATION DEFINITION
;; =============================================================================

(defn create-migration
  "Create a migration definition."
  [version name & {:keys [up down description]}]
  {:version version
   :name name
   :description description
   :up up
   :down down
   :created-at (System/currentTimeMillis)})

(defn register-migration!
  "Register a migration."
  [migration]
  (log/info "Registering migration" {:version (:version migration) :name (:name migration)})
  (swap! migration-state assoc-in [:migrations (:version migration)] migration)
  (:version migration))

(defn get-migration
  "Get a migration by version."
  [version]
  (get-in @migration-state [:migrations version]))

(defn list-migrations
  "List all registered migrations."
  []
  (sort-by :version (vals (:migrations @migration-state))))

;; =============================================================================
;; VERSION MANAGEMENT
;; =============================================================================

(defn get-current-version
  "Get the current schema version."
  []
  (:current-version @migration-state))

(defn set-current-version!
  "Set the current schema version."
  [version]
  (swap! migration-state assoc :current-version version))

(defn get-pending-migrations
  "Get migrations that haven't been applied yet."
  []
  (let [current (get-current-version)
        all-migrations (list-migrations)]
    (if current
      (filter #(> (:version %) current) all-migrations)
      all-migrations)))

(defn get-applied-migrations
  "Get migrations that have been applied."
  []
  (:applied @migration-state))

;; =============================================================================
;; MIGRATION EXECUTION
;; =============================================================================

(defn execute-migration!
  "Execute a single migration."
  [migration direction & {:keys [dry-run] :or {dry-run false}}]
  (when (flags/is-enabled? "migration-manager")
    (let [fn-to-run (if (= direction :up) (:up migration) (:down migration))]
      (log/info "Executing migration" {:version (:version migration)
                                       :name (:name migration)
                                       :direction direction
                                       :dry-run dry-run})
      (if dry-run
        (do
          (log/info "Dry run - would execute" {:version (:version migration)})
          {:success true :dry-run true})
        (try
          (when fn-to-run
            (fn-to-run))
          ;; Update state
          (if (= direction :up)
            (do
              (swap! migration-state update :applied conj (:version migration))
              (set-current-version! (:version migration)))
            (do
              (swap! migration-state update :applied
                     (fn [applied] (remove #(= % (:version migration)) applied)))
              (let [remaining (sort (remove #(= % (:version migration))
                                            (:applied @migration-state)))]
                (set-current-version! (last remaining)))))
          ;; Record history
          (swap! migration-state update :history conj
                 {:version (:version migration)
                  :direction direction
                  :executed-at (System/currentTimeMillis)
                  :success true})
          ;; Record metrics
          (metrics/inc-counter! :migration/executed)
          ;; Publish event
          (events/publish! :migration/executed {:version (:version migration)
                                                :direction direction})
          {:success true :version (:version migration)}
          (catch Exception e
            (log/error "Migration failed" {:version (:version migration)
                                           :error (.getMessage e)})
            (swap! migration-state update :history conj
                   {:version (:version migration)
                    :direction direction
                    :executed-at (System/currentTimeMillis)
                    :success false
                    :error (.getMessage e)})
            (metrics/inc-counter! :migration/failed)
            {:success false :error (.getMessage e)}))))))

(defn migrate!
  "Run all pending migrations."
  [& {:keys [dry-run target-version] :or {dry-run false}}]
  (log/info "Running migrations" {:dry-run dry-run :target-version target-version})
  (let [pending (get-pending-migrations)
        to-run (if target-version
                 (filter #(<= (:version %) target-version) pending)
                 pending)]
    (if (empty? to-run)
      (do
        (log/info "No pending migrations")
        {:success true :migrations-run 0})
      (loop [migrations to-run
             results []]
        (if (empty? migrations)
          {:success (every? :success results)
           :migrations-run (count results)
           :results results}
          (let [migration (first migrations)
                result (execute-migration! migration :up :dry-run dry-run)]
            (if (:success result)
              (recur (rest migrations) (conj results result))
              {:success false
               :migrations-run (count results)
               :results (conj results result)
               :failed-at (:version migration)})))))))

(defn rollback!
  "Rollback the last migration or to a specific version."
  [& {:keys [steps target-version dry-run] :or {steps 1 dry-run false}}]
  (log/info "Rolling back" {:steps steps :target-version target-version :dry-run dry-run})
  (let [applied (reverse (sort (get-applied-migrations)))
        to-rollback (if target-version
                      (filter #(> % target-version) applied)
                      (take steps applied))]
    (if (empty? to-rollback)
      (do
        (log/info "Nothing to rollback")
        {:success true :migrations-rolled-back 0})
      (loop [versions to-rollback
             results []]
        (if (empty? versions)
          {:success (every? :success results)
           :migrations-rolled-back (count results)
           :results results}
          (let [version (first versions)
                migration (get-migration version)
                result (execute-migration! migration :down :dry-run dry-run)]
            (if (:success result)
              (recur (rest versions) (conj results result))
              {:success false
               :migrations-rolled-back (count results)
               :results (conj results result)
               :failed-at version})))))))

;; =============================================================================
;; MIGRATION HISTORY
;; =============================================================================

(defn get-migration-history
  "Get migration execution history."
  [& {:keys [limit] :or {limit 50}}]
  (take limit (reverse (:history @migration-state))))

(defn save-history!
  "Save migration history to file."
  []
  (let [history-file (get-in @migration-state [:config :history-file])]
    (log/info "Saving migration history" {:file history-file})
    (spit history-file (pr-str {:applied (:applied @migration-state)
                                :current-version (:current-version @migration-state)
                                :history (:history @migration-state)}))))

(defn load-history!
  "Load migration history from file."
  []
  (let [history-file (get-in @migration-state [:config :history-file])]
    (when (.exists (io/file history-file))
      (log/info "Loading migration history" {:file history-file})
      (let [data (edn/read-string (slurp history-file))]
        (swap! migration-state merge data)))))

;; =============================================================================
;; MIGRATION FILE MANAGEMENT
;; =============================================================================

(defn generate-migration-file!
  "Generate a new migration file."
  [name]
  (let [migrations-dir (get-in @migration-state [:config :migrations-dir])
        timestamp (System/currentTimeMillis)
        filename (str migrations-dir "/" timestamp "_" (str/replace name #"\s+" "_") ".edn")]
    (io/make-parents filename)
    (spit filename (pr-str {:version timestamp
                            :name name
                            :description ""
                            :up nil
                            :down nil}))
    (log/info "Generated migration file" {:file filename})
    filename))

(defn load-migrations-from-dir!
  "Load migrations from the migrations directory."
  []
  (let [migrations-dir (get-in @migration-state [:config :migrations-dir])
        dir (io/file migrations-dir)]
    (when (.exists dir)
      (doseq [file (.listFiles dir)]
        (when (.endsWith (.getName file) ".edn")
          (try
            (let [migration (edn/read-string (slurp file))]
              (register-migration! migration))
            (catch Exception e
              (log/error "Failed to load migration" {:file (.getName file)
                                                     :error (.getMessage e)}))))))))

;; =============================================================================
;; VALIDATION
;; =============================================================================

(defn validate-migrations
  "Validate all registered migrations."
  []
  (let [migrations (list-migrations)
        errors (atom [])]
    (doseq [migration migrations]
      (when-not (:version migration)
        (swap! errors conj {:migration migration :error "Missing version"}))
      (when-not (:name migration)
        (swap! errors conj {:migration migration :error "Missing name"}))
      (when-not (:up migration)
        (swap! errors conj {:migration migration :error "Missing up function"})))
    (if (empty? @errors)
      {:valid true :migration-count (count migrations)}
      {:valid false :errors @errors})))

(defn check-migration-order
  "Check that migrations are in correct order."
  []
  (let [migrations (list-migrations)
        versions (map :version migrations)]
    (if (= versions (sort versions))
      {:valid true}
      {:valid false :error "Migrations are not in order"})))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-migration-status
  "Get detailed migration status."
  []
  {:current-version (get-current-version)
   :total-migrations (count (list-migrations))
   :applied-count (count (get-applied-migrations))
   :pending-count (count (get-pending-migrations))
   :pending (map #(select-keys % [:version :name]) (get-pending-migrations))
   :last-migration (last (get-migration-history :limit 1))})

;; =============================================================================
;; PREDEFINED MIGRATIONS
;; =============================================================================

(def initial-schema-migration
  (create-migration
   1
   "initial-schema"
   :description "Create initial schema"
   :up (fn []
         (log/info "Creating initial schema")
         ;; Schema creation logic here
         )
   :down (fn []
           (log/info "Dropping initial schema")
           ;; Schema drop logic here
           )))

(def add-indexes-migration
  (create-migration
   2
   "add-indexes"
   :description "Add performance indexes"
   :up (fn []
         (log/info "Adding indexes")
         ;; Index creation logic here
         )
   :down (fn []
           (log/info "Dropping indexes")
           ;; Index drop logic here
           )))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-migration-manager!
  "Initialize migration manager."
  []
  (log/info "Initializing migration manager")
  ;; Register feature flag
  (flags/register-flag! "migration-manager" "Enable migration manager" true)
  ;; Create metrics
  (metrics/create-counter! :migration/executed "Migrations executed")
  (metrics/create-counter! :migration/failed "Migrations failed")
  (metrics/create-gauge! :migration/pending "Pending migrations"
                         #(count (get-pending-migrations)))
  ;; Load history
  (load-history!)
  ;; Register predefined migrations
  (register-migration! initial-schema-migration)
  (register-migration! add-indexes-migration)
  (log/info "Migration manager initialized"))

;; =============================================================================
;; MODULE STATUS
;; =============================================================================

(defn get-manager-status []
  {:enabled (flags/is-enabled? "migration-manager")
   :current-version (get-current-version)
   :total-migrations (count (list-migrations))
   :pending-migrations (count (get-pending-migrations))
   :applied-migrations (count (get-applied-migrations))})
