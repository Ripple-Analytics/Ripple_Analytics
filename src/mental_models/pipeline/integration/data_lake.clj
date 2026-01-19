(ns mental-models.pipeline.integration.data-lake
  "Data lake for mental model analysis data storage.
   
   Features:
   - Multi-format data storage
   - Data partitioning
   - Schema evolution
   - Data cataloging
   - Query optimization
   - Data lifecycle management
   - Compression and encoding
   - Access control"
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
           [java.time Instant LocalDate]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:tables {}           ;; table-id -> table-metadata
         :partitions {}       ;; table-id -> partitions
         :data {}             ;; {table-id partition-key} -> data
         :schemas {}          ;; table-id -> schema-versions
         :catalog {}          ;; catalog entries
         :access-policies {}  ;; policy-id -> access-policy
         :lifecycle-rules {}  ;; rule-id -> lifecycle-rule
         :stats {:reads 0 :writes 0 :bytes-stored 0}
         :initialized? false}))

;; ============================================================================
;; Table Management
;; ============================================================================

(defn create-table!
  "Create a data lake table."
  [table-id config]
  (let [table {:id table-id
               :name (get config :name (name table-id))
               :description (get config :description "")
               :format (get config :format :parquet)
               :schema (get config :schema {})
               :partition-keys (get config :partition-keys [])
               :sort-keys (get config :sort-keys [])
               :compression (get config :compression :snappy)
               :location (get config :location (str "/data/lake/" (name table-id)))
               :properties (get config :properties {})
               :created-at (System/currentTimeMillis)
               :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:tables table-id] table)
    (swap! state assoc-in [:partitions table-id] {})
    (swap! state assoc-in [:schemas table-id] [{:version 1
                                                 :schema (:schema table)
                                                 :created-at (System/currentTimeMillis)}])
    (logging/log :info "Created table" {:table-id table-id})
    (events/emit! :table-created {:table-id table-id})
    table-id))

(defn get-table
  "Get a table."
  [table-id]
  (get-in @state [:tables table-id]))

(defn list-tables
  "List all tables."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :format (:format t)
           :partition-keys (:partition-keys t)})
        (:tables @state)))

(defn update-table!
  "Update table metadata."
  [table-id updates]
  (swap! state update-in [:tables table-id]
         (fn [t]
           (-> t
               (merge updates)
               (assoc :updated-at (System/currentTimeMillis))))))

(defn drop-table!
  "Drop a table."
  [table-id]
  (swap! state update :tables dissoc table-id)
  (swap! state update :partitions dissoc table-id)
  (swap! state update :schemas dissoc table-id)
  (logging/log :info "Dropped table" {:table-id table-id})
  (events/emit! :table-dropped {:table-id table-id}))

;; ============================================================================
;; Schema Management
;; ============================================================================

(defn get-schema
  "Get current schema for a table."
  [table-id]
  (let [versions (get-in @state [:schemas table-id] [])]
    (:schema (last versions))))

(defn get-schema-history
  "Get schema version history."
  [table-id]
  (get-in @state [:schemas table-id] []))

(defn evolve-schema!
  "Evolve table schema (add columns, etc.)."
  [table-id new-schema & {:keys [migration-fn]}]
  (let [current-versions (get-in @state [:schemas table-id] [])
        new-version (inc (count current-versions))]
    (swap! state update-in [:schemas table-id]
           conj {:version new-version
                 :schema new-schema
                 :created-at (System/currentTimeMillis)})
    (swap! state assoc-in [:tables table-id :schema] new-schema)
    (logging/log :info "Evolved schema" {:table-id table-id :version new-version})
    new-version))

;; ============================================================================
;; Partitioning
;; ============================================================================

(defn- compute-partition-key
  "Compute partition key from record."
  [record partition-keys]
  (if (empty? partition-keys)
    "default"
    (str/join "/" (map #(str (name %) "=" (get record %)) partition-keys))))

(defn get-partitions
  "Get partitions for a table."
  [table-id]
  (keys (get-in @state [:partitions table-id] {})))

(defn get-partition-stats
  "Get statistics for a partition."
  [table-id partition-key]
  (let [data-key [table-id partition-key]
        data (get-in @state [:data data-key] [])]
    {:partition-key partition-key
     :record-count (count data)
     :size-estimate (* (count data) 100) ;; Rough estimate
     :created-at (get-in @state [:partitions table-id partition-key :created-at])}))

;; ============================================================================
;; Data Operations
;; ============================================================================

(defn write-records!
  "Write records to a table."
  [table-id records]
  (when (flags/enabled? :data-lake)
    (let [table (get-table table-id)
          partition-keys (:partition-keys table)]
      (doseq [record records]
        (let [partition-key (compute-partition-key record partition-keys)
              data-key [table-id partition-key]]
          ;; Create partition if needed
          (when-not (get-in @state [:partitions table-id partition-key])
            (swap! state assoc-in [:partitions table-id partition-key]
                   {:created-at (System/currentTimeMillis)
                    :updated-at (System/currentTimeMillis)}))
          ;; Append record
          (swap! state update-in [:data data-key] (fnil conj []) record)
          (swap! state assoc-in [:partitions table-id partition-key :updated-at]
                 (System/currentTimeMillis))))
      (swap! state update-in [:stats :writes] + (count records))
      (swap! state update-in [:stats :bytes-stored] + (* (count records) 100))
      (metrics/increment :lake-writes {:table-id table-id} (count records))
      (logging/log :debug "Wrote records" {:table-id table-id :count (count records)})
      (count records))))

(defn read-records
  "Read records from a table."
  [table-id & {:keys [partition-key filter-fn limit]}]
  (when (flags/enabled? :data-lake)
    (let [partitions (if partition-key
                       [partition-key]
                       (get-partitions table-id))
          all-records (mapcat (fn [pk]
                                (get-in @state [:data [table-id pk]] []))
                              partitions)
          filtered (if filter-fn
                     (filter filter-fn all-records)
                     all-records)
          limited (if limit
                    (take limit filtered)
                    filtered)]
      (swap! state update-in [:stats :reads] inc)
      (metrics/increment :lake-reads {:table-id table-id})
      (vec limited))))

(defn query-table
  "Query a table with SQL-like operations."
  [table-id query]
  (let [{:keys [select where order-by limit offset]} query
        records (read-records table-id)
        ;; Apply where clause
        filtered (if where
                   (filter where records)
                   records)
        ;; Apply ordering
        sorted (if order-by
                 (sort-by (first order-by)
                          (if (= :desc (second order-by)) > <)
                          filtered)
                 filtered)
        ;; Apply offset
        offset-applied (if offset
                         (drop offset sorted)
                         sorted)
        ;; Apply limit
        limited (if limit
                  (take limit offset-applied)
                  offset-applied)
        ;; Apply select
        projected (if select
                    (map #(select-keys % select) limited)
                    limited)]
    {:records (vec projected)
     :count (count projected)
     :total (count filtered)}))

(defn delete-records!
  "Delete records matching a predicate."
  [table-id predicate]
  (let [partitions (get-partitions table-id)
        deleted-count (atom 0)]
    (doseq [pk partitions]
      (let [data-key [table-id pk]
            records (get-in @state [:data data-key] [])
            remaining (filterv #(not (predicate %)) records)
            deleted (- (count records) (count remaining))]
        (swap! deleted-count + deleted)
        (swap! state assoc-in [:data data-key] remaining)))
    (logging/log :info "Deleted records" {:table-id table-id :count @deleted-count})
    @deleted-count))

;; ============================================================================
;; Data Catalog
;; ============================================================================

(defn register-catalog-entry!
  "Register a catalog entry."
  [entry-id config]
  (let [entry {:id entry-id
               :name (get config :name (name entry-id))
               :type (get config :type :table)
               :table-id (get config :table-id)
               :description (get config :description "")
               :tags (get config :tags [])
               :owner (get config :owner "system")
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:catalog entry-id] entry)
    entry-id))

(defn search-catalog
  "Search the data catalog."
  [query]
  (let [entries (vals (:catalog @state))
        matches (filter (fn [e]
                          (or (str/includes? (str/lower-case (:name e ""))
                                             (str/lower-case query))
                              (str/includes? (str/lower-case (:description e ""))
                                             (str/lower-case query))
                              (some #(str/includes? (str/lower-case %)
                                                    (str/lower-case query))
                                    (:tags e []))))
                        entries)]
    (vec matches)))

(defn get-catalog-entry
  "Get a catalog entry."
  [entry-id]
  (get-in @state [:catalog entry-id]))

;; ============================================================================
;; Access Control
;; ============================================================================

(defn create-access-policy!
  "Create an access policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :table-id (get config :table-id)
                :principals (get config :principals [])
                :permissions (get config :permissions [:read])
                :row-filter (get config :row-filter nil)
                :column-mask (get config :column-mask {})
                :enabled? (get config :enabled? true)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:access-policies policy-id] policy)
    policy-id))

(defn check-access
  "Check if a principal has access to a table."
  [principal table-id permission]
  (let [policies (filter (fn [[_ p]]
                           (and (:enabled? p)
                                (= (:table-id p) table-id)
                                (contains? (set (:principals p)) principal)
                                (contains? (set (:permissions p)) permission)))
                         (:access-policies @state))]
    (seq policies)))

;; ============================================================================
;; Lifecycle Management
;; ============================================================================

(defn create-lifecycle-rule!
  "Create a data lifecycle rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :table-id (get config :table-id)
              :action (get config :action :delete)
              :condition (get config :condition)
              :retention-days (get config :retention-days 365)
              :enabled? (get config :enabled? true)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:lifecycle-rules rule-id] rule)
    rule-id))

(defn apply-lifecycle-rules!
  "Apply lifecycle rules to tables."
  []
  (let [now (System/currentTimeMillis)
        rules (filter #(:enabled? (val %)) (:lifecycle-rules @state))]
    (doseq [[rule-id rule] rules]
      (let [table-id (:table-id rule)
            retention-ms (* (:retention-days rule) 24 60 60 1000)
            cutoff (- now retention-ms)]
        (when (= :delete (:action rule))
          (let [deleted (delete-records! table-id
                                         #(< (get % :created-at 0) cutoff))]
            (when (pos? deleted)
              (logging/log :info "Applied lifecycle rule"
                           {:rule-id rule-id :deleted deleted}))))))))

;; ============================================================================
;; Compaction
;; ============================================================================

(defn compact-partition!
  "Compact a partition (merge small files)."
  [table-id partition-key]
  (logging/log :info "Compacting partition" {:table-id table-id :partition-key partition-key})
  ;; In production, would merge small files into larger ones
  (swap! state assoc-in [:partitions table-id partition-key :last-compaction]
         (System/currentTimeMillis)))

(defn compact-table!
  "Compact all partitions in a table."
  [table-id]
  (let [partitions (get-partitions table-id)]
    (doseq [pk partitions]
      (compact-partition! table-id pk))
    (count partitions)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-lake-stats
  "Get data lake statistics."
  []
  (let [stats (:stats @state)
        tables (vals (:tables @state))
        total-partitions (reduce + (map #(count (get-partitions (:id %))) tables))
        total-records (reduce + (map #(count (read-records (:id %))) tables))]
    {:total-tables (count tables)
     :total-partitions total-partitions
     :total-records total-records
     :total-reads (:reads stats)
     :total-writes (:writes stats)
     :bytes-stored (:bytes-stored stats)
     :catalog-entries (count (:catalog @state))
     :access-policies (count (:access-policies @state))
     :lifecycle-rules (count (:lifecycle-rules @state))}))

(defn get-table-stats
  "Get statistics for a specific table."
  [table-id]
  (let [table (get-table table-id)
        partitions (get-partitions table-id)
        records (read-records table-id)]
    {:table-id table-id
     :name (:name table)
     :format (:format table)
     :partition-count (count partitions)
     :record-count (count records)
     :schema-versions (count (get-schema-history table-id))
     :created-at (:created-at table)
     :updated-at (:updated-at table)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-data-lake!
  "Initialize the data lake."
  []
  (when-not (:initialized? @state)
    ;; Create default tables
    (create-table! :documents
                   {:name "Documents"
                    :description "Analyzed documents"
                    :format :parquet
                    :schema {:id :string
                             :content :string
                             :analyzed-at :timestamp
                             :models :array}
                    :partition-keys [:year :month]})
    
    (create-table! :analysis-results
                   {:name "Analysis Results"
                    :description "Mental model analysis results"
                    :format :parquet
                    :schema {:id :string
                             :document-id :string
                             :model-id :string
                             :confidence :float
                             :created-at :timestamp}
                    :partition-keys [:model-id]})
    
    (create-table! :events
                   {:name "Events"
                    :description "System events"
                    :format :parquet
                    :schema {:id :string
                             :type :string
                             :data :map
                             :timestamp :timestamp}
                    :partition-keys [:date]})
    
    ;; Register catalog entries
    (register-catalog-entry! :documents-catalog
                             {:name "Documents Table"
                              :type :table
                              :table-id :documents
                              :description "All analyzed documents"
                              :tags ["documents" "analysis"]})
    
    ;; Create default lifecycle rule
    (create-lifecycle-rule! :events-retention
                            {:name "Events Retention"
                             :table-id :events
                             :action :delete
                             :retention-days 90})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Data lake initialized")
    (events/emit! :data-lake-initialized {})
    true))
