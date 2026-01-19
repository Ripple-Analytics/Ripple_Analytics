(ns mental-models.pipeline.integration.data-catalog
  "Data catalog for mental model analysis assets.
   
   Features:
   - Asset registration
   - Metadata management
   - Schema discovery
   - Data profiling
   - Lineage tracking
   - Access control
   - Search and discovery
   - Data quality metrics"
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
  (atom {:assets {}           ;; asset-id -> asset
         :schemas {}          ;; schema-id -> schema
         :profiles {}         ;; asset-id -> profile
         :lineage {}          ;; asset-id -> lineage-info
         :tags {}             ;; tag -> [asset-ids]
         :owners {}           ;; owner-id -> [asset-ids]
         :quality-scores {}   ;; asset-id -> quality-score
         :stats {:assets-registered 0 :searches 0}
         :initialized? false}))

;; ============================================================================
;; Asset Management
;; ============================================================================

(defn register-asset!
  "Register a data asset."
  [asset-id config]
  (let [asset {:id asset-id
               :name (get config :name (name asset-id))
               :description (get config :description "")
               :type (get config :type :dataset)
               :format (get config :format :unknown)
               :location (get config :location)
               :owner (get config :owner)
               :tags (get config :tags #{})
               :metadata (get config :metadata {})
               :schema-id (get config :schema-id)
               :status (get config :status :active)
               :created-at (System/currentTimeMillis)
               :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:assets asset-id] asset)
    
    ;; Index by tags
    (doseq [tag (:tags asset)]
      (swap! state update-in [:tags tag] (fnil conj #{}) asset-id))
    
    ;; Index by owner
    (when (:owner asset)
      (swap! state update-in [:owners (:owner asset)] (fnil conj #{}) asset-id))
    
    (swap! state update-in [:stats :assets-registered] inc)
    (logging/log :info "Registered asset" {:asset-id asset-id})
    (events/emit! :asset-registered {:asset-id asset-id})
    asset-id))

(defn get-asset
  "Get an asset."
  [asset-id]
  (get-in @state [:assets asset-id]))

(defn list-assets
  "List assets."
  [& {:keys [type owner tag status limit] :or {limit 100}}]
  (let [assets (vals (:assets @state))
        filtered (cond->> assets
                   type (filter #(= (:type %) type))
                   owner (filter #(= (:owner %) owner))
                   tag (filter #(contains? (:tags %) tag))
                   status (filter #(= (:status %) status))
                   true (take limit))]
    (mapv #(select-keys % [:id :name :type :format :owner :status]) filtered)))

(defn update-asset!
  "Update an asset."
  [asset-id updates]
  (swap! state update-in [:assets asset-id]
         (fn [a]
           (merge a updates {:updated-at (System/currentTimeMillis)}))))

(defn delete-asset!
  "Delete an asset."
  [asset-id]
  (when-let [asset (get-asset asset-id)]
    ;; Remove from tag index
    (doseq [tag (:tags asset)]
      (swap! state update-in [:tags tag] disj asset-id))
    ;; Remove from owner index
    (when (:owner asset)
      (swap! state update-in [:owners (:owner asset)] disj asset-id))
    ;; Remove asset
    (swap! state update :assets dissoc asset-id)
    (swap! state update :profiles dissoc asset-id)
    (swap! state update :lineage dissoc asset-id)
    (swap! state update :quality-scores dissoc asset-id)
    (logging/log :info "Deleted asset" {:asset-id asset-id})))

(defn add-tag!
  "Add a tag to an asset."
  [asset-id tag]
  (swap! state update-in [:assets asset-id :tags] conj tag)
  (swap! state update-in [:tags tag] (fnil conj #{}) asset-id))

(defn remove-tag!
  "Remove a tag from an asset."
  [asset-id tag]
  (swap! state update-in [:assets asset-id :tags] disj tag)
  (swap! state update-in [:tags tag] disj asset-id))

;; ============================================================================
;; Schema Management
;; ============================================================================

(defn register-schema!
  "Register a schema."
  [schema-id config]
  (let [schema {:id schema-id
                :name (get config :name (name schema-id))
                :description (get config :description "")
                :version (get config :version "1.0.0")
                :fields (get config :fields [])
                :primary-key (get config :primary-key)
                :indexes (get config :indexes [])
                :constraints (get config :constraints [])
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schemas schema-id] schema)
    (logging/log :info "Registered schema" {:schema-id schema-id})
    schema-id))

(defn get-schema
  "Get a schema."
  [schema-id]
  (get-in @state [:schemas schema-id]))

(defn list-schemas
  "List all schemas."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :version (:version s)
           :field-count (count (:fields s))})
        (:schemas @state)))

(defn discover-schema
  "Discover schema from sample data."
  [sample-data]
  (when (and (seq sample-data) (map? (first sample-data)))
    (let [fields (mapv (fn [[k v]]
                         {:name (name k)
                          :type (cond
                                  (string? v) :string
                                  (integer? v) :integer
                                  (float? v) :float
                                  (boolean? v) :boolean
                                  (map? v) :object
                                  (coll? v) :array
                                  :else :unknown)
                          :nullable? true})
                       (first sample-data))]
      {:fields fields
       :sample-size (count sample-data)
       :discovered-at (System/currentTimeMillis)})))

;; ============================================================================
;; Data Profiling
;; ============================================================================

(defn profile-asset!
  "Profile a data asset."
  [asset-id sample-data]
  (when (seq sample-data)
    (let [row-count (count sample-data)
          columns (when (map? (first sample-data)) (keys (first sample-data)))
          
          column-profiles (when columns
                            (into {}
                                  (map (fn [col]
                                         (let [values (map #(get % col) sample-data)
                                               non-null (remove nil? values)
                                               unique (distinct non-null)]
                                           [col {:null-count (- row-count (count non-null))
                                                 :null-rate (/ (- row-count (count non-null)) row-count)
                                                 :unique-count (count unique)
                                                 :cardinality (/ (count unique) (max 1 (count non-null)))
                                                 :sample-values (take 5 unique)}]))
                                       columns)))
          
          profile {:asset-id asset-id
                   :row-count row-count
                   :column-count (count columns)
                   :columns column-profiles
                   :profiled-at (System/currentTimeMillis)}]
      
      (swap! state assoc-in [:profiles asset-id] profile)
      (logging/log :info "Profiled asset" {:asset-id asset-id :row-count row-count})
      profile)))

(defn get-profile
  "Get asset profile."
  [asset-id]
  (get-in @state [:profiles asset-id]))

;; ============================================================================
;; Lineage Tracking
;; ============================================================================

(defn set-lineage!
  "Set lineage information for an asset."
  [asset-id lineage-info]
  (let [lineage {:asset-id asset-id
                 :upstream (get lineage-info :upstream [])
                 :downstream (get lineage-info :downstream [])
                 :transformations (get lineage-info :transformations [])
                 :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:lineage asset-id] lineage)
    
    ;; Update downstream references for upstream assets
    (doseq [upstream-id (:upstream lineage)]
      (swap! state update-in [:lineage upstream-id :downstream]
             (fnil conj []) asset-id))
    
    (logging/log :info "Set lineage" {:asset-id asset-id})
    lineage))

(defn get-lineage
  "Get lineage for an asset."
  [asset-id]
  (get-in @state [:lineage asset-id]))

(defn get-upstream
  "Get upstream assets."
  [asset-id & {:keys [depth] :or {depth 1}}]
  (letfn [(get-upstream-recursive [id current-depth]
            (if (> current-depth depth)
              []
              (let [lineage (get-lineage id)
                    upstream (:upstream lineage [])]
                (concat upstream
                        (mapcat #(get-upstream-recursive % (inc current-depth)) upstream)))))]
    (distinct (get-upstream-recursive asset-id 1))))

(defn get-downstream
  "Get downstream assets."
  [asset-id & {:keys [depth] :or {depth 1}}]
  (letfn [(get-downstream-recursive [id current-depth]
            (if (> current-depth depth)
              []
              (let [lineage (get-lineage id)
                    downstream (:downstream lineage [])]
                (concat downstream
                        (mapcat #(get-downstream-recursive % (inc current-depth)) downstream)))))]
    (distinct (get-downstream-recursive asset-id 1))))

(defn get-impact-analysis
  "Analyze impact of changes to an asset."
  [asset-id]
  (let [downstream (get-downstream asset-id :depth 10)]
    {:asset-id asset-id
     :impacted-assets downstream
     :impact-count (count downstream)
     :impact-by-type (frequencies (map #(:type (get-asset %)) downstream))}))

;; ============================================================================
;; Data Quality
;; ============================================================================

(defn calculate-quality-score
  "Calculate data quality score for an asset."
  [asset-id]
  (when-let [profile (get-profile asset-id)]
    (let [;; Completeness: inverse of average null rate
          null-rates (map :null-rate (vals (:columns profile)))
          completeness (if (seq null-rates)
                         (- 1 (/ (reduce + null-rates) (count null-rates)))
                         1.0)
          
          ;; Uniqueness: average cardinality
          cardinalities (map :cardinality (vals (:columns profile)))
          uniqueness (if (seq cardinalities)
                       (/ (reduce + cardinalities) (count cardinalities))
                       1.0)
          
          ;; Overall score (weighted average)
          score (* 0.5 (+ completeness uniqueness))]
      
      (swap! state assoc-in [:quality-scores asset-id]
             {:asset-id asset-id
              :completeness completeness
              :uniqueness uniqueness
              :overall-score score
              :calculated-at (System/currentTimeMillis)})
      
      {:completeness completeness
       :uniqueness uniqueness
       :overall-score score})))

(defn get-quality-score
  "Get quality score for an asset."
  [asset-id]
  (get-in @state [:quality-scores asset-id]))

;; ============================================================================
;; Search and Discovery
;; ============================================================================

(defn search-assets
  "Search assets."
  [query & {:keys [type owner tags limit] :or {limit 50}}]
  (swap! state update-in [:stats :searches] inc)
  (let [assets (vals (:assets @state))
        query-lower (str/lower-case (str query))
        
        ;; Score each asset
        scored (map (fn [asset]
                      (let [name-match (if (str/includes? (str/lower-case (:name asset)) query-lower) 10 0)
                            desc-match (if (str/includes? (str/lower-case (str (:description asset))) query-lower) 5 0)
                            tag-match (if (some #(str/includes? (str/lower-case (str %)) query-lower) (:tags asset)) 3 0)
                            score (+ name-match desc-match tag-match)]
                        (assoc asset :search-score score)))
                    assets)
        
        ;; Filter and sort
        filtered (cond->> scored
                   (pos? (count query)) (filter #(pos? (:search-score %)))
                   type (filter #(= (:type %) type))
                   owner (filter #(= (:owner %) owner))
                   tags (filter #(some (:tags %) tags))
                   true (sort-by :search-score >)
                   limit (take limit))]
    
    (mapv #(select-keys % [:id :name :type :description :tags :search-score]) filtered)))

(defn get-popular-tags
  "Get popular tags."
  [& {:keys [limit] :or {limit 20}}]
  (let [tag-counts (map (fn [[tag assets]]
                          {:tag tag :count (count assets)})
                        (:tags @state))]
    (take limit (sort-by :count > tag-counts))))

(defn get-recent-assets
  "Get recently added/updated assets."
  [& {:keys [limit] :or {limit 10}}]
  (let [assets (vals (:assets @state))]
    (take limit (sort-by :updated-at > assets))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-catalog-stats
  "Get catalog statistics."
  []
  (let [stats (:stats @state)
        assets (vals (:assets @state))
        by-type (frequencies (map :type assets))
        by-status (frequencies (map :status assets))]
    {:total-assets (count assets)
     :total-schemas (count (:schemas @state))
     :total-tags (count (:tags @state))
     :total-owners (count (:owners @state))
     :assets-by-type by-type
     :assets-by-status by-status
     :assets-registered (:assets-registered stats)
     :searches (:searches stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-data-catalog!
  "Initialize the data catalog."
  []
  (when-not (:initialized? @state)
    ;; Register sample schema
    (register-schema! :document-analysis
                      {:name "Document Analysis"
                       :description "Schema for analyzed documents"
                       :fields [{:name "id" :type :string :nullable? false}
                                {:name "content" :type :string :nullable? false}
                                {:name "models" :type :array :nullable? true}
                                {:name "confidence" :type :float :nullable? true}
                                {:name "analyzed_at" :type :timestamp :nullable? false}]
                       :primary-key "id"})
    
    ;; Register sample assets
    (register-asset! :mental-models-dataset
                     {:name "Mental Models Dataset"
                      :description "Core dataset of mental model definitions"
                      :type :dataset
                      :format :edn
                      :owner :system
                      :tags #{:mental-models :core :reference}
                      :schema-id :document-analysis})
    
    (register-asset! :analysis-results
                     {:name "Analysis Results"
                      :description "Results from document analysis"
                      :type :dataset
                      :format :json
                      :owner :system
                      :tags #{:analysis :results :derived}})
    
    ;; Set lineage
    (set-lineage! :analysis-results
                  {:upstream [:mental-models-dataset]
                   :transformations [{:type :analysis :description "Mental model detection"}]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Data catalog initialized")
    (events/emit! :data-catalog-initialized {})
    true))
