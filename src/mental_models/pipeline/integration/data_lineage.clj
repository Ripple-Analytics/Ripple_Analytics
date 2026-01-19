(ns mental-models.pipeline.integration.data-lineage
  "Data lineage tracking for mental model analysis pipeline.
   
   Features:
   - Source tracking for all data
   - Transformation history
   - Data provenance graphs
   - Impact analysis
   - Audit trail generation
   - Lineage visualization data
   - Data quality tracking
   - Compliance reporting"
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
         :datasets {}         ;; dataset-id -> dataset-info
         :transformations {}  ;; transform-id -> transformation
         :lineage-graph {}    ;; node-id -> {:inputs [] :outputs []}
         :quality-records {}  ;; dataset-id -> quality-metrics
         :initialized? false}))

;; ============================================================================
;; Source Registration
;; ============================================================================

(defn register-source!
  "Register a data source."
  [source-id config]
  (let [source {:id source-id
                :name (get config :name (name source-id))
                :type (get config :type :file)
                :location (get config :location "")
                :format (get config :format :text)
                :owner (get config :owner "")
                :description (get config :description "")
                :metadata (get config :metadata {})
                :created-at (System/currentTimeMillis)
                :last-accessed nil
                :access-count 0}]
    (swap! state assoc-in [:sources source-id] source)
    (logging/log :info "Registered data source" {:source-id source-id :type (:type source)})
    (events/emit! :source-registered {:source-id source-id})
    source-id))

(defn get-source
  "Get a data source."
  [source-id]
  (get-in @state [:sources source-id]))

(defn list-sources
  "List all registered sources."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :type (:type s)
           :format (:format s)
           :access-count (:access-count s)})
        (:sources @state)))

(defn record-source-access!
  "Record an access to a data source."
  [source-id & {:keys [user operation]}]
  (swap! state
         (fn [s]
           (-> s
               (assoc-in [:sources source-id :last-accessed] (System/currentTimeMillis))
               (update-in [:sources source-id :access-count] inc))))
  (metrics/increment :source-accesses {:source-id source-id}))

;; ============================================================================
;; Dataset Tracking
;; ============================================================================

(defn create-dataset!
  "Create a tracked dataset."
  [dataset-id config]
  (let [dataset {:id dataset-id
                 :name (get config :name (name dataset-id))
                 :source-id (get config :source-id)
                 :schema (get config :schema {})
                 :row-count (get config :row-count 0)
                 :column-count (get config :column-count 0)
                 :created-at (System/currentTimeMillis)
                 :created-by (get config :created-by "system")
                 :version 1
                 :parent-datasets (get config :parent-datasets [])
                 :transformations-applied []
                 :tags (get config :tags [])
                 :metadata (get config :metadata {})}]
    (swap! state assoc-in [:datasets dataset-id] dataset)
    
    ;; Add to lineage graph
    (swap! state assoc-in [:lineage-graph dataset-id]
           {:type :dataset
            :inputs (vec (:parent-datasets dataset))
            :outputs []})
    
    ;; Update parent outputs
    (doseq [parent-id (:parent-datasets dataset)]
      (swap! state update-in [:lineage-graph parent-id :outputs] conj dataset-id))
    
    (logging/log :info "Created dataset" {:dataset-id dataset-id})
    (events/emit! :dataset-created {:dataset-id dataset-id})
    dataset-id))

(defn get-dataset
  "Get a dataset."
  [dataset-id]
  (get-in @state [:datasets dataset-id]))

(defn list-datasets
  "List all datasets."
  []
  (mapv (fn [[id d]]
          {:id id
           :name (:name d)
           :source-id (:source-id d)
           :version (:version d)
           :row-count (:row-count d)
           :created-at (:created-at d)})
        (:datasets @state)))

(defn update-dataset!
  "Update a dataset (creates new version)."
  [dataset-id updates]
  (swap! state
         (fn [s]
           (-> s
               (update-in [:datasets dataset-id] merge updates)
               (update-in [:datasets dataset-id :version] inc)
               (assoc-in [:datasets dataset-id :updated-at] (System/currentTimeMillis)))))
  (logging/log :info "Updated dataset" {:dataset-id dataset-id}))

;; ============================================================================
;; Transformation Tracking
;; ============================================================================

(defn record-transformation!
  "Record a data transformation."
  [transform-id config]
  (let [transformation {:id transform-id
                        :name (get config :name (name transform-id))
                        :type (get config :type :unknown)
                        :input-datasets (get config :input-datasets [])
                        :output-datasets (get config :output-datasets [])
                        :operation (get config :operation "")
                        :parameters (get config :parameters {})
                        :executed-at (System/currentTimeMillis)
                        :executed-by (get config :executed-by "system")
                        :duration-ms (get config :duration-ms 0)
                        :status (get config :status :completed)
                        :error (get config :error nil)}]
    (swap! state assoc-in [:transformations transform-id] transformation)
    
    ;; Update lineage graph
    (swap! state assoc-in [:lineage-graph transform-id]
           {:type :transformation
            :inputs (vec (:input-datasets transformation))
            :outputs (vec (:output-datasets transformation))})
    
    ;; Update input dataset outputs
    (doseq [input-id (:input-datasets transformation)]
      (swap! state update-in [:lineage-graph input-id :outputs] conj transform-id))
    
    ;; Update output dataset inputs
    (doseq [output-id (:output-datasets transformation)]
      (swap! state update-in [:lineage-graph output-id :inputs] conj transform-id)
      (swap! state update-in [:datasets output-id :transformations-applied] conj transform-id))
    
    (logging/log :info "Recorded transformation" {:transform-id transform-id :type (:type transformation)})
    (events/emit! :transformation-recorded {:transform-id transform-id})
    transform-id))

(defn get-transformation
  "Get a transformation."
  [transform-id]
  (get-in @state [:transformations transform-id]))

(defn list-transformations
  "List all transformations."
  [& {:keys [dataset-id type since limit]}]
  (let [transforms (vals (:transformations @state))
        filtered (cond->> transforms
                   dataset-id (filter #(or (contains? (set (:input-datasets %)) dataset-id)
                                           (contains? (set (:output-datasets %)) dataset-id)))
                   type (filter #(= (:type %) type))
                   since (filter #(>= (:executed-at %) since))
                   true (sort-by :executed-at >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Lineage Graph Operations
;; ============================================================================

(defn get-lineage-node
  "Get a node from the lineage graph."
  [node-id]
  (get-in @state [:lineage-graph node-id]))

(defn get-upstream-lineage
  "Get all upstream nodes (ancestors) for a dataset."
  [dataset-id & {:keys [max-depth] :or {max-depth 10}}]
  (let [visited (atom #{})
        result (atom [])]
    (letfn [(traverse [node-id depth]
              (when (and (not (contains? @visited node-id))
                         (< depth max-depth))
                (swap! visited conj node-id)
                (when-let [node (get-lineage-node node-id)]
                  (swap! result conj {:id node-id
                                      :type (:type node)
                                      :depth depth})
                  (doseq [input-id (:inputs node)]
                    (traverse input-id (inc depth))))))]
      (traverse dataset-id 0)
      @result)))

(defn get-downstream-lineage
  "Get all downstream nodes (descendants) for a dataset."
  [dataset-id & {:keys [max-depth] :or {max-depth 10}}]
  (let [visited (atom #{})
        result (atom [])]
    (letfn [(traverse [node-id depth]
              (when (and (not (contains? @visited node-id))
                         (< depth max-depth))
                (swap! visited conj node-id)
                (when-let [node (get-lineage-node node-id)]
                  (swap! result conj {:id node-id
                                      :type (:type node)
                                      :depth depth})
                  (doseq [output-id (:outputs node)]
                    (traverse output-id (inc depth))))))]
      (traverse dataset-id 0)
      @result)))

(defn get-full-lineage
  "Get complete lineage (both upstream and downstream)."
  [dataset-id]
  {:dataset-id dataset-id
   :upstream (get-upstream-lineage dataset-id)
   :downstream (get-downstream-lineage dataset-id)})

(defn find-common-ancestors
  "Find common ancestors between two datasets."
  [dataset-id-1 dataset-id-2]
  (let [ancestors-1 (set (map :id (get-upstream-lineage dataset-id-1)))
        ancestors-2 (set (map :id (get-upstream-lineage dataset-id-2)))]
    (vec (clojure.set/intersection ancestors-1 ancestors-2))))

;; ============================================================================
;; Impact Analysis
;; ============================================================================

(defn analyze-impact
  "Analyze the impact of changes to a dataset."
  [dataset-id & {:keys [change-type]}]
  (let [downstream (get-downstream-lineage dataset-id)
        affected-datasets (filter #(= (:type %) :dataset) downstream)
        affected-transforms (filter #(= (:type %) :transformation) downstream)]
    {:source-dataset dataset-id
     :change-type (or change-type :modification)
     :affected-datasets (mapv :id affected-datasets)
     :affected-transformations (mapv :id affected-transforms)
     :total-affected (count downstream)
     :impact-level (cond
                     (> (count affected-datasets) 10) :high
                     (> (count affected-datasets) 5) :medium
                     :else :low)
     :analyzed-at (System/currentTimeMillis)}))

(defn get-critical-paths
  "Identify critical paths in the lineage graph."
  []
  (let [datasets (:datasets @state)
        ;; Find datasets with most downstream dependencies
        impact-scores (for [[id _] datasets]
                        {:id id
                         :downstream-count (count (get-downstream-lineage id))
                         :upstream-count (count (get-upstream-lineage id))})]
    (->> impact-scores
         (sort-by :downstream-count >)
         (take 10)
         vec)))

;; ============================================================================
;; Data Quality Tracking
;; ============================================================================

(defn record-quality-metrics!
  "Record quality metrics for a dataset."
  [dataset-id metrics]
  (let [quality-record {:dataset-id dataset-id
                        :completeness (get metrics :completeness 1.0)
                        :accuracy (get metrics :accuracy 1.0)
                        :consistency (get metrics :consistency 1.0)
                        :timeliness (get metrics :timeliness 1.0)
                        :validity (get metrics :validity 1.0)
                        :uniqueness (get metrics :uniqueness 1.0)
                        :null-percentage (get metrics :null-percentage 0.0)
                        :duplicate-percentage (get metrics :duplicate-percentage 0.0)
                        :overall-score (/ (+ (get metrics :completeness 1.0)
                                             (get metrics :accuracy 1.0)
                                             (get metrics :consistency 1.0)
                                             (get metrics :timeliness 1.0)
                                             (get metrics :validity 1.0)
                                             (get metrics :uniqueness 1.0))
                                          6.0)
                        :recorded-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:quality-records dataset-id] quality-record)
    (metrics/gauge :data-quality-score {:dataset-id dataset-id} (:overall-score quality-record))
    (logging/log :info "Recorded quality metrics" {:dataset-id dataset-id :score (:overall-score quality-record)})
    quality-record))

(defn get-quality-metrics
  "Get quality metrics for a dataset."
  [dataset-id]
  (get-in @state [:quality-records dataset-id]))

(defn get-quality-trend
  "Get quality trend for a dataset (placeholder for historical data)."
  [dataset-id]
  (when-let [current (get-quality-metrics dataset-id)]
    {:current current
     :trend :stable
     :history [current]}))

;; ============================================================================
;; Audit Trail
;; ============================================================================

(defn generate-audit-trail
  "Generate audit trail for a dataset."
  [dataset-id]
  (let [dataset (get-dataset dataset-id)
        upstream (get-upstream-lineage dataset-id)
        transforms (list-transformations :dataset-id dataset-id)]
    {:dataset-id dataset-id
     :dataset-name (:name dataset)
     :created-at (:created-at dataset)
     :created-by (:created-by dataset)
     :current-version (:version dataset)
     :source-chain (mapv (fn [node]
                           (case (:type node)
                             :dataset (select-keys (get-dataset (:id node))
                                                   [:id :name :source-id :created-at])
                             :transformation (select-keys (get-transformation (:id node))
                                                          [:id :name :type :executed-at])
                             {:id (:id node) :type (:type node)}))
                         upstream)
     :transformations-applied (mapv #(select-keys % [:id :name :type :executed-at :executed-by])
                                    transforms)
     :quality-metrics (get-quality-metrics dataset-id)
     :generated-at (System/currentTimeMillis)}))

(defn export-audit-trail
  "Export audit trail in a specific format."
  [dataset-id format]
  (let [trail (generate-audit-trail dataset-id)]
    (case format
      :edn (pr-str trail)
      :json trail
      :text (str "Audit Trail for Dataset: " (:dataset-name trail) "\n"
                 "Created: " (:created-at trail) "\n"
                 "Version: " (:current-version trail) "\n"
                 "Transformations: " (count (:transformations-applied trail)) "\n")
      trail)))

;; ============================================================================
;; Visualization Data
;; ============================================================================

(defn get-lineage-visualization-data
  "Get data formatted for lineage visualization."
  [dataset-id & {:keys [include-upstream? include-downstream?]
                  :or {include-upstream? true include-downstream? true}}]
  (let [upstream (when include-upstream? (get-upstream-lineage dataset-id))
        downstream (when include-downstream? (get-downstream-lineage dataset-id))
        all-nodes (distinct (concat [{:id dataset-id :type :dataset :depth 0}]
                                    upstream
                                    downstream))
        
        ;; Build nodes for visualization
        nodes (mapv (fn [node]
                      (let [details (case (:type node)
                                      :dataset (get-dataset (:id node))
                                      :transformation (get-transformation (:id node))
                                      {})]
                        {:id (:id node)
                         :type (:type node)
                         :label (or (:name details) (str (:id node)))
                         :depth (:depth node)
                         :metadata details}))
                    all-nodes)
        
        ;; Build edges
        edges (for [node all-nodes
                    :let [lineage-node (get-lineage-node (:id node))]
                    output-id (:outputs lineage-node)]
                {:source (:id node)
                 :target output-id
                 :type :data-flow})]
    
    {:nodes nodes
     :edges (vec edges)
     :root-id dataset-id
     :total-nodes (count nodes)
     :total-edges (count edges)}))

;; ============================================================================
;; Compliance Reporting
;; ============================================================================

(defn generate-compliance-report
  "Generate a compliance report for data lineage."
  [& {:keys [datasets regulation]}]
  (let [target-datasets (or datasets (keys (:datasets @state)))
        reports (for [ds-id target-datasets]
                  (let [trail (generate-audit-trail ds-id)
                        quality (get-quality-metrics ds-id)]
                    {:dataset-id ds-id
                     :has-source? (some? (:source-id (get-dataset ds-id)))
                     :has-audit-trail? (seq (:source-chain trail))
                     :quality-score (:overall-score quality)
                     :meets-quality-threshold? (>= (or (:overall-score quality) 0) 0.8)
                     :transformations-documented? (every? :name (:transformations-applied trail))}))]
    {:regulation (or regulation :general)
     :generated-at (System/currentTimeMillis)
     :datasets-reviewed (count target-datasets)
     :compliant-datasets (count (filter #(and (:has-source? %)
                                              (:has-audit-trail? %)
                                              (:meets-quality-threshold? %))
                                        reports))
     :non-compliant-datasets (filterv #(not (and (:has-source? %)
                                                  (:has-audit-trail? %)
                                                  (:meets-quality-threshold? %)))
                                       reports)
     :details reports}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-lineage-stats
  "Get data lineage statistics."
  []
  {:total-sources (count (:sources @state))
   :total-datasets (count (:datasets @state))
   :total-transformations (count (:transformations @state))
   :lineage-nodes (count (:lineage-graph @state))
   :quality-records (count (:quality-records @state))
   :avg-quality-score (let [scores (map :overall-score (vals (:quality-records @state)))]
                        (if (seq scores)
                          (/ (reduce + scores) (count scores))
                          0.0))
   :critical-paths (get-critical-paths)})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-data-lineage!
  "Initialize data lineage tracking."
  []
  (when-not (:initialized? @state)
    ;; Register sample sources
    (register-source! :local-files
                      {:name "Local File System"
                       :type :file
                       :location "/data"
                       :format :mixed})
    
    (register-source! :lm-studio
                      {:name "LM Studio API"
                       :type :api
                       :location "http://localhost:1234"
                       :format :json})
    
    ;; Create sample dataset
    (create-dataset! :raw-documents
                     {:name "Raw Documents"
                      :source-id :local-files
                      :schema {:text :string :filename :string :timestamp :long}
                      :tags [:input :raw]})
    
    (create-dataset! :analyzed-documents
                     {:name "Analyzed Documents"
                      :source-id :lm-studio
                      :parent-datasets [:raw-documents]
                      :schema {:text :string :models :vector :confidence :double}
                      :tags [:processed :analysis]})
    
    ;; Record sample transformation
    (record-transformation! :initial-analysis
                            {:name "Initial Mental Model Analysis"
                             :type :analysis
                             :input-datasets [:raw-documents]
                             :output-datasets [:analyzed-documents]
                             :operation "mental-model-detection"})
    
    ;; Record quality metrics
    (record-quality-metrics! :raw-documents
                             {:completeness 0.95
                              :accuracy 1.0
                              :consistency 0.98})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Data lineage tracking initialized")
    (events/emit! :data-lineage-initialized {})
    true))
