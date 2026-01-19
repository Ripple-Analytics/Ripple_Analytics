(ns mental-models.pipeline.integration.feature-store
  "Feature store for machine learning features in mental model analysis.
   
   Features:
   - Feature registration and versioning
   - Online and offline feature serving
   - Feature transformation pipelines
   - Point-in-time correct joins
   - Feature monitoring and drift detection
   - Feature lineage tracking
   - Feature groups and entities
   - Materialization scheduling"
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
  (atom {:entities {}         ;; entity-id -> entity-definition
         :feature-groups {}   ;; group-id -> feature-group
         :features {}         ;; feature-id -> feature-definition
         :feature-values {}   ;; {entity-id feature-id} -> values
         :transformations {}  ;; transform-id -> transformation
         :materializations {} ;; materialization-id -> schedule
         :drift-monitors {}   ;; monitor-id -> drift-config
         :stats {:features-served 0 :features-written 0}
         :initialized? false}))

;; ============================================================================
;; Entity Management
;; ============================================================================

(defn register-entity!
  "Register an entity type."
  [entity-id config]
  (let [entity {:id entity-id
                :name (get config :name (name entity-id))
                :description (get config :description "")
                :join-keys (get config :join-keys [:id])
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:entities entity-id] entity)
    (logging/log :info "Registered entity" {:entity-id entity-id})
    entity-id))

(defn get-entity
  "Get an entity definition."
  [entity-id]
  (get-in @state [:entities entity-id]))

(defn list-entities
  "List all entities."
  []
  (mapv (fn [[id e]]
          {:id id
           :name (:name e)
           :join-keys (:join-keys e)})
        (:entities @state)))

;; ============================================================================
;; Feature Group Management
;; ============================================================================

(defn create-feature-group!
  "Create a feature group."
  [group-id config]
  (let [group {:id group-id
               :name (get config :name (name group-id))
               :description (get config :description "")
               :entity-id (get config :entity-id)
               :features []
               :online-enabled? (get config :online-enabled? true)
               :offline-enabled? (get config :offline-enabled? true)
               :ttl-ms (get config :ttl-ms nil)
               :tags (get config :tags [])
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:feature-groups group-id] group)
    (logging/log :info "Created feature group" {:group-id group-id})
    (events/emit! :feature-group-created {:group-id group-id})
    group-id))

(defn get-feature-group
  "Get a feature group."
  [group-id]
  (get-in @state [:feature-groups group-id]))

(defn list-feature-groups
  "List all feature groups."
  []
  (mapv (fn [[id g]]
          {:id id
           :name (:name g)
           :entity-id (:entity-id g)
           :feature-count (count (:features g))})
        (:feature-groups @state)))

;; ============================================================================
;; Feature Definition
;; ============================================================================

(defn register-feature!
  "Register a feature."
  [feature-id config]
  (let [feature {:id feature-id
                 :name (get config :name (name feature-id))
                 :description (get config :description "")
                 :group-id (get config :group-id)
                 :dtype (get config :dtype :float)
                 :default-value (get config :default-value nil)
                 :transformation (get config :transformation nil)
                 :version (get config :version 1)
                 :tags (get config :tags [])
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:features feature-id] feature)
    ;; Add to feature group
    (when-let [group-id (:group-id feature)]
      (swap! state update-in [:feature-groups group-id :features] conj feature-id))
    (logging/log :info "Registered feature" {:feature-id feature-id})
    (events/emit! :feature-registered {:feature-id feature-id})
    feature-id))

(defn get-feature
  "Get a feature definition."
  [feature-id]
  (get-in @state [:features feature-id]))

(defn list-features
  "List all features."
  [& {:keys [group-id tag]}]
  (let [features (vals (:features @state))
        filtered (cond->> features
                   group-id (filter #(= (:group-id %) group-id))
                   tag (filter #(contains? (set (:tags %)) tag)))]
    (mapv #(select-keys % [:id :name :group-id :dtype :version]) filtered)))

(defn update-feature!
  "Update a feature definition."
  [feature-id updates]
  (swap! state update-in [:features feature-id]
         (fn [f]
           (-> f
               (merge updates)
               (update :version inc)))))

;; ============================================================================
;; Feature Values (Online Store)
;; ============================================================================

(defn write-feature!
  "Write a feature value to the online store."
  [entity-key feature-id value & {:keys [timestamp]}]
  (when (flags/enabled? :feature-store)
    (let [ts (or timestamp (System/currentTimeMillis))
          key [entity-key feature-id]
          entry {:value value
                 :timestamp ts
                 :written-at (System/currentTimeMillis)}]
      (swap! state assoc-in [:feature-values key] entry)
      (swap! state update-in [:stats :features-written] inc)
      (metrics/increment :features-written {:feature-id feature-id})
      entry)))

(defn write-features!
  "Write multiple feature values."
  [entity-key features & {:keys [timestamp]}]
  (doseq [[feature-id value] features]
    (write-feature! entity-key feature-id value :timestamp timestamp)))

(defn get-feature-value
  "Get a feature value from the online store."
  [entity-key feature-id]
  (when (flags/enabled? :feature-store)
    (let [key [entity-key feature-id]
          entry (get-in @state [:feature-values key])
          feature (get-feature feature-id)]
      (swap! state update-in [:stats :features-served] inc)
      (metrics/increment :features-served {:feature-id feature-id})
      (if entry
        (:value entry)
        (:default-value feature)))))

(defn get-feature-values
  "Get multiple feature values for an entity."
  [entity-key feature-ids]
  (into {}
        (map (fn [fid]
               [fid (get-feature-value entity-key fid)])
             feature-ids)))

(defn get-feature-vector
  "Get a feature vector for an entity."
  [entity-key feature-ids]
  (mapv #(get-feature-value entity-key %) feature-ids))

;; ============================================================================
;; Point-in-Time Joins (Offline Store)
;; ============================================================================

(defn- get-feature-at-time
  "Get feature value at a specific point in time."
  [entity-key feature-id timestamp]
  (let [key [entity-key feature-id]
        entry (get-in @state [:feature-values key])]
    (when (and entry (<= (:timestamp entry) timestamp))
      (:value entry))))

(defn get-historical-features
  "Get historical feature values for point-in-time correct joins."
  [entity-key feature-ids timestamp]
  (into {}
        (map (fn [fid]
               [fid (get-feature-at-time entity-key fid timestamp)])
             feature-ids)))

(defn create-training-dataset
  "Create a training dataset with point-in-time correct features."
  [entity-timestamps feature-ids]
  (mapv (fn [[entity-key timestamp]]
          {:entity-key entity-key
           :timestamp timestamp
           :features (get-historical-features entity-key feature-ids timestamp)})
        entity-timestamps))

;; ============================================================================
;; Feature Transformations
;; ============================================================================

(defn register-transformation!
  "Register a feature transformation."
  [transform-id config]
  (let [transform {:id transform-id
                   :name (get config :name (name transform-id))
                   :type (get config :type :python)
                   :source-features (get config :source-features [])
                   :target-feature (get config :target-feature)
                   :transform-fn (get config :transform-fn identity)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:transformations transform-id] transform)
    (logging/log :info "Registered transformation" {:transform-id transform-id})
    transform-id))

(defn apply-transformation
  "Apply a transformation to compute a derived feature."
  [transform-id entity-key]
  (when-let [transform (get-in @state [:transformations transform-id])]
    (let [source-values (get-feature-values entity-key (:source-features transform))
          transform-fn (:transform-fn transform)
          result (transform-fn source-values)]
      (write-feature! entity-key (:target-feature transform) result)
      result)))

(defn list-transformations
  "List all transformations."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :source-features (:source-features t)
           :target-feature (:target-feature t)})
        (:transformations @state)))

;; ============================================================================
;; Feature Materialization
;; ============================================================================

(defn schedule-materialization!
  "Schedule feature materialization."
  [materialization-id config]
  (let [materialization {:id materialization-id
                         :name (get config :name (name materialization-id))
                         :feature-group-id (get config :feature-group-id)
                         :schedule (get config :schedule "0 * * * *") ;; Cron expression
                         :enabled? (get config :enabled? true)
                         :last-run nil
                         :next-run nil
                         :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:materializations materialization-id] materialization)
    (logging/log :info "Scheduled materialization" {:materialization-id materialization-id})
    materialization-id))

(defn run-materialization!
  "Run a materialization job."
  [materialization-id]
  (when-let [mat (get-in @state [:materializations materialization-id])]
    (logging/log :info "Running materialization" {:materialization-id materialization-id})
    (swap! state assoc-in [:materializations materialization-id :last-run] (System/currentTimeMillis))
    (events/emit! :materialization-completed {:materialization-id materialization-id})))

;; ============================================================================
;; Feature Drift Detection
;; ============================================================================

(defn register-drift-monitor!
  "Register a feature drift monitor."
  [monitor-id config]
  (let [monitor {:id monitor-id
                 :feature-id (get config :feature-id)
                 :baseline-stats (get config :baseline-stats {})
                 :threshold (get config :threshold 0.1)
                 :window-size (get config :window-size 1000)
                 :enabled? (get config :enabled? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:drift-monitors monitor-id] monitor)
    (logging/log :info "Registered drift monitor" {:monitor-id monitor-id})
    monitor-id))

(defn- calculate-drift-score
  "Calculate drift score between baseline and current distribution."
  [baseline-stats current-stats]
  (let [baseline-mean (get baseline-stats :mean 0)
        current-mean (get current-stats :mean 0)
        baseline-std (get baseline-stats :std 1)]
    (if (pos? baseline-std)
      (Math/abs (/ (- current-mean baseline-mean) baseline-std))
      0.0)))

(defn check-drift
  "Check for feature drift."
  [monitor-id current-stats]
  (when-let [monitor (get-in @state [:drift-monitors monitor-id])]
    (let [drift-score (calculate-drift-score (:baseline-stats monitor) current-stats)
          drifted? (> drift-score (:threshold monitor))]
      (when drifted?
        (logging/log :warn "Feature drift detected" {:monitor-id monitor-id :drift-score drift-score})
        (events/emit! :feature-drift-detected {:monitor-id monitor-id :drift-score drift-score}))
      {:monitor-id monitor-id
       :feature-id (:feature-id monitor)
       :drift-score drift-score
       :drifted? drifted?
       :threshold (:threshold monitor)})))

;; ============================================================================
;; Feature Lineage
;; ============================================================================

(defn get-feature-lineage
  "Get the lineage of a feature."
  [feature-id]
  (let [feature (get-feature feature-id)
        ;; Find transformations that produce this feature
        producing-transforms (filter #(= (:target-feature (val %)) feature-id)
                                     (:transformations @state))
        ;; Get source features
        source-features (mapcat #(:source-features (val %)) producing-transforms)]
    {:feature-id feature-id
     :version (:version feature)
     :group-id (:group-id feature)
     :transformations (mapv key producing-transforms)
     :source-features (vec (distinct source-features))
     :created-at (:created-at feature)}))

(defn get-feature-impact
  "Get features that depend on a given feature."
  [feature-id]
  (let [dependent-transforms (filter #(contains? (set (:source-features (val %))) feature-id)
                                     (:transformations @state))
        dependent-features (mapv #(:target-feature (val %)) dependent-transforms)]
    {:feature-id feature-id
     :dependent-features dependent-features
     :impact-count (count dependent-features)}))

;; ============================================================================
;; Feature Statistics
;; ============================================================================

(defn compute-feature-stats
  "Compute statistics for a feature."
  [feature-id]
  (let [values (for [[[ek fid] entry] (:feature-values @state)
                     :when (= fid feature-id)]
                 (:value entry))
        numeric-values (filter number? values)]
    (when (seq numeric-values)
      {:feature-id feature-id
       :count (count numeric-values)
       :mean (/ (reduce + numeric-values) (count numeric-values))
       :min (apply min numeric-values)
       :max (apply max numeric-values)
       :std (let [mean (/ (reduce + numeric-values) (count numeric-values))
                  variance (/ (reduce + (map #(Math/pow (- % mean) 2) numeric-values))
                              (count numeric-values))]
              (Math/sqrt variance))
       :computed-at (System/currentTimeMillis)})))

;; ============================================================================
;; Store Statistics
;; ============================================================================

(defn get-store-stats
  "Get feature store statistics."
  []
  (let [stats (:stats @state)]
    {:total-entities (count (:entities @state))
     :total-feature-groups (count (:feature-groups @state))
     :total-features (count (:features @state))
     :total-feature-values (count (:feature-values @state))
     :total-transformations (count (:transformations @state))
     :total-materializations (count (:materializations @state))
     :total-drift-monitors (count (:drift-monitors @state))
     :features-served (:features-served stats)
     :features-written (:features-written stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-feature-store!
  "Initialize the feature store."
  []
  (when-not (:initialized? @state)
    ;; Register default entities
    (register-entity! :document
                      {:name "Document"
                       :description "Document entity for analysis"
                       :join-keys [:id]})
    
    (register-entity! :user
                      {:name "User"
                       :description "User entity"
                       :join-keys [:id]})
    
    ;; Create default feature group
    (create-feature-group! :document-features
                           {:name "Document Features"
                            :description "Features extracted from documents"
                            :entity-id :document
                            :online-enabled? true})
    
    ;; Register default features
    (register-feature! :word-count
                       {:name "Word Count"
                        :group-id :document-features
                        :dtype :int
                        :default-value 0})
    
    (register-feature! :sentiment-score
                       {:name "Sentiment Score"
                        :group-id :document-features
                        :dtype :float
                        :default-value 0.0})
    
    (register-feature! :model-count
                       {:name "Mental Model Count"
                        :group-id :document-features
                        :dtype :int
                        :default-value 0})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Feature store initialized")
    (events/emit! :feature-store-initialized {})
    true))
