(ns mental-models.registry.models
  "Mental Model Registry
   
   Centralized registry for managing mental models:
   - Model registration and discovery
   - Model metadata and documentation
   - Model versioning
   - Model categories and tags
   - Model relationships and dependencies
   - Model statistics and usage tracking"
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))

;; =============================================================================
;; MODEL SPECS
;; =============================================================================

(s/def ::model-id keyword?)
(s/def ::name string?)
(s/def ::description string?)
(s/def ::category keyword?)
(s/def ::tags (s/coll-of keyword?))
(s/def ::version string?)
(s/def ::detection-threshold (s/and number? #(<= 0 % 1)))

(s/def ::mental-model
  (s/keys :req-un [::model-id ::name ::description ::category]
          :opt-un [::tags ::version ::detection-threshold]))

;; =============================================================================
;; REGISTRY STATE
;; =============================================================================

(defonce model-registry (atom {}))
(defonce model-stats (atom {}))
(defonce model-versions (atom {}))

;; =============================================================================
;; MODEL CATEGORIES
;; =============================================================================

(def categories
  {:cognitive-bias "Systematic patterns of deviation from rationality"
   :heuristic "Mental shortcuts for problem-solving"
   :fallacy "Errors in reasoning or logic"
   :mental-model "Frameworks for understanding the world"
   :decision-framework "Structured approaches to decision-making"
   :psychological-tendency "Behavioral patterns from psychology"})

;; =============================================================================
;; CORE REGISTRY OPERATIONS
;; =============================================================================

(defn register-model!
  "Register a new mental model"
  [model]
  (if (s/valid? ::mental-model model)
    (do
      (swap! model-registry assoc (:model-id model) model)
      (swap! model-versions update (:model-id model)
             (fnil conj [])
             {:version (or (:version model) "1.0.0")
              :timestamp (System/currentTimeMillis)
              :model model})
      model)
    (throw (ex-info "Invalid model spec" {:model model}))))

(defn unregister-model!
  "Remove a model from the registry"
  [model-id]
  (swap! model-registry dissoc model-id))

(defn get-model
  "Get a model by ID"
  [model-id]
  (get @model-registry model-id))

(defn get-all-models
  "Get all registered models"
  []
  (vals @model-registry))

(defn model-exists?
  "Check if a model is registered"
  [model-id]
  (contains? @model-registry model-id))

;; =============================================================================
;; MODEL QUERIES
;; =============================================================================

(defn find-by-category
  "Find models by category"
  [category]
  (filter #(= category (:category %)) (get-all-models)))

(defn find-by-tag
  "Find models by tag"
  [tag]
  (filter #(some #{tag} (:tags %)) (get-all-models)))

(defn find-by-name
  "Find models by name (partial match)"
  [name-pattern]
  (let [pattern (re-pattern (str "(?i)" name-pattern))]
    (filter #(re-find pattern (:name %)) (get-all-models))))

(defn get-model-count
  "Get count of registered models"
  []
  (count @model-registry))

(defn get-category-counts
  "Get model counts by category"
  []
  (frequencies (map :category (get-all-models))))

;; =============================================================================
;; MODEL STATISTICS
;; =============================================================================

(defn record-detection!
  "Record a model detection event"
  [model-id confidence source]
  (swap! model-stats update model-id
         (fn [stats]
           (-> (or stats {:detections 0 :total-confidence 0 :sources #{}})
               (update :detections inc)
               (update :total-confidence + confidence)
               (update :sources conj source)))))

(defn get-model-stats
  "Get statistics for a model"
  [model-id]
  (let [stats (get @model-stats model-id)]
    (when stats
      (assoc stats
             :avg-confidence (if (pos? (:detections stats))
                               (/ (:total-confidence stats) (:detections stats))
                               0)))))

(defn get-top-models
  "Get most frequently detected models"
  [& {:keys [limit] :or {limit 10}}]
  (->> @model-stats
       (sort-by (comp :detections val) >)
       (take limit)
       (map (fn [[id stats]]
              (assoc stats :model-id id :model (get-model id))))))

;; =============================================================================
;; MODEL VERSIONING
;; =============================================================================

(defn get-model-versions
  "Get version history for a model"
  [model-id]
  (get @model-versions model-id []))

(defn update-model!
  "Update a model (creates new version)"
  [model-id updates]
  (when-let [current (get-model model-id)]
    (let [updated (merge current updates)]
      (register-model! updated))))

;; =============================================================================
;; BULK OPERATIONS
;; =============================================================================

(defn register-models!
  "Register multiple models at once"
  [models]
  (doseq [model models]
    (try
      (register-model! model)
      (catch Exception e
        (println "[REGISTRY] Failed:" (:name model))))))

(defn export-registry
  "Export registry as EDN"
  []
  (pr-str @model-registry))

(defn import-registry!
  "Import registry from EDN"
  [edn-str]
  (let [models (read-string edn-str)]
    (reset! model-registry models)))

;; =============================================================================
;; MUNGER'S CORE TENDENCIES
;; =============================================================================

(def munger-tendencies
  [{:model-id :reward-punishment
    :name "Reward and Punishment Superresponse Tendency"
    :description "People respond strongly to incentives"
    :category :psychological-tendency
    :tags [:incentives :behavior]
    :detection-threshold 0.7}
   {:model-id :liking-loving
    :name "Liking/Loving Tendency"
    :description "Tendency to favor those we like"
    :category :psychological-tendency
    :tags [:bias :social]
    :detection-threshold 0.7}
   {:model-id :disliking-hating
    :name "Disliking/Hating Tendency"
    :description "Tendency to disfavor those we dislike"
    :category :psychological-tendency
    :tags [:bias :social]
    :detection-threshold 0.7}
   {:model-id :doubt-avoidance
    :name "Doubt-Avoidance Tendency"
    :description "Tendency to quickly remove doubt"
    :category :psychological-tendency
    :tags [:decision-making :uncertainty]
    :detection-threshold 0.7}
   {:model-id :inconsistency-avoidance
    :name "Inconsistency-Avoidance Tendency"
    :description "Reluctance to change habits and beliefs"
    :category :psychological-tendency
    :tags [:consistency :habits]
    :detection-threshold 0.7}])

(defn init-core-models!
  "Initialize registry with Munger's core tendencies"
  []
  (register-models! munger-tendencies))
