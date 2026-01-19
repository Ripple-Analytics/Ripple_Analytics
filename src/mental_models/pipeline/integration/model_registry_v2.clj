(ns mental-models.pipeline.integration.model-registry-v2
  "Model Registry V2 Module
   
   Enhanced mental model registry:
   - Model versioning and lifecycle
   - Model dependencies and relationships
   - Model metadata and documentation
   - Model search and discovery
   - Model analytics and usage tracking"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; MODEL REGISTRY STATE
;; =============================================================================

(defonce registry-state (atom {:models (ConcurrentHashMap.)
                               :versions (ConcurrentHashMap.)
                               :categories (ConcurrentHashMap.)
                               :relationships (ConcurrentHashMap.)
                               :usage-stats (ConcurrentHashMap.)
                               :model-count (AtomicLong. 0)
                               :usage-count (AtomicLong. 0)
                               :config {:max-versions 10
                                        :track-usage true}}))

;; =============================================================================
;; MODEL DEFINITION
;; =============================================================================

(defn create-model
  "Create a model definition."
  [model-id {:keys [name description category keywords examples
                    detection-patterns confidence-threshold
                    related-models failure-modes safeguards]}]
  {:id model-id
   :name name
   :description description
   :category category
   :keywords (or keywords [])
   :examples (or examples [])
   :detection-patterns (or detection-patterns [])
   :confidence-threshold (or confidence-threshold 0.7)
   :related-models (or related-models [])
   :failure-modes (or failure-modes [])
   :safeguards (or safeguards [])
   :version "1.0.0"
   :status :active
   :created-at (System/currentTimeMillis)
   :updated-at (System/currentTimeMillis)})

(defn register-model!
  "Register a mental model."
  [model-id model-def]
  (.incrementAndGet ^AtomicLong (:model-count @registry-state))
  (log/info "Registering model" {:id model-id :name (:name model-def)})
  (let [model (if (:id model-def)
                model-def
                (create-model model-id model-def))]
    (.put ^ConcurrentHashMap (:models @registry-state) model-id model)
    ;; Track version
    (let [versions-key (str model-id "-versions")
          versions (or (.get ^ConcurrentHashMap (:versions @registry-state) versions-key) [])]
      (.put ^ConcurrentHashMap (:versions @registry-state) versions-key
            (conj versions {:version (:version model)
                            :model model
                            :created-at (System/currentTimeMillis)})))
    ;; Add to category
    (when-let [category (:category model)]
      (let [cat-models (or (.get ^ConcurrentHashMap (:categories @registry-state) category) #{})]
        (.put ^ConcurrentHashMap (:categories @registry-state) category
              (conj cat-models model-id))))
    ;; Publish event
    (events/publish! :modelregistry/model-registered {:model-id model-id})
    model))

(defn unregister-model!
  "Unregister a model."
  [model-id]
  (when-let [model (get-model model-id)]
    ;; Remove from category
    (when-let [category (:category model)]
      (let [cat-models (.get ^ConcurrentHashMap (:categories @registry-state) category)]
        (.put ^ConcurrentHashMap (:categories @registry-state) category
              (disj cat-models model-id))))
    (.remove ^ConcurrentHashMap (:models @registry-state) model-id)))

(defn get-model
  "Get a model by ID."
  [model-id]
  (.get ^ConcurrentHashMap (:models @registry-state) model-id))

(defn list-models
  "List all models."
  [& {:keys [category status keyword]}]
  (let [models (vals (:models @registry-state))]
    (cond->> models
      category (filter #(= (:category %) category))
      status (filter #(= (:status %) status))
      keyword (filter #(some (fn [k] (str/includes? (str/lower-case k) (str/lower-case keyword)))
                             (:keywords %))))))

(defn update-model!
  "Update a model."
  [model-id updates]
  (when-let [model (get-model model-id)]
    (let [updated (merge model updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:models @registry-state) model-id updated)
      updated)))

;; =============================================================================
;; MODEL VERSIONING
;; =============================================================================

(defn create-version!
  "Create a new version of a model."
  [model-id version-string changes]
  (when-let [model (get-model model-id)]
    (let [new-model (assoc model
                           :version version-string
                           :updated-at (System/currentTimeMillis))
          versions-key (str model-id "-versions")
          versions (or (.get ^ConcurrentHashMap (:versions @registry-state) versions-key) [])
          max-versions (get-in @registry-state [:config :max-versions])]
      ;; Update model
      (.put ^ConcurrentHashMap (:models @registry-state) model-id new-model)
      ;; Track version
      (let [new-versions (conj versions {:version version-string
                                         :model new-model
                                         :changes changes
                                         :created-at (System/currentTimeMillis)})]
        (.put ^ConcurrentHashMap (:versions @registry-state) versions-key
              (if (> (count new-versions) max-versions)
                (vec (drop 1 new-versions))
                new-versions)))
      (log/info "Model version created" {:model model-id :version version-string})
      new-model)))

(defn get-model-versions
  "Get all versions of a model."
  [model-id]
  (let [versions-key (str model-id "-versions")]
    (or (.get ^ConcurrentHashMap (:versions @registry-state) versions-key) [])))

(defn get-model-version
  "Get a specific version of a model."
  [model-id version-string]
  (let [versions (get-model-versions model-id)]
    (:model (first (filter #(= (:version %) version-string) versions)))))

;; =============================================================================
;; MODEL RELATIONSHIPS
;; =============================================================================

(defn add-relationship!
  "Add a relationship between models."
  [from-model-id to-model-id relationship-type & {:keys [strength description]}]
  (let [rel-id (str from-model-id "->" to-model-id)
        relationship {:id rel-id
                      :from from-model-id
                      :to to-model-id
                      :type relationship-type
                      :strength (or strength 1.0)
                      :description description
                      :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:relationships @registry-state) rel-id relationship)
    (log/info "Relationship added" {:from from-model-id :to to-model-id :type relationship-type})
    relationship))

(defn remove-relationship!
  "Remove a relationship."
  [from-model-id to-model-id]
  (let [rel-id (str from-model-id "->" to-model-id)]
    (.remove ^ConcurrentHashMap (:relationships @registry-state) rel-id)))

(defn get-relationships
  "Get relationships for a model."
  [model-id & {:keys [direction type]}]
  (let [rels (vals (:relationships @registry-state))]
    (cond->> rels
      (= direction :outgoing) (filter #(= (:from %) model-id))
      (= direction :incoming) (filter #(= (:to %) model-id))
      (nil? direction) (filter #(or (= (:from %) model-id) (= (:to %) model-id)))
      type (filter #(= (:type %) type)))))

(defn get-related-models
  "Get models related to a given model."
  [model-id & {:keys [type depth]}]
  (let [depth (or depth 1)
        rels (get-relationships model-id :type type)
        related-ids (set (mapcat (fn [r]
                                   [(when (= (:from r) model-id) (:to r))
                                    (when (= (:to r) model-id) (:from r))])
                                 rels))]
    (if (> depth 1)
      (reduce (fn [acc id]
                (into acc (get-related-models id :type type :depth (dec depth))))
              related-ids
              related-ids)
      (disj related-ids nil))))

;; =============================================================================
;; MODEL CATEGORIES
;; =============================================================================

(defn create-category!
  "Create a model category."
  [category-id {:keys [name description parent]}]
  (log/info "Creating category" {:id category-id})
  (.put ^ConcurrentHashMap (:categories @registry-state) (str "meta:" category-id)
        {:id category-id
         :name name
         :description description
         :parent parent
         :created-at (System/currentTimeMillis)}))

(defn get-category
  "Get a category."
  [category-id]
  (.get ^ConcurrentHashMap (:categories @registry-state) (str "meta:" category-id)))

(defn list-categories
  "List all categories."
  []
  (filter #(str/starts-with? (str (key %)) "meta:")
          (:categories @registry-state)))

(defn get-models-in-category
  "Get all models in a category."
  [category-id]
  (let [model-ids (.get ^ConcurrentHashMap (:categories @registry-state) category-id)]
    (map get-model (or model-ids #{}))))

;; =============================================================================
;; MODEL SEARCH
;; =============================================================================

(defn search-models
  "Search models by text."
  [query & {:keys [fields limit]}]
  (let [query-lower (str/lower-case query)
        fields (or fields [:name :description :keywords])
        models (vals (:models @registry-state))
        matches (filter (fn [model]
                          (some (fn [field]
                                  (let [value (get model field)]
                                    (cond
                                      (string? value) (str/includes? (str/lower-case value) query-lower)
                                      (coll? value) (some #(str/includes? (str/lower-case (str %)) query-lower) value)
                                      :else false)))
                                fields))
                        models)]
    (if limit
      (take limit matches)
      matches)))

(defn find-models-by-pattern
  "Find models that match a text pattern."
  [text]
  (let [text-lower (str/lower-case text)]
    (filter (fn [model]
              (some (fn [pattern]
                      (re-find (re-pattern (str/lower-case pattern)) text-lower))
                    (:detection-patterns model)))
            (vals (:models @registry-state)))))

;; =============================================================================
;; USAGE TRACKING
;; =============================================================================

(defn record-usage!
  "Record model usage."
  [model-id & {:keys [context result]}]
  (when (get-in @registry-state [:config :track-usage])
    (.incrementAndGet ^AtomicLong (:usage-count @registry-state))
    (let [stats (or (.get ^ConcurrentHashMap (:usage-stats @registry-state) model-id)
                    {:model-id model-id
                     :total-uses 0
                     :successful-uses 0
                     :last-used nil
                     :contexts []})
          updated (-> stats
                      (update :total-uses inc)
                      (update :successful-uses (if (:success result) inc identity))
                      (assoc :last-used (System/currentTimeMillis))
                      (update :contexts #(take 100 (conj % {:context context
                                                            :result result
                                                            :timestamp (System/currentTimeMillis)}))))]
      (.put ^ConcurrentHashMap (:usage-stats @registry-state) model-id updated))))

(defn get-usage-stats
  "Get usage statistics for a model."
  [model-id]
  (.get ^ConcurrentHashMap (:usage-stats @registry-state) model-id))

(defn get-top-models
  "Get top models by usage."
  [& {:keys [limit]}]
  (let [limit (or limit 10)
        stats (vals (:usage-stats @registry-state))]
    (take limit (sort-by :total-uses > stats))))

;; =============================================================================
;; MUNGER'S 25 TENDENCIES
;; =============================================================================

(defn init-munger-models!
  "Initialize Munger's 25 psychological tendencies."
  []
  (let [tendencies
        [{:id :reward-punishment
          :name "Reward and Punishment Superresponse"
          :category :psychology
          :keywords ["incentive" "reward" "punishment" "motivation"]
          :description "People respond strongly to incentives and disincentives"}
         {:id :liking-loving
          :name "Liking/Loving Tendency"
          :category :psychology
          :keywords ["affection" "bias" "favoritism"]
          :description "Tendency to favor people, products, and actions associated with those we like"}
         {:id :disliking-hating
          :name "Disliking/Hating Tendency"
          :category :psychology
          :keywords ["hatred" "bias" "prejudice"]
          :description "Tendency to disfavor people, products, and actions associated with those we dislike"}
         {:id :doubt-avoidance
          :name "Doubt-Avoidance Tendency"
          :category :psychology
          :keywords ["uncertainty" "decision" "quick judgment"]
          :description "Tendency to quickly remove doubt by reaching a decision"}
         {:id :inconsistency-avoidance
          :name "Inconsistency-Avoidance Tendency"
          :category :psychology
          :keywords ["habit" "consistency" "change resistance"]
          :description "Tendency to resist change to remain consistent with prior commitments"}
         {:id :curiosity
          :name "Curiosity Tendency"
          :category :psychology
          :keywords ["learning" "exploration" "interest"]
          :description "Natural tendency to seek new knowledge and experiences"}
         {:id :kantian-fairness
          :name "Kantian Fairness Tendency"
          :category :psychology
          :keywords ["fairness" "reciprocity" "justice"]
          :description "Tendency to expect and practice fair dealing"}
         {:id :envy-jealousy
          :name "Envy/Jealousy Tendency"
          :category :psychology
          :keywords ["comparison" "resentment" "competition"]
          :description "Tendency to feel envy or jealousy toward others' success"}
         {:id :reciprocation
          :name "Reciprocation Tendency"
          :category :psychology
          :keywords ["give and take" "obligation" "exchange"]
          :description "Tendency to reciprocate both favors and disfavors"}
         {:id :influence-from-association
          :name "Influence-from-Mere-Association Tendency"
          :category :psychology
          :keywords ["association" "conditioning" "branding"]
          :description "Tendency to be influenced by associated stimuli"}
         {:id :pain-avoidance
          :name "Simple Pain-Avoiding Psychological Denial"
          :category :psychology
          :keywords ["denial" "avoidance" "distortion"]
          :description "Tendency to distort reality to avoid psychological pain"}
         {:id :excessive-self-regard
          :name "Excessive Self-Regard Tendency"
          :category :psychology
          :keywords ["overconfidence" "ego" "self-serving bias"]
          :description "Tendency to overestimate one's own abilities and worth"}
         {:id :overoptimism
          :name "Overoptimism Tendency"
          :category :psychology
          :keywords ["optimism bias" "planning fallacy" "wishful thinking"]
          :description "Tendency to be excessively optimistic about outcomes"}
         {:id :deprival-superreaction
          :name "Deprival-Superreaction Tendency"
          :category :psychology
          :keywords ["loss aversion" "scarcity" "possession"]
          :description "Tendency to react strongly to real or threatened losses"}
         {:id :social-proof
          :name "Social-Proof Tendency"
          :category :psychology
          :keywords ["conformity" "herd behavior" "peer pressure"]
          :description "Tendency to follow the actions of others"}
         {:id :contrast-misreaction
          :name "Contrast-Misreaction Tendency"
          :category :psychology
          :keywords ["anchoring" "comparison" "relative judgment"]
          :description "Tendency to misjudge based on contrasts"}
         {:id :stress-influence
          :name "Stress-Influence Tendency"
          :category :psychology
          :keywords ["stress" "pressure" "performance"]
          :description "Tendency for stress to affect judgment and behavior"}
         {:id :availability-misweighing
          :name "Availability-Misweighing Tendency"
          :category :psychology
          :keywords ["availability heuristic" "recency" "salience"]
          :description "Tendency to overweight readily available information"}
         {:id :use-it-or-lose-it
          :name "Use-It-or-Lose-It Tendency"
          :category :psychology
          :keywords ["skill decay" "practice" "maintenance"]
          :description "Tendency for unused skills to atrophy"}
         {:id :drug-misinfluence
          :name "Drug Misinfluence Tendency"
          :category :psychology
          :keywords ["addiction" "substance" "dependency"]
          :description "Tendency for drugs to distort cognition and behavior"}
         {:id :senescence-misinfluence
          :name "Senescence-Misinfluence Tendency"
          :category :psychology
          :keywords ["aging" "cognitive decline" "wisdom"]
          :description "Tendency for aging to affect cognitive abilities"}
         {:id :authority-misinfluence
          :name "Authority-Misinfluence Tendency"
          :category :psychology
          :keywords ["obedience" "hierarchy" "expertise"]
          :description "Tendency to follow authority figures"}
         {:id :twaddle
          :name "Twaddle Tendency"
          :category :psychology
          :keywords ["nonsense" "verbosity" "empty talk"]
          :description "Tendency to engage in or be influenced by meaningless talk"}
         {:id :reason-respecting
          :name "Reason-Respecting Tendency"
          :category :psychology
          :keywords ["justification" "explanation" "rationality"]
          :description "Tendency to comply more when given reasons"}
         {:id :lollapalooza
          :name "Lollapalooza Tendency"
          :category :psychology
          :keywords ["confluence" "combination" "synergy"]
          :description "Tendency for multiple biases to combine for extreme outcomes"}]]
    (doseq [tendency tendencies]
      (register-model! (:id tendency) tendency))
    (log/info "Munger's 25 tendencies initialized")))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-registry-stats
  "Get registry statistics."
  []
  {:models (.size ^ConcurrentHashMap (:models @registry-state))
   :categories (count (filter #(not (str/starts-with? (str (key %)) "meta:"))
                              (:categories @registry-state)))
   :relationships (.size ^ConcurrentHashMap (:relationships @registry-state))
   :model-count (.get ^AtomicLong (:model-count @registry-state))
   :usage-count (.get ^AtomicLong (:usage-count @registry-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-model-registry-v2!
  "Initialize model registry v2."
  []
  (log/info "Initializing model registry v2")
  ;; Register feature flag
  (flags/register-flag! "model-registry-v2" "Enable model registry v2" true)
  ;; Create metrics
  (metrics/create-gauge! :modelregistryv2/total-models "Total models"
                         #(.size ^ConcurrentHashMap (:models @registry-state)))
  ;; Initialize Munger's tendencies
  (init-munger-models!)
  (log/info "Model registry v2 initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-model-registry-v2-status []
  {:enabled (flags/is-enabled? "model-registry-v2")
   :models (.size ^ConcurrentHashMap (:models @registry-state))
   :categories (count (filter #(not (str/starts-with? (str (key %)) "meta:"))
                              (:categories @registry-state)))
   :relationships (.size ^ConcurrentHashMap (:relationships @registry-state))
   :stats (get-registry-stats)
   :config (:config @registry-state)})
