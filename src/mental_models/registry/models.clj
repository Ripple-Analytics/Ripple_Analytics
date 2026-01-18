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
  [;; 1. Reward and Punishment Superresponse Tendency
   {:model-id :reward-punishment
    :name "Reward and Punishment Superresponse Tendency"
    :description "People respond strongly to incentives - both positive and negative. Incentives drive behavior more than anything else."
    :category :psychological-tendency
    :tags [:incentives :behavior :motivation]
    :detection-threshold 0.7}
   ;; 2. Liking/Loving Tendency
   {:model-id :liking-loving
    :name "Liking/Loving Tendency"
    :description "Tendency to favor those we like, ignore their faults, favor their products, and distort facts to facilitate love."
    :category :psychological-tendency
    :tags [:bias :social :affection]
    :detection-threshold 0.7}
   ;; 3. Disliking/Hating Tendency
   {:model-id :disliking-hating
    :name "Disliking/Hating Tendency"
    :description "Tendency to disfavor those we dislike, ignore their virtues, hate their products, and distort facts to facilitate hatred."
    :category :psychological-tendency
    :tags [:bias :social :aversion]
    :detection-threshold 0.7}
   ;; 4. Doubt-Avoidance Tendency
   {:model-id :doubt-avoidance
    :name "Doubt-Avoidance Tendency"
    :description "Tendency to quickly remove doubt by reaching a decision. Triggered by puzzlement and stress."
    :category :psychological-tendency
    :tags [:decision-making :uncertainty :stress]
    :detection-threshold 0.7}
   ;; 5. Inconsistency-Avoidance Tendency
   {:model-id :inconsistency-avoidance
    :name "Inconsistency-Avoidance Tendency"
    :description "Reluctance to change habits, beliefs, and conclusions. The brain conserves programming space by resisting change."
    :category :psychological-tendency
    :tags [:consistency :habits :change-resistance]
    :detection-threshold 0.7}
   ;; 6. Curiosity Tendency
   {:model-id :curiosity
    :name "Curiosity Tendency"
    :description "Innate curiosity that drives learning and exploration. Helps prevent boredom and enables knowledge acquisition."
    :category :psychological-tendency
    :tags [:learning :exploration :knowledge]
    :detection-threshold 0.7}
   ;; 7. Kantian Fairness Tendency
   {:model-id :kantian-fairness
    :name "Kantian Fairness Tendency"
    :description "Tendency to expect and demand fair exchanges. Tolerating unfairness causes resentment and dysfunction."
    :category :psychological-tendency
    :tags [:fairness :reciprocity :ethics]
    :detection-threshold 0.7}
   ;; 8. Envy/Jealousy Tendency
   {:model-id :envy-jealousy
    :name "Envy/Jealousy Tendency"
    :description "Tendency to feel envy when others have what we want. Drives much destructive behavior in society."
    :category :psychological-tendency
    :tags [:envy :comparison :competition]
    :detection-threshold 0.7}
   ;; 9. Reciprocation Tendency
   {:model-id :reciprocation
    :name "Reciprocation Tendency"
    :description "Tendency to reciprocate favors and disfavors. Powerful force for both good and evil in human affairs."
    :category :psychological-tendency
    :tags [:reciprocity :social :exchange]
    :detection-threshold 0.7}
   ;; 10. Influence-from-Mere-Association Tendency
   {:model-id :mere-association
    :name "Influence-from-Mere-Association Tendency"
    :description "Tendency to be influenced by associations, even when they are irrelevant. Includes Persian Messenger Syndrome."
    :category :psychological-tendency
    :tags [:association :conditioning :bias]
    :detection-threshold 0.7}
   ;; 11. Simple Pain-Avoiding Psychological Denial
   {:model-id :pain-avoiding-denial
    :name "Simple Pain-Avoiding Psychological Denial"
    :description "Tendency to distort reality to reduce psychological pain. Can cause extreme distortion of facts."
    :category :psychological-tendency
    :tags [:denial :pain-avoidance :distortion]
    :detection-threshold 0.7}
   ;; 12. Excessive Self-Regard Tendency
   {:model-id :excessive-self-regard
    :name "Excessive Self-Regard Tendency"
    :description "Tendency to overestimate oneself and one's possessions. Includes endowment effect and overconfidence."
    :category :psychological-tendency
    :tags [:overconfidence :self-esteem :endowment]
    :detection-threshold 0.7}
   ;; 13. Over-Optimism Tendency
   {:model-id :over-optimism
    :name "Over-Optimism Tendency"
    :description "Tendency to be excessively optimistic. What a man wishes, he will believe."
    :category :psychological-tendency
    :tags [:optimism :wishful-thinking :planning]
    :detection-threshold 0.7}
   ;; 14. Deprival-Superreaction Tendency
   {:model-id :deprival-superreaction
    :name "Deprival-Superreaction Tendency"
    :description "Tendency to react intensely to loss or threatened loss. Loss aversion is roughly 2x gain preference."
    :category :psychological-tendency
    :tags [:loss-aversion :scarcity :reaction]
    :detection-threshold 0.7}
   ;; 15. Social-Proof Tendency
   {:model-id :social-proof
    :name "Social-Proof Tendency"
    :description "Tendency to think and act as others around us are thinking and acting. Automatic and unconscious."
    :category :psychological-tendency
    :tags [:conformity :social :herd-behavior]
    :detection-threshold 0.7}
   ;; 16. Contrast-Misreaction Tendency
   {:model-id :contrast-misreaction
    :name "Contrast-Misreaction Tendency"
    :description "Tendency to misjudge based on contrast with nearby or recent things. Boiling frog syndrome."
    :category :psychological-tendency
    :tags [:contrast :perception :judgment]
    :detection-threshold 0.7}
   ;; 17. Stress-Influence Tendency
   {:model-id :stress-influence
    :name "Stress-Influence Tendency"
    :description "Tendency for stress to cause both dysfunction and enhanced performance. Light stress improves, heavy stress harms."
    :category :psychological-tendency
    :tags [:stress :performance :pressure]
    :detection-threshold 0.7}
   ;; 18. Availability-Misweighing Tendency
   {:model-id :availability-misweighing
    :name "Availability-Misweighing Tendency"
    :description "Tendency to overweight what is easily available in memory. Vivid, recent, or emotional events dominate."
    :category :psychological-tendency
    :tags [:availability :memory :judgment]
    :detection-threshold 0.7}
   ;; 19. Use-It-or-Lose-It Tendency
   {:model-id :use-it-or-lose-it
    :name "Use-It-or-Lose-It Tendency"
    :description "Skills attenuate with disuse. Must practice to maintain competence in any skill."
    :category :psychological-tendency
    :tags [:skills :practice :atrophy]
    :detection-threshold 0.7}
   ;; 20. Drug-Misinfluence Tendency
   {:model-id :drug-misinfluence
    :name "Drug-Misinfluence Tendency"
    :description "Tendency for drugs to cause cognitive and moral deterioration. Includes alcohol and other substances."
    :category :psychological-tendency
    :tags [:drugs :addiction :impairment]
    :detection-threshold 0.7}
   ;; 21. Senescence-Misinfluence Tendency
   {:model-id :senescence-misinfluence
    :name "Senescence-Misinfluence Tendency"
    :description "Cognitive decay with age. Continuous thinking and learning can slow but not eliminate decay."
    :category :psychological-tendency
    :tags [:aging :cognitive-decline :learning]
    :detection-threshold 0.7}
   ;; 22. Authority-Misinfluence Tendency
   {:model-id :authority-misinfluence
    :name "Authority-Misinfluence Tendency"
    :description "Tendency to follow leaders, especially in times of stress. Can lead to following bad leaders."
    :category :psychological-tendency
    :tags [:authority :obedience :leadership]
    :detection-threshold 0.7}
   ;; 23. Twaddle Tendency
   {:model-id :twaddle
    :name "Twaddle Tendency"
    :description "Tendency to prattle on about unimportant matters. Wastes time and obscures important issues."
    :category :psychological-tendency
    :tags [:communication :distraction :noise]
    :detection-threshold 0.7}
   ;; 24. Reason-Respecting Tendency
   {:model-id :reason-respecting
    :name "Reason-Respecting Tendency"
    :description "Tendency to comply more readily when given reasons. Even bad reasons increase compliance."
    :category :psychological-tendency
    :tags [:reasoning :compliance :persuasion]
    :detection-threshold 0.7}
   ;; 25. Lollapalooza Tendency
   {:model-id :lollapalooza
    :name "Lollapalooza Tendency"
    :description "Tendency for multiple psychological tendencies to combine and produce extreme outcomes. The whole exceeds sum of parts."
    :category :psychological-tendency
    :tags [:combination :synergy :extreme-outcomes]
    :detection-threshold 0.7}])

(defn init-core-models!
  "Initialize registry with Munger's core tendencies"
  []
  (register-models! munger-tendencies))
