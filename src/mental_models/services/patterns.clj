(ns mental-models.services.patterns
  "Cross-Domain Pattern Detection
   Identifies patterns that span multiple mental models and domains"
  (:require [mental-models.data.models :as models]
            [mental-models.services.statistics :as stats]
            [taoensso.timbre :as log]))

;; -- Pattern Types -----------------------------------------------------------

(def pattern-types
  {:reinforcing "Multiple models pointing in the same direction"
   :conflicting "Models suggesting opposite actions"
   :cascading "One model's outcome triggering another"
   :lollapalooza "3+ models combining for extreme effect"
   :hidden-connection "Non-obvious relationship between models"
   :temporal "Time-based pattern in model application"})

;; -- Model Relationships -----------------------------------------------------

(def model-relationships
  "Pre-defined relationships between mental models"
  {;; Cognitive biases that often appear together
   [:confirmation-bias :availability-heuristic] {:type :reinforcing :strength 0.8}
   [:confirmation-bias :anchoring] {:type :reinforcing :strength 0.7}
   [:loss-aversion :sunk-cost-fallacy] {:type :reinforcing :strength 0.9}
   [:overconfidence-bias :dunning-kruger] {:type :reinforcing :strength 0.85}
   
   ;; Systems thinking connections
   [:feedback-loops :second-order-thinking] {:type :cascading :strength 0.9}
   [:network-effects :critical-mass] {:type :cascading :strength 0.85}
   [:bottlenecks :leverage-points] {:type :hidden-connection :strength 0.7}
   
   ;; Economics connections
   [:incentives :principal-agent] {:type :reinforcing :strength 0.9}
   [:supply-and-demand :arbitrage] {:type :cascading :strength 0.7}
   [:compound-interest :compounding] {:type :reinforcing :strength 0.95}
   
   ;; Decision making
   [:first-principles :inversion] {:type :reinforcing :strength 0.8}
   [:expected-value :probabilistic-thinking] {:type :reinforcing :strength 0.9}
   [:circle-of-competence :margin-of-safety] {:type :reinforcing :strength 0.75}
   
   ;; Conflicting pairs
   [:optimism-bias :negativity-bias] {:type :conflicting :strength 0.8}
   [:satisficing :expected-value] {:type :conflicting :strength 0.6}
   [:status-quo-bias :first-principles] {:type :conflicting :strength 0.7}})

;; -- Pattern Detection -------------------------------------------------------

(defn find-related-models
  "Find models related to a given model"
  [model-slug]
  (let [model-key (keyword model-slug)]
    (for [[[m1 m2] rel] model-relationships
          :when (or (= m1 model-key) (= m2 model-key))]
      {:model (if (= m1 model-key) m2 m1)
       :relationship rel})))

(defn detect-reinforcing-patterns
  "Detect when multiple models reinforce each other"
  [model-slugs]
  (let [model-keys (set (map keyword model-slugs))
        pairs (for [m1 model-keys
                    m2 model-keys
                    :when (and (not= m1 m2)
                               (contains? model-relationships [m1 m2])
                               (= :reinforcing (:type (model-relationships [m1 m2]))))]
                [[m1 m2] (model-relationships [m1 m2])])]
    (when (seq pairs)
      {:type :reinforcing
       :models (distinct (mapcat first pairs))
       :pairs pairs
       :combined-strength (stats/mean (map #(get-in % [1 :strength]) pairs))})))

(defn detect-conflicting-patterns
  "Detect when models suggest conflicting actions"
  [model-slugs]
  (let [model-keys (set (map keyword model-slugs))
        conflicts (for [m1 model-keys
                        m2 model-keys
                        :when (and (not= m1 m2)
                                   (contains? model-relationships [m1 m2])
                                   (= :conflicting (:type (model-relationships [m1 m2]))))]
                    [[m1 m2] (model-relationships [m1 m2])])]
    (when (seq conflicts)
      {:type :conflicting
       :models (distinct (mapcat first conflicts))
       :conflicts conflicts
       :resolution-needed true})))

(defn detect-lollapalooza
  "Detect lollapalooza effects (3+ models combining)"
  [model-slugs]
  (let [model-keys (set (map keyword model-slugs))
        reinforcing (detect-reinforcing-patterns model-slugs)]
    (when (and reinforcing (>= (count (:models reinforcing)) 3))
      {:type :lollapalooza
       :models (:models reinforcing)
       :strength (* (:combined-strength reinforcing) (count (:models reinforcing)) 0.3)
       :warning "Multiple mental models combining - potential for extreme outcomes"})))

(defn detect-cascading-patterns
  "Detect cascading effects between models"
  [model-slugs]
  (let [model-keys (set (map keyword model-slugs))
        cascades (for [m1 model-keys
                       m2 model-keys
                       :when (and (not= m1 m2)
                                  (contains? model-relationships [m1 m2])
                                  (= :cascading (:type (model-relationships [m1 m2]))))]
                   {:trigger m1
                    :effect m2
                    :strength (get-in model-relationships [[m1 m2] :strength])})]
    (when (seq cascades)
      {:type :cascading
       :chains cascades
       :total-cascade-potential (reduce + (map :strength cascades))})))

;; -- Cross-Domain Analysis ---------------------------------------------------

(defn get-model-domains
  "Get the domain/category for each model"
  [model-slugs]
  (for [slug model-slugs
        :let [model (models/get-model-by-slug slug)]
        :when model]
    {:slug slug
     :category-id (:category-id model)
     :category (models/get-category-by-id (:category-id model))}))

(defn detect-cross-domain-patterns
  "Detect patterns that span multiple domains"
  [model-slugs]
  (let [domains (get-model-domains model-slugs)
        by-category (group-by :category-id domains)
        category-count (count by-category)]
    (when (> category-count 1)
      {:type :cross-domain
       :domains (keys by-category)
       :domain-count category-count
       :models-per-domain (into {} (map (fn [[k v]] [k (count v)]) by-category))
       :insight (str "Analysis spans " category-count " different domains - "
                     "consider how insights from each domain interact")})))

;; -- Temporal Pattern Detection ----------------------------------------------

(defn detect-temporal-patterns
  "Detect patterns in how models are used over time"
  [usage-history]
  (let [;; Group by time period (e.g., week)
        by-week (group-by #(quot (:timestamp %) (* 7 24 60 60 1000)) usage-history)
        ;; Calculate model frequency per period
        weekly-frequencies (for [[week usages] by-week]
                            {:week week
                             :models (frequencies (map :model-slug usages))
                             :count (count usages)})
        ;; Find trending models
        recent (take 4 (reverse (sort-by :week weekly-frequencies)))
        older (drop 4 (reverse (sort-by :week weekly-frequencies)))]
    (when (and (seq recent) (seq older))
      (let [recent-models (frequencies (mapcat #(keys (:models %)) recent))
            older-models (frequencies (mapcat #(keys (:models %)) older))
            trending (for [[model count] recent-models
                           :let [old-count (get older-models model 0)]
                           :when (> count (* 1.5 old-count))]
                       {:model model
                        :trend :increasing
                        :recent-count count
                        :old-count old-count})
            declining (for [[model count] older-models
                            :let [new-count (get recent-models model 0)]
                            :when (> count (* 1.5 new-count))]
                        {:model model
                         :trend :decreasing
                         :recent-count new-count
                         :old-count count})]
        {:type :temporal
         :trending trending
         :declining declining
         :analysis-period {:recent-weeks (count recent)
                           :comparison-weeks (count older)}}))))

;; -- Comprehensive Pattern Analysis ------------------------------------------

(defn analyze-patterns
  "Run comprehensive pattern analysis on a set of models"
  [model-slugs & {:keys [usage-history]}]
  (let [reinforcing (detect-reinforcing-patterns model-slugs)
        conflicting (detect-conflicting-patterns model-slugs)
        lollapalooza (detect-lollapalooza model-slugs)
        cascading (detect-cascading-patterns model-slugs)
        cross-domain (detect-cross-domain-patterns model-slugs)
        temporal (when usage-history (detect-temporal-patterns usage-history))]
    {:patterns (remove nil? [reinforcing conflicting lollapalooza cascading cross-domain temporal])
     :summary {:reinforcing-count (count (:pairs reinforcing))
               :conflict-count (count (:conflicts conflicting))
               :lollapalooza-detected (boolean lollapalooza)
               :cascade-potential (or (:total-cascade-potential cascading) 0)
               :cross-domain (boolean cross-domain)}
     :recommendations (generate-recommendations
                       {:reinforcing reinforcing
                        :conflicting conflicting
                        :lollapalooza lollapalooza
                        :cascading cascading})}))

(defn generate-recommendations
  "Generate actionable recommendations from pattern analysis"
  [{:keys [reinforcing conflicting lollapalooza cascading]}]
  (cond-> []
    reinforcing
    (conj {:type :leverage
           :message "Multiple models reinforce each other - leverage this alignment"
           :priority :medium})
    
    conflicting
    (conj {:type :resolve
           :message "Conflicting models detected - resolve before deciding"
           :priority :high})
    
    lollapalooza
    (conj {:type :caution
           :message "Lollapalooza effect possible - outcomes may be extreme"
           :priority :high})
    
    cascading
    (conj {:type :anticipate
           :message "Cascading effects likely - plan for second-order consequences"
           :priority :medium})))

;; -- Model Suggestion Engine -------------------------------------------------

(defn suggest-complementary-models
  "Suggest models that would complement the current set"
  [current-model-slugs]
  (let [current-keys (set (map keyword current-model-slugs))
        related (for [slug current-model-slugs
                      {:keys [model relationship]} (find-related-models slug)
                      :when (not (contains? current-keys model))]
                  {:model model
                   :reason (:type relationship)
                   :strength (:strength relationship)
                   :suggested-by slug})
        ;; Dedupe and rank by strength
        by-model (group-by :model related)
        ranked (for [[model suggestions] by-model]
                 {:model model
                  :total-strength (reduce + (map :strength suggestions))
                  :reasons (distinct (map :reason suggestions))
                  :suggested-by (distinct (map :suggested-by suggestions))})]
    (take 5 (reverse (sort-by :total-strength ranked)))))

(defn suggest-for-decision-type
  "Suggest models based on decision type"
  [decision-type]
  (case decision-type
    :investment [:margin-of-safety :circle-of-competence :compound-interest
                 :opportunity-cost :second-order-thinking]
    :career [:comparative-advantage :opportunity-cost :optionality
             :regret-minimization :circle-of-competence]
    :strategy [:first-principles :inversion :game-theory
               :network-effects :competitive-advantage]
    :risk [:margin-of-safety :expected-value :probabilistic-thinking
           :pre-mortem :antifragility]
    :negotiation [:incentives :game-theory :reciprocity
                  :anchoring :framing-effect]
    ;; Default
    [:first-principles :second-order-thinking :inversion
     :circle-of-competence :margin-of-safety]))
