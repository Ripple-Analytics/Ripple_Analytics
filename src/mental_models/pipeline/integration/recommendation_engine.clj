(ns mental-models.pipeline.integration.recommendation-engine
  "Recommendation Engine Module
   
   Mental model recommendations:
   - Content-based recommendations
   - Collaborative filtering
   - Context-aware suggestions
   - Learning from feedback
   - Personalized rankings"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; RECOMMENDATION ENGINE STATE
;; =============================================================================

(defonce engine-state (atom {:user-profiles (ConcurrentHashMap.)
                             :item-profiles (ConcurrentHashMap.)
                             :interactions (ConcurrentHashMap.)
                             :recommendations (ConcurrentHashMap.)
                             :feedback (ConcurrentHashMap.)
                             :recommendation-count (AtomicLong. 0)
                             :config {:max-recommendations 10
                                      :min-score 0.1
                                      :decay-factor 0.95
                                      :context-weight 0.3}}))

;; =============================================================================
;; USER PROFILES
;; =============================================================================

(defn create-user-profile!
  "Create a user profile."
  [user-id {:keys [preferences interests expertise context]}]
  (log/info "Creating user profile" {:user user-id})
  (.put ^ConcurrentHashMap (:user-profiles @engine-state) user-id
        {:id user-id
         :preferences (or preferences {})
         :interests (or interests #{})
         :expertise (or expertise #{})
         :context (or context {})
         :interaction-history []
         :created-at (System/currentTimeMillis)
         :updated-at (System/currentTimeMillis)}))

(defn update-user-profile!
  "Update a user profile."
  [user-id updates]
  (when-let [profile (.get ^ConcurrentHashMap (:user-profiles @engine-state) user-id)]
    (let [updated (merge profile updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:user-profiles @engine-state) user-id updated)
      updated)))

(defn get-user-profile
  "Get a user profile."
  [user-id]
  (.get ^ConcurrentHashMap (:user-profiles @engine-state) user-id))

(defn delete-user-profile!
  "Delete a user profile."
  [user-id]
  (.remove ^ConcurrentHashMap (:user-profiles @engine-state) user-id))

;; =============================================================================
;; ITEM PROFILES
;; =============================================================================

(defn create-item-profile!
  "Create an item profile (mental model, document, etc.)."
  [item-id {:keys [type category tags features description]}]
  (log/debug "Creating item profile" {:item item-id})
  (.put ^ConcurrentHashMap (:item-profiles @engine-state) item-id
        {:id item-id
         :type type
         :category category
         :tags (or tags #{})
         :features (or features {})
         :description description
         :popularity 0
         :created-at (System/currentTimeMillis)}))

(defn update-item-profile!
  "Update an item profile."
  [item-id updates]
  (when-let [profile (.get ^ConcurrentHashMap (:item-profiles @engine-state) item-id)]
    (let [updated (merge profile updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:item-profiles @engine-state) item-id updated)
      updated)))

(defn get-item-profile
  "Get an item profile."
  [item-id]
  (.get ^ConcurrentHashMap (:item-profiles @engine-state) item-id))

(defn list-items
  "List all items."
  [& {:keys [type category limit]}]
  (let [items (vals (:item-profiles @engine-state))]
    (cond->> items
      type (filter #(= (:type %) type))
      category (filter #(= (:category %) category))
      limit (take limit))))

;; =============================================================================
;; INTERACTIONS
;; =============================================================================

(defn record-interaction!
  "Record a user-item interaction."
  [user-id item-id {:keys [type rating duration context]}]
  (let [interaction-id (str user-id "-" item-id "-" (System/currentTimeMillis))
        interaction {:id interaction-id
                     :user-id user-id
                     :item-id item-id
                     :type (or type :view)
                     :rating rating
                     :duration duration
                     :context context
                     :timestamp (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:interactions @engine-state) interaction-id interaction)
    ;; Update user profile history
    (when-let [profile (get-user-profile user-id)]
      (update-user-profile! user-id
                            {:interaction-history (take 100 (conj (:interaction-history profile) interaction-id))}))
    ;; Update item popularity
    (when-let [item (get-item-profile item-id)]
      (update-item-profile! item-id {:popularity (inc (:popularity item))}))
    (log/debug "Interaction recorded" {:user user-id :item item-id :type type})
    interaction))

(defn get-user-interactions
  "Get interactions for a user."
  [user-id & {:keys [type limit since]}]
  (let [interactions (vals (:interactions @engine-state))]
    (cond->> interactions
      true (filter #(= (:user-id %) user-id))
      type (filter #(= (:type %) type))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

(defn get-item-interactions
  "Get interactions for an item."
  [item-id & {:keys [limit]}]
  (let [interactions (vals (:interactions @engine-state))]
    (cond->> interactions
      true (filter #(= (:item-id %) item-id))
      true (sort-by :timestamp >)
      limit (take limit))))

;; =============================================================================
;; SIMILARITY CALCULATIONS
;; =============================================================================

(defn jaccard-similarity
  "Calculate Jaccard similarity between two sets."
  [set1 set2]
  (if (and (seq set1) (seq set2))
    (/ (count (set/intersection set1 set2))
       (count (set/union set1 set2)))
    0.0))

(defn cosine-similarity
  "Calculate cosine similarity between two feature vectors."
  [features1 features2]
  (let [keys (set/union (set (keys features1)) (set (keys features2)))
        dot-product (reduce + (map (fn [k] (* (get features1 k 0) (get features2 k 0))) keys))
        magnitude1 (Math/sqrt (reduce + (map #(Math/pow (get features1 % 0) 2) keys)))
        magnitude2 (Math/sqrt (reduce + (map #(Math/pow (get features2 % 0) 2) keys)))]
    (if (and (pos? magnitude1) (pos? magnitude2))
      (/ dot-product (* magnitude1 magnitude2))
      0.0)))

(defn calculate-item-similarity
  "Calculate similarity between two items."
  [item1 item2]
  (let [tag-sim (jaccard-similarity (:tags item1) (:tags item2))
        feature-sim (cosine-similarity (:features item1) (:features item2))
        category-match (if (= (:category item1) (:category item2)) 0.2 0.0)]
    (+ (* 0.4 tag-sim) (* 0.4 feature-sim) category-match)))

(defn calculate-user-item-score
  "Calculate relevance score between user and item."
  [user item]
  (let [interest-match (jaccard-similarity (:interests user) (:tags item))
        preference-match (cosine-similarity (:preferences user) (:features item))
        expertise-bonus (if (contains? (:expertise user) (:category item)) 0.1 0.0)]
    (+ (* 0.5 interest-match) (* 0.4 preference-match) expertise-bonus)))

;; =============================================================================
;; RECOMMENDATION ALGORITHMS
;; =============================================================================

(defn content-based-recommendations
  "Generate content-based recommendations."
  [user-id & {:keys [limit]}]
  (let [limit (or limit (get-in @engine-state [:config :max-recommendations]))
        user (get-user-profile user-id)
        items (vals (:item-profiles @engine-state))
        ;; Get items user has interacted with
        interacted (set (map :item-id (get-user-interactions user-id)))
        ;; Score remaining items
        candidates (filter #(not (contains? interacted (:id %))) items)
        scored (map (fn [item]
                      {:item-id (:id item)
                       :score (calculate-user-item-score user item)
                       :reason :content-match})
                    candidates)]
    (take limit (sort-by :score > scored))))

(defn collaborative-recommendations
  "Generate collaborative filtering recommendations."
  [user-id & {:keys [limit]}]
  (let [limit (or limit (get-in @engine-state [:config :max-recommendations]))
        user-interactions (get-user-interactions user-id)
        user-items (set (map :item-id user-interactions))
        ;; Find similar users (users who interacted with same items)
        all-interactions (vals (:interactions @engine-state))
        other-users (distinct (map :user-id (filter #(and (not= (:user-id %) user-id)
                                                          (contains? user-items (:item-id %)))
                                                    all-interactions)))
        ;; Get items from similar users that current user hasn't seen
        similar-user-items (mapcat (fn [uid]
                                     (map :item-id (get-user-interactions uid)))
                                   other-users)
        candidates (filter #(not (contains? user-items %)) similar-user-items)
        ;; Score by frequency
        item-counts (frequencies candidates)
        scored (map (fn [[item-id count]]
                      {:item-id item-id
                       :score (/ count (max 1 (count other-users)))
                       :reason :collaborative})
                    item-counts)]
    (take limit (sort-by :score > scored))))

(defn popularity-recommendations
  "Generate popularity-based recommendations."
  [& {:keys [limit exclude-items]}]
  (let [limit (or limit (get-in @engine-state [:config :max-recommendations]))
        exclude (or exclude-items #{})
        items (vals (:item-profiles @engine-state))
        candidates (filter #(not (contains? exclude (:id %))) items)
        scored (map (fn [item]
                      {:item-id (:id item)
                       :score (/ (:popularity item 0) 100.0)
                       :reason :popularity})
                    candidates)]
    (take limit (sort-by :score > scored))))

(defn context-aware-recommendations
  "Generate context-aware recommendations."
  [user-id context & {:keys [limit]}]
  (let [limit (or limit (get-in @engine-state [:config :max-recommendations]))
        context-weight (get-in @engine-state [:config :context-weight])
        content-recs (content-based-recommendations user-id :limit (* 2 limit))
        ;; Boost items matching context
        boosted (map (fn [rec]
                       (let [item (get-item-profile (:item-id rec))
                             context-match (jaccard-similarity (set (keys context))
                                                               (set (keys (:features item {}))))]
                         (update rec :score + (* context-weight context-match))))
                     content-recs)]
    (take limit (sort-by :score > boosted))))

;; =============================================================================
;; HYBRID RECOMMENDATIONS
;; =============================================================================

(defn hybrid-recommendations
  "Generate hybrid recommendations combining multiple strategies."
  [user-id & {:keys [limit context weights]}]
  (.incrementAndGet ^AtomicLong (:recommendation-count @engine-state))
  (metrics/inc-counter! :recommendationengine/recommendations)
  (let [limit (or limit (get-in @engine-state [:config :max-recommendations]))
        weights (or weights {:content 0.4 :collaborative 0.3 :popularity 0.2 :context 0.1})
        min-score (get-in @engine-state [:config :min-score])
        ;; Get recommendations from each strategy
        content-recs (content-based-recommendations user-id :limit limit)
        collab-recs (collaborative-recommendations user-id :limit limit)
        pop-recs (popularity-recommendations :limit limit)
        context-recs (when context (context-aware-recommendations user-id context :limit limit))
        ;; Combine scores
        all-items (distinct (concat (map :item-id content-recs)
                                    (map :item-id collab-recs)
                                    (map :item-id pop-recs)
                                    (map :item-id (or context-recs []))))
        combined (map (fn [item-id]
                        (let [content-score (or (:score (first (filter #(= (:item-id %) item-id) content-recs))) 0)
                              collab-score (or (:score (first (filter #(= (:item-id %) item-id) collab-recs))) 0)
                              pop-score (or (:score (first (filter #(= (:item-id %) item-id) pop-recs))) 0)
                              ctx-score (or (:score (first (filter #(= (:item-id %) item-id) (or context-recs [])))) 0)
                              final-score (+ (* (:content weights) content-score)
                                             (* (:collaborative weights) collab-score)
                                             (* (:popularity weights) pop-score)
                                             (* (:context weights) ctx-score))]
                          {:item-id item-id
                           :score final-score
                           :components {:content content-score
                                        :collaborative collab-score
                                        :popularity pop-score
                                        :context ctx-score}}))
                      all-items)
        filtered (filter #(>= (:score %) min-score) combined)
        sorted (sort-by :score > filtered)
        result (take limit sorted)]
    ;; Store recommendations
    (.put ^ConcurrentHashMap (:recommendations @engine-state)
          (str user-id "-" (System/currentTimeMillis))
          {:user-id user-id
           :recommendations result
           :context context
           :generated-at (System/currentTimeMillis)})
    (log/info "Recommendations generated" {:user user-id :count (count result)})
    result))

;; =============================================================================
;; FEEDBACK
;; =============================================================================

(defn record-feedback!
  "Record feedback on a recommendation."
  [user-id item-id {:keys [rating clicked converted dismissed]}]
  (let [feedback-id (str user-id "-" item-id "-" (System/currentTimeMillis))
        feedback {:id feedback-id
                  :user-id user-id
                  :item-id item-id
                  :rating rating
                  :clicked clicked
                  :converted converted
                  :dismissed dismissed
                  :timestamp (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:feedback @engine-state) feedback-id feedback)
    ;; Record as interaction if positive
    (when (or clicked converted (and rating (>= rating 3)))
      (record-interaction! user-id item-id {:type :recommendation-click :rating rating}))
    (log/debug "Feedback recorded" {:user user-id :item item-id})
    feedback))

(defn get-feedback-stats
  "Get feedback statistics."
  [& {:keys [user-id item-id since]}]
  (let [feedback (vals (:feedback @engine-state))]
    (cond->> feedback
      user-id (filter #(= (:user-id %) user-id))
      item-id (filter #(= (:item-id %) item-id))
      since (filter #(>= (:timestamp %) since))
      true (reduce (fn [acc f]
                     (-> acc
                         (update :total inc)
                         (update :clicked + (if (:clicked f) 1 0))
                         (update :converted + (if (:converted f) 1 0))
                         (update :dismissed + (if (:dismissed f) 1 0))))
                   {:total 0 :clicked 0 :converted 0 :dismissed 0}))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-engine-stats
  "Get recommendation engine statistics."
  []
  {:users (.size ^ConcurrentHashMap (:user-profiles @engine-state))
   :items (.size ^ConcurrentHashMap (:item-profiles @engine-state))
   :interactions (.size ^ConcurrentHashMap (:interactions @engine-state))
   :recommendations (.size ^ConcurrentHashMap (:recommendations @engine-state))
   :feedback (.size ^ConcurrentHashMap (:feedback @engine-state))
   :recommendation-count (.get ^AtomicLong (:recommendation-count @engine-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-recommendation-engine!
  "Initialize recommendation engine."
  []
  (log/info "Initializing recommendation engine")
  ;; Register feature flag
  (flags/register-flag! "recommendation-engine" "Enable recommendation engine" true)
  ;; Create metrics
  (metrics/create-counter! :recommendationengine/recommendations "Recommendations generated")
  (metrics/create-gauge! :recommendationengine/users "Total users"
                         #(.size ^ConcurrentHashMap (:user-profiles @engine-state)))
  (log/info "Recommendation engine initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-recommendation-engine-status []
  {:enabled (flags/is-enabled? "recommendation-engine")
   :users (.size ^ConcurrentHashMap (:user-profiles @engine-state))
   :items (.size ^ConcurrentHashMap (:item-profiles @engine-state))
   :interactions (.size ^ConcurrentHashMap (:interactions @engine-state))
   :stats (get-engine-stats)
   :config (:config @engine-state)})
