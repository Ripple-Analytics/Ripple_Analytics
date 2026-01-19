(ns mental-models.pipeline.integration.context-analyzer
  "Context Analyzer Module
   
   Context analysis for mental models:
   - Entity extraction
   - Relationship mapping
   - Temporal context
   - Domain classification
   - Context enrichment"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.util.regex Pattern]))

;; =============================================================================
;; CONTEXT ANALYZER STATE
;; =============================================================================

(defonce analyzer-state (atom {:contexts (ConcurrentHashMap.)
                               :entities (ConcurrentHashMap.)
                               :domains (ConcurrentHashMap.)
                               :relationships (ConcurrentHashMap.)
                               :analysis-count (AtomicLong. 0)
                               :config {:max-entities 50
                                        :min-entity-length 2
                                        :context-window 500
                                        :relationship-depth 2}}))

;; =============================================================================
;; ENTITY EXTRACTION
;; =============================================================================

(defn extract-capitalized-entities
  "Extract capitalized words as potential entities."
  [text]
  (let [pattern (Pattern/compile "\\b[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)*\\b")
        matcher (.matcher pattern text)]
    (loop [entities []]
      (if (.find matcher)
        (recur (conj entities {:text (.group matcher)
                               :start (.start matcher)
                               :end (.end matcher)
                               :type :named-entity}))
        entities))))

(defn extract-quoted-entities
  "Extract quoted text as entities."
  [text]
  (let [pattern (Pattern/compile "\"([^\"]+)\"")
        matcher (.matcher pattern text)]
    (loop [entities []]
      (if (.find matcher)
        (recur (conj entities {:text (.group matcher 1)
                               :start (.start matcher)
                               :end (.end matcher)
                               :type :quoted}))
        entities))))

(defn extract-numeric-entities
  "Extract numbers and percentages."
  [text]
  (let [pattern (Pattern/compile "\\b\\d+(?:\\.\\d+)?%?\\b")
        matcher (.matcher pattern text)]
    (loop [entities []]
      (if (.find matcher)
        (recur (conj entities {:text (.group matcher)
                               :start (.start matcher)
                               :end (.end matcher)
                               :type :numeric}))
        entities))))

(defn extract-temporal-entities
  "Extract temporal references."
  [text]
  (let [patterns [#"(?i)\b(?:today|yesterday|tomorrow|last\s+(?:week|month|year)|next\s+(?:week|month|year))\b"
                  #"(?i)\b(?:january|february|march|april|may|june|july|august|september|october|november|december)\s+\d{1,2}(?:,?\s+\d{4})?\b"
                  #"\b\d{4}-\d{2}-\d{2}\b"
                  #"\b\d{1,2}/\d{1,2}/\d{2,4}\b"]]
    (mapcat (fn [pattern]
              (let [matcher (.matcher pattern text)]
                (loop [entities []]
                  (if (.find matcher)
                    (recur (conj entities {:text (.group matcher)
                                           :start (.start matcher)
                                           :end (.end matcher)
                                           :type :temporal}))
                    entities))))
            patterns)))

(defn extract-all-entities
  "Extract all entities from text."
  [text]
  (let [max-entities (get-in @analyzer-state [:config :max-entities])
        min-length (get-in @analyzer-state [:config :min-entity-length])
        all-entities (concat (extract-capitalized-entities text)
                             (extract-quoted-entities text)
                             (extract-numeric-entities text)
                             (extract-temporal-entities text))
        filtered (filter #(>= (count (:text %)) min-length) all-entities)
        unique (distinct filtered)]
    (take max-entities unique)))

;; =============================================================================
;; DOMAIN CLASSIFICATION
;; =============================================================================

(defn register-domain!
  "Register a domain with keywords."
  [domain-id {:keys [name description keywords patterns]}]
  (log/debug "Registering domain" {:id domain-id :name name})
  (.put ^ConcurrentHashMap (:domains @analyzer-state) domain-id
        {:id domain-id
         :name name
         :description description
         :keywords (set (map str/lower-case (or keywords [])))
         :patterns (or patterns [])
         :match-count (AtomicLong. 0)}))

(defn get-domain
  "Get a domain by ID."
  [domain-id]
  (.get ^ConcurrentHashMap (:domains @analyzer-state) domain-id))

(defn list-domains
  "List all domains."
  []
  (vals (:domains @analyzer-state)))

(defn classify-domain
  "Classify text into domains."
  [text]
  (let [text-lower (str/lower-case text)
        words (set (str/split text-lower #"\s+"))
        domains (list-domains)
        scored (map (fn [domain]
                      (let [keyword-matches (set/intersection words (:keywords domain))
                            score (/ (count keyword-matches)
                                     (max 1 (count (:keywords domain))))]
                        {:domain-id (:id domain)
                         :domain-name (:name domain)
                         :score score
                         :matched-keywords keyword-matches}))
                    domains)
        filtered (filter #(> (:score %) 0) scored)
        sorted (sort-by :score > filtered)]
    (when (seq sorted)
      (.incrementAndGet ^AtomicLong (:match-count (get-domain (:domain-id (first sorted)))))
      sorted)))

;; =============================================================================
;; RELATIONSHIP EXTRACTION
;; =============================================================================

(defn extract-relationships
  "Extract relationships between entities."
  [text entities]
  (let [relationship-patterns [{:pattern #"(?i)(\w+)\s+(?:is|are|was|were)\s+(\w+)"
                                :type :is-a}
                               {:pattern #"(?i)(\w+)\s+(?:causes?|leads?\s+to|results?\s+in)\s+(\w+)"
                                :type :causes}
                               {:pattern #"(?i)(\w+)\s+(?:and|with|plus)\s+(\w+)"
                                :type :associated-with}
                               {:pattern #"(?i)(\w+)\s+(?:vs\.?|versus|against)\s+(\w+)"
                                :type :contrasts-with}]]
    (mapcat (fn [{:keys [pattern type]}]
              (let [matcher (.matcher pattern text)]
                (loop [rels []]
                  (if (.find matcher)
                    (recur (conj rels {:source (.group matcher 1)
                                       :target (.group matcher 2)
                                       :type type
                                       :context (subs text
                                                      (max 0 (- (.start matcher) 20))
                                                      (min (count text) (+ (.end matcher) 20)))}))
                    rels))))
            relationship-patterns)))

(defn build-entity-graph
  "Build a graph of entity relationships."
  [entities relationships]
  (let [entity-map (into {} (map (fn [e] [(:text e) e]) entities))
        graph {:nodes (map (fn [e] {:id (:text e)
                                    :type (:type e)
                                    :position {:start (:start e) :end (:end e)}})
                           entities)
               :edges (map (fn [r] {:source (:source r)
                                    :target (:target r)
                                    :type (:type r)})
                           relationships)}]
    graph))

;; =============================================================================
;; CONTEXT ENRICHMENT
;; =============================================================================

(defn extract-key-phrases
  "Extract key phrases from text."
  [text]
  (let [;; Simple n-gram extraction
        words (str/split (str/lower-case text) #"\s+")
        bigrams (map #(str/join " " %) (partition 2 1 words))
        trigrams (map #(str/join " " %) (partition 3 1 words))
        ;; Filter by frequency
        all-phrases (concat bigrams trigrams)
        freq (frequencies all-phrases)
        sorted (sort-by val > freq)]
    (take 10 (map first sorted))))

(defn calculate-sentiment-indicators
  "Calculate basic sentiment indicators."
  [text]
  (let [positive-words #{"good" "great" "excellent" "positive" "success" "benefit" "advantage" "opportunity"}
        negative-words #{"bad" "poor" "negative" "failure" "risk" "problem" "issue" "concern" "danger"}
        words (set (map str/lower-case (str/split text #"\s+")))
        positive-count (count (set/intersection words positive-words))
        negative-count (count (set/intersection words negative-words))
        total (+ positive-count negative-count)]
    {:positive-count positive-count
     :negative-count negative-count
     :sentiment-ratio (if (pos? total)
                        (/ (- positive-count negative-count) total)
                        0)
     :sentiment-label (cond
                        (> positive-count negative-count) :positive
                        (< positive-count negative-count) :negative
                        :else :neutral)}))

(defn enrich-context
  "Enrich context with additional metadata."
  [text entities domains]
  {:word-count (count (str/split text #"\s+"))
   :sentence-count (count (re-seq #"[.!?]+" text))
   :entity-count (count entities)
   :entity-types (frequencies (map :type entities))
   :primary-domain (first domains)
   :all-domains (map :domain-id domains)
   :key-phrases (extract-key-phrases text)
   :sentiment (calculate-sentiment-indicators text)
   :complexity-score (/ (count (filter #(> (count %) 6) (str/split text #"\s+")))
                        (max 1 (count (str/split text #"\s+"))))})

;; =============================================================================
;; CONTEXT ANALYSIS
;; =============================================================================

(defn analyze-context!
  "Perform full context analysis."
  [context-id text & {:keys [source metadata]}]
  (.incrementAndGet ^AtomicLong (:analysis-count @analyzer-state))
  (metrics/inc-counter! :contextanalyzer/analyses)
  (let [entities (extract-all-entities text)
        domains (classify-domain text)
        relationships (extract-relationships text entities)
        entity-graph (build-entity-graph entities relationships)
        enrichment (enrich-context text entities domains)
        context {:id context-id
                 :text-preview (subs text 0 (min 500 (count text)))
                 :text-length (count text)
                 :entities entities
                 :domains domains
                 :relationships relationships
                 :entity-graph entity-graph
                 :enrichment enrichment
                 :source source
                 :metadata metadata
                 :analyzed-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:contexts @analyzer-state) context-id context)
    (log/info "Context analyzed" {:id context-id
                                  :entities (count entities)
                                  :domains (count domains)})
    (events/publish! :context/analyzed {:context-id context-id
                                        :entity-count (count entities)})
    context))

;; =============================================================================
;; CONTEXT RETRIEVAL
;; =============================================================================

(defn get-context
  "Get a context by ID."
  [context-id]
  (.get ^ConcurrentHashMap (:contexts @analyzer-state) context-id))

(defn list-contexts
  "List contexts."
  [& {:keys [domain source limit since]}]
  (let [contexts (vals (:contexts @analyzer-state))]
    (cond->> contexts
      domain (filter #(some (fn [d] (= (:domain-id d) domain)) (:domains %)))
      source (filter #(= (:source %) source))
      since (filter #(>= (:analyzed-at %) since))
      true (sort-by :analyzed-at >)
      limit (take limit))))

(defn find-similar-contexts
  "Find contexts with similar entities."
  [context-id & {:keys [min-similarity limit]}]
  (let [min-sim (or min-similarity 0.3)
        limit (or limit 10)
        target (get-context context-id)
        target-entities (set (map :text (:entities target)))
        contexts (vals (:contexts @analyzer-state))]
    (->> contexts
         (filter #(not= (:id %) context-id))
         (map (fn [ctx]
                (let [ctx-entities (set (map :text (:entities ctx)))
                      intersection (set/intersection target-entities ctx-entities)
                      union (set/union target-entities ctx-entities)
                      similarity (if (pos? (count union))
                                   (/ (count intersection) (count union))
                                   0)]
                  (assoc ctx :similarity similarity))))
         (filter #(>= (:similarity %) min-sim))
         (sort-by :similarity >)
         (take limit))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-analyzer-stats
  "Get analyzer statistics."
  []
  {:contexts (.size ^ConcurrentHashMap (:contexts @analyzer-state))
   :domains (.size ^ConcurrentHashMap (:domains @analyzer-state))
   :analysis-count (.get ^AtomicLong (:analysis-count @analyzer-state))
   :entity-type-distribution (->> (vals (:contexts @analyzer-state))
                                  (mapcat :entities)
                                  (map :type)
                                  frequencies)
   :domain-distribution (->> (vals (:contexts @analyzer-state))
                             (mapcat :domains)
                             (map :domain-id)
                             frequencies)})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-default-domains!
  "Initialize default domains."
  []
  (log/info "Initializing default domains")
  ;; Finance Domain
  (register-domain! "finance"
                    {:name "Finance"
                     :description "Financial and investment context"
                     :keywords ["investment" "stock" "bond" "portfolio" "return" "risk"
                                "market" "dividend" "equity" "asset" "liability" "capital"
                                "profit" "loss" "revenue" "valuation" "price" "value"]})
  ;; Business Domain
  (register-domain! "business"
                    {:name "Business"
                     :description "Business and management context"
                     :keywords ["company" "business" "management" "strategy" "customer"
                                "product" "service" "market" "competition" "growth"
                                "revenue" "profit" "employee" "organization" "leadership"]})
  ;; Psychology Domain
  (register-domain! "psychology"
                    {:name "Psychology"
                     :description "Psychological and behavioral context"
                     :keywords ["behavior" "cognitive" "bias" "emotion" "decision"
                                "thinking" "perception" "motivation" "belief" "attitude"
                                "mental" "psychological" "mind" "brain" "learning"]})
  ;; Technology Domain
  (register-domain! "technology"
                    {:name "Technology"
                     :description "Technology and software context"
                     :keywords ["software" "technology" "system" "data" "algorithm"
                                "computer" "digital" "internet" "platform" "application"
                                "code" "programming" "automation" "artificial" "intelligence"]})
  ;; Science Domain
  (register-domain! "science"
                    {:name "Science"
                     :description "Scientific and research context"
                     :keywords ["research" "study" "experiment" "hypothesis" "theory"
                                "evidence" "data" "analysis" "scientific" "method"
                                "observation" "conclusion" "result" "finding" "discovery"]})
  (log/info "Default domains initialized" {:count 5}))

(defn init-context-analyzer!
  "Initialize context analyzer."
  []
  (log/info "Initializing context analyzer")
  ;; Register feature flag
  (flags/register-flag! "context-analyzer" "Enable context analyzer" true)
  ;; Create metrics
  (metrics/create-counter! :contextanalyzer/analyses "Context analyses performed")
  (metrics/create-gauge! :contextanalyzer/contexts "Total contexts"
                         #(.size ^ConcurrentHashMap (:contexts @analyzer-state)))
  ;; Initialize defaults
  (init-default-domains!)
  (log/info "Context analyzer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-context-analyzer-status []
  {:enabled (flags/is-enabled? "context-analyzer")
   :contexts (.size ^ConcurrentHashMap (:contexts @analyzer-state))
   :domains (.size ^ConcurrentHashMap (:domains @analyzer-state))
   :stats (get-analyzer-stats)
   :config (:config @analyzer-state)})
