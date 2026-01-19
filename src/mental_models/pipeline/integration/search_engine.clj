(ns mental-models.pipeline.integration.search-engine
  "Search engine for mental model analysis content.
   
   Features:
   - Full-text search
   - Inverted index
   - Fuzzy matching
   - Faceted search
   - Relevance scoring
   - Query parsing
   - Highlighting
   - Autocomplete"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
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
  (atom {:indexes {}          ;; index-id -> index-config
         :documents {}        ;; {index-id doc-id} -> document
         :inverted-index {}   ;; {index-id term} -> doc-ids
         :doc-vectors {}      ;; {index-id doc-id} -> term-frequencies
         :facets {}           ;; {index-id field} -> {value -> count}
         :suggestions {}      ;; prefix -> suggestions
         :stats {:searches 0 :documents-indexed 0}
         :initialized? false}))

;; ============================================================================
;; Index Management
;; ============================================================================

(defn create-index!
  "Create a search index."
  [index-id config]
  (let [index {:id index-id
               :name (get config :name (name index-id))
               :description (get config :description "")
               :fields (get config :fields [])
               :analyzers (get config :analyzers {:default :standard})
               :settings (get config :settings {:max-results 100})
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:indexes index-id] index)
    (logging/log :info "Created index" {:index-id index-id})
    (events/emit! :index-created {:index-id index-id})
    index-id))

(defn get-index
  "Get an index."
  [index-id]
  (get-in @state [:indexes index-id]))

(defn list-indexes
  "List all indexes."
  []
  (mapv (fn [[id idx]]
          {:id id
           :name (:name idx)
           :document-count (count (filter (fn [[k _]] (= (first k) id))
                                          (:documents @state)))})
        (:indexes @state)))

(defn delete-index!
  "Delete an index."
  [index-id]
  ;; Remove all documents and index data
  (swap! state update :documents (fn [docs]
                                   (into {} (filter (fn [[k _]] (not= (first k) index-id)) docs))))
  (swap! state update :inverted-index (fn [idx]
                                        (into {} (filter (fn [[k _]] (not= (first k) index-id)) idx))))
  (swap! state update :doc-vectors (fn [vecs]
                                     (into {} (filter (fn [[k _]] (not= (first k) index-id)) vecs))))
  (swap! state update :facets (fn [fac]
                                (into {} (filter (fn [[k _]] (not= (first k) index-id)) fac))))
  (swap! state update :indexes dissoc index-id)
  (logging/log :info "Deleted index" {:index-id index-id}))

;; ============================================================================
;; Text Analysis
;; ============================================================================

(defn- tokenize
  "Tokenize text into terms."
  [text]
  (when text
    (->> (str/lower-case text)
         (re-seq #"\w+")
         (filter #(> (count %) 1)))))

(defn- stem
  "Simple stemming (remove common suffixes)."
  [word]
  (cond
    (str/ends-with? word "ing") (subs word 0 (- (count word) 3))
    (str/ends-with? word "ed") (subs word 0 (- (count word) 2))
    (str/ends-with? word "ly") (subs word 0 (- (count word) 2))
    (str/ends-with? word "es") (subs word 0 (- (count word) 2))
    (str/ends-with? word "s") (subs word 0 (- (count word) 1))
    :else word))

(defn- analyze
  "Analyze text into normalized terms."
  [text & {:keys [analyzer] :or {analyzer :standard}}]
  (case analyzer
    :standard (->> (tokenize text)
                   (map stem)
                   (filter #(> (count %) 1)))
    :keyword [text]
    :whitespace (str/split (str/lower-case (or text "")) #"\s+")
    (tokenize text)))

(defn- compute-term-frequencies
  "Compute term frequencies for a document."
  [terms]
  (frequencies terms))

(defn- compute-tf-idf
  "Compute TF-IDF score for a term in a document."
  [term doc-tf total-docs docs-with-term]
  (let [tf (get doc-tf term 0)
        idf (Math/log (/ (inc total-docs) (inc docs-with-term)))]
    (* tf idf)))

;; ============================================================================
;; Document Indexing
;; ============================================================================

(defn index-document!
  "Index a document."
  [index-id doc-id document]
  (when (flags/enabled? :search-engine)
    (let [index (get-index index-id)
          fields (:fields index)
          ;; Extract text from all searchable fields
          text (str/join " " (map #(get document % "") fields))
          terms (analyze text)
          term-freqs (compute-term-frequencies terms)]
      
      ;; Store document
      (swap! state assoc-in [:documents [index-id doc-id]]
             (assoc document :_id doc-id :_indexed-at (System/currentTimeMillis)))
      
      ;; Update inverted index
      (doseq [term (distinct terms)]
        (swap! state update-in [:inverted-index [index-id term]]
               (fnil conj #{}) doc-id))
      
      ;; Store document vector
      (swap! state assoc-in [:doc-vectors [index-id doc-id]] term-freqs)
      
      ;; Update facets
      (doseq [[field value] document]
        (when (and (keyword? field) (or (string? value) (keyword? value)))
          (swap! state update-in [:facets [index-id field] (str value)]
                 (fnil inc 0))))
      
      ;; Update suggestions
      (doseq [term terms]
        (doseq [i (range 1 (min 6 (count term)))]
          (let [prefix (subs term 0 i)]
            (swap! state update-in [:suggestions prefix]
                   (fnil conj #{}) term))))
      
      (swap! state update-in [:stats :documents-indexed] inc)
      (metrics/increment :documents-indexed {:index-id index-id})
      (logging/log :debug "Indexed document" {:index-id index-id :doc-id doc-id})
      doc-id)))

(defn index-documents!
  "Index multiple documents."
  [index-id documents]
  (doseq [[doc-id doc] documents]
    (index-document! index-id doc-id doc))
  (count documents))

(defn delete-document!
  "Delete a document from the index."
  [index-id doc-id]
  (when-let [doc (get-in @state [:documents [index-id doc-id]])]
    ;; Remove from inverted index
    (let [term-freqs (get-in @state [:doc-vectors [index-id doc-id]] {})]
      (doseq [term (keys term-freqs)]
        (swap! state update-in [:inverted-index [index-id term]] disj doc-id)))
    ;; Remove document and vector
    (swap! state update :documents dissoc [index-id doc-id])
    (swap! state update :doc-vectors dissoc [index-id doc-id])
    (logging/log :debug "Deleted document" {:index-id index-id :doc-id doc-id})))

(defn get-document
  "Get a document by ID."
  [index-id doc-id]
  (get-in @state [:documents [index-id doc-id]]))

;; ============================================================================
;; Search
;; ============================================================================

(defn- score-document
  "Score a document for a query."
  [index-id doc-id query-terms]
  (let [doc-tf (get-in @state [:doc-vectors [index-id doc-id]] {})
        total-docs (count (filter (fn [[k _]] (= (first k) index-id)) (:documents @state)))
        scores (for [term query-terms]
                 (let [docs-with-term (count (get-in @state [:inverted-index [index-id term]] #{}))]
                   (compute-tf-idf term doc-tf total-docs docs-with-term)))]
    (reduce + scores)))

(defn- highlight-text
  "Highlight matching terms in text."
  [text terms & {:keys [pre-tag post-tag] :or {pre-tag "<em>" post-tag "</em>"}}]
  (reduce (fn [t term]
            (str/replace t (re-pattern (str "(?i)\\b" term "\\w*\\b"))
                         (str pre-tag "$0" post-tag)))
          text
          terms))

(defn search
  "Search the index."
  [index-id query & {:keys [limit offset filters facet-fields highlight?]
                     :or {limit 10 offset 0 highlight? false}}]
  (when (flags/enabled? :search-engine)
    (let [start-time (System/currentTimeMillis)
          query-terms (analyze query)
          
          ;; Find matching documents
          matching-doc-ids (if (empty? query-terms)
                             ;; Return all documents if no query
                             (set (map second (filter (fn [[k _]] (= (first k) index-id))
                                                      (keys (:documents @state)))))
                             ;; Find documents containing any query term
                             (reduce set/union
                                     (map #(get-in @state [:inverted-index [index-id %]] #{})
                                          query-terms)))
          
          ;; Apply filters
          filtered-ids (if filters
                         (filter (fn [doc-id]
                                   (let [doc (get-document index-id doc-id)]
                                     (every? (fn [[field value]]
                                               (= (get doc field) value))
                                             filters)))
                                 matching-doc-ids)
                         matching-doc-ids)
          
          ;; Score and sort
          scored-docs (map (fn [doc-id]
                             {:doc-id doc-id
                              :score (score-document index-id doc-id query-terms)
                              :document (get-document index-id doc-id)})
                           filtered-ids)
          sorted-docs (sort-by :score > scored-docs)
          
          ;; Paginate
          paginated (take limit (drop offset sorted-docs))
          
          ;; Add highlighting if requested
          results (if highlight?
                    (mapv (fn [result]
                            (let [doc (:document result)
                                  index (get-index index-id)
                                  highlighted (reduce (fn [d field]
                                                        (if-let [text (get d field)]
                                                          (assoc d (keyword (str (name field) "_highlighted"))
                                                                 (highlight-text (str text) query-terms))
                                                          d))
                                                      doc
                                                      (:fields index))]
                              (assoc result :document highlighted)))
                          paginated)
                    (vec paginated))
          
          ;; Compute facets if requested
          facets (when facet-fields
                   (into {}
                         (map (fn [field]
                                [field (get-in @state [:facets [index-id field]] {})])
                              facet-fields)))
          
          latency (- (System/currentTimeMillis) start-time)]
      
      (swap! state update-in [:stats :searches] inc)
      (metrics/increment :searches {:index-id index-id})
      (metrics/histogram :search-latency {:index-id index-id} latency)
      
      {:hits results
       :total (count filtered-ids)
       :offset offset
       :limit limit
       :facets facets
       :took-ms latency})))

;; ============================================================================
;; Fuzzy Search
;; ============================================================================

(defn- levenshtein-distance
  "Calculate Levenshtein distance between two strings."
  [s1 s2]
  (let [len1 (count s1)
        len2 (count s2)]
    (cond
      (zero? len1) len2
      (zero? len2) len1
      :else
      (let [matrix (vec (for [i (range (inc len1))]
                          (vec (for [j (range (inc len2))]
                                 (cond
                                   (zero? i) j
                                   (zero? j) i
                                   :else 0)))))]
        (loop [i 1
               m matrix]
          (if (> i len1)
            (get-in m [len1 len2])
            (recur (inc i)
                   (loop [j 1
                          m2 m]
                     (if (> j len2)
                       m2
                       (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)
                             val (min (inc (get-in m2 [(dec i) j]))
                                      (inc (get-in m2 [i (dec j)]))
                                      (+ (get-in m2 [(dec i) (dec j)]) cost))]
                         (recur (inc j) (assoc-in m2 [i j] val))))))))))))

(defn fuzzy-search
  "Search with fuzzy matching."
  [index-id query & {:keys [fuzziness limit] :or {fuzziness 2 limit 10}}]
  (let [query-terms (analyze query)
        all-terms (set (map first (filter (fn [[k _]] (= (first k) index-id))
                                          (keys (:inverted-index @state)))))
        ;; Find similar terms
        fuzzy-terms (for [qt query-terms
                         t all-terms
                         :when (<= (levenshtein-distance qt t) fuzziness)]
                      t)
        expanded-query (str/join " " (concat query-terms fuzzy-terms))]
    (search index-id expanded-query :limit limit)))

;; ============================================================================
;; Autocomplete
;; ============================================================================

(defn autocomplete
  "Get autocomplete suggestions."
  [prefix & {:keys [limit] :or {limit 10}}]
  (let [prefix-lower (str/lower-case prefix)
        suggestions (get-in @state [:suggestions prefix-lower] #{})]
    (take limit (sort suggestions))))

(defn suggest
  "Get search suggestions based on partial query."
  [index-id partial-query & {:keys [limit] :or {limit 5}}]
  (let [terms (analyze partial-query)
        last-term (last terms)
        completions (when last-term (autocomplete last-term :limit limit))
        prefix (str/join " " (butlast terms))]
    (mapv (fn [completion]
            (str/trim (str prefix " " completion)))
          completions)))

;; ============================================================================
;; Aggregations
;; ============================================================================

(defn aggregate
  "Perform aggregations on search results."
  [index-id query aggregations]
  (let [search-results (search index-id query :limit 10000)
        docs (map :document (:hits search-results))
        results (into {}
                      (map (fn [[agg-name agg-config]]
                             (let [field (:field agg-config)
                                   agg-type (:type agg-config :terms)]
                               [agg-name
                                (case agg-type
                                  :terms (frequencies (map #(get % field) docs))
                                  :sum (reduce + (filter number? (map #(get % field) docs)))
                                  :avg (let [nums (filter number? (map #(get % field) docs))]
                                         (when (seq nums)
                                           (/ (reduce + nums) (count nums))))
                                  :min (when-let [nums (seq (filter number? (map #(get % field) docs)))]
                                         (apply min nums))
                                  :max (when-let [nums (seq (filter number? (map #(get % field) docs)))]
                                         (apply max nums))
                                  :count (count docs)
                                  nil)]))
                           aggregations))]
    {:aggregations results
     :total (:total search-results)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-engine-stats
  "Get search engine statistics."
  []
  (let [stats (:stats @state)]
    {:total-indexes (count (:indexes @state))
     :total-documents (count (:documents @state))
     :total-terms (count (:inverted-index @state))
     :total-searches (:searches stats)
     :documents-indexed (:documents-indexed stats)}))

(defn get-index-stats
  "Get statistics for an index."
  [index-id]
  (let [docs (filter (fn [[k _]] (= (first k) index-id)) (:documents @state))
        terms (filter (fn [[k _]] (= (first k) index-id)) (:inverted-index @state))]
    {:index-id index-id
     :document-count (count docs)
     :term-count (count terms)
     :avg-doc-length (when (seq docs)
                       (/ (reduce + (map #(count (keys (get-in @state [:doc-vectors (key %)])))
                                         docs))
                          (count docs)))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-search-engine!
  "Initialize the search engine."
  []
  (when-not (:initialized? @state)
    ;; Create default indexes
    (create-index! :documents
                   {:name "Documents"
                    :description "Document search index"
                    :fields [:title :content :summary]})
    
    (create-index! :mental-models
                   {:name "Mental Models"
                    :description "Mental model search index"
                    :fields [:name :description :category :examples]})
    
    (create-index! :analyses
                   {:name "Analyses"
                    :description "Analysis results search index"
                    :fields [:summary :insights :models]})
    
    ;; Index some sample mental models
    (index-document! :mental-models :confirmation-bias
                     {:name "Confirmation Bias"
                      :description "The tendency to search for, interpret, favor, and recall information that confirms one's preexisting beliefs"
                      :category "Cognitive Bias"
                      :examples "Seeking news that aligns with political views"})
    
    (index-document! :mental-models :anchoring
                     {:name "Anchoring"
                      :description "The tendency to rely too heavily on the first piece of information encountered"
                      :category "Cognitive Bias"
                      :examples "Initial price in negotiations"})
    
    (index-document! :mental-models :availability-heuristic
                     {:name "Availability Heuristic"
                      :description "Overweighting information that comes to mind easily"
                      :category "Cognitive Bias"
                      :examples "Overestimating plane crash risk after news coverage"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Search engine initialized")
    (events/emit! :search-engine-initialized {})
    true))
