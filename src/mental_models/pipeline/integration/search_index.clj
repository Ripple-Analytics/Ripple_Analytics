(ns mental-models.pipeline.integration.search-index
  "Search Index Module
   
   Full-text search capabilities for analysis results:
   - Document indexing
   - Full-text search
   - Faceted search
   - Search suggestions
   - Result ranking"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.string :as str]))

;; =============================================================================
;; INDEX STATE
;; =============================================================================

(defonce index-state (atom {:documents {}
                            :inverted-index {}
                            :model-index {}
                            :stats {:total-documents 0
                                    :total-terms 0}}))

;; =============================================================================
;; TEXT PROCESSING
;; =============================================================================

(defn tokenize
  "Tokenize text into terms."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" " ")
        (str/split #"\s+")
        (->> (filter #(> (count %) 2))))))

(defn stem
  "Simple stemming - remove common suffixes."
  [word]
  (-> word
      (str/replace #"ing$" "")
      (str/replace #"ed$" "")
      (str/replace #"ly$" "")
      (str/replace #"tion$" "t")
      (str/replace #"ness$" "")))

(defn normalize-term [term]
  (-> term str/lower-case stem))

(defn extract-terms
  "Extract normalized terms from text."
  [text]
  (->> (tokenize text)
       (map normalize-term)
       (filter #(> (count %) 2))
       frequencies))

;; =============================================================================
;; INDEXING
;; =============================================================================

(defn index-document!
  "Index a document for search."
  [doc-id document]
  (when (flags/is-enabled? "search-index")
    (log/debug "Indexing document" {:doc-id doc-id})
    (let [text (or (:text document) (:content document) "")
          title (or (:title document) (:file-path document) "")
          combined-text (str title " " text)
          terms (extract-terms combined-text)
          models (map :model-id (:detections document))]
      ;; Store document
      (swap! index-state assoc-in [:documents doc-id]
             {:id doc-id
              :title title
              :text-preview (subs text 0 (min 500 (count text)))
              :models models
              :indexed-at (System/currentTimeMillis)
              :term-count (count terms)})
      ;; Update inverted index
      (doseq [[term freq] terms]
        (swap! index-state update-in [:inverted-index term]
               (fnil conj [])
               {:doc-id doc-id :frequency freq}))
      ;; Update model index
      (doseq [model models]
        (swap! index-state update-in [:model-index model]
               (fnil conj #{})
               doc-id))
      ;; Update stats
      (swap! index-state update-in [:stats :total-documents] inc)
      (swap! index-state update-in [:stats :total-terms] + (count terms))
      ;; Record metrics
      (metrics/inc-counter! :search-index/documents-indexed)
      ;; Publish event
      (events/publish! :search/document-indexed {:doc-id doc-id})
      doc-id)))

(defn remove-document!
  "Remove a document from the index."
  [doc-id]
  (when-let [doc (get-in @index-state [:documents doc-id])]
    (log/debug "Removing document from index" {:doc-id doc-id})
    ;; Remove from documents
    (swap! index-state update :documents dissoc doc-id)
    ;; Remove from inverted index
    (swap! index-state update :inverted-index
           (fn [idx]
             (reduce-kv (fn [m term postings]
                          (let [filtered (remove #(= (:doc-id %) doc-id) postings)]
                            (if (empty? filtered)
                              (dissoc m term)
                              (assoc m term filtered))))
                        {}
                        idx)))
    ;; Remove from model index
    (doseq [model (:models doc)]
      (swap! index-state update-in [:model-index model] disj doc-id))
    ;; Update stats
    (swap! index-state update-in [:stats :total-documents] dec)))

;; =============================================================================
;; SEARCHING
;; =============================================================================

(defn calculate-tf-idf
  "Calculate TF-IDF score for a term in a document."
  [term-freq doc-count total-docs]
  (let [tf (Math/log (inc term-freq))
        idf (Math/log (/ total-docs (inc doc-count)))]
    (* tf idf)))

(defn search
  "Search the index for documents matching the query."
  [query & {:keys [limit offset filters]
            :or {limit 20 offset 0 filters {}}}]
  (when (flags/is-enabled? "search-index")
    (log/debug "Searching" {:query query :limit limit})
    (let [start-time (System/currentTimeMillis)
          query-terms (extract-terms query)
          total-docs (get-in @index-state [:stats :total-documents])
          ;; Calculate scores for each document
          doc-scores (reduce
                      (fn [scores [term _]]
                        (let [postings (get-in @index-state [:inverted-index term] [])]
                          (reduce (fn [s {:keys [doc-id frequency]}]
                                    (update s doc-id (fnil + 0)
                                            (calculate-tf-idf frequency (count postings) total-docs)))
                                  scores
                                  postings)))
                      {}
                      query-terms)
          ;; Apply model filter if specified
          filtered-scores (if-let [model-filter (:model filters)]
                            (let [model-docs (get-in @index-state [:model-index model-filter] #{})]
                              (select-keys doc-scores model-docs))
                            doc-scores)
          ;; Sort by score and paginate
          sorted-results (->> filtered-scores
                              (sort-by val >)
                              (drop offset)
                              (take limit))
          ;; Build result objects
          results (map (fn [[doc-id score]]
                         (let [doc (get-in @index-state [:documents doc-id])]
                           (assoc doc :score score)))
                       sorted-results)
          duration (- (System/currentTimeMillis) start-time)]
      ;; Record metrics
      (metrics/inc-counter! :search-index/searches)
      (metrics/observe-histogram! :search-index/search-time duration)
      {:query query
       :total-results (count filtered-scores)
       :returned (count results)
       :offset offset
       :limit limit
       :duration-ms duration
       :results results})))

(defn search-by-model
  "Search for documents containing a specific model."
  [model-id & {:keys [limit] :or {limit 50}}]
  (let [doc-ids (get-in @index-state [:model-index model-id] #{})
        docs (map #(get-in @index-state [:documents %]) doc-ids)]
    {:model-id model-id
     :total (count docs)
     :results (take limit docs)}))

;; =============================================================================
;; SUGGESTIONS
;; =============================================================================

(defn get-suggestions
  "Get search suggestions based on partial query."
  [partial-query & {:keys [limit] :or {limit 10}}]
  (let [prefix (normalize-term partial-query)
        all-terms (keys (:inverted-index @index-state))
        matching (filter #(str/starts-with? % prefix) all-terms)]
    (->> matching
         (sort-by #(count (get-in @index-state [:inverted-index %])) >)
         (take limit))))

;; =============================================================================
;; FACETS
;; =============================================================================

(defn get-model-facets
  "Get facet counts for models in search results."
  [query]
  (let [search-results (search query :limit 1000)
        model-counts (frequencies
                      (mapcat :models (:results search-results)))]
    {:query query
     :facets (sort-by val > model-counts)}))

;; =============================================================================
;; INDEX MANAGEMENT
;; =============================================================================

(defn clear-index! []
  (log/warn "Clearing search index")
  (reset! index-state {:documents {}
                       :inverted-index {}
                       :model-index {}
                       :stats {:total-documents 0
                               :total-terms 0}}))

(defn rebuild-index!
  "Rebuild index from a collection of documents."
  [documents]
  (log/info "Rebuilding search index" {:document-count (count documents)})
  (clear-index!)
  (doseq [[doc-id doc] documents]
    (index-document! doc-id doc))
  (log/info "Index rebuilt" {:total-documents (get-in @index-state [:stats :total-documents])}))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-event-handlers! []
  (log/info "Setting up search index event handlers")
  (events/subscribe! :analysis/complete
                     (fn [result]
                       (index-document! (or (:id result) (str (java.util.UUID/randomUUID))) result))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-search-index!
  "Initialize search index."
  []
  (log/info "Initializing search index")
  ;; Register feature flag
  (flags/register-flag! "search-index" "Enable search index" true)
  ;; Create metrics
  (metrics/create-counter! :search-index/documents-indexed "Documents indexed")
  (metrics/create-counter! :search-index/searches "Searches performed")
  (metrics/create-histogram! :search-index/search-time "Search time" [5 10 50 100 500])
  (metrics/create-gauge! :search-index/document-count "Indexed documents"
                         #(get-in @index-state [:stats :total-documents]))
  ;; Setup event handlers
  (setup-event-handlers!)
  (log/info "Search index initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-index-status []
  {:enabled (flags/is-enabled? "search-index")
   :total-documents (get-in @index-state [:stats :total-documents])
   :total-terms (get-in @index-state [:stats :total-terms])
   :unique-terms (count (:inverted-index @index-state))
   :indexed-models (count (:model-index @index-state))})
