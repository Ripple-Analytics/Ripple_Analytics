(ns mental-models.pipeline.integration.document-indexer
  "Document Indexer Module
   
   Document indexing and retrieval:
   - Full-text indexing
   - Metadata indexing
   - Faceted search
   - Relevance ranking
   - Index management"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; INDEXER STATE
;; =============================================================================

(defonce indexer-state (atom {:documents {}
                              :inverted-index {}
                              :metadata-index {}
                              :facets {}
                              :config {:min-term-length 2
                                       :max-results 100
                                       :boost-title 2.0
                                       :boost-recent 1.5}}))

;; =============================================================================
;; TEXT PROCESSING
;; =============================================================================

(defn tokenize
  "Tokenize text into terms."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^a-z0-9\s]" " ")
        (str/split #"\s+")
        (->> (filter #(>= (count %) (get-in @indexer-state [:config :min-term-length])))))))

(defn normalize-term
  "Normalize a search term."
  [term]
  (-> term
      str/lower-case
      str/trim))

;; =============================================================================
;; DOCUMENT INDEXING
;; =============================================================================

(defn calculate-term-frequency
  "Calculate term frequency for a document."
  [tokens]
  (let [total (count tokens)
        freqs (frequencies tokens)]
    (into {} (map (fn [[term count]]
                    [term (/ (double count) total)])
                  freqs))))

(defn index-document!
  "Index a document."
  [doc-id {:keys [title content metadata tags created-at]}]
  (when (flags/is-enabled? "document-indexer")
    (log/info "Indexing document" {:id doc-id})
    (let [title-tokens (tokenize title)
          content-tokens (tokenize content)
          all-tokens (concat title-tokens content-tokens)
          term-freq (calculate-term-frequency all-tokens)
          doc-record {:id doc-id
                      :title title
                      :content-preview (when content (subs content 0 (min 500 (count content))))
                      :metadata metadata
                      :tags (set tags)
                      :created-at (or created-at (System/currentTimeMillis))
                      :indexed-at (System/currentTimeMillis)
                      :term-count (count all-tokens)
                      :term-freq term-freq}]
      ;; Store document
      (swap! indexer-state assoc-in [:documents doc-id] doc-record)
      ;; Update inverted index
      (doseq [[term freq] term-freq]
        (swap! indexer-state update-in [:inverted-index term]
               (fnil conj #{}) {:doc-id doc-id :freq freq :in-title (contains? (set title-tokens) term)}))
      ;; Update metadata index
      (doseq [[k v] metadata]
        (swap! indexer-state update-in [:metadata-index k v]
               (fnil conj #{}) doc-id))
      ;; Update facets
      (doseq [tag tags]
        (swap! indexer-state update-in [:facets :tags tag]
               (fnil inc 0)))
      (metrics/inc-counter! :indexer/documents-indexed)
      (events/publish! :indexer/document-indexed {:doc-id doc-id})
      doc-id)))

(defn remove-document!
  "Remove a document from the index."
  [doc-id]
  (when-let [doc (get-in @indexer-state [:documents doc-id])]
    (log/info "Removing document from index" {:id doc-id})
    ;; Remove from inverted index
    (doseq [[term _] (:term-freq doc)]
      (swap! indexer-state update-in [:inverted-index term]
             (fn [entries] (set (remove #(= (:doc-id %) doc-id) entries)))))
    ;; Remove from metadata index
    (doseq [[k v] (:metadata doc)]
      (swap! indexer-state update-in [:metadata-index k v] disj doc-id))
    ;; Update facets
    (doseq [tag (:tags doc)]
      (swap! indexer-state update-in [:facets :tags tag] dec))
    ;; Remove document
    (swap! indexer-state update :documents dissoc doc-id)
    (metrics/inc-counter! :indexer/documents-removed)))

(defn update-document!
  "Update a document in the index."
  [doc-id doc-data]
  (remove-document! doc-id)
  (index-document! doc-id doc-data))

;; =============================================================================
;; SEARCH
;; =============================================================================

(defn calculate-score
  "Calculate relevance score for a document."
  [doc-id query-terms]
  (let [doc (get-in @indexer-state [:documents doc-id])
        config (:config @indexer-state)
        term-freq (:term-freq doc)
        ;; Calculate base score from term frequency
        base-score (reduce (fn [score term]
                             (+ score (get term-freq term 0)))
                           0.0
                           query-terms)
        ;; Boost for title matches
        title-boost (if (some (fn [term]
                                (some #(and (= (:doc-id %) doc-id) (:in-title %))
                                      (get-in @indexer-state [:inverted-index term])))
                              query-terms)
                      (:boost-title config)
                      1.0)
        ;; Boost for recent documents
        age-days (/ (- (System/currentTimeMillis) (:created-at doc)) 86400000.0)
        recency-boost (if (< age-days 30)
                        (:boost-recent config)
                        1.0)]
    (* base-score title-boost recency-boost)))

(defn search
  "Search the index."
  [query & {:keys [limit offset filters sort-by] :or {limit 10 offset 0}}]
  (when (flags/is-enabled? "document-indexer")
    (log/info "Searching" {:query query})
    (metrics/inc-counter! :indexer/searches)
    (let [query-terms (tokenize query)
          ;; Find matching documents
          matching-docs (reduce (fn [docs term]
                                  (let [term-docs (get-in @indexer-state [:inverted-index term] #{})]
                                    (if (empty? docs)
                                      (set (map :doc-id term-docs))
                                      (clojure.set/intersection docs (set (map :doc-id term-docs))))))
                                #{}
                                query-terms)
          ;; Apply filters
          filtered-docs (if filters
                          (reduce (fn [docs [k v]]
                                    (let [filter-docs (get-in @indexer-state [:metadata-index k v] #{})]
                                      (clojure.set/intersection docs filter-docs)))
                                  matching-docs
                                  filters)
                          matching-docs)
          ;; Score and sort
          scored-docs (map (fn [doc-id]
                             {:doc-id doc-id
                              :score (calculate-score doc-id query-terms)
                              :doc (get-in @indexer-state [:documents doc-id])})
                           filtered-docs)
          sorted-docs (sort-by :score > scored-docs)
          ;; Paginate
          results (->> sorted-docs
                       (drop offset)
                       (take limit))]
      {:total (count filtered-docs)
       :offset offset
       :limit limit
       :results (map (fn [{:keys [doc-id score doc]}]
                       {:id doc-id
                        :score score
                        :title (:title doc)
                        :preview (:content-preview doc)
                        :tags (:tags doc)
                        :created-at (:created-at doc)})
                     results)})))

(defn search-by-tag
  "Search documents by tag."
  [tag & opts]
  (let [doc-ids (for [[doc-id doc] (:documents @indexer-state)
                      :when (contains? (:tags doc) tag)]
                  doc-id)]
    {:total (count doc-ids)
     :results (map (fn [doc-id]
                     (let [doc (get-in @indexer-state [:documents doc-id])]
                       {:id doc-id
                        :title (:title doc)
                        :preview (:content-preview doc)
                        :tags (:tags doc)}))
                   doc-ids)}))

(defn search-by-metadata
  "Search documents by metadata."
  [metadata-key metadata-value]
  (let [doc-ids (get-in @indexer-state [:metadata-index metadata-key metadata-value] #{})]
    {:total (count doc-ids)
     :results (map (fn [doc-id]
                     (let [doc (get-in @indexer-state [:documents doc-id])]
                       {:id doc-id
                        :title (:title doc)
                        :metadata (:metadata doc)}))
                   doc-ids)}))

;; =============================================================================
;; FACETS
;; =============================================================================

(defn get-facets
  "Get facet counts."
  [facet-type]
  (get-in @indexer-state [:facets facet-type] {}))

(defn get-tag-facets
  "Get tag facet counts."
  []
  (get-facets :tags))

;; =============================================================================
;; SUGGESTIONS
;; =============================================================================

(defn get-suggestions
  "Get search suggestions based on prefix."
  [prefix & {:keys [limit] :or {limit 10}}]
  (let [prefix (normalize-term prefix)
        terms (keys (:inverted-index @indexer-state))
        matching (filter #(str/starts-with? % prefix) terms)
        sorted (sort-by #(count (get-in @indexer-state [:inverted-index %])) > matching)]
    (take limit sorted)))

;; =============================================================================
;; INDEX MANAGEMENT
;; =============================================================================

(defn get-document
  "Get a document by ID."
  [doc-id]
  (get-in @indexer-state [:documents doc-id]))

(defn list-documents
  "List all indexed documents."
  [& {:keys [limit offset] :or {limit 100 offset 0}}]
  (let [docs (vals (:documents @indexer-state))
        sorted (sort-by :indexed-at > docs)]
    {:total (count docs)
     :results (->> sorted
                   (drop offset)
                   (take limit)
                   (map #(select-keys % [:id :title :tags :created-at :indexed-at])))}))

(defn clear-index!
  "Clear the entire index."
  []
  (log/info "Clearing index")
  (reset! indexer-state {:documents {}
                         :inverted-index {}
                         :metadata-index {}
                         :facets {}
                         :config (:config @indexer-state)})
  (events/publish! :indexer/index-cleared {}))

(defn rebuild-index!
  "Rebuild the index from documents."
  [documents]
  (log/info "Rebuilding index" {:count (count documents)})
  (clear-index!)
  (doseq [[doc-id doc-data] documents]
    (index-document! doc-id doc-data))
  (events/publish! :indexer/index-rebuilt {:count (count documents)}))

(defn get-index-stats
  "Get index statistics."
  []
  {:document-count (count (:documents @indexer-state))
   :term-count (count (:inverted-index @indexer-state))
   :metadata-fields (count (:metadata-index @indexer-state))
   :tag-count (count (get-in @indexer-state [:facets :tags]))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-document-indexer!
  "Initialize document indexer."
  []
  (log/info "Initializing document indexer")
  ;; Register feature flag
  (flags/register-flag! "document-indexer" "Enable document indexing" true)
  ;; Create metrics
  (metrics/create-counter! :indexer/documents-indexed "Documents indexed")
  (metrics/create-counter! :indexer/documents-removed "Documents removed")
  (metrics/create-counter! :indexer/searches "Searches performed")
  (metrics/create-gauge! :indexer/document-count "Document count"
                         #(count (:documents @indexer-state)))
  (metrics/create-gauge! :indexer/term-count "Term count"
                         #(count (:inverted-index @indexer-state)))
  (log/info "Document indexer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-indexer-status []
  (merge
   {:enabled (flags/is-enabled? "document-indexer")
    :config (:config @indexer-state)}
   (get-index-stats)))
