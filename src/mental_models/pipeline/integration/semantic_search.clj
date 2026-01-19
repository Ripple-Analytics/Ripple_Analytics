(ns mental-models.pipeline.integration.semantic-search
  "Semantic search using embeddings for mental model analysis.
   
   Features:
   - Vector embeddings
   - Similarity search
   - Hybrid search (keyword + semantic)
   - Clustering
   - Dimensionality reduction
   - Nearest neighbor search
   - Semantic deduplication
   - Cross-encoder reranking"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:collections {}      ;; collection-id -> collection-config
         :vectors {}          ;; {collection-id doc-id} -> vector
         :documents {}        ;; {collection-id doc-id} -> document
         :indexes {}          ;; collection-id -> index-structure
         :models {}           ;; model-id -> model-config
         :stats {:searches 0 :embeddings-generated 0}
         :initialized? false}))

;; ============================================================================
;; Vector Operations
;; ============================================================================

(defn- dot-product
  "Calculate dot product of two vectors."
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn- magnitude
  "Calculate magnitude of a vector."
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn- cosine-similarity
  "Calculate cosine similarity between two vectors."
  [v1 v2]
  (let [dot (dot-product v1 v2)
        mag1 (magnitude v1)
        mag2 (magnitude v2)]
    (if (or (zero? mag1) (zero? mag2))
      0.0
      (/ dot (* mag1 mag2)))))

(defn- euclidean-distance
  "Calculate Euclidean distance between two vectors."
  [v1 v2]
  (Math/sqrt (reduce + (map #(Math/pow (- %1 %2) 2) v1 v2))))

(defn- normalize-vector
  "Normalize a vector to unit length."
  [v]
  (let [mag (magnitude v)]
    (if (zero? mag)
      v
      (mapv #(/ % mag) v))))

;; ============================================================================
;; Embedding Generation
;; ============================================================================

(defn- generate-embedding
  "Generate an embedding for text (simplified - would use actual model in production)."
  [text & {:keys [dimensions] :or {dimensions 384}}]
  (let [;; Simple hash-based embedding for demonstration
        hash-val (hash text)
        random (Random. hash-val)]
    (vec (repeatedly dimensions #(- (* 2 (.nextDouble random)) 1)))))

(defn embed-text
  "Generate embedding for text."
  [text & {:keys [model-id] :or {model-id :default}}]
  (when (flags/enabled? :semantic-search)
    (let [embedding (generate-embedding text)]
      (swap! state update-in [:stats :embeddings-generated] inc)
      (normalize-vector embedding))))

(defn embed-batch
  "Generate embeddings for multiple texts."
  [texts & {:keys [model-id] :or {model-id :default}}]
  (mapv #(embed-text % :model-id model-id) texts))

;; ============================================================================
;; Collection Management
;; ============================================================================

(defn create-collection!
  "Create a vector collection."
  [collection-id config]
  (let [collection {:id collection-id
                    :name (get config :name (name collection-id))
                    :description (get config :description "")
                    :dimensions (get config :dimensions 384)
                    :distance-metric (get config :distance-metric :cosine)
                    :index-type (get config :index-type :flat)
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:collections collection-id] collection)
    (logging/log :info "Created collection" {:collection-id collection-id})
    (events/emit! :collection-created {:collection-id collection-id})
    collection-id))

(defn get-collection
  "Get a collection."
  [collection-id]
  (get-in @state [:collections collection-id]))

(defn list-collections
  "List all collections."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :dimensions (:dimensions c)
           :vector-count (count (filter (fn [[k _]] (= (first k) id))
                                        (:vectors @state)))})
        (:collections @state)))

(defn delete-collection!
  "Delete a collection."
  [collection-id]
  (swap! state update :vectors (fn [vecs]
                                 (into {} (filter (fn [[k _]] (not= (first k) collection-id)) vecs))))
  (swap! state update :documents (fn [docs]
                                   (into {} (filter (fn [[k _]] (not= (first k) collection-id)) docs))))
  (swap! state update :collections dissoc collection-id)
  (logging/log :info "Deleted collection" {:collection-id collection-id}))

;; ============================================================================
;; Vector Storage
;; ============================================================================

(defn upsert-vector!
  "Insert or update a vector."
  [collection-id doc-id vector & {:keys [metadata]}]
  (when (flags/enabled? :semantic-search)
    (let [collection (get-collection collection-id)
          normalized (normalize-vector vector)]
      (swap! state assoc-in [:vectors [collection-id doc-id]] normalized)
      (swap! state assoc-in [:documents [collection-id doc-id]]
             (merge metadata {:_id doc-id :_indexed-at (System/currentTimeMillis)}))
      (logging/log :debug "Upserted vector" {:collection-id collection-id :doc-id doc-id})
      doc-id)))

(defn upsert-text!
  "Embed and store text."
  [collection-id doc-id text & {:keys [metadata]}]
  (let [embedding (embed-text text)]
    (upsert-vector! collection-id doc-id embedding
                    :metadata (merge metadata {:text text}))))

(defn upsert-batch!
  "Upsert multiple vectors."
  [collection-id items]
  (doseq [{:keys [id vector text metadata]} items]
    (if vector
      (upsert-vector! collection-id id vector :metadata metadata)
      (upsert-text! collection-id id text :metadata metadata)))
  (count items))

(defn get-vector
  "Get a vector by ID."
  [collection-id doc-id]
  (get-in @state [:vectors [collection-id doc-id]]))

(defn get-document
  "Get a document by ID."
  [collection-id doc-id]
  (get-in @state [:documents [collection-id doc-id]]))

(defn delete-vector!
  "Delete a vector."
  [collection-id doc-id]
  (swap! state update :vectors dissoc [collection-id doc-id])
  (swap! state update :documents dissoc [collection-id doc-id]))

;; ============================================================================
;; Similarity Search
;; ============================================================================

(defn search-by-vector
  "Search for similar vectors."
  [collection-id query-vector & {:keys [limit filter-fn include-vectors?]
                                  :or {limit 10 include-vectors? false}}]
  (when (flags/enabled? :semantic-search)
    (let [start-time (System/currentTimeMillis)
          collection (get-collection collection-id)
          distance-fn (case (:distance-metric collection :cosine)
                        :cosine (fn [v1 v2] (- 1 (cosine-similarity v1 v2)))
                        :euclidean euclidean-distance
                        :dot (fn [v1 v2] (- (dot-product v1 v2))))
          normalized-query (normalize-vector query-vector)
          
          ;; Get all vectors in collection
          collection-vectors (filter (fn [[k _]] (= (first k) collection-id))
                                     (:vectors @state))
          
          ;; Calculate distances
          scored (for [[k v] collection-vectors
                       :let [doc-id (second k)
                             doc (get-document collection-id doc-id)]
                       :when (or (nil? filter-fn) (filter-fn doc))]
                   {:doc-id doc-id
                    :score (- 1 (distance-fn normalized-query v))
                    :document doc
                    :vector (when include-vectors? v)})
          
          ;; Sort by score (higher is better)
          sorted (sort-by :score > scored)
          results (take limit sorted)
          latency (- (System/currentTimeMillis) start-time)]
      
      (swap! state update-in [:stats :searches] inc)
      (metrics/increment :semantic-searches {:collection-id collection-id})
      (metrics/histogram :semantic-search-latency {:collection-id collection-id} latency)
      
      {:hits (vec results)
       :total (count scored)
       :took-ms latency})))

(defn search-by-text
  "Search using text query."
  [collection-id query-text & {:keys [limit filter-fn] :or {limit 10}}]
  (let [query-vector (embed-text query-text)]
    (search-by-vector collection-id query-vector :limit limit :filter-fn filter-fn)))

;; ============================================================================
;; Hybrid Search
;; ============================================================================

(defn hybrid-search
  "Combine keyword and semantic search."
  [collection-id query & {:keys [limit alpha] :or {limit 10 alpha 0.5}}]
  (let [;; Semantic search
        semantic-results (search-by-text collection-id query :limit (* 2 limit))
        semantic-scores (into {} (map (fn [r] [(:doc-id r) (:score r)])
                                      (:hits semantic-results)))
        
        ;; Simple keyword matching
        query-terms (set (str/split (str/lower-case query) #"\s+"))
        keyword-scores (into {}
                             (for [[k doc] (:documents @state)
                                   :when (= (first k) collection-id)
                                   :let [doc-id (second k)
                                         text (str/lower-case (str (:text doc) " " (:title doc "")))
                                         matches (count (filter #(str/includes? text %) query-terms))
                                         score (/ matches (max 1 (count query-terms)))]]
                               [doc-id score]))
        
        ;; Combine scores
        all-doc-ids (set (concat (keys semantic-scores) (keys keyword-scores)))
        combined (for [doc-id all-doc-ids]
                   (let [sem-score (get semantic-scores doc-id 0)
                         kw-score (get keyword-scores doc-id 0)
                         combined-score (+ (* alpha sem-score) (* (- 1 alpha) kw-score))]
                     {:doc-id doc-id
                      :score combined-score
                      :semantic-score sem-score
                      :keyword-score kw-score
                      :document (get-document collection-id doc-id)}))
        sorted (take limit (sort-by :score > combined))]
    
    {:hits (vec sorted)
     :total (count all-doc-ids)}))

;; ============================================================================
;; Clustering
;; ============================================================================

(defn- kmeans-init
  "Initialize k-means centroids."
  [vectors k]
  (vec (take k (shuffle vectors))))

(defn- assign-clusters
  "Assign vectors to nearest centroids."
  [vectors centroids]
  (mapv (fn [v]
          (let [distances (map-indexed (fn [i c] [i (euclidean-distance v c)]) centroids)
                [cluster-id _] (apply min-key second distances)]
            cluster-id))
        vectors))

(defn- update-centroids
  "Update centroids based on cluster assignments."
  [vectors assignments k dimensions]
  (vec (for [cluster-id (range k)]
         (let [cluster-vectors (map first (filter #(= (second %) cluster-id)
                                                  (map vector vectors assignments)))]
           (if (empty? cluster-vectors)
             (vec (repeat dimensions 0.0))
             (let [n (count cluster-vectors)]
               (mapv (fn [i]
                       (/ (reduce + (map #(nth % i) cluster-vectors)) n))
                     (range dimensions))))))))

(defn cluster-vectors
  "Cluster vectors using k-means."
  [collection-id k & {:keys [max-iterations] :or {max-iterations 100}}]
  (let [vectors (vec (map val (filter (fn [[key _]] (= (first key) collection-id))
                                      (:vectors @state))))
        dimensions (count (first vectors))
        initial-centroids (kmeans-init vectors k)]
    (loop [centroids initial-centroids
           iteration 0]
      (if (>= iteration max-iterations)
        {:centroids centroids
         :assignments (assign-clusters vectors centroids)
         :iterations iteration}
        (let [assignments (assign-clusters vectors centroids)
              new-centroids (update-centroids vectors assignments k dimensions)]
          (if (= centroids new-centroids)
            {:centroids centroids
             :assignments assignments
             :iterations iteration}
            (recur new-centroids (inc iteration))))))))

;; ============================================================================
;; Deduplication
;; ============================================================================

(defn find-duplicates
  "Find semantically similar documents (potential duplicates)."
  [collection-id & {:keys [threshold] :or {threshold 0.95}}]
  (let [vectors (filter (fn [[k _]] (= (first k) collection-id)) (:vectors @state))
        vector-list (vec vectors)
        n (count vector-list)
        duplicates (for [i (range n)
                         j (range (inc i) n)
                         :let [[k1 v1] (nth vector-list i)
                               [k2 v2] (nth vector-list j)
                               similarity (cosine-similarity v1 v2)]
                         :when (>= similarity threshold)]
                     {:doc1 (second k1)
                      :doc2 (second k2)
                      :similarity similarity})]
    {:duplicates (vec duplicates)
     :count (count duplicates)}))

(defn deduplicate!
  "Remove duplicate documents from collection."
  [collection-id & {:keys [threshold keep-strategy] :or {threshold 0.95 keep-strategy :first}}]
  (let [{:keys [duplicates]} (find-duplicates collection-id :threshold threshold)
        to-remove (case keep-strategy
                    :first (set (map :doc2 duplicates))
                    :last (set (map :doc1 duplicates))
                    #{})]
    (doseq [doc-id to-remove]
      (delete-vector! collection-id doc-id))
    {:removed (count to-remove)}))

;; ============================================================================
;; Reranking
;; ============================================================================

(defn rerank
  "Rerank search results using cross-encoder style scoring."
  [query results & {:keys [limit] :or {limit 10}}]
  (let [query-embedding (embed-text query)
        reranked (for [result results]
                   (let [doc-text (or (get-in result [:document :text]) "")
                         doc-embedding (embed-text doc-text)
                         ;; Simple reranking based on query-document similarity
                         rerank-score (* (cosine-similarity query-embedding doc-embedding)
                                         (:score result 1.0))]
                     (assoc result :rerank-score rerank-score)))
        sorted (take limit (sort-by :rerank-score > reranked))]
    (vec sorted)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-search-stats
  "Get semantic search statistics."
  []
  (let [stats (:stats @state)]
    {:total-collections (count (:collections @state))
     :total-vectors (count (:vectors @state))
     :total-searches (:searches stats)
     :embeddings-generated (:embeddings-generated stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-semantic-search!
  "Initialize semantic search."
  []
  (when-not (:initialized? @state)
    ;; Create default collections
    (create-collection! :documents
                        {:name "Documents"
                         :description "Document embeddings"
                         :dimensions 384
                         :distance-metric :cosine})
    
    (create-collection! :mental-models
                        {:name "Mental Models"
                         :description "Mental model embeddings"
                         :dimensions 384
                         :distance-metric :cosine})
    
    ;; Index some sample mental models
    (upsert-text! :mental-models :confirmation-bias
                  "Confirmation bias is the tendency to search for, interpret, favor, and recall information that confirms one's preexisting beliefs"
                  :metadata {:name "Confirmation Bias" :category "Cognitive Bias"})
    
    (upsert-text! :mental-models :anchoring
                  "Anchoring is the tendency to rely too heavily on the first piece of information encountered when making decisions"
                  :metadata {:name "Anchoring" :category "Cognitive Bias"})
    
    (upsert-text! :mental-models :availability-heuristic
                  "The availability heuristic is a mental shortcut that relies on immediate examples that come to mind"
                  :metadata {:name "Availability Heuristic" :category "Cognitive Bias"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Semantic search initialized")
    (events/emit! :semantic-search-initialized {})
    true))
