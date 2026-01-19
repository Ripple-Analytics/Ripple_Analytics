(ns mental-models.pipeline.integration.embedding-service
  "Text embedding service for semantic similarity and vector search.
   
   Features:
   - Multiple embedding model support (local and API-based)
   - Batch embedding generation
   - Embedding caching for efficiency
   - Similarity search with cosine/euclidean distance
   - Dimensionality reduction (PCA, t-SNE placeholders)
   - Embedding clustering
   - Vector database integration
   - Embedding visualization support"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
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
  (atom {:models {}           ;; model-id -> model-config
         :embeddings {}       ;; text-hash -> embedding
         :indices {}          ;; index-id -> vector-index
         :clusters {}         ;; cluster-id -> cluster-data
         :cache-stats {:hits 0 :misses 0}
         :initialized? false}))

;; ============================================================================
;; Embedding Models
;; ============================================================================

(defn register-model!
  "Register an embedding model."
  [model-id config]
  (let [model {:id model-id
               :name (get config :name (name model-id))
               :type (get config :type :local)
               :dimensions (get config :dimensions 384)
               :max-tokens (get config :max-tokens 512)
               :endpoint (get config :endpoint nil)
               :api-key (get config :api-key nil)
               :batch-size (get config :batch-size 32)
               :normalize? (get config :normalize? true)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:models model-id] model)
    (logging/log :info "Registered embedding model" {:model-id model-id})
    (events/emit! :embedding-model-registered {:model-id model-id})
    model-id))

(defn get-model
  "Get an embedding model configuration."
  [model-id]
  (get-in @state [:models model-id]))

(defn list-models
  "List all registered embedding models."
  []
  (mapv (fn [[id m]]
          {:id id
           :name (:name m)
           :type (:type m)
           :dimensions (:dimensions m)})
        (:models @state)))

;; ============================================================================
;; Embedding Generation
;; ============================================================================

(defn- text-hash
  "Generate a hash for text content."
  [text model-id]
  (str model-id ":" (hash text)))

(defn- generate-random-embedding
  "Generate a random embedding vector (placeholder for actual model)."
  [dimensions]
  (let [raw (repeatedly dimensions #(- (rand) 0.5))
        magnitude (Math/sqrt (reduce + (map #(* % %) raw)))]
    (mapv #(/ % magnitude) raw)))

(defn- call-embedding-api
  "Call external embedding API (placeholder)."
  [model text]
  ;; Placeholder: actual API call would happen here
  (generate-random-embedding (:dimensions model)))

(defn- generate-local-embedding
  "Generate embedding using local model (placeholder)."
  [model text]
  ;; Placeholder: actual local model inference would happen here
  (generate-random-embedding (:dimensions model)))

(defn embed-text
  "Generate embedding for a single text."
  [model-id text & {:keys [use-cache?] :or {use-cache? true}}]
  (when (flags/enabled? :embedding-service)
    (let [model (get-model model-id)
          cache-key (text-hash text model-id)]
      
      ;; Check cache first
      (if-let [cached (and use-cache? (get-in @state [:embeddings cache-key]))]
        (do
          (swap! state update-in [:cache-stats :hits] inc)
          (metrics/increment :embedding-cache-hits {:model-id model-id})
          (:embedding cached))
        (do
          (swap! state update-in [:cache-stats :misses] inc)
          (metrics/increment :embedding-cache-misses {:model-id model-id})
          
          (let [embedding (case (:type model)
                            :api (call-embedding-api model text)
                            :local (generate-local-embedding model text)
                            (generate-random-embedding (:dimensions model)))
                normalized (if (:normalize? model)
                             (let [mag (Math/sqrt (reduce + (map #(* % %) embedding)))]
                               (mapv #(/ % mag) embedding))
                             embedding)]
            
            ;; Cache the embedding
            (when use-cache?
              (swap! state assoc-in [:embeddings cache-key]
                     {:embedding normalized
                      :text text
                      :model-id model-id
                      :created-at (System/currentTimeMillis)}))
            
            (metrics/increment :embeddings-generated {:model-id model-id})
            normalized))))))

(defn embed-batch
  "Generate embeddings for multiple texts."
  [model-id texts & {:keys [use-cache?] :or {use-cache? true}}]
  (let [model (get-model model-id)
        batch-size (or (:batch-size model) 32)]
    (logging/log :info "Generating batch embeddings"
                 {:model-id model-id :count (count texts)})
    
    (let [results (doall
                   (for [batch (partition-all batch-size texts)]
                     (mapv #(embed-text model-id % :use-cache? use-cache?) batch)))]
      (vec (apply concat results)))))

(defn embed-async
  "Generate embedding asynchronously."
  [model-id text]
  (go
    (embed-text model-id text)))

;; ============================================================================
;; Similarity Search
;; ============================================================================

(defn cosine-similarity
  "Calculate cosine similarity between two vectors."
  [v1 v2]
  (let [dot-product (reduce + (map * v1 v2))
        mag1 (Math/sqrt (reduce + (map #(* % %) v1)))
        mag2 (Math/sqrt (reduce + (map #(* % %) v2)))]
    (if (or (zero? mag1) (zero? mag2))
      0.0
      (/ dot-product (* mag1 mag2)))))

(defn euclidean-distance
  "Calculate Euclidean distance between two vectors."
  [v1 v2]
  (Math/sqrt (reduce + (map (fn [a b] (Math/pow (- a b) 2)) v1 v2))))

(defn find-similar
  "Find similar embeddings to a query embedding."
  [query-embedding embeddings-map & {:keys [top-k metric threshold]
                                      :or {top-k 10 metric :cosine threshold 0.0}}]
  (let [score-fn (case metric
                   :cosine cosine-similarity
                   :euclidean (fn [v1 v2] (- 1 (euclidean-distance v1 v2)))
                   cosine-similarity)
        scored (for [[id data] embeddings-map
                     :let [score (score-fn query-embedding (:embedding data))]
                     :when (>= score threshold)]
                 {:id id
                  :score score
                  :text (:text data)
                  :metadata (:metadata data)})
        sorted (sort-by :score > scored)]
    (take top-k sorted)))

(defn semantic-search
  "Perform semantic search using text query."
  [model-id query-text corpus & {:keys [top-k threshold]
                                  :or {top-k 10 threshold 0.5}}]
  (let [query-embedding (embed-text model-id query-text)
        corpus-embeddings (into {}
                                (map-indexed
                                 (fn [idx item]
                                   [idx {:embedding (embed-text model-id (:text item))
                                         :text (:text item)
                                         :metadata (dissoc item :text)}])
                                 corpus))]
    (find-similar query-embedding corpus-embeddings
                  :top-k top-k :threshold threshold)))

;; ============================================================================
;; Vector Index
;; ============================================================================

(defn create-index!
  "Create a vector index for fast similarity search."
  [index-id config]
  (let [index {:id index-id
               :name (get config :name (name index-id))
               :model-id (get config :model-id :default)
               :metric (get config :metric :cosine)
               :vectors {}
               :metadata {}
               :created-at (System/currentTimeMillis)
               :updated-at (System/currentTimeMillis)
               :stats {:total-vectors 0}}]
    (swap! state assoc-in [:indices index-id] index)
    (logging/log :info "Created vector index" {:index-id index-id})
    (events/emit! :vector-index-created {:index-id index-id})
    index-id))

(defn add-to-index!
  "Add a vector to an index."
  [index-id vector-id embedding & {:keys [metadata text]}]
  (swap! state
         (fn [s]
           (-> s
               (assoc-in [:indices index-id :vectors vector-id] embedding)
               (assoc-in [:indices index-id :metadata vector-id]
                         {:text text :metadata metadata :added-at (System/currentTimeMillis)})
               (update-in [:indices index-id :stats :total-vectors] inc)
               (assoc-in [:indices index-id :updated-at] (System/currentTimeMillis)))))
  (metrics/increment :vectors-indexed {:index-id index-id})
  vector-id)

(defn add-text-to-index!
  "Embed and add text to an index."
  [index-id vector-id text & {:keys [metadata]}]
  (let [index (get-in @state [:indices index-id])
        model-id (:model-id index)
        embedding (embed-text model-id text)]
    (add-to-index! index-id vector-id embedding :metadata metadata :text text)))

(defn search-index
  "Search a vector index."
  [index-id query-embedding & {:keys [top-k threshold]
                                :or {top-k 10 threshold 0.0}}]
  (let [index (get-in @state [:indices index-id])
        vectors (:vectors index)
        metadata (:metadata index)
        metric (:metric index)
        score-fn (case metric
                   :cosine cosine-similarity
                   :euclidean (fn [v1 v2] (- 1 (euclidean-distance v1 v2)))
                   cosine-similarity)]
    (let [scored (for [[id vec] vectors
                       :let [score (score-fn query-embedding vec)]
                       :when (>= score threshold)]
                   (merge {:id id :score score}
                          (get metadata id)))
          sorted (sort-by :score > scored)]
      (take top-k sorted))))

(defn search-index-by-text
  "Search index using text query."
  [index-id query-text & {:keys [top-k threshold]
                           :or {top-k 10 threshold 0.5}}]
  (let [index (get-in @state [:indices index-id])
        model-id (:model-id index)
        query-embedding (embed-text model-id query-text)]
    (search-index index-id query-embedding :top-k top-k :threshold threshold)))

(defn remove-from-index!
  "Remove a vector from an index."
  [index-id vector-id]
  (swap! state
         (fn [s]
           (-> s
               (update-in [:indices index-id :vectors] dissoc vector-id)
               (update-in [:indices index-id :metadata] dissoc vector-id)
               (update-in [:indices index-id :stats :total-vectors] dec))))
  true)

(defn get-index-stats
  "Get statistics for a vector index."
  [index-id]
  (let [index (get-in @state [:indices index-id])]
    {:id index-id
     :name (:name index)
     :model-id (:model-id index)
     :total-vectors (get-in index [:stats :total-vectors] 0)
     :created-at (:created-at index)
     :updated-at (:updated-at index)}))

;; ============================================================================
;; Clustering
;; ============================================================================

(defn- kmeans-init-centroids
  "Initialize k centroids using k-means++ algorithm."
  [embeddings k]
  (loop [centroids [(rand-nth embeddings)]
         remaining (dec k)]
    (if (zero? remaining)
      centroids
      (let [distances (for [emb embeddings]
                        (apply min (map #(euclidean-distance emb %) centroids)))
            total-dist (reduce + distances)
            probs (map #(/ % total-dist) distances)
            cumulative (reductions + probs)
            r (rand)
            idx (count (take-while #(< % r) cumulative))
            new-centroid (nth embeddings (min idx (dec (count embeddings))))]
        (recur (conj centroids new-centroid) (dec remaining))))))

(defn- assign-clusters
  "Assign embeddings to nearest centroids."
  [embeddings centroids]
  (mapv (fn [emb]
          (let [distances (map-indexed
                           (fn [idx c] [idx (euclidean-distance emb c)])
                           centroids)]
            (first (apply min-key second distances))))
        embeddings))

(defn- update-centroids
  "Update centroids based on cluster assignments."
  [embeddings assignments k dimensions]
  (vec (for [cluster-id (range k)]
         (let [cluster-embs (map first
                                 (filter #(= (second %) cluster-id)
                                         (map vector embeddings assignments)))]
           (if (empty? cluster-embs)
             (vec (repeatedly dimensions #(rand)))
             (let [n (count cluster-embs)]
               (mapv (fn [dim]
                       (/ (reduce + (map #(nth % dim) cluster-embs)) n))
                     (range dimensions))))))))

(defn cluster-embeddings
  "Cluster embeddings using k-means algorithm."
  [embeddings k & {:keys [max-iterations] :or {max-iterations 100}}]
  (when (seq embeddings)
    (let [dimensions (count (first embeddings))
          initial-centroids (kmeans-init-centroids embeddings k)]
      (loop [centroids initial-centroids
             iteration 0]
        (if (>= iteration max-iterations)
          {:centroids centroids
           :assignments (assign-clusters embeddings centroids)
           :iterations iteration}
          (let [assignments (assign-clusters embeddings centroids)
                new-centroids (update-centroids embeddings assignments k dimensions)]
            (if (= centroids new-centroids)
              {:centroids centroids
               :assignments assignments
               :iterations iteration
               :converged? true}
              (recur new-centroids (inc iteration)))))))))

(defn create-cluster!
  "Create and store a clustering result."
  [cluster-id embeddings-with-ids k]
  (let [embeddings (mapv :embedding embeddings-with-ids)
        ids (mapv :id embeddings-with-ids)
        result (cluster-embeddings embeddings k)
        cluster-data {:id cluster-id
                      :k k
                      :centroids (:centroids result)
                      :assignments (zipmap ids (:assignments result))
                      :iterations (:iterations result)
                      :converged? (:converged? result)
                      :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:clusters cluster-id] cluster-data)
    (logging/log :info "Created cluster" {:cluster-id cluster-id :k k})
    (events/emit! :cluster-created {:cluster-id cluster-id})
    cluster-data))

(defn get-cluster
  "Get a clustering result."
  [cluster-id]
  (get-in @state [:clusters cluster-id]))

(defn get-cluster-members
  "Get members of a specific cluster."
  [cluster-id cluster-num]
  (let [cluster (get-cluster cluster-id)
        assignments (:assignments cluster)]
    (vec (for [[id assignment] assignments
               :when (= assignment cluster-num)]
           id))))

;; ============================================================================
;; Dimensionality Reduction
;; ============================================================================

(defn reduce-dimensions-pca
  "Reduce embedding dimensions using PCA (placeholder)."
  [embeddings target-dims]
  ;; Placeholder: actual PCA implementation would go here
  (mapv (fn [emb]
          (vec (take target-dims emb)))
        embeddings))

(defn reduce-dimensions-tsne
  "Reduce embedding dimensions using t-SNE (placeholder)."
  [embeddings target-dims & {:keys [perplexity] :or {perplexity 30}}]
  ;; Placeholder: actual t-SNE implementation would go here
  (mapv (fn [_]
          (vec (repeatedly target-dims #(- (rand 2) 1))))
        embeddings))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-service-stats
  "Get embedding service statistics."
  []
  (let [cache-stats (:cache-stats @state)
        total-requests (+ (:hits cache-stats) (:misses cache-stats))]
    {:total-models (count (:models @state))
     :total-cached-embeddings (count (:embeddings @state))
     :total-indices (count (:indices @state))
     :total-clusters (count (:clusters @state))
     :cache-hits (:hits cache-stats)
     :cache-misses (:misses cache-stats)
     :cache-hit-rate (if (pos? total-requests)
                       (double (/ (:hits cache-stats) total-requests))
                       0.0)}))

(defn clear-cache!
  "Clear the embedding cache."
  []
  (swap! state assoc :embeddings {})
  (swap! state assoc :cache-stats {:hits 0 :misses 0})
  (logging/log :info "Cleared embedding cache"))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-embedding-service!
  "Initialize the embedding service."
  []
  (when-not (:initialized? @state)
    ;; Register default local model
    (register-model! :default
                     {:name "Default Embedding Model"
                      :type :local
                      :dimensions 384
                      :max-tokens 512
                      :normalize? true})
    
    ;; Register LM Studio model
    (register-model! :lm-studio
                     {:name "LM Studio Embeddings"
                      :type :api
                      :dimensions 768
                      :endpoint "http://localhost:1234/v1/embeddings"
                      :normalize? true})
    
    ;; Create default index for mental models
    (create-index! :mental-models
                   {:name "Mental Models Index"
                    :model-id :default
                    :metric :cosine})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Embedding service initialized")
    (events/emit! :embedding-service-initialized {})
    true))
