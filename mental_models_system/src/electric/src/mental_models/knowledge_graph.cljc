(ns mental-models.knowledge-graph
  "Knowledge Graph Module - Electric Clojure
   
   Knowledge graph for mental model document analysis.
   Ported from Python to Electric Clojure for unified codebase.
   
   Features:
   - Document categorization by mental model
   - Tag-based organization
   - Relationship mapping between documents
   - Searchable index
   - Graph traversal and analysis"
  (:require [clojure.string :as str]
            [mental-models.models :as models]))

;; ============================================
;; Graph Data Structures
;; ============================================

(defn create-node
  "Create a node in the knowledge graph."
  [id type name & {:keys [properties] :or {properties {}}}]
  {:id id
   :type type
   :name name
   :properties properties})

(defn create-edge
  "Create an edge (relationship) in the knowledge graph."
  [source-id target-id type & {:keys [weight properties] :or {weight 1.0 properties {}}}]
  {:source source-id
   :target target-id
   :type type
   :weight weight
   :properties properties})

(defn create-graph
  "Create an empty knowledge graph."
  []
  {:nodes {}
   :edges []
   :indexes {:by-type {}
             :by-name {}
             :outgoing {}
             :incoming {}
             :doc-models {}
             :model-docs {}}})

;; ============================================
;; Graph Operations
;; ============================================

(defn generate-id
  "Generate a unique ID for a node."
  [type name]
  (str type "_" (hash name)))

(defn add-node
  "Add a node to the graph."
  [graph type name & {:keys [properties] :or {properties {}}}]
  (let [id (generate-id type name)
        node (create-node id type name :properties properties)]
    (-> graph
        (assoc-in [:nodes id] node)
        (update-in [:indexes :by-type type] (fnil conj #{}) id)
        (update-in [:indexes :by-name name] (fnil conj #{}) id))))

(defn add-edge
  "Add an edge to the graph."
  [graph source-id target-id type & {:keys [weight properties] :or {weight 1.0 properties {}}}]
  (let [edge (create-edge source-id target-id type :weight weight :properties properties)]
    (-> graph
        (update :edges conj edge)
        (update-in [:indexes :outgoing source-id] (fnil conj []) edge)
        (update-in [:indexes :incoming target-id] (fnil conj []) edge))))

(defn get-node
  "Get a node by ID."
  [graph id]
  (get-in graph [:nodes id]))

(defn get-nodes-by-type
  "Get all nodes of a specific type."
  [graph type]
  (let [ids (get-in graph [:indexes :by-type type] #{})]
    (map #(get-node graph %) ids)))

(defn get-outgoing-edges
  "Get all outgoing edges from a node."
  [graph node-id]
  (get-in graph [:indexes :outgoing node-id] []))

(defn get-incoming-edges
  "Get all incoming edges to a node."
  [graph node-id]
  (get-in graph [:indexes :incoming node-id] []))

;; ============================================
;; Document Analysis Integration
;; ============================================

(defn add-document
  "Add a document to the knowledge graph."
  [graph doc-name & {:keys [path summary tags categories models]}]
  (let [doc-id (generate-id "document" doc-name)
        graph (add-node graph "document" doc-name
                        :properties {:path path :summary summary})
        
        ;; Add tags
        graph (reduce (fn [g tag]
                        (let [tag-id (generate-id "tag" tag)]
                          (-> g
                              (add-node "tag" tag)
                              (add-edge doc-id tag-id "HAS_TAG"))))
                      graph
                      (or tags []))
        
        ;; Add categories
        graph (reduce (fn [g cat]
                        (let [cat-id (generate-id "category" cat)]
                          (-> g
                              (add-node "category" cat)
                              (add-edge doc-id cat-id "BELONGS_TO"))))
                      graph
                      (or categories []))
        
        ;; Add model relationships
        graph (reduce (fn [g {:keys [model-name relevance evidence]}]
                        (let [model-id (generate-id "model" model-name)]
                          (-> g
                              (add-node "model" model-name)
                              (add-edge doc-id model-id "APPLIES"
                                        :weight (or relevance 1.0)
                                        :properties {:evidence evidence})
                              (update-in [:indexes :doc-models doc-id] (fnil conj #{}) model-id)
                              (update-in [:indexes :model-docs model-id] (fnil conj #{}) doc-id))))
                      graph
                      (or models []))]
    graph))

;; ============================================
;; Query Functions
;; ============================================

(defn find-documents-by-model
  "Find all documents that apply a specific model."
  [graph model-name]
  (let [model-id (generate-id "model" model-name)
        doc-ids (get-in graph [:indexes :model-docs model-id] #{})]
    (map (fn [doc-id]
           (let [doc (get-node graph doc-id)
                 edges (get-outgoing-edges graph doc-id)
                 model-edge (first (filter #(and (= (:target %) model-id)
                                                  (= (:type %) "APPLIES"))
                                           edges))]
             {:document (:name doc)
              :path (get-in doc [:properties :path])
              :relevance (:weight model-edge)
              :evidence (get-in model-edge [:properties :evidence])}))
         doc-ids)))

(defn find-documents-by-tag
  "Find all documents with a specific tag."
  [graph tag]
  (let [tag-id (generate-id "tag" tag)
        edges (get-incoming-edges graph tag-id)]
    (keep (fn [edge]
            (when (= (:type edge) "HAS_TAG")
              (let [doc (get-node graph (:source edge))]
                {:document (:name doc)
                 :path (get-in doc [:properties :path])
                 :summary (get-in doc [:properties :summary])})))
          edges)))

(defn find-documents-by-category
  "Find all documents in a category."
  [graph category]
  (let [cat-id (generate-id "category" category)
        edges (get-incoming-edges graph cat-id)]
    (keep (fn [edge]
            (when (= (:type edge) "BELONGS_TO")
              (let [doc (get-node graph (:source edge))]
                {:document (:name doc)
                 :path (get-in doc [:properties :path])
                 :summary (get-in doc [:properties :summary])})))
          edges)))

(defn find-similar-documents
  "Find documents similar to a given document based on shared models."
  [graph doc-name & {:keys [min-similarity] :or {min-similarity 0.3}}]
  (let [doc-id (generate-id "document" doc-name)
        my-models (get-in graph [:indexes :doc-models doc-id] #{})
        all-docs (get-nodes-by-type graph "document")]
    (when (seq my-models)
      (->> all-docs
           (keep (fn [other-doc]
                   (when (not= (:id other-doc) doc-id)
                     (let [other-models (get-in graph [:indexes :doc-models (:id other-doc)] #{})
                           intersection (count (clojure.set/intersection my-models other-models))
                           union (count (clojure.set/union my-models other-models))
                           similarity (if (> union 0) (/ intersection union) 0)]
                       (when (>= similarity min-similarity)
                         {:document (:name other-doc)
                          :similarity similarity
                          :shared-models (map #(:name (get-node graph %))
                                              (clojure.set/intersection my-models other-models))})))))
           (sort-by :similarity >)))))

(defn search
  "Search the knowledge graph."
  [graph query & {:keys [types] :or {types ["document" "model" "category" "tag"]}}]
  (let [query-lower (str/lower-case query)]
    (->> (vals (:nodes graph))
         (filter #(contains? (set types) (:type %)))
         (filter (fn [node]
                   (or (str/includes? (str/lower-case (:name node)) query-lower)
                       (some (fn [[k v]]
                               (and (string? v)
                                    (str/includes? (str/lower-case v) query-lower)))
                             (:properties node)))))
         (map (fn [node]
                {:type (:type node)
                 :name (:name node)
                 :properties (:properties node)})))))

;; ============================================
;; Graph Statistics
;; ============================================

(defn get-model-statistics
  "Get statistics about model usage across documents."
  [graph]
  (->> (get-nodes-by-type graph "model")
       (map (fn [model]
              (let [doc-count (count (get-in graph [:indexes :model-docs (:id model)] #{}))
                    edges (get-incoming-edges graph (:id model))
                    relevances (keep #(when (= (:type %) "APPLIES") (:weight %)) edges)
                    avg-relevance (if (seq relevances)
                                    (/ (reduce + relevances) (count relevances))
                                    0)]
                {:model (:name model)
                 :document-count doc-count
                 :avg-relevance avg-relevance})))
       (sort-by :document-count >)))

(defn get-category-distribution
  "Get distribution of documents across categories."
  [graph]
  (->> (get-nodes-by-type graph "category")
       (map (fn [cat]
              (let [doc-count (count (filter #(= (:type %) "BELONGS_TO")
                                             (get-incoming-edges graph (:id cat))))]
                {:category (:name cat)
                 :document-count doc-count})))
       (sort-by :document-count >)))

(defn get-graph-summary
  "Get summary statistics for the graph."
  [graph]
  {:total-nodes (count (:nodes graph))
   :total-edges (count (:edges graph))
   :documents (count (get-in graph [:indexes :by-type "document"] #{}))
   :models (count (get-in graph [:indexes :by-type "model"] #{}))
   :categories (count (get-in graph [:indexes :by-type "category"] #{}))
   :tags (count (get-in graph [:indexes :by-type "tag"] #{}))})

;; ============================================
;; Graph Traversal
;; ============================================

(defn find-related-models
  "Find models related to a given model through shared documents."
  [graph model-name & {:keys [min-co-occurrence] :or {min-co-occurrence 2}}]
  (let [model-id (generate-id "model" model-name)
        my-docs (get-in graph [:indexes :model-docs model-id] #{})
        all-models (get-nodes-by-type graph "model")]
    (->> all-models
         (keep (fn [other-model]
                 (when (not= (:id other-model) model-id)
                   (let [other-docs (get-in graph [:indexes :model-docs (:id other-model)] #{})
                         shared-docs (clojure.set/intersection my-docs other-docs)]
                     (when (>= (count shared-docs) min-co-occurrence)
                       {:model (:name other-model)
                        :co-occurrence (count shared-docs)
                        :shared-documents (map #(:name (get-node graph %)) shared-docs)})))))
         (sort-by :co-occurrence >))))

(defn find-lollapalooza-clusters
  "Find clusters of models that frequently appear together (Lollapalooza effects)."
  [graph & {:keys [min-cluster-size min-co-occurrence]
            :or {min-cluster-size 3 min-co-occurrence 2}}]
  (let [models (get-nodes-by-type graph "model")
        model-pairs (for [m1 models
                          m2 models
                          :when (neg? (compare (:id m1) (:id m2)))]
                      [m1 m2])
        co-occurrences (keep (fn [[m1 m2]]
                               (let [docs1 (get-in graph [:indexes :model-docs (:id m1)] #{})
                                     docs2 (get-in graph [:indexes :model-docs (:id m2)] #{})
                                     shared (count (clojure.set/intersection docs1 docs2))]
                                 (when (>= shared min-co-occurrence)
                                   {:models [(:name m1) (:name m2)]
                                    :co-occurrence shared})))
                             model-pairs)]
    (->> co-occurrences
         (sort-by :co-occurrence >)
         (take 20))))

;; ============================================
;; Export Functions
;; ============================================

(defn export-json
  "Export graph to JSON format."
  [graph]
  {:exported-at (str #?(:clj (java.time.Instant/now) :cljs (js/Date.)))
   :statistics (get-graph-summary graph)
   :nodes (mapv (fn [[id node]]
                  {:id id
                   :type (:type node)
                   :name (:name node)
                   :properties (:properties node)})
                (:nodes graph))
   :edges (mapv (fn [edge]
                  {:source (:source edge)
                   :target (:target edge)
                   :type (:type edge)
                   :weight (:weight edge)
                   :properties (:properties edge)})
                (:edges graph))})

(defn export-cypher
  "Export graph as Neo4j Cypher statements."
  [graph]
  (let [node-statements (map (fn [[id node]]
                               (str "CREATE (n:" (:type node) " {id: '" id "', name: '" (:name node) "'})"))
                             (:nodes graph))
        edge-statements (map (fn [edge]
                               (str "MATCH (a {id: '" (:source edge) "'}), (b {id: '" (:target edge) "'}) "
                                    "CREATE (a)-[:" (:type edge) " {weight: " (:weight edge) "}]->(b)"))
                             (:edges graph))]
    (str/join "\n" (concat node-statements edge-statements))))
