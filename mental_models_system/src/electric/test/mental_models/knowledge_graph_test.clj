(ns mental-models.knowledge-graph-test
  "Tests for the knowledge graph module.
   
   Tests graph operations, document analysis, queries,
   and graph traversal."
  (:require [clojure.test :refer [deftest testing is are]]
            [mental-models.knowledge-graph :as kg]))

;; ============================================
;; Graph Creation Tests
;; ============================================

(deftest test-create-graph
  (testing "Create empty graph"
    (let [graph (kg/create-graph)]
      (is (map? graph))
      (is (contains? graph :nodes))
      (is (contains? graph :edges))
      (is (contains? graph :indexes))
      (is (empty? (:nodes graph)))
      (is (empty? (:edges graph))))))

(deftest test-create-node
  (testing "Create a node"
    (let [node (kg/create-node "test-id" "document" "Test Doc" :properties {:path "/test"})]
      (is (map? node))
      (is (= "test-id" (:id node)))
      (is (= "document" (:type node)))
      (is (= "Test Doc" (:name node)))
      (is (= {:path "/test"} (:properties node))))))

(deftest test-create-edge
  (testing "Create an edge"
    (let [edge (kg/create-edge "source" "target" "RELATES_TO" :weight 0.8 :properties {:note "test"})]
      (is (map? edge))
      (is (= "source" (:source edge)))
      (is (= "target" (:target edge)))
      (is (= "RELATES_TO" (:type edge)))
      (is (= 0.8 (:weight edge)))
      (is (= {:note "test"} (:properties edge))))))

;; ============================================
;; Graph Operations Tests
;; ============================================

(deftest test-generate-id
  (testing "Generate unique IDs"
    (let [id1 (kg/generate-id "document" "Doc1")
          id2 (kg/generate-id "document" "Doc2")
          id3 (kg/generate-id "document" "Doc1")]
      (is (string? id1))
      (is (not= id1 id2))
      (is (= id1 id3)))))

(deftest test-add-node
  (testing "Add node to graph"
    (let [graph (-> (kg/create-graph)
                    (kg/add-node "document" "Test Doc" :properties {:path "/test"}))]
      (is (= 1 (count (:nodes graph))))
      (is (= 1 (count (get-in graph [:indexes :by-type "document"]))))
      (is (= 1 (count (get-in graph [:indexes :by-name "Test Doc"])))))))

(deftest test-add-multiple-nodes
  (testing "Add multiple nodes"
    (let [graph (-> (kg/create-graph)
                    (kg/add-node "document" "Doc1")
                    (kg/add-node "document" "Doc2")
                    (kg/add-node "model" "Model1"))]
      (is (= 3 (count (:nodes graph))))
      (is (= 2 (count (get-in graph [:indexes :by-type "document"]))))
      (is (= 1 (count (get-in graph [:indexes :by-type "model"])))))))

(deftest test-add-edge
  (testing "Add edge to graph"
    (let [graph (-> (kg/create-graph)
                    (kg/add-node "document" "Doc1")
                    (kg/add-node "model" "Model1"))
          doc-id (kg/generate-id "document" "Doc1")
          model-id (kg/generate-id "model" "Model1")
          graph (kg/add-edge graph doc-id model-id "APPLIES" :weight 0.9)]
      (is (= 1 (count (:edges graph))))
      (is (= 1 (count (get-in graph [:indexes :outgoing doc-id]))))
      (is (= 1 (count (get-in graph [:indexes :incoming model-id])))))))

(deftest test-get-node
  (testing "Get node by ID"
    (let [graph (kg/add-node (kg/create-graph) "document" "Test Doc")
          id (kg/generate-id "document" "Test Doc")
          node (kg/get-node graph id)]
      (is (map? node))
      (is (= "Test Doc" (:name node)))
      (is (= "document" (:type node))))))

(deftest test-get-nodes-by-type
  (testing "Get nodes by type"
    (let [graph (-> (kg/create-graph)
                    (kg/add-node "document" "Doc1")
                    (kg/add-node "document" "Doc2")
                    (kg/add-node "model" "Model1"))
          docs (kg/get-nodes-by-type graph "document")
          models (kg/get-nodes-by-type graph "model")]
      (is (= 2 (count docs)))
      (is (= 1 (count models)))
      (is (every? #(= "document" (:type %)) docs)))))

;; ============================================
;; Document Analysis Tests
;; ============================================

(deftest test-add-document
  (testing "Add document with metadata"
    (let [graph (kg/add-document (kg/create-graph) "Test Document"
                                 :path "/path/to/doc.txt"
                                 :summary "A test document"
                                 :tags ["test" "example"]
                                 :categories ["Testing"]
                                 :models [{:model-name "Circle of Competence" :relevance 0.8}])]
      (is (>= (count (:nodes graph)) 4))
      (is (>= (count (:edges graph)) 3)))))

(deftest test-add-document-with-tags
  (testing "Document tags are indexed"
    (let [graph (kg/add-document (kg/create-graph) "Doc1"
                                 :tags ["important" "review"])]
      (is (= 2 (count (kg/get-nodes-by-type graph "tag")))))))

(deftest test-add-document-with-models
  (testing "Document model relationships"
    (let [graph (kg/add-document (kg/create-graph) "Doc1"
                                 :models [{:model-name "Model1" :relevance 0.9}
                                          {:model-name "Model2" :relevance 0.7}])]
      (is (= 2 (count (kg/get-nodes-by-type graph "model")))))))

;; ============================================
;; Query Tests
;; ============================================

(deftest test-find-documents-by-model
  (testing "Find documents by model"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "TestModel" :relevance 0.9}])
                    (kg/add-document "Doc2" :models [{:model-name "TestModel" :relevance 0.7}])
                    (kg/add-document "Doc3" :models [{:model-name "OtherModel" :relevance 0.8}]))
          results (kg/find-documents-by-model graph "TestModel")]
      (is (= 2 (count results)))
      (is (every? #(contains? % :document) results))
      (is (every? #(contains? % :relevance) results)))))

(deftest test-find-documents-by-tag
  (testing "Find documents by tag"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :tags ["important"])
                    (kg/add-document "Doc2" :tags ["important" "review"])
                    (kg/add-document "Doc3" :tags ["review"]))
          results (kg/find-documents-by-tag graph "important")]
      (is (= 2 (count results))))))

(deftest test-find-documents-by-category
  (testing "Find documents by category"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :categories ["Finance"])
                    (kg/add-document "Doc2" :categories ["Finance" "Strategy"])
                    (kg/add-document "Doc3" :categories ["Strategy"]))
          results (kg/find-documents-by-category graph "Finance")]
      (is (= 2 (count results))))))

(deftest test-search
  (testing "Search the graph"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Investment Analysis" :summary "Financial review")
                    (kg/add-document "Strategy Document" :summary "Business strategy"))
          results (kg/search graph "investment")]
      (is (>= (count results) 1))
      (is (some #(clojure.string/includes? (clojure.string/lower-case (:name %)) "investment") results)))))

(deftest test-search-with-types
  (testing "Search with type filter"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Test Doc" :tags ["test"]))
          doc-results (kg/search graph "test" :types ["document"])
          tag-results (kg/search graph "test" :types ["tag"])]
      (is (every? #(= "document" (:type %)) doc-results))
      (is (every? #(= "tag" (:type %)) tag-results)))))

;; ============================================
;; Similarity Tests
;; ============================================

(deftest test-find-similar-documents
  (testing "Find similar documents"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "Model1" :relevance 0.9}
                                                     {:model-name "Model2" :relevance 0.8}])
                    (kg/add-document "Doc2" :models [{:model-name "Model1" :relevance 0.7}
                                                     {:model-name "Model2" :relevance 0.6}])
                    (kg/add-document "Doc3" :models [{:model-name "Model3" :relevance 0.9}]))
          results (kg/find-similar-documents graph "Doc1" :min-similarity 0.3)]
      (is (seq results))
      (is (every? #(contains? % :similarity) results))
      (is (every? #(contains? % :shared-models) results)))))

;; ============================================
;; Statistics Tests
;; ============================================

(deftest test-get-graph-summary
  (testing "Get graph summary"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :tags ["tag1"] :categories ["cat1"]
                                     :models [{:model-name "Model1"}]))
          summary (kg/get-graph-summary graph)]
      (is (map? summary))
      (is (contains? summary :total-nodes))
      (is (contains? summary :total-edges))
      (is (contains? summary :documents))
      (is (contains? summary :models))
      (is (contains? summary :categories))
      (is (contains? summary :tags))
      (is (= 1 (:documents summary)))
      (is (= 1 (:models summary))))))

(deftest test-get-model-statistics
  (testing "Get model statistics"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "Model1" :relevance 0.9}])
                    (kg/add-document "Doc2" :models [{:model-name "Model1" :relevance 0.7}]))
          stats (kg/get-model-statistics graph)]
      (is (seq stats))
      (is (every? #(contains? % :model) stats))
      (is (every? #(contains? % :document-count) stats))
      (is (every? #(contains? % :avg-relevance) stats)))))

(deftest test-get-category-distribution
  (testing "Get category distribution"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :categories ["Finance"])
                    (kg/add-document "Doc2" :categories ["Finance"])
                    (kg/add-document "Doc3" :categories ["Strategy"]))
          dist (kg/get-category-distribution graph)]
      (is (seq dist))
      (is (every? #(contains? % :category) dist))
      (is (every? #(contains? % :document-count) dist)))))

;; ============================================
;; Graph Traversal Tests
;; ============================================

(deftest test-find-related-models
  (testing "Find related models"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "Model1"}
                                                     {:model-name "Model2"}])
                    (kg/add-document "Doc2" :models [{:model-name "Model1"}
                                                     {:model-name "Model2"}])
                    (kg/add-document "Doc3" :models [{:model-name "Model1"}
                                                     {:model-name "Model3"}]))
          results (kg/find-related-models graph "Model1" :min-co-occurrence 1)]
      (is (seq results))
      (is (every? #(contains? % :model) results))
      (is (every? #(contains? % :co-occurrence) results)))))

(deftest test-find-lollapalooza-clusters
  (testing "Find lollapalooza clusters"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "M1"} {:model-name "M2"} {:model-name "M3"}])
                    (kg/add-document "Doc2" :models [{:model-name "M1"} {:model-name "M2"} {:model-name "M3"}]))
          clusters (kg/find-lollapalooza-clusters graph :min-co-occurrence 1)]
      (is (seq clusters))
      (is (every? #(contains? % :models) clusters))
      (is (every? #(contains? % :co-occurrence) clusters)))))

;; ============================================
;; Export Tests
;; ============================================

(deftest test-export-json
  (testing "Export graph to JSON"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "Model1"}]))
          exported (kg/export-json graph)]
      (is (map? exported))
      (is (contains? exported :exported-at))
      (is (contains? exported :statistics))
      (is (contains? exported :nodes))
      (is (contains? exported :edges))
      (is (vector? (:nodes exported)))
      (is (vector? (:edges exported))))))

(deftest test-export-cypher
  (testing "Export graph to Cypher"
    (let [graph (-> (kg/create-graph)
                    (kg/add-document "Doc1" :models [{:model-name "Model1"}]))
          cypher (kg/export-cypher graph)]
      (is (string? cypher))
      (is (clojure.string/includes? cypher "CREATE")))))

;; ============================================
;; Edge Cases
;; ============================================

(deftest test-empty-graph-operations
  (testing "Operations on empty graph"
    (let [graph (kg/create-graph)]
      (is (empty? (kg/get-nodes-by-type graph "document")))
      (is (empty? (kg/find-documents-by-model graph "NonExistent")))
      (is (empty? (kg/search graph "test")))
      (is (= 0 (:total-nodes (kg/get-graph-summary graph)))))))

(deftest test-duplicate-nodes
  (testing "Adding duplicate nodes"
    (let [graph (-> (kg/create-graph)
                    (kg/add-node "document" "Same Name")
                    (kg/add-node "document" "Same Name"))]
      (is (= 1 (count (:nodes graph)))))))
