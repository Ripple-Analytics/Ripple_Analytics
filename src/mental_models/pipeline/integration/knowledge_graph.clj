(ns mental-models.pipeline.integration.knowledge-graph
  "Knowledge Graph Module
   
   Knowledge graph for mental model relationships:
   - Node and edge management
   - Graph traversal and queries
   - Path finding
   - Subgraph extraction
   - Graph analytics"
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
;; KNOWLEDGE GRAPH STATE
;; =============================================================================

(defonce graph-state (atom {:nodes (ConcurrentHashMap.)
                            :edges (ConcurrentHashMap.)
                            :adjacency (ConcurrentHashMap.)
                            :reverse-adjacency (ConcurrentHashMap.)
                            :node-count (AtomicLong. 0)
                            :edge-count (AtomicLong. 0)
                            :config {:max-path-length 10
                                     :default-weight 1.0}}))

;; =============================================================================
;; NODE MANAGEMENT
;; =============================================================================

(defn create-node!
  "Create a node in the graph."
  [node-id {:keys [type label properties]}]
  (.incrementAndGet ^AtomicLong (:node-count @graph-state))
  (log/debug "Creating node" {:id node-id :type type})
  (let [node {:id node-id
              :type type
              :label label
              :properties (or properties {})
              :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:nodes @graph-state) node-id node)
    ;; Initialize adjacency lists
    (.putIfAbsent ^ConcurrentHashMap (:adjacency @graph-state) node-id #{})
    (.putIfAbsent ^ConcurrentHashMap (:reverse-adjacency @graph-state) node-id #{})
    node))

(defn delete-node!
  "Delete a node and its edges."
  [node-id]
  ;; Remove all edges connected to this node
  (let [outgoing (or (.get ^ConcurrentHashMap (:adjacency @graph-state) node-id) #{})
        incoming (or (.get ^ConcurrentHashMap (:reverse-adjacency @graph-state) node-id) #{})]
    (doseq [target outgoing]
      (delete-edge! node-id target))
    (doseq [source incoming]
      (delete-edge! source node-id)))
  ;; Remove node
  (.remove ^ConcurrentHashMap (:nodes @graph-state) node-id)
  (.remove ^ConcurrentHashMap (:adjacency @graph-state) node-id)
  (.remove ^ConcurrentHashMap (:reverse-adjacency @graph-state) node-id))

(defn get-node
  "Get a node by ID."
  [node-id]
  (.get ^ConcurrentHashMap (:nodes @graph-state) node-id))

(defn update-node!
  "Update a node's properties."
  [node-id updates]
  (when-let [node (get-node node-id)]
    (let [updated (merge node updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:nodes @graph-state) node-id updated)
      updated)))

(defn list-nodes
  "List all nodes."
  [& {:keys [type limit]}]
  (let [nodes (vals (:nodes @graph-state))]
    (cond->> nodes
      type (filter #(= (:type %) type))
      limit (take limit))))

(defn find-nodes
  "Find nodes matching criteria."
  [predicate]
  (filter predicate (vals (:nodes @graph-state))))

;; =============================================================================
;; EDGE MANAGEMENT
;; =============================================================================

(defn edge-id
  "Generate an edge ID."
  [source target]
  (str source "->" target))

(defn create-edge!
  "Create an edge between nodes."
  [source target {:keys [type label weight properties]}]
  (.incrementAndGet ^AtomicLong (:edge-count @graph-state))
  (log/debug "Creating edge" {:source source :target target :type type})
  (let [eid (edge-id source target)
        edge {:id eid
              :source source
              :target target
              :type type
              :label label
              :weight (or weight (get-in @graph-state [:config :default-weight]))
              :properties (or properties {})
              :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:edges @graph-state) eid edge)
    ;; Update adjacency lists
    (let [adj (.get ^ConcurrentHashMap (:adjacency @graph-state) source)
          radj (.get ^ConcurrentHashMap (:reverse-adjacency @graph-state) target)]
      (.put ^ConcurrentHashMap (:adjacency @graph-state) source (conj (or adj #{}) target))
      (.put ^ConcurrentHashMap (:reverse-adjacency @graph-state) target (conj (or radj #{}) source)))
    edge))

(defn delete-edge!
  "Delete an edge."
  [source target]
  (let [eid (edge-id source target)]
    (.remove ^ConcurrentHashMap (:edges @graph-state) eid)
    ;; Update adjacency lists
    (let [adj (.get ^ConcurrentHashMap (:adjacency @graph-state) source)
          radj (.get ^ConcurrentHashMap (:reverse-adjacency @graph-state) target)]
      (when adj
        (.put ^ConcurrentHashMap (:adjacency @graph-state) source (disj adj target)))
      (when radj
        (.put ^ConcurrentHashMap (:reverse-adjacency @graph-state) target (disj radj source))))))

(defn get-edge
  "Get an edge by source and target."
  [source target]
  (.get ^ConcurrentHashMap (:edges @graph-state) (edge-id source target)))

(defn update-edge!
  "Update an edge's properties."
  [source target updates]
  (when-let [edge (get-edge source target)]
    (let [updated (merge edge updates {:updated-at (System/currentTimeMillis)})]
      (.put ^ConcurrentHashMap (:edges @graph-state) (edge-id source target) updated)
      updated)))

(defn list-edges
  "List all edges."
  [& {:keys [type source target limit]}]
  (let [edges (vals (:edges @graph-state))]
    (cond->> edges
      type (filter #(= (:type %) type))
      source (filter #(= (:source %) source))
      target (filter #(= (:target %) target))
      limit (take limit))))

;; =============================================================================
;; GRAPH TRAVERSAL
;; =============================================================================

(defn get-neighbors
  "Get neighbors of a node."
  [node-id & {:keys [direction]}]
  (case direction
    :outgoing (or (.get ^ConcurrentHashMap (:adjacency @graph-state) node-id) #{})
    :incoming (or (.get ^ConcurrentHashMap (:reverse-adjacency @graph-state) node-id) #{})
    (set/union (or (.get ^ConcurrentHashMap (:adjacency @graph-state) node-id) #{})
               (or (.get ^ConcurrentHashMap (:reverse-adjacency @graph-state) node-id) #{}))))

(defn get-degree
  "Get the degree of a node."
  [node-id & {:keys [direction]}]
  (count (get-neighbors node-id :direction direction)))

(defn bfs
  "Breadth-first search from a starting node."
  [start-id & {:keys [max-depth visit-fn]}]
  (let [max-depth (or max-depth (get-in @graph-state [:config :max-path-length]))]
    (loop [queue [[start-id 0]]
           visited #{start-id}
           result []]
      (if (empty? queue)
        result
        (let [[current depth] (first queue)
              neighbors (get-neighbors current :direction :outgoing)
              unvisited (filter #(not (contains? visited %)) neighbors)
              new-visited (into visited unvisited)]
          (when visit-fn (visit-fn current depth))
          (if (>= depth max-depth)
            (recur (rest queue) new-visited (conj result {:node current :depth depth}))
            (recur (concat (rest queue) (map #(vector % (inc depth)) unvisited))
                   new-visited
                   (conj result {:node current :depth depth}))))))))

(defn dfs
  "Depth-first search from a starting node."
  [start-id & {:keys [max-depth visit-fn]}]
  (let [max-depth (or max-depth (get-in @graph-state [:config :max-path-length]))]
    (loop [stack [[start-id 0 []]]
           visited #{}
           result []]
      (if (empty? stack)
        result
        (let [[current depth path] (peek stack)]
          (if (contains? visited current)
            (recur (pop stack) visited result)
            (let [new-path (conj path current)
                  neighbors (get-neighbors current :direction :outgoing)
                  unvisited (filter #(not (contains? visited %)) neighbors)]
              (when visit-fn (visit-fn current depth path))
              (if (>= depth max-depth)
                (recur (pop stack) (conj visited current) (conj result {:node current :depth depth :path new-path}))
                (recur (into (pop stack) (map #(vector % (inc depth) new-path) unvisited))
                       (conj visited current)
                       (conj result {:node current :depth depth :path new-path}))))))))))

;; =============================================================================
;; PATH FINDING
;; =============================================================================

(defn find-path
  "Find a path between two nodes using BFS."
  [start-id end-id]
  (loop [queue [[start-id [start-id]]]
         visited #{start-id}]
    (if (empty? queue)
      nil
      (let [[current path] (first queue)]
        (if (= current end-id)
          path
          (let [neighbors (get-neighbors current :direction :outgoing)
                unvisited (filter #(not (contains? visited %)) neighbors)
                new-paths (map #(vector % (conj path %)) unvisited)]
            (recur (concat (rest queue) new-paths)
                   (into visited unvisited))))))))

(defn find-all-paths
  "Find all paths between two nodes up to max length."
  [start-id end-id & {:keys [max-length]}]
  (let [max-length (or max-length (get-in @graph-state [:config :max-path-length]))]
    (loop [stack [[start-id [start-id]]]
           paths []]
      (if (empty? stack)
        paths
        (let [[current path] (peek stack)]
          (if (= current end-id)
            (recur (pop stack) (conj paths path))
            (if (>= (count path) max-length)
              (recur (pop stack) paths)
              (let [neighbors (get-neighbors current :direction :outgoing)
                    unvisited (filter #(not (contains? (set path) %)) neighbors)
                    new-paths (map #(vector % (conj path %)) unvisited)]
                (recur (into (pop stack) new-paths) paths)))))))))

(defn shortest-path
  "Find the shortest path between two nodes."
  [start-id end-id]
  (find-path start-id end-id))

;; =============================================================================
;; SUBGRAPH EXTRACTION
;; =============================================================================

(defn extract-subgraph
  "Extract a subgraph containing specified nodes."
  [node-ids]
  (let [node-set (set node-ids)
        nodes (filter #(contains? node-set (:id %)) (vals (:nodes @graph-state)))
        edges (filter #(and (contains? node-set (:source %))
                            (contains? node-set (:target %)))
                      (vals (:edges @graph-state)))]
    {:nodes nodes :edges edges}))

(defn extract-neighborhood
  "Extract the neighborhood subgraph around a node."
  [node-id & {:keys [depth]}]
  (let [depth (or depth 1)
        visited (bfs node-id :max-depth depth)
        node-ids (set (map :node visited))]
    (extract-subgraph node-ids)))

;; =============================================================================
;; GRAPH ANALYTICS
;; =============================================================================

(defn calculate-centrality
  "Calculate degree centrality for all nodes."
  []
  (let [nodes (vals (:nodes @graph-state))
        n (count nodes)]
    (into {} (map (fn [node]
                    [(:id node) (/ (get-degree (:id node)) (max 1 (dec n)))])
                  nodes))))

(defn find-connected-components
  "Find connected components in the graph."
  []
  (loop [unvisited (set (keys (:nodes @graph-state)))
         components []]
    (if (empty? unvisited)
      components
      (let [start (first unvisited)
            visited (set (map :node (bfs start)))
            component visited]
        (recur (set/difference unvisited component)
               (conj components component))))))

(defn is-connected?
  "Check if the graph is connected."
  []
  (<= (count (find-connected-components)) 1))

(defn find-cycles
  "Find cycles in the graph."
  []
  (let [nodes (keys (:nodes @graph-state))]
    (filter seq
            (for [node nodes]
              (let [paths (find-all-paths node node :max-length 10)]
                (when (seq paths)
                  {:start node :cycles paths}))))))

;; =============================================================================
;; MENTAL MODEL GRAPH
;; =============================================================================

(defn init-mental-model-graph!
  "Initialize the graph with mental model relationships."
  []
  ;; Create nodes for key mental models
  (let [models [{:id :confirmation-bias :type :bias :label "Confirmation Bias"}
                {:id :availability-heuristic :type :bias :label "Availability Heuristic"}
                {:id :anchoring :type :bias :label "Anchoring"}
                {:id :loss-aversion :type :bias :label "Loss Aversion"}
                {:id :social-proof :type :bias :label "Social Proof"}
                {:id :sunk-cost :type :fallacy :label "Sunk Cost Fallacy"}
                {:id :incentives :type :principle :label "Incentives"}
                {:id :second-order-thinking :type :framework :label "Second-Order Thinking"}
                {:id :inversion :type :framework :label "Inversion"}
                {:id :circle-of-competence :type :framework :label "Circle of Competence"}]]
    (doseq [model models]
      (create-node! (:id model) model)))
  ;; Create relationships
  (let [relationships [{:source :confirmation-bias :target :availability-heuristic :type :amplifies}
                       {:source :social-proof :target :confirmation-bias :type :reinforces}
                       {:source :loss-aversion :target :sunk-cost :type :causes}
                       {:source :anchoring :target :loss-aversion :type :interacts}
                       {:source :incentives :target :confirmation-bias :type :triggers}
                       {:source :second-order-thinking :target :sunk-cost :type :counters}
                       {:source :inversion :target :confirmation-bias :type :counters}
                       {:source :circle-of-competence :target :availability-heuristic :type :mitigates}]]
    (doseq [rel relationships]
      (create-edge! (:source rel) (:target rel) {:type (:type rel)})))
  (log/info "Mental model graph initialized"))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-graph-stats
  "Get graph statistics."
  []
  {:nodes (.size ^ConcurrentHashMap (:nodes @graph-state))
   :edges (.size ^ConcurrentHashMap (:edges @graph-state))
   :node-count (.get ^AtomicLong (:node-count @graph-state))
   :edge-count (.get ^AtomicLong (:edge-count @graph-state))
   :connected (is-connected?)
   :components (count (find-connected-components))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-knowledge-graph!
  "Initialize knowledge graph."
  []
  (log/info "Initializing knowledge graph")
  ;; Register feature flag
  (flags/register-flag! "knowledge-graph" "Enable knowledge graph" true)
  ;; Create metrics
  (metrics/create-gauge! :knowledgegraph/nodes "Total nodes"
                         #(.size ^ConcurrentHashMap (:nodes @graph-state)))
  (metrics/create-gauge! :knowledgegraph/edges "Total edges"
                         #(.size ^ConcurrentHashMap (:edges @graph-state)))
  ;; Initialize mental model graph
  (init-mental-model-graph!)
  (log/info "Knowledge graph initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-knowledge-graph-status []
  {:enabled (flags/is-enabled? "knowledge-graph")
   :nodes (.size ^ConcurrentHashMap (:nodes @graph-state))
   :edges (.size ^ConcurrentHashMap (:edges @graph-state))
   :stats (get-graph-stats)
   :config (:config @graph-state)})
