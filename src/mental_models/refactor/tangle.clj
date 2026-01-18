(ns mental-models.refactor.tangle
  "Tangle Detector - Find High-Connectivity Clusters
   Identifies locally tangled connected components in the DAG"
  (:require [mental-models.refactor.dag :as dag]
            [clojure.set :as set]
            [taoensso.timbre :as log]))

;; -- Graph Algorithms --------------------------------------------------------

(defn build-adjacency-map
  "Build adjacency map from DAG edges"
  [dag]
  (let [edges (:edges dag)]
    (reduce
     (fn [adj {:keys [source target]}]
       (-> adj
           (update source (fnil conj #{}) target)
           (update target (fnil conj #{}) source)))
     {}
     edges)))

(defn bfs-component
  "Find connected component using BFS"
  [start adjacency visited]
  (loop [queue [start]
         component #{}
         seen visited]
    (if (empty? queue)
      [component seen]
      (let [node (first queue)
            neighbors (get adjacency node #{})]
        (if (seen node)
          (recur (rest queue) component seen)
          (recur (into (rest queue) (remove seen neighbors))
                 (conj component node)
                 (conj seen node)))))))

(defn find-connected-components
  "Find all connected components in the DAG"
  [dag]
  (let [adjacency (build-adjacency-map dag)
        all-nodes (set (map :id (:nodes dag)))]
    (loop [remaining all-nodes
           components []
           visited #{}]
      (if (empty? remaining)
        components
        (let [start (first remaining)
              [component new-visited] (bfs-component start adjacency visited)]
          (recur (set/difference remaining component)
                 (conj components component)
                 new-visited))))))

;; -- Tangle Detection --------------------------------------------------------

(defn calculate-cluster-density
  "Calculate edge density within a cluster"
  [cluster-nodes dag]
  (let [cluster-set (set cluster-nodes)
        internal-edges (filter (fn [{:keys [source target]}]
                                (and (cluster-set source)
                                     (cluster-set target)))
                              (:edges dag))
        n (count cluster-nodes)
        max-edges (/ (* n (dec n)) 2)]
    (if (zero? max-edges)
      0
      (/ (count internal-edges) max-edges))))

(defn calculate-cluster-complexity
  "Calculate total complexity of nodes in a cluster"
  [cluster-nodes dag]
  (let [cluster-set (set cluster-nodes)
        nodes (filter #(cluster-set (:id %)) (:nodes dag))]
    (reduce + (map #(or (:complexity %) 0) nodes))))

(defn calculate-cluster-cohesion
  "Calculate cohesion score (internal vs external edges)"
  [cluster-nodes dag]
  (let [cluster-set (set cluster-nodes)
        edges (:edges dag)
        internal (count (filter (fn [{:keys [source target]}]
                                 (and (cluster-set source)
                                      (cluster-set target)))
                               edges))
        external (count (filter (fn [{:keys [source target]}]
                                 (or (and (cluster-set source)
                                          (not (cluster-set target)))
                                     (and (not (cluster-set source))
                                          (cluster-set target))))
                               edges))]
    (if (zero? (+ internal external))
      1.0
      (/ internal (+ internal external)))))

(defn identify-tangle
  "Identify if a cluster is a tangle based on metrics"
  [cluster-nodes dag & {:keys [density-threshold complexity-threshold size-threshold]
                        :or {density-threshold 0.3
                             complexity-threshold 50
                             size-threshold 3}}]
  (let [size (count cluster-nodes)
        density (calculate-cluster-density cluster-nodes dag)
        complexity (calculate-cluster-complexity cluster-nodes dag)
        cohesion (calculate-cluster-cohesion cluster-nodes dag)]
    (when (and (>= size size-threshold)
               (or (>= density density-threshold)
                   (>= complexity complexity-threshold)))
      {:nodes cluster-nodes
       :size size
       :density density
       :complexity complexity
       :cohesion cohesion
       :severity (cond
                   (and (>= density 0.6) (>= complexity 100)) :critical
                   (and (>= density 0.4) (>= complexity 50)) :high
                   (>= density 0.3) :medium
                   :else :low)})))

;; -- Subgraph Extraction -----------------------------------------------------

(defn extract-subgraph
  "Extract a subgraph containing only specified nodes"
  [node-ids dag]
  (let [node-set (set node-ids)
        nodes (filter #(node-set (:id %)) (:nodes dag))
        edges (filter (fn [{:keys [source target]}]
                       (and (node-set source) (node-set target)))
                     (:edges dag))]
    {:nodes (vec nodes)
     :edges (vec edges)}))

(defn find-local-tangles
  "Find tangles within a neighborhood of a node"
  [node-id dag & {:keys [depth] :or {depth 2}}]
  (let [adjacency (build-adjacency-map dag)]
    (loop [frontier #{node-id}
           visited #{node-id}
           d 0]
      (if (>= d depth)
        (identify-tangle visited dag)
        (let [neighbors (reduce set/union
                                (map #(get adjacency % #{}) frontier))
              new-nodes (set/difference neighbors visited)]
          (recur new-nodes
                 (set/union visited new-nodes)
                 (inc d)))))))

;; -- Cycle Detection ---------------------------------------------------------

(defn find-cycles
  "Find cycles in the DAG (indicates problematic dependencies)"
  [dag]
  (let [adjacency (reduce
                   (fn [adj {:keys [source target]}]
                     (update adj source (fnil conj #{}) target))
                   {}
                   (:edges dag))
        all-nodes (set (map :id (:nodes dag)))]
    (loop [remaining all-nodes
           cycles []]
      (if (empty? remaining)
        cycles
        (let [start (first remaining)
              ;; DFS to find back edges
              cycle (loop [stack [[start [start]]]
                           visited #{}]
                      (if (empty? stack)
                        nil
                        (let [[node path] (peek stack)
                              neighbors (get adjacency node #{})]
                          (if (visited node)
                            (recur (pop stack) visited)
                            (let [back-edge (first (filter #(some #{%} (butlast path)) neighbors))]
                              (if back-edge
                                (let [cycle-start (.indexOf path back-edge)]
                                  (subvec path cycle-start))
                                (recur (into (pop stack)
                                            (map #(vector % (conj path %))
                                                 (remove visited neighbors)))
                                       (conj visited node))))))))]
          (recur (rest remaining)
                 (if cycle (conj cycles cycle) cycles)))))))

;; -- Comprehensive Tangle Analysis -------------------------------------------

(defn analyze-tangles
  "Comprehensive tangle analysis of the DAG"
  [dag]
  (let [components (find-connected-components dag)
        tangles (keep #(identify-tangle % dag) components)
        cycles (find-cycles dag)
        dag-with-degrees (dag/calculate-degrees dag)
        hubs (dag/find-hubs dag-with-degrees :threshold 5)]
    {:tangles (sort-by #(case (:severity %)
                          :critical 0
                          :high 1
                          :medium 2
                          :low 3)
                       tangles)
     :cycles cycles
     :hubs hubs
     :summary {:total-tangles (count tangles)
               :critical-tangles (count (filter #(= :critical (:severity %)) tangles))
               :high-tangles (count (filter #(= :high (:severity %)) tangles))
               :cycle-count (count cycles)
               :hub-count (count hubs)
               :total-tangle-complexity (reduce + (map :complexity tangles))}}))

;; -- Tangle Prioritization ---------------------------------------------------

(defn calculate-refactor-priority
  "Calculate priority score for refactoring a tangle"
  [tangle]
  (let [{:keys [size density complexity cohesion severity]} tangle
        ;; Higher priority for:
        ;; - Higher complexity (harder to understand)
        ;; - Higher density (more tangled)
        ;; - Lower cohesion (less modular)
        ;; - Larger size (more impact)
        base-score (* complexity (/ density (max 0.1 cohesion)))
        severity-multiplier (case severity
                              :critical 4
                              :high 3
                              :medium 2
                              :low 1)]
    (* base-score severity-multiplier (Math/log (inc size)))))

(defn prioritize-tangles
  "Prioritize tangles for refactoring"
  [tangles]
  (->> tangles
       (map #(assoc % :priority (calculate-refactor-priority %)))
       (sort-by :priority >)))

;; -- Refactoring Suggestions -------------------------------------------------

(defn suggest-refactoring
  "Suggest refactoring approach for a tangle"
  [tangle dag]
  (let [{:keys [nodes size density complexity cohesion severity]} tangle
        subgraph (extract-subgraph nodes dag)]
    {:tangle tangle
     :suggestions
     (cond-> []
       ;; High density suggests extracting common functionality
       (>= density 0.5)
       (conj {:type :extract-module
              :reason "High interconnection suggests shared functionality"
              :action "Extract common logic into a separate module"})
       
       ;; Low cohesion suggests splitting
       (<= cohesion 0.3)
       (conj {:type :split-module
              :reason "Low cohesion indicates mixed responsibilities"
              :action "Split into separate modules by responsibility"})
       
       ;; High complexity suggests simplification
       (>= complexity 100)
       (conj {:type :simplify
              :reason "High cyclomatic complexity"
              :action "Break down complex functions into smaller units"})
       
       ;; Large size suggests decomposition
       (>= size 10)
       (conj {:type :decompose
              :reason "Large cluster size"
              :action "Decompose into smaller, focused components"})
       
       ;; Always suggest improving referential transparency
       true
       (conj {:type :increase-transparency
              :reason "General improvement"
              :action "Reduce side effects and increase pure functions"}))}))

;; -- Main Entry Point --------------------------------------------------------

(defn detect-and-prioritize
  "Main entry point: detect tangles and prioritize for refactoring"
  [dag]
  (log/info "Detecting tangles in DAG...")
  (let [analysis (analyze-tangles dag)
        prioritized (prioritize-tangles (:tangles analysis))
        suggestions (map #(suggest-refactoring % dag) (take 5 prioritized))]
    {:analysis analysis
     :prioritized-tangles prioritized
     :top-suggestions suggestions
     :action-plan (for [{:keys [tangle suggestions]} suggestions]
                    {:target (take 5 (:nodes tangle))
                     :severity (:severity tangle)
                     :priority (:priority tangle)
                     :actions (map :action suggestions)})}))
