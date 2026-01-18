(ns mental-models.tech-debt
  "Tech Debt Eliminator - Autonomous Code Quality Improvement
   
   Uses DAG analysis to identify and eliminate tech debt:
   1. Encode system as a program DAG
   2. Visualize the DAG
   3. Find locally tangled connected components
   4. Use AI to refactor for referential transparency
   5. Verify referential equivalence
   
   Based on the methodology:
   - Parse code into dependency graph
   - Detect high-coupling areas (tangles)
   - Suggest refactoring using LLM
   - Verify behavior preservation"
  #?(:clj (:require [clojure.string :as str]
                    [clojure.set :as set])
     :cljs (:require [clojure.string :as str]
                     [clojure.set :as set])))

;; ============================================
;; DAG Data Structures
;; ============================================

(defonce !dag (atom {:nodes {} :edges []}))
(defonce !tangles (atom []))
(defonce !refactoring-suggestions (atom []))

(defn create-node
  "Create a node in the DAG representing a code unit."
  [id & {:keys [type name file line complexity dependencies]
         :or {type :function name "" file "" line 0 complexity 1 dependencies []}}]
  {:id id
   :type type
   :name name
   :file file
   :line line
   :complexity complexity
   :dependencies dependencies
   :metadata {}})

(defn create-edge
  "Create an edge representing a dependency between nodes."
  [from to & {:keys [type weight]
              :or {type :calls weight 1}}]
  {:from from
   :to to
   :type type
   :weight weight})

;; ============================================
;; DAG Construction
;; ============================================

(defn add-node!
  "Add a node to the DAG."
  [node]
  (swap! !dag update :nodes assoc (:id node) node))

(defn add-edge!
  "Add an edge to the DAG."
  [edge]
  (swap! !dag update :edges conj edge))

(defn build-dag-from-code
  "Build a DAG from code analysis results.
   Takes a map of {:functions [...] :dependencies [...]}."
  [{:keys [functions dependencies]}]
  (reset! !dag {:nodes {} :edges []})
  (doseq [f functions]
    (add-node! (create-node (:id f)
                           :type :function
                           :name (:name f)
                           :file (:file f)
                           :line (:line f)
                           :complexity (:complexity f 1)
                           :dependencies (:deps f []))))
  (doseq [d dependencies]
    (add-edge! (create-edge (:from d) (:to d)
                           :type (:type d :calls)
                           :weight (:weight d 1))))
  @!dag)

;; ============================================
;; Graph Analysis Algorithms
;; ============================================

(defn get-adjacency-list
  "Convert edge list to adjacency list."
  [dag]
  (reduce (fn [adj {:keys [from to]}]
            (update adj from (fnil conj #{}) to))
          {}
          (:edges dag)))

(defn get-reverse-adjacency-list
  "Get reverse adjacency list (incoming edges)."
  [dag]
  (reduce (fn [adj {:keys [from to]}]
            (update adj to (fnil conj #{}) from))
          {}
          (:edges dag)))

(defn dfs-visit
  "Depth-first search visit for topological sort."
  [node adj visited finish-order]
  (if (contains? @visited node)
    finish-order
    (do
      (swap! visited conj node)
      (let [neighbors (get adj node #{})
            updated-order (reduce (fn [order neighbor]
                                   (dfs-visit neighbor adj visited order))
                                 finish-order
                                 neighbors)]
        (conj updated-order node)))))

(defn topological-sort
  "Topological sort of the DAG."
  [dag]
  (let [adj (get-adjacency-list dag)
        nodes (keys (:nodes dag))
        visited (atom #{})
        finish-order (atom [])]
    (doseq [node nodes]
      (when-not (contains? @visited node)
        (swap! finish-order #(dfs-visit node adj visited %))))
    (reverse @finish-order)))

(defn find-strongly-connected-components
  "Find strongly connected components using Kosaraju's algorithm.
   SCCs with more than one node indicate cycles (tangles)."
  [dag]
  (let [adj (get-adjacency-list dag)
        rev-adj (get-reverse-adjacency-list dag)
        nodes (keys (:nodes dag))
        ;; First DFS to get finish order
        visited1 (atom #{})
        finish-order (atom [])
        _ (doseq [node nodes]
            (when-not (contains? @visited1 node)
              (swap! finish-order #(dfs-visit node adj visited1 %))))
        ;; Second DFS on reverse graph in reverse finish order
        visited2 (atom #{})
        sccs (atom [])]
    (doseq [node (reverse @finish-order)]
      (when-not (contains? @visited2 node)
        (let [component (atom [])]
          (letfn [(dfs2 [n]
                    (when-not (contains? @visited2 n)
                      (swap! visited2 conj n)
                      (swap! component conj n)
                      (doseq [neighbor (get rev-adj n #{})]
                        (dfs2 neighbor))))]
            (dfs2 node))
          (swap! sccs conj @component))))
    @sccs))

(defn calculate-node-metrics
  "Calculate metrics for each node in the DAG."
  [dag]
  (let [adj (get-adjacency-list dag)
        rev-adj (get-reverse-adjacency-list dag)]
    (into {}
          (map (fn [[id node]]
                 [id (assoc node
                           :in-degree (count (get rev-adj id #{}))
                           :out-degree (count (get adj id #{}))
                           :fan-in (count (get rev-adj id #{}))
                           :fan-out (count (get adj id #{}))
                           :coupling (+ (count (get rev-adj id #{}))
                                       (count (get adj id #{}))))])
               (:nodes dag)))))

;; ============================================
;; Tangle Detection
;; ============================================

(defn detect-tangles
  "Detect tangled components in the DAG.
   A tangle is a highly coupled subgraph that should be refactored."
  [dag & {:keys [coupling-threshold complexity-threshold]
          :or {coupling-threshold 5 complexity-threshold 10}}]
  (let [sccs (find-strongly-connected-components dag)
        node-metrics (calculate-node-metrics dag)
        ;; Tangles are SCCs with multiple nodes (cycles)
        cyclic-tangles (filter #(> (count %) 1) sccs)
        ;; Also detect high-coupling nodes
        high-coupling-nodes (filter (fn [[id metrics]]
                                     (> (:coupling metrics) coupling-threshold))
                                   node-metrics)
        ;; High complexity nodes
        high-complexity-nodes (filter (fn [[id node]]
                                       (> (:complexity node 1) complexity-threshold))
                                     (:nodes dag))]
    {:cyclic-tangles cyclic-tangles
     :high-coupling-nodes (map first high-coupling-nodes)
     :high-complexity-nodes (map first high-complexity-nodes)
     :total-tangles (+ (count cyclic-tangles)
                      (count high-coupling-nodes)
                      (count high-complexity-nodes))}))

(defn calculate-tangle-severity
  "Calculate severity score for a tangle."
  [tangle-nodes dag]
  (let [node-metrics (calculate-node-metrics dag)
        total-coupling (reduce + (map #(:coupling (get node-metrics %)) tangle-nodes))
        total-complexity (reduce + (map #(:complexity (get (:nodes dag) %) 1) tangle-nodes))
        size (count tangle-nodes)]
    {:nodes tangle-nodes
     :size size
     :total-coupling total-coupling
     :total-complexity total-complexity
     :severity-score (+ (* size 2)
                       (* total-coupling 1.5)
                       total-complexity)
     :priority (cond
                (> size 5) :critical
                (> total-coupling 20) :high
                (> total-complexity 30) :high
                (> size 3) :medium
                :else :low)}))

;; ============================================
;; Refactoring Suggestions
;; ============================================

(defn suggest-refactoring
  "Generate refactoring suggestions for a tangle."
  [tangle dag]
  (let [severity (calculate-tangle-severity (:nodes tangle) dag)
        nodes (:nodes tangle)
        node-details (map #(get (:nodes dag) %) nodes)]
    {:tangle-id (hash nodes)
     :severity severity
     :nodes node-details
     :suggestions
     (cond-> []
       ;; Cyclic dependency
       (> (count nodes) 1)
       (conj {:type :break-cycle
              :description "Break cyclic dependency by introducing an interface or event"
              :technique "Dependency Inversion Principle"
              :mental-model "inversion"})
       
       ;; High coupling
       (> (:total-coupling severity) 10)
       (conj {:type :reduce-coupling
              :description "Reduce coupling by extracting shared functionality"
              :technique "Extract Method/Class"
              :mental-model "leverage"})
       
       ;; High complexity
       (> (:total-complexity severity) 20)
       (conj {:type :simplify
              :description "Simplify by breaking into smaller, focused functions"
              :technique "Single Responsibility Principle"
              :mental-model "first-principles"})
       
       ;; Large tangle
       (> (count nodes) 3)
       (conj {:type :modularize
              :description "Extract tangle into a separate module with clear interface"
              :technique "Module Extraction"
              :mental-model "circle-of-competence"}))}))

(defn generate-llm-refactoring-prompt
  "Generate a prompt for LLM to suggest specific refactoring."
  [tangle dag]
  (let [nodes (:nodes tangle)
        node-details (map #(get (:nodes dag) %) nodes)
        severity (calculate-tangle-severity nodes dag)]
    (str "Analyze this code tangle and suggest refactoring to increase referential transparency:\n\n"
         "TANGLE DETAILS:\n"
         "- Nodes: " (str/join ", " (map :name node-details)) "\n"
         "- Size: " (count nodes) " functions\n"
         "- Total coupling: " (:total-coupling severity) "\n"
         "- Total complexity: " (:total-complexity severity) "\n"
         "- Severity: " (:priority severity) "\n\n"
         "NODE DETAILS:\n"
         (str/join "\n" (map (fn [n]
                              (str "- " (:name n) " (file: " (:file n) ", line: " (:line n) ")"
                                   " complexity=" (:complexity n 1)
                                   " deps=" (str/join "," (:dependencies n []))))
                            node-details))
         "\n\n"
         "Please suggest:\n"
         "1. Specific refactoring steps to break this tangle\n"
         "2. How to increase referential transparency (make functions pure)\n"
         "3. New module/interface structure\n"
         "4. Test cases to verify referential equivalence after refactoring")))

;; ============================================
;; Referential Equivalence Verification
;; ============================================

(defn generate-equivalence-tests
  "Generate test cases to verify referential equivalence after refactoring."
  [original-node refactored-node]
  {:test-id (str "equiv-" (hash [(:id original-node) (:id refactored-node)]))
   :original (:id original-node)
   :refactored (:id refactored-node)
   :test-cases
   [{:type :property-based
     :description "Property-based test: same inputs produce same outputs"
     :generator "Generate random inputs within valid domain"}
    {:type :boundary
     :description "Boundary test: edge cases produce same results"
     :cases ["empty input" "null input" "max values" "min values"]}
    {:type :regression
     :description "Regression test: known inputs produce known outputs"
     :cases "Capture current behavior as golden tests"}
    {:type :performance
     :description "Performance test: refactored version is not slower"
     :threshold "Within 10% of original"}]})

(defn verify-referential-transparency
  "Check if a function is referentially transparent."
  [node dag]
  (let [deps (:dependencies node [])
        has-side-effects (some #(str/includes? (str %) "!") deps)
        has-io (some #(or (str/includes? (str %) "print")
                         (str/includes? (str %) "read")
                         (str/includes? (str %) "write")
                         (str/includes? (str %) "http"))
                    deps)
        has-state (some #(str/includes? (str %) "atom") deps)]
    {:node-id (:id node)
     :is-pure (not (or has-side-effects has-io has-state))
     :issues (cond-> []
              has-side-effects (conj "Uses functions with side effects (!)")
              has-io (conj "Performs I/O operations")
              has-state (conj "Accesses mutable state (atoms)"))
     :recommendation (cond
                      has-state "Extract state to function parameters"
                      has-io "Separate I/O from pure logic"
                      has-side-effects "Replace side effects with return values"
                      :else "Function is already referentially transparent")}))

;; ============================================
;; DAG Visualization Data
;; ============================================

(defn export-dag-for-visualization
  "Export DAG in a format suitable for D3.js visualization."
  [dag]
  (let [tangles (detect-tangles dag)
        tangle-node-set (set (flatten (:cyclic-tangles tangles)))
        high-coupling-set (set (:high-coupling-nodes tangles))
        high-complexity-set (set (:high-complexity-nodes tangles))]
    {:nodes (map (fn [[id node]]
                  {:id id
                   :name (:name node)
                   :type (:type node)
                   :file (:file node)
                   :complexity (:complexity node 1)
                   :group (cond
                           (contains? tangle-node-set id) "tangle"
                           (contains? high-coupling-set id) "high-coupling"
                           (contains? high-complexity-set id) "high-complexity"
                           :else "normal")
                   :size (+ 5 (* (:complexity node 1) 2))})
                (:nodes dag))
     :links (map (fn [{:keys [from to type weight]}]
                  {:source from
                   :target to
                   :type type
                   :value weight})
                (:edges dag))
     :tangles tangles
     :metrics {:total-nodes (count (:nodes dag))
               :total-edges (count (:edges dag))
               :cyclic-tangles (count (:cyclic-tangles tangles))
               :high-coupling-nodes (count (:high-coupling-nodes tangles))
               :high-complexity-nodes (count (:high-complexity-nodes tangles))}}))

;; ============================================
;; Main Analysis Pipeline
;; ============================================

(defn analyze-codebase
  "Run full tech debt analysis on a codebase DAG."
  [dag]
  (let [tangles (detect-tangles dag)
        node-metrics (calculate-node-metrics dag)
        tangle-severities (map #(calculate-tangle-severity % dag) 
                              (:cyclic-tangles tangles))
        suggestions (map #(suggest-refactoring {:nodes %} dag)
                        (:cyclic-tangles tangles))
        transparency-checks (map #(verify-referential-transparency (val %) dag)
                                (:nodes dag))]
    {:summary {:total-nodes (count (:nodes dag))
               :total-edges (count (:edges dag))
               :total-tangles (:total-tangles tangles)
               :cyclic-tangles (count (:cyclic-tangles tangles))
               :high-coupling-nodes (count (:high-coupling-nodes tangles))
               :high-complexity-nodes (count (:high-complexity-nodes tangles))
               :pure-functions (count (filter :is-pure transparency-checks))
               :impure-functions (count (filter #(not (:is-pure %)) transparency-checks))}
     :tangles tangles
     :tangle-severities (sort-by :severity-score > tangle-severities)
     :refactoring-suggestions suggestions
     :transparency-analysis transparency-checks
     :visualization (export-dag-for-visualization dag)
     :priority-order (map :nodes (sort-by :severity-score > tangle-severities))}))

(defn get-next-refactoring-target
  "Get the highest priority tangle to refactor next."
  [analysis]
  (first (:tangle-severities analysis)))

(defn generate-refactoring-plan
  "Generate a complete refactoring plan for the codebase."
  [analysis]
  (let [targets (take 5 (:tangle-severities analysis))]
    {:plan-id (str "refactor-" (System/currentTimeMillis))
     :total-targets (count (:tangle-severities analysis))
     :immediate-targets (count targets)
     :steps (map-indexed
             (fn [idx target]
               {:step (inc idx)
                :target (:nodes target)
                :priority (:priority target)
                :severity-score (:severity-score target)
                :actions (get-in (first (filter #(= (:nodes %) (:nodes target))
                                               (:refactoring-suggestions analysis)))
                                [:suggestions])})
             targets)
     :estimated-impact {:coupling-reduction "30-50%"
                       :complexity-reduction "20-40%"
                       :maintainability-improvement "Significant"}}))
