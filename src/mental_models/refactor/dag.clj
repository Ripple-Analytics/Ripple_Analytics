(ns mental-models.refactor.dag
  "DAG Encoder - Parse Clojure code into dependency graph
   Encodes the system as a program DAG for analysis and refactoring"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [taoensso.timbre :as log])
  (:import [java.io PushbackReader]))

;; -- Code Parsing ------------------------------------------------------------

(defn read-all-forms
  "Read all forms from a Clojure source file"
  [file-path]
  (try
    (with-open [reader (PushbackReader. (io/reader file-path))]
      (loop [forms []]
        (let [form (try (read reader false ::eof)
                       (catch Exception e ::error))]
          (cond
            (= form ::eof) forms
            (= form ::error) forms
            :else (recur (conj forms form))))))
    (catch Exception e
      (log/warn "Failed to parse" file-path (.getMessage e))
      [])))

(defn find-clj-files
  "Find all .clj, .cljs, .cljc files in a directory"
  [dir]
  (let [dir-file (io/file dir)]
    (->> (file-seq dir-file)
         (filter #(.isFile %))
         (filter #(re-matches #".*\.clj[cs]?$" (.getName %)))
         (map #(.getAbsolutePath %)))))

;; -- Symbol Extraction -------------------------------------------------------

(defn extract-ns-declaration
  "Extract namespace from ns form"
  [form]
  (when (and (seq? form) (= 'ns (first form)))
    {:name (second form)
     :requires (for [clause (rest form)
                     :when (and (seq? clause) (= :require (first clause)))
                     req (rest clause)]
                 (cond
                   (symbol? req) req
                   (vector? req) (first req)
                   :else nil))
     :imports (for [clause (rest form)
                    :when (and (seq? clause) (= :import (first clause)))
                    imp (rest clause)]
                imp)}))

(defn extract-definitions
  "Extract all definitions from forms"
  [forms]
  (for [form forms
        :when (and (seq? form)
                   (contains? #{'def 'defn 'defn- 'defmacro 'defmulti
                                'defmethod 'defonce 'defprotocol 'defrecord
                                'deftype 'e/defn} (first form)))]
    {:type (first form)
     :name (second form)
     :form form
     :arity (when (= 'defn (first form))
              (let [body (drop 2 form)
                    params (first (filter vector? body))]
                (when params (count params))))
     :doc (when (string? (nth form 2 nil))
            (nth form 2))
     :meta (when (map? (nth form 2 nil))
             (nth form 2))}))

(defn extract-references
  "Extract all symbol references from a form"
  [form]
  (let [refs (atom #{})]
    (walk/postwalk
     (fn [x]
       (when (symbol? x)
         (swap! refs conj x))
       x)
     form)
    @refs))

(defn extract-calls
  "Extract function calls from a form"
  [form]
  (let [calls (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (seq? x) (symbol? (first x)))
         (swap! calls conj {:fn (first x)
                            :arity (dec (count x))}))
       x)
     form)
    @calls))

;; -- DAG Construction --------------------------------------------------------

(defn build-file-node
  "Build a DAG node for a single file"
  [file-path]
  (let [forms (read-all-forms file-path)
        ns-decl (some extract-ns-declaration forms)
        definitions (extract-definitions forms)]
    {:id (str (:name ns-decl))
     :path file-path
     :namespace (:name ns-decl)
     :requires (set (:requires ns-decl))
     :definitions (vec definitions)
     :definition-count (count definitions)
     :loc (count (str/split-lines (slurp file-path)))
     :complexity (calculate-complexity forms)}))

(defn calculate-complexity
  "Calculate cyclomatic complexity of forms"
  [forms]
  (let [complexity-keywords #{'if 'when 'when-not 'when-let 'when-some
                              'if-let 'if-some 'cond 'condp 'case
                              'and 'or 'loop 'for 'doseq 'while
                              'try 'catch}
        count-complexity (fn [form]
                          (let [counter (atom 1)]
                            (walk/postwalk
                             (fn [x]
                               (when (and (seq? x) (complexity-keywords (first x)))
                                 (swap! counter inc))
                               x)
                             form)
                            @counter))]
    (reduce + (map count-complexity forms))))

(defn build-definition-nodes
  "Build DAG nodes for individual definitions within a namespace"
  [file-node]
  (for [{:keys [name type form]} (:definitions file-node)]
    {:id (str (:namespace file-node) "/" name)
     :namespace (:namespace file-node)
     :name name
     :type type
     :references (extract-references form)
     :calls (extract-calls form)
     :loc (count (str/split-lines (pr-str form)))
     :complexity (calculate-complexity [form])}))

(defn build-edges
  "Build edges between nodes based on dependencies"
  [nodes]
  (let [node-ids (set (map :id nodes))
        ns-to-defs (group-by :namespace nodes)]
    (for [node nodes
          ref (:references node)
          :let [ref-str (str ref)
                ;; Check if it's a qualified reference
                qualified-ref (when (str/includes? ref-str "/")
                               ref-str)
                ;; Check if it's in the same namespace
                local-ref (str (:namespace node) "/" ref)
                target (or (when (node-ids qualified-ref) qualified-ref)
                           (when (node-ids local-ref) local-ref))]
          :when target]
      {:source (:id node)
       :target target
       :type :reference})))

(defn build-namespace-edges
  "Build edges between namespaces based on requires"
  [file-nodes]
  (for [node file-nodes
        req (:requires node)
        :when req]
    {:source (:id node)
     :target (str req)
     :type :require}))

;; -- Full DAG Builder --------------------------------------------------------

(defn build-dag
  "Build complete DAG from a source directory"
  [source-dir]
  (let [files (find-clj-files source-dir)
        file-nodes (map build-file-node files)
        def-nodes (mapcat build-definition-nodes file-nodes)
        all-nodes (concat file-nodes def-nodes)
        def-edges (build-edges def-nodes)
        ns-edges (build-namespace-edges file-nodes)]
    {:nodes (vec all-nodes)
     :edges (vec (concat def-edges ns-edges))
     :metadata {:source-dir source-dir
                :file-count (count files)
                :definition-count (count def-nodes)
                :edge-count (+ (count def-edges) (count ns-edges))
                :total-loc (reduce + (map :loc file-nodes))
                :total-complexity (reduce + (map :complexity file-nodes))
                :built-at (java.util.Date.)}}))

;; -- DAG Analysis ------------------------------------------------------------

(defn calculate-in-degree
  "Calculate in-degree for each node"
  [dag]
  (let [edge-targets (frequencies (map :target (:edges dag)))]
    (for [node (:nodes dag)]
      (assoc node :in-degree (get edge-targets (:id node) 0)))))

(defn calculate-out-degree
  "Calculate out-degree for each node"
  [dag]
  (let [edge-sources (frequencies (map :source (:edges dag)))]
    (for [node (:nodes dag)]
      (assoc node :out-degree (get edge-sources (:id node) 0)))))

(defn calculate-degrees
  "Calculate both in and out degrees"
  [dag]
  (let [in-degrees (frequencies (map :target (:edges dag)))
        out-degrees (frequencies (map :source (:edges dag)))]
    (assoc dag :nodes
           (vec (for [node (:nodes dag)]
                  (assoc node
                         :in-degree (get in-degrees (:id node) 0)
                         :out-degree (get out-degrees (:id node) 0)
                         :total-degree (+ (get in-degrees (:id node) 0)
                                          (get out-degrees (:id node) 0))))))))

(defn find-hubs
  "Find hub nodes (high connectivity)"
  [dag & {:keys [threshold] :or {threshold 5}}]
  (let [dag-with-degrees (calculate-degrees dag)]
    (->> (:nodes dag-with-degrees)
         (filter #(> (:total-degree %) threshold))
         (sort-by :total-degree >))))

(defn find-orphans
  "Find orphan nodes (no connections)"
  [dag]
  (let [dag-with-degrees (calculate-degrees dag)]
    (->> (:nodes dag-with-degrees)
         (filter #(zero? (:total-degree %))))))

(defn find-leaves
  "Find leaf nodes (only incoming, no outgoing)"
  [dag]
  (let [dag-with-degrees (calculate-degrees dag)]
    (->> (:nodes dag-with-degrees)
         (filter #(and (pos? (:in-degree %))
                       (zero? (:out-degree %)))))))

;; -- Serialization -----------------------------------------------------------

(defn dag->edn
  "Serialize DAG to EDN format"
  [dag]
  (pr-str dag))

(defn dag->json
  "Serialize DAG to JSON format for visualization"
  [dag]
  (let [nodes (for [node (:nodes dag)]
                {:id (:id node)
                 :label (or (:name node) (:id node))
                 :type (name (or (:type node) :namespace))
                 :loc (:loc node)
                 :complexity (:complexity node)
                 :inDegree (:in-degree node 0)
                 :outDegree (:out-degree node 0)})
        links (for [edge (:edges dag)]
                {:source (:source edge)
                 :target (:target edge)
                 :type (name (:type edge))})]
    {:nodes (vec nodes)
     :links (vec links)
     :metadata (:metadata dag)}))

(defn save-dag!
  "Save DAG to file"
  [dag file-path]
  (spit file-path (dag->edn dag)))

(defn load-dag
  "Load DAG from file"
  [file-path]
  (edn/read-string (slurp file-path)))

;; -- Main Entry Point --------------------------------------------------------

(defn analyze-codebase
  "Analyze a codebase and return DAG with metrics"
  [source-dir]
  (log/info "Analyzing codebase at" source-dir)
  (let [dag (build-dag source-dir)
        dag-with-degrees (calculate-degrees dag)
        hubs (find-hubs dag-with-degrees)
        orphans (find-orphans dag-with-degrees)
        leaves (find-leaves dag-with-degrees)]
    {:dag dag-with-degrees
     :analysis {:hubs (take 10 hubs)
                :orphan-count (count orphans)
                :leaf-count (count leaves)
                :avg-complexity (/ (:total-complexity (:metadata dag))
                                   (max 1 (:file-count (:metadata dag))))
                :avg-loc (/ (:total-loc (:metadata dag))
                            (max 1 (:file-count (:metadata dag))))}}))
