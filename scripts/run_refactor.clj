#!/usr/bin/env clojure
;; run_refactor.clj
;; Script to run autonomous refactoring on the Mental Models codebase
;; Usage: clj -M scripts/run_refactor.clj

(ns run-refactor
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(println "")
(println "╔══════════════════════════════════════════════════════════════╗")
(println "║     AUTONOMOUS TECH DEBT ELIMINATION SYSTEM                  ║")
(println "║     Mental Models Codebase Refactoring                       ║")
(println "╚══════════════════════════════════════════════════════════════╝")
(println "")

;; -- Configuration -----------------------------------------------------------

(def config
  {:source-dirs ["src/mental_models"]
   :max-iterations 10
   :health-threshold 85
   :tangle-threshold 0.7
   :dry-run false})

;; -- File Discovery ----------------------------------------------------------

(defn find-clojure-files
  "Find all Clojure files in source directories"
  [dirs]
  (mapcat
   (fn [dir]
     (let [dir-file (io/file dir)]
       (when (.exists dir-file)
         (->> (file-seq dir-file)
              (filter #(.isFile %))
              (filter #(re-matches #".*\.clj[cs]?$" (.getName %)))
              (map #(.getPath %))))))
   dirs))

;; -- Simple Parser -----------------------------------------------------------

(defn parse-namespace
  "Extract namespace from file content"
  [content]
  (when-let [match (re-find #"\(ns\s+([\w\.\-]+)" content)]
    (second match)))

(defn parse-requires
  "Extract required namespaces from file content"
  [content]
  (->> (re-seq #"\[([a-z][\w\.\-]+)" content)
       (map second)
       (filter #(str/starts-with? % "mental-models"))
       (distinct)))

(defn parse-defns
  "Extract function definitions from file content"
  [content]
  (->> (re-seq #"\(defn?\-?\s+([\w\-\?!]+)" content)
       (map second)
       (distinct)))

(defn count-lines
  "Count lines of code (excluding blanks and comments)"
  [content]
  (->> (str/split-lines content)
       (remove str/blank?)
       (remove #(str/starts-with? (str/trim %) ";"))
       count))

;; -- DAG Construction --------------------------------------------------------

(defn build-dag
  "Build dependency DAG from source files"
  [files]
  (println "Building dependency DAG...")
  (let [file-data (for [f files]
                    (let [content (slurp f)
                          ns-name (parse-namespace content)]
                      {:file f
                       :namespace ns-name
                       :requires (parse-requires content)
                       :defns (parse-defns content)
                       :loc (count-lines content)}))]
    {:nodes (into {} (map (juxt :namespace identity) file-data))
     :edges (mapcat (fn [{:keys [namespace requires]}]
                      (map (fn [r] [namespace r]) requires))
                    file-data)}))

;; -- Metrics Calculation -----------------------------------------------------

(defn calculate-complexity
  "Calculate cyclomatic complexity approximation"
  [content]
  (let [branches (count (re-seq #"\b(if|when|cond|case|or|and)\b" content))
        loops (count (re-seq #"\b(loop|recur|for|doseq|dotimes)\b" content))
        fns (count (re-seq #"\(defn" content))]
    (+ 1 branches loops)))

(defn calculate-coupling
  "Calculate coupling score for a namespace"
  [dag namespace]
  (let [incoming (count (filter #(= namespace (second %)) (:edges dag)))
        outgoing (count (filter #(= namespace (first %)) (:edges dag)))]
    {:afferent incoming
     :efferent outgoing
     :instability (if (zero? (+ incoming outgoing))
                    0
                    (/ outgoing (+ incoming outgoing)))}))

(defn calculate-health-score
  "Calculate overall codebase health score (0-100)"
  [dag]
  (let [nodes (vals (:nodes dag))
        total-loc (reduce + (map :loc nodes))
        avg-loc (/ total-loc (max 1 (count nodes)))
        edge-count (count (:edges dag))
        node-count (count nodes)
        density (if (zero? node-count) 0 (/ edge-count (* node-count (dec node-count))))
        
        ;; Scoring factors
        loc-score (min 100 (- 100 (* 0.5 (max 0 (- avg-loc 200)))))
        density-score (- 100 (* 100 (min 1 density)))
        modularity-score (min 100 (* 10 node-count))]
    
    (int (/ (+ loc-score density-score modularity-score) 3))))

;; -- Tangle Detection --------------------------------------------------------

(defn find-tangles
  "Find tangled (highly coupled) components"
  [dag]
  (println "Detecting tangles...")
  (let [edges (:edges dag)
        ;; Find bidirectional dependencies (A->B and B->A)
        bidirectional (for [[a b] edges
                            :when (some #(= % [b a]) edges)]
                        #{a b})
        ;; Find cycles of length 3+
        cycles (for [[a b] edges
                     [c d] edges
                     :when (and (= b c) (not= a d))
                     [e f] edges
                     :when (and (= d e) (= f a))]
                 #{a b d})]
    {:bidirectional (distinct bidirectional)
     :cycles (distinct cycles)
     :tangle-score (/ (+ (count bidirectional) (* 2 (count cycles)))
                      (max 1 (count edges)))}))

;; -- Refactoring Suggestions -------------------------------------------------

(defn suggest-refactorings
  "Generate refactoring suggestions based on analysis"
  [dag tangles]
  (println "Generating refactoring suggestions...")
  (let [suggestions (atom [])]
    
    ;; Large files
    (doseq [[ns data] (:nodes dag)
            :when (> (:loc data) 300)]
      (swap! suggestions conj
             {:type :split-file
              :namespace ns
              :reason (format "File has %d LOC, consider splitting" (:loc data))
              :priority :medium}))
    
    ;; Bidirectional dependencies
    (doseq [pair (:bidirectional tangles)]
      (swap! suggestions conj
             {:type :break-cycle
              :namespaces (vec pair)
              :reason "Bidirectional dependency detected"
              :priority :high}))
    
    ;; High coupling
    (doseq [[ns data] (:nodes dag)]
      (let [coupling (calculate-coupling dag ns)]
        (when (> (:efferent coupling) 5)
          (swap! suggestions conj
                 {:type :reduce-coupling
                  :namespace ns
                  :reason (format "High efferent coupling: %d dependencies" (:efferent coupling))
                  :priority :medium}))))
    
    @suggestions))

;; -- Report Generation -------------------------------------------------------

(defn generate-report
  "Generate analysis report"
  [dag tangles suggestions health-score]
  (println "")
  (println "═══════════════════════════════════════════════════════════════")
  (println "                    ANALYSIS REPORT                            ")
  (println "═══════════════════════════════════════════════════════════════")
  (println "")
  
  (println "CODEBASE METRICS")
  (println "────────────────")
  (println (format "  Files analyzed:     %d" (count (:nodes dag))))
  (println (format "  Total LOC:          %d" (reduce + (map :loc (vals (:nodes dag))))))
  (println (format "  Dependencies:       %d" (count (:edges dag))))
  (println (format "  Health Score:       %d/100" health-score))
  (println "")
  
  (println "TANGLE ANALYSIS")
  (println "───────────────")
  (println (format "  Bidirectional deps: %d" (count (:bidirectional tangles))))
  (println (format "  Cycles detected:    %d" (count (:cycles tangles))))
  (println (format "  Tangle score:       %.2f" (float (:tangle-score tangles))))
  (println "")
  
  (when (seq suggestions)
    (println "REFACTORING SUGGESTIONS")
    (println "───────────────────────")
    (doseq [{:keys [type namespace namespaces reason priority]} suggestions]
      (println (format "  [%s] %s" 
                       (name priority)
                       (case type
                         :split-file (format "Split %s - %s" namespace reason)
                         :break-cycle (format "Break cycle between %s - %s" namespaces reason)
                         :reduce-coupling (format "Reduce coupling in %s - %s" namespace reason)
                         (str type " - " reason)))))
    (println ""))
  
  (println "═══════════════════════════════════════════════════════════════")
  (println "")
  
  {:health-score health-score
   :files (count (:nodes dag))
   :loc (reduce + (map :loc (vals (:nodes dag))))
   :tangles (+ (count (:bidirectional tangles)) (count (:cycles tangles)))
   :suggestions (count suggestions)})

;; -- Main Execution ----------------------------------------------------------

(defn run-analysis
  "Run full analysis on codebase"
  []
  (println "Starting autonomous refactoring analysis...")
  (println "")
  
  (let [files (find-clojure-files (:source-dirs config))]
    (println (format "Found %d Clojure files" (count files)))
    
    (let [dag (build-dag files)
          tangles (find-tangles dag)
          health-score (calculate-health-score dag)
          suggestions (suggest-refactorings dag tangles)]
      
      (generate-report dag tangles suggestions health-score)
      
      (cond
        (>= health-score (:health-threshold config))
        (do
          (println "✓ Codebase health is GOOD. No immediate action required.")
          (println ""))
        
        (< (:tangle-score tangles) (:tangle-threshold config))
        (do
          (println "⚠ Codebase has minor issues. Review suggestions above.")
          (println ""))
        
        :else
        (do
          (println "✗ Codebase needs attention. Prioritize high-priority suggestions.")
          (println ""))))))

;; Run the analysis
(run-analysis)

(println "Autonomous refactoring analysis complete.")
(println "")
