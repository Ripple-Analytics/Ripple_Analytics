(ns mental-models.models.core
  "Mental Models Registry Core - Electric Clojure
   
   Core registry functions for mental models.
   Model definitions are in separate category files.
   
   This is a .cljc file - runs on both client and server!"
  #?(:clj (:require [clojure.string :as str])
     :cljs (:require [clojure.string :as str])))

;; ============================================
;; Model Registry (Atoms for reactive state)
;; ============================================

(defonce !models (atom {}))
(defonce !failure-modes (atom {}))
(defonce !categories (atom {}))

(defn register-model
  "Register a model in the global registry."
  [model]
  (let [name (:name model)
        category (get model :category "general")]
    (swap! !models assoc name model)
    (swap! !categories update category (fnil conj []) name)
    (doseq [fm (:failure-modes model)]
      (swap! !failure-modes assoc (str name "_" (:name fm)) fm))
    model))

(defn get-model [name] (get @!models name))
(defn get-models-by-category [category] (map get-model (get @!categories category [])))
(defn get-all-models [] (vals @!models))
(defn get-all-categories [] (keys @!categories))

(defn search-models
  "Search models by name or description."
  [query]
  (let [q (str/lower-case (or query ""))]
    (if (empty? q)
      (vals @!models)
      (filter #(or (str/includes? (str/lower-case (or (:name %) "")) q)
                   (str/includes? (str/lower-case (or (:description %) "")) q))
              (vals @!models)))))

;; ============================================
;; Failure Mode Helper
;; ============================================

(defn failure
  "Create a failure mode map."
  [name severity description & {:keys [signals safeguards]}]
  {:name name
   :severity severity
   :description description
   :signals (or signals [])
   :safeguards (or safeguards [])})

;; ============================================
;; Model Count & Stats
;; ============================================

(defn model-count [] (count @!models))
(defn category-count [] (count @!categories))
(defn failure-mode-count [] (count @!failure-modes))

(defn get-stats
  "Get registry statistics."
  []
  {:models (model-count)
   :categories (category-count)
   :failure-modes (failure-mode-count)})
