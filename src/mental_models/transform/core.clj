(ns mental-models.transform.core
  "Data Transformation Module for Mental Models Pipeline
   
   Provides data transformation with:
   - Map/filter/reduce operations
   - Data normalization
   - Schema mapping
   - Type coercion
   - Pipeline composition"
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]))

;; =============================================================================
;; BASIC TRANSFORMATIONS
;; =============================================================================

(defn transform-map
  "Apply a function to each element in a collection."
  [f coll]
  (map f coll))

(defn transform-filter
  "Filter elements based on a predicate."
  [pred coll]
  (filter pred coll))

(defn transform-reduce
  "Reduce a collection to a single value."
  [f init coll]
  (reduce f init coll))

(defn transform-group-by
  "Group elements by a key function."
  [key-fn coll]
  (group-by key-fn coll))

(defn transform-sort-by
  "Sort elements by a key function."
  [key-fn coll]
  (sort-by key-fn coll))

;; =============================================================================
;; DATA NORMALIZATION
;; =============================================================================

(defn normalize-string
  "Normalize a string (trim, lowercase)."
  [s]
  (when s
    (-> s str str/trim str/lower-case)))

(defn normalize-whitespace
  "Normalize whitespace in a string."
  [s]
  (when s
    (str/replace s #"\s+" " ")))

(defn normalize-keys
  "Normalize map keys to keywords."
  [m]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {} (map (fn [[k v]] [(keyword (normalize-string (name k))) v]) x))
       x))
   m))

(defn normalize-values
  "Apply a function to all values in a map."
  [f m]
  (walk/postwalk
   (fn [x]
     (if (and (not (map? x)) (not (coll? x)))
       (f x)
       x))
   m))

;; =============================================================================
;; SCHEMA MAPPING
;; =============================================================================

(defn rename-keys
  "Rename keys in a map according to a mapping."
  [m key-mapping]
  (reduce-kv
   (fn [acc old-key new-key]
     (if (contains? m old-key)
       (-> acc
           (assoc new-key (get m old-key))
           (dissoc old-key))
       acc))
   m
   key-mapping))

(defn select-keys-deep
  "Select keys from nested maps."
  [m key-paths]
  (reduce
   (fn [acc path]
     (let [path-vec (if (vector? path) path [path])
           value (get-in m path-vec)]
       (if value
         (assoc-in acc path-vec value)
         acc)))
   {}
   key-paths))

(defn flatten-map
  "Flatten a nested map with dot-separated keys."
  ([m] (flatten-map m ""))
  ([m prefix]
   (reduce-kv
    (fn [acc k v]
      (let [new-key (if (empty? prefix)
                      (name k)
                      (str prefix "." (name k)))]
        (if (map? v)
          (merge acc (flatten-map v new-key))
          (assoc acc (keyword new-key) v))))
    {}
    m)))

(defn unflatten-map
  "Unflatten a map with dot-separated keys."
  [m]
  (reduce-kv
   (fn [acc k v]
     (let [path (map keyword (str/split (name k) #"\."))]
       (assoc-in acc path v)))
   {}
   m))

;; =============================================================================
;; TYPE COERCION
;; =============================================================================

(defn coerce-to-int [v]
  (cond
    (integer? v) v
    (string? v) (try (Integer/parseInt v) (catch Exception _ nil))
    (float? v) (int v)
    :else nil))

(defn coerce-to-float [v]
  (cond
    (float? v) v
    (integer? v) (float v)
    (string? v) (try (Float/parseFloat v) (catch Exception _ nil))
    :else nil))

(defn coerce-to-string [v]
  (when v (str v)))

(defn coerce-to-boolean [v]
  (cond
    (boolean? v) v
    (string? v) (contains? #{"true" "yes" "1" "on"} (str/lower-case v))
    (number? v) (not (zero? v))
    :else (boolean v)))

(defn coerce-to-keyword [v]
  (cond
    (keyword? v) v
    (string? v) (keyword (normalize-string v))
    (symbol? v) (keyword v)
    :else nil))

(def coercers
  {:int coerce-to-int
   :float coerce-to-float
   :string coerce-to-string
   :boolean coerce-to-boolean
   :keyword coerce-to-keyword})

(defn coerce [type-key v]
  (when-let [coercer (get coercers type-key)]
    (coercer v)))

;; =============================================================================
;; PIPELINE COMPOSITION
;; =============================================================================

(defn pipeline
  "Compose transformations into a pipeline."
  [& transforms]
  (fn [data]
    (reduce (fn [acc transform] (transform acc)) data transforms)))

(defn transform-when
  "Apply transformation only when predicate is true."
  [pred transform]
  (fn [data]
    (if (pred data)
      (transform data)
      data)))

(defn transform-each
  "Apply transformation to each element."
  [transform]
  (fn [coll]
    (map transform coll)))

(defn transform-with-index
  "Apply transformation with index."
  [f]
  (fn [coll]
    (map-indexed f coll)))

;; =============================================================================
;; ANALYSIS TRANSFORMATIONS
;; =============================================================================

(defn transform-analysis-result
  "Transform raw analysis result to standard format."
  [result]
  {:document-id (:document-id result)
   :models (map (fn [m]
                  {:id (:model-id m)
                   :confidence (:confidence m)
                   :evidence (:evidence m)})
                (:models result))
   :timestamp (:timestamp result)
   :metadata (select-keys result [:source :version])})

(defn transform-model-detection
  "Transform model detection to standard format."
  [detection]
  {:model-id (coerce-to-keyword (:model-id detection))
   :confidence (coerce-to-float (:confidence detection))
   :evidence (coerce-to-string (:evidence detection))})

(defn transform-lollapalooza-event
  "Transform Lollapalooza event to standard format."
  [event]
  {:id (:id event)
   :models (map coerce-to-keyword (:models event))
   :avg-confidence (coerce-to-float (:avg-confidence event))
   :document-id (:document-id event)
   :timestamp (:timestamp event)})

;; =============================================================================
;; BATCH TRANSFORMATIONS
;; =============================================================================

(defn batch-transform
  "Apply transformation to batches of data."
  [batch-size transform coll]
  (mapcat transform (partition-all batch-size coll)))

(defn parallel-transform
  "Apply transformation in parallel."
  [transform coll]
  (pmap transform coll))

;; =============================================================================
;; DATA CLEANING
;; =============================================================================

(defn remove-nil-values
  "Remove nil values from a map."
  [m]
  (into {} (filter (fn [[_ v]] (some? v)) m)))

(defn remove-empty-values
  "Remove nil and empty values from a map."
  [m]
  (into {} (filter (fn [[_ v]]
                     (and (some? v)
                          (not (and (coll? v) (empty? v)))
                          (not (and (string? v) (str/blank? v)))))
                   m)))

(defn default-values
  "Apply default values for missing keys."
  [m defaults]
  (merge defaults m))

(defn trim-strings
  "Trim all string values in a map."
  [m]
  (walk/postwalk
   (fn [x]
     (if (string? x)
       (str/trim x)
       x))
   m))

;; =============================================================================
;; AGGREGATION TRANSFORMATIONS
;; =============================================================================

(defn aggregate-by
  "Aggregate values by a key function."
  [key-fn agg-fn coll]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (agg-fn v)))
   {}
   (group-by key-fn coll)))

(defn count-by
  "Count occurrences by a key function."
  [key-fn coll]
  (frequencies (map key-fn coll)))

(defn sum-by
  "Sum values by a key function."
  [key-fn value-fn coll]
  (aggregate-by key-fn #(reduce + (map value-fn %)) coll))

(defn avg-by
  "Average values by a key function."
  [key-fn value-fn coll]
  (aggregate-by key-fn
                #(let [vals (map value-fn %)]
                   (/ (reduce + vals) (count vals)))
                coll))
