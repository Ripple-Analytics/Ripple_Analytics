(ns mental-models.query.builder
  "Query Builder Module for Mental Models Pipeline
   
   Provides query building with:
   - Fluent query API
   - Filter conditions
   - Sorting and pagination
   - Aggregations
   - Query optimization"
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; QUERY STRUCTURE
;; =============================================================================

(defrecord Query [source filters sorts limit offset aggregations projections])

(defn query
  "Create a new query."
  [source]
  (->Query source [] [] nil nil [] nil))

;; =============================================================================
;; FILTER OPERATIONS
;; =============================================================================

(defn where
  "Add a filter condition."
  [q field op value]
  (update q :filters conj {:field field :op op :value value}))

(defn where-eq
  "Add an equality filter."
  [q field value]
  (where q field := value))

(defn where-ne
  "Add a not-equal filter."
  [q field value]
  (where q field :!= value))

(defn where-gt
  "Add a greater-than filter."
  [q field value]
  (where q field :> value))

(defn where-gte
  "Add a greater-than-or-equal filter."
  [q field value]
  (where q field :>= value))

(defn where-lt
  "Add a less-than filter."
  [q field value]
  (where q field :< value))

(defn where-lte
  "Add a less-than-or-equal filter."
  [q field value]
  (where q field :<= value))

(defn where-in
  "Add an in-list filter."
  [q field values]
  (where q field :in values))

(defn where-not-in
  "Add a not-in-list filter."
  [q field values]
  (where q field :not-in values))

(defn where-like
  "Add a pattern matching filter."
  [q field pattern]
  (where q field :like pattern))

(defn where-between
  "Add a between filter."
  [q field min-val max-val]
  (where q field :between [min-val max-val]))

(defn where-nil
  "Add a nil check filter."
  [q field]
  (where q field :nil nil))

(defn where-not-nil
  "Add a not-nil check filter."
  [q field]
  (where q field :not-nil nil))

;; =============================================================================
;; SORTING
;; =============================================================================

(defn order-by
  "Add a sort condition."
  [q field direction]
  (update q :sorts conj {:field field :direction direction}))

(defn order-by-asc
  "Add ascending sort."
  [q field]
  (order-by q field :asc))

(defn order-by-desc
  "Add descending sort."
  [q field]
  (order-by q field :desc))

;; =============================================================================
;; PAGINATION
;; =============================================================================

(defn limit-to
  "Set the result limit."
  [q n]
  (assoc q :limit n))

(defn offset-by
  "Set the result offset."
  [q n]
  (assoc q :offset n))

(defn page
  "Set pagination by page number and size."
  [q page-num page-size]
  (-> q
      (limit-to page-size)
      (offset-by (* (dec page-num) page-size))))

;; =============================================================================
;; PROJECTIONS
;; =============================================================================

(defn select
  "Select specific fields."
  [q & fields]
  (assoc q :projections (vec fields)))

(defn select-all
  "Select all fields."
  [q]
  (assoc q :projections nil))

;; =============================================================================
;; AGGREGATIONS
;; =============================================================================

(defn aggregate
  "Add an aggregation."
  [q agg-type field alias]
  (update q :aggregations conj {:type agg-type :field field :alias alias}))

(defn count-all
  "Add a count aggregation."
  [q alias]
  (aggregate q :count :* alias))

(defn sum-field
  "Add a sum aggregation."
  [q field alias]
  (aggregate q :sum field alias))

(defn avg-field
  "Add an average aggregation."
  [q field alias]
  (aggregate q :avg field alias))

(defn min-field
  "Add a min aggregation."
  [q field alias]
  (aggregate q :min field alias))

(defn max-field
  "Add a max aggregation."
  [q field alias]
  (aggregate q :max field alias))

(defn group-by-field
  "Add a group-by clause."
  [q field]
  (update q :aggregations conj {:type :group-by :field field}))

;; =============================================================================
;; QUERY EXECUTION HELPERS
;; =============================================================================

(defn apply-filter [data {:keys [field op value]}]
  (let [get-val #(get % field)]
    (case op
      := (filter #(= (get-val %) value) data)
      :!= (filter #(not= (get-val %) value) data)
      :> (filter #(> (get-val %) value) data)
      :>= (filter #(>= (get-val %) value) data)
      :< (filter #(< (get-val %) value) data)
      :<= (filter #(<= (get-val %) value) data)
      :in (filter #(contains? (set value) (get-val %)) data)
      :not-in (filter #(not (contains? (set value) (get-val %))) data)
      :like (filter #(re-find (re-pattern value) (str (get-val %))) data)
      :between (filter #(let [v (get-val %)] (and (>= v (first value)) (<= v (second value)))) data)
      :nil (filter #(nil? (get-val %)) data)
      :not-nil (filter #(some? (get-val %)) data)
      data)))

(defn apply-filters [data filters]
  (reduce apply-filter data filters))

(defn apply-sort [data {:keys [field direction]}]
  (let [sorted (sort-by field data)]
    (if (= direction :desc)
      (reverse sorted)
      sorted)))

(defn apply-sorts [data sorts]
  (reduce apply-sort data sorts))

(defn apply-pagination [data limit offset]
  (cond->> data
    offset (drop offset)
    limit (take limit)))

(defn apply-projection [data projections]
  (if projections
    (map #(select-keys % projections) data)
    data))

;; =============================================================================
;; QUERY EXECUTION
;; =============================================================================

(defn execute
  "Execute a query against in-memory data."
  [{:keys [source filters sorts limit offset projections]} data]
  (-> data
      (apply-filters filters)
      (apply-sorts sorts)
      (apply-pagination limit offset)
      (apply-projection projections)))

;; =============================================================================
;; ANALYSIS QUERIES
;; =============================================================================

(defn analysis-results-query []
  (query :analysis-results))

(defn models-query []
  (query :models))

(defn lollapalooza-query []
  (query :lollapalooza-events))

(defn documents-query []
  (query :documents))

;; =============================================================================
;; COMMON QUERIES
;; =============================================================================

(defn recent-analyses [n]
  (-> (analysis-results-query)
      (order-by-desc :timestamp)
      (limit-to n)))

(defn high-confidence-detections [threshold]
  (-> (analysis-results-query)
      (where-gte :confidence threshold)
      (order-by-desc :confidence)))

(defn model-detections [model-id]
  (-> (analysis-results-query)
      (where-eq :model-id model-id)
      (order-by-desc :timestamp)))

(defn lollapalooza-events-in-range [start-time end-time]
  (-> (lollapalooza-query)
      (where-gte :timestamp start-time)
      (where-lte :timestamp end-time)
      (order-by-desc :timestamp)))

;; =============================================================================
;; QUERY SERIALIZATION
;; =============================================================================

(defn query-to-map
  "Convert a query to a map representation."
  [q]
  {:source (:source q)
   :filters (:filters q)
   :sorts (:sorts q)
   :limit (:limit q)
   :offset (:offset q)
   :aggregations (:aggregations q)
   :projections (:projections q)})

(defn map-to-query
  "Convert a map to a query."
  [m]
  (map->Query m))

;; =============================================================================
;; QUERY OPTIMIZATION
;; =============================================================================

(defn optimize-filters
  "Optimize filter order for better performance."
  [filters]
  ;; Put equality filters first, then range filters
  (sort-by #(case (:op %)
              := 0
              :in 1
              :between 2
              3)
           filters))

(defn optimize-query
  "Optimize a query for better performance."
  [q]
  (update q :filters optimize-filters))
