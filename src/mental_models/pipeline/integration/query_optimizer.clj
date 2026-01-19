(ns mental-models.pipeline.integration.query-optimizer
  "Query Optimizer Module
   
   Query optimization and execution:
   - Query plan generation
   - Cost-based optimization
   - Index utilization
   - Query caching
   - Execution statistics"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; QUERY OPTIMIZER STATE
;; =============================================================================

(defonce optimizer-state (atom {:indexes (ConcurrentHashMap.)
                                :query-cache (ConcurrentHashMap.)
                                :plan-cache (ConcurrentHashMap.)
                                :statistics (ConcurrentHashMap.)
                                :query-count (AtomicLong. 0)
                                :cache-hits (AtomicLong. 0)
                                :cache-misses (AtomicLong. 0)
                                :config {:cache-size 1000
                                         :plan-cache-size 500
                                         :enable-caching true
                                         :cost-threshold 100}}))

;; =============================================================================
;; INDEX MANAGEMENT
;; =============================================================================

(defn create-index!
  "Create an index on a collection."
  [index-id {:keys [collection field type unique]}]
  (log/info "Creating index" {:id index-id :collection collection :field field})
  (let [index {:id index-id
               :collection collection
               :field field
               :type (or type :btree)
               :unique (or unique false)
               :entries (ConcurrentHashMap.)
               :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:indexes @optimizer-state) index-id index)
    index))

(defn drop-index!
  "Drop an index."
  [index-id]
  (.remove ^ConcurrentHashMap (:indexes @optimizer-state) index-id))

(defn get-index
  "Get an index by ID."
  [index-id]
  (.get ^ConcurrentHashMap (:indexes @optimizer-state) index-id))

(defn list-indexes
  "List all indexes."
  [& {:keys [collection]}]
  (let [indexes (vals (:indexes @optimizer-state))]
    (if collection
      (filter #(= (:collection %) collection) indexes)
      indexes)))

(defn index-document!
  "Add a document to an index."
  [index-id doc-id value]
  (when-let [index (get-index index-id)]
    (let [entries ^ConcurrentHashMap (:entries index)
          existing (.get entries value)]
      (if (:unique index)
        (when (nil? existing)
          (.put entries value doc-id))
        (.put entries value (conj (or existing #{}) doc-id))))))

(defn remove-from-index!
  "Remove a document from an index."
  [index-id doc-id value]
  (when-let [index (get-index index-id)]
    (let [entries ^ConcurrentHashMap (:entries index)]
      (if (:unique index)
        (when (= (.get entries value) doc-id)
          (.remove entries value))
        (when-let [docs (.get entries value)]
          (.put entries value (disj docs doc-id)))))))

(defn lookup-index
  "Look up documents by index value."
  [index-id value]
  (when-let [index (get-index index-id)]
    (let [result (.get ^ConcurrentHashMap (:entries index) value)]
      (if (:unique index)
        (when result #{result})
        result))))

;; =============================================================================
;; QUERY PLAN GENERATION
;; =============================================================================

(defn estimate-cost
  "Estimate the cost of a query operation."
  [operation collection-size]
  (case (:type operation)
    :full-scan (* collection-size 1.0)
    :index-scan (* (Math/log (max 1 collection-size)) 2.0)
    :index-lookup 1.0
    :filter (* collection-size 0.5)
    :sort (* collection-size (Math/log (max 1 collection-size)))
    :limit (min (:limit operation) collection-size)
    :skip (min (:skip operation) collection-size)
    collection-size))

(defn find-applicable-indexes
  "Find indexes applicable to a query."
  [collection conditions]
  (let [indexes (list-indexes :collection collection)
        condition-fields (set (map :field conditions))]
    (filter #(contains? condition-fields (:field %)) indexes)))

(defn generate-plan
  "Generate a query execution plan."
  [query]
  (let [{:keys [collection conditions sort-by limit skip]} query
        collection-size (or (:estimated-size query) 10000)
        applicable-indexes (find-applicable-indexes collection conditions)]
    (if (seq applicable-indexes)
      ;; Use index scan
      {:type :index-plan
       :steps [{:type :index-scan
                :index (:id (first applicable-indexes))
                :condition (first (filter #(= (:field %) (:field (first applicable-indexes))) conditions))}
               (when (> (count conditions) 1)
                 {:type :filter
                  :conditions (rest conditions)})
               (when sort-by
                 {:type :sort
                  :field sort-by})
               (when skip
                 {:type :skip
                  :count skip})
               (when limit
                 {:type :limit
                  :count limit})]
       :estimated-cost (+ (estimate-cost {:type :index-scan} collection-size)
                          (if (> (count conditions) 1)
                            (estimate-cost {:type :filter} collection-size)
                            0)
                          (if sort-by
                            (estimate-cost {:type :sort} collection-size)
                            0))}
      ;; Full scan
      {:type :full-scan-plan
       :steps [{:type :full-scan
                :collection collection}
               (when (seq conditions)
                 {:type :filter
                  :conditions conditions})
               (when sort-by
                 {:type :sort
                  :field sort-by})
               (when skip
                 {:type :skip
                  :count skip})
               (when limit
                 {:type :limit
                  :count limit})]
       :estimated-cost (+ (estimate-cost {:type :full-scan} collection-size)
                          (if (seq conditions)
                            (estimate-cost {:type :filter} collection-size)
                            0)
                          (if sort-by
                            (estimate-cost {:type :sort} collection-size)
                            0))})))

(defn optimize-plan
  "Optimize a query plan."
  [plan]
  ;; Remove nil steps and optimize order
  (let [steps (filterv some? (:steps plan))
        ;; Push filters before sorts when possible
        optimized-steps (vec (sort-by (fn [step]
                                        (case (:type step)
                                          :index-scan 0
                                          :full-scan 0
                                          :filter 1
                                          :skip 2
                                          :limit 3
                                          :sort 4
                                          5))
                                      steps))]
    (assoc plan :steps optimized-steps :optimized true)))

;; =============================================================================
;; QUERY CACHING
;; =============================================================================

(defn cache-key
  "Generate a cache key for a query."
  [query]
  (str (hash query)))

(defn get-cached-result
  "Get a cached query result."
  [query]
  (when (get-in @optimizer-state [:config :enable-caching])
    (let [key (cache-key query)
          cached (.get ^ConcurrentHashMap (:query-cache @optimizer-state) key)]
      (when (and cached
                 (< (- (System/currentTimeMillis) (:cached-at cached))
                    (or (:ttl-ms cached) 60000)))
        (.incrementAndGet ^AtomicLong (:cache-hits @optimizer-state))
        (:result cached)))))

(defn cache-result!
  "Cache a query result."
  [query result & {:keys [ttl-ms]}]
  (when (get-in @optimizer-state [:config :enable-caching])
    (let [key (cache-key query)
          cache ^ConcurrentHashMap (:query-cache @optimizer-state)
          max-size (get-in @optimizer-state [:config :cache-size])]
      ;; Evict if cache is full
      (when (>= (.size cache) max-size)
        (let [oldest (first (sort-by :cached-at (vals cache)))]
          (.remove cache (cache-key (:query oldest)))))
      (.put cache key {:query query
                       :result result
                       :ttl-ms (or ttl-ms 60000)
                       :cached-at (System/currentTimeMillis)}))))

(defn invalidate-cache!
  "Invalidate cached results."
  [& {:keys [collection]}]
  (if collection
    (doseq [[k v] (:query-cache @optimizer-state)]
      (when (= (:collection (:query v)) collection)
        (.remove ^ConcurrentHashMap (:query-cache @optimizer-state) k)))
    (.clear ^ConcurrentHashMap (:query-cache @optimizer-state))))

;; =============================================================================
;; PLAN CACHING
;; =============================================================================

(defn get-cached-plan
  "Get a cached query plan."
  [query]
  (let [key (cache-key (dissoc query :params))
        cached (.get ^ConcurrentHashMap (:plan-cache @optimizer-state) key)]
    (when cached
      (:plan cached))))

(defn cache-plan!
  "Cache a query plan."
  [query plan]
  (let [key (cache-key (dissoc query :params))
        cache ^ConcurrentHashMap (:plan-cache @optimizer-state)
        max-size (get-in @optimizer-state [:config :plan-cache-size])]
    (when (>= (.size cache) max-size)
      (let [oldest (first (sort-by :cached-at (vals cache)))]
        (.remove cache (cache-key (:query oldest)))))
    (.put cache key {:query query
                     :plan plan
                     :cached-at (System/currentTimeMillis)})))

;; =============================================================================
;; QUERY EXECUTION
;; =============================================================================

(defn execute-step
  "Execute a single plan step."
  [step data context]
  (case (:type step)
    :full-scan data
    :index-scan (let [index (get-index (:index step))
                      condition (:condition step)
                      doc-ids (lookup-index (:index step) (:value condition))]
                  (filter #(contains? doc-ids (:id %)) data))
    :filter (filter (fn [doc]
                      (every? (fn [cond]
                                (let [field-val (get doc (:field cond))
                                      op (:op cond)
                                      val (:value cond)]
                                  (case op
                                    := (= field-val val)
                                    :!= (not= field-val val)
                                    :> (> field-val val)
                                    :>= (>= field-val val)
                                    :< (< field-val val)
                                    :<= (<= field-val val)
                                    :in (contains? (set val) field-val)
                                    :contains (str/includes? (str field-val) (str val))
                                    true)))
                              (:conditions step)))
                    data)
    :sort (sort-by (:field step) data)
    :skip (drop (:count step) data)
    :limit (take (:count step) data)
    data))

(defn execute-plan
  "Execute a query plan."
  [plan data]
  (reduce (fn [result step]
            (execute-step step result {}))
          data
          (:steps plan)))

(defn execute-query
  "Execute a query with optimization."
  [query data]
  (.incrementAndGet ^AtomicLong (:query-count @optimizer-state))
  (metrics/inc-counter! :queryopt/queries-executed)
  (let [start-time (System/currentTimeMillis)]
    ;; Check cache first
    (if-let [cached (get-cached-result query)]
      (do
        (log/debug "Query cache hit" {:query query})
        cached)
      (do
        (.incrementAndGet ^AtomicLong (:cache-misses @optimizer-state))
        ;; Get or generate plan
        (let [plan (or (get-cached-plan query)
                       (let [new-plan (optimize-plan (generate-plan query))]
                         (cache-plan! query new-plan)
                         new-plan))
              result (execute-plan plan data)
              duration-ms (- (System/currentTimeMillis) start-time)]
          ;; Record statistics
          (.put ^ConcurrentHashMap (:statistics @optimizer-state)
                (str (System/currentTimeMillis))
                {:query query
                 :plan-type (:type plan)
                 :duration-ms duration-ms
                 :result-count (count result)})
          ;; Cache result
          (cache-result! query result)
          ;; Publish event
          (events/publish! :queryopt/query-executed {:duration-ms duration-ms :result-count (count result)})
          (log/debug "Query executed" {:duration-ms duration-ms :result-count (count result)})
          result)))))

;; =============================================================================
;; QUERY BUILDER
;; =============================================================================

(defn query
  "Create a query builder."
  [collection]
  {:collection collection
   :conditions []
   :sort-by nil
   :limit nil
   :skip nil})

(defn where
  "Add a condition to the query."
  [q field op value]
  (update q :conditions conj {:field field :op op :value value}))

(defn order-by
  "Add sorting to the query."
  [q field]
  (assoc q :sort-by field))

(defn limit-results
  "Limit query results."
  [q n]
  (assoc q :limit n))

(defn skip-results
  "Skip query results."
  [q n]
  (assoc q :skip n))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-optimizer-stats
  "Get optimizer statistics."
  []
  (let [cache-hits (.get ^AtomicLong (:cache-hits @optimizer-state))
        cache-misses (.get ^AtomicLong (:cache-misses @optimizer-state))
        total (+ cache-hits cache-misses)]
    {:indexes (.size ^ConcurrentHashMap (:indexes @optimizer-state))
     :cached-queries (.size ^ConcurrentHashMap (:query-cache @optimizer-state))
     :cached-plans (.size ^ConcurrentHashMap (:plan-cache @optimizer-state))
     :query-count (.get ^AtomicLong (:query-count @optimizer-state))
     :cache-hits cache-hits
     :cache-misses cache-misses
     :cache-hit-rate (if (> total 0) (/ cache-hits (double total)) 0.0)}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-query-optimizer!
  "Initialize query optimizer."
  []
  (log/info "Initializing query optimizer")
  ;; Register feature flag
  (flags/register-flag! "query-optimizer" "Enable query optimizer" true)
  ;; Create metrics
  (metrics/create-counter! :queryopt/queries-executed "Queries executed")
  (metrics/create-gauge! :queryopt/cache-hit-rate "Cache hit rate"
                         #(:cache-hit-rate (get-optimizer-stats)))
  (log/info "Query optimizer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-query-optimizer-status []
  {:enabled (flags/is-enabled? "query-optimizer")
   :indexes (.size ^ConcurrentHashMap (:indexes @optimizer-state))
   :cached-queries (.size ^ConcurrentHashMap (:query-cache @optimizer-state))
   :cached-plans (.size ^ConcurrentHashMap (:plan-cache @optimizer-state))
   :query-count (.get ^AtomicLong (:query-count @optimizer-state))
   :stats (get-optimizer-stats)
   :config (:config @optimizer-state)})
