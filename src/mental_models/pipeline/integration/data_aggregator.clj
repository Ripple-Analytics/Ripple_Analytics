(ns mental-models.pipeline.integration.data-aggregator
  "Data Aggregator Module
   
   Data aggregation and summarization:
   - Multiple aggregation functions
   - Time-based aggregation
   - Group-by operations
   - Rolling aggregations
   - Custom aggregators"
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
;; DATA AGGREGATOR STATE
;; =============================================================================

(defonce aggregator-state (atom {:aggregators (ConcurrentHashMap.)
                                  :pipelines (ConcurrentHashMap.)
                                  :results (ConcurrentHashMap.)
                                  :aggregation-count (AtomicLong. 0)
                                  :config {:max-results 10000
                                           :default-window-ms 60000}}))

;; =============================================================================
;; BUILT-IN AGGREGATION FUNCTIONS
;; =============================================================================

(defn agg-count
  "Count aggregation."
  [values]
  (count values))

(defn agg-sum
  "Sum aggregation."
  [values]
  (reduce + 0 values))

(defn agg-avg
  "Average aggregation."
  [values]
  (if (empty? values)
    0
    (/ (reduce + 0 values) (count values))))

(defn agg-min
  "Minimum aggregation."
  [values]
  (when (seq values)
    (apply min values)))

(defn agg-max
  "Maximum aggregation."
  [values]
  (when (seq values)
    (apply max values)))

(defn agg-first
  "First value aggregation."
  [values]
  (first values))

(defn agg-last
  "Last value aggregation."
  [values]
  (last values))

(defn agg-distinct
  "Distinct values aggregation."
  [values]
  (vec (distinct values)))

(defn agg-distinct-count
  "Distinct count aggregation."
  [values]
  (count (distinct values)))

(defn agg-median
  "Median aggregation."
  [values]
  (when (seq values)
    (let [sorted (sort values)
          n (count sorted)
          mid (quot n 2)]
      (if (odd? n)
        (nth sorted mid)
        (/ (+ (nth sorted mid) (nth sorted (dec mid))) 2)))))

(defn agg-percentile
  "Percentile aggregation."
  [p values]
  (when (seq values)
    (let [sorted (sort values)
          n (count sorted)
          idx (int (* p (dec n)))]
      (nth sorted idx))))

(defn agg-stddev
  "Standard deviation aggregation."
  [values]
  (when (seq values)
    (let [avg (agg-avg values)
          variance (/ (reduce + (map #(Math/pow (- % avg) 2) values))
                      (count values))]
      (Math/sqrt variance))))

(defn agg-variance
  "Variance aggregation."
  [values]
  (when (seq values)
    (let [avg (agg-avg values)]
      (/ (reduce + (map #(Math/pow (- % avg) 2) values))
         (count values)))))

;; =============================================================================
;; AGGREGATOR REGISTRATION
;; =============================================================================

(defn register-aggregator!
  "Register a custom aggregator."
  [aggregator-id {:keys [name description fn]}]
  (log/info "Registering aggregator" {:id aggregator-id})
  (.put ^ConcurrentHashMap (:aggregators @aggregator-state) aggregator-id
        {:id aggregator-id
         :name name
         :description description
         :fn fn
         :registered-at (System/currentTimeMillis)}))

(defn unregister-aggregator!
  "Unregister an aggregator."
  [aggregator-id]
  (.remove ^ConcurrentHashMap (:aggregators @aggregator-state) aggregator-id))

(defn get-aggregator
  "Get an aggregator by ID."
  [aggregator-id]
  (or (.get ^ConcurrentHashMap (:aggregators @aggregator-state) aggregator-id)
      ;; Built-in aggregators
      (case aggregator-id
        :count {:fn agg-count}
        :sum {:fn agg-sum}
        :avg {:fn agg-avg}
        :min {:fn agg-min}
        :max {:fn agg-max}
        :first {:fn agg-first}
        :last {:fn agg-last}
        :distinct {:fn agg-distinct}
        :distinct-count {:fn agg-distinct-count}
        :median {:fn agg-median}
        :stddev {:fn agg-stddev}
        :variance {:fn agg-variance}
        nil)))

(defn list-aggregators
  "List all aggregators."
  []
  (concat [:count :sum :avg :min :max :first :last :distinct :distinct-count :median :stddev :variance]
          (keys (:aggregators @aggregator-state))))

;; =============================================================================
;; AGGREGATION OPERATIONS
;; =============================================================================

(defn aggregate
  "Aggregate values using specified aggregator."
  [aggregator-id values]
  (.incrementAndGet ^AtomicLong (:aggregation-count @aggregator-state))
  (metrics/inc-counter! :dataaggregator/aggregations)
  (if-let [aggregator (get-aggregator aggregator-id)]
    ((:fn aggregator) values)
    (throw (ex-info "Unknown aggregator" {:aggregator-id aggregator-id}))))

(defn aggregate-by
  "Aggregate values grouped by a key function."
  [aggregator-id key-fn values]
  (let [grouped (group-by key-fn values)]
    (into {} (map (fn [[k vs]]
                    [k (aggregate aggregator-id (map #(if (map? %) (val (first %)) %) vs))])
                  grouped))))

(defn aggregate-field
  "Aggregate a specific field from a collection of maps."
  [aggregator-id field data]
  (aggregate aggregator-id (map #(get % field) data)))

(defn multi-aggregate
  "Apply multiple aggregations to values."
  [aggregator-ids values]
  (into {} (map (fn [agg-id]
                  [agg-id (aggregate agg-id values)])
                aggregator-ids)))

(defn multi-aggregate-fields
  "Apply multiple aggregations to multiple fields."
  [field-aggregators data]
  (into {} (map (fn [[field agg-ids]]
                  [field (multi-aggregate agg-ids (map #(get % field) data))])
                field-aggregators)))

;; =============================================================================
;; TIME-BASED AGGREGATION
;; =============================================================================

(defn time-bucket
  "Get the time bucket for a timestamp."
  [timestamp window-ms]
  (* (quot timestamp window-ms) window-ms))

(defn aggregate-by-time
  "Aggregate values by time windows."
  [aggregator-id timestamp-fn window-ms values]
  (let [bucketed (group-by #(time-bucket (timestamp-fn %) window-ms) values)]
    (into (sorted-map)
          (map (fn [[bucket vs]]
                 [bucket (aggregate aggregator-id (map #(if (map? %) (val (first %)) %) vs))])
               bucketed))))

(defn aggregate-field-by-time
  "Aggregate a field by time windows."
  [aggregator-id field timestamp-field window-ms data]
  (let [bucketed (group-by #(time-bucket (get % timestamp-field) window-ms) data)]
    (into (sorted-map)
          (map (fn [[bucket vs]]
                 [bucket (aggregate aggregator-id (map #(get % field) vs))])
               bucketed))))

;; =============================================================================
;; ROLLING AGGREGATION
;; =============================================================================

(defn rolling-aggregate
  "Calculate rolling aggregation over a window."
  [aggregator-id window-size values]
  (let [n (count values)]
    (if (< n window-size)
      [(aggregate aggregator-id values)]
      (map (fn [i]
             (aggregate aggregator-id (subvec (vec values) i (+ i window-size))))
           (range (- n window-size -1))))))

(defn exponential-moving-average
  "Calculate exponential moving average."
  [alpha values]
  (when (seq values)
    (reduce (fn [ema v]
              (+ (* alpha v) (* (- 1 alpha) ema)))
            (first values)
            (rest values))))

;; =============================================================================
;; AGGREGATION PIPELINES
;; =============================================================================

(defn create-pipeline!
  "Create an aggregation pipeline."
  [pipeline-id {:keys [name description stages]}]
  (log/info "Creating aggregation pipeline" {:id pipeline-id})
  (.put ^ConcurrentHashMap (:pipelines @aggregator-state) pipeline-id
        {:id pipeline-id
         :name name
         :description description
         :stages (or stages [])
         :created-at (System/currentTimeMillis)}))

(defn delete-pipeline!
  "Delete a pipeline."
  [pipeline-id]
  (.remove ^ConcurrentHashMap (:pipelines @aggregator-state) pipeline-id))

(defn get-pipeline
  "Get a pipeline by ID."
  [pipeline-id]
  (.get ^ConcurrentHashMap (:pipelines @aggregator-state) pipeline-id))

(defn execute-stage
  "Execute a single pipeline stage."
  [stage data]
  (case (:type stage)
    :filter (filter (:predicate stage) data)
    :map (map (:transform stage) data)
    :group-by (group-by (:key-fn stage) data)
    :aggregate (aggregate (:aggregator stage) data)
    :aggregate-by (aggregate-by (:aggregator stage) (:key-fn stage) data)
    :sort (sort-by (:key-fn stage) (if (:desc stage) > <) data)
    :take (take (:n stage) data)
    :drop (drop (:n stage) data)
    data))

(defn execute-pipeline
  "Execute an aggregation pipeline."
  [pipeline-id data]
  (if-let [pipeline (get-pipeline pipeline-id)]
    (let [start-time (System/currentTimeMillis)
          result (reduce (fn [d stage]
                           (execute-stage stage d))
                         data
                         (:stages pipeline))
          duration-ms (- (System/currentTimeMillis) start-time)]
      (log/info "Pipeline executed" {:pipeline pipeline-id :duration-ms duration-ms})
      {:result result
       :duration-ms duration-ms
       :input-count (count data)})
    {:error "Pipeline not found"}))

;; =============================================================================
;; RESULT STORAGE
;; =============================================================================

(defn store-result!
  "Store an aggregation result."
  [result-id result & {:keys [ttl-ms]}]
  (let [max-results (get-in @aggregator-state [:config :max-results])
        results ^ConcurrentHashMap (:results @aggregator-state)]
    (when (>= (.size results) max-results)
      (let [oldest (first (sort-by :stored-at (vals results)))]
        (.remove results (:id oldest))))
    (.put results result-id
          {:id result-id
           :result result
           :ttl-ms ttl-ms
           :stored-at (System/currentTimeMillis)})))

(defn get-result
  "Get a stored result."
  [result-id]
  (when-let [stored (.get ^ConcurrentHashMap (:results @aggregator-state) result-id)]
    (when (or (nil? (:ttl-ms stored))
              (< (- (System/currentTimeMillis) (:stored-at stored)) (:ttl-ms stored)))
      (:result stored))))

(defn delete-result!
  "Delete a stored result."
  [result-id]
  (.remove ^ConcurrentHashMap (:results @aggregator-state) result-id))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-aggregator-stats
  "Get aggregator statistics."
  []
  {:custom-aggregators (.size ^ConcurrentHashMap (:aggregators @aggregator-state))
   :pipelines (.size ^ConcurrentHashMap (:pipelines @aggregator-state))
   :stored-results (.size ^ConcurrentHashMap (:results @aggregator-state))
   :aggregation-count (.get ^AtomicLong (:aggregation-count @aggregator-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-aggregator!
  "Initialize data aggregator."
  []
  (log/info "Initializing data aggregator")
  ;; Register feature flag
  (flags/register-flag! "data-aggregator" "Enable data aggregator" true)
  ;; Create metrics
  (metrics/create-counter! :dataaggregator/aggregations "Aggregations performed")
  (metrics/create-gauge! :dataaggregator/pipelines "Active pipelines"
                         #(.size ^ConcurrentHashMap (:pipelines @aggregator-state)))
  (log/info "Data aggregator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-data-aggregator-status []
  {:enabled (flags/is-enabled? "data-aggregator")
   :custom-aggregators (.size ^ConcurrentHashMap (:aggregators @aggregator-state))
   :pipelines (.size ^ConcurrentHashMap (:pipelines @aggregator-state))
   :stored-results (.size ^ConcurrentHashMap (:results @aggregator-state))
   :aggregation-count (.get ^AtomicLong (:aggregation-count @aggregator-state))
   :stats (get-aggregator-stats)
   :config (:config @aggregator-state)})
