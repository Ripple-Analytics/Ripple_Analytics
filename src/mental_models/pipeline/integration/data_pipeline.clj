(ns mental-models.pipeline.integration.data-pipeline
  "Data Pipeline Module
   
   ETL operations:
   - Pipeline definition
   - Stage execution
   - Data transformation
   - Error handling
   - Pipeline monitoring"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util UUID]))

;; =============================================================================
;; DATA PIPELINE STATE
;; =============================================================================

(defonce pipeline-state (atom {:pipelines {}
                               :executions {}
                               :config {:default-batch-size 100
                                        :default-parallelism 4
                                        :max-retries 3}}))

;; =============================================================================
;; STAGE TYPES
;; =============================================================================

(def stage-types #{:extract :transform :load :filter :aggregate :join :split :merge})

;; =============================================================================
;; STAGE CREATION
;; =============================================================================

(defn create-stage
  "Create a pipeline stage."
  [stage-id {:keys [type handler input-schema output-schema batch-size parallelism]}]
  {:id stage-id
   :type (or type :transform)
   :handler handler
   :input-schema input-schema
   :output-schema output-schema
   :batch-size (or batch-size (get-in @pipeline-state [:config :default-batch-size]))
   :parallelism (or parallelism (get-in @pipeline-state [:config :default-parallelism]))
   :records-processed (AtomicLong. 0)
   :records-failed (AtomicLong. 0)
   :duration-ms (AtomicLong. 0)})

;; =============================================================================
;; PIPELINE CREATION
;; =============================================================================

(defn create-pipeline
  "Create a new data pipeline."
  [pipeline-id {:keys [name description stages source sink error-handler]}]
  {:id pipeline-id
   :name (or name (str pipeline-id))
   :description description
   :stages (vec (map-indexed (fn [idx stage]
                               (create-stage (or (:id stage) (keyword (str "stage-" idx))) stage))
                             stages))
   :source source
   :sink sink
   :error-handler error-handler
   :enabled? (AtomicBoolean. true)
   :run-count (AtomicLong. 0)
   :success-count (AtomicLong. 0)
   :failure-count (AtomicLong. 0)
   :total-records (AtomicLong. 0)
   :created-at (System/currentTimeMillis)})

;; =============================================================================
;; PIPELINE REGISTRATION
;; =============================================================================

(defn register-pipeline!
  "Register a data pipeline."
  [pipeline-id opts]
  (log/info "Registering pipeline" {:id pipeline-id :name (:name opts)})
  (let [pipeline (create-pipeline pipeline-id opts)]
    (swap! pipeline-state assoc-in [:pipelines pipeline-id] pipeline)
    (metrics/inc-counter! :datapipeline/pipelines-registered)
    pipeline-id))

(defn unregister-pipeline!
  "Unregister a data pipeline."
  [pipeline-id]
  (log/info "Unregistering pipeline" {:id pipeline-id})
  (swap! pipeline-state update :pipelines dissoc pipeline-id))

(defn get-pipeline
  "Get a data pipeline."
  [pipeline-id]
  (get-in @pipeline-state [:pipelines pipeline-id]))

(defn list-pipelines
  "List all data pipelines."
  []
  (keys (:pipelines @pipeline-state)))

;; =============================================================================
;; STAGE EXECUTION
;; =============================================================================

(defn execute-stage
  "Execute a single pipeline stage."
  [stage data context]
  (let [start-time (System/currentTimeMillis)
        batch-size (:batch-size stage)
        batches (partition-all batch-size data)]
    (try
      (let [results (doall
                     (for [batch batches]
                       (try
                         (let [result ((:handler stage) batch context)]
                           (.addAndGet ^AtomicLong (:records-processed stage) (count batch))
                           result)
                         (catch Exception e
                           (.addAndGet ^AtomicLong (:records-failed stage) (count batch))
                           (throw e)))))]
        (.addAndGet ^AtomicLong (:duration-ms stage) (- (System/currentTimeMillis) start-time))
        (flatten results))
      (catch Exception e
        (log/error "Stage execution failed" {:stage (:id stage) :error (.getMessage e)})
        (throw e)))))

(defn execute-stage-parallel
  "Execute a stage with parallelism."
  [stage data context]
  (let [start-time (System/currentTimeMillis)
        batch-size (:batch-size stage)
        parallelism (:parallelism stage)
        batches (partition-all batch-size data)
        batch-groups (partition-all parallelism batches)]
    (try
      (let [results (doall
                     (for [group batch-groups]
                       (let [futures (doall
                                      (for [batch group]
                                        (future
                                          (try
                                            (let [result ((:handler stage) batch context)]
                                              (.addAndGet ^AtomicLong (:records-processed stage) (count batch))
                                              result)
                                            (catch Exception e
                                              (.addAndGet ^AtomicLong (:records-failed stage) (count batch))
                                              (throw e))))))]
                         (mapcat deref futures))))]
        (.addAndGet ^AtomicLong (:duration-ms stage) (- (System/currentTimeMillis) start-time))
        (flatten results))
      (catch Exception e
        (log/error "Parallel stage execution failed" {:stage (:id stage) :error (.getMessage e)})
        (throw e)))))

;; =============================================================================
;; PIPELINE EXECUTION
;; =============================================================================

(defn create-execution
  "Create a pipeline execution record."
  [pipeline-id]
  {:id (str (UUID/randomUUID))
   :pipeline-id pipeline-id
   :status :running
   :started-at (System/currentTimeMillis)
   :completed-at nil
   :records-processed 0
   :records-failed 0
   :stage-results {}
   :error nil})

(defn execute-pipeline!
  "Execute a data pipeline."
  [pipeline-id & {:keys [data context]}]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (when (.get ^AtomicBoolean (:enabled? pipeline))
      (let [execution-id (str (UUID/randomUUID))
            execution (create-execution pipeline-id)]
        (swap! pipeline-state assoc-in [:executions execution-id] execution)
        (.incrementAndGet ^AtomicLong (:run-count pipeline))
        (metrics/inc-counter! :datapipeline/pipelines-executed)
        (events/publish! :datapipeline/pipeline-started {:pipeline-id pipeline-id :execution-id execution-id})
        (try
          ;; Extract data from source
          (let [source-data (if data
                              data
                              (when-let [source (:source pipeline)]
                                (source context)))
                ctx (merge context {:pipeline-id pipeline-id :execution-id execution-id})]
            ;; Execute stages
            (loop [current-data source-data
                   stages (:stages pipeline)
                   stage-results {}]
              (if (empty? stages)
                (do
                  ;; Load data to sink
                  (when-let [sink (:sink pipeline)]
                    (sink current-data ctx))
                  ;; Update execution
                  (swap! pipeline-state update-in [:executions execution-id]
                         merge {:status :completed
                                :completed-at (System/currentTimeMillis)
                                :records-processed (count current-data)
                                :stage-results stage-results})
                  (.incrementAndGet ^AtomicLong (:success-count pipeline))
                  (.addAndGet ^AtomicLong (:total-records pipeline) (count current-data))
                  (events/publish! :datapipeline/pipeline-completed {:pipeline-id pipeline-id
                                                                      :execution-id execution-id
                                                                      :records (count current-data)})
                  {:execution-id execution-id
                   :status :completed
                   :records (count current-data)
                   :data current-data})
                (let [stage (first stages)
                      stage-start (System/currentTimeMillis)
                      result-data (if (> (:parallelism stage) 1)
                                    (execute-stage-parallel stage current-data ctx)
                                    (execute-stage stage current-data ctx))
                      stage-duration (- (System/currentTimeMillis) stage-start)]
                  (recur result-data
                         (rest stages)
                         (assoc stage-results (:id stage)
                                {:records-in (count current-data)
                                 :records-out (count result-data)
                                 :duration-ms stage-duration}))))))
          (catch Exception e
            (.incrementAndGet ^AtomicLong (:failure-count pipeline))
            (swap! pipeline-state update-in [:executions execution-id]
                   merge {:status :failed
                          :completed-at (System/currentTimeMillis)
                          :error (.getMessage e)})
            (when-let [error-handler (:error-handler pipeline)]
              (error-handler e {:pipeline-id pipeline-id :execution-id execution-id}))
            (events/publish! :datapipeline/pipeline-failed {:pipeline-id pipeline-id
                                                            :execution-id execution-id
                                                            :error (.getMessage e)})
            {:execution-id execution-id
             :status :failed
             :error (.getMessage e)}))))))

;; =============================================================================
;; PIPELINE CONTROL
;; =============================================================================

(defn enable-pipeline!
  "Enable a pipeline."
  [pipeline-id]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (log/info "Enabling pipeline" {:id pipeline-id})
    (.set ^AtomicBoolean (:enabled? pipeline) true)))

(defn disable-pipeline!
  "Disable a pipeline."
  [pipeline-id]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (log/info "Disabling pipeline" {:id pipeline-id})
    (.set ^AtomicBoolean (:enabled? pipeline) false)))

;; =============================================================================
;; BUILT-IN TRANSFORMATIONS
;; =============================================================================

(defn map-transform
  "Create a map transformation stage."
  [f]
  {:type :transform
   :handler (fn [batch _] (map f batch))})

(defn filter-transform
  "Create a filter transformation stage."
  [pred]
  {:type :filter
   :handler (fn [batch _] (filter pred batch))})

(defn aggregate-transform
  "Create an aggregate transformation stage."
  [group-fn agg-fn]
  {:type :aggregate
   :handler (fn [batch _]
              (for [[k v] (group-by group-fn batch)]
                {:key k :value (agg-fn v)}))})

(defn flatten-transform
  "Create a flatten transformation stage."
  []
  {:type :transform
   :handler (fn [batch _] (flatten batch))})

(defn distinct-transform
  "Create a distinct transformation stage."
  [& {:keys [key-fn]}]
  {:type :transform
   :handler (fn [batch _]
              (if key-fn
                (vals (into {} (map (fn [x] [(key-fn x) x]) batch)))
                (distinct batch)))})

(defn sort-transform
  "Create a sort transformation stage."
  [& {:keys [key-fn comparator]}]
  {:type :transform
   :handler (fn [batch _]
              (if comparator
                (sort comparator batch)
                (if key-fn
                  (sort-by key-fn batch)
                  (sort batch))))})

;; =============================================================================
;; EXECUTION HISTORY
;; =============================================================================

(defn get-execution
  "Get a pipeline execution."
  [execution-id]
  (get-in @pipeline-state [:executions execution-id]))

(defn list-executions
  "List pipeline executions."
  [& {:keys [pipeline-id status limit]}]
  (let [executions (vals (:executions @pipeline-state))]
    (cond->> executions
      pipeline-id (filter #(= (:pipeline-id %) pipeline-id))
      status (filter #(= (:status %) status))
      limit (take limit))))

(defn clear-executions!
  "Clear execution history."
  [& {:keys [pipeline-id]}]
  (if pipeline-id
    (swap! pipeline-state update :executions
           (fn [execs] (into {} (remove #(= (:pipeline-id (val %)) pipeline-id) execs))))
    (swap! pipeline-state assoc :executions {})))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-pipeline-stats
  "Get statistics for a pipeline."
  [pipeline-id]
  (when-let [pipeline (get-pipeline pipeline-id)]
    {:id pipeline-id
     :name (:name pipeline)
     :enabled? (.get ^AtomicBoolean (:enabled? pipeline))
     :stages (count (:stages pipeline))
     :run-count (.get ^AtomicLong (:run-count pipeline))
     :success-count (.get ^AtomicLong (:success-count pipeline))
     :failure-count (.get ^AtomicLong (:failure-count pipeline))
     :total-records (.get ^AtomicLong (:total-records pipeline))
     :success-rate (let [total (.get ^AtomicLong (:run-count pipeline))]
                     (if (pos? total)
                       (* 100.0 (/ (.get ^AtomicLong (:success-count pipeline)) total))
                       0.0))
     :stage-stats (into {} (for [stage (:stages pipeline)]
                             [(:id stage) {:records-processed (.get ^AtomicLong (:records-processed stage))
                                           :records-failed (.get ^AtomicLong (:records-failed stage))
                                           :duration-ms (.get ^AtomicLong (:duration-ms stage))}]))}))

(defn get-all-pipeline-stats
  "Get statistics for all pipelines."
  []
  (into {} (for [pipeline-id (list-pipelines)]
             [pipeline-id (get-pipeline-stats pipeline-id)])))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-pipeline!
  "Initialize data pipeline."
  []
  (log/info "Initializing data pipeline")
  ;; Register feature flag
  (flags/register-flag! "data-pipeline" "Enable data pipeline" true)
  ;; Create metrics
  (metrics/create-counter! :datapipeline/pipelines-registered "Pipelines registered")
  (metrics/create-counter! :datapipeline/pipelines-executed "Pipelines executed")
  (metrics/create-gauge! :datapipeline/total-pipelines "Total pipelines"
                         #(count (:pipelines @pipeline-state)))
    (metrics/create-gauge! :datapipeline/active-executions "Active executions"
                           (fn [] (count (filter (fn [e] (= (:status e) :running)) (vals (:executions @pipeline-state))))))
  (log/info "Data pipeline initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-pipeline-status []
  {:enabled (flags/is-enabled? "data-pipeline")
   :pipelines (count (:pipelines @pipeline-state))
   :executions (count (:executions @pipeline-state))
   :active-executions (count (filter #(= (:status %) :running) (vals (:executions @pipeline-state))))
   :stats (get-all-pipeline-stats)
   :config (:config @pipeline-state)})
