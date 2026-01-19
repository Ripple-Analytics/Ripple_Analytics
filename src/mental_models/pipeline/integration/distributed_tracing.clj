(ns mental-models.pipeline.integration.distributed-tracing
  "Distributed tracing for mental model analysis pipeline.
   
   Features:
   - Trace context propagation
   - Span management
   - Trace sampling
   - Baggage items
   - Trace export
   - Performance analysis
   - Error tracking
   - Service dependency mapping"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:traces {}           ;; trace-id -> trace
         :spans {}            ;; span-id -> span
         :active-spans {}     ;; thread-id -> span-id
         :samplers {}         ;; sampler-id -> sampler-config
         :exporters {}        ;; exporter-id -> exporter-config
         :config {:sample-rate 1.0
                  :max-spans-per-trace 1000
                  :span-timeout-ms 300000}
         :stats {:traces-created 0 :spans-created 0 :spans-finished 0}
         :initialized? false}))

;; ============================================================================
;; Trace ID Generation
;; ============================================================================

(defn generate-trace-id
  "Generate a new trace ID."
  []
  (str (UUID/randomUUID)))

(defn generate-span-id
  "Generate a new span ID."
  []
  (str (UUID/randomUUID)))

;; ============================================================================
;; Sampling
;; ============================================================================

(defn register-sampler!
  "Register a trace sampler."
  [sampler-id config]
  (let [sampler {:id sampler-id
                 :type (get config :type :probability)
                 :rate (get config :rate 1.0)
                 :rules (get config :rules [])
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:samplers sampler-id] sampler)
    sampler-id))

(defn should-sample?
  "Determine if a trace should be sampled."
  [trace-context]
  (let [sample-rate (get-in @state [:config :sample-rate] 1.0)]
    (< (rand) sample-rate)))

;; ============================================================================
;; Trace Management
;; ============================================================================

(defn start-trace!
  "Start a new trace."
  [operation-name & {:keys [tags baggage]}]
  (when (flags/enabled? :distributed-tracing)
    (let [trace-id (generate-trace-id)
          sampled? (should-sample? {})
          trace {:id trace-id
                 :operation operation-name
                 :sampled? sampled?
                 :baggage (or baggage {})
                 :spans []
                 :started-at (System/currentTimeMillis)
                 :finished-at nil
                 :status :active}]
      (when sampled?
        (swap! state assoc-in [:traces trace-id] trace)
        (swap! state update-in [:stats :traces-created] inc)
        (logging/log :debug "Started trace" {:trace-id trace-id :operation operation-name}))
      trace-id)))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (get-in @state [:traces trace-id]))

(defn finish-trace!
  "Finish a trace."
  [trace-id & {:keys [status]}]
  (when-let [trace (get-trace trace-id)]
    (swap! state update-in [:traces trace-id]
           (fn [t]
             (assoc t
                    :finished-at (System/currentTimeMillis)
                    :status (or status :completed)
                    :duration-ms (- (System/currentTimeMillis) (:started-at t)))))
    (logging/log :debug "Finished trace" {:trace-id trace-id})
    (events/emit! :trace-finished {:trace-id trace-id})))

(defn list-traces
  "List traces."
  [& {:keys [operation status since limit]}]
  (let [traces (vals (:traces @state))
        filtered (cond->> traces
                   operation (filter #(= (:operation %) operation))
                   status (filter #(= (:status %) status))
                   since (filter #(>= (:started-at %) since))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :operation :status :started-at :duration-ms]) filtered)))

;; ============================================================================
;; Span Management
;; ============================================================================

(defn start-span!
  "Start a new span within a trace."
  [trace-id operation-name & {:keys [parent-span-id tags]}]
  (when (flags/enabled? :distributed-tracing)
    (when-let [trace (get-trace trace-id)]
      (when (:sampled? trace)
        (let [span-id (generate-span-id)
              span {:id span-id
                    :trace-id trace-id
                    :parent-id parent-span-id
                    :operation operation-name
                    :tags (or tags {})
                    :logs []
                    :started-at (System/currentTimeMillis)
                    :finished-at nil
                    :status :active
                    :error nil}]
          (swap! state assoc-in [:spans span-id] span)
          (swap! state update-in [:traces trace-id :spans] conj span-id)
          (swap! state assoc-in [:active-spans (Thread/currentThread)] span-id)
          (swap! state update-in [:stats :spans-created] inc)
          (logging/log :debug "Started span" {:span-id span-id :operation operation-name})
          span-id)))))

(defn get-span
  "Get a span by ID."
  [span-id]
  (get-in @state [:spans span-id]))

(defn get-active-span
  "Get the active span for the current thread."
  []
  (get-in @state [:active-spans (Thread/currentThread)]))

(defn finish-span!
  "Finish a span."
  [span-id & {:keys [status error]}]
  (when-let [span (get-span span-id)]
    (swap! state update-in [:spans span-id]
           (fn [s]
             (assoc s
                    :finished-at (System/currentTimeMillis)
                    :status (or status (if error :error :completed))
                    :error error
                    :duration-ms (- (System/currentTimeMillis) (:started-at s)))))
    (swap! state update :active-spans dissoc (Thread/currentThread))
    (swap! state update-in [:stats :spans-finished] inc)
    (logging/log :debug "Finished span" {:span-id span-id})
    (metrics/histogram :span-duration {:operation (:operation span)}
                       (- (System/currentTimeMillis) (:started-at span)))))

(defn add-span-tag!
  "Add a tag to a span."
  [span-id key value]
  (swap! state assoc-in [:spans span-id :tags key] value))

(defn add-span-log!
  "Add a log entry to a span."
  [span-id message & {:keys [level fields]}]
  (let [log-entry {:timestamp (System/currentTimeMillis)
                   :message message
                   :level (or level :info)
                   :fields (or fields {})}]
    (swap! state update-in [:spans span-id :logs] conj log-entry)))

(defn set-span-error!
  "Set error on a span."
  [span-id error]
  (swap! state update-in [:spans span-id]
         (fn [s]
           (assoc s
                  :error {:message (if (instance? Exception error)
                                     (.getMessage error)
                                     (str error))
                          :type (if (instance? Exception error)
                                  (.getName (class error))
                                  "Error")}
                  :status :error))))

;; ============================================================================
;; Baggage
;; ============================================================================

(defn set-baggage!
  "Set a baggage item on a trace."
  [trace-id key value]
  (swap! state assoc-in [:traces trace-id :baggage key] value))

(defn get-baggage
  "Get a baggage item from a trace."
  [trace-id key]
  (get-in @state [:traces trace-id :baggage key]))

(defn get-all-baggage
  "Get all baggage items from a trace."
  [trace-id]
  (get-in @state [:traces trace-id :baggage] {}))

;; ============================================================================
;; Context Propagation
;; ============================================================================

(defn extract-context
  "Extract trace context from headers."
  [headers]
  (let [trace-id (get headers "x-trace-id")
        span-id (get headers "x-span-id")
        sampled (get headers "x-sampled")]
    (when trace-id
      {:trace-id trace-id
       :parent-span-id span-id
       :sampled? (= sampled "1")})))

(defn inject-context
  "Inject trace context into headers."
  [trace-id span-id]
  (let [trace (get-trace trace-id)]
    {"x-trace-id" trace-id
     "x-span-id" span-id
     "x-sampled" (if (:sampled? trace) "1" "0")
     "x-baggage" (pr-str (get-all-baggage trace-id))}))

(defn continue-trace!
  "Continue a trace from extracted context."
  [context operation-name]
  (when (flags/enabled? :distributed-tracing)
    (let [{:keys [trace-id parent-span-id sampled?]} context]
      (if (get-trace trace-id)
        ;; Trace exists, just start a new span
        (start-span! trace-id operation-name :parent-span-id parent-span-id)
        ;; Create new trace entry for this service
        (let [trace {:id trace-id
                     :operation operation-name
                     :sampled? sampled?
                     :baggage {}
                     :spans []
                     :started-at (System/currentTimeMillis)
                     :finished-at nil
                     :status :active
                     :continued? true}]
          (swap! state assoc-in [:traces trace-id] trace)
          (start-span! trace-id operation-name :parent-span-id parent-span-id))))))

;; ============================================================================
;; Trace Analysis
;; ============================================================================

(defn get-trace-tree
  "Get the span tree for a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (map get-span (:spans trace))
          root-spans (filter #(nil? (:parent-id %)) spans)
          build-tree (fn build-tree [span]
                       (let [children (filter #(= (:parent-id %) (:id span)) spans)]
                         (assoc span :children (mapv build-tree children))))]
      {:trace-id trace-id
       :operation (:operation trace)
       :duration-ms (:duration-ms trace)
       :tree (mapv build-tree root-spans)})))

(defn analyze-trace
  "Analyze a trace for performance insights."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (map get-span (:spans trace))
          total-duration (:duration-ms trace)
          span-durations (map :duration-ms (filter :duration-ms spans))
          errors (filter #(= :error (:status %)) spans)]
      {:trace-id trace-id
       :total-duration-ms total-duration
       :span-count (count spans)
       :error-count (count errors)
       :avg-span-duration (when (seq span-durations)
                            (/ (reduce + span-durations) (count span-durations)))
       :max-span-duration (when (seq span-durations)
                            (apply max span-durations))
       :critical-path (get-critical-path trace-id)
       :bottlenecks (find-bottlenecks trace-id)})))

(defn get-critical-path
  "Get the critical path (longest path) in a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (map get-span (:spans trace))
          root-spans (filter #(nil? (:parent-id %)) spans)]
      ;; Simplified: return the longest root span
      (when (seq root-spans)
        (let [longest (first (sort-by #(- (:duration-ms % 0)) root-spans))]
          {:span-id (:id longest)
           :operation (:operation longest)
           :duration-ms (:duration-ms longest)})))))

(defn find-bottlenecks
  "Find bottleneck spans in a trace."
  [trace-id & {:keys [threshold-pct] :or {threshold-pct 0.3}}]
  (when-let [trace (get-trace trace-id)]
    (let [total-duration (:duration-ms trace 1)
          spans (map get-span (:spans trace))
          threshold (* total-duration threshold-pct)]
      (->> spans
           (filter #(> (:duration-ms % 0) threshold))
           (sort-by :duration-ms >)
           (mapv #(select-keys % [:id :operation :duration-ms]))))))

;; ============================================================================
;; Exporters
;; ============================================================================

(defn register-exporter!
  "Register a trace exporter."
  [exporter-id config]
  (let [exporter {:id exporter-id
                  :type (get config :type :console)
                  :endpoint (get config :endpoint nil)
                  :batch-size (get config :batch-size 100)
                  :flush-interval-ms (get config :flush-interval-ms 5000)
                  :enabled? (get config :enabled? true)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:exporters exporter-id] exporter)
    exporter-id))

(defn export-trace
  "Export a trace."
  [trace-id exporter-id]
  (when-let [trace (get-trace trace-id)]
    (when-let [exporter (get-in @state [:exporters exporter-id])]
      (let [spans (map get-span (:spans trace))
            export-data {:trace trace
                         :spans spans
                         :exported-at (System/currentTimeMillis)}]
        (case (:type exporter)
          :console (logging/log :info "Exported trace" {:trace-id trace-id :spans (count spans)})
          :http nil ;; Would send to endpoint
          nil)
        export-data))))

;; ============================================================================
;; Service Dependency Mapping
;; ============================================================================

(defn get-service-dependencies
  "Get service dependencies from traces."
  []
  (let [traces (vals (:traces @state))
        edges (for [trace traces
                    span-id (:spans trace)
                    :let [span (get-span span-id)]
                    :when (:parent-id span)
                    :let [parent (get-span (:parent-id span))]]
                {:from (get-in parent [:tags :service] "unknown")
                 :to (get-in span [:tags :service] "unknown")
                 :operation (:operation span)})]
    {:edges (distinct edges)
     :services (distinct (concat (map :from edges) (map :to edges)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-tracing-stats
  "Get distributed tracing statistics."
  []
  (let [stats (:stats @state)
        traces (vals (:traces @state))
        spans (vals (:spans @state))]
    {:traces-created (:traces-created stats)
     :spans-created (:spans-created stats)
     :spans-finished (:spans-finished stats)
     :active-traces (count (filter #(= :active (:status %)) traces))
     :active-spans (count (filter #(= :active (:status %)) spans))
     :error-spans (count (filter #(= :error (:status %)) spans))
     :avg-trace-duration (let [durations (keep :duration-ms traces)]
                           (when (seq durations)
                             (/ (reduce + durations) (count durations))))
     :exporters (count (:exporters @state))
     :samplers (count (:samplers @state))}))

;; ============================================================================
;; Macros for Convenience
;; ============================================================================

(defmacro with-span
  "Execute body within a span."
  [trace-id operation-name & body]
  `(let [span-id# (start-span! ~trace-id ~operation-name)]
     (try
       (let [result# (do ~@body)]
         (finish-span! span-id#)
         result#)
       (catch Exception e#
         (set-span-error! span-id# e#)
         (finish-span! span-id# :status :error)
         (throw e#)))))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-distributed-tracing!
  "Initialize distributed tracing."
  []
  (when-not (:initialized? @state)
    ;; Register default sampler
    (register-sampler! :default
                       {:type :probability
                        :rate 1.0})
    
    ;; Register default exporter
    (register-exporter! :console
                        {:type :console
                         :enabled? true})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Distributed tracing initialized")
    (events/emit! :distributed-tracing-initialized {})
    true))
