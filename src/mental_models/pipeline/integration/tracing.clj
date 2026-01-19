(ns mental-models.pipeline.integration.tracing
  "Tracing Module
   
   Distributed tracing:
   - Trace and span management
   - Context propagation
   - Sampling strategies
   - Trace exporters
   - Performance analysis"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.util UUID]))

;; =============================================================================
;; TRACING STATE
;; =============================================================================

(defonce tracing-state (atom {:traces (ConcurrentHashMap.)
                              :spans (ConcurrentHashMap.)
                              :exporters {}
                              :samplers {}
                              :config {:default-sample-rate 1.0
                                       :max-traces 10000
                                       :max-spans-per-trace 1000
                                       :trace-ttl-ms 3600000}}))

;; Dynamic var for current trace context
(def ^:dynamic *trace-context* nil)

;; =============================================================================
;; ID GENERATION
;; =============================================================================

(defn generate-trace-id
  "Generate a unique trace ID."
  []
  (str (UUID/randomUUID)))

(defn generate-span-id
  "Generate a unique span ID."
  []
  (str (UUID/randomUUID)))

;; =============================================================================
;; TRACE MANAGEMENT
;; =============================================================================

(defn create-trace
  "Create a new trace."
  [trace-id {:keys [name service tags]}]
  {:id trace-id
   :name name
   :service service
   :tags (or tags {})
   :spans []
   :started-at (System/currentTimeMillis)
   :ended-at nil
   :status :active
   :root-span-id nil})

(defn start-trace!
  "Start a new trace."
  [& {:keys [name service tags]}]
  (let [trace-id (generate-trace-id)
        trace (create-trace trace-id {:name name :service service :tags tags})]
    (.put ^ConcurrentHashMap (:traces @tracing-state) trace-id trace)
    (metrics/inc-counter! :tracing/traces-started)
    trace-id))

(defn end-trace!
  "End a trace."
  [trace-id]
  (when-let [trace (.get ^ConcurrentHashMap (:traces @tracing-state) trace-id)]
    (.put ^ConcurrentHashMap (:traces @tracing-state) trace-id
          (assoc trace :ended-at (System/currentTimeMillis) :status :completed))
    (metrics/inc-counter! :tracing/traces-completed)
    (events/publish! :tracing/trace-completed {:trace-id trace-id})))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (.get ^ConcurrentHashMap (:traces @tracing-state) trace-id))

(defn list-traces
  "List all traces."
  [& {:keys [status service limit]}]
  (let [traces (vals (:traces @tracing-state))]
    (cond->> traces
      status (filter #(= (:status %) status))
      service (filter #(= (:service %) service))
      limit (take limit))))

;; =============================================================================
;; SPAN MANAGEMENT
;; =============================================================================

(defn create-span
  "Create a new span."
  [span-id {:keys [trace-id parent-span-id name operation tags]}]
  {:id span-id
   :trace-id trace-id
   :parent-span-id parent-span-id
   :name name
   :operation operation
   :tags (or tags {})
   :logs []
   :started-at (System/currentTimeMillis)
   :ended-at nil
   :duration-ms nil
   :status :active
   :error nil})

(defn start-span!
  "Start a new span."
  [trace-id & {:keys [parent-span-id name operation tags]}]
  (let [span-id (generate-span-id)
        span (create-span span-id {:trace-id trace-id
                                   :parent-span-id parent-span-id
                                   :name name
                                   :operation operation
                                   :tags tags})]
    (.put ^ConcurrentHashMap (:spans @tracing-state) span-id span)
    ;; Update trace with span
    (when-let [trace (get-trace trace-id)]
      (let [updated-trace (update trace :spans conj span-id)]
        (when (nil? (:root-span-id trace))
          (.put ^ConcurrentHashMap (:traces @tracing-state) trace-id
                (assoc updated-trace :root-span-id span-id)))))
    (metrics/inc-counter! :tracing/spans-started)
    span-id))

(defn end-span!
  "End a span."
  [span-id & {:keys [status error]}]
  (when-let [span (.get ^ConcurrentHashMap (:spans @tracing-state) span-id)]
    (let [ended-at (System/currentTimeMillis)
          duration-ms (- ended-at (:started-at span))]
      (.put ^ConcurrentHashMap (:spans @tracing-state) span-id
            (assoc span
                   :ended-at ended-at
                   :duration-ms duration-ms
                   :status (or status :completed)
                   :error error))
      (metrics/inc-counter! :tracing/spans-completed)
      (metrics/observe-histogram! :tracing/span-duration-ms duration-ms))))

(defn get-span
  "Get a span by ID."
  [span-id]
  (.get ^ConcurrentHashMap (:spans @tracing-state) span-id))

(defn get-trace-spans
  "Get all spans for a trace."
  [trace-id]
  (filter #(= (:trace-id %) trace-id) (vals (:spans @tracing-state))))

(defn add-span-log!
  "Add a log entry to a span."
  [span-id message & {:keys [level data]}]
  (when-let [span (get-span span-id)]
    (let [log-entry {:timestamp (System/currentTimeMillis)
                     :message message
                     :level (or level :info)
                     :data data}]
      (.put ^ConcurrentHashMap (:spans @tracing-state) span-id
            (update span :logs conj log-entry)))))

(defn add-span-tag!
  "Add a tag to a span."
  [span-id key value]
  (when-let [span (get-span span-id)]
    (.put ^ConcurrentHashMap (:spans @tracing-state) span-id
          (assoc-in span [:tags key] value))))

;; =============================================================================
;; CONTEXT PROPAGATION
;; =============================================================================

(defn create-context
  "Create a trace context."
  [trace-id span-id]
  {:trace-id trace-id
   :span-id span-id
   :baggage {}})

(defn get-current-context
  "Get the current trace context."
  []
  *trace-context*)

(defn with-context
  "Execute a function with a trace context."
  [context f]
  (binding [*trace-context* context]
    (f)))

(defmacro with-trace-context
  "Execute body with a trace context."
  [context & body]
  `(binding [*trace-context* ~context]
     ~@body))

(defn inject-context
  "Inject trace context into a carrier (e.g., HTTP headers)."
  [context carrier]
  (assoc carrier
         "x-trace-id" (:trace-id context)
         "x-span-id" (:span-id context)
         "x-baggage" (pr-str (:baggage context))))

(defn extract-context
  "Extract trace context from a carrier."
  [carrier]
  (when-let [trace-id (get carrier "x-trace-id")]
    {:trace-id trace-id
     :span-id (get carrier "x-span-id")
     :baggage (when-let [baggage (get carrier "x-baggage")]
                (read-string baggage))}))

;; =============================================================================
;; SAMPLING
;; =============================================================================

(defn always-sample
  "Always sample."
  []
  (fn [_] true))

(defn never-sample
  "Never sample."
  []
  (fn [_] false))

(defn rate-sample
  "Sample at a given rate (0.0 to 1.0)."
  [rate]
  (fn [_] (< (rand) rate)))

(defn parent-based-sample
  "Sample based on parent span."
  []
  (fn [context]
    (if-let [parent-span-id (:span-id context)]
      (when-let [parent-span (get-span parent-span-id)]
        (= (:status parent-span) :active))
      true)))

(defn register-sampler!
  "Register a sampler."
  [sampler-id sampler-fn]
  (swap! tracing-state assoc-in [:samplers sampler-id] sampler-fn))

(defn should-sample?
  "Check if a trace should be sampled."
  [context]
  (let [samplers (vals (:samplers @tracing-state))]
    (if (empty? samplers)
      (< (rand) (get-in @tracing-state [:config :default-sample-rate]))
      (every? #(% context) samplers))))

;; =============================================================================
;; EXPORTERS
;; =============================================================================

(defn register-exporter!
  "Register a trace exporter."
  [exporter-id export-fn]
  (log/info "Registering trace exporter" {:id exporter-id})
  (swap! tracing-state assoc-in [:exporters exporter-id] export-fn))

(defn unregister-exporter!
  "Unregister a trace exporter."
  [exporter-id]
  (swap! tracing-state update :exporters dissoc exporter-id))

(defn export-trace!
  "Export a trace to all exporters."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (get-trace-spans trace-id)
          trace-data {:trace trace :spans spans}]
      (doseq [[exporter-id export-fn] (:exporters @tracing-state)]
        (try
          (export-fn trace-data)
          (catch Exception e
            (log/error "Trace export failed" {:exporter exporter-id :error (.getMessage e)})))))))

;; =============================================================================
;; CONVENIENCE MACROS
;; =============================================================================

(defmacro with-span
  "Execute body within a span."
  [[span-name & {:keys [trace-id parent-span-id operation tags]}] & body]
  `(let [ctx# (get-current-context)
         trace-id# (or ~trace-id (:trace-id ctx#) (start-trace!))
         parent-id# (or ~parent-span-id (:span-id ctx#))
         span-id# (start-span! trace-id# :parent-span-id parent-id#
                               :name ~span-name :operation ~operation :tags ~tags)
         new-ctx# (create-context trace-id# span-id#)]
     (try
       (with-trace-context new-ctx#
         (let [result# (do ~@body)]
           (end-span! span-id# :status :completed)
           result#))
       (catch Exception e#
         (end-span! span-id# :status :error :error (.getMessage e#))
         (throw e#)))))

(defmacro with-trace
  "Execute body within a new trace."
  [[trace-name & {:keys [service tags]}] & body]
  `(let [trace-id# (start-trace! :name ~trace-name :service ~service :tags ~tags)]
     (try
       (let [result# (with-span [~trace-name :trace-id trace-id#]
                       ~@body)]
         (end-trace! trace-id#)
         result#)
       (catch Exception e#
         (end-trace! trace-id#)
         (throw e#)))))

;; =============================================================================
;; ANALYSIS
;; =============================================================================

(defn get-trace-duration
  "Get the total duration of a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (when (:ended-at trace)
      (- (:ended-at trace) (:started-at trace)))))

(defn get-critical-path
  "Get the critical path of a trace (longest path)."
  [trace-id]
  (let [spans (get-trace-spans trace-id)
        root-span (first (filter #(nil? (:parent-span-id %)) spans))]
    (when root-span
      (letfn [(get-children [span-id]
                (filter #(= (:parent-span-id %) span-id) spans))
              (longest-path [span]
                (let [children (get-children (:id span))
                      child-paths (map longest-path children)]
                  (if (empty? child-paths)
                    [span]
                    (cons span (apply max-key #(reduce + (map :duration-ms %)) child-paths)))))]
        (longest-path root-span)))))

(defn get-span-tree
  "Get the span tree for a trace."
  [trace-id]
  (let [spans (get-trace-spans trace-id)
        root-span (first (filter #(nil? (:parent-span-id %)) spans))]
    (when root-span
      (letfn [(build-tree [span]
                (let [children (filter #(= (:parent-span-id %) (:id span)) spans)]
                  (assoc span :children (map build-tree children))))]
        (build-tree root-span)))))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn cleanup-old-traces!
  "Clean up old traces."
  []
  (let [ttl (get-in @tracing-state [:config :trace-ttl-ms])
        cutoff (- (System/currentTimeMillis) ttl)
        traces ^ConcurrentHashMap (:traces @tracing-state)
        spans ^ConcurrentHashMap (:spans @tracing-state)]
    (doseq [[trace-id trace] traces]
      (when (and (:ended-at trace) (< (:ended-at trace) cutoff))
        ;; Remove spans
        (doseq [span-id (:spans trace)]
          (.remove spans span-id))
        ;; Remove trace
        (.remove traces trace-id)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-tracing!
  "Initialize tracing."
  []
  (log/info "Initializing tracing")
  ;; Register feature flag
  (flags/register-flag! "tracing" "Enable tracing" true)
  ;; Create metrics
  (metrics/create-counter! :tracing/traces-started "Traces started")
  (metrics/create-counter! :tracing/traces-completed "Traces completed")
  (metrics/create-counter! :tracing/spans-started "Spans started")
  (metrics/create-counter! :tracing/spans-completed "Spans completed")
  (metrics/create-histogram! :tracing/span-duration-ms "Span duration" [1 5 10 50 100 500 1000 5000])
  (metrics/create-gauge! :tracing/active-traces "Active traces"
                         #(count (filter #(= (:status %) :active) (vals (:traces @tracing-state)))))
  (log/info "Tracing initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-tracing-status []
  {:enabled (flags/is-enabled? "tracing")
   :traces (.size ^ConcurrentHashMap (:traces @tracing-state))
   :spans (.size ^ConcurrentHashMap (:spans @tracing-state))
   :exporters (count (:exporters @tracing-state))
   :samplers (count (:samplers @tracing-state))
   :active-traces (count (filter #(= (:status %) :active) (vals (:traces @tracing-state))))
   :config (:config @tracing-state)})
