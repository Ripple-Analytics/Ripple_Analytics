(ns mental-models.pipeline.integration.telemetry
  "Telemetry Module
   
   System observability and monitoring:
   - Distributed tracing
   - Span collection
   - Trace context propagation
   - Sampling strategies
   - Export to backends"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; TELEMETRY STATE
;; =============================================================================

(defonce telemetry-state (atom {:traces {}
                                :active-spans {}
                                :exporters []
                                :sampling-rate 1.0
                                :stats {:traces-created 0
                                        :spans-created 0
                                        :spans-exported 0}}))

;; =============================================================================
;; TRACE CONTEXT
;; =============================================================================

(defn generate-trace-id []
  (str (java.util.UUID/randomUUID)))

(defn generate-span-id []
  (format "%016x" (rand-int Integer/MAX_VALUE)))

(defn create-trace-context
  "Create a new trace context."
  [& {:keys [trace-id parent-span-id sampled]
      :or {sampled true}}]
  {:trace-id (or trace-id (generate-trace-id))
   :parent-span-id parent-span-id
   :sampled sampled})

(defn extract-context
  "Extract trace context from headers/metadata."
  [headers]
  (when-let [trace-parent (get headers "traceparent")]
    (let [parts (clojure.string/split trace-parent #"-")]
      (when (= 4 (count parts))
        {:trace-id (nth parts 1)
         :parent-span-id (nth parts 2)
         :sampled (= "01" (nth parts 3))}))))

(defn inject-context
  "Inject trace context into headers/metadata."
  [headers context]
  (assoc headers "traceparent"
         (format "00-%s-%s-%s"
                 (:trace-id context)
                 (or (:span-id context) "0000000000000000")
                 (if (:sampled context) "01" "00"))))

;; =============================================================================
;; SPAN MANAGEMENT
;; =============================================================================

(defn create-span
  "Create a new span."
  [name & {:keys [context attributes kind]
           :or {attributes {} kind :internal}}]
  (let [trace-id (or (:trace-id context) (generate-trace-id))
        span-id (generate-span-id)
        parent-span-id (:parent-span-id context)]
    {:span-id span-id
     :trace-id trace-id
     :parent-span-id parent-span-id
     :name name
     :kind kind
     :attributes attributes
     :status :unset
     :start-time (System/nanoTime)
     :start-timestamp (System/currentTimeMillis)
     :events []
     :links []}))

(defn start-span!
  "Start a new span and register it."
  [name & opts]
  (when (flags/is-enabled? "telemetry")
    (let [span (apply create-span name opts)]
      (log/debug "Starting span" {:name name :span-id (:span-id span)})
      (swap! telemetry-state assoc-in [:active-spans (:span-id span)] span)
      (swap! telemetry-state update-in [:stats :spans-created] inc)
      (metrics/inc-counter! :telemetry/spans-created)
      span)))

(defn end-span!
  "End a span and record it."
  [span & {:keys [status error]}]
  (when (and span (flags/is-enabled? "telemetry"))
    (let [end-time (System/nanoTime)
          duration-ns (- end-time (:start-time span))
          completed-span (assoc span
                                :end-time end-time
                                :end-timestamp (System/currentTimeMillis)
                                :duration-ns duration-ns
                                :duration-ms (/ duration-ns 1000000.0)
                                :status (or status :ok)
                                :error error)]
      (log/debug "Ending span" {:name (:name span) :duration-ms (:duration-ms completed-span)})
      ;; Remove from active spans
      (swap! telemetry-state update :active-spans dissoc (:span-id span))
      ;; Add to trace
      (swap! telemetry-state update-in [:traces (:trace-id span)]
             (fnil conj [])
             completed-span)
      ;; Record metrics
      (metrics/observe-histogram! :telemetry/span-duration (:duration-ms completed-span))
      ;; Export span
      (export-span! completed-span)
      completed-span)))

(defn add-span-event!
  "Add an event to a span."
  [span event-name & {:keys [attributes] :or {attributes {}}}]
  (when span
    (swap! telemetry-state update-in [:active-spans (:span-id span) :events]
           conj {:name event-name
                 :timestamp (System/currentTimeMillis)
                 :attributes attributes})))

(defn set-span-attribute!
  "Set an attribute on a span."
  [span key value]
  (when span
    (swap! telemetry-state assoc-in [:active-spans (:span-id span) :attributes key] value)))

(defn set-span-status!
  "Set the status of a span."
  [span status & {:keys [message]}]
  (when span
    (swap! telemetry-state update-in [:active-spans (:span-id span)]
           assoc :status status :status-message message)))

;; =============================================================================
;; TRACE MANAGEMENT
;; =============================================================================

(defn get-trace
  "Get all spans for a trace."
  [trace-id]
  (get-in @telemetry-state [:traces trace-id]))

(defn get-active-spans
  "Get all active spans."
  []
  (vals (:active-spans @telemetry-state)))

(defn clear-traces!
  "Clear all completed traces."
  []
  (log/info "Clearing traces")
  (swap! telemetry-state assoc :traces {}))

;; =============================================================================
;; SAMPLING
;; =============================================================================

(defn set-sampling-rate!
  "Set the sampling rate (0.0 to 1.0)."
  [rate]
  (swap! telemetry-state assoc :sampling-rate (max 0.0 (min 1.0 rate))))

(defn should-sample?
  "Determine if a trace should be sampled."
  []
  (< (rand) (:sampling-rate @telemetry-state)))

(defn sampled-span
  "Create a span only if sampling passes."
  [name & opts]
  (when (should-sample?)
    (apply start-span! name opts)))

;; =============================================================================
;; EXPORTERS
;; =============================================================================

(defn register-exporter!
  "Register a span exporter."
  [exporter-fn]
  (swap! telemetry-state update :exporters conj exporter-fn))

(defn export-span!
  "Export a span to all registered exporters."
  [span]
  (doseq [exporter (:exporters @telemetry-state)]
    (try
      (exporter span)
      (swap! telemetry-state update-in [:stats :spans-exported] inc)
      (catch Exception e
        (log/error "Exporter failed" {:error (.getMessage e)})))))

;; Console exporter
(defn console-exporter
  "Export spans to console."
  [span]
  (println (format "[TRACE] %s | %s | %.2fms | %s"
                   (:trace-id span)
                   (:name span)
                   (:duration-ms span)
                   (:status span))))

;; Memory exporter (for testing)
(defn create-memory-exporter
  "Create an in-memory exporter."
  []
  (let [spans (atom [])]
    {:export (fn [span] (swap! spans conj span))
     :get-spans (fn [] @spans)
     :clear (fn [] (reset! spans []))}))

;; =============================================================================
;; CONVENIENCE MACROS
;; =============================================================================

(defmacro with-span
  "Execute body within a span."
  [name opts & body]
  `(let [span# (start-span! ~name ~@opts)]
     (try
       (let [result# (do ~@body)]
         (end-span! span# :status :ok)
         result#)
       (catch Exception e#
         (end-span! span# :status :error :error (.getMessage e#))
         (throw e#)))))

(defmacro with-trace
  "Execute body within a new trace."
  [name & body]
  `(let [context# (create-trace-context)
         span# (start-span! ~name :context context#)]
     (try
       (let [result# (do ~@body)]
         (end-span! span# :status :ok)
         result#)
       (catch Exception e#
         (end-span! span# :status :error :error (.getMessage e#))
         (throw e#)))))

;; =============================================================================
;; TRACE ANALYSIS
;; =============================================================================

(defn analyze-trace
  "Analyze a trace for performance insights."
  [trace-id]
  (let [spans (get-trace trace-id)]
    (when (seq spans)
      (let [durations (map :duration-ms spans)
            total-duration (reduce + durations)]
        {:trace-id trace-id
         :span-count (count spans)
         :total-duration-ms total-duration
         :avg-span-duration-ms (/ total-duration (count spans))
         :max-span-duration-ms (apply max durations)
         :min-span-duration-ms (apply min durations)
         :slowest-span (apply max-key :duration-ms spans)}))))

(defn find-slow-traces
  "Find traces with duration above threshold."
  [threshold-ms]
  (->> (:traces @telemetry-state)
       (map (fn [[trace-id spans]]
              {:trace-id trace-id
               :total-duration-ms (reduce + (map :duration-ms spans))}))
       (filter #(> (:total-duration-ms %) threshold-ms))
       (sort-by :total-duration-ms >)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-telemetry!
  "Initialize telemetry system."
  []
  (log/info "Initializing telemetry")
  ;; Register feature flag
  (flags/register-flag! "telemetry" "Enable telemetry" true)
  ;; Create metrics
  (metrics/create-counter! :telemetry/spans-created "Spans created")
  (metrics/create-counter! :telemetry/spans-exported "Spans exported")
  (metrics/create-histogram! :telemetry/span-duration "Span duration" [1 5 10 50 100 500 1000])
  (metrics/create-gauge! :telemetry/active-spans "Active spans"
                         #(count (:active-spans @telemetry-state)))
  ;; Register default console exporter
  (register-exporter! console-exporter)
  (log/info "Telemetry initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-telemetry-status []
  {:enabled (flags/is-enabled? "telemetry")
   :sampling-rate (:sampling-rate @telemetry-state)
   :active-spans (count (:active-spans @telemetry-state))
   :traces-count (count (:traces @telemetry-state))
   :exporters-count (count (:exporters @telemetry-state))
   :stats (:stats @telemetry-state)})
