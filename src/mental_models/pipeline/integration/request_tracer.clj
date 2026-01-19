(ns mental-models.pipeline.integration.request-tracer
  "Request tracer for mental model analysis system.
   
   Features:
   - Distributed tracing
   - Span management
   - Trace propagation
   - Context injection
   - Trace sampling
   - Trace export
   - Trace visualization
   - Tracer metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:traces {}           ;; trace-id -> trace data
         :spans {}            ;; span-id -> span data
         :config {:enabled? true
                  :sample-rate 1.0
                  :max-traces 10000
                  :max-spans-per-trace 1000
                  :propagation-format :w3c
                  :export-interval-ms 60000}
         :stats {:traces-created (AtomicLong. 0)
                 :spans-created (AtomicLong. 0)
                 :traces-sampled (AtomicLong. 0)
                 :traces-exported (AtomicLong. 0)}
         :exporters []
         :initialized? false}))

;; ============================================================================
;; ID Generation
;; ============================================================================

(defn generate-trace-id
  "Generate a new trace ID."
  []
  (str (UUID/randomUUID)))

(defn generate-span-id
  "Generate a new span ID."
  []
  (subs (str (UUID/randomUUID)) 0 16))

;; ============================================================================
;; Sampling
;; ============================================================================

(defn should-sample?
  "Determine if a trace should be sampled."
  []
  (< (rand) (get-in @state [:config :sample-rate])))

;; ============================================================================
;; Trace Management
;; ============================================================================

(defn create-trace!
  "Create a new trace."
  [& {:keys [trace-id name metadata]}]
  (when (get-in @state [:config :enabled?])
    (let [sampled? (should-sample?)
          trace {:id (or trace-id (generate-trace-id))
                 :name name
                 :sampled? sampled?
                 :spans (atom [])
                 :metadata (or metadata {})
                 :start-time (System/currentTimeMillis)
                 :end-time (atom nil)
                 :status (atom :in-progress)}]
      
      (when sampled?
        (.incrementAndGet (:traces-sampled (:stats @state))))
      
      (swap! state assoc-in [:traces (:id trace)] trace)
      (.incrementAndGet (:traces-created (:stats @state)))
      
      (:id trace))))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (get-in @state [:traces trace-id]))

(defn list-traces
  "List all traces."
  [& {:keys [limit status] :or {limit 100}}]
  (let [traces (vals (:traces @state))]
    (->> traces
         (filter (fn [t] (or (nil? status) (= @(:status t) status))))
         (sort-by :start-time >)
         (take limit)
         (mapv (fn [t]
                 {:id (:id t)
                  :name (:name t)
                  :sampled? (:sampled? t)
                  :status @(:status t)
                  :start-time (:start-time t)
                  :end-time @(:end-time t)
                  :span-count (count @(:spans t))})))))

(defn end-trace!
  "End a trace."
  [trace-id & {:keys [status] :or {status :completed}}]
  (when-let [trace (get-trace trace-id)]
    (reset! (:end-time trace) (System/currentTimeMillis))
    (reset! (:status trace) status)))

(defn delete-trace!
  "Delete a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (doseq [span-id @(:spans trace)]
      (swap! state update :spans dissoc span-id))
    (swap! state update :traces dissoc trace-id)))

;; ============================================================================
;; Span Management
;; ============================================================================

(defn create-span!
  "Create a new span within a trace."
  [trace-id & {:keys [span-id parent-span-id name operation tags]}]
  (when-let [trace (get-trace trace-id)]
    (let [span {:id (or span-id (generate-span-id))
                :trace-id trace-id
                :parent-span-id parent-span-id
                :name name
                :operation operation
                :tags (or tags {})
                :logs (atom [])
                :start-time (System/currentTimeMillis)
                :end-time (atom nil)
                :status (atom :in-progress)
                :error (atom nil)}]
      
      (swap! state assoc-in [:spans (:id span)] span)
      (swap! (:spans trace) conj (:id span))
      (.incrementAndGet (:spans-created (:stats @state)))
      
      (:id span))))

(defn get-span
  "Get a span by ID."
  [span-id]
  (get-in @state [:spans span-id]))

(defn end-span!
  "End a span."
  [span-id & {:keys [status error] :or {status :completed}}]
  (when-let [span (get-span span-id)]
    (reset! (:end-time span) (System/currentTimeMillis))
    (reset! (:status span) status)
    (when error
      (reset! (:error span) error))))

(defn add-span-tag!
  "Add a tag to a span."
  [span-id key value]
  (when-let [span (get-span span-id)]
    (swap! state update-in [:spans span-id :tags] assoc key value)))

(defn add-span-log!
  "Add a log entry to a span."
  [span-id message & {:keys [level data] :or {level :info}}]
  (when-let [span (get-span span-id)]
    (swap! (:logs span) conj {:timestamp (System/currentTimeMillis)
                              :level level
                              :message message
                              :data data})))

;; ============================================================================
;; Context Propagation
;; ============================================================================

(def ^:dynamic *current-trace-id* nil)
(def ^:dynamic *current-span-id* nil)

(defmacro with-trace
  "Execute code within a trace context."
  [trace-id & body]
  `(binding [*current-trace-id* ~trace-id]
     ~@body))

(defmacro with-span
  "Execute code within a span context."
  [span-id & body]
  `(binding [*current-span-id* ~span-id]
     ~@body))

(defn current-trace-id
  "Get the current trace ID."
  []
  *current-trace-id*)

(defn current-span-id
  "Get the current span ID."
  []
  *current-span-id*)

;; ============================================================================
;; Header Propagation
;; ============================================================================

(defn extract-trace-context
  "Extract trace context from headers."
  [headers]
  (let [format (get-in @state [:config :propagation-format])]
    (case format
      :w3c
      (when-let [traceparent (get headers "traceparent")]
        (let [parts (str/split traceparent #"-")]
          (when (= (count parts) 4)
            {:trace-id (nth parts 1)
             :span-id (nth parts 2)
             :sampled? (= (nth parts 3) "01")})))
      
      :b3
      {:trace-id (get headers "x-b3-traceid")
       :span-id (get headers "x-b3-spanid")
       :parent-span-id (get headers "x-b3-parentspanid")
       :sampled? (= (get headers "x-b3-sampled") "1")}
      
      :jaeger
      (when-let [uber-trace (get headers "uber-trace-id")]
        (let [parts (str/split uber-trace #":")]
          (when (= (count parts) 4)
            {:trace-id (nth parts 0)
             :span-id (nth parts 1)
             :parent-span-id (nth parts 2)
             :sampled? (= (nth parts 3) "1")})))
      
      nil)))

(defn inject-trace-context
  "Inject trace context into headers."
  [headers trace-id span-id sampled?]
  (let [format (get-in @state [:config :propagation-format])]
    (case format
      :w3c
      (assoc headers "traceparent"
             (str "00-" trace-id "-" span-id "-" (if sampled? "01" "00")))
      
      :b3
      (-> headers
          (assoc "x-b3-traceid" trace-id)
          (assoc "x-b3-spanid" span-id)
          (assoc "x-b3-sampled" (if sampled? "1" "0")))
      
      :jaeger
      (assoc headers "uber-trace-id"
             (str trace-id ":" span-id ":0:" (if sampled? "1" "0")))
      
      headers)))

;; ============================================================================
;; Trace Export
;; ============================================================================

(defn register-exporter!
  "Register a trace exporter."
  [exporter-fn]
  (swap! state update :exporters conj exporter-fn))

(defn export-trace
  "Export a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (mapv get-span @(:spans trace))
          export-data {:trace trace :spans spans}]
      (doseq [exporter (:exporters @state)]
        (try
          (exporter export-data)
          (catch Exception e
            (logging/log :error "Trace export failed" {:error (.getMessage e)}))))
      (.incrementAndGet (:traces-exported (:stats @state)))
      export-data)))

(defn export-all-traces!
  "Export all completed traces."
  []
  (let [completed-traces (filter (fn [[_ t]] (= @(:status t) :completed))
                                 (:traces @state))]
    (doseq [[trace-id _] completed-traces]
      (export-trace trace-id))))

;; ============================================================================
;; Trace Visualization
;; ============================================================================

(defn get-trace-tree
  "Get trace as a tree structure."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (mapv get-span @(:spans trace))
          root-spans (filter #(nil? (:parent-span-id %)) spans)
          build-tree (fn build-tree [span]
                       (let [children (filter #(= (:parent-span-id %) (:id span)) spans)]
                         (assoc span :children (mapv build-tree children))))]
      {:trace trace
       :tree (mapv build-tree root-spans)})))

(defn get-trace-timeline
  "Get trace as a timeline."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (mapv get-span @(:spans trace))
          trace-start (:start-time trace)]
      {:trace-id trace-id
       :start-time trace-start
       :end-time @(:end-time trace)
       :duration (when @(:end-time trace)
                   (- @(:end-time trace) trace-start))
       :spans (mapv (fn [s]
                      {:id (:id s)
                       :name (:name s)
                       :operation (:operation s)
                       :relative-start (- (:start-time s) trace-start)
                       :duration (when @(:end-time s)
                                   (- @(:end-time s) (:start-time s)))
                       :status @(:status s)})
                    (sort-by :start-time spans))})))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-trace
  "Ring middleware for distributed tracing."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [context (extract-trace-context (:headers request))
            trace-id (or (:trace-id context) (create-trace! :name (:uri request)))
            span-id (create-span! trace-id
                                  :parent-span-id (:span-id context)
                                  :name "http-request"
                                  :operation (:request-method request)
                                  :tags {:uri (:uri request)
                                         :method (:request-method request)})]
        (with-trace trace-id
          (with-span span-id
            (try
              (let [response (handler (assoc request
                                             :trace-id trace-id
                                             :span-id span-id))]
                (add-span-tag! span-id :status (:status response))
                (end-span! span-id)
                (-> response
                    (assoc-in [:headers "X-Trace-Id"] trace-id)
                    (assoc-in [:headers "X-Span-Id"] span-id)))
              (catch Exception e
                (add-span-tag! span-id :error true)
                (end-span! span-id :status :error :error (.getMessage e))
                (throw e))))))
      (handler request))))

(defn wrap-trace-propagation
  "Ring middleware to propagate trace context to outgoing requests."
  [handler]
  (fn [request]
    (let [trace-id (or (:trace-id request) (current-trace-id))
          span-id (or (:span-id request) (current-span-id))
          trace (when trace-id (get-trace trace-id))]
      (if (and trace-id span-id trace)
        (let [headers (inject-trace-context (:headers request)
                                            trace-id
                                            span-id
                                            (:sampled? trace))]
          (handler (assoc request :headers headers)))
        (handler request)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable tracing."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-sample-rate!
  "Set sampling rate (0.0 to 1.0)."
  [rate]
  (swap! state assoc-in [:config :sample-rate] (max 0.0 (min 1.0 rate))))

(defn set-propagation-format!
  "Set propagation format (:w3c, :b3, :jaeger)."
  [format]
  (swap! state assoc-in [:config :propagation-format] format))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-tracer-metrics
  "Get tracer metrics."
  []
  (let [stats (:stats @state)]
    {:traces-created (.get (:traces-created stats))
     :spans-created (.get (:spans-created stats))
     :traces-sampled (.get (:traces-sampled stats))
     :traces-exported (.get (:traces-exported stats))
     :active-traces (count (:traces @state))
     :active-spans (count (:spans @state))
     :exporters-count (count (:exporters @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-tracer-stats
  "Get tracer statistics."
  []
  (merge (get-tracer-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :sample-rate (get-in @state [:config :sample-rate])
          :propagation-format (get-in @state [:config :propagation-format])}))

(defn reset-stats!
  "Reset tracer statistics."
  []
  (.set (:traces-created (:stats @state)) 0)
  (.set (:spans-created (:stats @state)) 0)
  (.set (:traces-sampled (:stats @state)) 0)
  (.set (:traces-exported (:stats @state)) 0))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn cleanup-old-traces!
  "Clean up old completed traces."
  [max-age-ms]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        old-traces (filter (fn [[_ t]]
                             (and (= @(:status t) :completed)
                                  (< (:start-time t) cutoff)))
                           (:traces @state))]
    (doseq [[trace-id _] old-traces]
      (delete-trace! trace-id))
    (count old-traces)))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-tracer!
  "Initialize the request tracer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request tracer initialized")
    (events/emit! :request-tracer-initialized {})
    true))
