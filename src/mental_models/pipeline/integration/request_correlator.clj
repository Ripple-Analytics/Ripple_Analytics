(ns mental-models.pipeline.integration.request-correlator
  "Request correlator for mental model analysis system.
   
   Features:
   - Request correlation IDs
   - Distributed tracing context
   - Parent-child relationships
   - Correlation propagation
   - Correlation storage
   - Correlation lookup
   - Correlation metrics
   - Correlation cleanup"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:correlations {}     ;; correlation-id -> correlation-data
         :traces {}           ;; trace-id -> trace-data
         :config {:header-name "X-Correlation-ID"
                  :trace-header "X-Trace-ID"
                  :span-header "X-Span-ID"
                  :parent-header "X-Parent-Span-ID"
                  :generate-if-missing? true
                  :propagate-to-children? true
                  :max-correlations 100000
                  :correlation-ttl-ms 3600000}  ;; 1 hour
         :stats {:correlations-created 0
                 :correlations-propagated 0
                 :correlations-looked-up 0
                 :traces-created 0
                 :spans-created 0}
         :initialized? false}))

;; ============================================================================
;; ID Generation
;; ============================================================================

(defn generate-correlation-id
  "Generate a new correlation ID."
  []
  (str (UUID/randomUUID)))

(defn generate-trace-id
  "Generate a new trace ID."
  []
  (str (UUID/randomUUID)))

(defn generate-span-id
  "Generate a new span ID."
  []
  (let [uuid (UUID/randomUUID)]
    (subs (str uuid) 0 16)))

;; ============================================================================
;; Correlation Management
;; ============================================================================

(defn create-correlation!
  "Create a new correlation."
  [correlation-id & {:keys [metadata]}]
  (let [correlation {:id correlation-id
                     :created-at (System/currentTimeMillis)
                     :metadata (or metadata {})
                     :children []
                     :events []}]
    
    ;; Cleanup if needed
    (let [max-correlations (get-in @state [:config :max-correlations])]
      (when (> (count (:correlations @state)) max-correlations)
        (let [ttl (get-in @state [:config :correlation-ttl-ms])
              cutoff (- (System/currentTimeMillis) ttl)]
          (swap! state update :correlations
                 (fn [corrs]
                   (into {} (filter (fn [[_ v]] (> (:created-at v) cutoff)) corrs)))))))
    
    (swap! state assoc-in [:correlations correlation-id] correlation)
    (swap! state update-in [:stats :correlations-created] inc)
    
    correlation-id))

(defn get-correlation
  "Get a correlation by ID."
  [correlation-id]
  (swap! state update-in [:stats :correlations-looked-up] inc)
  (get-in @state [:correlations correlation-id]))

(defn update-correlation!
  "Update a correlation."
  [correlation-id update-fn]
  (swap! state update-in [:correlations correlation-id] update-fn))

(defn add-correlation-event!
  "Add an event to a correlation."
  [correlation-id event]
  (update-correlation! correlation-id
                       (fn [corr]
                         (update corr :events conj
                                 (assoc event :timestamp (System/currentTimeMillis))))))

(defn add-child-correlation!
  "Add a child correlation."
  [parent-id child-id]
  (update-correlation! parent-id
                       (fn [corr]
                         (update corr :children conj child-id))))

;; ============================================================================
;; Trace Management
;; ============================================================================

(defn create-trace!
  "Create a new distributed trace."
  [trace-id & {:keys [name metadata]}]
  (let [trace {:id trace-id
               :name name
               :created-at (System/currentTimeMillis)
               :metadata (or metadata {})
               :spans []
               :root-span-id nil}]
    
    (swap! state assoc-in [:traces trace-id] trace)
    (swap! state update-in [:stats :traces-created] inc)
    
    trace-id))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (get-in @state [:traces trace-id]))

(defn create-span!
  "Create a new span in a trace."
  [trace-id span-name & {:keys [parent-span-id]}]
  (let [span-id (generate-span-id)
        span {:id span-id
              :name span-name
              :trace-id trace-id
              :parent-span-id parent-span-id
              :start-time (System/currentTimeMillis)
              :end-time nil
              :status :in-progress
              :tags {}
              :logs []}]
    
    (swap! state update-in [:traces trace-id :spans] conj span)
    (when-not parent-span-id
      (swap! state assoc-in [:traces trace-id :root-span-id] span-id))
    (swap! state update-in [:stats :spans-created] inc)
    
    span-id))

(defn end-span!
  "End a span."
  [trace-id span-id & {:keys [status]}]
  (swap! state update-in [:traces trace-id :spans]
         (fn [spans]
           (mapv (fn [span]
                   (if (= (:id span) span-id)
                     (assoc span
                            :end-time (System/currentTimeMillis)
                            :status (or status :completed))
                     span))
                 spans))))

(defn add-span-tag!
  "Add a tag to a span."
  [trace-id span-id key value]
  (swap! state update-in [:traces trace-id :spans]
         (fn [spans]
           (mapv (fn [span]
                   (if (= (:id span) span-id)
                     (assoc-in span [:tags key] value)
                     span))
                 spans))))

(defn add-span-log!
  "Add a log entry to a span."
  [trace-id span-id message & {:keys [level]}]
  (swap! state update-in [:traces trace-id :spans]
         (fn [spans]
           (mapv (fn [span]
                   (if (= (:id span) span-id)
                     (update span :logs conj
                             {:message message
                              :level (or level :info)
                              :timestamp (System/currentTimeMillis)})
                     span))
                 spans))))

;; ============================================================================
;; Request Correlation
;; ============================================================================

(defn extract-correlation-id
  "Extract correlation ID from request headers."
  [request]
  (let [header-name (get-in @state [:config :header-name])]
    (get-in request [:headers (str/lower-case header-name)])))

(defn extract-trace-context
  "Extract trace context from request headers."
  [request]
  (let [trace-header (get-in @state [:config :trace-header])
        span-header (get-in @state [:config :span-header])
        parent-header (get-in @state [:config :parent-header])
        headers (:headers request)]
    {:trace-id (get headers (str/lower-case trace-header))
     :span-id (get headers (str/lower-case span-header))
     :parent-span-id (get headers (str/lower-case parent-header))}))

(defn correlate-request
  "Add correlation context to a request."
  [request]
  (let [existing-id (extract-correlation-id request)
        correlation-id (or existing-id
                           (when (get-in @state [:config :generate-if-missing?])
                             (generate-correlation-id)))
        trace-context (extract-trace-context request)
        trace-id (or (:trace-id trace-context) (generate-trace-id))
        span-id (generate-span-id)]
    
    (when (and correlation-id (not existing-id))
      (create-correlation! correlation-id))
    
    (when (not (:trace-id trace-context))
      (create-trace! trace-id :name (:uri request)))
    
    (create-span! trace-id (str (:method request) " " (:uri request))
                  :parent-span-id (:parent-span-id trace-context))
    
    (swap! state update-in [:stats :correlations-propagated] inc)
    
    (-> request
        (assoc :correlation-id correlation-id)
        (assoc :trace-id trace-id)
        (assoc :span-id span-id)
        (assoc :parent-span-id (:parent-span-id trace-context)))))

(defn propagate-correlation
  "Propagate correlation context to outgoing request."
  [request correlation-context]
  (let [header-name (get-in @state [:config :header-name])
        trace-header (get-in @state [:config :trace-header])
        span-header (get-in @state [:config :span-header])
        parent-header (get-in @state [:config :parent-header])]
    (-> request
        (assoc-in [:headers header-name] (:correlation-id correlation-context))
        (assoc-in [:headers trace-header] (:trace-id correlation-context))
        (assoc-in [:headers parent-header] (:span-id correlation-context))
        (assoc-in [:headers span-header] (generate-span-id)))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-correlation
  "Ring middleware to add correlation context."
  [handler]
  (fn [request]
    (let [correlated-request (correlate-request request)
          response (handler correlated-request)
          header-name (get-in @state [:config :header-name])]
      (assoc-in response [:headers header-name] (:correlation-id correlated-request)))))

(defn wrap-trace
  "Ring middleware to add tracing context."
  [handler]
  (fn [request]
    (let [correlated-request (correlate-request request)
          trace-id (:trace-id correlated-request)
          span-id (:span-id correlated-request)]
      (try
        (let [response (handler correlated-request)]
          (end-span! trace-id span-id :status :completed)
          response)
        (catch Exception e
          (end-span! trace-id span-id :status :error)
          (add-span-log! trace-id span-id (.getMessage e) :level :error)
          (throw e))))))

;; ============================================================================
;; Context Propagation
;; ============================================================================

(def ^:dynamic *correlation-context* nil)

(defmacro with-correlation
  "Execute body with correlation context."
  [context & body]
  `(binding [*correlation-context* ~context]
     ~@body))

(defn current-correlation-id
  "Get the current correlation ID."
  []
  (:correlation-id *correlation-context*))

(defn current-trace-id
  "Get the current trace ID."
  []
  (:trace-id *correlation-context*))

(defn current-span-id
  "Get the current span ID."
  []
  (:span-id *correlation-context*))

;; ============================================================================
;; Correlation Lookup
;; ============================================================================

(defn find-correlations-by-metadata
  "Find correlations by metadata."
  [key value]
  (->> (vals (:correlations @state))
       (filter #(= (get-in % [:metadata key]) value))
       (vec)))

(defn find-child-correlations
  "Find all child correlations."
  [correlation-id]
  (when-let [correlation (get-correlation correlation-id)]
    (let [children (:children correlation)]
      (concat children
              (mapcat find-child-correlations children)))))

(defn get-correlation-tree
  "Get the full correlation tree."
  [correlation-id]
  (when-let [correlation (get-correlation correlation-id)]
    {:id correlation-id
     :metadata (:metadata correlation)
     :created-at (:created-at correlation)
     :children (mapv get-correlation-tree (:children correlation))}))

;; ============================================================================
;; Trace Visualization
;; ============================================================================

(defn get-trace-timeline
  "Get a timeline view of a trace."
  [trace-id]
  (when-let [trace (get-trace trace-id)]
    (let [spans (:spans trace)
          sorted-spans (sort-by :start-time spans)]
      {:trace-id trace-id
       :name (:name trace)
       :start-time (when (seq sorted-spans) (:start-time (first sorted-spans)))
       :end-time (when (seq sorted-spans)
                   (apply max (map #(or (:end-time %) (:start-time %)) sorted-spans)))
       :spans (mapv (fn [span]
                      {:id (:id span)
                       :name (:name span)
                       :start-time (:start-time span)
                       :end-time (:end-time span)
                       :duration-ms (when (:end-time span)
                                      (- (:end-time span) (:start-time span)))
                       :status (:status span)
                       :parent-span-id (:parent-span-id span)})
                    sorted-spans)})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-correlator-metrics
  "Get correlator metrics."
  []
  (let [stats (:stats @state)]
    {:correlations-created (:correlations-created stats)
     :correlations-propagated (:correlations-propagated stats)
     :correlations-looked-up (:correlations-looked-up stats)
     :traces-created (:traces-created stats)
     :spans-created (:spans-created stats)
     :active-correlations (count (:correlations @state))
     :active-traces (count (:traces @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-correlator-stats
  "Get correlator statistics."
  []
  (merge (get-correlator-metrics)
         {:header-name (get-in @state [:config :header-name])
          :generate-if-missing? (get-in @state [:config :generate-if-missing?])
          :correlation-ttl-ms (get-in @state [:config :correlation-ttl-ms])}))

(defn reset-stats!
  "Reset correlator statistics."
  []
  (swap! state assoc :stats {:correlations-created 0
                             :correlations-propagated 0
                             :correlations-looked-up 0
                             :traces-created 0
                             :spans-created 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-correlator!
  "Initialize the request correlator."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request correlator initialized")
    (events/emit! :request-correlator-initialized {})
    true))
