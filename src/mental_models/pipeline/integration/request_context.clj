(ns mental-models.pipeline.integration.request-context
  "Request context manager for mental model analysis system.
   
   Features:
   - Request-scoped context
   - Context propagation
   - Correlation IDs
   - User context
   - Tenant context
   - Feature flags per request
   - Context middleware
   - Async context preservation"
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
;; Thread-Local Context
;; ============================================================================

(def ^:dynamic *request-context* nil)

(defonce ^:private context-store
  (atom {})) ;; context-id -> context

;; ============================================================================
;; Context Creation
;; ============================================================================

(defn create-context
  "Create a new request context."
  [& {:keys [request-id correlation-id user-id tenant-id
             trace-id span-id parent-span-id
             metadata]}]
  {:id (or request-id (str (UUID/randomUUID)))
   :correlation-id (or correlation-id (str (UUID/randomUUID)))
   :trace-id (or trace-id (str (UUID/randomUUID)))
   :span-id (or span-id (str (UUID/randomUUID)))
   :parent-span-id parent-span-id
   :user-id user-id
   :tenant-id tenant-id
   :metadata (or metadata {})
   :attributes (atom {})
   :start-time (System/currentTimeMillis)
   :created-at (System/currentTimeMillis)})

(defn from-request
  "Create context from HTTP request."
  [request]
  (create-context
   :request-id (or (get-in request [:headers "x-request-id"])
                   (str (UUID/randomUUID)))
   :correlation-id (or (get-in request [:headers "x-correlation-id"])
                       (get-in request [:headers "x-request-id"])
                       (str (UUID/randomUUID)))
   :trace-id (get-in request [:headers "x-trace-id"])
   :span-id (get-in request [:headers "x-span-id"])
   :parent-span-id (get-in request [:headers "x-parent-span-id"])
   :user-id (get-in request [:user :id])
   :tenant-id (get-in request [:tenant :id])
   :metadata {:method (:request-method request)
              :uri (:uri request)
              :remote-addr (:remote-addr request)
              :user-agent (get-in request [:headers "user-agent"])}))

;; ============================================================================
;; Context Access
;; ============================================================================

(defn current-context
  "Get the current request context."
  []
  *request-context*)

(defn get-context
  "Get a context by ID."
  [context-id]
  (get @context-store context-id))

(defn get-request-id
  "Get current request ID."
  []
  (:id (current-context)))

(defn get-correlation-id
  "Get current correlation ID."
  []
  (:correlation-id (current-context)))

(defn get-trace-id
  "Get current trace ID."
  []
  (:trace-id (current-context)))

(defn get-user-id
  "Get current user ID."
  []
  (:user-id (current-context)))

(defn get-tenant-id
  "Get current tenant ID."
  []
  (:tenant-id (current-context)))

;; ============================================================================
;; Context Attributes
;; ============================================================================

(defn set-attribute!
  "Set an attribute in the current context."
  [key value]
  (when-let [ctx (current-context)]
    (swap! (:attributes ctx) assoc key value)))

(defn get-attribute
  "Get an attribute from the current context."
  [key & {:keys [default]}]
  (when-let [ctx (current-context)]
    (get @(:attributes ctx) key default)))

(defn remove-attribute!
  "Remove an attribute from the current context."
  [key]
  (when-let [ctx (current-context)]
    (swap! (:attributes ctx) dissoc key)))

(defn get-all-attributes
  "Get all attributes from the current context."
  []
  (when-let [ctx (current-context)]
    @(:attributes ctx)))

;; ============================================================================
;; Context Binding
;; ============================================================================

(defmacro with-context
  "Execute body with the given context."
  [context & body]
  `(binding [*request-context* ~context]
     (try
       (swap! context-store assoc (:id ~context) ~context)
       ~@body
       (finally
         (swap! context-store dissoc (:id ~context))))))

(defmacro with-new-context
  "Execute body with a new context."
  [opts & body]
  `(with-context (create-context ~@(mapcat identity opts))
     ~@body))

(defn wrap-context
  "Wrap a function to preserve context."
  [f]
  (let [ctx (current-context)]
    (fn [& args]
      (with-context ctx
        (apply f args)))))

;; ============================================================================
;; Context Propagation
;; ============================================================================

(defn propagate-headers
  "Get headers for context propagation."
  []
  (when-let [ctx (current-context)]
    {"X-Request-ID" (:id ctx)
     "X-Correlation-ID" (:correlation-id ctx)
     "X-Trace-ID" (:trace-id ctx)
     "X-Span-ID" (:span-id ctx)
     "X-Parent-Span-ID" (:parent-span-id ctx)}))

(defn create-child-context
  "Create a child context for nested operations."
  []
  (when-let [parent (current-context)]
    (create-context
     :correlation-id (:correlation-id parent)
     :trace-id (:trace-id parent)
     :parent-span-id (:span-id parent)
     :user-id (:user-id parent)
     :tenant-id (:tenant-id parent)
     :metadata (:metadata parent))))

(defmacro with-child-context
  "Execute body with a child context."
  [& body]
  `(with-context (create-child-context)
     ~@body))

;; ============================================================================
;; Async Context Preservation
;; ============================================================================

(defn go-with-context
  "Create a go block that preserves context."
  [& body]
  (let [ctx (current-context)]
    `(go
       (with-context ~ctx
         ~@body))))

(defn thread-with-context
  "Create a thread that preserves context."
  [f]
  (let [ctx (current-context)]
    (async/thread
      (with-context ctx
        (f)))))

(defn future-with-context
  "Create a future that preserves context."
  [f]
  (let [ctx (current-context)]
    (future
      (with-context ctx
        (f)))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn context-middleware
  "Ring middleware for request context."
  [handler]
  (fn [request]
    (let [ctx (from-request request)]
      (with-context ctx
        (let [response (handler (assoc request :context ctx))]
          (-> response
              (assoc-in [:headers "X-Request-ID"] (:id ctx))
              (assoc-in [:headers "X-Correlation-ID"] (:correlation-id ctx))))))))

;; ============================================================================
;; Logging Integration
;; ============================================================================

(defn context-for-logging
  "Get context data for logging."
  []
  (when-let [ctx (current-context)]
    {:request-id (:id ctx)
     :correlation-id (:correlation-id ctx)
     :trace-id (:trace-id ctx)
     :span-id (:span-id ctx)
     :user-id (:user-id ctx)
     :tenant-id (:tenant-id ctx)}))

(defn log-with-context
  "Log with context information."
  [level message & {:keys [data]}]
  (logging/log level message (merge (context-for-logging) data)))

;; ============================================================================
;; Metrics Integration
;; ============================================================================

(defn record-metric-with-context
  "Record a metric with context tags."
  [metric-name value & {:keys [tags]}]
  (let [ctx (current-context)
        context-tags (when ctx
                       {:tenant-id (:tenant-id ctx)
                        :user-id (:user-id ctx)})]
    (metrics/record! metric-name value :tags (merge context-tags tags))))

;; ============================================================================
;; Feature Flags Per Request
;; ============================================================================

(defn set-feature-flag!
  "Set a feature flag for the current request."
  [flag-name enabled?]
  (set-attribute! [:feature-flags flag-name] enabled?))

(defn feature-enabled?
  "Check if a feature is enabled for the current request."
  [flag-name]
  (let [request-flag (get-attribute [:feature-flags flag-name])]
    (if (some? request-flag)
      request-flag
      (flags/enabled? flag-name))))

;; ============================================================================
;; Request Timing
;; ============================================================================

(defn get-elapsed-time
  "Get elapsed time since context creation."
  []
  (when-let [ctx (current-context)]
    (- (System/currentTimeMillis) (:start-time ctx))))

(defn mark-checkpoint!
  "Mark a timing checkpoint."
  [checkpoint-name]
  (when-let [ctx (current-context)]
    (set-attribute! [:checkpoints checkpoint-name] (System/currentTimeMillis))))

(defn get-checkpoint-duration
  "Get duration between checkpoints."
  [from-checkpoint to-checkpoint]
  (let [from-time (get-attribute [:checkpoints from-checkpoint])
        to-time (get-attribute [:checkpoints to-checkpoint])]
    (when (and from-time to-time)
      (- to-time from-time))))

;; ============================================================================
;; Context Cleanup
;; ============================================================================

(defn cleanup-old-contexts!
  "Clean up old contexts from the store."
  [max-age-ms]
  (let [now (System/currentTimeMillis)
        old-contexts (filter (fn [[_ ctx]]
                               (> (- now (:created-at ctx)) max-age-ms))
                             @context-store)]
    (doseq [[id _] old-contexts]
      (swap! context-store dissoc id))
    (count old-contexts)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-context-stats
  "Get context statistics."
  []
  {:active-contexts (count @context-store)
   :current-context (when-let [ctx (current-context)]
                      {:id (:id ctx)
                       :correlation-id (:correlation-id ctx)
                       :elapsed-ms (get-elapsed-time)
                       :attributes-count (count @(:attributes ctx))})})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-context!
  "Initialize the request context system."
  []
  ;; Start cleanup task
  (go-loop []
    (<! (timeout 60000))
    (cleanup-old-contexts! 300000) ;; 5 minutes
    (recur))
  
  (logging/log :info "Request context system initialized")
  (events/emit! :request-context-initialized {})
  true)
