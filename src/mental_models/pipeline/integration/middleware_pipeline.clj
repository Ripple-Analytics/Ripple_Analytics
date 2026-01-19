(ns mental-models.pipeline.integration.middleware-pipeline
  "Middleware Pipeline Module
   
   Composable request/response processing:
   - Middleware chain composition
   - Request/response transformation
   - Error handling middleware
   - Timing middleware
   - Logging middleware"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; PIPELINE STATE
;; =============================================================================

(defonce pipelines (atom {}))

;; =============================================================================
;; MIDDLEWARE COMPOSITION
;; =============================================================================

(defn wrap-middleware
  "Wrap a handler with a middleware function."
  [handler middleware]
  (middleware handler))

(defn compose-middleware
  "Compose multiple middleware functions into a single middleware."
  [& middlewares]
  (fn [handler]
    (reduce wrap-middleware handler (reverse middlewares))))

(defn create-pipeline
  "Create a middleware pipeline."
  [pipeline-id handler & middlewares]
  {:id pipeline-id
   :handler handler
   :middlewares middlewares
   :composed (reduce wrap-middleware handler (reverse middlewares))
   :created-at (System/currentTimeMillis)})

(defn register-pipeline!
  "Register a middleware pipeline."
  [pipeline]
  (log/info "Registering pipeline" {:id (:id pipeline)})
  (swap! pipelines assoc (:id pipeline) pipeline)
  (:id pipeline))

(defn get-pipeline
  "Get a pipeline by ID."
  [pipeline-id]
  (get @pipelines pipeline-id))

(defn execute-pipeline
  "Execute a pipeline with a request."
  [pipeline-id request]
  (when (flags/is-enabled? "middleware-pipeline")
    (if-let [pipeline (get-pipeline pipeline-id)]
      (do
        (log/debug "Executing pipeline" {:id pipeline-id})
        (metrics/inc-counter! :middleware/executions)
        ((:composed pipeline) request))
      (throw (ex-info "Pipeline not found" {:pipeline-id pipeline-id})))))

;; =============================================================================
;; BUILT-IN MIDDLEWARE
;; =============================================================================

(defn logging-middleware
  "Middleware that logs requests and responses."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          request-id (or (:request-id request) (str (java.util.UUID/randomUUID)))]
      (log/info "Request started" {:request-id request-id :type (:type request)})
      (let [response (handler (assoc request :request-id request-id))
            duration (- (System/currentTimeMillis) start-time)]
        (log/info "Request completed" {:request-id request-id :duration-ms duration :status (:status response)})
        response))))

(defn timing-middleware
  "Middleware that adds timing information."
  [handler]
  (fn [request]
    (let [start-time (System/nanoTime)
          response (handler request)
          duration-ns (- (System/nanoTime) start-time)
          duration-ms (/ duration-ns 1000000.0)]
      (metrics/observe-histogram! :middleware/request-time duration-ms)
      (assoc response :timing {:duration-ns duration-ns
                               :duration-ms duration-ms}))))

(defn error-handling-middleware
  "Middleware that catches and handles errors."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (log/error "Request failed" {:error (.getMessage e) :request-id (:request-id request)})
        (metrics/inc-counter! :middleware/errors)
        {:status :error
         :error (.getMessage e)
         :error-type (type e)}))))

(defn validation-middleware
  "Middleware that validates requests."
  [validator]
  (fn [handler]
    (fn [request]
      (let [validation-result (validator request)]
        (if (:valid validation-result)
          (handler request)
          {:status :validation-error
           :errors (:errors validation-result)})))))

(defn authentication-middleware
  "Middleware that checks authentication."
  [auth-fn]
  (fn [handler]
    (fn [request]
      (if (auth-fn request)
        (handler request)
        {:status :unauthorized
         :error "Authentication required"}))))

(defn rate-limiting-middleware
  "Middleware that applies rate limiting."
  [rate-limiter]
  (fn [handler]
    (fn [request]
      (if (rate-limiter request)
        (handler request)
        {:status :rate-limited
         :error "Rate limit exceeded"}))))

(defn caching-middleware
  "Middleware that caches responses."
  [cache-atom cache-key-fn ttl-ms]
  (fn [handler]
    (fn [request]
      (let [cache-key (cache-key-fn request)
            cached (get @cache-atom cache-key)]
        (if (and cached (< (- (System/currentTimeMillis) (:cached-at cached)) ttl-ms))
          (do
            (metrics/inc-counter! :middleware/cache-hits)
            (:response cached))
          (let [response (handler request)]
            (swap! cache-atom assoc cache-key {:response response
                                               :cached-at (System/currentTimeMillis)})
            (metrics/inc-counter! :middleware/cache-misses)
            response))))))

(defn retry-middleware
  "Middleware that retries failed requests."
  [max-retries retry-delay-ms]
  (fn [handler]
    (fn [request]
      (loop [attempt 1]
        (let [response (handler request)]
          (if (and (= :error (:status response))
                   (< attempt max-retries))
            (do
              (Thread/sleep retry-delay-ms)
              (recur (inc attempt)))
            response))))))

(defn transformation-middleware
  "Middleware that transforms requests and responses."
  [request-transform response-transform]
  (fn [handler]
    (fn [request]
      (let [transformed-request (if request-transform
                                  (request-transform request)
                                  request)
            response (handler transformed-request)]
        (if response-transform
          (response-transform response)
          response)))))

(defn context-middleware
  "Middleware that adds context to requests."
  [context-fn]
  (fn [handler]
    (fn [request]
      (handler (merge request (context-fn request))))))

;; =============================================================================
;; ANALYSIS PIPELINE
;; =============================================================================

(defn create-analysis-pipeline
  "Create a standard analysis pipeline."
  [analyzer]
  (create-pipeline
   :analysis
   analyzer
   logging-middleware
   timing-middleware
   error-handling-middleware))

(defn create-batch-pipeline
  "Create a batch processing pipeline."
  [batch-handler]
  (create-pipeline
   :batch
   batch-handler
   logging-middleware
   timing-middleware
   error-handling-middleware
   (retry-middleware 3 1000)))

;; =============================================================================
;; PIPELINE MANAGEMENT
;; =============================================================================

(defn add-middleware!
  "Add middleware to an existing pipeline."
  [pipeline-id middleware]
  (when-let [pipeline (get-pipeline pipeline-id)]
    (let [new-middlewares (conj (:middlewares pipeline) middleware)
          new-composed (reduce wrap-middleware (:handler pipeline) (reverse new-middlewares))]
      (swap! pipelines assoc pipeline-id
             (assoc pipeline
                    :middlewares new-middlewares
                    :composed new-composed)))))

(defn remove-pipeline!
  "Remove a pipeline."
  [pipeline-id]
  (log/info "Removing pipeline" {:id pipeline-id})
  (swap! pipelines dissoc pipeline-id))

(defn list-pipelines
  "List all registered pipelines."
  []
  (keys @pipelines))

;; =============================================================================
;; ASYNC PIPELINE
;; =============================================================================

(defn async-middleware
  "Middleware that executes handler asynchronously."
  [handler]
  (fn [request]
    (let [result-promise (promise)]
      (future
        (try
          (deliver result-promise (handler request))
          (catch Exception e
            (deliver result-promise {:status :error :error (.getMessage e)}))))
      result-promise)))

(defn execute-async
  "Execute a pipeline asynchronously."
  [pipeline-id request]
  (let [pipeline (get-pipeline pipeline-id)
        async-handler (async-middleware (:composed pipeline))]
    (async-handler request)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-middleware-pipeline!
  "Initialize middleware pipeline system."
  []
  (log/info "Initializing middleware pipeline")
  ;; Register feature flag
  (flags/register-flag! "middleware-pipeline" "Enable middleware pipeline" true)
  ;; Create metrics
  (metrics/create-counter! :middleware/executions "Pipeline executions")
  (metrics/create-counter! :middleware/errors "Pipeline errors")
  (metrics/create-counter! :middleware/cache-hits "Cache hits")
  (metrics/create-counter! :middleware/cache-misses "Cache misses")
  (metrics/create-histogram! :middleware/request-time "Request time" [1 5 10 50 100 500])
  (log/info "Middleware pipeline initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-pipeline-status []
  {:enabled (flags/is-enabled? "middleware-pipeline")
   :registered-pipelines (count @pipelines)
   :pipelines (map (fn [[id p]]
                     {:id id
                      :middleware-count (count (:middlewares p))
                      :created-at (:created-at p)})
                   @pipelines)})
