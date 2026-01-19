(ns mental-models.pipeline.integration.api-gateway
  "API Gateway Module
   
   Unified API management:
   - Route registration
   - Request routing
   - API versioning
   - Request transformation
   - Response aggregation"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.pipeline.integration.access-control :as acl]
   [mental-models.pipeline.integration.response-cache :as cache]
   [mental-models.pipeline.integration.request-validator :as validator]))

;; =============================================================================
;; GATEWAY STATE
;; =============================================================================

(defonce gateway-state (atom {:routes {}
                              :middleware []
                              :transformers {}
                              :aggregators {}
                              :config {:default-version "v1"
                                       :rate-limit-per-minute 1000
                                       :timeout-ms 30000}}))

;; =============================================================================
;; ROUTE REGISTRATION
;; =============================================================================

(defn register-route!
  "Register an API route."
  [route-id {:keys [method path handler version middleware auth-required? cache-ttl schema]}]
  (log/info "Registering route" {:id route-id :method method :path path})
  (swap! gateway-state assoc-in [:routes route-id]
         {:id route-id
          :method (or method :get)
          :path path
          :handler handler
          :version (or version "v1")
          :middleware (or middleware [])
          :auth-required? (if (nil? auth-required?) true auth-required?)
          :cache-ttl cache-ttl
          :schema schema
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :gateway/routes-registered)
  route-id)

(defn unregister-route!
  "Unregister an API route."
  [route-id]
  (log/info "Unregistering route" {:id route-id})
  (swap! gateway-state update :routes dissoc route-id))

(defn get-route
  "Get a route by ID."
  [route-id]
  (get-in @gateway-state [:routes route-id]))

(defn list-routes
  "List all registered routes."
  [& {:keys [version method]}]
  (let [routes (vals (:routes @gateway-state))]
    (cond->> routes
      version (filter #(= (:version %) version))
      method (filter #(= (:method %) method)))))

;; =============================================================================
;; ROUTE MATCHING
;; =============================================================================

(defn path-matches?
  "Check if a path matches a route pattern."
  [pattern path]
  (let [pattern-parts (str/split pattern #"/")
        path-parts (str/split path #"/")]
    (when (= (count pattern-parts) (count path-parts))
      (every? (fn [[p1 p2]]
                (or (= p1 p2)
                    (str/starts-with? p1 ":")))
              (map vector pattern-parts path-parts)))))

(defn extract-path-params
  "Extract path parameters from a path."
  [pattern path]
  (let [pattern-parts (str/split pattern #"/")
        path-parts (str/split path #"/")]
    (into {}
          (for [[p1 p2] (map vector pattern-parts path-parts)
                :when (str/starts-with? p1 ":")]
            [(keyword (subs p1 1)) p2]))))

(defn find-route
  "Find a matching route for a request."
  [method path version]
  (let [routes (vals (:routes @gateway-state))]
    (first (filter (fn [route]
                     (and (= (:method route) method)
                          (= (:version route) version)
                          (path-matches? (:path route) path)))
                   routes))))

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn register-middleware!
  "Register global middleware."
  [middleware-fn & {:keys [priority] :or {priority 50}}]
  (swap! gateway-state update :middleware
         (fn [mw]
           (sort-by :priority
                    (conj mw {:fn middleware-fn :priority priority})))))

(defn apply-middleware
  "Apply middleware to a request."
  [request middleware-chain]
  (reduce (fn [req mw]
            (if (:error req)
              req
              (try
                ((:fn mw) req)
                (catch Exception e
                  (assoc req :error (.getMessage e))))))
          request
          middleware-chain))

;; =============================================================================
;; REQUEST TRANSFORMATION
;; =============================================================================

(defn register-transformer!
  "Register a request/response transformer."
  [transformer-id {:keys [request-fn response-fn]}]
  (log/info "Registering transformer" {:id transformer-id})
  (swap! gateway-state assoc-in [:transformers transformer-id]
         {:request-fn request-fn
          :response-fn response-fn}))

(defn transform-request
  "Transform a request."
  [request transformer-ids]
  (reduce (fn [req tid]
            (if-let [transformer (get-in @gateway-state [:transformers tid])]
              (if-let [transform-fn (:request-fn transformer)]
                (transform-fn req)
                req)
              req))
          request
          transformer-ids))

(defn transform-response
  "Transform a response."
  [response transformer-ids]
  (reduce (fn [resp tid]
            (if-let [transformer (get-in @gateway-state [:transformers tid])]
              (if-let [transform-fn (:response-fn transformer)]
                (transform-fn resp)
                resp)
              resp))
          response
          (reverse transformer-ids)))

;; =============================================================================
;; RESPONSE AGGREGATION
;; =============================================================================

(defn register-aggregator!
  "Register a response aggregator."
  [aggregator-id aggregator-fn]
  (log/info "Registering aggregator" {:id aggregator-id})
  (swap! gateway-state assoc-in [:aggregators aggregator-id] aggregator-fn))

(defn aggregate-responses
  "Aggregate multiple responses."
  [aggregator-id responses]
  (if-let [aggregator (get-in @gateway-state [:aggregators aggregator-id])]
    (aggregator responses)
    {:results responses}))

;; =============================================================================
;; REQUEST HANDLING
;; =============================================================================

(defn handle-request
  "Handle an API request through the gateway."
  [request]
  (when (flags/is-enabled? "api-gateway")
    (let [start-time (System/currentTimeMillis)
          {:keys [method path version user-id params body headers]} request
          version (or version (get-in @gateway-state [:config :default-version]))]
      (log/info "Handling request" {:method method :path path :version version})
      (metrics/inc-counter! :gateway/requests)
      ;; Find matching route
      (if-let [route (find-route method path version)]
        (let [;; Extract path params
              path-params (extract-path-params (:path route) path)
              ;; Build full request
              full-request (merge request
                                  {:route route
                                   :path-params path-params
                                   :params (merge params path-params)})
              ;; Check authentication
              auth-ok? (or (not (:auth-required? route))
                           (some? user-id))
              ;; Check authorization
              authz-ok? (or (not (:permission route))
                            (acl/has-permission? user-id (:permission route)))
              ;; Validate request
              validation-result (when (:schema route)
                                  (validator/validate (:schema route) params))]
          (cond
            ;; Auth failed
            (not auth-ok?)
            (do
              (metrics/inc-counter! :gateway/auth-failures)
              {:status 401 :body {:error "Unauthorized"}})
            
            ;; Authz failed
            (not authz-ok?)
            (do
              (metrics/inc-counter! :gateway/authz-failures)
              {:status 403 :body {:error "Forbidden"}})
            
            ;; Validation failed
            (and validation-result (not (:valid? validation-result)))
            (do
              (metrics/inc-counter! :gateway/validation-failures)
              {:status 400 :body {:error "Validation failed" :details (:errors validation-result)}})
            
            ;; Check cache
            (and (:cache-ttl route) (= method :get))
            (let [cache-key (cache/generate-cache-key method path version (pr-str params))]
              (if-let [cached (cache/cache-get cache-key)]
                (do
                  (metrics/inc-counter! :gateway/cache-hits)
                  cached)
                (let [response ((:handler route) full-request)]
                  (cache/cache-put! cache-key response :ttl-ms (:cache-ttl route))
                  response)))
            
            ;; Execute handler
            :else
            (try
              (let [;; Apply global middleware
                    processed-request (apply-middleware full-request (:middleware @gateway-state))
                    ;; Apply route middleware
                    final-request (apply-middleware processed-request
                                                    (map (fn [mw] {:fn mw :priority 0})
                                                         (:middleware route)))
                    ;; Execute handler
                    response (if (:error final-request)
                               {:status 500 :body {:error (:error final-request)}}
                               ((:handler route) final-request))
                    duration (- (System/currentTimeMillis) start-time)]
                (metrics/observe-histogram! :gateway/request-duration duration)
                (events/publish! :gateway/request-handled {:route (:id route) :duration duration})
                response)
              (catch Exception e
                (log/error "Request handler error" {:error (.getMessage e)})
                (metrics/inc-counter! :gateway/errors)
                {:status 500 :body {:error "Internal server error"}}))))
        ;; Route not found
        (do
          (metrics/inc-counter! :gateway/not-found)
          {:status 404 :body {:error "Not found"}})))))

;; =============================================================================
;; CONVENIENCE MACROS
;; =============================================================================

(defmacro defroute
  "Define a route with a handler."
  [route-id method path opts & body]
  `(register-route! ~route-id
                    (merge {:method ~method
                            :path ~path
                            :handler (fn [~'request] ~@body)}
                           ~opts)))

;; =============================================================================
;; DEFAULT MIDDLEWARE
;; =============================================================================

(def logging-middleware
  {:fn (fn [request]
         (log/debug "Request" {:method (:method request) :path (:path request)})
         request)
   :priority 10})

(def timing-middleware
  {:fn (fn [request]
         (assoc request :start-time (System/currentTimeMillis)))
   :priority 20})

(def cors-middleware
  {:fn (fn [request]
         (assoc-in request [:headers "Access-Control-Allow-Origin"] "*"))
   :priority 30})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-api-gateway!
  "Initialize API gateway."
  []
  (log/info "Initializing API gateway")
  ;; Register feature flag
  (flags/register-flag! "api-gateway" "Enable API gateway" true)
  ;; Create metrics
  (metrics/create-counter! :gateway/routes-registered "Routes registered")
  (metrics/create-counter! :gateway/requests "Requests handled")
  (metrics/create-counter! :gateway/auth-failures "Auth failures")
  (metrics/create-counter! :gateway/authz-failures "Authz failures")
  (metrics/create-counter! :gateway/validation-failures "Validation failures")
  (metrics/create-counter! :gateway/cache-hits "Cache hits")
  (metrics/create-counter! :gateway/not-found "Not found")
  (metrics/create-counter! :gateway/errors "Errors")
  (metrics/create-histogram! :gateway/request-duration "Request duration" [10 50 100 500 1000])
  ;; Register default middleware
  (register-middleware! (:fn logging-middleware) :priority (:priority logging-middleware))
  (register-middleware! (:fn timing-middleware) :priority (:priority timing-middleware))
  (register-middleware! (:fn cors-middleware) :priority (:priority cors-middleware))
  (log/info "API gateway initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-gateway-status []
  {:enabled (flags/is-enabled? "api-gateway")
   :routes (count (:routes @gateway-state))
   :middleware (count (:middleware @gateway-state))
   :transformers (count (:transformers @gateway-state))
   :aggregators (count (:aggregators @gateway-state))
   :config (:config @gateway-state)})
