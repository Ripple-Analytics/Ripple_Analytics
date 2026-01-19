(ns mental-models.pipeline.integration.request-router
  "Request Router Module
   
   Intelligent request routing:
   - Route definition and matching
   - Path parameter extraction
   - Query parameter parsing
   - Middleware chain execution
   - Route grouping and prefixing"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.util.regex Pattern Matcher]))

;; =============================================================================
;; REQUEST ROUTER STATE
;; =============================================================================

(defonce router-state (atom {:routes (ConcurrentHashMap.)
                             :groups (ConcurrentHashMap.)
                             :middleware []
                             :not-found-handler nil
                             :error-handler nil
                             :request-count (AtomicLong. 0)
                             :config {:case-sensitive false
                                      :trailing-slash :ignore
                                      :enable-cors false}}))

;; =============================================================================
;; PATH PARSING
;; =============================================================================

(defn parse-path-pattern
  "Parse a path pattern into segments and extract parameter names."
  [pattern]
  (let [segments (str/split pattern #"/")
        params (atom [])
        regex-parts (for [seg segments]
                      (cond
                        (empty? seg) ""
                        (str/starts-with? seg ":") (do
                                                     (swap! params conj (keyword (subs seg 1)))
                                                     "([^/]+)")
                        (str/starts-with? seg "*") (do
                                                     (swap! params conj (keyword (subs seg 1)))
                                                     "(.*)")
                        :else (Pattern/quote seg)))
        regex-str (str "^" (str/join "/" regex-parts) "$")]
    {:pattern pattern
     :regex (Pattern/compile regex-str)
     :params @params
     :segments segments}))

(defn match-path
  "Match a path against a parsed pattern."
  [parsed-pattern path]
  (let [matcher (.matcher ^Pattern (:regex parsed-pattern) path)]
    (when (.matches matcher)
      (let [params (:params parsed-pattern)
            values (for [i (range 1 (inc (.groupCount matcher)))]
                     (.group matcher (int i)))]
        {:matched true
         :params (zipmap params values)}))))

(defn normalize-path
  "Normalize a path based on configuration."
  [path config]
  (let [path (if (:case-sensitive config) path (str/lower-case path))]
    (case (:trailing-slash config)
      :ignore (str/replace path #"/$" "")
      :require (if (str/ends-with? path "/") path (str path "/"))
      :strip (str/replace path #"/$" "")
      path)))

;; =============================================================================
;; ROUTE DEFINITION
;; =============================================================================

(defn create-route
  "Create a route definition."
  [route-id {:keys [method path handler middleware name description]}]
  (let [parsed (parse-path-pattern path)]
    {:id route-id
     :method (or method :any)
     :path path
     :parsed-pattern parsed
     :handler handler
     :middleware (or middleware [])
     :name name
     :description description
     :created-at (System/currentTimeMillis)
     :request-count (AtomicLong. 0)
     :error-count (AtomicLong. 0)}))

(defn register-route!
  "Register a route."
  [route-id method path handler & {:keys [middleware name description]}]
  (log/info "Registering route" {:id route-id :method method :path path})
  (let [route (create-route route-id {:method method
                                      :path path
                                      :handler handler
                                      :middleware middleware
                                      :name name
                                      :description description})]
    (.put ^ConcurrentHashMap (:routes @router-state) route-id route)
    (metrics/inc-counter! :requestrouter/routes-registered)
    (events/publish! :requestrouter/route-registered {:route-id route-id})
    route-id))

(defn unregister-route!
  "Unregister a route."
  [route-id]
  (log/info "Unregistering route" {:id route-id})
  (.remove ^ConcurrentHashMap (:routes @router-state) route-id))

(defn get-route
  "Get a route by ID."
  [route-id]
  (.get ^ConcurrentHashMap (:routes @router-state) route-id))

(defn list-routes
  "List all registered routes."
  [& {:keys [method]}]
  (let [routes (vals (:routes @router-state))]
    (if method
      (filter #(or (= (:method %) :any) (= (:method %) method)) routes)
      routes)))

;; =============================================================================
;; ROUTE SHORTCUTS
;; =============================================================================

(defn GET [path handler & opts]
  (let [route-id (keyword (str "get-" (hash path)))]
    (apply register-route! route-id :get path handler opts)))

(defn POST [path handler & opts]
  (let [route-id (keyword (str "post-" (hash path)))]
    (apply register-route! route-id :post path handler opts)))

(defn PUT [path handler & opts]
  (let [route-id (keyword (str "put-" (hash path)))]
    (apply register-route! route-id :put path handler opts)))

(defn DELETE [path handler & opts]
  (let [route-id (keyword (str "delete-" (hash path)))]
    (apply register-route! route-id :delete path handler opts)))

(defn PATCH [path handler & opts]
  (let [route-id (keyword (str "patch-" (hash path)))]
    (apply register-route! route-id :patch path handler opts)))

(defn OPTIONS [path handler & opts]
  (let [route-id (keyword (str "options-" (hash path)))]
    (apply register-route! route-id :options path handler opts)))

(defn HEAD [path handler & opts]
  (let [route-id (keyword (str "head-" (hash path)))]
    (apply register-route! route-id :head path handler opts)))

;; =============================================================================
;; ROUTE GROUPS
;; =============================================================================

(defn create-group
  "Create a route group."
  [group-id {:keys [prefix middleware]}]
  {:id group-id
   :prefix (or prefix "")
   :middleware (or middleware [])
   :routes []})

(defn register-group!
  "Register a route group."
  [group-id & {:keys [prefix middleware]}]
  (log/info "Registering route group" {:id group-id :prefix prefix})
  (let [group (create-group group-id {:prefix prefix :middleware middleware})]
    (.put ^ConcurrentHashMap (:groups @router-state) group-id group)
    group-id))

(defn add-route-to-group!
  "Add a route to a group."
  [group-id route-id method path handler & opts]
  (when-let [group (.get ^ConcurrentHashMap (:groups @router-state) group-id)]
    (let [full-path (str (:prefix group) path)
          combined-middleware (concat (:middleware group) (:middleware (apply hash-map opts)))]
      (apply register-route! route-id method full-path handler
             :middleware combined-middleware opts)
      (swap! router-state update-in [:groups group-id :routes] conj route-id))))

(defn get-group
  "Get a route group."
  [group-id]
  (.get ^ConcurrentHashMap (:groups @router-state) group-id))

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn register-middleware!
  "Register global middleware."
  [middleware-fn & {:keys [priority]}]
  (swap! router-state update :middleware
         (fn [mw]
           (sort-by :priority >
                    (conj mw {:fn middleware-fn :priority (or priority 0)})))))

(defn clear-middleware!
  "Clear all global middleware."
  []
  (swap! router-state assoc :middleware []))

(defn wrap-middleware
  "Wrap a handler with middleware."
  [handler middleware-chain]
  (reduce (fn [h mw]
            (if (map? mw)
              ((:fn mw) h)
              (mw h)))
          handler
          (reverse middleware-chain)))

;; =============================================================================
;; REQUEST HANDLING
;; =============================================================================

(defn parse-query-string
  "Parse a query string into a map."
  [query-string]
  (when query-string
    (into {} (for [pair (str/split query-string #"&")
                   :let [[k v] (str/split pair #"=" 2)]
                   :when k]
               [(keyword k) (or v true)]))))

(defn create-request
  "Create a request map."
  [{:keys [method path query-string headers body]}]
  {:method (keyword (str/lower-case (name (or method :get))))
   :path path
   :query-string query-string
   :query-params (parse-query-string query-string)
   :headers (or headers {})
   :body body
   :timestamp (System/currentTimeMillis)})

(defn find-matching-route
  "Find a route that matches the request."
  [request]
  (let [method (:method request)
        path (normalize-path (:path request) (:config @router-state))
        routes (vals (:routes @router-state))]
    (first (for [route routes
                 :let [match (match-path (:parsed-pattern route) path)]
                 :when (and match
                            (or (= (:method route) :any)
                                (= (:method route) method)))]
             {:route route
              :params (:params match)}))))

(defn set-not-found-handler!
  "Set the not found handler."
  [handler]
  (swap! router-state assoc :not-found-handler handler))

(defn set-error-handler!
  "Set the error handler."
  [handler]
  (swap! router-state assoc :error-handler handler))

(defn handle-request
  "Handle a request through the router."
  [request]
  (.incrementAndGet ^AtomicLong (:request-count @router-state))
  (metrics/inc-counter! :requestrouter/requests-handled)
  (let [request (if (map? request) request (create-request request))
        start-time (System/currentTimeMillis)]
    (try
      (if-let [{:keys [route params]} (find-matching-route request)]
        (do
          (.incrementAndGet ^AtomicLong (:request-count route))
          (let [request-with-params (assoc request :path-params params :route-id (:id route))
                ;; Combine global and route middleware
                all-middleware (concat (:middleware @router-state) (:middleware route))
                handler (wrap-middleware (:handler route) all-middleware)
                response (handler request-with-params)
                duration (- (System/currentTimeMillis) start-time)]
            (metrics/observe-histogram! :requestrouter/request-duration duration)
            (events/publish! :requestrouter/request-completed {:route-id (:id route)
                                                                :method (:method request)
                                                                :path (:path request)
                                                                :duration duration})
            response))
        ;; No matching route
        (do
          (metrics/inc-counter! :requestrouter/not-found)
          (if-let [not-found-handler (:not-found-handler @router-state)]
            (not-found-handler request)
            {:status 404
             :body {:error "Not Found"
                    :path (:path request)}})))
      (catch Exception e
        (metrics/inc-counter! :requestrouter/errors)
        (log/error "Request handling error" {:path (:path request) :error (.getMessage e)})
        (if-let [error-handler (:error-handler @router-state)]
          (error-handler request e)
          {:status 500
           :body {:error "Internal Server Error"
                  :message (.getMessage e)}})))))

;; =============================================================================
;; CORS
;; =============================================================================

(defn cors-middleware
  "CORS middleware."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (assoc-in [:headers "Access-Control-Allow-Origin"] "*")
          (assoc-in [:headers "Access-Control-Allow-Methods"] "GET, POST, PUT, DELETE, PATCH, OPTIONS")
          (assoc-in [:headers "Access-Control-Allow-Headers"] "Content-Type, Authorization")))))

(defn enable-cors!
  "Enable CORS support."
  []
  (swap! router-state assoc-in [:config :enable-cors] true)
  (register-middleware! cors-middleware :priority 100))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-route-stats
  "Get statistics for a route."
  [route-id]
  (when-let [route (get-route route-id)]
    {:id route-id
     :method (:method route)
     :path (:path route)
     :name (:name route)
     :request-count (.get ^AtomicLong (:request-count route))
     :error-count (.get ^AtomicLong (:error-count route))
     :created-at (:created-at route)}))

(defn get-all-route-stats
  "Get statistics for all routes."
  []
  (into {} (for [route-id (keys (:routes @router-state))]
             [route-id (get-route-stats route-id)])))

(defn get-router-stats
  "Get overall router statistics."
  []
  {:total-routes (.size ^ConcurrentHashMap (:routes @router-state))
   :total-groups (.size ^ConcurrentHashMap (:groups @router-state))
   :total-requests (.get ^AtomicLong (:request-count @router-state))
   :middleware-count (count (:middleware @router-state))
   :routes (get-all-route-stats)})

;; =============================================================================
;; ROUTE LISTING
;; =============================================================================

(defn print-routes
  "Print all registered routes."
  []
  (let [routes (sort-by :path (vals (:routes @router-state)))]
    (doseq [route routes]
      (println (format "%-8s %s" (str/upper-case (name (:method route))) (:path route))))))

(defn routes-table
  "Get routes as a table."
  []
  (for [route (sort-by :path (vals (:routes @router-state)))]
    {:method (str/upper-case (name (:method route)))
     :path (:path route)
     :name (:name route)
     :middleware-count (count (:middleware route))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-request-router!
  "Initialize request router."
  []
  (log/info "Initializing request router")
  ;; Register feature flag
  (flags/register-flag! "request-router" "Enable request router" true)
  ;; Create metrics
  (metrics/create-counter! :requestrouter/routes-registered "Routes registered")
  (metrics/create-counter! :requestrouter/requests-handled "Requests handled")
  (metrics/create-counter! :requestrouter/not-found "Not found responses")
  (metrics/create-counter! :requestrouter/errors "Request errors")
  (metrics/create-histogram! :requestrouter/request-duration "Request duration in ms")
  (metrics/create-gauge! :requestrouter/total-routes "Total routes"
                         #(.size ^ConcurrentHashMap (:routes @router-state)))
  (log/info "Request router initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-router-status []
  {:enabled (flags/is-enabled? "request-router")
   :routes (.size ^ConcurrentHashMap (:routes @router-state))
   :groups (.size ^ConcurrentHashMap (:groups @router-state))
   :middleware (count (:middleware @router-state))
   :total-requests (.get ^AtomicLong (:request-count @router-state))
   :has-not-found-handler (some? (:not-found-handler @router-state))
   :has-error-handler (some? (:error-handler @router-state))
   :stats (get-router-stats)
   :config (:config @router-state)})
