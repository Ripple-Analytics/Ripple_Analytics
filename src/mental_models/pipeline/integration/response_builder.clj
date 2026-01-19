(ns mental-models.pipeline.integration.response-builder
  "Response Builder Module
   
   Fluent response construction:
   - Status code helpers
   - Header management
   - Body serialization
   - Content negotiation
   - Response templates"
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; RESPONSE BUILDER STATE
;; =============================================================================

(defonce builder-state (atom {:templates (ConcurrentHashMap.)
                              :serializers {}
                              :default-headers {}
                              :response-count (AtomicLong. 0)
                              :config {:default-content-type "application/json"
                                       :pretty-print false
                                       :include-timestamp true}}))

;; =============================================================================
;; STATUS CODES
;; =============================================================================

(def status-codes
  {;; 2xx Success
   :ok 200
   :created 201
   :accepted 202
   :no-content 204
   :reset-content 205
   :partial-content 206
   ;; 3xx Redirection
   :moved-permanently 301
   :found 302
   :see-other 303
   :not-modified 304
   :temporary-redirect 307
   :permanent-redirect 308
   ;; 4xx Client Errors
   :bad-request 400
   :unauthorized 401
   :payment-required 402
   :forbidden 403
   :not-found 404
   :method-not-allowed 405
   :not-acceptable 406
   :conflict 409
   :gone 410
   :unprocessable-entity 422
   :too-many-requests 429
   ;; 5xx Server Errors
   :internal-server-error 500
   :not-implemented 501
   :bad-gateway 502
   :service-unavailable 503
   :gateway-timeout 504})

(defn status-code
  "Get numeric status code from keyword or number."
  [status]
  (if (keyword? status)
    (get status-codes status 200)
    status))

(defn status-text
  "Get status text for a status code."
  [code]
  (case (int code)
    200 "OK"
    201 "Created"
    202 "Accepted"
    204 "No Content"
    301 "Moved Permanently"
    302 "Found"
    304 "Not Modified"
    400 "Bad Request"
    401 "Unauthorized"
    403 "Forbidden"
    404 "Not Found"
    405 "Method Not Allowed"
    409 "Conflict"
    422 "Unprocessable Entity"
    429 "Too Many Requests"
    500 "Internal Server Error"
    501 "Not Implemented"
    502 "Bad Gateway"
    503 "Service Unavailable"
    504 "Gateway Timeout"
    "Unknown"))

;; =============================================================================
;; RESPONSE CREATION
;; =============================================================================

(defn response
  "Create a basic response."
  [status body & {:keys [headers content-type]}]
  (.incrementAndGet ^AtomicLong (:response-count @builder-state))
  (metrics/inc-counter! :responsebuilder/responses-created)
  (let [code (status-code status)
        ct (or content-type (get-in @builder-state [:config :default-content-type]))]
    (cond-> {:status code
             :headers (merge (:default-headers @builder-state)
                             {"Content-Type" ct}
                             headers)
             :body body}
      (get-in @builder-state [:config :include-timestamp])
      (assoc :timestamp (System/currentTimeMillis)))))

;; =============================================================================
;; SUCCESS RESPONSES
;; =============================================================================

(defn ok
  "Create a 200 OK response."
  [body & opts]
  (apply response :ok body opts))

(defn created
  "Create a 201 Created response."
  [body & {:keys [location] :as opts}]
  (let [headers (when location {"Location" location})]
    (apply response :created body :headers headers (mapcat identity (dissoc opts :location)))))

(defn accepted
  "Create a 202 Accepted response."
  [body & opts]
  (apply response :accepted body opts))

(defn no-content
  "Create a 204 No Content response."
  []
  (response :no-content nil))

;; =============================================================================
;; REDIRECT RESPONSES
;; =============================================================================

(defn redirect
  "Create a redirect response."
  [url & {:keys [permanent]}]
  (response (if permanent :moved-permanently :found) nil
            :headers {"Location" url}))

(defn redirect-see-other
  "Create a 303 See Other redirect."
  [url]
  (response :see-other nil :headers {"Location" url}))

(defn not-modified
  "Create a 304 Not Modified response."
  []
  (response :not-modified nil))

;; =============================================================================
;; CLIENT ERROR RESPONSES
;; =============================================================================

(defn bad-request
  "Create a 400 Bad Request response."
  [& {:keys [message errors]}]
  (response :bad-request
            {:error "Bad Request"
             :message (or message "Invalid request")
             :errors errors}))

(defn unauthorized
  "Create a 401 Unauthorized response."
  [& {:keys [message realm]}]
  (response :unauthorized
            {:error "Unauthorized"
             :message (or message "Authentication required")}
            :headers (when realm {"WWW-Authenticate" (str "Bearer realm=\"" realm "\"")})))

(defn forbidden
  "Create a 403 Forbidden response."
  [& {:keys [message]}]
  (response :forbidden
            {:error "Forbidden"
             :message (or message "Access denied")}))

(defn not-found
  "Create a 404 Not Found response."
  [& {:keys [message resource]}]
  (response :not-found
            {:error "Not Found"
             :message (or message "Resource not found")
             :resource resource}))

(defn method-not-allowed
  "Create a 405 Method Not Allowed response."
  [allowed-methods]
  (response :method-not-allowed
            {:error "Method Not Allowed"
             :allowed-methods allowed-methods}
            :headers {"Allow" (str/join ", " (map str/upper-case (map name allowed-methods)))}))

(defn conflict
  "Create a 409 Conflict response."
  [& {:keys [message]}]
  (response :conflict
            {:error "Conflict"
             :message (or message "Resource conflict")}))

(defn unprocessable-entity
  "Create a 422 Unprocessable Entity response."
  [errors & {:keys [message]}]
  (response :unprocessable-entity
            {:error "Unprocessable Entity"
             :message (or message "Validation failed")
             :errors errors}))

(defn too-many-requests
  "Create a 429 Too Many Requests response."
  [& {:keys [retry-after message]}]
  (response :too-many-requests
            {:error "Too Many Requests"
             :message (or message "Rate limit exceeded")
             :retry-after retry-after}
            :headers (when retry-after {"Retry-After" (str retry-after)})))

;; =============================================================================
;; SERVER ERROR RESPONSES
;; =============================================================================

(defn internal-server-error
  "Create a 500 Internal Server Error response."
  [& {:keys [message error-id]}]
  (response :internal-server-error
            {:error "Internal Server Error"
             :message (or message "An unexpected error occurred")
             :error-id error-id}))

(defn not-implemented
  "Create a 501 Not Implemented response."
  [& {:keys [message]}]
  (response :not-implemented
            {:error "Not Implemented"
             :message (or message "Feature not implemented")}))

(defn service-unavailable
  "Create a 503 Service Unavailable response."
  [& {:keys [retry-after message]}]
  (response :service-unavailable
            {:error "Service Unavailable"
             :message (or message "Service temporarily unavailable")
             :retry-after retry-after}
            :headers (when retry-after {"Retry-After" (str retry-after)})))

(defn gateway-timeout
  "Create a 504 Gateway Timeout response."
  [& {:keys [message]}]
  (response :gateway-timeout
            {:error "Gateway Timeout"
             :message (or message "Upstream service timeout")}))

;; =============================================================================
;; BODY SERIALIZATION
;; =============================================================================

(defn register-serializer!
  "Register a body serializer for a content type."
  [content-type serialize-fn]
  (swap! builder-state assoc-in [:serializers content-type] serialize-fn))

(defn get-serializer
  "Get a serializer for a content type."
  [content-type]
  (get-in @builder-state [:serializers content-type]))

(defn serialize-body
  "Serialize a body based on content type."
  [body content-type]
  (if-let [serializer (get-serializer content-type)]
    (serializer body)
    (cond
      (str/includes? content-type "json") (json/write-str body)
      (str/includes? content-type "edn") (pr-str body)
      (string? body) body
      :else (str body))))

(defn with-serialized-body
  "Serialize the response body."
  [response]
  (let [content-type (get-in response [:headers "Content-Type"] "application/json")]
    (update response :body serialize-body content-type)))

;; =============================================================================
;; HEADER HELPERS
;; =============================================================================

(defn set-default-headers!
  "Set default headers for all responses."
  [headers]
  (swap! builder-state assoc :default-headers headers))

(defn add-default-header!
  "Add a default header."
  [name value]
  (swap! builder-state assoc-in [:default-headers name] value))

(defn with-header
  "Add a header to a response."
  [response name value]
  (assoc-in response [:headers name] value))

(defn with-headers
  "Add multiple headers to a response."
  [response headers]
  (update response :headers merge headers))

(defn with-content-type
  "Set the content type of a response."
  [response content-type]
  (with-header response "Content-Type" content-type))

(defn with-cache-control
  "Set cache control header."
  [response & {:keys [max-age no-cache no-store private public]}]
  (let [directives (cond-> []
                     max-age (conj (str "max-age=" max-age))
                     no-cache (conj "no-cache")
                     no-store (conj "no-store")
                     private (conj "private")
                     public (conj "public"))]
    (with-header response "Cache-Control" (str/join ", " directives))))

(defn with-etag
  "Set ETag header."
  [response etag]
  (with-header response "ETag" (str "\"" etag "\"")))

(defn with-last-modified
  "Set Last-Modified header."
  [response timestamp]
  (with-header response "Last-Modified" 
               (if (number? timestamp)
                 (str (java.util.Date. timestamp))
                 (str timestamp))))

;; =============================================================================
;; RESPONSE TEMPLATES
;; =============================================================================

(defn register-template!
  "Register a response template."
  [template-id template-fn]
  (log/info "Registering response template" {:id template-id})
  (.put ^ConcurrentHashMap (:templates @builder-state) template-id template-fn))

(defn unregister-template!
  "Unregister a response template."
  [template-id]
  (.remove ^ConcurrentHashMap (:templates @builder-state) template-id))

(defn get-template
  "Get a response template."
  [template-id]
  (.get ^ConcurrentHashMap (:templates @builder-state) template-id))

(defn from-template
  "Create a response from a template."
  [template-id & args]
  (if-let [template-fn (get-template template-id)]
    (apply template-fn args)
    (internal-server-error :message (str "Template not found: " template-id))))

;; =============================================================================
;; CONTENT NEGOTIATION
;; =============================================================================

(defn parse-accept-header
  "Parse an Accept header into a list of media types with quality."
  [accept-header]
  (when accept-header
    (->> (str/split accept-header #",")
         (map str/trim)
         (map (fn [part]
                (let [[media-type & params] (str/split part #";")
                      quality (or (some->> params
                                           (filter #(str/starts-with? % "q="))
                                           first
                                           (re-find #"q=([0-9.]+)")
                                           second
                                           Double/parseDouble)
                                  1.0)]
                  {:media-type (str/trim media-type)
                   :quality quality})))
         (sort-by :quality >))))

(defn negotiate-content-type
  "Negotiate content type based on Accept header."
  [accept-header available-types]
  (let [accepted (parse-accept-header accept-header)]
    (or (first (for [{:keys [media-type]} accepted
                     :when (or (= media-type "*/*")
                               (contains? (set available-types) media-type))]
                 (if (= media-type "*/*")
                   (first available-types)
                   media-type)))
        (first available-types))))

;; =============================================================================
;; PAGINATION HELPERS
;; =============================================================================

(defn paginated
  "Create a paginated response."
  [items & {:keys [page page-size total]}]
  (let [total-items (or total (count items))
        total-pages (when (and total page-size) (int (Math/ceil (/ total page-size))))]
    (ok {:data items
         :pagination {:page page
                      :page-size page-size
                      :total-items total-items
                      :total-pages total-pages}})))

(defn with-pagination-links
  "Add pagination links to a response."
  [response base-url page total-pages]
  (let [links (cond-> {}
                (> page 1) (assoc :prev (str base-url "?page=" (dec page)))
                (< page total-pages) (assoc :next (str base-url "?page=" (inc page)))
                true (assoc :first (str base-url "?page=1"))
                true (assoc :last (str base-url "?page=" total-pages)))]
    (assoc-in response [:body :links] links)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-builder-stats
  "Get response builder statistics."
  []
  {:total-responses (.get ^AtomicLong (:response-count @builder-state))
   :templates (.size ^ConcurrentHashMap (:templates @builder-state))
   :serializers (count (:serializers @builder-state))
   :default-headers (count (:default-headers @builder-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-response-builder!
  "Initialize response builder."
  []
  (log/info "Initializing response builder")
  ;; Register feature flag
  (flags/register-flag! "response-builder" "Enable response builder" true)
  ;; Create metrics
  (metrics/create-counter! :responsebuilder/responses-created "Responses created")
  (metrics/create-gauge! :responsebuilder/total-responses "Total responses"
                         #(.get ^AtomicLong (:response-count @builder-state)))
  ;; Register default serializers
  (register-serializer! "application/json" json/write-str)
  (register-serializer! "application/edn" pr-str)
  (register-serializer! "text/plain" str)
  ;; Register common templates
  (register-template! :success (fn [data] (ok {:success true :data data})))
  (register-template! :error (fn [message] (bad-request :message message)))
  (register-template! :list (fn [items] (ok {:items items :count (count items)})))
  (log/info "Response builder initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-response-builder-status []
  {:enabled (flags/is-enabled? "response-builder")
   :templates (.size ^ConcurrentHashMap (:templates @builder-state))
   :serializers (count (:serializers @builder-state))
   :default-headers (count (:default-headers @builder-state))
   :total-responses (.get ^AtomicLong (:response-count @builder-state))
   :stats (get-builder-stats)
   :config (:config @builder-state)})
