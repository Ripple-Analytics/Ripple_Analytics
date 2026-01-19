(ns mental-models.desktop.api.web-client
  "API client for communicating with Mental Models web app.
   Provides functions to fetch and sync data with the web app backend.
   Includes auth token storage, rate limiting with exponential backoff,
   and offline queue for failed API calls."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader OutputStreamWriter File]
           [java.util.concurrent Executors TimeUnit]
           [java.time Instant Duration]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *web-app-url* "https://mental-models.manus.space")
(def ^:dynamic *api-key* nil)
(def ^:dynamic *auth-token* nil)

;; Auth token storage path
(def auth-storage-path (str (System/getProperty "user.home") "/.mental-models/auth.json"))

;; Rate limiting state
(def rate-limit-state (atom {:last-request-time nil
                             :request-count 0
                             :backoff-until nil
                             :consecutive-failures 0}))

;; Offline queue for failed API calls
(def offline-queue (atom []))

;; Online status
(def online-status (atom {:online true
                          :last-check nil
                          :last-successful-request nil}))

;; Background executor for queue processing
(def queue-executor (Executors/newSingleThreadScheduledExecutor))

(defn set-web-app-url! [url]
  (alter-var-root #'*web-app-url* (constantly url)))

(defn set-api-key! [key]
  (alter-var-root #'*api-key* (constantly key)))

(defn set-auth-token! [token]
  (alter-var-root #'*auth-token* (constantly token)))

;; =============================================================================
;; Auth Token Storage
;; =============================================================================

(defn- ensure-auth-dir! []
  "Ensure the auth storage directory exists"
  (let [dir (io/file (str (System/getProperty "user.home") "/.mental-models"))]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn save-auth-token!
  "Save auth token to local storage"
  [token & {:keys [refresh-token expires-at]}]
  (ensure-auth-dir!)
  (let [auth-data {:token token
                   :refresh-token refresh-token
                   :expires-at expires-at
                   :saved-at (str (Instant/now))}]
    (spit auth-storage-path (json/write-str auth-data))
    (set-auth-token! token)
    auth-data))

(defn load-auth-token!
  "Load auth token from local storage"
  []
  (try
    (when (.exists (io/file auth-storage-path))
      (let [auth-data (json/read-str (slurp auth-storage-path) :key-fn keyword)]
        (when-let [token (:token auth-data)]
          (set-auth-token! token)
          auth-data)))
    (catch Exception e
      (println "Warning: Failed to load auth token:" (.getMessage e))
      nil)))

(defn clear-auth-token!
  "Clear stored auth token"
  []
  (set-auth-token! nil)
  (when (.exists (io/file auth-storage-path))
    (.delete (io/file auth-storage-path))))

(defn token-expired?
  "Check if the stored token has expired"
  [auth-data]
  (when-let [expires-at (:expires-at auth-data)]
    (try
      (let [expiry (Instant/parse expires-at)]
        (.isBefore expiry (Instant/now)))
      (catch Exception _ false))))

;; =============================================================================
;; Rate Limiting with Exponential Backoff
;; =============================================================================

(def max-requests-per-minute 60)
(def base-backoff-ms 1000)
(def max-backoff-ms 60000)
(def max-retries 5)

(defn- calculate-backoff
  "Calculate exponential backoff delay based on consecutive failures"
  [failures]
  (min max-backoff-ms
       (* base-backoff-ms (Math/pow 2 (min failures 6)))))

(defn- should-throttle?
  "Check if we should throttle requests due to rate limiting"
  []
  (let [{:keys [backoff-until request-count last-request-time]} @rate-limit-state
        now (Instant/now)]
    (cond
      ;; Check if we're in backoff period
      (and backoff-until (.isAfter (Instant/parse backoff-until) now))
      {:throttle true :reason :backoff :wait-ms (.toMillis (Duration/between now (Instant/parse backoff-until)))}
      
      ;; Check rate limit (requests per minute)
      (and last-request-time
           (< (.toMillis (Duration/between (Instant/parse last-request-time) now)) 60000)
           (>= request-count max-requests-per-minute))
      {:throttle true :reason :rate-limit :wait-ms (- 60000 (.toMillis (Duration/between (Instant/parse last-request-time) now)))}
      
      :else
      {:throttle false})))

(defn- update-rate-limit-success!
  "Update rate limit state after successful request"
  []
  (swap! rate-limit-state
         (fn [state]
           (let [now (str (Instant/now))]
             (-> state
                 (assoc :last-request-time now)
                 (assoc :consecutive-failures 0)
                 (assoc :backoff-until nil)
                 (update :request-count
                         (fn [cnt]
                           (if (and (:last-request-time state)
                                    (< (.toMillis (Duration/between 
                                                   (Instant/parse (:last-request-time state))
                                                   (Instant/now))) 60000))
                             (inc (or cnt 0))
                             1))))))))

(defn- update-rate-limit-failure!
  "Update rate limit state after failed request"
  [status]
  (swap! rate-limit-state
         (fn [state]
           (let [failures (inc (or (:consecutive-failures state) 0))
                 backoff-ms (calculate-backoff failures)
                 backoff-until (str (.plus (Instant/now) (Duration/ofMillis backoff-ms)))]
             (-> state
                 (assoc :consecutive-failures failures)
                 (assoc :backoff-until (when (or (= status 429) (>= failures 3)) backoff-until)))))))

;; =============================================================================
;; Offline Queue
;; =============================================================================

(def queue-storage-path (str (System/getProperty "user.home") "/.mental-models/offline-queue.json"))

(defn- save-queue-to-disk!
  "Persist offline queue to disk"
  []
  (ensure-auth-dir!)
  (try
    (spit queue-storage-path (json/write-str @offline-queue))
    (catch Exception e
      (println "Warning: Failed to save offline queue:" (.getMessage e)))))

(defn- load-queue-from-disk!
  "Load offline queue from disk"
  []
  (try
    (when (.exists (io/file queue-storage-path))
      (let [queue (json/read-str (slurp queue-storage-path) :key-fn keyword)]
        (reset! offline-queue (vec queue))))
    (catch Exception e
      (println "Warning: Failed to load offline queue:" (.getMessage e)))))

(defn queue-request!
  "Add a failed request to the offline queue"
  [{:keys [method endpoint body priority] :or {priority :normal}}]
  (let [request {:id (str (java.util.UUID/randomUUID))
                 :method method
                 :endpoint endpoint
                 :body body
                 :priority priority
                 :queued-at (str (Instant/now))
                 :retry-count 0}]
    (swap! offline-queue
           (fn [q]
             (let [new-q (conj q request)]
               ;; Sort by priority (high first) then by queued-at (newest first for same priority)
               (vec (sort-by (juxt #(case (:priority %) :high 0 :normal 1 :low 2)
                                   #(- (.toEpochMilli (Instant/parse (:queued-at %)))))
                             new-q)))))
    (save-queue-to-disk!)
    request))

(defn get-pending-queue
  "Get all pending requests in the queue"
  []
  @offline-queue)

(defn get-pending-count
  "Get count of pending requests"
  []
  (count @offline-queue))

(defn clear-queue!
  "Clear all pending requests from the queue"
  []
  (reset! offline-queue [])
  (save-queue-to-disk!))

(defn remove-from-queue!
  "Remove a specific request from the queue by ID"
  [request-id]
  (swap! offline-queue (fn [q] (vec (remove #(= (:id %) request-id) q))))
  (save-queue-to-disk!))

;; =============================================================================
;; Online/Offline Detection
;; =============================================================================

(defn check-online-status
  "Check if we can reach the web app"
  []
  (try
    (let [url (URL. (str *web-app-url* "/api/health"))
          conn (doto (.openConnection url)
                 (.setRequestMethod "HEAD")
                 (.setConnectTimeout 5000)
                 (.setReadTimeout 5000))]
      (let [status (.getResponseCode conn)
            online? (< status 500)]
        (swap! online-status assoc
               :online online?
               :last-check (str (Instant/now)))
        online?))
    (catch Exception _
      (swap! online-status assoc
             :online false
             :last-check (str (Instant/now)))
      false)))

(defn is-online?
  "Return current online status"
  []
  (:online @online-status))

(defn get-online-status
  "Get detailed online status"
  []
  @online-status)

;; =============================================================================
;; HTTP Utilities
;; =============================================================================

(defn- make-request-internal
  "Internal HTTP request function without rate limiting"
  [{:keys [method endpoint body headers timeout]
    :or {method "GET" headers {} timeout 30000}}]
  (try
    (let [url (URL. (str *web-app-url* endpoint))
          conn (doto (.openConnection url)
                 (.setRequestMethod method)
                 (.setConnectTimeout timeout)
                 (.setReadTimeout timeout)
                 (.setDoInput true))]
      
      ;; Set headers
      (doseq [[k v] (merge {"Content-Type" "application/json"
                           "Accept" "application/json"
                           "X-Desktop-API-Key" (or *api-key* "mm-desktop-2026-ripple")}
                          (when *auth-token*
                            {"Authorization" (str "Bearer " *auth-token*)})
                          headers)]
        (.setRequestProperty conn k v))
      
      ;; Send body if present
      (when body
        (.setDoOutput conn true)
        (with-open [writer (OutputStreamWriter. (.getOutputStream conn))]
          (.write writer (if (string? body) body (json/write-str body)))
          (.flush writer)))
      
      ;; Read response
      (let [status (.getResponseCode conn)
            stream (if (>= status 400)
                     (.getErrorStream conn)
                     (.getInputStream conn))
            response (when stream
                       (with-open [reader (BufferedReader. (InputStreamReader. stream))]
                         (slurp reader)))]
        {:status status
         :body (when (and response (not (str/blank? response)))
                 (try (json/read-str response :key-fn keyword)
                      (catch Exception _ response)))
         :success? (< status 400)}))
    (catch Exception e
      {:status 0
       :error (.getMessage e)
       :success? false})))

(defn- make-request
  "Make HTTP request to web app API with rate limiting and offline queue support"
  [{:keys [method endpoint body headers timeout queue-on-failure priority]
    :or {method "GET" headers {} timeout 30000 queue-on-failure true priority :normal}
    :as request-opts}]
  ;; Check if we should throttle
  (let [throttle-check (should-throttle?)]
    (if (:throttle throttle-check)
      ;; We're being throttled - queue the request if it's a mutation
      (if (and queue-on-failure (not= method "GET"))
        (do
          (queue-request! {:method method :endpoint endpoint :body body :priority priority})
          {:status 0
           :error (str "Request queued due to " (name (:reason throttle-check)))
           :queued true
           :success? false})
        {:status 0
         :error (str "Rate limited, retry after " (:wait-ms throttle-check) "ms")
         :success? false})
      ;; Not throttled - make the request
      (let [response (make-request-internal request-opts)]
        (if (:success? response)
          (do
            (update-rate-limit-success!)
            (swap! online-status assoc
                   :online true
                   :last-successful-request (str (Instant/now)))
            response)
          ;; Handle failure
          (do
            (update-rate-limit-failure! (:status response))
            ;; Queue mutation requests on failure if enabled
            (when (and queue-on-failure
                       (not= method "GET")
                       (or (zero? (:status response))  ;; Network error
                           (>= (:status response) 500))) ;; Server error
              (queue-request! {:method method :endpoint endpoint :body body :priority priority}))
            ;; Update online status on network errors
            (when (zero? (:status response))
              (swap! online-status assoc :online false))
            response))))))

(defn- make-request-with-retry
  "Make HTTP request with automatic retry and exponential backoff"
  [{:keys [max-retries] :or {max-retries 3} :as request-opts}]
  (loop [attempt 0]
    (let [response (make-request request-opts)]
      (if (or (:success? response)
              (:queued response)
              (>= attempt max-retries)
              ;; Don't retry client errors (4xx except 429)
              (and (>= (:status response) 400)
                   (< (:status response) 500)
                   (not= (:status response) 429)))
        response
        (do
          (Thread/sleep (calculate-backoff attempt))
          (recur (inc attempt)))))))

;; =============================================================================
;; Queue Processing
;; =============================================================================

(defn process-queue!
  "Process pending requests in the offline queue"
  []
  (when (and (is-online?) (pos? (get-pending-count)))
    (doseq [request @offline-queue]
      (when (< (:retry-count request) max-retries)
        (let [response (make-request-internal {:method (:method request)
                                               :endpoint (:endpoint request)
                                               :body (:body request)})]
          (if (:success? response)
            (remove-from-queue! (:id request))
            ;; Increment retry count
            (swap! offline-queue
                   (fn [q]
                     (vec (map #(if (= (:id %) (:id request))
                                  (update % :retry-count inc)
                                  %)
                               q))))))))
    (save-queue-to-disk!)))

(defn start-queue-processor!
  "Start background queue processor that runs every 30 seconds"
  []
  (.scheduleAtFixedRate queue-executor
                        (fn []
                          (try
                            (when (check-online-status)
                              (process-queue!))
                            (catch Exception e
                              (println "Queue processor error:" (.getMessage e)))))
                        30 30 TimeUnit/SECONDS))

(defn stop-queue-processor!
  "Stop the background queue processor"
  []
  (.shutdown queue-executor))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize the web client - load auth token and offline queue"
  []
  (load-auth-token!)
  (load-queue-from-disk!)
  (check-online-status)
  (start-queue-processor!))

(defn- trpc-query
  "Make tRPC query request"
  [procedure & [input]]
  (let [endpoint (str "/api/trpc/" procedure
                      (when input
                        (str "?input=" (java.net.URLEncoder/encode 
                                        (json/write-str input) "UTF-8"))))]
    (make-request {:method "GET" :endpoint endpoint})))

(defn- trpc-mutation
  "Make tRPC mutation request"
  [procedure input]
  (make-request {:method "POST"
                 :endpoint (str "/api/trpc/" procedure)
                 :body input}))

;; =============================================================================
;; Case Studies API
;; =============================================================================

(defn get-case-studies
  "Fetch all case studies from web app"
  []
  (let [response (trpc-query "caseStudies.list")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-case-study
  "Fetch a single case study by ID"
  [id]
  (let [response (trpc-query "caseStudies.getById" {:id id})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-case-study-by-slug
  "Fetch a case study by slug"
  [slug]
  (let [response (trpc-query "caseStudies.getBySlug" {:slug slug})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Signals API
;; =============================================================================

(defn get-signals
  "Fetch signals for current user"
  [& {:keys [limit] :or {limit 100}}]
  (let [response (trpc-query "signals.list" {:limit limit})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-unread-signals
  "Fetch unread signals count"
  []
  (let [response (trpc-query "signals.unread")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn mark-signal-read
  "Mark a signal as read"
  [signal-id]
  (let [response (trpc-mutation "signals.markAsRead" {:id signal-id})]
    (:success? response)))

(defn create-signal
  "Create a new signal"
  [signal-data]
  (let [response (trpc-mutation "signals.create" signal-data)]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Model Effectiveness API
;; =============================================================================

(defn get-effectiveness-stats
  "Fetch model effectiveness statistics"
  []
  (let [response (trpc-query "effectiveness.stats")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-top-combinations
  "Fetch top model combinations"
  [& {:keys [limit] :or {limit 10}}]
  (let [response (trpc-query "effectiveness.topCombinations" {:limit limit})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn record-effectiveness
  "Record model effectiveness data"
  [effectiveness-data]
  (let [response (trpc-mutation "effectiveness.record" effectiveness-data)]
    (:success? response)))

;; =============================================================================
;; Knowledge Graph API
;; =============================================================================

(defn get-knowledge-graph
  "Fetch user's knowledge graph"
  []
  (let [response (trpc-query "knowledgeGraph.get")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn create-knowledge-node
  "Create a knowledge graph node"
  [node-data]
  (let [response (trpc-mutation "knowledgeGraph.createNode" node-data)]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn create-knowledge-edge
  "Create a knowledge graph edge"
  [edge-data]
  (let [response (trpc-mutation "knowledgeGraph.createEdge" edge-data)]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Decisions API
;; =============================================================================

(defn get-decisions
  "Fetch user's decisions"
  []
  (let [response (trpc-query "decisions.list")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn create-decision
  "Create a new decision"
  [decision-data]
  (let [response (trpc-mutation "decisions.create" decision-data)]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn update-decision
  "Update an existing decision"
  [id decision-data]
  (let [response (trpc-mutation "decisions.update" (assoc decision-data :id id))]
    (:success? response)))

;; =============================================================================
;; Mental Models API
;; =============================================================================

(defn get-models
  "Fetch all mental models"
  []
  (let [response (trpc-query "models.list")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-model-by-slug
  "Fetch a model by slug"
  [slug]
  (let [response (trpc-query "models.getBySlug" {:slug slug})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn search-models
  "Search mental models"
  [query]
  (let [response (trpc-query "models.search" {:query query})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Categories API
;; =============================================================================

(defn get-categories
  "Fetch all categories"
  []
  (let [response (trpc-query "categories.list")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Text Analysis API
;; =============================================================================

(defn analyze-text
  "Analyze text for mental models"
  [text]
  (let [response (trpc-mutation "textAnalysis.analyze" {:text text})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn detect-lollapalooza
  "Detect lollapalooza effects in text"
  [text]
  (let [response (trpc-mutation "textAnalysis.detectLollapalooza" {:text text})]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Sync API
;; =============================================================================

(defn sync-scan-results
  "Sync scan results to web app"
  [results]
  (let [response (trpc-mutation "desktop.syncResults" {:results results})]
    (:success? response)))

(defn get-sync-status
  "Get sync status"
  []
  (let [response (trpc-query "desktop.syncStatus")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Lollapalooza Events API
;; =============================================================================

(defn get-lollapalooza-events
  "Fetch lollapalooza events"
  []
  (let [response (trpc-query "lollapalooza.list")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

(defn get-recent-lollapaloozas
  "Fetch recent lollapalooza events"
  []
  (let [response (trpc-query "lollapalooza.recent")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Dashboard API
;; =============================================================================

(defn get-dashboard-stats
  "Fetch dashboard statistics"
  []
  (let [response (trpc-query "dashboard.stats")]
    (when (:success? response)
      (get-in response [:body :result :data]))))

;; =============================================================================
;; Connection Test
;; =============================================================================

(defn test-connection
  "Test connection to web app"
  []
  (let [response (make-request {:method "GET" :endpoint "/api/health" :timeout 5000})]
    {:connected? (:success? response)
     :status (:status response)
     :error (:error response)}))

(defn check-auth
  "Check if current auth token is valid"
  []
  (let [response (trpc-query "auth.me")]
    {:authenticated? (:success? response)
     :user (get-in response [:body :result :data])}))
