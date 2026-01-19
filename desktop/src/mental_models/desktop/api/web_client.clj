(ns mental-models.desktop.api.web-client
  "API client for communicating with Mental Models web app.
   Provides functions to fetch and sync data with the web app backend."
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.net HttpURLConnection URL]
           [java.io BufferedReader InputStreamReader OutputStreamWriter]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:dynamic *web-app-url* "https://mental-models.manus.space")
(def ^:dynamic *api-key* nil)
(def ^:dynamic *auth-token* nil)

(defn set-web-app-url! [url]
  (alter-var-root #'*web-app-url* (constantly url)))

(defn set-api-key! [key]
  (alter-var-root #'*api-key* (constantly key)))

(defn set-auth-token! [token]
  (alter-var-root #'*auth-token* (constantly token)))

;; =============================================================================
;; HTTP Utilities
;; =============================================================================

(defn- make-request
  "Make HTTP request to web app API"
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
