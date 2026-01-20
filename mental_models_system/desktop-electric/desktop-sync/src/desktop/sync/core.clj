(ns desktop.sync.core
  "Desktop Sync Service - Electric Clojure
   
   State synchronization service for the Mental Models Desktop.
   Handles offline/online state management, data caching, and
   synchronization with backend services."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.tools.logging :as log])
  (:gen-class))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Sync Service"
  (Integer/parseInt (or (System/getenv "PORT") "3002")))

(def api-gateway-url
  "URL for the API Gateway"
  (or (System/getenv "API_GATEWAY_URL") "http://api-gateway:8000"))

(def storage-service-url
  "URL for the Storage Service"
  (or (System/getenv "STORAGE_SERVICE_URL") "http://storage-service:8003"))

(def data-dir
  "Directory for local data storage"
  (or (System/getenv "DATA_DIR") "/data"))

;; ============================================
;; Database Setup
;; ============================================

(def db-spec
  "SQLite database specification"
  {:dbtype "sqlite"
   :dbname (str data-dir "/sync.db")})

(defn init-db!
  "Initialize the SQLite database"
  []
  (try
    (io/make-parents (str data-dir "/sync.db"))
    (let [ds (jdbc/get-datasource db-spec)]
      ;; Create sync queue table
      (jdbc/execute! ds ["CREATE TABLE IF NOT EXISTS sync_queue (
                           id INTEGER PRIMARY KEY AUTOINCREMENT,
                           operation TEXT NOT NULL,
                           resource_type TEXT NOT NULL,
                           resource_id TEXT,
                           data TEXT,
                           priority INTEGER DEFAULT 0,
                           retries INTEGER DEFAULT 0,
                           max_retries INTEGER DEFAULT 5,
                           status TEXT DEFAULT 'pending',
                           created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                           updated_at TEXT DEFAULT CURRENT_TIMESTAMP
                         )"])
      ;; Create cache table
      (jdbc/execute! ds ["CREATE TABLE IF NOT EXISTS cache (
                           key TEXT PRIMARY KEY,
                           value TEXT NOT NULL,
                           ttl_seconds INTEGER DEFAULT 3600,
                           created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                           expires_at TEXT
                         )"])
      ;; Create sync state table
      (jdbc/execute! ds ["CREATE TABLE IF NOT EXISTS sync_state (
                           key TEXT PRIMARY KEY,
                           value TEXT NOT NULL,
                           version INTEGER DEFAULT 1,
                           updated_at TEXT DEFAULT CURRENT_TIMESTAMP
                         )"])
      (log/info "Database initialized successfully")
      ds)
    (catch Exception e
      (log/error "Failed to initialize database:" (.getMessage e))
      nil)))

(def datasource (atom nil))

;; ============================================
;; State Management
;; ============================================

(def sync-state
  "Sync service state atom"
  (atom {:online true
         :last-sync nil
         :pending-count 0
         :sync-in-progress false
         :error-count 0}))

;; ============================================
;; Cache Operations
;; ============================================

(defn cache-get
  "Get a value from cache"
  [key]
  (when @datasource
    (let [result (jdbc/execute-one! @datasource
                                    ["SELECT value, expires_at FROM cache WHERE key = ?" key]
                                    {:builder-fn rs/as-unqualified-maps})]
      (when result
        (let [expires-at (:expires_at result)]
          (if (or (nil? expires-at)
                  (> (compare expires-at (str (java.time.Instant/now))) 0))
            (json/parse-string (:value result) true)
            (do
              (jdbc/execute! @datasource ["DELETE FROM cache WHERE key = ?" key])
              nil)))))))

(defn cache-set
  "Set a value in cache"
  [key value & {:keys [ttl-seconds] :or {ttl-seconds 3600}}]
  (when @datasource
    (let [value-json (json/generate-string value)
          expires-at (str (.plusSeconds (java.time.Instant/now) ttl-seconds))]
      (jdbc/execute! @datasource
                     ["INSERT OR REPLACE INTO cache (key, value, ttl_seconds, expires_at, created_at)
                       VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)"
                      key value-json ttl-seconds expires-at])
      value)))

(defn cache-delete
  "Delete a value from cache"
  [key]
  (when @datasource
    (jdbc/execute! @datasource ["DELETE FROM cache WHERE key = ?" key])
    true))

(defn cache-clear
  "Clear all cache entries"
  []
  (when @datasource
    (jdbc/execute! @datasource ["DELETE FROM cache"])
    true))

;; ============================================
;; Sync Queue Operations
;; ============================================

(defn queue-operation
  "Add an operation to the sync queue"
  [operation resource-type resource-id data & {:keys [priority] :or {priority 0}}]
  (when @datasource
    (jdbc/execute! @datasource
                   ["INSERT INTO sync_queue (operation, resource_type, resource_id, data, priority)
                     VALUES (?, ?, ?, ?, ?)"
                    operation resource-type resource-id (json/generate-string data) priority])
    (swap! sync-state update :pending-count inc)
    {:success true :queued true}))

(defn get-pending-operations
  "Get pending operations from the queue"
  [& {:keys [limit] :or {limit 100}}]
  (when @datasource
    (jdbc/execute! @datasource
                   ["SELECT * FROM sync_queue WHERE status = 'pending' ORDER BY priority DESC, created_at ASC LIMIT ?"
                    limit]
                   {:builder-fn rs/as-unqualified-maps})))

(defn mark-operation-complete
  "Mark an operation as complete"
  [id]
  (when @datasource
    (jdbc/execute! @datasource
                   ["UPDATE sync_queue SET status = 'completed', updated_at = CURRENT_TIMESTAMP WHERE id = ?"
                    id])
    (swap! sync-state update :pending-count dec)))

(defn mark-operation-failed
  "Mark an operation as failed and increment retry count"
  [id]
  (when @datasource
    (let [result (jdbc/execute-one! @datasource
                                    ["SELECT retries, max_retries FROM sync_queue WHERE id = ?" id]
                                    {:builder-fn rs/as-unqualified-maps})]
      (if (and result (< (:retries result) (:max_retries result)))
        (jdbc/execute! @datasource
                       ["UPDATE sync_queue SET retries = retries + 1, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
                        id])
        (jdbc/execute! @datasource
                       ["UPDATE sync_queue SET status = 'failed', updated_at = CURRENT_TIMESTAMP WHERE id = ?"
                        id])))))

;; ============================================
;; Sync State Operations
;; ============================================

(defn get-sync-state
  "Get a sync state value"
  [key]
  (when @datasource
    (let [result (jdbc/execute-one! @datasource
                                    ["SELECT value, version FROM sync_state WHERE key = ?" key]
                                    {:builder-fn rs/as-unqualified-maps})]
      (when result
        {:value (json/parse-string (:value result) true)
         :version (:version result)}))))

(defn set-sync-state
  "Set a sync state value"
  [key value]
  (when @datasource
    (let [value-json (json/generate-string value)]
      (jdbc/execute! @datasource
                     ["INSERT INTO sync_state (key, value, version, updated_at)
                       VALUES (?, ?, 1, CURRENT_TIMESTAMP)
                       ON CONFLICT(key) DO UPDATE SET
                         value = excluded.value,
                         version = sync_state.version + 1,
                         updated_at = CURRENT_TIMESTAMP"
                      key value-json])
      {:success true})))

;; ============================================
;; Sync Operations
;; ============================================

(defn check-online-status
  "Check if we can reach the backend"
  []
  (try
    (let [response (http/get (str api-gateway-url "/health")
                             {:throw-exceptions false
                              :socket-timeout 5000
                              :connection-timeout 3000})]
      (let [online (= 200 (:status response))]
        (swap! sync-state assoc :online online)
        online))
    (catch Exception _
      (swap! sync-state assoc :online false)
      false)))

(defn sync-operation
  "Execute a single sync operation"
  [op]
  (try
    (let [url (case (:resource_type op)
                "analysis" (str api-gateway-url "/api/analysis")
                "model" (str api-gateway-url "/api/models")
                "document" (str storage-service-url "/api/storage/documents")
                (str api-gateway-url "/api/" (:resource_type op)))
          method (case (:operation op)
                   "create" :post
                   "update" :put
                   "delete" :delete
                   :post)
          data (json/parse-string (:data op) true)
          response (case method
                     :post (http/post url {:content-type :json
                                           :body (json/generate-string data)
                                           :throw-exceptions false})
                     :put (http/put (str url "/" (:resource_id op))
                                    {:content-type :json
                                     :body (json/generate-string data)
                                     :throw-exceptions false})
                     :delete (http/delete (str url "/" (:resource_id op))
                                          {:throw-exceptions false}))]
      (if (#{200 201 204} (:status response))
        (do
          (mark-operation-complete (:id op))
          {:success true :operation op})
        (do
          (mark-operation-failed (:id op))
          {:success false :operation op :error (:body response)})))
    (catch Exception e
      (mark-operation-failed (:id op))
      {:success false :operation op :error (.getMessage e)})))

(defn process-sync-queue
  "Process all pending sync operations"
  []
  (when (and (:online @sync-state)
             (not (:sync-in-progress @sync-state)))
    (swap! sync-state assoc :sync-in-progress true)
    (try
      (let [operations (get-pending-operations)
            results (doall (map sync-operation operations))]
        (swap! sync-state assoc
               :last-sync (str (java.time.Instant/now))
               :sync-in-progress false)
        {:success true
         :processed (count results)
         :succeeded (count (filter :success results))
         :failed (count (filter (complement :success) results))})
      (catch Exception e
        (swap! sync-state assoc :sync-in-progress false)
        (swap! sync-state update :error-count inc)
        {:success false :error (.getMessage e)}))))

;; ============================================
;; Background Sync Loop
;; ============================================

(defn start-sync-loop
  "Start the background sync loop"
  []
  (go-loop []
    (when (:online @sync-state)
      (check-online-status)
      (when (:online @sync-state)
        (process-sync-queue)))
    (<! (timeout 30000)) ;; Check every 30 seconds
    (recur)))

(defn start-online-check-loop
  "Start the online status check loop"
  []
  (go-loop []
    (check-online-status)
    (<! (timeout 10000)) ;; Check every 10 seconds
    (recur)))

;; ============================================
;; Route Handlers
;; ============================================

(defn health-handler
  "Health check handler"
  [_]
  (response/response {:status "healthy"
                      :service "desktop-sync"
                      :online (:online @sync-state)}))

(defn status-handler
  "Get sync status"
  [_]
  (response/response @sync-state))

(defn queue-handler
  "Queue a sync operation"
  [request]
  (let [body (:body request)
        {:keys [operation resource_type resource_id data priority]} body]
    (if (or (str/blank? operation) (str/blank? resource_type))
      (-> (response/response {:error "Missing required fields"})
          (response/status 400))
      (response/response (queue-operation operation resource_type resource_id data
                                          :priority (or priority 0))))))

(defn sync-now-handler
  "Trigger immediate sync"
  [_]
  (if (:online @sync-state)
    (response/response (process-sync-queue))
    (-> (response/response {:error "Offline - cannot sync"})
        (response/status 503))))

(defn cache-get-handler
  "Get cached value"
  [request]
  (let [key (get-in request [:params :key])]
    (if (str/blank? key)
      (-> (response/response {:error "No key provided"})
          (response/status 400))
      (if-let [value (cache-get key)]
        (response/response {:key key :value value})
        (-> (response/response {:error "Key not found"})
            (response/status 404))))))

(defn cache-set-handler
  "Set cached value"
  [request]
  (let [body (:body request)
        {:keys [key value ttl]} body]
    (if (or (str/blank? key) (nil? value))
      (-> (response/response {:error "Missing key or value"})
          (response/status 400))
      (do
        (cache-set key value :ttl-seconds (or ttl 3600))
        (response/response {:success true :key key})))))

(defn cache-delete-handler
  "Delete cached value"
  [request]
  (let [key (get-in request [:params :key])]
    (if (str/blank? key)
      (-> (response/response {:error "No key provided"})
          (response/status 400))
      (do
        (cache-delete key)
        (response/response {:success true :key key})))))

(defn state-get-handler
  "Get sync state value"
  [request]
  (let [key (get-in request [:params :key])]
    (if (str/blank? key)
      (-> (response/response {:error "No key provided"})
          (response/status 400))
      (if-let [state (get-sync-state key)]
        (response/response state)
        (-> (response/response {:error "Key not found"})
            (response/status 404))))))

(defn state-set-handler
  "Set sync state value"
  [request]
  (let [body (:body request)
        {:keys [key value]} body]
    (if (or (str/blank? key) (nil? value))
      (-> (response/response {:error "Missing key or value"})
          (response/status 400))
      (response/response (set-sync-state key value)))))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= uri "/health") (health-handler request)
      (= uri "/status") (status-handler request)
      (and (= uri "/queue") (= method :post)) (queue-handler request)
      (and (= uri "/sync") (= method :post)) (sync-now-handler request)
      (and (str/starts-with? uri "/cache/") (= method :get))
      (cache-get-handler (assoc-in request [:params :key] (subs uri 7)))
      (and (= uri "/cache") (= method :post)) (cache-set-handler request)
      (and (str/starts-with? uri "/cache/") (= method :delete))
      (cache-delete-handler (assoc-in request [:params :key] (subs uri 7)))
      (and (str/starts-with? uri "/state/") (= method :get))
      (state-get-handler (assoc-in request [:params :key] (subs uri 7)))
      (and (= uri "/state") (= method :post)) (state-set-handler request)
      :else (-> (response/response {:error "Not Found"})
                (response/status 404)))))

;; ============================================
;; Middleware
;; ============================================

(defn wrap-logging
  "Log requests"
  [handler]
  (fn [request]
    (log/info "Request:" (:request-method request) (:uri request))
    (handler request)))

(defn wrap-cors
  "Add CORS headers"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (response/header "Access-Control-Allow-Origin" "*")
          (response/header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS")
          (response/header "Access-Control-Allow-Headers" "Content-Type, Authorization")))))

(def app
  "Main application handler"
  (-> router
      wrap-logging
      wrap-cors
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-defaults api-defaults)))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the Sync server"
  []
  (log/info "Starting Desktop Sync Service on port" service-port)
  
  ;; Initialize database
  (reset! datasource (init-db!))
  
  ;; Start background loops
  (start-online-check-loop)
  (start-sync-loop)
  
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Desktop Sync Service starting...")
  (start-server)
  (log/info "Desktop Sync ready at http://localhost:" service-port))
