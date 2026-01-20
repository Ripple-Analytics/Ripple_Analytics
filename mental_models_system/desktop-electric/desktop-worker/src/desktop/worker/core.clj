(ns desktop.worker.core
  "Desktop Worker Service - Electric Clojure
   
   Background processing service for the Mental Models Desktop.
   Handles file watching, scheduled tasks, and offline queue processing."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:gen-class))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Worker Service"
  (Integer/parseInt (or (System/getenv "PORT") "3001")))

(def api-gateway-url
  "URL for the API Gateway"
  (or (System/getenv "API_GATEWAY_URL") "http://api-gateway:8000"))

(def harvester-service-url
  "URL for the Harvester Service"
  (or (System/getenv "HARVESTER_SERVICE_URL") "http://harvester-service:8002"))

;; ============================================
;; State Management
;; ============================================

(def worker-state
  "Worker state atom"
  (atom {:running true
         :watched-dirs #{}
         :file-queue (async/chan 1000)
         :url-queue (async/chan 1000)
         :processed-count 0
         :error-count 0
         :last-activity nil
         :scheduled-tasks []}))

;; ============================================
;; File Watching
;; ============================================

(defn process-file
  "Process a file for mental model analysis"
  [file-path]
  (try
    (log/info "Processing file:" file-path)
    (let [content (slurp file-path)
          response (http/post (str harvester-service-url "/api/harvester/process")
                              {:content-type :json
                               :body (json/generate-string {:type "file"
                                                            :path file-path
                                                            :content content})
                               :throw-exceptions false})]
      (if (= 200 (:status response))
        (do
          (swap! worker-state update :processed-count inc)
          (swap! worker-state assoc :last-activity (java.time.Instant/now))
          {:success true :file file-path})
        (do
          (swap! worker-state update :error-count inc)
          {:success false :file file-path :error (:body response)})))
    (catch Exception e
      (swap! worker-state update :error-count inc)
      {:success false :file file-path :error (.getMessage e)})))

(defn file-watcher-handler
  "Handle file system events"
  [ctx event]
  (let [file (:file event)
        kind (:kind event)]
    (when (and file (#{:create :modify} kind))
      (let [path (.getAbsolutePath file)]
        (when (and (not (.isDirectory file))
                   (re-matches #".*\.(txt|md|clj|cljs|edn|json)$" path))
          (log/info "File event:" kind path)
          (async/put! (:file-queue @worker-state) path))))))

(defn start-file-watcher
  "Start watching a directory for file changes"
  [dir-path]
  (try
    (log/info "Starting file watcher for:" dir-path)
    (swap! worker-state update :watched-dirs conj dir-path)
    {:success true :directory dir-path}
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn stop-file-watcher
  "Stop watching a directory"
  [dir-path]
  (swap! worker-state update :watched-dirs disj dir-path)
  {:success true :directory dir-path})

;; ============================================
;; URL Scraping Queue
;; ============================================

(defn process-url
  "Process a URL for scraping"
  [url]
  (try
    (log/info "Processing URL:" url)
    (let [response (http/post (str harvester-service-url "/api/harvester/scrape")
                              {:content-type :json
                               :body (json/generate-string {:url url})
                               :throw-exceptions false})]
      (if (= 200 (:status response))
        (do
          (swap! worker-state update :processed-count inc)
          (swap! worker-state assoc :last-activity (java.time.Instant/now))
          {:success true :url url})
        (do
          (swap! worker-state update :error-count inc)
          {:success false :url url :error (:body response)})))
    (catch Exception e
      (swap! worker-state update :error-count inc)
      {:success false :url url :error (.getMessage e)})))

(defn queue-url
  "Add a URL to the scraping queue"
  [url]
  (async/put! (:url-queue @worker-state) url)
  {:success true :url url :queued true})

;; ============================================
;; Background Processing Loop
;; ============================================

(defn start-file-processor
  "Start the background file processing loop"
  []
  (go-loop []
    (when (:running @worker-state)
      (when-let [file-path (<! (:file-queue @worker-state))]
        (process-file file-path))
      (<! (timeout 100))
      (recur))))

(defn start-url-processor
  "Start the background URL processing loop"
  []
  (go-loop []
    (when (:running @worker-state)
      (when-let [url (<! (:url-queue @worker-state))]
        (process-url url))
      (<! (timeout 100))
      (recur))))

;; ============================================
;; Scheduled Tasks
;; ============================================

(defn schedule-task
  "Schedule a recurring task"
  [task-id interval-ms task-fn]
  (let [task {:id task-id
              :interval interval-ms
              :running (atom true)}]
    (swap! worker-state update :scheduled-tasks conj task)
    (go-loop []
      (when @(:running task)
        (try
          (task-fn)
          (catch Exception e
            (log/error "Scheduled task error:" task-id (.getMessage e))))
        (<! (timeout interval-ms))
        (recur)))
    task))

(defn cancel-task
  "Cancel a scheduled task"
  [task-id]
  (let [tasks (:scheduled-tasks @worker-state)
        task (first (filter #(= task-id (:id %)) tasks))]
    (when task
      (reset! (:running task) false)
      (swap! worker-state update :scheduled-tasks
             (fn [ts] (remove #(= task-id (:id %)) ts))))
    {:success true :task-id task-id}))

;; ============================================
;; Route Handlers
;; ============================================

(defn health-handler
  "Health check handler"
  [_]
  (response/response {:status "healthy"
                      :service "desktop-worker"
                      :stats {:processed (:processed-count @worker-state)
                              :errors (:error-count @worker-state)
                              :watched-dirs (count (:watched-dirs @worker-state))}}))

(defn status-handler
  "Get worker status"
  [_]
  (response/response {:running (:running @worker-state)
                      :watched-dirs (vec (:watched-dirs @worker-state))
                      :processed-count (:processed-count @worker-state)
                      :error-count (:error-count @worker-state)
                      :last-activity (str (:last-activity @worker-state))
                      :scheduled-tasks (count (:scheduled-tasks @worker-state))}))

(defn watch-handler
  "Add a directory to watch"
  [request]
  (let [body (:body request)
        dir-path (:directory body)]
    (if (str/blank? dir-path)
      (-> (response/response {:error "No directory provided"})
          (response/status 400))
      (response/response (start-file-watcher dir-path)))))

(defn unwatch-handler
  "Remove a directory from watch"
  [request]
  (let [body (:body request)
        dir-path (:directory body)]
    (if (str/blank? dir-path)
      (-> (response/response {:error "No directory provided"})
          (response/status 400))
      (response/response (stop-file-watcher dir-path)))))

(defn queue-file-handler
  "Queue a file for processing"
  [request]
  (let [body (:body request)
        file-path (:file body)]
    (if (str/blank? file-path)
      (-> (response/response {:error "No file provided"})
          (response/status 400))
      (do
        (async/put! (:file-queue @worker-state) file-path)
        (response/response {:success true :file file-path :queued true})))))

(defn queue-url-handler
  "Queue a URL for scraping"
  [request]
  (let [body (:body request)
        url (:url body)]
    (if (str/blank? url)
      (-> (response/response {:error "No URL provided"})
          (response/status 400))
      (response/response (queue-url url)))))

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
      (and (= uri "/watch") (= method :post)) (watch-handler request)
      (and (= uri "/unwatch") (= method :post)) (unwatch-handler request)
      (and (= uri "/queue/file") (= method :post)) (queue-file-handler request)
      (and (= uri "/queue/url") (= method :post)) (queue-url-handler request)
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
  "Start the Worker server"
  []
  (log/info "Starting Desktop Worker Service on port" service-port)
  
  ;; Start background processors
  (start-file-processor)
  (start-url-processor)
  
  ;; Schedule periodic health check
  (schedule-task :health-ping 60000
                 #(log/debug "Worker health ping - processed:" (:processed-count @worker-state)))
  
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Desktop Worker Service starting...")
  (start-server)
  (log/info "Desktop Worker ready at http://localhost:" service-port))
