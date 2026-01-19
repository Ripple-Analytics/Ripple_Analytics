(ns gateway.core
  "API Gateway Service
   
   Central entry point for all Mental Models microservices.
   Routes requests to appropriate backend services.
   Provides health monitoring and load balancing."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.tools.logging :as log])
  (:gen-class))

;; ============================================
;; Service Configuration
;; ============================================

(def service-urls
  "URLs for backend microservices"
  {:analysis (or (System/getenv "ANALYSIS_SERVICE_URL") "http://analysis-service:8001")
   :harvester (or (System/getenv "HARVESTER_SERVICE_URL") "http://harvester-service:8002")
   :storage (or (System/getenv "STORAGE_SERVICE_URL") "http://storage-service:8003")})

(def gateway-port
  "Port for the API Gateway"
  (Integer/parseInt (or (System/getenv "PORT") "8000")))

;; ============================================
;; Service Health Checks
;; ============================================

(defn check-service-health
  "Check health of a single service"
  [service-name url]
  (try
    (let [response (http/get (str url "/health")
                             {:throw-exceptions false
                              :socket-timeout 3000
                              :connection-timeout 3000})]
      (if (= 200 (:status response))
        {:service service-name :status "healthy"}
        {:service service-name :status "unhealthy" :code (:status response)}))
    (catch Exception e
      {:service service-name :status "unavailable" :error (.getMessage e)})))

(defn check-all-services
  "Check health of all backend services"
  []
  (let [results (map (fn [[k v]] (check-service-health (name k) v)) service-urls)
        all-healthy? (every? #(= "healthy" (:status %)) results)]
    {:status (if all-healthy? "healthy" "degraded")
     :services (into {} (map (fn [r] [(:service r) (:status r)]) results))}))

;; ============================================
;; Service Proxy Functions
;; ============================================

(defn proxy-request
  "Proxy a request to a backend service"
  [service-url path method body]
  (try
    (let [url (str service-url path)
          opts {:throw-exceptions false
                :socket-timeout 30000
                :connection-timeout 5000
                :content-type :json
                :accept :json
                :as :json}
          opts (if body (assoc opts :body (json/generate-string body)) opts)
          response (case method
                     :get (http/get url opts)
                     :post (http/post url opts)
                     :put (http/put url opts)
                     :delete (http/delete url opts))]
      {:status (:status response)
       :body (:body response)})
    (catch Exception e
      (log/error e "Proxy request failed")
      {:status 503
       :body {:error "Service unavailable" :message (.getMessage e)}})))

;; ============================================
;; Route Handlers
;; ============================================

(defn root-handler
  "Root endpoint - service info"
  [_]
  (response/response
   {:service "Mental Models API Gateway"
    :version "1.0.0"
    :endpoints {:health "/health"
                :analysis "/api/analysis"
                :harvester "/api/harvester"
                :storage "/api/storage"}}))

(defn health-handler
  "Health check endpoint"
  [_]
  (response/response (check-all-services)))

;; Analysis Service Routes
(defn analyze-handler
  "Proxy to analysis service"
  [request]
  (let [body (:body request)
        result (proxy-request (:analysis service-urls) "/analyze" :post body)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn models-handler
  "Get all mental models"
  [_]
  (let [result (proxy-request (:analysis service-urls) "/models" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn model-by-id-handler
  "Get a specific mental model"
  [request]
  (let [id (get-in request [:params :id])
        result (proxy-request (:analysis service-urls) (str "/models/" id) :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn categories-handler
  "Get all model categories"
  [_]
  (let [result (proxy-request (:analysis service-urls) "/categories" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

;; Harvester Service Routes
(defn scrape-handler
  "Start a scraping job"
  [request]
  (let [body (:body request)
        result (proxy-request (:harvester service-urls) "/scrape" :post body)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn harvester-status-handler
  "Get harvester status"
  [_]
  (let [result (proxy-request (:harvester service-urls) "/status" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn jobs-handler
  "Get all harvester jobs"
  [_]
  (let [result (proxy-request (:harvester service-urls) "/jobs" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

;; Storage Service Routes
(defn storage-data-handler
  "Get stored data"
  [_]
  (let [result (proxy-request (:storage service-urls) "/data" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn store-data-handler
  "Store data"
  [request]
  (let [body (:body request)
        result (proxy-request (:storage service-urls) "/data" :post body)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn analyses-handler
  "Get stored analyses"
  [_]
  (let [result (proxy-request (:storage service-urls) "/analyses" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

(defn settings-handler
  "Get settings"
  [_]
  (let [result (proxy-request (:storage service-urls) "/settings" :get nil)]
    (-> (response/response (:body result))
        (response/status (:status result)))))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      ;; Root
      (= uri "/") (root-handler request)
      
      ;; Health
      (= uri "/health") (health-handler request)
      
      ;; Analysis Service
      (and (= uri "/api/analysis/analyze") (= method :post)) (analyze-handler request)
      (= uri "/api/analysis/models") (models-handler request)
      (re-matches #"/api/analysis/models/\d+" uri) (model-by-id-handler request)
      (= uri "/api/analysis/categories") (categories-handler request)
      
      ;; Harvester Service
      (and (= uri "/api/harvester/scrape") (= method :post)) (scrape-handler request)
      (= uri "/api/harvester/status") (harvester-status-handler request)
      (= uri "/api/harvester/jobs") (jobs-handler request)
      
      ;; Storage Service
      (= uri "/api/storage/data") (if (= method :post)
                                    (store-data-handler request)
                                    (storage-data-handler request))
      (= uri "/api/storage/analyses") (analyses-handler request)
      (= uri "/api/storage/settings") (settings-handler request)
      
      ;; Not found
      :else (-> (response/response {:error "Not found"})
                (response/status 404)))))

;; ============================================
;; Middleware
;; ============================================

(defn wrap-cors
  "Add CORS headers"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (response/header "Access-Control-Allow-Origin" "*")
          (response/header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS")
          (response/header "Access-Control-Allow-Headers" "Content-Type, Authorization")))))

(defn wrap-logging
  "Log requests"
  [handler]
  (fn [request]
    (log/info "Request:" (:request-method request) (:uri request))
    (let [response (handler request)]
      (log/info "Response:" (:status response))
      response)))

(def app
  "Main application handler"
  (-> router
      wrap-cors
      wrap-logging
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-defaults api-defaults)))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the API Gateway server"
  []
  (log/info "Starting API Gateway on port" gateway-port)
  (log/info "Service URLs:" service-urls)
  (jetty/run-jetty app {:port gateway-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models API Gateway starting...")
  (start-server)
  (log/info "API Gateway ready on port" gateway-port))
