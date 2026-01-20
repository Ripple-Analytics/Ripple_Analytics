(ns harvester.core
  "Harvester Service
   
   Data harvesting microservice for web scraping and file processing.
   Supports background job processing with status tracking.
   Hot-loadable for continuous deployment."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go <! >! chan]]
            [clojure.tools.logging :as log])
  (:import [org.jsoup Jsoup]
           [java.security MessageDigest]
           [java.time Instant])
  (:gen-class))

;; ============================================
;; State Management
;; ============================================

(def jobs
  "In-memory job storage"
  (atom {}))

(def scraped-data
  "In-memory scraped data storage"
  (atom []))

;; ============================================
;; Utility Functions
;; ============================================

(defn generate-job-id
  "Generate a unique job ID"
  [url]
  (let [md (MessageDigest/getInstance "MD5")
        input (str url (Instant/now))
        hash-bytes (.digest md (.getBytes input))]
    (apply str (take 12 (map #(format "%02x" %) hash-bytes)))))

(defn now-iso
  "Get current timestamp in ISO format"
  []
  (str (Instant/now)))

;; ============================================
;; Web Scraping Functions
;; ============================================

(defn scrape-url
  "Scrape content from a URL"
  [url]
  (try
    (let [response (http/get url {:socket-timeout 30000
                                   :connection-timeout 10000
                                   :throw-exceptions false
                                   :headers {"User-Agent" "Mental Models Harvester/1.0"}})
          status (:status response)]
      (if (= 200 status)
        (let [html (:body response)
              doc (Jsoup/parse html)
              title (.text (.select doc "title"))
              text (.text (.select doc "body"))
              links (map #(.attr % "href") (.select doc "a[href]"))
              word-count (count (str/split text #"\s+"))]
          {:success true
           :url url
           :title title
           :content (subs text 0 (min 5000 (count text)))
           :word-count word-count
           :links-found (count links)
           :scraped-at (now-iso)
           :content-hash (generate-job-id text)})
        {:success false
         :url url
         :error (str "HTTP " status)
         :scraped-at (now-iso)}))
    (catch Exception e
      {:success false
       :url url
       :error (.getMessage e)
       :scraped-at (now-iso)})))

(defn process-scrape-job
  "Process a scraping job in the background"
  [job-id url extract-text]
  (go
    (try
      ;; Update job status to running
      (swap! jobs assoc-in [job-id :status] "running")
      (swap! jobs assoc-in [job-id :progress] 0.1)
      (swap! jobs assoc-in [job-id :updated-at] (now-iso))
      
      ;; Perform scraping
      (<! (async/timeout 500)) ;; Small delay for realistic progress
      (swap! jobs assoc-in [job-id :progress] 0.5)
      
      (let [result (scrape-url url)]
        (if (:success result)
          (do
            ;; Store scraped data
            (swap! scraped-data conj result)
            
            ;; Update job as completed
            (swap! jobs assoc-in [job-id :status] "completed")
            (swap! jobs assoc-in [job-id :progress] 1.0)
            (swap! jobs assoc-in [job-id :result] result)
            (swap! jobs assoc-in [job-id :updated-at] (now-iso))
            (log/info "Scrape job" job-id "completed for" url))
          (do
            ;; Update job as failed
            (swap! jobs assoc-in [job-id :status] "failed")
            (swap! jobs assoc-in [job-id :error] (:error result))
            (swap! jobs assoc-in [job-id :updated-at] (now-iso))
            (log/warn "Scrape job" job-id "failed:" (:error result)))))
      
      (catch Exception e
        (swap! jobs assoc-in [job-id :status] "failed")
        (swap! jobs assoc-in [job-id :error] (.getMessage e))
        (swap! jobs assoc-in [job-id :updated-at] (now-iso))
        (log/error e "Scrape job" job-id "failed")))))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Harvester Service"
  (Integer/parseInt (or (System/getenv "PORT") "8002")))

;; ============================================
;; Route Handlers
;; ============================================

(defn root-handler
  "Root endpoint"
  [_]
  (response/response
   {:service "Mental Models Harvester Service"
    :version "1.0.0"
    :active-jobs (count (filter #(= "running" (:status %)) (vals @jobs)))}))

(defn health-handler
  "Health check endpoint"
  [_]
  (response/response {:status "healthy" :service "harvester"}))

(defn scrape-handler
  "Start a scraping job"
  [request]
  (let [body (:body request)
        url (or (:url body) "")
        extract-text (get body :extract_text true)]
    (if (str/blank? url)
      (-> (response/response {:error "No URL provided"})
          (response/status 400))
      (let [job-id (generate-job-id url)
            now (now-iso)]
        ;; Create job entry
        (swap! jobs assoc job-id
               {:job-id job-id
                :url url
                :status "pending"
                :progress 0.0
                :result nil
                :error nil
                :created-at now
                :updated-at now})
        
        ;; Start background processing
        (process-scrape-job job-id url extract-text)
        
        (response/response
         {:job-id job-id
          :status "pending"
          :message (str "Scraping job started for " url)})))))

(defn job-status-handler
  "Get status of a specific job"
  [request]
  (let [job-id (get-in request [:params :id])]
    (if-let [job (get @jobs job-id)]
      (response/response job)
      (-> (response/response {:error "Job not found"})
          (response/status 404)))))

(defn jobs-handler
  "List all jobs"
  [_]
  (let [all-jobs (vals @jobs)]
    (response/response
     {:jobs all-jobs
      :total (count all-jobs)
      :running (count (filter #(= "running" (:status %)) all-jobs))
      :completed (count (filter #(= "completed" (:status %)) all-jobs))
      :failed (count (filter #(= "failed" (:status %)) all-jobs))})))

(defn delete-job-handler
  "Delete a job"
  [request]
  (let [job-id (get-in request [:params :id])]
    (if (contains? @jobs job-id)
      (do
        (swap! jobs dissoc job-id)
        (response/response {:message (str "Job " job-id " deleted")}))
      (-> (response/response {:error "Job not found"})
          (response/status 404)))))

(defn status-handler
  "Get harvester status"
  [_]
  (let [all-jobs (vals @jobs)]
    (response/response
     {:status "running"
      :jobs-total (count all-jobs)
      :jobs-running (count (filter #(= "running" (:status %)) all-jobs))
      :jobs-completed (count (filter #(= "completed" (:status %)) all-jobs))
      :jobs-failed (count (filter #(= "failed" (:status %)) all-jobs))
      :data-collected (count @scraped-data)
      :uptime "healthy"})))

(defn data-handler
  "Get all scraped data"
  [_]
  (response/response
   {:data @scraped-data
    :count (count @scraped-data)}))

(defn clear-data-handler
  "Clear all scraped data"
  [_]
  (reset! scraped-data [])
  (response/response {:message "All scraped data cleared"}))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= uri "/") (root-handler request)
      (= uri "/health") (health-handler request)
      (and (= uri "/scrape") (= method :post)) (scrape-handler request)
      (= uri "/jobs") (jobs-handler request)
      (re-matches #"/jobs/([a-f0-9]+)" uri)
      (let [job-id (second (re-matches #"/jobs/([a-f0-9]+)" uri))]
        (if (= method :delete)
          (delete-job-handler (assoc-in request [:params :id] job-id))
          (job-status-handler (assoc-in request [:params :id] job-id))))
      (= uri "/status") (status-handler request)
      (= uri "/data") (if (= method :delete)
                        (clear-data-handler request)
                        (data-handler request))
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
  "Start the Harvester Service server"
  []
  (log/info "Starting Harvester Service on port" service-port)
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Harvester Service starting...")
  (start-server)
  (log/info "Harvester Service ready on port" service-port))
