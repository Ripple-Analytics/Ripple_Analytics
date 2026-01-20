(ns desktop.ui.core
  "Desktop UI Service - Electric Clojure
   
   Reactive web-based desktop UI for Mental Models System.
   Serves a light-mode responsive interface that connects
   to the backend microservices."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [hiccup.page :as page]
            [clojure.tools.logging :as log])
  (:gen-class))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Desktop UI Service"
  (Integer/parseInt (or (System/getenv "PORT") "3000")))

(def api-gateway-url
  "URL for the API Gateway"
  (or (System/getenv "API_GATEWAY_URL") "http://api-gateway:8000"))

(def worker-service-url
  "URL for the Worker Service"
  (or (System/getenv "WORKER_SERVICE_URL") "http://desktop-worker:3001"))

(def sync-service-url
  "URL for the Sync Service"
  (or (System/getenv "SYNC_SERVICE_URL") "http://desktop-sync:3002"))

;; ============================================
;; State Management
;; ============================================

(def app-state
  "Application state atom"
  (atom {:theme "light"
         :current-view :dashboard
         :analysis-results []
         :models []
         :settings {:api-url api-gateway-url
                    :auto-sync true
                    :notifications true}
         :harvester {:running false
                     :watched-dirs []
                     :scrape-urls []
                     :stats {:files-processed 0
                             :urls-scraped 0}}}))

;; ============================================
;; API Client Functions
;; ============================================

(defn api-call
  "Make an API call to the gateway"
  [method endpoint & [body]]
  (try
    (let [url (str api-gateway-url endpoint)
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
      {:success (= 200 (:status response))
       :data (:body response)
       :status (:status response)})
    (catch Exception e
      {:success false
       :error (.getMessage e)})))

(defn fetch-models
  "Fetch all mental models from the API"
  []
  (let [result (api-call :get "/api/analysis/models")]
    (when (:success result)
      (swap! app-state assoc :models (get-in result [:data :models])))))

(defn analyze-text
  "Analyze text for mental models"
  [text top-n]
  (let [result (api-call :post "/api/analysis/analyze" {:text text :top_n top-n})]
    (when (:success result)
      (swap! app-state update :analysis-results conj (:data result)))
    result))

(defn fetch-health
  "Fetch health status of all services"
  []
  (api-call :get "/health"))

;; ============================================
;; HTML Components (Hiccup)
;; ============================================

(defn render-head
  "Render HTML head with styles"
  []
  [:head
   [:meta {:charset "UTF-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
   [:title "Mental Models System - Electric Clojure Desktop"]
   [:script {:src "https://cdn.tailwindcss.com"}]
   [:style
    "
    :root {
      --bg-primary: #f8f9fa;
      --bg-secondary: #ffffff;
      --bg-card: #ffffff;
      --text-primary: #1a1a2e;
      --text-secondary: #6c757d;
      --accent: #0066cc;
      --accent-hover: #0052a3;
      --success: #28a745;
      --warning: #ffc107;
      --error: #dc3545;
      --border-color: #dee2e6;
    }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: var(--bg-primary);
      color: var(--text-primary);
      margin: 0;
      padding: 0;
    }
    .card {
      background: var(--bg-card);
      border: 1px solid var(--border-color);
      border-radius: 8px;
      padding: 16px;
      margin-bottom: 16px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }
    .btn {
      padding: 8px 16px;
      border-radius: 6px;
      font-weight: 500;
      cursor: pointer;
      border: none;
      transition: all 0.2s;
    }
    .btn-primary {
      background: var(--accent);
      color: white;
    }
    .btn-primary:hover {
      background: var(--accent-hover);
    }
    .btn-secondary {
      background: var(--bg-primary);
      color: var(--text-primary);
      border: 1px solid var(--border-color);
    }
    .btn-secondary:hover {
      background: var(--border-color);
    }
    .nav-item {
      padding: 12px 20px;
      cursor: pointer;
      border-bottom: 2px solid transparent;
      color: var(--text-secondary);
    }
    .nav-item:hover {
      background: var(--bg-primary);
    }
    .nav-item.active {
      color: var(--accent);
      border-bottom-color: var(--accent);
      font-weight: 600;
    }
    .input {
      padding: 8px 12px;
      border: 1px solid var(--border-color);
      border-radius: 6px;
      width: 100%;
      font-size: 14px;
    }
    .input:focus {
      outline: none;
      border-color: var(--accent);
      box-shadow: 0 0 0 3px rgba(0,102,204,0.1);
    }
    textarea.input {
      min-height: 120px;
      resize: vertical;
    }
    .status-badge {
      display: inline-block;
      padding: 4px 8px;
      border-radius: 12px;
      font-size: 12px;
      font-weight: 500;
    }
    .status-healthy {
      background: #d4edda;
      color: #155724;
    }
    .status-unhealthy {
      background: #f8d7da;
      color: #721c24;
    }
    .model-card {
      border-left: 4px solid var(--accent);
      padding-left: 12px;
      margin-bottom: 12px;
    }
    .model-name {
      font-weight: 600;
      color: var(--text-primary);
    }
    .model-category {
      font-size: 12px;
      color: var(--text-secondary);
    }
    .relevance-bar {
      height: 4px;
      background: var(--border-color);
      border-radius: 2px;
      margin-top: 8px;
    }
    .relevance-fill {
      height: 100%;
      background: var(--accent);
      border-radius: 2px;
    }
    "]])

(defn render-header
  "Render application header"
  []
  [:header {:class "bg-white border-b border-gray-200 px-6 py-4"}
   [:div {:class "flex justify-between items-center"}
    [:div
     [:h1 {:class "text-xl font-bold text-gray-900"} "Mental Models System"]
     [:p {:class "text-sm text-gray-500"} "Electric Clojure Desktop"]]
    [:div {:class "flex items-center gap-4"}
     [:span {:id "connection-status" :class "status-badge status-healthy"} "Connected"]
     [:button {:class "btn btn-secondary" :onclick "location.reload()"} "Refresh"]]]])

(defn render-nav
  "Render navigation"
  []
  [:nav {:class "bg-white border-b border-gray-200"}
   [:div {:class "flex"}
    [:a {:href "/" :class "nav-item active"} "Dashboard"]
    [:a {:href "/analysis" :class "nav-item"} "Analysis"]
    [:a {:href "/models" :class "nav-item"} "Models"]
    [:a {:href "/harvester" :class "nav-item"} "Harvester"]
    [:a {:href "/settings" :class "nav-item"} "Settings"]]])

(defn render-dashboard
  "Render dashboard view"
  [health-data]
  [:div {:class "p-6"}
   [:h2 {:class "text-lg font-semibold mb-4"} "System Dashboard"]
   
   ;; Service Status Cards
   [:div {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-6"}
    [:div {:class "card"}
     [:div {:class "text-sm text-gray-500"} "API Gateway"]
     [:div {:class "text-2xl font-bold"} "8000"]
     [:span {:class (str "status-badge " (if (= "healthy" (get-in health-data [:services :analysis])) "status-healthy" "status-unhealthy"))}
      (or (get-in health-data [:services :analysis]) "Unknown")]]
    
    [:div {:class "card"}
     [:div {:class "text-sm text-gray-500"} "Analysis Service"]
     [:div {:class "text-2xl font-bold"} "8001"]
     [:span {:class "status-badge status-healthy"} "Healthy"]]
    
    [:div {:class "card"}
     [:div {:class "text-sm text-gray-500"} "Harvester Service"]
     [:div {:class "text-2xl font-bold"} "8002"]
     [:span {:class "status-badge status-healthy"} "Healthy"]]
    
    [:div {:class "card"}
     [:div {:class "text-sm text-gray-500"} "Storage Service"]
     [:div {:class "text-2xl font-bold"} "8003"]
     [:span {:class "status-badge status-healthy"} "Healthy"]]]
   
   ;; Quick Actions
   [:div {:class "card"}
    [:h3 {:class "font-semibold mb-3"} "Quick Actions"]
    [:div {:class "flex gap-3"}
     [:a {:href "/analysis" :class "btn btn-primary"} "New Analysis"]
     [:a {:href "/models" :class "btn btn-secondary"} "Browse Models"]
     [:a {:href "/harvester" :class "btn btn-secondary"} "Start Harvester"]]]])

(defn render-analysis-page
  "Render analysis page"
  []
  [:div {:class "p-6"}
   [:h2 {:class "text-lg font-semibold mb-4"} "Mental Model Analysis"]
   
   [:div {:class "card"}
    [:form {:method "POST" :action "/api/analyze"}
     [:div {:class "mb-4"}
      [:label {:class "block text-sm font-medium mb-2"} "Text to Analyze"]
      [:textarea {:name "text" :class "input" :placeholder "Enter text to analyze for mental model patterns..."}]]
     
     [:div {:class "mb-4"}
      [:label {:class "block text-sm font-medium mb-2"} "Number of Results"]
      [:select {:name "top_n" :class "input" :style "width: auto;"}
       [:option {:value "3"} "Top 3"]
       [:option {:value "5" :selected true} "Top 5"]
       [:option {:value "10"} "Top 10"]]]
     
     [:button {:type "submit" :class "btn btn-primary"} "Analyze"]]]])

(defn render-models-page
  "Render models browser page"
  [models]
  [:div {:class "p-6"}
   [:h2 {:class "text-lg font-semibold mb-4"} "Mental Models Library"]
   [:p {:class "text-gray-500 mb-4"} (str (count models) " models available")]
   
   [:div {:class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"}
    (for [model models]
      [:div {:class "card model-card" :key (:id model)}
       [:div {:class "model-name"} (:name model)]
       [:div {:class "model-category"} (:category model)]
       [:p {:class "text-sm text-gray-600 mt-2"} (:description model)]])]])

(defn render-harvester-page
  "Render harvester page"
  []
  [:div {:class "p-6"}
   [:h2 {:class "text-lg font-semibold mb-4"} "Data Harvester"]
   
   [:div {:class "grid grid-cols-1 md:grid-cols-2 gap-6"}
    ;; Watched Directories
    [:div {:class "card"}
     [:h3 {:class "font-semibold mb-3"} "Watched Directories"]
     [:div {:class "flex gap-2 mb-3"}
      [:input {:type "text" :class "input" :placeholder "Add directory path..."}]
      [:button {:class "btn btn-primary"} "Add"]]
     [:div {:class "text-sm text-gray-500"} "No directories configured"]]
    
    ;; Scrape URLs
    [:div {:class "card"}
     [:h3 {:class "font-semibold mb-3"} "Scrape URLs"]
     [:div {:class "flex gap-2 mb-3"}
      [:input {:type "text" :class "input" :placeholder "Add URL to scrape..."}]
      [:button {:class "btn btn-primary"} "Add"]]
     [:div {:class "text-sm text-gray-500"} "No URLs configured"]]]
   
   ;; Harvester Controls
   [:div {:class "card mt-4"}
    [:h3 {:class "font-semibold mb-3"} "Harvester Status"]
    [:div {:class "flex items-center gap-4"}
     [:span {:class "status-badge status-unhealthy"} "Stopped"]
     [:button {:class "btn btn-primary"} "Start Harvester"]
     [:button {:class "btn btn-secondary"} "Stop"]]]])

(defn render-settings-page
  "Render settings page"
  []
  [:div {:class "p-6"}
   [:h2 {:class "text-lg font-semibold mb-4"} "Settings"]
   
   [:div {:class "card"}
    [:h3 {:class "font-semibold mb-3"} "API Configuration"]
    [:div {:class "mb-4"}
     [:label {:class "block text-sm font-medium mb-2"} "API Gateway URL"]
     [:input {:type "text" :class "input" :value api-gateway-url}]]
    
    [:div {:class "mb-4"}
     [:label {:class "flex items-center gap-2"}
      [:input {:type "checkbox" :checked true}]
      [:span {:class "text-sm"} "Auto-sync with backend"]]]
    
    [:div {:class "mb-4"}
     [:label {:class "flex items-center gap-2"}
      [:input {:type "checkbox" :checked true}]
      [:span {:class "text-sm"} "Enable notifications"]]]
    
    [:button {:class "btn btn-primary"} "Save Settings"]]])

(defn render-page
  "Render full HTML page"
  [view & [data]]
  (str
   "<!DOCTYPE html>"
   (h/html
    [:html {:lang "en"}
     (render-head)
     [:body
      (render-header)
      (render-nav)
      (case view
        :dashboard (render-dashboard data)
        :analysis (render-analysis-page)
        :models (render-models-page data)
        :harvester (render-harvester-page)
        :settings (render-settings-page)
        (render-dashboard data))]])))

;; ============================================
;; Route Handlers
;; ============================================

(defn dashboard-handler
  "Dashboard page handler"
  [_]
  (let [health (fetch-health)]
    (-> (response/response (render-page :dashboard (:data health)))
        (response/content-type "text/html"))))

(defn analysis-handler
  "Analysis page handler"
  [_]
  (-> (response/response (render-page :analysis))
      (response/content-type "text/html")))

(defn models-handler
  "Models page handler"
  [_]
  (fetch-models)
  (-> (response/response (render-page :models (:models @app-state)))
      (response/content-type "text/html")))

(defn harvester-handler
  "Harvester page handler"
  [_]
  (-> (response/response (render-page :harvester))
      (response/content-type "text/html")))

(defn settings-handler
  "Settings page handler"
  [_]
  (-> (response/response (render-page :settings))
      (response/content-type "text/html")))

(defn analyze-api-handler
  "API handler for analysis"
  [request]
  (let [params (or (:params request) (:body request))
        text (or (:text params) "")
        top-n (or (:top_n params) 5)]
    (if (str/blank? text)
      (-> (response/response {:error "No text provided"})
          (response/status 400))
      (let [result (analyze-text text top-n)]
        (if (:success result)
          (response/response (:data result))
          (-> (response/response {:error (:error result)})
              (response/status 500)))))))

(defn health-handler
  "Health check handler"
  [_]
  (response/response {:status "healthy" :service "desktop-ui"}))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= uri "/") (dashboard-handler request)
      (= uri "/analysis") (analysis-handler request)
      (= uri "/models") (models-handler request)
      (= uri "/harvester") (harvester-handler request)
      (= uri "/settings") (settings-handler request)
      (= uri "/health") (health-handler request)
      (and (= uri "/api/analyze") (= method :post)) (analyze-api-handler request)
      :else (-> (response/response "Not Found")
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

(def app
  "Main application handler"
  (-> router
      wrap-logging
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the Desktop UI server"
  []
  (log/info "Starting Desktop UI Service on port" service-port)
  (log/info "API Gateway URL:" api-gateway-url)
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Desktop UI Service starting...")
  (start-server)
  (log/info "Desktop UI ready at http://localhost:" service-port))
