(ns storage.core
  "Storage Service
   
   Data persistence microservice for storing analysis results,
   settings, and user data.
   Hot-loadable for continuous deployment."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.security MessageDigest]
           [java.time Instant])
  (:gen-class))

;; ============================================
;; State Management (In-Memory Storage)
;; ============================================

(def storage
  "In-memory storage for collections"
  (atom {:analyses {}
         :settings {}
         :decisions {}
         :models {}}))

;; ============================================
;; Utility Functions
;; ============================================

(defn generate-id
  "Generate a unique ID"
  [prefix]
  (let [md (MessageDigest/getInstance "MD5")
        input (str prefix (Instant/now) (rand-int 10000))
        hash-bytes (.digest md (.getBytes input))]
    (apply str (take 12 (map #(format "%02x" %) hash-bytes)))))

(defn now-iso
  "Get current timestamp in ISO format"
  []
  (str (Instant/now)))

;; ============================================
;; Storage Functions
;; ============================================

(defn store-item
  "Store an item in a collection"
  [collection key value metadata]
  (let [now (now-iso)
        existing (get-in @storage [collection key])
        item {:value value
              :metadata (or metadata {})
              :created-at (or (:created-at existing) now)
              :updated-at now}]
    (swap! storage assoc-in [collection key] item)
    {:success true
     :key key
     :collection (name collection)
     :message "Data stored successfully"}))

(defn get-item
  "Get an item from a collection"
  [collection key]
  (get-in @storage [collection key]))

(defn get-collection
  "Get all items in a collection"
  [collection]
  (get @storage collection {}))

(defn delete-item
  "Delete an item from a collection"
  [collection key]
  (if (contains? (get @storage collection) key)
    (do
      (swap! storage update collection dissoc key)
      {:success true
       :key key
       :collection (name collection)
       :message "Data deleted successfully"})
    {:success false
     :error "Item not found"}))

(defn delete-collection
  "Delete an entire collection"
  [collection]
  (let [count (count (get @storage collection {}))]
    (swap! storage assoc collection {})
    {:success true
     :collection (name collection)
     :items-deleted count
     :message (str "Collection " (name collection) " cleared")}))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Storage Service"
  (Integer/parseInt (or (System/getenv "PORT") "8003")))

;; ============================================
;; Route Handlers
;; ============================================

(defn root-handler
  "Root endpoint"
  [_]
  (response/response
   {:service "Mental Models Storage Service"
    :version "1.0.0"
    :collections (keys @storage)
    :total-items (reduce + (map count (vals @storage)))}))

(defn health-handler
  "Health check endpoint"
  [_]
  (response/response {:status "healthy" :service "storage"}))

(defn data-handler
  "Get all stored data overview"
  [_]
  (response/response
   {:collections (into {}
                       (map (fn [[k v]]
                              [k {:items (keys v)
                                  :count (count v)}])
                            @storage))
    :total-items (reduce + (map count (vals @storage)))}))

(defn store-data-handler
  "Store data item"
  [request]
  (let [body (:body request)
        key (or (:key body) (generate-id "item"))
        value (:value body)
        collection (keyword (or (:collection body) "default"))
        metadata (:metadata body)]
    (if (nil? value)
      (-> (response/response {:error "No value provided"})
          (response/status 400))
      (response/response (store-item collection key value metadata)))))

(defn get-collection-handler
  "Get all items in a collection"
  [request]
  (let [collection (keyword (get-in request [:params :collection]))]
    (if (contains? @storage collection)
      (response/response
       {:collection (name collection)
        :items (get-collection collection)
        :count (count (get-collection collection))})
      (-> (response/response {:error "Collection not found"})
          (response/status 404)))))

(defn get-item-handler
  "Get a specific item"
  [request]
  (let [collection (keyword (get-in request [:params :collection]))
        key (get-in request [:params :key])]
    (if-let [item (get-item collection key)]
      (response/response
       {:key key
        :collection (name collection)
        :value (:value item)
        :metadata (:metadata item)
        :created-at (:created-at item)
        :updated-at (:updated-at item)})
      (-> (response/response {:error "Item not found"})
          (response/status 404)))))

(defn delete-item-handler
  "Delete a specific item"
  [request]
  (let [collection (keyword (get-in request [:params :collection]))
        key (get-in request [:params :key])
        result (delete-item collection key)]
    (if (:success result)
      (response/response result)
      (-> (response/response result)
          (response/status 404)))))

(defn delete-collection-handler
  "Delete an entire collection"
  [request]
  (let [collection (keyword (get-in request [:params :collection]))]
    (response/response (delete-collection collection))))

;; Analysis-specific endpoints
(defn store-analysis-handler
  "Store an analysis result"
  [request]
  (let [body (:body request)
        analysis-id (generate-id "analysis")
        result (store-item :analyses analysis-id body {:type "analysis"})]
    (response/response
     {:success true
      :analysis-id analysis-id
      :message "Analysis stored successfully"})))

(defn get-analyses-handler
  "Get all stored analyses"
  [_]
  (let [analyses (get-collection :analyses)]
    (response/response
     {:analyses (map (fn [[k v]]
                       {:id k
                        :value (:value v)
                        :created-at (:created-at v)
                        :updated-at (:updated-at v)})
                     analyses)
      :count (count analyses)})))

(defn get-analysis-handler
  "Get a specific analysis"
  [request]
  (let [analysis-id (get-in request [:params :id])]
    (if-let [item (get-item :analyses analysis-id)]
      (response/response
       {:id analysis-id
        :value (:value item)
        :created-at (:created-at item)
        :updated-at (:updated-at item)})
      (-> (response/response {:error "Analysis not found"})
          (response/status 404)))))

;; Settings endpoints
(defn get-settings-handler
  "Get all settings"
  [_]
  (response/response (get-collection :settings)))

(defn update-settings-handler
  "Update settings"
  [request]
  (let [body (:body request)
        updated-keys (atom [])]
    (doseq [[k v] body]
      (store-item :settings (name k) v nil)
      (swap! updated-keys conj (name k)))
    (response/response
     {:success true
      :message "Settings updated"
      :updated-keys @updated-keys})))

(defn get-setting-handler
  "Get a specific setting"
  [request]
  (let [key (get-in request [:params :key])]
    (if-let [item (get-item :settings key)]
      (response/response
       {:key key
        :value (:value item)
        :updated-at (:updated-at item)})
      (-> (response/response {:error "Setting not found"})
          (response/status 404)))))

;; Stats endpoint
(defn stats-handler
  "Get storage statistics"
  [_]
  (response/response
   {:collections (count @storage)
    :total-items (reduce + (map count (vals @storage)))
    :analyses-count (count (get-collection :analyses))
    :settings-count (count (get-collection :settings))
    :decisions-count (count (get-collection :decisions))
    :storage-healthy true}))

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
      (= uri "/stats") (stats-handler request)
      
      ;; Generic data endpoints
      (= uri "/data") (if (= method :post)
                        (store-data-handler request)
                        (data-handler request))
      
      ;; Collection endpoints
      (re-matches #"/data/([^/]+)" uri)
      (let [collection (second (re-matches #"/data/([^/]+)" uri))]
        (if (= method :delete)
          (delete-collection-handler (assoc-in request [:params :collection] collection))
          (get-collection-handler (assoc-in request [:params :collection] collection))))
      
      ;; Item endpoints
      (re-matches #"/data/([^/]+)/([^/]+)" uri)
      (let [[_ collection key] (re-matches #"/data/([^/]+)/([^/]+)" uri)]
        (case method
          :get (get-item-handler (-> request
                                     (assoc-in [:params :collection] collection)
                                     (assoc-in [:params :key] key)))
          :delete (delete-item-handler (-> request
                                           (assoc-in [:params :collection] collection)
                                           (assoc-in [:params :key] key)))
          (-> (response/response {:error "Method not allowed"})
              (response/status 405))))
      
      ;; Analysis endpoints
      (= uri "/analyses") (if (= method :post)
                            (store-analysis-handler request)
                            (get-analyses-handler request))
      (re-matches #"/analyses/([^/]+)" uri)
      (let [id (second (re-matches #"/analyses/([^/]+)" uri))]
        (get-analysis-handler (assoc-in request [:params :id] id)))
      
      ;; Settings endpoints
      (= uri "/settings") (if (= method :post)
                            (update-settings-handler request)
                            (get-settings-handler request))
      (re-matches #"/settings/([^/]+)" uri)
      (let [key (second (re-matches #"/settings/([^/]+)" uri))]
        (get-setting-handler (assoc-in request [:params :key] key)))
      
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
  "Start the Storage Service server"
  []
  (log/info "Starting Storage Service on port" service-port)
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Storage Service starting...")
  (start-server)
  (log/info "Storage Service ready on port" service-port))
