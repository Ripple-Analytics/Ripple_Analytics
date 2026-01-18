(ns mental-models.core
  "Mental Models System - Core Web Server
   
   A comprehensive mental models analysis system built in Clojure.
   Provides REST API endpoints for:
   - Mental model queries and analysis
   - Latticework analysis (Munger's approach)
   - Lollapalooza detection
   - Statistical analysis
   - Document processing
   
   Built with Ring/Compojure for maximum expressiveness and
   rapid feature development."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.util.response :as response]
            [compojure.core :refer [defroutes GET POST PUT DELETE context]]
            [compojure.route :as route]
            [cheshire.core :as json]
            [mental-models.models :as models]
            [mental-models.analysis :as analysis]
            [mental-models.statistics :as stats]
            [mental-models.data-processing :as data])
  (:gen-class))

;; ============================================
;; Response Helpers
;; ============================================

(defn json-response
  "Create a JSON response with proper content type."
  [data & {:keys [status] :or {status 200}}]
  (-> (response/response data)
      (response/status status)
      (response/content-type "application/json")))

(defn error-response
  "Create an error response."
  [message & {:keys [status] :or {status 400}}]
  (json-response {:error message :status "error"} :status status))

(defn success-response
  "Create a success response."
  [data]
  (json-response (assoc data :status "success")))

;; ============================================
;; API Handlers - Models
;; ============================================

(defn get-all-models-handler
  "Get all mental models."
  [request]
  (success-response
   {:models (models/all-model-summaries)
    :total (count @models/models)
    :categories (keys @models/categories)}))

(defn get-model-handler
  "Get a specific mental model by name."
  [name]
  (if-let [model (models/get-model name)]
    (success-response {:model model})
    (error-response (str "Model not found: " name) :status 404)))

(defn get-models-by-category-handler
  "Get all models in a category."
  [category]
  (let [category-models (models/get-models-by-category category)]
    (if (seq category-models)
      (success-response {:category category
                         :models category-models
                         :count (count category-models)})
      (error-response (str "Category not found: " category) :status 404))))

(defn search-models-handler
  "Search models by query."
  [query]
  (let [results (models/search-models query)]
    (success-response {:query query
                       :results (map models/model-summary (map :name results))
                       :count (count results)})))

;; ============================================
;; API Handlers - Analysis
;; ============================================

(defn analyze-handler
  "Perform comprehensive analysis."
  [request]
  (let [body (:body request)
        context (get body "context" (get body :context {}))
        model-names (get body "models" (get body :models []))]
    (success-response
     {:analysis (analysis/analyze-comprehensive model-names context)})))

(defn latticework-handler
  "Perform latticework analysis."
  [request]
  (let [body (:body request)
        context (get body "context" (get body :context {}))
        model-names (get body "models" (get body :models []))]
    (success-response
     {:latticework (analysis/latticework-analyze model-names context)})))

(defn lollapalooza-handler
  "Detect lollapalooza effects."
  [request]
  (let [body (:body request)
        context (get body "context" (get body :context {}))
        model-names (get body "models" (get body :models []))]
    (success-response
     {:lollapalooza (analysis/detect-lollapalooza model-names context)})))

(defn inversion-handler
  "Perform inversion analysis."
  [request]
  (let [body (:body request)
        problem (get body "problem" (get body :problem ""))]
    (success-response
     {:inversion (analysis/invert problem)})))

(defn two-track-handler
  "Perform two-track analysis."
  [request]
  (let [body (:body request)
        situation (get body "situation" (get body :situation ""))]
    (success-response
     {:two-track (analysis/two-track-analysis situation)})))

(defn bias-detection-handler
  "Detect cognitive biases in text."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))]
    (success-response
     {:bias-detection (analysis/detect-biases text)})))

(defn decision-checklist-handler
  "Generate decision checklist."
  [request]
  (let [body (:body request)
        decision (get body "decision" (get body :decision ""))
        context (get body "context" (get body :context {}))]
    (success-response
     {:checklist (analysis/decision-checklist decision context)})))

;; ============================================
;; API Handlers - Statistics
;; ============================================

(defn descriptive-stats-handler
  "Calculate descriptive statistics."
  [request]
  (let [body (:body request)
        data (get body "data" (get body :data []))]
    (success-response
     {:statistics (stats/descriptive-stats data)})))

(defn correlation-handler
  "Calculate correlation between two variables."
  [request]
  (let [body (:body request)
        xs (get body "x" (get body :x []))
        ys (get body "y" (get body :y []))]
    (success-response
     {:correlation (stats/correlation-analysis xs ys)})))

(defn regression-handler
  "Perform linear regression."
  [request]
  (let [body (:body request)
        xs (get body "x" (get body :x []))
        ys (get body "y" (get body :y []))]
    (success-response
     {:regression (stats/linear-regression xs ys)})))

(defn comprehensive-stats-handler
  "Perform comprehensive statistical analysis."
  [request]
  (let [body (:body request)
        data (get body "data" (get body :data {}))]
    (success-response
     {:analysis (stats/comprehensive-statistical-analysis data)})))

;; ============================================
;; API Handlers - Data Processing
;; ============================================

(defn analyze-document-handler
  "Analyze document structure."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))]
    (success-response
     {:analysis (data/analyze-document text)})))

(defn chunk-text-handler
  "Chunk text for processing."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))
        chunk-size (get body "chunk_size" (get body :chunk-size 1000))
        overlap (get body "overlap" (get body :overlap 100))]
    (success-response
     {:chunks (data/chunk-text text chunk-size overlap)})))

(defn extract-entities-handler
  "Extract entities from text."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))]
    (success-response
     {:entities (data/extract-entities text)})))

(defn readability-handler
  "Calculate readability score."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))]
    (success-response
     {:readability (data/flesch-reading-ease text)})))

(defn classify-text-handler
  "Classify text by mental models."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))]
    (success-response
     {:classifications (data/classify-by-mental-models text)})))

(defn process-document-handler
  "Complete document processing pipeline."
  [request]
  (let [body (:body request)
        text (get body "text" (get body :text ""))
        chunk-size (get body "chunk_size" (get body :chunk-size 1000))
        overlap (get body "overlap" (get body :overlap 100))]
    (success-response
     {:processed (data/process-and-analyze text
                                           :chunk-size chunk-size
                                           :overlap overlap)})))

;; ============================================
;; Health Check
;; ============================================

(defn health-handler
  "Health check endpoint."
  [request]
  (success-response
   {:status "healthy"
    :service "mental-models-system"
    :version "1.0.0"
    :language "Clojure"
    :models-loaded (count @models/models)
    :failure-modes-loaded (count @models/failure-modes)
    :timestamp (str (java.time.Instant/now))}))

;; ============================================
;; Routes
;; ============================================

(defroutes app-routes
  ;; Health
  (GET "/" [] health-handler)
  (GET "/health" [] health-handler)
  
  ;; Models
  (context "/api/models" []
    (GET "/" [] get-all-models-handler)
    (GET "/search" [q] (search-models-handler q))
    (GET "/category/:category" [category] (get-models-by-category-handler category))
    (GET "/:name" [name] (get-model-handler name)))
  
  ;; Analysis
  (context "/api/analysis" []
    (POST "/comprehensive" [] analyze-handler)
    (POST "/latticework" [] latticework-handler)
    (POST "/lollapalooza" [] lollapalooza-handler)
    (POST "/inversion" [] inversion-handler)
    (POST "/two-track" [] two-track-handler)
    (POST "/bias-detection" [] bias-detection-handler)
    (POST "/decision-checklist" [] decision-checklist-handler))
  
  ;; Statistics
  (context "/api/statistics" []
    (POST "/descriptive" [] descriptive-stats-handler)
    (POST "/correlation" [] correlation-handler)
    (POST "/regression" [] regression-handler)
    (POST "/comprehensive" [] comprehensive-stats-handler))
  
  ;; Data Processing
  (context "/api/data" []
    (POST "/analyze" [] analyze-document-handler)
    (POST "/chunk" [] chunk-text-handler)
    (POST "/entities" [] extract-entities-handler)
    (POST "/readability" [] readability-handler)
    (POST "/classify" [] classify-text-handler)
    (POST "/process" [] process-document-handler))
  
  ;; Fallback
  (route/not-found (error-response "Not found" :status 404)))

;; ============================================
;; Middleware
;; ============================================

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete :options]
                 :access-control-allow-headers [:content-type :authorization])))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the web server."
  [& {:keys [port] :or {port 8000}}]
  (println (str "Starting Mental Models System on port " port))
  (println (str "Models loaded: " (count @models/models)))
  (println (str "Failure modes loaded: " (count @models/failure-modes)))
  (jetty/run-jetty app {:port port :join? false}))

(defn -main
  "Main entry point."
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8000"))]
    (start-server :port port)))
