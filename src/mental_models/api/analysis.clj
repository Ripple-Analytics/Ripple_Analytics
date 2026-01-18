(ns mental-models.api.analysis
  "Analysis API endpoints - REST interface for text analysis
   Desktop app sends text here, gets back mental model scores and Lollapalooza detection"
  (:require [ring.util.response :as response]
            [cheshire.core :as json]
            [mental-models.services.analyzer :as analyzer]
            [mental-models.db.postgres :as db]
            [taoensso.timbre :as log]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

;; -- Response Helpers --------------------------------------------------------

(defn json-response
  "Create JSON response with proper headers"
  [data & {:keys [status] :or {status 200}}]
  (-> (response/response (json/generate-string data))
      (response/status status)
      (response/content-type "application/json")
      (response/header "Cache-Control" "private, max-age=0")))

(defn error-response
  "Create error response"
  [message & {:keys [status] :or {status 400}}]
  (json-response {:error message :timestamp (str (Instant/now))} :status status))

;; -- Authentication --

(defn authenticate
  "Authenticate request from desktop app"
  [request]
  (when-let [auth-header (get-in request [:headers "authorization"])]
    (when (.startsWith auth-header "Bearer ")
      (let [token (subs auth-header 7)]
        ;; Validate JWT token and return user/device
        {:id "device-123"
         :device-id "windows-desktop-001"
         :name "Windows Desktop"}))))

(defn require-auth
  "Middleware to require authentication"
  [handler]
  (fn [request]
    (if-let [user (authenticate request)]
      (handler (assoc request :user user))
      (error-response "Unauthorized" :status 401))))

;; -- Analysis Endpoints -------

(defn analyze-text
  "POST /api/v1/analyze - Analyze text against all 129 mental models
   
   Request body:
   {
     \"text\": \"text to analyze\",
     \"source\": \"file|news|folder|manual\",
     \"source_url\": \"optional URL\",
     \"metadata\": {optional metadata object}
   }
   
   Response:
   {
     \"analysis_id\": \"uuid\",
     \"text_length\": 1234,
     \"models_analyzed\": 129,
     \"successful_analyses\": 127,
     \"average_score\": 0.45,
     \"top_10_models\": [...],
     \"lollapalooza_detected\": true,
     \"convergence_score\": 0.87,
     \"converging_models\": [...]
   }"
  [request]
  (try
    (let [body (json/parse-string (slurp (:body request)) true)
          text (get body :text "")
          source (get body :source "manual")
          source-url (get body :source-url)
          metadata (get body :metadata {})
          
          user-id (get-in request [:user :id])
          device-id (get-in request [:user :device-id])
          analysis-id (str (UUID/randomUUID))]
      
      (if (str/blank? text)
        (error-response "Text is required" :status 400)
        
        (try
          ;; Run analysis
          (let [analysis (analyzer/AnalyzeText text)
                
                ;; Save to database
                stored (db/save-analysis!
                       {:id analysis-id
                        :user-id user-id
                        :device-id device-id
                        :text text
                        :text-length (:text-length analysis)
                        :source source
                        :source-url source-url
                        :metadata metadata
                        :models-analyzed (:models-analyzed analysis)
                        :successful-analyses (:successful-analyses analysis)
                        :average-score (:average-score analysis)
                        :lollapalooza-detected (:lollapalooza-detected analysis)
                        :convergence-score (:convergence-score analysis)
                        :convergence-count (:convergence-count analysis)
                        :top-10-models (:top-10-models analysis)
                        :all-scores (:all-scores analysis)
                        :created-at (Instant/now)})]
            
            (log/info "Analysis completed:" analysis-id)
            
            (json-response
             {:analysis-id analysis-id
              :text-length (:text-length analysis)
              :models-analyzed (:models-analyzed analysis)
              :successful-analyses (:successful-analyses analysis)
              :average-score (:average-score analysis)
              :top-10-models (mapv #(select-keys % [:model-slug :model-name :score])
                                  (:top-10-models analysis))
              :lollapalooza-detected (:lollapalooza-detected analysis)
              :convergence-score (:convergence-score analysis)
              :convergence-count (:convergence-count analysis)
              :converging-models (mapv #(select-keys % [:model-slug :model-name :score])
                                      (:converging-models analysis))}))
          
          (catch Exception e
            (log/error e "Analysis failed")
            (error-response (str "Analysis failed: " (.getMessage e)) :status 500)))))
    
    (catch Exception e
      (log/error e "Request parsing failed")
      (error-response "Invalid request" :status 400))))

(defn get-analysis
  "GET /api/v1/analyze/:analysis-id - Get previous analysis results"
  [request]
  (try
    (let [analysis-id (get-in request [:path-params :analysis-id])
          user-id (get-in request [:user :id])
          
          analysis (db/get-analysis analysis-id user-id)]
      
      (if analysis
        (json-response
         {:analysis-id (:id analysis)
          :text-length (:text-length analysis)
          :models-analyzed (:models-analyzed analysis)
          :successful-analyses (:successful-analyses analysis)
          :average-score (:average-score analysis)
          :top-10-models (:top-10-models analysis)
          :lollapalooza-detected (:lollapalooza-detected analysis)
          :convergence-score (:convergence-score analysis)
          :convergence-count (:convergence-count analysis)
          :created-at (:created-at analysis)})
        (error-response "Analysis not found" :status 404)))
    
    (catch Exception e
      (log/error e "Failed to retrieve analysis")
      (error-response "Failed to retrieve analysis" :status 500))))

(defn list-analyses
  "GET /api/v1/analyze - List user's recent analyses with pagination"
  [request]
  (try
    (let [params (:query-params request)
          page (Integer/parseInt (get params "page" "1"))
          page-size (Integer/parseInt (get params "page_size" "20"))
          user-id (get-in request [:user :id])
          
          {:keys [items total]} (db/list-analyses user-id page page-size)]
      
      (json-response
       {:items (mapv #(select-keys % [:id :text-length :average-score 
                                      :lollapalooza-detected :convergence-score 
                                      :created-at])
                    items)
        :pagination {:page page
                    :page-size page-size
                    :total total
                    :total-pages (int (Math/ceil (/ total page-size)))
                    :has-next (< (* page page-size) total)
                    :has-prev (> page 1)}}))
    
    (catch Exception e
      (log/error e "Failed to list analyses")
      (error-response "Failed to list analyses" :status 500))))

(defn batch-analyze
  "POST /api/v1/analyze/batch - Analyze multiple texts at once
   
   Request body:
   {
     \"texts\": [
       {\"text\": \"...\", \"source\": \"file\", \"source_url\": \"...\"},
       ...
     ]
   }
   
   Response:
   {
     \"batch_id\": \"uuid\",
     \"analyses\": [...]
   }"
  [request]
  (try
    (let [body (json/parse-string (slurp (:body request)) true)
          texts (get body :texts [])
          batch-id (str (UUID/randomUUID))
          user-id (get-in request [:user :id])
          device-id (get-in request [:user :device-id])]
      
      (if (empty? texts)
        (error-response "Texts array is required" :status 400)
        
        (try
          ;; Analyze each text
          (let [analyses (mapv (fn [text-obj]
                                (let [text (:text text-obj)
                                      analysis (analyzer/AnalyzeText text)]
                                  (assoc analysis
                                        :source (:source text-obj)
                                        :source-url (:source-url text-obj))))
                             texts)]
            
            ;; Save batch to database
            (db/save-batch-analysis!
             {:batch-id batch-id
              :user-id user-id
              :device-id device-id
              :analyses analyses
              :created-at (Instant/now)})
            
            (log/info "Batch analysis completed:" batch-id "with" (count analyses) "items")
            
            (json-response
             {:batch-id batch-id
              :count (count analyses)
              :analyses (mapv #(select-keys % [:text-length :average-score 
                                               :lollapalooza-detected :convergence-score])
                             analyses)}))
          
          (catch Exception e
            (log/error e "Batch analysis failed")
            (error-response (str "Batch analysis failed: " (.getMessage e)) :status 500)))))
    
    (catch Exception e
      (log/error e "Request parsing failed")
      (error-response "Invalid request" :status 400))))

(defn get-top-models
  "GET /api/v1/models/top - Get most frequently detected models across all analyses"
  [request]
  (try
    (let [params (:query-params request)
          limit (Integer/parseInt (get params "limit" "20"))
          user-id (get-in request [:user :id])
          
          top-models (db/get-top-models user-id limit)]
      
      (json-response
       {:top-models (mapv #(select-keys % [:model-slug :model-name :detection-count :avg-score])
                         top-models)}))
    
    (catch Exception e
      (log/error e "Failed to get top models")
      (error-response "Failed to get top models" :status 500))))

(defn get-lollapalooza-events
  "GET /api/v1/lollapalooza - Get Lollapalooza events detected for user"
  [request]
  (try
    (let [params (:query-params request)
          page (Integer/parseInt (get params "page" "1"))
          page-size (Integer/parseInt (get params "page_size" "20"))
          user-id (get-in request [:user :id])
          
          {:keys [items total]} (db/list-lollapalooza-events user-id page page-size)]
      
      (json-response
       {:items (mapv #(select-keys % [:analysis-id :convergence-score :convergence-count 
                                      :converging-models :created-at])
                    items)
        :pagination {:page page
                    :page-size page-size
                    :total total
                    :total-pages (int (Math/ceil (/ total page-size)))
                    :has-next (< (* page page-size) total)
                    :has-prev (> page 1)}}))
    
    (catch Exception e
      (log/error e "Failed to get Lollapalooza events")
      (error-response "Failed to get Lollapalooza events" :status 500))))

;; -- Route Handlers --

(def routes
  [["POST" "/api/v1/analyze" (require-auth analyze-text)]
   ["GET" "/api/v1/analyze/:analysis-id" (require-auth get-analysis)]
   ["GET" "/api/v1/analyze" (require-auth list-analyses)]
   ["POST" "/api/v1/analyze/batch" (require-auth batch-analyze)]
   ["GET" "/api/v1/models/top" (require-auth get-top-models)]
   ["GET" "/api/v1/lollapalooza" (require-auth get-lollapalooza-events)]])
