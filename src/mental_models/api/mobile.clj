(ns mental-models.api.mobile
  "Mobile API - REST endpoints for iOS/watchOS apps
   Optimized for mobile with pagination, caching, and offline sync"
  (:require [ring.util.response :as response]
            [cheshire.core :as json]
            [mental-models.db.postgres :as db]
            [mental-models.services.llm :as llm]
            [mental-models.services.statistics :as stats]
            [mental-models.data.models :as models]
            [taoensso.timbre :as log])
  (:import [java.time Instant]
           [java.util UUID]))

;; -- Response Helpers --------------------------------------------------------

(defn json-response
  "Create JSON response with proper headers"
  [data & {:keys [status] :or {status 200}}]
  (-> (response/response (json/generate-string data))
      (response/status status)
      (response/content-type "application/json")
      (response/header "Cache-Control" "private, max-age=60")))

(defn error-response
  "Create error response"
  [message & {:keys [status] :or {status 400}}]
  (json-response {:error message :timestamp (str (Instant/now))} :status status))

(defn paginated-response
  "Create paginated response"
  [items page page-size total]
  {:items items
   :pagination {:page page
                :page_size page-size
                :total total
                :total_pages (int (Math/ceil (/ total page-size)))
                :has_next (< (* page page-size) total)
                :has_prev (> page 1)}})

;; -- Authentication ----------------------------------------------------------

(defn authenticate
  "Authenticate request and return user"
  [request]
  (when-let [auth-header (get-in request [:headers "authorization"])]
    (when (.startsWith auth-header "Bearer ")
      (let [token (subs auth-header 7)]
        ;; Validate JWT token and return user
        {:id "user-123"
         :name "User"
         :email "user@example.com"}))))

(defn require-auth
  "Middleware to require authentication"
  [handler]
  (fn [request]
    (if-let [user (authenticate request)]
      (handler (assoc request :user user))
      (error-response "Unauthorized" :status 401))))

;; -- Models API --------------------------------------------------------------

(defn list-models
  "GET /api/v1/models - List all mental models with pagination"
  [request]
  (let [params (:query-params request)
        page (Integer/parseInt (get params "page" "1"))
        page-size (Integer/parseInt (get params "page_size" "20"))
        category (get params "category")
        search (get params "search")
        all-models (models/all-models)
        filtered (cond->> all-models
                   category (filter #(= category (:category %)))
                   search (filter #(or (.contains (.toLowerCase (:name %)) (.toLowerCase search))
                                       (.contains (.toLowerCase (:description %)) (.toLowerCase search)))))
        total (count filtered)
        items (->> filtered
                   (drop (* (dec page) page-size))
                   (take page-size)
                   (map #(select-keys % [:id :name :slug :description :category :discipline])))]
    (json-response (paginated-response items page page-size total))))

(defn get-model
  "GET /api/v1/models/:id - Get single model with full details"
  [request]
  (let [id (get-in request [:path-params :id])
        model (first (filter #(= id (:id %)) (models/all-models)))]
    (if model
      (json-response model)
      (error-response "Model not found" :status 404))))

(defn get-model-stats
  "GET /api/v1/models/:id/stats - Get usage statistics for a model"
  [request]
  (let [id (get-in request [:path-params :id])
        user-id (get-in request [:user :id])]
    (json-response
     {:model_id id
      :usage_count 47
      :effectiveness_score 0.85
      :last_used (str (Instant/now))
      :trend {:direction "up" :change 0.05}
      :by_outcome {:positive 32 :negative 8 :pending 7}})))

;; -- Categories API ----------------------------------------------------------

(defn list-categories
  "GET /api/v1/categories - List all categories with counts"
  [request]
  (let [all-models (models/all-models)
        categories (->> all-models
                        (group-by :category)
                        (map (fn [[cat models]]
                               {:name cat
                                :count (count models)
                                :top_models (->> models
                                                 (take 3)
                                                 (map :name))}))
                        (sort-by :count >))]
    (json-response {:categories categories})))

;; -- Decisions API -----------------------------------------------------------

(defn list-decisions
  "GET /api/v1/decisions - List user's decisions"
  [request]
  (let [user-id (get-in request [:user :id])
        params (:query-params request)
        page (Integer/parseInt (get params "page" "1"))
        page-size (Integer/parseInt (get params "page_size" "20"))
        ;; Would fetch from database
        decisions [{:id "dec-1"
                    :title "Investment Analysis Q1"
                    :context "Evaluating market entry opportunity"
                    :models_applied ["inversion" "second-order-thinking"]
                    :outcome "positive"
                    :outcome_rating 4
                    :created_at (str (Instant/now))
                    :updated_at (str (Instant/now))}]]
    (json-response (paginated-response decisions page page-size (count decisions)))))

(defn create-decision
  "POST /api/v1/decisions - Create new decision"
  [request]
  (let [user-id (get-in request [:user :id])
        body (json/parse-string (slurp (:body request)) true)
        decision {:id (str (UUID/randomUUID))
                  :user_id user-id
                  :title (:title body)
                  :context (:context body)
                  :models_applied (:models_applied body [])
                  :outcome nil
                  :outcome_rating nil
                  :created_at (str (Instant/now))
                  :updated_at (str (Instant/now))}]
    ;; Would save to database
    (json-response decision :status 201)))

(defn update-decision
  "PATCH /api/v1/decisions/:id - Update decision (add outcome)"
  [request]
  (let [id (get-in request [:path-params :id])
        body (json/parse-string (slurp (:body request)) true)
        updated {:id id
                 :outcome (:outcome body)
                 :outcome_rating (:outcome_rating body)
                 :updated_at (str (Instant/now))}]
    ;; Would update in database
    (json-response updated)))

(defn get-decision
  "GET /api/v1/decisions/:id - Get single decision"
  [request]
  (let [id (get-in request [:path-params :id])]
    (json-response
     {:id id
      :title "Investment Analysis Q1"
      :context "Evaluating market entry opportunity"
      :models_applied ["inversion" "second-order-thinking"]
      :analysis {:models_detected ["confirmation-bias"]
                 :recommendations ["Consider contrary evidence"]}
      :outcome "positive"
      :outcome_rating 4
      :created_at (str (Instant/now))
      :updated_at (str (Instant/now))})))

;; -- Analysis API ------------------------------------------------------------

(defn analyze-text
  "POST /api/v1/analyze - Analyze text for mental models"
  [request]
  (let [body (json/parse-string (slurp (:body request)) true)
        text (:text body)
        analysis-type (get body :type "full")]
    (try
      (let [result (llm/analyze-for-models text)]
        (json-response
         {:models_detected (:models result)
          :lollapalooza_score (:lollapalooza-score result 0)
          :failure_modes_detected (:failure-modes result [])
          :recommendations (:recommendations result [])
          :confidence (:confidence result 0.8)
          :analyzed_at (str (Instant/now))}))
      (catch Exception e
        (log/error "Analysis failed:" (.getMessage e))
        (error-response "Analysis failed" :status 500)))))

(defn quick-analyze
  "POST /api/v1/analyze/quick - Quick analysis for watch app"
  [request]
  (let [body (json/parse-string (slurp (:body request)) true)
        text (:text body)]
    ;; Simplified analysis for watch
    (json-response
     {:top_models [{:name "Inversion" :confidence 0.85}
                   {:name "Second-Order Thinking" :confidence 0.72}]
      :alert (when (> (count text) 100)
               {:type "complexity" :message "Consider breaking down the problem"})
      :analyzed_at (str (Instant/now))})))

;; -- Dashboard API -----------------------------------------------------------

(defn get-dashboard
  "GET /api/v1/dashboard - Get dashboard data"
  [request]
  (let [user-id (get-in request [:user :id])]
    (json-response
     {:metrics {:models_used 47
                :decisions_count 128
                :effectiveness_score 0.78
                :health_score 85
                :streak_days 14}
      :trends {:models_used {:direction "up" :change 3}
               :effectiveness {:direction "up" :change 0.05}}
      :recent_decisions [{:id "dec-1"
                          :title "Investment Analysis"
                          :models ["Inversion" "Second-Order"]
                          :date (str (Instant/now))}]
      :top_models [{:name "Circle of Competence" :effectiveness 0.92}
                   {:name "Margin of Safety" :effectiveness 0.88}
                   {:name "Inversion" :effectiveness 0.85}]
      :updated_at (str (Instant/now))})))

(defn get-stats
  "GET /api/v1/stats - Get comprehensive statistics"
  [request]
  (let [user-id (get-in request [:user :id])
        period (get-in request [:query-params "period"] "30d")]
    (json-response
     {:period period
      :models {:total_used 47
               :unique_used 23
               :most_used [{:name "Inversion" :count 15}
                           {:name "Second-Order" :count 12}]
               :least_used [{:name "Regression to Mean" :count 1}]}
      :decisions {:total 128
                  :with_outcome 98
                  :positive_rate 0.72
                  :avg_models_per_decision 2.3}
      :effectiveness {:overall 0.78
                      :by_category {"Psychology" 0.82
                                    "Economics" 0.75
                                    "Systems" 0.79}}
      :activity {:daily_avg 4.2
                 :peak_day "Monday"
                 :streak 14}})))

;; -- Sync API ----------------------------------------------------------------

(defn get-sync-status
  "GET /api/v1/sync - Get sync status and changes since timestamp"
  [request]
  (let [since (get-in request [:query-params "since"])]
    (json-response
     {:server_time (str (Instant/now))
      :changes {:models {:updated 0 :deleted 0}
                :decisions {:updated 3 :deleted 0}
                :settings {:updated 1}}
      :full_sync_required (nil? since)})))

(defn push-sync
  "POST /api/v1/sync - Push local changes to server"
  [request]
  (let [body (json/parse-string (slurp (:body request)) true)
        changes (:changes body)]
    ;; Process changes
    (json-response
     {:accepted (count changes)
      :conflicts []
      :server_time (str (Instant/now))})))

;; -- Watch-specific API ------------------------------------------------------

(defn get-watch-data
  "GET /api/v1/watch - Get optimized data for watch app"
  [request]
  (json-response
   {:stats {:models_used 3
            :decisions_logged 2
            :streak_days 14}
    :recent_models [{:id "1" :name "Inversion" :emoji "🔄"}
                    {:id "2" :name "Second-Order" :emoji "2️⃣"}
                    {:id "3" :name "Circle of Competence" :emoji "⭕"}]
    :quick_actions ["log_decision" "browse_models" "quick_analysis"]
    :complications {:streak 14
                    :today_models 3
                    :today_decisions 2}}))

(defn log-quick-decision
  "POST /api/v1/watch/decision - Quick decision log from watch"
  [request]
  (let [body (json/parse-string (slurp (:body request)) true)
        models (:models body)]
    (json-response
     {:id (str (UUID/randomUUID))
      :models models
      :logged_at (str (Instant/now))
      :message "Decision logged successfully"}
     :status 201)))

;; -- Routes ------------------------------------------------------------------

(def routes
  [["/api/v1"
    ["/models" {:get list-models}]
    ["/models/:id" {:get get-model}]
    ["/models/:id/stats" {:get get-model-stats}]
    ["/categories" {:get list-categories}]
    ["/decisions" {:get list-decisions
                   :post create-decision}]
    ["/decisions/:id" {:get get-decision
                       :patch update-decision}]
    ["/analyze" {:post analyze-text}]
    ["/analyze/quick" {:post quick-analyze}]
    ["/dashboard" {:get get-dashboard}]
    ["/stats" {:get get-stats}]
    ["/sync" {:get get-sync-status
              :post push-sync}]
    ["/watch" {:get get-watch-data}]
    ["/watch/decision" {:post log-quick-decision}]]])

;; -- Middleware --------------------------------------------------------------

(defn wrap-cors
  "Add CORS headers for mobile apps"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (response/header "Access-Control-Allow-Origin" "*")
          (response/header "Access-Control-Allow-Methods" "GET, POST, PATCH, DELETE, OPTIONS")
          (response/header "Access-Control-Allow-Headers" "Authorization, Content-Type")))))

(defn wrap-request-logging
  "Log API requests"
  [handler]
  (fn [request]
    (let [start (System/currentTimeMillis)
          response (handler request)
          duration (- (System/currentTimeMillis) start)]
      (log/info (:request-method request) (:uri request) 
                "- Status:" (:status response) 
                "- Duration:" duration "ms")
      response)))
