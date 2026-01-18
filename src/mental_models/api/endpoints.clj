(ns mental-models.api.endpoints
  "REST API Endpoints for Mental Models Pipeline
   
   Provides HTTP endpoints for:
   - Analysis operations
   - Model queries
   - Health checks
   - Metrics
   - Configuration"
  (:require
   [ring.util.response :as response]
   [cheshire.core :as json]
   [mental-models.registry.models :as registry]
   [mental-models.health.checks :as health]
   [mental-models.benchmark.performance :as perf]
   [mental-models.config.pipeline-config :as config]))

;; =============================================================================
;; RESPONSE HELPERS
;; =============================================================================

(defn json-response [data]
  (-> (response/response (json/generate-string data))
      (response/content-type "application/json")))

(defn error-response [status message]
  (-> (response/response (json/generate-string {:error message}))
      (response/status status)
      (response/content-type "application/json")))

;; =============================================================================
;; ANALYSIS ENDPOINTS
;; =============================================================================

(defn analyze-text [request]
  (let [body (slurp (:body request))
        {:keys [text]} (json/parse-string body true)]
    (if text
      (json-response {:status "queued" :message "Analysis started"})
      (error-response 400 "Missing text field"))))

(defn get-analysis [request]
  (let [id (get-in request [:params :id])]
    (json-response {:id id :status "pending"})))

;; =============================================================================
;; MODEL ENDPOINTS
;; =============================================================================

(defn list-models [_]
  (json-response {:models (registry/get-all-models)
                  :count (registry/get-model-count)}))

(defn get-model [request]
  (let [id (keyword (get-in request [:params :id]))]
    (if-let [model (registry/get-model id)]
      (json-response model)
      (error-response 404 "Model not found"))))

(defn get-model-stats [request]
  (let [id (keyword (get-in request [:params :id]))]
    (if-let [stats (registry/get-model-stats id)]
      (json-response stats)
      (error-response 404 "No stats for model"))))

(defn get-top-models [request]
  (let [limit (or (get-in request [:params :limit]) 10)]
    (json-response {:models (registry/get-top-models :limit limit)})))

;; =============================================================================
;; HEALTH ENDPOINTS
;; =============================================================================

(defn health-check [_]
  (let [result (health/run-all-checks)]
    (if (= :healthy (:overall-status result))
      (json-response result)
      (-> (json-response result)
          (response/status 503)))))

(defn liveness [_]
  (let [result (health/liveness-probe)]
    (-> (json-response result)
        (response/status (:status result)))))

(defn readiness [_]
  (let [result (health/readiness-probe)]
    (-> (json-response result)
        (response/status (:status result)))))

;; =============================================================================
;; METRICS ENDPOINTS
;; =============================================================================

(defn get-metrics [_]
  (json-response (perf/get-benchmark-results)))

(defn get-throughput [_]
  (json-response {:throughput (perf/calculate-throughput)}))

;; =============================================================================
;; CONFIG ENDPOINTS
;; =============================================================================

(defn get-config [_]
  (json-response (config/get-config)))

(defn update-config [request]
  (let [body (slurp (:body request))
        updates (json/parse-string body true)]
    (config/update-config! updates)
    (json-response {:status "updated" :config (config/get-config)})))

;; =============================================================================
;; ROUTES
;; =============================================================================

(def routes
  {"POST /api/analyze" analyze-text
   "GET /api/analysis/:id" get-analysis
   "GET /api/models" list-models
   "GET /api/models/:id" get-model
   "GET /api/models/:id/stats" get-model-stats
   "GET /api/models/top" get-top-models
   "GET /api/health" health-check
   "GET /api/health/live" liveness
   "GET /api/health/ready" readiness
   "GET /api/metrics" get-metrics
   "GET /api/metrics/throughput" get-throughput
   "GET /api/config" get-config
   "PUT /api/config" update-config})
