(ns mental-models.pipeline.integration.cors-handler
  "CORS handler for mental model analysis system.
   
   Features:
   - Origin validation
   - Preflight handling
   - Credentials support
   - Custom headers
   - Max age configuration
   - Origin patterns
   - Method filtering
   - Expose headers"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:allowed-origins #{}
                  :allowed-origin-patterns []
                  :allow-all-origins? false
                  :allowed-methods #{:get :post :put :delete :patch :options :head}
                  :allowed-headers #{"Content-Type" "Authorization" "X-Requested-With"
                                     "Accept" "Origin" "X-Request-ID" "X-Correlation-ID"}
                  :exposed-headers #{"X-Request-ID" "X-Correlation-ID" "X-RateLimit-Limit"
                                     "X-RateLimit-Remaining" "X-RateLimit-Reset"}
                  :allow-credentials? true
                  :max-age-seconds 86400
                  :preflight-continue? false}
         :stats {:requests-handled 0
                 :preflight-requests 0
                 :blocked-origins 0}
         :initialized? false}))

;; ============================================================================
;; Origin Validation
;; ============================================================================

(defn add-allowed-origin!
  "Add an allowed origin."
  [origin]
  (swap! state update-in [:config :allowed-origins] conj origin)
  (logging/log :info "Added allowed origin" {:origin origin}))

(defn remove-allowed-origin!
  "Remove an allowed origin."
  [origin]
  (swap! state update-in [:config :allowed-origins] disj origin))

(defn add-origin-pattern!
  "Add an origin pattern (regex)."
  [pattern]
  (let [regex (if (instance? Pattern pattern)
                pattern
                (re-pattern pattern))]
    (swap! state update-in [:config :allowed-origin-patterns] conj regex)))

(defn- origin-matches-pattern?
  "Check if origin matches any pattern."
  [origin patterns]
  (some #(re-matches % origin) patterns))

(defn origin-allowed?
  "Check if an origin is allowed."
  [origin]
  (let [config (:config @state)]
    (or (:allow-all-origins? config)
        (contains? (:allowed-origins config) origin)
        (origin-matches-pattern? origin (:allowed-origin-patterns config)))))

(defn set-allow-all-origins!
  "Enable/disable allowing all origins."
  [allow?]
  (swap! state assoc-in [:config :allow-all-origins?] allow?)
  (when allow?
    (logging/log :warn "CORS: Allowing all origins - use with caution")))

;; ============================================================================
;; Method Configuration
;; ============================================================================

(defn add-allowed-method!
  "Add an allowed HTTP method."
  [method]
  (swap! state update-in [:config :allowed-methods] conj (keyword (str/lower-case (name method)))))

(defn remove-allowed-method!
  "Remove an allowed HTTP method."
  [method]
  (swap! state update-in [:config :allowed-methods] disj (keyword (str/lower-case (name method)))))

(defn method-allowed?
  "Check if a method is allowed."
  [method]
  (contains? (get-in @state [:config :allowed-methods])
             (keyword (str/lower-case (name method)))))

;; ============================================================================
;; Header Configuration
;; ============================================================================

(defn add-allowed-header!
  "Add an allowed request header."
  [header]
  (swap! state update-in [:config :allowed-headers] conj header))

(defn remove-allowed-header!
  "Remove an allowed request header."
  [header]
  (swap! state update-in [:config :allowed-headers] disj header))

(defn add-exposed-header!
  "Add an exposed response header."
  [header]
  (swap! state update-in [:config :exposed-headers] conj header))

(defn remove-exposed-header!
  "Remove an exposed response header."
  [header]
  (swap! state update-in [:config :exposed-headers] disj header))

;; ============================================================================
;; Credentials Configuration
;; ============================================================================

(defn set-allow-credentials!
  "Enable/disable credentials support."
  [allow?]
  (swap! state assoc-in [:config :allow-credentials?] allow?))

(defn set-max-age!
  "Set preflight cache max age in seconds."
  [seconds]
  (swap! state assoc-in [:config :max-age-seconds] seconds))

;; ============================================================================
;; CORS Headers
;; ============================================================================

(defn- get-cors-headers
  "Get CORS headers for a response."
  [origin]
  (let [config (:config @state)]
    (cond-> {}
      ;; Origin
      (origin-allowed? origin)
      (assoc "Access-Control-Allow-Origin" 
             (if (:allow-all-origins? config) "*" origin))
      
      ;; Credentials
      (and (:allow-credentials? config) (not (:allow-all-origins? config)))
      (assoc "Access-Control-Allow-Credentials" "true")
      
      ;; Exposed headers
      (seq (:exposed-headers config))
      (assoc "Access-Control-Expose-Headers" 
             (str/join ", " (:exposed-headers config)))
      
      ;; Vary header for caching
      true
      (assoc "Vary" "Origin"))))

(defn- get-preflight-headers
  "Get headers for preflight response."
  [origin]
  (let [config (:config @state)
        base-headers (get-cors-headers origin)]
    (merge base-headers
           {"Access-Control-Allow-Methods" 
            (str/join ", " (map #(str/upper-case (name %)) (:allowed-methods config)))
            
            "Access-Control-Allow-Headers"
            (str/join ", " (:allowed-headers config))
            
            "Access-Control-Max-Age"
            (str (:max-age-seconds config))})))

;; ============================================================================
;; Request Handling
;; ============================================================================

(defn- preflight-request?
  "Check if request is a CORS preflight."
  [request]
  (and (= :options (:request-method request))
       (get-in request [:headers "access-control-request-method"])))

(defn- handle-preflight
  "Handle CORS preflight request."
  [request]
  (let [origin (get-in request [:headers "origin"])
        requested-method (get-in request [:headers "access-control-request-method"])
        requested-headers (get-in request [:headers "access-control-request-headers"])]
    
    (swap! state update-in [:stats :preflight-requests] inc)
    
    (if (and (origin-allowed? origin)
             (method-allowed? requested-method))
      {:status 204
       :headers (get-preflight-headers origin)
       :body ""}
      (do
        (swap! state update-in [:stats :blocked-origins] inc)
        (logging/log :warn "CORS preflight blocked" {:origin origin :method requested-method})
        {:status 403
         :headers {}
         :body "CORS preflight check failed"}))))

(defn- add-cors-headers
  "Add CORS headers to response."
  [response origin]
  (if (origin-allowed? origin)
    (update response :headers merge (get-cors-headers origin))
    (do
      (swap! state update-in [:stats :blocked-origins] inc)
      response)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn cors-middleware
  "Ring middleware for CORS handling."
  [handler]
  (fn [request]
    (swap! state update-in [:stats :requests-handled] inc)
    (let [origin (get-in request [:headers "origin"])]
      (cond
        ;; No origin header - not a CORS request
        (nil? origin)
        (handler request)
        
        ;; Preflight request
        (preflight-request? request)
        (handle-preflight request)
        
        ;; Regular CORS request
        :else
        (-> (handler request)
            (add-cors-headers origin))))))

(defn wrap-cors
  "Wrap handler with CORS support using provided configuration."
  [handler & {:keys [origins methods headers credentials? max-age]}]
  (when origins
    (doseq [origin origins]
      (add-allowed-origin! origin)))
  (when methods
    (doseq [method methods]
      (add-allowed-method! method)))
  (when headers
    (doseq [header headers]
      (add-allowed-header! header)))
  (when (some? credentials?)
    (set-allow-credentials! credentials?))
  (when max-age
    (set-max-age! max-age))
  
  (cors-middleware handler))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn get-cors-config
  "Get current CORS configuration."
  []
  (:config @state))

(defn reset-cors-config!
  "Reset CORS configuration to defaults."
  []
  (swap! state assoc :config
         {:allowed-origins #{}
          :allowed-origin-patterns []
          :allow-all-origins? false
          :allowed-methods #{:get :post :put :delete :patch :options :head}
          :allowed-headers #{"Content-Type" "Authorization" "X-Requested-With"
                             "Accept" "Origin" "X-Request-ID" "X-Correlation-ID"}
          :exposed-headers #{"X-Request-ID" "X-Correlation-ID" "X-RateLimit-Limit"
                             "X-RateLimit-Remaining" "X-RateLimit-Reset"}
          :allow-credentials? true
          :max-age-seconds 86400
          :preflight-continue? false}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-cors-stats
  "Get CORS statistics."
  []
  (let [stats (:stats @state)]
    {:requests-handled (:requests-handled stats)
     :preflight-requests (:preflight-requests stats)
     :blocked-origins (:blocked-origins stats)
     :allowed-origins-count (count (get-in @state [:config :allowed-origins]))
     :patterns-count (count (get-in @state [:config :allowed-origin-patterns]))}))

(defn reset-stats!
  "Reset CORS statistics."
  []
  (swap! state assoc :stats {:requests-handled 0
                             :preflight-requests 0
                             :blocked-origins 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-cors-handler!
  "Initialize the CORS handler."
  []
  (when-not (:initialized? @state)
    ;; Add common development origins
    (add-allowed-origin! "http://localhost:3000")
    (add-allowed-origin! "http://localhost:8080")
    (add-allowed-origin! "http://127.0.0.1:3000")
    (add-allowed-origin! "http://127.0.0.1:8080")
    
    ;; Add pattern for localhost with any port
    (add-origin-pattern! #"http://localhost:\d+")
    (add-origin-pattern! #"http://127\.0\.0\.1:\d+")
    
    (swap! state assoc :initialized? true)
    (logging/log :info "CORS handler initialized")
    (events/emit! :cors-handler-initialized {})
    true))
