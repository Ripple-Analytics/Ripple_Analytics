(ns mental-models.pipeline.integration.response-enricher
  "Response enricher for mental model analysis system.
   
   Features:
   - Response metadata enrichment
   - Header enrichment
   - Body enrichment
   - Timing information
   - Request context
   - Custom enrichers
   - Enrichment rules
   - Enrichment metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate ZoneId]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:enrichers {}        ;; enricher-id -> enricher
         :rules {}            ;; rule-id -> rule
         :config {:add-timing? true
                  :add-request-id? true
                  :add-server-info? true
                  :add-version? true
                  :server-name "mental-models-api"
                  :api-version "1.0.0"}
         :stats {:enrichments 0
                 :headers-added 0
                 :body-enrichments 0
                 :errors 0}
         :initialized? false}))

;; ============================================================================
;; Header Enrichment
;; ============================================================================

(defn add-timing-headers
  "Add timing headers to response."
  [response request]
  (let [start-time (get request :start-time (System/currentTimeMillis))
        duration-ms (- (System/currentTimeMillis) start-time)]
    (swap! state update-in [:stats :headers-added] inc)
    (-> response
        (assoc-in [:headers "X-Response-Time"] (str duration-ms "ms"))
        (assoc-in [:headers "X-Request-Start"] (str start-time)))))

(defn add-request-id-header
  "Add request ID header to response."
  [response request]
  (let [request-id (or (:request-id request)
                       (:correlation-id request)
                       (str (UUID/randomUUID)))]
    (swap! state update-in [:stats :headers-added] inc)
    (assoc-in response [:headers "X-Request-ID"] request-id)))

(defn add-server-headers
  "Add server information headers."
  [response]
  (let [server-name (get-in @state [:config :server-name])
        api-version (get-in @state [:config :api-version])]
    (swap! state update-in [:stats :headers-added] + 2)
    (-> response
        (assoc-in [:headers "X-Powered-By"] server-name)
        (assoc-in [:headers "X-API-Version"] api-version))))

(defn add-cache-headers
  "Add cache control headers."
  [response & {:keys [max-age private? no-cache?]
               :or {max-age 0 private? true no-cache? false}}]
  (let [cache-control (cond
                        no-cache? "no-cache, no-store, must-revalidate"
                        private? (str "private, max-age=" max-age)
                        :else (str "public, max-age=" max-age))]
    (swap! state update-in [:stats :headers-added] inc)
    (assoc-in response [:headers "Cache-Control"] cache-control)))

(defn add-security-headers
  "Add security headers."
  [response]
  (swap! state update-in [:stats :headers-added] + 5)
  (-> response
      (assoc-in [:headers "X-Content-Type-Options"] "nosniff")
      (assoc-in [:headers "X-Frame-Options"] "DENY")
      (assoc-in [:headers "X-XSS-Protection"] "1; mode=block")
      (assoc-in [:headers "Referrer-Policy"] "strict-origin-when-cross-origin")
      (assoc-in [:headers "Content-Security-Policy"] "default-src 'self'")))

;; ============================================================================
;; Body Enrichment
;; ============================================================================

(defn enrich-body-with-metadata
  "Enrich response body with metadata."
  [response request]
  (when (and (map? (:body response))
             (not (get-in response [:body :_metadata])))
    (swap! state update-in [:stats :body-enrichments] inc)
    (let [start-time (get request :start-time (System/currentTimeMillis))
          duration-ms (- (System/currentTimeMillis) start-time)
          metadata {:timestamp (str (Instant/now))
                    :request-id (or (:request-id request) (:correlation-id request))
                    :duration-ms duration-ms
                    :path (:uri request)
                    :method (name (or (:method request) :get))}]
      (assoc-in response [:body :_metadata] metadata))))

(defn enrich-body-with-links
  "Enrich response body with HATEOAS links."
  [response links]
  (when (map? (:body response))
    (swap! state update-in [:stats :body-enrichments] inc)
    (assoc-in response [:body :_links] links)))

(defn enrich-body-with-pagination
  "Enrich response body with pagination info."
  [response pagination]
  (when (map? (:body response))
    (swap! state update-in [:stats :body-enrichments] inc)
    (assoc-in response [:body :_pagination] pagination)))

(defn wrap-body-in-envelope
  "Wrap response body in a standard envelope."
  [response & {:keys [success? message]}]
  (when (some? (:body response))
    (swap! state update-in [:stats :body-enrichments] inc)
    (let [status (:status response)
          success (if (some? success?) success? (< status 400))]
      (assoc response :body
             {:success success
              :data (:body response)
              :message message
              :status status}))))

;; ============================================================================
;; Custom Enrichers
;; ============================================================================

(defn register-enricher!
  "Register a custom enricher."
  [enricher-id config]
  (let [enricher {:id enricher-id
                  :name (get config :name (name enricher-id))
                  :type (get config :type :header)  ;; :header, :body, :both
                  :enrich-fn (get config :enrich-fn)
                  :condition-fn (get config :condition-fn (constantly true))
                  :priority (get config :priority 100)
                  :enabled? (atom true)
                  :metrics {:invocations (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:enrichers enricher-id] enricher)
    (logging/log :info "Registered enricher" {:enricher-id enricher-id})
    enricher-id))

(defn get-enricher
  "Get an enricher by ID."
  [enricher-id]
  (get-in @state [:enrichers enricher-id]))

(defn list-enrichers
  "List all enrichers."
  []
  (mapv (fn [[id e]]
          {:id id
           :name (:name e)
           :type (:type e)
           :priority (:priority e)
           :enabled? @(:enabled? e)
           :invocations @(get-in e [:metrics :invocations])})
        (:enrichers @state)))

(defn delete-enricher!
  "Delete an enricher."
  [enricher-id]
  (swap! state update :enrichers dissoc enricher-id))

;; ============================================================================
;; Enrichment Rules
;; ============================================================================

(defn register-rule!
  "Register an enrichment rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :condition-fn (get config :condition-fn (constantly true))
              :enrichers (get config :enrichers [])
              :priority (get config :priority 100)
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    rule-id))

(defn get-rule
  "Get an enrichment rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

;; ============================================================================
;; Full Enrichment
;; ============================================================================

(defn enrich-response
  "Apply all enrichments to a response."
  [response request]
  (swap! state update-in [:stats :enrichments] inc)
  
  (try
    (let [config (:config @state)]
      (cond-> response
        ;; Standard header enrichments
        (:add-timing? config)
        (add-timing-headers request)
        
        (:add-request-id? config)
        (add-request-id-header request)
        
        (:add-server-info? config)
        (add-server-headers)
        
        ;; Body enrichment for JSON responses
        (and (map? (:body response))
             (str/includes? (get-in response [:headers "Content-Type"] "") "json"))
        (enrich-body-with-metadata request)))
    (catch Exception e
      (swap! state update-in [:stats :errors] inc)
      (logging/log :error "Enrichment error" {:error (.getMessage e)})
      response)))

(defn apply-custom-enrichers
  "Apply custom enrichers to a response."
  [response request]
  (let [enrichers (->> (vals (:enrichers @state))
                       (filter #@(:enabled? %))
                       (filter #((:condition-fn %) request response))
                       (sort-by :priority))]
    (reduce (fn [resp enricher]
              (try
                (swap! (get-in enricher [:metrics :invocations]) inc)
                ((:enrich-fn enricher) resp request)
                (catch Exception e
                  (logging/log :error "Custom enricher error"
                               {:enricher-id (:id enricher) :error (.getMessage e)})
                  resp)))
            response
            enrichers)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-enrich-response
  "Ring middleware to enrich responses."
  [handler]
  (fn [request]
    (let [request-with-start (assoc request :start-time (System/currentTimeMillis))
          response (handler request-with-start)]
      (-> response
          (enrich-response request-with-start)
          (apply-custom-enrichers request-with-start)))))

(defn wrap-security-headers
  "Ring middleware to add security headers."
  [handler]
  (fn [request]
    (-> (handler request)
        (add-security-headers))))

(defn wrap-cache-headers
  "Ring middleware to add cache headers."
  [handler & {:keys [max-age private? no-cache?] :as opts}]
  (fn [request]
    (-> (handler request)
        (add-cache-headers opts))))

(defn wrap-envelope
  "Ring middleware to wrap responses in envelope."
  [handler]
  (fn [request]
    (-> (handler request)
        (wrap-body-in-envelope))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-add-timing!
  "Enable/disable timing headers."
  [enabled?]
  (swap! state assoc-in [:config :add-timing?] enabled?))

(defn set-add-request-id!
  "Enable/disable request ID header."
  [enabled?]
  (swap! state assoc-in [:config :add-request-id?] enabled?))

(defn set-add-server-info!
  "Enable/disable server info headers."
  [enabled?]
  (swap! state assoc-in [:config :add-server-info?] enabled?))

(defn set-server-name!
  "Set the server name."
  [name]
  (swap! state assoc-in [:config :server-name] name))

(defn set-api-version!
  "Set the API version."
  [version]
  (swap! state assoc-in [:config :api-version] version))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-enricher-metrics
  "Get enricher metrics."
  []
  (let [stats (:stats @state)]
    {:enrichments (:enrichments stats)
     :headers-added (:headers-added stats)
     :body-enrichments (:body-enrichments stats)
     :errors (:errors stats)
     :enrichers-count (count (:enrichers @state))
     :rules-count (count (:rules @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-enricher-stats
  "Get enricher statistics."
  []
  (merge (get-enricher-metrics)
         {:add-timing? (get-in @state [:config :add-timing?])
          :add-request-id? (get-in @state [:config :add-request-id?])
          :add-server-info? (get-in @state [:config :add-server-info?])
          :server-name (get-in @state [:config :server-name])
          :api-version (get-in @state [:config :api-version])}))

(defn reset-stats!
  "Reset enricher statistics."
  []
  (swap! state assoc :stats {:enrichments 0
                             :headers-added 0
                             :body-enrichments 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-enricher!
  "Initialize the response enricher."
  []
  (when-not (:initialized? @state)
    ;; Register default enrichers
    (register-enricher! :add-timestamp
                        {:name "Add Timestamp"
                         :type :header
                         :enrich-fn (fn [response _]
                                      (assoc-in response [:headers "X-Timestamp"]
                                                (str (Instant/now))))
                         :priority 50})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response enricher initialized")
    (events/emit! :response-enricher-initialized {})
    true))
