(ns mental-models.pipeline.integration.request-logging
  "Request logging for mental model analysis system.
   
   Features:
   - Structured request/response logging
   - Sensitive data masking
   - Performance timing
   - Error logging
   - Log levels
   - Request sampling
   - Log formatting
   - Async logging"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:enabled? true
                  :log-request-body? true
                  :log-response-body? false
                  :max-body-length 10000
                  :sensitive-headers #{"authorization" "cookie" "x-api-key" "x-auth-token"}
                  :sensitive-params #{"password" "token" "secret" "api_key" "apikey"}
                  :sample-rate 1.0
                  :slow-request-threshold-ms 1000
                  :format :json}
         :log-channel (async/chan 1000)
         :stats {:requests-logged 0
                 :errors-logged 0
                 :slow-requests 0
                 :sampled-out 0}
         :initialized? false}))

;; ============================================================================
;; Sensitive Data Masking
;; ============================================================================

(defn- mask-value
  "Mask a sensitive value."
  [value]
  (if (and value (string? value) (> (count value) 4))
    (str (subs value 0 2) "***" (subs value (- (count value) 2)))
    "***"))

(defn- mask-headers
  "Mask sensitive headers."
  [headers]
  (let [sensitive (get-in @state [:config :sensitive-headers])]
    (reduce (fn [h [k v]]
              (if (contains? sensitive (str/lower-case (name k)))
                (assoc h k (mask-value v))
                (assoc h k v)))
            {}
            headers)))

(defn- mask-params
  "Mask sensitive parameters."
  [params]
  (let [sensitive (get-in @state [:config :sensitive-params])]
    (reduce (fn [p [k v]]
              (if (contains? sensitive (str/lower-case (name k)))
                (assoc p k (mask-value v))
                (assoc p k v)))
            {}
            params)))

(defn- truncate-body
  "Truncate body if too long."
  [body]
  (let [max-length (get-in @state [:config :max-body-length])]
    (if (and body (string? body) (> (count body) max-length))
      (str (subs body 0 max-length) "...[truncated]")
      body)))

;; ============================================================================
;; Log Entry Creation
;; ============================================================================

(defn- create-request-log
  "Create a request log entry."
  [request start-time]
  {:type :request
   :timestamp (System/currentTimeMillis)
   :request-id (or (get-in request [:headers "x-request-id"])
                   (str (UUID/randomUUID)))
   :method (name (or (:request-method request) :unknown))
   :uri (:uri request)
   :query-string (:query-string request)
   :remote-addr (:remote-addr request)
   :headers (mask-headers (:headers request))
   :params (mask-params (:params request))
   :body (when (get-in @state [:config :log-request-body?])
           (truncate-body (:body request)))
   :start-time start-time})

(defn- create-response-log
  "Create a response log entry."
  [response request-log duration-ms]
  {:type :response
   :timestamp (System/currentTimeMillis)
   :request-id (:request-id request-log)
   :status (:status response)
   :headers (mask-headers (:headers response))
   :body (when (get-in @state [:config :log-response-body?])
           (truncate-body (str (:body response))))
   :duration-ms duration-ms
   :slow? (> duration-ms (get-in @state [:config :slow-request-threshold-ms]))})

(defn- create-error-log
  "Create an error log entry."
  [error request-log duration-ms]
  {:type :error
   :timestamp (System/currentTimeMillis)
   :request-id (:request-id request-log)
   :error-type (type error)
   :error-message (.getMessage error)
   :stack-trace (with-out-str (.printStackTrace error (java.io.PrintWriter. *out*)))
   :duration-ms duration-ms})

;; ============================================================================
;; Log Formatting
;; ============================================================================

(defn- format-log-json
  "Format log entry as JSON."
  [entry]
  (pr-str entry))

(defn- format-log-text
  "Format log entry as text."
  [entry]
  (case (:type entry)
    :request
    (format "[%s] %s %s %s - %s"
            (LocalDateTime/now)
            (:method entry)
            (:uri entry)
            (:remote-addr entry)
            (:request-id entry))
    
    :response
    (format "[%s] %s %dms - %s"
            (LocalDateTime/now)
            (:status entry)
            (:duration-ms entry)
            (:request-id entry))
    
    :error
    (format "[%s] ERROR %s: %s - %s"
            (LocalDateTime/now)
            (:error-type entry)
            (:error-message entry)
            (:request-id entry))
    
    (pr-str entry)))

(defn- format-log
  "Format log entry based on configuration."
  [entry]
  (case (get-in @state [:config :format])
    :json (format-log-json entry)
    :text (format-log-text entry)
    (format-log-json entry)))

;; ============================================================================
;; Async Logging
;; ============================================================================

(defn- start-log-processor!
  "Start the async log processor."
  []
  (go-loop []
    (when-let [entry (<! (:log-channel @state))]
      (try
        (let [formatted (format-log entry)]
          (case (:type entry)
            :error (logging/log :error formatted)
            :response (if (:slow? entry)
                        (logging/log :warn formatted)
                        (logging/log :info formatted))
            (logging/log :info formatted)))
        (catch Exception e
          (logging/log :error "Error processing log entry" {:error (.getMessage e)})))
      (recur))))

(defn- queue-log!
  "Queue a log entry for async processing."
  [entry]
  (async/put! (:log-channel @state) entry))

;; ============================================================================
;; Sampling
;; ============================================================================

(defn- should-log?
  "Check if request should be logged based on sample rate."
  []
  (let [sample-rate (get-in @state [:config :sample-rate])]
    (if (>= sample-rate 1.0)
      true
      (< (rand) sample-rate))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn request-logging-middleware
  "Ring middleware for request logging."
  [handler]
  (fn [request]
    (if-not (get-in @state [:config :enabled?])
      (handler request)
      (if-not (should-log?)
        (do
          (swap! state update-in [:stats :sampled-out] inc)
          (handler request))
        (let [start-time (System/currentTimeMillis)
              request-log (create-request-log request start-time)]
          
          ;; Log request
          (queue-log! request-log)
          (swap! state update-in [:stats :requests-logged] inc)
          
          (try
            (let [response (handler request)
                  duration-ms (- (System/currentTimeMillis) start-time)
                  response-log (create-response-log response request-log duration-ms)]
              
              ;; Log response
              (queue-log! response-log)
              
              ;; Track slow requests
              (when (:slow? response-log)
                (swap! state update-in [:stats :slow-requests] inc))
              
              ;; Add timing header
              (assoc-in response [:headers "X-Response-Time"] (str duration-ms "ms")))
            
            (catch Exception e
              (let [duration-ms (- (System/currentTimeMillis) start-time)
                    error-log (create-error-log e request-log duration-ms)]
                
                ;; Log error
                (queue-log! error-log)
                (swap! state update-in [:stats :errors-logged] inc)
                
                (throw e)))))))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn enable-logging!
  "Enable request logging."
  []
  (swap! state assoc-in [:config :enabled?] true))

(defn disable-logging!
  "Disable request logging."
  []
  (swap! state assoc-in [:config :enabled?] false))

(defn set-sample-rate!
  "Set logging sample rate (0.0 to 1.0)."
  [rate]
  (swap! state assoc-in [:config :sample-rate] (max 0.0 (min 1.0 rate))))

(defn set-slow-threshold!
  "Set slow request threshold in milliseconds."
  [ms]
  (swap! state assoc-in [:config :slow-request-threshold-ms] ms))

(defn set-log-format!
  "Set log format (:json or :text)."
  [format]
  (swap! state assoc-in [:config :format] format))

(defn add-sensitive-header!
  "Add a sensitive header to mask."
  [header]
  (swap! state update-in [:config :sensitive-headers] conj (str/lower-case header)))

(defn add-sensitive-param!
  "Add a sensitive parameter to mask."
  [param]
  (swap! state update-in [:config :sensitive-params] conj (str/lower-case param)))

(defn enable-request-body-logging!
  "Enable request body logging."
  []
  (swap! state assoc-in [:config :log-request-body?] true))

(defn disable-request-body-logging!
  "Disable request body logging."
  []
  (swap! state assoc-in [:config :log-request-body?] false))

(defn enable-response-body-logging!
  "Enable response body logging."
  []
  (swap! state assoc-in [:config :log-response-body?] true))

(defn disable-response-body-logging!
  "Disable response body logging."
  []
  (swap! state assoc-in [:config :log-response-body?] false))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-logging-stats
  "Get request logging statistics."
  []
  (let [stats (:stats @state)]
    {:requests-logged (:requests-logged stats)
     :errors-logged (:errors-logged stats)
     :slow-requests (:slow-requests stats)
     :sampled-out (:sampled-out stats)
     :sample-rate (get-in @state [:config :sample-rate])
     :slow-threshold-ms (get-in @state [:config :slow-request-threshold-ms])}))

(defn reset-stats!
  "Reset logging statistics."
  []
  (swap! state assoc :stats {:requests-logged 0
                             :errors-logged 0
                             :slow-requests 0
                             :sampled-out 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-logging!
  "Initialize the request logging system."
  []
  (when-not (:initialized? @state)
    ;; Start log processor
    (start-log-processor!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request logging initialized")
    (events/emit! :request-logging-initialized {})
    true))
