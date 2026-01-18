(ns mental-models.pipeline.integration.api-client
  "API Client Integration
   
   Unified API client for external service calls:
   - HTTP client with connection pooling
   - Retry logic with exponential backoff
   - Circuit breaker integration
   - Request/response logging
   - Rate limiting"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clj-http.client :as http]
   [clojure.data.json :as json])
  (:import
   [java.util.concurrent Semaphore TimeUnit]))

;; =============================================================================
;; CLIENT CONFIGURATION
;; =============================================================================

(def default-config
  {:connection-timeout 5000
   :socket-timeout 30000
   :max-connections 100
   :max-connections-per-route 20
   :retry-max-attempts 3
   :retry-initial-delay-ms 1000
   :retry-multiplier 2.0
   :retry-max-delay-ms 30000
   :rate-limit-permits 100
   :rate-limit-period-ms 1000})

(defonce client-state (atom {:config default-config
                             :rate-limiter nil
                             :stats {:requests 0
                                     :successes 0
                                     :failures 0
                                     :retries 0}}))

;; =============================================================================
;; RATE LIMITING
;; =============================================================================

(defn create-rate-limiter [permits period-ms]
  (let [semaphore (Semaphore. permits true)]
    {:semaphore semaphore
     :permits permits
     :period-ms period-ms}))

(defn acquire-permit! []
  (when-let [limiter (:rate-limiter @client-state)]
    (.tryAcquire (:semaphore limiter) 1 TimeUnit/SECONDS)))

(defn release-permit! []
  (when-let [limiter (:rate-limiter @client-state)]
    (.release (:semaphore limiter))))

;; =============================================================================
;; RETRY LOGIC
;; =============================================================================

(defn calculate-delay [attempt config]
  (let [delay (* (:retry-initial-delay-ms config)
                 (Math/pow (:retry-multiplier config) (dec attempt)))]
    (min delay (:retry-max-delay-ms config))))

(defn should-retry? [response attempt config]
  (and (< attempt (:retry-max-attempts config))
       (or (nil? response)
           (>= (:status response 0) 500)
           (= (:status response) 429))))

(defn with-retry [f config]
  (loop [attempt 1]
    (let [result (try
                   {:success true :response (f)}
                   (catch Exception e
                     {:success false :error e}))]
      (if (:success result)
        (:response result)
        (if (should-retry? nil attempt config)
          (do
            (swap! client-state update-in [:stats :retries] inc)
            (Thread/sleep (calculate-delay attempt config))
            (recur (inc attempt)))
          (throw (:error result)))))))

;; =============================================================================
;; HTTP METHODS
;; =============================================================================

(defn build-request-options [options]
  (let [config (:config @client-state)]
    (merge {:conn-timeout (:connection-timeout config)
            :socket-timeout (:socket-timeout config)
            :throw-exceptions false
            :as :auto}
           options)))

(defn log-request [method url options]
  (log/debug "API request" {:method method :url url :headers (dissoc (:headers options) "Authorization")}))

(defn log-response [method url response duration-ms]
  (log/debug "API response" {:method method :url url :status (:status response) :duration-ms duration-ms}))

(defn execute-request [method url options]
  (when (flags/is-enabled? "api-client")
    (when (acquire-permit!)
      (try
        (let [start-time (System/currentTimeMillis)
              config (:config @client-state)
              request-opts (build-request-options options)]
          (log-request method url options)
          (swap! client-state update-in [:stats :requests] inc)
          (let [response (with-retry
                           #(case method
                              :get (http/get url request-opts)
                              :post (http/post url request-opts)
                              :put (http/put url request-opts)
                              :patch (http/patch url request-opts)
                              :delete (http/delete url request-opts))
                           config)
                duration (- (System/currentTimeMillis) start-time)]
            (log-response method url response duration)
            ;; Record metrics
            (metrics/inc-counter! :api-client/requests)
            (metrics/observe-histogram! :api-client/request-duration duration)
            (if (< (:status response) 400)
              (do
                (swap! client-state update-in [:stats :successes] inc)
                (metrics/inc-counter! :api-client/successes))
              (do
                (swap! client-state update-in [:stats :failures] inc)
                (metrics/inc-counter! :api-client/failures)))
            ;; Audit
            (audit/log-operation! {:operation :api-request
                                   :method method
                                   :url url
                                   :status (:status response)
                                   :duration-ms duration})
            response))
        (finally
          (release-permit!))))))

;; =============================================================================
;; PUBLIC API
;; =============================================================================

(defn get-request
  "Make a GET request."
  [url & {:keys [headers query-params] :as options}]
  (execute-request :get url options))

(defn post-request
  "Make a POST request."
  [url body & {:keys [headers content-type] :or {content-type :json} :as options}]
  (let [opts (assoc options
                    :body (if (= content-type :json) (json/write-str body) body)
                    :content-type content-type)]
    (execute-request :post url opts)))

(defn put-request
  "Make a PUT request."
  [url body & {:keys [headers content-type] :or {content-type :json} :as options}]
  (let [opts (assoc options
                    :body (if (= content-type :json) (json/write-str body) body)
                    :content-type content-type)]
    (execute-request :put url opts)))

(defn patch-request
  "Make a PATCH request."
  [url body & {:keys [headers content-type] :or {content-type :json} :as options}]
  (let [opts (assoc options
                    :body (if (= content-type :json) (json/write-str body) body)
                    :content-type content-type)]
    (execute-request :patch url opts)))

(defn delete-request
  "Make a DELETE request."
  [url & {:keys [headers] :as options}]
  (execute-request :delete url options))

;; =============================================================================
;; CIRCUIT BREAKER WRAPPED REQUESTS
;; =============================================================================

(defn with-circuit-breaker
  "Execute request with circuit breaker protection."
  [circuit-breaker-name request-fn]
  (cb/with-circuit-breaker circuit-breaker-name request-fn))

(defn get-with-cb
  "GET request with circuit breaker."
  [circuit-breaker-name url & options]
  (with-circuit-breaker circuit-breaker-name
    #(apply get-request url options)))

(defn post-with-cb
  "POST request with circuit breaker."
  [circuit-breaker-name url body & options]
  (with-circuit-breaker circuit-breaker-name
    #(apply post-request url body options)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-api-client!
  "Initialize API client."
  [& {:keys [config] :or {config {}}}]
  (log/info "Initializing API client")
  (let [merged-config (merge default-config config)]
    (swap! client-state assoc
           :config merged-config
           :rate-limiter (create-rate-limiter
                          (:rate-limit-permits merged-config)
                          (:rate-limit-period-ms merged-config))))
  ;; Register feature flag
  (flags/register-flag! "api-client" "Enable API client" true)
  ;; Create metrics
  (metrics/create-counter! :api-client/requests "API requests")
  (metrics/create-counter! :api-client/successes "Successful requests")
  (metrics/create-counter! :api-client/failures "Failed requests")
  (metrics/create-histogram! :api-client/request-duration "Request duration" [50 100 500 1000 5000])
  (log/info "API client initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-client-status []
  {:enabled (flags/is-enabled? "api-client")
   :config (:config @client-state)
   :stats (:stats @client-state)})
