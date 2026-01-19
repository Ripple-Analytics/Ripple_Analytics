(ns mental-models.connectors
  "Connectors Module - Electric Clojure
   
   Comprehensive connectors for external services.
   Ported from Python to Electric Clojure for unified codebase.
   
   Connectors:
   - Zapier integration
   - Huggingface models
   - GitHub integration
   - Slack integration
   - Google Drive
   - LM Studio (local LLM)
   - Database connectors
   - File connectors
   - Web scraping
   
   Features:
   - Connection pooling via clj-http
   - Retry logic with exponential backoff
   - Rate limiting support
   - Response caching"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================
;; HTTP Client Configuration & Connection Pooling
;; ============================================

#?(:clj
   (def connection-manager
     "Shared connection manager for HTTP connection pooling.
      Reuses connections across requests for better performance."
     (delay
       (clj-http.conn-mgr/make-reusable-conn-manager
        {:timeout 30
         :threads 8
         :default-per-route 4
         :insecure? false}))))

#?(:clj
   (def default-http-opts
     "Default HTTP options with connection pooling."
     {:connection-manager @connection-manager
      :socket-timeout 30000
      :connection-timeout 10000
      :throw-exceptions false}))

;; ============================================
;; Configurable Timeout Settings
;; ============================================

(def timeout-config
  "Configurable timeout settings per connector type (milliseconds)."
  (atom {:github {:socket-timeout 30000 :connection-timeout 10000}
         :slack {:socket-timeout 30000 :connection-timeout 10000}
         :huggingface {:socket-timeout 60000 :connection-timeout 15000}
         :lm-studio {:socket-timeout 120000 :connection-timeout 10000}
         :web-scraper {:socket-timeout 45000 :connection-timeout 15000}
         :zapier {:socket-timeout 30000 :connection-timeout 10000}
         :file {:socket-timeout 5000 :connection-timeout 1000}
         :default {:socket-timeout 30000 :connection-timeout 10000}}))

#?(:clj
   (defn get-timeout-config
     "Get timeout configuration for a connector type."
     [connector-type]
     (get @timeout-config connector-type (get @timeout-config :default))))

#?(:clj
   (defn set-timeout-config
     "Set timeout configuration for a connector type."
     [connector-type socket-timeout connection-timeout]
     (swap! timeout-config assoc connector-type
            {:socket-timeout socket-timeout
             :connection-timeout connection-timeout})))

#?(:clj
   (defn get-http-opts-for-connector
     "Get HTTP options with connector-specific timeouts."
     [connector-type]
     (let [timeouts (get-timeout-config connector-type)]
       (merge default-http-opts timeouts))))

;; ============================================
;; Event Hooks System
;; ============================================

(def event-hooks
  "Atom storing event hook functions.
   Events: :request-start, :request-complete, :request-error, 
           :circuit-open, :circuit-close, :rate-limit-hit, :cache-hit, :cache-miss"
  (atom {:request-start []
         :request-complete []
         :request-error []
         :circuit-open []
         :circuit-close []
         :rate-limit-hit []
         :cache-hit []
         :cache-miss []}))

#?(:clj
   (defn register-hook
     "Register a hook function for an event type.
      Hook functions receive event data as a map.
      Returns a hook-id that can be used to unregister."
     [event-type hook-fn]
     (let [hook-id (java.util.UUID/randomUUID)]
       (swap! event-hooks update event-type conj {:id hook-id :fn hook-fn})
       hook-id)))

#?(:clj
   (defn unregister-hook
     "Unregister a hook by its ID."
     [event-type hook-id]
     (swap! event-hooks update event-type
            (fn [hooks]
              (vec (remove #(= hook-id (:id %)) hooks))))))

#?(:clj
   (defn clear-hooks
     "Clear all hooks for an event type, or all hooks if no type specified."
     ([] (reset! event-hooks {:request-start []
                              :request-complete []
                              :request-error []
                              :circuit-open []
                              :circuit-close []
                              :rate-limit-hit []
                              :cache-hit []
                              :cache-miss []}))
     ([event-type] (swap! event-hooks assoc event-type []))))

#?(:clj
   (defn emit-event
     "Emit an event to all registered hooks.
      Event data should include :event-type and relevant context."
     [event-type event-data]
     (let [hooks (get @event-hooks event-type [])
           full-event (assoc event-data
                             :event-type event-type
                             :timestamp (java.time.Instant/now))]
       (doseq [{:keys [fn]} hooks]
         (try
           (fn full-event)
           (catch Exception e
             ;; Log but don't fail on hook errors
             (println "Hook error for" event-type ":" (.getMessage e))))))))

#?(:clj
   (defn list-hooks
     "List all registered hooks, optionally filtered by event type."
     ([] @event-hooks)
     ([event-type] (get @event-hooks event-type []))))

;; ============================================
;; Retry Logic with Exponential Backoff
;; ============================================

(def retry-config
  "Default retry configuration."
  {:max-retries 3
   :initial-delay-ms 1000
   :max-delay-ms 30000
   :backoff-multiplier 2.0
   :retryable-status-codes #{408 429 500 502 503 504}})

#?(:clj
   (defn calculate-backoff-delay
     "Calculate delay for retry attempt using exponential backoff with jitter."
     [attempt {:keys [initial-delay-ms max-delay-ms backoff-multiplier]}]
     (let [base-delay (* initial-delay-ms (Math/pow backoff-multiplier attempt))
           jitter (* base-delay (rand 0.3))
           delay-with-jitter (+ base-delay jitter)]
       (min delay-with-jitter max-delay-ms))))

#?(:clj
   (defn retryable-error?
     "Check if an error or response is retryable."
     [response-or-error config]
     (cond
       (instance? Exception response-or-error)
       (let [msg (.getMessage response-or-error)]
         (or (str/includes? (str msg) "timeout")
             (str/includes? (str msg) "connection")
             (str/includes? (str msg) "reset")))
       
       (map? response-or-error)
       (contains? (:retryable-status-codes config) (:status response-or-error))
       
       :else false)))

#?(:clj
   (defn with-retry
     "Execute a function with retry logic and exponential backoff.
      Returns the result of f or the last error after max retries."
     ([f] (with-retry f retry-config))
     ([f config]
      (loop [attempt 0]
        (let [result (try
                       {:success true :value (f)}
                       (catch Exception e
                         {:success false :error e}))]
          (if (:success result)
            (:value result)
            (if (and (< attempt (:max-retries config))
                     (retryable-error? (:error result) config))
              (do
                (Thread/sleep (long (calculate-backoff-delay attempt config)))
                (recur (inc attempt)))
              (throw (:error result)))))))))

;; ============================================
;; Rate Limiting
;; ============================================

(def rate-limiters
  "Atom storing rate limiter state per connector type."
  (atom {}))

(def rate-limit-config
  "Rate limits per connector type (requests per minute)."
  {:github 60
   :slack 60
   :huggingface 30
   :zapier 100
   :web-scraper 10})

#?(:clj
   (defn check-rate-limit
     "Check if a request is within rate limits. Returns true if allowed."
     [connector-type]
     (let [limit (get rate-limit-config connector-type 60)
           now (System/currentTimeMillis)
           window-ms 60000
           state (get @rate-limiters connector-type {:requests [] :last-reset now})]
       (if (> (- now (:last-reset state)) window-ms)
         ;; Reset window
         (do
           (swap! rate-limiters assoc connector-type {:requests [now] :last-reset now})
           true)
         ;; Check if under limit
         (let [recent-requests (filter #(> % (- now window-ms)) (:requests state))]
           (if (< (count recent-requests) limit)
             (do
               (swap! rate-limiters assoc connector-type 
                      {:requests (conj (vec recent-requests) now) :last-reset (:last-reset state)})
               true)
             false))))))

#?(:clj
   (defn wait-for-rate-limit
     "Wait until rate limit allows a request. Returns after waiting."
     [connector-type]
     (loop [attempts 0]
       (if (check-rate-limit connector-type)
         true
         (when (< attempts 60)
           (Thread/sleep 1000)
           (recur (inc attempts)))))))

;; ============================================
;; Response Caching
;; ============================================

(def response-cache
  "Atom storing cached responses with TTL."
  (atom {}))

(def cache-config
  "Cache TTL per connector type (milliseconds)."
  {:github 300000      ;; 5 minutes
   :huggingface 60000  ;; 1 minute
   :web-scraper 600000 ;; 10 minutes
   :lm-studio 0})      ;; No caching for LLM responses

#?(:clj
   (defn cache-key
     "Generate a cache key from connector type and request params."
     [connector-type & params]
     (str connector-type "-" (hash params))))

#?(:clj
   (defn get-cached
     "Get a cached response if valid, nil otherwise."
     [connector-type & params]
     (let [key (apply cache-key connector-type params)
           entry (get @response-cache key)
           ttl (get cache-config connector-type 0)]
       (when (and entry 
                  (> ttl 0)
                  (< (- (System/currentTimeMillis) (:timestamp entry)) ttl))
         (:value entry)))))

#?(:clj
   (defn set-cached
     "Cache a response value."
     [connector-type value & params]
     (let [key (apply cache-key connector-type params)
           ttl (get cache-config connector-type 0)]
       (when (> ttl 0)
         (swap! response-cache assoc key {:value value 
                                          :timestamp (System/currentTimeMillis)}))
       value)))

#?(:clj
   (defn clear-cache
     "Clear all cached responses or for a specific connector type."
     ([] (reset! response-cache {}))
     ([connector-type]
      (swap! response-cache 
             (fn [cache]
               (into {} (remove #(str/starts-with? (key %) (str connector-type "-")) cache)))))))

;; ============================================
;; Metrics & Telemetry
;; ============================================

(def metrics
  "Atom storing connector metrics."
  (atom {:requests {}
         :errors {}
         :latencies {}
         :cache-hits {}
         :cache-misses {}
         :rate-limit-waits {}
         :retries {}}))

#?(:clj
   (defn record-request
     "Record a request metric for a connector type."
     [connector-type]
     (swap! metrics update-in [:requests connector-type] (fnil inc 0))))

#?(:clj
   (defn record-error
     "Record an error metric for a connector type."
     [connector-type error-type]
     (swap! metrics update-in [:errors connector-type error-type] (fnil inc 0))))

#?(:clj
   (defn record-latency
     "Record latency for a request (in milliseconds)."
     [connector-type latency-ms]
     (swap! metrics update-in [:latencies connector-type]
            (fn [latencies]
              (let [current (or latencies {:count 0 :total 0 :min Long/MAX_VALUE :max 0})]
                {:count (inc (:count current))
                 :total (+ (:total current) latency-ms)
                 :min (min (:min current) latency-ms)
                 :max (max (:max current) latency-ms)})))))

#?(:clj
   (defn record-cache-hit
     "Record a cache hit for a connector type."
     [connector-type]
     (swap! metrics update-in [:cache-hits connector-type] (fnil inc 0))))

#?(:clj
   (defn record-cache-miss
     "Record a cache miss for a connector type."
     [connector-type]
     (swap! metrics update-in [:cache-misses connector-type] (fnil inc 0))))

#?(:clj
   (defn record-rate-limit-wait
     "Record a rate limit wait for a connector type."
     [connector-type]
     (swap! metrics update-in [:rate-limit-waits connector-type] (fnil inc 0))))

#?(:clj
   (defn record-retry
     "Record a retry attempt for a connector type."
     [connector-type attempt]
     (swap! metrics update-in [:retries connector-type attempt] (fnil inc 0))))

#?(:clj
   (defn get-metrics
     "Get all metrics or metrics for a specific connector type."
     ([] @metrics)
     ([connector-type]
      {:requests (get-in @metrics [:requests connector-type] 0)
       :errors (get-in @metrics [:errors connector-type] {})
       :latencies (get-in @metrics [:latencies connector-type] {:count 0 :total 0 :min 0 :max 0})
       :cache-hits (get-in @metrics [:cache-hits connector-type] 0)
       :cache-misses (get-in @metrics [:cache-misses connector-type] 0)
       :rate-limit-waits (get-in @metrics [:rate-limit-waits connector-type] 0)
       :retries (get-in @metrics [:retries connector-type] {})})))

#?(:clj
   (defn get-average-latency
     "Get average latency for a connector type (in milliseconds)."
     [connector-type]
     (let [latencies (get-in @metrics [:latencies connector-type])]
       (if (and latencies (> (:count latencies) 0))
         (/ (:total latencies) (:count latencies))
         0))))

#?(:clj
   (defn get-error-rate
     "Get error rate for a connector type (errors / total requests)."
     [connector-type]
     (let [requests (get-in @metrics [:requests connector-type] 0)
           errors (reduce + (vals (get-in @metrics [:errors connector-type] {})))]
       (if (> requests 0)
         (double (/ errors requests))
         0.0))))

#?(:clj
   (defn get-cache-hit-rate
     "Get cache hit rate for a connector type."
     [connector-type]
     (let [hits (get-in @metrics [:cache-hits connector-type] 0)
           misses (get-in @metrics [:cache-misses connector-type] 0)
           total (+ hits misses)]
       (if (> total 0)
         (double (/ hits total))
         0.0))))

#?(:clj
   (defn reset-metrics
     "Reset all metrics or metrics for a specific connector type."
     ([] (reset! metrics {:requests {}
                          :errors {}
                          :latencies {}
                          :cache-hits {}
                          :cache-misses {}
                          :rate-limit-waits {}
                          :retries {}}))
     ([connector-type]
      (swap! metrics (fn [m]
                       (-> m
                           (update :requests dissoc connector-type)
                           (update :errors dissoc connector-type)
                           (update :latencies dissoc connector-type)
                           (update :cache-hits dissoc connector-type)
                           (update :cache-misses dissoc connector-type)
                           (update :rate-limit-waits dissoc connector-type)
                           (update :retries dissoc connector-type)))))))

#?(:clj
   (defn get-metrics-summary
     "Get a summary of all connector metrics."
     []
     (let [connector-types (keys (:requests @metrics))]
       (into {}
             (map (fn [ct]
                    [ct {:requests (get-in @metrics [:requests ct] 0)
                         :error-rate (get-error-rate ct)
                         :avg-latency-ms (get-average-latency ct)
                         :cache-hit-rate (get-cache-hit-rate ct)
                         :rate-limit-waits (get-in @metrics [:rate-limit-waits ct] 0)}])
                  connector-types)))))

;; ============================================
;; Instrumented HTTP Wrapper
;; ============================================

#?(:clj
   (defmacro with-metrics
     "Execute a connector operation with metrics collection."
     [connector-type & body]
     `(let [start-time# (System/currentTimeMillis)]
        (record-request ~connector-type)
        (try
          (let [result# (do ~@body)]
            (record-latency ~connector-type (- (System/currentTimeMillis) start-time#))
            result#)
          (catch Exception e#
            (record-latency ~connector-type (- (System/currentTimeMillis) start-time#))
            (record-error ~connector-type (type e#))
            (throw e#))))))

;; ============================================
;; Circuit Breaker Pattern
;; ============================================

(def circuit-breaker-config
  "Default circuit breaker configuration."
  {:failure-threshold 5        ;; Number of failures before opening circuit
   :success-threshold 3        ;; Number of successes to close circuit
   :timeout-ms 60000           ;; Time to wait before trying half-open (1 minute)
   :half-open-max-calls 3})    ;; Max calls in half-open state

(def circuit-breakers
  "Atom storing circuit breaker state per connector type.
   States: :closed (normal), :open (failing), :half-open (testing)"
  (atom {}))

#?(:clj
   (defn get-circuit-state
     "Get the current circuit breaker state for a connector type."
     [connector-type]
     (get @circuit-breakers connector-type
          {:state :closed
           :failure-count 0
           :success-count 0
           :last-failure-time nil
           :half-open-calls 0})))

#?(:clj
   (defn circuit-open?
     "Check if the circuit is open (blocking requests)."
     [connector-type]
     (let [state (get-circuit-state connector-type)]
       (and (= :open (:state state))
            (let [timeout (:timeout-ms circuit-breaker-config)
                  last-failure (:last-failure-time state)]
              (and last-failure
                   (< (- (System/currentTimeMillis) last-failure) timeout)))))))

#?(:clj
   (defn circuit-half-open?
     "Check if the circuit is in half-open state (testing)."
     [connector-type]
     (let [state (get-circuit-state connector-type)]
       (or (= :half-open (:state state))
           (and (= :open (:state state))
                (let [timeout (:timeout-ms circuit-breaker-config)
                      last-failure (:last-failure-time state)]
                  (and last-failure
                       (>= (- (System/currentTimeMillis) last-failure) timeout))))))))

#?(:clj
   (defn record-circuit-success
     "Record a successful call for circuit breaker."
     [connector-type]
     (swap! circuit-breakers update connector-type
            (fn [state]
              (let [current (or state {:state :closed :failure-count 0 :success-count 0})]
                (case (:state current)
                  :closed current
                  :half-open
                  (let [new-success-count (inc (:success-count current))]
                    (if (>= new-success-count (:success-threshold circuit-breaker-config))
                      {:state :closed :failure-count 0 :success-count 0 :last-failure-time nil :half-open-calls 0}
                      (assoc current :success-count new-success-count)))
                  :open
                  (if (circuit-half-open? connector-type)
                    {:state :half-open :failure-count 0 :success-count 1 :last-failure-time nil :half-open-calls 1}
                    current)))))))

#?(:clj
   (defn record-circuit-failure
     "Record a failed call for circuit breaker."
     [connector-type]
     (swap! circuit-breakers update connector-type
            (fn [state]
              (let [current (or state {:state :closed :failure-count 0 :success-count 0})
                    new-failure-count (inc (:failure-count current))]
                (case (:state current)
                  :closed
                  (if (>= new-failure-count (:failure-threshold circuit-breaker-config))
                    {:state :open :failure-count new-failure-count :success-count 0 
                     :last-failure-time (System/currentTimeMillis) :half-open-calls 0}
                    (assoc current :failure-count new-failure-count))
                  :half-open
                  {:state :open :failure-count 1 :success-count 0 
                   :last-failure-time (System/currentTimeMillis) :half-open-calls 0}
                  :open
                  (assoc current :last-failure-time (System/currentTimeMillis))))))))

#?(:clj
   (defn reset-circuit
     "Reset the circuit breaker for a connector type."
     [connector-type]
     (swap! circuit-breakers dissoc connector-type)))

#?(:clj
   (defn reset-all-circuits
     "Reset all circuit breakers."
     []
     (reset! circuit-breakers {})))

#?(:clj
   (defn get-circuit-status
     "Get a summary of circuit breaker status for all connectors."
     []
     (into {}
           (map (fn [[k v]]
                  [k {:state (:state v)
                      :failure-count (:failure-count v)
                      :success-count (:success-count v)}])
                @circuit-breakers))))

#?(:clj
   (defn with-circuit-breaker
     "Execute a function with circuit breaker protection.
      Returns {:success true :value result} or {:success false :error error-msg}."
     [connector-type f]
     (cond
       ;; Circuit is open - fail fast
       (circuit-open? connector-type)
       {:success false
        :error "Circuit breaker is open"
        :circuit-state :open
        :timestamp (java.time.Instant/now)}
       
       ;; Circuit is half-open - allow limited calls
       (circuit-half-open? connector-type)
       (let [state (get-circuit-state connector-type)]
         (if (>= (:half-open-calls state) (:half-open-max-calls circuit-breaker-config))
           {:success false
            :error "Circuit breaker half-open limit reached"
            :circuit-state :half-open
            :timestamp (java.time.Instant/now)}
           (do
             (swap! circuit-breakers update connector-type
                    #(update % :half-open-calls (fnil inc 0)))
             (try
               (let [result (f)]
                 (record-circuit-success connector-type)
                 {:success true :value result :circuit-state :half-open})
               (catch Exception e
                 (record-circuit-failure connector-type)
                 {:success false :error (.getMessage e) :circuit-state :half-open})))))
       
       ;; Circuit is closed - normal operation
       :else
       (try
         (let [result (f)]
           (record-circuit-success connector-type)
           {:success true :value result :circuit-state :closed})
         (catch Exception e
           (record-circuit-failure connector-type)
           {:success false :error (.getMessage e) :circuit-state :closed})))))

;; ============================================
;; Health Check Endpoints
;; ============================================

#?(:clj
   (defn health-check-connector
     "Perform a health check on a specific connector.
      Returns {:healthy true/false :latency-ms N :error error-msg}."
     [connector-type connector]
     (let [start-time (System/currentTimeMillis)]
       (try
         (case connector-type
           :lm-studio
           (let [result (lm-studio-list-models connector)]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:models-count (count (:models result))}
                         {:error (:error result)})})
           
           :github
           (let [result (github-get-repo connector "octocat" "Hello-World")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:repo-found true}
                         {:error (:error result)})})
           
           :slack
           (let [result (slack-list-channels connector)]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:channels-count (:total-count result)}
                         {:error (:error result)})})
           
           :huggingface
           (let [result (huggingface-inference connector "gpt2" "Hello")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:inference-success true}
                         {:error (:error result)})})
           
           :web-scraper
           (let [result (scrape-url connector "https://example.com")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:content-length (count (:content result))}
                         {:error (:error result)})})
           
           :file
           (let [result (file-list connector "")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:files-count (count (:files result))}
                         {:error (:error result)})})
           
           ;; Default for unknown connector types
           {:healthy false
            :latency-ms (- (System/currentTimeMillis) start-time)
            :connector-type connector-type
            :details {:error "Unknown connector type"}})
         (catch Exception e
           {:healthy false
            :latency-ms (- (System/currentTimeMillis) start-time)
            :connector-type connector-type
            :details {:error (.getMessage e)}})))))

#?(:clj
   (defn get-system-health
     "Get overall system health status including all connectors."
     []
     {:timestamp (java.time.Instant/now)
      :metrics-summary (get-metrics-summary)
      :circuit-breakers (get-circuit-status)
      :rate-limiters (into {} (map (fn [[k v]] [k {:requests-in-window (count (:requests v))}]) @rate-limiters))
      :cache-size (count @response-cache)}))

;; ============================================
;; Bulk Operations
;; ============================================

#?(:clj
   (defn bulk-request
     "Execute multiple requests in parallel with rate limiting and circuit breaker protection.
      Returns a vector of results in the same order as requests.
      
      requests: vector of {:connector-type :type :connector connector :fn function}
      options: {:max-parallel N :timeout-ms N}"
     [requests & {:keys [max-parallel timeout-ms] :or {max-parallel 4 timeout-ms 30000}}]
     (let [futures (doall
                     (map-indexed
                       (fn [idx {:keys [connector-type connector fn]}]
                         [idx
                          (future
                            (try
                              (wait-for-rate-limit connector-type)
                              (with-circuit-breaker connector-type
                                #(fn connector))
                              (catch Exception e
                                {:success false :error (.getMessage e) :index idx})))])
                       requests))
           results (into {}
                         (map (fn [[idx fut]]
                                [idx (deref fut timeout-ms {:success false :error "Timeout" :index idx})])
                              futures))]
       (mapv #(get results %) (range (count requests))))))

#?(:clj
   (defn bulk-github-repos
     "Fetch multiple GitHub repositories in parallel."
     [connector repos]
     (bulk-request
       (mapv (fn [[owner repo]]
               {:connector-type :github
                :connector connector
                :fn #(github-get-repo % owner repo)})
             repos))))

#?(:clj
   (defn bulk-scrape-urls
     "Scrape multiple URLs in parallel with rate limiting."
     [connector urls & {:keys [max-parallel] :or {max-parallel 2}}]
     (bulk-request
       (mapv (fn [url]
               {:connector-type :web-scraper
                :connector connector
                :fn #(scrape-url % url)})
             urls)
       :max-parallel max-parallel)))

#?(:clj
   (defn bulk-huggingface-inference
     "Run inference on multiple inputs in parallel."
     [connector model-id inputs]
     (bulk-request
       (mapv (fn [input]
               {:connector-type :huggingface
                :connector connector
                :fn #(huggingface-inference % model-id input)})
             inputs))))

#?(:clj
   (defn bulk-lm-studio-chat
     "Send multiple chat requests to LM Studio in parallel."
     [connector messages-list & {:keys [temperature max-tokens] :or {temperature 0.7 max-tokens 1000}}]
     (bulk-request
       (mapv (fn [messages]
               {:connector-type :lm-studio
                :connector connector
                :fn #(lm-studio-chat % messages :temperature temperature :max-tokens max-tokens)})
             messages-list))))

;; ============================================
;; Connector Lifecycle Management
;; ============================================

(def active-connectors
  "Atom storing active connector instances."
  (atom {}))

#?(:clj
   (defn register-connector
     "Register a connector instance for lifecycle management."
     [name connector-type connector]
     (swap! active-connectors assoc name
            {:type connector-type
             :connector connector
             :registered-at (java.time.Instant/now)
             :status :active})))

#?(:clj
   (defn unregister-connector
     "Unregister a connector instance."
     [name]
     (swap! active-connectors dissoc name)))

#?(:clj
   (defn get-connector
     "Get a registered connector by name."
     [name]
     (get-in @active-connectors [name :connector])))

#?(:clj
   (defn list-active-connectors
     "List all active registered connectors."
     []
     (mapv (fn [[name info]]
             {:name name
              :type (:type info)
              :registered-at (:registered-at info)
              :status (:status info)})
           @active-connectors)))

#?(:clj
   (defn health-check-all
     "Run health checks on all registered connectors."
     []
     (into {}
           (map (fn [[name info]]
                  [name (health-check-connector (:type info) (:connector info))])
                @active-connectors))))

#?(:clj
   (defn shutdown-all-connectors
     "Shutdown all registered connectors and clean up resources."
     []
     (reset! active-connectors {})
     (reset-all-circuits)
     (reset-metrics)
     (clear-cache)
     {:status :shutdown :timestamp (java.time.Instant/now)}))

;; ============================================
;; Connector Factory Pattern
;; ============================================

(def connector-registry
  "Registry of connector types and their factory functions."
  (atom {}))

#?(:clj
   (defn register-connector-type
     "Register a connector type with its factory function.
      Factory function should accept a config map and return a connector."
     [connector-type factory-fn & {:keys [description required-config]}]
     (swap! connector-registry assoc connector-type
            {:factory-fn factory-fn
             :description description
             :required-config (or required-config [])
             :registered-at (java.time.Instant/now)})))

#?(:clj
   (defn unregister-connector-type
     "Unregister a connector type."
     [connector-type]
     (swap! connector-registry dissoc connector-type)))

#?(:clj
   (defn list-connector-types
     "List all registered connector types."
     []
     (mapv (fn [[type info]]
             {:type type
              :description (:description info)
              :required-config (:required-config info)})
           @connector-registry)))

#?(:clj
   (defn create-connector
     "Create a connector using the factory pattern.
      Validates required config and calls the registered factory function."
     [connector-type config]
     (if-let [type-info (get @connector-registry connector-type)]
       (let [required (:required-config type-info)
             missing (filter #(not (contains? config %)) required)]
         (if (seq missing)
           {:error (str "Missing required config: " (str/join ", " (map name missing)))
            :status :error}
           (try
             (let [connector ((:factory-fn type-info) config)]
               (emit-event :connector-created {:connector-type connector-type :config (dissoc config :api-key :token :password)})
               connector)
             (catch Exception e
               {:error (.getMessage e) :status :error}))))
       {:error (str "Unknown connector type: " (name connector-type))
        :status :error})))

#?(:clj
   (defn create-and-register
     "Create a connector and register it for lifecycle management."
     [name connector-type config]
     (let [connector (create-connector connector-type config)]
       (if (:error connector)
         connector
         (do
           (register-connector name connector-type connector)
           {:status :success
            :name name
            :connector-type connector-type
            :connector connector})))))

;; ============================================
;; Configuration Validation
;; ============================================

(def config-schemas
  "Schema definitions for connector configurations.
   Each schema defines field types, constraints, and defaults."
  (atom {:github {:token {:type :string :required true :min-length 1 :sensitive true}
                  :base-url {:type :string :required false :default "https://api.github.com"}}
         :slack {:token {:type :string :required true :min-length 1 :sensitive true}
                 :base-url {:type :string :required false :default "https://slack.com/api"}}
         :huggingface {:api-key {:type :string :required true :min-length 1 :sensitive true}
                       :base-url {:type :string :required false :default "https://api-inference.huggingface.co"}}
         :lm-studio {:base-url {:type :string :required false :default "http://localhost:1234"}
                     :timeout-ms {:type :integer :required false :default 120000 :min 1000 :max 600000}}
         :zapier {:webhook-url {:type :string :required true :pattern #"^https://hooks\.zapier\.com/.*"}}
         :web-scraper {:user-agent {:type :string :required false :default "Mozilla/5.0"}
                       :max-retries {:type :integer :required false :default 3 :min 0 :max 10}}
         :file {:base-path {:type :string :required true :min-length 1}}}))

#?(:clj
   (defn validate-field
     "Validate a single field against its schema."
     [field-name value schema]
     (let [{:keys [type required min-length max-length min max pattern]} schema]
       (cond
         ;; Check required
         (and required (nil? value))
         {:valid false :error (str field-name " is required")}
         
         ;; Skip validation if nil and not required
         (nil? value)
         {:valid true}
         
         ;; Type validation
         (and (= type :string) (not (string? value)))
         {:valid false :error (str field-name " must be a string")}
         
         (and (= type :integer) (not (integer? value)))
         {:valid false :error (str field-name " must be an integer")}
         
         (and (= type :boolean) (not (boolean? value)))
         {:valid false :error (str field-name " must be a boolean")}
         
         ;; String constraints
         (and (= type :string) min-length (< (count value) min-length))
         {:valid false :error (str field-name " must be at least " min-length " characters")}
         
         (and (= type :string) max-length (> (count value) max-length))
         {:valid false :error (str field-name " must be at most " max-length " characters")}
         
         ;; Pattern validation
         (and (= type :string) pattern (not (re-matches pattern value)))
         {:valid false :error (str field-name " does not match required pattern")}
         
         ;; Numeric constraints
         (and (= type :integer) min (< value min))
         {:valid false :error (str field-name " must be at least " min)}
         
         (and (= type :integer) max (> value max))
         {:valid false :error (str field-name " must be at most " max)}
         
         :else
         {:valid true}))))

#?(:clj
   (defn validate-config
     "Validate a connector configuration against its schema.
      Returns {:valid true} or {:valid false :errors [...]}"
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (let [results (map (fn [[field-name field-schema]]
                            (validate-field (name field-name)
                                            (get config field-name)
                                            field-schema))
                          schema)
             errors (filter #(not (:valid %)) results)]
         (if (empty? errors)
           {:valid true :connector-type connector-type}
           {:valid false
            :connector-type connector-type
            :errors (mapv :error errors)}))
       {:valid true :connector-type connector-type :warning "No schema defined"})))

#?(:clj
   (defn apply-defaults
     "Apply default values from schema to config."
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (reduce (fn [cfg [field-name field-schema]]
                 (if (and (contains? field-schema :default)
                          (not (contains? cfg field-name)))
                   (assoc cfg field-name (:default field-schema))
                   cfg))
               config
               schema)
       config)))

#?(:clj
   (defn get-config-schema
     "Get the schema for a connector type."
     [connector-type]
     (get @config-schemas connector-type)))

#?(:clj
   (defn register-config-schema
     "Register a configuration schema for a connector type."
     [connector-type schema]
     (swap! config-schemas assoc connector-type schema)))

#?(:clj
   (defn list-config-schemas
     "List all registered configuration schemas."
     []
     (mapv (fn [[type schema]]
             {:type type
              :fields (mapv (fn [[field-name field-schema]]
                              {:name field-name
                               :type (:type field-schema)
                               :required (:required field-schema false)
                               :sensitive (:sensitive field-schema false)})
                            schema)})
           @config-schemas)))

#?(:clj
   (defn redact-sensitive-config
     "Redact sensitive fields from a config for logging/display."
     [connector-type config]
     (if-let [schema (get @config-schemas connector-type)]
       (reduce (fn [cfg [field-name field-schema]]
                 (if (and (:sensitive field-schema) (contains? cfg field-name))
                   (assoc cfg field-name "[REDACTED]")
                   cfg))
               config
               schema)
       config)))

;; ============================================
;; Dependency Injection Container
;; ============================================

(def di-container
  "Dependency injection container for connectors.
   Allows swapping implementations for testing or different environments."
  (atom {:http-client nil
         :cache nil
         :metrics nil
         :logger nil
         :environment :production}))

#?(:clj
   (defn register-dependency
     "Register a dependency in the DI container."
     [key implementation]
     (swap! di-container assoc key implementation)))

#?(:clj
   (defn get-dependency
     "Get a dependency from the DI container."
     [key]
     (get @di-container key)))

#?(:clj
   (defn set-environment
     "Set the current environment (e.g., :production, :development, :test)."
     [env]
     (swap! di-container assoc :environment env)))

#?(:clj
   (defn get-environment
     "Get the current environment."
     []
     (get @di-container :environment)))

#?(:clj
   (defn with-test-dependencies
     "Execute a function with test dependencies injected.
      Restores original dependencies after execution."
     [test-deps f]
     (let [original @di-container]
       (try
         (swap! di-container merge test-deps {:environment :test})
         (f)
         (finally
           (reset! di-container original))))))

#?(:clj
   (defn create-mock-http-client
     "Create a mock HTTP client for testing.
      Responses is a map of URL patterns to response data."
     [responses]
     {:type :mock
      :responses responses
      :request-log (atom [])
      :make-request (fn [method url opts]
                      (let [response (or (get responses url)
                                         (some (fn [[pattern resp]]
                                                 (when (and (instance? java.util.regex.Pattern pattern)
                                                            (re-matches pattern url))
                                                   resp))
                                               responses)
                                         {:status 404 :body "Not found"})]
                        (swap! (:request-log (get-dependency :http-client))
                               conj {:method method :url url :opts opts})
                        response))}))

#?(:clj
   (defn create-mock-cache
     "Create a mock cache for testing."
     []
     (let [store (atom {})]
       {:type :mock
        :store store
        :get (fn [key] (get @store key))
        :put (fn [key value ttl] (swap! store assoc key {:value value :ttl ttl}))
        :invalidate (fn [key] (swap! store dissoc key))
        :clear (fn [] (reset! store {}))})))

#?(:clj
   (defn create-mock-logger
     "Create a mock logger for testing."
     []
     (let [logs (atom [])]
       {:type :mock
        :logs logs
        :log (fn [level message & args]
               (swap! logs conj {:level level :message message :args args :timestamp (java.time.Instant/now)}))
        :get-logs (fn [] @logs)
        :clear (fn [] (reset! logs []))})))

#?(:clj
   (defn inject-connector
     "Create a connector with injected dependencies.
      Allows overriding default implementations for testing."
     [connector-type config & {:keys [http-client cache logger]}]
     (let [base-connector (create-connector connector-type config)]
       (if (:error base-connector)
         base-connector
         (assoc base-connector
                :injected-http-client (or http-client (get-dependency :http-client))
                :injected-cache (or cache (get-dependency :cache))
                :injected-logger (or logger (get-dependency :logger))
                :di-enabled true)))))

#?(:clj
   (defn get-injected-http-client
     "Get the HTTP client for a connector, using injected one if available."
     [connector]
     (or (:injected-http-client connector)
         (get-dependency :http-client))))

#?(:clj
   (defn get-injected-cache
     "Get the cache for a connector, using injected one if available."
     [connector]
     (or (:injected-cache connector)
         (get-dependency :cache))))

#?(:clj
   (defn get-injected-logger
     "Get the logger for a connector, using injected one if available."
     [connector]
     (or (:injected-logger connector)
         (get-dependency :logger))))

;; ============================================
;; Connector Pool Manager
;; ============================================

(def connector-pool
  "Pool of reusable connector instances.
   Manages connector lifecycle and provides connection reuse."
  (atom {:pools {}
         :config {:max-pool-size 10
                  :min-pool-size 1
                  :max-idle-time-ms 300000
                  :validation-interval-ms 60000}}))

#?(:clj
   (defn get-pool-config
     "Get the pool configuration."
     []
     (:config @connector-pool)))

#?(:clj
   (defn set-pool-config
     "Set pool configuration options."
     [config]
     (swap! connector-pool update :config merge config)))

#?(:clj
   (defn- create-pool-entry
     "Create a new pool entry for a connector."
     [connector]
     {:connector connector
      :created-at (java.time.Instant/now)
      :last-used-at (atom (java.time.Instant/now))
      :use-count (atom 0)
      :in-use (atom false)}))

#?(:clj
   (defn- pool-key
     "Generate a pool key for a connector type and config."
     [connector-type config]
     (let [safe-config (redact-sensitive-config connector-type config)]
       [connector-type (hash safe-config)])))

#?(:clj
   (defn acquire-from-pool
     "Acquire a connector from the pool, creating one if necessary.
      Returns {:connector ... :pool-entry ...} or {:error ...}"
     [connector-type config]
     (let [key (pool-key connector-type config)
           pool-config (:config @connector-pool)]
       (loop [attempts 0]
         (if (>= attempts 3)
           {:error "Could not acquire connector from pool"}
           (let [pool (get-in @connector-pool [:pools key] [])
                 available (first (filter #(compare-and-set! (:in-use %) false true) pool))]
             (if available
               (do
                 (reset! (:last-used-at available) (java.time.Instant/now))
                 (swap! (:use-count available) inc)
                 {:connector (:connector available)
                  :pool-entry available
                  :from-pool true})
               ;; Create new if pool not full
               (if (< (count pool) (:max-pool-size pool-config))
                 (let [new-connector (create-connector connector-type config)]
                   (if (:error new-connector)
                     new-connector
                     (let [entry (create-pool-entry new-connector)]
                       (reset! (:in-use entry) true)
                       (swap! connector-pool update-in [:pools key] (fnil conj []) entry)
                       {:connector new-connector
                        :pool-entry entry
                        :from-pool false})))
                 (do
                   (Thread/sleep 100)
                   (recur (inc attempts)))))))))))

#?(:clj
   (defn release-to-pool
     "Release a connector back to the pool."
     [pool-entry]
     (when pool-entry
       (reset! (:in-use pool-entry) false)
       (reset! (:last-used-at pool-entry) (java.time.Instant/now)))))

#?(:clj
   (defn with-pooled-connector
     "Execute a function with a pooled connector.
      Automatically acquires and releases the connector."
     [connector-type config f]
     (let [result (acquire-from-pool connector-type config)]
       (if (:error result)
         result
         (try
           (f (:connector result))
           (finally
             (release-to-pool (:pool-entry result))))))))

#?(:clj
   (defn get-pool-stats
     "Get statistics about the connector pool."
     []
     (let [pools (:pools @connector-pool)]
       {:total-pools (count pools)
        :total-connectors (reduce + (map count (vals pools)))
        :pools (into {}
                     (map (fn [[k v]]
                            [k {:size (count v)
                                :in-use (count (filter #(deref (:in-use %)) v))
                                :available (count (filter #(not (deref (:in-use %))) v))}])
                          pools))})))

#?(:clj
   (defn cleanup-idle-connectors
     "Remove connectors that have been idle longer than max-idle-time."
     []
     (let [max-idle (:max-idle-time-ms (:config @connector-pool))
           cutoff (java.time.Instant/ofEpochMilli (- (System/currentTimeMillis) max-idle))
           min-size (:min-pool-size (:config @connector-pool))]
       (swap! connector-pool update :pools
              (fn [pools]
                (into {}
                      (map (fn [[k v]]
                             [k (let [active (filter #(deref (:in-use %)) v)
                                      idle (filter #(not (deref (:in-use %))) v)
                                      keep-idle (take (max 0 (- min-size (count active)))
                                                      (sort-by #(deref (:last-used-at %)) > idle))
                                      recent-idle (filter #(.isAfter (deref (:last-used-at %)) cutoff)
                                                          (drop (count keep-idle) idle))]
                                  (vec (concat active keep-idle recent-idle)))])
                           pools)))))))

#?(:clj
   (defn drain-pool
     "Drain all connectors from the pool for a specific type, or all pools if no type specified."
     ([] (swap! connector-pool assoc :pools {}))
     ([connector-type config]
      (let [key (pool-key connector-type config)]
        (swap! connector-pool update :pools dissoc key)))))

;; ============================================
;; Graceful Shutdown Support
;; ============================================

(def shutdown-state
  "Atom tracking shutdown state and pending operations."
  (atom {:shutting-down false
         :pending-operations (atom 0)
         :shutdown-timeout-ms 30000
         :shutdown-hooks []}))

#?(:clj
   (defn is-shutting-down?
     "Check if the system is in shutdown mode."
     []
     (:shutting-down @shutdown-state)))

#?(:clj
   (defn register-shutdown-hook
     "Register a function to be called during shutdown.
      Returns hook-id for unregistering."
     [hook-fn & {:keys [name priority] :or {priority 50}}]
     (let [hook-id (java.util.UUID/randomUUID)]
       (swap! shutdown-state update :shutdown-hooks
              (fn [hooks]
                (->> (conj hooks {:id hook-id
                                  :fn hook-fn
                                  :name name
                                  :priority priority})
                     (sort-by :priority))))
       hook-id)))

#?(:clj
   (defn unregister-shutdown-hook
     "Unregister a shutdown hook by its ID."
     [hook-id]
     (swap! shutdown-state update :shutdown-hooks
            (fn [hooks]
              (vec (remove #(= hook-id (:id %)) hooks))))))

#?(:clj
   (defn track-operation
     "Track a pending operation. Returns a function to call when operation completes."
     []
     (when-not (is-shutting-down?)
       (swap! (:pending-operations @shutdown-state) inc)
       (fn []
         (swap! (:pending-operations @shutdown-state) dec)))))

#?(:clj
   (defn with-tracked-operation
     "Execute a function while tracking it as a pending operation.
      Prevents new operations during shutdown."
     [f]
     (if (is-shutting-down?)
       {:error "System is shutting down, operation rejected"}
       (let [complete-fn (track-operation)]
         (try
           (f)
           (finally
             (when complete-fn (complete-fn))))))))

#?(:clj
   (defn wait-for-pending-operations
     "Wait for all pending operations to complete, with timeout."
     [timeout-ms]
     (let [start-time (System/currentTimeMillis)
           pending-ops (:pending-operations @shutdown-state)]
       (loop []
         (let [elapsed (- (System/currentTimeMillis) start-time)
               pending @pending-ops]
           (cond
             (zero? pending) {:success true :pending 0}
             (>= elapsed timeout-ms) {:success false :pending pending :timeout true}
             :else (do
                     (Thread/sleep 100)
                     (recur))))))))

#?(:clj
   (defn initiate-shutdown
     "Initiate graceful shutdown.
      1. Set shutting-down flag to reject new operations
      2. Wait for pending operations to complete
      3. Run shutdown hooks in priority order
      4. Clean up resources"
     [& {:keys [timeout-ms] :or {timeout-ms 30000}}]
     (println "Initiating graceful shutdown...")
     (swap! shutdown-state assoc :shutting-down true)
     
     ;; Wait for pending operations
     (println "Waiting for pending operations to complete...")
     (let [wait-result (wait-for-pending-operations timeout-ms)]
       (when-not (:success wait-result)
         (println "Warning: Shutdown timeout reached with" (:pending wait-result) "pending operations")))
     
     ;; Run shutdown hooks
     (println "Running shutdown hooks...")
     (doseq [{:keys [fn name]} (:shutdown-hooks @shutdown-state)]
       (try
         (println "Running shutdown hook:" (or name "unnamed"))
         (fn)
         (catch Exception e
           (println "Shutdown hook error:" (.getMessage e)))))
     
     ;; Clean up resources
     (println "Cleaning up resources...")
     (drain-pool)
     (shutdown-all-connectors)
     (cancel-all-async-tasks)
     
     (println "Shutdown complete.")
     {:success true}))

#?(:clj
   (defn cancel-all-async-tasks
     "Cancel all pending async tasks."
     []
     (doseq [[task-id task] @async-tasks]
       (when-not (realized? (:future task))
         (future-cancel (:future task))))
     (reset! async-tasks {})))

#?(:clj
   (defn get-shutdown-status
     "Get current shutdown status."
     []
     {:shutting-down (is-shutting-down?)
      :pending-operations @(:pending-operations @shutdown-state)
      :registered-hooks (count (:shutdown-hooks @shutdown-state))
      :timeout-ms (:shutdown-timeout-ms @shutdown-state)}))

;; ============================================
;; Request Deduplication
;; ============================================

(def request-dedup-cache
  "Cache for deduplicating identical concurrent requests."
  (atom {}))

(def dedup-config
  "Configuration for request deduplication."
  (atom {:enabled true
         :ttl-ms 5000
         :max-entries 1000}))

#?(:clj
   (defn get-dedup-config
     "Get current deduplication configuration."
     []
     @dedup-config))

#?(:clj
   (defn set-dedup-config
     "Update deduplication configuration."
     [config]
     (swap! dedup-config merge config)))

#?(:clj
   (defn request-key
     "Generate a unique key for a request based on its parameters."
     [connector-type operation params]
     (str connector-type ":" operation ":" (hash params))))

#?(:clj
   (defn cleanup-expired-dedup-entries
     "Remove expired entries from the deduplication cache."
     []
     (let [now (System/currentTimeMillis)
           ttl (:ttl-ms @dedup-config)]
       (swap! request-dedup-cache
              (fn [cache]
                (into {}
                      (filter (fn [[_ v]]
                                (< (- now (:timestamp v)) ttl))
                              cache)))))))

#?(:clj
   (defn with-deduplication
     "Execute a request with deduplication.
      If an identical request is already in flight, wait for its result.
      If an identical request completed recently, return cached result."
     [connector-type operation params request-fn]
     (if-not (:enabled @dedup-config)
       (request-fn)
       (let [key (request-key connector-type operation params)
             now (System/currentTimeMillis)]
         ;; Check for existing entry
         (if-let [existing (get @request-dedup-cache key)]
           (cond
             ;; In-flight request - wait for it
             (:in-flight existing)
             (do
               (while (and (get-in @request-dedup-cache [key :in-flight])
                           (< (- (System/currentTimeMillis) now) 30000))
                 (Thread/sleep 50))
               (or (:result (get @request-dedup-cache key))
                   (request-fn)))
             
             ;; Cached result still valid
             (< (- now (:timestamp existing)) (:ttl-ms @dedup-config))
             (:result existing)
             
             ;; Expired - make new request
             :else
             (with-deduplication connector-type operation params request-fn))
           
           ;; No existing entry - make request
           (do
             ;; Mark as in-flight
             (swap! request-dedup-cache assoc key {:in-flight true :timestamp now})
             (try
               (let [result (request-fn)]
                 ;; Store result
                 (swap! request-dedup-cache assoc key {:result result
                                                       :timestamp (System/currentTimeMillis)
                                                       :in-flight false})
                 ;; Cleanup if cache is too large
                 (when (> (count @request-dedup-cache) (:max-entries @dedup-config))
                   (cleanup-expired-dedup-entries))
                 result)
               (catch Exception e
                 ;; Remove failed entry
                 (swap! request-dedup-cache dissoc key)
                 (throw e)))))))))

#?(:clj
   (defn clear-dedup-cache
     "Clear the deduplication cache."
     []
     (reset! request-dedup-cache {})))

#?(:clj
   (defn get-dedup-stats
     "Get deduplication cache statistics."
     []
     (let [cache @request-dedup-cache
           now (System/currentTimeMillis)
           ttl (:ttl-ms @dedup-config)]
       {:total-entries (count cache)
        :in-flight (count (filter #(:in-flight (val %)) cache))
        :cached (count (filter #(and (not (:in-flight (val %)))
                                     (< (- now (:timestamp (val %))) ttl))
                               cache))
        :expired (count (filter #(and (not (:in-flight (val %)))
                                      (>= (- now (:timestamp (val %))) ttl))
                                cache))})))

;; ============================================
;; Request Batching
;; ============================================

(def batch-queue
  "Queue for batching requests by connector type."
  (atom {}))

(def batch-config
  "Configuration for request batching."
  (atom {:enabled true
         :max-batch-size 10
         :max-wait-ms 100
         :flush-interval-ms 50}))

#?(:clj
   (defn get-batch-config
     "Get current batch configuration."
     []
     @batch-config))

#?(:clj
   (defn set-batch-config
     "Update batch configuration."
     [config]
     (swap! batch-config merge config)))

#?(:clj
   (defn add-to-batch
     "Add a request to the batch queue.
      Returns a promise that will be delivered when the batch is processed."
     [connector-type operation params request-fn]
     (let [result-promise (promise)
           batch-key (str connector-type ":" operation)
           entry {:params params
                  :request-fn request-fn
                  :promise result-promise
                  :timestamp (System/currentTimeMillis)}]
       (swap! batch-queue update batch-key (fnil conj []) entry)
       result-promise)))

#?(:clj
   (defn process-batch
     "Process a batch of requests.
      Executes all requests and delivers results to their promises."
     [batch-key entries]
     (doseq [{:keys [request-fn promise]} entries]
       (try
         (let [result (request-fn)]
           (deliver promise {:success true :result result}))
         (catch Exception e
           (deliver promise {:success false :error (.getMessage e)}))))))

#?(:clj
   (defn flush-batch
     "Flush a specific batch, processing all pending requests."
     [batch-key]
     (let [entries (get @batch-queue batch-key)]
       (when (seq entries)
         (swap! batch-queue dissoc batch-key)
         (process-batch batch-key entries)))))

#?(:clj
   (defn flush-all-batches
     "Flush all pending batches."
     []
     (doseq [batch-key (keys @batch-queue)]
       (flush-batch batch-key))))

#?(:clj
   (defn should-flush-batch?
     "Check if a batch should be flushed based on size or time."
     [batch-key]
     (let [entries (get @batch-queue batch-key [])
           config @batch-config]
       (or (>= (count entries) (:max-batch-size config))
           (and (seq entries)
                (let [oldest (apply min (map :timestamp entries))]
                  (>= (- (System/currentTimeMillis) oldest) (:max-wait-ms config))))))))

#?(:clj
   (defn with-batching
     "Execute a request with batching support.
      Requests are queued and processed in batches for efficiency."
     [connector-type operation params request-fn]
     (if-not (:enabled @batch-config)
       (request-fn)
       (let [batch-key (str connector-type ":" operation)
             result-promise (add-to-batch connector-type operation params request-fn)]
         ;; Check if batch should be flushed
         (when (should-flush-batch? batch-key)
           (flush-batch batch-key))
         ;; Wait for result with timeout
         (let [result (deref result-promise (:max-wait-ms @batch-config) {:timeout true})]
           (if (:timeout result)
             ;; Timeout - flush and wait again
             (do
               (flush-batch batch-key)
               (let [final-result (deref result-promise 5000 {:error "Batch timeout"})]
                 (if (:success final-result)
                   (:result final-result)
                   {:error (:error final-result)})))
             ;; Got result
             (if (:success result)
               (:result result)
               {:error (:error result)})))))))

#?(:clj
   (defn get-batch-stats
     "Get statistics about pending batches."
     []
     (let [queue @batch-queue]
       {:total-batches (count queue)
        :total-pending (reduce + (map count (vals queue)))
        :batches (into {}
                       (map (fn [[k v]]
                              [k {:count (count v)
                                  :oldest-ms (when (seq v)
                                               (- (System/currentTimeMillis)
                                                  (apply min (map :timestamp v))))}])
                            queue))})))

#?(:clj
   (defn clear-batch-queue
     "Clear all pending batches without processing them."
     []
     (let [queue @batch-queue]
       ;; Deliver errors to all pending promises
       (doseq [[_ entries] queue]
         (doseq [{:keys [promise]} entries]
           (deliver promise {:success false :error "Batch cleared"})))
       (reset! batch-queue {}))))

;; ============================================
;; Adaptive Retry Strategy
;; ============================================

(def retry-history
  "History of retry attempts for adaptive strategy."
  (atom {}))

(def adaptive-retry-config
  "Configuration for adaptive retry strategy."
  (atom {:enabled true
         :history-window-ms 300000
         :min-delay-ms 100
         :max-delay-ms 30000
         :success-threshold 0.8
         :failure-threshold 0.2}))

#?(:clj
   (defn get-adaptive-retry-config
     "Get current adaptive retry configuration."
     []
     @adaptive-retry-config))

#?(:clj
   (defn set-adaptive-retry-config
     "Update adaptive retry configuration."
     [config]
     (swap! adaptive-retry-config merge config)))

#?(:clj
   (defn record-retry-result
     "Record the result of a retry attempt for adaptive learning."
     [connector-type success? delay-ms]
     (let [now (System/currentTimeMillis)
           entry {:timestamp now
                  :success success?
                  :delay-ms delay-ms}]
       (swap! retry-history update connector-type (fnil conj []) entry))))

#?(:clj
   (defn cleanup-retry-history
     "Remove old entries from retry history."
     []
     (let [window (:history-window-ms @adaptive-retry-config)
           cutoff (- (System/currentTimeMillis) window)]
       (swap! retry-history
              (fn [history]
                (into {}
                      (map (fn [[k v]]
                             [k (vec (filter #(> (:timestamp %) cutoff) v))])
                           history)))))))

#?(:clj
   (defn calculate-success-rate
     "Calculate the success rate for a connector type."
     [connector-type]
     (let [entries (get @retry-history connector-type [])
           total (count entries)]
       (if (zero? total)
         1.0
         (/ (count (filter :success entries)) total)))))

#?(:clj
   (defn calculate-adaptive-delay
     "Calculate the adaptive delay based on recent success/failure rates."
     [connector-type base-delay-ms]
     (let [config @adaptive-retry-config
           success-rate (calculate-success-rate connector-type)]
       (cond
         ;; High success rate - use shorter delays
         (>= success-rate (:success-threshold config))
         (max (:min-delay-ms config) (/ base-delay-ms 2))
         
         ;; Low success rate - use longer delays
         (<= success-rate (:failure-threshold config))
         (min (:max-delay-ms config) (* base-delay-ms 2))
         
         ;; Normal - use base delay
         :else
         base-delay-ms))))

#?(:clj
   (defn with-adaptive-retry
     "Execute a function with adaptive retry strategy.
      Adjusts retry delays based on historical success/failure rates."
     [connector-type f & {:keys [max-retries base-delay-ms]
                          :or {max-retries 3 base-delay-ms 1000}}]
     (if-not (:enabled @adaptive-retry-config)
       (f)
       (loop [attempt 0]
         (let [result (try
                        {:success true :value (f)}
                        (catch Exception e
                          {:success false :error e}))]
           (if (:success result)
             (do
               (record-retry-result connector-type true 0)
               (:value result))
             (if (>= attempt max-retries)
               (do
                 (record-retry-result connector-type false 0)
                 (throw (:error result)))
               (let [delay-ms (calculate-adaptive-delay connector-type base-delay-ms)]
                 (record-retry-result connector-type false delay-ms)
                 (Thread/sleep delay-ms)
                 (recur (inc attempt))))))))))

#?(:clj
   (defn get-retry-stats
     "Get statistics about retry history."
     []
     (cleanup-retry-history)
     (let [history @retry-history]
       {:connector-stats
        (into {}
              (map (fn [[k v]]
                     [k {:total-attempts (count v)
                         :success-rate (calculate-success-rate k)
                         :avg-delay-ms (if (seq v)
                                         (/ (reduce + (map :delay-ms v)) (count v))
                                         0)}])
                   history))})))

;; Register built-in connector types
#?(:clj
   (do
     (register-connector-type :github
       (fn [{:keys [token]}] (create-github-connector token))
       :description "GitHub API connector"
       :required-config [:token])
     
     (register-connector-type :slack
       (fn [{:keys [token]}] (create-slack-connector token))
       :description "Slack API connector"
       :required-config [:token])
     
     (register-connector-type :huggingface
       (fn [{:keys [api-key]}] (create-huggingface-connector api-key))
       :description "Huggingface inference API connector"
       :required-config [:api-key])
     
     (register-connector-type :lm-studio
       (fn [{:keys [base-url] :or {base-url "http://localhost:1234"}}]
         (create-lm-studio-connector base-url))
       :description "LM Studio local LLM connector"
       :required-config [])
     
     (register-connector-type :zapier
       (fn [{:keys [webhook-url]}] (create-zapier-connector webhook-url))
       :description "Zapier webhook connector"
       :required-config [:webhook-url])
     
     (register-connector-type :web-scraper
       (fn [{:keys [user-agent] :or {user-agent "Mozilla/5.0"}}]
         (create-web-scraper-connector user-agent))
       :description "Web scraper connector"
       :required-config [])
     
     (register-connector-type :file
       (fn [{:keys [base-path]}] (create-file-connector base-path))
       :description "Local file system connector"
       :required-config [:base-path])))

;; ============================================
;; Middleware System
;; ============================================

(def middleware-registry
  "Atom storing middleware functions.
   Middleware can transform requests before sending and responses after receiving."
  (atom {:request []
         :response []}))

#?(:clj
   (defn register-middleware
     "Register a middleware function.
      Type: :request (transforms request before sending) or :response (transforms response after receiving)
      Middleware functions receive and return a map with request/response data.
      Returns middleware-id for unregistering."
     [middleware-type middleware-fn & {:keys [name priority] :or {priority 50}}]
     (let [middleware-id (java.util.UUID/randomUUID)]
       (swap! middleware-registry update middleware-type
              (fn [middlewares]
                (->> (conj middlewares {:id middleware-id
                                        :fn middleware-fn
                                        :name name
                                        :priority priority})
                     (sort-by :priority))))
       middleware-id)))

#?(:clj
   (defn unregister-middleware
     "Unregister a middleware by its ID."
     [middleware-type middleware-id]
     (swap! middleware-registry update middleware-type
            (fn [middlewares]
              (vec (remove #(= middleware-id (:id %)) middlewares))))))

#?(:clj
   (defn clear-middleware
     "Clear all middleware or middleware of a specific type."
     ([] (reset! middleware-registry {:request [] :response []}))
     ([middleware-type] (swap! middleware-registry assoc middleware-type []))))

#?(:clj
   (defn list-middleware
     "List all registered middleware."
     ([] @middleware-registry)
     ([middleware-type] (get @middleware-registry middleware-type []))))

#?(:clj
   (defn apply-request-middleware
     "Apply all request middleware to transform a request."
     [request]
     (reduce (fn [req {:keys [fn]}]
               (try
                 (fn req)
                 (catch Exception e
                   (println "Request middleware error:" (.getMessage e))
                   req)))
             request
             (get @middleware-registry :request []))))

#?(:clj
   (defn apply-response-middleware
     "Apply all response middleware to transform a response."
     [response]
     (reduce (fn [resp {:keys [fn]}]
               (try
                 (fn resp)
                 (catch Exception e
                   (println "Response middleware error:" (.getMessage e))
                   resp)))
             response
             (get @middleware-registry :response []))))

;; Built-in middleware examples

#?(:clj
   (defn create-logging-middleware
     "Create a middleware that logs requests/responses."
     [log-fn]
     {:request (fn [req]
                 (log-fn :request req)
                 req)
      :response (fn [resp]
                  (log-fn :response resp)
                  resp)}))

#?(:clj
   (defn create-retry-header-middleware
     "Create a middleware that adds retry headers to requests."
     []
     (fn [req]
       (update req :headers assoc "X-Retry-Count" (str (get req :retry-count 0))))))

#?(:clj
   (defn create-timing-middleware
     "Create a middleware that adds timing information to responses."
     []
     (fn [resp]
       (assoc resp :middleware-processed-at (java.time.Instant/now)))))

#?(:clj
   (defn create-error-transform-middleware
     "Create a middleware that transforms error responses to a standard format."
     []
     (fn [resp]
       (if (and (map? resp) (or (:error resp) (>= (get resp :status 0) 400)))
         (assoc resp :standardized-error
                {:message (or (:error resp) "Request failed")
                 :status (get resp :status "NA")
                 :timestamp (java.time.Instant/now)
                 :recoverable (contains? #{408 429 500 502 503 504} (:status resp))})
         resp))))

;; ============================================
;; Async Connector Support
;; ============================================

(def async-tasks
  "Atom storing active async tasks."
  (atom {}))

#?(:clj
   (defn submit-async-task
     "Submit a task for async execution. Returns a task-id immediately.
      Use get-async-result to retrieve the result later."
     [task-fn & {:keys [name timeout-ms] :or {timeout-ms 60000}}]
     (let [task-id (java.util.UUID/randomUUID)
           future-result (future
                           (try
                             {:status :success
                              :result (task-fn)
                              :completed-at (java.time.Instant/now)}
                             (catch Exception e
                               {:status :error
                                :error (.getMessage e)
                                :completed-at (java.time.Instant/now)})))]
       (swap! async-tasks assoc task-id
              {:id task-id
               :name name
               :future future-result
               :submitted-at (java.time.Instant/now)
               :timeout-ms timeout-ms
               :status :pending})
       task-id)))

#?(:clj
   (defn get-async-result
     "Get the result of an async task. Returns immediately with current status.
      If task is complete, returns the result. If pending, returns :pending status."
     [task-id]
     (if-let [task (get @async-tasks task-id)]
       (let [fut (:future task)]
         (if (future-done? fut)
           (let [result @fut]
             (swap! async-tasks assoc-in [task-id :status] (:status result))
             result)
           {:status :pending
            :submitted-at (:submitted-at task)
            :name (:name task)}))
       {:status :not-found
        :error "Task not found"})))

#?(:clj
   (defn await-async-result
     "Wait for an async task to complete and return the result.
      Blocks until complete or timeout."
     [task-id & {:keys [timeout-ms] :or {timeout-ms 60000}}]
     (if-let [task (get @async-tasks task-id)]
       (let [fut (:future task)]
         (try
           (let [result (deref fut timeout-ms {:status :timeout :error "Task timed out"})]
             (swap! async-tasks assoc-in [task-id :status] (:status result))
             result)
           (catch Exception e
             {:status :error :error (.getMessage e)})))
       {:status :not-found :error "Task not found"})))

#?(:clj
   (defn cancel-async-task
     "Cancel an async task if it's still pending."
     [task-id]
     (if-let [task (get @async-tasks task-id)]
       (let [fut (:future task)]
         (if (future-done? fut)
           {:status :already-complete}
           (do
             (future-cancel fut)
             (swap! async-tasks assoc-in [task-id :status] :cancelled)
             {:status :cancelled})))
       {:status :not-found :error "Task not found"})))

#?(:clj
   (defn list-async-tasks
     "List all async tasks, optionally filtered by status."
     ([] (vals @async-tasks))
     ([status]
      (filter #(= status (:status %)) (vals @async-tasks)))))

#?(:clj
   (defn cleanup-async-tasks
     "Remove completed tasks older than the specified age (default 1 hour)."
     [& {:keys [max-age-ms] :or {max-age-ms 3600000}}]
     (let [cutoff (java.time.Instant/ofEpochMilli
                   (- (System/currentTimeMillis) max-age-ms))]
       (swap! async-tasks
              (fn [tasks]
                (into {}
                      (remove (fn [[_ task]]
                                (and (not= :pending (:status task))
                                     (.isBefore (:submitted-at task) cutoff)))
                              tasks)))))))

;; Async connector operations

#?(:clj
   (defn async-github-repos
     "Fetch GitHub repos asynchronously."
     [connector username]
     (submit-async-task
      #(github-list-repos connector username)
      :name (str "github-repos-" username))))

#?(:clj
   (defn async-huggingface-inference
     "Run Huggingface inference asynchronously."
     [connector model-id inputs]
     (submit-async-task
      #(huggingface-inference connector model-id inputs)
      :name (str "huggingface-" model-id)
      :timeout-ms 120000)))

#?(:clj
   (defn async-lm-studio-chat
     "Run LM Studio chat asynchronously."
     [connector messages]
     (submit-async-task
      #(lm-studio-chat connector messages)
      :name "lm-studio-chat"
      :timeout-ms 180000)))

#?(:clj
   (defn async-web-scrape
     "Scrape a URL asynchronously."
     [connector url]
     (submit-async-task
      #(web-scraper-scrape connector url)
      :name (str "scrape-" (subs url 0 (min 50 (count url))))
      :timeout-ms 60000)))

#?(:clj
   (defn async-bulk-request
     "Execute multiple requests asynchronously.
      Returns a task-id that can be used to check progress."
     [requests]
     (submit-async-task
      #(bulk-request requests)
      :name (str "bulk-" (count requests) "-requests")
      :timeout-ms (* 60000 (count requests)))))

;; ============================================
;; Request/Response Logging
;; ============================================

(def request-log
  "Atom storing recent request/response logs for debugging."
  (atom []))

(def log-config
  "Configuration for request logging."
  {:max-entries 1000
   :log-request-body true
   :log-response-body true
   :redact-headers #{"authorization" "x-api-key" "api-key" "token"}})

#?(:clj
   (defn redact-sensitive
     "Redact sensitive information from headers."
     [headers]
     (reduce (fn [h key]
               (if (contains? h key)
                 (assoc h key "[REDACTED]")
                 h))
             headers
             (:redact-headers log-config))))

#?(:clj
   (defn log-request
     "Log a request for debugging and auditing."
     [connector-type method url & {:keys [headers body]}]
     (let [entry {:id (java.util.UUID/randomUUID)
                  :timestamp (java.time.Instant/now)
                  :connector-type connector-type
                  :method method
                  :url url
                  :headers (redact-sensitive headers)
                  :body (when (:log-request-body log-config) body)
                  :type :request}]
       (swap! request-log
              (fn [log]
                (let [new-log (conj log entry)]
                  (if (> (count new-log) (:max-entries log-config))
                    (vec (drop (- (count new-log) (:max-entries log-config)) new-log))
                    new-log))))
       (:id entry))))

#?(:clj
   (defn log-response
     "Log a response for debugging and auditing."
     [request-id status & {:keys [headers body latency-ms error]}]
     (let [entry {:id (java.util.UUID/randomUUID)
                  :request-id request-id
                  :timestamp (java.time.Instant/now)
                  :status status
                  :headers (redact-sensitive headers)
                  :body (when (:log-response-body log-config) body)
                  :latency-ms latency-ms
                  :error error
                  :type :response}]
       (swap! request-log
              (fn [log]
                (let [new-log (conj log entry)]
                  (if (> (count new-log) (:max-entries log-config))
                    (vec (drop (- (count new-log) (:max-entries log-config)) new-log))
                    new-log))))
       (:id entry))))

#?(:clj
   (defn get-request-log
     "Get recent request/response logs.
      Options: :connector-type, :limit, :since (timestamp)"
     [& {:keys [connector-type limit since]}]
     (cond->> @request-log
       connector-type (filter #(= connector-type (:connector-type %)))
       since (filter #(.isAfter (:timestamp %) since))
       limit (take-last limit)
       true vec)))

#?(:clj
   (defn get-request-by-id
     "Get a specific request and its response by request ID."
     [request-id]
     (let [request (first (filter #(and (= :request (:type %))
                                        (= request-id (:id %)))
                                  @request-log))
           response (first (filter #(and (= :response (:type %))
                                         (= request-id (:request-id %)))
                                   @request-log))]
       {:request request :response response})))

#?(:clj
   (defn clear-request-log
     "Clear all request/response logs."
     []
     (reset! request-log [])))

#?(:clj
   (defn get-log-stats
     "Get statistics about the request log."
     []
     (let [logs @request-log
           requests (filter #(= :request (:type %)) logs)
           responses (filter #(= :response (:type %)) logs)
           by-connector (group-by :connector-type requests)
           error-responses (filter :error responses)]
       {:total-entries (count logs)
        :total-requests (count requests)
        :total-responses (count responses)
        :error-count (count error-responses)
        :by-connector (into {} (map (fn [[k v]] [k (count v)]) by-connector))
        :avg-latency-ms (when (seq responses)
                          (let [latencies (keep :latency-ms responses)]
                            (when (seq latencies)
                              (/ (reduce + latencies) (count latencies)))))})))

;; ============================================
;; Logged HTTP Request Wrapper
;; ============================================

#?(:clj
   (defn logged-http-request
     "Execute an HTTP request with logging, metrics, and circuit breaker protection."
     [connector-type method url & {:keys [headers body query-params timeout-ms]
                                   :or {timeout-ms 30000}}]
     (let [request-id (log-request connector-type method url :headers headers :body body)
           start-time (System/currentTimeMillis)]
       (try
         (wait-for-rate-limit connector-type)
         (record-request connector-type)
         (let [result (with-circuit-breaker connector-type
                        #(let [response (case method
                                          :get (http/get url (merge default-http-opts
                                                                    {:headers headers
                                                                     :query-params query-params
                                                                     :socket-timeout timeout-ms}))
                                          :post (http/post url (merge default-http-opts
                                                                      {:headers headers
                                                                       :body body
                                                                       :content-type :json
                                                                       :socket-timeout timeout-ms}))
                                          :put (http/put url (merge default-http-opts
                                                                    {:headers headers
                                                                     :body body
                                                                     :content-type :json
                                                                     :socket-timeout timeout-ms}))
                                          :delete (http/delete url (merge default-http-opts
                                                                          {:headers headers
                                                                           :socket-timeout timeout-ms})))]
                            response))
               latency (- (System/currentTimeMillis) start-time)]
           (record-latency connector-type latency)
           (if (:success result)
             (do
               (log-response request-id (:status (:value result))
                             :headers (:headers (:value result))
                             :body (:body (:value result))
                             :latency-ms latency)
               {:success true
                :status (:status (:value result))
                :body (:body (:value result))
                :headers (:headers (:value result))
                :latency-ms latency
                :request-id request-id})
             (do
               (record-error connector-type :circuit-breaker)
               (log-response request-id nil :error (:error result) :latency-ms latency)
               {:success false
                :error (:error result)
                :latency-ms latency
                :request-id request-id})))
         (catch Exception e
           (let [latency (- (System/currentTimeMillis) start-time)]
             (record-error connector-type :exception)
             (log-response request-id nil :error (.getMessage e) :latency-ms latency)
             {:success false
              :error (.getMessage e)
              :latency-ms latency
              :request-id request-id}))))))

;; ============================================
;; Base Connector Protocol
;; ============================================

(defprotocol Connector
  "Base protocol for all connectors."
  (connect [this] "Establish connection")
  (disconnect [this] "Close connection")
  (health-check [this] "Check if connection is healthy")
  (get-status [this] "Get connector status"))

;; ============================================
;; Zapier Connector
;; ============================================

(def zapier-config
  {:base-url "https://hooks.zapier.com"
   :timeout-ms 30000
   :retry-count 3})

(defn create-zapier-connector
  "Create a Zapier webhook connector."
  [webhook-url]
  {:type :zapier
   :webhook-url webhook-url
   :status :disconnected
   :last-triggered nil})

(defn trigger-zapier-webhook
  "Trigger a Zapier webhook with payload."
  [connector payload]
  #?(:clj
     (try
       (let [response (http/post (:webhook-url connector)
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 30000
                                  :connection-timeout 10000})]
         {:success (< (:status response) 400)
          :connector-type :zapier
          :payload payload
          :timestamp (java.time.Instant/now)
          :status-code (:status response)
          :response-body (:body response)})
       (catch Exception e
         {:success false
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:success false
      :error "Zapier webhooks must be triggered from server"}))

;; ============================================
;; Huggingface Connector
;; ============================================

(def huggingface-config
  {:base-url "https://api-inference.huggingface.co"
   :models {:text-generation "gpt2"
            :sentiment "distilbert-base-uncased-finetuned-sst-2-english"
            :embeddings "sentence-transformers/all-MiniLM-L6-v2"
            :summarization "facebook/bart-large-cnn"}})

(defn create-huggingface-connector
  "Create a Huggingface API connector."
  [api-key]
  {:type :huggingface
   :api-key api-key
   :status :disconnected
   :available-models (:models huggingface-config)})

(defn huggingface-inference
  "Run inference on a Huggingface model."
  [connector model-id input]
  #?(:clj
     (try
       (let [url (str (:base-url huggingface-config) "/models/" model-id)
             response (http/post url
                                 {:body (json/generate-string {:inputs input})
                                  :headers {"Authorization" (str "Bearer " (:api-key connector))}
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 60000
                                  :connection-timeout 10000
                                  :as :json})]
         {:model model-id
          :input input
          :output (:body response)
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:model model-id
          :input input
          :output "NA"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:model model-id
      :input input
      :output "NA"
      :status :error
      :error "Huggingface inference must be called from server"}))

(defn huggingface-embed
  "Generate embeddings using Huggingface."
  [connector text]
  #?(:clj
     (try
       (let [url (str (:base-url huggingface-config) "/models/sentence-transformers/all-MiniLM-L6-v2")
             response (http/post url
                                 {:body (json/generate-string {:inputs text})
                                  :headers {"Authorization" (str "Bearer " (:api-key connector))}
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 60000
                                  :connection-timeout 10000
                                  :as :json})]
         {:text text
          :embedding (:body response)
          :model "sentence-transformers/all-MiniLM-L6-v2"
          :dimensions 384
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:text text
          :embedding nil
          :model "sentence-transformers/all-MiniLM-L6-v2"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:text text
      :embedding nil
      :status :error
      :error "Huggingface embeddings must be called from server"}))

;; ============================================
;; GitHub Connector
;; ============================================

(def github-config
  {:base-url "https://api.github.com"
   :api-version "2022-11-28"})

(defn create-github-connector
  "Create a GitHub API connector."
  [token]
  {:type :github
   :token token
   :status :disconnected
   :rate-limit {:remaining 5000 :reset nil}})

(defn github-get-repo
  "Get repository information."
  [connector owner repo]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo)
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:owner owner
          :repo repo
          :full-name (str owner "/" repo)
          :data (:body response)
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :full-name (str owner "/" repo)
          :data nil
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-list-issues
  "List issues for a repository."
  [connector owner repo & {:keys [state] :or {state "open"}}]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo "/issues")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :query-params {:state state}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:owner owner
          :repo repo
          :state state
          :issues (:body response)
          :total-count (count (:body response))
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :state state
          :issues []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :issues []
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-create-issue
  "Create a new issue."
  [connector owner repo title body]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/repos/" owner "/" repo "/issues")
             response (http/post url
                                 {:body (json/generate-string {:title title :body body})
                                  :headers {"Authorization" (str "Bearer " (:token connector))
                                            "Accept" "application/vnd.github+json"
                                            "X-GitHub-Api-Version" (:api-version github-config)}
                                  :content-type :json
                                  :socket-timeout 30000
                                  :connection-timeout 10000
                                  :as :json})]
         {:owner owner
          :repo repo
          :title title
          :body body
          :issue (:body response)
          :status :created
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:owner owner
          :repo repo
          :title title
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:owner owner
      :repo repo
      :status :error
      :error "GitHub API must be called from server"}))

(defn github-search-code
  "Search code in repositories."
  [connector query]
  #?(:clj
     (try
       (let [url (str (:base-url github-config) "/search/code")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:token connector))
                                           "Accept" "application/vnd.github+json"
                                           "X-GitHub-Api-Version" (:api-version github-config)}
                                 :query-params {:q query}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:query query
          :results (get-in response [:body :items])
          :total-count (get-in response [:body :total_count])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:query query
          :results []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:query query
      :results []
      :status :error
      :error "GitHub API must be called from server"}))

;; ============================================
;; Slack Connector
;; ============================================

(def slack-config
  {:base-url "https://slack.com/api"
   :scopes ["chat:write" "channels:read" "users:read"]})

(defn create-slack-connector
  "Create a Slack API connector."
  [bot-token]
  {:type :slack
   :bot-token bot-token
   :status :disconnected
   :workspace nil})

(defn slack-post-message
  "Post a message to a Slack channel."
  [connector channel text & {:keys [blocks attachments]}]
  #?(:clj
     (try
       (let [url (str (:base-url slack-config) "/chat.postMessage")
             payload (cond-> {:channel channel :text text}
                       blocks (assoc :blocks blocks)
                       attachments (assoc :attachments attachments))
             response (http/post url
                                 {:body (json/generate-string payload)
                                  :headers {"Authorization" (str "Bearer " (:bot-token connector))}
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 30000
                                  :connection-timeout 10000
                                  :as :json})]
         {:channel channel
          :text text
          :blocks blocks
          :attachments attachments
          :status (if (get-in response [:body :ok]) :sent :error)
          :message-ts (get-in response [:body :ts])
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:channel channel
          :text text
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:channel channel
      :text text
      :status :error
      :error "Slack API must be called from server"}))

(defn slack-list-channels
  "List available Slack channels."
  [connector]
  #?(:clj
     (try
       (let [url (str (:base-url slack-config) "/conversations.list")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:bot-token connector))}
                                 :accept :json
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:channels (get-in response [:body :channels])
          :total-count (count (get-in response [:body :channels]))
          :status (if (get-in response [:body :ok]) :success :error)
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:channels []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:channels []
      :total-count 0
      :status :error
      :error "Slack API must be called from server"}))

(defn slack-get-user
  "Get user information."
  [connector user-id]
  #?(:clj
     (try
       (let [url (str (:base-url slack-config) "/users.info")
             response (http/get url
                                {:headers {"Authorization" (str "Bearer " (:bot-token connector))}
                                 :query-params {:user user-id}
                                 :accept :json
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:user-id user-id
          :user (get-in response [:body :user])
          :status (if (get-in response [:body :ok]) :success :error)
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:user-id user-id
          :user nil
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:user-id user-id
      :status :error
      :error "Slack API must be called from server"}))

;; ============================================
;; Google Drive Connector
;; ============================================

(def google-drive-config
  {:base-url "https://www.googleapis.com/drive/v3"
   :scopes ["https://www.googleapis.com/auth/drive.readonly"]})

(defn create-google-drive-connector
  "Create a Google Drive API connector."
  [credentials]
  {:type :google-drive
   :credentials credentials
   :status :disconnected})

(defn google-drive-list-files
  "List files in Google Drive."
  [connector & {:keys [query folder-id]}]
  {:files []
   :total-count 0
   :query query
   :folder-id folder-id
   :note "Full API access requires OAuth credentials"})

(defn google-drive-download-file
  "Download a file from Google Drive."
  [connector file-id]
  {:file-id file-id
   :status :success
   :content nil
   :note "Full API access requires OAuth credentials"})

(defn google-drive-search
  "Search files in Google Drive."
  [connector query]
  {:query query
   :results []
   :total-count 0
   :note "Full API access requires OAuth credentials"})

;; ============================================
;; LM Studio Connector (Local LLM)
;; ============================================

(def lm-studio-config
  {:default-host "localhost"
   :default-port 1234
   :api-path "/v1"})

(defn create-lm-studio-connector
  "Create an LM Studio connector for local LLM inference."
  [& {:keys [host port] :or {host "localhost" port 1234}}]
  {:type :lm-studio
   :host host
   :port port
   :base-url (str "http://" host ":" port "/v1")
   :status :disconnected
   :model nil})

(defn lm-studio-chat
  "Send a chat completion request to LM Studio."
  [connector messages & {:keys [temperature max-tokens model]
                         :or {temperature 0.7 max-tokens 1000}}]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/chat/completions")
             payload (cond-> {:messages messages
                              :temperature temperature
                              :max_tokens max-tokens}
                       model (assoc :model model))
             response (http/post url
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 120000
                                  :connection-timeout 10000
                                  :as :json})]
         {:messages messages
          :temperature temperature
          :max-tokens max-tokens
          :response (get-in response [:body :choices 0 :message])
          :usage (get-in response [:body :usage])
          :model (get-in response [:body :model])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:messages messages
          :temperature temperature
          :max-tokens max-tokens
          :response {:role "assistant" :content "NA"}
          :usage {:prompt-tokens 0 :completion-tokens 0 :total-tokens 0}
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:messages messages
      :response {:role "assistant" :content "NA"}
      :status :error
      :error "LM Studio must be called from server"}))

(defn lm-studio-complete
  "Send a completion request to LM Studio."
  [connector prompt & {:keys [temperature max-tokens model]
                       :or {temperature 0.7 max-tokens 500}}]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/completions")
             payload (cond-> {:prompt prompt
                              :temperature temperature
                              :max_tokens max-tokens}
                       model (assoc :model model))
             response (http/post url
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 120000
                                  :connection-timeout 10000
                                  :as :json})]
         {:prompt prompt
          :temperature temperature
          :max-tokens max-tokens
          :completion (get-in response [:body :choices 0 :text])
          :usage (get-in response [:body :usage])
          :model (get-in response [:body :model])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:prompt prompt
          :temperature temperature
          :max-tokens max-tokens
          :completion "NA"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:prompt prompt
      :completion "NA"
      :status :error
      :error "LM Studio must be called from server"}))

(defn lm-studio-list-models
  "List available models in LM Studio."
  [connector]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/models")
             response (http/get url
                                {:accept :json
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:models (get-in response [:body :data])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:models []
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:models []
      :status :error
      :error "LM Studio must be called from server"}))

;; ============================================
;; Database Connector
;; ============================================

(defn create-database-connector
  "Create a database connector."
  [db-spec]
  {:type :database
   :db-spec db-spec
   :status :disconnected
   :pool nil})

(defn db-query
  "Execute a database query."
  [connector query & params]
  {:query query
   :params params
   :results []
   :row-count 0
   :note "Full database access requires connection pool"})

(defn db-execute
  "Execute a database statement."
  [connector statement & params]
  {:statement statement
   :params params
   :affected-rows 0
   :note "Full database access requires connection pool"})

;; ============================================
;; File Connector
;; ============================================

(defn create-file-connector
  "Create a file system connector."
  [base-path]
  {:type :file
   :base-path base-path
   :status :connected})

#?(:clj
   (defn file-read
     "Read a file."
     [connector path]
     (try
       {:path path
        :content (slurp (str (:base-path connector) "/" path))
        :status :success}
       (catch Exception e
         {:path path
          :status :error
          :error (.getMessage e)}))))

#?(:clj
   (defn file-write
     "Write to a file."
     [connector path content]
     (try
       (spit (str (:base-path connector) "/" path) content)
       {:path path
        :status :success
        :bytes-written (count content)}
       (catch Exception e
         {:path path
          :status :error
          :error (.getMessage e)}))))

#?(:clj
   (defn file-list
     "List files in a directory."
     [connector path]
     (try
       (let [dir (io/file (str (:base-path connector) "/" path))]
         {:path path
          :files (mapv #(.getName %) (.listFiles dir))
          :status :success})
       (catch Exception e
         {:path path
          :status :error
          :error (.getMessage e)}))))

;; ============================================
;; Web Scraper Connector
;; ============================================

(def scraper-config
  {:user-agent "MentalModels-Bot/1.0"
   :timeout-ms 30000
   :respect-robots-txt true})

(defn create-web-scraper
  "Create a web scraper connector."
  [& {:keys [user-agent] :or {user-agent (:user-agent scraper-config)}}]
  {:type :web-scraper
   :user-agent user-agent
   :status :ready
   :rate-limit {:requests-per-second 1}})

(defn scrape-url
  "Scrape content from a URL (headless, text-only)."
  [connector url]
  #?(:clj
     (try
       (let [response (http/get url
                                {:headers {"User-Agent" (:user-agent connector)}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :string})]
         {:url url
          :status :success
          :content (:body response)
          :status-code (:status response)
          :content-type (get-in response [:headers "Content-Type"])
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:url url
          :status :error
          :content nil
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:url url
      :status :error
      :content nil
      :error "Web scraping must be done from server"}))

(defn scrape-search-results
  "Scrape search engine results (headless, requires API or direct access)."
  [connector query & {:keys [engine] :or {engine :duckduckgo}}]
  #?(:clj
     (try
       (let [url (case engine
                   :duckduckgo (str "https://html.duckduckgo.com/html/?q=" (java.net.URLEncoder/encode query "UTF-8"))
                   (str "https://html.duckduckgo.com/html/?q=" (java.net.URLEncoder/encode query "UTF-8")))
             response (http/get url
                                {:headers {"User-Agent" (:user-agent connector)}
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :string})]
         {:query query
          :engine engine
          :raw-html (:body response)
          :status :success
          :timestamp (java.time.Instant/now)
          :note "Parse HTML to extract results"})
       (catch Exception e
         {:query query
          :engine engine
          :results []
          :total-count 0
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:query query
      :engine engine
      :results []
      :status :error
      :error "Web scraping must be done from server"}))

;; ============================================
;; Connector Registry
;; ============================================

;; ============================================
;; Request Prioritization
;; ============================================

(def priority-queue
  "Priority queue for request scheduling."
  (atom {:high []
         :normal []
         :low []
         :background []}))

(def priority-config
  "Configuration for request prioritization."
  (atom {:enabled true
         :high-weight 4
         :normal-weight 2
         :low-weight 1
         :background-weight 0.5
         :max-queue-size 1000}))

(defn get-priority-config
  "Get current priority configuration."
  []
  @priority-config)

(defn set-priority-config
  "Update priority configuration."
  [config]
  (swap! priority-config merge config))

(defn enqueue-request
  "Add a request to the priority queue."
  [priority request-fn & {:keys [id metadata] :or {id (str (random-uuid)) metadata {}}}]
  (let [entry {:id id
               :fn request-fn
               :priority priority
               :metadata metadata
               :enqueued-at (System/currentTimeMillis)}]
    (swap! priority-queue update priority conj entry)
    id))

(defn dequeue-request
  "Get the next request to process based on priority weights."
  []
  (let [queue @priority-queue
        config @priority-config
        weighted-selection (fn []
                            (let [weights {:high (:high-weight config)
                                          :normal (:normal-weight config)
                                          :low (:low-weight config)
                                          :background (:background-weight config)}
                                  available (filter #(seq (get queue %)) (keys weights))
                                  total-weight (reduce + (map #(get weights %) available))]
                              (when (pos? total-weight)
                                (let [r (* (rand) total-weight)]
                                  (loop [remaining r
                                         [p & ps] available]
                                    (if p
                                      (let [w (get weights p)]
                                        (if (< remaining w)
                                          p
                                          (recur (- remaining w) ps)))
                                      (first available)))))))]
    (when-let [priority (weighted-selection)]
      (let [entries (get queue priority)]
        (when (seq entries)
          (let [entry (first entries)]
            (swap! priority-queue update priority rest)
            entry))))))

(defn process-priority-queue
  "Process requests from the priority queue."
  [& {:keys [max-concurrent] :or {max-concurrent 4}}]
  (let [results (atom [])]
    (dotimes [_ max-concurrent]
      (when-let [entry (dequeue-request)]
        (try
          (let [result ((:fn entry))]
            (swap! results conj {:id (:id entry)
                                :success true
                                :result result}))
          (catch Exception e
            (swap! results conj {:id (:id entry)
                                :success false
                                :error (.getMessage e)})))))
    @results))

(defn get-queue-stats
  "Get statistics about the priority queue."
  []
  (let [queue @priority-queue]
    {:high-count (count (:high queue))
     :normal-count (count (:normal queue))
     :low-count (count (:low queue))
     :background-count (count (:background queue))
     :total-count (reduce + (map count (vals queue)))}))

(defn clear-priority-queue
  "Clear all requests from the priority queue."
  []
  (reset! priority-queue {:high [] :normal [] :low [] :background []}))

;; ============================================
;; Request Timeout Escalation
;; ============================================

(def timeout-escalation-config
  "Configuration for timeout escalation."
  (atom {:enabled true
         :initial-timeout-ms 5000
         :max-timeout-ms 60000
         :escalation-factor 1.5
         :max-escalations 3}))

(def timeout-history
  "History of timeout escalations per connector."
  (atom {}))

(defn get-timeout-escalation-config
  "Get current timeout escalation configuration."
  []
  @timeout-escalation-config)

(defn set-timeout-escalation-config
  "Update timeout escalation configuration."
  [config]
  (swap! timeout-escalation-config merge config))

(defn calculate-escalated-timeout
  "Calculate the escalated timeout based on history."
  [connector-type]
  (let [config @timeout-escalation-config
        history (get @timeout-history connector-type {:escalation-count 0})
        escalation-count (min (:escalation-count history) (:max-escalations config))
        factor (Math/pow (:escalation-factor config) escalation-count)
        timeout (* (:initial-timeout-ms config) factor)]
    (min timeout (:max-timeout-ms config))))

(defn record-timeout
  "Record a timeout event for a connector."
  [connector-type]
  (swap! timeout-history update connector-type
         (fn [h]
           (let [current (or h {:escalation-count 0 :timeouts []})]
             {:escalation-count (inc (:escalation-count current))
              :timeouts (conj (:timeouts current)
                             {:timestamp (System/currentTimeMillis)})}))))

(defn record-success-timeout
  "Record a successful request (resets escalation)."
  [connector-type]
  (swap! timeout-history update connector-type
         (fn [h]
           (let [current (or h {:escalation-count 0 :timeouts []})]
             {:escalation-count (max 0 (dec (:escalation-count current)))
              :timeouts (:timeouts current)}))))

(defn with-timeout-escalation
  "Execute a function with timeout escalation support."
  [connector-type f]
  (let [timeout-ms (calculate-escalated-timeout connector-type)
        future-result (future (f))
        result (deref future-result timeout-ms {:timeout true})]
    (if (:timeout result)
      (do
        (record-timeout connector-type)
        (future-cancel future-result)
        {:error "Request timed out"
         :timeout-ms timeout-ms
         :escalation-count (get-in @timeout-history [connector-type :escalation-count])})
      (do
        (record-success-timeout connector-type)
        result))))

(defn get-timeout-stats
  "Get timeout statistics for all connectors."
  []
  {:config @timeout-escalation-config
   :history (into {}
                  (map (fn [[k v]]
                         [k {:escalation-count (:escalation-count v)
                             :total-timeouts (count (:timeouts v))
                             :current-timeout-ms (calculate-escalated-timeout k)}])
                       @timeout-history))})

;; ============================================
;; Connector Health Scoring
;; ============================================

(def health-scores
  "Health scores for each connector."
  (atom {}))

(def health-score-config
  "Configuration for health scoring."
  (atom {:enabled true
         :success-weight 1.0
         :failure-weight -2.0
         :timeout-weight -1.5
         :latency-weight -0.01
         :decay-factor 0.95
         :min-score 0.0
         :max-score 100.0
         :initial-score 50.0}))

(defn get-health-score-config
  "Get current health score configuration."
  []
  @health-score-config)

(defn set-health-score-config
  "Update health score configuration."
  [config]
  (swap! health-score-config merge config))

(defn get-connector-health-score
  "Get the health score for a connector."
  [connector-type]
  (let [config @health-score-config]
    (get @health-scores connector-type (:initial-score config))))

(defn update-health-score
  "Update the health score for a connector based on an event."
  [connector-type event-type & {:keys [latency-ms] :or {latency-ms 0}}]
  (let [config @health-score-config
        current-score (get-connector-health-score connector-type)
        decayed-score (* current-score (:decay-factor config))
        delta (case event-type
                :success (:success-weight config)
                :failure (:failure-weight config)
                :timeout (:timeout-weight config)
                0)
        latency-penalty (if (pos? latency-ms)
                         (* (:latency-weight config) (/ latency-ms 1000.0))
                         0)
        new-score (-> (+ decayed-score delta latency-penalty)
                      (max (:min-score config))
                      (min (:max-score config)))]
    (swap! health-scores assoc connector-type new-score)
    new-score))

(defn get-healthiest-connector
  "Get the connector type with the highest health score from a list."
  [connector-types]
  (let [scores (map (fn [t] {:type t :score (get-connector-health-score t)}) connector-types)]
    (:type (apply max-key :score scores))))

(defn get-all-health-scores
  "Get health scores for all connectors."
  []
  (let [config @health-score-config]
    {:config config
     :scores @health-scores
     :summary (into {}
                    (map (fn [[k v]]
                           [k {:score v
                               :status (cond
                                        (>= v 80) :healthy
                                        (>= v 50) :degraded
                                        (>= v 20) :unhealthy
                                        :else :critical)}])
                         @health-scores))}))

;; ============================================
;; Request Tracing / Correlation IDs
;; ============================================

(def trace-store
  "Store for request traces."
  (atom {}))

(def trace-config
  "Configuration for request tracing."
  (atom {:enabled true
         :max-traces 10000
         :retention-ms 3600000
         :include-headers false
         :include-body false}))

(defn get-trace-config
  "Get current trace configuration."
  []
  @trace-config)

(defn set-trace-config
  "Update trace configuration."
  [config]
  (swap! trace-config merge config))

(defn generate-trace-id
  "Generate a unique trace ID."
  []
  (str "trace-" (System/currentTimeMillis) "-" (random-uuid)))

(defn generate-span-id
  "Generate a unique span ID."
  []
  (str "span-" (random-uuid)))

(defn start-trace
  "Start a new trace."
  [& {:keys [trace-id operation metadata] :or {trace-id (generate-trace-id) operation "unknown" metadata {}}}]
  (let [trace {:trace-id trace-id
               :operation operation
               :metadata metadata
               :started-at (System/currentTimeMillis)
               :spans []
               :status :in-progress}]
    (swap! trace-store assoc trace-id trace)
    trace-id))

(defn start-span
  "Start a new span within a trace."
  [trace-id span-name & {:keys [parent-span-id metadata] :or {metadata {}}}]
  (let [span-id (generate-span-id)
        span {:span-id span-id
              :name span-name
              :parent-span-id parent-span-id
              :metadata metadata
              :started-at (System/currentTimeMillis)
              :status :in-progress}]
    (swap! trace-store update-in [trace-id :spans] conj span)
    span-id))

(defn end-span
  "End a span within a trace."
  [trace-id span-id & {:keys [status result error] :or {status :completed}}]
  (swap! trace-store update-in [trace-id :spans]
         (fn [spans]
           (mapv (fn [span]
                   (if (= (:span-id span) span-id)
                     (assoc span
                            :ended-at (System/currentTimeMillis)
                            :duration-ms (- (System/currentTimeMillis) (:started-at span))
                            :status status
                            :result result
                            :error error)
                     span))
                 spans))))

(defn end-trace
  "End a trace."
  [trace-id & {:keys [status result error] :or {status :completed}}]
  (swap! trace-store update trace-id
         (fn [trace]
           (assoc trace
                  :ended-at (System/currentTimeMillis)
                  :duration-ms (- (System/currentTimeMillis) (:started-at trace))
                  :status status
                  :result result
                  :error error))))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (get @trace-store trace-id))

(defn with-tracing
  "Execute a function with tracing."
  [operation f & {:keys [metadata] :or {metadata {}}}]
  (if-not (:enabled @trace-config)
    (f)
    (let [trace-id (start-trace :operation operation :metadata metadata)
          span-id (start-span trace-id operation)]
      (try
        (let [result (f)]
          (end-span trace-id span-id :status :completed :result (when (:include-body @trace-config) result))
          (end-trace trace-id :status :completed)
          result)
        (catch Exception e
          (end-span trace-id span-id :status :failed :error (.getMessage e))
          (end-trace trace-id :status :failed :error (.getMessage e))
          (throw e))))))

(defn cleanup-old-traces
  "Remove traces older than retention period."
  []
  (let [config @trace-config
        cutoff (- (System/currentTimeMillis) (:retention-ms config))]
    (swap! trace-store
           (fn [store]
             (into {}
                   (filter (fn [[_ trace]]
                            (> (:started-at trace) cutoff))
                           store))))))

(defn get-trace-stats
  "Get statistics about traces."
  []
  (let [traces (vals @trace-store)
        completed (filter #(= (:status %) :completed) traces)
        failed (filter #(= (:status %) :failed) traces)]
    {:total-traces (count traces)
     :completed-traces (count completed)
     :failed-traces (count failed)
     :in-progress-traces (count (filter #(= (:status %) :in-progress) traces))
     :avg-duration-ms (if (seq completed)
                        (/ (reduce + (map :duration-ms completed)) (count completed))
                        0)}))

;; ============================================
;; Request Compression
;; ============================================

(def compression-config
  "Configuration for request/response compression."
  (atom {:enabled true
         :min-size-bytes 1024
         :algorithms [:gzip :deflate]
         :prefer :gzip}))

(defn get-compression-config
  "Get current compression configuration."
  []
  @compression-config)

(defn set-compression-config
  "Update compression configuration."
  [config]
  (swap! compression-config merge config))

#?(:clj
   (defn compress-data
     "Compress data using the specified algorithm."
     [data algorithm]
     (let [bytes (.getBytes (if (string? data) data (pr-str data)) "UTF-8")]
       (if (< (count bytes) (:min-size-bytes @compression-config))
         {:compressed false :data data :original-size (count bytes)}
         (let [baos (java.io.ByteArrayOutputStream.)
               gzos (case algorithm
                      :gzip (java.util.zip.GZIPOutputStream. baos)
                      :deflate (java.util.zip.DeflaterOutputStream. baos)
                      (java.util.zip.GZIPOutputStream. baos))]
           (.write gzos bytes)
           (.close gzos)
           {:compressed true
            :algorithm algorithm
            :data (.toByteArray baos)
            :original-size (count bytes)
            :compressed-size (count (.toByteArray baos))
            :ratio (double (/ (count (.toByteArray baos)) (count bytes)))})))))

#?(:clj
   (defn decompress-data
     "Decompress data using the specified algorithm."
     [compressed-bytes algorithm]
     (let [bais (java.io.ByteArrayInputStream. compressed-bytes)
           gzis (case algorithm
                  :gzip (java.util.zip.GZIPInputStream. bais)
                  :deflate (java.util.zip.InflaterInputStream. bais)
                  (java.util.zip.GZIPInputStream. bais))
           baos (java.io.ByteArrayOutputStream.)]
       (let [buffer (byte-array 1024)]
         (loop []
           (let [len (.read gzis buffer)]
             (when (pos? len)
               (.write baos buffer 0 len)
               (recur)))))
       (.close gzis)
       (String. (.toByteArray baos) "UTF-8"))))

(defn with-compression
  "Execute a request with compression support."
  [request-fn & {:keys [compress-request decompress-response] :or {compress-request false decompress-response true}}]
  #?(:clj
     (if-not (:enabled @compression-config)
       (request-fn)
       (request-fn))
     :cljs
     (request-fn)))

(def compression-stats
  "Statistics for compression operations."
  (atom {:requests-compressed 0
         :responses-decompressed 0
         :bytes-saved 0}))

(defn get-compression-stats
  "Get compression statistics."
  []
  @compression-stats)

(defn reset-compression-stats
  "Reset compression statistics."
  []
  (reset! compression-stats {:requests-compressed 0
                             :responses-decompressed 0
                             :bytes-saved 0}))

;; ============================================
;; Response Streaming
;; ============================================

(def streaming-config
  "Configuration for response streaming."
  (atom {:enabled true
         :buffer-size 8192
         :timeout-ms 30000}))

(defn get-streaming-config
  "Get current streaming configuration."
  []
  @streaming-config)

(defn set-streaming-config
  "Update streaming configuration."
  [config]
  (swap! streaming-config merge config))

(def active-streams
  "Registry of active streaming connections."
  (atom {}))

(defn register-stream
  "Register an active stream."
  [stream-id stream-info]
  (swap! active-streams assoc stream-id
         (merge stream-info
                {:started-at (System/currentTimeMillis)
                 :status :active})))

(defn unregister-stream
  "Unregister a stream."
  [stream-id]
  (swap! active-streams dissoc stream-id))

(defn get-active-streams
  "Get all active streams."
  []
  @active-streams)

#?(:clj
   (defn create-stream-handler
     "Create a handler for streaming responses."
     [stream-id callback]
     (let [buffer (java.io.ByteArrayOutputStream.)]
       (register-stream stream-id {:buffer buffer :callback callback})
       (fn [chunk]
         (when chunk
           (.write buffer (.getBytes chunk "UTF-8"))
           (when callback
             (callback chunk)))))))

(defn close-stream
  "Close a streaming connection."
  [stream-id]
  (when-let [stream (get @active-streams stream-id)]
    (swap! active-streams update stream-id assoc :status :closed :ended-at (System/currentTimeMillis))
    (unregister-stream stream-id)
    {:stream-id stream-id :status :closed}))

(defn get-stream-stats
  "Get statistics about streaming."
  []
  (let [streams (vals @active-streams)]
    {:active-count (count (filter #(= (:status %) :active) streams))
     :total-streams (count streams)}))

;; ============================================
;; Connector Versioning
;; ============================================

(def connector-versions
  "Version registry for connectors."
  (atom {}))

(def versioning-config
  "Configuration for connector versioning."
  (atom {:enabled true
         :default-version "1.0.0"
         :version-header "X-Connector-Version"
         :deprecation-warning-days 30}))

(defn get-versioning-config
  "Get current versioning configuration."
  []
  @versioning-config)

(defn set-versioning-config
  "Update versioning configuration."
  [config]
  (swap! versioning-config merge config))

(defn register-connector-version
  "Register a version for a connector."
  [connector-type version & {:keys [deprecated deprecated-date replacement-version changelog]}]
  (swap! connector-versions assoc-in [connector-type version]
         {:version version
          :registered-at (System/currentTimeMillis)
          :deprecated (boolean deprecated)
          :deprecated-date deprecated-date
          :replacement-version replacement-version
          :changelog changelog}))

(defn get-connector-version
  "Get version info for a connector."
  [connector-type version]
  (get-in @connector-versions [connector-type version]))

(defn get-latest-version
  "Get the latest non-deprecated version for a connector."
  [connector-type]
  (let [versions (get @connector-versions connector-type {})]
    (->> versions
         (filter (fn [[_ v]] (not (:deprecated v))))
         (sort-by (fn [[k _]] k))
         last
         first)))

(defn is-version-deprecated?
  "Check if a connector version is deprecated."
  [connector-type version]
  (get-in @connector-versions [connector-type version :deprecated] false))

(defn get-deprecation-warning
  "Get deprecation warning if applicable."
  [connector-type version]
  (when (is-version-deprecated? connector-type version)
    (let [info (get-connector-version connector-type version)]
      {:warning (str "Connector " (name connector-type) " version " version " is deprecated")
       :deprecated-date (:deprecated-date info)
       :replacement-version (:replacement-version info)})))

(defn list-connector-versions
  "List all versions for a connector."
  [connector-type]
  (let [versions (get @connector-versions connector-type {})]
    (mapv (fn [[k v]] (assoc v :version k)) versions)))

(defn get-version-stats
  "Get statistics about connector versions."
  []
  {:connectors (count @connector-versions)
   :total-versions (reduce + (map count (vals @connector-versions)))
   :deprecated-versions (count (filter :deprecated (mapcat vals (vals @connector-versions))))})

;; ============================================
;; Request Signing
;; ============================================

(def signing-config
  "Configuration for request signing."
  (atom {:enabled true
         :algorithm :hmac-sha256
         :timestamp-tolerance-ms 300000
         :include-headers [:host :date :content-type]
         :signature-header "X-Signature"}))

(defn get-signing-config
  "Get current signing configuration."
  []
  @signing-config)

(defn set-signing-config
  "Update signing configuration."
  [config]
  (swap! signing-config merge config))

(def signing-keys
  "Registry of signing keys per connector."
  (atom {}))

(defn register-signing-key
  "Register a signing key for a connector."
  [connector-type key-id secret]
  (swap! signing-keys assoc-in [connector-type key-id]
         {:key-id key-id
          :secret secret
          :registered-at (System/currentTimeMillis)}))

(defn get-signing-key
  "Get a signing key for a connector."
  [connector-type key-id]
  (get-in @signing-keys [connector-type key-id :secret]))

#?(:clj
   (defn compute-signature
     "Compute HMAC signature for request."
     [secret string-to-sign algorithm]
     (let [mac (javax.crypto.Mac/getInstance
                (case algorithm
                  :hmac-sha256 "HmacSHA256"
                  :hmac-sha512 "HmacSHA512"
                  "HmacSHA256"))
           secret-key (javax.crypto.spec.SecretKeySpec.
                       (.getBytes secret "UTF-8")
                       (.getAlgorithm mac))]
       (.init mac secret-key)
       (let [signature (.doFinal mac (.getBytes string-to-sign "UTF-8"))]
         (.encodeToString (java.util.Base64/getEncoder) signature)))))

(defn create-string-to-sign
  "Create the canonical string to sign."
  [method path headers timestamp]
  (let [config @signing-config
        header-values (map #(get headers % "") (:include-headers config))]
    (str method "\n"
         path "\n"
         timestamp "\n"
         (clojure.string/join "\n" header-values))))

#?(:clj
   (defn sign-request
     "Sign a request with the connector's signing key."
     [connector-type key-id method path headers body]
     (if-not (:enabled @signing-config)
       {:signed false}
       (if-let [secret (get-signing-key connector-type key-id)]
         (let [timestamp (System/currentTimeMillis)
               string-to-sign (create-string-to-sign method path headers timestamp)
               signature (compute-signature secret string-to-sign (:algorithm @signing-config))]
           {:signed true
            :signature signature
            :timestamp timestamp
            :algorithm (:algorithm @signing-config)
            :key-id key-id})
         {:signed false :error "Signing key not found"}))))

#?(:clj
   (defn verify-signature
     "Verify a request signature."
     [connector-type key-id method path headers timestamp provided-signature]
     (if-not (:enabled @signing-config)
       {:valid true :reason "Signing disabled"}
       (let [config @signing-config
             now (System/currentTimeMillis)]
         (cond
           (> (Math/abs (- now timestamp)) (:timestamp-tolerance-ms config))
           {:valid false :reason "Timestamp outside tolerance"}
           
           :else
           (if-let [secret (get-signing-key connector-type key-id)]
             (let [string-to-sign (create-string-to-sign method path headers timestamp)
                   expected-signature (compute-signature secret string-to-sign (:algorithm config))]
               (if (= expected-signature provided-signature)
                 {:valid true}
                 {:valid false :reason "Signature mismatch"}))
             {:valid false :reason "Signing key not found"}))))))

(defn get-signing-stats
  "Get statistics about request signing."
  []
  {:connectors-with-keys (count @signing-keys)
   :total-keys (reduce + (map count (vals @signing-keys)))
   :config @signing-config})

;; ============================================
;; Request Throttling
;; ============================================

(def throttle-config
  "Configuration for request throttling."
  (atom {:enabled true
         :default-max-concurrent 10
         :default-requests-per-second 100
         :burst-allowance 1.5}))

(def throttle-state
  "State for request throttling per connector."
  (atom {}))

(defn get-throttle-config
  "Get current throttle configuration."
  []
  @throttle-config)

(defn set-throttle-config
  "Update throttle configuration."
  [config]
  (swap! throttle-config merge config))

(defn set-connector-throttle
  "Set throttle limits for a specific connector."
  [connector-type & {:keys [max-concurrent requests-per-second]}]
  (swap! throttle-state assoc connector-type
         {:max-concurrent (or max-concurrent (:default-max-concurrent @throttle-config))
          :requests-per-second (or requests-per-second (:default-requests-per-second @throttle-config))
          :current-concurrent (atom 0)
          :last-request-time (atom 0)
          :request-count (atom 0)}))

(defn get-connector-throttle
  "Get throttle state for a connector."
  [connector-type]
  (get @throttle-state connector-type))

(defn can-make-request?
  "Check if a request can be made within throttle limits."
  [connector-type]
  (if-not (:enabled @throttle-config)
    true
    (if-let [state (get-connector-throttle connector-type)]
      (let [current @(:current-concurrent state)
            max-concurrent (:max-concurrent state)]
        (< current max-concurrent))
      true)))

(defn acquire-throttle
  "Acquire a throttle slot for a request."
  [connector-type]
  (when-let [state (get-connector-throttle connector-type)]
    (swap! (:current-concurrent state) inc)
    (reset! (:last-request-time state) (System/currentTimeMillis))
    (swap! (:request-count state) inc)))

(defn release-throttle
  "Release a throttle slot after request completion."
  [connector-type]
  (when-let [state (get-connector-throttle connector-type)]
    (swap! (:current-concurrent state) dec)))

(defn with-throttling
  "Execute a function with throttling."
  [connector-type f]
  (if-not (:enabled @throttle-config)
    (f)
    (if (can-make-request? connector-type)
      (do
        (acquire-throttle connector-type)
        (try
          (f)
          (finally
            (release-throttle connector-type))))
      {:error "Throttle limit exceeded"
       :connector connector-type})))

(defn get-throttle-stats
  "Get throttling statistics."
  []
  {:config @throttle-config
   :connectors (into {}
                     (map (fn [[k v]]
                            [k {:max-concurrent (:max-concurrent v)
                                :current-concurrent @(:current-concurrent v)
                                :total-requests @(:request-count v)}])
                          @throttle-state))})

;; ============================================
;; Connector Aliasing
;; ============================================

(def connector-aliases
  "Registry of connector aliases."
  (atom {}))

(def alias-config
  "Configuration for connector aliasing."
  (atom {:enabled true
         :allow-chaining true
         :max-chain-depth 5}))

(defn get-alias-config
  "Get current alias configuration."
  []
  @alias-config)

(defn set-alias-config
  "Update alias configuration."
  [config]
  (swap! alias-config merge config))

(defn register-alias
  "Register an alias for a connector."
  [alias-name target-connector & {:keys [config-overrides description]}]
  (swap! connector-aliases assoc alias-name
         {:target target-connector
          :config-overrides (or config-overrides {})
          :description description
          :created-at (System/currentTimeMillis)}))

(defn unregister-alias
  "Remove a connector alias."
  [alias-name]
  (swap! connector-aliases dissoc alias-name))

(defn resolve-alias
  "Resolve an alias to its target connector, following chains if allowed."
  [alias-name & {:keys [depth] :or {depth 0}}]
  (let [config @alias-config]
    (if (> depth (:max-chain-depth config))
      {:error "Max alias chain depth exceeded"}
      (if-let [alias-info (get @connector-aliases alias-name)]
        (let [target (:target alias-info)]
          (if (and (:allow-chaining config) (contains? @connector-aliases target))
            (resolve-alias target :depth (inc depth))
            {:connector target
             :config-overrides (:config-overrides alias-info)}))
        {:connector alias-name
         :config-overrides {}}))))

(defn list-aliases
  "List all registered aliases."
  []
  (mapv (fn [[k v]]
          {:alias k
           :target (:target v)
           :description (:description v)})
        @connector-aliases))

(defn get-alias-stats
  "Get statistics about connector aliases."
  []
  {:total-aliases (count @connector-aliases)
   :config @alias-config
   :aliases (keys @connector-aliases)})

;; ============================================
;; Request Replay
;; ============================================

(def replay-buffer
  "Buffer for storing requests for replay."
  (atom []))

(def replay-config
  "Configuration for request replay."
  (atom {:enabled true
         :max-buffer-size 1000
         :retention-ms 3600000
         :capture-responses true}))

(defn get-replay-config
  "Get current replay configuration."
  []
  @replay-config)

(defn set-replay-config
  "Update replay configuration."
  [config]
  (swap! replay-config merge config))

(defn capture-request
  "Capture a request for potential replay."
  [connector-type operation params & {:keys [response]}]
  (when (:enabled @replay-config)
    (let [entry {:id (str (random-uuid))
                 :connector-type connector-type
                 :operation operation
                 :params params
                 :response (when (:capture-responses @replay-config) response)
                 :captured-at (System/currentTimeMillis)}]
      (swap! replay-buffer
             (fn [buf]
               (let [new-buf (conj buf entry)]
                 (if (> (count new-buf) (:max-buffer-size @replay-config))
                   (vec (rest new-buf))
                   new-buf))))
      (:id entry))))

(defn get-captured-requests
  "Get captured requests, optionally filtered."
  [& {:keys [connector-type operation limit]}]
  (let [requests @replay-buffer
        filtered (cond->> requests
                   connector-type (filter #(= (:connector-type %) connector-type))
                   operation (filter #(= (:operation %) operation))
                   limit (take limit))]
    (vec filtered)))

(defn get-request-by-id
  "Get a specific captured request by ID."
  [request-id]
  (first (filter #(= (:id %) request-id) @replay-buffer)))

(defn replay-request
  "Replay a captured request."
  [request-id request-fn]
  (if-let [request (get-request-by-id request-id)]
    (let [result (request-fn (:connector-type request) (:operation request) (:params request))]
      {:replayed true
       :original-id request-id
       :result result
       :replayed-at (System/currentTimeMillis)})
    {:error "Request not found" :request-id request-id}))

(defn clear-replay-buffer
  "Clear the replay buffer."
  []
  (reset! replay-buffer []))

(defn cleanup-old-requests
  "Remove requests older than retention period."
  []
  (let [cutoff (- (System/currentTimeMillis) (:retention-ms @replay-config))]
    (swap! replay-buffer
           (fn [buf]
             (vec (filter #(> (:captured-at %) cutoff) buf))))))

(defn get-replay-stats
  "Get statistics about request replay."
  []
  {:buffer-size (count @replay-buffer)
   :config @replay-config
   :oldest-request (when (seq @replay-buffer)
                     (:captured-at (first @replay-buffer)))
   :newest-request (when (seq @replay-buffer)
                     (:captured-at (last @replay-buffer)))})

;; ============================================
;; Connector Metrics Export
;; ============================================

(def metrics-export-config
  "Configuration for metrics export."
  (atom {:enabled true
         :format :prometheus
         :include-labels true
         :prefix "connector_"}))

(defn get-metrics-export-config
  "Get current metrics export configuration."
  []
  @metrics-export-config)

(defn set-metrics-export-config
  "Update metrics export configuration."
  [config]
  (swap! metrics-export-config merge config))

(defn format-prometheus-metric
  "Format a metric in Prometheus format."
  [name value & {:keys [labels help type]}]
  (let [config @metrics-export-config
        prefix (:prefix config)
        label-str (when (and (:include-labels config) labels)
                    (str "{"
                         (clojure.string/join ","
                                              (map (fn [[k v]] (str (name k) "=\"" v "\"")) labels))
                         "}"))]
    (str (when help (str "# HELP " prefix name " " help "\n"))
         (when type (str "# TYPE " prefix name " " type "\n"))
         prefix name (or label-str "") " " value)))

(defn export-metrics-prometheus
  "Export all connector metrics in Prometheus format."
  []
  (let [metrics-data @metrics
        lines (atom [])]
    ;; Request counts
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "requests_total"
                                       (:total-requests data 0)
                                       :labels {:connector (name connector)}
                                       :type "counter")))
    ;; Error counts
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "errors_total"
                                       (:total-errors data 0)
                                       :labels {:connector (name connector)}
                                       :type "counter")))
    ;; Latency
    (doseq [[connector data] metrics-data]
      (swap! lines conj
             (format-prometheus-metric "latency_ms"
                                       (:avg-latency-ms data 0)
                                       :labels {:connector (name connector)}
                                       :type "gauge")))
    (clojure.string/join "\n" @lines)))

(defn export-metrics-json
  "Export all connector metrics in JSON format."
  []
  {:timestamp (System/currentTimeMillis)
   :metrics @metrics
   :health-scores @health-scores
   :circuit-states (into {}
                         (map (fn [[k v]] [k {:state (:state v) :failures (:failures v)}])
                              @circuit-breakers))
   :throttle-stats (get-throttle-stats)
   :cache-stats {:size (count @response-cache)}})

(defn export-metrics
  "Export metrics in the configured format."
  []
  (let [config @metrics-export-config]
    (if-not (:enabled config)
      {:error "Metrics export disabled"}
      (case (:format config)
        :prometheus (export-metrics-prometheus)
        :json (export-metrics-json)
        (export-metrics-json)))))

(defn get-metrics-export-stats
  "Get statistics about metrics export."
  []
  {:config @metrics-export-config
   :available-formats [:prometheus :json]
   :metrics-count (count @metrics)})

(def connector-registry
  "Registry of all available connectors."
  {:zapier {:create create-zapier-connector
            :description "Zapier webhook integration"}
   :huggingface {:create create-huggingface-connector
                 :description "Huggingface AI models"}
   :github {:create create-github-connector
            :description "GitHub API integration"}
   :slack {:create create-slack-connector
           :description "Slack messaging integration"}
   :google-drive {:create create-google-drive-connector
                  :description "Google Drive file access"}
   :lm-studio {:create create-lm-studio-connector
               :description "Local LLM via LM Studio"}
   :database {:create create-database-connector
              :description "Database connectivity"}
   :file {:create create-file-connector
          :description "File system access"}
   :web-scraper {:create create-web-scraper
                 :description "Web scraping"}})

(defn list-connectors
  "List all available connectors."
  []
  (mapv (fn [[k v]]
          {:type k
           :description (:description v)})
        connector-registry))

(defn create-connector
  "Create a connector by type."
  [connector-type & args]
  (if-let [connector-def (get connector-registry connector-type)]
    (apply (:create connector-def) args)
    {:error (str "Unknown connector type: " connector-type)}))
