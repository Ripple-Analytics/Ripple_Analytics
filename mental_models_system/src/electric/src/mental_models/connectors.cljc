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
