(ns mental-models.pipeline.integration.response-cacher
  "Response cacher for mental model analysis system.
   
   Features:
   - Response caching
   - Cache key generation
   - TTL management
   - Cache invalidation
   - Conditional caching
   - Cache headers
   - Cache warming
   - Cache metrics"
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
           [java.security MessageDigest]
           [java.util Base64]
           [java.util.concurrent ConcurrentHashMap]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:cache (ConcurrentHashMap.)
         :config {:enabled? true
                  :default-ttl-ms 300000      ;; 5 minutes
                  :max-entries 10000
                  :max-entry-size 1048576     ;; 1MB
                  :cacheable-methods #{:get :head}
                  :cacheable-statuses #{200 203 204 206 300 301 308}}
         :stats {:hits 0
                 :misses 0
                 :stores 0
                 :evictions 0
                 :invalidations 0}
         :initialized? false}))

;; ============================================================================
;; Cache Key Generation
;; ============================================================================

(defn- hash-string
  "Hash a string using SHA-256."
  [s]
  (let [md (MessageDigest/getInstance "SHA-256")
        bytes (.digest md (.getBytes s "UTF-8"))]
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn generate-cache-key
  "Generate a cache key for a request."
  [request & {:keys [include-headers vary-headers] :or {include-headers [] vary-headers []}}]
  (let [method (name (get request :method :get))
        uri (get request :uri "/")
        query (get request :query-string "")
        headers (get request :headers {})
        vary-values (map #(get headers (str/lower-case %)) vary-headers)
        key-parts (concat [method uri query] vary-values include-headers)]
    (hash-string (str/join "|" key-parts))))

;; ============================================================================
;; Cache Entry Management
;; ============================================================================

(defn- create-cache-entry
  "Create a cache entry."
  [response ttl-ms]
  {:response response
   :created-at (System/currentTimeMillis)
   :expires-at (+ (System/currentTimeMillis) ttl-ms)
   :hits (atom 0)})

(defn- is-expired?
  "Check if a cache entry is expired."
  [entry]
  (> (System/currentTimeMillis) (:expires-at entry)))

(defn- is-fresh?
  "Check if a cache entry is fresh."
  [entry]
  (not (is-expired? entry)))

;; ============================================================================
;; Cache Operations
;; ============================================================================

(defn cache-get
  "Get a response from cache."
  [cache-key]
  (when (get-in @state [:config :enabled?])
    (let [cache (:cache @state)]
      (when-let [entry (.get cache cache-key)]
        (if (is-fresh? entry)
          (do
            (swap! (:hits entry) inc)
            (swap! state update-in [:stats :hits] inc)
            (:response entry))
          (do
            (.remove cache cache-key)
            (swap! state update-in [:stats :evictions] inc)
            nil))))))

(defn cache-put!
  "Put a response in cache."
  [cache-key response & {:keys [ttl-ms] :or {ttl-ms nil}}]
  (when (get-in @state [:config :enabled?])
    (let [cache (:cache @state)
          max-entries (get-in @state [:config :max-entries])
          max-size (get-in @state [:config :max-entry-size])
          actual-ttl (or ttl-ms (get-in @state [:config :default-ttl-ms]))
          response-size (count (str (:body response)))]
      
      ;; Check size limit
      (when (<= response-size max-size)
        ;; Evict if at capacity
        (when (>= (.size cache) max-entries)
          (let [oldest-key (first (sort-by #(:created-at (.get cache %)) (keys cache)))]
            (when oldest-key
              (.remove cache oldest-key)
              (swap! state update-in [:stats :evictions] inc))))
        
        (.put cache cache-key (create-cache-entry response actual-ttl))
        (swap! state update-in [:stats :stores] inc)
        true))))

(defn cache-remove!
  "Remove an entry from cache."
  [cache-key]
  (let [cache (:cache @state)]
    (when (.remove cache cache-key)
      (swap! state update-in [:stats :invalidations] inc)
      true)))

(defn cache-clear!
  "Clear all cache entries."
  []
  (let [cache (:cache @state)
        count (.size cache)]
    (.clear cache)
    (swap! state update-in [:stats :invalidations] + count)
    count))

;; ============================================================================
;; Cacheability Checks
;; ============================================================================

(defn- is-cacheable-method?
  "Check if request method is cacheable."
  [request]
  (let [method (get request :method :get)
        cacheable-methods (get-in @state [:config :cacheable-methods])]
    (contains? cacheable-methods method)))

(defn- is-cacheable-status?
  "Check if response status is cacheable."
  [response]
  (let [status (get response :status 200)
        cacheable-statuses (get-in @state [:config :cacheable-statuses])]
    (contains? cacheable-statuses status)))

(defn- has-no-cache-header?
  "Check if request has no-cache header."
  [request]
  (let [cache-control (get-in request [:headers "cache-control"] "")]
    (or (str/includes? cache-control "no-cache")
        (str/includes? cache-control "no-store"))))

(defn is-cacheable?
  "Check if a request/response pair is cacheable."
  [request response]
  (and (is-cacheable-method? request)
       (is-cacheable-status? response)
       (not (has-no-cache-header? request))))

;; ============================================================================
;; Cache Headers
;; ============================================================================

(defn- parse-max-age
  "Parse max-age from Cache-Control header."
  [cache-control]
  (when cache-control
    (when-let [match (re-find #"max-age=(\d+)" cache-control)]
      (* 1000 (Long/parseLong (second match))))))

(defn- get-ttl-from-headers
  "Get TTL from response headers."
  [response]
  (let [cache-control (get-in response [:headers "cache-control"])
        max-age (parse-max-age cache-control)]
    (or max-age (get-in @state [:config :default-ttl-ms]))))

(defn add-cache-headers
  "Add cache headers to response."
  [response & {:keys [ttl-ms private?] :or {private? false}}]
  (let [actual-ttl (or ttl-ms (get-in @state [:config :default-ttl-ms]))
        max-age (quot actual-ttl 1000)
        cache-control (if private?
                        (str "private, max-age=" max-age)
                        (str "public, max-age=" max-age))]
    (assoc-in response [:headers "Cache-Control"] cache-control)))

(defn add-etag
  "Add ETag header to response."
  [response]
  (let [body-hash (hash-string (str (:body response)))]
    (assoc-in response [:headers "ETag"] (str "\"" body-hash "\""))))

;; ============================================================================
;; Conditional Requests
;; ============================================================================

(defn- check-if-none-match
  "Check If-None-Match header."
  [request response]
  (let [if-none-match (get-in request [:headers "if-none-match"])
        etag (get-in response [:headers "ETag"])]
    (and if-none-match etag (= if-none-match etag))))

(defn- check-if-modified-since
  "Check If-Modified-Since header."
  [request entry]
  (let [if-modified-since (get-in request [:headers "if-modified-since"])]
    ;; Simplified - would need proper date parsing
    (when if-modified-since
      (< (:created-at entry) (System/currentTimeMillis)))))

(defn handle-conditional-request
  "Handle conditional request (304 Not Modified)."
  [request cached-response]
  (if (check-if-none-match request cached-response)
    {:status 304
     :headers {"ETag" (get-in cached-response [:headers "ETag"])}}
    cached-response))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-cache
  "Ring middleware for response caching."
  [handler & {:keys [key-fn ttl-ms] :or {key-fn generate-cache-key}}]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [cache-key (key-fn request)]
        (if-let [cached (cache-get cache-key)]
          (do
            (logging/log :debug "Cache hit" {:key cache-key})
            (handle-conditional-request request cached))
          (let [response (handler request)]
            (swap! state update-in [:stats :misses] inc)
            (when (is-cacheable? request response)
              (let [actual-ttl (or ttl-ms (get-ttl-from-headers response))]
                (cache-put! cache-key response :ttl-ms actual-ttl)))
            response)))
      (handler request))))

(defn wrap-cache-control
  "Ring middleware to add cache control headers."
  [handler & {:keys [ttl-ms private?] :or {ttl-ms 300000 private? false}}]
  (fn [request]
    (let [response (handler request)]
      (if (is-cacheable? request response)
        (add-cache-headers response :ttl-ms ttl-ms :private? private?)
        response))))

;; ============================================================================
;; Cache Warming
;; ============================================================================

(defn warm-cache!
  "Warm the cache with pre-computed responses."
  [entries]
  (doseq [{:keys [key response ttl-ms]} entries]
    (cache-put! key response :ttl-ms ttl-ms))
  (count entries))

(defn warm-cache-from-handler!
  "Warm the cache by calling a handler with requests."
  [handler requests & {:keys [key-fn] :or {key-fn generate-cache-key}}]
  (doseq [request requests]
    (let [cache-key (key-fn request)
          response (handler request)]
      (when (is-cacheable? request response)
        (cache-put! cache-key response))))
  (count requests))

;; ============================================================================
;; Cache Invalidation Patterns
;; ============================================================================

(defn invalidate-by-pattern!
  "Invalidate cache entries matching a pattern."
  [pattern]
  (let [cache (:cache @state)
        keys-to-remove (filter #(re-matches pattern %) (keys cache))]
    (doseq [k keys-to-remove]
      (.remove cache k))
    (swap! state update-in [:stats :invalidations] + (count keys-to-remove))
    (count keys-to-remove)))

(defn invalidate-by-prefix!
  "Invalidate cache entries with a key prefix."
  [prefix]
  (let [cache (:cache @state)
        keys-to-remove (filter #(str/starts-with? % prefix) (keys cache))]
    (doseq [k keys-to-remove]
      (.remove cache k))
    (swap! state update-in [:stats :invalidations] + (count keys-to-remove))
    (count keys-to-remove)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn enable-cache!
  "Enable caching."
  []
  (swap! state assoc-in [:config :enabled?] true))

(defn disable-cache!
  "Disable caching."
  []
  (swap! state assoc-in [:config :enabled?] false))

(defn set-default-ttl!
  "Set the default TTL."
  [ttl-ms]
  (swap! state assoc-in [:config :default-ttl-ms] ttl-ms))

(defn set-max-entries!
  "Set the maximum number of cache entries."
  [max-entries]
  (swap! state assoc-in [:config :max-entries] max-entries))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-cache-metrics
  "Get cache metrics."
  []
  (let [stats (:stats @state)
        total (+ (:hits stats) (:misses stats))]
    {:hits (:hits stats)
     :misses (:misses stats)
     :hit-rate (if (pos? total) (/ (:hits stats) total) 0)
     :stores (:stores stats)
     :evictions (:evictions stats)
     :invalidations (:invalidations stats)
     :entries (.size (:cache @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-cache-stats
  "Get cache statistics."
  []
  (merge (get-cache-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :default-ttl-ms (get-in @state [:config :default-ttl-ms])
          :max-entries (get-in @state [:config :max-entries])}))

(defn reset-stats!
  "Reset cache statistics."
  []
  (swap! state assoc :stats {:hits 0
                             :misses 0
                             :stores 0
                             :evictions 0
                             :invalidations 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-cacher!
  "Initialize the response cacher."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response cacher initialized")
    (events/emit! :response-cacher-initialized {})
    true))
