(ns mental-models.pipeline.integration.cache-manager
  "Cache Manager Module
   
   Advanced caching:
   - Multiple cache backends
   - TTL and LRU eviction
   - Cache warming
   - Write-through/write-behind
   - Cache statistics"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap LinkedHashMap Executors ScheduledExecutorService TimeUnit]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; CACHE MANAGER STATE
;; =============================================================================

(defonce cache-state (atom {:caches {}
                            :scheduler nil
                            :config {:default-max-size 1000
                                     :default-ttl-ms 300000
                                     :cleanup-interval-ms 60000
                                     :stats-interval-ms 30000}}))

;; =============================================================================
;; CACHE ENTRY
;; =============================================================================

(defn create-entry
  "Create a cache entry."
  [key value ttl-ms]
  {:key key
   :value value
   :created-at (System/currentTimeMillis)
   :expires-at (when ttl-ms (+ (System/currentTimeMillis) ttl-ms))
   :last-accessed (AtomicLong. (System/currentTimeMillis))
   :access-count (AtomicLong. 0)})

(defn is-expired?
  "Check if a cache entry is expired."
  [entry]
  (when-let [expires-at (:expires-at entry)]
    (> (System/currentTimeMillis) expires-at)))

(defn touch!
  "Update last accessed time."
  [entry]
  (.set ^AtomicLong (:last-accessed entry) (System/currentTimeMillis))
  (.incrementAndGet ^AtomicLong (:access-count entry)))

;; =============================================================================
;; LRU CACHE IMPLEMENTATION
;; =============================================================================

(defn create-lru-map
  "Create an LRU map with max size."
  [max-size]
  (proxy [LinkedHashMap] [16 0.75 true]
    (removeEldestEntry [eldest]
      (> (.size this) max-size))))

;; =============================================================================
;; CACHE CREATION
;; =============================================================================

(defn create-cache
  "Create a new cache."
  [cache-id {:keys [max-size ttl-ms eviction-policy write-through-fn write-behind-fn]}]
  (let [config (:config @cache-state)]
    {:id cache-id
     :max-size (or max-size (:default-max-size config))
     :ttl-ms (or ttl-ms (:default-ttl-ms config))
     :eviction-policy (or eviction-policy :lru)
     :write-through-fn write-through-fn
     :write-behind-fn write-behind-fn
     :store (ConcurrentHashMap.)
     :hits (AtomicLong. 0)
     :misses (AtomicLong. 0)
     :evictions (AtomicLong. 0)
     :writes (AtomicLong. 0)
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; CACHE REGISTRATION
;; =============================================================================

(defn register-cache!
  "Register a cache."
  [cache-id opts]
  (log/info "Registering cache" {:id cache-id})
  (let [cache (create-cache cache-id opts)]
    (swap! cache-state assoc-in [:caches cache-id] cache)
    (metrics/inc-counter! :cachemanager/caches-registered)
    cache-id))

(defn unregister-cache!
  "Unregister a cache."
  [cache-id]
  (log/info "Unregistering cache" {:id cache-id})
  (swap! cache-state update :caches dissoc cache-id))

(defn get-cache
  "Get a cache."
  [cache-id]
  (get-in @cache-state [:caches cache-id]))

(defn list-caches
  "List all caches."
  []
  (keys (:caches @cache-state)))

;; =============================================================================
;; CACHE OPERATIONS
;; =============================================================================

(defn cache-get
  "Get a value from the cache."
  [cache-id key]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)
          entry (.get store key)]
      (if (and entry (not (is-expired? entry)))
        (do
          (touch! entry)
          (.incrementAndGet ^AtomicLong (:hits cache))
          (metrics/inc-counter! :cachemanager/cache-hits)
          (:value entry))
        (do
          (when entry
            (.remove store key))
          (.incrementAndGet ^AtomicLong (:misses cache))
          (metrics/inc-counter! :cachemanager/cache-misses)
          nil)))))

(defn cache-put!
  "Put a value in the cache."
  [cache-id key value & {:keys [ttl-ms]}]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)
          entry (create-entry key value (or ttl-ms (:ttl-ms cache)))]
      ;; Check size and evict if necessary
      (when (>= (.size store) (:max-size cache))
        (evict-entries! cache-id 1))
      (.put store key entry)
      (.incrementAndGet ^AtomicLong (:writes cache))
      (metrics/inc-counter! :cachemanager/cache-writes)
      ;; Write-through
      (when-let [write-fn (:write-through-fn cache)]
        (try
          (write-fn key value)
          (catch Exception e
            (log/error "Write-through failed" {:cache cache-id :key key :error (.getMessage e)}))))
      value)))

(defn cache-remove!
  "Remove a value from the cache."
  [cache-id key]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)]
      (.remove store key))))

(defn cache-clear!
  "Clear all entries from the cache."
  [cache-id]
  (when-let [cache (get-cache cache-id)]
    (log/info "Clearing cache" {:id cache-id})
    (let [^ConcurrentHashMap store (:store cache)]
      (.clear store))))

(defn cache-contains?
  "Check if the cache contains a key."
  [cache-id key]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)
          entry (.get store key)]
      (and entry (not (is-expired? entry))))))

(defn cache-size
  "Get the size of the cache."
  [cache-id]
  (when-let [cache (get-cache cache-id)]
    (.size ^ConcurrentHashMap (:store cache))))

;; =============================================================================
;; EVICTION
;; =============================================================================

(defn evict-expired!
  "Evict expired entries from the cache."
  [cache-id]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)
          expired-keys (atom [])]
      (doseq [[k v] store]
        (when (is-expired? v)
          (swap! expired-keys conj k)))
      (doseq [k @expired-keys]
        (.remove store k)
        (.incrementAndGet ^AtomicLong (:evictions cache)))
      (count @expired-keys))))

(defn evict-entries!
  "Evict entries based on eviction policy."
  [cache-id count]
  (when-let [cache (get-cache cache-id)]
    (let [^ConcurrentHashMap store (:store cache)
          policy (:eviction-policy cache)]
      (case policy
        :lru (let [entries (sort-by #(.get ^AtomicLong (:last-accessed (val %))) store)
                   to-evict (take count entries)]
               (doseq [[k _] to-evict]
                 (.remove store k)
                 (.incrementAndGet ^AtomicLong (:evictions cache))))
        :lfu (let [entries (sort-by #(.get ^AtomicLong (:access-count (val %))) store)
                   to-evict (take count entries)]
               (doseq [[k _] to-evict]
                 (.remove store k)
                 (.incrementAndGet ^AtomicLong (:evictions cache))))
        :fifo (let [entries (sort-by #(:created-at (val %)) store)
                    to-evict (take count entries)]
                (doseq [[k _] to-evict]
                  (.remove store k)
                  (.incrementAndGet ^AtomicLong (:evictions cache))))
        ;; Default to LRU
        (let [entries (sort-by #(.get ^AtomicLong (:last-accessed (val %))) store)
              to-evict (take count entries)]
          (doseq [[k _] to-evict]
            (.remove store k)
            (.incrementAndGet ^AtomicLong (:evictions cache))))))))

;; =============================================================================
;; CACHE WARMING
;; =============================================================================

(defn warm-cache!
  "Warm the cache with data."
  [cache-id data-fn & {:keys [keys]}]
  (log/info "Warming cache" {:id cache-id})
  (let [data (if keys
               (into {} (for [k keys] [k (data-fn k)]))
               (data-fn))]
    (doseq [[k v] data]
      (cache-put! cache-id k v))
    (count data)))

(defn warm-cache-async!
  "Warm the cache asynchronously."
  [cache-id data-fn & opts]
  (future
    (apply warm-cache! cache-id data-fn opts)))

;; =============================================================================
;; CACHE-ASIDE PATTERN
;; =============================================================================

(defn get-or-load!
  "Get from cache or load from source."
  [cache-id key load-fn & {:keys [ttl-ms]}]
  (if-let [cached (cache-get cache-id key)]
    cached
    (let [value (load-fn key)]
      (when value
        (cache-put! cache-id key value :ttl-ms ttl-ms))
      value)))

(defmacro with-cache
  "Execute body with caching."
  [[cache-id key & opts] & body]
  `(get-or-load! ~cache-id ~key (fn [_#] ~@body) ~@opts))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-cache-stats
  "Get statistics for a cache."
  [cache-id]
  (when-let [cache (get-cache cache-id)]
    (let [hits (.get ^AtomicLong (:hits cache))
          misses (.get ^AtomicLong (:misses cache))
          total (+ hits misses)]
      {:id cache-id
       :size (.size ^ConcurrentHashMap (:store cache))
       :max-size (:max-size cache)
       :hits hits
       :misses misses
       :writes (.get ^AtomicLong (:writes cache))
       :evictions (.get ^AtomicLong (:evictions cache))
       :hit-rate (if (pos? total) (* 100.0 (/ hits total)) 0.0)
       :utilization (* 100.0 (/ (.size ^ConcurrentHashMap (:store cache)) (:max-size cache)))})))

(defn get-all-cache-stats
  "Get statistics for all caches."
  []
  (into {} (for [cache-id (list-caches)]
             [cache-id (get-cache-stats cache-id)])))

(defn reset-cache-stats!
  "Reset statistics for a cache."
  [cache-id]
  (when-let [cache (get-cache cache-id)]
    (.set ^AtomicLong (:hits cache) 0)
    (.set ^AtomicLong (:misses cache) 0)
    (.set ^AtomicLong (:writes cache) 0)
    (.set ^AtomicLong (:evictions cache) 0)))

;; =============================================================================
;; MAINTENANCE
;; =============================================================================

(defn cleanup-all-caches!
  "Clean up expired entries from all caches."
  []
  (doseq [cache-id (list-caches)]
    (evict-expired! cache-id)))

(defn start-cleanup-scheduler!
  "Start the cache cleanup scheduler."
  []
  (when (and (flags/is-enabled? "cache-manager")
             (nil? (:scheduler @cache-state)))
    (log/info "Starting cache cleanup scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @cache-state [:config :cleanup-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (cleanup-all-caches!)
                               (catch Exception e
                                 (log/error "Cache cleanup error" {:error (.getMessage e)})))
                            interval
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! cache-state assoc :scheduler executor))))

(defn stop-cleanup-scheduler!
  "Stop the cache cleanup scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @cache-state)]
    (log/info "Stopping cache cleanup scheduler")
    (.shutdown executor)
    (swap! cache-state assoc :scheduler nil)))

;; =============================================================================
;; MULTI-LEVEL CACHE
;; =============================================================================

(defn create-multi-level-cache!
  "Create a multi-level cache (L1 -> L2 -> ...)."
  [cache-id levels]
  (doseq [[idx {:keys [id] :as opts}] (map-indexed vector levels)]
    (register-cache! (or id (keyword (str (name cache-id) "-l" (inc idx)))) opts))
  cache-id)

(defn multi-level-get
  "Get from multi-level cache."
  [cache-ids key]
  (loop [ids cache-ids
         found-at nil]
    (if (empty? ids)
      nil
      (if-let [value (cache-get (first ids) key)]
        (do
          ;; Populate higher levels
          (when found-at
            (doseq [id (take found-at cache-ids)]
              (cache-put! id key value)))
          value)
        (recur (rest ids) (if found-at found-at (count (take-while #(not= % (first ids)) cache-ids))))))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-cache-manager!
  "Initialize cache manager."
  []
  (log/info "Initializing cache manager")
  ;; Register feature flag
  (flags/register-flag! "cache-manager" "Enable cache manager" true)
  ;; Create metrics
  (metrics/create-counter! :cachemanager/caches-registered "Caches registered")
  (metrics/create-counter! :cachemanager/cache-hits "Cache hits")
  (metrics/create-counter! :cachemanager/cache-misses "Cache misses")
  (metrics/create-counter! :cachemanager/cache-writes "Cache writes")
  (metrics/create-gauge! :cachemanager/total-caches "Total caches"
                         #(count (:caches @cache-state)))
  (log/info "Cache manager initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-cache-manager-status []
  {:enabled (flags/is-enabled? "cache-manager")
   :caches (count (:caches @cache-state))
   :scheduler-active (some? (:scheduler @cache-state))
   :stats (get-all-cache-stats)
   :config (:config @cache-state)})
