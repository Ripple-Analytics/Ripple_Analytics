(ns mental-models.pipeline.integration.response-cache
  "Response Cache Module
   
   Intelligent caching for API responses:
   - TTL-based expiration
   - LRU eviction
   - Cache invalidation
   - Cache warming
   - Statistics tracking"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.security MessageDigest]))

;; =============================================================================
;; CACHE STATE
;; =============================================================================

(defonce cache-state (atom {:entries (ConcurrentHashMap.)
                            :access-order []
                            :config {:max-size 10000
                                     :default-ttl-ms 300000
                                     :eviction-batch-size 100}
                            :stats {:hits 0
                                    :misses 0
                                    :evictions 0
                                    :invalidations 0}}))

;; =============================================================================
;; CACHE KEY GENERATION
;; =============================================================================

(defn generate-cache-key
  "Generate a cache key from request parameters."
  [& parts]
  (let [key-str (str/join ":" (map str parts))
        digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest (.getBytes key-str "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) hash-bytes))))

(defn request->cache-key
  "Generate a cache key from a request map."
  [{:keys [method url params headers]}]
  (generate-cache-key method url (pr-str (sort params)) (pr-str (sort headers))))

;; =============================================================================
;; CACHE ENTRY MANAGEMENT
;; =============================================================================

(defn create-entry
  "Create a cache entry."
  [value ttl-ms]
  {:value value
   :created-at (System/currentTimeMillis)
   :expires-at (+ (System/currentTimeMillis) ttl-ms)
   :access-count 0
   :last-accessed (System/currentTimeMillis)})

(defn entry-expired?
  "Check if a cache entry has expired."
  [entry]
  (> (System/currentTimeMillis) (:expires-at entry)))

(defn touch-entry
  "Update access time and count for an entry."
  [entry]
  (-> entry
      (update :access-count inc)
      (assoc :last-accessed (System/currentTimeMillis))))

;; =============================================================================
;; CACHE OPERATIONS
;; =============================================================================

(defn cache-get
  "Get a value from the cache."
  [key]
  (when (flags/is-enabled? "response-cache")
    (let [^ConcurrentHashMap entries (:entries @cache-state)
          entry (.get entries key)]
      (if (and entry (not (entry-expired? entry)))
        (do
          (.put entries key (touch-entry entry))
          (swap! cache-state update-in [:stats :hits] inc)
          (metrics/inc-counter! :cache/hits)
          (:value entry))
        (do
          (when entry
            (.remove entries key))
          (swap! cache-state update-in [:stats :misses] inc)
          (metrics/inc-counter! :cache/misses)
          nil)))))

(defn cache-put!
  "Put a value in the cache."
  [key value & {:keys [ttl-ms]}]
  (when (flags/is-enabled? "response-cache")
    (let [ttl (or ttl-ms (get-in @cache-state [:config :default-ttl-ms]))
          ^ConcurrentHashMap entries (:entries @cache-state)
          entry (create-entry value ttl)]
      ;; Check if we need to evict
      (when (>= (.size entries) (get-in @cache-state [:config :max-size]))
        (evict-lru!))
      (.put entries key entry)
      (swap! cache-state update :access-order conj key)
      (metrics/inc-counter! :cache/puts)
      value)))

(defn cache-delete!
  "Delete a value from the cache."
  [key]
  (let [^ConcurrentHashMap entries (:entries @cache-state)]
    (.remove entries key)
    (swap! cache-state update :access-order #(remove #{key} %))
    (swap! cache-state update-in [:stats :invalidations] inc)
    (metrics/inc-counter! :cache/invalidations)))

(defn cache-exists?
  "Check if a key exists in the cache (and is not expired)."
  [key]
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        entry (.get entries key)]
    (and entry (not (entry-expired? entry)))))

;; =============================================================================
;; EVICTION
;; =============================================================================

(defn evict-lru!
  "Evict least recently used entries."
  []
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        batch-size (get-in @cache-state [:config :eviction-batch-size])
        all-entries (into [] (.entrySet entries))
        sorted-entries (sort-by #(get-in (val %) [:last-accessed]) all-entries)
        to-evict (take batch-size sorted-entries)]
    (doseq [entry to-evict]
      (.remove entries (key entry)))
    (swap! cache-state update-in [:stats :evictions] + (count to-evict))
    (metrics/observe-histogram! :cache/evictions (count to-evict))
    (log/debug "Evicted cache entries" {:count (count to-evict)})))

(defn evict-expired!
  "Evict all expired entries."
  []
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        now (System/currentTimeMillis)
        expired-keys (for [[k v] entries
                           :when (> now (:expires-at v))]
                       k)]
    (doseq [key expired-keys]
      (.remove entries key))
    (swap! cache-state update-in [:stats :evictions] + (count expired-keys))
    (log/debug "Evicted expired entries" {:count (count expired-keys)})))

;; =============================================================================
;; CACHE INVALIDATION
;; =============================================================================

(defn invalidate-pattern!
  "Invalidate all keys matching a pattern."
  [pattern]
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        regex (re-pattern pattern)
        matching-keys (filter #(re-find regex (str %)) (keys entries))]
    (doseq [key matching-keys]
      (.remove entries key))
    (swap! cache-state update-in [:stats :invalidations] + (count matching-keys))
    (log/info "Invalidated cache entries" {:pattern pattern :count (count matching-keys)})))

(defn invalidate-by-tag!
  "Invalidate all entries with a specific tag."
  [tag]
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        tagged-keys (for [[k v] entries
                          :when (contains? (:tags v) tag)]
                      k)]
    (doseq [key tagged-keys]
      (.remove entries key))
    (swap! cache-state update-in [:stats :invalidations] + (count tagged-keys))))

(defn clear-cache!
  "Clear the entire cache."
  []
  (let [^ConcurrentHashMap entries (:entries @cache-state)]
    (.clear entries)
    (swap! cache-state assoc :access-order [])
    (log/info "Cache cleared")))

;; =============================================================================
;; CACHE WARMING
;; =============================================================================

(defn warm-cache!
  "Warm the cache with pre-computed values."
  [entries-map & {:keys [ttl-ms]}]
  (log/info "Warming cache" {:count (count entries-map)})
  (doseq [[key value] entries-map]
    (cache-put! key value :ttl-ms ttl-ms))
  (events/publish! :cache/warmed {:count (count entries-map)}))

(defn warm-from-fn!
  "Warm the cache by calling a function for each key."
  [keys compute-fn & {:keys [ttl-ms parallel?]}]
  (log/info "Warming cache from function" {:count (count keys)})
  (let [compute (fn [key]
                  (try
                    (let [value (compute-fn key)]
                      (cache-put! key value :ttl-ms ttl-ms))
                    (catch Exception e
                      (log/warn "Failed to warm cache entry" {:key key :error (.getMessage e)}))))]
    (if parallel?
      (doall (pmap compute keys))
      (doseq [key keys]
        (compute key)))))

;; =============================================================================
;; CACHE-ASIDE PATTERN
;; =============================================================================

(defn cache-aside
  "Cache-aside pattern: get from cache or compute and cache."
  [key compute-fn & {:keys [ttl-ms]}]
  (if-let [cached (cache-get key)]
    cached
    (let [value (compute-fn)]
      (cache-put! key value :ttl-ms ttl-ms)
      value)))

(defmacro with-cache
  "Macro for cache-aside pattern."
  [key opts & body]
  `(cache-aside ~key (fn [] ~@body) ~@opts))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-cache-stats
  "Get cache statistics."
  []
  (let [^ConcurrentHashMap entries (:entries @cache-state)
        stats (:stats @cache-state)
        total-requests (+ (:hits stats) (:misses stats))]
    {:size (.size entries)
     :max-size (get-in @cache-state [:config :max-size])
     :hits (:hits stats)
     :misses (:misses stats)
     :hit-rate (if (pos? total-requests)
                 (/ (double (:hits stats)) total-requests)
                 0.0)
     :evictions (:evictions stats)
     :invalidations (:invalidations stats)}))

(defn reset-stats!
  "Reset cache statistics."
  []
  (swap! cache-state assoc :stats {:hits 0 :misses 0 :evictions 0 :invalidations 0}))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defn configure!
  "Configure the cache."
  [{:keys [max-size default-ttl-ms eviction-batch-size]}]
  (when max-size
    (swap! cache-state assoc-in [:config :max-size] max-size))
  (when default-ttl-ms
    (swap! cache-state assoc-in [:config :default-ttl-ms] default-ttl-ms))
  (when eviction-batch-size
    (swap! cache-state assoc-in [:config :eviction-batch-size] eviction-batch-size))
  (log/info "Cache configured" (:config @cache-state)))

;; =============================================================================
;; BACKGROUND CLEANUP
;; =============================================================================

(defonce cleanup-scheduler (atom nil))

(defn start-cleanup-scheduler!
  "Start background cleanup scheduler."
  [& {:keys [interval-ms] :or {interval-ms 60000}}]
  (log/info "Starting cache cleanup scheduler" {:interval-ms interval-ms})
  (reset! cleanup-scheduler
          (future
            (while true
              (Thread/sleep interval-ms)
              (try
                (evict-expired!)
                (catch Exception e
                  (log/error "Cache cleanup error" {:error (.getMessage e)})))))))

(defn stop-cleanup-scheduler!
  "Stop background cleanup scheduler."
  []
  (when-let [scheduler @cleanup-scheduler]
    (future-cancel scheduler)
    (reset! cleanup-scheduler nil)
    (log/info "Cache cleanup scheduler stopped")))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-response-cache!
  "Initialize response cache."
  []
  (log/info "Initializing response cache")
  ;; Register feature flag
  (flags/register-flag! "response-cache" "Enable response caching" true)
  ;; Create metrics
  (metrics/create-counter! :cache/hits "Cache hits")
  (metrics/create-counter! :cache/misses "Cache misses")
  (metrics/create-counter! :cache/puts "Cache puts")
  (metrics/create-counter! :cache/invalidations "Cache invalidations")
  (metrics/create-histogram! :cache/evictions "Cache evictions" [1 10 50 100])
  (metrics/create-gauge! :cache/size "Cache size"
                         #(.size ^ConcurrentHashMap (:entries @cache-state)))
  (metrics/create-gauge! :cache/hit-rate "Cache hit rate"
                         #(:hit-rate (get-cache-stats)))
  ;; Start cleanup scheduler
  (start-cleanup-scheduler!)
  (log/info "Response cache initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-cache-status []
  (merge
   {:enabled (flags/is-enabled? "response-cache")
    :config (:config @cache-state)
    :cleanup-running (some? @cleanup-scheduler)}
   (get-cache-stats)))
