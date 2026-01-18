(ns mental-models.cache.core
  "Caching Layer for Mental Models Pipeline
   
   Provides caching with:
   - In-memory LRU cache
   - TTL-based expiration
   - Cache statistics
   - Namespace isolation
   - Cache warming
   - Write-through and write-behind strategies"
  (:require
   [clojure.string :as str])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.time Instant Duration]))

;; =============================================================================
;; CACHE ENTRY
;; =============================================================================

(defrecord CacheEntry [value created-at expires-at access-count last-accessed])

(defn create-entry [value ttl-seconds]
  (let [now (Instant/now)]
    (->CacheEntry value now
                  (when ttl-seconds (.plusSeconds now ttl-seconds))
                  (atom 1)
                  (atom now))))

(defn expired? [{:keys [expires-at]}]
  (and expires-at (.isAfter (Instant/now) expires-at)))

(defn touch-entry [{:keys [access-count last-accessed] :as entry}]
  (swap! access-count inc)
  (reset! last-accessed (Instant/now))
  entry)

;; =============================================================================
;; LRU CACHE
;; =============================================================================

(def ^:private caches (ConcurrentHashMap.))
(def ^:private cache-stats (atom {}))

(defn get-cache [namespace]
  (or (.get caches namespace)
      (let [cache (ConcurrentHashMap.)]
        (.putIfAbsent caches namespace cache)
        (.get caches namespace))))

;; =============================================================================
;; CACHE OPERATIONS
;; =============================================================================

(defn cache-get
  "Get a value from the cache. Returns nil if not found or expired."
  ([namespace key] (cache-get namespace key nil))
  ([namespace key default]
   (let [cache (get-cache namespace)]
     (if-let [entry (.get cache key)]
       (if (expired? entry)
         (do (.remove cache key)
             (swap! cache-stats update-in [namespace :misses] (fnil inc 0))
             default)
         (do (touch-entry entry)
             (swap! cache-stats update-in [namespace :hits] (fnil inc 0))
             (:value entry)))
       (do (swap! cache-stats update-in [namespace :misses] (fnil inc 0))
           default)))))

(defn cache-put
  "Put a value in the cache with optional TTL in seconds."
  ([namespace key value] (cache-put namespace key value nil))
  ([namespace key value ttl-seconds]
   (let [cache (get-cache namespace)
         entry (create-entry value ttl-seconds)]
     (.put cache key entry)
     (swap! cache-stats update-in [namespace :writes] (fnil inc 0))
     value)))

(defn cache-remove
  "Remove a value from the cache."
  [namespace key]
  (let [cache (get-cache namespace)]
    (.remove cache key)
    (swap! cache-stats update-in [namespace :evictions] (fnil inc 0))))

(defn cache-clear
  "Clear all entries in a namespace."
  [namespace]
  (let [cache (get-cache namespace)
        size (.size cache)]
    (.clear cache)
    (swap! cache-stats update-in [namespace :clears] (fnil inc 0))
    size))

(defn cache-contains?
  "Check if a key exists in the cache and is not expired."
  [namespace key]
  (let [cache (get-cache namespace)]
    (when-let [entry (.get cache key)]
      (if (expired? entry)
        (do (.remove cache key) false)
        true))))

;; =============================================================================
;; CACHE STATISTICS
;; =============================================================================

(defn get-stats
  "Get cache statistics for a namespace or all namespaces."
  ([] @cache-stats)
  ([namespace]
   (let [cache (get-cache namespace)
         stats (get @cache-stats namespace {})]
     (merge stats
            {:size (.size cache)
             :hit-rate (let [hits (get stats :hits 0)
                             misses (get stats :misses 0)
                             total (+ hits misses)]
                         (if (zero? total) 0.0 (/ hits total)))}))))

(defn reset-stats
  "Reset statistics for a namespace or all namespaces."
  ([] (reset! cache-stats {}))
  ([namespace] (swap! cache-stats dissoc namespace)))

;; =============================================================================
;; CACHE WARMING
;; =============================================================================

(defn warm-cache
  "Warm the cache with a map of key-value pairs."
  ([namespace entries] (warm-cache namespace entries nil))
  ([namespace entries ttl-seconds]
   (doseq [[k v] entries]
     (cache-put namespace k v ttl-seconds))
   (count entries)))

;; =============================================================================
;; MEMOIZATION
;; =============================================================================

(defn memoize-with-cache
  "Memoize a function using the cache."
  ([namespace f] (memoize-with-cache namespace f nil))
  ([namespace f ttl-seconds]
   (fn [& args]
     (let [key (hash args)]
       (if-let [cached (cache-get namespace key)]
         cached
         (let [result (apply f args)]
           (cache-put namespace key result ttl-seconds)
           result))))))

;; =============================================================================
;; EVICTION POLICIES
;; =============================================================================

(defn evict-expired
  "Remove all expired entries from a namespace."
  [namespace]
  (let [cache (get-cache namespace)
        expired-keys (filter #(expired? (.get cache %)) (keys cache))]
    (doseq [k expired-keys]
      (.remove cache k))
    (count expired-keys)))

(defn evict-lru
  "Evict least recently used entries to bring cache size below max-size."
  [namespace max-size]
  (let [cache (get-cache namespace)]
    (when (> (.size cache) max-size)
      (let [entries (sort-by #(deref (:last-accessed (val %)))
                             (into {} cache))
            to-evict (- (.size cache) max-size)]
        (doseq [[k _] (take to-evict entries)]
          (.remove cache k))
        to-evict))))

(defn evict-lfu
  "Evict least frequently used entries to bring cache size below max-size."
  [namespace max-size]
  (let [cache (get-cache namespace)]
    (when (> (.size cache) max-size)
      (let [entries (sort-by #(deref (:access-count (val %)))
                             (into {} cache))
            to-evict (- (.size cache) max-size)]
        (doseq [[k _] (take to-evict entries)]
          (.remove cache k))
        to-evict))))

;; =============================================================================
;; WRITE STRATEGIES
;; =============================================================================

(defn write-through
  "Write-through cache: writes to both cache and backing store."
  [namespace key value write-fn ttl-seconds]
  (write-fn key value)
  (cache-put namespace key value ttl-seconds))

(defn write-behind
  "Write-behind cache: writes to cache immediately, backing store async."
  [namespace key value write-fn ttl-seconds]
  (cache-put namespace key value ttl-seconds)
  (future (write-fn key value)))

;; =============================================================================
;; BULK OPERATIONS
;; =============================================================================

(defn cache-get-many
  "Get multiple values from the cache."
  [namespace keys]
  (into {} (for [k keys
                 :let [v (cache-get namespace k)]
                 :when v]
             [k v])))

(defn cache-put-many
  "Put multiple values in the cache."
  ([namespace entries] (cache-put-many namespace entries nil))
  ([namespace entries ttl-seconds]
   (doseq [[k v] entries]
     (cache-put namespace k v ttl-seconds))
   (count entries)))

;; =============================================================================
;; CACHE NAMESPACES
;; =============================================================================

(def namespaces
  {:models "mental-models"
   :analysis "analysis-results"
   :documents "document-cache"
   :lm-studio "lm-studio-responses"
   :metrics "metrics-cache"})

(defn get-all-namespaces []
  (keys caches))

(defn get-total-size []
  (reduce + (map #(.size (get-cache %)) (get-all-namespaces))))
