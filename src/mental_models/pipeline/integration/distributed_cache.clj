(ns mental-models.pipeline.integration.distributed-cache
  "Distributed caching layer for mental model analysis.
   
   Features:
   - Multi-tier caching (L1 local, L2 distributed)
   - Cache invalidation strategies
   - TTL and LRU eviction
   - Cache warming
   - Consistent hashing for distribution
   - Cache statistics and monitoring
   - Write-through and write-behind modes
   - Cache partitioning"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.util.concurrent ConcurrentHashMap]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:l1-cache {}         ;; Local in-memory cache
         :l2-cache {}         ;; Simulated distributed cache
         :partitions {}       ;; partition-id -> partition-config
         :stats {:hits 0 :misses 0 :evictions 0 :writes 0}
         :config {:l1-max-size 1000
                  :l2-max-size 10000
                  :default-ttl-ms 3600000
                  :eviction-policy :lru}
         :warming-queue []
         :initialized? false}))

;; ============================================================================
;; Cache Key Management
;; ============================================================================

(defn- normalize-key
  "Normalize a cache key."
  [key]
  (if (keyword? key)
    (name key)
    (str key)))

(defn- compute-partition
  "Compute partition for a key using consistent hashing."
  [key num-partitions]
  (mod (Math/abs (hash key)) num-partitions))

(defn- make-cache-entry
  "Create a cache entry with metadata."
  [value & {:keys [ttl-ms tags]}]
  {:value value
   :created-at (System/currentTimeMillis)
   :accessed-at (System/currentTimeMillis)
   :access-count 1
   :ttl-ms (or ttl-ms (get-in @state [:config :default-ttl-ms]))
   :tags (or tags #{})
   :size (count (pr-str value))})

(defn- is-expired?
  "Check if a cache entry is expired."
  [entry]
  (let [ttl (:ttl-ms entry)
        created (:created-at entry)
        now (System/currentTimeMillis)]
    (and ttl (> (- now created) ttl))))

;; ============================================================================
;; L1 Cache (Local)
;; ============================================================================

(defn- l1-get
  "Get from L1 cache."
  [key]
  (let [normalized (normalize-key key)
        entry (get-in @state [:l1-cache normalized])]
    (when (and entry (not (is-expired? entry)))
      (swap! state update-in [:l1-cache normalized :accessed-at] (constantly (System/currentTimeMillis)))
      (swap! state update-in [:l1-cache normalized :access-count] inc)
      (:value entry))))

(defn- l1-put!
  "Put into L1 cache."
  [key value & {:keys [ttl-ms tags]}]
  (let [normalized (normalize-key key)
        entry (make-cache-entry value :ttl-ms ttl-ms :tags tags)
        max-size (get-in @state [:config :l1-max-size])]
    ;; Evict if necessary
    (when (>= (count (:l1-cache @state)) max-size)
      (l1-evict-lru!))
    (swap! state assoc-in [:l1-cache normalized] entry)))

(defn- l1-delete!
  "Delete from L1 cache."
  [key]
  (let [normalized (normalize-key key)]
    (swap! state update :l1-cache dissoc normalized)))

(defn- l1-evict-lru!
  "Evict least recently used entry from L1."
  []
  (let [cache (:l1-cache @state)
        lru-key (first (sort-by #(:accessed-at (val %)) cache))]
    (when lru-key
      (swap! state update :l1-cache dissoc (key lru-key))
      (swap! state update-in [:stats :evictions] inc))))

;; ============================================================================
;; L2 Cache (Distributed)
;; ============================================================================

(defn- l2-get
  "Get from L2 cache."
  [key]
  (let [normalized (normalize-key key)
        entry (get-in @state [:l2-cache normalized])]
    (when (and entry (not (is-expired? entry)))
      (swap! state update-in [:l2-cache normalized :accessed-at] (constantly (System/currentTimeMillis)))
      (swap! state update-in [:l2-cache normalized :access-count] inc)
      (:value entry))))

(defn- l2-put!
  "Put into L2 cache."
  [key value & {:keys [ttl-ms tags]}]
  (let [normalized (normalize-key key)
        entry (make-cache-entry value :ttl-ms ttl-ms :tags tags)
        max-size (get-in @state [:config :l2-max-size])]
    ;; Evict if necessary
    (when (>= (count (:l2-cache @state)) max-size)
      (l2-evict-lru!))
    (swap! state assoc-in [:l2-cache normalized] entry)))

(defn- l2-delete!
  "Delete from L2 cache."
  [key]
  (let [normalized (normalize-key key)]
    (swap! state update :l2-cache dissoc normalized)))

(defn- l2-evict-lru!
  "Evict least recently used entry from L2."
  []
  (let [cache (:l2-cache @state)
        lru-key (first (sort-by #(:accessed-at (val %)) cache))]
    (when lru-key
      (swap! state update :l2-cache dissoc (key lru-key))
      (swap! state update-in [:stats :evictions] inc))))

;; ============================================================================
;; Multi-Tier Cache Operations
;; ============================================================================

(defn cache-get
  "Get value from cache (checks L1 then L2)."
  [key]
  (when (flags/enabled? :distributed-cache)
    (let [normalized (normalize-key key)]
      ;; Try L1 first
      (if-let [value (l1-get normalized)]
        (do
          (swap! state update-in [:stats :hits] inc)
          (metrics/increment :cache-hits {:tier :l1})
          value)
        ;; Try L2
        (if-let [value (l2-get normalized)]
          (do
            ;; Promote to L1
            (l1-put! normalized value)
            (swap! state update-in [:stats :hits] inc)
            (metrics/increment :cache-hits {:tier :l2})
            value)
          (do
            (swap! state update-in [:stats :misses] inc)
            (metrics/increment :cache-misses {})
            nil))))))

(defn cache-put!
  "Put value into cache."
  [key value & {:keys [ttl-ms tags write-through?]
                 :or {write-through? true}}]
  (when (flags/enabled? :distributed-cache)
    (let [normalized (normalize-key key)]
      ;; Write to L1
      (l1-put! normalized value :ttl-ms ttl-ms :tags tags)
      ;; Write to L2 if write-through
      (when write-through?
        (l2-put! normalized value :ttl-ms ttl-ms :tags tags))
      (swap! state update-in [:stats :writes] inc)
      (metrics/increment :cache-writes {})
      true)))

(defn cache-delete!
  "Delete value from cache."
  [key]
  (when (flags/enabled? :distributed-cache)
    (let [normalized (normalize-key key)]
      (l1-delete! normalized)
      (l2-delete! normalized)
      true)))

(defn cache-exists?
  "Check if key exists in cache."
  [key]
  (some? (cache-get key)))

;; ============================================================================
;; Batch Operations
;; ============================================================================

(defn cache-get-multi
  "Get multiple values from cache."
  [keys]
  (into {}
        (map (fn [k] [k (cache-get k)])
             keys)))

(defn cache-put-multi!
  "Put multiple values into cache."
  [entries & {:keys [ttl-ms]}]
  (doseq [[k v] entries]
    (cache-put! k v :ttl-ms ttl-ms)))

(defn cache-delete-multi!
  "Delete multiple values from cache."
  [keys]
  (doseq [k keys]
    (cache-delete! k)))

;; ============================================================================
;; Tag-Based Operations
;; ============================================================================

(defn cache-get-by-tag
  "Get all cache entries with a specific tag."
  [tag]
  (let [l1-entries (filter #(contains? (:tags (val %)) tag) (:l1-cache @state))
        l2-entries (filter #(contains? (:tags (val %)) tag) (:l2-cache @state))]
    (merge (into {} (map (fn [[k v]] [k (:value v)]) l2-entries))
           (into {} (map (fn [[k v]] [k (:value v)]) l1-entries)))))

(defn cache-invalidate-by-tag!
  "Invalidate all cache entries with a specific tag."
  [tag]
  (let [l1-keys (map key (filter #(contains? (:tags (val %)) tag) (:l1-cache @state)))
        l2-keys (map key (filter #(contains? (:tags (val %)) tag) (:l2-cache @state)))]
    (doseq [k l1-keys] (l1-delete! k))
    (doseq [k l2-keys] (l2-delete! k))
    (logging/log :info "Invalidated cache by tag" {:tag tag :count (+ (count l1-keys) (count l2-keys))})
    (+ (count l1-keys) (count l2-keys))))

;; ============================================================================
;; Cache Warming
;; ============================================================================

(defn add-to-warming-queue!
  "Add items to the cache warming queue."
  [items]
  (swap! state update :warming-queue concat items))

(defn warm-cache!
  "Warm the cache with queued items."
  [loader-fn]
  (let [queue (:warming-queue @state)]
    (when (seq queue)
      (logging/log :info "Warming cache" {:items (count queue)})
      (doseq [item queue]
        (when-let [value (loader-fn item)]
          (cache-put! item value)))
      (swap! state assoc :warming-queue [])
      (count queue))))

(defn preload-cache!
  "Preload cache with specific data."
  [data]
  (doseq [[k v] data]
    (cache-put! k v))
  (logging/log :info "Preloaded cache" {:items (count data)}))

;; ============================================================================
;; Cache Invalidation
;; ============================================================================

(defn invalidate-pattern!
  "Invalidate cache entries matching a pattern."
  [pattern]
  (let [regex (re-pattern pattern)
        l1-keys (filter #(re-matches regex %) (keys (:l1-cache @state)))
        l2-keys (filter #(re-matches regex %) (keys (:l2-cache @state)))]
    (doseq [k l1-keys] (l1-delete! k))
    (doseq [k l2-keys] (l2-delete! k))
    (logging/log :info "Invalidated cache by pattern" {:pattern pattern :count (+ (count l1-keys) (count l2-keys))})
    (+ (count l1-keys) (count l2-keys))))

(defn invalidate-expired!
  "Remove all expired entries from cache."
  []
  (let [l1-expired (filter #(is-expired? (val %)) (:l1-cache @state))
        l2-expired (filter #(is-expired? (val %)) (:l2-cache @state))]
    (doseq [[k _] l1-expired] (l1-delete! k))
    (doseq [[k _] l2-expired] (l2-delete! k))
    (+ (count l1-expired) (count l2-expired))))

(defn clear-cache!
  "Clear all cache entries."
  []
  (swap! state assoc :l1-cache {} :l2-cache {})
  (logging/log :info "Cleared all cache"))

;; ============================================================================
;; Cache-Aside Pattern
;; ============================================================================

(defn cache-aside
  "Implement cache-aside pattern."
  [key loader-fn & {:keys [ttl-ms]}]
  (if-let [cached (cache-get key)]
    cached
    (when-let [value (loader-fn)]
      (cache-put! key value :ttl-ms ttl-ms)
      value)))

;; ============================================================================
;; Write-Behind Queue
;; ============================================================================

(defonce ^:private write-behind-queue (atom []))

(defn queue-write-behind!
  "Queue a write for later persistence."
  [key value]
  (swap! write-behind-queue conj {:key key :value value :queued-at (System/currentTimeMillis)}))

(defn flush-write-behind!
  "Flush write-behind queue to persistent storage."
  [writer-fn]
  (let [queue @write-behind-queue]
    (when (seq queue)
      (reset! write-behind-queue [])
      (doseq [{:keys [key value]} queue]
        (writer-fn key value))
      (logging/log :info "Flushed write-behind queue" {:count (count queue)})
      (count queue))))

;; ============================================================================
;; Partitioning
;; ============================================================================

(defn create-partition!
  "Create a cache partition."
  [partition-id config]
  (let [partition {:id partition-id
                   :name (get config :name (name partition-id))
                   :max-size (get config :max-size 1000)
                   :ttl-ms (get config :ttl-ms 3600000)
                   :eviction-policy (get config :eviction-policy :lru)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:partitions partition-id] partition)
    partition-id))

(defn get-partition-stats
  "Get statistics for a partition."
  [partition-id]
  (let [prefix (str (name partition-id) ":")
        l1-entries (filter #(str/starts-with? (key %) prefix) (:l1-cache @state))
        l2-entries (filter #(str/starts-with? (key %) prefix) (:l2-cache @state))]
    {:partition-id partition-id
     :l1-entries (count l1-entries)
     :l2-entries (count l2-entries)
     :total-size (reduce + (map #(:size (val %)) (concat l1-entries l2-entries)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-cache-stats
  "Get cache statistics."
  []
  (let [stats (:stats @state)
        total-requests (+ (:hits stats) (:misses stats))]
    {:l1-size (count (:l1-cache @state))
     :l2-size (count (:l2-cache @state))
     :total-hits (:hits stats)
     :total-misses (:misses stats)
     :total-evictions (:evictions stats)
     :total-writes (:writes stats)
     :hit-rate (if (pos? total-requests)
                 (/ (:hits stats) (double total-requests))
                 0.0)
     :l1-memory-estimate (reduce + (map #(:size (val %)) (:l1-cache @state)))
     :l2-memory-estimate (reduce + (map #(:size (val %)) (:l2-cache @state)))
     :partitions (count (:partitions @state))
     :warming-queue-size (count (:warming-queue @state))
     :write-behind-queue-size (count @write-behind-queue)}))

(defn reset-stats!
  "Reset cache statistics."
  []
  (swap! state assoc :stats {:hits 0 :misses 0 :evictions 0 :writes 0}))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn configure-cache!
  "Configure cache settings."
  [config]
  (swap! state update :config merge config)
  (logging/log :info "Updated cache configuration" config))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-distributed-cache!
  "Initialize the distributed cache."
  []
  (when-not (:initialized? @state)
    ;; Create default partitions
    (create-partition! :models
                       {:name "Mental Models"
                        :max-size 500
                        :ttl-ms 7200000})
    
    (create-partition! :analysis
                       {:name "Analysis Results"
                        :max-size 1000
                        :ttl-ms 3600000})
    
    (create-partition! :embeddings
                       {:name "Text Embeddings"
                        :max-size 2000
                        :ttl-ms 86400000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Distributed cache initialized")
    (events/emit! :distributed-cache-initialized {})
    true))
