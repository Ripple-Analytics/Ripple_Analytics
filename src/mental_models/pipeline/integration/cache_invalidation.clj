(ns mental-models.pipeline.integration.cache-invalidation
  "Cache invalidation manager for mental model analysis system.
   
   Features:
   - Tag-based invalidation
   - Pattern-based invalidation
   - Time-based expiration
   - Event-driven invalidation
   - Cascade invalidation
   - Invalidation queues
   - Consistency guarantees
   - Invalidation metrics"
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
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:cache {}            ;; key -> cached-value
         :tags {}             ;; tag -> #{keys}
         :key-tags {}         ;; key -> #{tags}
         :ttls {}             ;; key -> expiration-time
         :dependencies {}     ;; key -> #{dependent-keys}
         :invalidation-queue (async/chan 1000)
         :rules {}            ;; rule-id -> invalidation-rule
         :config {:default-ttl-ms 300000
                  :max-cache-size 10000
                  :cleanup-interval-ms 60000}
         :stats {:hits 0 :misses 0 :invalidations 0 :evictions 0}
         :initialized? false}))

;; ============================================================================
;; Cache Operations
;; ============================================================================

(defn cache-put!
  "Put a value in the cache."
  [key value & {:keys [tags ttl-ms dependencies]}]
  (let [ttl (or ttl-ms (get-in @state [:config :default-ttl-ms]))
        expiration (+ (System/currentTimeMillis) ttl)]
    
    ;; Store value
    (swap! state assoc-in [:cache key] {:value value
                                         :created-at (System/currentTimeMillis)
                                         :accessed-at (System/currentTimeMillis)})
    
    ;; Store TTL
    (swap! state assoc-in [:ttls key] expiration)
    
    ;; Store tags
    (when (seq tags)
      (swap! state assoc-in [:key-tags key] (set tags))
      (doseq [tag tags]
        (swap! state update-in [:tags tag] (fnil conj #{}) key)))
    
    ;; Store dependencies
    (when (seq dependencies)
      (doseq [dep dependencies]
        (swap! state update-in [:dependencies dep] (fnil conj #{}) key)))
    
    (logging/log :debug "Cache put" {:key key :tags tags})
    key))

(defn cache-get
  "Get a value from the cache."
  [key]
  (let [entry (get-in @state [:cache key])
        expiration (get-in @state [:ttls key])
        now (System/currentTimeMillis)]
    (cond
      ;; Not found
      (nil? entry)
      (do
        (swap! state update-in [:stats :misses] inc)
        nil)
      
      ;; Expired
      (and expiration (> now expiration))
      (do
        (invalidate-key! key)
        (swap! state update-in [:stats :misses] inc)
        nil)
      
      ;; Found and valid
      :else
      (do
        (swap! state assoc-in [:cache key :accessed-at] now)
        (swap! state update-in [:stats :hits] inc)
        (:value entry)))))

(defn cache-exists?
  "Check if a key exists in cache."
  [key]
  (let [entry (get-in @state [:cache key])
        expiration (get-in @state [:ttls key])
        now (System/currentTimeMillis)]
    (and (some? entry)
         (or (nil? expiration) (<= now expiration)))))

;; ============================================================================
;; Key Invalidation
;; ============================================================================

(defn invalidate-key!
  "Invalidate a single cache key."
  [key]
  (when (get-in @state [:cache key])
    ;; Remove from cache
    (swap! state update :cache dissoc key)
    (swap! state update :ttls dissoc key)
    
    ;; Remove from tags
    (when-let [tags (get-in @state [:key-tags key])]
      (doseq [tag tags]
        (swap! state update-in [:tags tag] disj key))
      (swap! state update :key-tags dissoc key))
    
    ;; Cascade to dependents
    (when-let [dependents (get-in @state [:dependencies key])]
      (doseq [dep dependents]
        (invalidate-key! dep))
      (swap! state update :dependencies dissoc key))
    
    (swap! state update-in [:stats :invalidations] inc)
    (logging/log :debug "Cache key invalidated" {:key key})
    (events/emit! :cache-invalidated {:key key})
    true))

(defn invalidate-keys!
  "Invalidate multiple cache keys."
  [keys]
  (doseq [key keys]
    (invalidate-key! key))
  (count keys))

;; ============================================================================
;; Tag-Based Invalidation
;; ============================================================================

(defn invalidate-by-tag!
  "Invalidate all keys with a specific tag."
  [tag]
  (when-let [keys (get-in @state [:tags tag])]
    (let [key-list (vec keys)]
      (doseq [key key-list]
        (invalidate-key! key))
      (swap! state update :tags dissoc tag)
      (logging/log :info "Invalidated by tag" {:tag tag :count (count key-list)})
      (count key-list))))

(defn invalidate-by-tags!
  "Invalidate all keys with any of the specified tags."
  [tags]
  (reduce + (map invalidate-by-tag! tags)))

(defn get-keys-by-tag
  "Get all keys with a specific tag."
  [tag]
  (vec (get-in @state [:tags tag] #{})))

;; ============================================================================
;; Pattern-Based Invalidation
;; ============================================================================

(defn invalidate-by-pattern!
  "Invalidate all keys matching a pattern."
  [pattern]
  (let [regex (if (instance? Pattern pattern)
                pattern
                (re-pattern pattern))
        all-keys (keys (:cache @state))
        matching-keys (filter #(re-matches regex (str %)) all-keys)]
    (doseq [key matching-keys]
      (invalidate-key! key))
    (logging/log :info "Invalidated by pattern" {:pattern (str pattern) :count (count matching-keys)})
    (count matching-keys)))

(defn invalidate-by-prefix!
  "Invalidate all keys with a specific prefix."
  [prefix]
  (invalidate-by-pattern! (re-pattern (str "^" (Pattern/quote prefix) ".*"))))

(defn invalidate-by-suffix!
  "Invalidate all keys with a specific suffix."
  [suffix]
  (invalidate-by-pattern! (re-pattern (str ".*" (Pattern/quote suffix) "$"))))

;; ============================================================================
;; Time-Based Expiration
;; ============================================================================

(defn cleanup-expired!
  "Clean up all expired entries."
  []
  (let [now (System/currentTimeMillis)
        expired-keys (filter (fn [[key expiration]]
                               (and expiration (> now expiration)))
                             (:ttls @state))]
    (doseq [[key _] expired-keys]
      (invalidate-key! key))
    (count expired-keys)))

(defn extend-ttl!
  "Extend TTL for a key."
  [key additional-ms]
  (when-let [current-ttl (get-in @state [:ttls key])]
    (swap! state assoc-in [:ttls key] (+ current-ttl additional-ms))))

(defn set-ttl!
  "Set absolute TTL for a key."
  [key expiration-time]
  (swap! state assoc-in [:ttls key] expiration-time))

(defn get-ttl
  "Get remaining TTL for a key."
  [key]
  (when-let [expiration (get-in @state [:ttls key])]
    (max 0 (- expiration (System/currentTimeMillis)))))

;; ============================================================================
;; Event-Driven Invalidation
;; ============================================================================

(defn create-invalidation-rule!
  "Create an invalidation rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :event-type (get config :event-type)
              :condition-fn (get config :condition-fn (constantly true))
              :invalidation-fn (get config :invalidation-fn)
              :tags (get config :tags)
              :pattern (get config :pattern)
              :enabled? (get config :enabled? true)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Created invalidation rule" {:rule-id rule-id})
    rule-id))

(defn get-invalidation-rule
  "Get an invalidation rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-invalidation-rules
  "List all invalidation rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :event-type (:event-type r)
           :enabled? (:enabled? r)})
        (:rules @state)))

(defn process-event!
  "Process an event and apply matching invalidation rules."
  [event-type event-data]
  (let [rules (filter (fn [[_ r]]
                        (and (:enabled? r)
                             (= (:event-type r) event-type)
                             ((:condition-fn r) event-data)))
                      (:rules @state))
        invalidated (atom 0)]
    (doseq [[_ rule] rules]
      (cond
        ;; Custom invalidation function
        (:invalidation-fn rule)
        (swap! invalidated + (or ((:invalidation-fn rule) event-data) 0))
        
        ;; Tag-based
        (:tags rule)
        (swap! invalidated + (invalidate-by-tags! (:tags rule)))
        
        ;; Pattern-based
        (:pattern rule)
        (swap! invalidated + (invalidate-by-pattern! (:pattern rule)))))
    
    (when (pos? @invalidated)
      (logging/log :info "Event-driven invalidation" {:event-type event-type :invalidated @invalidated}))
    @invalidated))

;; ============================================================================
;; Cascade Invalidation
;; ============================================================================

(defn add-dependency!
  "Add a dependency between cache keys."
  [parent-key dependent-key]
  (swap! state update-in [:dependencies parent-key] (fnil conj #{}) dependent-key))

(defn remove-dependency!
  "Remove a dependency between cache keys."
  [parent-key dependent-key]
  (swap! state update-in [:dependencies parent-key] disj dependent-key))

(defn get-dependents
  "Get all keys that depend on a key."
  [key]
  (vec (get-in @state [:dependencies key] #{})))

(defn get-cascade-keys
  "Get all keys that would be invalidated in a cascade."
  [key]
  (loop [to-process [key]
         processed #{}
         result []]
    (if (empty? to-process)
      result
      (let [current (first to-process)
            dependents (get-dependents current)]
        (recur (concat (rest to-process) (remove processed dependents))
               (conj processed current)
               (conj result current))))))

;; ============================================================================
;; Invalidation Queue
;; ============================================================================

(defn queue-invalidation!
  "Queue an invalidation for async processing."
  [invalidation-type target]
  (async/put! (:invalidation-queue @state)
              {:type invalidation-type
               :target target
               :queued-at (System/currentTimeMillis)}))

(defn start-invalidation-processor!
  "Start the async invalidation processor."
  []
  (go-loop []
    (when-let [item (<! (:invalidation-queue @state))]
      (try
        (case (:type item)
          :key (invalidate-key! (:target item))
          :tag (invalidate-by-tag! (:target item))
          :pattern (invalidate-by-pattern! (:target item))
          nil)
        (catch Exception e
          (logging/log :error "Invalidation error" {:error (.getMessage e)})))
      (recur))))

;; ============================================================================
;; Cache Eviction
;; ============================================================================

(defn evict-lru!
  "Evict least recently used entries."
  [count-to-evict]
  (let [entries (sort-by (fn [[k v]] (get-in v [:accessed-at] 0))
                         (:cache @state))
        to-evict (take count-to-evict (map first entries))]
    (doseq [key to-evict]
      (invalidate-key! key)
      (swap! state update-in [:stats :evictions] inc))
    (count to-evict)))

(defn enforce-max-size!
  "Enforce maximum cache size."
  []
  (let [max-size (get-in @state [:config :max-cache-size])
        current-size (count (:cache @state))]
    (when (> current-size max-size)
      (evict-lru! (- current-size max-size)))))

;; ============================================================================
;; Bulk Operations
;; ============================================================================

(defn invalidate-all!
  "Invalidate all cache entries."
  []
  (let [count (count (:cache @state))]
    (swap! state assoc :cache {})
    (swap! state assoc :tags {})
    (swap! state assoc :key-tags {})
    (swap! state assoc :ttls {})
    (swap! state assoc :dependencies {})
    (swap! state update-in [:stats :invalidations] + count)
    (logging/log :warn "All cache invalidated" {:count count})
    (events/emit! :cache-cleared {})
    count))

(defn warm-cache!
  "Warm cache with provided data."
  [entries]
  (doseq [{:keys [key value tags ttl-ms]} entries]
    (cache-put! key value :tags tags :ttl-ms ttl-ms))
  (count entries))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-cache-stats
  "Get cache statistics."
  []
  (let [stats (:stats @state)
        cache-size (count (:cache @state))
        total-requests (+ (:hits stats) (:misses stats))]
    {:cache-size cache-size
     :tag-count (count (:tags @state))
     :rule-count (count (:rules @state))
     :hits (:hits stats)
     :misses (:misses stats)
     :hit-rate (if (pos? total-requests)
                 (/ (:hits stats) total-requests)
                 0)
     :invalidations (:invalidations stats)
     :evictions (:evictions stats)}))

(defn get-cache-keys
  "Get all cache keys."
  []
  (keys (:cache @state)))

(defn get-cache-info
  "Get detailed cache info for a key."
  [key]
  (when-let [entry (get-in @state [:cache key])]
    {:key key
     :created-at (:created-at entry)
     :accessed-at (:accessed-at entry)
     :ttl-remaining (get-ttl key)
     :tags (get-in @state [:key-tags key])
     :dependents (get-dependents key)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-cache-invalidation!
  "Initialize the cache invalidation system."
  []
  (when-not (:initialized? @state)
    ;; Create invalidation rules
    (create-invalidation-rule! :model-update
                               {:name "Model Update Invalidation"
                                :event-type :model-updated
                                :tags [:analysis-results :model-cache]})
    
    (create-invalidation-rule! :analysis-complete
                               {:name "Analysis Complete Invalidation"
                                :event-type :analysis-completed
                                :invalidation-fn (fn [data]
                                                   (invalidate-by-prefix! (str "analysis:" (:document-id data))))})
    
    (create-invalidation-rule! :config-change
                               {:name "Config Change Invalidation"
                                :event-type :config-changed
                                :pattern "config:.*"})
    
    ;; Start cleanup task
    (go-loop []
      (<! (timeout (get-in @state [:config :cleanup-interval-ms])))
      (cleanup-expired!)
      (enforce-max-size!)
      (recur))
    
    ;; Start invalidation processor
    (start-invalidation-processor!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Cache invalidation system initialized")
    (events/emit! :cache-invalidation-initialized {})
    true))
