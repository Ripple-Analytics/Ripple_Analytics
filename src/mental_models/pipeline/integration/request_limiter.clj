(ns mental-models.pipeline.integration.request-limiter
  "Request limiter for mental model analysis system.
   
   Features:
   - Request rate limiting
   - Concurrent request limiting
   - Bandwidth limiting
   - Per-client limiting
   - Per-endpoint limiting
   - Limit quotas
   - Limit headers
   - Limiting metrics"
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
           [java.util.concurrent.atomic AtomicLong AtomicInteger]
           [java.util.concurrent Semaphore]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:limiters {}         ;; limiter-id -> limiter
         :quotas {}           ;; client-id -> quota-state
         :config {:default-rate-limit 100        ;; requests per window
                  :default-window-ms 60000       ;; 1 minute
                  :default-concurrent-limit 10
                  :default-bandwidth-limit-bytes 10485760  ;; 10MB per window
                  :include-headers? true}
         :stats {:requests-checked 0
                 :requests-allowed 0
                 :requests-denied 0
                 :rate-limited 0
                 :concurrent-limited 0
                 :bandwidth-limited 0}
         :initialized? false}))

;; ============================================================================
;; Limiter Creation
;; ============================================================================

(defn create-limiter!
  "Create a new request limiter."
  [limiter-id config]
  (let [limiter {:id limiter-id
                 :name (get config :name (name limiter-id))
                 :type (get config :type :rate)  ;; :rate, :concurrent, :bandwidth, :composite
                 :rate-limit (get config :rate-limit (get-in @state [:config :default-rate-limit]))
                 :window-ms (get config :window-ms (get-in @state [:config :default-window-ms]))
                 :concurrent-limit (get config :concurrent-limit (get-in @state [:config :default-concurrent-limit]))
                 :bandwidth-limit (get config :bandwidth-limit (get-in @state [:config :default-bandwidth-limit-bytes]))
                 :key-fn (get config :key-fn (constantly :global))
                 :counters (atom {})  ;; key -> {:count, :window-start, :bytes}
                 :semaphore (when (#{:concurrent :composite} (get config :type :rate))
                              (Semaphore. (get config :concurrent-limit 10)))
                 :enabled? (atom true)
                 :metrics {:checked (atom 0)
                           :allowed (atom 0)
                           :denied (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:limiters limiter-id] limiter)
    (logging/log :info "Created limiter" {:limiter-id limiter-id :type (:type limiter)})
    limiter-id))

(defn get-limiter
  "Get a limiter by ID."
  [limiter-id]
  (get-in @state [:limiters limiter-id]))

(defn list-limiters
  "List all limiters."
  []
  (mapv (fn [[id l]]
          {:id id
           :name (:name l)
           :type (:type l)
           :rate-limit (:rate-limit l)
           :window-ms (:window-ms l)
           :enabled? @(:enabled? l)})
        (:limiters @state)))

(defn delete-limiter!
  "Delete a limiter."
  [limiter-id]
  (swap! state update :limiters dissoc limiter-id))

;; ============================================================================
;; Rate Limiting
;; ============================================================================

(defn- get-or-create-counter
  "Get or create a counter for a key."
  [limiter key]
  (let [counters (:counters limiter)
        now (System/currentTimeMillis)
        window-ms (:window-ms limiter)]
    (swap! counters
           (fn [c]
             (let [existing (get c key)]
               (if (and existing
                        (< (- now (:window-start existing)) window-ms))
                 c
                 (assoc c key {:count (AtomicLong. 0)
                               :bytes (AtomicLong. 0)
                               :window-start now})))))
    (get @counters key)))

(defn- check-rate-limit
  "Check if a request is within rate limit."
  [limiter key]
  (let [counter (get-or-create-counter limiter key)
        count (.incrementAndGet (:count counter))
        limit (:rate-limit limiter)]
    (<= count limit)))

(defn- get-rate-limit-info
  "Get rate limit info for headers."
  [limiter key]
  (let [counter (get-or-create-counter limiter key)
        count (.get (:count counter))
        limit (:rate-limit limiter)
        window-ms (:window-ms limiter)
        window-start (:window-start counter)
        reset-at (+ window-start window-ms)
        remaining (max 0 (- limit count))]
    {:limit limit
     :remaining remaining
     :reset-at reset-at
     :window-ms window-ms}))

;; ============================================================================
;; Concurrent Limiting
;; ============================================================================

(defn- acquire-concurrent
  "Try to acquire a concurrent slot."
  [limiter]
  (when-let [semaphore (:semaphore limiter)]
    (.tryAcquire semaphore)))

(defn- release-concurrent
  "Release a concurrent slot."
  [limiter]
  (when-let [semaphore (:semaphore limiter)]
    (.release semaphore)))

(defn- get-concurrent-info
  "Get concurrent limit info."
  [limiter]
  (when-let [semaphore (:semaphore limiter)]
    {:limit (:concurrent-limit limiter)
     :available (.availablePermits semaphore)
     :in-use (- (:concurrent-limit limiter) (.availablePermits semaphore))}))

;; ============================================================================
;; Bandwidth Limiting
;; ============================================================================

(defn- check-bandwidth-limit
  "Check if a request is within bandwidth limit."
  [limiter key request-size]
  (let [counter (get-or-create-counter limiter key)
        new-bytes (.addAndGet (:bytes counter) request-size)
        limit (:bandwidth-limit limiter)]
    (<= new-bytes limit)))

(defn- get-bandwidth-info
  "Get bandwidth limit info."
  [limiter key]
  (let [counter (get-or-create-counter limiter key)
        bytes (.get (:bytes counter))
        limit (:bandwidth-limit limiter)
        remaining (max 0 (- limit bytes))]
    {:limit limit
     :used bytes
     :remaining remaining}))

;; ============================================================================
;; Composite Limiting
;; ============================================================================

(defn check-limit
  "Check if a request passes all limits."
  [limiter-id request]
  (swap! state update-in [:stats :requests-checked] inc)
  
  (if-let [limiter (get-limiter limiter-id)]
    (if @(:enabled? limiter)
      (let [key ((:key-fn limiter) request)
            request-size (or (get-in request [:headers "content-length"]) 0)
            type (:type limiter)]
        
        (swap! (get-in limiter [:metrics :checked]) inc)
        
        (let [allowed? (case type
                         :rate (check-rate-limit limiter key)
                         :concurrent (acquire-concurrent limiter)
                         :bandwidth (check-bandwidth-limit limiter key request-size)
                         :composite (and (check-rate-limit limiter key)
                                         (acquire-concurrent limiter)
                                         (check-bandwidth-limit limiter key request-size))
                         true)]
          
          (if allowed?
            (do
              (swap! (get-in limiter [:metrics :allowed]) inc)
              (swap! state update-in [:stats :requests-allowed] inc)
              {:allowed? true
               :limiter-id limiter-id
               :key key
               :info (merge (get-rate-limit-info limiter key)
                            (get-concurrent-info limiter)
                            (get-bandwidth-info limiter key))})
            (do
              (swap! (get-in limiter [:metrics :denied]) inc)
              (swap! state update-in [:stats :requests-denied] inc)
              (case type
                :rate (swap! state update-in [:stats :rate-limited] inc)
                :concurrent (swap! state update-in [:stats :concurrent-limited] inc)
                :bandwidth (swap! state update-in [:stats :bandwidth-limited] inc)
                nil)
              {:allowed? false
               :limiter-id limiter-id
               :key key
               :reason type
               :info (merge (get-rate-limit-info limiter key)
                            (get-concurrent-info limiter)
                            (get-bandwidth-info limiter key))}))))
      {:allowed? true :limiter-id limiter-id :reason :disabled})
    {:allowed? true :limiter-id limiter-id :reason :not-found}))

(defn release-limit
  "Release any held resources (for concurrent limiting)."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (when (#{:concurrent :composite} (:type limiter))
      (release-concurrent limiter))))

;; ============================================================================
;; Quota Management
;; ============================================================================

(defn set-quota!
  "Set a quota for a client."
  [client-id quota]
  (swap! state assoc-in [:quotas client-id]
         {:rate-limit (get quota :rate-limit)
          :concurrent-limit (get quota :concurrent-limit)
          :bandwidth-limit (get quota :bandwidth-limit)
          :expires-at (get quota :expires-at)
          :created-at (System/currentTimeMillis)}))

(defn get-quota
  "Get a client's quota."
  [client-id]
  (get-in @state [:quotas client-id]))

(defn delete-quota!
  "Delete a client's quota."
  [client-id]
  (swap! state update :quotas dissoc client-id))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn- add-limit-headers
  "Add rate limit headers to response."
  [response info]
  (if (get-in @state [:config :include-headers?])
    (-> response
        (assoc-in [:headers "X-RateLimit-Limit"] (str (:limit info)))
        (assoc-in [:headers "X-RateLimit-Remaining"] (str (:remaining info)))
        (assoc-in [:headers "X-RateLimit-Reset"] (str (:reset-at info))))
    response))

(defn wrap-limiter
  "Ring middleware for request limiting."
  [handler limiter-id]
  (fn [request]
    (let [result (check-limit limiter-id request)]
      (if (:allowed? result)
        (try
          (let [response (handler request)]
            (add-limit-headers response (:info result)))
          (finally
            (release-limit limiter-id)))
        {:status 429
         :headers (cond-> {"Content-Type" "application/json"}
                    (:info result)
                    (merge {"X-RateLimit-Limit" (str (get-in result [:info :limit]))
                            "X-RateLimit-Remaining" "0"
                            "X-RateLimit-Reset" (str (get-in result [:info :reset-at]))
                            "Retry-After" (str (quot (- (get-in result [:info :reset-at])
                                                        (System/currentTimeMillis))
                                                     1000))}))
         :body {:error "Rate limit exceeded"
                :reason (:reason result)
                :retry-after (when-let [reset-at (get-in result [:info :reset-at])]
                               (quot (- reset-at (System/currentTimeMillis)) 1000))}}))))

(defn wrap-per-client-limiter
  "Ring middleware for per-client limiting."
  [handler limiter-id client-key-fn]
  (let [limiter (get-limiter limiter-id)]
    (when limiter
      (swap! state assoc-in [:limiters limiter-id :key-fn] client-key-fn)))
  (wrap-limiter handler limiter-id))

;; ============================================================================
;; Limiter Control
;; ============================================================================

(defn enable-limiter!
  "Enable a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (reset! (:enabled? limiter) true)))

(defn disable-limiter!
  "Disable a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (reset! (:enabled? limiter) false)))

(defn reset-limiter!
  "Reset a limiter's counters."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    (reset! (:counters limiter) {})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-limiter-metrics
  "Get metrics for a limiter."
  [limiter-id]
  (when-let [limiter (get-limiter limiter-id)]
    {:limiter-id limiter-id
     :name (:name limiter)
     :type (:type limiter)
     :checked @(get-in limiter [:metrics :checked])
     :allowed @(get-in limiter [:metrics :allowed])
     :denied @(get-in limiter [:metrics :denied])
     :denial-rate (let [checked @(get-in limiter [:metrics :checked])]
                    (if (pos? checked)
                      (/ @(get-in limiter [:metrics :denied]) checked)
                      0))
     :enabled? @(:enabled? limiter)}))

(defn get-all-limiter-metrics
  "Get metrics for all limiters."
  []
  (mapv (fn [[id _]] (get-limiter-metrics id)) (:limiters @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-limiter-stats
  "Get limiter statistics."
  []
  (let [stats (:stats @state)]
    {:limiters-count (count (:limiters @state))
     :quotas-count (count (:quotas @state))
     :requests-checked (:requests-checked stats)
     :requests-allowed (:requests-allowed stats)
     :requests-denied (:requests-denied stats)
     :rate-limited (:rate-limited stats)
     :concurrent-limited (:concurrent-limited stats)
     :bandwidth-limited (:bandwidth-limited stats)
     :denial-rate (if (pos? (:requests-checked stats))
                    (/ (:requests-denied stats) (:requests-checked stats))
                    0)}))

(defn reset-stats!
  "Reset limiter statistics."
  []
  (swap! state assoc :stats {:requests-checked 0
                             :requests-allowed 0
                             :requests-denied 0
                             :rate-limited 0
                             :concurrent-limited 0
                             :bandwidth-limited 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-limiter!
  "Initialize the request limiter."
  []
  (when-not (:initialized? @state)
    ;; Create default global limiter
    (create-limiter! :global
                     {:name "Global Rate Limiter"
                      :type :rate
                      :rate-limit 1000
                      :window-ms 60000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request limiter initialized")
    (events/emit! :request-limiter-initialized {})
    true))
