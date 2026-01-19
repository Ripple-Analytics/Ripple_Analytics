(ns mental-models.pipeline.integration.rate-limiter-v2
  "Advanced rate limiter for mental model analysis system.
   
   Features:
   - Multiple algorithms (token bucket, sliding window, fixed window)
   - Distributed rate limiting
   - Per-user and per-API limits
   - Burst handling
   - Rate limit headers
   - Quota management
   - Adaptive rate limiting
   - Cost-based limiting"
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
           [java.util.concurrent ConcurrentHashMap]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:limiters {}         ;; limiter-id -> limiter-config
         :buckets {}          ;; key -> bucket-state
         :quotas {}           ;; quota-id -> quota
         :overrides {}        ;; key -> override
         :config {:default-algorithm :token-bucket
                  :default-rate 100
                  :default-burst 10
                  :sync-interval-ms 1000}
         :stats {:requests-allowed 0 :requests-denied 0 :total-requests 0}
         :initialized? false}))

;; ============================================================================
;; Rate Limiter Configuration
;; ============================================================================

(defn create-limiter!
  "Create a rate limiter."
  [limiter-id config]
  (let [limiter {:id limiter-id
                 :name (get config :name (name limiter-id))
                 :algorithm (get config :algorithm (get-in @state [:config :default-algorithm]))
                 :rate (get config :rate (get-in @state [:config :default-rate])) ;; requests per window
                 :window-ms (get config :window-ms 60000) ;; window size in ms
                 :burst (get config :burst (get-in @state [:config :default-burst]))
                 :key-fn (get config :key-fn identity) ;; function to extract key from request
                 :cost-fn (get config :cost-fn (constantly 1)) ;; function to calculate request cost
                 :enabled? (get config :enabled? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:limiters limiter-id] limiter)
    (logging/log :info "Created rate limiter" {:limiter-id limiter-id :algorithm (:algorithm limiter)})
    limiter-id))

(defn get-limiter
  "Get a rate limiter."
  [limiter-id]
  (get-in @state [:limiters limiter-id]))

(defn list-limiters
  "List all rate limiters."
  []
  (mapv (fn [[id l]]
          {:id id
           :name (:name l)
           :algorithm (:algorithm l)
           :rate (:rate l)
           :enabled? (:enabled? l)})
        (:limiters @state)))

(defn enable-limiter!
  "Enable a rate limiter."
  [limiter-id]
  (swap! state assoc-in [:limiters limiter-id :enabled?] true))

(defn disable-limiter!
  "Disable a rate limiter."
  [limiter-id]
  (swap! state assoc-in [:limiters limiter-id :enabled?] false))

;; ============================================================================
;; Token Bucket Algorithm
;; ============================================================================

(defn- get-or-create-bucket
  "Get or create a token bucket."
  [limiter-id key]
  (let [bucket-key [limiter-id key]
        limiter (get-limiter limiter-id)]
    (if-let [bucket (get-in @state [:buckets bucket-key])]
      bucket
      (let [new-bucket {:tokens (atom (double (:burst limiter)))
                        :last-refill (atom (System/currentTimeMillis))
                        :created-at (System/currentTimeMillis)}]
        (swap! state assoc-in [:buckets bucket-key] new-bucket)
        new-bucket))))

(defn- refill-bucket!
  "Refill tokens in a bucket."
  [bucket limiter]
  (let [now (System/currentTimeMillis)
        last-refill @(:last-refill bucket)
        elapsed-ms (- now last-refill)
        refill-rate (/ (:rate limiter) (:window-ms limiter))
        tokens-to-add (* refill-rate elapsed-ms)
        current-tokens @(:tokens bucket)
        new-tokens (min (:burst limiter) (+ current-tokens tokens-to-add))]
    (reset! (:tokens bucket) new-tokens)
    (reset! (:last-refill bucket) now)
    new-tokens))

(defn- try-consume-token-bucket
  "Try to consume tokens from a bucket."
  [limiter-id key cost]
  (let [limiter (get-limiter limiter-id)
        bucket (get-or-create-bucket limiter-id key)
        _ (refill-bucket! bucket limiter)
        current-tokens @(:tokens bucket)]
    (if (>= current-tokens cost)
      (do
        (swap! (:tokens bucket) - cost)
        {:allowed? true
         :remaining (int @(:tokens bucket))
         :limit (:burst limiter)
         :reset-ms (:window-ms limiter)})
      {:allowed? false
       :remaining 0
       :limit (:burst limiter)
       :reset-ms (long (/ (* cost (:window-ms limiter)) (:rate limiter)))
       :retry-after-ms (long (/ (* (- cost current-tokens) (:window-ms limiter)) (:rate limiter)))})))

;; ============================================================================
;; Sliding Window Algorithm
;; ============================================================================

(defn- get-or-create-window
  "Get or create a sliding window."
  [limiter-id key]
  (let [window-key [limiter-id key]]
    (if-let [window (get-in @state [:buckets window-key])]
      window
      (let [new-window {:requests (atom [])
                        :created-at (System/currentTimeMillis)}]
        (swap! state assoc-in [:buckets window-key] new-window)
        new-window))))

(defn- try-consume-sliding-window
  "Try to consume from a sliding window."
  [limiter-id key cost]
  (let [limiter (get-limiter limiter-id)
        window (get-or-create-window limiter-id key)
        now (System/currentTimeMillis)
        window-start (- now (:window-ms limiter))
        
        ;; Clean old requests
        _ (swap! (:requests window) (fn [reqs]
                                      (vec (filter #(> (:timestamp %) window-start) reqs))))
        
        current-requests @(:requests window)
        current-count (reduce + (map :cost current-requests))]
    
    (if (< (+ current-count cost) (:rate limiter))
      (do
        (swap! (:requests window) conj {:timestamp now :cost cost})
        {:allowed? true
         :remaining (int (- (:rate limiter) current-count cost))
         :limit (:rate limiter)
         :reset-ms (:window-ms limiter)})
      {:allowed? false
       :remaining 0
       :limit (:rate limiter)
       :reset-ms (if (seq current-requests)
                   (- (:window-ms limiter) (- now (:timestamp (first current-requests))))
                   (:window-ms limiter))
       :retry-after-ms (if (seq current-requests)
                         (- (:window-ms limiter) (- now (:timestamp (first current-requests))))
                         1000)})))

;; ============================================================================
;; Fixed Window Algorithm
;; ============================================================================

(defn- get-or-create-fixed-window
  "Get or create a fixed window."
  [limiter-id key]
  (let [window-key [limiter-id key]
        limiter (get-limiter limiter-id)
        now (System/currentTimeMillis)
        window-start (- now (mod now (:window-ms limiter)))]
    (if-let [window (get-in @state [:buckets window-key])]
      (if (= (:window-start window) window-start)
        window
        ;; New window period
        (let [new-window {:count (atom 0)
                          :window-start window-start
                          :created-at now}]
          (swap! state assoc-in [:buckets window-key] new-window)
          new-window))
      (let [new-window {:count (atom 0)
                        :window-start window-start
                        :created-at now}]
        (swap! state assoc-in [:buckets window-key] new-window)
        new-window))))

(defn- try-consume-fixed-window
  "Try to consume from a fixed window."
  [limiter-id key cost]
  (let [limiter (get-limiter limiter-id)
        window (get-or-create-fixed-window limiter-id key)
        now (System/currentTimeMillis)
        current-count @(:count window)
        window-end (+ (:window-start window) (:window-ms limiter))]
    
    (if (< (+ current-count cost) (:rate limiter))
      (do
        (swap! (:count window) + cost)
        {:allowed? true
         :remaining (int (- (:rate limiter) current-count cost))
         :limit (:rate limiter)
         :reset-ms (- window-end now)})
      {:allowed? false
       :remaining 0
       :limit (:rate limiter)
       :reset-ms (- window-end now)
       :retry-after-ms (- window-end now)})))

;; ============================================================================
;; Main Rate Limiting Function
;; ============================================================================

(defn check-rate-limit
  "Check if a request is allowed."
  [limiter-id request]
  (when (flags/enabled? :rate-limiter-v2)
    (when-let [limiter (get-limiter limiter-id)]
      (when (:enabled? limiter)
        (let [key ((:key-fn limiter) request)
              cost ((:cost-fn limiter) request)
              
              ;; Check for override
              override (get-in @state [:overrides key])
              
              result (if override
                       {:allowed? true :override? true}
                       (case (:algorithm limiter)
                         :token-bucket (try-consume-token-bucket limiter-id key cost)
                         :sliding-window (try-consume-sliding-window limiter-id key cost)
                         :fixed-window (try-consume-fixed-window limiter-id key cost)
                         (try-consume-token-bucket limiter-id key cost)))]
          
          ;; Update stats
          (swap! state update-in [:stats :total-requests] inc)
          (if (:allowed? result)
            (swap! state update-in [:stats :requests-allowed] inc)
            (do
              (swap! state update-in [:stats :requests-denied] inc)
              (events/emit! :rate-limit-exceeded {:limiter-id limiter-id :key key})))
          
          result)))))

(defn allow?
  "Simple check if request is allowed."
  [limiter-id request]
  (:allowed? (check-rate-limit limiter-id request)))

;; ============================================================================
;; Quota Management
;; ============================================================================

(defn create-quota!
  "Create a quota."
  [quota-id config]
  (let [quota {:id quota-id
               :name (get config :name (name quota-id))
               :limit (get config :limit 1000)
               :period (get config :period :monthly) ;; :daily, :weekly, :monthly
               :key (get config :key)
               :used (atom 0)
               :reset-at (atom (+ (System/currentTimeMillis) (* 30 24 60 60 1000)))
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:quotas quota-id] quota)
    (logging/log :info "Created quota" {:quota-id quota-id :limit (:limit quota)})
    quota-id))

(defn get-quota
  "Get a quota."
  [quota-id]
  (get-in @state [:quotas quota-id]))

(defn check-quota
  "Check if quota allows request."
  [quota-id amount]
  (when-let [quota (get-quota quota-id)]
    (let [now (System/currentTimeMillis)]
      ;; Reset if period expired
      (when (> now @(:reset-at quota))
        (reset! (:used quota) 0)
        (reset! (:reset-at quota) (+ now (* 30 24 60 60 1000))))
      
      (let [current-used @(:used quota)
            remaining (- (:limit quota) current-used)]
        (if (>= remaining amount)
          (do
            (swap! (:used quota) + amount)
            {:allowed? true
             :used (+ current-used amount)
             :remaining (- remaining amount)
             :limit (:limit quota)})
          {:allowed? false
           :used current-used
           :remaining remaining
           :limit (:limit quota)
           :reset-at @(:reset-at quota)})))))

(defn get-quota-usage
  "Get quota usage."
  [quota-id]
  (when-let [quota (get-quota quota-id)]
    {:quota-id quota-id
     :used @(:used quota)
     :limit (:limit quota)
     :remaining (- (:limit quota) @(:used quota))
     :utilization (/ @(:used quota) (:limit quota))
     :reset-at @(:reset-at quota)}))

;; ============================================================================
;; Overrides
;; ============================================================================

(defn add-override!
  "Add a rate limit override (whitelist)."
  [key & {:keys [expires-at reason]}]
  (swap! state assoc-in [:overrides key]
         {:key key
          :expires-at expires-at
          :reason reason
          :created-at (System/currentTimeMillis)})
  (logging/log :info "Added rate limit override" {:key key}))

(defn remove-override!
  "Remove a rate limit override."
  [key]
  (swap! state update :overrides dissoc key)
  (logging/log :info "Removed rate limit override" {:key key}))

(defn list-overrides
  "List all overrides."
  []
  (vals (:overrides @state)))

;; ============================================================================
;; Rate Limit Headers
;; ============================================================================

(defn get-rate-limit-headers
  "Get rate limit headers for response."
  [result]
  {"X-RateLimit-Limit" (str (:limit result))
   "X-RateLimit-Remaining" (str (:remaining result))
   "X-RateLimit-Reset" (str (+ (System/currentTimeMillis) (:reset-ms result)))
   "Retry-After" (when-not (:allowed? result)
                   (str (int (/ (:retry-after-ms result 1000) 1000))))})

;; ============================================================================
;; Adaptive Rate Limiting
;; ============================================================================

(defn adjust-rate!
  "Adjust rate limit based on system load."
  [limiter-id adjustment-factor]
  (when-let [limiter (get-limiter limiter-id)]
    (let [new-rate (int (* (:rate limiter) adjustment-factor))]
      (swap! state assoc-in [:limiters limiter-id :rate] new-rate)
      (logging/log :info "Adjusted rate limit" {:limiter-id limiter-id :new-rate new-rate}))))

(defn auto-adjust-rates!
  "Auto-adjust rates based on error rate."
  [error-rate]
  (let [adjustment-factor (cond
                            (> error-rate 0.1) 0.5  ;; High errors, reduce rate
                            (> error-rate 0.05) 0.8 ;; Medium errors, slightly reduce
                            (< error-rate 0.01) 1.2 ;; Low errors, increase rate
                            :else 1.0)]
    (doseq [[limiter-id _] (:limiters @state)]
      (adjust-rate! limiter-id adjustment-factor))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-rate-limiter-stats
  "Get rate limiter statistics."
  []
  (let [stats (:stats @state)]
    {:total-limiters (count (:limiters @state))
     :total-buckets (count (:buckets @state))
     :total-quotas (count (:quotas @state))
     :total-overrides (count (:overrides @state))
     :total-requests (:total-requests stats)
     :requests-allowed (:requests-allowed stats)
     :requests-denied (:requests-denied stats)
     :denial-rate (if (pos? (:total-requests stats))
                    (/ (:requests-denied stats) (:total-requests stats))
                    0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-rate-limiter-v2!
  "Initialize the rate limiter."
  []
  (when-not (:initialized? @state)
    ;; Create default limiters
    (create-limiter! :api-default
                     {:name "API Default Rate Limit"
                      :algorithm :token-bucket
                      :rate 100
                      :window-ms 60000
                      :burst 20
                      :key-fn :client-id})
    
    (create-limiter! :analysis-requests
                     {:name "Analysis Request Rate Limit"
                      :algorithm :sliding-window
                      :rate 10
                      :window-ms 60000
                      :key-fn :user-id
                      :cost-fn (fn [req] (get req :document-size 1))})
    
    (create-limiter! :lm-studio-calls
                     {:name "LM Studio Call Rate Limit"
                      :algorithm :token-bucket
                      :rate 60
                      :window-ms 60000
                      :burst 5
                      :key-fn (constantly :global)})
    
    ;; Create default quota
    (create-quota! :monthly-analysis
                   {:name "Monthly Analysis Quota"
                    :limit 10000
                    :period :monthly})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Rate limiter v2 initialized")
    (events/emit! :rate-limiter-v2-initialized {})
    true))
