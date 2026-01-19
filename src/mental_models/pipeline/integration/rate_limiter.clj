(ns mental-models.pipeline.integration.rate-limiter
  "Rate Limiter Module
   
   Request throttling:
   - Token bucket algorithm
   - Sliding window
   - Fixed window
   - Leaky bucket
   - Per-client limits"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; RATE LIMITER STATE
;; =============================================================================

(defonce limiter-state (atom {:limiters {}
                              :buckets {}
                              :windows {}
                              :config {:default-rate 100
                                       :default-window-ms 60000
                                       :default-burst 10}}))

;; =============================================================================
;; TOKEN BUCKET
;; =============================================================================

(defn create-token-bucket
  "Create a token bucket."
  [rate burst]
  {:tokens (AtomicLong. burst)
   :rate rate
   :burst burst
   :last-refill (AtomicLong. (System/currentTimeMillis))})

(defn refill-bucket!
  "Refill tokens in a bucket."
  [bucket]
  (let [now (System/currentTimeMillis)
        last-refill (.get ^AtomicLong (:last-refill bucket))
        elapsed (- now last-refill)
        tokens-to-add (long (* (:rate bucket) (/ elapsed 1000.0)))
        current-tokens (.get ^AtomicLong (:tokens bucket))
        new-tokens (min (:burst bucket) (+ current-tokens tokens-to-add))]
    (when (pos? tokens-to-add)
      (.set ^AtomicLong (:tokens bucket) new-tokens)
      (.set ^AtomicLong (:last-refill bucket) now))
    new-tokens))

(defn try-acquire-token!
  "Try to acquire a token from a bucket."
  [bucket & {:keys [tokens] :or {tokens 1}}]
  (refill-bucket! bucket)
  (let [current (.get ^AtomicLong (:tokens bucket))]
    (if (>= current tokens)
      (do
        (.addAndGet ^AtomicLong (:tokens bucket) (- tokens))
        true)
      false)))

;; =============================================================================
;; SLIDING WINDOW
;; =============================================================================

(defn create-sliding-window
  "Create a sliding window counter."
  [window-ms max-requests]
  {:window-ms window-ms
   :max-requests max-requests
   :requests (atom [])})

(defn clean-window!
  "Remove expired entries from window."
  [window]
  (let [cutoff (- (System/currentTimeMillis) (:window-ms window))]
    (swap! (:requests window) (fn [reqs] (vec (filter #(> % cutoff) reqs))))))

(defn try-sliding-window!
  "Try to add a request to sliding window."
  [window]
  (clean-window! window)
  (let [current-count (count @(:requests window))]
    (if (< current-count (:max-requests window))
      (do
        (swap! (:requests window) conj (System/currentTimeMillis))
        true)
      false)))

;; =============================================================================
;; FIXED WINDOW
;; =============================================================================

(defn create-fixed-window
  "Create a fixed window counter."
  [window-ms max-requests]
  {:window-ms window-ms
   :max-requests max-requests
   :count (AtomicLong. 0)
   :window-start (AtomicLong. (System/currentTimeMillis))})

(defn try-fixed-window!
  "Try to add a request to fixed window."
  [window]
  (let [now (System/currentTimeMillis)
        window-start (.get ^AtomicLong (:window-start window))]
    ;; Check if window expired
    (when (> (- now window-start) (:window-ms window))
      (.set ^AtomicLong (:count window) 0)
      (.set ^AtomicLong (:window-start window) now))
    ;; Try to increment
    (let [current (.get ^AtomicLong (:count window))]
      (if (< current (:max-requests window))
        (do
          (.incrementAndGet ^AtomicLong (:count window))
          true)
        false))))

;; =============================================================================
;; LEAKY BUCKET
;; =============================================================================

(defn create-leaky-bucket
  "Create a leaky bucket."
  [rate capacity]
  {:rate rate
   :capacity capacity
   :water (AtomicLong. 0)
   :last-leak (AtomicLong. (System/currentTimeMillis))})

(defn leak-bucket!
  "Leak water from bucket."
  [bucket]
  (let [now (System/currentTimeMillis)
        last-leak (.get ^AtomicLong (:last-leak bucket))
        elapsed (- now last-leak)
        leaked (long (* (:rate bucket) (/ elapsed 1000.0)))
        current (.get ^AtomicLong (:water bucket))
        new-water (max 0 (- current leaked))]
    (when (pos? leaked)
      (.set ^AtomicLong (:water bucket) new-water)
      (.set ^AtomicLong (:last-leak bucket) now))
    new-water))

(defn try-leaky-bucket!
  "Try to add water to leaky bucket."
  [bucket]
  (leak-bucket! bucket)
  (let [current (.get ^AtomicLong (:water bucket))]
    (if (< current (:capacity bucket))
      (do
        (.incrementAndGet ^AtomicLong (:water bucket))
        true)
      false)))

;; =============================================================================
;; LIMITER MANAGEMENT
;; =============================================================================

(defn register-limiter!
  "Register a rate limiter."
  [limiter-id {:keys [type rate window-ms burst capacity]}]
  (log/info "Registering rate limiter" {:id limiter-id :type type})
  (let [limiter (case type
                  :token-bucket (create-token-bucket
                                 (or rate (get-in @limiter-state [:config :default-rate]))
                                 (or burst (get-in @limiter-state [:config :default-burst])))
                  :sliding-window (create-sliding-window
                                   (or window-ms (get-in @limiter-state [:config :default-window-ms]))
                                   (or rate (get-in @limiter-state [:config :default-rate])))
                  :fixed-window (create-fixed-window
                                 (or window-ms (get-in @limiter-state [:config :default-window-ms]))
                                 (or rate (get-in @limiter-state [:config :default-rate])))
                  :leaky-bucket (create-leaky-bucket
                                 (or rate (get-in @limiter-state [:config :default-rate]))
                                 (or capacity (get-in @limiter-state [:config :default-burst])))
                  ;; Default to token bucket
                  (create-token-bucket
                   (or rate (get-in @limiter-state [:config :default-rate]))
                   (or burst (get-in @limiter-state [:config :default-burst]))))]
    (swap! limiter-state assoc-in [:limiters limiter-id]
           {:id limiter-id
            :type (or type :token-bucket)
            :limiter limiter
            :created-at (System/currentTimeMillis)})
    (metrics/inc-counter! :ratelimit/limiters-registered)
    limiter-id))

(defn unregister-limiter!
  "Unregister a rate limiter."
  [limiter-id]
  (log/info "Unregistering rate limiter" {:id limiter-id})
  (swap! limiter-state update :limiters dissoc limiter-id))

(defn get-limiter
  "Get a rate limiter."
  [limiter-id]
  (get-in @limiter-state [:limiters limiter-id]))

(defn list-limiters
  "List all rate limiters."
  []
  (keys (:limiters @limiter-state)))

;; =============================================================================
;; RATE LIMITING
;; =============================================================================

(defn allow?
  "Check if a request is allowed."
  [limiter-id]
  (when (flags/is-enabled? "rate-limiter")
    (if-let [limiter-info (get-limiter limiter-id)]
      (let [limiter (:limiter limiter-info)
            allowed? (case (:type limiter-info)
                       :token-bucket (try-acquire-token! limiter)
                       :sliding-window (try-sliding-window! limiter)
                       :fixed-window (try-fixed-window! limiter)
                       :leaky-bucket (try-leaky-bucket! limiter)
                       false)]
        (if allowed?
          (do
            (metrics/inc-counter! :ratelimit/requests-allowed)
            true)
          (do
            (metrics/inc-counter! :ratelimit/requests-denied)
            (events/publish! :ratelimit/request-denied {:limiter limiter-id})
            false)))
      ;; No limiter registered, allow by default
      true)))

(defn check-rate-limit
  "Check rate limit and return result with details."
  [limiter-id]
  (when (flags/is-enabled? "rate-limiter")
    (if-let [limiter-info (get-limiter limiter-id)]
      (let [limiter (:limiter limiter-info)
            allowed? (allow? limiter-id)]
        {:allowed? allowed?
         :limiter-id limiter-id
         :type (:type limiter-info)
         :remaining (case (:type limiter-info)
                      :token-bucket (.get ^AtomicLong (:tokens limiter))
                      :sliding-window (- (:max-requests limiter) (count @(:requests limiter)))
                      :fixed-window (- (:max-requests limiter) (.get ^AtomicLong (:count limiter)))
                      :leaky-bucket (- (:capacity limiter) (.get ^AtomicLong (:water limiter)))
                      0)})
      {:allowed? true :limiter-id limiter-id :remaining nil})))

;; =============================================================================
;; PER-CLIENT LIMITING
;; =============================================================================

(defn get-client-limiter-id
  "Get limiter ID for a client."
  [base-limiter-id client-id]
  (str base-limiter-id ":" client-id))

(defn ensure-client-limiter!
  "Ensure a limiter exists for a client."
  [base-limiter-id client-id]
  (let [client-limiter-id (get-client-limiter-id base-limiter-id client-id)]
    (when-not (get-limiter client-limiter-id)
      (when-let [base-limiter (get-limiter base-limiter-id)]
        (register-limiter! client-limiter-id
                           {:type (:type base-limiter)
                            :rate (get-in base-limiter [:limiter :rate])
                            :burst (get-in base-limiter [:limiter :burst])
                            :window-ms (get-in base-limiter [:limiter :window-ms])
                            :capacity (get-in base-limiter [:limiter :capacity])})))
    client-limiter-id))

(defn allow-client?
  "Check if a client request is allowed."
  [base-limiter-id client-id]
  (let [client-limiter-id (ensure-client-limiter! base-limiter-id client-id)]
    (allow? client-limiter-id)))

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn rate-limit-middleware
  "Create rate limiting middleware."
  [limiter-id]
  (fn [handler]
    (fn [request]
      (if (allow? limiter-id)
        (handler request)
        {:status 429
         :headers {"Retry-After" "60"}
         :body {:error "Rate limit exceeded"}}))))

(defn client-rate-limit-middleware
  "Create per-client rate limiting middleware."
  [base-limiter-id client-id-fn]
  (fn [handler]
    (fn [request]
      (let [client-id (client-id-fn request)]
        (if (allow-client? base-limiter-id client-id)
          (handler request)
          {:status 429
           :headers {"Retry-After" "60"}
           :body {:error "Rate limit exceeded"}})))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-rate-limiter!
  "Initialize rate limiter."
  []
  (log/info "Initializing rate limiter")
  ;; Register feature flag
  (flags/register-flag! "rate-limiter" "Enable rate limiter" true)
  ;; Create metrics
  (metrics/create-counter! :ratelimit/limiters-registered "Limiters registered")
  (metrics/create-counter! :ratelimit/requests-allowed "Requests allowed")
  (metrics/create-counter! :ratelimit/requests-denied "Requests denied")
  (metrics/create-gauge! :ratelimit/active-limiters "Active limiters"
                         #(count (:limiters @limiter-state)))
  (log/info "Rate limiter initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-limiter-status []
  {:enabled (flags/is-enabled? "rate-limiter")
   :limiters (count (:limiters @limiter-state))
   :config (:config @limiter-state)})

(defn get-limiter-stats
  "Get statistics for a limiter."
  [limiter-id]
  (when-let [limiter-info (get-limiter limiter-id)]
    (let [limiter (:limiter limiter-info)]
      {:id limiter-id
       :type (:type limiter-info)
       :stats (case (:type limiter-info)
                :token-bucket {:tokens (.get ^AtomicLong (:tokens limiter))
                               :rate (:rate limiter)
                               :burst (:burst limiter)}
                :sliding-window {:requests (count @(:requests limiter))
                                 :max-requests (:max-requests limiter)
                                 :window-ms (:window-ms limiter)}
                :fixed-window {:count (.get ^AtomicLong (:count limiter))
                               :max-requests (:max-requests limiter)
                               :window-ms (:window-ms limiter)}
                :leaky-bucket {:water (.get ^AtomicLong (:water limiter))
                               :rate (:rate limiter)
                               :capacity (:capacity limiter)}
                {})})))
