(ns mental-models.middleware.rate-limit
  "Rate Limiting Middleware for Mental Models Pipeline
   
   Provides rate limiting with:
   - Token bucket algorithm
   - Per-client rate limits
   - Configurable limits per endpoint
   - Rate limit headers in responses
   - Sliding window counters"
  (:require
   [clojure.string :as str])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.time Instant Duration]))

;; =============================================================================
;; STATE
;; =============================================================================

(def ^:private rate-limit-state (atom {}))
(def ^:private client-buckets (ConcurrentHashMap.))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(def default-config
  {:default-limit 100
   :default-window-seconds 60
   :endpoints {"/api/analyze" {:limit 10 :window-seconds 60}
               "/api/models" {:limit 100 :window-seconds 60}
               "/api/health" {:limit 1000 :window-seconds 60}}})

(defn get-config []
  (or @rate-limit-state default-config))

(defn set-config! [config]
  (reset! rate-limit-state config))

;; =============================================================================
;; TOKEN BUCKET IMPLEMENTATION
;; =============================================================================

(defrecord TokenBucket [tokens last-refill capacity refill-rate])

(defn create-bucket [capacity refill-rate]
  (->TokenBucket capacity (Instant/now) capacity refill-rate))

(defn refill-bucket [{:keys [tokens last-refill capacity refill-rate] :as bucket}]
  (let [now (Instant/now)
        elapsed-seconds (.getSeconds (Duration/between last-refill now))
        new-tokens (min capacity (+ tokens (* elapsed-seconds refill-rate)))]
    (assoc bucket :tokens new-tokens :last-refill now)))

(defn try-consume [{:keys [tokens] :as bucket} amount]
  (let [refilled (refill-bucket bucket)]
    (if (>= (:tokens refilled) amount)
      [(assoc refilled :tokens (- (:tokens refilled) amount)) true]
      [refilled false])))

;; =============================================================================
;; CLIENT TRACKING
;; =============================================================================

(defn get-client-id [request]
  (or (get-in request [:headers "x-forwarded-for"])
      (get-in request [:headers "x-real-ip"])
      (:remote-addr request)
      "unknown"))

(defn get-bucket-key [client-id endpoint]
  (str client-id ":" endpoint))

(defn get-or-create-bucket [client-id endpoint config]
  (let [key (get-bucket-key client-id endpoint)
        endpoint-config (get-in config [:endpoints endpoint] 
                                {:limit (:default-limit config)
                                 :window-seconds (:default-window-seconds config)})
        capacity (:limit endpoint-config)
        refill-rate (/ capacity (:window-seconds endpoint-config))]
    (or (.get client-buckets key)
        (let [bucket (create-bucket capacity refill-rate)]
          (.putIfAbsent client-buckets key bucket)
          (.get client-buckets key)))))

(defn update-bucket! [client-id endpoint bucket]
  (.put client-buckets (get-bucket-key client-id endpoint) bucket))

;; =============================================================================
;; RATE LIMIT HEADERS
;; =============================================================================

(defn add-rate-limit-headers [response bucket config endpoint]
  (let [endpoint-config (get-in config [:endpoints endpoint]
                                {:limit (:default-limit config)
                                 :window-seconds (:default-window-seconds config)})]
    (-> response
        (assoc-in [:headers "X-RateLimit-Limit"] (str (:limit endpoint-config)))
        (assoc-in [:headers "X-RateLimit-Remaining"] (str (int (:tokens bucket))))
        (assoc-in [:headers "X-RateLimit-Reset"] (str (+ (.getEpochSecond (Instant/now))
                                                         (:window-seconds endpoint-config)))))))

(defn rate-limit-exceeded-response [bucket config endpoint]
  (let [endpoint-config (get-in config [:endpoints endpoint]
                                {:limit (:default-limit config)
                                 :window-seconds (:default-window-seconds config)})]
    {:status 429
     :headers {"Content-Type" "application/json"
               "X-RateLimit-Limit" (str (:limit endpoint-config))
               "X-RateLimit-Remaining" "0"
               "X-RateLimit-Reset" (str (+ (.getEpochSecond (Instant/now))
                                           (:window-seconds endpoint-config)))
               "Retry-After" (str (:window-seconds endpoint-config))}
     :body "{\"error\": \"Rate limit exceeded\"}"}))

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn wrap-rate-limit [handler]
  (fn [request]
    (let [config (get-config)
          client-id (get-client-id request)
          endpoint (:uri request)
          bucket (get-or-create-bucket client-id endpoint config)
          [new-bucket allowed?] (try-consume bucket 1)]
      (update-bucket! client-id endpoint new-bucket)
      (if allowed?
        (-> (handler request)
            (add-rate-limit-headers new-bucket config endpoint))
        (rate-limit-exceeded-response new-bucket config endpoint)))))

;; =============================================================================
;; SLIDING WINDOW COUNTER
;; =============================================================================

(def ^:private window-counters (ConcurrentHashMap.))

(defrecord WindowCounter [counts window-start window-seconds])

(defn create-window-counter [window-seconds]
  (->WindowCounter {} (Instant/now) window-seconds))

(defn get-window-key [timestamp window-seconds]
  (quot (.getEpochSecond timestamp) window-seconds))

(defn increment-window-counter [{:keys [counts window-start window-seconds] :as counter}]
  (let [now (Instant/now)
        current-window (get-window-key now window-seconds)
        previous-window (dec current-window)
        new-counts (-> counts
                       (update current-window (fnil inc 0))
                       (select-keys [current-window previous-window]))]
    (assoc counter :counts new-counts :window-start now)))

(defn get-window-count [{:keys [counts window-seconds]}]
  (let [now (Instant/now)
        current-window (get-window-key now window-seconds)
        previous-window (dec current-window)
        current-count (get counts current-window 0)
        previous-count (get counts previous-window 0)
        elapsed-in-window (mod (.getEpochSecond now) window-seconds)
        weight (/ (- window-seconds elapsed-in-window) window-seconds)]
    (+ current-count (* previous-count weight))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-rate-limit-stats []
  {:total-clients (.size client-buckets)
   :config (get-config)
   :buckets (into {} (map (fn [[k v]] [k {:tokens (:tokens v)
                                          :capacity (:capacity v)}])
                          client-buckets))})

(defn clear-expired-buckets! []
  (let [now (Instant/now)
        expiry-threshold (Duration/ofHours 1)]
    (doseq [[k bucket] client-buckets]
      (when (> (.getSeconds (Duration/between (:last-refill bucket) now))
               (.getSeconds expiry-threshold))
        (.remove client-buckets k)))))
