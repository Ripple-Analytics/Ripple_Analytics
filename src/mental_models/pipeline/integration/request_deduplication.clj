(ns mental-models.pipeline.integration.request-deduplication
  "Request deduplication for mental model analysis system.
   
   Features:
   - Idempotency key handling
   - Request fingerprinting
   - Duplicate detection
   - Response caching
   - TTL-based expiration
   - Concurrent request handling
   - Deduplication metrics
   - Cleanup automation"
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
           [java.nio.charset StandardCharsets]
           [java.util.concurrent ConcurrentHashMap]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:requests {}         ;; idempotency-key -> request-state
         :fingerprints {}     ;; fingerprint -> idempotency-key
         :in-flight {}        ;; idempotency-key -> promise
         :config {:default-ttl-ms 86400000  ;; 24 hours
                  :max-entries 100000
                  :cleanup-interval-ms 300000
                  :fingerprint-fields [:method :path :body]}
         :stats {:total-requests 0 :duplicates-detected 0 :in-flight-hits 0}
         :initialized? false}))

;; ============================================================================
;; Fingerprinting
;; ============================================================================

(defn- compute-hash
  "Compute SHA-256 hash of data."
  [data]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (.getBytes (pr-str data) StandardCharsets/UTF_8)]
    (.update digest bytes)
    (let [hash-bytes (.digest digest)]
      (apply str (map #(format "%02x" %) hash-bytes)))))

(defn compute-fingerprint
  "Compute fingerprint for a request."
  [request & {:keys [fields]}]
  (let [fingerprint-fields (or fields (get-in @state [:config :fingerprint-fields]))
        fingerprint-data (select-keys request fingerprint-fields)]
    (compute-hash fingerprint-data)))

(defn extract-idempotency-key
  "Extract idempotency key from request."
  [request]
  (or (get-in request [:headers "Idempotency-Key"])
      (get-in request [:headers "X-Idempotency-Key"])
      (get request :idempotency-key)))

;; ============================================================================
;; Request State Management
;; ============================================================================

(defn- create-request-state
  "Create a new request state."
  [idempotency-key request]
  {:idempotency-key idempotency-key
   :fingerprint (compute-fingerprint request)
   :status :pending ;; :pending, :processing, :completed, :failed
   :request-summary {:method (:method request)
                     :path (:path request)
                     :timestamp (System/currentTimeMillis)}
   :response nil
   :created-at (System/currentTimeMillis)
   :completed-at nil
   :ttl-ms (get-in @state [:config :default-ttl-ms])})

(defn get-request-state
  "Get request state by idempotency key."
  [idempotency-key]
  (get-in @state [:requests idempotency-key]))

(defn- is-expired?
  "Check if a request state is expired."
  [request-state]
  (let [created-at (:created-at request-state)
        ttl-ms (:ttl-ms request-state)
        now (System/currentTimeMillis)]
    (> now (+ created-at ttl-ms))))

;; ============================================================================
;; Deduplication Logic
;; ============================================================================

(defn check-duplicate
  "Check if a request is a duplicate."
  [request]
  (when (flags/enabled? :request-deduplication)
    (swap! state update-in [:stats :total-requests] inc)
    
    (let [idempotency-key (extract-idempotency-key request)
          fingerprint (compute-fingerprint request)]
      
      (cond
        ;; Has idempotency key - check by key
        idempotency-key
        (when-let [existing (get-request-state idempotency-key)]
          (when-not (is-expired? existing)
            (swap! state update-in [:stats :duplicates-detected] inc)
            {:duplicate? true
             :idempotency-key idempotency-key
             :status (:status existing)
             :response (:response existing)
             :original-timestamp (:created-at existing)}))
        
        ;; No idempotency key - check by fingerprint
        :else
        (when-let [existing-key (get-in @state [:fingerprints fingerprint])]
          (when-let [existing (get-request-state existing-key)]
            (when-not (is-expired? existing)
              (swap! state update-in [:stats :duplicates-detected] inc)
              {:duplicate? true
               :idempotency-key existing-key
               :status (:status existing)
               :response (:response existing)
               :original-timestamp (:created-at existing)})))))))

(defn register-request!
  "Register a new request for deduplication."
  [request & {:keys [idempotency-key ttl-ms]}]
  (let [key (or idempotency-key
                (extract-idempotency-key request)
                (str (UUID/randomUUID)))
        fingerprint (compute-fingerprint request)
        request-state (-> (create-request-state key request)
                          (assoc :ttl-ms (or ttl-ms (get-in @state [:config :default-ttl-ms]))))]
    
    ;; Store request state
    (swap! state assoc-in [:requests key] request-state)
    
    ;; Store fingerprint mapping
    (swap! state assoc-in [:fingerprints fingerprint] key)
    
    (logging/log :debug "Registered request" {:idempotency-key key})
    key))

(defn start-processing!
  "Mark a request as processing."
  [idempotency-key]
  (swap! state assoc-in [:requests idempotency-key :status] :processing)
  
  ;; Create a promise for concurrent requests
  (let [p (promise)]
    (swap! state assoc-in [:in-flight idempotency-key] p)
    p))

(defn complete-request!
  "Mark a request as completed with response."
  [idempotency-key response & {:keys [status] :or {status :completed}}]
  (let [now (System/currentTimeMillis)]
    (swap! state update-in [:requests idempotency-key]
           (fn [r]
             (assoc r
                    :status status
                    :response response
                    :completed-at now)))
    
    ;; Deliver to waiting requests
    (when-let [p (get-in @state [:in-flight idempotency-key])]
      (deliver p {:status status :response response})
      (swap! state update :in-flight dissoc idempotency-key))
    
    (logging/log :debug "Completed request" {:idempotency-key idempotency-key :status status})))

(defn fail-request!
  "Mark a request as failed."
  [idempotency-key error]
  (complete-request! idempotency-key {:error error} :status :failed))

;; ============================================================================
;; Concurrent Request Handling
;; ============================================================================

(defn wait-for-completion
  "Wait for an in-flight request to complete."
  [idempotency-key & {:keys [timeout-ms] :or {timeout-ms 30000}}]
  (when-let [p (get-in @state [:in-flight idempotency-key])]
    (swap! state update-in [:stats :in-flight-hits] inc)
    (deref p timeout-ms {:status :timeout :response nil})))

(defn is-in-flight?
  "Check if a request is currently being processed."
  [idempotency-key]
  (contains? (:in-flight @state) idempotency-key))

(defn get-in-flight-count
  "Get count of in-flight requests."
  []
  (count (:in-flight @state)))

;; ============================================================================
;; Middleware
;; ============================================================================

(defn deduplication-middleware
  "Ring middleware for request deduplication."
  [handler]
  (fn [request]
    (if-let [duplicate (check-duplicate request)]
      ;; Duplicate detected
      (cond
        ;; Already completed - return cached response
        (= :completed (:status duplicate))
        (assoc (:response duplicate)
               :headers {"X-Idempotency-Key" (:idempotency-key duplicate)
                         "X-Duplicate-Request" "true"})
        
        ;; Still processing - wait for completion
        (= :processing (:status duplicate))
        (let [result (wait-for-completion (:idempotency-key duplicate))]
          (assoc (:response result)
                 :headers {"X-Idempotency-Key" (:idempotency-key duplicate)
                           "X-Duplicate-Request" "true"}))
        
        ;; Failed - allow retry
        :else
        (handler request))
      
      ;; New request
      (let [idempotency-key (register-request! request)
            _ (start-processing! idempotency-key)]
        (try
          (let [response (handler (assoc request :idempotency-key idempotency-key))]
            (complete-request! idempotency-key response)
            (assoc response
                   :headers (merge (:headers response)
                                   {"X-Idempotency-Key" idempotency-key})))
          (catch Exception e
            (fail-request! idempotency-key (.getMessage e))
            (throw e)))))))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn cleanup-expired!
  "Clean up expired request states."
  []
  (let [now (System/currentTimeMillis)
        expired-keys (filter (fn [[key state]]
                               (is-expired? state))
                             (:requests @state))]
    (doseq [[key state] expired-keys]
      ;; Remove request state
      (swap! state update :requests dissoc key)
      ;; Remove fingerprint mapping
      (swap! state update :fingerprints dissoc (:fingerprint state)))
    (count expired-keys)))

(defn enforce-max-entries!
  "Enforce maximum entries limit."
  []
  (let [max-entries (get-in @state [:config :max-entries])
        current-count (count (:requests @state))]
    (when (> current-count max-entries)
      (let [to-remove (- current-count max-entries)
            sorted-entries (sort-by (fn [[_ v]] (:created-at v))
                                    (:requests @state))
            entries-to-remove (take to-remove sorted-entries)]
        (doseq [[key state] entries-to-remove]
          (swap! state update :requests dissoc key)
          (swap! state update :fingerprints dissoc (:fingerprint state)))
        to-remove))))

(defn start-cleanup-task!
  "Start the cleanup background task."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :cleanup-interval-ms])))
    (let [expired (cleanup-expired!)
          evicted (enforce-max-entries!)]
      (when (or (pos? expired) (pos? evicted))
        (logging/log :debug "Deduplication cleanup" {:expired expired :evicted evicted})))
    (recur)))

;; ============================================================================
;; Manual Operations
;; ============================================================================

(defn invalidate-request!
  "Manually invalidate a request state."
  [idempotency-key]
  (when-let [state (get-request-state idempotency-key)]
    (swap! state update :requests dissoc idempotency-key)
    (swap! state update :fingerprints dissoc (:fingerprint state))
    (swap! state update :in-flight dissoc idempotency-key)
    true))

(defn clear-all!
  "Clear all request states."
  []
  (let [count (count (:requests @state))]
    (swap! state assoc :requests {})
    (swap! state assoc :fingerprints {})
    (swap! state assoc :in-flight {})
    (logging/log :warn "Cleared all deduplication state" {:count count})
    count))

(defn extend-ttl!
  "Extend TTL for a request."
  [idempotency-key additional-ms]
  (swap! state update-in [:requests idempotency-key :ttl-ms] + additional-ms))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-deduplication-stats
  "Get deduplication statistics."
  []
  (let [stats (:stats @state)
        total (:total-requests stats)]
    {:total-entries (count (:requests @state))
     :in-flight-count (count (:in-flight @state))
     :total-requests total
     :duplicates-detected (:duplicates-detected stats)
     :in-flight-hits (:in-flight-hits stats)
     :duplicate-rate (if (pos? total)
                       (/ (:duplicates-detected stats) total)
                       0)}))

(defn list-recent-requests
  "List recent request states."
  [& {:keys [limit status] :or {limit 100}}]
  (let [requests (vals (:requests @state))
        filtered (cond->> requests
                   status (filter #(= (:status %) status))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:idempotency-key :status :created-at :completed-at]) filtered)))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-deduplication!
  "Initialize the request deduplication system."
  []
  (when-not (:initialized? @state)
    ;; Start cleanup task
    (start-cleanup-task!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request deduplication initialized")
    (events/emit! :request-deduplication-initialized {})
    true))
