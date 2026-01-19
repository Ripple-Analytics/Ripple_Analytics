(ns mental-models.pipeline.integration.request-deduplicator
  "Request deduplicator for mental model analysis system.
   
   Features:
   - Request deduplication
   - Duplicate detection
   - Fingerprinting strategies
   - Deduplication windows
   - Response caching
   - Duplicate handling
   - Deduplication metrics
   - Cleanup management"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.security MessageDigest]
           [java.util.concurrent ConcurrentHashMap]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:fingerprints (ConcurrentHashMap.)  ;; fingerprint -> entry
         :strategies {}       ;; strategy-id -> fingerprint strategy
         :config {:default-strategy :content-hash
                  :window-ms 60000
                  :max-entries 100000
                  :return-cached? true
                  :cleanup-interval-ms 60000}
         :stats {:requests-checked (AtomicLong. 0)
                 :duplicates-found (AtomicLong. 0)
                 :cached-responses (AtomicLong. 0)
                 :cleanups (AtomicLong. 0)}
         :cleanup-running? (atom false)
         :initialized? false}))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn- sha256
  "Generate SHA-256 hash."
  [s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) hash-bytes))))

;; ============================================================================
;; Fingerprinting Strategies
;; ============================================================================

(def built-in-strategies
  {:content-hash
   (fn [request]
     (sha256 (str (:request-method request)
                  (:uri request)
                  (:query-string request)
                  (:body request))))
   
   :url-only
   (fn [request]
     (sha256 (str (:request-method request)
                  (:uri request)
                  (:query-string request))))
   
   :body-only
   (fn [request]
     (sha256 (str (:body request))))
   
   :idempotency-key
   (fn [request]
     (or (get-in request [:headers "idempotency-key"])
         (get-in request [:headers "x-idempotency-key"])
         (sha256 (str (:request-method request)
                      (:uri request)
                      (:body request)))))
   
   :user-action
   (fn [request]
     (sha256 (str (get-in request [:auth :user-id])
                  (:request-method request)
                  (:uri request)
                  (:body request))))})

(defn register-strategy!
  "Register a custom fingerprinting strategy."
  [strategy-id fingerprint-fn]
  (swap! state assoc-in [:strategies strategy-id]
         {:id strategy-id
          :fingerprint-fn fingerprint-fn
          :created-at (System/currentTimeMillis)}))

(defn get-strategy
  "Get a fingerprinting strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id])
      (when-let [built-in (get built-in-strategies strategy-id)]
        {:id strategy-id :fingerprint-fn built-in})))

(defn list-strategies
  "List all strategies."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-strategies)
   (mapv (fn [[id s]] {:id id :type :custom :created-at (:created-at s)})
         (:strategies @state))))

;; ============================================================================
;; Fingerprint Generation
;; ============================================================================

(defn generate-fingerprint
  "Generate a fingerprint for a request."
  [request & {:keys [strategy] :or {strategy :content-hash}}]
  (let [strategy-config (get-strategy strategy)
        fingerprint-fn (or (:fingerprint-fn strategy-config)
                           (:fingerprint-fn (get-strategy :content-hash)))]
    (fingerprint-fn request)))

;; ============================================================================
;; Deduplication
;; ============================================================================

(defn- get-fingerprints
  "Get the fingerprints map."
  []
  (:fingerprints @state))

(defn check-duplicate
  "Check if a request is a duplicate."
  [request & {:keys [strategy] :or {strategy :content-hash}}]
  (.incrementAndGet (:requests-checked (:stats @state)))
  
  (let [fingerprint (generate-fingerprint request :strategy strategy)
        fingerprints (get-fingerprints)
        entry (.get fingerprints fingerprint)
        window-ms (get-in @state [:config :window-ms])
        now (System/currentTimeMillis)]
    
    (if (and entry
             (< (- now (:timestamp entry)) window-ms))
      (do
        (.incrementAndGet (:duplicates-found (:stats @state)))
        {:duplicate? true
         :fingerprint fingerprint
         :original-timestamp (:timestamp entry)
         :cached-response (:response entry)})
      {:duplicate? false
       :fingerprint fingerprint})))

(defn record-request!
  "Record a request fingerprint."
  [fingerprint & {:keys [response]}]
  (let [fingerprints (get-fingerprints)
        entry {:fingerprint fingerprint
               :timestamp (System/currentTimeMillis)
               :response response}]
    (.put fingerprints fingerprint entry)
    
    (when response
      (.incrementAndGet (:cached-responses (:stats @state))))
    
    fingerprint))

(defn remove-fingerprint!
  "Remove a fingerprint."
  [fingerprint]
  (.remove (get-fingerprints) fingerprint))

(defn clear-fingerprints!
  "Clear all fingerprints."
  []
  (.clear (get-fingerprints)))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn cleanup-expired!
  "Clean up expired fingerprints."
  []
  (when (compare-and-set! (:cleanup-running? @state) false true)
    (try
      (.incrementAndGet (:cleanups (:stats @state)))
      
      (let [fingerprints (get-fingerprints)
            window-ms (get-in @state [:config :window-ms])
            now (System/currentTimeMillis)
            expired (filter (fn [[_ entry]]
                              (>= (- now (:timestamp entry)) window-ms))
                            fingerprints)]
        (doseq [[fp _] expired]
          (.remove fingerprints fp))
        
        (logging/log :debug "Cleaned up expired fingerprints" {:count (count expired)})
        (count expired))
      (finally
        (reset! (:cleanup-running? @state) false)))))

(defn start-cleanup-task!
  "Start the background cleanup task."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :cleanup-interval-ms])))
    (cleanup-expired!)
    (recur)))

;; ============================================================================
;; Deduplication with Response
;; ============================================================================

(defn deduplicate
  "Deduplicate a request and optionally return cached response."
  [request handler & {:keys [strategy] :or {strategy :content-hash}}]
  (let [check-result (check-duplicate request :strategy strategy)]
    (if (:duplicate? check-result)
      (if (and (get-in @state [:config :return-cached?])
               (:cached-response check-result))
        (assoc (:cached-response check-result)
               :headers (assoc (get-in check-result [:cached-response :headers] {})
                               "X-Deduplicated" "true"
                               "X-Original-Timestamp" (str (:original-timestamp check-result))))
        {:status 409
         :headers {"Content-Type" "application/json"
                   "X-Deduplicated" "true"}
         :body {:error "Duplicate request"
                :fingerprint (:fingerprint check-result)
                :original-timestamp (:original-timestamp check-result)}})
      (let [response (handler request)]
        (record-request! (:fingerprint check-result) :response response)
        response))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-deduplicate
  "Ring middleware for request deduplication."
  [handler & {:keys [strategy] :or {strategy :content-hash}}]
  (fn [request]
    (deduplicate request handler :strategy strategy)))

(defn wrap-deduplicate-idempotent
  "Ring middleware for idempotent request deduplication."
  [handler]
  (fn [request]
    (if (#{:post :put :patch :delete} (:request-method request))
      (deduplicate request handler :strategy :idempotency-key)
      (handler request))))

(defn wrap-record-fingerprint
  "Ring middleware to record request fingerprints without blocking."
  [handler & {:keys [strategy] :or {strategy :content-hash}}]
  (fn [request]
    (let [fingerprint (generate-fingerprint request :strategy strategy)
          response (handler request)]
      (record-request! fingerprint :response response)
      response)))

(defn wrap-check-duplicate
  "Ring middleware to check for duplicates and add header."
  [handler & {:keys [strategy] :or {strategy :content-hash}}]
  (fn [request]
    (let [check-result (check-duplicate request :strategy strategy)
          response (handler (assoc request :duplicate-check check-result))]
      (if (:duplicate? check-result)
        (assoc-in response [:headers "X-Duplicate-Detected"] "true")
        response))))

;; ============================================================================
;; Idempotency Key Management
;; ============================================================================

(defn get-idempotency-key
  "Get idempotency key from request."
  [request]
  (or (get-in request [:headers "idempotency-key"])
      (get-in request [:headers "x-idempotency-key"])))

(defn has-idempotency-key?
  "Check if request has idempotency key."
  [request]
  (some? (get-idempotency-key request)))

(defn require-idempotency-key
  "Middleware to require idempotency key for certain methods."
  [handler & {:keys [methods] :or {methods #{:post :put :patch}}}]
  (fn [request]
    (if (and (contains? methods (:request-method request))
             (not (has-idempotency-key? request)))
      {:status 400
       :headers {"Content-Type" "application/json"}
       :body {:error "Idempotency key required"
              :header "Idempotency-Key"}}
      (handler request))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set default fingerprinting strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-window!
  "Set deduplication window."
  [window-ms]
  (swap! state assoc-in [:config :window-ms] window-ms))

(defn set-return-cached!
  "Enable/disable returning cached responses."
  [return?]
  (swap! state assoc-in [:config :return-cached?] return?))

(defn set-max-entries!
  "Set maximum fingerprint entries."
  [max-entries]
  (swap! state assoc-in [:config :max-entries] max-entries))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-deduplicator-metrics
  "Get deduplicator metrics."
  []
  (let [stats (:stats @state)]
    {:requests-checked (.get (:requests-checked stats))
     :duplicates-found (.get (:duplicates-found stats))
     :cached-responses (.get (:cached-responses stats))
     :cleanups (.get (:cleanups stats))
     :fingerprints-count (.size (get-fingerprints))
     :duplicate-rate (let [checked (.get (:requests-checked stats))]
                       (if (pos? checked)
                         (/ (.get (:duplicates-found stats)) checked)
                         0.0))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-deduplicator-stats
  "Get deduplicator statistics."
  []
  (merge (get-deduplicator-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :window-ms (get-in @state [:config :window-ms])
          :return-cached? (get-in @state [:config :return-cached?])
          :max-entries (get-in @state [:config :max-entries])}))

(defn reset-stats!
  "Reset deduplicator statistics."
  []
  (.set (:requests-checked (:stats @state)) 0)
  (.set (:duplicates-found (:stats @state)) 0)
  (.set (:cached-responses (:stats @state)) 0)
  (.set (:cleanups (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-deduplicator!
  "Initialize the request deduplicator."
  []
  (when-not (:initialized? @state)
    (start-cleanup-task!)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request deduplicator initialized")
    (events/emit! :request-deduplicator-initialized {})
    true))
