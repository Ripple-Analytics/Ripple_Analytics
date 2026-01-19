(ns mental-models.pipeline.integration.idempotency-handler
  "Idempotency handler for mental model analysis system.
   
   Features:
   - Idempotency key management
   - Request deduplication
   - Response caching
   - TTL-based expiration
   - Distributed idempotency
   - Conflict detection
   - Idempotency metrics
   - Ring middleware"
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
           [java.security MessageDigest]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:keys (ConcurrentHashMap.)  ;; idempotency-key -> entry
         :config {:default-ttl-ms 86400000  ;; 24 hours
                  :max-entries 100000
                  :cleanup-interval-ms 60000
                  :key-header "Idempotency-Key"}
         :stats {:requests-received 0
                 :cache-hits 0
                 :cache-misses 0
                 :conflicts 0
                 :expirations 0}
         :initialized? false}))

;; ============================================================================
;; Key Generation
;; ============================================================================

(defn generate-key
  "Generate an idempotency key."
  []
  (str (UUID/randomUUID)))

(defn- hash-request
  "Generate a hash for a request."
  [request]
  (let [digest (MessageDigest/getInstance "SHA-256")
        content (str (:request-method request)
                     (:uri request)
                     (pr-str (:body request)))
        bytes (.digest digest (.getBytes content "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

(defn derive-key
  "Derive an idempotency key from request content."
  [request]
  (hash-request request))

;; ============================================================================
;; Entry Management
;; ============================================================================

(defrecord IdempotencyEntry [key status response created-at expires-at locked?])

(defn- create-entry
  "Create a new idempotency entry."
  [key ttl-ms]
  (let [now (System/currentTimeMillis)]
    (->IdempotencyEntry
     key
     :processing
     nil
     now
     (+ now ttl-ms)
     true)))

(defn- is-expired?
  "Check if an entry is expired."
  [entry]
  (> (System/currentTimeMillis) (:expires-at entry)))

(defn- get-entry
  "Get an idempotency entry."
  [key]
  (let [keys-map (:keys @state)]
    (.get keys-map key)))

(defn- put-entry!
  "Put an idempotency entry."
  [key entry]
  (let [keys-map (:keys @state)]
    (.put keys-map key entry)))

(defn- remove-entry!
  "Remove an idempotency entry."
  [key]
  (let [keys-map (:keys @state)]
    (.remove keys-map key)))

;; ============================================================================
;; Idempotency Operations
;; ============================================================================

(defn acquire-lock
  "Try to acquire an idempotency lock."
  [key & {:keys [ttl-ms]}]
  (swap! state update-in [:stats :requests-received] inc)
  
  (let [effective-ttl (or ttl-ms (get-in @state [:config :default-ttl-ms]))
        existing (get-entry key)]
    (cond
      ;; No existing entry - create new
      (nil? existing)
      (do
        (put-entry! key (create-entry key effective-ttl))
        (swap! state update-in [:stats :cache-misses] inc)
        {:acquired? true :new? true})
      
      ;; Existing entry is expired - replace
      (is-expired? existing)
      (do
        (put-entry! key (create-entry key effective-ttl))
        (swap! state update-in [:stats :expirations] inc)
        {:acquired? true :new? true :expired-previous? true})
      
      ;; Existing entry is still processing
      (:locked? existing)
      (do
        (swap! state update-in [:stats :conflicts] inc)
        {:acquired? false :reason :in-progress})
      
      ;; Existing entry has completed - return cached response
      :else
      (do
        (swap! state update-in [:stats :cache-hits] inc)
        {:acquired? false :reason :already-processed :response (:response existing)}))))

(defn complete-request!
  "Complete an idempotent request with a response."
  [key response]
  (when-let [entry (get-entry key)]
    (put-entry! key (assoc entry
                           :status :completed
                           :response response
                           :locked? false))
    (logging/log :debug "Completed idempotent request" {:key key})
    true))

(defn fail-request!
  "Mark an idempotent request as failed."
  [key & {:keys [error]}]
  (when-let [entry (get-entry key)]
    (put-entry! key (assoc entry
                           :status :failed
                           :error error
                           :locked? false))
    (logging/log :debug "Failed idempotent request" {:key key :error error})
    true))

(defn release-lock!
  "Release an idempotency lock without storing a response."
  [key]
  (remove-entry! key)
  (logging/log :debug "Released idempotency lock" {:key key}))

;; ============================================================================
;; Idempotent Execution
;; ============================================================================

(defn execute-idempotent
  "Execute a function idempotently."
  [key f & {:keys [ttl-ms on-duplicate]}]
  (let [lock-result (acquire-lock key :ttl-ms ttl-ms)]
    (if (:acquired? lock-result)
      (try
        (let [response (f)]
          (complete-request! key response)
          {:status :executed :response response})
        (catch Exception e
          (fail-request! key :error (.getMessage e))
          (throw e)))
      (if-let [cached-response (:response lock-result)]
        (do
          (when on-duplicate
            (on-duplicate cached-response))
          {:status :duplicate :response cached-response})
        {:status :conflict :reason (:reason lock-result)}))))

(defmacro with-idempotency
  "Execute body idempotently."
  [key & body]
  `(execute-idempotent ~key (fn [] ~@body)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn idempotency-middleware
  "Ring middleware for idempotency."
  [handler & {:keys [key-header methods ttl-ms]}]
  (let [header (or key-header (get-in @state [:config :key-header]))
        idempotent-methods (or methods #{:post :put :patch})]
    (fn [request]
      (if (contains? idempotent-methods (:request-method request))
        (if-let [key (get-in request [:headers (str/lower-case header)])]
          (let [lock-result (acquire-lock key :ttl-ms ttl-ms)]
            (if (:acquired? lock-result)
              (try
                (let [response (handler request)]
                  (complete-request! key response)
                  (assoc-in response [:headers "Idempotency-Key"] key))
                (catch Exception e
                  (fail-request! key :error (.getMessage e))
                  (throw e)))
              (if-let [cached-response (:response lock-result)]
                (assoc-in cached-response [:headers "Idempotency-Key"] key)
                {:status 409
                 :headers {"Content-Type" "application/json"}
                 :body {:error "Conflict"
                        :message "Request with this idempotency key is already being processed"}})))
          ;; No idempotency key provided
          (handler request))
        ;; Non-idempotent method
        (handler request)))))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn- cleanup-expired!
  "Remove expired entries."
  []
  (let [keys-map (:keys @state)
        now (System/currentTimeMillis)
        expired-keys (atom [])]
    (doseq [[k v] keys-map]
      (when (and (not (:locked? v)) (> now (:expires-at v)))
        (swap! expired-keys conj k)))
    (doseq [k @expired-keys]
      (remove-entry! k))
    (let [count (count @expired-keys)]
      (when (pos? count)
        (logging/log :debug "Cleaned up expired idempotency entries" {:count count})))))

(defn- start-cleanup-task!
  "Start the background cleanup task."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :cleanup-interval-ms])))
    (cleanup-expired!)
    (recur)))

;; ============================================================================
;; Entry Queries
;; ============================================================================

(defn get-entry-status
  "Get the status of an idempotency entry."
  [key]
  (when-let [entry (get-entry key)]
    {:key key
     :status (:status entry)
     :created-at (:created-at entry)
     :expires-at (:expires-at entry)
     :locked? (:locked? entry)
     :expired? (is-expired? entry)}))

(defn list-entries
  "List idempotency entries."
  [& {:keys [status limit] :or {limit 100}}]
  (let [keys-map (:keys @state)
        entries (map (fn [[k v]]
                       {:key k
                        :status (:status v)
                        :created-at (:created-at v)
                        :locked? (:locked? v)})
                     keys-map)
        filtered (cond->> entries
                   status (filter #(= (:status %) status))
                   limit (take limit))]
    (vec filtered)))

(defn count-entries
  "Count idempotency entries."
  []
  (.size (:keys @state)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-ttl!
  "Set the default TTL for idempotency entries."
  [ttl-ms]
  (swap! state assoc-in [:config :default-ttl-ms] ttl-ms))

(defn set-max-entries!
  "Set the maximum number of entries."
  [max-entries]
  (swap! state assoc-in [:config :max-entries] max-entries))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-idempotency-stats
  "Get idempotency statistics."
  []
  (let [stats (:stats @state)
        received (:requests-received stats)]
    {:entries-count (count-entries)
     :requests-received received
     :cache-hits (:cache-hits stats)
     :cache-misses (:cache-misses stats)
     :conflicts (:conflicts stats)
     :expirations (:expirations stats)
     :hit-rate (if (pos? received)
                 (/ (:cache-hits stats) received)
                 0)}))

(defn reset-stats!
  "Reset idempotency statistics."
  []
  (swap! state assoc :stats {:requests-received 0
                             :cache-hits 0
                             :cache-misses 0
                             :conflicts 0
                             :expirations 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-idempotency-handler!
  "Initialize the idempotency handler."
  []
  (when-not (:initialized? @state)
    ;; Start cleanup task
    (start-cleanup-task!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Idempotency handler initialized")
    (events/emit! :idempotency-handler-initialized {})
    true))
