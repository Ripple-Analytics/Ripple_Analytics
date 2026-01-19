(ns mental-models.pipeline.integration.connection-pool
  "Connection Pool Module
   
   Connection management:
   - Pool creation and configuration
   - Connection lifecycle
   - Health checking
   - Auto-scaling
   - Connection reuse"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent LinkedBlockingQueue TimeUnit Executors ScheduledExecutorService]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]))

;; =============================================================================
;; CONNECTION POOL STATE
;; =============================================================================

(defonce pool-state (atom {:pools {}
                           :scheduler nil
                           :config {:default-min-size 2
                                    :default-max-size 10
                                    :default-acquire-timeout-ms 5000
                                    :default-idle-timeout-ms 300000
                                    :default-max-lifetime-ms 1800000
                                    :health-check-interval-ms 30000}}))

;; =============================================================================
;; CONNECTION WRAPPER
;; =============================================================================

(defn create-connection-wrapper
  "Create a wrapper for a connection."
  [connection pool-id]
  {:connection connection
   :pool-id pool-id
   :created-at (System/currentTimeMillis)
   :last-used-at (AtomicLong. (System/currentTimeMillis))
   :use-count (AtomicLong. 0)
   :healthy? (AtomicBoolean. true)})

(defn mark-used!
  "Mark a connection as used."
  [wrapper]
  (.set ^AtomicLong (:last-used-at wrapper) (System/currentTimeMillis))
  (.incrementAndGet ^AtomicLong (:use-count wrapper)))

(defn mark-unhealthy!
  "Mark a connection as unhealthy."
  [wrapper]
  (.set ^AtomicBoolean (:healthy? wrapper) false))

(defn is-healthy?
  "Check if a connection is healthy."
  [wrapper]
  (.get ^AtomicBoolean (:healthy? wrapper)))

(defn is-expired?
  "Check if a connection has exceeded its max lifetime."
  [wrapper max-lifetime-ms]
  (> (- (System/currentTimeMillis) (:created-at wrapper)) max-lifetime-ms))

(defn is-idle?
  "Check if a connection has been idle too long."
  [wrapper idle-timeout-ms]
  (> (- (System/currentTimeMillis) (.get ^AtomicLong (:last-used-at wrapper))) idle-timeout-ms))

;; =============================================================================
;; POOL CREATION
;; =============================================================================

(defn create-pool
  "Create a new connection pool."
  [pool-id {:keys [min-size max-size acquire-timeout-ms idle-timeout-ms
                   max-lifetime-ms create-fn destroy-fn validate-fn]}]
  (let [config (:config @pool-state)]
    {:id pool-id
     :min-size (or min-size (:default-min-size config))
     :max-size (or max-size (:default-max-size config))
     :acquire-timeout-ms (or acquire-timeout-ms (:default-acquire-timeout-ms config))
     :idle-timeout-ms (or idle-timeout-ms (:default-idle-timeout-ms config))
     :max-lifetime-ms (or max-lifetime-ms (:default-max-lifetime-ms config))
     :create-fn create-fn
     :destroy-fn (or destroy-fn (fn [_] nil))
     :validate-fn (or validate-fn (fn [_] true))
     :available (LinkedBlockingQueue.)
     :in-use (atom #{})
     :total-created (AtomicLong. 0)
     :total-destroyed (AtomicLong. 0)
     :created-at (System/currentTimeMillis)
     :closed? (AtomicBoolean. false)}))

;; =============================================================================
;; POOL REGISTRATION
;; =============================================================================

(defn register-pool!
  "Register a connection pool."
  [pool-id opts]
  (log/info "Registering connection pool" {:id pool-id})
  (let [pool (create-pool pool-id opts)]
    (swap! pool-state assoc-in [:pools pool-id] pool)
    (metrics/inc-counter! :connectionpool/pools-registered)
    ;; Initialize minimum connections
    (dotimes [_ (:min-size pool)]
      (try
        (let [conn ((:create-fn pool))
              wrapper (create-connection-wrapper conn pool-id)]
          (.offer ^LinkedBlockingQueue (:available pool) wrapper)
          (.incrementAndGet ^AtomicLong (:total-created pool)))
        (catch Exception e
          (log/error "Failed to create initial connection" {:pool pool-id :error (.getMessage e)}))))
    pool-id))

(defn unregister-pool!
  "Unregister and close a connection pool."
  [pool-id]
  (when-let [pool (get-in @pool-state [:pools pool-id])]
    (log/info "Unregistering connection pool" {:id pool-id})
    (.set ^AtomicBoolean (:closed? pool) true)
    ;; Destroy all available connections
    (while-let [wrapper (.poll ^LinkedBlockingQueue (:available pool))]
      (try
        ((:destroy-fn pool) (:connection wrapper))
        (.incrementAndGet ^AtomicLong (:total-destroyed pool))
        (catch Exception _)))
    ;; Destroy all in-use connections
    (doseq [wrapper @(:in-use pool)]
      (try
        ((:destroy-fn pool) (:connection wrapper))
        (.incrementAndGet ^AtomicLong (:total-destroyed pool))
        (catch Exception _)))
    (swap! pool-state update :pools dissoc pool-id)))

(defn get-pool
  "Get a connection pool."
  [pool-id]
  (get-in @pool-state [:pools pool-id]))

(defn list-pools
  "List all connection pools."
  []
  (keys (:pools @pool-state)))

;; =============================================================================
;; CONNECTION ACQUISITION
;; =============================================================================

(defn try-create-connection
  "Try to create a new connection if pool isn't at max size."
  [pool]
  (let [current-size (+ (.size ^LinkedBlockingQueue (:available pool))
                        (count @(:in-use pool)))]
    (when (< current-size (:max-size pool))
      (try
        (let [conn ((:create-fn pool))
              wrapper (create-connection-wrapper conn (:id pool))]
          (.incrementAndGet ^AtomicLong (:total-created pool))
          wrapper)
        (catch Exception e
          (log/error "Failed to create connection" {:pool (:id pool) :error (.getMessage e)})
          nil)))))

(defn acquire!
  "Acquire a connection from the pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (when (.get ^AtomicBoolean (:closed? pool))
      (throw (ex-info "Pool is closed" {:pool-id pool-id})))
    (let [timeout (:acquire-timeout-ms pool)
          start-time (System/currentTimeMillis)]
      (loop []
        (let [elapsed (- (System/currentTimeMillis) start-time)]
          (when (>= elapsed timeout)
            (metrics/inc-counter! :connectionpool/acquire-timeouts)
            (throw (ex-info "Connection acquire timeout" {:pool-id pool-id})))
          ;; Try to get from available
          (if-let [wrapper (.poll ^LinkedBlockingQueue (:available pool) 100 TimeUnit/MILLISECONDS)]
            (if (and (is-healthy? wrapper)
                     (not (is-expired? wrapper (:max-lifetime-ms pool)))
                     ((:validate-fn pool) (:connection wrapper)))
              (do
                (mark-used! wrapper)
                (swap! (:in-use pool) conj wrapper)
                (metrics/inc-counter! :connectionpool/connections-acquired)
                wrapper)
              (do
                ;; Connection is invalid, destroy and try again
                (try
                  ((:destroy-fn pool) (:connection wrapper))
                  (.incrementAndGet ^AtomicLong (:total-destroyed pool))
                  (catch Exception _))
                (recur)))
            ;; No available connection, try to create one
            (if-let [wrapper (try-create-connection pool)]
              (do
                (mark-used! wrapper)
                (swap! (:in-use pool) conj wrapper)
                (metrics/inc-counter! :connectionpool/connections-acquired)
                wrapper)
              (recur))))))))

(defn release!
  "Release a connection back to the pool."
  [wrapper]
  (when-let [pool (get-pool (:pool-id wrapper))]
    (swap! (:in-use pool) disj wrapper)
    (if (and (is-healthy? wrapper)
             (not (is-expired? wrapper (:max-lifetime-ms pool)))
             (not (.get ^AtomicBoolean (:closed? pool))))
      (do
        (.offer ^LinkedBlockingQueue (:available pool) wrapper)
        (metrics/inc-counter! :connectionpool/connections-released))
      (do
        ;; Connection is invalid, destroy it
        (try
          ((:destroy-fn pool) (:connection wrapper))
          (.incrementAndGet ^AtomicLong (:total-destroyed pool))
          (catch Exception _))))))

;; =============================================================================
;; CONNECTION USAGE
;; =============================================================================

(defn with-connection
  "Execute a function with a pooled connection."
  [pool-id f]
  (let [wrapper (acquire! pool-id)]
    (try
      (f (:connection wrapper))
      (catch Exception e
        (mark-unhealthy! wrapper)
        (throw e))
      (finally
        (release! wrapper)))))

(defmacro with-pooled-connection
  "Execute body with a pooled connection bound to conn-sym."
  [[conn-sym pool-id] & body]
  `(with-connection ~pool-id (fn [~conn-sym] ~@body)))

;; =============================================================================
;; POOL MAINTENANCE
;; =============================================================================

(defn cleanup-idle-connections!
  "Remove idle connections from the pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [idle-timeout (:idle-timeout-ms pool)
          available (:available pool)
          to-remove (atom [])]
      ;; Find idle connections
      (doseq [wrapper (seq available)]
        (when (and (is-idle? wrapper idle-timeout)
                   (> (+ (.size available) (count @(:in-use pool))) (:min-size pool)))
          (swap! to-remove conj wrapper)))
      ;; Remove and destroy idle connections
      (doseq [wrapper @to-remove]
        (when (.remove available wrapper)
          (try
            ((:destroy-fn pool) (:connection wrapper))
            (.incrementAndGet ^AtomicLong (:total-destroyed pool))
            (metrics/inc-counter! :connectionpool/idle-connections-removed)
            (catch Exception _)))))))

(defn cleanup-expired-connections!
  "Remove expired connections from the pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [max-lifetime (:max-lifetime-ms pool)
          available (:available pool)
          to-remove (atom [])]
      ;; Find expired connections
      (doseq [wrapper (seq available)]
        (when (is-expired? wrapper max-lifetime)
          (swap! to-remove conj wrapper)))
      ;; Remove and destroy expired connections
      (doseq [wrapper @to-remove]
        (when (.remove available wrapper)
          (try
            ((:destroy-fn pool) (:connection wrapper))
            (.incrementAndGet ^AtomicLong (:total-destroyed pool))
            (metrics/inc-counter! :connectionpool/expired-connections-removed)
            (catch Exception _)))))))

(defn ensure-min-connections!
  "Ensure pool has minimum connections."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [current-size (+ (.size ^LinkedBlockingQueue (:available pool))
                          (count @(:in-use pool)))
          needed (- (:min-size pool) current-size)]
      (when (pos? needed)
        (dotimes [_ needed]
          (try
            (let [conn ((:create-fn pool))
                  wrapper (create-connection-wrapper conn pool-id)]
              (.offer ^LinkedBlockingQueue (:available pool) wrapper)
              (.incrementAndGet ^AtomicLong (:total-created pool)))
            (catch Exception e
              (log/error "Failed to create connection" {:pool pool-id :error (.getMessage e)}))))))))

(defn maintain-pool!
  "Perform maintenance on a pool."
  [pool-id]
  (cleanup-idle-connections! pool-id)
  (cleanup-expired-connections! pool-id)
  (ensure-min-connections! pool-id))

;; =============================================================================
;; HEALTH CHECKING
;; =============================================================================

(defn check-pool-health
  "Check the health of a connection pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [available-count (.size ^LinkedBlockingQueue (:available pool))
          in-use-count (count @(:in-use pool))
          total-count (+ available-count in-use-count)]
      {:id pool-id
       :healthy? (and (not (.get ^AtomicBoolean (:closed? pool)))
                      (>= total-count (:min-size pool)))
       :available available-count
       :in-use in-use-count
       :total total-count
       :min-size (:min-size pool)
       :max-size (:max-size pool)
       :total-created (.get ^AtomicLong (:total-created pool))
       :total-destroyed (.get ^AtomicLong (:total-destroyed pool))
       :utilization (if (pos? (:max-size pool))
                      (* 100.0 (/ in-use-count (:max-size pool)))
                      0.0)})))

(defn check-all-pools-health
  "Check the health of all connection pools."
  []
  (into {} (for [pool-id (list-pools)]
             [pool-id (check-pool-health pool-id)])))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-maintenance-scheduler!
  "Start the pool maintenance scheduler."
  []
  (when (and (flags/is-enabled? "connection-pool")
             (nil? (:scheduler @pool-state)))
    (log/info "Starting connection pool maintenance scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @pool-state [:config :health-check-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (doseq [pool-id (list-pools)]
                                 (maintain-pool! pool-id))
                               (catch Exception e
                                 (log/error "Pool maintenance error" {:error (.getMessage e)})))
                            interval
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! pool-state assoc :scheduler executor))))

(defn stop-maintenance-scheduler!
  "Stop the pool maintenance scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @pool-state)]
    (log/info "Stopping connection pool maintenance scheduler")
    (.shutdown executor)
    (swap! pool-state assoc :scheduler nil)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-connection-pool!
  "Initialize connection pool."
  []
  (log/info "Initializing connection pool")
  ;; Register feature flag
  (flags/register-flag! "connection-pool" "Enable connection pool" true)
  ;; Create metrics
  (metrics/create-counter! :connectionpool/pools-registered "Pools registered")
  (metrics/create-counter! :connectionpool/connections-acquired "Connections acquired")
  (metrics/create-counter! :connectionpool/connections-released "Connections released")
  (metrics/create-counter! :connectionpool/acquire-timeouts "Acquire timeouts")
  (metrics/create-counter! :connectionpool/idle-connections-removed "Idle connections removed")
  (metrics/create-counter! :connectionpool/expired-connections-removed "Expired connections removed")
  (metrics/create-gauge! :connectionpool/total-pools "Total pools"
                         #(count (:pools @pool-state)))
  (log/info "Connection pool initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-pool-status []
  {:enabled (flags/is-enabled? "connection-pool")
   :pools (count (:pools @pool-state))
   :scheduler-active (some? (:scheduler @pool-state))
   :pool-health (check-all-pools-health)
   :config (:config @pool-state)})
