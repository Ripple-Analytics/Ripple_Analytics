(ns mental-models.pipeline.integration.connection-pool-manager
  "Connection pool manager for mental model analysis system.
   
   Features:
   - Multi-pool management
   - Connection lifecycle
   - Health checking
   - Pool sizing
   - Connection validation
   - Leak detection
   - Pool metrics
   - Graceful shutdown"
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
           [java.util.concurrent Semaphore TimeUnit LinkedBlockingQueue]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:pools {}            ;; pool-id -> pool
         :config {:default-min-size 2
                  :default-max-size 10
                  :default-idle-timeout-ms 300000
                  :default-max-lifetime-ms 1800000
                  :default-connection-timeout-ms 30000
                  :validation-interval-ms 30000
                  :leak-detection-threshold-ms 60000}
         :stats {:connections-created 0
                 :connections-destroyed 0
                 :connections-borrowed 0
                 :connections-returned 0
                 :validation-failures 0
                 :leaks-detected 0}
         :initialized? false}))

;; ============================================================================
;; Connection Wrapper
;; ============================================================================

(defn- create-connection-wrapper
  "Create a wrapper for a connection."
  [connection pool-id]
  {:id (str (UUID/randomUUID))
   :pool-id pool-id
   :connection connection
   :created-at (System/currentTimeMillis)
   :last-used-at (atom (System/currentTimeMillis))
   :last-validated-at (atom (System/currentTimeMillis))
   :borrowed? (atom false)
   :borrowed-at (atom nil)
   :borrowed-by (atom nil)})

(defn- update-last-used!
  "Update last used timestamp."
  [wrapper]
  (reset! (:last-used-at wrapper) (System/currentTimeMillis)))

(defn- mark-borrowed!
  "Mark connection as borrowed."
  [wrapper thread-name]
  (reset! (:borrowed? wrapper) true)
  (reset! (:borrowed-at wrapper) (System/currentTimeMillis))
  (reset! (:borrowed-by wrapper) thread-name))

(defn- mark-returned!
  "Mark connection as returned."
  [wrapper]
  (reset! (:borrowed? wrapper) false)
  (reset! (:borrowed-at wrapper) nil)
  (reset! (:borrowed-by wrapper) nil)
  (update-last-used! wrapper))

;; ============================================================================
;; Pool Creation
;; ============================================================================

(defn create-pool!
  "Create a new connection pool."
  [pool-id config]
  (let [min-size (get config :min-size (get-in @state [:config :default-min-size]))
        max-size (get config :max-size (get-in @state [:config :default-max-size]))
        pool {:id pool-id
              :name (get config :name (name pool-id))
              :create-fn (get config :create-fn)
              :destroy-fn (get config :destroy-fn (fn [_]))
              :validate-fn (get config :validate-fn (constantly true))
              :min-size min-size
              :max-size max-size
              :idle-timeout-ms (get config :idle-timeout-ms
                                    (get-in @state [:config :default-idle-timeout-ms]))
              :max-lifetime-ms (get config :max-lifetime-ms
                                    (get-in @state [:config :default-max-lifetime-ms]))
              :connection-timeout-ms (get config :connection-timeout-ms
                                          (get-in @state [:config :default-connection-timeout-ms]))
              :connections (atom [])
              :available (LinkedBlockingQueue. max-size)
              :semaphore (Semaphore. max-size)
              :created-at (System/currentTimeMillis)
              :enabled? (atom true)}]
    
    ;; Initialize minimum connections
    (dotimes [_ min-size]
      (try
        (let [conn ((:create-fn pool))
              wrapper (create-connection-wrapper conn pool-id)]
          (swap! (:connections pool) conj wrapper)
          (.offer (:available pool) wrapper)
          (swap! state update-in [:stats :connections-created] inc))
        (catch Exception e
          (logging/log :error "Failed to create initial connection" {:pool-id pool-id :error (.getMessage e)}))))
    
    (swap! state assoc-in [:pools pool-id] pool)
    (logging/log :info "Created connection pool" {:pool-id pool-id :min-size min-size :max-size max-size})
    (events/emit! :pool-created {:pool-id pool-id})
    pool-id))

(defn get-pool
  "Get a connection pool."
  [pool-id]
  (get-in @state [:pools pool-id]))

(defn list-pools
  "List all connection pools."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :min-size (:min-size p)
           :max-size (:max-size p)
           :current-size (count @(:connections p))
           :available-count (.size (:available p))
           :enabled? @(:enabled? p)})
        (:pools @state)))

;; ============================================================================
;; Connection Acquisition
;; ============================================================================

(defn- create-new-connection
  "Create a new connection for the pool."
  [pool]
  (try
    (let [conn ((:create-fn pool))
          wrapper (create-connection-wrapper conn (:id pool))]
      (swap! (:connections pool) conj wrapper)
      (swap! state update-in [:stats :connections-created] inc)
      wrapper)
    (catch Exception e
      (logging/log :error "Failed to create connection" {:pool-id (:id pool) :error (.getMessage e)})
      nil)))

(defn- validate-connection
  "Validate a connection."
  [pool wrapper]
  (try
    (let [valid? ((:validate-fn pool) (:connection wrapper))]
      (reset! (:last-validated-at wrapper) (System/currentTimeMillis))
      valid?)
    (catch Exception e
      (swap! state update-in [:stats :validation-failures] inc)
      false)))

(defn- is-connection-expired?
  "Check if connection has exceeded max lifetime."
  [pool wrapper]
  (let [age (- (System/currentTimeMillis) (:created-at wrapper))]
    (> age (:max-lifetime-ms pool))))

(defn- is-connection-idle-too-long?
  "Check if connection has been idle too long."
  [pool wrapper]
  (let [idle-time (- (System/currentTimeMillis) @(:last-used-at wrapper))]
    (> idle-time (:idle-timeout-ms pool))))

(defn borrow-connection
  "Borrow a connection from the pool."
  [pool-id & {:keys [timeout-ms]}]
  (when-let [pool (get-pool pool-id)]
    (when @(:enabled? pool)
      (let [timeout (or timeout-ms (:connection-timeout-ms pool))
            acquired? (.tryAcquire (:semaphore pool) timeout TimeUnit/MILLISECONDS)]
        (if-not acquired?
          (do
            (logging/log :warn "Connection acquisition timeout" {:pool-id pool-id})
            nil)
          (let [wrapper (or (.poll (:available pool) 0 TimeUnit/MILLISECONDS)
                            (create-new-connection pool))]
            (if (and wrapper
                     (not (is-connection-expired? pool wrapper))
                     (validate-connection pool wrapper))
              (do
                (mark-borrowed! wrapper (.getName (Thread/currentThread)))
                (swap! state update-in [:stats :connections-borrowed] inc)
                wrapper)
              (do
                ;; Connection invalid, try again
                (when wrapper
                  ((:destroy-fn pool) (:connection wrapper))
                  (swap! (:connections pool) (fn [conns] (remove #(= (:id %) (:id wrapper)) conns)))
                  (swap! state update-in [:stats :connections-destroyed] inc))
                (.release (:semaphore pool))
                (borrow-connection pool-id :timeout-ms timeout)))))))))

(defn return-connection
  "Return a connection to the pool."
  [pool-id wrapper]
  (when-let [pool (get-pool pool-id)]
    (mark-returned! wrapper)
    (.offer (:available pool) wrapper)
    (.release (:semaphore pool))
    (swap! state update-in [:stats :connections-returned] inc)))

(defmacro with-connection
  "Execute body with a borrowed connection."
  [pool-id conn-binding & body]
  `(let [wrapper# (borrow-connection ~pool-id)]
     (if wrapper#
       (let [~conn-binding (:connection wrapper#)]
         (try
           ~@body
           (finally
             (return-connection ~pool-id wrapper#))))
       (throw (ex-info "Failed to acquire connection" {:pool-id ~pool-id})))))

;; ============================================================================
;; Pool Maintenance
;; ============================================================================

(defn- evict-idle-connections!
  "Evict idle connections from pool."
  [pool]
  (let [connections @(:connections pool)
        current-size (count connections)
        min-size (:min-size pool)]
    (doseq [wrapper connections]
      (when (and (> current-size min-size)
                 (not @(:borrowed? wrapper))
                 (is-connection-idle-too-long? pool wrapper))
        (.remove (:available pool) wrapper)
        ((:destroy-fn pool) (:connection wrapper))
        (swap! (:connections pool) (fn [conns] (remove #(= (:id %) (:id wrapper)) conns)))
        (swap! state update-in [:stats :connections-destroyed] inc)
        (logging/log :debug "Evicted idle connection" {:pool-id (:id pool) :connection-id (:id wrapper)})))))

(defn- detect-leaks!
  "Detect connection leaks."
  [pool]
  (let [threshold (get-in @state [:config :leak-detection-threshold-ms])
        now (System/currentTimeMillis)]
    (doseq [wrapper @(:connections pool)]
      (when (and @(:borrowed? wrapper)
                 (> (- now @(:borrowed-at wrapper)) threshold))
        (swap! state update-in [:stats :leaks-detected] inc)
        (logging/log :warn "Potential connection leak detected"
                     {:pool-id (:id pool)
                      :connection-id (:id wrapper)
                      :borrowed-by @(:borrowed-by wrapper)
                      :borrowed-duration-ms (- now @(:borrowed-at wrapper))})
        (events/emit! :connection-leak-detected {:pool-id (:id pool)
                                                  :connection-id (:id wrapper)})))))

(defn- maintain-pool!
  "Perform pool maintenance."
  [pool]
  (evict-idle-connections! pool)
  (detect-leaks! pool)
  
  ;; Ensure minimum connections
  (let [current-size (count @(:connections pool))
        min-size (:min-size pool)]
    (when (< current-size min-size)
      (dotimes [_ (- min-size current-size)]
        (when-let [wrapper (create-new-connection pool)]
          (.offer (:available pool) wrapper))))))

(defn start-maintenance-task!
  "Start the pool maintenance background task."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :validation-interval-ms])))
    (doseq [[_ pool] (:pools @state)]
      (when @(:enabled? pool)
        (maintain-pool! pool)))
    (recur)))

;; ============================================================================
;; Pool Operations
;; ============================================================================

(defn resize-pool!
  "Resize a connection pool."
  [pool-id new-min-size new-max-size]
  (when-let [pool (get-pool pool-id)]
    (swap! state update-in [:pools pool-id]
           assoc :min-size new-min-size :max-size new-max-size)
    (logging/log :info "Resized pool" {:pool-id pool-id :min-size new-min-size :max-size new-max-size})))

(defn disable-pool!
  "Disable a connection pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (reset! (:enabled? pool) false)
    (logging/log :info "Disabled pool" {:pool-id pool-id})))

(defn enable-pool!
  "Enable a connection pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (reset! (:enabled? pool) true)
    (logging/log :info "Enabled pool" {:pool-id pool-id})))

(defn destroy-pool!
  "Destroy a connection pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (reset! (:enabled? pool) false)
    
    ;; Destroy all connections
    (doseq [wrapper @(:connections pool)]
      (try
        ((:destroy-fn pool) (:connection wrapper))
        (swap! state update-in [:stats :connections-destroyed] inc)
        (catch Exception _)))
    
    (swap! state update :pools dissoc pool-id)
    (logging/log :info "Destroyed pool" {:pool-id pool-id})
    (events/emit! :pool-destroyed {:pool-id pool-id})))

;; ============================================================================
;; Pool Statistics
;; ============================================================================

(defn get-pool-stats
  "Get statistics for a specific pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [connections @(:connections pool)
          borrowed (count (filter #(deref (:borrowed? %)) connections))]
      {:pool-id pool-id
       :name (:name pool)
       :total-connections (count connections)
       :available-connections (.size (:available pool))
       :borrowed-connections borrowed
       :min-size (:min-size pool)
       :max-size (:max-size pool)
       :enabled? @(:enabled? pool)})))

(defn get-all-pool-stats
  "Get statistics for all pools."
  []
  (mapv (fn [[id _]] (get-pool-stats id)) (:pools @state)))

(defn get-global-stats
  "Get global connection pool statistics."
  []
  (let [stats (:stats @state)]
    {:pools-count (count (:pools @state))
     :connections-created (:connections-created stats)
     :connections-destroyed (:connections-destroyed stats)
     :connections-borrowed (:connections-borrowed stats)
     :connections-returned (:connections-returned stats)
     :validation-failures (:validation-failures stats)
     :leaks-detected (:leaks-detected stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-connection-pool-manager!
  "Initialize the connection pool manager."
  []
  (when-not (:initialized? @state)
    ;; Start maintenance task
    (start-maintenance-task!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Connection pool manager initialized")
    (events/emit! :connection-pool-manager-initialized {})
    true))
