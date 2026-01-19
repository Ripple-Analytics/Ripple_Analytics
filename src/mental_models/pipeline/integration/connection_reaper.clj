(ns mental-models.pipeline.integration.connection-reaper
  "Connection reaper for mental model analysis system.
   
   Features:
   - Idle connection detection
   - Connection timeout enforcement
   - Stale connection cleanup
   - Connection health validation
   - Reaper scheduling
   - Connection metrics
   - Graceful connection closing
   - Reaper configuration"
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
           [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:pools {}            ;; pool-id -> pool
         :connections {}      ;; connection-id -> connection-info
         :executor nil        ;; ScheduledThreadPoolExecutor
         :config {:idle-timeout-ms 300000      ;; 5 minutes
                  :max-lifetime-ms 1800000     ;; 30 minutes
                  :validation-interval-ms 60000 ;; 1 minute
                  :reap-interval-ms 30000      ;; 30 seconds
                  :grace-period-ms 5000}       ;; 5 seconds
         :stats {:connections-reaped 0
                 :idle-connections-closed 0
                 :expired-connections-closed 0
                 :unhealthy-connections-closed 0
                 :validation-checks 0}
         :initialized? false}))

;; ============================================================================
;; Connection Registration
;; ============================================================================

(defn register-connection!
  "Register a connection for reaping."
  [connection-id config]
  (let [connection {:id connection-id
                    :pool-id (get config :pool-id)
                    :created-at (System/currentTimeMillis)
                    :last-used-at (atom (System/currentTimeMillis))
                    :last-validated-at (atom (System/currentTimeMillis))
                    :idle-timeout-ms (get config :idle-timeout-ms
                                          (get-in @state [:config :idle-timeout-ms]))
                    :max-lifetime-ms (get config :max-lifetime-ms
                                          (get-in @state [:config :max-lifetime-ms]))
                    :close-fn (get config :close-fn)
                    :validate-fn (get config :validate-fn)
                    :healthy? (atom true)
                    :in-use? (atom false)
                    :marked-for-reaping? (atom false)}]
    
    (swap! state assoc-in [:connections connection-id] connection)
    (logging/log :debug "Registered connection" {:connection-id connection-id})
    connection-id))

(defn unregister-connection!
  "Unregister a connection."
  [connection-id]
  (swap! state update :connections dissoc connection-id)
  (logging/log :debug "Unregistered connection" {:connection-id connection-id}))

(defn get-connection
  "Get a connection."
  [connection-id]
  (get-in @state [:connections connection-id]))

(defn list-connections
  "List all connections."
  []
  (mapv (fn [[id c]]
          {:id id
           :pool-id (:pool-id c)
           :created-at (:created-at c)
           :last-used-at @(:last-used-at c)
           :healthy? @(:healthy? c)
           :in-use? @(:in-use? c)})
        (:connections @state)))

;; ============================================================================
;; Connection Usage Tracking
;; ============================================================================

(defn mark-in-use!
  "Mark a connection as in use."
  [connection-id]
  (when-let [conn (get-connection connection-id)]
    (reset! (:in-use? conn) true)
    (reset! (:last-used-at conn) (System/currentTimeMillis))))

(defn mark-idle!
  "Mark a connection as idle."
  [connection-id]
  (when-let [conn (get-connection connection-id)]
    (reset! (:in-use? conn) false)
    (reset! (:last-used-at conn) (System/currentTimeMillis))))

(defn touch-connection!
  "Update the last used time for a connection."
  [connection-id]
  (when-let [conn (get-connection connection-id)]
    (reset! (:last-used-at conn) (System/currentTimeMillis))))

;; ============================================================================
;; Connection Validation
;; ============================================================================

(defn- validate-connection!
  "Validate a connection's health."
  [connection]
  (swap! state update-in [:stats :validation-checks] inc)
  
  (if-let [validate-fn (:validate-fn connection)]
    (try
      (let [healthy? (validate-fn)]
        (reset! (:healthy? connection) healthy?)
        (reset! (:last-validated-at connection) (System/currentTimeMillis))
        healthy?)
      (catch Exception e
        (logging/log :warn "Connection validation failed" {:connection-id (:id connection)
                                                            :error (.getMessage e)})
        (reset! (:healthy? connection) false)
        false))
    (do
      (reset! (:last-validated-at connection) (System/currentTimeMillis))
      true)))

(defn validate-all-connections!
  "Validate all connections."
  []
  (doseq [[id conn] (:connections @state)]
    (when-not @(:in-use? conn)
      (validate-connection! conn))))

;; ============================================================================
;; Connection Reaping
;; ============================================================================

(defn- is-idle?
  "Check if a connection is idle."
  [connection]
  (let [now (System/currentTimeMillis)
        last-used @(:last-used-at connection)
        idle-time (- now last-used)]
    (and (not @(:in-use? connection))
         (> idle-time (:idle-timeout-ms connection)))))

(defn- is-expired?
  "Check if a connection has exceeded its max lifetime."
  [connection]
  (let [now (System/currentTimeMillis)
        created (:created-at connection)
        lifetime (- now created)]
    (> lifetime (:max-lifetime-ms connection))))

(defn- close-connection!
  "Close a connection."
  [connection reason]
  (when-not @(:marked-for-reaping? connection)
    (reset! (:marked-for-reaping? connection) true)
    
    (logging/log :debug "Closing connection" {:connection-id (:id connection)
                                               :reason reason})
    
    ;; Wait for grace period if in use
    (when @(:in-use? connection)
      (Thread/sleep (get-in @state [:config :grace-period-ms])))
    
    ;; Close the connection
    (when-let [close-fn (:close-fn connection)]
      (try
        (close-fn)
        (catch Exception e
          (logging/log :warn "Error closing connection" {:connection-id (:id connection)
                                                          :error (.getMessage e)}))))
    
    ;; Update stats
    (case reason
      :idle (swap! state update-in [:stats :idle-connections-closed] inc)
      :expired (swap! state update-in [:stats :expired-connections-closed] inc)
      :unhealthy (swap! state update-in [:stats :unhealthy-connections-closed] inc)
      nil)
    
    (swap! state update-in [:stats :connections-reaped] inc)
    
    ;; Unregister
    (unregister-connection! (:id connection))
    
    (events/emit! :connection-reaped {:connection-id (:id connection)
                                       :reason reason})))

(defn- reap-connections!
  "Reap idle, expired, and unhealthy connections."
  []
  (doseq [[id conn] (:connections @state)]
    (cond
      ;; Close unhealthy connections
      (not @(:healthy? conn))
      (close-connection! conn :unhealthy)
      
      ;; Close expired connections
      (is-expired? conn)
      (close-connection! conn :expired)
      
      ;; Close idle connections
      (is-idle? conn)
      (close-connection! conn :idle))))

;; ============================================================================
;; Pool Management
;; ============================================================================

(defn register-pool!
  "Register a connection pool."
  [pool-id config]
  (let [pool {:id pool-id
              :name (get config :name (name pool-id))
              :idle-timeout-ms (get config :idle-timeout-ms
                                    (get-in @state [:config :idle-timeout-ms]))
              :max-lifetime-ms (get config :max-lifetime-ms
                                    (get-in @state [:config :max-lifetime-ms]))
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:pools pool-id] pool)
    (logging/log :info "Registered pool" {:pool-id pool-id})
    pool-id))

(defn get-pool-connections
  "Get all connections for a pool."
  [pool-id]
  (filter (fn [[_ c]] (= (:pool-id c) pool-id))
          (:connections @state)))

(defn reap-pool!
  "Reap connections for a specific pool."
  [pool-id]
  (doseq [[id conn] (get-pool-connections pool-id)]
    (cond
      (not @(:healthy? conn)) (close-connection! conn :unhealthy)
      (is-expired? conn) (close-connection! conn :expired)
      (is-idle? conn) (close-connection! conn :idle))))

;; ============================================================================
;; Reaper Scheduling
;; ============================================================================

(defn- start-reaper!
  "Start the connection reaper."
  []
  (let [executor (ScheduledThreadPoolExecutor. 2)
        reap-interval (get-in @state [:config :reap-interval-ms])
        validation-interval (get-in @state [:config :validation-interval-ms])]
    
    ;; Schedule reaping
    (.scheduleAtFixedRate executor
                          (fn [] (try (reap-connections!)
                                      (catch Exception e
                                        (logging/log :error "Reaper error" {:error (.getMessage e)}))))
                          reap-interval
                          reap-interval
                          TimeUnit/MILLISECONDS)
    
    ;; Schedule validation
    (.scheduleAtFixedRate executor
                          (fn [] (try (validate-all-connections!)
                                      (catch Exception e
                                        (logging/log :error "Validation error" {:error (.getMessage e)}))))
                          validation-interval
                          validation-interval
                          TimeUnit/MILLISECONDS)
    
    (swap! state assoc :executor executor)
    (logging/log :info "Started connection reaper")))

(defn- stop-reaper!
  "Stop the connection reaper."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor)
    (swap! state assoc :executor nil)
    (logging/log :info "Stopped connection reaper")))

;; ============================================================================
;; Manual Reaping
;; ============================================================================

(defn force-reap!
  "Force immediate reaping of all connections."
  []
  (reap-connections!)
  (logging/log :info "Forced connection reaping"))

(defn force-close!
  "Force close a specific connection."
  [connection-id]
  (when-let [conn (get-connection connection-id)]
    (close-connection! conn :forced)))

(defn force-close-pool!
  "Force close all connections in a pool."
  [pool-id]
  (doseq [[id conn] (get-pool-connections pool-id)]
    (close-connection! conn :forced)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-idle-timeout!
  "Set the idle timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :idle-timeout-ms] timeout-ms))

(defn set-max-lifetime!
  "Set the max lifetime."
  [lifetime-ms]
  (swap! state assoc-in [:config :max-lifetime-ms] lifetime-ms))

(defn set-reap-interval!
  "Set the reap interval."
  [interval-ms]
  (swap! state assoc-in [:config :reap-interval-ms] interval-ms)
  ;; Restart reaper with new interval
  (stop-reaper!)
  (start-reaper!))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-reaper-metrics
  "Get reaper metrics."
  []
  (let [connections (vals (:connections @state))
        now (System/currentTimeMillis)]
    {:total-connections (count connections)
     :idle-connections (count (filter #(and (not @(:in-use? %))
                                            (not @(:marked-for-reaping? %))) connections))
     :in-use-connections (count (filter #(and @(:in-use? %)
                                              (not @(:marked-for-reaping? %))) connections))
     :unhealthy-connections (count (filter #(not @(:healthy? %)) connections))
     :avg-idle-time (if (seq connections)
                      (/ (reduce + (map #(- now @(:last-used-at %)) connections))
                         (count connections))
                      0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-reaper-stats
  "Get reaper statistics."
  []
  (let [stats (:stats @state)]
    {:pools-count (count (:pools @state))
     :connections-count (count (:connections @state))
     :connections-reaped (:connections-reaped stats)
     :idle-connections-closed (:idle-connections-closed stats)
     :expired-connections-closed (:expired-connections-closed stats)
     :unhealthy-connections-closed (:unhealthy-connections-closed stats)
     :validation-checks (:validation-checks stats)}))

(defn reset-stats!
  "Reset reaper statistics."
  []
  (swap! state assoc :stats {:connections-reaped 0
                             :idle-connections-closed 0
                             :expired-connections-closed 0
                             :unhealthy-connections-closed 0
                             :validation-checks 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-connection-reaper!
  "Initialize the connection reaper."
  []
  (when-not (:initialized? @state)
    (start-reaper!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Connection reaper initialized")
    (events/emit! :connection-reaper-initialized {})
    true))

(defn shutdown-connection-reaper!
  "Shutdown the connection reaper."
  []
  (stop-reaper!)
  
  ;; Close all connections
  (doseq [[id conn] (:connections @state)]
    (close-connection! conn :shutdown))
  
  (swap! state assoc :initialized? false)
  (logging/log :info "Connection reaper shutdown"))
