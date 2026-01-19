(ns mental-models.pipeline.integration.graceful-shutdown
  "Graceful shutdown manager for mental model analysis system.
   
   Features:
   - Shutdown hooks
   - Connection draining
   - In-flight request handling
   - Resource cleanup
   - Timeout management
   - Health status updates
   - Shutdown ordering
   - Recovery support"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:status :running     ;; :running, :draining, :shutting-down, :stopped
         :hooks {}            ;; hook-id -> shutdown-hook
         :in-flight (atom 0)  ;; count of in-flight requests
         :connections {}      ;; connection-id -> connection
         :resources {}        ;; resource-id -> resource
         :shutdown-start nil
         :config {:drain-timeout-ms 30000
                  :shutdown-timeout-ms 60000
                  :health-check-interval-ms 1000
                  :force-shutdown-delay-ms 5000}
         :stats {:shutdowns 0 :forced-shutdowns 0 :hooks-executed 0}
         :initialized? false}))

;; ============================================================================
;; Status Management
;; ============================================================================

(defn get-status
  "Get current shutdown status."
  []
  (:status @state))

(defn is-running?
  "Check if system is running."
  []
  (= :running (:status @state)))

(defn is-shutting-down?
  "Check if system is shutting down."
  []
  (contains? #{:draining :shutting-down} (:status @state)))

(defn is-stopped?
  "Check if system is stopped."
  []
  (= :stopped (:status @state)))

(defn- set-status!
  "Set shutdown status."
  [status]
  (swap! state assoc :status status)
  (logging/log :info "Shutdown status changed" {:status status})
  (events/emit! :shutdown-status-changed {:status status}))

;; ============================================================================
;; Shutdown Hooks
;; ============================================================================

(defn register-hook!
  "Register a shutdown hook."
  [hook-id config]
  (let [hook {:id hook-id
              :name (get config :name (name hook-id))
              :priority (get config :priority 50) ;; 0-100, higher = earlier
              :timeout-ms (get config :timeout-ms 10000)
              :shutdown-fn (get config :shutdown-fn)
              :async? (get config :async? false)
              :critical? (get config :critical? false)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:hooks hook-id] hook)
    (logging/log :debug "Registered shutdown hook" {:hook-id hook-id :priority (:priority hook)})
    hook-id))

(defn unregister-hook!
  "Unregister a shutdown hook."
  [hook-id]
  (swap! state update :hooks dissoc hook-id))

(defn list-hooks
  "List all shutdown hooks."
  []
  (sort-by :priority > (vals (:hooks @state))))

(defn- execute-hook
  "Execute a single shutdown hook."
  [hook]
  (let [start-time (System/currentTimeMillis)]
    (try
      (logging/log :debug "Executing shutdown hook" {:hook-id (:id hook)})
      (if (:async? hook)
        ;; Async execution with timeout
        (let [result-chan (async/thread ((:shutdown-fn hook)))
              timeout-chan (timeout (:timeout-ms hook))]
          (async/alt!!
            result-chan ([v] {:success? true :result v})
            timeout-chan ([_] {:success? false :error "Timeout"})))
        ;; Sync execution
        (do
          ((:shutdown-fn hook))
          {:success? true}))
      (catch Exception e
        {:success? false :error (.getMessage e)})
      (finally
        (swap! state update-in [:stats :hooks-executed] inc)))))

(defn- execute-all-hooks
  "Execute all shutdown hooks in priority order."
  []
  (let [hooks (list-hooks)
        results (atom [])]
    (doseq [hook hooks]
      (let [result (execute-hook hook)]
        (swap! results conj (assoc result :hook-id (:id hook)))
        (when (and (not (:success? result)) (:critical? hook))
          (logging/log :error "Critical shutdown hook failed" {:hook-id (:id hook) :error (:error result)}))))
    @results))

;; ============================================================================
;; In-Flight Request Tracking
;; ============================================================================

(defn track-request-start!
  "Track start of a request."
  []
  (swap! (:in-flight @state) inc))

(defn track-request-end!
  "Track end of a request."
  []
  (swap! (:in-flight @state) dec))

(defn get-in-flight-count
  "Get count of in-flight requests."
  []
  @(:in-flight @state))

(defn wait-for-in-flight
  "Wait for all in-flight requests to complete."
  [& {:keys [timeout-ms] :or {timeout-ms 30000}}]
  (let [start-time (System/currentTimeMillis)
        deadline (+ start-time timeout-ms)]
    (loop []
      (let [count (get-in-flight-count)
            now (System/currentTimeMillis)]
        (cond
          (zero? count) {:success? true :remaining 0}
          (> now deadline) {:success? false :remaining count :timeout? true}
          :else (do
                  (Thread/sleep 100)
                  (recur)))))))

(defmacro with-request-tracking
  "Execute body with request tracking."
  [& body]
  `(do
     (track-request-start!)
     (try
       ~@body
       (finally
         (track-request-end!)))))

;; ============================================================================
;; Connection Management
;; ============================================================================

(defn register-connection!
  "Register a connection for graceful shutdown."
  [connection-id connection & {:keys [drain-fn close-fn]}]
  (swap! state assoc-in [:connections connection-id]
         {:id connection-id
          :connection connection
          :drain-fn drain-fn
          :close-fn close-fn
          :registered-at (System/currentTimeMillis)}))

(defn unregister-connection!
  "Unregister a connection."
  [connection-id]
  (swap! state update :connections dissoc connection-id))

(defn drain-connections!
  "Drain all connections."
  []
  (doseq [[id conn] (:connections @state)]
    (when-let [drain-fn (:drain-fn conn)]
      (try
        (drain-fn (:connection conn))
        (logging/log :debug "Drained connection" {:connection-id id})
        (catch Exception e
          (logging/log :error "Error draining connection" {:connection-id id :error (.getMessage e)}))))))

(defn close-connections!
  "Close all connections."
  []
  (doseq [[id conn] (:connections @state)]
    (when-let [close-fn (:close-fn conn)]
      (try
        (close-fn (:connection conn))
        (logging/log :debug "Closed connection" {:connection-id id})
        (catch Exception e
          (logging/log :error "Error closing connection" {:connection-id id :error (.getMessage e)}))))
    (unregister-connection! id)))

;; ============================================================================
;; Resource Management
;; ============================================================================

(defn register-resource!
  "Register a resource for cleanup."
  [resource-id resource & {:keys [cleanup-fn priority]}]
  (swap! state assoc-in [:resources resource-id]
         {:id resource-id
          :resource resource
          :cleanup-fn cleanup-fn
          :priority (or priority 50)
          :registered-at (System/currentTimeMillis)}))

(defn unregister-resource!
  "Unregister a resource."
  [resource-id]
  (swap! state update :resources dissoc resource-id))

(defn cleanup-resources!
  "Clean up all resources."
  []
  (let [resources (sort-by :priority > (vals (:resources @state)))]
    (doseq [res resources]
      (when-let [cleanup-fn (:cleanup-fn res)]
        (try
          (cleanup-fn (:resource res))
          (logging/log :debug "Cleaned up resource" {:resource-id (:id res)})
          (catch Exception e
            (logging/log :error "Error cleaning up resource" {:resource-id (:id res) :error (.getMessage e)}))))
      (unregister-resource! (:id res)))))

;; ============================================================================
;; Shutdown Process
;; ============================================================================

(defn initiate-shutdown!
  "Initiate graceful shutdown."
  [& {:keys [reason timeout-ms]}]
  (when (is-running?)
    (logging/log :info "Initiating graceful shutdown" {:reason reason})
    (swap! state assoc :shutdown-start (System/currentTimeMillis))
    (swap! state update-in [:stats :shutdowns] inc)
    (events/emit! :shutdown-initiated {:reason reason})
    
    ;; Phase 1: Draining
    (set-status! :draining)
    (drain-connections!)
    
    ;; Wait for in-flight requests
    (let [drain-timeout (or timeout-ms (get-in @state [:config :drain-timeout-ms]))
          drain-result (wait-for-in-flight :timeout-ms drain-timeout)]
      (when-not (:success? drain-result)
        (logging/log :warn "Drain timeout, proceeding with shutdown" {:remaining (:remaining drain-result)})))
    
    ;; Phase 2: Shutting down
    (set-status! :shutting-down)
    
    ;; Execute shutdown hooks
    (let [hook-results (execute-all-hooks)]
      (logging/log :info "Shutdown hooks executed" {:results (count hook-results)}))
    
    ;; Close connections
    (close-connections!)
    
    ;; Cleanup resources
    (cleanup-resources!)
    
    ;; Phase 3: Stopped
    (set-status! :stopped)
    (logging/log :info "Graceful shutdown complete"
                 {:duration-ms (- (System/currentTimeMillis) (:shutdown-start @state))})
    (events/emit! :shutdown-complete {})
    
    true))

(defn force-shutdown!
  "Force immediate shutdown."
  []
  (logging/log :warn "Forcing immediate shutdown")
  (swap! state update-in [:stats :forced-shutdowns] inc)
  (events/emit! :force-shutdown {})
  
  (set-status! :shutting-down)
  
  ;; Execute critical hooks only
  (doseq [hook (filter :critical? (list-hooks))]
    (try
      ((:shutdown-fn hook))
      (catch Exception _)))
  
  ;; Force close connections
  (close-connections!)
  
  ;; Cleanup resources
  (cleanup-resources!)
  
  (set-status! :stopped)
  true)

(defn abort-shutdown!
  "Abort an in-progress shutdown (if possible)."
  []
  (when (= :draining (:status @state))
    (logging/log :info "Aborting shutdown")
    (set-status! :running)
    (swap! state assoc :shutdown-start nil)
    (events/emit! :shutdown-aborted {})
    true))

;; ============================================================================
;; JVM Shutdown Hook
;; ============================================================================

(defn install-jvm-shutdown-hook!
  "Install JVM shutdown hook."
  []
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    (fn []
      (when (is-running?)
        (initiate-shutdown! :reason "JVM shutdown"))))))

;; ============================================================================
;; Health Check Integration
;; ============================================================================

(defn get-health-status
  "Get health status for load balancer."
  []
  (case (:status @state)
    :running {:status :healthy :accepting-traffic? true}
    :draining {:status :draining :accepting-traffic? false}
    :shutting-down {:status :unhealthy :accepting-traffic? false}
    :stopped {:status :stopped :accepting-traffic? false}
    {:status :unknown :accepting-traffic? false}))

(defn should-accept-traffic?
  "Check if system should accept new traffic."
  []
  (= :running (:status @state)))

;; ============================================================================
;; Middleware
;; ============================================================================

(defn shutdown-middleware
  "Ring middleware for graceful shutdown."
  [handler]
  (fn [request]
    (if (should-accept-traffic?)
      (with-request-tracking
        (handler request))
      {:status 503
       :headers {"Retry-After" "30"}
       :body "Service is shutting down"})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-shutdown-stats
  "Get shutdown statistics."
  []
  (let [stats (:stats @state)]
    {:status (:status @state)
     :in-flight-requests (get-in-flight-count)
     :registered-hooks (count (:hooks @state))
     :registered-connections (count (:connections @state))
     :registered-resources (count (:resources @state))
     :shutdowns (:shutdowns stats)
     :forced-shutdowns (:forced-shutdowns stats)
     :hooks-executed (:hooks-executed stats)
     :shutdown-start (:shutdown-start @state)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-graceful-shutdown!
  "Initialize the graceful shutdown system."
  []
  (when-not (:initialized? @state)
    ;; Register default hooks
    (register-hook! :flush-logs
                    {:name "Flush Logs"
                     :priority 10
                     :shutdown-fn (fn []
                                    (logging/log :info "Flushing logs before shutdown"))})
    
    (register-hook! :save-metrics
                    {:name "Save Metrics"
                     :priority 20
                     :shutdown-fn (fn []
                                    (logging/log :info "Saving metrics before shutdown"))})
    
    (register-hook! :close-database
                    {:name "Close Database Connections"
                     :priority 80
                     :critical? true
                     :shutdown-fn (fn []
                                    (logging/log :info "Closing database connections"))})
    
    (register-hook! :notify-shutdown
                    {:name "Notify Shutdown"
                     :priority 90
                     :shutdown-fn (fn []
                                    (events/emit! :system-shutting-down {}))})
    
    ;; Install JVM shutdown hook
    (install-jvm-shutdown-hook!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Graceful shutdown system initialized")
    (events/emit! :graceful-shutdown-initialized {})
    true))
