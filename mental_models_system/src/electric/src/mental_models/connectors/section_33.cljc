(ns mental-models.connectors.section-33
  "Connectors Module - Section 33"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def shutdown-state
  "Atom tracking shutdown state and pending operations."
  (atom {:shutting-down false
         :pending-operations (atom 0)
         :shutdown-timeout-ms 30000
         :shutdown-hooks []}))

#?(:clj
   (defn is-shutting-down?
     "Check if the system is in shutdown mode."
     []
     (:shutting-down @shutdown-state)))

#?(:clj
   (defn register-shutdown-hook
     "Register a function to be called during shutdown.
      Returns hook-id for unregistering."
     [hook-fn & {:keys [name priority] :or {priority 50}}]
     (let [hook-id (java.util.UUID/randomUUID)]
       (swap! shutdown-state update :shutdown-hooks
              (fn [hooks]
                (->> (conj hooks {:id hook-id
                                  :fn hook-fn
                                  :name name
                                  :priority priority})
                     (sort-by :priority))))
       hook-id)))

#?(:clj
   (defn unregister-shutdown-hook
     "Unregister a shutdown hook by its ID."
     [hook-id]
     (swap! shutdown-state update :shutdown-hooks
            (fn [hooks]
              (vec (remove #(= hook-id (:id %)) hooks))))))

#?(:clj
   (defn track-operation
     "Track a pending operation. Returns a function to call when operation completes."
     []
     (when-not (is-shutting-down?)
       (swap! (:pending-operations @shutdown-state) inc)
       (fn []
         (swap! (:pending-operations @shutdown-state) dec)))))

#?(:clj
   (defn with-tracked-operation
     "Execute a function while tracking it as a pending operation.
      Prevents new operations during shutdown."
     [f]
     (if (is-shutting-down?)
       {:error "System is shutting down, operation rejected"}
       (let [complete-fn (track-operation)]
         (try
           (f)
           (finally
             (when complete-fn (complete-fn))))))))

#?(:clj
   (defn wait-for-pending-operations
     "Wait for all pending operations to complete, with timeout."
     [timeout-ms]
     (let [start-time (System/currentTimeMillis)
           pending-ops (:pending-operations @shutdown-state)]
       (loop []
         (let [elapsed (- (System/currentTimeMillis) start-time)
               pending @pending-ops]
           (cond
             (zero? pending) {:success true :pending 0}
             (>= elapsed timeout-ms) {:success false :pending pending :timeout true}
             :else (do
                     (Thread/sleep 100)
                     (recur))))))))

#?(:clj
   (defn initiate-shutdown
     "Initiate graceful shutdown.
      1. Set shutting-down flag to reject new operations
      2. Wait for pending operations to complete
      3. Run shutdown hooks in priority order
      4. Clean up resources"
     [& {:keys [timeout-ms] :or {timeout-ms 30000}}]
     (println "Initiating graceful shutdown...")
     (swap! shutdown-state assoc :shutting-down true)
     
     ;; Wait for pending operations
     (println "Waiting for pending operations to complete...")
     (let [wait-result (wait-for-pending-operations timeout-ms)]
       (when-not (:success wait-result)
         (println "Warning: Shutdown timeout reached with" (:pending wait-result) "pending operations")))
     
     ;; Run shutdown hooks
     (println "Running shutdown hooks...")
     (doseq [{:keys [fn name]} (:shutdown-hooks @shutdown-state)]
       (try
         (println "Running shutdown hook:" (or name "unnamed"))
         (fn)
         (catch Exception e
           (println "Shutdown hook error:" (.getMessage e)))))
     
     ;; Clean up resources
     (println "Cleaning up resources...")
     (drain-pool)
     (shutdown-all-connectors)
     (cancel-all-async-tasks)
     
     (println "Shutdown complete.")
     {:success true}))

#?(:clj
   (defn cancel-all-async-tasks
     "Cancel all pending async tasks."
     []
     (doseq [[task-id task] @async-tasks]
       (when-not (realized? (:future task))
         (future-cancel (:future task))))
     (reset! async-tasks {})))

#?(:clj
   (defn get-shutdown-status
     "Get current shutdown status."
     []
     {:shutting-down (is-shutting-down?)
      :pending-operations @(:pending-operations @shutdown-state)
      :registered-hooks (count (:shutdown-hooks @shutdown-state))
      :timeout-ms (:shutdown-timeout-ms @shutdown-state)}))

;; ============================================
;; Request Deduplication
