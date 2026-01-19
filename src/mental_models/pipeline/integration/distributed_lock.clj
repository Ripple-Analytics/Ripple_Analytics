(ns mental-models.pipeline.integration.distributed-lock
  "Distributed Lock Module
   
   Distributed coordination:
   - Lock acquisition
   - Lock release
   - Lock renewal
   - Deadlock detection
   - Fair locking"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]
   [java.util.concurrent.locks ReentrantLock]))

;; =============================================================================
;; LOCK STATE
;; =============================================================================

(defonce lock-state (atom {:locks {}
                           :waiters {}
                           :local-locks {}
                           :config {:default-ttl-ms 30000
                                    :renewal-interval-ms 10000
                                    :max-wait-ms 60000
                                    :fair-locking true}}))

;; =============================================================================
;; LOCK CREATION
;; =============================================================================

(defn generate-lock-id
  "Generate a unique lock ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn generate-owner-id
  "Generate a unique owner ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-lock
  "Create a lock entry."
  [lock-name owner-id & {:keys [ttl-ms]}]
  {:name lock-name
   :owner-id owner-id
   :acquired-at (System/currentTimeMillis)
   :expires-at (+ (System/currentTimeMillis)
                  (or ttl-ms (get-in @lock-state [:config :default-ttl-ms])))
   :renewals 0})

;; =============================================================================
;; LOCK OPERATIONS
;; =============================================================================

(defn lock-exists?
  "Check if a lock exists and is valid."
  [lock-name]
  (when-let [lock (get-in @lock-state [:locks lock-name])]
    (> (:expires-at lock) (System/currentTimeMillis))))

(defn lock-owner
  "Get the owner of a lock."
  [lock-name]
  (when (lock-exists? lock-name)
    (:owner-id (get-in @lock-state [:locks lock-name]))))

(defn acquire-lock!
  "Attempt to acquire a lock."
  [lock-name owner-id & {:keys [ttl-ms wait-ms]}]
  (when (flags/is-enabled? "distributed-lock")
    (let [wait-ms (or wait-ms 0)
          start-time (System/currentTimeMillis)
          max-wait (get-in @lock-state [:config :max-wait-ms])]
      (log/debug "Attempting to acquire lock" {:name lock-name :owner owner-id})
      (loop []
        (cond
          ;; Lock doesn't exist or expired
          (not (lock-exists? lock-name))
          (do
            (swap! lock-state assoc-in [:locks lock-name]
                   (create-lock lock-name owner-id :ttl-ms ttl-ms))
            (metrics/inc-counter! :lock/locks-acquired)
            (events/publish! :lock/acquired {:name lock-name :owner owner-id})
            (log/info "Lock acquired" {:name lock-name :owner owner-id})
            {:success true :lock-name lock-name :owner-id owner-id})
          
          ;; Already own the lock (reentrant)
          (= (lock-owner lock-name) owner-id)
          (do
            (log/debug "Lock already owned" {:name lock-name :owner owner-id})
            {:success true :lock-name lock-name :owner-id owner-id :reentrant true})
          
          ;; Wait for lock
          (and (> wait-ms 0)
               (< (- (System/currentTimeMillis) start-time) (min wait-ms max-wait)))
          (do
            ;; Add to waiters
            (swap! lock-state update-in [:waiters lock-name] (fnil conj []) owner-id)
            (Thread/sleep 100)
            (recur))
          
          ;; Failed to acquire
          :else
          (do
            (metrics/inc-counter! :lock/lock-failures)
            (log/debug "Failed to acquire lock" {:name lock-name :owner owner-id})
            {:success false :error "Lock held by another owner"}))))))

(defn release-lock!
  "Release a lock."
  [lock-name owner-id]
  (when (flags/is-enabled? "distributed-lock")
    (log/debug "Releasing lock" {:name lock-name :owner owner-id})
    (if (= (lock-owner lock-name) owner-id)
      (do
        (swap! lock-state update :locks dissoc lock-name)
        ;; Remove from waiters
        (swap! lock-state update :waiters dissoc lock-name)
        (metrics/inc-counter! :lock/locks-released)
        (events/publish! :lock/released {:name lock-name :owner owner-id})
        (log/info "Lock released" {:name lock-name :owner owner-id})
        {:success true})
      {:success false :error "Not lock owner"})))

(defn renew-lock!
  "Renew a lock's TTL."
  [lock-name owner-id & {:keys [ttl-ms]}]
  (when (flags/is-enabled? "distributed-lock")
    (if (= (lock-owner lock-name) owner-id)
      (let [new-ttl (or ttl-ms (get-in @lock-state [:config :default-ttl-ms]))]
        (swap! lock-state update-in [:locks lock-name]
               (fn [lock]
                 (-> lock
                     (assoc :expires-at (+ (System/currentTimeMillis) new-ttl))
                     (update :renewals inc))))
        (metrics/inc-counter! :lock/locks-renewed)
        (log/debug "Lock renewed" {:name lock-name :owner owner-id})
        {:success true})
      {:success false :error "Not lock owner"})))

;; =============================================================================
;; LOCK QUERIES
;; =============================================================================

(defn get-lock
  "Get lock information."
  [lock-name]
  (get-in @lock-state [:locks lock-name]))

(defn list-locks
  "List all active locks."
  []
  (let [now (System/currentTimeMillis)]
    (filter (fn [[_ lock]] (> (:expires-at lock) now))
            (:locks @lock-state))))

(defn get-waiters
  "Get waiters for a lock."
  [lock-name]
  (get-in @lock-state [:waiters lock-name] []))

(defn is-locked?
  "Check if a resource is locked."
  [lock-name]
  (lock-exists? lock-name))

(defn owns-lock?
  "Check if an owner owns a lock."
  [lock-name owner-id]
  (= (lock-owner lock-name) owner-id))

;; =============================================================================
;; LOCAL LOCKS (JVM-level)
;; =============================================================================

(defn get-local-lock
  "Get or create a local JVM lock."
  [lock-name]
  (or (get-in @lock-state [:local-locks lock-name])
      (let [lock (ReentrantLock. (get-in @lock-state [:config :fair-locking]))]
        (swap! lock-state assoc-in [:local-locks lock-name] lock)
        lock)))

(defn with-local-lock
  "Execute a function with a local lock."
  [lock-name f]
  (let [^ReentrantLock lock (get-local-lock lock-name)]
    (try
      (.lock lock)
      (f)
      (finally
        (.unlock lock)))))

;; =============================================================================
;; LOCK MACROS
;; =============================================================================

(defmacro with-lock
  "Execute body with a distributed lock."
  [lock-name owner-id opts & body]
  `(let [result# (acquire-lock! ~lock-name ~owner-id ~@opts)]
     (if (:success result#)
       (try
         ~@body
         (finally
           (release-lock! ~lock-name ~owner-id)))
       (throw (ex-info "Failed to acquire lock" result#)))))

(defmacro try-with-lock
  "Try to execute body with a lock, return nil if lock not acquired."
  [lock-name owner-id & body]
  `(let [result# (acquire-lock! ~lock-name ~owner-id)]
     (when (:success result#)
       (try
         ~@body
         (finally
           (release-lock! ~lock-name ~owner-id))))))

;; =============================================================================
;; DEADLOCK DETECTION
;; =============================================================================

(defn detect-deadlocks
  "Detect potential deadlocks."
  []
  (let [locks (:locks @lock-state)
        waiters (:waiters @lock-state)]
    ;; Simple cycle detection in wait graph
    (for [[lock-name lock] locks
          :let [owner (:owner-id lock)
                waiting (get waiters lock-name [])]
          waiter waiting
          :when (some (fn [[other-lock other-info]]
                        (and (= (:owner-id other-info) waiter)
                             (some #{owner} (get waiters other-lock []))))
                      locks)]
      {:lock lock-name
       :owner owner
       :waiter waiter
       :type :potential-deadlock})))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn cleanup-expired-locks!
  "Clean up expired locks."
  []
  (let [now (System/currentTimeMillis)
        expired (filter (fn [[_ lock]] (<= (:expires-at lock) now))
                        (:locks @lock-state))]
    (doseq [[lock-name _] expired]
      (swap! lock-state update :locks dissoc lock-name)
      (swap! lock-state update :waiters dissoc lock-name)
      (metrics/inc-counter! :lock/locks-expired))
    (count expired)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defonce cleanup-executor (atom nil))

(defn start-cleanup-scheduler!
  "Start the lock cleanup scheduler."
  []
  (when (and (flags/is-enabled? "distributed-lock")
             (nil? @cleanup-executor))
    (log/info "Starting lock cleanup scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate executor
                            #(try (cleanup-expired-locks!)
                                  (catch Exception e
                                    (log/error "Lock cleanup error" {:error (.getMessage e)})))
                            0
                            5000
                            TimeUnit/MILLISECONDS)
      (reset! cleanup-executor executor))))

(defn stop-cleanup-scheduler!
  "Stop the lock cleanup scheduler."
  []
  (when-let [^ScheduledExecutorService executor @cleanup-executor]
    (log/info "Stopping lock cleanup scheduler")
    (.shutdown executor)
    (reset! cleanup-executor nil)))

(defn init-distributed-lock!
  "Initialize distributed lock."
  []
  (log/info "Initializing distributed lock")
  ;; Register feature flag
  (flags/register-flag! "distributed-lock" "Enable distributed lock" true)
  ;; Create metrics
  (metrics/create-counter! :lock/locks-acquired "Locks acquired")
  (metrics/create-counter! :lock/locks-released "Locks released")
  (metrics/create-counter! :lock/locks-renewed "Locks renewed")
  (metrics/create-counter! :lock/locks-expired "Locks expired")
  (metrics/create-counter! :lock/lock-failures "Lock failures")
  (metrics/create-gauge! :lock/active-locks "Active locks"
                         #(count (list-locks)))
  (log/info "Distributed lock initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-lock-status []
  {:enabled (flags/is-enabled? "distributed-lock")
   :active-locks (count (list-locks))
   :total-waiters (reduce + (map count (vals (:waiters @lock-state))))
   :local-locks (count (:local-locks @lock-state))
   :cleanup-running (some? @cleanup-executor)
   :deadlocks (count (detect-deadlocks))
   :config (:config @lock-state)})
