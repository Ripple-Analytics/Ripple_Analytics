(ns mental-models.pipeline.integration.resource-pool
  "Resource Pool Module
   
   Shared resource management:
   - Connection pooling
   - Resource lifecycle
   - Borrowing and returning
   - Health checking
   - Auto-scaling"
  (:require
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; POOL STATE
;; =============================================================================

(defonce pools (atom {}))

;; =============================================================================
;; POOL CONFIGURATION
;; =============================================================================

(def default-config
  {:min-size 2
   :max-size 10
   :acquire-timeout-ms 5000
   :idle-timeout-ms 300000
   :validation-interval-ms 30000
   :max-lifetime-ms 1800000})

;; =============================================================================
;; RESOURCE WRAPPER
;; =============================================================================

(defn wrap-resource
  "Wrap a resource with metadata."
  [resource]
  {:resource resource
   :id (str (java.util.UUID/randomUUID))
   :created-at (System/currentTimeMillis)
   :last-used-at (System/currentTimeMillis)
   :use-count 0
   :state :available})

(defn update-resource-usage
  "Update resource usage metadata."
  [wrapped]
  (-> wrapped
      (update :use-count inc)
      (assoc :last-used-at (System/currentTimeMillis))))

;; =============================================================================
;; POOL CREATION
;; =============================================================================

(defn create-pool!
  "Create a new resource pool."
  [pool-id & {:keys [factory validator destroyer config]
              :or {config default-config}}]
  (log/info "Creating resource pool" {:id pool-id :config config})
  (let [pool {:id pool-id
              :factory factory
              :validator (or validator (constantly true))
              :destroyer (or destroyer (constantly nil))
              :config (merge default-config config)
              :resources (atom [])
              :available (chan (:max-size config))
              :stats (atom {:created 0
                            :destroyed 0
                            :acquired 0
                            :released 0
                            :timeouts 0
                            :validation-failures 0})
              :created-at (System/currentTimeMillis)}]
    ;; Initialize minimum resources
    (dotimes [_ (:min-size (:config pool))]
      (try
        (let [resource (wrap-resource (factory))]
          (swap! (:resources pool) conj resource)
          (go (>! (:available pool) resource))
          (swap! (:stats pool) update :created inc))
        (catch Exception e
          (log/error "Failed to create initial resource" {:error (.getMessage e)}))))
    (swap! pools assoc pool-id pool)
    (metrics/inc-counter! :resource-pool/pools-created)
    pool-id))

(defn get-pool
  "Get a pool by ID."
  [pool-id]
  (get @pools pool-id))

;; =============================================================================
;; RESOURCE ACQUISITION
;; =============================================================================

(defn try-create-resource
  "Try to create a new resource if pool not at max."
  [pool]
  (let [current-size (count @(:resources pool))
        max-size (get-in pool [:config :max-size])]
    (when (< current-size max-size)
      (try
        (let [resource (wrap-resource ((:factory pool)))]
          (swap! (:resources pool) conj resource)
          (swap! (:stats pool) update :created inc)
          (log/debug "Created new resource" {:pool-id (:id pool) :resource-id (:id resource)})
          resource)
        (catch Exception e
          (log/error "Failed to create resource" {:error (.getMessage e)})
          nil)))))

(defn validate-resource
  "Validate a resource before use."
  [pool wrapped]
  (try
    ((:validator pool) (:resource wrapped))
    (catch Exception e
      (log/warn "Resource validation failed" {:error (.getMessage e)})
      false)))

(defn acquire!
  "Acquire a resource from the pool."
  [pool-id & {:keys [timeout-ms] :or {timeout-ms 5000}}]
  (when (flags/is-enabled? "resource-pool")
    (let [pool (get-pool pool-id)]
      (when-not pool
        (throw (ex-info "Pool not found" {:pool-id pool-id})))
      (log/debug "Acquiring resource" {:pool-id pool-id})
      (let [start-time (System/currentTimeMillis)
            result (async/alt!!
                     (:available pool) ([resource] resource)
                     (timeout timeout-ms) :timeout)]
        (cond
          (= result :timeout)
          (do
            ;; Try to create a new resource
            (if-let [new-resource (try-create-resource pool)]
              (do
                (swap! (:stats pool) update :acquired inc)
                (metrics/inc-counter! :resource-pool/acquired)
                (update-resource-usage new-resource))
              (do
                (swap! (:stats pool) update :timeouts inc)
                (metrics/inc-counter! :resource-pool/timeouts)
                (log/warn "Resource acquisition timeout" {:pool-id pool-id})
                nil)))
          
          (not (validate-resource pool result))
          (do
            (swap! (:stats pool) update :validation-failures inc)
            ;; Destroy invalid resource and try again
            (destroy-resource! pool result)
            (acquire! pool-id :timeout-ms (- timeout-ms (- (System/currentTimeMillis) start-time))))
          
          :else
          (do
            (swap! (:stats pool) update :acquired inc)
            (metrics/inc-counter! :resource-pool/acquired)
            (update-resource-usage result)))))))

(defn release!
  "Release a resource back to the pool."
  [pool-id wrapped]
  (when (and wrapped (flags/is-enabled? "resource-pool"))
    (let [pool (get-pool pool-id)]
      (when pool
        (log/debug "Releasing resource" {:pool-id pool-id :resource-id (:id wrapped)})
        ;; Check if resource is still valid
        (if (validate-resource pool wrapped)
          (do
            (go (>! (:available pool) (assoc wrapped :state :available)))
            (swap! (:stats pool) update :released inc)
            (metrics/inc-counter! :resource-pool/released))
          (do
            (destroy-resource! pool wrapped)
            ;; Create replacement if below min
            (when (< (count @(:resources pool)) (get-in pool [:config :min-size]))
              (try-create-resource pool))))))))

;; =============================================================================
;; RESOURCE DESTRUCTION
;; =============================================================================

(defn destroy-resource!
  "Destroy a resource."
  [pool wrapped]
  (log/debug "Destroying resource" {:pool-id (:id pool) :resource-id (:id wrapped)})
  (try
    ((:destroyer pool) (:resource wrapped))
    (catch Exception e
      (log/error "Failed to destroy resource" {:error (.getMessage e)})))
  (swap! (:resources pool) (fn [rs] (remove #(= (:id %) (:id wrapped)) rs)))
  (swap! (:stats pool) update :destroyed inc)
  (metrics/inc-counter! :resource-pool/destroyed))

;; =============================================================================
;; POOL MANAGEMENT
;; =============================================================================

(defn resize-pool!
  "Resize a pool to a new size."
  [pool-id new-min new-max]
  (when-let [pool (get-pool pool-id)]
    (log/info "Resizing pool" {:pool-id pool-id :new-min new-min :new-max new-max})
    (swap! pools assoc-in [pool-id :config :min-size] new-min)
    (swap! pools assoc-in [pool-id :config :max-size] new-max)
    ;; Create resources if below new minimum
    (let [current-size (count @(:resources pool))]
      (when (< current-size new-min)
        (dotimes [_ (- new-min current-size)]
          (when-let [resource (try-create-resource pool)]
            (go (>! (:available pool) resource))))))))

(defn drain-pool!
  "Drain all resources from a pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (log/info "Draining pool" {:pool-id pool-id})
    (doseq [wrapped @(:resources pool)]
      (destroy-resource! pool wrapped))))

(defn destroy-pool!
  "Destroy a pool and all its resources."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (log/info "Destroying pool" {:pool-id pool-id})
    (drain-pool! pool-id)
    (close! (:available pool))
    (swap! pools dissoc pool-id)))

;; =============================================================================
;; HEALTH CHECKING
;; =============================================================================

(defn check-pool-health
  "Check the health of a pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [resources @(:resources pool)
          valid-count (count (filter #(validate-resource pool %) resources))
          stats @(:stats pool)]
      {:pool-id pool-id
       :total-resources (count resources)
       :valid-resources valid-count
       :min-size (get-in pool [:config :min-size])
       :max-size (get-in pool [:config :max-size])
       :stats stats
       :healthy (>= valid-count (get-in pool [:config :min-size]))})))

(defn start-health-checker!
  "Start periodic health checking for a pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [interval (get-in pool [:config :validation-interval-ms])
          control-chan (chan)]
      (go-loop []
        (let [[v ch] (async/alts! [(timeout interval) control-chan])]
          (when-not (= ch control-chan)
            (let [health (check-pool-health pool-id)]
              (when-not (:healthy health)
                (log/warn "Pool unhealthy" health)
                (events/publish! :resource-pool/unhealthy health)))
            (recur))))
      control-chan)))

;; =============================================================================
;; CONVENIENCE MACRO
;; =============================================================================

(defmacro with-resource
  "Execute body with an acquired resource."
  [pool-id binding & body]
  `(let [wrapped# (acquire! ~pool-id)
         ~binding (:resource wrapped#)]
     (try
       ~@body
       (finally
         (release! ~pool-id wrapped#)))))

;; =============================================================================
;; POOL STATISTICS
;; =============================================================================

(defn get-pool-stats
  "Get statistics for a pool."
  [pool-id]
  (when-let [pool (get-pool pool-id)]
    (let [resources @(:resources pool)
          stats @(:stats pool)]
      {:pool-id pool-id
       :current-size (count resources)
       :available (count (filter #(= :available (:state %)) resources))
       :in-use (count (filter #(= :in-use (:state %)) resources))
       :stats stats
       :config (:config pool)})))

(defn get-all-pool-stats
  "Get statistics for all pools."
  []
  (map (fn [[id _]] (get-pool-stats id)) @pools))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-resource-pool!
  "Initialize resource pool system."
  []
  (log/info "Initializing resource pool")
  ;; Register feature flag
  (flags/register-flag! "resource-pool" "Enable resource pool" true)
  ;; Create metrics
  (metrics/create-counter! :resource-pool/pools-created "Pools created")
  (metrics/create-counter! :resource-pool/acquired "Resources acquired")
  (metrics/create-counter! :resource-pool/released "Resources released")
  (metrics/create-counter! :resource-pool/destroyed "Resources destroyed")
  (metrics/create-counter! :resource-pool/timeouts "Acquisition timeouts")
  (metrics/create-gauge! :resource-pool/total-pools "Total pools"
                         #(count @pools))
  (log/info "Resource pool initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-pool-status []
  {:enabled (flags/is-enabled? "resource-pool")
   :total-pools (count @pools)
   :pools (map (fn [[id _]] (get-pool-stats id)) @pools)})
