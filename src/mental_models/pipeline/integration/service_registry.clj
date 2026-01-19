(ns mental-models.pipeline.integration.service-registry
  "Service Registry Module
   
   Service discovery and registration:
   - Service registration and deregistration
   - Health checking
   - Service lookup and discovery
   - Load balancing integration
   - Service metadata management"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]))

;; =============================================================================
;; SERVICE REGISTRY STATE
;; =============================================================================

(defonce registry-state (atom {:services (ConcurrentHashMap.)
                               :instances (ConcurrentHashMap.)
                               :health-checks (ConcurrentHashMap.)
                               :watchers {}
                               :scheduler nil
                               :config {:health-check-interval-ms 10000
                                        :unhealthy-threshold 3
                                        :healthy-threshold 2
                                        :deregister-critical-after-ms 60000}}))

;; =============================================================================
;; SERVICE INSTANCE
;; =============================================================================

(defn create-instance
  "Create a service instance."
  [instance-id {:keys [service-id host port protocol metadata tags weight]}]
  {:id instance-id
   :service-id service-id
   :host host
   :port port
   :protocol (or protocol "http")
   :address (str (or protocol "http") "://" host ":" port)
   :metadata (or metadata {})
   :tags (or tags #{})
   :weight (or weight 100)
   :status :healthy
   :registered-at (System/currentTimeMillis)
   :last-heartbeat (AtomicLong. (System/currentTimeMillis))
   :consecutive-failures (AtomicLong. 0)
   :consecutive-successes (AtomicLong. 0)
   :total-requests (AtomicLong. 0)
   :failed-requests (AtomicLong. 0)})

;; =============================================================================
;; SERVICE REGISTRATION
;; =============================================================================

(defn register-service!
  "Register a new service."
  [service-id {:keys [name description version]}]
  (log/info "Registering service" {:id service-id})
  (let [service {:id service-id
                 :name (or name (name service-id))
                 :description description
                 :version version
                 :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:services @registry-state) service-id service)
    (.putIfAbsent ^ConcurrentHashMap (:instances @registry-state) service-id (ConcurrentHashMap.))
    (metrics/inc-counter! :serviceregistry/services-registered)
    (events/publish! :serviceregistry/service-registered {:service-id service-id})
    service-id))

(defn deregister-service!
  "Deregister a service."
  [service-id]
  (log/info "Deregistering service" {:id service-id})
  (.remove ^ConcurrentHashMap (:services @registry-state) service-id)
  (.remove ^ConcurrentHashMap (:instances @registry-state) service-id)
  (metrics/inc-counter! :serviceregistry/services-deregistered)
  (events/publish! :serviceregistry/service-deregistered {:service-id service-id}))

(defn get-service
  "Get a service by ID."
  [service-id]
  (.get ^ConcurrentHashMap (:services @registry-state) service-id))

(defn list-services
  "List all registered services."
  []
  (vec (vals (:services @registry-state))))

;; =============================================================================
;; INSTANCE REGISTRATION
;; =============================================================================

(defn register-instance!
  "Register a service instance."
  [service-id instance-id {:keys [host port protocol metadata tags weight]}]
  (log/info "Registering instance" {:service service-id :instance instance-id})
  ;; Ensure service exists
  (when-not (get-service service-id)
    (register-service! service-id {}))
  (let [instance (create-instance instance-id {:service-id service-id
                                               :host host
                                               :port port
                                               :protocol protocol
                                               :metadata metadata
                                               :tags tags
                                               :weight weight})
        instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
    (.put ^ConcurrentHashMap instances instance-id instance)
    (metrics/inc-counter! :serviceregistry/instances-registered)
    (events/publish! :serviceregistry/instance-registered {:service-id service-id :instance-id instance-id})
    (notify-watchers! service-id :instance-registered {:instance-id instance-id})
    instance-id))

(defn deregister-instance!
  "Deregister a service instance."
  [service-id instance-id]
  (log/info "Deregistering instance" {:service service-id :instance instance-id})
  (when-let [instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
    (.remove ^ConcurrentHashMap instances instance-id)
    (metrics/inc-counter! :serviceregistry/instances-deregistered)
    (events/publish! :serviceregistry/instance-deregistered {:service-id service-id :instance-id instance-id})
    (notify-watchers! service-id :instance-deregistered {:instance-id instance-id})))

(defn get-instance
  "Get a service instance."
  [service-id instance-id]
  (when-let [instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
    (.get ^ConcurrentHashMap instances instance-id)))

(defn list-instances
  "List all instances of a service."
  [service-id & {:keys [healthy-only tag]}]
  (when-let [instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
    (let [all-instances (vals instances)]
      (cond->> all-instances
        healthy-only (filter #(= (:status %) :healthy))
        tag (filter #(contains? (:tags %) tag))))))

;; =============================================================================
;; HEARTBEAT
;; =============================================================================

(defn heartbeat!
  "Send a heartbeat for an instance."
  [service-id instance-id]
  (when-let [instance (get-instance service-id instance-id)]
    (.set ^AtomicLong (:last-heartbeat instance) (System/currentTimeMillis))
    (metrics/inc-counter! :serviceregistry/heartbeats-received)))

(defn update-instance-status!
  "Update instance status."
  [service-id instance-id status]
  (when-let [instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
    (when-let [instance (.get ^ConcurrentHashMap instances instance-id)]
      (let [old-status (:status instance)
            updated-instance (assoc instance :status status)]
        (.put ^ConcurrentHashMap instances instance-id updated-instance)
        (when (not= old-status status)
          (log/info "Instance status changed" {:service service-id :instance instance-id :from old-status :to status})
          (events/publish! :serviceregistry/instance-status-changed {:service-id service-id
                                                                      :instance-id instance-id
                                                                      :old-status old-status
                                                                      :new-status status})
          (notify-watchers! service-id :status-changed {:instance-id instance-id :status status}))))))

;; =============================================================================
;; HEALTH CHECKING
;; =============================================================================

(defn register-health-check!
  "Register a health check for a service."
  [service-id check-fn & {:keys [interval-ms timeout-ms]}]
  (log/info "Registering health check" {:service service-id})
  (.put ^ConcurrentHashMap (:health-checks @registry-state) service-id
        {:check-fn check-fn
         :interval-ms (or interval-ms (get-in @registry-state [:config :health-check-interval-ms]))
         :timeout-ms (or timeout-ms 5000)}))

(defn unregister-health-check!
  "Unregister a health check."
  [service-id]
  (.remove ^ConcurrentHashMap (:health-checks @registry-state) service-id))

(defn check-instance-health!
  "Check health of an instance."
  [service-id instance-id]
  (when-let [health-check (.get ^ConcurrentHashMap (:health-checks @registry-state) service-id)]
    (when-let [instance (get-instance service-id instance-id)]
      (let [check-fn (:check-fn health-check)
            healthy? (try
                       (check-fn instance)
                       (catch Exception e
                         (log/warn "Health check failed" {:service service-id :instance instance-id :error (.getMessage e)})
                         false))
            unhealthy-threshold (get-in @registry-state [:config :unhealthy-threshold])
            healthy-threshold (get-in @registry-state [:config :healthy-threshold])]
        (if healthy?
          (do
            (.set ^AtomicLong (:consecutive-failures instance) 0)
            (.incrementAndGet ^AtomicLong (:consecutive-successes instance))
            (when (and (not= (:status instance) :healthy)
                       (>= (.get ^AtomicLong (:consecutive-successes instance)) healthy-threshold))
              (update-instance-status! service-id instance-id :healthy)))
          (do
            (.set ^AtomicLong (:consecutive-successes instance) 0)
            (.incrementAndGet ^AtomicLong (:consecutive-failures instance))
            (when (>= (.get ^AtomicLong (:consecutive-failures instance)) unhealthy-threshold)
              (update-instance-status! service-id instance-id :unhealthy))))
        (metrics/inc-counter! :serviceregistry/health-checks-performed)
        healthy?))))

(defn check-all-health!
  "Check health of all instances."
  []
  (doseq [service-id (keys (:services @registry-state))]
    (when-let [instances (.get ^ConcurrentHashMap (:instances @registry-state) service-id)]
      (doseq [instance-id (keys instances)]
        (check-instance-health! service-id instance-id)))))

;; =============================================================================
;; SERVICE DISCOVERY
;; =============================================================================

(defn discover
  "Discover healthy instances of a service."
  [service-id & {:keys [tag]}]
  (list-instances service-id :healthy-only true :tag tag))

(defn discover-one
  "Discover one healthy instance (random selection)."
  [service-id & {:keys [tag]}]
  (let [instances (discover service-id :tag tag)]
    (when (seq instances)
      (rand-nth instances))))

(defn discover-weighted
  "Discover one healthy instance using weighted random selection."
  [service-id & {:keys [tag]}]
  (let [instances (discover service-id :tag tag)]
    (when (seq instances)
      (let [total-weight (reduce + (map :weight instances))
            random-weight (rand-int total-weight)]
        (loop [remaining instances
               cumulative 0]
          (when (seq remaining)
            (let [instance (first remaining)
                  new-cumulative (+ cumulative (:weight instance))]
              (if (< random-weight new-cumulative)
                instance
                (recur (rest remaining) new-cumulative)))))))))

(defn get-service-address
  "Get the address of a healthy service instance."
  [service-id & {:keys [tag]}]
  (:address (discover-one service-id :tag tag)))

;; =============================================================================
;; WATCHERS
;; =============================================================================

(defn watch!
  "Watch a service for changes."
  [service-id watcher-id callback]
  (log/info "Adding watcher" {:service service-id :watcher watcher-id})
  (swap! registry-state assoc-in [:watchers service-id watcher-id] callback))

(defn unwatch!
  "Stop watching a service."
  [service-id watcher-id]
  (log/info "Removing watcher" {:service service-id :watcher watcher-id})
  (swap! registry-state update-in [:watchers service-id] dissoc watcher-id))

(defn notify-watchers!
  "Notify watchers of a service change."
  [service-id event-type event-data]
  (when-let [watchers (get-in @registry-state [:watchers service-id])]
    (doseq [[watcher-id callback] watchers]
      (try
        (callback {:service-id service-id
                   :event-type event-type
                   :data event-data
                   :timestamp (System/currentTimeMillis)})
        (catch Exception e
          (log/warn "Watcher callback failed" {:service service-id :watcher watcher-id :error (.getMessage e)}))))))

;; =============================================================================
;; INSTANCE METRICS
;; =============================================================================

(defn record-request!
  "Record a request to an instance."
  [service-id instance-id & {:keys [success?]}]
  (when-let [instance (get-instance service-id instance-id)]
    (.incrementAndGet ^AtomicLong (:total-requests instance))
    (when-not success?
      (.incrementAndGet ^AtomicLong (:failed-requests instance)))))

(defn get-instance-stats
  "Get statistics for an instance."
  [service-id instance-id]
  (when-let [instance (get-instance service-id instance-id)]
    {:id instance-id
     :service-id service-id
     :address (:address instance)
     :status (:status instance)
     :weight (:weight instance)
     :registered-at (:registered-at instance)
     :last-heartbeat (.get ^AtomicLong (:last-heartbeat instance))
     :consecutive-failures (.get ^AtomicLong (:consecutive-failures instance))
     :consecutive-successes (.get ^AtomicLong (:consecutive-successes instance))
     :total-requests (.get ^AtomicLong (:total-requests instance))
     :failed-requests (.get ^AtomicLong (:failed-requests instance))
     :success-rate (let [total (.get ^AtomicLong (:total-requests instance))]
                     (if (pos? total)
                       (double (/ (- total (.get ^AtomicLong (:failed-requests instance))) total))
                       1.0))}))

(defn get-service-stats
  "Get statistics for a service."
  [service-id]
  (when-let [service (get-service service-id)]
    (let [instances (list-instances service-id)
          healthy-count (count (filter #(= (:status %) :healthy) instances))
          unhealthy-count (count (filter #(= (:status %) :unhealthy) instances))]
      {:id service-id
       :name (:name service)
       :version (:version service)
       :total-instances (count instances)
       :healthy-instances healthy-count
       :unhealthy-instances unhealthy-count
       :instances (mapv #(get-instance-stats service-id (:id %)) instances)})))

(defn get-all-service-stats
  "Get statistics for all services."
  []
  (into {} (for [service-id (keys (:services @registry-state))]
             [service-id (get-service-stats service-id)])))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-health-check-scheduler!
  "Start the health check scheduler."
  []
  (log/info "Starting health check scheduler")
  (let [interval (get-in @registry-state [:config :health-check-interval-ms])
        scheduler (ScheduledThreadPoolExecutor. 1)]
    (.scheduleAtFixedRate scheduler
                          (fn []
                            (try
                              (check-all-health!)
                              (catch Exception e
                                (log/error "Health check scheduler error" {:error (.getMessage e)}))))
                          interval
                          interval
                          TimeUnit/MILLISECONDS)
    (swap! registry-state assoc :scheduler scheduler)))

(defn stop-health-check-scheduler!
  "Stop the health check scheduler."
  []
  (when-let [scheduler (:scheduler @registry-state)]
    (log/info "Stopping health check scheduler")
    (.shutdown ^ScheduledThreadPoolExecutor scheduler)
    (swap! registry-state assoc :scheduler nil)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-service-registry!
  "Initialize service registry."
  []
  (log/info "Initializing service registry")
  ;; Register feature flag
  (flags/register-flag! "service-registry" "Enable service registry" true)
  ;; Create metrics
  (metrics/create-counter! :serviceregistry/services-registered "Services registered")
  (metrics/create-counter! :serviceregistry/services-deregistered "Services deregistered")
  (metrics/create-counter! :serviceregistry/instances-registered "Instances registered")
  (metrics/create-counter! :serviceregistry/instances-deregistered "Instances deregistered")
  (metrics/create-counter! :serviceregistry/heartbeats-received "Heartbeats received")
  (metrics/create-counter! :serviceregistry/health-checks-performed "Health checks performed")
  (metrics/create-gauge! :serviceregistry/total-services "Total services"
                         #(.size ^ConcurrentHashMap (:services @registry-state)))
  ;; Start scheduler
  (start-health-check-scheduler!)
  (log/info "Service registry initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-registry-status []
  {:enabled (flags/is-enabled? "service-registry")
   :services (.size ^ConcurrentHashMap (:services @registry-state))
   :total-instances (reduce + (map #(.size ^ConcurrentHashMap %) (vals (:instances @registry-state))))
   :health-checks (.size ^ConcurrentHashMap (:health-checks @registry-state))
   :watchers (count (apply concat (vals (:watchers @registry-state))))
   :scheduler-running (some? (:scheduler @registry-state))
   :stats (get-all-service-stats)
   :config (:config @registry-state)})
