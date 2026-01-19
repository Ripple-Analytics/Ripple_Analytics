(ns mental-models.pipeline.integration.load-balancer
  "Load Balancer Module
   
   Request distribution:
   - Multiple algorithms (round-robin, weighted, least-connections, random)
   - Health-aware routing
   - Sticky sessions
   - Failover support
   - Dynamic backend management"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util.concurrent ConcurrentHashMap Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; LOAD BALANCER STATE
;; =============================================================================

(defonce balancer-state (atom {:balancers {}
                               :scheduler nil
                               :config {:default-algorithm :round-robin
                                        :health-check-interval-ms 10000
                                        :failure-threshold 3
                                        :recovery-threshold 2}}))

;; =============================================================================
;; ALGORITHMS
;; =============================================================================

(def algorithms #{:round-robin :weighted :least-connections :random :ip-hash})

;; =============================================================================
;; BACKEND MANAGEMENT
;; =============================================================================

(defn create-backend
  "Create a backend server."
  [backend-id {:keys [host port weight health-check-fn]}]
  {:id backend-id
   :host host
   :port port
   :weight (or weight 1)
   :health-check-fn health-check-fn
   :healthy? (AtomicBoolean. true)
   :active-connections (AtomicLong. 0)
   :total-requests (AtomicLong. 0)
   :failed-requests (AtomicLong. 0)
   :consecutive-failures (AtomicLong. 0)
   :consecutive-successes (AtomicLong. 0)
   :last-health-check (AtomicLong. 0)
   :created-at (System/currentTimeMillis)})

(defn is-backend-healthy?
  "Check if a backend is healthy."
  [backend]
  (.get ^AtomicBoolean (:healthy? backend)))

(defn mark-backend-healthy!
  "Mark a backend as healthy."
  [backend]
  (.set ^AtomicBoolean (:healthy? backend) true)
  (.set ^AtomicLong (:consecutive-failures backend) 0))

(defn mark-backend-unhealthy!
  "Mark a backend as unhealthy."
  [backend]
  (.set ^AtomicBoolean (:healthy? backend) false)
  (.set ^AtomicLong (:consecutive-successes backend) 0))

(defn increment-connections!
  "Increment active connections for a backend."
  [backend]
  (.incrementAndGet ^AtomicLong (:active-connections backend)))

(defn decrement-connections!
  "Decrement active connections for a backend."
  [backend]
  (.decrementAndGet ^AtomicLong (:active-connections backend)))

(defn record-request!
  "Record a request to a backend."
  [backend success?]
  (.incrementAndGet ^AtomicLong (:total-requests backend))
  (if success?
    (do
      (.set ^AtomicLong (:consecutive-failures backend) 0)
      (.incrementAndGet ^AtomicLong (:consecutive-successes backend)))
    (do
      (.incrementAndGet ^AtomicLong (:failed-requests backend))
      (.set ^AtomicLong (:consecutive-successes backend) 0)
      (.incrementAndGet ^AtomicLong (:consecutive-failures backend)))))

;; =============================================================================
;; LOAD BALANCER CREATION
;; =============================================================================

(defn create-balancer
  "Create a new load balancer."
  [balancer-id {:keys [algorithm sticky-sessions session-ttl-ms]}]
  (let [config (:config @balancer-state)]
    {:id balancer-id
     :algorithm (or algorithm (:default-algorithm config))
     :backends (ConcurrentHashMap.)
     :round-robin-counter (AtomicLong. 0)
     :sticky-sessions (or sticky-sessions false)
     :session-ttl-ms (or session-ttl-ms 3600000)
     :session-map (ConcurrentHashMap.)
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; BALANCER REGISTRATION
;; =============================================================================

(defn register-balancer!
  "Register a load balancer."
  [balancer-id opts]
  (log/info "Registering load balancer" {:id balancer-id :algorithm (:algorithm opts)})
  (let [balancer (create-balancer balancer-id opts)]
    (swap! balancer-state assoc-in [:balancers balancer-id] balancer)
    (metrics/inc-counter! :loadbalancer/balancers-registered)
    balancer-id))

(defn unregister-balancer!
  "Unregister a load balancer."
  [balancer-id]
  (log/info "Unregistering load balancer" {:id balancer-id})
  (swap! balancer-state update :balancers dissoc balancer-id))

(defn get-balancer
  "Get a load balancer."
  [balancer-id]
  (get-in @balancer-state [:balancers balancer-id]))

(defn list-balancers
  "List all load balancers."
  []
  (keys (:balancers @balancer-state)))

;; =============================================================================
;; BACKEND REGISTRATION
;; =============================================================================

(defn add-backend!
  "Add a backend to a load balancer."
  [balancer-id backend-id opts]
  (when-let [balancer (get-balancer balancer-id)]
    (log/info "Adding backend" {:balancer balancer-id :backend backend-id})
    (let [backend (create-backend backend-id opts)]
      (.put ^ConcurrentHashMap (:backends balancer) backend-id backend)
      (metrics/inc-counter! :loadbalancer/backends-added)
      (events/publish! :loadbalancer/backend-added {:balancer balancer-id :backend backend-id})
      backend-id)))

(defn remove-backend!
  "Remove a backend from a load balancer."
  [balancer-id backend-id]
  (when-let [balancer (get-balancer balancer-id)]
    (log/info "Removing backend" {:balancer balancer-id :backend backend-id})
    (.remove ^ConcurrentHashMap (:backends balancer) backend-id)
    (events/publish! :loadbalancer/backend-removed {:balancer balancer-id :backend backend-id})))

(defn get-backend
  "Get a backend from a load balancer."
  [balancer-id backend-id]
  (when-let [balancer (get-balancer balancer-id)]
    (.get ^ConcurrentHashMap (:backends balancer) backend-id)))

(defn list-backends
  "List all backends for a load balancer."
  [balancer-id]
  (when-let [balancer (get-balancer balancer-id)]
    (vec (.keySet ^ConcurrentHashMap (:backends balancer)))))

(defn healthy-backends
  "Get all healthy backends for a load balancer."
  [balancer-id]
  (when-let [balancer (get-balancer balancer-id)]
    (filter is-backend-healthy? (vals (:backends balancer)))))

;; =============================================================================
;; SELECTION ALGORITHMS
;; =============================================================================

(defn select-round-robin
  "Select a backend using round-robin."
  [balancer backends]
  (let [counter (:round-robin-counter balancer)
        idx (mod (.getAndIncrement ^AtomicLong counter) (count backends))]
    (nth backends idx)))

(defn select-weighted
  "Select a backend using weighted random."
  [_ backends]
  (let [total-weight (reduce + (map :weight backends))
        random-weight (rand total-weight)]
    (loop [cumulative 0
           remaining backends]
      (if (empty? remaining)
        (last backends)
        (let [backend (first remaining)
              new-cumulative (+ cumulative (:weight backend))]
          (if (< random-weight new-cumulative)
            backend
            (recur new-cumulative (rest remaining))))))))

(defn select-least-connections
  "Select a backend with least active connections."
  [_ backends]
  (apply min-key #(.get ^AtomicLong (:active-connections %)) backends))

(defn select-random
  "Select a random backend."
  [_ backends]
  (rand-nth backends))

(defn select-ip-hash
  "Select a backend based on client IP hash."
  [_ backends client-ip]
  (let [idx (mod (Math/abs (hash client-ip)) (count backends))]
    (nth backends idx)))

(defn select-backend
  "Select a backend based on the algorithm."
  [balancer-id & {:keys [client-ip session-id]}]
  (when-let [balancer (get-balancer balancer-id)]
    (let [backends (vec (healthy-backends balancer-id))]
      (when (seq backends)
        ;; Check sticky session first
        (if (and (:sticky-sessions balancer) session-id)
          (let [session-map ^ConcurrentHashMap (:session-map balancer)
                cached-backend-id (.get session-map session-id)]
            (if (and cached-backend-id
                     (get-backend balancer-id cached-backend-id)
                     (is-backend-healthy? (get-backend balancer-id cached-backend-id)))
              (get-backend balancer-id cached-backend-id)
              ;; Select new backend and cache
              (let [backend (case (:algorithm balancer)
                              :round-robin (select-round-robin balancer backends)
                              :weighted (select-weighted balancer backends)
                              :least-connections (select-least-connections balancer backends)
                              :random (select-random balancer backends)
                              :ip-hash (select-ip-hash balancer backends client-ip)
                              (select-round-robin balancer backends))]
                (.put session-map session-id (:id backend))
                backend)))
          ;; No sticky session
          (case (:algorithm balancer)
            :round-robin (select-round-robin balancer backends)
            :weighted (select-weighted balancer backends)
            :least-connections (select-least-connections balancer backends)
            :random (select-random balancer backends)
            :ip-hash (select-ip-hash balancer backends client-ip)
            (select-round-robin balancer backends)))))))

;; =============================================================================
;; REQUEST ROUTING
;; =============================================================================

(defn route-request
  "Route a request through the load balancer."
  [balancer-id request-fn & {:keys [client-ip session-id timeout-ms]}]
  (if-let [backend (select-backend balancer-id :client-ip client-ip :session-id session-id)]
    (do
      (increment-connections! backend)
      (metrics/inc-counter! :loadbalancer/requests-routed)
      (try
        (let [result (if timeout-ms
                       (deref (future (request-fn backend)) timeout-ms ::timeout)
                       (request-fn backend))]
          (if (= result ::timeout)
            (do
              (record-request! backend false)
              (throw (ex-info "Request timeout" {:backend (:id backend)})))
            (do
              (record-request! backend true)
              result)))
        (catch Exception e
          (record-request! backend false)
          (throw e))
        (finally
          (decrement-connections! backend))))
    (throw (ex-info "No healthy backends available" {:balancer balancer-id}))))

(defmacro with-backend
  "Execute body with a selected backend."
  [[backend-sym balancer-id & opts] & body]
  `(let [~backend-sym (select-backend ~balancer-id ~@opts)]
     (when ~backend-sym
       (increment-connections! ~backend-sym)
       (try
         ~@body
         (finally
           (decrement-connections! ~backend-sym))))))

;; =============================================================================
;; HEALTH CHECKING
;; =============================================================================

(defn check-backend-health!
  "Check the health of a backend."
  [balancer-id backend-id]
  (when-let [backend (get-backend balancer-id backend-id)]
    (let [config (:config @balancer-state)
          health-check-fn (:health-check-fn backend)]
      (.set ^AtomicLong (:last-health-check backend) (System/currentTimeMillis))
      (if health-check-fn
        (try
          (if (health-check-fn backend)
            (do
              (.incrementAndGet ^AtomicLong (:consecutive-successes backend))
              (.set ^AtomicLong (:consecutive-failures backend) 0)
              (when (and (not (is-backend-healthy? backend))
                         (>= (.get ^AtomicLong (:consecutive-successes backend))
                             (:recovery-threshold config)))
                (mark-backend-healthy! backend)
                (log/info "Backend recovered" {:balancer balancer-id :backend backend-id})
                (events/publish! :loadbalancer/backend-recovered {:balancer balancer-id :backend backend-id})))
            (do
              (.incrementAndGet ^AtomicLong (:consecutive-failures backend))
              (.set ^AtomicLong (:consecutive-successes backend) 0)
              (when (and (is-backend-healthy? backend)
                         (>= (.get ^AtomicLong (:consecutive-failures backend))
                             (:failure-threshold config)))
                (mark-backend-unhealthy! backend)
                (log/warn "Backend unhealthy" {:balancer balancer-id :backend backend-id})
                (events/publish! :loadbalancer/backend-unhealthy {:balancer balancer-id :backend backend-id}))))
          (catch Exception e
            (log/error "Health check failed" {:backend backend-id :error (.getMessage e)})
            (.incrementAndGet ^AtomicLong (:consecutive-failures backend))))
        ;; No health check function, assume healthy
        true))))

(defn check-all-backends-health!
  "Check the health of all backends."
  [balancer-id]
  (doseq [backend-id (list-backends balancer-id)]
    (check-backend-health! balancer-id backend-id)))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-health-checker!
  "Start the health check scheduler."
  []
  (when (and (flags/is-enabled? "load-balancer")
             (nil? (:scheduler @balancer-state)))
    (log/info "Starting load balancer health checker")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @balancer-state [:config :health-check-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (doseq [balancer-id (list-balancers)]
                                 (check-all-backends-health! balancer-id))
                               (catch Exception e
                                 (log/error "Health check error" {:error (.getMessage e)})))
                            0
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! balancer-state assoc :scheduler executor))))

(defn stop-health-checker!
  "Stop the health check scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @balancer-state)]
    (log/info "Stopping load balancer health checker")
    (.shutdown executor)
    (swap! balancer-state assoc :scheduler nil)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-backend-stats
  "Get statistics for a backend."
  [balancer-id backend-id]
  (when-let [backend (get-backend balancer-id backend-id)]
    {:id backend-id
     :host (:host backend)
     :port (:port backend)
     :weight (:weight backend)
     :healthy? (is-backend-healthy? backend)
     :active-connections (.get ^AtomicLong (:active-connections backend))
     :total-requests (.get ^AtomicLong (:total-requests backend))
     :failed-requests (.get ^AtomicLong (:failed-requests backend))
     :success-rate (let [total (.get ^AtomicLong (:total-requests backend))
                         failed (.get ^AtomicLong (:failed-requests backend))]
                     (if (pos? total)
                       (* 100.0 (/ (- total failed) total))
                       100.0))}))

(defn get-balancer-stats
  "Get statistics for a load balancer."
  [balancer-id]
  (when-let [balancer (get-balancer balancer-id)]
    {:id balancer-id
     :algorithm (:algorithm balancer)
     :sticky-sessions (:sticky-sessions balancer)
     :total-backends (count (list-backends balancer-id))
     :healthy-backends (count (healthy-backends balancer-id))
     :backends (into {} (for [backend-id (list-backends balancer-id)]
                          [backend-id (get-backend-stats balancer-id backend-id)]))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-load-balancer!
  "Initialize load balancer."
  []
  (log/info "Initializing load balancer")
  ;; Register feature flag
  (flags/register-flag! "load-balancer" "Enable load balancer" true)
  ;; Create metrics
  (metrics/create-counter! :loadbalancer/balancers-registered "Balancers registered")
  (metrics/create-counter! :loadbalancer/backends-added "Backends added")
  (metrics/create-counter! :loadbalancer/requests-routed "Requests routed")
  (metrics/create-gauge! :loadbalancer/total-balancers "Total balancers"
                         #(count (:balancers @balancer-state)))
  (log/info "Load balancer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-balancer-status []
  {:enabled (flags/is-enabled? "load-balancer")
   :balancers (count (:balancers @balancer-state))
   :scheduler-active (some? (:scheduler @balancer-state))
   :stats (into {} (for [balancer-id (list-balancers)]
                     [balancer-id (get-balancer-stats balancer-id)]))
   :config (:config @balancer-state)})
