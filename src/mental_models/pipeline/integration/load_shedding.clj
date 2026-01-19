(ns mental-models.pipeline.integration.load-shedding
  "Load shedding for mental model analysis system.
   
   Features:
   - Adaptive load shedding
   - Priority-based shedding
   - Queue depth monitoring
   - CPU/memory-based shedding
   - Graceful degradation
   - Shedding policies
   - Recovery detection
   - Shedding metrics"
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
           [java.lang.management ManagementFactory]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:policies {}         ;; policy-id -> shedding-policy
         :config {:cpu-threshold 0.8
                  :memory-threshold 0.85
                  :queue-depth-threshold 1000
                  :check-interval-ms 1000
                  :recovery-window-ms 10000
                  :min-accept-rate 0.1}
         :current-load {:cpu 0.0
                        :memory 0.0
                        :queue-depth 0}
         :shedding-active? (atom false)
         :accept-rate (atom 1.0)
         :stats {:requests-received 0
                 :requests-accepted 0
                 :requests-shed 0
                 :shedding-events 0
                 :recovery-events 0}
         :initialized? false}))

;; ============================================================================
;; System Metrics
;; ============================================================================

(defn- get-cpu-load
  "Get current CPU load."
  []
  (try
    (let [os-bean (ManagementFactory/getOperatingSystemMXBean)]
      (if (instance? com.sun.management.OperatingSystemMXBean os-bean)
        (.getSystemCpuLoad ^com.sun.management.OperatingSystemMXBean os-bean)
        0.5))
    (catch Exception _ 0.5)))

(defn- get-memory-usage
  "Get current memory usage ratio."
  []
  (let [runtime (Runtime/getRuntime)
        max-memory (.maxMemory runtime)
        used-memory (- (.totalMemory runtime) (.freeMemory runtime))]
    (/ (double used-memory) (double max-memory))))

(defn- update-load-metrics!
  "Update current load metrics."
  []
  (swap! state assoc :current-load
         {:cpu (get-cpu-load)
          :memory (get-memory-usage)
          :queue-depth (get-in @state [:current-load :queue-depth] 0)
          :timestamp (System/currentTimeMillis)}))

;; ============================================================================
;; Shedding Policies
;; ============================================================================

(defn create-policy!
  "Create a load shedding policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :priority (get config :priority 5) ;; 1-10, higher = more important
                :cpu-threshold (get config :cpu-threshold
                                    (get-in @state [:config :cpu-threshold]))
                :memory-threshold (get config :memory-threshold
                                       (get-in @state [:config :memory-threshold]))
                :queue-threshold (get config :queue-threshold
                                      (get-in @state [:config :queue-depth-threshold]))
                :shed-probability (get config :shed-probability 0.5)
                :on-shed (get config :on-shed)
                :fallback-fn (get config :fallback-fn)
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created shedding policy" {:policy-id policy-id :priority (:priority policy)})
    policy-id))

(defn get-policy
  "Get a shedding policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all shedding policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :priority (:priority p)
           :enabled? @(:enabled? p)})
        (:policies @state)))

;; ============================================================================
;; Shedding Decision
;; ============================================================================

(defn- calculate-shed-probability
  "Calculate probability of shedding based on load."
  [policy]
  (let [load (:current-load @state)
        cpu-ratio (/ (:cpu load) (:cpu-threshold policy))
        memory-ratio (/ (:memory load) (:memory-threshold policy))
        queue-ratio (/ (:queue-depth load) (:queue-threshold policy))
        max-ratio (max cpu-ratio memory-ratio queue-ratio)]
    (if (< max-ratio 1.0)
      0.0 ;; Under threshold, no shedding
      (min 1.0 (* (:shed-probability policy) (- max-ratio 0.9))))))

(defn- should-shed?
  "Determine if a request should be shed."
  [policy]
  (when @(:enabled? policy)
    (let [shed-prob (calculate-shed-probability policy)
          priority-factor (/ (- 11 (:priority policy)) 10.0) ;; Higher priority = lower shed chance
          adjusted-prob (* shed-prob priority-factor)]
      (< (rand) adjusted-prob))))

(defn- adaptive-accept-rate
  "Calculate adaptive accept rate based on system load."
  []
  (let [load (:current-load @state)
        config (:config @state)
        cpu-factor (if (> (:cpu load) (:cpu-threshold config))
                     (- 1.0 (/ (- (:cpu load) (:cpu-threshold config))
                               (- 1.0 (:cpu-threshold config))))
                     1.0)
        memory-factor (if (> (:memory load) (:memory-threshold config))
                        (- 1.0 (/ (- (:memory load) (:memory-threshold config))
                                  (- 1.0 (:memory-threshold config))))
                        1.0)]
    (max (:min-accept-rate config)
         (min 1.0 (* cpu-factor memory-factor)))))

;; ============================================================================
;; Load Shedding Execution
;; ============================================================================

(defn try-accept
  "Try to accept a request, may shed based on load."
  [policy-id]
  (swap! state update-in [:stats :requests-received] inc)
  
  (if-let [policy (get-policy policy-id)]
    (if (should-shed? policy)
      (do
        (swap! state update-in [:stats :requests-shed] inc)
        (logging/log :debug "Request shed" {:policy-id policy-id})
        (when-let [on-shed (:on-shed policy)]
          (try (on-shed) (catch Exception _)))
        {:accepted? false :reason :load-shedding})
      (do
        (swap! state update-in [:stats :requests-accepted] inc)
        {:accepted? true}))
    (do
      (swap! state update-in [:stats :requests-accepted] inc)
      {:accepted? true})))

(defn execute-with-shedding
  "Execute a function with load shedding protection."
  [policy-id f]
  (let [result (try-accept policy-id)]
    (if (:accepted? result)
      (f)
      (if-let [policy (get-policy policy-id)]
        (if-let [fallback (:fallback-fn policy)]
          (fallback)
          (throw (ex-info "Request shed due to load" {:policy-id policy-id})))
        (throw (ex-info "Request shed due to load" {:policy-id policy-id}))))))

(defmacro with-load-shedding
  "Execute body with load shedding protection."
  [policy-id & body]
  `(execute-with-shedding ~policy-id (fn [] ~@body)))

;; ============================================================================
;; Adaptive Shedding
;; ============================================================================

(defn- update-accept-rate!
  "Update the adaptive accept rate."
  []
  (let [new-rate (adaptive-accept-rate)
        old-rate @(:accept-rate @state)]
    (reset! (:accept-rate @state) new-rate)
    
    ;; Detect shedding start/stop
    (when (and (< new-rate 1.0) (= old-rate 1.0))
      (reset! (:shedding-active? @state) true)
      (swap! state update-in [:stats :shedding-events] inc)
      (logging/log :warn "Load shedding activated" {:accept-rate new-rate})
      (events/emit! :shedding-activated {:accept-rate new-rate}))
    
    (when (and (= new-rate 1.0) (< old-rate 1.0))
      (reset! (:shedding-active? @state) false)
      (swap! state update-in [:stats :recovery-events] inc)
      (logging/log :info "Load shedding deactivated" {})
      (events/emit! :shedding-deactivated {}))))

(defn try-accept-adaptive
  "Try to accept using adaptive rate limiting."
  []
  (swap! state update-in [:stats :requests-received] inc)
  (let [accept-rate @(:accept-rate @state)]
    (if (< (rand) accept-rate)
      (do
        (swap! state update-in [:stats :requests-accepted] inc)
        {:accepted? true :accept-rate accept-rate})
      (do
        (swap! state update-in [:stats :requests-shed] inc)
        {:accepted? false :reason :adaptive-shedding :accept-rate accept-rate}))))

;; ============================================================================
;; Queue Depth Management
;; ============================================================================

(defn update-queue-depth!
  "Update the current queue depth."
  [depth]
  (swap! state assoc-in [:current-load :queue-depth] depth))

(defn increment-queue-depth!
  "Increment queue depth."
  []
  (swap! state update-in [:current-load :queue-depth] inc))

(defn decrement-queue-depth!
  "Decrement queue depth."
  []
  (swap! state update-in [:current-load :queue-depth] #(max 0 (dec %))))

;; ============================================================================
;; Priority-Based Shedding
;; ============================================================================

(defn try-accept-with-priority
  "Try to accept based on request priority."
  [priority] ;; 1-10, higher = more important
  (swap! state update-in [:stats :requests-received] inc)
  (let [accept-rate @(:accept-rate @state)
        priority-boost (/ priority 10.0)
        effective-rate (min 1.0 (+ accept-rate (* (- 1.0 accept-rate) priority-boost)))]
    (if (< (rand) effective-rate)
      (do
        (swap! state update-in [:stats :requests-accepted] inc)
        {:accepted? true :priority priority :effective-rate effective-rate})
      (do
        (swap! state update-in [:stats :requests-shed] inc)
        {:accepted? false :reason :priority-shedding :priority priority}))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn load-shedding-middleware
  "Ring middleware for load shedding."
  [handler & {:keys [policy-id priority-header]}]
  (fn [request]
    (let [priority (when priority-header
                     (some-> (get-in request [:headers priority-header])
                             Integer/parseInt))
          result (if priority
                   (try-accept-with-priority priority)
                   (try-accept-adaptive))]
      (if (:accepted? result)
        (handler request)
        {:status 503
         :headers {"Retry-After" "5"
                   "X-Shed-Reason" (name (:reason result))}
         :body "Service temporarily unavailable due to high load"}))))

;; ============================================================================
;; Background Monitoring
;; ============================================================================

(defn- start-load-monitor!
  "Start the background load monitor."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :check-interval-ms])))
    (update-load-metrics!)
    (update-accept-rate!)
    (recur)))

;; ============================================================================
;; Policy Management
;; ============================================================================

(defn enable-policy!
  "Enable a shedding policy."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    (reset! (:enabled? policy) true)
    (logging/log :info "Enabled shedding policy" {:policy-id policy-id})))

(defn disable-policy!
  "Disable a shedding policy."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    (reset! (:enabled? policy) false)
    (logging/log :info "Disabled shedding policy" {:policy-id policy-id})))

(defn set-thresholds!
  "Set global shedding thresholds."
  [& {:keys [cpu memory queue-depth]}]
  (when cpu
    (swap! state assoc-in [:config :cpu-threshold] cpu))
  (when memory
    (swap! state assoc-in [:config :memory-threshold] memory))
  (when queue-depth
    (swap! state assoc-in [:config :queue-depth-threshold] queue-depth)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-shedding-stats
  "Get load shedding statistics."
  []
  (let [stats (:stats @state)
        received (:requests-received stats)]
    {:current-load (:current-load @state)
     :shedding-active? @(:shedding-active? @state)
     :accept-rate @(:accept-rate @state)
     :requests-received received
     :requests-accepted (:requests-accepted stats)
     :requests-shed (:requests-shed stats)
     :shed-rate (if (pos? received)
                  (/ (:requests-shed stats) received)
                  0)
     :shedding-events (:shedding-events stats)
     :recovery-events (:recovery-events stats)
     :policies-count (count (:policies @state))}))

(defn reset-stats!
  "Reset shedding statistics."
  []
  (swap! state assoc :stats {:requests-received 0
                             :requests-accepted 0
                             :requests-shed 0
                             :shedding-events 0
                             :recovery-events 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-load-shedding!
  "Initialize the load shedding system."
  []
  (when-not (:initialized? @state)
    ;; Start load monitor
    (start-load-monitor!)
    
    ;; Create default policies
    (create-policy! :critical
                    {:name "Critical"
                     :priority 10
                     :cpu-threshold 0.95
                     :memory-threshold 0.95})
    
    (create-policy! :high
                    {:name "High Priority"
                     :priority 8
                     :cpu-threshold 0.85
                     :memory-threshold 0.9})
    
    (create-policy! :normal
                    {:name "Normal"
                     :priority 5
                     :cpu-threshold 0.8
                     :memory-threshold 0.85})
    
    (create-policy! :low
                    {:name "Low Priority"
                     :priority 2
                     :cpu-threshold 0.7
                     :memory-threshold 0.8})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Load shedding initialized")
    (events/emit! :load-shedding-initialized {})
    true))
