(ns mental-models.pipeline.integration.request-throttler
  "Request throttler for mental model analysis system.
   
   Features:
   - Concurrent request limiting
   - Request queuing
   - Priority-based throttling
   - Adaptive throttling
   - Throttle policies
   - Request coalescing
   - Throttle metrics
   - Fair scheduling"
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
           [java.util.concurrent Semaphore TimeUnit PriorityBlockingQueue
            LinkedBlockingQueue Executors ScheduledExecutorService]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:throttlers {}       ;; throttler-id -> throttler
         :config {:default-max-concurrent 10
                  :default-queue-size 1000
                  :default-timeout-ms 30000}
         :stats {:requests-received 0
                 :requests-executed 0
                 :requests-queued 0
                 :requests-rejected 0
                 :requests-timed-out 0}
         :initialized? false}))

;; ============================================================================
;; Priority Request
;; ============================================================================

(defrecord PriorityRequest [priority timestamp request-id request callback]
  Comparable
  (compareTo [this other]
    (let [priority-cmp (compare (:priority other) (:priority this))]
      (if (zero? priority-cmp)
        (compare (:timestamp this) (:timestamp other))
        priority-cmp))))

;; ============================================================================
;; Throttler Creation
;; ============================================================================

(defn create-throttler!
  "Create a request throttler."
  [throttler-id config]
  (let [max-concurrent (get config :max-concurrent
                            (get-in @state [:config :default-max-concurrent]))
        queue-size (get config :queue-size
                        (get-in @state [:config :default-queue-size]))
        timeout-ms (get config :timeout-ms
                        (get-in @state [:config :default-timeout-ms]))
        use-priority? (get config :use-priority? false)
        
        throttler {:id throttler-id
                   :name (get config :name (name throttler-id))
                   :max-concurrent max-concurrent
                   :queue-size queue-size
                   :timeout-ms timeout-ms
                   :use-priority? use-priority?
                   :semaphore (Semaphore. max-concurrent true)
                   :queue (if use-priority?
                            (PriorityBlockingQueue. queue-size)
                            (LinkedBlockingQueue. queue-size))
                   :active-count (atom 0)
                   :on-reject (get config :on-reject)
                   :on-timeout (get config :on-timeout)
                   :enabled? (atom true)
                   :paused? (atom false)
                   :metrics {:received (atom 0)
                             :executed (atom 0)
                             :queued (atom 0)
                             :rejected (atom 0)
                             :timed-out (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:throttlers throttler-id] throttler)
    (logging/log :info "Created throttler" {:throttler-id throttler-id :max-concurrent max-concurrent})
    (events/emit! :throttler-created {:throttler-id throttler-id})
    throttler-id))

(defn get-throttler
  "Get a throttler."
  [throttler-id]
  (get-in @state [:throttlers throttler-id]))

(defn list-throttlers
  "List all throttlers."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :max-concurrent (:max-concurrent t)
           :active-count @(:active-count t)
           :queue-size (.size (:queue t))
           :enabled? @(:enabled? t)})
        (:throttlers @state)))

;; ============================================================================
;; Request Execution
;; ============================================================================

(defn- try-acquire
  "Try to acquire a permit from the throttler."
  [throttler timeout-ms]
  (if (pos? timeout-ms)
    (.tryAcquire (:semaphore throttler) timeout-ms TimeUnit/MILLISECONDS)
    (.tryAcquire (:semaphore throttler))))

(defn- release-permit
  "Release a permit back to the throttler."
  [throttler]
  (.release (:semaphore throttler))
  (swap! (:active-count throttler) dec))

(defn- execute-request
  "Execute a request with the throttler."
  [throttler f]
  (swap! (:active-count throttler) inc)
  (try
    (let [result (f)]
      (swap! (get-in throttler [:metrics :executed]) inc)
      (swap! state update-in [:stats :requests-executed] inc)
      result)
    (finally
      (release-permit throttler))))

(defn throttle
  "Execute a function with throttling."
  [throttler-id f & {:keys [priority timeout-ms]}]
  (swap! state update-in [:stats :requests-received] inc)
  
  (if-let [throttler (get-throttler throttler-id)]
    (if-not @(:enabled? throttler)
      (f) ;; Throttler disabled, execute directly
      (do
        (swap! (get-in throttler [:metrics :received]) inc)
        (let [effective-timeout (or timeout-ms (:timeout-ms throttler))]
          (if (try-acquire throttler effective-timeout)
            (execute-request throttler f)
            (do
              (swap! (get-in throttler [:metrics :timed-out]) inc)
              (swap! state update-in [:stats :requests-timed-out] inc)
              (when-let [on-timeout (:on-timeout throttler)]
                (on-timeout))
              (throw (ex-info "Throttle timeout" {:throttler-id throttler-id})))))))
    (f)))

(defmacro with-throttle
  "Execute body with throttling."
  [throttler-id & body]
  `(throttle ~throttler-id (fn [] ~@body)))

;; ============================================================================
;; Async Throttling
;; ============================================================================

(defn throttle-async
  "Execute a function with throttling asynchronously."
  [throttler-id f & {:keys [priority callback]}]
  (go
    (try
      (let [result (throttle throttler-id f :priority priority)]
        (when callback
          (callback {:status :success :result result}))
        result)
      (catch Exception e
        (when callback
          (callback {:status :error :error e}))
        nil))))

(defn- process-queue!
  "Process queued requests."
  [throttler]
  (go-loop []
    (when @(:enabled? throttler)
      (when-not @(:paused? throttler)
        (when-let [request (.poll (:queue throttler) 100 TimeUnit/MILLISECONDS)]
          (when (try-acquire throttler 0)
            (try
              (let [result (execute-request throttler (:request request))]
                (when-let [callback (:callback request)]
                  (callback {:status :success :result result})))
              (catch Exception e
                (when-let [callback (:callback request)]
                  (callback {:status :error :error e})))))))
      (recur))))

(defn queue-request
  "Queue a request for later execution."
  [throttler-id f & {:keys [priority callback]}]
  (if-let [throttler (get-throttler throttler-id)]
    (let [request (->PriorityRequest
                   (or priority 5)
                   (System/currentTimeMillis)
                   (str (UUID/randomUUID))
                   f
                   callback)]
      (if (.offer (:queue throttler) request)
        (do
          (swap! (get-in throttler [:metrics :queued]) inc)
          (swap! state update-in [:stats :requests-queued] inc)
          {:queued? true :request-id (:request-id request)})
        (do
          (swap! (get-in throttler [:metrics :rejected]) inc)
          (swap! state update-in [:stats :requests-rejected] inc)
          (when-let [on-reject (:on-reject throttler)]
            (on-reject))
          {:queued? false :reason :queue-full})))
    {:queued? false :reason :throttler-not-found}))

;; ============================================================================
;; Request Coalescing
;; ============================================================================

(defonce ^:private coalesce-state
  (atom {:pending {}          ;; key -> {:requests [] :result-promise promise}
         :config {:coalesce-window-ms 100}}))

(defn coalesce-request
  "Coalesce similar requests within a time window."
  [throttler-id coalesce-key f]
  (let [pending (:pending @coalesce-state)]
    (if-let [existing (get pending coalesce-key)]
      ;; Add to existing coalesced request
      (let [result-promise (:result-promise existing)]
        (swap! coalesce-state update-in [:pending coalesce-key :requests] conj f)
        @result-promise)
      ;; Create new coalesced request
      (let [result-promise (promise)]
        (swap! coalesce-state assoc-in [:pending coalesce-key]
               {:requests [f]
                :result-promise result-promise
                :created-at (System/currentTimeMillis)})
        ;; Schedule execution after coalesce window
        (go
          (<! (timeout (get-in @coalesce-state [:config :coalesce-window-ms])))
          (let [requests (get-in @coalesce-state [:pending coalesce-key :requests])]
            (swap! coalesce-state update :pending dissoc coalesce-key)
            (try
              (let [result (throttle throttler-id (first requests))]
                (deliver result-promise result))
              (catch Exception e
                (deliver result-promise e)))))
        @result-promise))))

;; ============================================================================
;; Adaptive Throttling
;; ============================================================================

(defn- calculate-adaptive-limit
  "Calculate adaptive concurrency limit based on metrics."
  [throttler]
  (let [executed @(get-in throttler [:metrics :executed])
        timed-out @(get-in throttler [:metrics :timed-out])
        total (+ executed timed-out)
        success-rate (if (pos? total) (/ executed total) 1.0)
        current-limit (:max-concurrent throttler)]
    (cond
      (and (> success-rate 0.95) (< current-limit 100))
      (min 100 (int (* current-limit 1.1)))
      
      (< success-rate 0.8)
      (max 1 (int (* current-limit 0.9)))
      
      :else current-limit)))

(defn adjust-throttle-limit!
  "Adjust the throttle limit based on performance."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (let [new-limit (calculate-adaptive-limit throttler)
          old-limit (:max-concurrent throttler)]
      (when (not= new-limit old-limit)
        (let [diff (- new-limit old-limit)
              semaphore (:semaphore throttler)]
          (if (pos? diff)
            (.release semaphore diff)
            (dotimes [_ (Math/abs diff)]
              (.tryAcquire semaphore)))
          (swap! state assoc-in [:throttlers throttler-id :max-concurrent] new-limit)
          (logging/log :info "Adjusted throttle limit" {:throttler-id throttler-id
                                                         :old-limit old-limit
                                                         :new-limit new-limit}))))))

;; ============================================================================
;; Throttler Management
;; ============================================================================

(defn enable-throttler!
  "Enable a throttler."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (reset! (:enabled? throttler) true)
    (logging/log :info "Enabled throttler" {:throttler-id throttler-id})))

(defn disable-throttler!
  "Disable a throttler."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (reset! (:enabled? throttler) false)
    (logging/log :info "Disabled throttler" {:throttler-id throttler-id})))

(defn pause-throttler!
  "Pause a throttler (stop processing queue)."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (reset! (:paused? throttler) true)
    (logging/log :info "Paused throttler" {:throttler-id throttler-id})))

(defn resume-throttler!
  "Resume a throttler."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (reset! (:paused? throttler) false)
    (logging/log :info "Resumed throttler" {:throttler-id throttler-id})))

(defn resize-throttler!
  "Resize a throttler's concurrency limit."
  [throttler-id new-limit]
  (when-let [throttler (get-throttler throttler-id)]
    (let [old-limit (:max-concurrent throttler)
          diff (- new-limit old-limit)
          semaphore (:semaphore throttler)]
      (if (pos? diff)
        (.release semaphore diff)
        (dotimes [_ (Math/abs diff)]
          (.tryAcquire semaphore)))
      (swap! state assoc-in [:throttlers throttler-id :max-concurrent] new-limit)
      (logging/log :info "Resized throttler" {:throttler-id throttler-id
                                               :old-limit old-limit
                                               :new-limit new-limit}))))

(defn destroy-throttler!
  "Destroy a throttler."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    (reset! (:enabled? throttler) false)
    (.clear (:queue throttler))
    (swap! state update :throttlers dissoc throttler-id)
    (logging/log :info "Destroyed throttler" {:throttler-id throttler-id})
    (events/emit! :throttler-destroyed {:throttler-id throttler-id})))

;; ============================================================================
;; Throttler Metrics
;; ============================================================================

(defn get-throttler-metrics
  "Get metrics for a throttler."
  [throttler-id]
  (when-let [throttler (get-throttler throttler-id)]
    {:throttler-id throttler-id
     :name (:name throttler)
     :max-concurrent (:max-concurrent throttler)
     :active-count @(:active-count throttler)
     :available-permits (.availablePermits (:semaphore throttler))
     :queue-size (.size (:queue throttler))
     :received @(get-in throttler [:metrics :received])
     :executed @(get-in throttler [:metrics :executed])
     :queued @(get-in throttler [:metrics :queued])
     :rejected @(get-in throttler [:metrics :rejected])
     :timed-out @(get-in throttler [:metrics :timed-out])
     :enabled? @(:enabled? throttler)
     :paused? @(:paused? throttler)}))

(defn get-all-throttler-metrics
  "Get metrics for all throttlers."
  []
  (mapv (fn [[id _]] (get-throttler-metrics id)) (:throttlers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-throttler-stats
  "Get global throttler statistics."
  []
  (let [stats (:stats @state)]
    {:throttlers-count (count (:throttlers @state))
     :requests-received (:requests-received stats)
     :requests-executed (:requests-executed stats)
     :requests-queued (:requests-queued stats)
     :requests-rejected (:requests-rejected stats)
     :requests-timed-out (:requests-timed-out stats)
     :execution-rate (if (pos? (:requests-received stats))
                       (/ (:requests-executed stats) (:requests-received stats))
                       1.0)}))

(defn reset-stats!
  "Reset throttler statistics."
  []
  (swap! state assoc :stats {:requests-received 0
                             :requests-executed 0
                             :requests-queued 0
                             :requests-rejected 0
                             :requests-timed-out 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-throttler!
  "Initialize the request throttler system."
  []
  (when-not (:initialized? @state)
    ;; Create default throttlers
    (create-throttler! :analysis
                       {:name "Analysis"
                        :max-concurrent 10
                        :queue-size 100
                        :timeout-ms 60000})
    
    (create-throttler! :lm-studio
                       {:name "LM Studio"
                        :max-concurrent 5
                        :queue-size 50
                        :timeout-ms 120000
                        :use-priority? true})
    
    (create-throttler! :api
                       {:name "API"
                        :max-concurrent 100
                        :queue-size 1000
                        :timeout-ms 30000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request throttler initialized")
    (events/emit! :request-throttler-initialized {})
    true))
