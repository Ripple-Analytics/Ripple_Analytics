(ns mental-models.pipeline.integration.backpressure-handler
  "Backpressure handler for mental model analysis system.
   
   Features:
   - Reactive backpressure
   - Buffer management
   - Flow control
   - Producer throttling
   - Consumer pacing
   - Overflow strategies
   - Backpressure signals
   - Adaptive buffering"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout
                                                   buffer dropping-buffer sliding-buffer]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent Semaphore TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:handlers {}         ;; handler-id -> backpressure-handler
         :config {:default-buffer-size 1000
                  :high-watermark 0.8
                  :low-watermark 0.3
                  :check-interval-ms 100}
         :stats {:items-received 0
                 :items-processed 0
                 :items-dropped 0
                 :backpressure-events 0
                 :overflow-events 0}
         :initialized? false}))

;; ============================================================================
;; Overflow Strategies
;; ============================================================================

(defn drop-oldest-strategy
  "Drop oldest items when buffer is full."
  []
  {:type :drop-oldest
   :on-overflow (fn [buffer item]
                  (when (seq @buffer)
                    (swap! buffer rest))
                  (swap! buffer conj item)
                  (swap! state update-in [:stats :items-dropped] inc))})

(defn drop-newest-strategy
  "Drop newest items when buffer is full."
  []
  {:type :drop-newest
   :on-overflow (fn [buffer item]
                  (swap! state update-in [:stats :items-dropped] inc)
                  nil)})

(defn block-strategy
  "Block producer when buffer is full."
  []
  {:type :block
   :on-overflow (fn [buffer item]
                  ;; Will be handled by semaphore
                  nil)})

(defn error-strategy
  "Throw error when buffer is full."
  []
  {:type :error
   :on-overflow (fn [buffer item]
                  (throw (ex-info "Buffer overflow" {:buffer-size (count @buffer)})))})

;; ============================================================================
;; Backpressure Handler Creation
;; ============================================================================

(defn create-handler!
  "Create a backpressure handler."
  [handler-id config]
  (let [buffer-size (get config :buffer-size
                         (get-in @state [:config :default-buffer-size]))
        high-watermark (get config :high-watermark
                            (get-in @state [:config :high-watermark]))
        low-watermark (get config :low-watermark
                           (get-in @state [:config :low-watermark]))
        overflow-strategy (get config :overflow-strategy (block-strategy))
        
        handler {:id handler-id
                 :name (get config :name (name handler-id))
                 :buffer-size buffer-size
                 :high-watermark high-watermark
                 :low-watermark low-watermark
                 :overflow-strategy overflow-strategy
                 :buffer (atom [])
                 :semaphore (Semaphore. buffer-size true)
                 :backpressure-active? (atom false)
                 :paused? (atom false)
                 :on-backpressure (get config :on-backpressure)
                 :on-resume (get config :on-resume)
                 :input-chan (chan buffer-size)
                 :output-chan (chan buffer-size)
                 :metrics {:received (atom 0)
                           :processed (atom 0)
                           :dropped (atom 0)}
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:handlers handler-id] handler)
    (logging/log :info "Created backpressure handler" {:handler-id handler-id :buffer-size buffer-size})
    (events/emit! :backpressure-handler-created {:handler-id handler-id})
    handler-id))

(defn get-handler
  "Get a backpressure handler."
  [handler-id]
  (get-in @state [:handlers handler-id]))

(defn list-handlers
  "List all backpressure handlers."
  []
  (mapv (fn [[id h]]
          {:id id
           :name (:name h)
           :buffer-size (:buffer-size h)
           :current-size (count @(:buffer h))
           :backpressure-active? @(:backpressure-active? h)
           :enabled? @(:enabled? h)})
        (:handlers @state)))

;; ============================================================================
;; Buffer Management
;; ============================================================================

(defn- get-fill-ratio
  "Get the buffer fill ratio."
  [handler]
  (/ (double (count @(:buffer handler))) (:buffer-size handler)))

(defn- check-watermarks!
  "Check watermarks and trigger backpressure signals."
  [handler]
  (let [fill-ratio (get-fill-ratio handler)
        high-wm (:high-watermark handler)
        low-wm (:low-watermark handler)
        was-active? @(:backpressure-active? handler)]
    
    (cond
      ;; Trigger backpressure
      (and (not was-active?) (>= fill-ratio high-wm))
      (do
        (reset! (:backpressure-active? handler) true)
        (swap! state update-in [:stats :backpressure-events] inc)
        (logging/log :warn "Backpressure activated" {:handler-id (:id handler) :fill-ratio fill-ratio})
        (events/emit! :backpressure-activated {:handler-id (:id handler)})
        (when-let [on-bp (:on-backpressure handler)]
          (try (on-bp) (catch Exception _))))
      
      ;; Release backpressure
      (and was-active? (<= fill-ratio low-wm))
      (do
        (reset! (:backpressure-active? handler) false)
        (logging/log :info "Backpressure released" {:handler-id (:id handler) :fill-ratio fill-ratio})
        (events/emit! :backpressure-released {:handler-id (:id handler)})
        (when-let [on-resume (:on-resume handler)]
          (try (on-resume) (catch Exception _)))))))

;; ============================================================================
;; Push/Pull Operations
;; ============================================================================

(defn push!
  "Push an item to the handler."
  [handler-id item & {:keys [timeout-ms] :or {timeout-ms 0}}]
  (when-let [handler (get-handler handler-id)]
    (when @(:enabled? handler)
      (swap! (get-in handler [:metrics :received]) inc)
      (swap! state update-in [:stats :items-received] inc)
      
      (let [strategy (:overflow-strategy handler)
            buffer (:buffer handler)
            semaphore (:semaphore handler)]
        
        (case (:type strategy)
          :block
          (if (pos? timeout-ms)
            (when (.tryAcquire semaphore timeout-ms TimeUnit/MILLISECONDS)
              (swap! buffer conj item)
              (check-watermarks! handler)
              true)
            (do
              (.acquire semaphore)
              (swap! buffer conj item)
              (check-watermarks! handler)
              true))
          
          :drop-oldest
          (if (>= (count @buffer) (:buffer-size handler))
            (do
              ((:on-overflow strategy) buffer item)
              (swap! state update-in [:stats :overflow-events] inc)
              false)
            (do
              (swap! buffer conj item)
              (check-watermarks! handler)
              true))
          
          :drop-newest
          (if (>= (count @buffer) (:buffer-size handler))
            (do
              ((:on-overflow strategy) buffer item)
              (swap! state update-in [:stats :overflow-events] inc)
              false)
            (do
              (swap! buffer conj item)
              (check-watermarks! handler)
              true))
          
          :error
          (if (>= (count @buffer) (:buffer-size handler))
            ((:on-overflow strategy) buffer item)
            (do
              (swap! buffer conj item)
              (check-watermarks! handler)
              true)))))))

(defn pull!
  "Pull an item from the handler."
  [handler-id & {:keys [timeout-ms] :or {timeout-ms 0}}]
  (when-let [handler (get-handler handler-id)]
    (when @(:enabled? handler)
      (let [buffer (:buffer handler)
            semaphore (:semaphore handler)]
        (when (seq @buffer)
          (let [item (first @buffer)]
            (swap! buffer rest)
            (.release semaphore)
            (swap! (get-in handler [:metrics :processed]) inc)
            (swap! state update-in [:stats :items-processed] inc)
            (check-watermarks! handler)
            item))))))

(defn pull-batch!
  "Pull a batch of items from the handler."
  [handler-id batch-size]
  (when-let [handler (get-handler handler-id)]
    (when @(:enabled? handler)
      (let [buffer (:buffer handler)
            semaphore (:semaphore handler)
            items (take batch-size @buffer)]
        (swap! buffer #(drop batch-size %))
        (dotimes [_ (count items)]
          (.release semaphore))
        (swap! (get-in handler [:metrics :processed]) + (count items))
        (swap! state update-in [:stats :items-processed] + (count items))
        (check-watermarks! handler)
        items))))

;; ============================================================================
;; Async Channel Operations
;; ============================================================================

(defn push-async!
  "Push an item asynchronously."
  [handler-id item]
  (when-let [handler (get-handler handler-id)]
    (go
      (>! (:input-chan handler) item)
      (swap! (get-in handler [:metrics :received]) inc)
      (swap! state update-in [:stats :items-received] inc))))

(defn pull-async!
  "Pull an item asynchronously."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (go
      (let [item (<! (:output-chan handler))]
        (swap! (get-in handler [:metrics :processed]) inc)
        (swap! state update-in [:stats :items-processed] inc)
        item))))

(defn start-processor!
  "Start a processor that moves items from input to output."
  [handler-id process-fn]
  (when-let [handler (get-handler handler-id)]
    (go-loop []
      (when @(:enabled? handler)
        (when-not @(:paused? handler)
          (when-let [item (<! (:input-chan handler))]
            (let [result (process-fn item)]
              (>! (:output-chan handler) result))))
        (<! (timeout 1))
        (recur)))))

;; ============================================================================
;; Flow Control
;; ============================================================================

(defn pause!
  "Pause the handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:paused? handler) true)
    (logging/log :info "Paused handler" {:handler-id handler-id})))

(defn resume!
  "Resume the handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:paused? handler) false)
    (logging/log :info "Resumed handler" {:handler-id handler-id})))

(defn is-backpressure-active?
  "Check if backpressure is active."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    @(:backpressure-active? handler)))

(defn get-available-capacity
  "Get available buffer capacity."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (- (:buffer-size handler) (count @(:buffer handler)))))

;; ============================================================================
;; Adaptive Buffering
;; ============================================================================

(defn resize-buffer!
  "Resize the handler buffer."
  [handler-id new-size]
  (when-let [handler (get-handler handler-id)]
    (let [old-size (:buffer-size handler)
          current-count (count @(:buffer handler))]
      (when (>= new-size current-count)
        (swap! state assoc-in [:handlers handler-id :buffer-size] new-size)
        ;; Adjust semaphore permits
        (let [semaphore (:semaphore handler)
              diff (- new-size old-size)]
          (if (pos? diff)
            (.release semaphore diff)
            (dotimes [_ (Math/abs diff)]
              (.tryAcquire semaphore))))
        (logging/log :info "Resized buffer" {:handler-id handler-id :old-size old-size :new-size new-size})))))

;; ============================================================================
;; Handler Metrics
;; ============================================================================

(defn get-handler-metrics
  "Get metrics for a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    {:handler-id handler-id
     :name (:name handler)
     :buffer-size (:buffer-size handler)
     :current-size (count @(:buffer handler))
     :fill-ratio (get-fill-ratio handler)
     :backpressure-active? @(:backpressure-active? handler)
     :paused? @(:paused? handler)
     :received @(get-in handler [:metrics :received])
     :processed @(get-in handler [:metrics :processed])
     :dropped @(get-in handler [:metrics :dropped])
     :available-capacity (get-available-capacity handler-id)}))

(defn get-all-handler-metrics
  "Get metrics for all handlers."
  []
  (mapv (fn [[id _]] (get-handler-metrics id)) (:handlers @state)))

;; ============================================================================
;; Handler Lifecycle
;; ============================================================================

(defn enable-handler!
  "Enable a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:enabled? handler) true)
    (logging/log :info "Enabled handler" {:handler-id handler-id})))

(defn disable-handler!
  "Disable a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:enabled? handler) false)
    (logging/log :info "Disabled handler" {:handler-id handler-id})))

(defn destroy-handler!
  "Destroy a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:enabled? handler) false)
    (async/close! (:input-chan handler))
    (async/close! (:output-chan handler))
    (swap! state update :handlers dissoc handler-id)
    (logging/log :info "Destroyed handler" {:handler-id handler-id})
    (events/emit! :backpressure-handler-destroyed {:handler-id handler-id})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-backpressure-stats
  "Get backpressure statistics."
  []
  (let [stats (:stats @state)]
    {:handlers-count (count (:handlers @state))
     :items-received (:items-received stats)
     :items-processed (:items-processed stats)
     :items-dropped (:items-dropped stats)
     :backpressure-events (:backpressure-events stats)
     :overflow-events (:overflow-events stats)
     :processing-rate (if (pos? (:items-received stats))
                        (/ (:items-processed stats) (:items-received stats))
                        1.0)}))

(defn reset-stats!
  "Reset backpressure statistics."
  []
  (swap! state assoc :stats {:items-received 0
                             :items-processed 0
                             :items-dropped 0
                             :backpressure-events 0
                             :overflow-events 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-backpressure-handler!
  "Initialize the backpressure handler system."
  []
  (when-not (:initialized? @state)
    ;; Create default handlers
    (create-handler! :analysis
                     {:name "Analysis"
                      :buffer-size 1000
                      :overflow-strategy (drop-oldest-strategy)})
    
    (create-handler! :lm-studio
                     {:name "LM Studio"
                      :buffer-size 100
                      :overflow-strategy (block-strategy)})
    
    (create-handler! :notifications
                     {:name "Notifications"
                      :buffer-size 500
                      :overflow-strategy (drop-newest-strategy)})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Backpressure handler initialized")
    (events/emit! :backpressure-handler-initialized {})
    true))
