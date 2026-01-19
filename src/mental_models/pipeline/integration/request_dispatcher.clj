(ns mental-models.pipeline.integration.request-dispatcher
  "Request dispatcher for mental model analysis system.
   
   Features:
   - Request dispatching
   - Handler routing
   - Load balancing
   - Failover handling
   - Dispatch strategies
   - Dispatch queuing
   - Dispatch metrics
   - Dispatch timeouts"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! put! take! alt!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent.atomic AtomicLong AtomicInteger]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:handlers {}         ;; handler-id -> handler
         :routes {}           ;; route-pattern -> handler-id
         :config {:default-timeout-ms 30000
                  :max-retries 3
                  :retry-delay-ms 1000
                  :strategy :round-robin}  ;; :round-robin, :random, :least-connections, :weighted
         :stats {:dispatches 0
                 :successes 0
                 :failures 0
                 :timeouts 0
                 :retries 0}
         :initialized? false}))

;; ============================================================================
;; Handler Registration
;; ============================================================================

(defn register-handler!
  "Register a request handler."
  [handler-id config]
  (let [handler {:id handler-id
                 :name (get config :name (name handler-id))
                 :handler-fn (get config :handler-fn)
                 :weight (get config :weight 1)
                 :max-connections (get config :max-connections 100)
                 :current-connections (AtomicInteger. 0)
                 :healthy? (atom true)
                 :enabled? (atom true)
                 :metrics {:requests (atom 0)
                           :successes (atom 0)
                           :failures (atom 0)
                           :total-time-ms (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:handlers handler-id] handler)
    (logging/log :info "Registered handler" {:handler-id handler-id})
    handler-id))

(defn get-handler
  "Get a handler by ID."
  [handler-id]
  (get-in @state [:handlers handler-id]))

(defn list-handlers
  "List all handlers."
  []
  (mapv (fn [[id h]]
          {:id id
           :name (:name h)
           :weight (:weight h)
           :healthy? @(:healthy? h)
           :enabled? @(:enabled? h)
           :connections (.get (:current-connections h))
           :requests @(get-in h [:metrics :requests])})
        (:handlers @state)))

(defn unregister-handler!
  "Unregister a handler."
  [handler-id]
  (swap! state update :handlers dissoc handler-id))

;; ============================================================================
;; Route Registration
;; ============================================================================

(defn register-route!
  "Register a route to a handler."
  [pattern handler-id & {:keys [methods] :or {methods #{:get :post :put :delete :patch}}}]
  (swap! state assoc-in [:routes pattern]
         {:handler-id handler-id
          :methods methods
          :pattern (if (string? pattern) (re-pattern pattern) pattern)})
  pattern)

(defn get-route
  "Get a route by pattern."
  [pattern]
  (get-in @state [:routes pattern]))

(defn list-routes
  "List all routes."
  []
  (mapv (fn [[pattern r]]
          {:pattern (str pattern)
           :handler-id (:handler-id r)
           :methods (:methods r)})
        (:routes @state)))

(defn unregister-route!
  "Unregister a route."
  [pattern]
  (swap! state update :routes dissoc pattern))

;; ============================================================================
;; Handler Selection Strategies
;; ============================================================================

(def ^:private round-robin-counter (AtomicLong. 0))

(defn- get-available-handlers
  "Get all available handlers."
  []
  (->> (vals (:handlers @state))
       (filter #(and @(:enabled? %) @(:healthy? %)))
       (filter #(< (.get (:current-connections %)) (:max-connections %)))
       vec))

(defn- select-round-robin
  "Select handler using round-robin."
  [handlers]
  (when (seq handlers)
    (let [index (mod (.getAndIncrement round-robin-counter) (count handlers))]
      (nth handlers index))))

(defn- select-random
  "Select handler randomly."
  [handlers]
  (when (seq handlers)
    (rand-nth handlers)))

(defn- select-least-connections
  "Select handler with least connections."
  [handlers]
  (when (seq handlers)
    (apply min-key #(.get (:current-connections %)) handlers)))

(defn- select-weighted
  "Select handler using weighted random selection."
  [handlers]
  (when (seq handlers)
    (let [total-weight (reduce + (map :weight handlers))
          random-weight (rand total-weight)]
      (loop [remaining handlers
             cumulative 0]
        (when (seq remaining)
          (let [handler (first remaining)
                new-cumulative (+ cumulative (:weight handler))]
            (if (< random-weight new-cumulative)
              handler
              (recur (rest remaining) new-cumulative))))))))

(defn select-handler
  "Select a handler based on the configured strategy."
  []
  (let [handlers (get-available-handlers)
        strategy (get-in @state [:config :strategy])]
    (case strategy
      :round-robin (select-round-robin handlers)
      :random (select-random handlers)
      :least-connections (select-least-connections handlers)
      :weighted (select-weighted handlers)
      (select-round-robin handlers))))

;; ============================================================================
;; Route Matching
;; ============================================================================

(defn- match-route
  "Find a matching route for a request."
  [request]
  (let [uri (:uri request)
        method (:method request)]
    (first (for [[_ route] (:routes @state)
                 :when (and (re-matches (:pattern route) uri)
                            (contains? (:methods route) method))]
             route))))

(defn- get-handler-for-route
  "Get the handler for a matched route."
  [route]
  (when route
    (get-handler (:handler-id route))))

;; ============================================================================
;; Request Dispatching
;; ============================================================================

(defn- dispatch-to-handler
  "Dispatch a request to a specific handler."
  [handler request]
  (let [start-time (System/currentTimeMillis)]
    (.incrementAndGet (:current-connections handler))
    (swap! (get-in handler [:metrics :requests]) inc)
    
    (try
      (let [result ((:handler-fn handler) request)
            duration-ms (- (System/currentTimeMillis) start-time)]
        (swap! (get-in handler [:metrics :successes]) inc)
        (swap! (get-in handler [:metrics :total-time-ms]) + duration-ms)
        {:success? true :result result :duration-ms duration-ms :handler-id (:id handler)})
      (catch Exception e
        (swap! (get-in handler [:metrics :failures]) inc)
        {:success? false :error (.getMessage e) :handler-id (:id handler)})
      (finally
        (.decrementAndGet (:current-connections handler))))))

(defn dispatch-request
  "Dispatch a request to an appropriate handler."
  [request]
  (swap! state update-in [:stats :dispatches] inc)
  
  (let [route (match-route request)
        handler (or (get-handler-for-route route) (select-handler))]
    (if handler
      (let [result (dispatch-to-handler handler request)]
        (if (:success? result)
          (do
            (swap! state update-in [:stats :successes] inc)
            result)
          (do
            (swap! state update-in [:stats :failures] inc)
            result)))
      (do
        (swap! state update-in [:stats :failures] inc)
        {:success? false :error "No available handler"}))))

(defn dispatch-with-retry
  "Dispatch a request with retry logic."
  [request]
  (let [max-retries (get-in @state [:config :max-retries])
        retry-delay (get-in @state [:config :retry-delay-ms])]
    (loop [attempt 0]
      (let [result (dispatch-request request)]
        (if (or (:success? result) (>= attempt max-retries))
          result
          (do
            (swap! state update-in [:stats :retries] inc)
            (Thread/sleep retry-delay)
            (recur (inc attempt))))))))

(defn dispatch-with-timeout
  "Dispatch a request with timeout."
  [request & {:keys [timeout-ms] :or {timeout-ms nil}}]
  (let [actual-timeout (or timeout-ms (get-in @state [:config :default-timeout-ms]))
        result-chan (chan 1)]
    
    (go
      (let [result (dispatch-request request)]
        (>! result-chan result)))
    
    (let [[result _] (alt!
                       result-chan ([v] [v :result])
                       (timeout actual-timeout) ([_] [nil :timeout]))]
      (if result
        result
        (do
          (swap! state update-in [:stats :timeouts] inc)
          {:success? false :error "Request timeout"})))))

;; ============================================================================
;; Async Dispatching
;; ============================================================================

(defn dispatch-async
  "Dispatch a request asynchronously."
  [request callback]
  (go
    (let [result (dispatch-request request)]
      (callback result))))

(defn dispatch-batch
  "Dispatch multiple requests in parallel."
  [requests]
  (let [result-chans (mapv (fn [req]
                             (let [ch (chan 1)]
                               (go (>! ch (dispatch-request req)))
                               ch))
                           requests)]
    (mapv (fn [ch] (<!! (async/into [] ch))) result-chans)))

;; ============================================================================
;; Handler Health
;; ============================================================================

(defn mark-handler-healthy!
  "Mark a handler as healthy."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:healthy? handler) true)
    (logging/log :info "Handler marked healthy" {:handler-id handler-id})))

(defn mark-handler-unhealthy!
  "Mark a handler as unhealthy."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:healthy? handler) false)
    (logging/log :warn "Handler marked unhealthy" {:handler-id handler-id})))

(defn enable-handler!
  "Enable a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:enabled? handler) true)))

(defn disable-handler!
  "Disable a handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    (reset! (:enabled? handler) false)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-dispatch
  "Ring middleware to dispatch requests."
  [_]
  (fn [request]
    (let [result (dispatch-request request)]
      (if (:success? result)
        (:result result)
        {:status 503
         :body {:error (:error result)}}))))

(defn wrap-dispatch-with-retry
  "Ring middleware to dispatch with retry."
  [_]
  (fn [request]
    (let [result (dispatch-with-retry request)]
      (if (:success? result)
        (:result result)
        {:status 503
         :body {:error (:error result)}}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-strategy!
  "Set the dispatch strategy."
  [strategy]
  (swap! state assoc-in [:config :strategy] strategy))

(defn set-default-timeout!
  "Set the default timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :default-timeout-ms] timeout-ms))

(defn set-max-retries!
  "Set the maximum retries."
  [max-retries]
  (swap! state assoc-in [:config :max-retries] max-retries))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-dispatcher-metrics
  "Get dispatcher metrics."
  []
  (let [stats (:stats @state)]
    {:dispatches (:dispatches stats)
     :successes (:successes stats)
     :failures (:failures stats)
     :timeouts (:timeouts stats)
     :retries (:retries stats)
     :success-rate (if (pos? (:dispatches stats))
                     (/ (:successes stats) (:dispatches stats))
                     1.0)
     :handlers-count (count (:handlers @state))
     :routes-count (count (:routes @state))}))

(defn get-handler-metrics
  "Get metrics for a specific handler."
  [handler-id]
  (when-let [handler (get-handler handler-id)]
    {:handler-id handler-id
     :name (:name handler)
     :requests @(get-in handler [:metrics :requests])
     :successes @(get-in handler [:metrics :successes])
     :failures @(get-in handler [:metrics :failures])
     :avg-time-ms (let [requests @(get-in handler [:metrics :requests])]
                    (if (pos? requests)
                      (/ @(get-in handler [:metrics :total-time-ms]) requests)
                      0))
     :current-connections (.get (:current-connections handler))
     :healthy? @(:healthy? handler)
     :enabled? @(:enabled? handler)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-dispatcher-stats
  "Get dispatcher statistics."
  []
  (merge (get-dispatcher-metrics)
         {:strategy (get-in @state [:config :strategy])
          :default-timeout-ms (get-in @state [:config :default-timeout-ms])
          :max-retries (get-in @state [:config :max-retries])}))

(defn reset-stats!
  "Reset dispatcher statistics."
  []
  (swap! state assoc :stats {:dispatches 0
                             :successes 0
                             :failures 0
                             :timeouts 0
                             :retries 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-dispatcher!
  "Initialize the request dispatcher."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request dispatcher initialized")
    (events/emit! :request-dispatcher-initialized {})
    true))
