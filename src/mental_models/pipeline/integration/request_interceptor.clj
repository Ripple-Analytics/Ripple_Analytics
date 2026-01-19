(ns mental-models.pipeline.integration.request-interceptor
  "Request interceptor for mental model analysis system.
   
   Features:
   - Pre-request interceptors
   - Post-request interceptors
   - Error interceptors
   - Interceptor chains
   - Conditional interception
   - Interceptor ordering
   - Interceptor metrics
   - Async interceptors"
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
  (atom {:interceptors {}     ;; interceptor-id -> interceptor
         :chains {}           ;; chain-id -> [interceptor-ids]
         :config {:default-timeout-ms 5000
                  :max-chain-length 20}
         :stats {:pre-intercepted 0
                 :post-intercepted 0
                 :error-intercepted 0
                 :interceptor-errors 0}
         :initialized? false}))

;; ============================================================================
;; Interceptor Creation
;; ============================================================================

(defn register-interceptor!
  "Register an interceptor."
  [interceptor-id config]
  (let [interceptor {:id interceptor-id
                     :name (get config :name (name interceptor-id))
                     :type (get config :type :pre)  ;; :pre, :post, :error, :around
                     :handler-fn (get config :handler-fn)
                     :condition-fn (get config :condition-fn (constantly true))
                     :priority (get config :priority 100)
                     :enabled? (atom true)
                     :async? (get config :async? false)
                     :metrics {:invocations (atom 0)
                               :errors (atom 0)
                               :total-time-ms (atom 0)}
                     :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:interceptors interceptor-id] interceptor)
    (logging/log :info "Registered interceptor" {:interceptor-id interceptor-id :type (:type interceptor)})
    (events/emit! :interceptor-registered {:interceptor-id interceptor-id})
    interceptor-id))

(defn get-interceptor
  "Get an interceptor."
  [interceptor-id]
  (get-in @state [:interceptors interceptor-id]))

(defn list-interceptors
  "List all interceptors."
  []
  (mapv (fn [[id i]]
          {:id id
           :name (:name i)
           :type (:type i)
           :priority (:priority i)
           :enabled? @(:enabled? i)})
        (:interceptors @state)))

(defn unregister-interceptor!
  "Unregister an interceptor."
  [interceptor-id]
  (swap! state update :interceptors dissoc interceptor-id)
  (logging/log :info "Unregistered interceptor" {:interceptor-id interceptor-id}))

;; ============================================================================
;; Interceptor Chains
;; ============================================================================

(defn create-chain!
  "Create an interceptor chain."
  [chain-id interceptor-ids]
  (let [max-length (get-in @state [:config :max-chain-length])]
    (when (<= (count interceptor-ids) max-length)
      (swap! state assoc-in [:chains chain-id] (vec interceptor-ids))
      (logging/log :info "Created interceptor chain" {:chain-id chain-id :length (count interceptor-ids)})
      chain-id)))

(defn get-chain
  "Get an interceptor chain."
  [chain-id]
  (get-in @state [:chains chain-id]))

(defn add-to-chain!
  "Add an interceptor to a chain."
  [chain-id interceptor-id]
  (when (get-chain chain-id)
    (swap! state update-in [:chains chain-id] conj interceptor-id)))

(defn remove-from-chain!
  "Remove an interceptor from a chain."
  [chain-id interceptor-id]
  (when (get-chain chain-id)
    (swap! state update-in [:chains chain-id] #(vec (remove #{interceptor-id} %)))))

(defn delete-chain!
  "Delete an interceptor chain."
  [chain-id]
  (swap! state update :chains dissoc chain-id)
  (logging/log :info "Deleted interceptor chain" {:chain-id chain-id}))

;; ============================================================================
;; Interceptor Execution
;; ============================================================================

(defn- execute-interceptor
  "Execute a single interceptor."
  [interceptor context]
  (when (and @(:enabled? interceptor)
             ((:condition-fn interceptor) context))
    (let [start-time (System/currentTimeMillis)]
      (try
        (swap! (get-in interceptor [:metrics :invocations]) inc)
        (let [result ((:handler-fn interceptor) context)]
          (swap! (get-in interceptor [:metrics :total-time-ms])
                 + (- (System/currentTimeMillis) start-time))
          result)
        (catch Exception e
          (swap! (get-in interceptor [:metrics :errors]) inc)
          (swap! state update-in [:stats :interceptor-errors] inc)
          (logging/log :error "Interceptor error" {:interceptor-id (:id interceptor)
                                                    :error (.getMessage e)})
          (throw e))))))

(defn- get-sorted-interceptors
  "Get interceptors sorted by priority."
  [interceptor-ids type]
  (->> interceptor-ids
       (map get-interceptor)
       (filter some?)
       (filter #(= (:type %) type))
       (sort-by :priority)))

;; ============================================================================
;; Pre-Request Interception
;; ============================================================================

(defn intercept-pre
  "Execute pre-request interceptors."
  [chain-id context]
  (if-let [interceptor-ids (get-chain chain-id)]
    (let [interceptors (get-sorted-interceptors interceptor-ids :pre)]
      (swap! state update-in [:stats :pre-intercepted] inc)
      (reduce (fn [ctx interceptor]
                (or (execute-interceptor interceptor ctx) ctx))
              context
              interceptors))
    context))

;; ============================================================================
;; Post-Request Interception
;; ============================================================================

(defn intercept-post
  "Execute post-request interceptors."
  [chain-id context response]
  (if-let [interceptor-ids (get-chain chain-id)]
    (let [interceptors (get-sorted-interceptors interceptor-ids :post)
          ctx (assoc context :response response)]
      (swap! state update-in [:stats :post-intercepted] inc)
      (reduce (fn [c interceptor]
                (or (execute-interceptor interceptor c) c))
              ctx
              interceptors))
    (assoc context :response response)))

;; ============================================================================
;; Error Interception
;; ============================================================================

(defn intercept-error
  "Execute error interceptors."
  [chain-id context error]
  (if-let [interceptor-ids (get-chain chain-id)]
    (let [interceptors (get-sorted-interceptors interceptor-ids :error)
          ctx (assoc context :error error)]
      (swap! state update-in [:stats :error-intercepted] inc)
      (reduce (fn [c interceptor]
                (try
                  (or (execute-interceptor interceptor c) c)
                  (catch Exception e
                    (logging/log :error "Error interceptor failed" {:error (.getMessage e)})
                    c)))
              ctx
              interceptors))
    (assoc context :error error)))

;; ============================================================================
;; Around Interception
;; ============================================================================

(defn intercept-around
  "Execute around interceptors (wrap the entire request)."
  [chain-id context handler-fn]
  (if-let [interceptor-ids (get-chain chain-id)]
    (let [interceptors (get-sorted-interceptors interceptor-ids :around)]
      (if (empty? interceptors)
        (handler-fn context)
        (let [wrapped-fn (reduce (fn [f interceptor]
                                   (fn [ctx]
                                     (if (and @(:enabled? interceptor)
                                              ((:condition-fn interceptor) ctx))
                                       ((:handler-fn interceptor) ctx f)
                                       (f ctx))))
                                 handler-fn
                                 (reverse interceptors))]
          (wrapped-fn context))))
    (handler-fn context)))

;; ============================================================================
;; Full Interception Pipeline
;; ============================================================================

(defn intercept
  "Execute full interception pipeline."
  [chain-id context handler-fn]
  (try
    (let [pre-ctx (intercept-pre chain-id context)
          result (intercept-around chain-id pre-ctx handler-fn)
          post-ctx (intercept-post chain-id pre-ctx result)]
      (:response post-ctx))
    (catch Exception e
      (let [error-ctx (intercept-error chain-id context e)]
        (if-let [handled-error (:handled-error error-ctx)]
          handled-error
          (throw e))))))

(defmacro with-interception
  "Execute body with interception."
  [chain-id context & body]
  `(intercept ~chain-id ~context (fn [_#] ~@body)))

;; ============================================================================
;; Interceptor Control
;; ============================================================================

(defn enable-interceptor!
  "Enable an interceptor."
  [interceptor-id]
  (when-let [interceptor (get-interceptor interceptor-id)]
    (reset! (:enabled? interceptor) true)
    (logging/log :info "Enabled interceptor" {:interceptor-id interceptor-id})))

(defn disable-interceptor!
  "Disable an interceptor."
  [interceptor-id]
  (when-let [interceptor (get-interceptor interceptor-id)]
    (reset! (:enabled? interceptor) false)
    (logging/log :info "Disabled interceptor" {:interceptor-id interceptor-id})))

(defn set-priority!
  "Set interceptor priority."
  [interceptor-id priority]
  (swap! state assoc-in [:interceptors interceptor-id :priority] priority))

;; ============================================================================
;; Built-in Interceptors
;; ============================================================================

(defn- logging-interceptor
  "Logging interceptor handler."
  [context]
  (logging/log :debug "Request intercepted" {:uri (:uri context)
                                              :method (:method context)})
  context)

(defn- timing-interceptor
  "Timing interceptor handler."
  [context]
  (assoc context :start-time (System/currentTimeMillis)))

(defn- timing-post-interceptor
  "Timing post-interceptor handler."
  [context]
  (when-let [start-time (:start-time context)]
    (let [duration (- (System/currentTimeMillis) start-time)]
      (logging/log :debug "Request completed" {:duration-ms duration})))
  context)

(defn- error-logging-interceptor
  "Error logging interceptor handler."
  [context]
  (when-let [error (:error context)]
    (logging/log :error "Request error" {:error (.getMessage error)
                                          :uri (:uri context)}))
  context)

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-interceptor-metrics
  "Get metrics for an interceptor."
  [interceptor-id]
  (when-let [interceptor (get-interceptor interceptor-id)]
    (let [invocations @(get-in interceptor [:metrics :invocations])
          total-time @(get-in interceptor [:metrics :total-time-ms])]
      {:interceptor-id interceptor-id
       :name (:name interceptor)
       :type (:type interceptor)
       :invocations invocations
       :errors @(get-in interceptor [:metrics :errors])
       :total-time-ms total-time
       :avg-time-ms (if (pos? invocations) (/ total-time invocations) 0)
       :enabled? @(:enabled? interceptor)})))

(defn get-all-interceptor-metrics
  "Get metrics for all interceptors."
  []
  (mapv (fn [[id _]] (get-interceptor-metrics id)) (:interceptors @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-interceptor-stats
  "Get interceptor statistics."
  []
  (let [stats (:stats @state)]
    {:interceptors-count (count (:interceptors @state))
     :chains-count (count (:chains @state))
     :pre-intercepted (:pre-intercepted stats)
     :post-intercepted (:post-intercepted stats)
     :error-intercepted (:error-intercepted stats)
     :interceptor-errors (:interceptor-errors stats)}))

(defn reset-stats!
  "Reset interceptor statistics."
  []
  (swap! state assoc :stats {:pre-intercepted 0
                             :post-intercepted 0
                             :error-intercepted 0
                             :interceptor-errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-interceptor!
  "Initialize the request interceptor system."
  []
  (when-not (:initialized? @state)
    ;; Register built-in interceptors
    (register-interceptor! :logging
                           {:name "Logging"
                            :type :pre
                            :priority 10
                            :handler-fn logging-interceptor})
    
    (register-interceptor! :timing
                           {:name "Timing"
                            :type :pre
                            :priority 5
                            :handler-fn timing-interceptor})
    
    (register-interceptor! :timing-post
                           {:name "Timing Post"
                            :type :post
                            :priority 5
                            :handler-fn timing-post-interceptor})
    
    (register-interceptor! :error-logging
                           {:name "Error Logging"
                            :type :error
                            :priority 10
                            :handler-fn error-logging-interceptor})
    
    ;; Create default chain
    (create-chain! :default [:logging :timing :timing-post :error-logging])
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request interceptor initialized")
    (events/emit! :request-interceptor-initialized {})
    true))
