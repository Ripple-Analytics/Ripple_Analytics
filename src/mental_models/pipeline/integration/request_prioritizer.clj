(ns mental-models.pipeline.integration.request-prioritizer
  "Request prioritizer for mental model analysis system.
   
   Features:
   - Request priority assignment
   - Priority queuing
   - Priority boosting
   - Priority decay
   - Priority rules
   - Priority inheritance
   - Priority metrics
   - Priority scheduling"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! put! take!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID PriorityQueue Comparator]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:rules {}            ;; rule-id -> rule
         :queues {}           ;; queue-id -> priority-queue
         :config {:default-priority 50
                  :min-priority 0
                  :max-priority 100
                  :decay-rate 0.1
                  :decay-interval-ms 60000
                  :boost-amount 10}
         :stats {:requests-prioritized 0
                 :priority-boosts 0
                 :priority-decays 0
                 :high-priority-requests 0
                 :low-priority-requests 0}
         :initialized? false}))

;; ============================================================================
;; Priority Levels
;; ============================================================================

(def priority-levels
  {:critical 100
   :high 80
   :normal 50
   :low 20
   :background 0})

(defn priority-level->value
  "Convert a priority level to a numeric value."
  [level]
  (get priority-levels level (get-in @state [:config :default-priority])))

(defn value->priority-level
  "Convert a numeric value to a priority level."
  [value]
  (cond
    (>= value 90) :critical
    (>= value 70) :high
    (>= value 40) :normal
    (>= value 10) :low
    :else :background))

;; ============================================================================
;; Priority Rules
;; ============================================================================

(defn register-rule!
  "Register a priority rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :condition-fn (get config :condition-fn)
              :priority-fn (get config :priority-fn)
              :priority-value (get config :priority-value)
              :order (get config :order 100)
              :enabled? (atom true)
              :metrics {:matches (atom 0)}
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Registered priority rule" {:rule-id rule-id})
    rule-id))

(defn get-rule
  "Get a priority rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all priority rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :order (:order r)
           :enabled? @(:enabled? r)
           :matches @(get-in r [:metrics :matches])})
        (:rules @state)))

(defn delete-rule!
  "Delete a priority rule."
  [rule-id]
  (swap! state update :rules dissoc rule-id))

(defn enable-rule!
  "Enable a priority rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) true)))

(defn disable-rule!
  "Disable a priority rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) false)))

;; ============================================================================
;; Priority Calculation
;; ============================================================================

(defn- apply-rules
  "Apply priority rules to a request."
  [request]
  (let [rules (->> (vals (:rules @state))
                   (filter #@(:enabled? %))
                   (sort-by :order))]
    (reduce (fn [priority rule]
              (if ((:condition-fn rule) request)
                (do
                  (swap! (get-in rule [:metrics :matches]) inc)
                  (if-let [priority-fn (:priority-fn rule)]
                    (priority-fn request priority)
                    (or (:priority-value rule) priority)))
                priority))
            (get-in @state [:config :default-priority])
            rules)))

(defn calculate-priority
  "Calculate the priority for a request."
  [request]
  (let [explicit-priority (get request :priority)
        calculated-priority (if explicit-priority
                              (if (keyword? explicit-priority)
                                (priority-level->value explicit-priority)
                                explicit-priority)
                              (apply-rules request))
        min-priority (get-in @state [:config :min-priority])
        max-priority (get-in @state [:config :max-priority])
        clamped-priority (max min-priority (min max-priority calculated-priority))]
    
    (swap! state update-in [:stats :requests-prioritized] inc)
    (cond
      (>= clamped-priority 70) (swap! state update-in [:stats :high-priority-requests] inc)
      (<= clamped-priority 30) (swap! state update-in [:stats :low-priority-requests] inc))
    
    clamped-priority))

(defn assign-priority
  "Assign priority to a request."
  [request]
  (let [priority (calculate-priority request)]
    (assoc request
           :priority priority
           :priority-level (value->priority-level priority)
           :priority-assigned-at (System/currentTimeMillis))))

;; ============================================================================
;; Priority Queue
;; ============================================================================

(defn create-priority-queue
  "Create a priority queue."
  [queue-id & {:keys [capacity] :or {capacity 1000}}]
  (let [comparator (reify Comparator
                     (compare [_ a b]
                       (- (get b :priority 0) (get a :priority 0))))
        queue (PriorityQueue. capacity comparator)
        queue-state {:id queue-id
                     :queue queue
                     :capacity capacity
                     :metrics {:enqueued (atom 0)
                               :dequeued (atom 0)
                               :dropped (atom 0)}
                     :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:queues queue-id] queue-state)
    (logging/log :info "Created priority queue" {:queue-id queue-id :capacity capacity})
    queue-id))

(defn get-queue
  "Get a priority queue."
  [queue-id]
  (get-in @state [:queues queue-id]))

(defn enqueue!
  "Enqueue a request with priority."
  [queue-id request]
  (if-let [queue-state (get-queue queue-id)]
    (let [queue (:queue queue-state)
          prioritized-request (assign-priority request)]
      (if (< (.size queue) (:capacity queue-state))
        (do
          (.offer queue prioritized-request)
          (swap! (get-in queue-state [:metrics :enqueued]) inc)
          {:enqueued? true :priority (:priority prioritized-request)})
        (do
          (swap! (get-in queue-state [:metrics :dropped]) inc)
          {:enqueued? false :reason :queue-full})))
    {:enqueued? false :reason :queue-not-found}))

(defn dequeue!
  "Dequeue the highest priority request."
  [queue-id]
  (if-let [queue-state (get-queue queue-id)]
    (let [queue (:queue queue-state)]
      (when-let [request (.poll queue)]
        (swap! (get-in queue-state [:metrics :dequeued]) inc)
        request))
    nil))

(defn peek-queue
  "Peek at the highest priority request without removing it."
  [queue-id]
  (when-let [queue-state (get-queue queue-id)]
    (.peek (:queue queue-state))))

(defn queue-size
  "Get the size of a priority queue."
  [queue-id]
  (when-let [queue-state (get-queue queue-id)]
    (.size (:queue queue-state))))

;; ============================================================================
;; Priority Boosting
;; ============================================================================

(defn boost-priority!
  "Boost the priority of a request."
  [request & {:keys [amount] :or {amount nil}}]
  (let [boost-amount (or amount (get-in @state [:config :boost-amount]))
        current-priority (get request :priority (get-in @state [:config :default-priority]))
        max-priority (get-in @state [:config :max-priority])
        new-priority (min max-priority (+ current-priority boost-amount))]
    
    (swap! state update-in [:stats :priority-boosts] inc)
    (assoc request
           :priority new-priority
           :priority-level (value->priority-level new-priority)
           :boosted? true
           :boost-amount boost-amount)))

(defn decay-priority!
  "Decay the priority of a request."
  [request]
  (let [decay-rate (get-in @state [:config :decay-rate])
        current-priority (get request :priority (get-in @state [:config :default-priority]))
        min-priority (get-in @state [:config :min-priority])
        new-priority (max min-priority (* current-priority (- 1 decay-rate)))]
    
    (swap! state update-in [:stats :priority-decays] inc)
    (assoc request
           :priority new-priority
           :priority-level (value->priority-level new-priority)
           :decayed? true)))

;; ============================================================================
;; Priority Inheritance
;; ============================================================================

(defn inherit-priority
  "Inherit priority from a parent request."
  [child-request parent-request]
  (let [parent-priority (get parent-request :priority (get-in @state [:config :default-priority]))]
    (assoc child-request
           :priority parent-priority
           :priority-level (value->priority-level parent-priority)
           :inherited-from (:id parent-request))))

(defn propagate-priority
  "Propagate priority to dependent requests."
  [requests priority]
  (mapv #(assoc %
                :priority priority
                :priority-level (value->priority-level priority)
                :propagated? true)
        requests))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-prioritize
  "Ring middleware to assign priority to requests."
  [handler]
  (fn [request]
    (handler (assign-priority request))))

(defn wrap-priority-queue
  "Ring middleware to queue requests by priority."
  [handler queue-id]
  (fn [request]
    (let [result (enqueue! queue-id request)]
      (if (:enqueued? result)
        {:status 202
         :body {:message "Request queued"
                :priority (:priority result)}}
        {:status 503
         :body {:error "Queue full"}}))))

;; ============================================================================
;; Built-in Rules
;; ============================================================================

(defn register-default-rules!
  "Register default priority rules."
  []
  ;; Admin requests get high priority
  (register-rule! :admin-requests
                  {:name "Admin Requests"
                   :order 10
                   :condition-fn (fn [req]
                                   (get-in req [:headers "x-admin-request"]))
                   :priority-value 90})
  
  ;; Health checks get low priority
  (register-rule! :health-checks
                  {:name "Health Checks"
                   :order 20
                   :condition-fn (fn [req]
                                   (str/includes? (or (:uri req) "") "/health"))
                   :priority-value 10})
  
  ;; API requests get normal priority
  (register-rule! :api-requests
                  {:name "API Requests"
                   :order 30
                   :condition-fn (fn [req]
                                   (str/starts-with? (or (:uri req) "") "/api"))
                   :priority-value 50}))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-prioritizer-metrics
  "Get prioritizer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-prioritized (:requests-prioritized stats)
     :priority-boosts (:priority-boosts stats)
     :priority-decays (:priority-decays stats)
     :high-priority-requests (:high-priority-requests stats)
     :low-priority-requests (:low-priority-requests stats)
     :rules-count (count (:rules @state))
     :queues-count (count (:queues @state))}))

(defn get-queue-metrics
  "Get metrics for a priority queue."
  [queue-id]
  (when-let [queue-state (get-queue queue-id)]
    {:queue-id queue-id
     :size (.size (:queue queue-state))
     :capacity (:capacity queue-state)
     :enqueued @(get-in queue-state [:metrics :enqueued])
     :dequeued @(get-in queue-state [:metrics :dequeued])
     :dropped @(get-in queue-state [:metrics :dropped])}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-prioritizer-stats
  "Get prioritizer statistics."
  []
  (merge (get-prioritizer-metrics)
         {:default-priority (get-in @state [:config :default-priority])
          :min-priority (get-in @state [:config :min-priority])
          :max-priority (get-in @state [:config :max-priority])}))

(defn reset-stats!
  "Reset prioritizer statistics."
  []
  (swap! state assoc :stats {:requests-prioritized 0
                             :priority-boosts 0
                             :priority-decays 0
                             :high-priority-requests 0
                             :low-priority-requests 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-prioritizer!
  "Initialize the request prioritizer."
  []
  (when-not (:initialized? @state)
    (register-default-rules!)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request prioritizer initialized")
    (events/emit! :request-prioritizer-initialized {})
    true))
