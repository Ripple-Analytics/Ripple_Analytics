(ns mental-models.pipeline.integration.priority-queue
  "Priority queue for mental model analysis system.
   
   Features:
   - Multi-level priorities
   - Priority aging
   - Fair scheduling
   - Priority boost
   - Starvation prevention
   - Priority inheritance
   - Queue monitoring
   - Priority metrics"
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
           [java.util.concurrent PriorityBlockingQueue TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:queues {}           ;; queue-id -> priority-queue
         :config {:default-priority 5
                  :max-priority 10
                  :min-priority 1
                  :aging-interval-ms 1000
                  :aging-increment 1
                  :max-age-boost 3}
         :stats {:items-enqueued 0
                 :items-dequeued 0
                 :priority-boosts 0
                 :aging-events 0}
         :initialized? false}))

;; ============================================================================
;; Priority Item
;; ============================================================================

(defrecord PriorityItem [id priority effective-priority payload created-at age-boosts]
  Comparable
  (compareTo [this other]
    (let [priority-cmp (compare (:effective-priority other) (:effective-priority this))]
      (if (zero? priority-cmp)
        (compare (:created-at this) (:created-at other))
        priority-cmp))))

(defn- create-item
  "Create a priority item."
  [payload priority]
  (->PriorityItem
   (str (UUID/randomUUID))
   priority
   priority
   payload
   (System/currentTimeMillis)
   0))

;; ============================================================================
;; Queue Creation
;; ============================================================================

(defn create-queue!
  "Create a priority queue."
  [queue-id config]
  (let [queue {:id queue-id
               :name (get config :name (name queue-id))
               :queue (PriorityBlockingQueue.)
               :default-priority (get config :default-priority
                                      (get-in @state [:config :default-priority]))
               :aging-enabled? (get config :aging-enabled? true)
               :aging-interval-ms (get config :aging-interval-ms
                                       (get-in @state [:config :aging-interval-ms]))
               :aging-increment (get config :aging-increment
                                     (get-in @state [:config :aging-increment]))
               :max-age-boost (get config :max-age-boost
                                   (get-in @state [:config :max-age-boost]))
               :enabled? (atom true)
               :metrics {:enqueued (atom 0)
                         :dequeued (atom 0)
                         :boosts (atom 0)}
               :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:queues queue-id] queue)
    (logging/log :info "Created priority queue" {:queue-id queue-id})
    (events/emit! :priority-queue-created {:queue-id queue-id})
    
    ;; Start aging if enabled
    (when (:aging-enabled? queue)
      (start-aging! queue-id))
    
    queue-id))

(defn get-queue
  "Get a priority queue."
  [queue-id]
  (get-in @state [:queues queue-id]))

(defn list-queues
  "List all priority queues."
  []
  (mapv (fn [[id q]]
          {:id id
           :name (:name q)
           :size (.size (:queue q))
           :aging-enabled? (:aging-enabled? q)
           :enabled? @(:enabled? q)})
        (:queues @state)))

(defn delete-queue!
  "Delete a priority queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (reset! (:enabled? queue) false)
    (.clear (:queue queue))
    (swap! state update :queues dissoc queue-id)
    (logging/log :info "Deleted priority queue" {:queue-id queue-id})))

;; ============================================================================
;; Enqueue/Dequeue Operations
;; ============================================================================

(defn enqueue!
  "Enqueue an item with priority."
  [queue-id payload & {:keys [priority]}]
  (when-let [queue (get-queue queue-id)]
    (when @(:enabled? queue)
      (let [effective-priority (or priority (:default-priority queue))
            item (create-item payload effective-priority)]
        (.offer (:queue queue) item)
        (swap! (get-in queue [:metrics :enqueued]) inc)
        (swap! state update-in [:stats :items-enqueued] inc)
        (logging/log :debug "Enqueued item" {:queue-id queue-id
                                              :item-id (:id item)
                                              :priority effective-priority})
        {:item-id (:id item) :priority effective-priority}))))

(defn dequeue!
  "Dequeue the highest priority item."
  [queue-id & {:keys [timeout-ms] :or {timeout-ms 0}}]
  (when-let [queue (get-queue queue-id)]
    (when @(:enabled? queue)
      (let [item (if (pos? timeout-ms)
                   (.poll (:queue queue) timeout-ms TimeUnit/MILLISECONDS)
                   (.poll (:queue queue)))]
        (when item
          (swap! (get-in queue [:metrics :dequeued]) inc)
          (swap! state update-in [:stats :items-dequeued] inc)
          (logging/log :debug "Dequeued item" {:queue-id queue-id
                                                :item-id (:id item)
                                                :priority (:priority item)
                                                :effective-priority (:effective-priority item)})
          (:payload item))))))

(defn peek-queue
  "Peek at the highest priority item without removing."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (when-let [item (.peek (:queue queue))]
      {:item-id (:id item)
       :priority (:priority item)
       :effective-priority (:effective-priority item)
       :payload (:payload item)
       :created-at (:created-at item)})))

(defn queue-size
  "Get the size of a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (.size (:queue queue))))

;; ============================================================================
;; Priority Aging
;; ============================================================================

(defn- age-items!
  "Age items in the queue to prevent starvation."
  [queue]
  (let [pq (:queue queue)
        items (vec (.toArray pq))
        max-boost (:max-age-boost queue)
        increment (:aging-increment queue)]
    
    ;; Remove all items
    (.clear pq)
    
    ;; Re-add with aged priorities
    (doseq [item items]
      (let [age-boosts (:age-boosts item)
            new-boosts (if (< age-boosts max-boost)
                         (inc age-boosts)
                         age-boosts)
            new-effective (min (get-in @state [:config :max-priority])
                               (+ (:priority item) (* new-boosts increment)))
            aged-item (assoc item
                             :age-boosts new-boosts
                             :effective-priority new-effective)]
        (.offer pq aged-item)
        (when (> new-boosts age-boosts)
          (swap! (get-in queue [:metrics :boosts]) inc)
          (swap! state update-in [:stats :priority-boosts] inc))))
    
    (swap! state update-in [:stats :aging-events] inc)))

(defn start-aging!
  "Start the aging process for a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (go-loop []
      (when @(:enabled? queue)
        (<! (timeout (:aging-interval-ms queue)))
        (when (pos? (.size (:queue queue)))
          (age-items! queue))
        (recur)))))

;; ============================================================================
;; Priority Boost
;; ============================================================================

(defn boost-priority!
  "Boost the priority of an item."
  [queue-id item-id boost-amount]
  (when-let [queue (get-queue queue-id)]
    (let [pq (:queue queue)
          items (vec (.toArray pq))
          max-priority (get-in @state [:config :max-priority])]
      
      ;; Find and boost the item
      (when-let [item (first (filter #(= (:id %) item-id) items))]
        (.clear pq)
        (doseq [i items]
          (if (= (:id i) item-id)
            (let [new-priority (min max-priority (+ (:effective-priority i) boost-amount))]
              (.offer pq (assoc i :effective-priority new-priority))
              (swap! (get-in queue [:metrics :boosts]) inc)
              (swap! state update-in [:stats :priority-boosts] inc)
              (logging/log :debug "Boosted priority" {:queue-id queue-id
                                                       :item-id item-id
                                                       :new-priority new-priority}))
            (.offer pq i)))
        true))))

(defn set-priority!
  "Set the priority of an item."
  [queue-id item-id new-priority]
  (when-let [queue (get-queue queue-id)]
    (let [pq (:queue queue)
          items (vec (.toArray pq))
          clamped-priority (max (get-in @state [:config :min-priority])
                                (min (get-in @state [:config :max-priority]) new-priority))]
      
      (.clear pq)
      (doseq [i items]
        (if (= (:id i) item-id)
          (.offer pq (assoc i :priority clamped-priority :effective-priority clamped-priority))
          (.offer pq i)))
      true)))

;; ============================================================================
;; Queue Operations
;; ============================================================================

(defn clear-queue!
  "Clear all items from a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (let [count (.size (:queue queue))]
      (.clear (:queue queue))
      (logging/log :info "Cleared queue" {:queue-id queue-id :items-cleared count})
      count)))

(defn enable-queue!
  "Enable a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (reset! (:enabled? queue) true)
    (logging/log :info "Enabled queue" {:queue-id queue-id})))

(defn disable-queue!
  "Disable a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (reset! (:enabled? queue) false)
    (logging/log :info "Disabled queue" {:queue-id queue-id})))

;; ============================================================================
;; Queue Inspection
;; ============================================================================

(defn get-items-by-priority
  "Get items grouped by priority."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (let [items (vec (.toArray (:queue queue)))]
      (group-by :priority items))))

(defn get-queue-distribution
  "Get the priority distribution of a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (let [items (vec (.toArray (:queue queue)))]
      (frequencies (map :priority items)))))

(defn find-item
  "Find an item in the queue."
  [queue-id item-id]
  (when-let [queue (get-queue queue-id)]
    (let [items (vec (.toArray (:queue queue)))]
      (first (filter #(= (:id %) item-id) items)))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-queue-metrics
  "Get metrics for a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    {:queue-id queue-id
     :name (:name queue)
     :size (.size (:queue queue))
     :enqueued @(get-in queue [:metrics :enqueued])
     :dequeued @(get-in queue [:metrics :dequeued])
     :boosts @(get-in queue [:metrics :boosts])
     :aging-enabled? (:aging-enabled? queue)
     :enabled? @(:enabled? queue)}))

(defn get-all-queue-metrics
  "Get metrics for all queues."
  []
  (mapv (fn [[id _]] (get-queue-metrics id)) (:queues @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-priority-queue-stats
  "Get priority queue statistics."
  []
  (let [stats (:stats @state)]
    {:queues-count (count (:queues @state))
     :items-enqueued (:items-enqueued stats)
     :items-dequeued (:items-dequeued stats)
     :priority-boosts (:priority-boosts stats)
     :aging-events (:aging-events stats)}))

(defn reset-stats!
  "Reset priority queue statistics."
  []
  (swap! state assoc :stats {:items-enqueued 0
                             :items-dequeued 0
                             :priority-boosts 0
                             :aging-events 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-priority-queue!
  "Initialize the priority queue system."
  []
  (when-not (:initialized? @state)
    ;; Create default queues
    (create-queue! :analysis
                   {:name "Analysis"
                    :default-priority 5
                    :aging-enabled? true
                    :aging-interval-ms 5000})
    
    (create-queue! :critical
                   {:name "Critical"
                    :default-priority 10
                    :aging-enabled? false})
    
    (create-queue! :background
                   {:name "Background"
                    :default-priority 1
                    :aging-enabled? true
                    :aging-interval-ms 10000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Priority queue initialized")
    (events/emit! :priority-queue-initialized {})
    true))
