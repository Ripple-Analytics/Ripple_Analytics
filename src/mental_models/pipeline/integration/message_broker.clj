(ns mental-models.pipeline.integration.message-broker
  "Message broker for asynchronous communication in mental model analysis.
   
   Features:
   - Topic-based pub/sub
   - Queue-based messaging
   - Message persistence
   - Dead letter queues
   - Message filtering
   - Delivery guarantees
   - Consumer groups
   - Message replay"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout pub sub unsub]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:topics {}           ;; topic-id -> topic-config
         :queues {}           ;; queue-id -> queue-config
         :subscriptions {}    ;; subscription-id -> subscription
         :consumers {}        ;; consumer-id -> consumer-config
         :messages {}         ;; message-id -> message
         :dlq {}              ;; dead letter queue
         :offsets {}          ;; {consumer-group topic} -> offset
         :stats {:published 0 :delivered 0 :failed 0}
         :channels {}         ;; Internal async channels
         :initialized? false}))

;; ============================================================================
;; Topic Management
;; ============================================================================

(defn create-topic!
  "Create a topic."
  [topic-id config]
  (let [topic {:id topic-id
               :name (get config :name (name topic-id))
               :description (get config :description "")
               :partitions (get config :partitions 1)
               :retention-ms (get config :retention-ms 604800000) ;; 7 days
               :max-message-size (get config :max-message-size 1048576) ;; 1MB
               :created-at (System/currentTimeMillis)}
        ch (chan 1000)]
    (swap! state assoc-in [:topics topic-id] topic)
    (swap! state assoc-in [:channels topic-id] ch)
    (logging/log :info "Created topic" {:topic-id topic-id})
    (events/emit! :topic-created {:topic-id topic-id})
    topic-id))

(defn get-topic
  "Get a topic."
  [topic-id]
  (get-in @state [:topics topic-id]))

(defn list-topics
  "List all topics."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :partitions (:partitions t)})
        (:topics @state)))

(defn delete-topic!
  "Delete a topic."
  [topic-id]
  (when-let [ch (get-in @state [:channels topic-id])]
    (async/close! ch))
  (swap! state update :topics dissoc topic-id)
  (swap! state update :channels dissoc topic-id)
  (logging/log :info "Deleted topic" {:topic-id topic-id}))

;; ============================================================================
;; Queue Management
;; ============================================================================

(defn create-queue!
  "Create a queue."
  [queue-id config]
  (let [queue {:id queue-id
               :name (get config :name (name queue-id))
               :description (get config :description "")
               :max-size (get config :max-size 10000)
               :visibility-timeout-ms (get config :visibility-timeout-ms 30000)
               :message-retention-ms (get config :message-retention-ms 345600000) ;; 4 days
               :dlq-id (get config :dlq-id nil)
               :max-receive-count (get config :max-receive-count 3)
               :messages []
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:queues queue-id] queue)
    (logging/log :info "Created queue" {:queue-id queue-id})
    queue-id))

(defn get-queue
  "Get a queue."
  [queue-id]
  (get-in @state [:queues queue-id]))

(defn list-queues
  "List all queues."
  []
  (mapv (fn [[id q]]
          {:id id
           :name (:name q)
           :message-count (count (:messages q))
           :max-size (:max-size q)})
        (:queues @state)))

(defn delete-queue!
  "Delete a queue."
  [queue-id]
  (swap! state update :queues dissoc queue-id)
  (logging/log :info "Deleted queue" {:queue-id queue-id}))

;; ============================================================================
;; Message Publishing
;; ============================================================================

(defn- create-message
  "Create a message."
  [topic-id body & {:keys [key headers partition]}]
  {:id (str (UUID/randomUUID))
   :topic-id topic-id
   :key key
   :body body
   :headers (or headers {})
   :partition (or partition 0)
   :timestamp (System/currentTimeMillis)
   :attempts 0})

(defn publish!
  "Publish a message to a topic."
  [topic-id body & {:keys [key headers partition]}]
  (when (flags/enabled? :message-broker)
    (when-let [topic (get-topic topic-id)]
      (let [message (create-message topic-id body :key key :headers headers :partition partition)]
        ;; Store message
        (swap! state assoc-in [:messages (:id message)] message)
        ;; Put on channel
        (when-let [ch (get-in @state [:channels topic-id])]
          (async/put! ch message))
        (swap! state update-in [:stats :published] inc)
        (metrics/increment :messages-published {:topic-id topic-id})
        (logging/log :debug "Published message" {:topic-id topic-id :message-id (:id message)})
        (:id message)))))

(defn publish-batch!
  "Publish multiple messages to a topic."
  [topic-id messages]
  (mapv #(publish! topic-id %) messages))

;; ============================================================================
;; Queue Operations
;; ============================================================================

(defn send-to-queue!
  "Send a message to a queue."
  [queue-id body & {:keys [delay-ms headers]}]
  (when (flags/enabled? :message-broker)
    (when-let [queue (get-queue queue-id)]
      (let [message {:id (str (UUID/randomUUID))
                     :queue-id queue-id
                     :body body
                     :headers (or headers {})
                     :visible-at (+ (System/currentTimeMillis) (or delay-ms 0))
                     :receive-count 0
                     :created-at (System/currentTimeMillis)}]
        (swap! state update-in [:queues queue-id :messages] conj message)
        (swap! state update-in [:stats :published] inc)
        (logging/log :debug "Sent to queue" {:queue-id queue-id :message-id (:id message)})
        (:id message)))))

(defn receive-from-queue!
  "Receive messages from a queue."
  [queue-id & {:keys [max-messages visibility-timeout-ms] :or {max-messages 1 visibility-timeout-ms 30000}}]
  (when (flags/enabled? :message-broker)
    (when-let [queue (get-queue queue-id)]
      (let [now (System/currentTimeMillis)
            visible-messages (filter #(and (<= (:visible-at %) now)
                                           (nil? (:receipt-handle %)))
                                     (:messages queue))
            to-receive (take max-messages visible-messages)]
        ;; Update visibility
        (doseq [msg to-receive]
          (let [receipt-handle (str (UUID/randomUUID))]
            (swap! state update-in [:queues queue-id :messages]
                   (fn [msgs]
                     (mapv (fn [m]
                             (if (= (:id m) (:id msg))
                               (assoc m
                                      :receipt-handle receipt-handle
                                      :visible-at (+ now visibility-timeout-ms)
                                      :receive-count (inc (:receive-count m)))
                               m))
                           msgs)))))
        (vec to-receive)))))

(defn delete-from-queue!
  "Delete a message from a queue."
  [queue-id message-id]
  (swap! state update-in [:queues queue-id :messages]
         (fn [msgs]
           (filterv #(not= (:id %) message-id) msgs)))
  (swap! state update-in [:stats :delivered] inc)
  (logging/log :debug "Deleted from queue" {:queue-id queue-id :message-id message-id}))

(defn change-visibility!
  "Change message visibility timeout."
  [queue-id message-id visibility-timeout-ms]
  (swap! state update-in [:queues queue-id :messages]
         (fn [msgs]
           (mapv (fn [m]
                   (if (= (:id m) message-id)
                     (assoc m :visible-at (+ (System/currentTimeMillis) visibility-timeout-ms))
                     m))
                 msgs))))

;; ============================================================================
;; Subscriptions
;; ============================================================================

(defn subscribe!
  "Subscribe to a topic."
  [subscription-id config]
  (let [subscription {:id subscription-id
                      :topic-id (get config :topic-id)
                      :consumer-group (get config :consumer-group "default")
                      :filter-fn (get config :filter-fn nil)
                      :handler (get config :handler (fn [_] nil))
                      :auto-ack? (get config :auto-ack? true)
                      :created-at (System/currentTimeMillis)}
        ch (chan 100)]
    (swap! state assoc-in [:subscriptions subscription-id] (assoc subscription :channel ch))
    
    ;; Start consumer loop
    (go-loop []
      (when-let [topic-ch (get-in @state [:channels (:topic-id subscription)])]
        (when-let [message (<! topic-ch)]
          ;; Apply filter
          (when (or (nil? (:filter-fn subscription))
                    ((:filter-fn subscription) message))
            (try
              ((:handler subscription) message)
              (swap! state update-in [:stats :delivered] inc)
              (catch Exception e
                (logging/log :error "Message handler failed" {:subscription-id subscription-id :error (.getMessage e)})
                (swap! state update-in [:stats :failed] inc))))
          (recur))))
    
    (logging/log :info "Created subscription" {:subscription-id subscription-id :topic-id (:topic-id subscription)})
    subscription-id))

(defn unsubscribe!
  "Unsubscribe from a topic."
  [subscription-id]
  (when-let [subscription (get-in @state [:subscriptions subscription-id])]
    (when-let [ch (:channel subscription)]
      (async/close! ch)))
  (swap! state update :subscriptions dissoc subscription-id)
  (logging/log :info "Removed subscription" {:subscription-id subscription-id}))

(defn list-subscriptions
  "List all subscriptions."
  [& {:keys [topic-id]}]
  (let [subs (vals (:subscriptions @state))
        filtered (if topic-id
                   (filter #(= (:topic-id %) topic-id) subs)
                   subs)]
    (mapv #(select-keys % [:id :topic-id :consumer-group]) filtered)))

;; ============================================================================
;; Consumer Groups
;; ============================================================================

(defn get-consumer-offset
  "Get consumer group offset for a topic."
  [consumer-group topic-id]
  (get-in @state [:offsets [consumer-group topic-id]] 0))

(defn commit-offset!
  "Commit consumer group offset."
  [consumer-group topic-id offset]
  (swap! state assoc-in [:offsets [consumer-group topic-id]] offset)
  (logging/log :debug "Committed offset" {:consumer-group consumer-group :topic-id topic-id :offset offset}))

;; ============================================================================
;; Dead Letter Queue
;; ============================================================================

(defn move-to-dlq!
  "Move a message to the dead letter queue."
  [queue-id message-id & {:keys [reason]}]
  (when-let [queue (get-queue queue-id)]
    (let [message (first (filter #(= (:id %) message-id) (:messages queue)))]
      (when message
        ;; Add to DLQ
        (swap! state update :dlq conj (assoc message
                                             :original-queue queue-id
                                             :dlq-reason reason
                                             :moved-at (System/currentTimeMillis)))
        ;; Remove from original queue
        (delete-from-queue! queue-id message-id)
        (logging/log :warn "Moved message to DLQ" {:queue-id queue-id :message-id message-id :reason reason})))))

(defn get-dlq-messages
  "Get messages from the dead letter queue."
  [& {:keys [limit]}]
  (let [messages (:dlq @state)]
    (if limit
      (take limit messages)
      messages)))

(defn replay-dlq-message!
  "Replay a message from the DLQ."
  [message-id]
  (when-let [message (first (filter #(= (:id %) message-id) (:dlq @state)))]
    (send-to-queue! (:original-queue message) (:body message))
    (swap! state update :dlq (fn [msgs] (filterv #(not= (:id %) message-id) msgs)))
    (logging/log :info "Replayed DLQ message" {:message-id message-id})))

;; ============================================================================
;; Message Replay
;; ============================================================================

(defn replay-messages!
  "Replay messages from a topic starting from an offset."
  [topic-id from-offset & {:keys [to-offset handler]}]
  (let [messages (filter (fn [[_ m]]
                           (and (= (:topic-id m) topic-id)
                                (>= (:timestamp m) from-offset)
                                (or (nil? to-offset) (<= (:timestamp m) to-offset))))
                         (:messages @state))
        sorted (sort-by #(:timestamp (val %)) messages)]
    (doseq [[_ message] sorted]
      (when handler
        (handler message)))
    (count sorted)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-broker-stats
  "Get message broker statistics."
  []
  (let [stats (:stats @state)
        queues (vals (:queues @state))]
    {:total-topics (count (:topics @state))
     :total-queues (count (:queues @state))
     :total-subscriptions (count (:subscriptions @state))
     :messages-published (:published stats)
     :messages-delivered (:delivered stats)
     :messages-failed (:failed stats)
     :total-messages-in-queues (reduce + (map #(count (:messages %)) queues))
     :dlq-size (count (:dlq @state))}))

(defn get-topic-stats
  "Get statistics for a topic."
  [topic-id]
  (let [messages (filter (fn [[_ m]] (= (:topic-id m) topic-id)) (:messages @state))
        subscriptions (filter #(= (:topic-id %) topic-id) (vals (:subscriptions @state)))]
    {:topic-id topic-id
     :message-count (count messages)
     :subscription-count (count subscriptions)
     :oldest-message (when (seq messages)
                       (:timestamp (val (first (sort-by #(:timestamp (val %)) messages)))))
     :newest-message (when (seq messages)
                       (:timestamp (val (last (sort-by #(:timestamp (val %)) messages)))))}))

(defn get-queue-stats
  "Get statistics for a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (let [messages (:messages queue)
          now (System/currentTimeMillis)]
      {:queue-id queue-id
       :total-messages (count messages)
       :visible-messages (count (filter #(<= (:visible-at %) now) messages))
       :in-flight-messages (count (filter #(> (:visible-at %) now) messages))
       :oldest-message (when (seq messages)
                         (:created-at (first (sort-by :created-at messages))))})))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-message-broker!
  "Initialize the message broker."
  []
  (when-not (:initialized? @state)
    ;; Create default topics
    (create-topic! :analysis-events
                   {:name "Analysis Events"
                    :description "Events from analysis pipeline"
                    :partitions 4})
    
    (create-topic! :model-updates
                   {:name "Model Updates"
                    :description "Mental model update notifications"
                    :partitions 2})
    
    (create-topic! :alerts
                   {:name "Alerts"
                    :description "System alerts and notifications"
                    :partitions 1})
    
    ;; Create default queues
    (create-queue! :analysis-jobs
                   {:name "Analysis Jobs"
                    :description "Queue for analysis job processing"
                    :max-size 10000})
    
    (create-queue! :notifications
                   {:name "Notifications"
                    :description "Queue for notification delivery"
                    :max-size 5000})
    
    ;; Create DLQ
    (create-queue! :dlq
                   {:name "Dead Letter Queue"
                    :description "Failed messages"
                    :max-size 10000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Message broker initialized")
    (events/emit! :message-broker-initialized {})
    true))
