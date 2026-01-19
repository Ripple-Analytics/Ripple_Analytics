(ns mental-models.pipeline.integration.message-queue
  "Message Queue Module
   
   Asynchronous messaging:
   - Queue management
   - Message publishing/consuming
   - Dead letter queues
   - Message acknowledgment
   - Priority queues"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent LinkedBlockingQueue PriorityBlockingQueue TimeUnit Executors ScheduledExecutorService]
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util UUID]))

;; =============================================================================
;; MESSAGE QUEUE STATE
;; =============================================================================

(defonce queue-state (atom {:queues {}
                            :consumers {}
                            :scheduler nil
                            :config {:default-max-size 10000
                                     :default-visibility-timeout-ms 30000
                                     :default-max-retries 3
                                     :cleanup-interval-ms 60000}}))

;; =============================================================================
;; MESSAGE
;; =============================================================================

(defn create-message
  "Create a message."
  [payload & {:keys [priority headers delay-ms]}]
  {:id (str (UUID/randomUUID))
   :payload payload
   :priority (or priority 0)
   :headers (or headers {})
   :delay-until (when delay-ms (+ (System/currentTimeMillis) delay-ms))
   :created-at (System/currentTimeMillis)
   :attempts (AtomicLong. 0)
   :visible-at (AtomicLong. 0)
   :acknowledged? (AtomicBoolean. false)})

(defn is-visible?
  "Check if a message is visible."
  [message]
  (let [visible-at (.get ^AtomicLong (:visible-at message))
        delay-until (:delay-until message)
        now (System/currentTimeMillis)]
    (and (or (zero? visible-at) (>= now visible-at))
         (or (nil? delay-until) (>= now delay-until)))))

(defn is-acknowledged?
  "Check if a message is acknowledged."
  [message]
  (.get ^AtomicBoolean (:acknowledged? message)))

(defn acknowledge!
  "Acknowledge a message."
  [message]
  (.set ^AtomicBoolean (:acknowledged? message) true))

(defn set-invisible!
  "Set a message as invisible for a duration."
  [message timeout-ms]
  (.set ^AtomicLong (:visible-at message) (+ (System/currentTimeMillis) timeout-ms)))

(defn increment-attempts!
  "Increment the attempt count."
  [message]
  (.incrementAndGet ^AtomicLong (:attempts message)))

;; =============================================================================
;; QUEUE CREATION
;; =============================================================================

(defn create-queue
  "Create a new queue."
  [queue-id {:keys [max-size visibility-timeout-ms max-retries dead-letter-queue priority?]}]
  (let [config (:config @queue-state)]
    {:id queue-id
     :max-size (or max-size (:default-max-size config))
     :visibility-timeout-ms (or visibility-timeout-ms (:default-visibility-timeout-ms config))
     :max-retries (or max-retries (:default-max-retries config))
     :dead-letter-queue dead-letter-queue
     :priority? (or priority? false)
     :messages (if priority?
                 (PriorityBlockingQueue. 100 (comparator (fn [a b] (compare (:priority b) (:priority a)))))
                 (LinkedBlockingQueue.))
     :in-flight (atom {})
     :enqueued (AtomicLong. 0)
     :dequeued (AtomicLong. 0)
     :acknowledged (AtomicLong. 0)
     :dead-lettered (AtomicLong. 0)
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; QUEUE REGISTRATION
;; =============================================================================

(defn register-queue!
  "Register a queue."
  [queue-id opts]
  (log/info "Registering queue" {:id queue-id})
  (let [queue (create-queue queue-id opts)]
    (swap! queue-state assoc-in [:queues queue-id] queue)
    (metrics/inc-counter! :messagequeue/queues-registered)
    queue-id))

(defn unregister-queue!
  "Unregister a queue."
  [queue-id]
  (log/info "Unregistering queue" {:id queue-id})
  (swap! queue-state update :queues dissoc queue-id))

(defn get-queue
  "Get a queue."
  [queue-id]
  (get-in @queue-state [:queues queue-id]))

(defn list-queues
  "List all queues."
  []
  (keys (:queues @queue-state)))

;; =============================================================================
;; PUBLISHING
;; =============================================================================

(defn publish!
  "Publish a message to a queue."
  [queue-id payload & opts]
  (when-let [queue (get-queue queue-id)]
    (let [message (apply create-message payload opts)
          messages (:messages queue)]
      (if (< (.size messages) (:max-size queue))
        (do
          (.offer messages message)
          (.incrementAndGet ^AtomicLong (:enqueued queue))
          (metrics/inc-counter! :messagequeue/messages-published)
          (events/publish! :messagequeue/message-published {:queue queue-id :message-id (:id message)})
          (:id message))
        (do
          (log/warn "Queue full" {:queue queue-id})
          (metrics/inc-counter! :messagequeue/queue-full-rejections)
          nil)))))

(defn publish-batch!
  "Publish multiple messages to a queue."
  [queue-id payloads & opts]
  (doall (for [payload payloads]
           (apply publish! queue-id payload opts))))

(defn publish-delayed!
  "Publish a delayed message."
  [queue-id payload delay-ms & opts]
  (apply publish! queue-id payload :delay-ms delay-ms opts))

;; =============================================================================
;; CONSUMING
;; =============================================================================

(defn receive!
  "Receive a message from a queue."
  [queue-id & {:keys [timeout-ms]}]
  (when-let [queue (get-queue queue-id)]
    (let [messages (:messages queue)
          timeout (or timeout-ms 0)]
      (loop []
        (when-let [message (if (pos? timeout)
                            (.poll messages timeout TimeUnit/MILLISECONDS)
                            (.poll messages))]
          (if (and (is-visible? message) (not (is-acknowledged? message)))
            (do
              (increment-attempts! message)
              (set-invisible! message (:visibility-timeout-ms queue))
              (swap! (:in-flight queue) assoc (:id message) message)
              (.incrementAndGet ^AtomicLong (:dequeued queue))
              (metrics/inc-counter! :messagequeue/messages-received)
              message)
            (recur)))))))

(defn receive-batch!
  "Receive multiple messages from a queue."
  [queue-id max-messages & opts]
  (loop [messages []
         remaining max-messages]
    (if (zero? remaining)
      messages
      (if-let [message (apply receive! queue-id opts)]
        (recur (conj messages message) (dec remaining))
        messages))))

(defn ack!
  "Acknowledge a message."
  [queue-id message-id]
  (when-let [queue (get-queue queue-id)]
    (when-let [message (get @(:in-flight queue) message-id)]
      (acknowledge! message)
      (swap! (:in-flight queue) dissoc message-id)
      (.incrementAndGet ^AtomicLong (:acknowledged queue))
      (metrics/inc-counter! :messagequeue/messages-acknowledged)
      (events/publish! :messagequeue/message-acknowledged {:queue queue-id :message-id message-id})
      true)))

(defn nack!
  "Negative acknowledge a message (return to queue)."
  [queue-id message-id & {:keys [delay-ms]}]
  (when-let [queue (get-queue queue-id)]
    (when-let [message (get @(:in-flight queue) message-id)]
      (swap! (:in-flight queue) dissoc message-id)
      (if (>= (.get ^AtomicLong (:attempts message)) (:max-retries queue))
        ;; Move to dead letter queue
        (when-let [dlq (:dead-letter-queue queue)]
          (publish! dlq (:payload message) :headers (assoc (:headers message)
                                                           :original-queue queue-id
                                                           :attempts (.get ^AtomicLong (:attempts message))))
          (.incrementAndGet ^AtomicLong (:dead-lettered queue))
          (metrics/inc-counter! :messagequeue/messages-dead-lettered))
        ;; Return to queue
        (do
          (when delay-ms
            (set-invisible! message delay-ms))
          (.set ^AtomicLong (:visible-at message) 0)
          (.offer (:messages queue) message)))
      true)))

;; =============================================================================
;; CONSUMERS
;; =============================================================================

(defn register-consumer!
  "Register a message consumer."
  [consumer-id queue-id handler & {:keys [concurrency auto-ack]}]
  (log/info "Registering consumer" {:id consumer-id :queue queue-id})
  (let [running (AtomicBoolean. true)
        concurrency (or concurrency 1)
        workers (doall
                 (for [i (range concurrency)]
                   (future
                     (while (.get running)
                       (try
                         (when-let [message (receive! queue-id :timeout-ms 1000)]
                           (try
                             (handler message)
                             (when auto-ack
                               (ack! queue-id (:id message)))
                             (catch Exception e
                               (log/error "Consumer handler error" {:consumer consumer-id :error (.getMessage e)})
                               (nack! queue-id (:id message)))))
                         (catch Exception e
                           (log/error "Consumer error" {:consumer consumer-id :error (.getMessage e)})))))))]
    (swap! queue-state assoc-in [:consumers consumer-id]
           {:id consumer-id
            :queue-id queue-id
            :running running
            :workers workers
            :concurrency concurrency})
    consumer-id))

(defn unregister-consumer!
  "Unregister a message consumer."
  [consumer-id]
  (when-let [consumer (get-in @queue-state [:consumers consumer-id])]
    (log/info "Unregistering consumer" {:id consumer-id})
    (.set ^AtomicBoolean (:running consumer) false)
    (swap! queue-state update :consumers dissoc consumer-id)))

(defn list-consumers
  "List all consumers."
  []
  (keys (:consumers @queue-state)))

;; =============================================================================
;; QUEUE OPERATIONS
;; =============================================================================

(defn purge-queue!
  "Purge all messages from a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (log/info "Purging queue" {:id queue-id})
    (.clear (:messages queue))
    (reset! (:in-flight queue) {})))

(defn queue-size
  "Get the size of a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (.size (:messages queue))))

(defn in-flight-count
  "Get the count of in-flight messages."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (count @(:in-flight queue))))

;; =============================================================================
;; VISIBILITY TIMEOUT HANDLING
;; =============================================================================

(defn return-expired-messages!
  "Return messages with expired visibility timeout to the queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    (let [now (System/currentTimeMillis)
          in-flight @(:in-flight queue)
          expired (filter (fn [[_ msg]]
                            (and (not (is-acknowledged? msg))
                                 (< (.get ^AtomicLong (:visible-at msg)) now)))
                          in-flight)]
      (doseq [[msg-id message] expired]
        (swap! (:in-flight queue) dissoc msg-id)
        (if (>= (.get ^AtomicLong (:attempts message)) (:max-retries queue))
          (when-let [dlq (:dead-letter-queue queue)]
            (publish! dlq (:payload message))
            (.incrementAndGet ^AtomicLong (:dead-lettered queue)))
          (.offer (:messages queue) message))))))

(defn cleanup-all-queues!
  "Clean up all queues."
  []
  (doseq [queue-id (list-queues)]
    (return-expired-messages! queue-id)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-queue-stats
  "Get statistics for a queue."
  [queue-id]
  (when-let [queue (get-queue queue-id)]
    {:id queue-id
     :size (.size (:messages queue))
     :max-size (:max-size queue)
     :in-flight (count @(:in-flight queue))
     :enqueued (.get ^AtomicLong (:enqueued queue))
     :dequeued (.get ^AtomicLong (:dequeued queue))
     :acknowledged (.get ^AtomicLong (:acknowledged queue))
     :dead-lettered (.get ^AtomicLong (:dead-lettered queue))
     :utilization (* 100.0 (/ (.size (:messages queue)) (:max-size queue)))}))

(defn get-all-queue-stats
  "Get statistics for all queues."
  []
  (into {} (for [queue-id (list-queues)]
             [queue-id (get-queue-stats queue-id)])))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-cleanup-scheduler!
  "Start the cleanup scheduler."
  []
  (when (and (flags/is-enabled? "message-queue")
             (nil? (:scheduler @queue-state)))
    (log/info "Starting message queue cleanup scheduler")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          interval (get-in @queue-state [:config :cleanup-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try
                               (cleanup-all-queues!)
                               (catch Exception e
                                 (log/error "Queue cleanup error" {:error (.getMessage e)})))
                            interval
                            interval
                            TimeUnit/MILLISECONDS)
      (swap! queue-state assoc :scheduler executor))))

(defn stop-cleanup-scheduler!
  "Stop the cleanup scheduler."
  []
  (when-let [^ScheduledExecutorService executor (:scheduler @queue-state)]
    (log/info "Stopping message queue cleanup scheduler")
    (.shutdown executor)
    (swap! queue-state assoc :scheduler nil)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-message-queue!
  "Initialize message queue."
  []
  (log/info "Initializing message queue")
  ;; Register feature flag
  (flags/register-flag! "message-queue" "Enable message queue" true)
  ;; Create metrics
  (metrics/create-counter! :messagequeue/queues-registered "Queues registered")
  (metrics/create-counter! :messagequeue/messages-published "Messages published")
  (metrics/create-counter! :messagequeue/messages-received "Messages received")
  (metrics/create-counter! :messagequeue/messages-acknowledged "Messages acknowledged")
  (metrics/create-counter! :messagequeue/messages-dead-lettered "Messages dead-lettered")
  (metrics/create-counter! :messagequeue/queue-full-rejections "Queue full rejections")
  (metrics/create-gauge! :messagequeue/total-queues "Total queues"
                         #(count (:queues @queue-state)))
  (log/info "Message queue initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-queue-status []
  {:enabled (flags/is-enabled? "message-queue")
   :queues (count (:queues @queue-state))
   :consumers (count (:consumers @queue-state))
   :scheduler-active (some? (:scheduler @queue-state))
   :stats (get-all-queue-stats)
   :config (:config @queue-state)})
