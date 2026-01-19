(ns mental-models.pipeline.integration.outbox-pattern
  "Outbox Pattern Module
   
   Reliable event publishing:
   - Outbox storage
   - Event publishing
   - Delivery tracking
   - Retry handling
   - Dead letter queue"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; OUTBOX STATE
;; =============================================================================

(defonce outbox-state (atom {:messages {}
                             :publishers {}
                             :dead-letter []
                             :processor nil
                             :config {:batch-size 100
                                      :poll-interval-ms 1000
                                      :max-retries 5
                                      :retry-delay-ms 5000
                                      :dead-letter-threshold 5}}))

;; =============================================================================
;; MESSAGE CREATION
;; =============================================================================

(defn generate-message-id
  "Generate a unique message ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-message
  "Create an outbox message."
  [topic payload & {:keys [metadata partition-key]}]
  {:id (generate-message-id)
   :topic topic
   :payload payload
   :metadata (or metadata {})
   :partition-key partition-key
   :status :pending
   :retry-count 0
   :last-error nil
   :created-at (System/currentTimeMillis)
   :published-at nil})

;; =============================================================================
;; OUTBOX OPERATIONS
;; =============================================================================

(defn add-to-outbox!
  "Add a message to the outbox."
  [message]
  (when (flags/is-enabled? "outbox-pattern")
    (log/debug "Adding message to outbox" {:id (:id message) :topic (:topic message)})
    (swap! outbox-state assoc-in [:messages (:id message)] message)
    (metrics/inc-counter! :outbox/messages-added)
    (:id message)))

(defn publish!
  "Add a message to the outbox (convenience function)."
  [topic payload & opts]
  (add-to-outbox! (apply create-message topic payload opts)))

(defn get-message
  "Get a message from the outbox."
  [message-id]
  (get-in @outbox-state [:messages message-id]))

(defn update-message!
  "Update a message in the outbox."
  [message-id updates]
  (swap! outbox-state update-in [:messages message-id] merge updates))

(defn delete-message!
  "Delete a message from the outbox."
  [message-id]
  (swap! outbox-state update :messages dissoc message-id))

(defn list-messages
  "List messages in the outbox."
  [& {:keys [status topic limit]}]
  (let [messages (vals (:messages @outbox-state))]
    (cond->> messages
      status (filter #(= (:status %) status))
      topic (filter #(= (:topic %) topic))
      limit (take limit))))

(defn pending-messages
  "Get all pending messages."
  [& {:keys [limit] :or {limit 100}}]
  (list-messages :status :pending :limit limit))

;; =============================================================================
;; PUBLISHER REGISTRATION
;; =============================================================================

(defn register-publisher!
  "Register a publisher for a topic."
  [topic publisher-fn & {:keys [batch-size]}]
  (log/info "Registering publisher" {:topic topic})
  (swap! outbox-state assoc-in [:publishers topic]
         {:topic topic
          :publisher publisher-fn
          :batch-size batch-size
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :outbox/publishers-registered))

(defn unregister-publisher!
  "Unregister a publisher."
  [topic]
  (log/info "Unregistering publisher" {:topic topic})
  (swap! outbox-state update :publishers dissoc topic))

(defn get-publisher
  "Get a publisher for a topic."
  [topic]
  (get-in @outbox-state [:publishers topic]))

(defn list-publishers
  "List all registered publishers."
  []
  (keys (:publishers @outbox-state)))

;; =============================================================================
;; MESSAGE PUBLISHING
;; =============================================================================

(defn publish-message!
  "Publish a single message."
  [message]
  (let [topic (:topic message)
        publisher (get-publisher topic)]
    (if publisher
      (try
        (log/debug "Publishing message" {:id (:id message) :topic topic})
        ((:publisher publisher) message)
        (update-message! (:id message) {:status :published
                                        :published-at (System/currentTimeMillis)})
        (metrics/inc-counter! :outbox/messages-published)
        (events/publish! :outbox/message-published {:id (:id message) :topic topic})
        {:success true}
        (catch Exception e
          (log/error "Failed to publish message" {:id (:id message) :error (.getMessage e)})
          (let [retry-count (inc (:retry-count message))
                max-retries (get-in @outbox-state [:config :max-retries])]
            (if (>= retry-count max-retries)
              ;; Move to dead letter queue
              (do
                (update-message! (:id message) {:status :dead-letter
                                                :retry-count retry-count
                                                :last-error (.getMessage e)})
                (swap! outbox-state update :dead-letter conj message)
                (metrics/inc-counter! :outbox/dead-letter-messages))
              ;; Mark for retry
              (update-message! (:id message) {:status :pending
                                              :retry-count retry-count
                                              :last-error (.getMessage e)}))
            (metrics/inc-counter! :outbox/publish-failures)
            {:success false :error (.getMessage e)})))
      (do
        (log/warn "No publisher for topic" {:topic topic})
        {:success false :error "No publisher registered"}))))

(defn publish-batch!
  "Publish a batch of messages."
  [messages]
  (let [results (map publish-message! messages)]
    {:total (count messages)
     :success (count (filter :success results))
     :failed (count (remove :success results))}))

;; =============================================================================
;; OUTBOX PROCESSOR
;; =============================================================================

(defn process-outbox!
  "Process pending messages in the outbox."
  []
  (when (flags/is-enabled? "outbox-pattern")
    (let [batch-size (get-in @outbox-state [:config :batch-size])
          messages (pending-messages :limit batch-size)]
      (when (seq messages)
        (log/debug "Processing outbox" {:count (count messages)})
        (publish-batch! messages)))))

(defn start-processor!
  "Start the outbox processor."
  []
  (when (and (flags/is-enabled? "outbox-pattern")
             (nil? (:processor @outbox-state)))
    (log/info "Starting outbox processor")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          poll-interval (get-in @outbox-state [:config :poll-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try (process-outbox!)
                                  (catch Exception e
                                    (log/error "Outbox processor error" {:error (.getMessage e)})))
                            0
                            poll-interval
                            TimeUnit/MILLISECONDS)
      (swap! outbox-state assoc :processor executor)
      (events/publish! :outbox/processor-started {}))))

(defn stop-processor!
  "Stop the outbox processor."
  []
  (when-let [^ScheduledExecutorService executor (:processor @outbox-state)]
    (log/info "Stopping outbox processor")
    (.shutdown executor)
    (swap! outbox-state assoc :processor nil)
    (events/publish! :outbox/processor-stopped {})))

;; =============================================================================
;; DEAD LETTER QUEUE
;; =============================================================================

(defn get-dead-letter-messages
  "Get messages in the dead letter queue."
  []
  (:dead-letter @outbox-state))

(defn retry-dead-letter!
  "Retry a message from the dead letter queue."
  [message-id]
  (when-let [message (get-message message-id)]
    (when (= (:status message) :dead-letter)
      (log/info "Retrying dead letter message" {:id message-id})
      (update-message! message-id {:status :pending
                                   :retry-count 0
                                   :last-error nil})
      (swap! outbox-state update :dead-letter
             (fn [dlq] (vec (remove #(= (:id %) message-id) dlq))))
      (metrics/inc-counter! :outbox/dead-letter-retried))))

(defn clear-dead-letter!
  "Clear the dead letter queue."
  []
  (log/info "Clearing dead letter queue")
  (let [dlq-messages (get-dead-letter-messages)]
    (doseq [msg dlq-messages]
      (delete-message! (:id msg)))
    (swap! outbox-state assoc :dead-letter [])
    (count dlq-messages)))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn cleanup-published-messages!
  "Clean up old published messages."
  [& {:keys [max-age-ms] :or {max-age-ms 86400000}}]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        to-remove (filter (fn [[_ msg]]
                            (and (= (:status msg) :published)
                                 (< (:published-at msg) cutoff)))
                          (:messages @outbox-state))]
    (doseq [[msg-id _] to-remove]
      (delete-message! msg-id))
    (count to-remove)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-outbox-pattern!
  "Initialize outbox pattern."
  []
  (log/info "Initializing outbox pattern")
  ;; Register feature flag
  (flags/register-flag! "outbox-pattern" "Enable outbox pattern" true)
  ;; Create metrics
  (metrics/create-counter! :outbox/messages-added "Messages added")
  (metrics/create-counter! :outbox/messages-published "Messages published")
  (metrics/create-counter! :outbox/publish-failures "Publish failures")
  (metrics/create-counter! :outbox/publishers-registered "Publishers registered")
  (metrics/create-counter! :outbox/dead-letter-messages "Dead letter messages")
  (metrics/create-counter! :outbox/dead-letter-retried "Dead letter retried")
  (metrics/create-gauge! :outbox/pending-count "Pending messages"
                         #(count (pending-messages)))
  (metrics/create-gauge! :outbox/dead-letter-count "Dead letter count"
                         #(count (get-dead-letter-messages)))
  (log/info "Outbox pattern initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-outbox-status []
  {:enabled (flags/is-enabled? "outbox-pattern")
   :total-messages (count (:messages @outbox-state))
   :pending (count (pending-messages))
   :published (count (list-messages :status :published))
   :dead-letter (count (get-dead-letter-messages))
   :publishers (count (:publishers @outbox-state))
   :processor-running (some? (:processor @outbox-state))
   :config (:config @outbox-state)})
