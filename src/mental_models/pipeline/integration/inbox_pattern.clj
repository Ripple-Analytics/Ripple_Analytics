(ns mental-models.pipeline.integration.inbox-pattern
  "Inbox Pattern Module
   
   Reliable message consumption:
   - Inbox storage
   - Deduplication
   - Idempotent processing
   - Message ordering
   - Acknowledgment handling"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

;; =============================================================================
;; INBOX STATE
;; =============================================================================

(defonce inbox-state (atom {:messages {}
                            :processed-ids #{}
                            :handlers {}
                            :processor nil
                            :config {:batch-size 100
                                     :poll-interval-ms 1000
                                     :dedup-window-ms 86400000
                                     :max-retries 3
                                     :ordering-enabled true}}))

;; =============================================================================
;; MESSAGE OPERATIONS
;; =============================================================================

(defn generate-message-id
  "Generate a unique message ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn receive-message!
  "Receive a message into the inbox."
  [message]
  (when (flags/is-enabled? "inbox-pattern")
    (let [message-id (or (:id message) (generate-message-id))
          message (assoc message :id message-id)]
      ;; Check for duplicate
      (if (contains? (:processed-ids @inbox-state) message-id)
        (do
          (log/debug "Duplicate message ignored" {:id message-id})
          (metrics/inc-counter! :inbox/duplicates-ignored)
          nil)
        (do
          (log/debug "Receiving message" {:id message-id :topic (:topic message)})
          (swap! inbox-state assoc-in [:messages message-id]
                 (merge message
                        {:status :pending
                         :retry-count 0
                         :received-at (System/currentTimeMillis)
                         :processed-at nil}))
          (metrics/inc-counter! :inbox/messages-received)
          message-id)))))

(defn get-message
  "Get a message from the inbox."
  [message-id]
  (get-in @inbox-state [:messages message-id]))

(defn update-message!
  "Update a message in the inbox."
  [message-id updates]
  (swap! inbox-state update-in [:messages message-id] merge updates))

(defn delete-message!
  "Delete a message from the inbox."
  [message-id]
  (swap! inbox-state update :messages dissoc message-id))

(defn list-messages
  "List messages in the inbox."
  [& {:keys [status topic limit]}]
  (let [messages (vals (:messages @inbox-state))]
    (cond->> messages
      status (filter #(= (:status %) status))
      topic (filter #(= (:topic %) topic))
      limit (take limit))))

(defn pending-messages
  "Get all pending messages."
  [& {:keys [limit] :or {limit 100}}]
  (let [messages (list-messages :status :pending :limit limit)]
    (if (get-in @inbox-state [:config :ordering-enabled])
      (sort-by :received-at messages)
      messages)))

;; =============================================================================
;; HANDLER REGISTRATION
;; =============================================================================

(defn register-handler!
  "Register a message handler for a topic."
  [topic handler-fn & {:keys [idempotent?] :or {idempotent? true}}]
  (log/info "Registering inbox handler" {:topic topic})
  (swap! inbox-state assoc-in [:handlers topic]
         {:topic topic
          :handler handler-fn
          :idempotent? idempotent?
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :inbox/handlers-registered))

(defn unregister-handler!
  "Unregister a message handler."
  [topic]
  (log/info "Unregistering inbox handler" {:topic topic})
  (swap! inbox-state update :handlers dissoc topic))

(defn get-handler
  "Get a handler for a topic."
  [topic]
  (get-in @inbox-state [:handlers topic]))

(defn list-handlers
  "List all registered handlers."
  []
  (keys (:handlers @inbox-state)))

;; =============================================================================
;; MESSAGE PROCESSING
;; =============================================================================

(defn process-message!
  "Process a single message."
  [message]
  (let [topic (:topic message)
        handler-info (get-handler topic)]
    (if handler-info
      (try
        (log/debug "Processing message" {:id (:id message) :topic topic})
        (update-message! (:id message) {:status :processing})
        ((:handler handler-info) message)
        ;; Mark as processed
        (update-message! (:id message) {:status :processed
                                        :processed-at (System/currentTimeMillis)})
        ;; Add to processed IDs for deduplication
        (swap! inbox-state update :processed-ids conj (:id message))
        (metrics/inc-counter! :inbox/messages-processed)
        (events/publish! :inbox/message-processed {:id (:id message) :topic topic})
        {:success true}
        (catch Exception e
          (log/error "Failed to process message" {:id (:id message) :error (.getMessage e)})
          (let [retry-count (inc (:retry-count message))
                max-retries (get-in @inbox-state [:config :max-retries])]
            (if (>= retry-count max-retries)
              ;; Mark as failed
              (do
                (update-message! (:id message) {:status :failed
                                                :retry-count retry-count
                                                :last-error (.getMessage e)})
                (metrics/inc-counter! :inbox/messages-failed))
              ;; Mark for retry
              (update-message! (:id message) {:status :pending
                                              :retry-count retry-count
                                              :last-error (.getMessage e)}))
            (metrics/inc-counter! :inbox/process-failures)
            {:success false :error (.getMessage e)})))
      (do
        (log/warn "No handler for topic" {:topic topic})
        {:success false :error "No handler registered"}))))

(defn process-batch!
  "Process a batch of messages."
  [messages]
  (let [results (map process-message! messages)]
    {:total (count messages)
     :success (count (filter :success results))
     :failed (count (remove :success results))}))

(defn acknowledge!
  "Acknowledge a message (mark as processed)."
  [message-id]
  (when-let [message (get-message message-id)]
    (log/debug "Acknowledging message" {:id message-id})
    (update-message! message-id {:status :processed
                                 :processed-at (System/currentTimeMillis)})
    (swap! inbox-state update :processed-ids conj message-id)
    (metrics/inc-counter! :inbox/messages-acknowledged)))

(defn reject!
  "Reject a message (mark as failed)."
  [message-id & {:keys [reason]}]
  (when-let [message (get-message message-id)]
    (log/debug "Rejecting message" {:id message-id :reason reason})
    (update-message! message-id {:status :failed
                                 :last-error reason})
    (metrics/inc-counter! :inbox/messages-rejected)))

;; =============================================================================
;; INBOX PROCESSOR
;; =============================================================================

(defn process-inbox!
  "Process pending messages in the inbox."
  []
  (when (flags/is-enabled? "inbox-pattern")
    (let [batch-size (get-in @inbox-state [:config :batch-size])
          messages (pending-messages :limit batch-size)]
      (when (seq messages)
        (log/debug "Processing inbox" {:count (count messages)})
        (process-batch! messages)))))

(defn start-processor!
  "Start the inbox processor."
  []
  (when (and (flags/is-enabled? "inbox-pattern")
             (nil? (:processor @inbox-state)))
    (log/info "Starting inbox processor")
    (let [executor (Executors/newSingleThreadScheduledExecutor)
          poll-interval (get-in @inbox-state [:config :poll-interval-ms])]
      (.scheduleAtFixedRate executor
                            #(try (process-inbox!)
                                  (catch Exception e
                                    (log/error "Inbox processor error" {:error (.getMessage e)})))
                            0
                            poll-interval
                            TimeUnit/MILLISECONDS)
      (swap! inbox-state assoc :processor executor)
      (events/publish! :inbox/processor-started {}))))

(defn stop-processor!
  "Stop the inbox processor."
  []
  (when-let [^ScheduledExecutorService executor (:processor @inbox-state)]
    (log/info "Stopping inbox processor")
    (.shutdown executor)
    (swap! inbox-state assoc :processor nil)
    (events/publish! :inbox/processor-stopped {})))

;; =============================================================================
;; DEDUPLICATION
;; =============================================================================

(defn is-duplicate?
  "Check if a message ID has been processed."
  [message-id]
  (contains? (:processed-ids @inbox-state) message-id))

(defn cleanup-processed-ids!
  "Clean up old processed IDs."
  []
  (let [dedup-window (get-in @inbox-state [:config :dedup-window-ms])
        cutoff (- (System/currentTimeMillis) dedup-window)
        ;; We need to track when IDs were added to properly clean up
        ;; For now, just keep the set manageable
        current-count (count (:processed-ids @inbox-state))]
    (when (> current-count 100000)
      ;; Keep only the most recent half
      (swap! inbox-state update :processed-ids
             (fn [ids] (set (take 50000 ids))))
      (- current-count 50000))))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn cleanup-processed-messages!
  "Clean up old processed messages."
  [& {:keys [max-age-ms] :or {max-age-ms 86400000}}]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        to-remove (filter (fn [[_ msg]]
                            (and (= (:status msg) :processed)
                                 (< (:processed-at msg) cutoff)))
                          (:messages @inbox-state))]
    (doseq [[msg-id _] to-remove]
      (delete-message! msg-id))
    (count to-remove)))

(defn retry-failed-messages!
  "Retry all failed messages."
  []
  (let [failed (list-messages :status :failed)]
    (doseq [msg failed]
      (update-message! (:id msg) {:status :pending
                                  :retry-count 0
                                  :last-error nil}))
    (count failed)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-inbox-pattern!
  "Initialize inbox pattern."
  []
  (log/info "Initializing inbox pattern")
  ;; Register feature flag
  (flags/register-flag! "inbox-pattern" "Enable inbox pattern" true)
  ;; Create metrics
  (metrics/create-counter! :inbox/messages-received "Messages received")
  (metrics/create-counter! :inbox/messages-processed "Messages processed")
  (metrics/create-counter! :inbox/messages-failed "Messages failed")
  (metrics/create-counter! :inbox/messages-acknowledged "Messages acknowledged")
  (metrics/create-counter! :inbox/messages-rejected "Messages rejected")
  (metrics/create-counter! :inbox/duplicates-ignored "Duplicates ignored")
  (metrics/create-counter! :inbox/handlers-registered "Handlers registered")
  (metrics/create-counter! :inbox/process-failures "Process failures")
  (metrics/create-gauge! :inbox/pending-count "Pending messages"
                         #(count (pending-messages)))
  (metrics/create-gauge! :inbox/processed-ids-count "Processed IDs count"
                         #(count (:processed-ids @inbox-state)))
  (log/info "Inbox pattern initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-inbox-status []
  {:enabled (flags/is-enabled? "inbox-pattern")
   :total-messages (count (:messages @inbox-state))
   :pending (count (pending-messages))
   :processed (count (list-messages :status :processed))
   :failed (count (list-messages :status :failed))
   :handlers (count (:handlers @inbox-state))
   :processed-ids (count (:processed-ids @inbox-state))
   :processor-running (some? (:processor @inbox-state))
   :config (:config @inbox-state)})
