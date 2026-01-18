(ns mental-models.retry.queue
  "Retry Queue Module for Mental Models Pipeline
   
   Provides retry functionality with:
   - Exponential backoff
   - Max retry limits
   - Dead letter queue
   - Retry policies
   - Priority-based retry"
  (:require
   [clojure.core.async :as async :refer [go go-loop chan <! >! >!! <!! close! timeout]])
  (:import
   [java.util UUID]
   [java.time Instant Duration]
   [java.util.concurrent PriorityBlockingQueue Comparator]))

;; =============================================================================
;; RETRY ITEM
;; =============================================================================

(defrecord RetryItem [id operation payload attempts max-attempts next-retry created-at last-error priority])

(defn create-retry-item [operation payload & {:keys [max-attempts priority] :or {max-attempts 3 priority 5}}]
  (->RetryItem (str (UUID/randomUUID))
               operation
               payload
               0
               max-attempts
               (Instant/now)
               (Instant/now)
               nil
               priority))

;; =============================================================================
;; BACKOFF STRATEGIES
;; =============================================================================

(defn exponential-backoff [attempt base-ms max-ms]
  (min max-ms (* base-ms (Math/pow 2 attempt))))

(defn linear-backoff [attempt base-ms max-ms]
  (min max-ms (* base-ms (inc attempt))))

(defn constant-backoff [_ base-ms _]
  base-ms)

(def backoff-strategies
  {:exponential exponential-backoff
   :linear linear-backoff
   :constant constant-backoff})

;; =============================================================================
;; RETRY QUEUE STATE
;; =============================================================================

(def ^:private retry-queue
  (PriorityBlockingQueue. 100
    (reify Comparator
      (compare [_ a b]
        (let [time-cmp (.compareTo (:next-retry a) (:next-retry b))]
          (if (zero? time-cmp)
            (- (:priority a) (:priority b))
            time-cmp))))))

(def ^:private dead-letter-queue (atom []))
(def ^:private retry-handlers (atom {}))
(def ^:private retry-config (atom {:base-delay-ms 1000
                                   :max-delay-ms 30000
                                   :backoff-strategy :exponential}))

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defn set-config! [config]
  (swap! retry-config merge config))

(defn get-config []
  @retry-config)

;; =============================================================================
;; HANDLER REGISTRATION
;; =============================================================================

(defn register-handler!
  "Register a handler for an operation type."
  [operation handler]
  (swap! retry-handlers assoc operation handler))

(defn unregister-handler!
  "Unregister a handler for an operation type."
  [operation]
  (swap! retry-handlers dissoc operation))

(defn get-handler [operation]
  (get @retry-handlers operation))

;; =============================================================================
;; QUEUE OPERATIONS
;; =============================================================================

(defn enqueue!
  "Add an item to the retry queue."
  [operation payload & opts]
  (let [item (apply create-retry-item operation payload opts)]
    (.offer retry-queue item)
    item))

(defn dequeue!
  "Remove and return the next item ready for retry."
  []
  (when-let [item (.peek retry-queue)]
    (when (.isBefore (:next-retry item) (Instant/now))
      (.poll retry-queue))))

(defn peek-queue []
  (.peek retry-queue))

(defn queue-size []
  (.size retry-queue))

(defn clear-queue! []
  (.clear retry-queue))

;; =============================================================================
;; RETRY EXECUTION
;; =============================================================================

(defn calculate-next-retry [item]
  (let [{:keys [base-delay-ms max-delay-ms backoff-strategy]} @retry-config
        strategy-fn (get backoff-strategies backoff-strategy exponential-backoff)
        delay-ms (strategy-fn (:attempts item) base-delay-ms max-delay-ms)]
    (.plusMillis (Instant/now) (long delay-ms))))

(defn retry-item! [item]
  (let [handler (get-handler (:operation item))]
    (if handler
      (try
        (handler (:payload item))
        {:status :success :item item}
        (catch Exception e
          (let [new-attempts (inc (:attempts item))]
            (if (>= new-attempts (:max-attempts item))
              (do
                (swap! dead-letter-queue conj (assoc item :last-error (str e)))
                {:status :dead-letter :item item :error (str e)})
              (let [updated-item (assoc item
                                        :attempts new-attempts
                                        :next-retry (calculate-next-retry item)
                                        :last-error (str e))]
                (.offer retry-queue updated-item)
                {:status :requeued :item updated-item :error (str e)})))))
      {:status :no-handler :item item})))

;; =============================================================================
;; RETRY WORKER
;; =============================================================================

(def ^:private worker-running (atom false))
(def ^:private worker-channel (chan))

(defn start-worker!
  "Start the retry worker."
  []
  (when-not @worker-running
    (reset! worker-running true)
    (go-loop []
      (when @worker-running
        (if-let [item (dequeue!)]
          (retry-item! item)
          (<! (timeout 1000)))
        (recur)))
    {:status :started}))

(defn stop-worker!
  "Stop the retry worker."
  []
  (reset! worker-running false)
  {:status :stopped})

;; =============================================================================
;; DEAD LETTER QUEUE
;; =============================================================================

(defn get-dead-letters []
  @dead-letter-queue)

(defn dead-letter-count []
  (count @dead-letter-queue))

(defn clear-dead-letters! []
  (reset! dead-letter-queue []))

(defn retry-dead-letter! [id]
  (let [item (first (filter #(= (:id %) id) @dead-letter-queue))]
    (when item
      (swap! dead-letter-queue #(remove (fn [i] (= (:id i) id)) %))
      (let [reset-item (assoc item :attempts 0 :next-retry (Instant/now))]
        (.offer retry-queue reset-item)
        reset-item))))

(defn retry-all-dead-letters! []
  (let [items @dead-letter-queue]
    (reset! dead-letter-queue [])
    (doseq [item items]
      (let [reset-item (assoc item :attempts 0 :next-retry (Instant/now))]
        (.offer retry-queue reset-item)))
    (count items)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  {:queue-size (queue-size)
   :dead-letter-count (dead-letter-count)
   :worker-running @worker-running
   :config @retry-config
   :handlers (keys @retry-handlers)})

;; =============================================================================
;; BUILT-IN HANDLERS
;; =============================================================================

(defn register-analysis-handler! [analyze-fn]
  (register-handler! :analysis
    (fn [{:keys [document-id text]}]
      (analyze-fn document-id text))))

(defn register-notification-handler! [notify-fn]
  (register-handler! :notification
    (fn [{:keys [channel message]}]
      (notify-fn channel message))))

(defn register-export-handler! [export-fn]
  (register-handler! :export
    (fn [{:keys [format data path]}]
      (export-fn format data path))))

;; =============================================================================
;; CONVENIENCE FUNCTIONS
;; =============================================================================

(defn retry-analysis! [document-id text]
  (enqueue! :analysis {:document-id document-id :text text}))

(defn retry-notification! [channel message]
  (enqueue! :notification {:channel channel :message message}))

(defn retry-export! [format data path]
  (enqueue! :export {:format format :data data :path path}))
