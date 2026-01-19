(ns mental-models.pipeline.integration.stream-processor
  "Stream Processor Module
   
   Real-time data processing:
   - Stream creation and management
   - Windowing operations
   - Stateful processing
   - Backpressure handling
   - Stream joins"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent.atomic AtomicLong AtomicBoolean]
   [java.util UUID]))

;; =============================================================================
;; STREAM PROCESSOR STATE
;; =============================================================================

(defonce stream-state (atom {:streams {}
                             :processors {}
                             :windows {}
                             :config {:default-buffer-size 1000
                                      :default-window-size-ms 60000
                                      :default-slide-ms 10000}}))

;; =============================================================================
;; STREAM CREATION
;; =============================================================================

(defn create-stream
  "Create a new stream."
  [stream-id {:keys [buffer-size]}]
  (let [config (:config @stream-state)
        buf-size (or buffer-size (:default-buffer-size config))]
    {:id stream-id
     :channel (chan buf-size)
     :buffer-size buf-size
     :subscribers (atom [])
     :records-in (AtomicLong. 0)
     :records-out (AtomicLong. 0)
     :active? (AtomicBoolean. true)
     :created-at (System/currentTimeMillis)}))

;; =============================================================================
;; STREAM REGISTRATION
;; =============================================================================

(defn register-stream!
  "Register a stream."
  [stream-id opts]
  (log/info "Registering stream" {:id stream-id})
  (let [stream (create-stream stream-id opts)]
    (swap! stream-state assoc-in [:streams stream-id] stream)
    (metrics/inc-counter! :streamprocessor/streams-registered)
    stream-id))

(defn unregister-stream!
  "Unregister a stream."
  [stream-id]
  (when-let [stream (get-in @stream-state [:streams stream-id])]
    (log/info "Unregistering stream" {:id stream-id})
    (.set ^AtomicBoolean (:active? stream) false)
    (close! (:channel stream))
    (swap! stream-state update :streams dissoc stream-id)))

(defn get-stream
  "Get a stream."
  [stream-id]
  (get-in @stream-state [:streams stream-id]))

(defn list-streams
  "List all streams."
  []
  (keys (:streams @stream-state)))

;; =============================================================================
;; STREAM OPERATIONS
;; =============================================================================

(defn emit!
  "Emit a record to a stream."
  [stream-id record]
  (when-let [stream (get-stream stream-id)]
    (when (.get ^AtomicBoolean (:active? stream))
      (go
        (>! (:channel stream) {:data record
                               :timestamp (System/currentTimeMillis)
                               :id (str (UUID/randomUUID))})
        (.incrementAndGet ^AtomicLong (:records-in stream))
        (metrics/inc-counter! :streamprocessor/records-emitted)))))

(defn emit-batch!
  "Emit multiple records to a stream."
  [stream-id records]
  (doseq [record records]
    (emit! stream-id record)))

(defn subscribe!
  "Subscribe to a stream."
  [stream-id handler]
  (when-let [stream (get-stream stream-id)]
    (let [sub-id (str (UUID/randomUUID))
          sub-chan (chan (:buffer-size stream))]
      ;; Add subscriber
      (swap! (:subscribers stream) conj {:id sub-id :channel sub-chan :handler handler})
      ;; Start forwarding
      (go-loop []
        (when-let [record (<! (:channel stream))]
          (when (.get ^AtomicBoolean (:active? stream))
            ;; Forward to all subscribers
            (doseq [{:keys [channel]} @(:subscribers stream)]
              (>! channel record))
            (.incrementAndGet ^AtomicLong (:records-out stream))
            (recur))))
      ;; Start handler
      (go-loop []
        (when-let [record (<! sub-chan)]
          (try
            (handler record)
            (catch Exception e
              (log/error "Stream handler error" {:stream stream-id :error (.getMessage e)})))
          (recur)))
      sub-id)))

(defn unsubscribe!
  "Unsubscribe from a stream."
  [stream-id sub-id]
  (when-let [stream (get-stream stream-id)]
    (let [subs @(:subscribers stream)
          sub (first (filter #(= (:id %) sub-id) subs))]
      (when sub
        (close! (:channel sub))
        (swap! (:subscribers stream) (fn [s] (vec (remove #(= (:id %) sub-id) s))))))))

;; =============================================================================
;; STREAM PROCESSORS
;; =============================================================================

(defn create-processor
  "Create a stream processor."
  [processor-id {:keys [input-stream output-stream handler filter-fn map-fn]}]
  {:id processor-id
   :input-stream input-stream
   :output-stream output-stream
   :handler handler
   :filter-fn filter-fn
   :map-fn map-fn
   :records-processed (AtomicLong. 0)
   :active? (AtomicBoolean. true)
   :created-at (System/currentTimeMillis)})

(defn register-processor!
  "Register a stream processor."
  [processor-id opts]
  (log/info "Registering processor" {:id processor-id})
  (let [processor (create-processor processor-id opts)]
    (swap! stream-state assoc-in [:processors processor-id] processor)
    ;; Start processing
    (when-let [input-stream (get-stream (:input-stream opts))]
      (subscribe! (:input-stream opts)
                  (fn [record]
                    (when (.get ^AtomicBoolean (:active? processor))
                      (let [data (:data record)
                            ;; Apply filter
                            passes-filter? (if (:filter-fn processor)
                                             ((:filter-fn processor) data)
                                             true)]
                        (when passes-filter?
                          ;; Apply map
                          (let [mapped-data (if (:map-fn processor)
                                              ((:map-fn processor) data)
                                              data)]
                            ;; Apply handler
                            (when (:handler processor)
                              ((:handler processor) mapped-data))
                            ;; Emit to output stream
                            (when (:output-stream processor)
                              (emit! (:output-stream processor) mapped-data))
                            (.incrementAndGet ^AtomicLong (:records-processed processor)))))))))
    (metrics/inc-counter! :streamprocessor/processors-registered)
    processor-id))

(defn unregister-processor!
  "Unregister a stream processor."
  [processor-id]
  (when-let [processor (get-in @stream-state [:processors processor-id])]
    (log/info "Unregistering processor" {:id processor-id})
    (.set ^AtomicBoolean (:active? processor) false)
    (swap! stream-state update :processors dissoc processor-id)))

(defn get-processor
  "Get a stream processor."
  [processor-id]
  (get-in @stream-state [:processors processor-id]))

(defn list-processors
  "List all stream processors."
  []
  (keys (:processors @stream-state)))

;; =============================================================================
;; WINDOWING
;; =============================================================================

(defn create-window
  "Create a time window."
  [window-id {:keys [stream-id window-size-ms slide-ms aggregator]}]
  (let [config (:config @stream-state)]
    {:id window-id
     :stream-id stream-id
     :window-size-ms (or window-size-ms (:default-window-size-ms config))
     :slide-ms (or slide-ms (:default-slide-ms config))
     :aggregator aggregator
     :buffer (atom [])
     :results (atom [])
     :active? (AtomicBoolean. true)
     :created-at (System/currentTimeMillis)}))

(defn register-window!
  "Register a time window."
  [window-id opts]
  (log/info "Registering window" {:id window-id})
  (let [window (create-window window-id opts)]
    (swap! stream-state assoc-in [:windows window-id] window)
    ;; Subscribe to stream
    (subscribe! (:stream-id opts)
                (fn [record]
                  (when (.get ^AtomicBoolean (:active? window))
                    (swap! (:buffer window) conj record))))
    ;; Start window processing
    (go-loop []
      (when (.get ^AtomicBoolean (:active? window))
        (<! (timeout (:slide-ms window)))
        (let [now (System/currentTimeMillis)
              cutoff (- now (:window-size-ms window))
              buffer @(:buffer window)
              ;; Get records in window
              window-records (filter #(>= (:timestamp %) cutoff) buffer)
              ;; Apply aggregator
              result (when (:aggregator window)
                       ((:aggregator window) (map :data window-records)))]
          ;; Store result
          (when result
            (swap! (:results window) conj {:timestamp now
                                           :value result
                                           :record-count (count window-records)}))
          ;; Clean old records from buffer
          (swap! (:buffer window) (fn [b] (vec (filter #(>= (:timestamp %) cutoff) b)))))
        (recur)))
    (metrics/inc-counter! :streamprocessor/windows-registered)
    window-id))

(defn unregister-window!
  "Unregister a time window."
  [window-id]
  (when-let [window (get-in @stream-state [:windows window-id])]
    (log/info "Unregistering window" {:id window-id})
    (.set ^AtomicBoolean (:active? window) false)
    (swap! stream-state update :windows dissoc window-id)))

(defn get-window
  "Get a time window."
  [window-id]
  (get-in @stream-state [:windows window-id]))

(defn get-window-results
  "Get results from a time window."
  [window-id & {:keys [limit]}]
  (when-let [window (get-window window-id)]
    (let [results @(:results window)]
      (if limit
        (take-last limit results)
        results))))

;; =============================================================================
;; BUILT-IN AGGREGATORS
;; =============================================================================

(defn count-aggregator
  "Count aggregator."
  []
  (fn [records] (count records)))

(defn sum-aggregator
  "Sum aggregator."
  [key-fn]
  (fn [records] (reduce + (map key-fn records))))

(defn avg-aggregator
  "Average aggregator."
  [key-fn]
  (fn [records]
    (let [values (map key-fn records)]
      (when (seq values)
        (/ (reduce + values) (count values))))))

(defn min-aggregator
  "Min aggregator."
  [key-fn]
  (fn [records]
    (when (seq records)
      (apply min (map key-fn records)))))

(defn max-aggregator
  "Max aggregator."
  [key-fn]
  (fn [records]
    (when (seq records)
      (apply max (map key-fn records)))))

(defn group-aggregator
  "Group by aggregator."
  [group-fn agg-fn]
  (fn [records]
    (into {} (for [[k v] (group-by group-fn records)]
               [k (agg-fn v)]))))

;; =============================================================================
;; STREAM JOINS
;; =============================================================================

(defn join-streams!
  "Join two streams."
  [join-id {:keys [left-stream right-stream join-key-fn window-ms output-stream]}]
  (let [left-buffer (atom {})
        right-buffer (atom {})
        window-ms (or window-ms 60000)]
    ;; Subscribe to left stream
    (subscribe! left-stream
                (fn [record]
                  (let [key (join-key-fn (:data record))]
                    (swap! left-buffer assoc key record)
                    ;; Check for match
                    (when-let [right-record (get @right-buffer key)]
                      (emit! output-stream {:left (:data record)
                                            :right (:data right-record)
                                            :joined-at (System/currentTimeMillis)})))))
    ;; Subscribe to right stream
    (subscribe! right-stream
                (fn [record]
                  (let [key (join-key-fn (:data record))]
                    (swap! right-buffer assoc key record)
                    ;; Check for match
                    (when-let [left-record (get @left-buffer key)]
                      (emit! output-stream {:left (:data left-record)
                                            :right (:data record)
                                            :joined-at (System/currentTimeMillis)})))))
    ;; Cleanup old records
    (go-loop []
      (<! (timeout window-ms))
      (let [cutoff (- (System/currentTimeMillis) window-ms)]
        (swap! left-buffer (fn [b] (into {} (filter #(>= (:timestamp (val %)) cutoff) b))))
        (swap! right-buffer (fn [b] (into {} (filter #(>= (:timestamp (val %)) cutoff) b)))))
      (recur))
    join-id))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stream-stats
  "Get statistics for a stream."
  [stream-id]
  (when-let [stream (get-stream stream-id)]
    {:id stream-id
     :active? (.get ^AtomicBoolean (:active? stream))
     :buffer-size (:buffer-size stream)
     :subscribers (count @(:subscribers stream))
     :records-in (.get ^AtomicLong (:records-in stream))
     :records-out (.get ^AtomicLong (:records-out stream))}))

(defn get-processor-stats
  "Get statistics for a processor."
  [processor-id]
  (when-let [processor (get-processor processor-id)]
    {:id processor-id
     :active? (.get ^AtomicBoolean (:active? processor))
     :input-stream (:input-stream processor)
     :output-stream (:output-stream processor)
     :records-processed (.get ^AtomicLong (:records-processed processor))}))

(defn get-all-stats
  "Get statistics for all streams and processors."
  []
  {:streams (into {} (for [stream-id (list-streams)]
                       [stream-id (get-stream-stats stream-id)]))
   :processors (into {} (for [processor-id (list-processors)]
                          [processor-id (get-processor-stats processor-id)]))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-stream-processor!
  "Initialize stream processor."
  []
  (log/info "Initializing stream processor")
  ;; Register feature flag
  (flags/register-flag! "stream-processor" "Enable stream processor" true)
  ;; Create metrics
  (metrics/create-counter! :streamprocessor/streams-registered "Streams registered")
  (metrics/create-counter! :streamprocessor/processors-registered "Processors registered")
  (metrics/create-counter! :streamprocessor/windows-registered "Windows registered")
  (metrics/create-counter! :streamprocessor/records-emitted "Records emitted")
  (metrics/create-gauge! :streamprocessor/total-streams "Total streams"
                         #(count (:streams @stream-state)))
  (metrics/create-gauge! :streamprocessor/total-processors "Total processors"
                         #(count (:processors @stream-state)))
  (log/info "Stream processor initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-stream-processor-status []
  {:enabled (flags/is-enabled? "stream-processor")
   :streams (count (:streams @stream-state))
   :processors (count (:processors @stream-state))
   :windows (count (:windows @stream-state))
   :stats (get-all-stats)
   :config (:config @stream-state)})
