(ns mental-models.pipeline.integration.response-streamer
  "Response streamer for mental model analysis system.
   
   Features:
   - Chunked response streaming
   - Server-sent events (SSE)
   - Progress streaming
   - Backpressure handling
   - Stream buffering
   - Stream transformation
   - Stream metrics
   - Stream cancellation"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! put! take!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.io OutputStream PipedInputStream PipedOutputStream]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:streams {}          ;; stream-id -> stream
         :config {:default-buffer-size 1024
                  :default-chunk-size 8192
                  :heartbeat-interval-ms 30000
                  :max-streams 1000}
         :stats {:streams-created 0
                 :streams-completed 0
                 :streams-cancelled 0
                 :bytes-streamed 0
                 :chunks-sent 0}
         :initialized? false}))

;; ============================================================================
;; Stream Creation
;; ============================================================================

(defn create-stream
  "Create a new response stream."
  [& {:keys [buffer-size chunk-size] :or {buffer-size nil chunk-size nil}}]
  (let [stream-id (UUID/randomUUID)
        actual-buffer-size (or buffer-size (get-in @state [:config :default-buffer-size]))
        actual-chunk-size (or chunk-size (get-in @state [:config :default-chunk-size]))
        data-chan (chan actual-buffer-size)
        stream {:id stream-id
                :data-chan data-chan
                :buffer-size actual-buffer-size
                :chunk-size actual-chunk-size
                :status (atom :active)
                :bytes-sent (atom 0)
                :chunks-sent (atom 0)
                :created-at (System/currentTimeMillis)
                :last-activity (atom (System/currentTimeMillis))}]
    
    (swap! state assoc-in [:streams stream-id] stream)
    (swap! state update-in [:stats :streams-created] inc)
    
    (logging/log :debug "Created stream" {:stream-id stream-id})
    stream))

(defn get-stream
  "Get a stream by ID."
  [stream-id]
  (get-in @state [:streams stream-id]))

(defn list-streams
  "List all active streams."
  []
  (mapv (fn [[id s]]
          {:id id
           :status @(:status s)
           :bytes-sent @(:bytes-sent s)
           :chunks-sent @(:chunks-sent s)
           :created-at (:created-at s)})
        (:streams @state)))

;; ============================================================================
;; Stream Operations
;; ============================================================================

(defn write-to-stream!
  "Write data to a stream."
  [stream data]
  (when (and stream (= @(:status stream) :active))
    (let [data-bytes (if (string? data) (.getBytes data "UTF-8") data)
          size (count data-bytes)]
      (put! (:data-chan stream) data-bytes)
      (swap! (:bytes-sent stream) + size)
      (swap! (:chunks-sent stream) inc)
      (reset! (:last-activity stream) (System/currentTimeMillis))
      (swap! state update-in [:stats :bytes-streamed] + size)
      (swap! state update-in [:stats :chunks-sent] inc)
      true)))

(defn close-stream!
  "Close a stream."
  [stream]
  (when stream
    (reset! (:status stream) :closed)
    (close! (:data-chan stream))
    (swap! state update :streams dissoc (:id stream))
    (swap! state update-in [:stats :streams-completed] inc)
    (logging/log :debug "Closed stream" {:stream-id (:id stream)})
    true))

(defn cancel-stream!
  "Cancel a stream."
  [stream]
  (when stream
    (reset! (:status stream) :cancelled)
    (close! (:data-chan stream))
    (swap! state update :streams dissoc (:id stream))
    (swap! state update-in [:stats :streams-cancelled] inc)
    (logging/log :debug "Cancelled stream" {:stream-id (:id stream)})
    true))

;; ============================================================================
;; Chunked Streaming
;; ============================================================================

(defn stream-chunked-response
  "Create a chunked response stream."
  [producer-fn]
  (let [stream (create-stream)
        output-stream (PipedOutputStream.)
        input-stream (PipedInputStream. output-stream)]
    
    ;; Start producer in background
    (go
      (try
        (producer-fn (fn [data]
                       (when (= @(:status stream) :active)
                         (let [bytes (if (string? data) (.getBytes data "UTF-8") data)]
                           (.write output-stream bytes)
                           (.flush output-stream)
                           (swap! (:bytes-sent stream) + (count bytes))
                           (swap! (:chunks-sent stream) inc)))))
        (finally
          (.close output-stream)
          (close-stream! stream))))
    
    {:status 200
     :headers {"Transfer-Encoding" "chunked"
               "Content-Type" "application/octet-stream"}
     :body input-stream
     :stream stream}))

;; ============================================================================
;; Server-Sent Events (SSE)
;; ============================================================================

(defn- format-sse-event
  "Format data as an SSE event."
  [event-type data & {:keys [id retry]}]
  (let [lines (cond-> []
                id (conj (str "id: " id))
                event-type (conj (str "event: " event-type))
                retry (conj (str "retry: " retry))
                true (conj (str "data: " (if (string? data) data (pr-str data))))
                true (conj ""))]
    (str (str/join "\n" lines) "\n")))

(defn create-sse-stream
  "Create a Server-Sent Events stream."
  []
  (let [stream (create-stream)]
    {:stream stream
     :send-event (fn [event-type data & opts]
                   (let [event-str (apply format-sse-event event-type data opts)]
                     (write-to-stream! stream event-str)))
     :close (fn [] (close-stream! stream))}))

(defn stream-sse-response
  "Create an SSE response."
  [producer-fn]
  (let [sse (create-sse-stream)
        stream (:stream sse)
        output-stream (PipedOutputStream.)
        input-stream (PipedInputStream. output-stream)]
    
    ;; Start producer in background
    (go
      (try
        (producer-fn (:send-event sse))
        (finally
          (.close output-stream)
          ((:close sse)))))
    
    ;; Pipe stream data to output
    (go-loop []
      (when-let [data (<! (:data-chan stream))]
        (.write output-stream data)
        (.flush output-stream)
        (recur)))
    
    {:status 200
     :headers {"Content-Type" "text/event-stream"
               "Cache-Control" "no-cache"
               "Connection" "keep-alive"}
     :body input-stream
     :stream stream}))

;; ============================================================================
;; Progress Streaming
;; ============================================================================

(defn create-progress-stream
  "Create a progress streaming response."
  [total-items]
  (let [stream (create-stream)
        progress (atom {:completed 0 :total total-items :percentage 0})]
    {:stream stream
     :progress progress
     :update-progress (fn [completed & {:keys [message]}]
                        (let [pct (if (pos? total-items)
                                    (* 100 (/ completed total-items))
                                    0)]
                          (reset! progress {:completed completed
                                            :total total-items
                                            :percentage pct
                                            :message message})
                          (write-to-stream! stream
                                            (pr-str {:type :progress
                                                     :completed completed
                                                     :total total-items
                                                     :percentage pct
                                                     :message message}))))
     :complete (fn [result]
                 (write-to-stream! stream (pr-str {:type :complete :result result}))
                 (close-stream! stream))
     :error (fn [error]
              (write-to-stream! stream (pr-str {:type :error :error error}))
              (close-stream! stream))}))

;; ============================================================================
;; Stream Transformation
;; ============================================================================

(defn transform-stream
  "Transform a stream with a function."
  [stream transform-fn]
  (let [new-stream (create-stream)
        source-chan (:data-chan stream)
        dest-chan (:data-chan new-stream)]
    
    (go-loop []
      (if-let [data (<! source-chan)]
        (do
          (when-let [transformed (transform-fn data)]
            (>! dest-chan transformed))
          (recur))
        (close! dest-chan)))
    
    new-stream))

(defn filter-stream
  "Filter a stream with a predicate."
  [stream pred-fn]
  (transform-stream stream (fn [data] (when (pred-fn data) data))))

(defn map-stream
  "Map a function over a stream."
  [stream map-fn]
  (transform-stream stream map-fn))

;; ============================================================================
;; Backpressure Handling
;; ============================================================================

(defn create-backpressure-stream
  "Create a stream with backpressure handling."
  [& {:keys [high-watermark low-watermark on-pause on-resume]
      :or {high-watermark 100 low-watermark 50}}]
  (let [stream (create-stream :buffer-size high-watermark)
        paused? (atom false)]
    
    ;; Monitor buffer level
    (go-loop []
      (when (= @(:status stream) :active)
        (<! (timeout 100))
        (let [buffer-size (count (:data-chan stream))]
          (cond
            (and (not @paused?) (>= buffer-size high-watermark))
            (do
              (reset! paused? true)
              (when on-pause (on-pause)))
            
            (and @paused? (<= buffer-size low-watermark))
            (do
              (reset! paused? false)
              (when on-resume (on-resume)))))
        (recur)))
    
    (assoc stream :paused? paused?)))

;; ============================================================================
;; Heartbeat
;; ============================================================================

(defn start-heartbeat!
  "Start sending heartbeats to a stream."
  [stream & {:keys [interval-ms] :or {interval-ms nil}}]
  (let [actual-interval (or interval-ms (get-in @state [:config :heartbeat-interval-ms]))]
    (go-loop []
      (when (= @(:status stream) :active)
        (<! (timeout actual-interval))
        (write-to-stream! stream ": heartbeat\n\n")
        (recur)))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-streaming
  "Ring middleware to support streaming responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if-let [stream (:stream response)]
        (do
          (logging/log :debug "Streaming response" {:stream-id (:id stream)})
          response)
        response))))

;; ============================================================================
;; Utilities
;; ============================================================================

(defn stream-file
  "Stream a file in chunks."
  [file-path & {:keys [chunk-size] :or {chunk-size 8192}}]
  (stream-chunked-response
   (fn [write-fn]
     (with-open [input (clojure.java.io/input-stream file-path)]
       (let [buffer (byte-array chunk-size)]
         (loop []
           (let [read (.read input buffer)]
             (when (pos? read)
               (write-fn (java.util.Arrays/copyOf buffer read))
               (recur)))))))))

(defn stream-collection
  "Stream a collection item by item."
  [coll & {:keys [delay-ms] :or {delay-ms 0}}]
  (stream-chunked-response
   (fn [write-fn]
     (doseq [item coll]
       (write-fn (str (pr-str item) "\n"))
       (when (pos? delay-ms)
         (Thread/sleep delay-ms))))))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn cleanup-stale-streams!
  "Clean up stale streams."
  []
  (let [now (System/currentTimeMillis)
        timeout-ms 300000  ;; 5 minutes
        stale-streams (filter (fn [[_ s]]
                                (> (- now @(:last-activity s)) timeout-ms))
                              (:streams @state))]
    (doseq [[id _] stale-streams]
      (when-let [stream (get-stream id)]
        (cancel-stream! stream)))
    (count stale-streams)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-streamer-metrics
  "Get streamer metrics."
  []
  (let [stats (:stats @state)]
    {:streams-created (:streams-created stats)
     :streams-completed (:streams-completed stats)
     :streams-cancelled (:streams-cancelled stats)
     :active-streams (count (:streams @state))
     :bytes-streamed (:bytes-streamed stats)
     :chunks-sent (:chunks-sent stats)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-streamer-stats
  "Get streamer statistics."
  []
  (merge (get-streamer-metrics)
         {:max-streams (get-in @state [:config :max-streams])
          :default-buffer-size (get-in @state [:config :default-buffer-size])
          :default-chunk-size (get-in @state [:config :default-chunk-size])}))

(defn reset-stats!
  "Reset streamer statistics."
  []
  (swap! state assoc :stats {:streams-created 0
                             :streams-completed 0
                             :streams-cancelled 0
                             :bytes-streamed 0
                             :chunks-sent 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-streamer!
  "Initialize the response streamer."
  []
  (when-not (:initialized? @state)
    ;; Start cleanup task
    (go-loop []
      (when (:initialized? @state)
        (<! (timeout 60000))
        (cleanup-stale-streams!)
        (recur)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response streamer initialized")
    (events/emit! :response-streamer-initialized {})
    true))
