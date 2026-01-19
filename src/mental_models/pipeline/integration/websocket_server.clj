(ns mental-models.pipeline.integration.websocket-server
  "WebSocket Server for Real-Time Streaming
   
   Provides WebSocket server for streaming analysis results to web clients:
   - Real-time analysis progress updates
   - Lollapalooza alert broadcasting
   - Model detection streaming
   - Client connection management"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.core.async :as async :refer [go go-loop chan <! >! close! mult tap untap]]
   [clojure.data.json :as json])
  (:import
   [java.net InetSocketAddress]
   [org.java_websocket.server WebSocketServer]
   [org.java_websocket.handshake ClientHandshake]
   [org.java_websocket WebSocket]))

;; =============================================================================
;; SERVER STATE
;; =============================================================================

(defonce server-state (atom {:server nil
                             :clients #{}
                             :broadcast-channel nil
                             :broadcast-mult nil
                             :running false}))

;; =============================================================================
;; MESSAGE TYPES
;; =============================================================================

(def message-types
  {:analysis-started "ANALYSIS_STARTED"
   :analysis-progress "ANALYSIS_PROGRESS"
   :analysis-complete "ANALYSIS_COMPLETE"
   :model-detected "MODEL_DETECTED"
   :lollapalooza-alert "LOLLAPALOOZA_ALERT"
   :batch-started "BATCH_STARTED"
   :batch-progress "BATCH_PROGRESS"
   :batch-complete "BATCH_COMPLETE"
   :file-processed "FILE_PROCESSED"
   :error "ERROR"
   :heartbeat "HEARTBEAT"})

;; =============================================================================
;; MESSAGE FORMATTING
;; =============================================================================

(defn format-message [type data]
  (json/write-str {:type (get message-types type type)
                   :timestamp (System/currentTimeMillis)
                   :data data}))

(defn parse-message [msg]
  (try
    (json/read-str msg :key-fn keyword)
    (catch Exception e
      (log/warn "Failed to parse WebSocket message" {:error (.getMessage e)})
      nil)))

;; =============================================================================
;; CLIENT MANAGEMENT
;; =============================================================================

(defn add-client! [conn]
  (swap! server-state update :clients conj conn)
  (metrics/inc-counter! :websocket/connections)
  (log/info "WebSocket client connected" {:remote-address (str (.getRemoteSocketAddress conn))
                                          :total-clients (count (:clients @server-state))}))

(defn remove-client! [conn]
  (swap! server-state update :clients disj conn)
  (metrics/inc-counter! :websocket/disconnections)
  (log/info "WebSocket client disconnected" {:total-clients (count (:clients @server-state))}))

(defn get-client-count []
  (count (:clients @server-state)))

;; =============================================================================
;; BROADCASTING
;; =============================================================================

(defn broadcast!
  "Broadcast a message to all connected clients."
  [type data]
  (when (flags/is-enabled? "websocket-streaming")
    (let [msg (format-message type data)
          clients (:clients @server-state)]
      (doseq [client clients]
        (try
          (when (.isOpen client)
            (.send client msg))
          (catch Exception e
            (log/warn "Failed to send to client" {:error (.getMessage e)}))))
      (metrics/inc-counter! :websocket/messages-sent)
      (log/debug "Broadcast message" {:type type :client-count (count clients)}))))

(defn send-to-client!
  "Send a message to a specific client."
  [conn type data]
  (when (.isOpen conn)
    (try
      (.send conn (format-message type data))
      (catch Exception e
        (log/warn "Failed to send to client" {:error (.getMessage e)})))))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-event-handlers! []
  (log/info "Setting up WebSocket event handlers")
  ;; Analysis events
  (events/subscribe! :analysis/started
                     (fn [data] (broadcast! :analysis-started data)))
  (events/subscribe! :analysis/progress
                     (fn [data] (broadcast! :analysis-progress data)))
  (events/subscribe! :analysis/complete
                     (fn [data] (broadcast! :analysis-complete data)))
  ;; Model detection events
  (events/subscribe! :model/detected
                     (fn [data] (broadcast! :model-detected data)))
  ;; Lollapalooza events
  (events/subscribe! :lollapalooza/detected
                     (fn [data] (broadcast! :lollapalooza-alert data)))
  ;; Batch events
  (events/subscribe! :batch/started
                     (fn [data] (broadcast! :batch-started data)))
  (events/subscribe! :batch/progress
                     (fn [data] (broadcast! :batch-progress data)))
  (events/subscribe! :batch/complete
                     (fn [data] (broadcast! :batch-complete data)))
  ;; File watcher events
  (events/subscribe! :file-watcher/file-processed
                     (fn [data] (broadcast! :file-processed data))))

;; =============================================================================
;; HEARTBEAT
;; =============================================================================

(defn start-heartbeat! []
  (go-loop []
    (when (:running @server-state)
      (<! (async/timeout 30000))
      (broadcast! :heartbeat {:server-time (System/currentTimeMillis)
                              :client-count (get-client-count)})
      (recur))))

;; =============================================================================
;; WEBSOCKET SERVER
;; =============================================================================

(defn create-websocket-server [port]
  (proxy [WebSocketServer] [(InetSocketAddress. port)]
    (onOpen [conn handshake]
      (add-client! conn)
      (send-to-client! conn :analysis-started {:message "Connected to Mental Models Pipeline"}))
    (onClose [conn code reason remote]
      (remove-client! conn))
    (onMessage [conn message]
      (log/debug "Received WebSocket message" {:message message})
      (when-let [parsed (parse-message message)]
        (case (:type parsed)
          "PING" (send-to-client! conn :heartbeat {:pong true})
          "SUBSCRIBE" (log/info "Client subscribed" {:topics (:topics parsed)})
          (log/debug "Unknown message type" {:type (:type parsed)}))))
    (onError [conn ex]
      (log/error "WebSocket error" {:error (.getMessage ex)}))
    (onStart []
      (log/info "WebSocket server started" {:port port}))))

;; =============================================================================
;; SERVER LIFECYCLE
;; =============================================================================

(defn start-server!
  "Start the WebSocket server."
  [& {:keys [port] :or {port 8765}}]
  (when-not (:running @server-state)
    (log/info "Starting WebSocket server" {:port port})
    (let [server (create-websocket-server port)]
      (.start server)
      (swap! server-state assoc
             :server server
             :running true)
      ;; Setup event handlers
      (setup-event-handlers!)
      ;; Start heartbeat
      (start-heartbeat!)
      ;; Register feature flag
      (flags/register-flag! "websocket-streaming" "Enable WebSocket streaming" true)
      ;; Create metrics
      (metrics/create-counter! :websocket/connections "WebSocket connections")
      (metrics/create-counter! :websocket/disconnections "WebSocket disconnections")
      (metrics/create-counter! :websocket/messages-sent "Messages sent")
      (metrics/create-gauge! :websocket/active-clients "Active clients" get-client-count)
      (log/info "WebSocket server started successfully"))))

(defn stop-server!
  "Stop the WebSocket server."
  []
  (when (:running @server-state)
    (log/info "Stopping WebSocket server")
    (when-let [server (:server @server-state)]
      (.stop server 1000))
    (swap! server-state assoc
           :server nil
           :clients #{}
           :running false)
    (log/info "WebSocket server stopped")))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-server-status []
  {:running (:running @server-state)
   :client-count (get-client-count)
   :enabled (flags/is-enabled? "websocket-streaming")})
