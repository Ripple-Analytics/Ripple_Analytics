(ns mental-models.pipeline.integration.streaming-analysis
  "Streaming Analysis Module
   
   Real-time streaming analysis with token-by-token processing:
   - Streaming LM Studio integration
   - Progressive model detection
   - Real-time confidence updates
   - Early Lollapalooza detection"
  (:require
   [mental-models.lm-studio.client :as lm-client]
   [mental-models.pipeline.integration.model-interaction :as interaction]
   [mental-models.pipeline.integration.websocket-server :as ws]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.core.async :as async :refer [go go-loop chan <! >! close! timeout]]))

;; =============================================================================
;; STREAMING STATE
;; =============================================================================

(defonce streaming-state (atom {:active-streams {}
                                :total-streams 0}))

;; =============================================================================
;; STREAM STRUCTURE
;; =============================================================================

(defn create-stream [stream-id text]
  {:id stream-id
   :text text
   :status :pending
   :tokens []
   :partial-response ""
   :detections []
   :confidence-history []
   :started-at nil
   :completed-at nil
   :lollapalooza-detected false})

;; =============================================================================
;; STREAM MANAGEMENT
;; =============================================================================

(defn register-stream! [stream]
  (swap! streaming-state assoc-in [:active-streams (:id stream)] stream)
  (swap! streaming-state update :total-streams inc)
  (metrics/inc-counter! :streaming/streams-created)
  (log/info "Stream registered" {:stream-id (:id stream)}))

(defn update-stream! [stream-id updates]
  (swap! streaming-state update-in [:active-streams stream-id] merge updates))

(defn get-stream [stream-id]
  (get-in @streaming-state [:active-streams stream-id]))

(defn remove-stream! [stream-id]
  (swap! streaming-state update :active-streams dissoc stream-id))

;; =============================================================================
;; TOKEN PROCESSING
;; =============================================================================

(defn process-token!
  "Process a single token from the stream."
  [stream-id token]
  (let [stream (get-stream stream-id)
        new-partial (str (:partial-response stream) token)]
    (update-stream! stream-id {:tokens (conj (:tokens stream) token)
                               :partial-response new-partial})
    ;; Publish token event
    (events/publish! :streaming/token-received {:stream-id stream-id :token token})
    ;; Broadcast to WebSocket clients
    (ws/broadcast! :analysis-progress {:stream-id stream-id
                                       :token token
                                       :token-count (inc (count (:tokens stream)))})))

(defn extract-partial-detections
  "Extract model detections from partial response."
  [partial-response]
  ;; Parse partial JSON response for early detection
  (try
    (when (> (count partial-response) 50)
      (let [model-patterns [["social-proof" #"(?i)social\s*proof|conformity|herd"]
                            ["authority" #"(?i)authority|expert|credential"]
                            ["scarcity" #"(?i)scarc|limited|rare|exclusive"]
                            ["reciprocity" #"(?i)reciproc|give.*take|favor"]
                            ["commitment" #"(?i)commit|consisten|sunk\s*cost"]
                            ["liking" #"(?i)lik|similar|familiar|attractive"]
                            ["anchoring" #"(?i)anchor|reference\s*point|first\s*impression"]
                            ["availability" #"(?i)availab|recent|vivid|memorable"]
                            ["confirmation" #"(?i)confirm|bias|belief"]
                            ["overconfidence" #"(?i)overconfiden|hubris|certain"]]]
        (for [[model-id pattern] model-patterns
              :when (re-find pattern partial-response)]
          {:model-id (keyword model-id)
           :model-name (clojure.string/replace model-id "-" " ")
           :confidence 0.5
           :partial true})))
    (catch Exception _ nil)))

;; =============================================================================
;; PROGRESSIVE DETECTION
;; =============================================================================

(defn update-detections!
  "Update detections based on partial response."
  [stream-id]
  (let [stream (get-stream stream-id)
        partial-detections (extract-partial-detections (:partial-response stream))]
    (when (seq partial-detections)
      (update-stream! stream-id {:detections partial-detections})
      ;; Check for early Lollapalooza
      (when (>= (count partial-detections) 3)
        (let [lollapalooza (interaction/detect-lollapalooza-pattern partial-detections)]
          (when lollapalooza
            (update-stream! stream-id {:lollapalooza-detected true})
            (events/publish! :streaming/early-lollapalooza {:stream-id stream-id
                                                            :detections partial-detections})
            (ws/broadcast! :lollapalooza-alert {:stream-id stream-id
                                                :early-detection true
                                                :models (map :model-name partial-detections)})))))))

;; =============================================================================
;; STREAMING ANALYSIS
;; =============================================================================

(defn analyze-streaming!
  "Perform streaming analysis with real-time updates."
  [text & {:keys [on-token on-detection on-complete]}]
  (when (flags/is-enabled? "streaming-analysis")
    (let [stream-id (str (java.util.UUID/randomUUID))
          stream (create-stream stream-id text)
          output-chan (chan 100)]
      (register-stream! stream)
      (update-stream! stream-id {:status :running :started-at (System/currentTimeMillis)})
      (events/publish! :streaming/started {:stream-id stream-id})
      ;; Start streaming from LM Studio
      (go
        (try
          (let [response-chan (lm-client/analyze-streaming text)]
            (loop []
              (when-let [token (<! response-chan)]
                (process-token! stream-id token)
                (when on-token (on-token token))
                ;; Update detections every 10 tokens
                (when (zero? (mod (count (:tokens (get-stream stream-id))) 10))
                  (update-detections! stream-id)
                  (when on-detection
                    (on-detection (:detections (get-stream stream-id)))))
                (recur)))
            ;; Stream complete
            (let [final-stream (get-stream stream-id)]
              (update-stream! stream-id {:status :completed
                                         :completed-at (System/currentTimeMillis)})
              ;; Final detection update
              (update-detections! stream-id)
              ;; Publish completion
              (events/publish! :streaming/completed {:stream-id stream-id
                                                     :detections (:detections final-stream)
                                                     :token-count (count (:tokens final-stream))})
              (ws/broadcast! :analysis-complete {:stream-id stream-id
                                                 :detections (:detections final-stream)})
              ;; Metrics
              (metrics/inc-counter! :streaming/streams-completed)
              (metrics/observe-histogram! :streaming/stream-duration
                                          (- (:completed-at final-stream) (:started-at final-stream)))
              ;; Callback
              (when on-complete (on-complete final-stream))
              ;; Cleanup
              (remove-stream! stream-id)))
          (catch Exception e
            (log/error "Streaming analysis failed" {:stream-id stream-id :error (.getMessage e)})
            (update-stream! stream-id {:status :failed})
            (metrics/inc-counter! :streaming/streams-failed))))
      stream-id)))

;; =============================================================================
;; BATCH STREAMING
;; =============================================================================

(defn analyze-batch-streaming!
  "Analyze multiple texts with streaming, processing in parallel."
  [texts & {:keys [concurrency] :or {concurrency 4}}]
  (let [results-chan (chan (count texts))
        semaphore (java.util.concurrent.Semaphore. concurrency)]
    (doseq [text texts]
      (go
        (.acquire semaphore)
        (try
          (let [stream-id (analyze-streaming! text
                                              :on-complete #(>! results-chan %))]
            (<! (timeout 60000))) ; Max 60s per analysis
          (finally
            (.release semaphore)))))
    results-chan))

;; =============================================================================
;; STREAM MONITORING
;; =============================================================================

(defn get-active-streams []
  (vals (:active-streams @streaming-state)))

(defn get-stream-progress [stream-id]
  (when-let [stream (get-stream stream-id)]
    {:stream-id stream-id
     :status (:status stream)
     :token-count (count (:tokens stream))
     :detection-count (count (:detections stream))
     :lollapalooza-detected (:lollapalooza-detected stream)
     :elapsed-ms (when (:started-at stream)
                   (- (or (:completed-at stream) (System/currentTimeMillis))
                      (:started-at stream)))}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-streaming-analysis!
  "Initialize streaming analysis module."
  []
  (log/info "Initializing streaming analysis")
  ;; Register feature flag
  (flags/register-flag! "streaming-analysis" "Enable streaming analysis" true)
  ;; Create metrics
  (metrics/create-counter! :streaming/streams-created "Streams created")
  (metrics/create-counter! :streaming/streams-completed "Streams completed")
  (metrics/create-counter! :streaming/streams-failed "Streams failed")
  (metrics/create-histogram! :streaming/stream-duration "Stream duration" [1000 5000 10000 30000 60000])
  (log/info "Streaming analysis initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-streaming-status []
  {:enabled (flags/is-enabled? "streaming-analysis")
   :active-streams (count (get-active-streams))
   :total-streams (:total-streams @streaming-state)})
