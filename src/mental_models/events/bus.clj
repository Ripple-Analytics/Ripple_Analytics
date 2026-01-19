(ns mental-models.events.bus
  "Event Bus Module for Mental Models Pipeline
   
   Provides event-driven architecture with:
   - Publish/subscribe pattern
   - Event filtering
   - Async event handling
   - Event history
   - Dead letter queue"
  (:require
   [clojure.core.async :as async :refer [go go-loop chan <! >! >!! <!! close! mult tap untap pub sub]])
  (:import
   [java.util UUID]
   [java.time Instant]
   [java.util.concurrent ConcurrentHashMap]))

;; =============================================================================
;; EVENT DEFINITION
;; =============================================================================

(defrecord Event [id type topic payload timestamp source])

(defn create-event [type topic payload source]
  (->Event (str (UUID/randomUUID))
           type
           topic
           payload
           (Instant/now)
           source))

;; =============================================================================
;; EVENT BUS STATE
;; =============================================================================

(def ^:private main-channel (chan 1000))
(def ^:private publisher (pub main-channel :topic))
(def ^:private subscribers (ConcurrentHashMap.))
(def ^:private event-history (atom []))
(def ^:private dead-letter-queue (atom []))
(def ^:private max-history (atom 1000))

;; =============================================================================
;; PUBLISHING
;; =============================================================================

(defn publish!
  "Publish an event to the bus.
   Takes a topic keyword and a payload map."
  [topic payload & {:keys [source] :or {source "system"}}]
  (let [event (create-event topic topic payload source)]
    (swap! event-history conj event)
    (when (> (count @event-history) @max-history)
      (swap! event-history #(vec (drop 100 %))))
    (>!! main-channel event)
    event))

(defn publish-async!
  "Publish an event asynchronously.
   Takes a topic keyword and a payload map."
  [topic payload & {:keys [source] :or {source "system"}}]
  (go
    (let [event (create-event topic topic payload source)]
      (swap! event-history conj event)
      (>! main-channel event)
      event)))

;; =============================================================================
;; SUBSCRIBING
;; =============================================================================

(defn subscribe!
  "Subscribe to events on a topic."
  [topic handler & {:keys [id] :or {id (str (UUID/randomUUID))}}]
  (let [ch (chan 100)]
    (sub publisher topic ch)
    (.put subscribers id {:channel ch :topic topic :handler handler})
    (go-loop []
      (when-let [event (<! ch)]
        (try
          (handler event)
          (catch Exception e
            (swap! dead-letter-queue conj {:event event :error (str e) :timestamp (Instant/now)})))
        (recur)))
    id))

(defn unsubscribe!
  "Unsubscribe from events."
  [subscription-id]
  (when-let [{:keys [channel topic]} (.remove subscribers subscription-id)]
    (unsub publisher topic channel)
    (close! channel)
    true))

;; =============================================================================
;; EVENT TOPICS
;; =============================================================================

(def topics
  {:analysis-started "analysis.started"
   :analysis-completed "analysis.completed"
   :analysis-failed "analysis.failed"
   :model-detected "model.detected"
   :lollapalooza-detected "lollapalooza.detected"
   :document-ingested "document.ingested"
   :config-changed "config.changed"
   :health-check "health.check"
   :metrics-collected "metrics.collected"
   :error "error"})

;; =============================================================================
;; CONVENIENCE PUBLISHERS
;; =============================================================================

(defn emit-analysis-started [document-id]
  (publish! (:analysis-started topics)
            {:document-id document-id}))

(defn emit-analysis-completed [document-id results]
  (publish! (:analysis-completed topics)
            {:document-id document-id :results results}))

(defn emit-analysis-failed [document-id error]
  (publish! (:analysis-failed topics)
            {:document-id document-id :error error}))

(defn emit-model-detected [document-id model-id confidence]
  (publish! (:model-detected topics)
            {:document-id document-id :model-id model-id :confidence confidence}))

(defn emit-lollapalooza [document-id models avg-confidence]
  (publish! (:lollapalooza-detected topics)
            {:document-id document-id :models models :avg-confidence avg-confidence}))

(defn emit-document-ingested [document-id path]
  (publish! (:document-ingested topics)
            {:document-id document-id :path path}))

(defn emit-config-changed [key old-value new-value]
  (publish! (:config-changed topics)
            {:key key :old-value old-value :new-value new-value}))

(defn emit-error [source error]
  (publish! (:error topics)
            {:source source :error error}))

;; =============================================================================
;; QUERIES
;; =============================================================================

(defn get-event-history
  "Get event history, optionally filtered."
  ([] @event-history)
  ([n] (take-last n @event-history))
  ([topic n] (take-last n (filter #(= (:topic %) topic) @event-history))))

(defn get-dead-letters []
  @dead-letter-queue)

(defn clear-dead-letters []
  (reset! dead-letter-queue []))

(defn get-subscribers []
  (into {} subscribers))

(defn get-subscriber-count []
  (.size subscribers))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  {:total-events (count @event-history)
   :subscriber-count (.size subscribers)
   :dead-letters (count @dead-letter-queue)
   :events-by-topic (frequencies (map :topic @event-history))
   :events-by-type (frequencies (map :type @event-history))})

;; =============================================================================
;; LIFECYCLE
;; =============================================================================

(defn reset-bus! []
  (doseq [[id _] subscribers]
    (unsubscribe! id))
  (reset! event-history [])
  (reset! dead-letter-queue [])
  {:status :reset})

(defn set-max-history! [n]
  (reset! max-history n))

;; =============================================================================
;; EVENT FILTERING
;; =============================================================================

(defn filter-events [pred events]
  (filter pred events))

(defn events-in-range [start-time end-time]
  (filter #(and (.isAfter (:timestamp %) start-time)
                (.isBefore (:timestamp %) end-time))
          @event-history))

(defn events-by-source [source]
  (filter #(= (:source %) source) @event-history))

;; =============================================================================
;; REPLAY
;; =============================================================================

(defn replay-events
  "Replay events to a handler."
  [events handler]
  (doseq [event events]
    (handler event)))

(defn replay-topic
  "Replay all events for a topic to a handler."
  [topic handler]
  (replay-events (filter #(= (:topic %) topic) @event-history) handler))
