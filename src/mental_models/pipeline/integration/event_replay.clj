(ns mental-models.pipeline.integration.event-replay
  "Event Replay Module
   
   Event replay and time travel debugging:
   - Event store with persistence
   - Point-in-time replay
   - Selective replay by event type
   - State reconstruction
   - Replay scheduling"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; EVENT REPLAY STATE
;; =============================================================================

(defonce replay-state (atom {:event-store (ConcurrentHashMap.)
                             :snapshots (ConcurrentHashMap.)
                             :replay-sessions (ConcurrentHashMap.)
                             :handlers {}
                             :event-count (AtomicLong. 0)
                             :replay-count (AtomicLong. 0)
                             :config {:max-events 100000
                                      :snapshot-interval 1000
                                      :retention-days 30}}))

;; =============================================================================
;; EVENT STORAGE
;; =============================================================================

(defn store-event!
  "Store an event in the event store."
  [event-type payload & {:keys [aggregate-id correlation-id]}]
  (let [event-id (str (System/currentTimeMillis) "-" (.incrementAndGet ^AtomicLong (:event-count @replay-state)))
        event {:id event-id
               :type event-type
               :payload payload
               :aggregate-id aggregate-id
               :correlation-id correlation-id
               :timestamp (System/currentTimeMillis)
               :sequence (.get ^AtomicLong (:event-count @replay-state))}]
    ;; Check max events
    (let [store ^ConcurrentHashMap (:event-store @replay-state)
          max-events (get-in @replay-state [:config :max-events])]
      (when (>= (.size store) max-events)
        ;; Remove oldest events
        (let [oldest (take (/ max-events 10)
                           (sort-by :sequence (vals store)))]
          (doseq [e oldest]
            (.remove store (:id e))))))
    (.put ^ConcurrentHashMap (:event-store @replay-state) event-id event)
    (metrics/inc-counter! :eventreplay/events-stored)
    (log/debug "Event stored" {:id event-id :type event-type})
    event))

(defn get-event
  "Get an event by ID."
  [event-id]
  (.get ^ConcurrentHashMap (:event-store @replay-state) event-id))

(defn get-events
  "Get events with optional filtering."
  [& {:keys [type aggregate-id since until limit]}]
  (let [events (vals (:event-store @replay-state))]
    (cond->> events
      type (filter #(= (:type %) type))
      aggregate-id (filter #(= (:aggregate-id %) aggregate-id))
      since (filter #(>= (:timestamp %) since))
      until (filter #(<= (:timestamp %) until))
      true (sort-by :sequence)
      limit (take limit))))

(defn delete-event!
  "Delete an event."
  [event-id]
  (.remove ^ConcurrentHashMap (:event-store @replay-state) event-id))

(defn clear-events!
  "Clear all events."
  [& {:keys [before type]}]
  (if (or before type)
    (doseq [[k v] (:event-store @replay-state)]
      (when (and (or (nil? before) (< (:timestamp v) before))
                 (or (nil? type) (= (:type v) type)))
        (.remove ^ConcurrentHashMap (:event-store @replay-state) k)))
    (.clear ^ConcurrentHashMap (:event-store @replay-state))))

;; =============================================================================
;; SNAPSHOTS
;; =============================================================================

(defn create-snapshot!
  "Create a state snapshot."
  [snapshot-id state & {:keys [description]}]
  (log/info "Creating snapshot" {:id snapshot-id})
  (let [snapshot {:id snapshot-id
                  :state state
                  :description description
                  :event-sequence (.get ^AtomicLong (:event-count @replay-state))
                  :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:snapshots @replay-state) snapshot-id snapshot)
    snapshot))

(defn get-snapshot
  "Get a snapshot by ID."
  [snapshot-id]
  (.get ^ConcurrentHashMap (:snapshots @replay-state) snapshot-id))

(defn list-snapshots
  "List all snapshots."
  []
  (sort-by :created-at > (vals (:snapshots @replay-state))))

(defn delete-snapshot!
  "Delete a snapshot."
  [snapshot-id]
  (.remove ^ConcurrentHashMap (:snapshots @replay-state) snapshot-id))

(defn get-nearest-snapshot
  "Get the nearest snapshot before a given timestamp."
  [timestamp]
  (let [snapshots (filter #(<= (:created-at %) timestamp)
                          (vals (:snapshots @replay-state)))]
    (last (sort-by :created-at snapshots))))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn register-handler!
  "Register an event handler for replay."
  [event-type handler-fn]
  (swap! replay-state assoc-in [:handlers event-type] handler-fn))

(defn unregister-handler!
  "Unregister an event handler."
  [event-type]
  (swap! replay-state update :handlers dissoc event-type))

(defn get-handler
  "Get a handler for an event type."
  [event-type]
  (get-in @replay-state [:handlers event-type]))

;; =============================================================================
;; REPLAY SESSIONS
;; =============================================================================

(defn create-replay-session!
  "Create a replay session."
  [session-id {:keys [from-timestamp to-timestamp event-types aggregate-id speed]}]
  (log/info "Creating replay session" {:id session-id})
  (let [session {:id session-id
                 :from-timestamp from-timestamp
                 :to-timestamp to-timestamp
                 :event-types (when event-types (set event-types))
                 :aggregate-id aggregate-id
                 :speed (or speed 1.0)
                 :status :created
                 :current-position 0
                 :events-replayed 0
                 :created-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:replay-sessions @replay-state) session-id session)
    session))

(defn get-replay-session
  "Get a replay session."
  [session-id]
  (.get ^ConcurrentHashMap (:replay-sessions @replay-state) session-id))

(defn update-replay-session!
  "Update a replay session."
  [session-id updates]
  (when-let [session (get-replay-session session-id)]
    (.put ^ConcurrentHashMap (:replay-sessions @replay-state) session-id
          (merge session updates))))

(defn delete-replay-session!
  "Delete a replay session."
  [session-id]
  (.remove ^ConcurrentHashMap (:replay-sessions @replay-state) session-id))

;; =============================================================================
;; REPLAY EXECUTION
;; =============================================================================

(defn apply-event
  "Apply an event to state using registered handler."
  [state event]
  (if-let [handler (get-handler (:type event))]
    (try
      (handler state event)
      (catch Exception e
        (log/error "Event handler failed" {:event-id (:id event) :error (.getMessage e)})
        state))
    state))

(defn replay-events
  "Replay events and return final state."
  [events & {:keys [initial-state]}]
  (.incrementAndGet ^AtomicLong (:replay-count @replay-state))
  (metrics/inc-counter! :eventreplay/replays)
  (let [start-time (System/currentTimeMillis)
        sorted-events (sort-by :sequence events)
        final-state (reduce apply-event (or initial-state {}) sorted-events)
        duration-ms (- (System/currentTimeMillis) start-time)]
    (log/info "Events replayed" {:count (count events) :duration-ms duration-ms})
    {:state final-state
     :events-count (count events)
     :duration-ms duration-ms}))

(defn replay-to-timestamp
  "Replay events up to a specific timestamp."
  [timestamp & {:keys [initial-state event-types aggregate-id]}]
  (let [;; Find nearest snapshot
        snapshot (get-nearest-snapshot timestamp)
        start-state (or (:state snapshot) initial-state {})
        start-sequence (or (:event-sequence snapshot) 0)
        ;; Get events after snapshot
        events (get-events :since (or (:created-at snapshot) 0)
                           :until timestamp
                           :type (first event-types)
                           :aggregate-id aggregate-id)]
    (replay-events (filter #(> (:sequence %) start-sequence) events)
                   :initial-state start-state)))

(defn replay-session!
  "Execute a replay session."
  [session-id & {:keys [initial-state callback]}]
  (let [session (get-replay-session session-id)]
    (when session
      (update-replay-session! session-id {:status :running})
      (try
        (let [events (get-events :since (:from-timestamp session)
                                 :until (:to-timestamp session)
                                 :aggregate-id (:aggregate-id session))
              filtered-events (if (:event-types session)
                                (filter #(contains? (:event-types session) (:type %)) events)
                                events)
              result (replay-events filtered-events :initial-state initial-state)]
          (update-replay-session! session-id {:status :completed
                                              :events-replayed (count filtered-events)
                                              :completed-at (System/currentTimeMillis)})
          (when callback
            (callback result))
          result)
        (catch Exception e
          (update-replay-session! session-id {:status :failed
                                              :error (.getMessage e)})
          (throw e))))))

;; =============================================================================
;; TIME TRAVEL
;; =============================================================================

(defn get-state-at
  "Get the state at a specific point in time."
  [timestamp & {:keys [aggregate-id]}]
  (:state (replay-to-timestamp timestamp :aggregate-id aggregate-id)))

(defn get-state-changes
  "Get state changes between two timestamps."
  [from-timestamp to-timestamp & {:keys [aggregate-id]}]
  (let [events (get-events :since from-timestamp
                           :until to-timestamp
                           :aggregate-id aggregate-id)]
    (map (fn [event]
           {:event event
            :state-before (get-state-at (dec (:timestamp event)) :aggregate-id aggregate-id)
            :state-after (get-state-at (:timestamp event) :aggregate-id aggregate-id)})
         events)))

;; =============================================================================
;; EVENT SOURCING HELPERS
;; =============================================================================

(defn rebuild-aggregate
  "Rebuild an aggregate from its events."
  [aggregate-id & {:keys [initial-state]}]
  (let [events (get-events :aggregate-id aggregate-id)]
    (replay-events events :initial-state initial-state)))

(defn get-aggregate-history
  "Get the history of an aggregate."
  [aggregate-id]
  (let [events (get-events :aggregate-id aggregate-id)]
    (reductions (fn [state event]
                  {:event event
                   :state (apply-event (:state state) event)})
                {:state {} :event nil}
                events)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-replay-stats
  "Get event replay statistics."
  []
  {:events (.size ^ConcurrentHashMap (:event-store @replay-state))
   :snapshots (.size ^ConcurrentHashMap (:snapshots @replay-state))
   :sessions (.size ^ConcurrentHashMap (:replay-sessions @replay-state))
   :handlers (count (:handlers @replay-state))
   :event-count (.get ^AtomicLong (:event-count @replay-state))
   :replay-count (.get ^AtomicLong (:replay-count @replay-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-event-replay!
  "Initialize event replay."
  []
  (log/info "Initializing event replay")
  ;; Register feature flag
  (flags/register-flag! "event-replay" "Enable event replay" true)
  ;; Create metrics
  (metrics/create-counter! :eventreplay/events-stored "Events stored")
  (metrics/create-counter! :eventreplay/replays "Replays executed")
  (metrics/create-gauge! :eventreplay/total-events "Total events"
                         #(.size ^ConcurrentHashMap (:event-store @replay-state)))
  (log/info "Event replay initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-event-replay-status []
  {:enabled (flags/is-enabled? "event-replay")
   :events (.size ^ConcurrentHashMap (:event-store @replay-state))
   :snapshots (.size ^ConcurrentHashMap (:snapshots @replay-state))
   :sessions (.size ^ConcurrentHashMap (:replay-sessions @replay-state))
   :handlers (count (:handlers @replay-state))
   :stats (get-replay-stats)
   :config (:config @replay-state)})
