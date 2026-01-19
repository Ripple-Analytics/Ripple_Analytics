(ns mental-models.pipeline.integration.event-sourcing
  "Event Sourcing Module
   
   Event-driven state management:
   - Event store
   - Event replay
   - Projections
   - Snapshots
   - Event versioning"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; EVENT STORE STATE
;; =============================================================================

(defonce event-store (atom {:events []
                            :streams {}
                            :projections {}
                            :snapshots {}
                            :handlers {}
                            :config {:snapshot-interval 100
                                     :max-events-per-stream 10000}}))

;; =============================================================================
;; EVENT CREATION
;; =============================================================================

(defn generate-event-id
  "Generate a unique event ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-event
  "Create an event."
  [event-type stream-id data & {:keys [metadata]}]
  {:id (generate-event-id)
   :type event-type
   :stream-id stream-id
   :data data
   :metadata (merge {:timestamp (System/currentTimeMillis)
                     :version 1}
                    metadata)
   :sequence-number nil})

;; =============================================================================
;; EVENT STORAGE
;; =============================================================================

(defn append-event!
  "Append an event to the store."
  [event]
  (when (flags/is-enabled? "event-sourcing")
    (let [stream-id (:stream-id event)
          stream-events (get-in @event-store [:streams stream-id] [])
          seq-num (inc (count stream-events))
          event-with-seq (assoc event :sequence-number seq-num)]
      (log/debug "Appending event" {:type (:type event) :stream stream-id :seq seq-num})
      ;; Add to global events
      (swap! event-store update :events conj event-with-seq)
      ;; Add to stream
      (swap! event-store update-in [:streams stream-id] (fnil conj []) event-with-seq)
      ;; Update projections
      (doseq [[proj-id projection] (:projections @event-store)]
        (when ((:filter projection) event-with-seq)
          (swap! event-store update-in [:projections proj-id :state]
                 (:reducer projection) event-with-seq)))
      ;; Check for snapshot
      (when (zero? (mod seq-num (get-in @event-store [:config :snapshot-interval])))
        (create-snapshot! stream-id))
      (metrics/inc-counter! :eventsourcing/events-appended)
      (events/publish! :eventsourcing/event-appended {:event-id (:id event-with-seq)})
      event-with-seq)))

(defn append-events!
  "Append multiple events atomically."
  [events]
  (doseq [event events]
    (append-event! event)))

;; =============================================================================
;; EVENT RETRIEVAL
;; =============================================================================

(defn get-events
  "Get all events."
  [& {:keys [from-seq to-seq event-type stream-id]}]
  (let [events (:events @event-store)]
    (cond->> events
      from-seq (filter #(>= (:sequence-number %) from-seq))
      to-seq (filter #(<= (:sequence-number %) to-seq))
      event-type (filter #(= (:type %) event-type))
      stream-id (filter #(= (:stream-id %) stream-id)))))

(defn get-stream-events
  "Get events for a specific stream."
  [stream-id & {:keys [from-seq]}]
  (let [events (get-in @event-store [:streams stream-id] [])]
    (if from-seq
      (filter #(>= (:sequence-number %) from-seq) events)
      events)))

(defn get-event
  "Get a specific event by ID."
  [event-id]
  (first (filter #(= (:id %) event-id) (:events @event-store))))

(defn get-latest-event
  "Get the latest event for a stream."
  [stream-id]
  (last (get-in @event-store [:streams stream-id])))

;; =============================================================================
;; PROJECTIONS
;; =============================================================================

(defn register-projection!
  "Register a projection."
  [projection-id {:keys [initial-state reducer filter]}]
  (log/info "Registering projection" {:id projection-id})
  (swap! event-store assoc-in [:projections projection-id]
         {:id projection-id
          :initial-state initial-state
          :state initial-state
          :reducer reducer
          :filter (or filter (constantly true))
          :created-at (System/currentTimeMillis)}))

(defn get-projection-state
  "Get the current state of a projection."
  [projection-id]
  (get-in @event-store [:projections projection-id :state]))

(defn rebuild-projection!
  "Rebuild a projection from events."
  [projection-id]
  (log/info "Rebuilding projection" {:id projection-id})
  (when-let [projection (get-in @event-store [:projections projection-id])]
    (let [events (:events @event-store)
          filtered-events (filter (:filter projection) events)
          new-state (reduce (:reducer projection) (:initial-state projection) filtered-events)]
      (swap! event-store assoc-in [:projections projection-id :state] new-state)
      new-state)))

(defn list-projections
  "List all projections."
  []
  (keys (:projections @event-store)))

;; =============================================================================
;; SNAPSHOTS
;; =============================================================================

(defn create-snapshot!
  "Create a snapshot for a stream."
  [stream-id]
  (log/info "Creating snapshot" {:stream stream-id})
  (let [events (get-stream-events stream-id)
        latest-seq (or (:sequence-number (last events)) 0)]
    (swap! event-store assoc-in [:snapshots stream-id]
           {:stream-id stream-id
            :sequence-number latest-seq
            :state (reduce (fn [state event]
                             (merge state (:data event)))
                           {}
                           events)
            :created-at (System/currentTimeMillis)})
    (metrics/inc-counter! :eventsourcing/snapshots-created)))

(defn get-snapshot
  "Get the latest snapshot for a stream."
  [stream-id]
  (get-in @event-store [:snapshots stream-id]))

(defn restore-from-snapshot
  "Restore state from snapshot and replay events."
  [stream-id]
  (if-let [snapshot (get-snapshot stream-id)]
    (let [events-after (get-stream-events stream-id :from-seq (inc (:sequence-number snapshot)))]
      (reduce (fn [state event]
                (merge state (:data event)))
              (:state snapshot)
              events-after))
    ;; No snapshot, replay all events
    (reduce (fn [state event]
              (merge state (:data event)))
            {}
            (get-stream-events stream-id))))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn register-handler!
  "Register an event handler."
  [handler-id event-type handler-fn]
  (log/info "Registering event handler" {:id handler-id :type event-type})
  (swap! event-store update-in [:handlers event-type] (fnil conj [])
         {:id handler-id :fn handler-fn}))

(defn unregister-handler!
  "Unregister an event handler."
  [handler-id event-type]
  (swap! event-store update-in [:handlers event-type]
         (fn [handlers]
           (vec (remove #(= (:id %) handler-id) handlers)))))

(defn dispatch-event!
  "Dispatch an event to handlers."
  [event]
  (let [handlers (get-in @event-store [:handlers (:type event)] [])]
    (doseq [{:keys [fn]} handlers]
      (try
        (fn event)
        (catch Exception e
          (log/error "Event handler error" {:event-type (:type event) :error (.getMessage e)}))))))

;; =============================================================================
;; AGGREGATE
;; =============================================================================

(defn create-aggregate
  "Create an aggregate from events."
  [stream-id apply-fn initial-state]
  (let [events (get-stream-events stream-id)]
    (reduce apply-fn initial-state events)))

(defn aggregate-command!
  "Execute a command on an aggregate."
  [stream-id command apply-fn validate-fn initial-state]
  (let [current-state (create-aggregate stream-id apply-fn initial-state)]
    (when (validate-fn current-state command)
      (let [events ((:execute command) current-state)]
        (append-events! (map #(create-event (:type %) stream-id (:data %)) events))
        events))))

;; =============================================================================
;; EVENT REPLAY
;; =============================================================================

(defn replay-events!
  "Replay events to rebuild state."
  [stream-id & {:keys [to-seq]}]
  (log/info "Replaying events" {:stream stream-id :to-seq to-seq})
  (let [events (if to-seq
                 (filter #(<= (:sequence-number %) to-seq) (get-stream-events stream-id))
                 (get-stream-events stream-id))]
    (doseq [event events]
      (dispatch-event! event))
    (count events)))

(defn replay-all!
  "Replay all events."
  []
  (log/info "Replaying all events")
  (doseq [event (:events @event-store)]
    (dispatch-event! event))
  (count (:events @event-store)))

;; =============================================================================
;; STREAM MANAGEMENT
;; =============================================================================

(defn list-streams
  "List all streams."
  []
  (keys (:streams @event-store)))

(defn get-stream-info
  "Get information about a stream."
  [stream-id]
  (let [events (get-stream-events stream-id)
        snapshot (get-snapshot stream-id)]
    {:stream-id stream-id
     :event-count (count events)
     :latest-sequence (or (:sequence-number (last events)) 0)
     :has-snapshot (some? snapshot)
     :snapshot-sequence (:sequence-number snapshot)}))

(defn delete-stream!
  "Delete a stream and its events."
  [stream-id]
  (log/info "Deleting stream" {:stream stream-id})
  (swap! event-store update :events
         (fn [events] (vec (remove #(= (:stream-id %) stream-id) events))))
  (swap! event-store update :streams dissoc stream-id)
  (swap! event-store update :snapshots dissoc stream-id))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-event-sourcing!
  "Initialize event sourcing."
  []
  (log/info "Initializing event sourcing")
  ;; Register feature flag
  (flags/register-flag! "event-sourcing" "Enable event sourcing" true)
  ;; Create metrics
  (metrics/create-counter! :eventsourcing/events-appended "Events appended")
  (metrics/create-counter! :eventsourcing/snapshots-created "Snapshots created")
  (metrics/create-gauge! :eventsourcing/total-events "Total events"
                         #(count (:events @event-store)))
  (metrics/create-gauge! :eventsourcing/total-streams "Total streams"
                         #(count (:streams @event-store)))
  (log/info "Event sourcing initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-event-store-status []
  {:enabled (flags/is-enabled? "event-sourcing")
   :total-events (count (:events @event-store))
   :total-streams (count (:streams @event-store))
   :projections (count (:projections @event-store))
   :snapshots (count (:snapshots @event-store))
   :handlers (count (:handlers @event-store))
   :config (:config @event-store)})
