(ns mental-models.pipeline.integration.request-tracker
  "Request tracker for mental model analysis system.
   
   Features:
   - Request lifecycle tracking
   - Request state management
   - Request history
   - Request timeline
   - Request dependencies
   - Request cancellation
   - Request progress
   - Tracking metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent ConcurrentHashMap]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:requests (ConcurrentHashMap.)  ;; request-id -> request-state
         :history []                      ;; completed requests
         :config {:max-history 10000
                  :request-ttl-ms 3600000  ;; 1 hour
                  :track-progress? true
                  :track-dependencies? true}
         :stats {:requests-tracked 0
                 :requests-completed 0
                 :requests-failed 0
                 :requests-cancelled 0
                 :avg-duration-ms 0}
         :initialized? false}))

;; ============================================================================
;; Request States
;; ============================================================================

(def request-states
  #{:pending :queued :processing :waiting :completed :failed :cancelled :timeout})

;; ============================================================================
;; Request Tracking
;; ============================================================================

(defn start-tracking
  "Start tracking a request."
  [request-id & {:keys [metadata parent-id]}]
  (let [now (System/currentTimeMillis)
        request-state {:id request-id
                       :state :pending
                       :parent-id parent-id
                       :children (atom [])
                       :metadata (or metadata {})
                       :timeline [{:state :pending :timestamp now}]
                       :progress {:current 0 :total 100 :message nil}
                       :dependencies (atom [])
                       :result (atom nil)
                       :error (atom nil)
                       :cancelled? (atom false)
                       :created-at now
                       :updated-at (atom now)
                       :completed-at (atom nil)}]
    
    (.put (:requests @state) request-id request-state)
    (swap! state update-in [:stats :requests-tracked] inc)
    
    ;; Register as child of parent
    (when parent-id
      (when-let [parent (.get (:requests @state) parent-id)]
        (swap! (:children parent) conj request-id)))
    
    (logging/log :debug "Started tracking request" {:request-id request-id})
    (events/emit! :request-tracking-started {:request-id request-id})
    request-id))

(defn get-request
  "Get a tracked request."
  [request-id]
  (.get (:requests @state) request-id))

(defn list-requests
  "List all tracked requests."
  [& {:keys [state-filter limit] :or {limit 100}}]
  (let [requests (vals (:requests @state))]
    (cond->> requests
      state-filter (filter #(= (:state %) state-filter))
      true (take limit)
      true (mapv (fn [r]
                   {:id (:id r)
                    :state (:state r)
                    :progress (:progress r)
                    :created-at (:created-at r)
                    :duration-ms (when @(:completed-at r)
                                   (- @(:completed-at r) (:created-at r)))})))))

;; ============================================================================
;; State Transitions
;; ============================================================================

(defn update-state!
  "Update the state of a tracked request."
  [request-id new-state & {:keys [message]}]
  (when-let [request (get-request request-id)]
    (let [now (System/currentTimeMillis)
          old-state (:state request)]
      ;; Update state
      (.put (:requests @state) request-id
            (-> request
                (assoc :state new-state)
                (update :timeline conj {:state new-state
                                        :timestamp now
                                        :message message})))
      (reset! (:updated-at request) now)
      
      (logging/log :debug "Request state changed"
                   {:request-id request-id :from old-state :to new-state})
      (events/emit! :request-state-changed
                    {:request-id request-id :from old-state :to new-state})
      new-state)))

(defn mark-queued!
  "Mark a request as queued."
  [request-id & {:keys [position]}]
  (update-state! request-id :queued :message (str "Queued at position " position)))

(defn mark-processing!
  "Mark a request as processing."
  [request-id]
  (update-state! request-id :processing))

(defn mark-waiting!
  "Mark a request as waiting."
  [request-id & {:keys [reason]}]
  (update-state! request-id :waiting :message reason))

(defn mark-completed!
  "Mark a request as completed."
  [request-id & {:keys [result]}]
  (when-let [request (get-request request-id)]
    (let [now (System/currentTimeMillis)
          duration-ms (- now (:created-at request))]
      (reset! (:result request) result)
      (reset! (:completed-at request) now)
      (update-state! request-id :completed)
      (swap! state update-in [:stats :requests-completed] inc)
      
      ;; Update average duration
      (let [completed (:requests-completed (:stats @state))
            current-avg (:avg-duration-ms (:stats @state))
            new-avg (/ (+ (* current-avg (dec completed)) duration-ms) completed)]
        (swap! state assoc-in [:stats :avg-duration-ms] new-avg))
      
      ;; Move to history
      (move-to-history! request-id))))

(defn mark-failed!
  "Mark a request as failed."
  [request-id & {:keys [error]}]
  (when-let [request (get-request request-id)]
    (reset! (:error request) error)
    (reset! (:completed-at request) (System/currentTimeMillis))
    (update-state! request-id :failed :message (str error))
    (swap! state update-in [:stats :requests-failed] inc)
    (move-to-history! request-id)))

(defn mark-cancelled!
  "Mark a request as cancelled."
  [request-id & {:keys [reason]}]
  (when-let [request (get-request request-id)]
    (reset! (:cancelled? request) true)
    (reset! (:completed-at request) (System/currentTimeMillis))
    (update-state! request-id :cancelled :message reason)
    (swap! state update-in [:stats :requests-cancelled] inc)
    (move-to-history! request-id)))

(defn mark-timeout!
  "Mark a request as timed out."
  [request-id]
  (when-let [request (get-request request-id)]
    (reset! (:completed-at request) (System/currentTimeMillis))
    (update-state! request-id :timeout)
    (move-to-history! request-id)))

;; ============================================================================
;; Progress Tracking
;; ============================================================================

(defn update-progress!
  "Update the progress of a request."
  [request-id current & {:keys [total message]}]
  (when-let [request (get-request request-id)]
    (let [progress (:progress request)
          new-progress (cond-> {:current current}
                         total (assoc :total total)
                         (not total) (assoc :total (:total progress))
                         message (assoc :message message))]
      (.put (:requests @state) request-id
            (assoc request :progress new-progress))
      (reset! (:updated-at request) (System/currentTimeMillis))
      new-progress)))

(defn get-progress
  "Get the progress of a request."
  [request-id]
  (when-let [request (get-request request-id)]
    (let [progress (:progress request)]
      {:current (:current progress)
       :total (:total progress)
       :percentage (if (pos? (:total progress))
                     (* 100.0 (/ (:current progress) (:total progress)))
                     0)
       :message (:message progress)})))

;; ============================================================================
;; Dependencies
;; ============================================================================

(defn add-dependency!
  "Add a dependency to a request."
  [request-id dependency-id]
  (when-let [request (get-request request-id)]
    (swap! (:dependencies request) conj dependency-id)))

(defn get-dependencies
  "Get the dependencies of a request."
  [request-id]
  (when-let [request (get-request request-id)]
    @(:dependencies request)))

(defn dependencies-satisfied?
  "Check if all dependencies are satisfied."
  [request-id]
  (let [deps (get-dependencies request-id)]
    (every? (fn [dep-id]
              (let [dep (get-request dep-id)]
                (or (nil? dep)
                    (= (:state dep) :completed))))
            deps)))

;; ============================================================================
;; History Management
;; ============================================================================

(defn- move-to-history!
  "Move a completed request to history."
  [request-id]
  (when-let [request (get-request request-id)]
    (let [history-entry {:id (:id request)
                         :state (:state request)
                         :timeline (:timeline request)
                         :created-at (:created-at request)
                         :completed-at @(:completed-at request)
                         :duration-ms (- @(:completed-at request) (:created-at request))
                         :result @(:result request)
                         :error @(:error request)}
          max-history (get-in @state [:config :max-history])]
      
      ;; Add to history
      (swap! state update :history
             (fn [h]
               (let [new-history (conj h history-entry)]
                 (if (> (count new-history) max-history)
                   (vec (drop 1 new-history))
                   new-history))))
      
      ;; Remove from active requests
      (.remove (:requests @state) request-id))))

(defn get-history
  "Get request history."
  [& {:keys [limit since state-filter] :or {limit 100}}]
  (let [history (:history @state)]
    (cond->> history
      since (filter #(> (:completed-at %) since))
      state-filter (filter #(= (:state %) state-filter))
      true (take-last limit)
      true vec)))

(defn clear-history!
  "Clear request history."
  []
  (swap! state assoc :history []))

;; ============================================================================
;; Timeline
;; ============================================================================

(defn get-timeline
  "Get the timeline of a request."
  [request-id]
  (when-let [request (get-request request-id)]
    (:timeline request)))

(defn get-duration
  "Get the duration of a request."
  [request-id]
  (when-let [request (get-request request-id)]
    (let [end-time (or @(:completed-at request) (System/currentTimeMillis))]
      (- end-time (:created-at request)))))

;; ============================================================================
;; Cancellation
;; ============================================================================

(defn cancel-request!
  "Cancel a request."
  [request-id & {:keys [reason]}]
  (when-let [request (get-request request-id)]
    (when-not (#{:completed :failed :cancelled :timeout} (:state request))
      (mark-cancelled! request-id :reason reason)
      
      ;; Cancel children
      (doseq [child-id @(:children request)]
        (cancel-request! child-id :reason "Parent cancelled"))
      
      true)))

(defn is-cancelled?
  "Check if a request is cancelled."
  [request-id]
  (when-let [request (get-request request-id)]
    @(:cancelled? request)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-track-request
  "Ring middleware to track requests."
  [handler]
  (fn [request]
    (let [request-id (or (:request-id request) (str (UUID/randomUUID)))]
      (start-tracking request-id
                      :metadata {:uri (:uri request)
                                 :method (:request-method request)})
      (mark-processing! request-id)
      
      (try
        (let [response (handler (assoc request :tracking-id request-id))]
          (mark-completed! request-id :result {:status (:status response)})
          response)
        (catch Exception e
          (mark-failed! request-id :error (.getMessage e))
          (throw e))))))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn cleanup-expired!
  "Clean up expired requests."
  []
  (let [now (System/currentTimeMillis)
        ttl (get-in @state [:config :request-ttl-ms])
        expired (filter (fn [[_ r]]
                          (and (#{:completed :failed :cancelled :timeout} (:state r))
                               (> (- now @(:updated-at r)) ttl)))
                        (:requests @state))]
    (doseq [[id _] expired]
      (.remove (:requests @state) id))
    (count expired)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-tracker-metrics
  "Get tracker metrics."
  []
  (let [stats (:stats @state)
        requests (vals (:requests @state))]
    {:requests-tracked (:requests-tracked stats)
     :requests-completed (:requests-completed stats)
     :requests-failed (:requests-failed stats)
     :requests-cancelled (:requests-cancelled stats)
     :avg-duration-ms (:avg-duration-ms stats)
     :active-requests (count requests)
     :by-state (frequencies (map :state requests))
     :history-size (count (:history @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-tracker-stats
  "Get tracker statistics."
  []
  (merge (get-tracker-metrics)
         {:max-history (get-in @state [:config :max-history])
          :request-ttl-ms (get-in @state [:config :request-ttl-ms])}))

(defn reset-stats!
  "Reset tracker statistics."
  []
  (swap! state assoc :stats {:requests-tracked 0
                             :requests-completed 0
                             :requests-failed 0
                             :requests-cancelled 0
                             :avg-duration-ms 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-tracker!
  "Initialize the request tracker."
  []
  (when-not (:initialized? @state)
    ;; Start cleanup task
    (go-loop []
      (when (:initialized? @state)
        (<! (timeout 60000))
        (cleanup-expired!)
        (recur)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request tracker initialized")
    (events/emit! :request-tracker-initialized {})
    true))
