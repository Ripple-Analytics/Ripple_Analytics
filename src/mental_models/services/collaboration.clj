(ns mental-models.services.collaboration
  "Real-time collaboration service with WebSocket multiplayer
   Enables shared decision analysis sessions and presence awareness"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! mult tap]]
            [taoensso.timbre :as log])
  (:import [java.util UUID]
           [java.time Instant]))

;; -- State Management --------------------------------------------------------

(defonce ^:private state
  (atom {:sessions {}      ; session-id -> session data
         :users {}         ; user-id -> user data
         :presence {}      ; resource-id -> #{user-ids}
         :channels {}}))   ; user-id -> channel

(defonce ^:private broadcast-chan (chan 1000))
(defonce ^:private broadcast-mult (mult broadcast-chan))

;; -- Session Management ------------------------------------------------------

(defn create-session
  "Create a new collaboration session"
  [{:keys [owner-id title type resource-id]}]
  (let [session-id (str (UUID/randomUUID))
        session {:id session-id
                 :owner-id owner-id
                 :title title
                 :type type  ; :decision-analysis, :model-review, :brainstorm
                 :resource-id resource-id
                 :participants #{owner-id}
                 :state {}
                 :history []
                 :created-at (Instant/now)
                 :updated-at (Instant/now)}]
    (swap! state assoc-in [:sessions session-id] session)
    (log/info "Created collaboration session:" session-id)
    session))

(defn join-session
  "Join an existing collaboration session"
  [session-id user-id]
  (when-let [session (get-in @state [:sessions session-id])]
    (swap! state update-in [:sessions session-id :participants] conj user-id)
    (broadcast-to-session session-id
                          {:type :user-joined
                           :user-id user-id
                           :timestamp (Instant/now)})
    (get-in @state [:sessions session-id])))

(defn leave-session
  "Leave a collaboration session"
  [session-id user-id]
  (when-let [session (get-in @state [:sessions session-id])]
    (swap! state update-in [:sessions session-id :participants] disj user-id)
    (broadcast-to-session session-id
                          {:type :user-left
                           :user-id user-id
                           :timestamp (Instant/now)})
    ;; Close session if empty
    (when (empty? (get-in @state [:sessions session-id :participants]))
      (swap! state update :sessions dissoc session-id)
      (log/info "Closed empty session:" session-id))))

(defn get-session
  "Get session by ID"
  [session-id]
  (get-in @state [:sessions session-id]))

(defn list-sessions
  "List all active sessions, optionally filtered"
  [& {:keys [user-id type]}]
  (cond->> (vals (:sessions @state))
    user-id (filter #(contains? (:participants %) user-id))
    type (filter #(= type (:type %)))))

;; -- Presence Management -----------------------------------------------------

(defn update-presence
  "Update user presence on a resource"
  [user-id resource-id cursor-position]
  (let [presence {:user-id user-id
                  :resource-id resource-id
                  :cursor cursor-position
                  :last-seen (Instant/now)}]
    (swap! state assoc-in [:presence resource-id user-id] presence)
    (broadcast-presence resource-id presence)))

(defn remove-presence
  "Remove user presence from a resource"
  [user-id resource-id]
  (swap! state update-in [:presence resource-id] dissoc user-id)
  (broadcast-presence resource-id {:user-id user-id :left true}))

(defn get-presence
  "Get all users present on a resource"
  [resource-id]
  (vals (get-in @state [:presence resource-id] {})))

(defn cleanup-stale-presence
  "Remove presence entries older than threshold"
  [threshold-seconds]
  (let [cutoff (.minusSeconds (Instant/now) threshold-seconds)]
    (doseq [[resource-id users] (:presence @state)
            [user-id presence] users]
      (when (.isBefore (:last-seen presence) cutoff)
        (remove-presence user-id resource-id)))))

;; -- Channel Management ------------------------------------------------------

(defn register-channel
  "Register a WebSocket channel for a user"
  [user-id ws-channel]
  (let [user-chan (chan 100)]
    (tap broadcast-mult user-chan)
    (swap! state assoc-in [:channels user-id] {:ws ws-channel
                                                :internal user-chan})
    ;; Start forwarding loop
    (go-loop []
      (when-let [msg (<! user-chan)]
        (when (should-receive? user-id msg)
          (send-to-ws ws-channel msg))
        (recur)))
    user-chan))

(defn unregister-channel
  "Unregister a user's WebSocket channel"
  [user-id]
  (when-let [{:keys [internal]} (get-in @state [:channels user-id])]
    (close! internal)
    (swap! state update :channels dissoc user-id)
    ;; Clean up presence
    (doseq [[resource-id _] (:presence @state)]
      (remove-presence user-id resource-id))))

(defn- should-receive?
  "Check if user should receive this message"
  [user-id msg]
  (or (nil? (:target msg))
      (= user-id (:target msg))
      (contains? (:targets msg) user-id)
      (when-let [session-id (:session-id msg)]
        (contains? (get-in @state [:sessions session-id :participants]) user-id))))

(defn- send-to-ws
  "Send message to WebSocket (implementation depends on server)"
  [ws-channel msg]
  ;; This would be implemented based on the WebSocket library used
  (log/debug "Sending to WS:" msg))

;; -- Broadcasting ------------------------------------------------------------

(defn broadcast-to-session
  "Broadcast message to all session participants"
  [session-id msg]
  (let [msg (assoc msg :session-id session-id)]
    (go (>! broadcast-chan msg))))

(defn broadcast-presence
  "Broadcast presence update"
  [resource-id presence]
  (go (>! broadcast-chan {:type :presence-update
                          :resource-id resource-id
                          :presence presence})))

(defn broadcast-to-all
  "Broadcast message to all connected users"
  [msg]
  (go (>! broadcast-chan msg)))

;; -- Collaborative Operations ------------------------------------------------

(defn apply-operation
  "Apply a collaborative operation to session state"
  [session-id user-id operation]
  (when-let [session (get-session session-id)]
    (let [op-with-meta (assoc operation
                              :user-id user-id
                              :timestamp (Instant/now)
                              :version (count (:history session)))]
      ;; Apply operation to state
      (swap! state update-in [:sessions session-id]
             (fn [s]
               (-> s
                   (update :state (partial apply-op-to-state operation))
                   (update :history conj op-with-meta)
                   (assoc :updated-at (Instant/now)))))
      ;; Broadcast to participants
      (broadcast-to-session session-id
                            {:type :operation
                             :operation op-with-meta})
      op-with-meta)))

(defn- apply-op-to-state
  "Apply operation to session state based on type"
  [operation state]
  (case (:op-type operation)
    :set-value (assoc-in state (:path operation) (:value operation))
    :add-item (update-in state (:path operation) conj (:value operation))
    :remove-item (update-in state (:path operation) #(remove #{(:value operation)} %))
    :update-item (update-in state (:path operation) 
                            (fn [items]
                              (map #(if (= (:id %) (:id (:value operation)))
                                      (:value operation)
                                      %)
                                   items)))
    :add-annotation (update-in state [:annotations] conj (:value operation))
    :add-model (update-in state [:applied-models] conj (:value operation))
    :remove-model (update-in state [:applied-models] disj (:value operation))
    :set-conclusion (assoc state :conclusion (:value operation))
    state))

(defn get-session-state
  "Get current state of a session"
  [session-id]
  (get-in @state [:sessions session-id :state]))

(defn get-session-history
  "Get operation history for a session"
  [session-id & {:keys [since limit]}]
  (let [history (get-in @state [:sessions session-id :history] [])]
    (cond->> history
      since (filter #(.isAfter (:timestamp %) since))
      limit (take-last limit))))

;; -- Decision Analysis Sessions ----------------------------------------------

(defn create-decision-session
  "Create a collaborative decision analysis session"
  [owner-id decision-id title]
  (create-session {:owner-id owner-id
                   :title title
                   :type :decision-analysis
                   :resource-id decision-id}))

(defn add-model-to-analysis
  "Add a mental model to the analysis"
  [session-id user-id model-id rationale]
  (apply-operation session-id user-id
                   {:op-type :add-model
                    :value {:model-id model-id
                            :added-by user-id
                            :rationale rationale}}))

(defn add-annotation
  "Add an annotation to the analysis"
  [session-id user-id annotation]
  (apply-operation session-id user-id
                   {:op-type :add-annotation
                    :value {:id (str (UUID/randomUUID))
                            :author user-id
                            :text annotation
                            :timestamp (Instant/now)}}))

(defn set-conclusion
  "Set the session conclusion"
  [session-id user-id conclusion]
  (apply-operation session-id user-id
                   {:op-type :set-conclusion
                    :value {:text conclusion
                            :author user-id
                            :timestamp (Instant/now)}}))

;; -- Cursor Sync for Real-time Editing ---------------------------------------

(defn sync-cursor
  "Sync cursor position for collaborative editing"
  [session-id user-id cursor]
  (broadcast-to-session session-id
                        {:type :cursor-sync
                         :user-id user-id
                         :cursor cursor}))

(defn sync-selection
  "Sync text selection for collaborative editing"
  [session-id user-id selection]
  (broadcast-to-session session-id
                        {:type :selection-sync
                         :user-id user-id
                         :selection selection}))

;; -- Typing Indicators -------------------------------------------------------

(defn set-typing
  "Set typing indicator"
  [session-id user-id field-id]
  (broadcast-to-session session-id
                        {:type :typing
                         :user-id user-id
                         :field-id field-id
                         :typing true}))

(defn clear-typing
  "Clear typing indicator"
  [session-id user-id field-id]
  (broadcast-to-session session-id
                        {:type :typing
                         :user-id user-id
                         :field-id field-id
                         :typing false}))

;; -- Session Templates -------------------------------------------------------

(def session-templates
  {:decision-analysis
   {:name "Decision Analysis"
    :description "Collaborative analysis of a decision using mental models"
    :initial-state {:applied-models #{}
                    :annotations []
                    :pros []
                    :cons []
                    :risks []
                    :conclusion nil}}
   
   :model-review
   {:name "Model Review"
    :description "Review and discuss a mental model's application"
    :initial-state {:examples []
                    :counter-examples []
                    :edge-cases []
                    :notes []}}
   
   :brainstorm
   {:name "Brainstorming Session"
    :description "Free-form collaborative brainstorming"
    :initial-state {:ideas []
                    :categories {}
                    :votes {}}}
   
   :retrospective
   {:name "Decision Retrospective"
    :description "Review past decisions and their outcomes"
    :initial-state {:what-worked []
                    :what-didnt []
                    :lessons []
                    :action-items []}}})

(defn create-from-template
  "Create a session from a template"
  [owner-id template-key title resource-id]
  (when-let [template (get session-templates template-key)]
    (let [session (create-session {:owner-id owner-id
                                   :title title
                                   :type template-key
                                   :resource-id resource-id})]
      (swap! state assoc-in [:sessions (:id session) :state]
             (:initial-state template))
      (get-session (:id session)))))

;; -- Cleanup -----------------------------------------------------------------

(defn cleanup!
  "Clean up stale sessions and presence"
  []
  (cleanup-stale-presence 300) ; 5 minutes
  ;; Remove sessions with no activity for 1 hour
  (let [cutoff (.minusSeconds (Instant/now) 3600)]
    (doseq [[session-id session] (:sessions @state)]
      (when (.isBefore (:updated-at session) cutoff)
        (swap! state update :sessions dissoc session-id)
        (log/info "Cleaned up stale session:" session-id)))))

;; Start cleanup job
(defonce cleanup-job
  (go-loop []
    (<! (async/timeout 60000)) ; Every minute
    (cleanup!)
    (recur)))
