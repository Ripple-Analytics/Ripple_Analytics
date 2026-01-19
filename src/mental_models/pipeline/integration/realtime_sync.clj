(ns mental-models.pipeline.integration.realtime-sync
  "Real-time synchronization for mental model analysis.
   
   Features:
   - Real-time data sync
   - Conflict resolution
   - Operational transformation
   - Presence awareness
   - Collaborative editing
   - Change tracking
   - Offline support
   - Sync status monitoring"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout pub sub unsub]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:documents {}        ;; doc-id -> document-state
         :clients {}          ;; client-id -> client-info
         :presence {}         ;; doc-id -> {client-id -> presence}
         :operations {}       ;; doc-id -> [operations]
         :pending-ops {}      ;; client-id -> [pending-operations]
         :sync-status {}      ;; client-id -> sync-status
         :channels {}         ;; client-id -> channel
         :stats {:ops-applied 0 :conflicts-resolved 0 :syncs 0}
         :initialized? false}))

;; ============================================================================
;; Document Management
;; ============================================================================

(defn create-document!
  "Create a synchronized document."
  [doc-id config]
  (let [document {:id doc-id
                  :content (get config :content {})
                  :version 0
                  :created-by (get config :created-by "system")
                  :created-at (System/currentTimeMillis)
                  :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:documents doc-id] document)
    (swap! state assoc-in [:operations doc-id] [])
    (logging/log :info "Created document" {:doc-id doc-id})
    (events/emit! :document-created {:doc-id doc-id})
    doc-id))

(defn get-document
  "Get a document."
  [doc-id]
  (get-in @state [:documents doc-id]))

(defn list-documents
  "List all documents."
  []
  (mapv (fn [[id d]]
          {:id id
           :version (:version d)
           :updated-at (:updated-at d)})
        (:documents @state)))

(defn delete-document!
  "Delete a document."
  [doc-id]
  (swap! state update :documents dissoc doc-id)
  (swap! state update :operations dissoc doc-id)
  (swap! state update :presence dissoc doc-id)
  (logging/log :info "Deleted document" {:doc-id doc-id}))

;; ============================================================================
;; Client Management
;; ============================================================================

(defn connect-client!
  "Connect a client."
  [client-id & {:keys [user-id metadata]}]
  (let [client {:id client-id
                :user-id user-id
                :metadata (or metadata {})
                :connected-at (System/currentTimeMillis)
                :last-seen (System/currentTimeMillis)}
        ch (chan 100)]
    (swap! state assoc-in [:clients client-id] client)
    (swap! state assoc-in [:channels client-id] ch)
    (swap! state assoc-in [:sync-status client-id] {:status :connected :last-sync nil})
    (swap! state assoc-in [:pending-ops client-id] [])
    (logging/log :info "Client connected" {:client-id client-id})
    (events/emit! :client-connected {:client-id client-id})
    client-id))

(defn disconnect-client!
  "Disconnect a client."
  [client-id]
  (when-let [ch (get-in @state [:channels client-id])]
    (async/close! ch))
  ;; Remove presence from all documents
  (doseq [[doc-id _] (:presence @state)]
    (swap! state update-in [:presence doc-id] dissoc client-id))
  (swap! state update :clients dissoc client-id)
  (swap! state update :channels dissoc client-id)
  (swap! state update :sync-status dissoc client-id)
  (swap! state update :pending-ops dissoc client-id)
  (logging/log :info "Client disconnected" {:client-id client-id})
  (events/emit! :client-disconnected {:client-id client-id}))

(defn get-client
  "Get a client."
  [client-id]
  (get-in @state [:clients client-id]))

(defn list-clients
  "List all clients."
  []
  (mapv (fn [[id c]]
          {:id id
           :user-id (:user-id c)
           :connected-at (:connected-at c)})
        (:clients @state)))

(defn update-client-activity!
  "Update client last seen timestamp."
  [client-id]
  (swap! state assoc-in [:clients client-id :last-seen] (System/currentTimeMillis)))

;; ============================================================================
;; Presence
;; ============================================================================

(defn join-document!
  "Join a document for collaboration."
  [client-id doc-id & {:keys [cursor selection]}]
  (let [presence {:client-id client-id
                  :cursor cursor
                  :selection selection
                  :joined-at (System/currentTimeMillis)
                  :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:presence doc-id client-id] presence)
    (broadcast-to-document! doc-id :presence-joined {:client-id client-id :presence presence})
    (logging/log :debug "Client joined document" {:client-id client-id :doc-id doc-id})))

(defn leave-document!
  "Leave a document."
  [client-id doc-id]
  (swap! state update-in [:presence doc-id] dissoc client-id)
  (broadcast-to-document! doc-id :presence-left {:client-id client-id})
  (logging/log :debug "Client left document" {:client-id client-id :doc-id doc-id}))

(defn update-presence!
  "Update client presence in a document."
  [client-id doc-id presence-data]
  (swap! state update-in [:presence doc-id client-id]
         merge presence-data {:updated-at (System/currentTimeMillis)})
  (broadcast-to-document! doc-id :presence-updated {:client-id client-id :presence presence-data}))

(defn get-document-presence
  "Get all presence information for a document."
  [doc-id]
  (get-in @state [:presence doc-id] {}))

;; ============================================================================
;; Operations
;; ============================================================================

(defn- generate-op-id
  "Generate a unique operation ID."
  []
  (str (UUID/randomUUID)))

(defn- transform-operation
  "Transform an operation against another operation (OT)."
  [op1 op2]
  ;; Simplified operational transformation
  ;; In production, would implement full OT algorithm
  (cond
    ;; Insert vs Insert
    (and (= (:type op1) :insert) (= (:type op2) :insert))
    (if (<= (:position op1) (:position op2))
      op1
      (update op1 :position + (count (:value op2))))
    
    ;; Delete vs Insert
    (and (= (:type op1) :delete) (= (:type op2) :insert))
    (if (< (:position op1) (:position op2))
      op1
      (update op1 :position + (count (:value op2))))
    
    ;; Insert vs Delete
    (and (= (:type op1) :insert) (= (:type op2) :delete))
    (if (<= (:position op1) (:position op2))
      op1
      (update op1 :position - (:length op2)))
    
    ;; Delete vs Delete
    (and (= (:type op1) :delete) (= (:type op2) :delete))
    (cond
      (< (+ (:position op1) (:length op1)) (:position op2)) op1
      (< (+ (:position op2) (:length op2)) (:position op1))
      (update op1 :position - (:length op2))
      :else (assoc op1 :length 0)) ;; Overlapping deletes
    
    :else op1))

(defn- apply-operation
  "Apply an operation to document content."
  [content op]
  (case (:type op)
    :insert (let [path (:path op [])
                  pos (:position op 0)
                  value (:value op)]
              (if (empty? path)
                (str (subs (str content) 0 pos) value (subs (str content) pos))
                (update-in content path
                           (fn [v]
                             (str (subs (str v) 0 pos) value (subs (str v) pos))))))
    
    :delete (let [path (:path op [])
                  pos (:position op 0)
                  len (:length op 1)]
              (if (empty? path)
                (str (subs (str content) 0 pos) (subs (str content) (+ pos len)))
                (update-in content path
                           (fn [v]
                             (str (subs (str v) 0 pos) (subs (str v) (+ pos len)))))))
    
    :set (let [path (:path op [])
               value (:value op)]
           (if (empty? path)
             value
             (assoc-in content path value)))
    
    :update (let [path (:path op [])
                  update-fn (:fn op identity)]
              (if (empty? path)
                (update-fn content)
                (update-in content path update-fn)))
    
    content))

(defn submit-operation!
  "Submit an operation from a client."
  [client-id doc-id operation]
  (when (flags/enabled? :realtime-sync)
    (when-let [document (get-document doc-id)]
      (let [op-id (generate-op-id)
            client-version (:base-version operation (:version document))
            server-version (:version document)
            op (assoc operation
                      :id op-id
                      :client-id client-id
                      :timestamp (System/currentTimeMillis))]
        
        ;; Transform operation if needed
        (let [ops-to-transform (drop client-version (get-in @state [:operations doc-id] []))
              transformed-op (reduce transform-operation op ops-to-transform)
              
              ;; Apply operation
              new-content (apply-operation (:content document) transformed-op)
              new-version (inc server-version)]
          
          ;; Update document
          (swap! state update-in [:documents doc-id]
                 (fn [d]
                   (assoc d
                          :content new-content
                          :version new-version
                          :updated-at (System/currentTimeMillis))))
          
          ;; Store operation
          (swap! state update-in [:operations doc-id] conj transformed-op)
          
          ;; Broadcast to other clients
          (broadcast-to-document! doc-id :operation
                                  {:operation transformed-op
                                   :version new-version}
                                  :exclude client-id)
          
          (swap! state update-in [:stats :ops-applied] inc)
          (metrics/increment :operations-applied {:doc-id doc-id})
          (logging/log :debug "Applied operation" {:doc-id doc-id :op-id op-id :version new-version})
          
          {:success true
           :op-id op-id
           :version new-version})))))

;; ============================================================================
;; Broadcasting
;; ============================================================================

(defn- broadcast-to-document!
  "Broadcast a message to all clients in a document."
  [doc-id event-type data & {:keys [exclude]}]
  (let [presence (get-document-presence doc-id)
        message {:type event-type
                 :doc-id doc-id
                 :data data
                 :timestamp (System/currentTimeMillis)}]
    (doseq [[client-id _] presence
            :when (not= client-id exclude)]
      (when-let [ch (get-in @state [:channels client-id])]
        (async/put! ch message)))))

(defn broadcast-to-client!
  "Send a message to a specific client."
  [client-id event-type data]
  (when-let [ch (get-in @state [:channels client-id])]
    (async/put! ch {:type event-type
                    :data data
                    :timestamp (System/currentTimeMillis)})))

;; ============================================================================
;; Sync Status
;; ============================================================================

(defn get-sync-status
  "Get sync status for a client."
  [client-id]
  (get-in @state [:sync-status client-id]))

(defn update-sync-status!
  "Update sync status for a client."
  [client-id status]
  (swap! state update-in [:sync-status client-id]
         merge status {:updated-at (System/currentTimeMillis)}))

(defn request-sync!
  "Request a full sync for a client."
  [client-id doc-id]
  (when-let [document (get-document doc-id)]
    (update-sync-status! client-id {:status :syncing})
    (broadcast-to-client! client-id :sync
                          {:doc-id doc-id
                           :content (:content document)
                           :version (:version document)})
    (update-sync-status! client-id {:status :synced :last-sync (System/currentTimeMillis)})
    (swap! state update-in [:stats :syncs] inc)
    (logging/log :debug "Sync completed" {:client-id client-id :doc-id doc-id})))

;; ============================================================================
;; Offline Support
;; ============================================================================

(defn queue-offline-operation!
  "Queue an operation for offline client."
  [client-id operation]
  (swap! state update-in [:pending-ops client-id] conj
         (assoc operation :queued-at (System/currentTimeMillis))))

(defn get-pending-operations
  "Get pending operations for a client."
  [client-id]
  (get-in @state [:pending-ops client-id] []))

(defn sync-offline-operations!
  "Sync pending offline operations."
  [client-id doc-id]
  (let [pending (get-pending-operations client-id)]
    (doseq [op pending]
      (submit-operation! client-id doc-id op))
    (swap! state assoc-in [:pending-ops client-id] [])
    (count pending)))

;; ============================================================================
;; Conflict Resolution
;; ============================================================================

(defn- resolve-conflict
  "Resolve a conflict between operations."
  [op1 op2 strategy]
  (case strategy
    :last-write-wins (if (> (:timestamp op1) (:timestamp op2)) op1 op2)
    :first-write-wins (if (< (:timestamp op1) (:timestamp op2)) op1 op2)
    :merge (merge op1 op2)
    op1))

(defn handle-conflict!
  "Handle a conflict between operations."
  [doc-id op1 op2 & {:keys [strategy] :or {strategy :last-write-wins}}]
  (let [resolved (resolve-conflict op1 op2 strategy)]
    (swap! state update-in [:stats :conflicts-resolved] inc)
    (logging/log :info "Resolved conflict" {:doc-id doc-id :strategy strategy})
    resolved))

;; ============================================================================
;; Change Tracking
;; ============================================================================

(defn get-document-history
  "Get operation history for a document."
  [doc-id & {:keys [since limit]}]
  (let [ops (get-in @state [:operations doc-id] [])
        filtered (if since
                   (filter #(> (:timestamp %) since) ops)
                   ops)]
    (if limit
      (take-last limit filtered)
      filtered)))

(defn get-changes-since
  "Get changes since a specific version."
  [doc-id version]
  (let [ops (get-in @state [:operations doc-id] [])]
    (drop version ops)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-sync-stats
  "Get real-time sync statistics."
  []
  (let [stats (:stats @state)]
    {:total-documents (count (:documents @state))
     :total-clients (count (:clients @state))
     :operations-applied (:ops-applied stats)
     :conflicts-resolved (:conflicts-resolved stats)
     :total-syncs (:syncs stats)
     :active-collaborations (count (filter #(> (count (val %)) 1) (:presence @state)))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-realtime-sync!
  "Initialize real-time sync."
  []
  (when-not (:initialized? @state)
    ;; Create sample document
    (create-document! :sample-analysis
                      {:content {:title "Sample Analysis"
                                 :text "This is a sample document for collaborative analysis."
                                 :models []}
                       :created-by "system"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Real-time sync initialized")
    (events/emit! :realtime-sync-initialized {})
    true))
