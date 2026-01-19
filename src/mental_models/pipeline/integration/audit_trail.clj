(ns mental-models.pipeline.integration.audit-trail
  "Comprehensive audit trail for mental model analysis system.
   
   Features:
   - Immutable audit logging
   - Event sourcing integration
   - Tamper detection
   - Compliance reporting
   - Query and search
   - Retention policies
   - Export capabilities
   - Chain of custody"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.security MessageDigest]
           [java.nio.charset StandardCharsets]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:entries []          ;; audit entries (append-only)
         :indexes {}          ;; various indexes for fast lookup
         :policies {}         ;; retention policies
         :checksums {}        ;; entry checksums for tamper detection
         :config {:hash-algorithm "SHA-256"
                  :max-entries 1000000
                  :retention-days 2555}
         :stats {:entries-created 0 :queries 0 :exports 0}
         :initialized? false}))

;; ============================================================================
;; Hashing and Integrity
;; ============================================================================

(defn- compute-hash
  "Compute SHA-256 hash of data."
  [data]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (.getBytes (pr-str data) StandardCharsets/UTF_8)]
    (.update digest bytes)
    (let [hash-bytes (.digest digest)]
      (apply str (map #(format "%02x" %) hash-bytes)))))

(defn- compute-chain-hash
  "Compute hash including previous entry hash for chain integrity."
  [entry previous-hash]
  (compute-hash (assoc entry :previous-hash previous-hash)))

;; ============================================================================
;; Audit Entry Creation
;; ============================================================================

(defn create-entry!
  "Create an immutable audit entry."
  [config]
  (when (flags/enabled? :audit-trail)
    (let [entry-id (str (UUID/randomUUID))
          previous-entries (:entries @state)
          previous-hash (if (seq previous-entries)
                          (get-in @state [:checksums (-> previous-entries last :id)])
                          "genesis")
          entry {:id entry-id
                 :timestamp (System/currentTimeMillis)
                 :date (str (LocalDate/now))
                 :action (get config :action)
                 :category (get config :category :general) ;; :auth, :data, :config, :system, :analysis
                 :actor (get config :actor)
                 :actor-type (get config :actor-type :user) ;; :user, :system, :api, :scheduled
                 :resource-type (get config :resource-type)
                 :resource-id (get config :resource-id)
                 :details (get config :details {})
                 :ip-address (get config :ip-address)
                 :user-agent (get config :user-agent)
                 :session-id (get config :session-id)
                 :correlation-id (get config :correlation-id)
                 :outcome (get config :outcome :success) ;; :success, :failure, :partial
                 :severity (get config :severity :info) ;; :debug, :info, :warning, :error, :critical
                 :tags (get config :tags #{})}
          
          entry-hash (compute-chain-hash entry previous-hash)]
      
      ;; Append entry (immutable)
      (swap! state update :entries conj entry)
      (swap! state assoc-in [:checksums entry-id] entry-hash)
      
      ;; Update indexes
      (swap! state update-in [:indexes :by-action (:action entry)] (fnil conj []) entry-id)
      (swap! state update-in [:indexes :by-actor (:actor entry)] (fnil conj []) entry-id)
      (swap! state update-in [:indexes :by-resource (:resource-type entry) (:resource-id entry)] (fnil conj []) entry-id)
      (swap! state update-in [:indexes :by-date (:date entry)] (fnil conj []) entry-id)
      
      (swap! state update-in [:stats :entries-created] inc)
      
      (logging/log :debug "Audit entry created" {:entry-id entry-id :action (:action entry)})
      
      entry-id)))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(defn log-auth!
  "Log authentication event."
  [action actor & {:keys [outcome details ip-address]}]
  (create-entry! {:action action
                  :category :auth
                  :actor actor
                  :outcome (or outcome :success)
                  :details (or details {})
                  :ip-address ip-address
                  :severity (if (= outcome :failure) :warning :info)}))

(defn log-data-access!
  "Log data access event."
  [action actor resource-type resource-id & {:keys [details]}]
  (create-entry! {:action action
                  :category :data
                  :actor actor
                  :resource-type resource-type
                  :resource-id resource-id
                  :details (or details {})}))

(defn log-config-change!
  "Log configuration change."
  [action actor & {:keys [old-value new-value details]}]
  (create-entry! {:action action
                  :category :config
                  :actor actor
                  :details (merge {:old-value old-value :new-value new-value} (or details {}))
                  :severity :warning}))

(defn log-analysis!
  "Log analysis event."
  [action actor & {:keys [document-id models-detected details]}]
  (create-entry! {:action action
                  :category :analysis
                  :actor actor
                  :resource-type :analysis
                  :details (merge {:document-id document-id :models-detected models-detected} (or details {}))}))

(defn log-system-event!
  "Log system event."
  [action & {:keys [details severity]}]
  (create-entry! {:action action
                  :category :system
                  :actor-type :system
                  :details (or details {})
                  :severity (or severity :info)}))

;; ============================================================================
;; Query Functions
;; ============================================================================

(defn get-entry
  "Get an audit entry by ID."
  [entry-id]
  (first (filter #(= (:id %) entry-id) (:entries @state))))

(defn query-entries
  "Query audit entries with filters."
  [& {:keys [action category actor resource-type resource-id date-from date-to
             outcome severity limit offset]
      :or {limit 100 offset 0}}]
  (swap! state update-in [:stats :queries] inc)
  (let [entries (:entries @state)
        filtered (cond->> entries
                   action (filter #(= (:action %) action))
                   category (filter #(= (:category %) category))
                   actor (filter #(= (:actor %) actor))
                   resource-type (filter #(= (:resource-type %) resource-type))
                   resource-id (filter #(= (:resource-id %) resource-id))
                   date-from (filter #(>= (:timestamp %) date-from))
                   date-to (filter #(<= (:timestamp %) date-to))
                   outcome (filter #(= (:outcome %) outcome))
                   severity (filter #(= (:severity %) severity))
                   true (drop offset)
                   limit (take limit))]
    (vec filtered)))

(defn get-entries-by-actor
  "Get all entries for an actor."
  [actor & {:keys [limit] :or {limit 100}}]
  (let [entry-ids (get-in @state [:indexes :by-actor actor] [])]
    (take limit (map get-entry entry-ids))))

(defn get-entries-by-resource
  "Get all entries for a resource."
  [resource-type resource-id & {:keys [limit] :or {limit 100}}]
  (let [entry-ids (get-in @state [:indexes :by-resource resource-type resource-id] [])]
    (take limit (map get-entry entry-ids))))

(defn get-entries-by-date
  "Get all entries for a date."
  [date]
  (let [entry-ids (get-in @state [:indexes :by-date date] [])]
    (mapv get-entry entry-ids)))

(defn search-entries
  "Search entries by text in details."
  [search-text & {:keys [limit] :or {limit 100}}]
  (let [entries (:entries @state)
        matches (filter (fn [e]
                          (str/includes? (str (:details e)) search-text))
                        entries)]
    (take limit matches)))

;; ============================================================================
;; Integrity Verification
;; ============================================================================

(defn verify-entry-integrity
  "Verify integrity of a single entry."
  [entry-id]
  (when-let [entry (get-entry entry-id)]
    (let [stored-hash (get-in @state [:checksums entry-id])
          entries (:entries @state)
          entry-index (.indexOf (mapv :id entries) entry-id)
          previous-hash (if (pos? entry-index)
                          (get-in @state [:checksums (:id (nth entries (dec entry-index)))])
                          "genesis")
          computed-hash (compute-chain-hash entry previous-hash)]
      {:entry-id entry-id
       :valid? (= stored-hash computed-hash)
       :stored-hash stored-hash
       :computed-hash computed-hash})))

(defn verify-chain-integrity
  "Verify integrity of the entire audit chain."
  []
  (let [entries (:entries @state)
        results (atom {:valid? true :invalid-entries []})]
    (loop [remaining entries
           previous-hash "genesis"]
      (when (seq remaining)
        (let [entry (first remaining)
              stored-hash (get-in @state [:checksums (:id entry)])
              computed-hash (compute-chain-hash entry previous-hash)]
          (when (not= stored-hash computed-hash)
            (swap! results assoc :valid? false)
            (swap! results update :invalid-entries conj (:id entry)))
          (recur (rest remaining) stored-hash))))
    @results))

;; ============================================================================
;; Retention Policies
;; ============================================================================

(defn create-retention-policy!
  "Create a retention policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :category (get config :category)
                :retention-days (get config :retention-days (get-in @state [:config :retention-days]))
                :action (get config :action :archive) ;; :archive, :delete
                :enabled? (get config :enabled? true)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created retention policy" {:policy-id policy-id})
    policy-id))

(defn apply-retention-policies!
  "Apply retention policies to audit entries."
  []
  (let [now (System/currentTimeMillis)
        policies (filter :enabled? (vals (:policies @state)))
        to-process (atom {:archived [] :deleted []})]
    
    (doseq [policy policies]
      (let [cutoff-ms (* (:retention-days policy) 24 60 60 1000)
            cutoff-time (- now cutoff-ms)
            entries-to-process (filter (fn [e]
                                         (and (< (:timestamp e) cutoff-time)
                                              (or (nil? (:category policy))
                                                  (= (:category e) (:category policy)))))
                                       (:entries @state))]
        (doseq [entry entries-to-process]
          (case (:action policy)
            :archive (swap! to-process update :archived conj (:id entry))
            :delete (swap! to-process update :deleted conj (:id entry))
            nil))))
    
    @to-process))

;; ============================================================================
;; Export Functions
;; ============================================================================

(defn export-entries
  "Export audit entries."
  [& {:keys [format date-from date-to category] :or {format :edn}}]
  (swap! state update-in [:stats :exports] inc)
  (let [entries (query-entries :date-from date-from :date-to date-to :category category :limit nil)]
    (case format
      :edn (pr-str entries)
      :json (str "[" (str/join "," (map pr-str entries)) "]")
      :csv (let [headers "id,timestamp,action,category,actor,resource_type,resource_id,outcome,severity"
                 rows (map (fn [e]
                             (str/join "," [(:id e) (:timestamp e) (:action e) (:category e)
                                            (:actor e) (:resource-type e) (:resource-id e)
                                            (:outcome e) (:severity e)]))
                           entries)]
             (str headers "\n" (str/join "\n" rows)))
      (pr-str entries))))

(defn generate-compliance-report
  "Generate compliance report."
  [& {:keys [date-from date-to]}]
  (let [entries (query-entries :date-from date-from :date-to date-to :limit nil)
        by-category (group-by :category entries)
        by-outcome (group-by :outcome entries)
        by-severity (group-by :severity entries)]
    {:period {:from date-from :to date-to}
     :total-entries (count entries)
     :by-category (reduce-kv (fn [m k v] (assoc m k (count v))) {} by-category)
     :by-outcome (reduce-kv (fn [m k v] (assoc m k (count v))) {} by-outcome)
     :by-severity (reduce-kv (fn [m k v] (assoc m k (count v))) {} by-severity)
     :failures (count (filter #(= :failure (:outcome %)) entries))
     :critical-events (count (filter #(= :critical (:severity %)) entries))
     :chain-integrity (verify-chain-integrity)
     :generated-at (System/currentTimeMillis)}))

;; ============================================================================
;; Chain of Custody
;; ============================================================================

(defn get-chain-of-custody
  "Get chain of custody for a resource."
  [resource-type resource-id]
  (let [entries (get-entries-by-resource resource-type resource-id)]
    {:resource-type resource-type
     :resource-id resource-id
     :custody-chain (mapv (fn [e]
                            {:timestamp (:timestamp e)
                             :action (:action e)
                             :actor (:actor e)
                             :details (:details e)})
                          (sort-by :timestamp entries))
     :total-events (count entries)
     :first-event (first (sort-by :timestamp entries))
     :last-event (last (sort-by :timestamp entries))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-audit-stats
  "Get audit trail statistics."
  []
  (let [stats (:stats @state)
        entries (:entries @state)]
    {:total-entries (count entries)
     :entries-by-category (frequencies (map :category entries))
     :entries-by-outcome (frequencies (map :outcome entries))
     :entries-by-severity (frequencies (map :severity entries))
     :unique-actors (count (distinct (map :actor entries)))
     :date-range {:earliest (when (seq entries) (:timestamp (first entries)))
                  :latest (when (seq entries) (:timestamp (last entries)))}
     :chain-valid? (:valid? (verify-chain-integrity))
     :entries-created (:entries-created stats)
     :queries (:queries stats)
     :exports (:exports stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-audit-trail!
  "Initialize the audit trail system."
  []
  (when-not (:initialized? @state)
    ;; Create default retention policies
    (create-retention-policy! :default-retention
                              {:name "Default Retention"
                               :retention-days 2555 ;; 7 years
                               :action :archive})
    
    (create-retention-policy! :debug-retention
                              {:name "Debug Log Retention"
                               :category :system
                               :retention-days 30
                               :action :delete})
    
    ;; Log system initialization
    (log-system-event! :audit-trail-initialized
                       :details {:version "1.0.0"
                                 :hash-algorithm "SHA-256"}
                       :severity :info)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Audit trail initialized")
    (events/emit! :audit-trail-initialized {})
    true))
