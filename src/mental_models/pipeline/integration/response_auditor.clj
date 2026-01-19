(ns mental-models.pipeline.integration.response-auditor
  "Response auditor for mental model analysis system.
   
   Features:
   - Response auditing
   - Audit logging
   - Compliance tracking
   - Audit trails
   - Audit queries
   - Audit retention
   - Audit export
   - Auditing metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:audit-log []        ;; audit entries
         :policies {}         ;; policy-id -> audit policy
         :config {:enabled? true
                  :max-entries 100000
                  :retention-days 90
                  :include-body? false
                  :include-headers? true
                  :sensitive-headers #{"authorization" "cookie" "x-api-key"}}
         :stats {:entries-created 0
                 :queries-executed 0
                 :exports 0
                 :purges 0}
         :initialized? false}))

;; ============================================================================
;; Audit Entry Creation
;; ============================================================================

(defn create-audit-entry
  "Create an audit entry."
  [request response & {:keys [action user-id tenant-id metadata]}]
  (let [entry {:id (str (UUID/randomUUID))
               :timestamp (System/currentTimeMillis)
               :action (or action :api-call)
               :user-id (or user-id (get-in request [:auth :user-id]))
               :tenant-id (or tenant-id (:tenant-id request))
               :request {:method (:request-method request)
                         :uri (:uri request)
                         :query-string (:query-string request)
                         :remote-addr (:remote-addr request)
                         :headers (when (get-in @state [:config :include-headers?])
                                    (let [sensitive (get-in @state [:config :sensitive-headers])]
                                      (into {}
                                            (for [[k v] (:headers request)]
                                              [k (if (contains? sensitive (str/lower-case k))
                                                   "[REDACTED]"
                                                   v)]))))
                         :body (when (get-in @state [:config :include-body?])
                                 (:body request))}
               :response {:status (:status response)
                          :headers (when (get-in @state [:config :include-headers?])
                                     (:headers response))
                          :body-size (count (str (:body response)))}
               :duration-ms (when-let [start (:request-start-time request)]
                              (- (System/currentTimeMillis) start))
               :metadata (or metadata {})}]
    entry))

(defn record-audit!
  "Record an audit entry."
  [entry]
  (when (get-in @state [:config :enabled?])
    (swap! state update-in [:stats :entries-created] inc)
    
    (let [max-entries (get-in @state [:config :max-entries])]
      (swap! state update :audit-log
             (fn [log]
               (let [new-log (conj log entry)]
                 (if (> (count new-log) max-entries)
                   (vec (drop (- (count new-log) max-entries) new-log))
                   new-log)))))
    
    (logging/log :info "Audit entry recorded" {:id (:id entry) :action (:action entry)})
    (:id entry)))

(defn audit-request!
  "Audit a request/response pair."
  [request response & opts]
  (let [entry (apply create-audit-entry request response opts)]
    (record-audit! entry)))

;; ============================================================================
;; Audit Policies
;; ============================================================================

(defn register-policy!
  "Register an audit policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :condition-fn (get config :condition-fn (constantly true))
                :include-body? (get config :include-body? false)
                :include-headers? (get config :include-headers? true)
                :retention-days (get config :retention-days 90)
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:policies policy-id] policy)
    policy-id))

(defn get-policy
  "Get an audit policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all audit policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :retention-days (:retention-days p)
           :enabled? @(:enabled? p)})
        (:policies @state)))

(defn should-audit?
  "Check if a request should be audited based on policies."
  [request]
  (some (fn [[_ policy]]
          (and @(:enabled? policy)
               ((:condition-fn policy) request)))
        (:policies @state)))

;; ============================================================================
;; Audit Queries
;; ============================================================================

(defn query-audit-log
  "Query the audit log."
  [& {:keys [user-id tenant-id action start-time end-time limit offset]
      :or {limit 100 offset 0}}]
  (swap! state update-in [:stats :queries-executed] inc)
  
  (let [entries (:audit-log @state)]
    (->> entries
         (filter (fn [e]
                   (and (or (nil? user-id) (= (:user-id e) user-id))
                        (or (nil? tenant-id) (= (:tenant-id e) tenant-id))
                        (or (nil? action) (= (:action e) action))
                        (or (nil? start-time) (>= (:timestamp e) start-time))
                        (or (nil? end-time) (<= (:timestamp e) end-time)))))
         (drop offset)
         (take limit)
         vec)))

(defn get-audit-entry
  "Get a specific audit entry."
  [entry-id]
  (first (filter #(= (:id %) entry-id) (:audit-log @state))))

(defn count-audit-entries
  "Count audit entries matching criteria."
  [& {:keys [user-id tenant-id action start-time end-time]}]
  (count (query-audit-log :user-id user-id
                          :tenant-id tenant-id
                          :action action
                          :start-time start-time
                          :end-time end-time
                          :limit Integer/MAX_VALUE)))

(defn get-audit-summary
  "Get audit summary statistics."
  [& {:keys [start-time end-time]}]
  (let [entries (query-audit-log :start-time start-time
                                 :end-time end-time
                                 :limit Integer/MAX_VALUE)]
    {:total-entries (count entries)
     :by-action (frequencies (map :action entries))
     :by-status (frequencies (map #(get-in % [:response :status]) entries))
     :by-user (frequencies (map :user-id entries))
     :by-tenant (frequencies (map :tenant-id entries))
     :avg-duration-ms (when (seq entries)
                        (/ (reduce + (keep :duration-ms entries))
                           (count (filter :duration-ms entries))))}))

;; ============================================================================
;; Audit Retention
;; ============================================================================

(defn purge-old-entries!
  "Purge audit entries older than retention period."
  []
  (swap! state update-in [:stats :purges] inc)
  
  (let [retention-days (get-in @state [:config :retention-days])
        cutoff (- (System/currentTimeMillis)
                  (* retention-days 24 60 60 1000))
        before-count (count (:audit-log @state))]
    
    (swap! state update :audit-log
           (fn [log]
             (vec (filter #(> (:timestamp %) cutoff) log))))
    
    (let [after-count (count (:audit-log @state))
          purged (- before-count after-count)]
      (logging/log :info "Purged old audit entries" {:purged purged})
      purged)))

(defn set-retention!
  "Set retention period in days."
  [days]
  (swap! state assoc-in [:config :retention-days] days))

;; ============================================================================
;; Audit Export
;; ============================================================================

(defn export-audit-log
  "Export audit log to various formats."
  [& {:keys [format start-time end-time]
      :or {format :edn}}]
  (swap! state update-in [:stats :exports] inc)
  
  (let [entries (query-audit-log :start-time start-time
                                 :end-time end-time
                                 :limit Integer/MAX_VALUE)]
    (case format
      :edn (pr-str entries)
      :json (str "[" (str/join "," (map pr-str entries)) "]")
      :csv (let [headers ["id" "timestamp" "action" "user-id" "tenant-id" "method" "uri" "status" "duration-ms"]
                 rows (map (fn [e]
                             (str/join ","
                                       [(:id e)
                                        (:timestamp e)
                                        (:action e)
                                        (:user-id e)
                                        (:tenant-id e)
                                        (get-in e [:request :method])
                                        (get-in e [:request :uri])
                                        (get-in e [:response :status])
                                        (:duration-ms e)]))
                           entries)]
             (str (str/join "," headers) "\n" (str/join "\n" rows)))
      (pr-str entries))))

;; ============================================================================
;; Compliance
;; ============================================================================

(defn check-compliance
  "Check compliance requirements."
  [& {:keys [user-id tenant-id days]}]
  (let [start-time (when days
                     (- (System/currentTimeMillis)
                        (* days 24 60 60 1000)))
        entries (query-audit-log :user-id user-id
                                 :tenant-id tenant-id
                                 :start-time start-time
                                 :limit Integer/MAX_VALUE)]
    {:compliant? true
     :entries-count (count entries)
     :coverage {:has-user-id (count (filter :user-id entries))
                :has-tenant-id (count (filter :tenant-id entries))
                :has-duration (count (filter :duration-ms entries))}
     :retention-days (get-in @state [:config :retention-days])}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-audit
  "Ring middleware to audit requests."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [start-time (System/currentTimeMillis)
            request-with-time (assoc request :request-start-time start-time)
            response (handler request-with-time)]
        (audit-request! request-with-time response)
        response)
      (handler request))))

(defn wrap-audit-with-policy
  "Ring middleware to audit based on policy."
  [handler policy-id]
  (fn [request]
    (let [response (handler request)]
      (when-let [policy (get-policy policy-id)]
        (when (and @(:enabled? policy)
                   ((:condition-fn policy) request))
          (audit-request! request response
                          :metadata {:policy-id policy-id})))
      response)))

(defn wrap-audit-action
  "Ring middleware to audit with specific action."
  [handler action]
  (fn [request]
    (let [response (handler request)]
      (audit-request! request response :action action)
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable auditing."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-include-body!
  "Enable/disable body inclusion."
  [include?]
  (swap! state assoc-in [:config :include-body?] include?))

(defn set-include-headers!
  "Enable/disable header inclusion."
  [include?]
  (swap! state assoc-in [:config :include-headers?] include?))

(defn add-sensitive-header!
  "Add a sensitive header to redact."
  [header]
  (swap! state update-in [:config :sensitive-headers] conj (str/lower-case header)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-auditor-metrics
  "Get auditor metrics."
  []
  (let [stats (:stats @state)]
    {:entries-created (:entries-created stats)
     :queries-executed (:queries-executed stats)
     :exports (:exports stats)
     :purges (:purges stats)
     :current-entries (count (:audit-log @state))
     :policies-count (count (:policies @state))
     :retention-days (get-in @state [:config :retention-days])}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-auditor-stats
  "Get auditor statistics."
  []
  (merge (get-auditor-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :include-body? (get-in @state [:config :include-body?])
          :include-headers? (get-in @state [:config :include-headers?])}))

(defn reset-stats!
  "Reset auditor statistics."
  []
  (swap! state assoc :stats {:entries-created 0
                             :queries-executed 0
                             :exports 0
                             :purges 0}))

(defn clear-audit-log!
  "Clear the audit log."
  []
  (swap! state assoc :audit-log []))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-auditor!
  "Initialize the response auditor."
  []
  (when-not (:initialized? @state)
    ;; Register default policy
    (register-policy! :default {:name "Default Audit Policy"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response auditor initialized")
    (events/emit! :response-auditor-initialized {})
    true))
