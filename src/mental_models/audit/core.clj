(ns mental-models.audit.core
  "Audit Logging Module for Mental Models Pipeline
   
   Provides audit logging with:
   - Operation tracking
   - User action logging
   - Data change tracking
   - Compliance reporting
   - Audit trail queries"
  (:require
   [clojure.string :as str]
   [cheshire.core :as json])
  (:import
   [java.util UUID]
   [java.time Instant]
   [java.util.concurrent ConcurrentLinkedQueue]))

;; =============================================================================
;; AUDIT ENTRY
;; =============================================================================

(defrecord AuditEntry [id timestamp operation user resource action details result])

(defn create-entry [operation user resource action details result]
  (->AuditEntry (str (UUID/randomUUID))
                (Instant/now)
                operation
                user
                resource
                action
                details
                result))

;; =============================================================================
;; AUDIT LOG STORAGE
;; =============================================================================

(def ^:private audit-log (ConcurrentLinkedQueue.))
(def ^:private max-entries (atom 10000))

(defn set-max-entries! [n]
  (reset! max-entries n))

(defn trim-log []
  (while (> (.size audit-log) @max-entries)
    (.poll audit-log)))

;; =============================================================================
;; LOGGING OPERATIONS
;; =============================================================================

(defn log-operation
  "Log an operation to the audit trail."
  [{:keys [operation user resource action details result]}]
  (let [entry (create-entry operation user resource action details result)]
    (.offer audit-log entry)
    (trim-log)
    entry))

(defn log-analysis
  "Log an analysis operation."
  [user document-id models-detected confidence]
  (log-operation {:operation :analysis
                  :user user
                  :resource document-id
                  :action :analyze
                  :details {:models-detected models-detected
                            :confidence confidence}
                  :result :success}))

(defn log-model-update
  "Log a model update operation."
  [user model-id changes]
  (log-operation {:operation :model-update
                  :user user
                  :resource model-id
                  :action :update
                  :details changes
                  :result :success}))

(defn log-config-change
  "Log a configuration change."
  [user config-key old-value new-value]
  (log-operation {:operation :config-change
                  :user user
                  :resource config-key
                  :action :update
                  :details {:old-value old-value
                            :new-value new-value}
                  :result :success}))

(defn log-access
  "Log a resource access."
  [user resource action]
  (log-operation {:operation :access
                  :user user
                  :resource resource
                  :action action
                  :details {}
                  :result :success}))

(defn log-error
  "Log an error."
  [user operation resource error]
  (log-operation {:operation operation
                  :user user
                  :resource resource
                  :action :error
                  :details {:error (str error)}
                  :result :failure}))

;; =============================================================================
;; QUERY OPERATIONS
;; =============================================================================

(defn get-all-entries []
  (vec audit-log))

(defn get-entries-by-user [user]
  (filter #(= (:user %) user) (get-all-entries)))

(defn get-entries-by-operation [operation]
  (filter #(= (:operation %) operation) (get-all-entries)))

(defn get-entries-by-resource [resource]
  (filter #(= (:resource %) resource) (get-all-entries)))

(defn get-entries-in-range [start-time end-time]
  (filter #(and (.isAfter (:timestamp %) start-time)
                (.isBefore (:timestamp %) end-time))
          (get-all-entries)))

(defn get-recent-entries [n]
  (take-last n (get-all-entries)))

(defn get-failed-operations []
  (filter #(= (:result %) :failure) (get-all-entries)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  (let [entries (get-all-entries)]
    {:total-entries (count entries)
     :by-operation (frequencies (map :operation entries))
     :by-result (frequencies (map :result entries))
     :by-user (frequencies (map :user entries))
     :oldest (when-let [e (first entries)] (:timestamp e))
     :newest (when-let [e (last entries)] (:timestamp e))}))

(defn get-user-activity [user]
  (let [entries (get-entries-by-user user)]
    {:total-actions (count entries)
     :by-operation (frequencies (map :operation entries))
     :by-result (frequencies (map :result entries))
     :recent (take-last 10 entries)}))

;; =============================================================================
;; COMPLIANCE REPORTING
;; =============================================================================

(defn generate-compliance-report [start-time end-time]
  (let [entries (get-entries-in-range start-time end-time)]
    {:report-period {:start start-time :end end-time}
     :total-operations (count entries)
     :operations-by-type (frequencies (map :operation entries))
     :success-rate (let [total (count entries)
                         successes (count (filter #(= (:result %) :success) entries))]
                     (if (zero? total) 1.0 (/ successes total)))
     :unique-users (count (distinct (map :user entries)))
     :unique-resources (count (distinct (map :resource entries)))
     :errors (filter #(= (:result %) :failure) entries)}))

;; =============================================================================
;; EXPORT
;; =============================================================================

(defn export-audit-log [format]
  (let [entries (map (fn [e]
                       {:id (:id e)
                        :timestamp (str (:timestamp e))
                        :operation (name (:operation e))
                        :user (:user e)
                        :resource (:resource e)
                        :action (name (:action e))
                        :details (:details e)
                        :result (name (:result e))})
                     (get-all-entries))]
    (case format
      :json (json/generate-string entries {:pretty true})
      :edn (pr-str entries)
      entries)))

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn wrap-audit-logging [handler]
  (fn [request]
    (let [user (or (get-in request [:headers "x-user-id"]) "anonymous")
          resource (:uri request)
          action (keyword (str/lower-case (name (:request-method request))))
          start-time (System/currentTimeMillis)]
      (try
        (let [response (handler request)
              duration (- (System/currentTimeMillis) start-time)]
          (log-operation {:operation :http-request
                          :user user
                          :resource resource
                          :action action
                          :details {:method (:request-method request)
                                    :status (:status response)
                                    :duration-ms duration}
                          :result (if (< (:status response) 400) :success :failure)})
          response)
        (catch Exception e
          (log-error user :http-request resource e)
          (throw e))))))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn clear-audit-log []
  (.clear audit-log))

(defn archive-old-entries [days-to-keep]
  (let [cutoff (.minusSeconds (Instant/now) (* days-to-keep 24 60 60))
        old-entries (filter #(.isBefore (:timestamp %) cutoff) (get-all-entries))]
    (doseq [e old-entries]
      (.remove audit-log e))
    (count old-entries)))
