(ns mental-models.pipeline.integration.compliance-manager
  "Compliance manager for mental model analysis system.
   
   Features:
   - Policy management
   - Compliance checks
   - Audit trails
   - Data retention
   - Privacy controls
   - Regulatory reporting
   - Consent management
   - Data classification"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:policies {}         ;; policy-id -> policy
         :checks {}           ;; check-id -> check-result
         :audit-log []        ;; audit entries
         :retention-rules {}  ;; rule-id -> rule
         :consents {}         ;; consent-id -> consent
         :classifications {}  ;; data-id -> classification
         :violations []       ;; violation records
         :config {:default-retention-days 365
                  :audit-retention-days 2555}
         :stats {:checks-performed 0 :violations-detected 0}
         :initialized? false}))

;; ============================================================================
;; Policy Management
;; ============================================================================

(defn create-policy!
  "Create a compliance policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :description (get config :description "")
                :type (get config :type :general) ;; :general, :gdpr, :hipaa, :sox, :pci
                :rules (get config :rules [])
                :severity (get config :severity :medium) ;; :low, :medium, :high, :critical
                :enabled? (get config :enabled? true)
                :created-at (System/currentTimeMillis)
                :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:policies policy-id] policy)
    (logging/log :info "Created policy" {:policy-id policy-id})
    policy-id))

(defn get-policy
  "Get a policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all policies."
  [& {:keys [type enabled?]}]
  (let [policies (vals (:policies @state))
        filtered (cond->> policies
                   type (filter #(= (:type %) type))
                   (some? enabled?) (filter #(= (:enabled? %) enabled?)))]
    (mapv #(select-keys % [:id :name :type :severity :enabled?]) filtered)))

(defn update-policy!
  "Update a policy."
  [policy-id updates]
  (swap! state update-in [:policies policy-id]
         (fn [p]
           (merge p updates {:updated-at (System/currentTimeMillis)}))))

(defn enable-policy!
  "Enable a policy."
  [policy-id]
  (update-policy! policy-id {:enabled? true}))

(defn disable-policy!
  "Disable a policy."
  [policy-id]
  (update-policy! policy-id {:enabled? false}))

;; ============================================================================
;; Compliance Checks
;; ============================================================================

(defn- evaluate-rule
  "Evaluate a single rule."
  [rule data]
  (let [field (:field rule)
        operator (:operator rule)
        expected (:value rule)
        actual (get data field)]
    (case operator
      :exists (some? actual)
      :not-exists (nil? actual)
      :equals (= actual expected)
      :not-equals (not= actual expected)
      :contains (and (string? actual) (str/includes? actual expected))
      :not-contains (and (string? actual) (not (str/includes? actual expected)))
      :matches (and (string? actual) (re-matches (re-pattern expected) actual))
      :greater-than (and (number? actual) (> actual expected))
      :less-than (and (number? actual) (< actual expected))
      :in (contains? (set expected) actual)
      :not-in (not (contains? (set expected) actual))
      false)))

(defn check-compliance
  "Check data against a policy."
  [policy-id data]
  (when (flags/enabled? :compliance-manager)
    (when-let [policy (get-policy policy-id)]
      (when (:enabled? policy)
        (let [rules (:rules policy)
              results (mapv (fn [rule]
                              {:rule-id (:id rule)
                               :passed? (evaluate-rule rule data)
                               :field (:field rule)
                               :message (:message rule)})
                            rules)
              all-passed? (every? :passed? results)
              check-id (str (UUID/randomUUID))
              check-result {:id check-id
                            :policy-id policy-id
                            :data-hash (hash data)
                            :passed? all-passed?
                            :results results
                            :timestamp (System/currentTimeMillis)}]
          
          (swap! state assoc-in [:checks check-id] check-result)
          (swap! state update-in [:stats :checks-performed] inc)
          
          ;; Record violation if failed
          (when-not all-passed?
            (let [violation {:id (str (UUID/randomUUID))
                             :policy-id policy-id
                             :check-id check-id
                             :severity (:severity policy)
                             :failed-rules (filter (complement :passed?) results)
                             :timestamp (System/currentTimeMillis)}]
              (swap! state update :violations conj violation)
              (swap! state update-in [:stats :violations-detected] inc)
              (logging/log :warn "Compliance violation detected" {:policy-id policy-id})
              (events/emit! :compliance-violation {:policy-id policy-id :check-id check-id})))
          
          check-result)))))

(defn check-all-policies
  "Check data against all enabled policies."
  [data]
  (let [policies (filter :enabled? (vals (:policies @state)))]
    (mapv #(check-compliance (:id %) data) policies)))

(defn get-check
  "Get a check result."
  [check-id]
  (get-in @state [:checks check-id]))

;; ============================================================================
;; Audit Trail
;; ============================================================================

(defn log-audit!
  "Log an audit entry."
  [config]
  (let [entry {:id (str (UUID/randomUUID))
               :action (get config :action)
               :actor (get config :actor)
               :resource (get config :resource)
               :resource-id (get config :resource-id)
               :details (get config :details {})
               :ip-address (get config :ip-address)
               :user-agent (get config :user-agent)
               :timestamp (System/currentTimeMillis)
               :date (str (LocalDate/now))}]
    (swap! state update :audit-log conj entry)
    (logging/log :debug "Audit logged" {:action (:action entry)})
    entry))

(defn get-audit-log
  "Get audit log entries."
  [& {:keys [action actor resource date-from date-to limit] :or {limit 1000}}]
  (let [log (:audit-log @state)
        filtered (cond->> log
                   action (filter #(= (:action %) action))
                   actor (filter #(= (:actor %) actor))
                   resource (filter #(= (:resource %) resource))
                   date-from (filter #(>= (:timestamp %) date-from))
                   date-to (filter #(<= (:timestamp %) date-to))
                   limit (take-last limit))]
    (vec filtered)))

(defn export-audit-log
  "Export audit log for reporting."
  [& {:keys [format date-from date-to] :or {format :edn}}]
  (let [log (get-audit-log :date-from date-from :date-to date-to)]
    (case format
      :edn (pr-str log)
      :json (str "[" (str/join "," (map pr-str log)) "]")
      :csv (str "id,action,actor,resource,timestamp\n"
                (str/join "\n"
                          (map #(str/join "," [(:id %) (:action %) (:actor %) (:resource %) (:timestamp %)])
                               log))))))

;; ============================================================================
;; Data Retention
;; ============================================================================

(defn create-retention-rule!
  "Create a data retention rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :data-type (get config :data-type)
              :retention-days (get config :retention-days (get-in @state [:config :default-retention-days]))
              :action (get config :action :delete) ;; :delete, :archive, :anonymize
              :enabled? (get config :enabled? true)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:retention-rules rule-id] rule)
    (logging/log :info "Created retention rule" {:rule-id rule-id})
    rule-id))

(defn get-retention-rule
  "Get a retention rule."
  [rule-id]
  (get-in @state [:retention-rules rule-id]))

(defn list-retention-rules
  "List all retention rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :data-type (:data-type r)
           :retention-days (:retention-days r)
           :action (:action r)
           :enabled? (:enabled? r)})
        (:retention-rules @state)))

(defn apply-retention-rules!
  "Apply retention rules to data."
  [data-type data-items]
  (let [rules (filter #(and (:enabled? %)
                            (= (:data-type %) data-type))
                      (vals (:retention-rules @state)))
        now (System/currentTimeMillis)]
    (mapv (fn [rule]
            (let [cutoff-ms (* (:retention-days rule) 24 60 60 1000)
                  expired (filter #(> (- now (get % :created-at 0)) cutoff-ms) data-items)]
              {:rule-id (:id rule)
               :action (:action rule)
               :affected-count (count expired)
               :items expired}))
          rules)))

;; ============================================================================
;; Consent Management
;; ============================================================================

(defn record-consent!
  "Record user consent."
  [consent-id config]
  (let [consent {:id consent-id
                 :user-id (get config :user-id)
                 :purpose (get config :purpose)
                 :scope (get config :scope [])
                 :granted? (get config :granted? true)
                 :expires-at (get config :expires-at)
                 :source (get config :source)
                 :ip-address (get config :ip-address)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:consents consent-id] consent)
    (log-audit! {:action :consent-recorded
                 :actor (:user-id consent)
                 :resource :consent
                 :resource-id consent-id
                 :details {:purpose (:purpose consent) :granted? (:granted? consent)}})
    (logging/log :info "Recorded consent" {:consent-id consent-id})
    consent-id))

(defn get-consent
  "Get a consent record."
  [consent-id]
  (get-in @state [:consents consent-id]))

(defn check-consent
  "Check if user has given consent for a purpose."
  [user-id purpose]
  (let [consents (filter #(and (= (:user-id %) user-id)
                               (= (:purpose %) purpose)
                               (:granted? %)
                               (or (nil? (:expires-at %))
                                   (> (:expires-at %) (System/currentTimeMillis))))
                         (vals (:consents @state)))]
    {:has-consent? (seq consents)
     :consents (vec consents)}))

(defn revoke-consent!
  "Revoke a consent."
  [consent-id]
  (swap! state assoc-in [:consents consent-id :granted?] false)
  (swap! state assoc-in [:consents consent-id :revoked-at] (System/currentTimeMillis))
  (log-audit! {:action :consent-revoked
               :resource :consent
               :resource-id consent-id}))

;; ============================================================================
;; Data Classification
;; ============================================================================

(defn classify-data!
  "Classify data for compliance purposes."
  [data-id config]
  (let [classification {:id data-id
                        :level (get config :level :internal) ;; :public, :internal, :confidential, :restricted
                        :categories (get config :categories #{}) ;; :pii, :phi, :financial, :sensitive
                        :handling-requirements (get config :handling-requirements [])
                        :retention-policy (get config :retention-policy)
                        :classified-by (get config :classified-by)
                        :classified-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:classifications data-id] classification)
    (logging/log :info "Classified data" {:data-id data-id :level (:level classification)})
    classification))

(defn get-classification
  "Get data classification."
  [data-id]
  (get-in @state [:classifications data-id]))

(defn list-classifications
  "List data classifications."
  [& {:keys [level category]}]
  (let [classifications (vals (:classifications @state))
        filtered (cond->> classifications
                   level (filter #(= (:level %) level))
                   category (filter #(contains? (:categories %) category)))]
    (vec filtered)))

;; ============================================================================
;; Violations
;; ============================================================================

(defn get-violations
  "Get compliance violations."
  [& {:keys [policy-id severity date-from date-to resolved?]}]
  (let [violations (:violations @state)
        filtered (cond->> violations
                   policy-id (filter #(= (:policy-id %) policy-id))
                   severity (filter #(= (:severity %) severity))
                   date-from (filter #(>= (:timestamp %) date-from))
                   date-to (filter #(<= (:timestamp %) date-to))
                   (some? resolved?) (filter #(= (:resolved? %) resolved?)))]
    (vec filtered)))

(defn resolve-violation!
  "Resolve a violation."
  [violation-id resolution]
  (swap! state update :violations
         (fn [violations]
           (mapv (fn [v]
                   (if (= (:id v) violation-id)
                     (assoc v
                            :resolved? true
                            :resolution resolution
                            :resolved-at (System/currentTimeMillis))
                     v))
                 violations)))
  (log-audit! {:action :violation-resolved
               :resource :violation
               :resource-id violation-id
               :details {:resolution resolution}}))

;; ============================================================================
;; Reports
;; ============================================================================

(defn generate-compliance-report
  "Generate compliance report."
  [& {:keys [date-from date-to]}]
  (let [checks (filter #(and (or (nil? date-from) (>= (:timestamp %) date-from))
                             (or (nil? date-to) (<= (:timestamp %) date-to)))
                       (vals (:checks @state)))
        violations (get-violations :date-from date-from :date-to date-to)
        policies (vals (:policies @state))]
    {:period {:from date-from :to date-to}
     :summary {:total-checks (count checks)
               :passed-checks (count (filter :passed? checks))
               :failed-checks (count (filter (complement :passed?) checks))
               :total-violations (count violations)
               :resolved-violations (count (filter :resolved? violations))
               :unresolved-violations (count (filter (complement :resolved?) violations))}
     :by-policy (reduce (fn [m policy]
                          (let [policy-checks (filter #(= (:policy-id %) (:id policy)) checks)]
                            (assoc m (:id policy)
                                   {:name (:name policy)
                                    :checks (count policy-checks)
                                    :passed (count (filter :passed? policy-checks))
                                    :failed (count (filter (complement :passed?) policy-checks))})))
                        {}
                        policies)
     :by-severity (frequencies (map :severity violations))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-compliance-stats
  "Get compliance statistics."
  []
  (let [stats (:stats @state)]
    {:total-policies (count (:policies @state))
     :enabled-policies (count (filter :enabled? (vals (:policies @state))))
     :total-retention-rules (count (:retention-rules @state))
     :total-consents (count (:consents @state))
     :total-classifications (count (:classifications @state))
     :total-violations (count (:violations @state))
     :unresolved-violations (count (filter (complement :resolved?) (:violations @state)))
     :audit-log-entries (count (:audit-log @state))
     :checks-performed (:checks-performed stats)
     :violations-detected (:violations-detected stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-compliance-manager!
  "Initialize the compliance manager."
  []
  (when-not (:initialized? @state)
    ;; Create data privacy policy
    (create-policy! :data-privacy
                    {:name "Data Privacy Policy"
                     :description "Ensures personal data is handled properly"
                     :type :gdpr
                     :severity :high
                     :rules [{:id :pii-encrypted
                              :field :encryption
                              :operator :equals
                              :value true
                              :message "PII must be encrypted"}
                             {:id :consent-required
                              :field :has-consent
                              :operator :equals
                              :value true
                              :message "User consent required for data processing"}]})
    
    ;; Create data retention policy
    (create-policy! :data-retention
                    {:name "Data Retention Policy"
                     :description "Ensures data is retained appropriately"
                     :type :general
                     :severity :medium
                     :rules [{:id :retention-defined
                              :field :retention-policy
                              :operator :exists
                              :message "Retention policy must be defined"}]})
    
    ;; Create retention rules
    (create-retention-rule! :analysis-results
                            {:name "Analysis Results Retention"
                             :data-type :analysis
                             :retention-days 365
                             :action :archive})
    
    (create-retention-rule! :audit-logs
                            {:name "Audit Log Retention"
                             :data-type :audit
                             :retention-days 2555 ;; 7 years
                             :action :archive})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Compliance manager initialized")
    (events/emit! :compliance-manager-initialized {})
    true))
