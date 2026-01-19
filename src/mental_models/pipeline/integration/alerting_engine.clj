(ns mental-models.pipeline.integration.alerting-engine
  "Advanced alerting engine for mental model analysis.
   
   Features:
   - Multi-channel alert delivery
   - Alert rules and conditions
   - Alert aggregation and deduplication
   - Escalation policies
   - On-call scheduling
   - Alert silencing
   - Alert correlation
   - Incident management"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalTime DayOfWeek]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:alerts {}           ;; alert-id -> alert
         :rules {}            ;; rule-id -> rule
         :channels {}         ;; channel-id -> channel-config
         :escalations {}      ;; escalation-id -> escalation-policy
         :schedules {}        ;; schedule-id -> on-call-schedule
         :silences {}         ;; silence-id -> silence-config
         :incidents {}        ;; incident-id -> incident
         :stats {:alerts-fired 0 :alerts-resolved 0 :notifications-sent 0}
         :initialized? false}))

;; ============================================================================
;; Alert Channels
;; ============================================================================

(defn register-channel!
  "Register an alert channel."
  [channel-id config]
  (let [channel {:id channel-id
                 :name (get config :name (name channel-id))
                 :type (get config :type :email)
                 :config (get config :config {})
                 :enabled? (get config :enabled? true)
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:channels channel-id] channel)
    (logging/log :info "Registered alert channel" {:channel-id channel-id :type (:type channel)})
    channel-id))

(defn get-channel
  "Get an alert channel."
  [channel-id]
  (get-in @state [:channels channel-id]))

(defn list-channels
  "List all alert channels."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :type (:type c)
           :enabled? (:enabled? c)})
        (:channels @state)))

(defn- send-to-channel!
  "Send an alert to a channel."
  [channel-id alert]
  (when-let [channel (get-channel channel-id)]
    (when (:enabled? channel)
      (case (:type channel)
        :email (logging/log :info "Sending email alert" {:to (get-in channel [:config :to]) :alert-id (:id alert)})
        :slack (logging/log :info "Sending Slack alert" {:channel (get-in channel [:config :channel]) :alert-id (:id alert)})
        :webhook (logging/log :info "Sending webhook alert" {:url (get-in channel [:config :url]) :alert-id (:id alert)})
        :pagerduty (logging/log :info "Sending PagerDuty alert" {:service (get-in channel [:config :service]) :alert-id (:id alert)})
        (logging/log :info "Sending alert" {:channel-id channel-id :alert-id (:id alert)}))
      (swap! state update-in [:stats :notifications-sent] inc)
      true)))

;; ============================================================================
;; Alert Rules
;; ============================================================================

(defn create-rule!
  "Create an alert rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :description (get config :description "")
              :condition (get config :condition)
              :severity (get config :severity :warning)
              :channels (get config :channels [])
              :labels (get config :labels {})
              :annotations (get config :annotations {})
              :for-duration-ms (get config :for-duration-ms 0)
              :evaluation-interval-ms (get config :evaluation-interval-ms 60000)
              :enabled? (get config :enabled? true)
              :last-evaluation nil
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Created alert rule" {:rule-id rule-id})
    rule-id))

(defn get-rule
  "Get an alert rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all alert rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :severity (:severity r)
           :enabled? (:enabled? r)})
        (:rules @state)))

(defn update-rule!
  "Update an alert rule."
  [rule-id updates]
  (swap! state update-in [:rules rule-id] merge updates))

(defn delete-rule!
  "Delete an alert rule."
  [rule-id]
  (swap! state update :rules dissoc rule-id))

(defn enable-rule!
  "Enable an alert rule."
  [rule-id]
  (update-rule! rule-id {:enabled? true}))

(defn disable-rule!
  "Disable an alert rule."
  [rule-id]
  (update-rule! rule-id {:enabled? false}))

;; ============================================================================
;; Alert Management
;; ============================================================================

(defn- generate-alert-fingerprint
  "Generate a fingerprint for alert deduplication."
  [rule-id labels]
  (str rule-id "-" (hash labels)))

(defn fire-alert!
  "Fire an alert."
  [rule-id & {:keys [labels annotations value]}]
  (when (flags/enabled? :alerting-engine)
    (let [rule (get-rule rule-id)
          fingerprint (generate-alert-fingerprint rule-id labels)
          existing (first (filter #(= (:fingerprint (val %)) fingerprint)
                                  (:alerts @state)))
          alert-id (or (key existing) (str (UUID/randomUUID)))
          alert {:id alert-id
                 :rule-id rule-id
                 :fingerprint fingerprint
                 :name (:name rule)
                 :severity (:severity rule)
                 :labels (merge (:labels rule) labels)
                 :annotations (merge (:annotations rule) annotations)
                 :value value
                 :status :firing
                 :fired-at (or (get-in existing [1 :fired-at]) (System/currentTimeMillis))
                 :updated-at (System/currentTimeMillis)
                 :resolved-at nil
                 :notification-count (inc (get-in existing [1 :notification-count] 0))}]
      
      ;; Check if silenced
      (when-not (is-silenced? alert)
        (swap! state assoc-in [:alerts alert-id] alert)
        
        ;; Send to channels if new or re-firing
        (when (or (nil? existing) (= :resolved (get-in existing [1 :status])))
          (doseq [channel-id (:channels rule)]
            (send-to-channel! channel-id alert))
          (swap! state update-in [:stats :alerts-fired] inc))
        
        (logging/log :warn "Alert fired" {:alert-id alert-id :rule-id rule-id :severity (:severity rule)})
        (events/emit! :alert-fired {:alert-id alert-id :rule-id rule-id})
        (metrics/increment :alerts-fired {:severity (:severity rule)})
        
        alert-id))))

(defn resolve-alert!
  "Resolve an alert."
  [alert-id]
  (when-let [alert (get-in @state [:alerts alert-id])]
    (swap! state update-in [:alerts alert-id]
           (fn [a]
             (assoc a
                    :status :resolved
                    :resolved-at (System/currentTimeMillis))))
    (swap! state update-in [:stats :alerts-resolved] inc)
    (logging/log :info "Alert resolved" {:alert-id alert-id})
    (events/emit! :alert-resolved {:alert-id alert-id})))

(defn get-alert
  "Get an alert."
  [alert-id]
  (get-in @state [:alerts alert-id]))

(defn list-alerts
  "List alerts."
  [& {:keys [status severity rule-id since limit]}]
  (let [alerts (vals (:alerts @state))
        filtered (cond->> alerts
                   status (filter #(= (:status %) status))
                   severity (filter #(= (:severity %) severity))
                   rule-id (filter #(= (:rule-id %) rule-id))
                   since (filter #(>= (:fired-at %) since))
                   true (sort-by :fired-at >)
                   limit (take limit))]
    (vec filtered)))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id & {:keys [acknowledged-by comment]}]
  (swap! state update-in [:alerts alert-id]
         (fn [a]
           (assoc a
                  :acknowledged? true
                  :acknowledged-at (System/currentTimeMillis)
                  :acknowledged-by acknowledged-by
                  :acknowledge-comment comment)))
  (logging/log :info "Alert acknowledged" {:alert-id alert-id :by acknowledged-by}))

;; ============================================================================
;; Alert Silencing
;; ============================================================================

(defn create-silence!
  "Create an alert silence."
  [silence-id config]
  (let [silence {:id silence-id
                 :matchers (get config :matchers {})
                 :starts-at (get config :starts-at (System/currentTimeMillis))
                 :ends-at (get config :ends-at (+ (System/currentTimeMillis) 3600000))
                 :created-by (get config :created-by "system")
                 :comment (get config :comment "")
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:silences silence-id] silence)
    (logging/log :info "Created silence" {:silence-id silence-id})
    silence-id))

(defn is-silenced?
  "Check if an alert is silenced."
  [alert]
  (let [now (System/currentTimeMillis)
        active-silences (filter (fn [[_ s]]
                                  (and (<= (:starts-at s) now)
                                       (>= (:ends-at s) now)))
                                (:silences @state))]
    (some (fn [[_ silence]]
            (every? (fn [[k v]]
                      (= (get-in alert [:labels k]) v))
                    (:matchers silence)))
          active-silences)))

(defn delete-silence!
  "Delete a silence."
  [silence-id]
  (swap! state update :silences dissoc silence-id))

(defn list-silences
  "List all silences."
  []
  (mapv (fn [[id s]]
          {:id id
           :matchers (:matchers s)
           :starts-at (:starts-at s)
           :ends-at (:ends-at s)
           :created-by (:created-by s)})
        (:silences @state)))

;; ============================================================================
;; Escalation Policies
;; ============================================================================

(defn create-escalation!
  "Create an escalation policy."
  [escalation-id config]
  (let [escalation {:id escalation-id
                    :name (get config :name (name escalation-id))
                    :steps (get config :steps [])
                    :repeat-after-ms (get config :repeat-after-ms nil)
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:escalations escalation-id] escalation)
    (logging/log :info "Created escalation policy" {:escalation-id escalation-id})
    escalation-id))

(defn get-escalation
  "Get an escalation policy."
  [escalation-id]
  (get-in @state [:escalations escalation-id]))

(defn escalate-alert!
  "Escalate an alert to the next level."
  [alert-id escalation-id]
  (when-let [alert (get-alert alert-id)]
    (when-let [escalation (get-escalation escalation-id)]
      (let [current-step (get alert :escalation-step 0)
            steps (:steps escalation)
            next-step (get steps current-step)]
        (when next-step
          ;; Send to escalation channels
          (doseq [channel-id (:channels next-step)]
            (send-to-channel! channel-id alert))
          ;; Update alert
          (swap! state update-in [:alerts alert-id]
                 (fn [a]
                   (assoc a
                          :escalation-step (inc current-step)
                          :last-escalation (System/currentTimeMillis))))
          (logging/log :info "Escalated alert" {:alert-id alert-id :step (inc current-step)}))))))

;; ============================================================================
;; On-Call Scheduling
;; ============================================================================

(defn create-schedule!
  "Create an on-call schedule."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :timezone (get config :timezone "UTC")
                  :rotations (get config :rotations [])
                  :overrides (get config :overrides [])
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schedules schedule-id] schedule)
    (logging/log :info "Created on-call schedule" {:schedule-id schedule-id})
    schedule-id))

(defn get-on-call
  "Get the current on-call person for a schedule."
  [schedule-id]
  (when-let [schedule (get-in @state [:schedules schedule-id])]
    (let [now (System/currentTimeMillis)
          ;; Check overrides first
          override (first (filter (fn [o]
                                    (and (<= (:starts-at o) now)
                                         (>= (:ends-at o) now)))
                                  (:overrides schedule)))]
      (if override
        {:user (:user override)
         :type :override}
        ;; Fall back to rotation
        (when-let [rotation (first (:rotations schedule))]
          {:user (first (:users rotation))
           :type :rotation})))))

;; ============================================================================
;; Incident Management
;; ============================================================================

(defn create-incident!
  "Create an incident from alerts."
  [incident-id config]
  (let [incident {:id incident-id
                  :title (get config :title "Incident")
                  :description (get config :description "")
                  :severity (get config :severity :warning)
                  :status :open
                  :alerts (get config :alerts [])
                  :assignee (get config :assignee nil)
                  :timeline []
                  :created-at (System/currentTimeMillis)
                  :resolved-at nil}]
    (swap! state assoc-in [:incidents incident-id] incident)
    (logging/log :info "Created incident" {:incident-id incident-id})
    (events/emit! :incident-created {:incident-id incident-id})
    incident-id))

(defn update-incident!
  "Update an incident."
  [incident-id updates]
  (swap! state update-in [:incidents incident-id] merge updates)
  ;; Add to timeline
  (swap! state update-in [:incidents incident-id :timeline]
         conj {:action :updated
               :updates updates
               :timestamp (System/currentTimeMillis)}))

(defn resolve-incident!
  "Resolve an incident."
  [incident-id & {:keys [resolution]}]
  (swap! state update-in [:incidents incident-id]
         (fn [i]
           (assoc i
                  :status :resolved
                  :resolution resolution
                  :resolved-at (System/currentTimeMillis))))
  (swap! state update-in [:incidents incident-id :timeline]
         conj {:action :resolved
               :resolution resolution
               :timestamp (System/currentTimeMillis)})
  (logging/log :info "Resolved incident" {:incident-id incident-id})
  (events/emit! :incident-resolved {:incident-id incident-id}))

(defn get-incident
  "Get an incident."
  [incident-id]
  (get-in @state [:incidents incident-id]))

(defn list-incidents
  "List incidents."
  [& {:keys [status severity since limit]}]
  (let [incidents (vals (:incidents @state))
        filtered (cond->> incidents
                   status (filter #(= (:status %) status))
                   severity (filter #(= (:severity %) severity))
                   since (filter #(>= (:created-at %) since))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Alert Correlation
;; ============================================================================

(defn correlate-alerts
  "Find correlated alerts."
  [alert-id & {:keys [time-window-ms] :or {time-window-ms 300000}}]
  (when-let [alert (get-alert alert-id)]
    (let [fired-at (:fired-at alert)
          window-start (- fired-at time-window-ms)
          window-end (+ fired-at time-window-ms)
          all-alerts (vals (:alerts @state))
          correlated (filter (fn [a]
                               (and (not= (:id a) alert-id)
                                    (>= (:fired-at a) window-start)
                                    (<= (:fired-at a) window-end)))
                             all-alerts)]
      {:alert-id alert-id
       :correlated (mapv #(select-keys % [:id :name :severity :fired-at]) correlated)
       :count (count correlated)})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-engine-stats
  "Get alerting engine statistics."
  []
  (let [stats (:stats @state)
        alerts (vals (:alerts @state))
        by-status (group-by :status alerts)
        by-severity (group-by :severity alerts)]
    {:alerts-fired (:alerts-fired stats)
     :alerts-resolved (:alerts-resolved stats)
     :notifications-sent (:notifications-sent stats)
     :active-alerts (count (get by-status :firing []))
     :resolved-alerts (count (get by-status :resolved []))
     :by-severity (into {} (map (fn [[k v]] [k (count v)]) by-severity))
     :total-rules (count (:rules @state))
     :total-channels (count (:channels @state))
     :total-incidents (count (:incidents @state))
     :active-silences (count (filter (fn [[_ s]]
                                       (let [now (System/currentTimeMillis)]
                                         (and (<= (:starts-at s) now)
                                              (>= (:ends-at s) now))))
                                     (:silences @state)))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-alerting-engine!
  "Initialize the alerting engine."
  []
  (when-not (:initialized? @state)
    ;; Register default channels
    (register-channel! :console
                       {:name "Console"
                        :type :console
                        :enabled? true})
    
    (register-channel! :slack-alerts
                       {:name "Slack Alerts"
                        :type :slack
                        :config {:channel "#alerts"}
                        :enabled? false})
    
    ;; Create default rules
    (create-rule! :high-error-rate
                  {:name "High Error Rate"
                   :description "Alert when error rate exceeds threshold"
                   :severity :critical
                   :channels [:console]
                   :labels {:category "errors"}})
    
    (create-rule! :lollapalooza-detected
                  {:name "Lollapalooza Effect Detected"
                   :description "Alert when Lollapalooza effect is detected"
                   :severity :info
                   :channels [:console]
                   :labels {:category "analysis"}})
    
    ;; Create default escalation
    (create-escalation! :default
                        {:name "Default Escalation"
                         :steps [{:delay-ms 0 :channels [:console]}
                                 {:delay-ms 300000 :channels [:slack-alerts]}]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Alerting engine initialized")
    (events/emit! :alerting-engine-initialized {})
    true))
