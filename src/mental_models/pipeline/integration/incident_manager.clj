(ns mental-models.pipeline.integration.incident-manager
  "Incident manager for mental model analysis system.
   
   Features:
   - Incident creation
   - Severity classification
   - Escalation management
   - On-call scheduling
   - Incident timeline
   - Post-mortem tracking
   - Runbook integration
   - Status page updates"
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
  (atom {:incidents {}        ;; incident-id -> incident
         :escalations {}      ;; escalation-id -> escalation-policy
         :on-call {}          ;; schedule-id -> on-call-schedule
         :runbooks {}         ;; runbook-id -> runbook
         :post-mortems {}     ;; post-mortem-id -> post-mortem
         :status-page {}      ;; component-id -> status
         :config {:default-severity :medium
                  :auto-escalate-after-mins 30}
         :stats {:incidents-created 0 :incidents-resolved 0 :mttr-ms 0}
         :initialized? false}))

;; ============================================================================
;; Incident Management
;; ============================================================================

(defn create-incident!
  "Create an incident."
  [config]
  (when (flags/enabled? :incident-manager)
    (let [incident-id (str (UUID/randomUUID))
          incident {:id incident-id
                    :title (get config :title "Untitled Incident")
                    :description (get config :description "")
                    :severity (get config :severity (get-in @state [:config :default-severity]))
                    :status :open ;; :open, :investigating, :identified, :monitoring, :resolved
                    :priority (get config :priority :p2) ;; :p1, :p2, :p3, :p4
                    :affected-services (get config :affected-services [])
                    :assignee (get config :assignee)
                    :commander (get config :commander)
                    :timeline [{:event :created
                                :timestamp (System/currentTimeMillis)
                                :details {:created-by (get config :created-by)}}]
                    :tags (get config :tags #{})
                    :related-alerts (get config :related-alerts [])
                    :runbook-id (get config :runbook-id)
                    :created-at (System/currentTimeMillis)
                    :updated-at (System/currentTimeMillis)}]
      
      (swap! state assoc-in [:incidents incident-id] incident)
      (swap! state update-in [:stats :incidents-created] inc)
      
      ;; Update status page
      (doseq [service (:affected-services incident)]
        (update-status-page! service :degraded))
      
      (logging/log :warn "Incident created" {:incident-id incident-id :severity (:severity incident)})
      (events/emit! :incident-created {:incident-id incident-id})
      
      incident-id)))

(defn get-incident
  "Get an incident."
  [incident-id]
  (get-in @state [:incidents incident-id]))

(defn list-incidents
  "List incidents."
  [& {:keys [status severity assignee limit] :or {limit 100}}]
  (let [incidents (vals (:incidents @state))
        filtered (cond->> incidents
                   status (filter #(= (:status %) status))
                   severity (filter #(= (:severity %) severity))
                   assignee (filter #(= (:assignee %) assignee))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :title :severity :status :priority :created-at]) filtered)))

(defn update-incident!
  "Update an incident."
  [incident-id updates]
  (when-let [incident (get-incident incident-id)]
    (let [timeline-entry {:event :updated
                          :timestamp (System/currentTimeMillis)
                          :details updates}]
      (swap! state update-in [:incidents incident-id]
             (fn [i]
               (-> i
                   (merge updates)
                   (update :timeline conj timeline-entry)
                   (assoc :updated-at (System/currentTimeMillis)))))
      (logging/log :info "Incident updated" {:incident-id incident-id}))))

(defn add-timeline-event!
  "Add an event to incident timeline."
  [incident-id event details]
  (let [timeline-entry {:event event
                        :timestamp (System/currentTimeMillis)
                        :details details}]
    (swap! state update-in [:incidents incident-id :timeline] conj timeline-entry)))

(defn change-status!
  "Change incident status."
  [incident-id new-status & {:keys [message]}]
  (when-let [incident (get-incident incident-id)]
    (add-timeline-event! incident-id :status-change {:from (:status incident)
                                                      :to new-status
                                                      :message message})
    (swap! state assoc-in [:incidents incident-id :status] new-status)
    
    (when (= new-status :resolved)
      (let [created-at (:created-at incident)
            resolved-at (System/currentTimeMillis)
            resolution-time (- resolved-at created-at)]
        (swap! state assoc-in [:incidents incident-id :resolved-at] resolved-at)
        (swap! state update-in [:stats :incidents-resolved] inc)
        
        ;; Update MTTR
        (let [total-resolved (get-in @state [:stats :incidents-resolved])
              current-mttr (get-in @state [:stats :mttr-ms] 0)
              new-mttr (/ (+ (* current-mttr (dec total-resolved)) resolution-time) total-resolved)]
          (swap! state assoc-in [:stats :mttr-ms] new-mttr))
        
        ;; Update status page
        (doseq [service (:affected-services incident)]
          (update-status-page! service :operational))))
    
    (logging/log :info "Incident status changed" {:incident-id incident-id :status new-status})
    (events/emit! :incident-status-changed {:incident-id incident-id :status new-status})))

(defn assign-incident!
  "Assign an incident."
  [incident-id assignee]
  (add-timeline-event! incident-id :assigned {:assignee assignee})
  (swap! state assoc-in [:incidents incident-id :assignee] assignee)
  (logging/log :info "Incident assigned" {:incident-id incident-id :assignee assignee}))

;; ============================================================================
;; Escalation Management
;; ============================================================================

(defn create-escalation-policy!
  "Create an escalation policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :levels (get config :levels []) ;; [{:level 1 :responders [...] :timeout-mins 15}]
                :repeat? (get config :repeat? false)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:escalations policy-id] policy)
    (logging/log :info "Created escalation policy" {:policy-id policy-id})
    policy-id))

(defn get-escalation-policy
  "Get an escalation policy."
  [policy-id]
  (get-in @state [:escalations policy-id]))

(defn list-escalation-policies
  "List all escalation policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :levels (count (:levels p))})
        (:escalations @state)))

(defn escalate-incident!
  "Escalate an incident to the next level."
  [incident-id policy-id]
  (when-let [incident (get-incident incident-id)]
    (when-let [policy (get-escalation-policy policy-id)]
      (let [current-level (get incident :escalation-level 0)
            next-level (inc current-level)
            level-config (nth (:levels policy) next-level nil)]
        (when level-config
          (add-timeline-event! incident-id :escalated {:level next-level
                                                        :responders (:responders level-config)})
          (swap! state assoc-in [:incidents incident-id :escalation-level] next-level)
          (logging/log :warn "Incident escalated" {:incident-id incident-id :level next-level})
          (events/emit! :incident-escalated {:incident-id incident-id :level next-level}))))))

;; ============================================================================
;; On-Call Management
;; ============================================================================

(defn create-on-call-schedule!
  "Create an on-call schedule."
  [schedule-id config]
  (let [schedule {:id schedule-id
                  :name (get config :name (name schedule-id))
                  :team (get config :team)
                  :rotation (get config :rotation []) ;; [{:user "user1" :start "2024-01-01" :end "2024-01-07"}]
                  :timezone (get config :timezone "UTC")
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:on-call schedule-id] schedule)
    (logging/log :info "Created on-call schedule" {:schedule-id schedule-id})
    schedule-id))

(defn get-on-call-schedule
  "Get an on-call schedule."
  [schedule-id]
  (get-in @state [:on-call schedule-id]))

(defn get-current-on-call
  "Get current on-call person for a schedule."
  [schedule-id]
  (when-let [schedule (get-on-call-schedule schedule-id)]
    (let [now (System/currentTimeMillis)
          current (first (filter (fn [r]
                                   (and (<= (or (:start-ms r) 0) now)
                                        (>= (or (:end-ms r) Long/MAX_VALUE) now)))
                                 (:rotation schedule)))]
      (:user current))))

(defn add-on-call-override!
  "Add an on-call override."
  [schedule-id override]
  (swap! state update-in [:on-call schedule-id :overrides]
         (fnil conj [])
         (merge override {:created-at (System/currentTimeMillis)})))

;; ============================================================================
;; Runbook Management
;; ============================================================================

(defn create-runbook!
  "Create a runbook."
  [runbook-id config]
  (let [runbook {:id runbook-id
                 :name (get config :name (name runbook-id))
                 :description (get config :description "")
                 :steps (get config :steps [])
                 :tags (get config :tags #{})
                 :created-at (System/currentTimeMillis)
                 :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:runbooks runbook-id] runbook)
    (logging/log :info "Created runbook" {:runbook-id runbook-id})
    runbook-id))

(defn get-runbook
  "Get a runbook."
  [runbook-id]
  (get-in @state [:runbooks runbook-id]))

(defn list-runbooks
  "List all runbooks."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :step-count (count (:steps r))
           :tags (:tags r)})
        (:runbooks @state)))

(defn execute-runbook-step!
  "Mark a runbook step as executed for an incident."
  [incident-id runbook-id step-index & {:keys [result notes]}]
  (add-timeline-event! incident-id :runbook-step-executed
                       {:runbook-id runbook-id
                        :step-index step-index
                        :result result
                        :notes notes}))

;; ============================================================================
;; Post-Mortem Management
;; ============================================================================

(defn create-post-mortem!
  "Create a post-mortem."
  [incident-id config]
  (let [post-mortem-id (str (UUID/randomUUID))
        post-mortem {:id post-mortem-id
                     :incident-id incident-id
                     :title (get config :title)
                     :summary (get config :summary "")
                     :timeline (get config :timeline [])
                     :root-cause (get config :root-cause "")
                     :contributing-factors (get config :contributing-factors [])
                     :impact (get config :impact {})
                     :action-items (get config :action-items [])
                     :lessons-learned (get config :lessons-learned [])
                     :status :draft ;; :draft, :review, :published
                     :created-at (System/currentTimeMillis)
                     :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:post-mortems post-mortem-id] post-mortem)
    (swap! state assoc-in [:incidents incident-id :post-mortem-id] post-mortem-id)
    (logging/log :info "Created post-mortem" {:post-mortem-id post-mortem-id :incident-id incident-id})
    post-mortem-id))

(defn get-post-mortem
  "Get a post-mortem."
  [post-mortem-id]
  (get-in @state [:post-mortems post-mortem-id]))

(defn list-post-mortems
  "List all post-mortems."
  []
  (mapv (fn [[id pm]]
          {:id id
           :incident-id (:incident-id pm)
           :title (:title pm)
           :status (:status pm)})
        (:post-mortems @state)))

(defn update-post-mortem!
  "Update a post-mortem."
  [post-mortem-id updates]
  (swap! state update-in [:post-mortems post-mortem-id]
         (fn [pm]
           (merge pm updates {:updated-at (System/currentTimeMillis)}))))

(defn publish-post-mortem!
  "Publish a post-mortem."
  [post-mortem-id]
  (update-post-mortem! post-mortem-id {:status :published :published-at (System/currentTimeMillis)})
  (logging/log :info "Published post-mortem" {:post-mortem-id post-mortem-id}))

;; ============================================================================
;; Status Page
;; ============================================================================

(defn register-component!
  "Register a status page component."
  [component-id config]
  (let [component {:id component-id
                   :name (get config :name (name component-id))
                   :description (get config :description "")
                   :status :operational ;; :operational, :degraded, :partial-outage, :major-outage
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:status-page component-id] component)
    component-id))

(defn get-component-status
  "Get component status."
  [component-id]
  (get-in @state [:status-page component-id]))

(defn update-status-page!
  "Update status page component."
  [component-id status]
  (swap! state assoc-in [:status-page component-id :status] status)
  (swap! state assoc-in [:status-page component-id :updated-at] (System/currentTimeMillis))
  (events/emit! :status-page-updated {:component-id component-id :status status}))

(defn get-status-page
  "Get full status page."
  []
  (let [components (vals (:status-page @state))
        overall-status (cond
                         (some #(= :major-outage (:status %)) components) :major-outage
                         (some #(= :partial-outage (:status %)) components) :partial-outage
                         (some #(= :degraded (:status %)) components) :degraded
                         :else :operational)]
    {:overall-status overall-status
     :components (mapv #(select-keys % [:id :name :status :updated-at]) components)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-incident-stats
  "Get incident statistics."
  []
  (let [stats (:stats @state)
        incidents (vals (:incidents @state))
        open-incidents (filter #(not= :resolved (:status %)) incidents)
        by-severity (frequencies (map :severity incidents))]
    {:total-incidents (count incidents)
     :open-incidents (count open-incidents)
     :incidents-by-severity by-severity
     :total-post-mortems (count (:post-mortems @state))
     :incidents-created (:incidents-created stats)
     :incidents-resolved (:incidents-resolved stats)
     :mttr-hours (/ (:mttr-ms stats 0) 3600000.0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-incident-manager!
  "Initialize the incident manager."
  []
  (when-not (:initialized? @state)
    ;; Create escalation policy
    (create-escalation-policy! :default-escalation
                               {:name "Default Escalation"
                                :levels [{:level 1 :responders ["on-call-primary"] :timeout-mins 15}
                                         {:level 2 :responders ["on-call-secondary" "team-lead"] :timeout-mins 30}
                                         {:level 3 :responders ["engineering-manager"] :timeout-mins 60}]
                                :repeat? true})
    
    ;; Create on-call schedule
    (create-on-call-schedule! :engineering-on-call
                              {:name "Engineering On-Call"
                               :team "engineering"
                               :rotation []
                               :timezone "UTC"})
    
    ;; Create runbooks
    (create-runbook! :lm-studio-connection-failure
                     {:name "LM Studio Connection Failure"
                      :description "Steps to diagnose and resolve LM Studio connection issues"
                      :steps [{:title "Check LM Studio status"
                               :description "Verify LM Studio is running on localhost:1234"}
                              {:title "Check network connectivity"
                               :description "Verify network connectivity to LM Studio"}
                              {:title "Restart LM Studio"
                               :description "Restart the LM Studio application"}
                              {:title "Check logs"
                               :description "Review application logs for errors"}]
                      :tags #{:lm-studio :connection}})
    
    (create-runbook! :high-latency
                     {:name "High Latency Investigation"
                      :description "Steps to investigate high latency issues"
                      :steps [{:title "Check system resources"
                               :description "Monitor CPU, memory, and disk usage"}
                              {:title "Review recent changes"
                               :description "Check for recent deployments or configuration changes"}
                              {:title "Check external dependencies"
                               :description "Verify external service health"}
                              {:title "Scale resources if needed"
                               :description "Add more workers or increase capacity"}]
                      :tags #{:performance :latency}})
    
    ;; Register status page components
    (register-component! :analysis-service
                         {:name "Analysis Service"
                          :description "Mental model analysis service"})
    
    (register-component! :lm-studio-integration
                         {:name "LM Studio Integration"
                          :description "LM Studio LLM integration"})
    
    (register-component! :api
                         {:name "API"
                          :description "REST API endpoints"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Incident manager initialized")
    (events/emit! :incident-manager-initialized {})
    true))
