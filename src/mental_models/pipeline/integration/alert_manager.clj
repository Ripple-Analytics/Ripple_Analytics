(ns mental-models.pipeline.integration.alert-manager
  "Alert Manager Module
   
   Manages and routes alerts from the analysis pipeline:
   - Alert creation and classification
   - Severity levels and escalation
   - Alert routing to appropriate channels
   - Alert acknowledgment and resolution
   - Alert history and analytics"
  (:require
   [mental-models.pipeline.integration.notifications :as notif]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.core.async :as async :refer [go go-loop chan <! >! close!]]))

;; =============================================================================
;; ALERT CONFIGURATION
;; =============================================================================

(def severity-levels
  {:critical {:priority 1 :color "#ff0000" :escalation-minutes 5}
   :high {:priority 2 :color "#ff6600" :escalation-minutes 15}
   :medium {:priority 3 :color "#ffcc00" :escalation-minutes 60}
   :low {:priority 4 :color "#00cc00" :escalation-minutes 240}
   :info {:priority 5 :color "#0066ff" :escalation-minutes nil}})

(def alert-types
  {:lollapalooza {:default-severity :critical
                  :channels [:slack :email :desktop :websocket]}
   :high-confidence {:default-severity :high
                     :channels [:slack :websocket]}
   :batch-failure {:default-severity :high
                   :channels [:slack :email]}
   :system-error {:default-severity :critical
                  :channels [:slack :email :desktop]}
   :threshold-breach {:default-severity :medium
                      :channels [:slack :websocket]}
   :analysis-complete {:default-severity :info
                       :channels [:websocket]}})

;; =============================================================================
;; ALERT STATE
;; =============================================================================

(defonce alert-state (atom {:active-alerts {}
                            :alert-history []
                            :suppressed-alerts #{}
                            :escalation-rules {}
                            :total-alerts 0}))

;; =============================================================================
;; ALERT CREATION
;; =============================================================================

(defn create-alert
  "Create a new alert."
  [alert-type message & {:keys [severity data source]
                         :or {severity nil data {} source "pipeline"}}]
  (let [type-config (get alert-types alert-type {:default-severity :medium :channels [:slack]})
        actual-severity (or severity (:default-severity type-config))]
    {:id (str (java.util.UUID/randomUUID))
     :type alert-type
     :message message
     :severity actual-severity
     :priority (get-in severity-levels [actual-severity :priority] 5)
     :channels (:channels type-config)
     :data data
     :source source
     :status :active
     :created-at (System/currentTimeMillis)
     :acknowledged-at nil
     :resolved-at nil
     :acknowledged-by nil
     :escalated false}))

;; =============================================================================
;; ALERT MANAGEMENT
;; =============================================================================

(defn register-alert!
  "Register a new alert in the system."
  [alert]
  (swap! alert-state update :active-alerts assoc (:id alert) alert)
  (swap! alert-state update :total-alerts inc)
  (swap! alert-state update :alert-history conj
         (select-keys alert [:id :type :severity :message :created-at]))
  (metrics/inc-counter! :alerts/created)
  (metrics/inc-counter! (keyword "alerts" (name (:severity alert))))
  (events/publish! :alert/created alert)
  (log/info "Alert registered" {:alert-id (:id alert) :type (:type alert) :severity (:severity alert)})
  alert)

(defn get-alert [alert-id]
  (get-in @alert-state [:active-alerts alert-id]))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id & {:keys [acknowledged-by]}]
  (when-let [alert (get-alert alert-id)]
    (swap! alert-state update-in [:active-alerts alert-id] merge
           {:status :acknowledged
            :acknowledged-at (System/currentTimeMillis)
            :acknowledged-by acknowledged-by})
    (metrics/inc-counter! :alerts/acknowledged)
    (events/publish! :alert/acknowledged {:alert-id alert-id})
    (log/info "Alert acknowledged" {:alert-id alert-id :by acknowledged-by})))

(defn resolve-alert!
  "Resolve an alert."
  [alert-id & {:keys [resolution-note]}]
  (when-let [alert (get-alert alert-id)]
    (swap! alert-state update-in [:active-alerts alert-id] merge
           {:status :resolved
            :resolved-at (System/currentTimeMillis)
            :resolution-note resolution-note})
    ;; Move to history after a delay
    (go
      (<! (async/timeout 300000)) ; 5 minutes
      (swap! alert-state update :active-alerts dissoc alert-id))
    (metrics/inc-counter! :alerts/resolved)
    (events/publish! :alert/resolved {:alert-id alert-id})
    (log/info "Alert resolved" {:alert-id alert-id})))

(defn suppress-alert-type!
  "Suppress alerts of a specific type temporarily."
  [alert-type duration-ms]
  (swap! alert-state update :suppressed-alerts conj alert-type)
  (go
    (<! (async/timeout duration-ms))
    (swap! alert-state update :suppressed-alerts disj alert-type))
  (log/info "Alert type suppressed" {:type alert-type :duration-ms duration-ms}))

;; =============================================================================
;; ALERT ROUTING
;; =============================================================================

(defn route-alert!
  "Route an alert to appropriate channels."
  [alert]
  (when (and (flags/is-enabled? "alert-routing")
             (not (contains? (:suppressed-alerts @alert-state) (:type alert))))
    (log/debug "Routing alert" {:alert-id (:id alert) :channels (:channels alert)})
    (doseq [channel (:channels alert)]
      (case channel
        :slack (notif/send-slack-notification!
                {:title (str "[" (name (:severity alert)) "] " (name (:type alert)))
                 :message (:message alert)
                 :color (get-in severity-levels [(:severity alert) :color])})
        :email (notif/send-email-notification!
                {:subject (str "Alert: " (name (:type alert)))
                 :body (:message alert)
                 :priority (:priority alert)})
        :desktop (notif/send-desktop-notification!
                  {:title (name (:type alert))
                   :message (:message alert)})
        :websocket (notif/broadcast-to-websockets
                    {:type :alert
                     :alert alert})
        (log/warn "Unknown alert channel" {:channel channel})))))

;; =============================================================================
;; ALERT ESCALATION
;; =============================================================================

(defn check-escalation!
  "Check if an alert needs escalation."
  [alert]
  (when-let [escalation-minutes (get-in severity-levels [(:severity alert) :escalation-minutes])]
    (let [age-minutes (/ (- (System/currentTimeMillis) (:created-at alert)) 60000)]
      (when (and (> age-minutes escalation-minutes)
                 (= (:status alert) :active)
                 (not (:escalated alert)))
        (log/warn "Escalating alert" {:alert-id (:id alert) :age-minutes age-minutes})
        (swap! alert-state assoc-in [:active-alerts (:id alert) :escalated] true)
        (metrics/inc-counter! :alerts/escalated)
        (events/publish! :alert/escalated alert)
        ;; Send escalation notification
        (notif/send-slack-notification!
         {:title (str "ESCALATION: " (name (:type alert)))
          :message (str "Alert has been active for " (int age-minutes) " minutes: " (:message alert))
          :color "#ff0000"})))))

(defn start-escalation-monitor! []
  (go-loop []
    (when (flags/is-enabled? "alert-escalation")
      (<! (async/timeout 60000)) ; Check every minute
      (doseq [[_ alert] (:active-alerts @alert-state)]
        (check-escalation! alert))
      (recur))))

;; =============================================================================
;; ALERT QUERIES
;; =============================================================================

(defn get-active-alerts []
  (vals (:active-alerts @alert-state)))

(defn get-alerts-by-severity [severity]
  (filter #(= (:severity %) severity) (get-active-alerts)))

(defn get-alerts-by-type [alert-type]
  (filter #(= (:type %) alert-type) (get-active-alerts)))

(defn get-unacknowledged-alerts []
  (filter #(= (:status %) :active) (get-active-alerts)))

(defn get-alert-history
  "Get alert history."
  [& {:keys [limit] :or {limit 100}}]
  (take-last limit (:alert-history @alert-state)))

;; =============================================================================
;; CONVENIENCE FUNCTIONS
;; =============================================================================

(defn trigger-lollapalooza-alert!
  "Trigger a Lollapalooza alert."
  [lollapalooza-data]
  (let [alert (create-alert :lollapalooza
                            (str "Lollapalooza effect detected with "
                                 (:model-count lollapalooza-data) " models")
                            :data lollapalooza-data)]
    (register-alert! alert)
    (route-alert! alert)))

(defn trigger-system-error-alert!
  "Trigger a system error alert."
  [error-message & {:keys [data]}]
  (let [alert (create-alert :system-error error-message :data data)]
    (register-alert! alert)
    (route-alert! alert)))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-event-handlers! []
  (log/info "Setting up alert event handlers")
  (events/subscribe! :lollapalooza/detected trigger-lollapalooza-alert!)
  (events/subscribe! :batch/failed
                     (fn [data]
                       (let [alert (create-alert :batch-failure
                                                 (str "Batch job failed: " (:job-id data))
                                                 :data data)]
                         (register-alert! alert)
                         (route-alert! alert)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-alert-manager!
  "Initialize alert manager."
  []
  (log/info "Initializing alert manager")
  ;; Register feature flags
  (flags/register-flag! "alert-routing" "Enable alert routing" true)
  (flags/register-flag! "alert-escalation" "Enable alert escalation" true)
  ;; Create metrics
  (metrics/create-counter! :alerts/created "Alerts created")
  (metrics/create-counter! :alerts/acknowledged "Alerts acknowledged")
  (metrics/create-counter! :alerts/resolved "Alerts resolved")
  (metrics/create-counter! :alerts/escalated "Alerts escalated")
  (metrics/create-counter! :alerts/critical "Critical alerts")
  (metrics/create-counter! :alerts/high "High severity alerts")
  (metrics/create-counter! :alerts/medium "Medium severity alerts")
  (metrics/create-counter! :alerts/low "Low severity alerts")
  (metrics/create-gauge! :alerts/active "Active alerts" #(count (get-active-alerts)))
  ;; Setup event handlers
  (setup-event-handlers!)
  ;; Start escalation monitor
  (start-escalation-monitor!)
  (log/info "Alert manager initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-manager-status []
  {:enabled (flags/is-enabled? "alert-routing")
   :escalation-enabled (flags/is-enabled? "alert-escalation")
   :active-alerts (count (get-active-alerts))
   :unacknowledged (count (get-unacknowledged-alerts))
   :total-alerts (:total-alerts @alert-state)
   :suppressed-types (:suppressed-alerts @alert-state)})
