(ns mental-models.pipeline.integration.state-machine
  "State Machine Module
   
   Finite state machine for workflow management:
   - State definitions
   - Transition rules
   - Guards and actions
   - State persistence
   - Event-driven transitions"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; STATE MACHINE STATE
;; =============================================================================

(defonce machines (atom {}))
(defonce instances (atom {}))

;; =============================================================================
;; STATE MACHINE DEFINITION
;; =============================================================================

(defn define-machine
  "Define a state machine."
  [machine-id & {:keys [initial-state states transitions on-enter on-exit context]}]
  {:id machine-id
   :initial-state initial-state
   :states (set states)
   :transitions transitions
   :on-enter (or on-enter {})
   :on-exit (or on-exit {})
   :context (or context {})})

(defn register-machine!
  "Register a state machine definition."
  [machine]
  (log/info "Registering state machine" {:id (:id machine)})
  (swap! machines assoc (:id machine) machine)
  (:id machine))

(defn get-machine
  "Get a state machine definition."
  [machine-id]
  (get @machines machine-id))

;; =============================================================================
;; STATE MACHINE INSTANCE
;; =============================================================================

(defn create-instance!
  "Create an instance of a state machine."
  [machine-id & {:keys [instance-id context]
                 :or {instance-id (str (java.util.UUID/randomUUID))
                      context {}}}]
  (when-let [machine (get-machine machine-id)]
    (log/info "Creating state machine instance" {:machine-id machine-id :instance-id instance-id})
    (let [instance {:id instance-id
                    :machine-id machine-id
                    :current-state (:initial-state machine)
                    :context (merge (:context machine) context)
                    :history [{:state (:initial-state machine)
                               :timestamp (System/currentTimeMillis)
                               :event :init}]
                    :created-at (System/currentTimeMillis)}]
      (swap! instances assoc instance-id instance)
      ;; Execute on-enter for initial state
      (execute-on-enter! instance-id (:initial-state machine))
      ;; Record metrics
      (metrics/inc-counter! :state-machine/instances-created)
      ;; Publish event
      (events/publish! :state-machine/instance-created {:instance-id instance-id :machine-id machine-id})
      instance-id)))

(defn get-instance
  "Get a state machine instance."
  [instance-id]
  (get @instances instance-id))

(defn get-current-state
  "Get the current state of an instance."
  [instance-id]
  (:current-state (get-instance instance-id)))

(defn get-context
  "Get the context of an instance."
  [instance-id]
  (:context (get-instance instance-id)))

(defn update-context!
  "Update the context of an instance."
  [instance-id updates]
  (swap! instances update-in [instance-id :context] merge updates))

;; =============================================================================
;; TRANSITIONS
;; =============================================================================

(defn find-transition
  "Find a valid transition for an event."
  [machine current-state event]
  (let [transitions (:transitions machine)]
    (first (filter (fn [t]
                     (and (= (:from t) current-state)
                          (= (:event t) event)))
                   transitions))))

(defn check-guard
  "Check if a transition guard passes."
  [transition context]
  (if-let [guard (:guard transition)]
    (try
      (guard context)
      (catch Exception e
        (log/error "Guard check failed" {:error (.getMessage e)})
        false))
    true))

(defn execute-action!
  "Execute a transition action."
  [action context]
  (when action
    (try
      (action context)
      (catch Exception e
        (log/error "Action execution failed" {:error (.getMessage e)})))))

(defn execute-on-exit!
  "Execute on-exit handler for a state."
  [instance-id state]
  (let [instance (get-instance instance-id)
        machine (get-machine (:machine-id instance))
        handler (get-in machine [:on-exit state])]
    (when handler
      (log/debug "Executing on-exit" {:instance-id instance-id :state state})
      (execute-action! handler (:context instance)))))

(defn execute-on-enter!
  "Execute on-enter handler for a state."
  [instance-id state]
  (let [instance (get-instance instance-id)
        machine (get-machine (:machine-id instance))
        handler (get-in machine [:on-enter state])]
    (when handler
      (log/debug "Executing on-enter" {:instance-id instance-id :state state})
      (execute-action! handler (:context instance)))))

(defn transition!
  "Attempt to transition an instance to a new state."
  [instance-id event & {:keys [context-updates] :or {context-updates {}}}]
  (when (flags/is-enabled? "state-machine")
    (let [instance (get-instance instance-id)
          machine (get-machine (:machine-id instance))
          current-state (:current-state instance)
          transition (find-transition machine current-state event)]
      (cond
        (nil? instance)
        (do
          (log/warn "Instance not found" {:instance-id instance-id})
          {:success false :error :instance-not-found})
        
        (nil? transition)
        (do
          (log/warn "No valid transition" {:instance-id instance-id :state current-state :event event})
          {:success false :error :no-valid-transition})
        
        (not (check-guard transition (merge (:context instance) context-updates)))
        (do
          (log/warn "Guard check failed" {:instance-id instance-id :transition transition})
          {:success false :error :guard-failed})
        
        :else
        (let [new-state (:to transition)]
          (log/info "Transitioning" {:instance-id instance-id :from current-state :to new-state :event event})
          ;; Execute on-exit
          (execute-on-exit! instance-id current-state)
          ;; Update context
          (when (seq context-updates)
            (update-context! instance-id context-updates))
          ;; Execute transition action
          (execute-action! (:action transition) (get-context instance-id))
          ;; Update state
          (swap! instances update instance-id
                 (fn [inst]
                   (-> inst
                       (assoc :current-state new-state)
                       (update :history conj {:state new-state
                                              :timestamp (System/currentTimeMillis)
                                              :event event
                                              :from current-state}))))
          ;; Execute on-enter
          (execute-on-enter! instance-id new-state)
          ;; Record metrics
          (metrics/inc-counter! :state-machine/transitions)
          ;; Publish event
          (events/publish! :state-machine/transitioned
                           {:instance-id instance-id :from current-state :to new-state :event event})
          {:success true :from current-state :to new-state})))))

;; =============================================================================
;; QUERY FUNCTIONS
;; =============================================================================

(defn can-transition?
  "Check if a transition is possible."
  [instance-id event]
  (let [instance (get-instance instance-id)
        machine (get-machine (:machine-id instance))
        transition (find-transition machine (:current-state instance) event)]
    (and transition (check-guard transition (:context instance)))))

(defn get-available-events
  "Get available events for the current state."
  [instance-id]
  (let [instance (get-instance instance-id)
        machine (get-machine (:machine-id instance))
        current-state (:current-state instance)]
    (->> (:transitions machine)
         (filter #(= (:from %) current-state))
         (filter #(check-guard % (:context instance)))
         (map :event))))

(defn get-history
  "Get the state history of an instance."
  [instance-id]
  (:history (get-instance instance-id)))

(defn is-in-state?
  "Check if an instance is in a specific state."
  [instance-id state]
  (= (get-current-state instance-id) state))

;; =============================================================================
;; PREDEFINED MACHINES
;; =============================================================================

(def analysis-workflow-machine
  (define-machine :analysis-workflow
    :initial-state :pending
    :states [:pending :validating :analyzing :detecting :formatting :complete :failed]
    :transitions [{:from :pending :event :start :to :validating}
                  {:from :validating :event :valid :to :analyzing}
                  {:from :validating :event :invalid :to :failed}
                  {:from :analyzing :event :analyzed :to :detecting}
                  {:from :analyzing :event :error :to :failed}
                  {:from :detecting :event :detected :to :formatting}
                  {:from :detecting :event :no-models :to :complete}
                  {:from :formatting :event :formatted :to :complete}
                  {:from :formatting :event :error :to :failed}
                  {:from :failed :event :retry :to :pending}]))

(def batch-job-machine
  (define-machine :batch-job
    :initial-state :created
    :states [:created :queued :running :paused :completed :failed :cancelled]
    :transitions [{:from :created :event :submit :to :queued}
                  {:from :queued :event :start :to :running}
                  {:from :running :event :pause :to :paused}
                  {:from :running :event :complete :to :completed}
                  {:from :running :event :fail :to :failed}
                  {:from :running :event :cancel :to :cancelled}
                  {:from :paused :event :resume :to :running}
                  {:from :paused :event :cancel :to :cancelled}
                  {:from :failed :event :retry :to :queued}]))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn delete-instance!
  "Delete a state machine instance."
  [instance-id]
  (log/info "Deleting instance" {:instance-id instance-id})
  (swap! instances dissoc instance-id))

(defn cleanup-completed!
  "Clean up completed instances older than max-age."
  [& {:keys [max-age-ms] :or {max-age-ms 3600000}}]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        to-remove (filter (fn [[id inst]]
                            (and (#{:complete :completed :failed :cancelled} (:current-state inst))
                                 (< (:created-at inst) cutoff)))
                          @instances)]
    (doseq [[id _] to-remove]
      (delete-instance! id))
    (count to-remove)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-state-machine!
  "Initialize state machine system."
  []
  (log/info "Initializing state machine")
  ;; Register feature flag
  (flags/register-flag! "state-machine" "Enable state machine" true)
  ;; Create metrics
  (metrics/create-counter! :state-machine/instances-created "Instances created")
  (metrics/create-counter! :state-machine/transitions "Transitions executed")
  (metrics/create-gauge! :state-machine/active-instances "Active instances"
                         #(count @instances))
  ;; Register predefined machines
  (register-machine! analysis-workflow-machine)
  (register-machine! batch-job-machine)
  (log/info "State machine initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-machine-status []
  {:enabled (flags/is-enabled? "state-machine")
   :registered-machines (count @machines)
   :active-instances (count @instances)
   :machines (keys @machines)})
