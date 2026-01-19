(ns mental-models.pipeline.integration.saga-orchestrator
  "Saga Orchestrator Module
   
   Distributed transaction management:
   - Saga definition
   - Step execution
   - Compensation handling
   - Rollback support
   - Saga state tracking"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; SAGA STATE
;; =============================================================================

(defonce saga-state (atom {:sagas {}
                           :instances {}
                           :step-handlers {}
                           :compensation-handlers {}
                           :config {:max-retries 3
                                    :retry-delay-ms 1000
                                    :timeout-ms 30000}}))

;; =============================================================================
;; SAGA DEFINITION
;; =============================================================================

(defn define-saga!
  "Define a saga with steps."
  [saga-id {:keys [steps on-complete on-failure]}]
  (log/info "Defining saga" {:id saga-id :steps (count steps)})
  (swap! saga-state assoc-in [:sagas saga-id]
         {:id saga-id
          :steps steps
          :on-complete on-complete
          :on-failure on-failure
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :saga/sagas-defined)
  saga-id)

(defn get-saga
  "Get a saga definition."
  [saga-id]
  (get-in @saga-state [:sagas saga-id]))

(defn list-sagas
  "List all saga definitions."
  []
  (keys (:sagas @saga-state)))

(defn delete-saga!
  "Delete a saga definition."
  [saga-id]
  (log/info "Deleting saga" {:id saga-id})
  (swap! saga-state update :sagas dissoc saga-id))

;; =============================================================================
;; STEP HANDLERS
;; =============================================================================

(defn register-step-handler!
  "Register a step handler."
  [step-type handler-fn]
  (log/info "Registering step handler" {:type step-type})
  (swap! saga-state assoc-in [:step-handlers step-type] handler-fn))

(defn register-compensation-handler!
  "Register a compensation handler."
  [step-type handler-fn]
  (log/info "Registering compensation handler" {:type step-type})
  (swap! saga-state assoc-in [:compensation-handlers step-type] handler-fn))

(defn get-step-handler
  "Get a step handler."
  [step-type]
  (get-in @saga-state [:step-handlers step-type]))

(defn get-compensation-handler
  "Get a compensation handler."
  [step-type]
  (get-in @saga-state [:compensation-handlers step-type]))

;; =============================================================================
;; SAGA INSTANCE
;; =============================================================================

(defn generate-instance-id
  "Generate a unique instance ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-instance
  "Create a saga instance."
  [saga-id context]
  (let [instance-id (generate-instance-id)]
    {:id instance-id
     :saga-id saga-id
     :context context
     :status :pending
     :current-step 0
     :completed-steps []
     :failed-step nil
     :error nil
     :started-at nil
     :completed-at nil
     :created-at (System/currentTimeMillis)}))

(defn get-instance
  "Get a saga instance."
  [instance-id]
  (get-in @saga-state [:instances instance-id]))

(defn update-instance!
  "Update a saga instance."
  [instance-id updates]
  (swap! saga-state update-in [:instances instance-id] merge updates))

(defn list-instances
  "List saga instances."
  [& {:keys [saga-id status]}]
  (let [instances (vals (:instances @saga-state))]
    (cond->> instances
      saga-id (filter #(= (:saga-id %) saga-id))
      status (filter #(= (:status %) status)))))

;; =============================================================================
;; STEP EXECUTION
;; =============================================================================

(defn execute-step
  "Execute a single saga step."
  [instance step]
  (let [step-type (:type step)
        handler (or (:handler step) (get-step-handler step-type))]
    (if handler
      (try
        (log/debug "Executing step" {:step step-type :instance (:id instance)})
        (let [result (handler (:context instance) step)]
          {:success true :result result})
        (catch Exception e
          (log/error "Step execution failed" {:step step-type :error (.getMessage e)})
          {:success false :error (.getMessage e)}))
      {:success false :error (str "No handler for step type: " step-type)})))

(defn compensate-step
  "Execute compensation for a step."
  [instance step]
  (let [step-type (:type step)
        handler (or (:compensation step) (get-compensation-handler step-type))]
    (when handler
      (try
        (log/debug "Compensating step" {:step step-type :instance (:id instance)})
        (handler (:context instance) step)
        {:success true}
        (catch Exception e
          (log/error "Compensation failed" {:step step-type :error (.getMessage e)})
          {:success false :error (.getMessage e)})))))

;; =============================================================================
;; SAGA EXECUTION
;; =============================================================================

(defn run-saga!
  "Run a saga instance."
  [instance-id]
  (when (flags/is-enabled? "saga-orchestrator")
    (let [instance (get-instance instance-id)
          saga (get-saga (:saga-id instance))
          steps (:steps saga)]
      (log/info "Running saga" {:instance instance-id :saga (:saga-id instance)})
      (update-instance! instance-id {:status :running :started-at (System/currentTimeMillis)})
      (metrics/inc-counter! :saga/sagas-started)
      (loop [step-idx (:current-step instance)
             context (:context instance)
             completed []]
        (if (>= step-idx (count steps))
          ;; All steps completed
          (do
            (log/info "Saga completed" {:instance instance-id})
            (update-instance! instance-id {:status :completed
                                           :completed-steps completed
                                           :context context
                                           :completed-at (System/currentTimeMillis)})
            (metrics/inc-counter! :saga/sagas-completed)
            (events/publish! :saga/completed {:instance-id instance-id})
            (when-let [on-complete (:on-complete saga)]
              (on-complete context))
            {:success true :context context})
          ;; Execute next step
          (let [step (nth steps step-idx)
                updated-instance (assoc instance :context context)
                result (execute-step updated-instance step)]
            (if (:success result)
              ;; Step succeeded, continue
              (do
                (update-instance! instance-id {:current-step (inc step-idx)
                                               :context (merge context (:result result))
                                               :completed-steps (conj completed step)})
                (recur (inc step-idx)
                       (merge context (:result result))
                       (conj completed step)))
              ;; Step failed, compensate
              (do
                (log/warn "Saga step failed, compensating" {:instance instance-id :step (:type step)})
                (update-instance! instance-id {:status :compensating
                                               :failed-step step
                                               :error (:error result)})
                (metrics/inc-counter! :saga/sagas-failed)
                ;; Compensate completed steps in reverse order
                (doseq [completed-step (reverse completed)]
                  (compensate-step updated-instance completed-step))
                (update-instance! instance-id {:status :failed
                                               :completed-at (System/currentTimeMillis)})
                (events/publish! :saga/failed {:instance-id instance-id :error (:error result)})
                (when-let [on-failure (:on-failure saga)]
                  (on-failure context (:error result)))
                {:success false :error (:error result)}))))))))

(defn start-saga!
  "Start a new saga."
  [saga-id context]
  (when (flags/is-enabled? "saga-orchestrator")
    (let [instance (create-instance saga-id context)
          instance-id (:id instance)]
      (log/info "Starting saga" {:saga saga-id :instance instance-id})
      (swap! saga-state assoc-in [:instances instance-id] instance)
      (run-saga! instance-id))))

;; =============================================================================
;; SAGA CONTROL
;; =============================================================================

(defn pause-saga!
  "Pause a running saga."
  [instance-id]
  (log/info "Pausing saga" {:instance instance-id})
  (update-instance! instance-id {:status :paused}))

(defn resume-saga!
  "Resume a paused saga."
  [instance-id]
  (log/info "Resuming saga" {:instance instance-id})
  (update-instance! instance-id {:status :running})
  (run-saga! instance-id))

(defn cancel-saga!
  "Cancel a saga and compensate."
  [instance-id]
  (log/info "Cancelling saga" {:instance instance-id})
  (let [instance (get-instance instance-id)]
    (update-instance! instance-id {:status :cancelling})
    ;; Compensate completed steps
    (doseq [step (reverse (:completed-steps instance))]
      (compensate-step instance step))
    (update-instance! instance-id {:status :cancelled
                                   :completed-at (System/currentTimeMillis)})
    (events/publish! :saga/cancelled {:instance-id instance-id})))

(defn retry-saga!
  "Retry a failed saga from the failed step."
  [instance-id]
  (log/info "Retrying saga" {:instance instance-id})
  (let [instance (get-instance instance-id)]
    (when (= (:status instance) :failed)
      (update-instance! instance-id {:status :running
                                     :error nil
                                     :failed-step nil})
      (run-saga! instance-id))))

;; =============================================================================
;; SAGA QUERIES
;; =============================================================================

(defn get-saga-status
  "Get the status of a saga instance."
  [instance-id]
  (when-let [instance (get-instance instance-id)]
    {:id instance-id
     :saga-id (:saga-id instance)
     :status (:status instance)
     :current-step (:current-step instance)
     :completed-steps (count (:completed-steps instance))
     :error (:error instance)
     :started-at (:started-at instance)
     :completed-at (:completed-at instance)}))

(defn pending-sagas
  "Get all pending saga instances."
  []
  (list-instances :status :pending))

(defn running-sagas
  "Get all running saga instances."
  []
  (list-instances :status :running))

(defn failed-sagas
  "Get all failed saga instances."
  []
  (list-instances :status :failed))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn cleanup-completed-sagas!
  "Clean up old completed saga instances."
  [& {:keys [max-age-ms] :or {max-age-ms 86400000}}]
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)
        to-remove (filter (fn [[_ instance]]
                            (and (#{:completed :failed :cancelled} (:status instance))
                                 (< (:completed-at instance) cutoff)))
                          (:instances @saga-state))]
    (doseq [[instance-id _] to-remove]
      (swap! saga-state update :instances dissoc instance-id))
    (count to-remove)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-saga-orchestrator!
  "Initialize saga orchestrator."
  []
  (log/info "Initializing saga orchestrator")
  ;; Register feature flag
  (flags/register-flag! "saga-orchestrator" "Enable saga orchestrator" true)
  ;; Create metrics
  (metrics/create-counter! :saga/sagas-defined "Sagas defined")
  (metrics/create-counter! :saga/sagas-started "Sagas started")
  (metrics/create-counter! :saga/sagas-completed "Sagas completed")
  (metrics/create-counter! :saga/sagas-failed "Sagas failed")
  (metrics/create-gauge! :saga/running-sagas "Running sagas"
                         #(count (running-sagas)))
  (metrics/create-gauge! :saga/pending-sagas "Pending sagas"
                         #(count (pending-sagas)))
  (log/info "Saga orchestrator initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-orchestrator-status []
  {:enabled (flags/is-enabled? "saga-orchestrator")
   :sagas (count (:sagas @saga-state))
   :instances (count (:instances @saga-state))
   :running (count (running-sagas))
   :pending (count (pending-sagas))
   :failed (count (failed-sagas))
   :step-handlers (count (:step-handlers @saga-state))
   :compensation-handlers (count (:compensation-handlers @saga-state))
   :config (:config @saga-state)})
