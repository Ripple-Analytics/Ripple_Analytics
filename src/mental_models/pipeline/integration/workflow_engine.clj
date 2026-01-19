(ns mental-models.pipeline.integration.workflow-engine
  "Workflow Engine Module
   
   Complex workflow orchestration:
   - Workflow definition
   - Step execution
   - Conditional branching
   - Parallel execution
   - Error handling and recovery"
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; WORKFLOW STATE
;; =============================================================================

(defonce workflow-state (atom {:definitions {}
                               :instances {}
                               :step-handlers {}
                               :config {:max-parallel 10
                                        :default-timeout-ms 300000
                                        :max-retries 3}}))

;; =============================================================================
;; WORKFLOW DEFINITION
;; =============================================================================

(defn define-workflow!
  "Define a workflow."
  [workflow-id {:keys [name description steps on-error on-complete]}]
  (log/info "Defining workflow" {:id workflow-id :steps (count steps)})
  (swap! workflow-state assoc-in [:definitions workflow-id]
         {:id workflow-id
          :name name
          :description description
          :steps steps
          :on-error on-error
          :on-complete on-complete
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :workflow/defined)
  workflow-id)

(defn get-workflow
  "Get a workflow definition."
  [workflow-id]
  (get-in @workflow-state [:definitions workflow-id]))

(defn list-workflows
  "List all workflow definitions."
  []
  (keys (:definitions @workflow-state)))

(defn delete-workflow!
  "Delete a workflow definition."
  [workflow-id]
  (swap! workflow-state update :definitions dissoc workflow-id))

;; =============================================================================
;; STEP HANDLERS
;; =============================================================================

(defn register-step-handler!
  "Register a step handler."
  [step-type handler-fn]
  (log/info "Registering step handler" {:type step-type})
  (swap! workflow-state assoc-in [:step-handlers step-type] handler-fn))

(defn get-step-handler
  "Get a step handler."
  [step-type]
  (get-in @workflow-state [:step-handlers step-type]))

;; =============================================================================
;; WORKFLOW INSTANCE
;; =============================================================================

(defn generate-instance-id
  "Generate a unique instance ID."
  []
  (str (java.util.UUID/randomUUID)))

(defn create-instance
  "Create a workflow instance."
  [workflow-id input]
  {:id (generate-instance-id)
   :workflow-id workflow-id
   :input input
   :context {}
   :status :pending
   :current-step nil
   :step-results {}
   :error nil
   :created-at (System/currentTimeMillis)
   :started-at nil
   :completed-at nil})

(defn get-instance
  "Get a workflow instance."
  [instance-id]
  (get-in @workflow-state [:instances instance-id]))

(defn update-instance!
  "Update a workflow instance."
  [instance-id updates]
  (swap! workflow-state update-in [:instances instance-id] merge updates))

(defn list-instances
  "List workflow instances."
  [& {:keys [workflow-id status limit]}]
  (let [instances (vals (:instances @workflow-state))
        filtered (cond->> instances
                   workflow-id (filter #(= (:workflow-id %) workflow-id))
                   status (filter #(= (:status %) status))
                   limit (take limit))]
    (sort-by :created-at > filtered)))

;; =============================================================================
;; STEP EXECUTION
;; =============================================================================

(defn execute-step
  "Execute a single workflow step."
  [instance step]
  (log/info "Executing step" {:instance (:id instance) :step (:id step)})
  (let [step-type (:type step)
        handler (get-step-handler step-type)]
    (if handler
      (try
        (let [result (handler {:step step
                               :input (:input instance)
                               :context (:context instance)
                               :step-results (:step-results instance)})]
          {:success true :result result})
        (catch Exception e
          (log/error "Step execution failed" {:step (:id step) :error (.getMessage e)})
          {:success false :error (.getMessage e)}))
      (do
        (log/error "No handler for step type" {:type step-type})
        {:success false :error (str "No handler for step type: " step-type)}))))

(defn evaluate-condition
  "Evaluate a step condition."
  [condition context step-results]
  (cond
    (nil? condition) true
    (fn? condition) (condition context step-results)
    (keyword? condition) (get step-results condition)
    :else true))

(defn get-next-steps
  "Get the next steps to execute."
  [workflow current-step-id step-result]
  (let [steps (:steps workflow)
        current-idx (if current-step-id
                      (inc (.indexOf (mapv :id steps) current-step-id))
                      0)]
    (when (< current-idx (count steps))
      (let [next-step (nth steps current-idx)]
        (if (:parallel next-step)
          ;; Return all parallel steps
          (take-while :parallel (drop current-idx steps))
          ;; Return single step
          [next-step])))))

;; =============================================================================
;; WORKFLOW EXECUTION
;; =============================================================================

(defn run-workflow!
  "Run a workflow instance."
  [instance-id]
  (when (flags/is-enabled? "workflow-engine")
    (let [instance (get-instance instance-id)
          workflow (get-workflow (:workflow-id instance))]
      (when (and instance workflow)
        (log/info "Running workflow" {:instance instance-id :workflow (:workflow-id instance)})
        (update-instance! instance-id {:status :running :started-at (System/currentTimeMillis)})
        (metrics/inc-counter! :workflow/started)
        (events/publish! :workflow/started {:instance instance-id})
        (go
          (loop [current-step-id nil
                 context (:context instance)
                 step-results (:step-results instance)]
            (let [next-steps (get-next-steps workflow current-step-id nil)]
              (if (empty? next-steps)
                ;; Workflow complete
                (do
                  (update-instance! instance-id {:status :completed
                                                 :completed-at (System/currentTimeMillis)
                                                 :context context
                                                 :step-results step-results})
                  (metrics/inc-counter! :workflow/completed)
                  (events/publish! :workflow/completed {:instance instance-id})
                  (when-let [on-complete (:on-complete workflow)]
                    (on-complete {:instance (get-instance instance-id)}))
                  {:success true :instance instance-id})
                ;; Execute next steps
                (let [results (if (> (count next-steps) 1)
                                ;; Parallel execution
                                (let [result-chan (chan (count next-steps))]
                                  (doseq [step next-steps]
                                    (go (>! result-chan
                                            [(:id step)
                                             (execute-step (assoc instance
                                                                  :context context
                                                                  :step-results step-results)
                                                           step)])))
                                  (loop [collected {}
                                         remaining (count next-steps)]
                                    (if (zero? remaining)
                                      collected
                                      (let [[step-id result] (<! result-chan)]
                                        (recur (assoc collected step-id result)
                                               (dec remaining))))))
                                ;; Sequential execution
                                (let [step (first next-steps)
                                      result (execute-step (assoc instance
                                                                  :context context
                                                                  :step-results step-results)
                                                           step)]
                                  {(:id step) result}))
                      ;; Check for errors
                      errors (filter (fn [[_ r]] (not (:success r))) results)]
                  (if (seq errors)
                    ;; Handle errors
                    (let [error-msg (str/join ", " (map (fn [[id r]] (str id ": " (:error r))) errors))]
                      (update-instance! instance-id {:status :failed
                                                     :error error-msg
                                                     :completed-at (System/currentTimeMillis)
                                                     :step-results (merge step-results results)})
                      (metrics/inc-counter! :workflow/failed)
                      (events/publish! :workflow/failed {:instance instance-id :error error-msg})
                      (when-let [on-error (:on-error workflow)]
                        (on-error {:instance (get-instance instance-id) :error error-msg}))
                      {:success false :error error-msg})
                    ;; Continue to next steps
                    (let [new-results (merge step-results
                                             (into {} (map (fn [[k v]] [k (:result v)]) results)))
                          new-context (reduce (fn [ctx [_ result]]
                                                (merge ctx (:context result)))
                                              context
                                              results)
                          last-step-id (:id (last next-steps))]
                      (update-instance! instance-id {:current-step last-step-id
                                                     :context new-context
                                                     :step-results new-results})
                      (recur last-step-id new-context new-results))))))))))))

(defn start-workflow!
  "Start a new workflow instance."
  [workflow-id input]
  (when-let [workflow (get-workflow workflow-id)]
    (let [instance (create-instance workflow-id input)]
      (swap! workflow-state assoc-in [:instances (:id instance)] instance)
      (run-workflow! (:id instance))
      (:id instance))))

;; =============================================================================
;; WORKFLOW CONTROL
;; =============================================================================

(defn pause-workflow!
  "Pause a running workflow."
  [instance-id]
  (update-instance! instance-id {:status :paused})
  (events/publish! :workflow/paused {:instance instance-id}))

(defn resume-workflow!
  "Resume a paused workflow."
  [instance-id]
  (let [instance (get-instance instance-id)]
    (when (= (:status instance) :paused)
      (run-workflow! instance-id))))

(defn cancel-workflow!
  "Cancel a workflow."
  [instance-id]
  (update-instance! instance-id {:status :cancelled :completed-at (System/currentTimeMillis)})
  (metrics/inc-counter! :workflow/cancelled)
  (events/publish! :workflow/cancelled {:instance instance-id}))

(defn retry-workflow!
  "Retry a failed workflow from the failed step."
  [instance-id]
  (let [instance (get-instance instance-id)]
    (when (= (:status instance) :failed)
      (update-instance! instance-id {:status :pending :error nil})
      (run-workflow! instance-id))))

;; =============================================================================
;; DEFAULT STEP HANDLERS
;; =============================================================================

(def default-handlers
  {:task (fn [{:keys [step context]}]
           (let [task-fn (:fn step)]
             (when task-fn
               (task-fn context))))
   
   :delay (fn [{:keys [step]}]
            (Thread/sleep (or (:duration-ms step) 1000))
            {:delayed true})
   
   :condition (fn [{:keys [step context step-results]}]
                (let [condition-fn (:condition step)]
                  {:result (condition-fn context step-results)}))
   
   :transform (fn [{:keys [step context]}]
                (let [transform-fn (:transform step)]
                  {:context (transform-fn context)}))
   
   :notify (fn [{:keys [step context]}]
             (events/publish! (:event step) (merge (:data step) context))
             {:notified true})
   
   :http (fn [{:keys [step]}]
           (log/info "HTTP step" {:url (:url step) :method (:method step)})
           {:http-called true})})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-workflow-engine!
  "Initialize workflow engine."
  []
  (log/info "Initializing workflow engine")
  ;; Register feature flag
  (flags/register-flag! "workflow-engine" "Enable workflow engine" true)
  ;; Create metrics
  (metrics/create-counter! :workflow/defined "Workflows defined")
  (metrics/create-counter! :workflow/started "Workflows started")
  (metrics/create-counter! :workflow/completed "Workflows completed")
  (metrics/create-counter! :workflow/failed "Workflows failed")
  (metrics/create-counter! :workflow/cancelled "Workflows cancelled")
  (metrics/create-gauge! :workflow/active "Active workflows"
                         #(count (filter #(= (:status %) :running)
                                         (vals (:instances @workflow-state)))))
  ;; Register default handlers
  (doseq [[step-type handler] default-handlers]
    (register-step-handler! step-type handler))
  (log/info "Workflow engine initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-engine-status []
  {:enabled (flags/is-enabled? "workflow-engine")
   :definitions (count (:definitions @workflow-state))
   :instances {:total (count (:instances @workflow-state))
               :running (count (filter #(= (:status %) :running)
                                       (vals (:instances @workflow-state))))
               :completed (count (filter #(= (:status %) :completed)
                                         (vals (:instances @workflow-state))))
               :failed (count (filter #(= (:status %) :failed)
                                      (vals (:instances @workflow-state))))}
   :step-handlers (count (:step-handlers @workflow-state))})
