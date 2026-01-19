(ns mental-models.pipeline.integration.request-orchestrator
  "Request orchestrator for mental model analysis system.
   
   Features:
   - Request orchestration
   - Workflow management
   - Step execution
   - Parallel orchestration
   - Conditional flows
   - Error handling
   - Orchestration state
   - Orchestration metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent Executors ExecutorService CountDownLatch TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:workflows {}        ;; workflow-id -> workflow definition
         :executions {}       ;; execution-id -> execution state
         :config {:max-parallel 10
                  :default-timeout-ms 60000
                  :retry-count 3
                  :retry-delay-ms 1000}
         :stats {:workflows-executed 0
                 :steps-executed 0
                 :parallel-executions 0
                 :errors 0}
         :executor nil
         :initialized? false}))

;; ============================================================================
;; Workflow Definition
;; ============================================================================

(defn define-workflow!
  "Define a workflow."
  [workflow-id config]
  (let [workflow {:id workflow-id
                  :name (get config :name (name workflow-id))
                  :steps (get config :steps [])
                  :on-error (get config :on-error :stop)
                  :timeout-ms (get config :timeout-ms
                                   (get-in @state [:config :default-timeout-ms]))
                  :metadata (get config :metadata {})
                  :enabled? (atom true)
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:workflows workflow-id] workflow)
    (logging/log :info "Defined workflow" {:workflow-id workflow-id})
    workflow-id))

(defn get-workflow
  "Get a workflow."
  [workflow-id]
  (get-in @state [:workflows workflow-id]))

(defn list-workflows
  "List all workflows."
  []
  (mapv (fn [[id w]]
          {:id id
           :name (:name w)
           :steps-count (count (:steps w))
           :enabled? @(:enabled? w)})
        (:workflows @state)))

(defn delete-workflow!
  "Delete a workflow."
  [workflow-id]
  (swap! state update :workflows dissoc workflow-id))

;; ============================================================================
;; Step Types
;; ============================================================================

(defmulti execute-step
  "Execute a workflow step."
  (fn [step context] (:type step)))

(defmethod execute-step :handler
  [step context]
  (let [handler (:handler step)
        input (if (:input-fn step)
                ((:input-fn step) context)
                (:input context))]
    (handler input)))

(defmethod execute-step :transform
  [step context]
  (let [transform-fn (:transform-fn step)]
    (transform-fn (:result context))))

(defmethod execute-step :condition
  [step context]
  (let [condition-fn (:condition-fn step)
        then-step (:then step)
        else-step (:else step)]
    (if (condition-fn context)
      (when then-step (execute-step then-step context))
      (when else-step (execute-step else-step context)))))

(defmethod execute-step :parallel
  [step context]
  (let [steps (:steps step)
        executor (or (:executor @state)
                     (Executors/newFixedThreadPool
                      (get-in @state [:config :max-parallel])))
        results (atom [])
        latch (CountDownLatch. (count steps))]
    
    (swap! state update-in [:stats :parallel-executions] inc)
    
    (doseq [s steps]
      (.submit executor
               (fn []
                 (try
                   (let [result (execute-step s context)]
                     (swap! results conj {:step (:id s) :result result :success true}))
                   (catch Exception e
                     (swap! results conj {:step (:id s) :error (.getMessage e) :success false}))
                   (finally
                     (.countDown latch))))))
    
    (.await latch (get-in @state [:config :default-timeout-ms]) TimeUnit/MILLISECONDS)
    @results))

(defmethod execute-step :sequence
  [step context]
  (reduce (fn [ctx s]
            (let [result (execute-step s ctx)]
              (assoc ctx :result result :previous-results (conj (get ctx :previous-results []) result))))
          context
          (:steps step)))

(defmethod execute-step :retry
  [step context]
  (let [inner-step (:step step)
        max-retries (get step :retries (get-in @state [:config :retry-count]))
        delay-ms (get step :delay-ms (get-in @state [:config :retry-delay-ms]))]
    (loop [attempt 0]
      (let [result (try
                     {:success true :result (execute-step inner-step context)}
                     (catch Exception e
                       {:success false :error (.getMessage e)}))]
        (if (or (:success result) (>= attempt max-retries))
          result
          (do
            (Thread/sleep delay-ms)
            (recur (inc attempt))))))))

(defmethod execute-step :delay
  [step context]
  (Thread/sleep (:delay-ms step))
  (:result context))

(defmethod execute-step :default
  [step context]
  (logging/log :warn "Unknown step type" {:type (:type step)})
  nil)

;; ============================================================================
;; Workflow Execution
;; ============================================================================

(defn create-execution
  "Create a workflow execution."
  [workflow-id input]
  (let [execution-id (str (UUID/randomUUID))
        execution {:id execution-id
                   :workflow-id workflow-id
                   :input input
                   :state :created
                   :current-step (atom 0)
                   :results (atom [])
                   :errors (atom [])
                   :started-at (atom nil)
                   :completed-at (atom nil)
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:executions execution-id] execution)
    execution-id))

(defn get-execution
  "Get an execution."
  [execution-id]
  (get-in @state [:executions execution-id]))

(defn list-executions
  "List all executions."
  [& {:keys [workflow-id state-filter limit] :or {limit 100}}]
  (cond->> (vals (:executions @state))
    workflow-id (filter #(= (:workflow-id %) workflow-id))
    state-filter (filter #(= (:state %) state-filter))
    true (take-last limit)
    true (mapv (fn [e]
                 {:id (:id e)
                  :workflow-id (:workflow-id e)
                  :state (:state e)
                  :steps-completed (count @(:results e))
                  :errors-count (count @(:errors e))}))))

(defn execute-workflow
  "Execute a workflow."
  [workflow-id input]
  (when-let [workflow (get-workflow workflow-id)]
    (when @(:enabled? workflow)
      (swap! state update-in [:stats :workflows-executed] inc)
      
      (let [execution-id (create-execution workflow-id input)
            execution (get-execution execution-id)]
        
        (reset! (:started-at execution) (System/currentTimeMillis))
        (swap! state assoc-in [:executions execution-id :state] :running)
        
        (try
          (let [context {:input input
                         :workflow-id workflow-id
                         :execution-id execution-id
                         :result nil
                         :previous-results []}
                final-context (reduce
                               (fn [ctx step]
                                 (swap! (:current-step execution) inc)
                                 (swap! state update-in [:stats :steps-executed] inc)
                                 
                                 (try
                                   (let [result (execute-step step ctx)]
                                     (swap! (:results execution) conj
                                            {:step-id (:id step)
                                             :result result
                                             :timestamp (System/currentTimeMillis)})
                                     (assoc ctx
                                            :result result
                                            :previous-results (conj (:previous-results ctx) result)))
                                   (catch Exception e
                                     (swap! (:errors execution) conj
                                            {:step-id (:id step)
                                             :error (.getMessage e)
                                             :timestamp (System/currentTimeMillis)})
                                     (swap! state update-in [:stats :errors] inc)
                                     
                                     (case (:on-error workflow)
                                       :stop (throw e)
                                       :continue ctx
                                       :skip (assoc ctx :result nil)
                                       (throw e)))))
                               context
                               (:steps workflow))]
            
            (reset! (:completed-at execution) (System/currentTimeMillis))
            (swap! state assoc-in [:executions execution-id :state] :completed)
            
            {:execution-id execution-id
             :success true
             :result (:result final-context)
             :results @(:results execution)
             :duration-ms (- @(:completed-at execution) @(:started-at execution))})
          
          (catch Exception e
            (reset! (:completed-at execution) (System/currentTimeMillis))
            (swap! state assoc-in [:executions execution-id :state] :failed)
            
            {:execution-id execution-id
             :success false
             :error (.getMessage e)
             :results @(:results execution)
             :errors @(:errors execution)
             :duration-ms (- @(:completed-at execution) @(:started-at execution))}))))))

(defn execute-workflow-async
  "Execute a workflow asynchronously."
  [workflow-id input]
  (let [result-chan (chan)]
    (go
      (let [result (execute-workflow workflow-id input)]
        (>! result-chan result)
        (close! result-chan)))
    result-chan))

;; ============================================================================
;; Workflow Builders
;; ============================================================================

(defn handler-step
  "Create a handler step."
  [id handler & {:keys [input-fn]}]
  {:id id :type :handler :handler handler :input-fn input-fn})

(defn transform-step
  "Create a transform step."
  [id transform-fn]
  {:id id :type :transform :transform-fn transform-fn})

(defn condition-step
  "Create a condition step."
  [id condition-fn then-step else-step]
  {:id id :type :condition :condition-fn condition-fn :then then-step :else else-step})

(defn parallel-step
  "Create a parallel step."
  [id steps]
  {:id id :type :parallel :steps steps})

(defn sequence-step
  "Create a sequence step."
  [id steps]
  {:id id :type :sequence :steps steps})

(defn retry-step
  "Create a retry step."
  [id step & {:keys [retries delay-ms]}]
  {:id id :type :retry :step step :retries retries :delay-ms delay-ms})

(defn delay-step
  "Create a delay step."
  [id delay-ms]
  {:id id :type :delay :delay-ms delay-ms})

;; ============================================================================
;; Orchestration Patterns
;; ============================================================================

(defn scatter-gather
  "Execute scatter-gather pattern."
  [handlers input & {:keys [timeout-ms] :or {timeout-ms 30000}}]
  (let [results (atom [])
        executor (or (:executor @state)
                     (Executors/newFixedThreadPool (count handlers)))
        latch (CountDownLatch. (count handlers))]
    
    (doseq [[idx handler] (map-indexed vector handlers)]
      (.submit executor
               (fn []
                 (try
                   (let [result (handler input)]
                     (swap! results conj {:index idx :result result :success true}))
                   (catch Exception e
                     (swap! results conj {:index idx :error (.getMessage e) :success false}))
                   (finally
                     (.countDown latch))))))
    
    (.await latch timeout-ms TimeUnit/MILLISECONDS)
    (sort-by :index @results)))

(defn saga
  "Execute saga pattern with compensations."
  [steps input]
  (let [completed (atom [])]
    (try
      (reduce (fn [ctx {:keys [action compensation]}]
                (let [result (action ctx)]
                  (swap! completed conj {:action action :compensation compensation :result result})
                  (assoc ctx :result result)))
              {:input input :result nil}
              steps)
      (catch Exception e
        ;; Compensate in reverse order
        (doseq [{:keys [compensation result]} (reverse @completed)]
          (when compensation
            (try
              (compensation result)
              (catch Exception _))))
        {:success false :error (.getMessage e) :compensated true}))))

(defn pipeline
  "Execute pipeline pattern."
  [handlers input]
  (reduce (fn [data handler]
            (handler data))
          input
          handlers))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-orchestrate
  "Ring middleware to orchestrate requests."
  [handler workflow-id]
  (fn [request]
    (let [result (execute-workflow workflow-id request)]
      (if (:success result)
        (:result result)
        (handler request)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-parallel!
  "Set maximum parallel executions."
  [max-parallel]
  (swap! state assoc-in [:config :max-parallel] max-parallel))

(defn set-default-timeout!
  "Set default timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :default-timeout-ms] timeout-ms))

(defn set-retry-count!
  "Set default retry count."
  [count]
  (swap! state assoc-in [:config :retry-count] count))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-orchestrator-metrics
  "Get orchestrator metrics."
  []
  (let [stats (:stats @state)]
    {:workflows-executed (:workflows-executed stats)
     :steps-executed (:steps-executed stats)
     :parallel-executions (:parallel-executions stats)
     :errors (:errors stats)
     :workflows-count (count (:workflows @state))
     :executions-count (count (:executions @state))
     :success-rate (if (pos? (:workflows-executed stats))
                     (/ (- (:workflows-executed stats) (:errors stats))
                        (:workflows-executed stats))
                     1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-orchestrator-stats
  "Get orchestrator statistics."
  []
  (merge (get-orchestrator-metrics)
         {:max-parallel (get-in @state [:config :max-parallel])
          :default-timeout-ms (get-in @state [:config :default-timeout-ms])
          :retry-count (get-in @state [:config :retry-count])}))

(defn reset-stats!
  "Reset orchestrator statistics."
  []
  (swap! state assoc :stats {:workflows-executed 0
                             :steps-executed 0
                             :parallel-executions 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-orchestrator!
  "Initialize the request orchestrator."
  []
  (when-not (:initialized? @state)
    (let [executor (Executors/newFixedThreadPool
                    (get-in @state [:config :max-parallel]))]
      (swap! state assoc :executor executor))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request orchestrator initialized")
    (events/emit! :request-orchestrator-initialized {})
    true))

(defn shutdown!
  "Shutdown the orchestrator."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor))
  (swap! state assoc :initialized? false))
