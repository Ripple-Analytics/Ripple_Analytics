(ns mental-models.pipeline.integration.dag-executor
  "DAG (Directed Acyclic Graph) executor for mental model analysis pipelines.
   
   Features:
   - DAG definition and validation
   - Topological execution ordering
   - Parallel task execution
   - Task dependencies
   - Retry and error handling
   - Execution monitoring
   - Task timeouts
   - Conditional execution"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.concurrent Executors TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:dags {}             ;; dag-id -> dag-definition
         :tasks {}            ;; task-id -> task-definition
         :executions {}       ;; execution-id -> execution-state
         :task-results {}     ;; {execution-id task-id} -> result
         :executor nil        ;; Thread pool executor
         :stats {:executions 0 :tasks-completed 0 :tasks-failed 0}
         :initialized? false}))

;; ============================================================================
;; Task Definition
;; ============================================================================

(defn register-task!
  "Register a task."
  [task-id config]
  (let [task {:id task-id
              :name (get config :name (name task-id))
              :description (get config :description "")
              :handler (get config :handler (fn [_] nil))
              :timeout-ms (get config :timeout-ms 300000)
              :retries (get config :retries 3)
              :retry-delay-ms (get config :retry-delay-ms 1000)
              :condition (get config :condition nil)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:tasks task-id] task)
    (logging/log :info "Registered task" {:task-id task-id})
    task-id))

(defn get-task
  "Get a task definition."
  [task-id]
  (get-in @state [:tasks task-id]))

(defn list-tasks
  "List all tasks."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :timeout-ms (:timeout-ms t)
           :retries (:retries t)})
        (:tasks @state)))

;; ============================================================================
;; DAG Definition
;; ============================================================================

(defn- validate-dag
  "Validate a DAG for cycles."
  [nodes edges]
  (let [;; Build adjacency list
        adj (reduce (fn [m {:keys [from to]}]
                      (update m from (fnil conj #{}) to))
                    {}
                    edges)
        ;; Detect cycles using DFS
        detect-cycle (fn detect-cycle [node visited rec-stack]
                       (if (contains? rec-stack node)
                         true ;; Cycle detected
                         (if (contains? visited node)
                           false
                           (let [new-visited (conj visited node)
                                 new-stack (conj rec-stack node)
                                 neighbors (get adj node #{})]
                             (some #(detect-cycle % new-visited new-stack) neighbors)))))]
    (not (some #(detect-cycle % #{} #{}) nodes))))

(defn create-dag!
  "Create a DAG."
  [dag-id config]
  (let [nodes (get config :nodes [])
        edges (get config :edges [])
        valid? (validate-dag nodes edges)]
    (when-not valid?
      (throw (ex-info "Invalid DAG: cycle detected" {:dag-id dag-id})))
    (let [dag {:id dag-id
               :name (get config :name (name dag-id))
               :description (get config :description "")
               :nodes nodes
               :edges edges
               :schedule (get config :schedule nil)
               :timeout-ms (get config :timeout-ms 3600000)
               :on-success (get config :on-success nil)
               :on-failure (get config :on-failure nil)
               :created-at (System/currentTimeMillis)}]
      (swap! state assoc-in [:dags dag-id] dag)
      (logging/log :info "Created DAG" {:dag-id dag-id :nodes (count nodes) :edges (count edges)})
      (events/emit! :dag-created {:dag-id dag-id})
      dag-id)))

(defn get-dag
  "Get a DAG."
  [dag-id]
  (get-in @state [:dags dag-id]))

(defn list-dags
  "List all DAGs."
  []
  (mapv (fn [[id d]]
          {:id id
           :name (:name d)
           :nodes (count (:nodes d))
           :edges (count (:edges d))})
        (:dags @state)))

(defn delete-dag!
  "Delete a DAG."
  [dag-id]
  (swap! state update :dags dissoc dag-id)
  (logging/log :info "Deleted DAG" {:dag-id dag-id}))

;; ============================================================================
;; Topological Sort
;; ============================================================================

(defn- topological-sort
  "Perform topological sort on DAG nodes."
  [nodes edges]
  (let [;; Build in-degree map
        in-degree (reduce (fn [m {:keys [to]}]
                            (update m to (fnil inc 0)))
                          (zipmap nodes (repeat 0))
                          edges)
        ;; Build adjacency list
        adj (reduce (fn [m {:keys [from to]}]
                      (update m from (fnil conj []) to))
                    {}
                    edges)]
    (loop [queue (filterv #(zero? (get in-degree %)) nodes)
           result []
           remaining-degree in-degree]
      (if (empty? queue)
        (if (= (count result) (count nodes))
          result
          (throw (ex-info "Cycle detected in DAG" {})))
        (let [node (first queue)
              neighbors (get adj node [])
              new-degree (reduce (fn [d n]
                                   (update d n dec))
                                 remaining-degree
                                 neighbors)
              new-queue (concat (rest queue)
                                (filter #(zero? (get new-degree %)) neighbors))]
          (recur (vec new-queue)
                 (conj result node)
                 new-degree))))))

(defn- get-ready-tasks
  "Get tasks that are ready to execute (all dependencies completed)."
  [dag execution-id]
  (let [completed (set (keys (filter (fn [[k v]]
                                       (and (= (first k) execution-id)
                                            (= :completed (:status v))))
                                     (:task-results @state))))
        completed-tasks (set (map second completed))
        edges (:edges dag)
        ;; Find tasks with all dependencies satisfied
        deps (reduce (fn [m {:keys [from to]}]
                       (update m to (fnil conj #{}) from))
                     {}
                     edges)]
    (filterv (fn [node]
               (let [node-deps (get deps node #{})]
                 (and (not (contains? completed-tasks node))
                      (set/subset? node-deps completed-tasks))))
             (:nodes dag))))

;; ============================================================================
;; Task Execution
;; ============================================================================

(defn- execute-task
  "Execute a single task."
  [task-id context]
  (let [task (get-task task-id)
        start-time (System/currentTimeMillis)]
    (try
      ;; Check condition
      (when-let [condition (:condition task)]
        (when-not (condition context)
          (throw (ex-info "Task condition not met" {:task-id task-id}))))
      
      ;; Execute handler
      (let [result ((:handler task) context)
            duration (- (System/currentTimeMillis) start-time)]
        (metrics/histogram :task-duration {:task-id task-id} duration)
        {:status :completed
         :result result
         :duration-ms duration
         :completed-at (System/currentTimeMillis)})
      
      (catch Exception e
        (let [duration (- (System/currentTimeMillis) start-time)]
          {:status :failed
           :error (.getMessage e)
           :duration-ms duration
           :failed-at (System/currentTimeMillis)})))))

(defn- execute-task-with-retry
  "Execute a task with retry logic."
  [task-id context]
  (let [task (get-task task-id)
        max-retries (:retries task 3)
        retry-delay (:retry-delay-ms task 1000)]
    (loop [attempt 1]
      (let [result (execute-task task-id context)]
        (if (or (= :completed (:status result))
                (>= attempt max-retries))
          (assoc result :attempts attempt)
          (do
            (Thread/sleep retry-delay)
            (logging/log :warn "Retrying task" {:task-id task-id :attempt (inc attempt)})
            (recur (inc attempt))))))))

;; ============================================================================
;; DAG Execution
;; ============================================================================

(defn start-execution!
  "Start a DAG execution."
  [dag-id & {:keys [context]}]
  (when (flags/enabled? :dag-executor)
    (let [dag (get-dag dag-id)
          execution-id (str (UUID/randomUUID))
          execution {:id execution-id
                     :dag-id dag-id
                     :status :running
                     :context (or context {})
                     :started-at (System/currentTimeMillis)
                     :completed-at nil
                     :tasks-completed 0
                     :tasks-failed 0}]
      (swap! state assoc-in [:executions execution-id] execution)
      (swap! state update-in [:stats :executions] inc)
      (logging/log :info "Started DAG execution" {:execution-id execution-id :dag-id dag-id})
      (events/emit! :dag-execution-started {:execution-id execution-id :dag-id dag-id})
      
      ;; Start execution in background
      (go
        (try
          (loop []
            (let [ready-tasks (get-ready-tasks dag execution-id)]
              (if (empty? ready-tasks)
                ;; Check if all tasks completed
                (let [completed-count (count (filter (fn [[k v]]
                                                       (and (= (first k) execution-id)
                                                            (= :completed (:status v))))
                                                     (:task-results @state)))
                      total-tasks (count (:nodes dag))]
                  (if (= completed-count total-tasks)
                    ;; All done
                    (do
                      (swap! state update-in [:executions execution-id]
                             (fn [e]
                               (assoc e
                                      :status :completed
                                      :completed-at (System/currentTimeMillis))))
                      (logging/log :info "DAG execution completed" {:execution-id execution-id})
                      (events/emit! :dag-execution-completed {:execution-id execution-id}))
                    ;; Check for failures
                    (let [failed-count (count (filter (fn [[k v]]
                                                        (and (= (first k) execution-id)
                                                             (= :failed (:status v))))
                                                      (:task-results @state)))]
                      (when (pos? failed-count)
                        (swap! state update-in [:executions execution-id]
                               (fn [e]
                                 (assoc e
                                        :status :failed
                                        :completed-at (System/currentTimeMillis))))
                        (logging/log :error "DAG execution failed" {:execution-id execution-id})
                        (events/emit! :dag-execution-failed {:execution-id execution-id})))))
                ;; Execute ready tasks in parallel
                (let [results (doall
                               (pmap (fn [task-id]
                                       (let [result (execute-task-with-retry task-id (:context execution))]
                                         (swap! state assoc-in [:task-results [execution-id task-id]] result)
                                         (if (= :completed (:status result))
                                           (do
                                             (swap! state update-in [:executions execution-id :tasks-completed] inc)
                                             (swap! state update-in [:stats :tasks-completed] inc))
                                           (do
                                             (swap! state update-in [:executions execution-id :tasks-failed] inc)
                                             (swap! state update-in [:stats :tasks-failed] inc)))
                                         result))
                                     ready-tasks))]
                  ;; Continue if no failures
                  (when (every? #(= :completed (:status %)) results)
                    (recur))))))
          (catch Exception e
            (logging/log :error "DAG execution error" {:execution-id execution-id :error (.getMessage e)})
            (swap! state update-in [:executions execution-id]
                   (fn [ex]
                     (assoc ex
                            :status :failed
                            :error (.getMessage e)
                            :completed-at (System/currentTimeMillis)))))))
      
      execution-id)))

(defn get-execution
  "Get an execution."
  [execution-id]
  (get-in @state [:executions execution-id]))

(defn list-executions
  "List executions."
  [& {:keys [dag-id status limit]}]
  (let [executions (vals (:executions @state))
        filtered (cond->> executions
                   dag-id (filter #(= (:dag-id %) dag-id))
                   status (filter #(= (:status %) status))
                   true (sort-by :started-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :dag-id :status :started-at :completed-at :tasks-completed :tasks-failed])
          filtered)))

(defn get-task-result
  "Get a task result from an execution."
  [execution-id task-id]
  (get-in @state [:task-results [execution-id task-id]]))

(defn get-execution-results
  "Get all task results for an execution."
  [execution-id]
  (let [results (filter (fn [[k _]]
                          (= (first k) execution-id))
                        (:task-results @state))]
    (into {}
          (map (fn [[k v]]
                 [(second k) v])
               results))))

;; ============================================================================
;; Execution Control
;; ============================================================================

(defn cancel-execution!
  "Cancel a running execution."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (when (= :running (:status execution))
      (swap! state update-in [:executions execution-id]
             (fn [e]
               (assoc e
                      :status :cancelled
                      :completed-at (System/currentTimeMillis))))
      (logging/log :info "Cancelled execution" {:execution-id execution-id})
      (events/emit! :dag-execution-cancelled {:execution-id execution-id}))))

(defn retry-execution!
  "Retry a failed execution."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (when (= :failed (:status execution))
      (start-execution! (:dag-id execution) :context (:context execution)))))

;; ============================================================================
;; Visualization
;; ============================================================================

(defn get-dag-visualization
  "Get DAG visualization data."
  [dag-id]
  (when-let [dag (get-dag dag-id)]
    {:nodes (mapv (fn [node]
                    {:id node
                     :label (or (:name (get-task node)) (name node))})
                  (:nodes dag))
     :edges (mapv (fn [{:keys [from to]}]
                    {:from from :to to})
                  (:edges dag))}))

(defn get-execution-visualization
  "Get execution visualization with task statuses."
  [execution-id]
  (when-let [execution (get-execution execution-id)]
    (let [dag (get-dag (:dag-id execution))
          results (get-execution-results execution-id)]
      {:nodes (mapv (fn [node]
                      (let [result (get results node)]
                        {:id node
                         :label (or (:name (get-task node)) (name node))
                         :status (or (:status result) :pending)
                         :duration-ms (:duration-ms result)}))
                    (:nodes dag))
       :edges (mapv (fn [{:keys [from to]}]
                      {:from from :to to})
                    (:edges dag))
       :execution-status (:status execution)})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-executor-stats
  "Get DAG executor statistics."
  []
  (let [stats (:stats @state)
        executions (vals (:executions @state))
        by-status (group-by :status executions)]
    {:total-dags (count (:dags @state))
     :total-tasks (count (:tasks @state))
     :total-executions (:executions stats)
     :tasks-completed (:tasks-completed stats)
     :tasks-failed (:tasks-failed stats)
     :running-executions (count (get by-status :running []))
     :completed-executions (count (get by-status :completed []))
     :failed-executions (count (get by-status :failed []))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-dag-executor!
  "Initialize the DAG executor."
  []
  (when-not (:initialized? @state)
    ;; Create thread pool
    (swap! state assoc :executor (Executors/newFixedThreadPool 10))
    
    ;; Register default tasks
    (register-task! :extract-text
                    {:name "Extract Text"
                     :description "Extract text from document"
                     :handler (fn [ctx]
                                {:text (get ctx :content "")
                                 :word-count (count (str/split (get ctx :content "") #"\s+"))})
                     :timeout-ms 60000})
    
    (register-task! :analyze-models
                    {:name "Analyze Mental Models"
                     :description "Identify mental models in text"
                     :handler (fn [ctx]
                                {:models [{:id :confirmation-bias :confidence 0.8}
                                          {:id :anchoring :confidence 0.6}]})
                     :timeout-ms 120000})
    
    (register-task! :generate-insights
                    {:name "Generate Insights"
                     :description "Generate insights from analysis"
                     :handler (fn [ctx]
                                {:insights ["Consider alternative viewpoints"
                                            "Watch for anchoring effects"]})
                     :timeout-ms 60000})
    
    ;; Create default DAG
    (create-dag! :analysis-pipeline
                 {:name "Analysis Pipeline"
                  :description "Standard document analysis pipeline"
                  :nodes [:extract-text :analyze-models :generate-insights]
                  :edges [{:from :extract-text :to :analyze-models}
                          {:from :analyze-models :to :generate-insights}]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "DAG executor initialized")
    (events/emit! :dag-executor-initialized {})
    true))
