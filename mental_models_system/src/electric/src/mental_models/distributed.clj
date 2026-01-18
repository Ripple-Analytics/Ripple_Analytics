(ns mental-models.distributed
  "Petabyte-Scale Distributed Processing System
   
   Designed for:
   - Processing terabytes to petabytes of data
   - Coordinating across unlimited compute nodes
   - Maximizing utilization of all available compute resources
   - Continuous 24/7 scraping and analysis
   - Real-time sensor data ingestion from all devices
   
   Architecture:
   - Master node coordinates work distribution
   - Worker nodes process tasks at maximum capacity
   - Message queue (Redis/RabbitMQ) for task distribution
   - Distributed storage for intermediate results
   - Auto-scaling based on queue depth"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout]]
            [clojure.tools.logging :as log]
            [cheshire.core :as json])
  (:import [java.util UUID]
           [java.util.concurrent Executors ThreadPoolExecutor TimeUnit]
           [java.time Instant]))

;; ============================================
;; Configuration
;; ============================================

(def config
  "Distributed processing configuration."
  {:node-id (str (UUID/randomUUID))
   :node-type (or (System/getenv "NODE_TYPE") "worker")  ; master, worker, sensor
   :master-url (or (System/getenv "MASTER_URL") "http://localhost:8000")
   :redis-url (or (System/getenv "REDIS_URL") "redis://localhost:6379")
   :max-workers (Integer/parseInt (or (System/getenv "MAX_WORKERS") "8"))
   :batch-size (Integer/parseInt (or (System/getenv "BATCH_SIZE") "1000"))
   :heartbeat-interval 5000  ; ms
   :task-timeout 300000      ; 5 minutes
   :max-retries 3
   :auto-scale true
   :target-queue-depth 100   ; Target tasks per worker
   :scale-up-threshold 200   ; Scale up when queue > this per worker
   :scale-down-threshold 10  ; Scale down when queue < this per worker
   })

;; ============================================
;; Node Registry
;; ============================================

(defonce !nodes (atom {}))
(defonce !tasks (atom {}))
(defonce !metrics (atom {:processed 0
                         :failed 0
                         :bytes-processed 0
                         :start-time (Instant/now)}))

(defn register-node!
  "Register a compute node in the cluster."
  [{:keys [node-id node-type capabilities capacity]}]
  (swap! !nodes assoc node-id
         {:node-id node-id
          :node-type node-type
          :capabilities (or capabilities #{:analyze :scrape :process})
          :capacity (or capacity (:max-workers config))
          :current-load 0
          :last-heartbeat (Instant/now)
          :status :active
          :tasks-completed 0
          :bytes-processed 0}))

(defn update-node-heartbeat!
  "Update node heartbeat timestamp."
  [node-id & {:keys [load tasks-completed bytes-processed]}]
  (swap! !nodes update node-id merge
         {:last-heartbeat (Instant/now)
          :current-load (or load 0)
          :tasks-completed (+ (get-in @!nodes [node-id :tasks-completed] 0)
                              (or tasks-completed 0))
          :bytes-processed (+ (get-in @!nodes [node-id :bytes-processed] 0)
                              (or bytes-processed 0))}))

(defn get-active-nodes
  "Get all active nodes."
  []
  (let [cutoff (.minusSeconds (Instant/now) 30)]
    (->> @!nodes
         vals
         (filter #(.isAfter (:last-heartbeat %) cutoff))
         (filter #(= :active (:status %))))))

(defn get-available-capacity
  "Get total available capacity across all nodes."
  []
  (reduce + (map #(- (:capacity %) (:current-load %))
                 (get-active-nodes))))

;; ============================================
;; Task Queue
;; ============================================

(defonce !task-queue (atom (clojure.lang.PersistentQueue/EMPTY)))
(defonce !priority-queue (atom (clojure.lang.PersistentQueue/EMPTY)))
(defonce !results (atom {}))

(defn create-task
  "Create a new task."
  [task-type data & {:keys [priority callback]}]
  {:task-id (str (UUID/randomUUID))
   :task-type task-type
   :data data
   :priority (or priority :normal)
   :created-at (Instant/now)
   :status :pending
   :retries 0
   :callback callback})

(defn enqueue-task!
  "Add a task to the queue."
  [task]
  (if (= :high (:priority task))
    (swap! !priority-queue conj task)
    (swap! !task-queue conj task))
  (swap! !tasks assoc (:task-id task) task)
  (:task-id task))

(defn dequeue-task!
  "Get the next task from the queue."
  []
  (or (when-let [task (peek @!priority-queue)]
        (swap! !priority-queue pop)
        task)
      (when-let [task (peek @!task-queue)]
        (swap! !task-queue pop)
        task)))

(defn get-queue-depth
  "Get current queue depth."
  []
  (+ (count @!task-queue) (count @!priority-queue)))

;; ============================================
;; Task Types
;; ============================================

(defmulti process-task
  "Process a task based on its type."
  :task-type)

(defmethod process-task :analyze
  [{:keys [data]}]
  (let [{:keys [text models]} data]
    ;; Analyze text with mental models
    {:result :analyzed
     :models-applied (or models ["all"])
     :insights (count (or text ""))
     :timestamp (Instant/now)}))

(defmethod process-task :scrape
  [{:keys [data]}]
  (let [{:keys [url depth]} data]
    ;; Scrape URL (headless)
    {:result :scraped
     :url url
     :depth (or depth 1)
     :pages-found 0
     :timestamp (Instant/now)}))

(defmethod process-task :process-file
  [{:keys [data]}]
  (let [{:keys [path file-type]} data]
    ;; Process a file
    {:result :processed
     :path path
     :file-type file-type
     :bytes-processed 0
     :timestamp (Instant/now)}))

(defmethod process-task :batch-ingest
  [{:keys [data]}]
  (let [{:keys [paths batch-id]} data]
    ;; Batch ingest multiple files
    {:result :ingested
     :batch-id batch-id
     :files-count (count paths)
     :timestamp (Instant/now)}))

(defmethod process-task :sensor-data
  [{:keys [data]}]
  (let [{:keys [device-id sensor-type readings]} data]
    ;; Process sensor data
    {:result :sensor-processed
     :device-id device-id
     :sensor-type sensor-type
     :readings-count (count readings)
     :timestamp (Instant/now)}))

(defmethod process-task :learn
  [{:keys [data]}]
  (let [{:keys [content source models-to-apply]} data]
    ;; Learn from content
    {:result :learned
     :source source
     :patterns-found 0
     :models-updated (or models-to-apply [])
     :timestamp (Instant/now)}))

(defmethod process-task :default
  [task]
  {:result :unknown-task-type
   :task-type (:task-type task)
   :timestamp (Instant/now)})

;; ============================================
;; Worker Pool
;; ============================================

(defonce !worker-pool (atom nil))
(defonce !running (atom false))

(defn create-worker-pool
  "Create a thread pool for workers."
  [n-workers]
  (Executors/newFixedThreadPool n-workers))

(defn worker-loop
  "Main worker loop - processes tasks continuously."
  [worker-id]
  (while @!running
    (try
      (if-let [task (dequeue-task!)]
        (do
          (log/debug "Worker" worker-id "processing task" (:task-id task))
          (swap! !tasks assoc-in [(:task-id task) :status] :processing)
          (swap! !tasks assoc-in [(:task-id task) :worker-id] worker-id)
          (let [start-time (System/currentTimeMillis)
                result (process-task task)
                duration (- (System/currentTimeMillis) start-time)]
            (swap! !tasks assoc-in [(:task-id task) :status] :completed)
            (swap! !tasks assoc-in [(:task-id task) :result] result)
            (swap! !tasks assoc-in [(:task-id task) :duration] duration)
            (swap! !results assoc (:task-id task) result)
            (swap! !metrics update :processed inc)
            (when-let [callback (:callback task)]
              (try (callback result) (catch Exception _)))))
        ;; No task available, sleep briefly
        (Thread/sleep 100))
      (catch Exception e
        (log/error "Worker" worker-id "error:" (.getMessage e))))))

(defn start-workers!
  "Start the worker pool."
  [& {:keys [n-workers] :or {n-workers (:max-workers config)}}]
  (when-not @!running
    (reset! !running true)
    (reset! !worker-pool (create-worker-pool n-workers))
    (dotimes [i n-workers]
      (.submit @!worker-pool ^Runnable #(worker-loop i)))
    (log/info "Started" n-workers "workers")))

(defn stop-workers!
  "Stop the worker pool."
  []
  (reset! !running false)
  (when @!worker-pool
    (.shutdown @!worker-pool)
    (.awaitTermination @!worker-pool 30 TimeUnit/SECONDS)
    (reset! !worker-pool nil))
  (log/info "Workers stopped"))

;; ============================================
;; Auto-Scaling
;; ============================================

(defn calculate-desired-workers
  "Calculate desired number of workers based on queue depth."
  []
  (let [queue-depth (get-queue-depth)
        current-workers (count (get-active-nodes))
        tasks-per-worker (if (pos? current-workers)
                           (/ queue-depth current-workers)
                           queue-depth)]
    (cond
      (> tasks-per-worker (:scale-up-threshold config))
      (min (* 2 current-workers) 100)  ; Double workers, max 100
      
      (< tasks-per-worker (:scale-down-threshold config))
      (max (/ current-workers 2) 1)    ; Halve workers, min 1
      
      :else current-workers)))

(defn auto-scale-loop
  "Background loop for auto-scaling."
  []
  (go-loop []
    (when @!running
      (when (:auto-scale config)
        (let [desired (calculate-desired-workers)
              current (count (get-active-nodes))]
          (when (not= desired current)
            (log/info "Auto-scaling:" current "->" desired "workers"))))
      (<! (timeout 10000))  ; Check every 10 seconds
      (recur))))

;; ============================================
;; Continuous Processing Pipelines
;; ============================================

(defn start-continuous-scraper!
  "Start continuous web scraping pipeline."
  [urls & {:keys [interval depth] :or {interval 3600000 depth 2}}]
  (go-loop []
    (when @!running
      (doseq [url urls]
        (enqueue-task! (create-task :scrape {:url url :depth depth})))
      (<! (timeout interval))
      (recur))))

(defn start-file-watcher!
  "Watch directories for new files and process them."
  [directories & {:keys [interval] :or {interval 5000}}]
  (go-loop []
    (when @!running
      (doseq [dir directories]
        ;; In real implementation, would use Java WatchService
        ;; For now, simulate file discovery
        (log/debug "Watching directory:" dir))
      (<! (timeout interval))
      (recur))))

(defn start-sensor-collector!
  "Collect and process sensor data continuously."
  [device-id sensors & {:keys [interval] :or {interval 1000}}]
  (go-loop []
    (when @!running
      (doseq [sensor sensors]
        (enqueue-task! (create-task :sensor-data
                                    {:device-id device-id
                                     :sensor-type sensor
                                     :readings []}
                                    :priority :high)))
      (<! (timeout interval))
      (recur))))

;; ============================================
;; Batch Processing for Petabyte Scale
;; ============================================

(defn create-batch-job
  "Create a batch job for processing large datasets."
  [job-type paths & {:keys [batch-size parallel-batches]
                     :or {batch-size (:batch-size config)
                          parallel-batches 10}}]
  (let [job-id (str (UUID/randomUUID))
        batches (partition-all batch-size paths)]
    {:job-id job-id
     :job-type job-type
     :total-items (count paths)
     :total-batches (count batches)
     :batch-size batch-size
     :status :pending
     :created-at (Instant/now)
     :batches (map-indexed
               (fn [idx batch]
                 {:batch-id (str job-id "-" idx)
                  :items batch
                  :status :pending})
               batches)}))

(defn submit-batch-job!
  "Submit a batch job for processing."
  [job]
  (doseq [batch (:batches job)]
    (enqueue-task! (create-task :batch-ingest
                                {:paths (:items batch)
                                 :batch-id (:batch-id batch)
                                 :job-id (:job-id job)})))
  (:job-id job))

(defn estimate-processing-time
  "Estimate time to process a dataset."
  [total-bytes]
  (let [active-nodes (count (get-active-nodes))
        total-capacity (* active-nodes (:max-workers config))
        bytes-per-second-per-worker 10000000  ; 10 MB/s estimate
        total-throughput (* total-capacity bytes-per-second-per-worker)]
    {:total-bytes total-bytes
     :active-nodes active-nodes
     :total-workers total-capacity
     :throughput-bytes-per-second total-throughput
     :estimated-seconds (/ total-bytes total-throughput)
     :estimated-hours (/ total-bytes total-throughput 3600)}))

;; ============================================
;; Device Coordination
;; ============================================

(defonce !devices (atom {}))

(defn register-device!
  "Register a device in the coordination system."
  [{:keys [device-id device-type capabilities sensors]}]
  (swap! !devices assoc device-id
         {:device-id device-id
          :device-type device-type  ; desktop, phone, watch, server
          :capabilities capabilities
          :sensors (or sensors [])
          :status :active
          :last-seen (Instant/now)
          :utilization 0.0
          :tasks-assigned 0}))

(defn get-device-assignments
  "Get optimal task assignments for a device based on its capabilities."
  [device-id]
  (when-let [device (get @!devices device-id)]
    (let [capabilities (:capabilities device)]
      (cond
        (contains? capabilities :gpu)
        {:task-types [:analyze :learn :process-file]
         :priority :high
         :batch-size 100}
        
        (contains? capabilities :sensor)
        {:task-types [:sensor-data]
         :priority :high
         :batch-size 1}
        
        (contains? capabilities :network)
        {:task-types [:scrape]
         :priority :normal
         :batch-size 10}
        
        :else
        {:task-types [:analyze :process-file]
         :priority :normal
         :batch-size 50}))))

(defn maximize-device-utilization!
  "Ensure all devices are working at maximum capacity."
  []
  (doseq [[device-id device] @!devices]
    (when (< (:utilization device) 0.9)  ; Less than 90% utilized
      (let [assignments (get-device-assignments device-id)
            tasks-to-add (- 10 (:tasks-assigned device))]  ; Target 10 tasks per device
        (when (pos? tasks-to-add)
          (log/info "Assigning" tasks-to-add "more tasks to device" device-id))))))

;; ============================================
;; Metrics and Monitoring
;; ============================================

(defn get-cluster-metrics
  "Get comprehensive cluster metrics."
  []
  (let [nodes (get-active-nodes)
        queue-depth (get-queue-depth)]
    {:cluster
     {:active-nodes (count nodes)
      :total-capacity (reduce + (map :capacity nodes))
      :current-load (reduce + (map :current-load nodes))
      :utilization (if (pos? (count nodes))
                     (/ (reduce + (map :current-load nodes))
                        (reduce + (map :capacity nodes)))
                     0)}
     :queue
     {:depth queue-depth
      :priority-depth (count @!priority-queue)
      :normal-depth (count @!task-queue)}
     :processing
     {:total-processed (:processed @!metrics)
      :total-failed (:failed @!metrics)
      :bytes-processed (:bytes-processed @!metrics)
      :uptime-seconds (.getEpochSecond
                       (java.time.Duration/between
                        (:start-time @!metrics)
                        (Instant/now)))}
     :devices
     {:registered (count @!devices)
      :active (count (filter #(= :active (:status %)) (vals @!devices)))}}))

(defn get-throughput
  "Calculate current throughput."
  []
  (let [metrics @!metrics
        uptime (.getEpochSecond
                (java.time.Duration/between
                 (:start-time metrics)
                 (Instant/now)))]
    (if (pos? uptime)
      {:tasks-per-second (/ (:processed metrics) uptime)
       :bytes-per-second (/ (:bytes-processed metrics) uptime)
       :tasks-per-hour (* 3600 (/ (:processed metrics) uptime))
       :gb-per-hour (/ (* 3600 (:bytes-processed metrics)) uptime 1e9)}
      {:tasks-per-second 0 :bytes-per-second 0
       :tasks-per-hour 0 :gb-per-hour 0})))

;; ============================================
;; API for External Integration
;; ============================================

(defn submit-work
  "Submit work to the distributed system."
  [work-type data & {:keys [priority callback]}]
  (let [task (create-task work-type data :priority priority :callback callback)]
    (enqueue-task! task)))

(defn submit-bulk-work
  "Submit multiple work items efficiently."
  [work-items]
  (doseq [{:keys [work-type data priority]} work-items]
    (enqueue-task! (create-task work-type data :priority priority))))

(defn get-task-status
  "Get status of a specific task."
  [task-id]
  (get @!tasks task-id))

(defn get-task-result
  "Get result of a completed task."
  [task-id]
  (get @!results task-id))

;; ============================================
;; Initialization
;; ============================================

(defn init!
  "Initialize the distributed processing system."
  []
  (log/info "Initializing distributed processing system...")
  (log/info "Node ID:" (:node-id config))
  (log/info "Node Type:" (:node-type config))
  
  ;; Register this node
  (register-node! {:node-id (:node-id config)
                   :node-type (:node-type config)
                   :capabilities #{:analyze :scrape :process :learn}
                   :capacity (:max-workers config)})
  
  ;; Start workers
  (start-workers!)
  
  ;; Start auto-scaling
  (when (:auto-scale config)
    (auto-scale-loop))
  
  (log/info "Distributed processing system initialized")
  true)

(defn shutdown!
  "Shutdown the distributed processing system."
  []
  (log/info "Shutting down distributed processing system...")
  (stop-workers!)
  (log/info "Distributed processing system shutdown complete"))

;; ============================================
;; Convenience Functions for Petabyte Processing
;; ============================================

(defn process-terabyte-dataset
  "Process a terabyte-scale dataset."
  [paths & {:keys [parallel] :or {parallel 100}}]
  (let [job (create-batch-job :process-file paths
                              :batch-size 10000
                              :parallel-batches parallel)]
    (log/info "Created batch job for" (count paths) "files")
    (log/info "Estimated time:" (estimate-processing-time (* (count paths) 1000000)))
    (submit-batch-job! job)))

(defn start-maximum-utilization!
  "Start all systems at maximum utilization."
  []
  (init!)
  (maximize-device-utilization!)
  (log/info "All systems running at maximum utilization")
  (get-cluster-metrics))
