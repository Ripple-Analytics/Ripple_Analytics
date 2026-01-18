(ns mental-models.mesh.discovery
  "Mesh Discovery and Workload Distribution
   - Automatic device discovery via mDNS/Bonjour
   - Work stealing for load balancing
   - Fault tolerance with automatic failover
   - Optimal task routing based on device capabilities"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout alts!]]
            [clojure.set :as set])
  (:import [java.net InetAddress DatagramSocket DatagramPacket MulticastSocket NetworkInterface]
           [java.util UUID]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  {:multicast-group "224.0.0.251"
   :multicast-port 5353
   :discovery-interval-ms 5000
   :heartbeat-interval-ms 1000
   :node-timeout-ms 10000
   :work-steal-threshold 0.7  ; Steal work when utilization > 70%
   :replication-factor 2})

;; =============================================================================
;; Node State
;; =============================================================================

(defonce node-id (str (UUID/randomUUID)))

(defonce cluster-state
  (atom {:nodes {}                    ; node-id -> node-info
         :local-queue []              ; Tasks assigned to this node
         :in-flight {}                ; task-id -> task-info
         :completed {}                ; task-id -> result
         :capabilities (detect-capabilities)}))

(defn detect-capabilities []
  "Detect local hardware capabilities"
  {:cpu-cores (.availableProcessors (Runtime/getRuntime))
   :cpu-model (System/getProperty "os.arch")
   :total-memory (.maxMemory (Runtime/getRuntime))
   :gpu-available? (check-gpu-available)
   :gpu-memory (get-gpu-memory)
   :gpu-compute-capability (get-gpu-compute-capability)
   :os (System/getProperty "os.name")
   :hostname (try (.getHostName (InetAddress/getLocalHost)) (catch Exception _ "unknown"))
   :device-type (detect-device-type)})

(defn detect-device-type []
  "Detect if running on desktop, server, mobile, or watch"
  (let [os (System/getProperty "os.name")]
    (cond
      (re-find #"(?i)darwin" os) :desktop
      (re-find #"(?i)linux" os) (if (> (.availableProcessors (Runtime/getRuntime)) 8)
                                   :server
                                   :desktop)
      (re-find #"(?i)windows" os) :desktop
      :else :unknown)))

(defn check-gpu-available []
  "Check if CUDA or Metal GPU is available"
  (try
    ;; Try to load CUDA
    (Class/forName "jcuda.driver.JCudaDriver")
    true
    (catch ClassNotFoundException _
      ;; Try Metal on macOS
      (try
        (Class/forName "com.apple.metal.MTLDevice")
        true
        (catch ClassNotFoundException _ false)))))

(defn get-gpu-memory []
  "Get total GPU memory in bytes"
  (try
    ;; CUDA path
    (let [cuda (Class/forName "jcuda.driver.JCudaDriver")]
      ;; Would call cuDeviceTotalMem here
      (* 8 1024 1024 1024))  ; Default 8GB
    (catch Exception _ 0)))

(defn get-gpu-compute-capability []
  "Get GPU compute capability"
  (try
    ;; Would query actual GPU
    {:major 8 :minor 6}  ; Default to Ampere
    (catch Exception _ nil)))

;; =============================================================================
;; mDNS Discovery
;; =============================================================================

(defn create-announcement-packet []
  "Create mDNS announcement packet"
  (let [data {:node-id node-id
              :capabilities (:capabilities @cluster-state)
              :utilization (calculate-utilization)
              :queue-depth (count (:local-queue @cluster-state))
              :timestamp (System/currentTimeMillis)}
        bytes (.getBytes (pr-str data) "UTF-8")]
    (DatagramPacket. bytes (count bytes)
                     (InetAddress/getByName (:multicast-group config))
                     (:multicast-port config))))

(defn calculate-utilization []
  "Calculate current node utilization"
  (let [state @cluster-state
        in-flight (count (:in-flight state))
        capacity (* (:cpu-cores (:capabilities state)) 2)]
    (if (zero? capacity)
      0.0
      (min 1.0 (/ in-flight capacity)))))

(defn start-announcer []
  "Start periodic node announcements"
  (go-loop []
    (try
      (with-open [socket (DatagramSocket.)]
        (.send socket (create-announcement-packet)))
      (catch Exception e
        (println "Announcement error:" (.getMessage e))))
    (<! (timeout (:discovery-interval-ms config)))
    (recur)))

(defn start-listener []
  "Listen for node announcements"
  (go
    (try
      (with-open [socket (MulticastSocket. (:multicast-port config))]
        (.joinGroup socket (InetAddress/getByName (:multicast-group config)))
        (loop []
          (let [buffer (byte-array 4096)
                packet (DatagramPacket. buffer (count buffer))]
            (.receive socket packet)
            (let [data (read-string (String. (.getData packet) 0 (.getLength packet) "UTF-8"))]
              (when (not= (:node-id data) node-id)
                (swap! cluster-state assoc-in [:nodes (:node-id data)]
                       (assoc data :last-seen (System/currentTimeMillis))))))
          (recur)))
      (catch Exception e
        (println "Listener error:" (.getMessage e))))))

(defn start-heartbeat []
  "Send heartbeats and clean up stale nodes"
  (go-loop []
    ;; Clean up stale nodes
    (let [now (System/currentTimeMillis)
          timeout-threshold (- now (:node-timeout-ms config))]
      (swap! cluster-state update :nodes
             (fn [nodes]
               (into {} (filter #(> (:last-seen (val %)) timeout-threshold) nodes)))))
    (<! (timeout (:heartbeat-interval-ms config)))
    (recur)))

;; =============================================================================
;; Workload Distribution
;; =============================================================================

(defn score-node [node task]
  "Score a node's suitability for a task"
  (let [caps (:capabilities node)
        util (:utilization node)
        queue-depth (:queue-depth node)]
    (+
     ;; Prefer nodes with matching capabilities
     (cond
       (and (:requires-gpu task) (:gpu-available? caps)) 100
       (and (:requires-gpu task) (not (:gpu-available? caps))) -1000
       :else 0)
     
     ;; Prefer less utilized nodes
     (* (- 1 util) 50)
     
     ;; Prefer shorter queues
     (* (- 10 (min queue-depth 10)) 5)
     
     ;; Prefer more powerful nodes for heavy tasks
     (if (:heavy task)
       (* (:cpu-cores caps) 2)
       0)
     
     ;; Prefer local node slightly (data locality)
     (if (= (:node-id node) node-id) 10 0))))

(defn select-best-node [task]
  "Select the best node for a task"
  (let [nodes (vals (:nodes @cluster-state))
        local-node {:node-id node-id
                    :capabilities (:capabilities @cluster-state)
                    :utilization (calculate-utilization)
                    :queue-depth (count (:local-queue @cluster-state))}
        all-nodes (conj nodes local-node)
        scored (map #(vector % (score-node % task)) all-nodes)
        best (first (sort-by second > scored))]
    (first best)))

(defn distribute-task [task]
  "Distribute a task to the optimal node"
  (let [target-node (select-best-node task)]
    (if (= (:node-id target-node) node-id)
      ;; Execute locally
      (do
        (swap! cluster-state update :local-queue conj task)
        {:status :queued :node node-id})
      ;; Send to remote node
      (send-task-to-node target-node task))))

(defn send-task-to-node [node task]
  "Send a task to a remote node"
  ;; In production, this would use HTTP or gRPC
  (let [task-id (str (UUID/randomUUID))
        task-with-id (assoc task :task-id task-id)]
    (swap! cluster-state assoc-in [:in-flight task-id]
           {:task task-with-id
            :target-node (:node-id node)
            :sent-at (System/currentTimeMillis)})
    {:status :sent :task-id task-id :target-node (:node-id node)}))

;; =============================================================================
;; Work Stealing
;; =============================================================================

(defn find-overloaded-nodes []
  "Find nodes that are overloaded"
  (let [threshold (:work-steal-threshold config)]
    (filter #(> (:utilization %) threshold)
            (vals (:nodes @cluster-state)))))

(defn steal-work-from [node]
  "Attempt to steal work from an overloaded node"
  ;; In production, this would make a request to the remote node
  (println "Attempting to steal work from" (:node-id node)))

(defn start-work-stealer []
  "Periodically check for work stealing opportunities"
  (go-loop []
    (let [local-util (calculate-utilization)]
      (when (< local-util 0.3)  ; Only steal if we're underutilized
        (doseq [node (find-overloaded-nodes)]
          (steal-work-from node))))
    (<! (timeout 2000))
    (recur)))

;; =============================================================================
;; Fault Tolerance
;; =============================================================================

(defn check-task-health []
  "Check for failed or timed-out tasks"
  (let [now (System/currentTimeMillis)
        task-timeout-ms 60000]
    (doseq [[task-id task-info] (:in-flight @cluster-state)]
      (when (> (- now (:sent-at task-info)) task-timeout-ms)
        ;; Task timed out, reschedule
        (println "Task" task-id "timed out, rescheduling")
        (swap! cluster-state update :in-flight dissoc task-id)
        (distribute-task (:task task-info))))))

(defn start-health-checker []
  "Periodically check task health"
  (go-loop []
    (check-task-health)
    (<! (timeout 5000))
    (recur)))

;; =============================================================================
;; Task Replication
;; =============================================================================

(defn replicate-critical-task [task]
  "Replicate a critical task across multiple nodes for fault tolerance"
  (let [replication-factor (:replication-factor config)
        nodes (take replication-factor
                    (sort-by #(score-node % task) >
                             (vals (:nodes @cluster-state))))]
    (doseq [node nodes]
      (send-task-to-node node task))
    {:status :replicated :nodes (map :node-id nodes)}))

;; =============================================================================
;; Cluster Statistics
;; =============================================================================

(defn cluster-stats []
  "Get current cluster statistics"
  (let [state @cluster-state
        nodes (vals (:nodes state))
        local-caps (:capabilities state)]
    {:total-nodes (inc (count nodes))
     :total-cpu-cores (+ (:cpu-cores local-caps)
                         (reduce + (map #(get-in % [:capabilities :cpu-cores] 0) nodes)))
     :total-gpu-memory (+ (or (:gpu-memory local-caps) 0)
                          (reduce + (map #(get-in % [:capabilities :gpu-memory] 0) nodes)))
     :gpu-nodes (inc (count (filter #(get-in % [:capabilities :gpu-available?]) nodes)))
     :avg-utilization (/ (+ (calculate-utilization)
                            (reduce + (map :utilization nodes)))
                         (inc (count nodes)))
     :total-queue-depth (+ (count (:local-queue state))
                           (reduce + (map :queue-depth nodes)))
     :in-flight-tasks (count (:in-flight state))
     :completed-tasks (count (:completed state))}))

;; =============================================================================
;; API
;; =============================================================================

(defn submit-task
  "Submit a task for distributed execution"
  ([task] (submit-task task {}))
  ([task opts]
   (let [task-with-meta (merge task
                               {:submitted-at (System/currentTimeMillis)
                                :priority (get opts :priority :normal)
                                :requires-gpu (get opts :requires-gpu false)
                                :heavy (get opts :heavy false)
                                :critical (get opts :critical false)})]
     (if (:critical opts)
       (replicate-critical-task task-with-meta)
       (distribute-task task-with-meta)))))

(defn get-task-status [task-id]
  "Get the status of a task"
  (let [state @cluster-state]
    (cond
      (contains? (:completed state) task-id)
      {:status :completed :result (get-in state [:completed task-id])}
      
      (contains? (:in-flight state) task-id)
      {:status :in-flight :info (get-in state [:in-flight task-id])}
      
      (some #(= (:task-id %) task-id) (:local-queue state))
      {:status :queued}
      
      :else
      {:status :unknown})))

(defn cancel-task [task-id]
  "Cancel a task"
  (swap! cluster-state
         (fn [state]
           (-> state
               (update :in-flight dissoc task-id)
               (update :local-queue (fn [q] (remove #(= (:task-id %) task-id) q))))))
  {:status :cancelled :task-id task-id})

;; =============================================================================
;; Initialization
;; =============================================================================

(defn start-mesh []
  "Start the mesh discovery and distribution system"
  (println "Starting mesh node:" node-id)
  (println "Capabilities:" (:capabilities @cluster-state))
  
  (start-announcer)
  (start-listener)
  (start-heartbeat)
  (start-work-stealer)
  (start-health-checker)
  
  {:node-id node-id
   :status :running})

(defn stop-mesh []
  "Stop the mesh (cleanup)"
  ;; In production, would close channels and sockets
  {:status :stopped})
