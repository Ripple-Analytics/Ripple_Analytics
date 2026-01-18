(ns mental-models.mesh.core
  "Distributed Compute Mesh - Core Architecture
   
   A self-organizing network of compute nodes that:
   - Discovers and connects to all available devices
   - Saturates compute capacity across the mesh
   - Distributes work automatically with zero manual intervention
   - Self-heals when nodes join/leave
   - Scales from single device to thousands of nodes
   
   Inspired by: BitTorrent (peer discovery), Erlang (fault tolerance),
   SETI@home (distributed compute), Borg (resource management)"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan close! 
                                                   mult tap timeout alts!]]
            [taoensso.timbre :as log])
  (:import [java.util UUID]
           [java.time Instant Duration]
           [java.net InetAddress NetworkInterface]))

;; =============================================================================
;; MESH CONFIGURATION
;; =============================================================================

(def config
  {:node-id (str (UUID/randomUUID))
   :cluster-name "mental-models-mesh"
   :discovery-port 7777
   :gossip-port 7778
   :work-port 7779
   :heartbeat-interval-ms 1000
   :failure-detection-ms 5000
   :work-steal-threshold 0.8  ; Steal work when utilization > 80%
   :target-utilization 0.95   ; Target 95% CPU utilization
   :max-queue-depth 10000
   :replication-factor 3})

;; =============================================================================
;; NODE STATE
;; =============================================================================

(defonce node-state
  (atom {:id (:node-id config)
         :status :initializing
         :started-at nil
         :capabilities {}
         :resources {:cpu-cores 0
                     :memory-mb 0
                     :gpu-count 0
                     :storage-gb 0}
         :utilization {:cpu 0.0
                       :memory 0.0
                       :gpu 0.0
                       :network-in 0
                       :network-out 0}
         :peers {}           ; peer-id -> peer-info
         :work-queue []      ; pending work items
         :active-tasks {}    ; task-id -> task-info
         :completed-count 0
         :failed-count 0}))

;; =============================================================================
;; CAPABILITY DETECTION
;; =============================================================================

(defn detect-capabilities
  "Detect what this node can do"
  []
  (let [runtime (Runtime/getRuntime)
        os-name (System/getProperty "os.name")
        arch (System/getProperty "os.arch")]
    {:cpu-cores (.availableProcessors runtime)
     :memory-mb (quot (.maxMemory runtime) (* 1024 1024))
     :os os-name
     :arch arch
     :gpu-available? (detect-gpu)
     :capabilities #{:scrape :analyze :store :ml-inference
                     (when (detect-gpu) :ml-training)
                     :text-processing :image-processing
                     :network-io :disk-io}}))

(defn- detect-gpu
  "Check if GPU is available"
  []
  ;; Would use CUDA/OpenCL detection in production
  false)

(defn get-local-addresses
  "Get all local IP addresses for discovery"
  []
  (->> (NetworkInterface/getNetworkInterfaces)
       enumeration-seq
       (mapcat #(enumeration-seq (.getInetAddresses %)))
       (filter #(not (.isLoopbackAddress %)))
       (map #(.getHostAddress %))))

;; =============================================================================
;; PEER DISCOVERY (Gossip Protocol)
;; =============================================================================

(defn start-discovery!
  "Start peer discovery using gossip protocol"
  []
  (log/info "Starting peer discovery on port" (:discovery-port config))
  
  ;; Multicast discovery for local network
  (go-loop []
    (broadcast-presence!)
    (<! (timeout 5000))
    (when (= :running (:status @node-state))
      (recur)))
  
  ;; Listen for peer announcements
  (go-loop []
    (when-let [peer (<! (listen-for-peers))]
      (register-peer! peer)
      (recur))))

(defn broadcast-presence!
  "Broadcast this node's presence to the network"
  []
  (let [announcement {:type :node-announcement
                      :node-id (:id @node-state)
                      :addresses (get-local-addresses)
                      :capabilities (:capabilities @node-state)
                      :resources (:resources @node-state)
                      :utilization (:utilization @node-state)
                      :timestamp (Instant/now)}]
    ;; Would send via UDP multicast in production
    (log/debug "Broadcasting presence:" (:node-id announcement))))

(defn listen-for-peers
  "Listen for peer announcements"
  []
  ;; Would listen on UDP multicast in production
  (let [c (chan)]
    c))

(defn register-peer!
  "Register a discovered peer"
  [peer-info]
  (let [peer-id (:node-id peer-info)]
    (when (not= peer-id (:id @node-state))
      (swap! node-state assoc-in [:peers peer-id]
             (assoc peer-info
                    :last-seen (Instant/now)
                    :status :alive))
      (log/info "Discovered peer:" peer-id))))

(defn remove-peer!
  "Remove a peer from the mesh"
  [peer-id]
  (swap! node-state update :peers dissoc peer-id)
  (log/info "Removed peer:" peer-id)
  ;; Redistribute any work that was assigned to this peer
  (redistribute-work! peer-id))

;; =============================================================================
;; HEARTBEAT & FAILURE DETECTION
;; =============================================================================

(defn start-heartbeat!
  "Start heartbeat monitoring"
  []
  (go-loop []
    (<! (timeout (:heartbeat-interval-ms config)))
    (when (= :running (:status @node-state))
      ;; Send heartbeat to all peers
      (doseq [[peer-id _] (:peers @node-state)]
        (send-heartbeat! peer-id))
      ;; Check for failed peers
      (check-peer-health!)
      (recur))))

(defn send-heartbeat!
  "Send heartbeat to a peer"
  [peer-id]
  (let [heartbeat {:type :heartbeat
                   :from (:id @node-state)
                   :to peer-id
                   :utilization (:utilization @node-state)
                   :queue-depth (count (:work-queue @node-state))
                   :timestamp (Instant/now)}]
    ;; Would send via TCP in production
    nil))

(defn check-peer-health!
  "Check if any peers have failed"
  []
  (let [now (Instant/now)
        threshold (Duration/ofMillis (:failure-detection-ms config))]
    (doseq [[peer-id peer-info] (:peers @node-state)]
      (when (.isAfter now (.plus (:last-seen peer-info) threshold))
        (log/warn "Peer appears dead:" peer-id)
        (remove-peer! peer-id)))))

;; =============================================================================
;; WORK DISTRIBUTION
;; =============================================================================

(defn submit-work!
  "Submit work to the mesh"
  [work-item]
  (let [item (assoc work-item
                    :id (or (:id work-item) (str (UUID/randomUUID)))
                    :submitted-at (Instant/now)
                    :status :pending)]
    ;; Find best node for this work
    (if-let [target-node (select-node-for-work item)]
      (assign-work! target-node item)
      ;; No suitable node, queue locally
      (swap! node-state update :work-queue conj item))
    (:id item)))

(defn select-node-for-work
  "Select the best node to handle this work"
  [work-item]
  (let [required-caps (:required-capabilities work-item #{})
        all-nodes (cons {:id (:id @node-state)
                         :capabilities (:capabilities @node-state)
                         :utilization (:utilization @node-state)
                         :queue-depth (count (:work-queue @node-state))}
                        (vals (:peers @node-state)))
        capable-nodes (filter #(clojure.set/subset? required-caps 
                                                     (:capabilities %))
                              all-nodes)
        ;; Sort by utilization (prefer less loaded nodes)
        sorted (sort-by #(+ (:cpu (:utilization %))
                            (* 0.1 (:queue-depth %)))
                        capable-nodes)]
    (first sorted)))

(defn assign-work!
  "Assign work to a specific node"
  [node work-item]
  (if (= (:id node) (:id @node-state))
    ;; Local execution
    (execute-locally! work-item)
    ;; Remote execution
    (send-work-to-peer! (:id node) work-item)))

(defn send-work-to-peer!
  "Send work item to a remote peer"
  [peer-id work-item]
  (let [msg {:type :work-assignment
             :from (:id @node-state)
             :to peer-id
             :work work-item
             :timestamp (Instant/now)}]
    ;; Would send via TCP in production
    (log/debug "Sending work to peer:" peer-id (:id work-item))))

(defn redistribute-work!
  "Redistribute work from a failed node"
  [failed-node-id]
  (log/info "Redistributing work from failed node:" failed-node-id)
  ;; In production, would query distributed state for work assigned to this node
  )

;; =============================================================================
;; WORK STEALING (Load Balancing)
;; =============================================================================

(defn start-work-stealing!
  "Start work stealing for load balancing"
  []
  (go-loop []
    (<! (timeout 1000))
    (when (= :running (:status @node-state))
      (let [utilization (get-in @node-state [:utilization :cpu])]
        (when (< utilization (:work-steal-threshold config))
          (attempt-work-steal!)))
      (recur))))

(defn attempt-work-steal!
  "Try to steal work from overloaded peers"
  []
  (let [overloaded-peers (->> (:peers @node-state)
                              (filter (fn [[_ p]]
                                        (and (> (get-in p [:utilization :cpu] 0) 0.9)
                                             (> (:queue-depth p 0) 5))))
                              (map first))]
    (when (seq overloaded-peers)
      (let [target (rand-nth overloaded-peers)]
        (request-work-from-peer! target)))))

(defn request-work-from-peer!
  "Request work from an overloaded peer"
  [peer-id]
  (let [msg {:type :work-steal-request
             :from (:id @node-state)
             :to peer-id
             :capacity (available-capacity)
             :timestamp (Instant/now)}]
    ;; Would send via TCP in production
    (log/debug "Requesting work from:" peer-id)))

(defn available-capacity
  "Calculate available capacity on this node"
  []
  (let [util (:utilization @node-state)
        target (:target-utilization config)]
    (max 0 (- target (:cpu util 0)))))

;; =============================================================================
;; LOCAL EXECUTION
;; =============================================================================

(defn execute-locally!
  "Execute work item on this node"
  [work-item]
  (let [task-id (:id work-item)]
    (swap! node-state assoc-in [:active-tasks task-id]
           (assoc work-item :started-at (Instant/now)))
    
    (go
      (try
        (let [result (execute-work work-item)]
          (complete-task! task-id result))
        (catch Exception e
          (fail-task! task-id e))))))

(defmulti execute-work
  "Execute work based on type"
  :type)

(defmethod execute-work :scrape
  [{:keys [url options]}]
  {:type :scrape-result
   :url url
   :content "scraped content"
   :timestamp (Instant/now)})

(defmethod execute-work :analyze
  [{:keys [content analysis-type]}]
  {:type :analysis-result
   :models-detected []
   :patterns []
   :timestamp (Instant/now)})

(defmethod execute-work :ml-inference
  [{:keys [model-id input]}]
  {:type :inference-result
   :predictions []
   :confidence 0.0
   :timestamp (Instant/now)})

(defmethod execute-work :default
  [work-item]
  (log/warn "Unknown work type:" (:type work-item))
  {:type :unknown-result
   :error "Unknown work type"})

(defn complete-task!
  "Mark task as completed"
  [task-id result]
  (swap! node-state (fn [s]
                      (-> s
                          (update :active-tasks dissoc task-id)
                          (update :completed-count inc))))
  (log/debug "Completed task:" task-id)
  ;; Store result and notify requestor
  (store-result! task-id result))

(defn fail-task!
  "Mark task as failed"
  [task-id error]
  (swap! node-state (fn [s]
                      (-> s
                          (update :active-tasks dissoc task-id)
                          (update :failed-count inc))))
  (log/error "Failed task:" task-id (.getMessage error))
  ;; Retry or redistribute
  (retry-or-redistribute! task-id error))

(defn store-result!
  "Store task result in distributed storage"
  [task-id result]
  ;; Would store in distributed storage (S3, HDFS, etc.)
  nil)

(defn retry-or-redistribute!
  "Retry failed task or redistribute to another node"
  [task-id error]
  ;; Would implement retry logic with backoff
  nil)

;; =============================================================================
;; RESOURCE MONITORING
;; =============================================================================

(defn start-resource-monitor!
  "Start monitoring local resources"
  []
  (go-loop []
    (<! (timeout 500))
    (when (= :running (:status @node-state))
      (update-utilization!)
      (recur))))

(defn update-utilization!
  "Update current resource utilization"
  []
  (let [runtime (Runtime/getRuntime)
        used-memory (- (.totalMemory runtime) (.freeMemory runtime))
        max-memory (.maxMemory runtime)
        ;; Would use proper CPU monitoring in production
        cpu-estimate (/ (count (:active-tasks @node-state))
                        (max 1 (:cpu-cores (:resources @node-state))))]
    (swap! node-state assoc :utilization
           {:cpu (min 1.0 cpu-estimate)
            :memory (/ used-memory max-memory)
            :gpu 0.0
            :active-tasks (count (:active-tasks @node-state))
            :queue-depth (count (:work-queue @node-state))})))

;; =============================================================================
;; MESH LIFECYCLE
;; =============================================================================

(defn start-mesh!
  "Start the compute mesh on this node"
  []
  (log/info "Starting compute mesh node:" (:node-id config))
  
  ;; Initialize state
  (swap! node-state assoc
         :status :running
         :started-at (Instant/now)
         :capabilities (detect-capabilities)
         :resources (detect-capabilities))
  
  ;; Start all subsystems
  (start-discovery!)
  (start-heartbeat!)
  (start-work-stealing!)
  (start-resource-monitor!)
  (start-work-processor!)
  
  (log/info "Compute mesh node started")
  @node-state)

(defn stop-mesh!
  "Stop the compute mesh on this node"
  []
  (log/info "Stopping compute mesh node")
  (swap! node-state assoc :status :stopping)
  
  ;; Graceful shutdown - redistribute pending work
  (doseq [work-item (:work-queue @node-state)]
    (submit-work! work-item))
  
  (swap! node-state assoc :status :stopped)
  (log/info "Compute mesh node stopped"))

(defn start-work-processor!
  "Start processing work from the queue"
  []
  (go-loop []
    (<! (timeout 100))
    (when (= :running (:status @node-state))
      ;; Process work if we have capacity
      (when (and (seq (:work-queue @node-state))
                 (< (:cpu (:utilization @node-state)) 
                    (:target-utilization config)))
        (let [work-item (first (:work-queue @node-state))]
          (swap! node-state update :work-queue rest)
          (execute-locally! work-item)))
      (recur))))

;; =============================================================================
;; MESH STATUS
;; =============================================================================

(defn mesh-status
  "Get current mesh status"
  []
  {:node (:id @node-state)
   :status (:status @node-state)
   :uptime (when (:started-at @node-state)
             (.toMillis (Duration/between (:started-at @node-state) (Instant/now))))
   :peers (count (:peers @node-state))
   :utilization (:utilization @node-state)
   :completed (:completed-count @node-state)
   :failed (:failed-count @node-state)
   :queue-depth (count (:work-queue @node-state))
   :active-tasks (count (:active-tasks @node-state))})

(defn mesh-topology
  "Get the full mesh topology"
  []
  {:nodes (cons {:id (:id @node-state)
                 :type :self
                 :capabilities (:capabilities @node-state)
                 :utilization (:utilization @node-state)}
                (map (fn [[id info]]
                       {:id id
                        :type :peer
                        :capabilities (:capabilities info)
                        :utilization (:utilization info)
                        :last-seen (:last-seen info)})
                     (:peers @node-state)))
   :total-capacity (reduce + (map #(get-in % [:resources :cpu-cores] 0)
                                  (cons @node-state (vals (:peers @node-state)))))
   :total-memory-gb (/ (reduce + (map #(get-in % [:resources :memory-mb] 0)
                                      (cons @node-state (vals (:peers @node-state)))))
                       1024)})
