(ns mental-models.pipeline.integration.response-profiler
  "Response profiler for mental model analysis system.
   
   Features:
   - Response profiling
   - Performance profiling
   - Memory profiling
   - CPU profiling
   - Profile snapshots
   - Profile comparison
   - Profile reports
   - Profiling metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.lang.management ManagementFactory MemoryMXBean ThreadMXBean]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:profiles []         ;; profile results
         :snapshots {}        ;; snapshot-id -> snapshot
         :config {:enabled? true
                  :sample-rate 1.0
                  :max-profiles 10000
                  :track-memory? true
                  :track-cpu? true
                  :track-gc? true}
         :stats {:profiles-created 0
                 :snapshots-taken 0
                 :comparisons-made 0
                 :total-duration-ms 0}
         :memory-bean nil
         :thread-bean nil
         :initialized? false}))

;; ============================================================================
;; JMX Beans
;; ============================================================================

(defn- get-memory-bean
  "Get the memory MXBean."
  []
  (or (:memory-bean @state)
      (ManagementFactory/getMemoryMXBean)))

(defn- get-thread-bean
  "Get the thread MXBean."
  []
  (or (:thread-bean @state)
      (ManagementFactory/getThreadMXBean)))

;; ============================================================================
;; Memory Profiling
;; ============================================================================

(defn get-memory-usage
  "Get current memory usage."
  []
  (let [memory-bean (get-memory-bean)
        heap (.getHeapMemoryUsage memory-bean)
        non-heap (.getNonHeapMemoryUsage memory-bean)]
    {:heap {:used (.getUsed heap)
            :committed (.getCommitted heap)
            :max (.getMax heap)
            :init (.getInit heap)}
     :non-heap {:used (.getUsed non-heap)
                :committed (.getCommitted non-heap)
                :max (.getMax non-heap)
                :init (.getInit non-heap)}
     :total-used (+ (.getUsed heap) (.getUsed non-heap))}))

(defn get-gc-stats
  "Get garbage collection statistics."
  []
  (let [gc-beans (ManagementFactory/getGarbageCollectorMXBeans)]
    (mapv (fn [gc]
            {:name (.getName gc)
             :collection-count (.getCollectionCount gc)
             :collection-time-ms (.getCollectionTime gc)})
          gc-beans)))

(defn profile-memory
  "Profile memory for a function execution."
  [f]
  (let [before (get-memory-usage)
        gc-before (get-gc-stats)
        result (f)
        after (get-memory-usage)
        gc-after (get-gc-stats)]
    {:result result
     :memory {:before before
              :after after
              :delta {:heap (- (get-in after [:heap :used])
                               (get-in before [:heap :used]))
                      :non-heap (- (get-in after [:non-heap :used])
                                   (get-in before [:non-heap :used]))
                      :total (- (:total-used after) (:total-used before))}}
     :gc {:before gc-before
          :after gc-after}}))

;; ============================================================================
;; CPU Profiling
;; ============================================================================

(defn get-cpu-usage
  "Get current CPU usage."
  []
  (let [thread-bean (get-thread-bean)
        os-bean (ManagementFactory/getOperatingSystemMXBean)]
    {:thread-count (.getThreadCount thread-bean)
     :peak-thread-count (.getPeakThreadCount thread-bean)
     :daemon-thread-count (.getDaemonThreadCount thread-bean)
     :total-started-thread-count (.getTotalStartedThreadCount thread-bean)
     :system-load-average (.getSystemLoadAverage os-bean)
     :available-processors (.getAvailableProcessors os-bean)}))

(defn get-thread-cpu-time
  "Get CPU time for current thread."
  []
  (let [thread-bean (get-thread-bean)]
    (when (.isCurrentThreadCpuTimeSupported thread-bean)
      {:cpu-time-ns (.getCurrentThreadCpuTime thread-bean)
       :user-time-ns (.getCurrentThreadUserTime thread-bean)})))

(defn profile-cpu
  "Profile CPU for a function execution."
  [f]
  (let [thread-bean (get-thread-bean)
        cpu-before (get-thread-cpu-time)
        start-time (System/nanoTime)
        result (f)
        end-time (System/nanoTime)
        cpu-after (get-thread-cpu-time)]
    {:result result
     :timing {:wall-time-ns (- end-time start-time)
              :wall-time-ms (/ (- end-time start-time) 1000000.0)}
     :cpu (when (and cpu-before cpu-after)
            {:cpu-time-ns (- (:cpu-time-ns cpu-after) (:cpu-time-ns cpu-before))
             :user-time-ns (- (:user-time-ns cpu-after) (:user-time-ns cpu-before))
             :cpu-time-ms (/ (- (:cpu-time-ns cpu-after) (:cpu-time-ns cpu-before)) 1000000.0)})}))

;; ============================================================================
;; Response Profiling
;; ============================================================================

(defn profile-response
  "Profile a response generation."
  [handler request]
  (when (and (get-in @state [:config :enabled?])
             (< (rand) (get-in @state [:config :sample-rate])))
    (swap! state update-in [:stats :profiles-created] inc)
    
    (let [start-time (System/currentTimeMillis)
          memory-before (when (get-in @state [:config :track-memory?])
                          (get-memory-usage))
          cpu-before (when (get-in @state [:config :track-cpu?])
                       (get-thread-cpu-time))
          gc-before (when (get-in @state [:config :track-gc?])
                      (get-gc-stats))
          
          response (handler request)
          
          end-time (System/currentTimeMillis)
          duration-ms (- end-time start-time)
          memory-after (when (get-in @state [:config :track-memory?])
                         (get-memory-usage))
          cpu-after (when (get-in @state [:config :track-cpu?])
                      (get-thread-cpu-time))
          gc-after (when (get-in @state [:config :track-gc?])
                     (get-gc-stats))
          
          profile {:id (str (UUID/randomUUID))
                   :timestamp start-time
                   :request {:method (:request-method request)
                             :uri (:uri request)}
                   :response {:status (:status response)
                              :body-size (count (str (:body response)))}
                   :timing {:duration-ms duration-ms
                            :start-time start-time
                            :end-time end-time}
                   :memory (when memory-before
                             {:before memory-before
                              :after memory-after
                              :delta {:heap (- (get-in memory-after [:heap :used])
                                               (get-in memory-before [:heap :used]))
                                      :total (- (:total-used memory-after)
                                                (:total-used memory-before))}})
                   :cpu (when (and cpu-before cpu-after)
                          {:cpu-time-ns (- (:cpu-time-ns cpu-after) (:cpu-time-ns cpu-before))
                           :user-time-ns (- (:user-time-ns cpu-after) (:user-time-ns cpu-before))})
                   :gc (when gc-before
                         {:before gc-before
                          :after gc-after})}]
      
      (swap! state update-in [:stats :total-duration-ms] + duration-ms)
      
      ;; Store profile
      (let [max-profiles (get-in @state [:config :max-profiles])]
        (swap! state update :profiles
               (fn [p]
                 (let [new-profiles (conj p profile)]
                   (if (> (count new-profiles) max-profiles)
                     (vec (drop 1 new-profiles))
                     new-profiles)))))
      
      {:response response
       :profile profile})))

;; ============================================================================
;; Snapshots
;; ============================================================================

(defn take-snapshot!
  "Take a profiling snapshot."
  [& {:keys [name]}]
  (let [snapshot-id (str (UUID/randomUUID))
        snapshot {:id snapshot-id
                  :name (or name (str "Snapshot " snapshot-id))
                  :timestamp (System/currentTimeMillis)
                  :memory (get-memory-usage)
                  :cpu (get-cpu-usage)
                  :gc (get-gc-stats)
                  :profiles-count (count (:profiles @state))
                  :stats (:stats @state)}]
    
    (swap! state assoc-in [:snapshots snapshot-id] snapshot)
    (swap! state update-in [:stats :snapshots-taken] inc)
    snapshot-id))

(defn get-snapshot
  "Get a snapshot."
  [snapshot-id]
  (get-in @state [:snapshots snapshot-id]))

(defn list-snapshots
  "List all snapshots."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :timestamp (:timestamp s)
           :memory-used (get-in s [:memory :total-used])})
        (:snapshots @state)))

(defn delete-snapshot!
  "Delete a snapshot."
  [snapshot-id]
  (swap! state update :snapshots dissoc snapshot-id))

;; ============================================================================
;; Profile Comparison
;; ============================================================================

(defn compare-snapshots
  "Compare two snapshots."
  [snapshot-id-1 snapshot-id-2]
  (let [s1 (get-snapshot snapshot-id-1)
        s2 (get-snapshot snapshot-id-2)]
    (when (and s1 s2)
      (swap! state update-in [:stats :comparisons-made] inc)
      {:snapshot-1 {:id snapshot-id-1 :name (:name s1)}
       :snapshot-2 {:id snapshot-id-2 :name (:name s2)}
       :time-diff-ms (- (:timestamp s2) (:timestamp s1))
       :memory-diff {:heap (- (get-in s2 [:memory :heap :used])
                              (get-in s1 [:memory :heap :used]))
                     :total (- (get-in s2 [:memory :total-used])
                               (get-in s1 [:memory :total-used]))}
       :thread-diff (- (get-in s2 [:cpu :thread-count])
                       (get-in s1 [:cpu :thread-count]))
       :profiles-diff (- (:profiles-count s2) (:profiles-count s1))})))

(defn compare-profiles
  "Compare two profiles."
  [profile-1 profile-2]
  {:timing-diff-ms (- (get-in profile-2 [:timing :duration-ms])
                      (get-in profile-1 [:timing :duration-ms]))
   :memory-diff (when (and (:memory profile-1) (:memory profile-2))
                  (- (get-in profile-2 [:memory :delta :total])
                     (get-in profile-1 [:memory :delta :total])))
   :cpu-diff (when (and (:cpu profile-1) (:cpu profile-2))
               (- (get-in profile-2 [:cpu :cpu-time-ns])
                  (get-in profile-1 [:cpu :cpu-time-ns])))})

;; ============================================================================
;; Profile Analysis
;; ============================================================================

(defn analyze-profiles
  "Analyze collected profiles."
  [& {:keys [limit] :or {limit 1000}}]
  (let [profiles (take-last limit (:profiles @state))
        durations (map #(get-in % [:timing :duration-ms]) profiles)
        memory-deltas (keep #(get-in % [:memory :delta :total]) profiles)]
    {:count (count profiles)
     :timing {:avg-ms (when (seq durations)
                        (/ (reduce + durations) (count durations)))
              :min-ms (when (seq durations) (apply min durations))
              :max-ms (when (seq durations) (apply max durations))
              :p50-ms (when (seq durations)
                        (nth (sort durations) (int (* 0.5 (count durations))) 0))
              :p95-ms (when (seq durations)
                        (nth (sort durations) (int (* 0.95 (count durations))) 0))
              :p99-ms (when (seq durations)
                        (nth (sort durations) (int (* 0.99 (count durations))) 0))}
     :memory {:avg-delta (when (seq memory-deltas)
                           (/ (reduce + memory-deltas) (count memory-deltas)))
              :min-delta (when (seq memory-deltas) (apply min memory-deltas))
              :max-delta (when (seq memory-deltas) (apply max memory-deltas))}
     :by-status (frequencies (map #(get-in % [:response :status]) profiles))
     :by-uri (frequencies (map #(get-in % [:request :uri]) profiles))}))

(defn get-slow-profiles
  "Get profiles slower than threshold."
  [threshold-ms & {:keys [limit] :or {limit 100}}]
  (->> (:profiles @state)
       (filter #(> (get-in % [:timing :duration-ms]) threshold-ms))
       (take-last limit)
       vec))

(defn get-memory-intensive-profiles
  "Get profiles with high memory usage."
  [threshold-bytes & {:keys [limit] :or {limit 100}}]
  (->> (:profiles @state)
       (filter #(> (get-in % [:memory :delta :total] 0) threshold-bytes))
       (take-last limit)
       vec))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-profile
  "Ring middleware to profile responses."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [result (profile-response handler request)]
        (:response result))
      (handler request))))

(defn wrap-profile-with-header
  "Ring middleware to profile and add timing header."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [result (profile-response handler request)
            response (:response result)
            profile (:profile result)]
        (assoc-in response [:headers "X-Response-Time"]
                  (str (get-in profile [:timing :duration-ms]) "ms")))
      (handler request))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable profiling."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-sample-rate!
  "Set profiling sample rate."
  [rate]
  (swap! state assoc-in [:config :sample-rate] (max 0.0 (min 1.0 rate))))

(defn set-track-memory!
  "Enable/disable memory tracking."
  [enabled?]
  (swap! state assoc-in [:config :track-memory?] enabled?))

(defn set-track-cpu!
  "Enable/disable CPU tracking."
  [enabled?]
  (swap! state assoc-in [:config :track-cpu?] enabled?))

(defn set-track-gc!
  "Enable/disable GC tracking."
  [enabled?]
  (swap! state assoc-in [:config :track-gc?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-profiler-metrics
  "Get profiler metrics."
  []
  (let [stats (:stats @state)]
    {:profiles-created (:profiles-created stats)
     :snapshots-taken (:snapshots-taken stats)
     :comparisons-made (:comparisons-made stats)
     :total-duration-ms (:total-duration-ms stats)
     :profiles-count (count (:profiles @state))
     :snapshots-count (count (:snapshots @state))
     :avg-duration-ms (when (pos? (:profiles-created stats))
                        (/ (:total-duration-ms stats) (:profiles-created stats)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-profiler-stats
  "Get profiler statistics."
  []
  (merge (get-profiler-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :sample-rate (get-in @state [:config :sample-rate])
          :track-memory? (get-in @state [:config :track-memory?])
          :track-cpu? (get-in @state [:config :track-cpu?])
          :track-gc? (get-in @state [:config :track-gc?])}))

(defn reset-stats!
  "Reset profiler statistics."
  []
  (swap! state assoc :stats {:profiles-created 0
                             :snapshots-taken 0
                             :comparisons-made 0
                             :total-duration-ms 0}))

(defn clear-profiles!
  "Clear all profiles."
  []
  (swap! state assoc :profiles []))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-profiler!
  "Initialize the response profiler."
  []
  (when-not (:initialized? @state)
    (swap! state assoc
           :memory-bean (ManagementFactory/getMemoryMXBean)
           :thread-bean (ManagementFactory/getThreadMXBean))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response profiler initialized")
    (events/emit! :response-profiler-initialized {})
    true))
