(ns mental-models.pipeline.integration.performance-profiler
  "Performance Profiler Module
   
   Performance measurement and optimization:
   - Function timing
   - Memory profiling
   - Throughput measurement
   - Bottleneck detection
   - Performance reports"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; PROFILER STATE
;; =============================================================================

(defonce profiler-state (atom {:profiles {}
                               :active-spans {}
                               :stats {:total-profiles 0
                                       :total-time-ms 0}}))

;; =============================================================================
;; TIMING UTILITIES
;; =============================================================================

(defn current-time-ns []
  (System/nanoTime))

(defn ns-to-ms [ns]
  (/ ns 1000000.0))

(defn ns-to-us [ns]
  (/ ns 1000.0))

;; =============================================================================
;; SPAN TRACKING
;; =============================================================================

(defn start-span!
  "Start a profiling span."
  [span-id & {:keys [parent-id tags] :or {tags {}}}]
  (when (flags/is-enabled? "performance-profiler")
    (let [span {:id span-id
                :parent-id parent-id
                :start-time (current-time-ns)
                :tags tags
                :children []}]
      (swap! profiler-state assoc-in [:active-spans span-id] span)
      span-id)))

(defn end-span!
  "End a profiling span and record results."
  [span-id & {:keys [status] :or {status :success}}]
  (when (flags/is-enabled? "performance-profiler")
    (when-let [span (get-in @profiler-state [:active-spans span-id])]
      (let [end-time (current-time-ns)
            duration-ns (- end-time (:start-time span))
            duration-ms (ns-to-ms duration-ns)
            result (assoc span
                          :end-time end-time
                          :duration-ns duration-ns
                          :duration-ms duration-ms
                          :status status)]
        ;; Remove from active spans
        (swap! profiler-state update :active-spans dissoc span-id)
        ;; Store in profiles
        (swap! profiler-state update-in [:profiles span-id]
               (fnil conj [])
               result)
        ;; Update stats
        (swap! profiler-state update-in [:stats :total-profiles] inc)
        (swap! profiler-state update-in [:stats :total-time-ms] + duration-ms)
        ;; Record metrics
        (metrics/observe-histogram! :profiler/span-duration duration-ms)
        result))))

(defmacro with-span
  "Execute body within a profiling span."
  [span-id opts & body]
  `(let [span-id# (start-span! ~span-id ~@opts)]
     (try
       (let [result# (do ~@body)]
         (end-span! span-id# :status :success)
         result#)
       (catch Exception e#
         (end-span! span-id# :status :error)
         (throw e#)))))

;; =============================================================================
;; FUNCTION PROFILING
;; =============================================================================

(defn profile-fn
  "Wrap a function with profiling."
  [f name]
  (fn [& args]
    (let [span-id (str name "-" (System/currentTimeMillis))]
      (start-span! span-id :tags {:function name :args-count (count args)})
      (try
        (let [result (apply f args)]
          (end-span! span-id :status :success)
          result)
        (catch Exception e
          (end-span! span-id :status :error)
          (throw e))))))

(defmacro defn-profiled
  "Define a profiled function."
  [name & fdecl]
  `(def ~name (profile-fn (fn ~@fdecl) ~(str name))))

;; =============================================================================
;; MEMORY PROFILING
;; =============================================================================

(defn get-memory-usage
  "Get current memory usage."
  []
  (let [runtime (Runtime/getRuntime)
        total (.totalMemory runtime)
        free (.freeMemory runtime)
        used (- total free)
        max (.maxMemory runtime)]
    {:total-mb (/ total 1048576.0)
     :free-mb (/ free 1048576.0)
     :used-mb (/ used 1048576.0)
     :max-mb (/ max 1048576.0)
     :usage-percent (* 100.0 (/ used max))}))

(defn record-memory-snapshot!
  "Record a memory snapshot."
  [label]
  (let [memory (get-memory-usage)]
    (swap! profiler-state update-in [:memory-snapshots]
           (fnil conj [])
           {:label label
            :timestamp (System/currentTimeMillis)
            :memory memory})
    (metrics/set-gauge! :profiler/memory-used (:used-mb memory))
    memory))

(defn force-gc!
  "Force garbage collection and return memory delta."
  []
  (let [before (get-memory-usage)]
    (System/gc)
    (Thread/sleep 100)
    (let [after (get-memory-usage)]
      {:before before
       :after after
       :freed-mb (- (:used-mb before) (:used-mb after))})))

;; =============================================================================
;; THROUGHPUT MEASUREMENT
;; =============================================================================

(defonce throughput-state (atom {:windows {}
                                 :counters {}}))

(defn record-operation!
  "Record an operation for throughput measurement."
  [operation-type]
  (let [now (System/currentTimeMillis)
        window-key (quot now 1000)] ;; 1-second windows
    (swap! throughput-state update-in [:windows operation-type window-key]
           (fnil inc 0))
    (swap! throughput-state update-in [:counters operation-type]
           (fnil inc 0))))

(defn get-throughput
  "Get throughput for an operation type (ops/sec)."
  [operation-type & {:keys [window-seconds] :or {window-seconds 60}}]
  (let [now (System/currentTimeMillis)
        start-window (quot (- now (* window-seconds 1000)) 1000)
        windows (get-in @throughput-state [:windows operation-type] {})
        relevant-windows (filter (fn [[k _]] (>= k start-window)) windows)
        total-ops (reduce + (map second relevant-windows))
        actual-seconds (min window-seconds (count relevant-windows))]
    (if (pos? actual-seconds)
      (/ total-ops (double actual-seconds))
      0.0)))

(defn get-all-throughputs
  "Get throughput for all operation types."
  []
  (let [operation-types (keys (:windows @throughput-state))]
    (into {} (map (fn [op] [op (get-throughput op)]) operation-types))))

;; =============================================================================
;; BOTTLENECK DETECTION
;; =============================================================================

(defn analyze-profiles
  "Analyze profiles to detect bottlenecks."
  [span-name & {:keys [percentile] :or {percentile 95}}]
  (let [profiles (get-in @profiler-state [:profiles span-name] [])
        durations (map :duration-ms profiles)
        sorted (sort durations)
        count (count sorted)]
    (when (pos? count)
      (let [p-index (int (* (/ percentile 100.0) count))
            p-value (nth sorted (min p-index (dec count)))]
        {:span-name span-name
         :count count
         :min-ms (first sorted)
         :max-ms (last sorted)
         :avg-ms (/ (reduce + durations) count)
         :p50-ms (nth sorted (int (* 0.5 count)))
         :p95-ms p-value
         :p99-ms (nth sorted (int (* 0.99 count)))}))))

(defn find-bottlenecks
  "Find performance bottlenecks across all profiles."
  [& {:keys [threshold-ms] :or {threshold-ms 100}}]
  (let [all-spans (keys (:profiles @profiler-state))
        analyses (map analyze-profiles all-spans)]
    (->> analyses
         (filter some?)
         (filter #(> (:p95-ms %) threshold-ms))
         (sort-by :p95-ms >))))

;; =============================================================================
;; PERFORMANCE REPORTS
;; =============================================================================

(defn generate-performance-report
  "Generate a comprehensive performance report."
  []
  (let [memory (get-memory-usage)
        throughputs (get-all-throughputs)
        bottlenecks (find-bottlenecks)
        stats (:stats @profiler-state)]
    {:timestamp (System/currentTimeMillis)
     :summary {:total-profiles (:total-profiles stats)
               :total-time-ms (:total-time-ms stats)
               :active-spans (count (:active-spans @profiler-state))}
     :memory memory
     :throughput throughputs
     :bottlenecks (take 10 bottlenecks)
     :recommendations (generate-recommendations memory throughputs bottlenecks)}))

(defn generate-recommendations
  "Generate performance recommendations."
  [memory throughputs bottlenecks]
  (let [recs (atom [])]
    ;; Memory recommendations
    (when (> (:usage-percent memory) 80)
      (swap! recs conj {:type :memory
                        :severity :high
                        :message "Memory usage above 80%, consider increasing heap size or optimizing memory usage"}))
    ;; Bottleneck recommendations
    (doseq [bottleneck (take 3 bottlenecks)]
      (swap! recs conj {:type :performance
                        :severity :medium
                        :message (str "Span '" (:span-name bottleneck) "' has p95 latency of " 
                                      (format "%.2f" (:p95-ms bottleneck)) "ms")}))
    @recs))

;; =============================================================================
;; CLEANUP
;; =============================================================================

(defn clear-profiles!
  "Clear all profile data."
  []
  (log/info "Clearing profile data")
  (reset! profiler-state {:profiles {}
                          :active-spans {}
                          :stats {:total-profiles 0
                                  :total-time-ms 0}})
  (reset! throughput-state {:windows {}
                            :counters {}}))

(defn cleanup-old-data!
  "Clean up old profile data."
  [& {:keys [max-age-ms] :or {max-age-ms 3600000}}] ;; 1 hour default
  (let [cutoff (- (System/currentTimeMillis) max-age-ms)]
    ;; Clean up throughput windows
    (swap! throughput-state update :windows
           (fn [windows]
             (into {} (map (fn [[op ws]]
                             [op (into {} (filter (fn [[k _]] (> (* k 1000) cutoff)) ws))])
                           windows))))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-performance-profiler!
  "Initialize performance profiler."
  []
  (log/info "Initializing performance profiler")
  ;; Register feature flag
  (flags/register-flag! "performance-profiler" "Enable performance profiler" true)
  ;; Create metrics
  (metrics/create-histogram! :profiler/span-duration "Span duration" [1 5 10 50 100 500 1000])
  (metrics/create-gauge! :profiler/memory-used "Memory used (MB)" #(:used-mb (get-memory-usage)))
  (metrics/create-gauge! :profiler/active-spans "Active spans" #(count (:active-spans @profiler-state)))
  ;; Record initial memory snapshot
  (record-memory-snapshot! "init")
  (log/info "Performance profiler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-profiler-status []
  {:enabled (flags/is-enabled? "performance-profiler")
   :total-profiles (get-in @profiler-state [:stats :total-profiles])
   :active-spans (count (:active-spans @profiler-state))
   :memory (get-memory-usage)
   :throughputs (get-all-throughputs)})
