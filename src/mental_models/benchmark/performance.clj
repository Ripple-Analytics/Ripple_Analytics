(ns mental-models.benchmark.performance
  "Performance Benchmarking Module for Mental Models Pipeline
   
   Provides comprehensive performance measurement:
   - Throughput (documents/second)
   - Latency (p50, p95, p99)
   - Memory usage tracking
   - CPU utilization
   - Concurrent processing metrics
   - Historical trend analysis"
  (:require
   [clojure.core.async :as async :refer [go chan put! <! <!! >!! close! timeout]]
   [clojure.string :as str])
  (:import
   [java.lang.management ManagementFactory]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; METRICS STATE
;; =============================================================================

(defonce metrics-state
  (atom {:start-time nil
         :end-time nil
         :total-documents 0
         :processed-documents 0
         :failed-documents 0
         :latencies []
         :memory-samples []
         :cpu-samples []
         :errors []}))

(defonce latency-histogram (atom []))
(defonce throughput-counter (AtomicLong. 0))

;; =============================================================================
;; SYSTEM METRICS
;; =============================================================================

(defn get-memory-usage
  "Get current JVM memory usage in MB"
  []
  (let [runtime (Runtime/getRuntime)
        used (- (.totalMemory runtime) (.freeMemory runtime))
        max (.maxMemory runtime)]
    {:used-mb (/ used 1048576.0)
     :max-mb (/ max 1048576.0)
     :percent (* 100.0 (/ used max))}))

(defn get-cpu-usage
  "Get system CPU load (0.0 to 1.0)"
  []
  (try
    (let [os-bean (ManagementFactory/getOperatingSystemMXBean)]
      (if (instance? com.sun.management.OperatingSystemMXBean os-bean)
        (.getSystemCpuLoad ^com.sun.management.OperatingSystemMXBean os-bean)
        -1.0))
    (catch Exception _ -1.0)))

(defn get-thread-count
  "Get current thread count"
  []
  (let [thread-bean (ManagementFactory/getThreadMXBean)]
    {:active (.getThreadCount thread-bean)
     :peak (.getPeakThreadCount thread-bean)
     :daemon (.getDaemonThreadCount thread-bean)}))

;; =============================================================================
;; LATENCY TRACKING
;; =============================================================================

(defmacro with-timing
  "Execute body and record latency"
  [& body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         elapsed-ms# (/ (- (System/nanoTime) start#) 1000000.0)]
     (swap! latency-histogram conj elapsed-ms#)
     result#))

(defn record-latency!
  "Record a latency measurement in milliseconds"
  [latency-ms]
  (swap! latency-histogram conj latency-ms)
  (swap! metrics-state update :latencies conj latency-ms))

(defn calculate-percentiles
  "Calculate p50, p95, p99 from latency data"
  [latencies]
  (when (seq latencies)
    (let [sorted (sort latencies)
          n (count sorted)
          p50-idx (int (* 0.5 n))
          p95-idx (int (* 0.95 n))
          p99-idx (int (* 0.99 n))]
      {:p50 (nth sorted (min p50-idx (dec n)))
       :p95 (nth sorted (min p95-idx (dec n)))
       :p99 (nth sorted (min p99-idx (dec n)))
       :min (first sorted)
       :max (last sorted)
       :mean (/ (reduce + latencies) n)})))

;; =============================================================================
;; THROUGHPUT TRACKING
;; =============================================================================

(defn increment-processed!
  "Increment processed document counter"
  []
  (.incrementAndGet throughput-counter)
  (swap! metrics-state update :processed-documents inc))

(defn increment-failed!
  "Increment failed document counter"
  []
  (swap! metrics-state update :failed-documents inc))

(defn calculate-throughput
  "Calculate documents per second"
  []
  (let [{:keys [start-time processed-documents]} @metrics-state
        elapsed-seconds (when start-time
                          (/ (- (System/currentTimeMillis) start-time) 1000.0))]
    (if (and elapsed-seconds (pos? elapsed-seconds))
      (/ processed-documents elapsed-seconds)
      0.0)))

;; =============================================================================
;; BENCHMARK LIFECYCLE
;; =============================================================================

(defn start-benchmark!
  "Start a new benchmark run"
  [total-documents]
  (reset! latency-histogram [])
  (.set throughput-counter 0)
  (reset! metrics-state
          {:start-time (System/currentTimeMillis)
           :end-time nil
           :total-documents total-documents
           :processed-documents 0
           :failed-documents 0
           :latencies []
           :memory-samples []
           :cpu-samples []
           :errors []})
  (println "[BENCHMARK] Started with" total-documents "documents"))

(defn stop-benchmark!
  "Stop the current benchmark and return results"
  []
  (swap! metrics-state assoc :end-time (System/currentTimeMillis))
  (get-benchmark-results))

(defn get-benchmark-results
  "Get comprehensive benchmark results"
  []
  (let [{:keys [start-time end-time total-documents processed-documents
                failed-documents latencies memory-samples]} @metrics-state
        elapsed-ms (if (and start-time end-time)
                     (- end-time start-time)
                     (when start-time (- (System/currentTimeMillis) start-time)))
        elapsed-seconds (when elapsed-ms (/ elapsed-ms 1000.0))]
    {:summary
     {:total-documents total-documents
      :processed processed-documents
      :failed failed-documents
      :success-rate (if (pos? total-documents)
                      (* 100.0 (/ processed-documents total-documents))
                      0.0)
      :elapsed-seconds elapsed-seconds
      :throughput-dps (if (and elapsed-seconds (pos? elapsed-seconds))
                        (/ processed-documents elapsed-seconds)
                        0.0)}
     :latency (calculate-percentiles @latency-histogram)
     :memory (get-memory-usage)
     :threads (get-thread-count)
     :timestamp (System/currentTimeMillis)}))

;; =============================================================================
;; BACKGROUND MONITORING
;; =============================================================================

(defonce monitor-channel (atom nil))

(defn start-monitoring!
  "Start background monitoring of system metrics"
  [interval-ms]
  (let [ch (chan)]
    (reset! monitor-channel ch)
    (go
      (loop []
        (let [[_ port] (async/alts! [ch (timeout interval-ms)])]
          (when-not (= port ch)
            (swap! metrics-state update :memory-samples conj (get-memory-usage))
            (swap! metrics-state update :cpu-samples conj (get-cpu-usage))
            (recur)))))
    ch))

(defn stop-monitoring!
  "Stop background monitoring"
  []
  (when-let [ch @monitor-channel]
    (close! ch)
    (reset! monitor-channel nil)))

;; =============================================================================
;; BENCHMARK RUNNER
;; =============================================================================

(defn run-benchmark
  "Run a benchmark with the given function and documents"
  [process-fn documents & {:keys [concurrency warmup-count]
                           :or {concurrency 4 warmup-count 10}}]
  (println "[BENCHMARK] Warming up with" warmup-count "documents...")
  
  ;; Warmup phase
  (doseq [doc (take warmup-count documents)]
    (try (process-fn doc) (catch Exception _)))
  
  ;; Reset and start actual benchmark
  (start-benchmark! (count documents))
  (start-monitoring! 1000)
  
  (println "[BENCHMARK] Processing" (count documents) "documents with concurrency" concurrency)
  
  ;; Process documents in parallel
  (let [results (doall
                  (pmap
                    (fn [doc]
                      (let [start (System/nanoTime)]
                        (try
                          (let [result (process-fn doc)
                                elapsed-ms (/ (- (System/nanoTime) start) 1000000.0)]
                            (record-latency! elapsed-ms)
                            (increment-processed!)
                            {:status :success :latency-ms elapsed-ms})
                          (catch Exception e
                            (increment-failed!)
                            {:status :error :error (.getMessage e)}))))
                    documents))]
    
    (stop-monitoring!)
    (let [final-results (stop-benchmark!)]
      (println "[BENCHMARK] Complete:")
      (println "  Throughput:" (format "%.2f" (get-in final-results [:summary :throughput-dps])) "docs/sec")
      (println "  Latency p50:" (format "%.2f" (get-in final-results [:latency :p50] 0.0)) "ms")
      (println "  Latency p99:" (format "%.2f" (get-in final-results [:latency :p99] 0.0)) "ms")
      (println "  Success rate:" (format "%.1f" (get-in final-results [:summary :success-rate])) "%")
      final-results)))

;; =============================================================================
;; REPORT GENERATION
;; =============================================================================

(defn format-benchmark-report
  "Generate a formatted benchmark report"
  [results]
  (let [{:keys [summary latency memory threads]} results]
    (str
      "=== BENCHMARK REPORT ===
"
      "
"
      "THROUGHPUT
"
      "  Documents processed: " (:processed summary) "/" (:total-documents summary) "
"
      "  Failed: " (:failed summary) "
"
      "  Success rate: " (format "%.1f%%" (:success-rate summary)) "
"
      "  Throughput: " (format "%.2f docs/sec" (:throughput-dps summary)) "
"
      "  Total time: " (format "%.2f seconds" (:elapsed-seconds summary)) "
"
      "
"
      "LATENCY
"
      "  Min: " (format "%.2f ms" (:min latency 0.0)) "
"
      "  p50: " (format "%.2f ms" (:p50 latency 0.0)) "
"
      "  p95: " (format "%.2f ms" (:p95 latency 0.0)) "
"
      "  p99: " (format "%.2f ms" (:p99 latency 0.0)) "
"
      "  Max: " (format "%.2f ms" (:max latency 0.0)) "
"
      "  Mean: " (format "%.2f ms" (:mean latency 0.0)) "
"
      "
"
      "MEMORY
"
      "  Used: " (format "%.1f MB" (:used-mb memory)) "
"
      "  Max: " (format "%.1f MB" (:max-mb memory)) "
"
      "  Utilization: " (format "%.1f%%" (:percent memory)) "
"
      "
"
      "THREADS
"
      "  Active: " (:active threads) "
"
      "  Peak: " (:peak threads) "
"
      "  Daemon: " (:daemon threads) "
"
      "
"
      "========================
")))

(defn save-benchmark-results!
  "Save benchmark results to a file"
  [results filepath]
  (spit filepath (pr-str results))
  (println "[BENCHMARK] Results saved to" filepath))
