(ns mental-models.logging.structured
  "Structured Logging Module for Mental Models Pipeline
   
   Provides structured logging with:
   - JSON-formatted log entries
   - Log levels (debug, info, warn, error, fatal)
   - Context propagation
   - Log aggregation
   - Performance metrics in logs
   - Correlation IDs for request tracing"
  (:require
   [clojure.string :as str]
   [cheshire.core :as json])
  (:import
   [java.time Instant ZoneId]
   [java.time.format DateTimeFormatter]))

;; =============================================================================
;; LOG LEVELS
;; =============================================================================

(def log-levels
  {:debug 0
   :info 1
   :warn 2
   :error 3
   :fatal 4})

(def ^:dynamic *log-level* :info)
(def ^:dynamic *log-context* {})
(def ^:dynamic *correlation-id* nil)

;; =============================================================================
;; LOG STATE
;; =============================================================================

(defonce log-buffer (atom []))
(defonce log-handlers (atom []))
(def max-buffer-size 10000)

;; =============================================================================
;; FORMATTING
;; =============================================================================

(def iso-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))

(defn format-timestamp
  "Format timestamp as ISO-8601"
  [^long millis]
  (.format iso-formatter
           (.atZone (Instant/ofEpochMilli millis)
                    (ZoneId/of "UTC"))))

(defn format-log-entry
  "Format a log entry as JSON"
  [{:keys [level message timestamp context correlation-id data exception]}]
  (json/generate-string
   (cond-> {:level (name level)
            :message message
            :timestamp (format-timestamp timestamp)
            :service "mental-models-pipeline"}
     context (assoc :context context)
     correlation-id (assoc :correlation_id correlation-id)
     data (assoc :data data)
     exception (assoc :exception {:class (.getName (class exception))
                                  :message (.getMessage exception)
                                  :stacktrace (mapv str (.getStackTrace exception))}))))

;; =============================================================================
;; LOG HANDLERS
;; =============================================================================

(defn console-handler
  "Log to console with colors"
  [{:keys [level message timestamp] :as entry}]
  (let [color (case level
                :debug "[36m"  ; cyan
                :info "[32m"   ; green
                :warn "[33m"   ; yellow
                :error "[31m"  ; red
                :fatal "[35m"  ; magenta
                "")
        reset "[0m"]
    (println (str color "[" (str/upper-case (name level)) "]" reset
                  " " (format-timestamp timestamp)
                  " - " message))))

(defn json-handler
  "Log as JSON to stdout"
  [entry]
  (println (format-log-entry entry)))

(defn buffer-handler
  "Store logs in memory buffer"
  [entry]
  (swap! log-buffer (fn [buf]
                      (let [new-buf (conj buf entry)]
                        (if (> (count new-buf) max-buffer-size)
                          (vec (drop (/ max-buffer-size 2) new-buf))
                          new-buf)))))

(defn file-handler
  "Log to file"
  [filepath]
  (fn [entry]
    (spit filepath (str (format-log-entry entry) "
") :append true)))

;; =============================================================================
;; CORE LOGGING
;; =============================================================================

(defn should-log?
  "Check if message should be logged at current level"
  [level]
  (>= (get log-levels level 0)
      (get log-levels *log-level* 0)))

(defn log!
  "Core logging function"
  [level message & {:keys [data exception context]}]
  (when (should-log? level)
    (let [entry {:level level
                 :message message
                 :timestamp (System/currentTimeMillis)
                 :context (merge *log-context* context)
                 :correlation-id *correlation-id*
                 :data data
                 :exception exception}]
      (doseq [handler @log-handlers]
        (try
          (handler entry)
          (catch Exception e
            (println "[LOG ERROR]" (.getMessage e))))))))

;; =============================================================================
;; LOG LEVEL FUNCTIONS
;; =============================================================================

(defn debug
  "Log at DEBUG level"
  [message & {:as opts}]
  (apply log! :debug message (mapcat identity opts)))

(defn info
  "Log at INFO level"
  [message & {:as opts}]
  (apply log! :info message (mapcat identity opts)))

(defn warn
  "Log at WARN level"
  [message & {:as opts}]
  (apply log! :warn message (mapcat identity opts)))

(defn error
  "Log at ERROR level"
  [message & {:as opts}]
  (apply log! :error message (mapcat identity opts)))

(defn fatal
  "Log at FATAL level"
  [message & {:as opts}]
  (apply log! :fatal message (mapcat identity opts)))

;; =============================================================================
;; CONTEXT MANAGEMENT
;; =============================================================================

(defmacro with-context
  "Execute body with additional log context"
  [context & body]
  `(binding [*log-context* (merge *log-context* ~context)]
     ~@body))

(defmacro with-correlation-id
  "Execute body with correlation ID for request tracing"
  [id & body]
  `(binding [*correlation-id* ~id]
     ~@body))

(defn generate-correlation-id
  "Generate a unique correlation ID"
  []
  (str (java.util.UUID/randomUUID)))

;; =============================================================================
;; PERFORMANCE LOGGING
;; =============================================================================

(defmacro with-timing
  "Log execution time of body"
  [operation-name & body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         elapsed-ms# (/ (- (System/nanoTime) start#) 1000000.0)]
     (info (str ~operation-name " completed")
           :data {:operation ~operation-name
                  :duration_ms elapsed-ms#})
     result#))

(defn log-metrics
  "Log performance metrics"
  [operation metrics]
  (info (str "Metrics: " operation)
        :data (merge {:operation operation} metrics)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-logging!
  "Initialize logging with handlers"
  [& {:keys [level handlers json-output file-path]
      :or {level :info handlers [:console :buffer]}}]
  (alter-var-root #'*log-level* (constantly level))
  (reset! log-handlers
          (vec (keep identity
                     [(when (some #{:console} handlers)
                        (if json-output json-handler console-handler))
                      (when (some #{:buffer} handlers)
                        buffer-handler)
                      (when file-path
                        (file-handler file-path))])))
  (info "Logging initialized" :data {:level level :handlers handlers}))

(defn set-log-level!
  "Change log level at runtime"
  [level]
  (alter-var-root #'*log-level* (constantly level))
  (info "Log level changed" :data {:new-level level}))

;; =============================================================================
;; LOG QUERIES
;; =============================================================================

(defn get-recent-logs
  "Get recent log entries from buffer"
  [& {:keys [limit level] :or {limit 100}}]
  (let [logs @log-buffer
        filtered (if level
                   (filter #(= level (:level %)) logs)
                   logs)]
    (take-last limit filtered)))

(defn get-error-logs
  "Get recent error and fatal logs"
  [& {:keys [limit] :or {limit 50}}]
  (let [logs @log-buffer]
    (take-last limit
               (filter #(#{:error :fatal} (:level %)) logs))))

(defn clear-log-buffer!
  "Clear the log buffer"
  []
  (reset! log-buffer [])
  (info "Log buffer cleared"))

(defn export-logs
  "Export logs as JSON array"
  [& {:keys [limit] :or {limit 1000}}]
  (json/generate-string (take-last limit @log-buffer)))
