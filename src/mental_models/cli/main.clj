(ns mental-models.cli.main
  "Command-Line Interface for Mental Models Pipeline
   
   Provides CLI commands for:
   - Starting/stopping the pipeline
   - Running batch analysis
   - Checking health status
   - Managing configuration
   - Viewing metrics and logs"
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [mental-models.config.pipeline-config :as config]
   [mental-models.health.checks :as health]
   [mental-models.benchmark.performance :as perf]
   [mental-models.scanner.auto-scanner :as scanner]
   [mental-models.pipeline.core :as pipeline])
  (:gen-class))

;; =============================================================================
;; CLI OPTIONS
;; =============================================================================

(def cli-options
  [[nil "--config FILE" "Configuration file path"
    :default "config.edn"]
   [nil "--profile PROFILE" "Configuration profile (development, testing, production)"
    :parse-fn keyword]
   [nil "--watch-dir DIR" "Directory to watch for files"
    :assoc-fn (fn [m k v] (update m k (fnil conj []) v))]
   [nil "--concurrency N" "Number of parallel workers"
    :parse-fn #(Integer/parseInt %)
    :default 4]
   [nil "--lm-studio-url URL" "LM Studio server URL"
    :default "http://localhost:1234"]
   [nil "--no-database" "Disable database storage"
    :id :disable-database]
   [nil "--no-notifications" "Disable notifications"
    :id :disable-notifications]
   ["-v" "--verbose" "Enable verbose output"]
   ["-h" "--help" "Show help"]])

;; =============================================================================
;; COMMAND HANDLERS
;; =============================================================================

(defn cmd-start
  "Start the analysis pipeline"
  [options]
  (println "Starting Mental Models Pipeline...")
  (config/init-config! :config-file (:config options))
  (when (:profile options)
    (config/apply-profile! (:profile options)))
  (when (:watch-dir options)
    (config/update-config! {:watch-dirs (:watch-dir options)}))
  (when (:concurrency options)
    (config/update-config! {:concurrency (:concurrency options)}))
  (when (:lm-studio-url options)
    (config/update-config! {:lm-studio-url (:lm-studio-url options)}))
  (when (:disable-database options)
    (config/update-config! {:enable-database false}))
  (when (:disable-notifications options)
    (config/update-config! {:enable-notifications false}))
  
  (println "Configuration loaded:")
  (println "  Watch dirs:" (config/get-config :watch-dirs))
  (println "  Concurrency:" (config/get-config :concurrency))
  (println "  LM Studio:" (config/get-config :lm-studio-url))
  
  (health/start-health-monitoring! 30000)
  (println "Health monitoring started")
  
  ;; Start the pipeline
  (println "Pipeline started. Press Ctrl+C to stop.")
  (scanner/start-watching! (config/get-config :watch-dirs))
  
  ;; Keep running until interrupted
  (while true
    (Thread/sleep 1000)))

(defn cmd-analyze
  "Analyze a single file or directory"
  [args options]
  (let [path (first args)]
    (if (nil? path)
      (println "Error: Please specify a file or directory to analyze")
      (do
        (println "Analyzing:" path)
        (config/init-config! :config-file (:config options))
        (let [start-time (System/currentTimeMillis)
              results (if (.isDirectory (java.io.File. path))
                        (scanner/scan-and-process-directory path)
                        (pipeline/process-file-pipeline path))
              elapsed (- (System/currentTimeMillis) start-time)]
          (println "
Analysis complete in" elapsed "ms")
          (if (map? results)
            (do
              (println "File:" (:file-path results))
              (println "Models detected:" (count (:biases results)))
              (when (get-in results [:lollapalooza :is_lollapalooza])
                (println "LOLLAPALOOZA DETECTED!")))
            (do
              (println "Files processed:" (count results))
              (println "Lollapalooza events:" (count (filter #(get-in % [:lollapalooza :is_lollapalooza]) results))))))))))

(defn cmd-batch
  "Run batch analysis on multiple files"
  [args options]
  (let [dir (first args)
        batch-size (or (:batch-size options) 100)]
    (if (nil? dir)
      (println "Error: Please specify a directory for batch analysis")
      (do
        (println "Running batch analysis on:" dir)
        (println "Batch size:" batch-size)
        (config/init-config! :config-file (:config options))
        (perf/start-benchmark! batch-size)
        (let [results (scanner/scan-and-process-directory dir :batch-size batch-size)
              final-results (perf/stop-benchmark!)]
          (println "
" (perf/format-benchmark-report final-results)))))))

(defn cmd-health
  "Check pipeline health status"
  [options]
  (println "Checking pipeline health...")
  (config/init-config! :config-file (:config options))
  (let [result (health/run-all-checks)]
    (println "
Overall Status:" (name (:overall-status result)))
    (println "
Component Status:")
    (doseq [[component status] (:components result)]
      (println "  " (name component) "-" (name (:status status)) "-" (:message status)))
    (println "
Probes:")
    (println "  Liveness:" (if (:alive (health/liveness-probe)) "PASS" "FAIL"))
    (println "  Readiness:" (if (:ready (health/readiness-probe)) "PASS" "FAIL"))
    (println "  Startup:" (if (:started (health/startup-probe)) "PASS" "FAIL"))))

(defn cmd-config
  "Show or update configuration"
  [args options]
  (config/init-config! :config-file (:config options))
  (case (first args)
    "show" (do
              (println "Current Configuration:")
              (doseq [[k v] (sort (config/get-config))]
                (println "  " k ":" v)))
    "set" (let [[_ key value] args]
             (if (and key value)
               (do
                 (config/update-config! {(keyword key) (read-string value)})
                 (println "Updated" key "to" value))
               (println "Usage: config set <key> <value>")))
    "profile" (let [profile (keyword (second args))]
                (config/apply-profile! profile)
                (println "Applied profile:" profile))
    "export" (let [filepath (or (second args) "config-export.edn")]
               (config/save-config-file! filepath (config/get-config))
               (println "Exported to" filepath))
    (println "Usage: config [show|set|profile|export]")))

(defn cmd-metrics
  "Show pipeline metrics"
  [options]
  (println "Pipeline Metrics:")
  (let [results (perf/get-benchmark-results)]
    (println (perf/format-benchmark-report results))))

(defn cmd-version
  "Show version information"
  []
  (println "Mental Models Pipeline v1.0.0")
  (println "Built with Electric Clojure")
  (println "https://github.com/Ripple-Analytics/Ripple_Analytics"))

;; =============================================================================
;; HELP TEXT
;; =============================================================================

(def help-text
  "Mental Models Pipeline CLI

Usage: mental-models <command> [options] [args]

Commands:
  start       Start the analysis pipeline
  analyze     Analyze a single file or directory
  batch       Run batch analysis with benchmarking
  health      Check pipeline health status
  config      Show or update configuration
  metrics     Show pipeline metrics
  version     Show version information

Examples:
  mental-models start --watch-dir /data/documents
  mental-models analyze /path/to/document.txt
  mental-models batch /data/documents --concurrency 8
  mental-models health
  mental-models config show
  mental-models config set concurrency 8
  mental-models config profile production

Options:")

;; =============================================================================
;; MAIN ENTRY POINT
;; =============================================================================

(defn -main
  "Main entry point for CLI"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        command (first arguments)
        cmd-args (rest arguments)]
    
    (cond
      (:help options)
      (do
        (println help-text)
        (println summary))
      
      errors
      (do
        (println "Errors:")
        (doseq [e errors]
          (println "  " e))
        (System/exit 1))
      
      :else
      (case command
        "start" (cmd-start options)
        "analyze" (cmd-analyze cmd-args options)
        "batch" (cmd-batch cmd-args options)
        "health" (cmd-health options)
        "config" (cmd-config cmd-args options)
        "metrics" (cmd-metrics options)
        "version" (cmd-version)
        nil (do
              (println help-text)
              (println summary))
        (do
          (println "Unknown command:" command)
          (println "Run with --help for usage information")
          (System/exit 1))))))
