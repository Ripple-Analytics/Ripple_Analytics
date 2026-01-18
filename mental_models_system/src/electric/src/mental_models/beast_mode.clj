(ns mental-models.beast-mode
  "Beast Mode Engine - Electric Clojure
   
   Maximum performance continuous learning mode.
   When activated, all compute resources strain for maximum learning.
   
   Features:
   - Continuous data harvesting and analysis
   - Parallel processing across all available cores
   - Real-time learning from sensors, apps, and web
   - Automatic model improvement
   - 24/7 operation with background refresh
   
   Architecture:
   ┌─────────────────────────────────────────────────────────────────┐
   │                      BEAST MODE ENGINE                          │
   ├─────────────────────────────────────────────────────────────────┤
   │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │
   │  │   Sensors   │  │    Apps     │  │  Web Data   │             │
   │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘             │
   │         └────────────────┼────────────────┘                     │
   │                          ▼                                      │
   │              ┌───────────────────────┐                         │
   │              │   Data Ingestion      │                         │
   │              │   (Continuous Stream) │                         │
   │              └───────────┬───────────┘                         │
   │                          ▼                                      │
   │  ┌───────────────────────────────────────────────┐             │
   │  │           Parallel Processing Pool             │             │
   │  │  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐     │             │
   │  │  │ W1  │ │ W2  │ │ W3  │ │ W4  │ │ ... │     │             │
   │  │  └─────┘ └─────┘ └─────┘ └─────┘ └─────┘     │             │
   │  └───────────────────────┬───────────────────────┘             │
   │                          ▼                                      │
   │              ┌───────────────────────┐                         │
   │              │   Mental Model        │                         │
   │              │   Analysis Pipeline   │                         │
   │              └───────────┬───────────┘                         │
   │                          ▼                                      │
   │              ┌───────────────────────┐                         │
   │              │   Learning & Update   │                         │
   │              │   (Model Improvement) │                         │
   │              └───────────────────────┘                         │
   └─────────────────────────────────────────────────────────────────┘"
  (:require [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop chan <! >! <!! >!! close! timeout]]
            [mental-models.algorithms :as algo]
            [mental-models.llm :as llm])
  (:import [java.util.concurrent Executors ExecutorService]
           [java.time Instant Duration]))

;; ============================================
;; Beast Mode State
;; ============================================

(defonce beast-mode-state
  (atom {:active false
         :started-at nil
         :stats {:data-points-processed 0
                 :models-applied 0
                 :insights-generated 0
                 :learning-cycles 0
                 :errors 0}
         :config {:max-workers 8
                  :batch-size 100
                  :learning-interval-ms 5000
                  :auto-save-interval-ms 60000}
         :workers []
         :channels {:input nil
                    :output nil
                    :control nil}}))

(defonce learning-history
  (atom []))

;; ============================================
;; Configuration
;; ============================================

(def default-config
  {:max-workers (max 2 (- (.availableProcessors (Runtime/getRuntime)) 1))
   :batch-size 100
   :learning-interval-ms 5000
   :auto-save-interval-ms 60000
   :max-queue-size 10000
   :llm-enabled true
   :continuous-scraping true
   :sensor-integration true})

(defn configure-beast-mode
  "Configure Beast Mode settings."
  [config]
  (swap! beast-mode-state update :config merge config))

;; ============================================
;; Data Sources
;; ============================================

(defprotocol DataSource
  "Protocol for data sources."
  (fetch-data [this] "Fetch data from the source.")
  (source-name [this] "Get the source name."))

(defrecord FileWatcher [path patterns]
  DataSource
  (fetch-data [this]
    ;; Watch directory for new files
    (try
      (let [dir (java.io.File. path)
            files (when (.exists dir)
                    (->> (.listFiles dir)
                         (filter #(.isFile %))
                         (filter #(some (fn [p] (re-matches (re-pattern p) (.getName %))) patterns))
                         (map #(.getAbsolutePath %))))]
        {:source "file-watcher"
         :path path
         :files (vec files)
         :count (count files)})
      (catch Exception e
        {:source "file-watcher" :error (.getMessage e)})))
  (source-name [this] (str "FileWatcher:" path)))

(defrecord WebScraper [urls]
  DataSource
  (fetch-data [this]
    ;; Placeholder for web scraping
    {:source "web-scraper"
     :urls urls
     :status "ready"})
  (source-name [this] "WebScraper"))

(defrecord SensorStream [sensor-type]
  DataSource
  (fetch-data [this]
    ;; Placeholder for sensor data
    {:source "sensor"
     :type sensor-type
     :timestamp (System/currentTimeMillis)
     :data {:value (rand) :unit "normalized"}})
  (source-name [this] (str "Sensor:" sensor-type)))

;; ============================================
;; Processing Pipeline
;; ============================================

(defn process-data-point
  "Process a single data point through the mental models pipeline."
  [data-point]
  (try
    (let [text (cond
                 (string? data-point) data-point
                 (map? data-point) (or (:content data-point) 
                                       (:text data-point)
                                       (str data-point))
                 :else (str data-point))
          ;; Run through algorithms
          relevant-models (algo/get-top-models text 3)
          lollapalooza (algo/detect-lollapalooza text)
          failure-modes (algo/analyze-failure-modes text)]
      {:success true
       :timestamp (System/currentTimeMillis)
       :input-length (count text)
       :relevant-models (map :model relevant-models)
       :lollapalooza-detected (seq lollapalooza)
       :failure-modes-detected (count failure-modes)
       :insights (when (seq relevant-models)
                   (str "Top model: " (:model (first relevant-models))
                        " (relevance: " (format "%.2f" (:relevance (first relevant-models))) ")"))})
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :timestamp (System/currentTimeMillis)})))

(defn process-batch
  "Process a batch of data points in parallel."
  [data-points]
  (let [results (pmap process-data-point data-points)
        successful (filter :success results)
        failed (remove :success results)]
    {:total (count data-points)
     :successful (count successful)
     :failed (count failed)
     :results successful
     :errors (map :error failed)}))

;; ============================================
;; Worker Pool
;; ============================================

(defn create-worker
  "Create a worker that processes data from the input channel."
  [worker-id input-chan output-chan control-chan]
  (go-loop []
    (let [[v ch] (async/alts! [input-chan control-chan])]
      (cond
        (= ch control-chan)
        (do
          (println (str "[Worker " worker-id "] Shutting down"))
          :shutdown)
        
        (nil? v)
        (do
          (println (str "[Worker " worker-id "] Input channel closed"))
          :closed)
        
        :else
        (do
          (let [result (process-data-point v)]
            (>! output-chan {:worker-id worker-id :result result})
            (swap! beast-mode-state update-in [:stats :data-points-processed] inc)
            (when (:success result)
              (swap! beast-mode-state update-in [:stats :models-applied] 
                     + (count (:relevant-models result)))))
          (recur))))))

(defn start-worker-pool
  "Start the worker pool."
  [n-workers]
  (let [input-chan (chan 1000)
        output-chan (chan 1000)
        control-chan (chan)]
    (swap! beast-mode-state assoc-in [:channels :input] input-chan)
    (swap! beast-mode-state assoc-in [:channels :output] output-chan)
    (swap! beast-mode-state assoc-in [:channels :control] control-chan)
    
    (let [workers (doall
                   (for [i (range n-workers)]
                     (create-worker i input-chan output-chan control-chan)))]
      (swap! beast-mode-state assoc :workers workers)
      {:input input-chan
       :output output-chan
       :control control-chan
       :worker-count n-workers})))

(defn stop-worker-pool
  "Stop the worker pool."
  []
  (when-let [control-chan (get-in @beast-mode-state [:channels :control])]
    (dotimes [_ (count (:workers @beast-mode-state))]
      (>!! control-chan :shutdown))
    (close! control-chan))
  (when-let [input-chan (get-in @beast-mode-state [:channels :input])]
    (close! input-chan))
  (when-let [output-chan (get-in @beast-mode-state [:channels :output])]
    (close! output-chan))
  (swap! beast-mode-state assoc :workers []))

;; ============================================
;; Continuous Learning Loop
;; ============================================

(defn learning-cycle
  "Execute one learning cycle."
  []
  (let [state @beast-mode-state
        stats (:stats state)]
    ;; Increment learning cycle counter
    (swap! beast-mode-state update-in [:stats :learning-cycles] inc)
    
    ;; Record learning history
    (swap! learning-history conj
           {:timestamp (System/currentTimeMillis)
            :cycle (get-in @beast-mode-state [:stats :learning-cycles])
            :data-points (:data-points-processed stats)
            :models-applied (:models-applied stats)
            :insights (:insights-generated stats)})
    
    ;; Keep only last 1000 history entries
    (when (> (count @learning-history) 1000)
      (swap! learning-history #(vec (take-last 1000 %))))
    
    {:cycle (get-in @beast-mode-state [:stats :learning-cycles])
     :stats (:stats @beast-mode-state)}))

(defn start-learning-loop
  "Start the continuous learning loop."
  []
  (let [interval-ms (get-in @beast-mode-state [:config :learning-interval-ms] 5000)]
    (go-loop []
      (when (:active @beast-mode-state)
        (<! (timeout interval-ms))
        (learning-cycle)
        (recur)))))

;; ============================================
;; Beast Mode Control
;; ============================================

(defn activate-beast-mode!
  "Activate Beast Mode - maximum performance continuous learning."
  [& {:keys [config] :or {config {}}}]
  (when-not (:active @beast-mode-state)
    (println "[BEAST MODE] ACTIVATING - Maximum performance enabled")
    
    ;; Apply configuration
    (configure-beast-mode (merge default-config config))
    
    ;; Start worker pool
    (let [n-workers (get-in @beast-mode-state [:config :max-workers])]
      (start-worker-pool n-workers)
      (println (str "[BEAST MODE] Started " n-workers " workers")))
    
    ;; Update state
    (swap! beast-mode-state assoc
           :active true
           :started-at (System/currentTimeMillis))
    
    ;; Start learning loop
    (start-learning-loop)
    
    {:status "activated"
     :workers (get-in @beast-mode-state [:config :max-workers])
     :started-at (:started-at @beast-mode-state)}))

(defn deactivate-beast-mode!
  "Deactivate Beast Mode."
  []
  (when (:active @beast-mode-state)
    (println "[BEAST MODE] DEACTIVATING")
    
    ;; Stop worker pool
    (stop-worker-pool)
    
    ;; Update state
    (swap! beast-mode-state assoc :active false)
    
    {:status "deactivated"
     :final-stats (:stats @beast-mode-state)
     :duration-ms (when-let [started (:started-at @beast-mode-state)]
                    (- (System/currentTimeMillis) started))}))

(defn beast-mode-status
  "Get current Beast Mode status."
  []
  (let [state @beast-mode-state]
    {:active (:active state)
     :started-at (:started-at state)
     :duration-ms (when (and (:active state) (:started-at state))
                    (- (System/currentTimeMillis) (:started-at state)))
     :stats (:stats state)
     :config (:config state)
     :worker-count (count (:workers state))
     :learning-history-size (count @learning-history)}))

;; ============================================
;; Data Ingestion
;; ============================================

(defn ingest-data!
  "Ingest data into the Beast Mode processing pipeline."
  [data]
  (when (:active @beast-mode-state)
    (let [input-chan (get-in @beast-mode-state [:channels :input])]
      (if (sequential? data)
        (doseq [item data]
          (>!! input-chan item))
        (>!! input-chan data))
      {:ingested (if (sequential? data) (count data) 1)
       :queue-size 0}))) ; Would need to track actual queue size

(defn ingest-file!
  "Ingest a file into the processing pipeline."
  [file-path]
  (try
    (let [content (slurp file-path)]
      (ingest-data! content)
      {:success true :file file-path :size (count content)})
    (catch Exception e
      {:success false :file file-path :error (.getMessage e)})))

(defn ingest-directory!
  "Ingest all files from a directory."
  [dir-path & {:keys [pattern] :or {pattern ".*\\.txt$"}}]
  (let [dir (java.io.File. dir-path)
        files (when (.exists dir)
                (->> (.listFiles dir)
                     (filter #(.isFile %))
                     (filter #(re-matches (re-pattern pattern) (.getName %)))
                     (map #(.getAbsolutePath %))))]
    (doseq [file files]
      (ingest-file! file))
    {:directory dir-path
     :files-ingested (count files)}))

;; ============================================
;; Metrics and Monitoring
;; ============================================

(defn get-metrics
  "Get current Beast Mode metrics."
  []
  (let [state @beast-mode-state
        stats (:stats state)
        history @learning-history
        recent-history (take-last 10 history)]
    {:current-stats stats
     :throughput (when (seq recent-history)
                   (let [time-span (- (:timestamp (last recent-history))
                                      (:timestamp (first recent-history)))]
                     (when (pos? time-span)
                       {:data-points-per-second (/ (* 1000 (:data-points-processed stats))
                                                   time-span)
                        :models-per-second (/ (* 1000 (:models-applied stats))
                                              time-span)})))
     :learning-cycles (:learning-cycles stats)
     :uptime-ms (when (:started-at state)
                  (- (System/currentTimeMillis) (:started-at state)))}))

(defn get-learning-history
  "Get learning history."
  [& {:keys [limit] :or {limit 100}}]
  (take-last limit @learning-history))

;; ============================================
;; API Functions for Electric Clojure UI
;; ============================================

(defn toggle-beast-mode!
  "Toggle Beast Mode on/off."
  []
  (if (:active @beast-mode-state)
    (deactivate-beast-mode!)
    (activate-beast-mode!)))

(defn quick-status
  "Quick status check for UI."
  []
  {:active (:active @beast-mode-state)
   :data-points (get-in @beast-mode-state [:stats :data-points-processed])
   :models-applied (get-in @beast-mode-state [:stats :models-applied])
   :learning-cycles (get-in @beast-mode-state [:stats :learning-cycles])})
