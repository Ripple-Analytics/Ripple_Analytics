(ns mental-models.beast.engine
  "BEAST ENGINE - Unified Autonomous Processing System
   
   Combines:
   - Folder devourer (recursive scanning)
   - GPU compute (saturate all hardware)
   - Web scraping (continuous ingestion)
   - Pattern detection (automatic analysis)
   - Distributed mesh (all devices working)
   
   Zero manual interaction. Point and destroy."
  (:require [mental-models.beast.devourer :as devourer]
            [mental-models.beast.gpu :as gpu]
            [mental-models.mesh.core :as mesh]
            [clojure.core.async :as async :refer [go go-loop <! >! chan 
                                                   close! mult tap pipeline]]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration]
           [java.util UUID]))

;; =============================================================================
;; BEAST ENGINE STATE
;; =============================================================================

(defonce engine-state
  (atom {:status :idle
         :mode nil  ; :local, :distributed, :full
         :started-at nil
         :config {}
         :components {:devourer nil
                      :gpu nil
                      :mesh nil
                      :scraper nil}
         :channels {}
         :stats {:total-items 0
                 :cpu-utilization 0.0
                 :gpu-utilization 0.0
                 :network-throughput 0
                 :storage-used-gb 0}}))

;; =============================================================================
;; PROCESSING PIPELINE
;; =============================================================================

(defn create-pipeline!
  "Create the unified processing pipeline"
  []
  (let [;; Input channels
        file-chan (chan 1000000)      ; Files from devourer
        web-chan (chan 100000)        ; Content from web scraper
        
        ;; Processing channels
        extract-chan (chan 100000)    ; Content extraction
        gpu-in-chan (chan 10000)      ; GPU input
        gpu-out-chan (chan 10000)     ; GPU output
        
        ;; Output channels
        pattern-chan (chan 100000)    ; Detected patterns
        storage-chan (chan 100000)    ; For persistence
        
        ;; Multiplexers
        file-mult (mult file-chan)
        result-mult (mult gpu-out-chan)]
    
    ;; Store channels
    (swap! engine-state assoc :channels
           {:file file-chan
            :web web-chan
            :extract extract-chan
            :gpu-in gpu-in-chan
            :gpu-out gpu-out-chan
            :pattern pattern-chan
            :storage storage-chan})
    
    ;; Pipeline: Files -> Extraction
    (pipeline 16 extract-chan
              (map (fn [file-info]
                     (try
                       (assoc file-info 
                              :content (devourer/extract-content file-info)
                              :extracted-at (Instant/now))
                       (catch Exception e
                         (assoc file-info :error (.getMessage e))))))
              file-chan)
    
    ;; Pipeline: Extraction -> GPU
    (pipeline 8 gpu-in-chan
              (comp
                (filter :content)
                (map (fn [item]
                       {:id (:id item)
                        :text (if (map? (:content item))
                                (:content (:content item))
                                (:content item))
                        :metadata (dissoc item :content)})))
              extract-chan)
    
    ;; Start GPU pipeline
    (gpu/start-gpu-pipeline! gpu-in-chan gpu-out-chan)
    
    ;; Pipeline: GPU Results -> Pattern Detection
    (go-loop []
      (when-let [batch-result (<! gpu-out-chan)]
        (doseq [result (:results batch-result)]
          (let [patterns (analyze-patterns result)
                models (match-models result)]
            (>! pattern-chan {:result result
                              :patterns patterns
                              :models models
                              :analyzed-at (Instant/now)})
            (>! storage-chan {:type :analysis-result
                              :data {:result result
                                     :patterns patterns
                                     :models models}})))
        (recur)))
    
    {:file-chan file-chan
     :web-chan web-chan
     :pattern-chan pattern-chan
     :storage-chan storage-chan}))

;; =============================================================================
;; PATTERN ANALYSIS
;; =============================================================================

(defn analyze-patterns
  "Analyze GPU results for patterns"
  [result]
  (let [embedding (:embedding result)
        classification (:classification result)]
    {:embedding-magnitude (Math/sqrt (reduce + (map #(* % %) embedding)))
     :top-classifications (take 10 (sort-by second > 
                                            (map-indexed vector classification)))
     :confidence (apply max classification)
     :entropy (- (reduce + (map #(if (pos? %) (* % (Math/log %)) 0) 
                                classification)))}))

(defn match-models
  "Match result against mental models"
  [result]
  (let [top-models (:top-models result)
        model-names ["compound-interest" "probability" "incentives" 
                     "inversion" "margin-of-safety" "second-order-thinking"
                     "circle-of-competence" "opportunity-cost" "sunk-cost"
                     "confirmation-bias"]]
    (mapv (fn [idx]
            {:model-id idx
             :model-name (get model-names (mod idx (count model-names)))
             :confidence (nth (:classification result) idx 0)})
          (take 5 top-models))))

;; =============================================================================
;; WEB SCRAPER
;; =============================================================================

(defn start-scraper!
  "Start continuous web scraping"
  [web-chan targets]
  (log/info "Starting web scraper with" (count targets) "targets")
  
  (go-loop [idx 0]
    (when (= :running (:status @engine-state))
      ;; Rotate through targets
      (let [target (nth targets (mod idx (count targets)))]
        (try
          ;; Would use actual HTTP client
          (let [content {:url target
                         :content (str "Scraped content from " target)
                         :scraped-at (Instant/now)
                         :id (str (UUID/randomUUID))}]
            (>! web-chan content))
          (catch Exception e
            (log/debug "Scrape error:" target (.getMessage e)))))
      
      ;; Rate limit
      (<! (async/timeout 100))
      (recur (inc idx)))))

(def default-scrape-targets
  "Default targets for continuous scraping"
  [;; Academic
   "https://arxiv.org/list/q-fin/recent"
   "https://arxiv.org/list/econ/recent"
   "https://arxiv.org/list/cs.AI/recent"
   "https://ssrn.com/index.cfm/en/top-downloads/"
   
   ;; News
   "https://news.ycombinator.com/"
   "https://lobste.rs/"
   "https://www.reddit.com/r/investing/.json"
   "https://www.reddit.com/r/economics/.json"
   
   ;; Financial
   "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent"
   "https://finviz.com/news.ashx"
   
   ;; Blogs
   "https://fs.blog/blog/"
   "https://www.collaborativefund.com/blog/"
   "https://www.gatesnotes.com/"
   "https://paulgraham.com/articles.html"])

;; =============================================================================
;; ENGINE CONTROL
;; =============================================================================

(defn unleash!
  "UNLEASH THE BEAST
   
   Modes:
   - :local    - Single machine, all cores + GPU
   - :distributed - All devices in mesh
   - :full     - Everything + web scraping
   
   Usage:
   (unleash! :full [\"/path/to/data\"])"
  [mode targets & {:keys [scrape-targets]
                   :or {scrape-targets default-scrape-targets}}]
  
  (log/info "🔥🔥🔥 UNLEASHING THE BEAST 🔥🔥🔥")
  (log/info "Mode:" mode)
  (log/info "Targets:" targets)
  
  ;; Initialize state
  (swap! engine-state assoc
         :status :running
         :mode mode
         :started-at (Instant/now)
         :config {:targets targets
                  :scrape-targets scrape-targets})
  
  ;; Initialize GPU
  (gpu/init-gpu!)
  
  ;; Create processing pipeline
  (let [pipeline (create-pipeline!)
        file-chan (:file-chan pipeline)
        web-chan (:web-chan pipeline)]
    
    ;; Start folder devourer
    (let [beast (devourer/devour! targets)]
      (swap! engine-state assoc-in [:components :devourer] beast))
    
    ;; Start distributed mesh if requested
    (when (#{:distributed :full} mode)
      (let [mesh-node (mesh/start-mesh!)]
        (swap! engine-state assoc-in [:components :mesh] mesh-node)))
    
    ;; Start web scraper if full mode
    (when (= :full mode)
      (start-scraper! web-chan scrape-targets))
    
    ;; Start stats collector
    (start-stats-collector!)
    
    (log/info "Beast unleashed. Processing...")
    
    {:stop! #(cage!)
     :status #(engine-status)
     :stats #(engine-stats)}))

(defn cage!
  "Stop the beast"
  []
  (log/info "Caging the beast...")
  
  (swap! engine-state assoc :status :stopping)
  
  ;; Stop components
  (when-let [devourer (get-in @engine-state [:components :devourer])]
    ((:stop! devourer)))
  
  (when-let [mesh (get-in @engine-state [:components :mesh])]
    (mesh/stop-mesh!))
  
  (gpu/shutdown-gpu!)
  
  ;; Close channels
  (doseq [[_ ch] (:channels @engine-state)]
    (when ch (close! ch)))
  
  (swap! engine-state assoc :status :stopped)
  (log/info "Beast caged"))

;; =============================================================================
;; STATS & MONITORING
;; =============================================================================

(defn start-stats-collector!
  "Collect and aggregate stats from all components"
  []
  (go-loop []
    (<! (async/timeout 1000))
    (when (= :running (:status @engine-state))
      (let [devourer-stats (when-let [d (get-in @engine-state [:components :devourer])]
                            ((:status d)))
            gpu-stats (gpu/gpu-status)
            mesh-stats (when (get-in @engine-state [:components :mesh])
                        (mesh/mesh-status))]
        
        (swap! engine-state assoc :stats
               {:total-items (get-in devourer-stats [:stats :files-processed] 0)
                :files-discovered (get-in devourer-stats [:stats :files-discovered] 0)
                :files-per-second (get-in devourer-stats [:rate :files-per-second] 0)
                :bytes-processed (get-in devourer-stats [:stats :bytes-processed] 0)
                :cpu-utilization (get-in devourer-stats [:utilization :cpu] 0)
                :gpu-utilization (:avg-utilization gpu-stats 0)
                :gpu-batches (get-in gpu-stats [:stats :batches-processed] 0)
                :mesh-peers (get mesh-stats :peers 0)
                :patterns-found (get-in devourer-stats [:stats :patterns-found] 0)
                :models-matched (get-in devourer-stats [:stats :models-matched] 0)}))
      (recur))))

(defn engine-status
  "Get current engine status"
  []
  (let [s @engine-state
        uptime (when (:started-at s)
                 (.toSeconds (Duration/between (:started-at s) (Instant/now))))]
    {:status (:status s)
     :mode (:mode s)
     :uptime-seconds uptime
     :uptime-human (when uptime
                     (format "%d:%02d:%02d" 
                             (quot uptime 3600)
                             (mod (quot uptime 60) 60)
                             (mod uptime 60)))
     :stats (:stats s)}))

(defn engine-stats
  "Get detailed engine statistics"
  []
  (:stats @engine-state))

;; =============================================================================
;; CLI INTERFACE
;; =============================================================================

(defn -main
  "CLI entry point for beast engine"
  [& args]
  (let [mode (keyword (or (first args) "local"))
        targets (or (rest args) ["."])]
    
    (println "")
    (println "  ╔══════════════════════════════════════╗")
    (println "  ║     🔥 BEAST MODE ACTIVATED 🔥       ║")
    (println "  ╠══════════════════════════════════════╣")
    (println (format "  ║  Mode: %-30s ║" (name mode)))
    (println (format "  ║  Targets: %-27s ║" (count targets)))
    (println (format "  ║  CPUs: %-30s ║" (.availableProcessors (Runtime/getRuntime))))
    (println "  ╚══════════════════════════════════════╝")
    (println "")
    
    (let [beast (unleash! mode (vec targets))]
      
      ;; Print status every 5 seconds
      (go-loop []
        (<! (async/timeout 5000))
        (when (= :running (:status @engine-state))
          (let [s (engine-stats)]
            (println (format "📊 Files: %d/%d | Rate: %d/s | GPU: %.0f%% | Patterns: %d | Models: %d"
                             (:total-items s 0)
                             (:files-discovered s 0)
                             (:files-per-second s 0)
                             (* 100 (:gpu-utilization s 0))
                             (:patterns-found s 0)
                             (:models-matched s 0))))
          (recur)))
      
      ;; Keep running
      (println "Press Ctrl+C to stop")
      @(promise))))
