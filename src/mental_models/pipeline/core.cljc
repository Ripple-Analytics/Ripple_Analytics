(ns mental-models.pipeline.core
  "End-to-End Analysis Pipeline
   
   Connects: folder watcher -> file extraction -> LM Studio analysis 
             -> database storage -> web app display
   
   Features:
   - Real-time streaming via WebSocket
   - Lollapalooza alerts when 3+ biases converge
   - Progress tracking and resumable processing
   - Parallel processing with core.async"
  (:require
   [clojure.core.async :as async :refer [go go-loop chan <! >! put! close! timeout mult tap]]
   [clojure.string :as str]
   [mental-models.scanner.auto-scanner :as scanner]
   [mental-models.scanner.file-extractors :as extractors]
   [mental-models.llm.lm-studio :as lm-studio]
   [mental-models.alerts.lollapalooza :as lollapalooza]
   #?(:clj [clojure.java.io :as io])
   #?(:clj [hyperfiddle.electric :as e])))

;; =============================================================================
;; PIPELINE STATE
;; =============================================================================

(defonce pipeline-state
  (atom {:running false
         :stage "idle"
         :progress {:total 0 :completed 0 :percentage 0}
         :current-file nil
         :stats {:files-processed 0
                 :lollapalooza-detected 0
                 :errors 0
                 :avg-processing-time-ms 0}
         :recent-results []
         :recent-alerts []}))

;; =============================================================================
;; WEBSOCKET STREAMING CHANNELS
;; =============================================================================

(defonce websocket-channel (chan 1000))
(defonce websocket-mult (mult websocket-channel))

(defn create-websocket-tap
  "Create a new tap for a WebSocket client"
  []
  (let [client-chan (chan 100)]
    (tap websocket-mult client-chan)
    client-chan))

(defn broadcast!
  "Broadcast message to all WebSocket clients"
  [message]
  (put! websocket-channel message))

;; =============================================================================
;; PIPELINE STAGES
;; =============================================================================

#?(:clj
   (defn stage-extract-text
     "Stage 1: Extract text from file"
     [file-path]
     (broadcast! {:type :progress :stage "extracting" :file file-path})
     (swap! pipeline-state assoc :stage "extracting" :current-file file-path)
     (let [text (extractors/extract-and-preprocess file-path)
           metadata (extractors/get-file-metadata file-path)]
       (when text
         (broadcast! {:type :extraction-complete 
                      :file file-path 
                      :text-length (count text)
                      :metadata metadata}))
       {:text text :metadata metadata :file-path file-path})))

#?(:clj
   (defn stage-analyze-llm
     "Stage 2: Analyze with LM Studio"
     [{:keys [text file-path] :as data}]
     (when text
       (broadcast! {:type :progress :stage "analyzing" :file file-path})
       (swap! pipeline-state assoc :stage "analyzing")
       (let [start-time (System/currentTimeMillis)
             stream-chan (lm-studio/stream-analysis text)]
         (go-loop []
           (when-let [msg (<! stream-chan)]
             (broadcast! {:type :analysis-token :data msg :file file-path})
             (when (not= :done (:type msg))
               (recur))))
         (let [biases (lm-studio/analyze-for-all-biases text)
               lollapalooza-result (lm-studio/detect-lollapalooza text)
               elapsed (- (System/currentTimeMillis) start-time)]
           (broadcast! {:type :analysis-complete 
                        :file file-path 
                        :biases biases
                        :lollapalooza lollapalooza-result
                        :elapsed-ms elapsed})
           (assoc data 
                  :biases biases 
                  :lollapalooza lollapalooza-result
                  :analysis-time-ms elapsed))))))

#?(:clj
   (defn stage-check-lollapalooza
     "Stage 3: Check for Lollapalooza and dispatch alerts"
     [{:keys [biases lollapalooza file-path metadata] :as data}]
     (when data
       (broadcast! {:type :progress :stage "checking-lollapalooza" :file file-path})
       (swap! pipeline-state assoc :stage "checking-lollapalooza")
       (when (:is_lollapalooza lollapalooza)
         (let [alert-result (lollapalooza/dispatch-alert! 
                             lollapalooza 
                             {:name file-path :metadata metadata})]
           (broadcast! {:type :lollapalooza-alert :file file-path :alert alert-result})
           (swap! pipeline-state update-in [:stats :lollapalooza-detected] inc)
           (swap! pipeline-state update :recent-alerts 
                  #(take 50 (conj % {:file file-path 
                                     :alert lollapalooza 
                                     :timestamp (System/currentTimeMillis)})))))
       data)))

#?(:clj
   (defn stage-store-results
     "Stage 4: Store results to database"
     [{:keys [text file-path biases lollapalooza metadata analysis-time-ms] :as data}]
     (when data
       (broadcast! {:type :progress :stage "storing" :file file-path})
       (swap! pipeline-state assoc :stage "storing")
       (let [result {:id (str (java.util.UUID/randomUUID))
                     :file-path file-path
                     :text-length (count text)
                     :text-preview (subs text 0 (min 500 (count text)))
                     :biases biases
                     :lollapalooza lollapalooza
                     :metadata metadata
                     :analysis-time-ms analysis-time-ms
                     :timestamp (System/currentTimeMillis)}]
         (broadcast! {:type :stored :file file-path :result-id (:id result)})
         (swap! pipeline-state update :recent-results #(take 100 (conj % result)))
         result))))

;; =============================================================================
;; FULL PIPELINE
;; =============================================================================

#?(:clj
   (defn process-file-pipeline
     "Run full pipeline on a single file"
     [file-path]
     (let [start-time (System/currentTimeMillis)]
       (try
         (broadcast! {:type :pipeline-start :file file-path})
         (-> file-path
             stage-extract-text
             stage-analyze-llm
             stage-check-lollapalooza
             stage-store-results)
         (let [elapsed (- (System/currentTimeMillis) start-time)]
           (swap! pipeline-state update-in [:stats :files-processed] inc)
           (broadcast! {:type :pipeline-complete :file file-path :elapsed-ms elapsed})
           (let [total (get-in @pipeline-state [:stats :files-processed])
                 current-avg (get-in @pipeline-state [:stats :avg-processing-time-ms] 0)
                 new-avg (/ (+ (* current-avg (dec total)) elapsed) total)]
             (swap! pipeline-state assoc-in [:stats :avg-processing-time-ms] new-avg)))
         (catch Exception e
           (swap! pipeline-state update-in [:stats :errors] inc)
           (broadcast! {:type :pipeline-error :file file-path :error (.getMessage e)})
           nil)))))

#?(:clj
   (defn process-batch-pipeline
     "Process multiple files through the pipeline in parallel"
     [file-paths & {:keys [concurrency] :or {concurrency 4}}]
     (let [total (count file-paths)
           work-chan (chan)
           result-chan (chan 100)
           completed (atom 0)]
       (swap! pipeline-state assoc :running true :progress {:total total :completed 0 :percentage 0})
       (broadcast! {:type :batch-start :total total})
       (go
         (doseq [file file-paths]
           (>! work-chan file))
         (close! work-chan))
       (dotimes [_ concurrency]
         (go-loop []
           (when-let [file (<! work-chan)]
             (let [result (process-file-pipeline file)]
               (swap! completed inc)
               (let [pct (int (* 100 (/ @completed total)))]
                 (swap! pipeline-state assoc :progress {:total total :completed @completed :percentage pct})
                 (broadcast! {:type :batch-progress :completed @completed :total total :percentage pct}))
               (when result (>! result-chan result)))
             (recur))))
       (go-loop [results []]
         (if (< (count results) total)
           (if-let [result (<! result-chan)]
             (recur (conj results result))
             results)
           (do
             (swap! pipeline-state assoc :running false :stage "complete")
             (broadcast! {:type :batch-complete :total total :successful (count results)})
             results))))))

;; =============================================================================
;; INTEGRATION WITH SCANNER
;; =============================================================================

#?(:clj
   (defn start-pipeline!
     "Start the full pipeline with folder watching"
     [{:keys [watch-dirs llm-endpoint concurrency] :as config}]
     (println "[PIPELINE] Starting end-to-end analysis pipeline...")
     (lm-studio/init! {:url (or llm-endpoint "http://localhost:1234")})
     (scanner/init! (merge config {:use-llm true :concurrency (or concurrency 4)}))
     (lollapalooza/start-alert-processor!)
     (go-loop []
       (when-let [result (<! scanner/result-channel)]
         (broadcast! {:type :scanner-result :data result})
         (recur)))
     (go-loop []
       (when-let [progress (<! scanner/progress-channel)]
         (broadcast! {:type :scanner-progress :data progress})
         (recur)))
     (swap! pipeline-state assoc :running true)
     (println "[PIPELINE] Pipeline started successfully")))

#?(:clj
   (defn stop-pipeline!
     "Stop the pipeline"
     []
     (scanner/stop-scanner!)
     (lm-studio/shutdown!)
     (swap! pipeline-state assoc :running false :stage "stopped")
     (broadcast! {:type :pipeline-stopped})
     (println "[PIPELINE] Pipeline stopped")))

;; =============================================================================
;; API
;; =============================================================================

(defn get-pipeline-status [] @pipeline-state)
(defn get-recent-results [n] (take n (:recent-results @pipeline-state)))
(defn get-recent-alerts [n] (take n (:recent-alerts @pipeline-state)))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

#?(:clj
   (e/defn PipelineStatus []
     (e/server
      (let [status (e/watch pipeline-state)]
        (e/client
         [:div.pipeline-status
          [:div.header
           [:h3 "Analysis Pipeline"]
           [:span.status {:class (if (:running status) "running" "stopped")}
            (if (:running status) "RUNNING" "STOPPED")]]
          [:div.progress
           [:div.progress-bar {:style {:width (str (get-in status [:progress :percentage]) "%")}}]
           [:span (str (get-in status [:progress :completed]) "/" (get-in status [:progress :total]) " files")]]
          [:div.stage
           [:span "Stage: " (:stage status)]
           (when (:current-file status) [:span " - " (:current-file status)])]
          [:div.stats
           [:div [:span "Processed: "] [:strong (get-in status [:stats :files-processed])]]
           [:div [:span "Lollapalooza: "] [:strong (get-in status [:stats :lollapalooza-detected])]]
           [:div [:span "Errors: "] [:strong (get-in status [:stats :errors])]]
           [:div [:span "Avg Time: "] [:strong (str (int (get-in status [:stats :avg-processing-time-ms])) "ms")]]]])))))

#?(:clj
   (e/defn RecentAlerts []
     (e/server
      (let [alerts (:recent-alerts (e/watch pipeline-state))]
        (e/client
         [:div.recent-alerts
          [:h3 "Recent Lollapalooza Alerts"]
          (if (empty? alerts)
            [:p.no-alerts "No alerts yet"]
            [:ul.alert-list
             (e/for [alert alerts]
               [:li.alert-item {:class (if (get-in alert [:alert :critical?]) "critical" "warning")}
                [:span.file (:file alert)]
                [:span.models (str (count (get-in alert [:alert :converging-models])) " models")]
                [:span.time (str (- (System/currentTimeMillis) (:timestamp alert)) "ms ago")]])])])))))
