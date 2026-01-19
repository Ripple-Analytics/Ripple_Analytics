(ns mental-models.pipeline.integration.history-tracker
  "Analysis History Tracker
   
   Tracks analysis history and provides trend analysis:
   - Historical analysis storage
   - Trend detection over time
   - Model frequency analysis
   - Lollapalooza pattern history
   - Statistical summaries"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

;; =============================================================================
;; HISTORY STATE
;; =============================================================================

(defonce history-state (atom {:analyses []
                              :model-frequencies {}
                              :lollapaloozas []
                              :daily-stats {}
                              :max-history-size 10000}))

;; =============================================================================
;; HISTORY STORAGE
;; =============================================================================

(defn add-analysis!
  "Add an analysis result to history."
  [result]
  (when (flags/is-enabled? "history-tracking")
    (let [entry {:id (str (java.util.UUID/randomUUID))
                 :timestamp (System/currentTimeMillis)
                 :file-path (:file-path result)
                 :detections (:detections result)
                 :model-count (count (:detections result))
                 :lollapalooza (boolean (:lollapalooza result))
                 :confidence-avg (when (seq (:detections result))
                                   (/ (reduce + (map :confidence (:detections result)))
                                      (count (:detections result))))}]
      ;; Add to history
      (swap! history-state update :analyses
             (fn [analyses]
               (let [new-analyses (conj analyses entry)]
                 (if (> (count new-analyses) (:max-history-size @history-state))
                   (vec (drop 1000 new-analyses))
                   new-analyses))))
      ;; Update model frequencies
      (doseq [detection (:detections result)]
        (swap! history-state update-in [:model-frequencies (:model-id detection)]
               (fnil inc 0)))
      ;; Track Lollapalooza
      (when (:lollapalooza result)
        (swap! history-state update :lollapaloozas conj
               {:timestamp (System/currentTimeMillis)
                :models (map :model-id (:detections result))
                :confidence (:combined-confidence (:lollapalooza result))}))
      ;; Update daily stats
      (let [day-key (quot (System/currentTimeMillis) 86400000)]
        (swap! history-state update-in [:daily-stats day-key]
               (fn [stats]
                 (-> (or stats {:analyses 0 :models 0 :lollapaloozas 0})
                     (update :analyses inc)
                     (update :models + (count (:detections result)))
                     (update :lollapaloozas + (if (:lollapalooza result) 1 0))))))
      ;; Record metrics
      (metrics/inc-counter! :history/analyses-tracked)
      ;; Publish event
      (events/publish! :history/analysis-added entry)
      entry)))

;; =============================================================================
;; TREND ANALYSIS
;; =============================================================================

(defn get-model-trends
  "Get model detection trends over time."
  [& {:keys [days] :or {days 7}}]
  (let [cutoff (- (System/currentTimeMillis) (* days 86400000))
        recent-analyses (filter #(> (:timestamp %) cutoff) (:analyses @history-state))
        model-counts (reduce (fn [counts analysis]
                               (reduce (fn [c detection]
                                         (update c (:model-id detection) (fnil inc 0)))
                                       counts
                                       (:detections analysis)))
                             {}
                             recent-analyses)]
    {:period-days days
     :total-analyses (count recent-analyses)
     :model-counts (sort-by val > model-counts)
     :top-models (take 10 (sort-by val > model-counts))}))

(defn get-lollapalooza-trends
  "Get Lollapalooza detection trends."
  [& {:keys [days] :or {days 30}}]
  (let [cutoff (- (System/currentTimeMillis) (* days 86400000))
        recent (filter #(> (:timestamp %) cutoff) (:lollapaloozas @history-state))]
    {:period-days days
     :total-lollapaloozas (count recent)
     :avg-per-day (/ (count recent) (max 1 days))
     :common-model-combinations (frequencies (map :models recent))}))

(defn get-confidence-trends
  "Get confidence score trends over time."
  [& {:keys [days] :or {days 7}}]
  (let [cutoff (- (System/currentTimeMillis) (* days 86400000))
        recent (filter #(and (> (:timestamp %) cutoff) (:confidence-avg %))
                       (:analyses @history-state))
        confidences (map :confidence-avg recent)]
    (when (seq confidences)
      {:period-days days
       :sample-size (count confidences)
       :avg-confidence (/ (reduce + confidences) (count confidences))
       :min-confidence (apply min confidences)
       :max-confidence (apply max confidences)})))

;; =============================================================================
;; STATISTICAL SUMMARIES
;; =============================================================================

(defn get-summary-stats []
  "Get summary statistics for all history."
  (let [analyses (:analyses @history-state)
        lollapaloozas (:lollapaloozas @history-state)]
    {:total-analyses (count analyses)
     :total-lollapaloozas (count lollapaloozas)
     :unique-models (count (:model-frequencies @history-state))
     :most-common-models (take 5 (sort-by val > (:model-frequencies @history-state)))
     :lollapalooza-rate (if (pos? (count analyses))
                          (/ (count lollapaloozas) (count analyses))
                          0)}))

(defn get-daily-summary
  "Get summary for a specific day."
  [day-key]
  (get-in @history-state [:daily-stats day-key]))

(defn get-recent-daily-stats
  "Get daily stats for recent days."
  [& {:keys [days] :or {days 7}}]
  (let [today (quot (System/currentTimeMillis) 86400000)
        day-keys (range (- today days) (inc today))]
    (for [day day-keys]
      {:day day
       :date (java.util.Date. (* day 86400000))
       :stats (get-daily-summary day)})))

;; =============================================================================
;; HISTORY QUERIES
;; =============================================================================

(defn get-recent-analyses
  "Get recent analysis results."
  [& {:keys [limit] :or {limit 100}}]
  (take-last limit (:analyses @history-state)))

(defn get-analyses-by-model
  "Get analyses that detected a specific model."
  [model-id & {:keys [limit] :or {limit 100}}]
  (take limit
        (filter (fn [analysis]
                  (some #(= (:model-id %) model-id) (:detections analysis)))
                (reverse (:analyses @history-state)))))

(defn get-analyses-with-lollapalooza
  "Get analyses that detected Lollapalooza effect."
  [& {:keys [limit] :or {limit 50}}]
  (take limit
        (filter :lollapalooza (reverse (:analyses @history-state)))))

(defn search-analyses
  "Search analyses by file path pattern."
  [pattern & {:keys [limit] :or {limit 100}}]
  (let [regex (re-pattern pattern)]
    (take limit
          (filter (fn [analysis]
                    (and (:file-path analysis)
                         (re-find regex (:file-path analysis))))
                  (reverse (:analyses @history-state))))))

;; =============================================================================
;; PERSISTENCE
;; =============================================================================

(defn save-history!
  "Save history to disk."
  [file-path]
  (log/info "Saving history" {:path file-path})
  (io/make-parents file-path)
  (spit file-path (pr-str @history-state))
  (log/info "History saved" {:analyses (count (:analyses @history-state))}))

(defn load-history!
  "Load history from disk."
  [file-path]
  (when (.exists (io/file file-path))
    (log/info "Loading history" {:path file-path})
    (let [data (edn/read-string (slurp file-path))]
      (reset! history-state data)
      (log/info "History loaded" {:analyses (count (:analyses @history-state))}))))

;; =============================================================================
;; EVENT HANDLERS
;; =============================================================================

(defn setup-event-handlers! []
  (log/info "Setting up history event handlers")
  (events/subscribe! :analysis/complete add-analysis!)
  (events/subscribe! :batch/complete
                     (fn [batch-result]
                       (doseq [result (:results batch-result)]
                         (add-analysis! result)))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-history-tracker!
  "Initialize history tracker."
  [& {:keys [history-file] :or {history-file ".history/analysis-history.edn"}}]
  (log/info "Initializing history tracker")
  ;; Load existing history
  (load-history! history-file)
  ;; Register feature flag
  (flags/register-flag! "history-tracking" "Enable history tracking" true)
  ;; Create metrics
  (metrics/create-counter! :history/analyses-tracked "Analyses tracked")
  (metrics/create-gauge! :history/total-analyses "Total analyses" #(count (:analyses @history-state)))
  ;; Setup event handlers
  (setup-event-handlers!)
  (log/info "History tracker initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-tracker-status []
  {:enabled (flags/is-enabled? "history-tracking")
   :total-analyses (count (:analyses @history-state))
   :total-lollapaloozas (count (:lollapaloozas @history-state))
   :unique-models (count (:model-frequencies @history-state))})
