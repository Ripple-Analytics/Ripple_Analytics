(ns mental-models.db.pipeline-storage
  "Database storage integration for the analysis pipeline
   
   Provides a clean interface between the pipeline and database layer.
   Wraps db.analysis functions with pipeline-specific logic."
  (:require [mental-models.db.analysis :as db-analysis]
            [mental-models.db.postgres :as db]
            [cheshire.core :as json]
            [taoensso.timbre :as log])
  (:import [java.time Instant]))

;; =============================================================================
;; PIPELINE RESULT STORAGE
;; =============================================================================

(defn save-pipeline-result!
  "Save a pipeline analysis result to the database.
   
   Takes the result map from process-file-pipeline and persists it.
   Returns the saved analysis ID or nil on failure."
  [{:keys [id file-path text text-length biases lollapalooza metadata 
           analysis-time-ms timestamp] :as result}
   & {:keys [user-id device-id] :or {user-id "system" device-id "pipeline"}}]
  (try
    (let [is-lollapalooza (get-in lollapalooza [:is_lollapalooza] false)
          converging-models (when is-lollapalooza
                              (filter #(> (:confidence % 0) 0.7) 
                                      (get lollapalooza :converging-models [])))
          convergence-score (when is-lollapalooza
                              (get lollapalooza :convergence-score 0))
          convergence-count (when is-lollapalooza
                              (count converging-models))
          ;; Calculate average score from biases
          bias-scores (map #(get % :confidence 0) (vals biases))
          avg-score (if (seq bias-scores)
                      (/ (reduce + bias-scores) (count bias-scores))
                      0.0)
          ;; Build top 10 models by confidence
          top-10-models (->> biases
                             (map (fn [[k v]] 
                                    {:model-slug (name k)
                                     :model-name (get v :name (name k))
                                     :score (get v :confidence 0)
                                     :success (> (get v :confidence 0) 0.5)}))
                             (sort-by :score >)
                             (take 10)
                             vec)
          ;; Build all scores
          all-scores (->> biases
                          (map (fn [[k v]] 
                                 {:model-slug (name k)
                                  :model-name (get v :name (name k))
                                  :score (get v :confidence 0)
                                  :success (> (get v :confidence 0) 0.5)}))
                          vec)
          ;; Analysis record for database
          analysis-record {:id (or id (str (java.util.UUID/randomUUID)))
                           :user-id user-id
                           :device-id device-id
                           :text (or text "")
                           :text-length (or text-length 0)
                           :source "pipeline"
                           :source-url file-path
                           :metadata metadata
                           :models-analyzed (count biases)
                           :successful-analyses (count (filter #(> (get % :confidence 0) 0.5) (vals biases)))
                           :average-score avg-score
                           :lollapalooza-detected is-lollapalooza
                           :convergence-score convergence-score
                           :convergence-count convergence-count
                           :top-10-models top-10-models
                           :all-scores all-scores
                           :created-at (Instant/now)}]
      (db-analysis/save-analysis! analysis-record)
      (log/info "Pipeline result saved:" (:id analysis-record))
      (:id analysis-record))
    (catch Exception e
      (log/error e "Failed to save pipeline result:" file-path)
      nil)))

(defn save-batch-results!
  "Save multiple pipeline results as a batch.
   
   Returns {:saved count :failed count :ids [...]}"
  [results & {:keys [user-id device-id] :or {user-id "system" device-id "pipeline"}}]
  (let [batch-id (str (java.util.UUID/randomUUID))
        save-results (map #(save-pipeline-result! % :user-id user-id :device-id device-id) results)
        saved-ids (filter some? save-results)]
    (log/info "Batch saved:" batch-id "- saved:" (count saved-ids) "failed:" (- (count results) (count saved-ids)))
    {:batch-id batch-id
     :saved (count saved-ids)
     :failed (- (count results) (count saved-ids))
     :ids saved-ids}))

;; =============================================================================
;; QUERY FUNCTIONS
;; =============================================================================

(defn get-recent-analyses
  "Get recent analyses from the database.
   
   Returns a list of analysis summaries."
  [user-id & {:keys [limit] :or {limit 50}}]
  (try
    (:items (db-analysis/list-analyses user-id 1 limit))
    (catch Exception e
      (log/error e "Failed to get recent analyses")
      [])))

(defn get-analysis-by-id
  "Get a specific analysis by ID."
  [analysis-id user-id]
  (try
    (db-analysis/get-analysis analysis-id user-id)
    (catch Exception e
      (log/error e "Failed to get analysis:" analysis-id)
      nil)))

(defn get-lollapalooza-events
  "Get recent Lollapalooza events."
  [user-id & {:keys [limit] :or {limit 20}}]
  (try
    (:items (db-analysis/list-lollapalooza-events user-id 1 limit))
    (catch Exception e
      (log/error e "Failed to get Lollapalooza events")
      [])))

(defn get-top-models
  "Get most frequently detected models for a user."
  [user-id & {:keys [limit] :or {limit 25}}]
  (try
    (db-analysis/get-top-models user-id limit)
    (catch Exception e
      (log/error e "Failed to get top models")
      [])))

(defn get-user-stats
  "Get analysis statistics for a user."
  [user-id]
  (try
    (db-analysis/get-user-stats user-id)
    (catch Exception e
      (log/error e "Failed to get user stats")
      nil)))

(defn get-analysis-trends
  "Get analysis trends over time."
  [user-id & {:keys [days] :or {days 30}}]
  (try
    (db-analysis/get-analysis-trends user-id days)
    (catch Exception e
      (log/error e "Failed to get analysis trends")
      [])))

;; =============================================================================
;; FAILURE MODE TRACKING
;; =============================================================================

(defn save-failure-mode-detection!
  "Record a detected failure mode from analysis."
  [analysis-id user-id model-slug model-name failure-mode-name risk-level]
  (try
    (db-analysis/save-failure-mode-detection! 
     analysis-id user-id model-slug model-name failure-mode-name risk-level)
    (catch Exception e
      (log/error e "Failed to save failure mode detection")
      false)))

(defn get-high-risk-failure-modes
  "Get high-risk failure modes detected for a user."
  [user-id & {:keys [limit] :or {limit 10}}]
  (try
    (db-analysis/get-failure-modes-by-risk user-id "high" limit)
    (catch Exception e
      (log/error e "Failed to get high-risk failure modes")
      [])))

;; =============================================================================
;; HEALTH CHECK
;; =============================================================================

(defn check-db-connection
  "Check if database connection is healthy."
  []
  (try
    (db/get-datasource)
    {:status :healthy :message "Database connection OK"}
    (catch Exception e
      {:status :unhealthy :message (.getMessage e)})))
