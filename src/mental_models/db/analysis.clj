(ns mental-models.db.analysis
  "Database functions for storing and retrieving analysis results"
  (:require [mental-models.db.postgres :as db]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [java.time :refer [Instant]]))

;; -- Analysis Storage -------

(defn save-analysis!
  "Save analysis results to database"
  [{:keys [id user-id device-id text text-length source source-url metadata
           models-analyzed successful-analyses average-score
           lollapalooza-detected convergence-score convergence-count
           top-10-models all-scores created-at]}]
  (try
    (let [query "INSERT INTO analyses 
                 (id, user_id, device_id, text, text_length, source, source_url, metadata,
                  models_analyzed, successful_analyses, average_score,
                  lollapalooza_detected, convergence_score, convergence_count,
                  top_10_models, all_scores, created_at)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                 RETURNING id"
          
          result (db/query query
                           [id user-id device-id text text-length source source-url
                            (json/generate-string metadata)
                            models-analyzed successful-analyses average-score
                            lollapalooza-detected convergence-score convergence-count
                            (json/generate-string top-10-models)
                            (json/generate-string all-scores)
                            created-at])]
      
      ;; If Lollapalooza detected, save to lollapalooza_events table
      (when lollapalooza-detected
        (let [converging-models (filter #(> (:score %) 0.7) all-scores)]
          (db/execute "INSERT INTO lollapalooza_events 
                       (analysis_id, user_id, convergence_score, convergence_count, converging_models)
                       VALUES (?, ?, ?, ?, ?)"
                     [id user-id convergence-score convergence-count
                      (json/generate-string converging-models)])))
      
      ;; Update model detection frequencies
      (doseq [model (filter :success all-scores)]
        (db/execute "INSERT INTO model_detections 
                     (user_id, model_slug, model_name, detection_count, avg_score)
                     VALUES (?, ?, ?, 1, ?)
                     ON CONFLICT (user_id, model_slug)
                     DO UPDATE SET detection_count = detection_count + 1,
                                   avg_score = (avg_score + ?) / 2,
                                   last_detected = CURRENT_TIMESTAMP"
                    [user-id (:model-slug model) (:model-name model) (:score model) (:score model)]))
      
      ;; Update user statistics
      (db/execute "INSERT INTO analysis_statistics 
                   (user_id, total_analyses, total_texts_analyzed, avg_analysis_score, 
                    lollapalooza_count, last_analysis)
                   VALUES (?, 1, 1, ?, ?, CURRENT_TIMESTAMP)
                   ON CONFLICT (user_id)
                   DO UPDATE SET total_analyses = total_analyses + 1,
                                 total_texts_analyzed = total_texts_analyzed + 1,
                                 avg_analysis_score = (avg_analysis_score + ?) / 2,
                                 lollapalooza_count = lollapalooza_count + ?,
                                 last_analysis = CURRENT_TIMESTAMP"
                  [user-id average-score (if lollapalooza-detected 1 0) average-score
                   (if lollapalooza-detected 1 0)])
      
      (log/info "Analysis saved:" id)
      result)
    
    (catch Exception e
      (log/error e "Failed to save analysis")
      nil)))

(defn get-analysis
  "Retrieve analysis by ID (user must own it)"
  [analysis-id user-id]
  (try
    (let [query "SELECT * FROM analyses WHERE id = ? AND user_id = ?"
          result (db/query-one query [analysis-id user-id])]
      
      (when result
        (-> result
            (update :metadata #(json/parse-string % true))
            (update :top-10-models #(json/parse-string % true))
            (update :all-scores #(json/parse-string % true))))
      
      result)
    
    (catch Exception e
      (log/error e "Failed to get analysis")
      nil)))

(defn list-analyses
  "List user's analyses with pagination"
  [user-id page page-size]
  (try
    (let [offset (* (dec page) page-size)
          query "SELECT id, text_length, average_score, lollapalooza_detected, 
                        convergence_score, created_at
                 FROM analyses
                 WHERE user_id = ?
                 ORDER BY created_at DESC
                 LIMIT ? OFFSET ?"
          
          count-query "SELECT COUNT(*) as count FROM analyses WHERE user_id = ?"
          
          items (db/query query [user-id page-size offset])
          total (:count (db/query-one count-query [user-id]))]
      
      {:items items :total total})
    
    (catch Exception e
      (log/error e "Failed to list analyses")
      {:items [] :total 0})))

;; -- Batch Analysis Storage --

(defn save-batch-analysis!
  "Save batch analysis results"
  [{:keys [batch-id user-id device-id analyses created-at]}]
  (try
    (let [query "INSERT INTO batch_analyses 
                 (batch_id, user_id, device_id, analysis_count, analyses, created_at)
                 VALUES (?, ?, ?, ?, ?, ?)
                 RETURNING id"
          
          result (db/query query
                           [batch-id user-id device-id (count analyses)
                            (json/generate-string analyses) created-at])]
      
      ;; Save each analysis individually
      (doseq [analysis analyses]
        (save-analysis! (assoc analysis :user-id user-id :device-id device-id)))
      
      (log/info "Batch analysis saved:" batch-id)
      result)
    
    (catch Exception e
      (log/error e "Failed to save batch analysis")
      nil)))

;; -- Model Detection Analytics --

(defn get-top-models
  "Get most frequently detected models for user"
  [user-id limit]
  (try
    (let [query "SELECT model_slug, model_name, detection_count, avg_score
                 FROM model_detections
                 WHERE user_id = ?
                 ORDER BY detection_count DESC, avg_score DESC
                 LIMIT ?"
          
          results (db/query query [user-id limit])]
      
      results)
    
    (catch Exception e
      (log/error e "Failed to get top models")
      [])))

(defn get-model-history
  "Get detection history for a specific model"
  [user-id model-slug limit]
  (try
    (let [query "SELECT a.id, a.average_score, a.created_at
                 FROM analyses a
                 WHERE a.user_id = ? 
                 AND a.all_scores::text LIKE ?
                 ORDER BY a.created_at DESC
                 LIMIT ?"
          
          results (db/query query [user-id (str "%\"model-slug\":\"" model-slug "%") limit])]
      
      results)
    
    (catch Exception e
      (log/error e "Failed to get model history")
      [])))

;; -- Lollapalooza Events --

(defn list-lollapalooza-events
  "List Lollapalooza events for user with pagination"
  [user-id page page-size]
  (try
    (let [offset (* (dec page) page-size)
          query "SELECT analysis_id, convergence_score, convergence_count, 
                        converging_models, created_at
                 FROM lollapalooza_events
                 WHERE user_id = ?
                 ORDER BY created_at DESC
                 LIMIT ? OFFSET ?"
          
          count-query "SELECT COUNT(*) as count FROM lollapalooza_events WHERE user_id = ?"
          
          items (db/query query [user-id page-size offset])
          total (:count (db/query-one count-query [user-id]))]
      
      {:items (mapv #(update % :converging-models json/parse-string true) items)
       :total total})
    
    (catch Exception e
      (log/error e "Failed to list Lollapalooza events")
      {:items [] :total 0})))

(defn get-lollapalooza-stats
  "Get Lollapalooza statistics for user"
  [user-id]
  (try
    (let [query "SELECT COUNT(*) as total_events,
                        AVG(convergence_score) as avg_convergence,
                        MAX(convergence_score) as max_convergence,
                        AVG(convergence_count) as avg_model_count
                 FROM lollapalooza_events
                 WHERE user_id = ?"
          
          result (db/query-one query [user-id])]
      
      result)
    
    (catch Exception e
      (log/error e "Failed to get Lollapalooza stats")
      nil)))

;; -- Failure Mode Tracking --

(defn save-failure-mode-detection!
  "Record a failure mode detection"
  [analysis-id user-id model-slug model-name failure-mode-name risk-level]
  (try
    (db/execute "INSERT INTO failure_mode_detections 
                 (analysis_id, user_id, model_slug, model_name, failure_mode_name, risk_level)
                 VALUES (?, ?, ?, ?, ?, ?)"
               [analysis-id user-id model-slug model-name failure-mode-name risk-level])
    
    (log/info "Failure mode saved:" failure-mode-name)
    true)
    
    (catch Exception e
      (log/error e "Failed to save failure mode")
      false)))

(defn get-failure-modes-by-risk
  "Get failure modes detected for user by risk level"
  [user-id risk-level limit]
  (try
    (let [query "SELECT model_slug, model_name, failure_mode_name, 
                        COUNT(*) as detection_count
                 FROM failure_mode_detections
                 WHERE user_id = ? AND risk_level = ?
                 GROUP BY model_slug, model_name, failure_mode_name
                 ORDER BY detection_count DESC
                 LIMIT ?"
          
          results (db/query query [user-id risk-level limit])]
      
      results)
    
    (catch Exception e
      (log/error e "Failed to get failure modes")
      [])))

;; -- User Statistics --

(defn get-user-stats
  "Get analysis statistics for user"
  [user-id]
  (try
    (let [query "SELECT * FROM analysis_statistics WHERE user_id = ?"
          result (db/query-one query [user-id])]
      
      result)
    
    (catch Exception e
      (log/error e "Failed to get user stats")
      nil)))

(defn get-analysis-trends
  "Get analysis trends over time (daily aggregates)"
  [user-id days]
  (try
    (let [query "SELECT DATE(created_at) as date,
                        COUNT(*) as analysis_count,
                        AVG(average_score) as avg_score,
                        SUM(CASE WHEN lollapalooza_detected THEN 1 ELSE 0 END) as lollapalooza_count
                 FROM analyses
                 WHERE user_id = ? AND created_at >= CURRENT_DATE - INTERVAL ? DAY
                 GROUP BY DATE(created_at)
                 ORDER BY date DESC"
          
          results (db/query query [user-id days])]
      
      results)
    
    (catch Exception e
      (log/error e "Failed to get analysis trends")
      [])))
