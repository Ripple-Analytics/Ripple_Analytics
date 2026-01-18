(ns mental-models.effectiveness-tracker
  "Model Effectiveness Tracker - Measure which mental models actually predict outcomes
   
   Tracks predictions, records outcomes, calculates effectiveness metrics using Bayesian updating.
   Enables data-driven model selection and continuous improvement.
   
   This is a .cljc file - runs on both client and server!"
  #?(:clj (:require [clojure.string :as str]
                    [clojure.java.io :as io])
     :cljs (:require [clojure.string :as str])))

;; ============================================
;; State Management
;; ============================================

(defonce !predictions (atom []))
(defonce !outcomes (atom {}))
(defonce !effectiveness-scores (atom {}))
(defonce !model-combinations (atom {}))

;; ============================================
;; Prediction Tracking
;; ============================================

(defn record-prediction
  "Record a prediction made using mental models.
   
   Args:
     prediction-id: Unique identifier for this prediction
     models-used: Vector of model names used
     prediction: The actual prediction (string or map)
     confidence: Confidence level (0.0-1.0)
     context: Map with additional context (date, domain, etc.)
   
   Returns:
     The prediction record"
  [prediction-id models-used prediction confidence context]
  (let [record {:id prediction-id
                :models models-used
                :prediction prediction
                :confidence confidence
                :context context
                :timestamp #?(:clj (java.time.Instant/now)
                             :cljs (js/Date.))
                :outcome nil
                :outcome-recorded false}]
    (swap! !predictions conj record)
    (doseq [model models-used]
      (swap! !model-combinations update model (fnil inc 0)))
    record))

(defn record-outcome
  "Record the actual outcome for a prediction.
   
   Args:
     prediction-id: ID of the prediction
     outcome: The actual outcome
     correct?: Boolean indicating if prediction was correct
     notes: Optional notes about the outcome
   
   Returns:
     Updated prediction record"
  [prediction-id outcome correct? notes]
  (let [outcome-record {:outcome outcome
                        :correct? correct?
                        :notes notes
                        :recorded-at #?(:clj (java.time.Instant/now)
                                       :cljs (js/Date.))}]
    (swap! !outcomes assoc prediction-id outcome-record)
    (swap! !predictions
           (fn [preds]
             (mapv #(if (= (:id %) prediction-id)
                     (assoc % 
                            :outcome outcome
                            :outcome-recorded true
                            :correct? correct?)
                     %)
                   preds)))
    outcome-record))

;; ============================================
;; Effectiveness Calculation
;; ============================================

(defn calculate-model-accuracy
  "Calculate accuracy for a specific model.
   
   Args:
     model-name: Name of the model
   
   Returns:
     Map with accuracy metrics"
  [model-name]
  (let [predictions-with-model (filter #(some #{model-name} (:models %)) @!predictions)
        predictions-with-outcomes (filter :outcome-recorded predictions-with-model)
        total-predictions (count predictions-with-outcomes)
        correct-predictions (count (filter :correct? predictions-with-outcomes))
        accuracy (if (pos? total-predictions)
                  (/ correct-predictions total-predictions)
                  0.0)]
    {:model model-name
     :total-predictions total-predictions
     :correct-predictions correct-predictions
     :accuracy accuracy
     :confidence-interval (if (pos? total-predictions)
                           [(max 0.0 (- accuracy (/ 1.96 (Math/sqrt total-predictions))))
                            (min 1.0 (+ accuracy (/ 1.96 (Math/sqrt total-predictions))))]
                           [0.0 0.0])}))

(defn calculate-combination-synergy
  "Calculate synergy score for model combinations.
   
   Synergy exists when models used together perform better than individually.
   
   Args:
     model-combo: Vector of model names
   
   Returns:
     Synergy score (positive = synergy, negative = interference)"
  [model-combo]
  (let [combo-predictions (filter #(= (set (:models %)) (set model-combo)) @!predictions)
        combo-with-outcomes (filter :outcome-recorded combo-predictions)
        combo-accuracy (if (pos? (count combo-with-outcomes))
                        (/ (count (filter :correct? combo-with-outcomes))
                           (count combo-with-outcomes))
                        0.0)
        individual-accuracies (map #(:accuracy (calculate-model-accuracy %)) model-combo)
        expected-accuracy (if (empty? individual-accuracies)
                           0.0
                           (/ (reduce + individual-accuracies) (count individual-accuracies)))
        synergy (- combo-accuracy expected-accuracy)]
    {:combination model-combo
     :combo-accuracy combo-accuracy
     :expected-accuracy expected-accuracy
     :synergy synergy
     :total-uses (count combo-with-outcomes)}))

(defn calculate-calibration-score
  "Calculate calibration score for a model.
   
   Calibration measures if confidence levels match actual accuracy.
   Perfect calibration = 1.0
   
   Args:
     model-name: Name of the model
   
   Returns:
     Calibration score (0.0-1.0)"
  [model-name]
  (let [predictions-with-model (filter #(some #{model-name} (:models %)) @!predictions)
        predictions-with-outcomes (filter :outcome-recorded predictions-with-model)
        confidence-bins (group-by #(int (* 10 (:confidence %))) predictions-with-outcomes)
        calibration-errors (map (fn [[bin preds]]
                                 (let [avg-confidence (/ (reduce + (map :confidence preds)) (count preds))
                                       actual-accuracy (/ (count (filter :correct? preds)) (count preds))
                                       error (Math/abs (- avg-confidence actual-accuracy))]
                                   error))
                               confidence-bins)
        mean-calibration-error (if (empty? calibration-errors)
                                0.0
                                (/ (reduce + calibration-errors) (count calibration-errors)))
        calibration-score (- 1.0 mean-calibration-error)]
    {:model model-name
     :calibration-score calibration-score
     :mean-calibration-error mean-calibration-error
     :bins (count confidence-bins)}))

(defn calculate-brier-score
  "Calculate Brier score for probabilistic predictions.
   
   Brier score measures accuracy of probabilistic predictions.
   Lower is better (0.0 = perfect, 1.0 = worst).
   
   Args:
     model-name: Name of the model
   
   Returns:
     Brier score"
  [model-name]
  (let [predictions-with-model (filter #(some #{model-name} (:models %)) @!predictions)
        predictions-with-outcomes (filter :outcome-recorded predictions-with-model)
        squared-errors (map (fn [pred]
                             (let [forecast (:confidence pred)
                                   outcome (if (:correct? pred) 1.0 0.0)
                                   error (- forecast outcome)]
                               (* error error)))
                           predictions-with-outcomes)
        brier-score (if (empty? squared-errors)
                     1.0
                     (/ (reduce + squared-errors) (count squared-errors)))]
    {:model model-name
     :brier-score brier-score
     :predictions-count (count predictions-with-outcomes)}))

;; ============================================
;; Effectiveness Reporting
;; ============================================

(defn update-effectiveness-scores
  "Recalculate effectiveness scores for all models.
   
   Returns:
     Map of model names to effectiveness scores"
  []
  (let [all-models (distinct (mapcat :models @!predictions))
        scores (into {} (map (fn [model]
                              [model {:accuracy (calculate-model-accuracy model)
                                     :calibration (calculate-calibration-score model)
                                     :brier (calculate-brier-score model)}])
                            all-models))]
    (reset! !effectiveness-scores scores)
    scores))

(defn get-top-models
  "Get top N models by accuracy.
   
   Args:
     n: Number of top models to return (default 10)
     min-predictions: Minimum predictions required (default 5)
   
   Returns:
     Vector of top models with scores"
  ([] (get-top-models 10 5))
  ([n min-predictions]
   (let [scores (update-effectiveness-scores)
         filtered-scores (filter #(>= (get-in (val %) [:accuracy :total-predictions]) min-predictions) scores)
         sorted-scores (sort-by #(get-in (val %) [:accuracy :accuracy]) > filtered-scores)]
     (take n sorted-scores))))

(defn get-model-effectiveness-report
  "Generate comprehensive effectiveness report for a model.
   
   Args:
     model-name: Name of the model
   
   Returns:
     Comprehensive effectiveness report"
  [model-name]
  (let [accuracy (calculate-model-accuracy model-name)
        calibration (calculate-calibration-score model-name)
        brier (calculate-brier-score model-name)
        predictions-with-model (filter #(some #{model-name} (:models %)) @!predictions)
        recent-predictions (take 10 (reverse (filter :outcome-recorded predictions-with-model)))]
    {:model model-name
     :accuracy accuracy
     :calibration calibration
     :brier brier
     :total-uses (count predictions-with-model)
     :recent-predictions recent-predictions
     :recommendation (cond
                      (< (:total-predictions accuracy) 5) "Insufficient data - need more predictions"
                      (>= (:accuracy accuracy) 0.7) "Highly effective - use frequently"
                      (>= (:accuracy accuracy) 0.5) "Moderately effective - use with caution"
                      :else "Low effectiveness - reconsider usage")}))

(defn get-system-effectiveness-summary
  "Get overall system effectiveness summary.
   
   Returns:
     Summary of entire system's predictive performance"
  []
  (let [total-predictions (count @!predictions)
        predictions-with-outcomes (filter :outcome-recorded @!predictions)
        total-outcomes (count predictions-with-outcomes)
        correct-outcomes (count (filter :correct? predictions-with-outcomes))
        overall-accuracy (if (pos? total-outcomes)
                          (/ correct-outcomes total-outcomes)
                          0.0)
        unique-models (count (distinct (mapcat :models @!predictions)))
        top-models (get-top-models 5 3)]
    {:total-predictions total-predictions
     :predictions-with-outcomes total-outcomes
     :overall-accuracy overall-accuracy
     :unique-models-used unique-models
     :top-performing-models top-models
     :improvement-rate (if (>= total-outcomes 10)
                        (let [recent (take 10 (reverse predictions-with-outcomes))
                              older (take 10 (drop 10 (reverse predictions-with-outcomes)))
                              recent-accuracy (/ (count (filter :correct? recent)) (count recent))
                              older-accuracy (if (empty? older) 0.0 (/ (count (filter :correct? older)) (count older)))]
                          (- recent-accuracy older-accuracy))
                        0.0)}))

;; ============================================
;; Data Persistence (File-based for now)
;; ============================================

(defn save-predictions-to-file
  "Save predictions to JSON file.
   
   Args:
     filepath: Path to save file
   
   Returns:
     Success boolean"
  [filepath]
  #?(:clj
     (try
       (spit filepath (pr-str {:predictions @!predictions
                               :outcomes @!outcomes
                               :effectiveness-scores @!effectiveness-scores}))
       true
       (catch Exception e
         (println "Error saving predictions:" (.getMessage e))
         false))
     :cljs
     (println "File saving not supported in ClojureScript")))

(defn load-predictions-from-file
  "Load predictions from JSON file.
   
   Args:
     filepath: Path to load file
   
   Returns:
     Success boolean"
  [filepath]
  #?(:clj
     (try
       (let [data (read-string (slurp filepath))]
         (reset! !predictions (:predictions data))
         (reset! !outcomes (:outcomes data))
         (reset! !effectiveness-scores (:effectiveness-scores data))
         true)
       (catch Exception e
         (println "Error loading predictions:" (.getMessage e))
         false))
     :cljs
     (println "File loading not supported in ClojureScript")))

;; ============================================
;; Usage Examples
;; ============================================

(comment
  ;; Record a prediction
  (record-prediction "pred-001"
                     ["circle-of-competence" "margin-of-safety"]
                     "Stock XYZ will outperform market by 15% in next 12 months"
                     0.75
                     {:date "2026-01-18"
                      :domain "investing"
                      :analyst "John Doe"})
  
  ;; Record outcome
  (record-outcome "pred-001"
                  "Stock XYZ outperformed by 18%"
                  true
                  "Prediction was accurate")
  
  ;; Get model effectiveness
  (get-model-effectiveness-report "circle-of-competence")
  
  ;; Get top performing models
  (get-top-models 10 5)
  
  ;; Get system summary
  (get-system-effectiveness-summary)
  
  ;; Calculate combination synergy
  (calculate-combination-synergy ["circle-of-competence" "margin-of-safety"])
  
  ;; Save/load data
  (save-predictions-to-file "data/predictions.edn")
  (load-predictions-from-file "data/predictions.edn"))
