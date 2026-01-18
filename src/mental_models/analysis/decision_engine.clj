(ns mental-models.analysis.decision-engine
  "Decision Tracking & Improvement Engine - Electric Clojure
   Replaces Python decision_tracker.py and improvement_engine.py
   Tracks decisions, outcomes, and mental model effectiveness"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [mental-models.analysis.bayesian :as bayesian]
            [mental-models.analysis.statistical :as stats]
            [taoensso.timbre :as log]
            [java.time :refer [Instant]]))

;; -- Decision Records --

(defrecord Decision
  [id decision-text context mental-models-applied
   confidence-score created-at])

(defrecord Outcome
  [id decision-id outcome-text outcome-score
   actual-vs-predicted models-accuracy created-at])

(defrecord DecisionAnalysis
  [decision models-used accuracy-improvement
   model-effectiveness failure-modes lessons-learned])

;; -- Decision Tracking --

(defn create-decision
  "Create and record a decision"
  [decision-text context mental-models confidence]
  (->Decision
   (java.util.UUID/randomUUID)
   decision-text
   context
   mental-models
   confidence
   (Instant/now)))

(defn record-outcome
  "Record outcome of a decision"
  [decision-id outcome-text outcome-score actual-vs-predicted]
  (->Outcome
   (java.util.UUID/randomUUID)
   decision-id
   outcome-text
   outcome-score
   actual-vs-predicted
   (Instant/now)))

;; -- Model Effectiveness Analysis --

(defn calculate-model-accuracy
  "Calculate accuracy of each model in predicting outcome"
  [decision outcome]
  (let [models-used (:mental-models-applied decision)
        outcome-score (:outcome-score outcome)
        actual-vs-predicted (:actual-vs-predicted outcome)]
    
    (mapv (fn [model]
           (let [;; Score how well this model predicted the outcome
                 prediction-accuracy (if (> actual-vs-predicted 0)
                                      ;; Positive outcome: model was right if it suggested caution
                                      (if (contains? #{"Risk Aversion" "Margin of Safety"} model)
                                        0.8
                                        0.3)
                                      ;; Negative outcome: model was right if it warned
                                      (if (contains? #{"Overconfidence" "Availability Heuristic"} model)
                                        0.8
                                        0.3))]
             {:model model
              :accuracy prediction-accuracy
              :outcome-alignment actual-vs-predicted}))
         models-used)))

(defn model-effectiveness-score
  "Calculate overall effectiveness score for a model across decisions"
  [decisions outcomes model-name]
  (let [relevant-decisions (filter #(contains? (set (:mental-models-applied %)) model-name) decisions)
        relevant-outcomes (filter #(some #{(:decision-id %)} (map :id relevant-decisions)) outcomes)
        
        accuracies (mapv (fn [decision]
                         (let [outcome (first (filter #(= (:decision-id %) (:id decision)) relevant-outcomes))]
                           (when outcome
                             (let [analysis (calculate-model-accuracy decision outcome)]
                               (first (filter #(= (:model %) model-name) analysis))))))
                        relevant-decisions)
        
        valid-accuracies (filter some? accuracies)
        avg-accuracy (if (empty? valid-accuracies)
                      0.0
                      (stats/mean (map :accuracy valid-accuracies)))]
    
    {:model model-name
     :decisions-used (count relevant-decisions)
     :outcomes-tracked (count relevant-outcomes)
     :average-accuracy avg-accuracy
     :trend (if (> (count valid-accuracies) 1)
            (- (last valid-accuracies) (first valid-accuracies))
            0.0)}))

;; -- Decision Quality Metrics --

(defn decision-quality-score
  "Calculate quality score of a decision"
  [decision outcome]
  (let [confidence (:confidence-score decision)
        outcome-score (:outcome-score outcome)
        actual-vs-predicted (:actual-vs-predicted outcome)
        
        ;; Quality = how well confidence matched outcome
        confidence-accuracy (if (> actual-vs-predicted 0)
                            (min 1.0 (+ confidence 0.2))  ;; Bonus for positive outcome
                            (max 0.0 (- confidence 0.2)))  ;; Penalty for negative
        
        ;; Adjust by outcome magnitude
        adjusted-quality (* confidence-accuracy (math/abs outcome-score))]
    
    (max 0.0 (min 1.0 adjusted-quality))))

;; -- Improvement Recommendations --

(defn identify-improvement-areas
  "Identify where decision-making can improve"
  [decisions outcomes]
  (let [model-scores (mapv #(model-effectiveness-score decisions outcomes %)
                          (distinct (mapcat :mental-models-applied decisions)))
        
        low-performing (filter #(< (:average-accuracy %) 0.5) model-scores)
        improving (filter #(> (:trend %) 0.1) model-scores)
        declining (filter #(< (:trend %) -0.1) model-scores)]
    
    {:low-performing-models (map :model low-performing)
     :improving-models (map :model improving)
     :declining-models (map :model declining)
     :focus-areas (cond
                   (seq low-performing) "Focus on improving low-performing models"
                   (seq declining) "Address declining model effectiveness"
                   (seq improving) "Continue building on improving models"
                   :else "Maintain current approach")}))

(defn generate-improvement-plan
  "Generate actionable improvement plan"
  [decisions outcomes]
  (let [improvements (identify-improvement-areas decisions outcomes)
        low-models (:low-performing-models improvements)
        
        recommendations (mapv (fn [model]
                              (case model
                                "Confirmation Bias" "Actively seek disconfirming evidence"
                                "Anchoring" "Generate multiple reference points before deciding"
                                "Availability Heuristic" "Use base rates and statistics"
                                "Overconfidence" "Widen confidence intervals and plan for downside"
                                "Survivorship Bias" "Study failures as much as successes"
                                (str "Review " model " application in decisions")))
                             low-models)]
    
    {:improvements improvements
     :recommendations recommendations
     :priority-order (take 3 recommendations)}))

;; -- Failure Mode Analysis --

(defn detect-failure-patterns
  "Detect recurring failure patterns"
  [decisions outcomes]
  (let [negative-outcomes (filter #(< (:outcome-score %) 0.5) outcomes)
        failure-decisions (mapv #(first (filter (fn [d] (= (:id d) (:decision-id %))) decisions))
                              negative-outcomes)
        
        model-failure-counts (frequencies (mapcat :mental-models-applied failure-decisions))
        sorted-failures (sort-by val > model-failure-counts)]
    
    {:failure-patterns sorted-failures
     :most-common-failure (first (first sorted-failures))
     :failure-rate (/ (count negative-outcomes) (count outcomes))}))

;; -- Learning Metrics --

(defn calculate-learning-curve
  "Calculate learning curve (improvement over time)"
  [decisions outcomes]
  (let [sorted-outcomes (sort-by :created-at outcomes)
        quality-scores (mapv (fn [outcome]
                             (let [decision (first (filter #(= (:id %) (:decision-id outcome)) decisions))]
                               (decision-quality-score decision outcome)))
                           sorted-outcomes)
        
        windows (partition 5 1 quality-scores)  ;; 5-decision rolling average
        averages (mapv stats/mean windows)]
    
    {:raw-scores quality-scores
     :smoothed-trend averages
     :overall-improvement (if (> (count averages) 1)
                          (- (last averages) (first averages))
                          0.0)}))

;; -- Electric Components --

(e/defn DecisionTracker
  "Component to track and analyze decisions"
  [decisions outcomes]
  (e/client
    (let [quality-scores (mapv (fn [outcome]
                               (let [decision (first (filter #(= (:id %) (:decision-id outcome)) decisions))]
                                 (decision-quality-score decision outcome)))
                             outcomes)
          avg-quality (if (empty? quality-scores) 0.0 (stats/mean quality-scores))
          
          improvements (identify-improvement-areas decisions outcomes)
          learning (calculate-learning-curve decisions outcomes)]
      
      (dom/div {:class "decision-tracker"}
        (dom/h2 "Decision Analysis")
        
        ;; Summary Stats
        (dom/div {:class "summary"}
          (dom/p (str "Decisions Tracked: " (count decisions)))
          (dom/p (str "Outcomes Recorded: " (count outcomes)))
          (dom/p (str "Average Quality Score: " (format "%.2f" avg-quality))))
        
        ;; Improvement Areas
        (dom/div {:class "improvements"}
          (dom/h3 "Improvement Areas")
          (dom/p (:focus-areas improvements))
          (when (seq (:low-performing-models improvements))
            (dom/div
              (dom/p "Low-Performing Models:")
              (dom/ul
                (doseq [model (:low-performing-models improvements)]
                  (dom/li model))))))
        
        ;; Learning Curve
        (dom/div {:class "learning-curve"}
          (dom/h3 "Learning Progress")
          (dom/p (str "Overall Improvement: " (format "%.2f" (:overall-improvement learning))))
          (dom/p (if (> (:overall-improvement learning) 0)
                  "✓ Improving over time"
                  "✗ Declining trend")))))))

(e/defn ModelEffectivenessChart
  "Component to display model effectiveness"
  [decisions outcomes]
  (e/client
    (let [models (distinct (mapcat :mental-models-applied decisions))
          effectiveness (mapv #(model-effectiveness-score decisions outcomes %) models)
          sorted (sort-by :average-accuracy > effectiveness)]
      
      (dom/div {:class "model-effectiveness"}
        (dom/h3 "Model Effectiveness")
        (dom/table
          (dom/thead
            (dom/tr
              (dom/th "Model")
              (dom/th "Accuracy")
              (dom/th "Decisions")
              (dom/th "Trend")))
          (dom/tbody
            (doseq [model sorted]
              (dom/tr
                (dom/td (:model model))
                (dom/td (format "%.1f%%" (* (:average-accuracy model) 100)))
                (dom/td (:decisions-used model))
                (dom/td (if (> (:trend model) 0)
                        "↑ Improving"
                        "↓ Declining"))))))))))

;; -- Utilities --

(defn export-decision-log
  "Export decision log for external analysis"
  [decisions outcomes]
  {:decisions decisions
   :outcomes outcomes
   :analysis {:improvements (identify-improvement-areas decisions outcomes)
              :learning (calculate-learning-curve decisions outcomes)
              :failures (detect-failure-patterns decisions outcomes)}})
