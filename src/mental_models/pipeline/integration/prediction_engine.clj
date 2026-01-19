(ns mental-models.pipeline.integration.prediction-engine
  "Prediction engine for mental model outcomes and trends.
   
   Features:
   - Time series forecasting
   - Outcome prediction based on mental model combinations
   - Confidence interval estimation
   - Prediction tracking and accuracy measurement
   - Model ensemble predictions
   - Scenario analysis
   - What-if simulations
   - Prediction explanation"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:predictors {}       ;; predictor-id -> predictor-config
         :predictions {}      ;; prediction-id -> prediction
         :outcomes {}         ;; prediction-id -> actual-outcome
         :scenarios {}        ;; scenario-id -> scenario
         :ensembles {}        ;; ensemble-id -> ensemble-config
         :accuracy-history [] ;; historical accuracy records
         :initialized? false}))

;; ============================================================================
;; Statistical Helpers
;; ============================================================================

(defn- mean [xs]
  (if (empty? xs) 0.0 (/ (reduce + xs) (count xs))))

(defn- std-dev [xs]
  (if (< (count xs) 2)
    0.0
    (let [m (mean xs)
          variance (/ (reduce + (map #(Math/pow (- % m) 2) xs)) (dec (count xs)))]
      (Math/sqrt variance))))

(defn- weighted-mean [values weights]
  (if (empty? values)
    0.0
    (let [total-weight (reduce + weights)]
      (if (zero? total-weight)
        (mean values)
        (/ (reduce + (map * values weights)) total-weight)))))

;; ============================================================================
;; Predictor Registration
;; ============================================================================

(defn register-predictor!
  "Register a prediction model."
  [predictor-id config]
  (let [predictor {:id predictor-id
                   :name (get config :name (name predictor-id))
                   :type (get config :type :linear)
                   :features (get config :features [])
                   :target (get config :target nil)
                   :parameters (get config :parameters {})
                   :trained? false
                   :accuracy nil
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:predictors predictor-id] predictor)
    (logging/log :info "Registered predictor" {:predictor-id predictor-id :type (:type predictor)})
    (events/emit! :predictor-registered {:predictor-id predictor-id})
    predictor-id))

(defn get-predictor
  "Get a predictor configuration."
  [predictor-id]
  (get-in @state [:predictors predictor-id]))

(defn list-predictors
  "List all registered predictors."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :type (:type p)
           :trained? (:trained? p)
           :accuracy (:accuracy p)})
        (:predictors @state)))

;; ============================================================================
;; Time Series Forecasting
;; ============================================================================

(defn- simple-moving-average
  "Calculate simple moving average forecast."
  [values window-size]
  (if (< (count values) window-size)
    (mean values)
    (mean (take-last window-size values))))

(defn- exponential-smoothing
  "Calculate exponential smoothing forecast."
  [values alpha]
  (reduce (fn [smoothed value]
            (+ (* alpha value) (* (- 1 alpha) smoothed)))
          (first values)
          (rest values)))

(defn- linear-trend
  "Calculate linear trend forecast."
  [values periods-ahead]
  (let [n (count values)
        x-mean (/ (dec n) 2.0)
        y-mean (mean values)
        xs (range n)
        numerator (reduce + (map (fn [x y] (* (- x x-mean) (- y y-mean))) xs values))
        denominator (reduce + (map (fn [x] (Math/pow (- x x-mean) 2)) xs))
        slope (if (zero? denominator) 0 (/ numerator denominator))
        intercept (- y-mean (* slope x-mean))]
    (+ intercept (* slope (+ n periods-ahead -1)))))

(defn forecast-time-series
  "Forecast future values in a time series."
  [values & {:keys [method periods-ahead window-size alpha]
              :or {method :linear periods-ahead 1 window-size 5 alpha 0.3}}]
  (when (flags/enabled? :prediction-engine)
    (let [forecast (case method
                     :sma (simple-moving-average values window-size)
                     :ema (exponential-smoothing values alpha)
                     :linear (linear-trend values periods-ahead)
                     (mean values))
          ;; Calculate confidence interval
          residuals (case method
                      :sma (let [sma-values (map #(simple-moving-average (take % values) window-size)
                                                 (range window-size (inc (count values))))]
                             (map - (drop (dec window-size) values) sma-values))
                      :linear (let [n (count values)
                                    predictions (map #(linear-trend (take % values) 1)
                                                     (range 2 (inc n)))]
                                (map - (drop 1 values) predictions))
                      [])
          std-error (if (seq residuals) (std-dev residuals) 0)
          confidence-95 (* 1.96 std-error)]
      {:forecast forecast
       :method method
       :periods-ahead periods-ahead
       :confidence-interval {:lower (- forecast confidence-95)
                             :upper (+ forecast confidence-95)
                             :level 0.95}
       :std-error std-error
       :timestamp (System/currentTimeMillis)})))

;; ============================================================================
;; Mental Model Outcome Prediction
;; ============================================================================

(defn- calculate-model-interaction-score
  "Calculate interaction score between mental models."
  [models]
  (let [n (count models)
        ;; Simplified interaction scoring
        base-score (/ n 10.0)
        ;; Check for known synergies
        synergy-pairs #{#{:confirmation-bias :availability-heuristic}
                        #{:loss-aversion :sunk-cost}
                        #{:anchoring :adjustment-heuristic}
                        #{:social-proof :authority-bias}}
        model-set (set (map :model-id models))
        synergy-bonus (count (filter #(clojure.set/subset? % model-set) synergy-pairs))]
    (+ base-score (* 0.1 synergy-bonus))))

(defn predict-outcome
  "Predict outcome based on detected mental models."
  [models context & {:keys [predictor-id]}]
  (when (flags/enabled? :prediction-engine)
    (let [predictor (when predictor-id (get-predictor predictor-id))
          ;; Calculate base prediction from model confidences
          avg-confidence (if (seq models)
                           (mean (map :confidence models))
                           0.5)
          model-count (count models)
          interaction-score (calculate-model-interaction-score models)
          
          ;; Adjust based on context
          context-factor (cond
                           (contains? context :high-stakes) 1.2
                           (contains? context :time-pressure) 1.1
                           :else 1.0)
          
          ;; Calculate predicted outcome probability
          base-probability (* avg-confidence context-factor)
          adjusted-probability (min 1.0 (+ base-probability interaction-score))
          
          ;; Determine outcome category
          outcome-category (cond
                             (>= adjusted-probability 0.8) :highly-likely
                             (>= adjusted-probability 0.6) :likely
                             (>= adjusted-probability 0.4) :uncertain
                             (>= adjusted-probability 0.2) :unlikely
                             :else :highly-unlikely)]
      
      {:prediction-id (str (UUID/randomUUID))
       :models (mapv :model-id models)
       :model-count model-count
       :avg-confidence avg-confidence
       :interaction-score interaction-score
       :context-factor context-factor
       :probability adjusted-probability
       :outcome-category outcome-category
       :confidence-interval {:lower (max 0 (- adjusted-probability 0.15))
                             :upper (min 1 (+ adjusted-probability 0.15))}
       :timestamp (System/currentTimeMillis)})))

(defn store-prediction!
  "Store a prediction for tracking."
  [prediction & {:keys [source description]}]
  (let [prediction-id (:prediction-id prediction)
        record (merge prediction
                      {:source source
                       :description description
                       :stored-at (System/currentTimeMillis)
                       :outcome nil
                       :accuracy nil})]
    (swap! state assoc-in [:predictions prediction-id] record)
    (metrics/increment :predictions-made)
    (events/emit! :prediction-stored {:prediction-id prediction-id})
    prediction-id))

(defn get-prediction
  "Get a stored prediction."
  [prediction-id]
  (get-in @state [:predictions prediction-id]))

(defn list-predictions
  "List predictions with optional filters."
  [& {:keys [since outcome-category has-outcome? limit]}]
  (let [predictions (vals (:predictions @state))
        filtered (cond->> predictions
                   since (filter #(>= (:timestamp %) since))
                   outcome-category (filter #(= (:outcome-category %) outcome-category))
                   (some? has-outcome?) (filter #(if has-outcome?
                                                   (some? (:outcome %))
                                                   (nil? (:outcome %))))
                   true (sort-by :timestamp >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Outcome Recording
;; ============================================================================

(defn record-outcome!
  "Record the actual outcome for a prediction."
  [prediction-id outcome & {:keys [notes]}]
  (let [prediction (get-prediction prediction-id)
        predicted-prob (:probability prediction)
        ;; Calculate accuracy (Brier score component)
        outcome-value (if outcome 1.0 0.0)
        accuracy (- 1 (Math/pow (- predicted-prob outcome-value) 2))]
    
    (swap! state
           (fn [s]
             (-> s
                 (assoc-in [:predictions prediction-id :outcome] outcome)
                 (assoc-in [:predictions prediction-id :outcome-recorded-at]
                           (System/currentTimeMillis))
                 (assoc-in [:predictions prediction-id :accuracy] accuracy)
                 (assoc-in [:predictions prediction-id :notes] notes)
                 (assoc-in [:outcomes prediction-id]
                           {:outcome outcome
                            :accuracy accuracy
                            :recorded-at (System/currentTimeMillis)})
                 (update :accuracy-history conj
                         {:prediction-id prediction-id
                          :predicted predicted-prob
                          :actual outcome-value
                          :accuracy accuracy
                          :timestamp (System/currentTimeMillis)}))))
    
    (metrics/gauge :prediction-accuracy {} accuracy)
    (logging/log :info "Recorded prediction outcome"
                 {:prediction-id prediction-id :outcome outcome :accuracy accuracy})
    (events/emit! :outcome-recorded {:prediction-id prediction-id :accuracy accuracy})
    accuracy))

(defn get-accuracy-stats
  "Get prediction accuracy statistics."
  []
  (let [history (:accuracy-history @state)
        accuracies (map :accuracy history)
        n (count accuracies)]
    (if (zero? n)
      {:total-predictions 0
       :avg-accuracy nil
       :brier-score nil}
      {:total-predictions n
       :avg-accuracy (mean accuracies)
       :brier-score (- 1 (mean accuracies))
       :min-accuracy (apply min accuracies)
       :max-accuracy (apply max accuracies)
       :std-dev (std-dev accuracies)})))

;; ============================================================================
;; Ensemble Predictions
;; ============================================================================

(defn create-ensemble!
  "Create an ensemble of predictors."
  [ensemble-id config]
  (let [ensemble {:id ensemble-id
                  :name (get config :name (name ensemble-id))
                  :predictor-ids (get config :predictor-ids [])
                  :weights (get config :weights nil)
                  :aggregation (get config :aggregation :weighted-mean)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:ensembles ensemble-id] ensemble)
    (logging/log :info "Created prediction ensemble" {:ensemble-id ensemble-id})
    ensemble-id))

(defn ensemble-predict
  "Make a prediction using an ensemble of predictors."
  [ensemble-id models context]
  (let [ensemble (get-in @state [:ensembles ensemble-id])
        predictor-ids (:predictor-ids ensemble)
        weights (or (:weights ensemble)
                    (repeat (count predictor-ids) 1.0))
        
        ;; Get predictions from each predictor
        predictions (for [pid predictor-ids]
                      (predict-outcome models context :predictor-id pid))
        probabilities (map :probability predictions)
        
        ;; Aggregate predictions
        final-prob (case (:aggregation ensemble)
                     :weighted-mean (weighted-mean probabilities weights)
                     :mean (mean probabilities)
                     :median (nth (sort probabilities) (quot (count probabilities) 2))
                     :max (apply max probabilities)
                     :min (apply min probabilities)
                     (mean probabilities))]
    
    {:ensemble-id ensemble-id
     :individual-predictions predictions
     :aggregated-probability final-prob
     :aggregation-method (:aggregation ensemble)
     :confidence-interval {:lower (apply min (map #(get-in % [:confidence-interval :lower]) predictions))
                           :upper (apply max (map #(get-in % [:confidence-interval :upper]) predictions))}
     :timestamp (System/currentTimeMillis)}))

;; ============================================================================
;; Scenario Analysis
;; ============================================================================

(defn create-scenario!
  "Create a scenario for what-if analysis."
  [scenario-id config]
  (let [scenario {:id scenario-id
                  :name (get config :name (name scenario-id))
                  :description (get config :description "")
                  :base-models (get config :base-models [])
                  :modifications (get config :modifications [])
                  :context (get config :context {})
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:scenarios scenario-id] scenario)
    (logging/log :info "Created scenario" {:scenario-id scenario-id})
    scenario-id))

(defn run-scenario
  "Run a what-if scenario analysis."
  [scenario-id]
  (let [scenario (get-in @state [:scenarios scenario-id])
        base-models (:base-models scenario)
        modifications (:modifications scenario)
        context (:context scenario)
        
        ;; Apply modifications to base models
        modified-models (reduce
                         (fn [models mod]
                           (case (:type mod)
                             :add (conj models (:model mod))
                             :remove (filterv #(not= (:model-id %) (:model-id mod)) models)
                             :adjust (mapv (fn [m]
                                             (if (= (:model-id m) (:model-id mod))
                                               (merge m (:changes mod))
                                               m))
                                           models)
                             models))
                         base-models
                         modifications)
        
        ;; Get predictions for base and modified scenarios
        base-prediction (predict-outcome base-models context)
        modified-prediction (predict-outcome modified-models context)
        
        ;; Calculate impact
        impact (- (:probability modified-prediction) (:probability base-prediction))]
    
    {:scenario-id scenario-id
     :base-prediction base-prediction
     :modified-prediction modified-prediction
     :impact impact
     :impact-direction (cond
                         (> impact 0.1) :positive
                         (< impact -0.1) :negative
                         :else :neutral)
     :modifications-applied (count modifications)
     :timestamp (System/currentTimeMillis)}))

(defn compare-scenarios
  "Compare multiple scenarios."
  [scenario-ids]
  (let [results (mapv (fn [sid]
                        (merge {:scenario-id sid}
                               (run-scenario sid)))
                      scenario-ids)
        sorted (sort-by #(get-in % [:modified-prediction :probability]) > results)]
    {:scenarios sorted
     :best-scenario (:scenario-id (first sorted))
     :worst-scenario (:scenario-id (last sorted))
     :probability-range {:min (get-in (last sorted) [:modified-prediction :probability])
                         :max (get-in (first sorted) [:modified-prediction :probability])}}))

;; ============================================================================
;; Prediction Explanation
;; ============================================================================

(defn explain-prediction
  "Generate explanation for a prediction."
  [prediction-id]
  (when-let [prediction (get-prediction prediction-id)]
    (let [models (:models prediction)
          probability (:probability prediction)
          category (:outcome-category prediction)]
      {:prediction-id prediction-id
       :summary (format "Based on %d detected mental models, the predicted outcome probability is %.1f%% (%s)"
                        (count models)
                        (* 100 probability)
                        (name category))
       :key-factors [{:factor "Model count"
                      :value (:model-count prediction)
                      :impact (if (> (:model-count prediction) 3) :high :moderate)}
                     {:factor "Average confidence"
                      :value (:avg-confidence prediction)
                      :impact (if (> (:avg-confidence prediction) 0.7) :high :moderate)}
                     {:factor "Model interactions"
                      :value (:interaction-score prediction)
                      :impact (if (> (:interaction-score prediction) 0.2) :high :low)}]
       :contributing-models models
       :confidence-level (cond
                           (< (- (get-in prediction [:confidence-interval :upper])
                                 (get-in prediction [:confidence-interval :lower])) 0.2)
                           :high
                           (< (- (get-in prediction [:confidence-interval :upper])
                                 (get-in prediction [:confidence-interval :lower])) 0.4)
                           :moderate
                           :else :low)
       :recommendations (cond
                          (= category :highly-likely)
                          ["Consider the prediction reliable"
                           "Monitor for confirmation"]
                          (= category :uncertain)
                          ["Gather more information"
                           "Consider alternative scenarios"]
                          :else
                          ["Low confidence prediction"
                           "Seek additional data sources"])})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-engine-stats
  "Get prediction engine statistics."
  []
  (let [predictions (vals (:predictions @state))
        with-outcomes (filter :outcome predictions)
        accuracy-stats (get-accuracy-stats)]
    {:total-predictors (count (:predictors @state))
     :total-predictions (count predictions)
     :predictions-with-outcomes (count with-outcomes)
     :pending-outcomes (- (count predictions) (count with-outcomes))
     :total-scenarios (count (:scenarios @state))
     :total-ensembles (count (:ensembles @state))
     :accuracy-stats accuracy-stats
     :predictions-by-category (frequencies (map :outcome-category predictions))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-prediction-engine!
  "Initialize the prediction engine."
  []
  (when-not (:initialized? @state)
    ;; Register default predictors
    (register-predictor! :mental-model-outcome
                         {:name "Mental Model Outcome Predictor"
                          :type :ensemble
                          :features [:model-count :avg-confidence :interaction-score]
                          :target :outcome})
    
    (register-predictor! :time-series-trend
                         {:name "Time Series Trend Predictor"
                          :type :linear
                          :features [:historical-values]
                          :target :future-value})
    
    ;; Create default ensemble
    (create-ensemble! :default-ensemble
                      {:name "Default Prediction Ensemble"
                       :predictor-ids [:mental-model-outcome]
                       :aggregation :weighted-mean})
    
    ;; Create sample scenario
    (create-scenario! :baseline
                      {:name "Baseline Scenario"
                       :description "No modifications to detected models"
                       :base-models []
                       :modifications []
                       :context {}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Prediction engine initialized")
    (events/emit! :prediction-engine-initialized {})
    true))
