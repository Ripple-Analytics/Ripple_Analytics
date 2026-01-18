(ns mental-models.analysis.bayesian
  "Bayesian Analysis Engine - Electric Clojure
   Replaces Python bayesian.py and bayesian_updater.py
   Probabilistic inference for mental model confidence updates"
  (:require [clojure.math :as math]
            [taoensso.timbre :as log]))

;; -- Bayesian Inference --

(defn prior
  "Calculate prior probability for a model"
  [model-frequency total-models]
  (/ model-frequency total-models))

(defn likelihood
  "Calculate likelihood of evidence given model
   Uses LM Studio confidence score as evidence"
  [evidence-score]
  (let [;; Convert 0-1 score to likelihood
        ;; Higher score = higher likelihood of model being correct
        likelihood (math/pow evidence-score 2)]
    likelihood))

(defn posterior
  "Calculate posterior probability using Bayes' theorem
   P(Model|Evidence) = P(Evidence|Model) * P(Model) / P(Evidence)"
  [prior-prob likelihood-score evidence-prob]
  (if (zero? evidence-prob)
    prior-prob
    (/ (* likelihood-score prior-prob) evidence-prob)))

(defn evidence-probability
  "Calculate total probability of evidence across all models
   P(Evidence) = Σ P(Evidence|Model) * P(Model)"
  [models evidence-scores]
  (let [total (reduce + (map-indexed (fn [idx model]
                                      (let [score (nth evidence-scores idx 0.0)
                                            likelihood (likelihood score)
                                            prior-prob (prior 1 (count models))]
                                        (* likelihood prior-prob)))
                                    models))]
    (if (zero? total) 1.0 total)))

;; -- Bayesian Update --

(defrecord BayesianUpdate
  [model-id model-name prior-prob likelihood-score posterior-prob confidence])

(defn update-belief
  "Update belief about a model given new evidence"
  [model prior-prob evidence-score]
  (let [likelihood-score (likelihood evidence-score)
        ;; Simplified: assume uniform evidence probability
        evidence-prob 0.5
        posterior-prob (posterior prior-prob likelihood-score evidence-prob)
        confidence (min 0.99 (max 0.01 posterior-prob))]
    
    (->BayesianUpdate
     (:id model)
     (:name model)
     prior-prob
     likelihood-score
     posterior-prob
     confidence)))

(defn batch-update-beliefs
  "Update beliefs for multiple models"
  [models evidence-scores priors]
  (mapv (fn [idx model]
         (let [evidence-score (nth evidence-scores idx 0.0)
               prior-prob (nth priors idx (/ 1.0 (count models)))]
           (update-belief model prior-prob evidence-score)))
       (range (count models))
       models))

;; -- Confidence Aggregation --

(defn aggregate-confidence
  "Aggregate confidence across multiple analyses"
  [analyses]
  (let [total (count analyses)
        sum (reduce + (map :confidence analyses))]
    (/ sum total)))

(defn weighted-confidence
  "Calculate weighted confidence based on recency"
  [analyses]
  (let [now (System/currentTimeMillis)
        weighted-sum (reduce + (map-indexed (fn [idx analysis]
                                             (let [age (- now (:timestamp analysis))
                                                   ;; Exponential decay: older = lower weight
                                                   weight (math/exp (- (/ age 86400000)))  ;; 1 day = 1.0
                                                   confidence (:confidence analysis)]
                                               (* weight confidence)))
                                           analyses))
        weight-sum (reduce + (map-indexed (fn [idx analysis]
                                          (let [age (- now (:timestamp analysis))
                                                weight (math/exp (- (/ age 86400000)))]
                                            weight))
                                        analyses))]
    (if (zero? weight-sum) 0.0 (/ weighted-sum weight-sum))))

;; -- Confidence Intervals --

(defn confidence-interval
  "Calculate 95% confidence interval for belief"
  [mean variance]
  (let [std-dev (math/sqrt variance)
        z-score 1.96  ;; 95% CI
        margin (* z-score std-dev)]
    {:lower (- mean margin)
     :upper (+ mean margin)
     :margin margin}))

(defn calculate-variance
  "Calculate variance of confidence estimates"
  [analyses]
  (let [mean (aggregate-confidence analyses)
        squared-diffs (map #(math/pow (- (:confidence %) mean) 2) analyses)
        sum (reduce + squared-diffs)]
    (/ sum (count analyses))))

;; -- Convergence Detection --

(defn has-converged?
  "Check if beliefs have converged (low variance)"
  [analyses threshold]
  (let [variance (calculate-variance analyses)]
    (<= variance threshold)))

(defn convergence-rate
  "Calculate rate of convergence"
  [analyses]
  (if (< (count analyses) 2)
    0.0
    (let [recent (take 2 (reverse analyses))
          [prev curr] recent
          variance-change (- (:variance prev) (:variance curr))]
      (max 0.0 variance-change))))

;; -- Sensitivity Analysis --

(defn sensitivity-analysis
  "Analyze sensitivity of posterior to prior changes"
  [model evidence-score prior-range]
  (let [likelihood-score (likelihood evidence-score)
        priors (range (:min prior-range) (:max prior-range) 0.01)
        posteriors (map #(posterior % likelihood-score 0.5) priors)]
    
    {:priors priors
     :posteriors posteriors
     :sensitivity (/ (- (apply max posteriors) (apply min posteriors))
                    (- (:max prior-range) (:min prior-range)))}))

;; -- Model Comparison --

(defn bayes-factor
  "Calculate Bayes factor to compare two models
   BF = P(Evidence|Model1) / P(Evidence|Model2)"
  [likelihood1 likelihood2]
  (if (zero? likelihood2)
    Double/POSITIVE_INFINITY
    (/ likelihood1 likelihood2)))

(defn compare-models
  "Compare two models using Bayes factor"
  [model1-evidence model2-evidence]
  (let [bf (bayes-factor
           (likelihood model1-evidence)
           (likelihood model2-evidence))]
    {:bayes-factor bf
     :interpretation (cond
                      (> bf 10) "Strong evidence for model 1"
                      (> bf 3) "Moderate evidence for model 1"
                      (> bf 1) "Weak evidence for model 1"
                      (< bf 0.1) "Strong evidence for model 2"
                      (< bf 0.33) "Moderate evidence for model 2"
                      (< bf 1) "Weak evidence for model 2"
                      :else "No significant difference")}))

;; -- Hierarchical Bayesian Model --

(defn hierarchical-prior
  "Calculate hierarchical prior based on model category"
  [model category-frequency total-models]
  (let [category-prior (/ category-frequency total-models)
        ;; Within-category uniform prior
        within-category-prior 0.5
        ;; Combine: 70% from category, 30% from global
        combined (+ (* 0.7 category-prior) (* 0.3 within-category-prior))]
    combined))

(defn hierarchical-update
  "Update beliefs using hierarchical model"
  [model evidence-score category-frequency total-models]
  (let [prior-prob (hierarchical-prior model category-frequency total-models)
        likelihood-score (likelihood evidence-score)
        posterior-prob (posterior prior-prob likelihood-score 0.5)]
    
    {:model-id (:id model)
     :model-name (:name model)
     :prior prior-prob
     :likelihood likelihood-score
     :posterior posterior-prob}))

;; -- Predictive Distribution --

(defn predictive-distribution
  "Calculate predictive distribution for future evidence"
  [posterior-prob]
  (let [;; Beta distribution parameters
        alpha (+ posterior-prob 1)
        beta (+ (- 1 posterior-prob) 1)
        ;; Mean of beta distribution
        mean (/ alpha (+ alpha beta))
        ;; Variance of beta distribution
        variance (/ (* alpha beta) (* (math/pow (+ alpha beta) 2) (+ alpha beta 1)))]
    
    {:mean mean
     :variance variance
     :std-dev (math/sqrt variance)}))

;; -- Utilities --

(defn normalize-probabilities
  "Normalize probabilities to sum to 1"
  [probs]
  (let [total (reduce + probs)]
    (if (zero? total)
      (vec (repeat (count probs) (/ 1.0 (count probs))))
      (mapv #(/ % total) probs))))

(defn log-odds
  "Convert probability to log odds"
  [prob]
  (math/log (/ prob (- 1 prob))))

(defn odds-from-log-odds
  "Convert log odds back to probability"
  [log-odds-val]
  (let [odds (math/exp log-odds-val)]
    (/ odds (+ 1 odds))))
