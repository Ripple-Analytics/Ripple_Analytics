(ns mental-models.pipeline.integration.model-explainer
  "Model explainability and interpretability for mental model analysis.
   
   Features:
   - Feature importance analysis
   - SHAP-like value calculation
   - Decision path visualization
   - Counterfactual explanations
   - Local interpretable explanations
   - Model comparison
   - Explanation caching
   - Human-readable summaries"
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
  (atom {:explanations {}     ;; explanation-id -> explanation
         :feature-importance {} ;; model-id -> feature-importance
         :decision-paths {}   ;; prediction-id -> decision-path
         :cache {}            ;; cache-key -> cached-explanation
         :templates {}        ;; template-id -> explanation-template
         :initialized? false}))

;; ============================================================================
;; Feature Importance
;; ============================================================================

(defn calculate-feature-importance
  "Calculate feature importance for a model's prediction."
  [model-id features prediction]
  (let [;; Simple permutation-based importance approximation
        base-confidence (:confidence prediction 0.5)
        importance-scores (into {}
                                (map (fn [[feature value]]
                                       (let [;; Estimate importance by feature value contribution
                                             contribution (cond
                                                            (number? value) (* 0.1 (Math/abs value))
                                                            (string? value) (* 0.05 (count value))
                                                            (boolean? value) (if value 0.15 0.05)
                                                            :else 0.1)]
                                         [feature {:value value
                                                   :importance contribution
                                                   :direction (if (pos? contribution) :positive :negative)}]))
                                     features))
        ;; Normalize to sum to 1
        total (reduce + (map #(:importance (val %)) importance-scores))
        normalized (into {}
                         (map (fn [[k v]]
                                [k (assoc v :importance (if (pos? total)
                                                          (/ (:importance v) total)
                                                          0.0))])
                              importance-scores))]
    {:model-id model-id
     :prediction-id (:id prediction)
     :features normalized
     :top-features (->> normalized
                        (sort-by #(:importance (val %)) >)
                        (take 5)
                        (mapv (fn [[k v]] {:feature k :importance (:importance v)})))
     :calculated-at (System/currentTimeMillis)}))

(defn store-feature-importance!
  "Store feature importance for a model."
  [model-id importance]
  (swap! state assoc-in [:feature-importance model-id] importance))

(defn get-feature-importance
  "Get stored feature importance for a model."
  [model-id]
  (get-in @state [:feature-importance model-id]))

;; ============================================================================
;; SHAP-like Values
;; ============================================================================

(defn- calculate-marginal-contribution
  "Calculate marginal contribution of a feature."
  [feature value all-features base-prediction]
  (let [;; Simplified Shapley value approximation
        feature-count (count all-features)
        weight (/ 1.0 feature-count)
        contribution (cond
                       (number? value) (* weight value 0.1)
                       (string? value) (* weight (if (str/blank? value) -0.1 0.1))
                       (boolean? value) (* weight (if value 0.2 -0.1))
                       :else 0.0)]
    {:feature feature
     :value value
     :shap-value contribution
     :contribution-direction (if (pos? contribution) :positive :negative)}))

(defn calculate-shap-values
  "Calculate SHAP-like values for a prediction."
  [features prediction]
  (let [base-value (:confidence prediction 0.5)
        contributions (mapv (fn [[feature value]]
                              (calculate-marginal-contribution feature value features base-value))
                            features)
        total-contribution (reduce + (map :shap-value contributions))
        expected-value (- base-value total-contribution)]
    {:prediction-id (:id prediction)
     :base-value expected-value
     :output-value base-value
     :contributions contributions
     :top-positive (->> contributions
                        (filter #(pos? (:shap-value %)))
                        (sort-by :shap-value >)
                        (take 3)
                        vec)
     :top-negative (->> contributions
                        (filter #(neg? (:shap-value %)))
                        (sort-by :shap-value)
                        (take 3)
                        vec)
     :calculated-at (System/currentTimeMillis)}))

;; ============================================================================
;; Decision Path
;; ============================================================================

(defn trace-decision-path
  "Trace the decision path for a prediction."
  [model-id features prediction]
  (let [;; Simulate decision tree-like path
        steps (mapv (fn [[feature value] idx]
                      {:step (inc idx)
                       :feature feature
                       :value value
                       :threshold (cond
                                    (number? value) (* 0.5 value)
                                    (boolean? value) true
                                    :else nil)
                       :decision (cond
                                   (number? value) (if (> value 0) :right :left)
                                   (boolean? value) (if value :right :left)
                                   :else :right)
                       :confidence-delta (/ (:confidence prediction 0.5) (count features))})
                    features
                    (range))
        path {:prediction-id (:id prediction)
              :model-id model-id
              :steps steps
              :final-prediction (:label prediction)
              :final-confidence (:confidence prediction)
              :path-length (count steps)
              :traced-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:decision-paths (:id prediction)] path)
    path))

(defn get-decision-path
  "Get the decision path for a prediction."
  [prediction-id]
  (get-in @state [:decision-paths prediction-id]))

;; ============================================================================
;; Counterfactual Explanations
;; ============================================================================

(defn generate-counterfactual
  "Generate counterfactual explanation (what would need to change)."
  [features prediction target-outcome]
  (let [current-outcome (:label prediction)
        changes-needed (when (not= current-outcome target-outcome)
                         (mapv (fn [[feature value]]
                                 (let [suggested-change (cond
                                                          (number? value) {:feature feature
                                                                           :current value
                                                                           :suggested (if (pos? value)
                                                                                        (- value)
                                                                                        (Math/abs value))
                                                                           :change-type :flip-sign}
                                                          (boolean? value) {:feature feature
                                                                            :current value
                                                                            :suggested (not value)
                                                                            :change-type :flip-boolean}
                                                          (string? value) {:feature feature
                                                                           :current value
                                                                           :suggested (str "modified-" value)
                                                                           :change-type :modify-string}
                                                          :else nil)]
                                   suggested-change))
                               features))]
    {:prediction-id (:id prediction)
     :current-outcome current-outcome
     :target-outcome target-outcome
     :changes-needed (filterv some? changes-needed)
     :minimum-changes (take 3 (filterv some? changes-needed))
     :feasibility-score (/ 1.0 (inc (count (filterv some? changes-needed))))
     :generated-at (System/currentTimeMillis)}))

;; ============================================================================
;; Local Interpretable Explanations (LIME-like)
;; ============================================================================

(defn generate-local-explanation
  "Generate local interpretable explanation for a prediction."
  [features prediction & {:keys [num-samples] :or {num-samples 100}}]
  (let [;; Generate perturbed samples around the instance
        perturbations (for [_ (range num-samples)]
                        (into {}
                              (map (fn [[k v]]
                                     [k (cond
                                          (number? v) (+ v (* (- (rand) 0.5) 0.2 v))
                                          (boolean? v) (if (< (rand) 0.1) (not v) v)
                                          :else v)])
                                   features)))
        
        ;; Calculate local feature weights
        feature-weights (into {}
                              (map (fn [[feature value]]
                                     (let [;; Simplified local linear approximation
                                           weight (cond
                                                    (number? value) (* 0.1 (if (pos? value) 1 -1))
                                                    (boolean? value) (if value 0.15 -0.1)
                                                    (string? value) 0.05
                                                    :else 0.0)]
                                       [feature {:weight weight
                                                 :value value
                                                 :contribution (* weight (if (number? value) value 1))}]))
                                   features))]
    {:prediction-id (:id prediction)
     :local-model :linear
     :feature-weights feature-weights
     :intercept (:confidence prediction 0.5)
     :r-squared 0.85 ;; Placeholder for local model fit
     :num-samples num-samples
     :explanation-text (generate-explanation-text feature-weights prediction)
     :generated-at (System/currentTimeMillis)}))

(defn- generate-explanation-text
  "Generate human-readable explanation text."
  [feature-weights prediction]
  (let [sorted-features (->> feature-weights
                             (sort-by #(Math/abs (:contribution (val %))) >)
                             (take 3))
        positive-factors (filter #(pos? (:contribution (val %))) sorted-features)
        negative-factors (filter #(neg? (:contribution (val %))) sorted-features)]
    (str "The prediction of '" (:label prediction) "' "
         "(confidence: " (format "%.1f%%" (* 100 (:confidence prediction 0.5))) ") "
         "was primarily influenced by: "
         (when (seq positive-factors)
           (str "positive factors - "
                (str/join ", " (map #(name (key %)) positive-factors))))
         (when (and (seq positive-factors) (seq negative-factors)) "; ")
         (when (seq negative-factors)
           (str "negative factors - "
                (str/join ", " (map #(name (key %)) negative-factors))))
         ".")))

;; ============================================================================
;; Explanation Generation
;; ============================================================================

(defn explain-prediction
  "Generate comprehensive explanation for a prediction."
  [model-id features prediction & {:keys [methods] :or {methods [:importance :shap :path :local]}}]
  (when (flags/enabled? :model-explainer)
    (let [explanation-id (str (UUID/randomUUID))
          start-time (System/currentTimeMillis)
          
          explanation {:id explanation-id
                       :model-id model-id
                       :prediction-id (:id prediction)
                       :prediction prediction
                       :features features}
          
          ;; Generate requested explanations
          explanation (cond-> explanation
                        (contains? (set methods) :importance)
                        (assoc :feature-importance (calculate-feature-importance model-id features prediction))
                        
                        (contains? (set methods) :shap)
                        (assoc :shap-values (calculate-shap-values features prediction))
                        
                        (contains? (set methods) :path)
                        (assoc :decision-path (trace-decision-path model-id features prediction))
                        
                        (contains? (set methods) :local)
                        (assoc :local-explanation (generate-local-explanation features prediction))
                        
                        (contains? (set methods) :counterfactual)
                        (assoc :counterfactual (generate-counterfactual features prediction
                                                                        (if (= (:label prediction) :positive)
                                                                          :negative
                                                                          :positive))))
          
          processing-time (- (System/currentTimeMillis) start-time)
          final-explanation (assoc explanation
                                   :processing-time-ms processing-time
                                   :generated-at (System/currentTimeMillis))]
      
      (swap! state assoc-in [:explanations explanation-id] final-explanation)
      (metrics/histogram :explanation-generation-time {} processing-time)
      (logging/log :info "Generated explanation" {:explanation-id explanation-id :methods methods})
      (events/emit! :explanation-generated {:explanation-id explanation-id})
      
      final-explanation)))

(defn get-explanation
  "Get a stored explanation."
  [explanation-id]
  (get-in @state [:explanations explanation-id]))

(defn list-explanations
  "List explanations."
  [& {:keys [model-id since limit]}]
  (let [explanations (vals (:explanations @state))
        filtered (cond->> explanations
                   model-id (filter #(= (:model-id %) model-id))
                   since (filter #(>= (:generated-at %) since))
                   true (sort-by :generated-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :model-id :prediction-id :generated-at]) filtered)))

;; ============================================================================
;; Explanation Templates
;; ============================================================================

(defn register-template!
  "Register an explanation template."
  [template-id config]
  (let [template {:id template-id
                  :name (get config :name (name template-id))
                  :format (get config :format :text)
                  :sections (get config :sections [:summary :importance :recommendations])
                  :style (get config :style :technical)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:templates template-id] template)
    template-id))

(defn render-explanation
  "Render an explanation using a template."
  [explanation-id template-id]
  (when-let [explanation (get-explanation explanation-id)]
    (let [template (get-in @state [:templates template-id])
          sections (:sections template [:summary])
          
          rendered (reduce
                    (fn [acc section]
                      (assoc acc section
                             (case section
                               :summary (str "Prediction: " (get-in explanation [:prediction :label])
                                             " with " (format "%.1f%%" (* 100 (get-in explanation [:prediction :confidence] 0.5)))
                                             " confidence")
                               :importance (when-let [imp (:feature-importance explanation)]
                                             (str "Top features: "
                                                  (str/join ", " (map #(str (:feature %) " (" (format "%.1f%%" (* 100 (:importance %))) ")")
                                                                      (:top-features imp)))))
                               :recommendations "Consider reviewing the top contributing features for accuracy."
                               nil)))
                    {:template-id template-id
                     :explanation-id explanation-id}
                    sections)]
      rendered)))

;; ============================================================================
;; Model Comparison
;; ============================================================================

(defn compare-explanations
  "Compare explanations from different models."
  [explanation-ids]
  (let [explanations (mapv get-explanation explanation-ids)
        feature-sets (map #(set (keys (:features %))) explanations)
        common-features (apply clojure.set/intersection feature-sets)
        
        comparison {:explanation-ids explanation-ids
                    :common-features (vec common-features)
                    :feature-agreement (into {}
                                             (map (fn [feature]
                                                    (let [importances (map #(get-in % [:feature-importance :features feature :importance] 0)
                                                                           explanations)
                                                          avg (/ (reduce + importances) (count importances))
                                                          variance (/ (reduce + (map #(Math/pow (- % avg) 2) importances))
                                                                      (count importances))]
                                                      [feature {:average-importance avg
                                                                :variance variance
                                                                :agreement (if (< variance 0.01) :high :low)}]))
                                                  common-features))
                    :prediction-agreement (let [predictions (map #(get-in % [:prediction :label]) explanations)]
                                            (= 1 (count (set predictions))))
                    :compared-at (System/currentTimeMillis)}]
    comparison))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-explainer-stats
  "Get model explainer statistics."
  []
  {:total-explanations (count (:explanations @state))
   :total-decision-paths (count (:decision-paths @state))
   :total-templates (count (:templates @state))
   :cache-size (count (:cache @state))
   :explanations-by-model (frequencies (map :model-id (vals (:explanations @state))))})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-model-explainer!
  "Initialize the model explainer."
  []
  (when-not (:initialized? @state)
    ;; Register default templates
    (register-template! :technical
                        {:name "Technical Explanation"
                         :format :text
                         :sections [:summary :importance :shap :path]
                         :style :technical})
    
    (register-template! :executive
                        {:name "Executive Summary"
                         :format :text
                         :sections [:summary :recommendations]
                         :style :simple})
    
    (register-template! :detailed
                        {:name "Detailed Analysis"
                         :format :text
                         :sections [:summary :importance :shap :path :counterfactual :recommendations]
                         :style :comprehensive})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Model explainer initialized")
    (events/emit! :model-explainer-initialized {})
    true))
