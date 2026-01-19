(ns mental-models.pipeline.integration.ab-testing
  "A/B testing framework for mental model analysis experiments.
   
   Features:
   - Experiment definition and management
   - Variant assignment with traffic splitting
   - Statistical significance calculation
   - Conversion tracking
   - Multi-armed bandit support
   - Experiment scheduling
   - Results analysis and reporting
   - Feature flag integration"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:experiments {}      ;; experiment-id -> experiment
         :assignments {}      ;; user-id -> {experiment-id -> variant}
         :conversions {}      ;; experiment-id -> [conversion-events]
         :metrics {}          ;; experiment-id -> {variant -> metrics}
         :initialized? false}))

(def ^:private random (Random.))

;; ============================================================================
;; Experiment Definition
;; ============================================================================

(defn create-experiment!
  "Create a new A/B test experiment."
  [experiment-id config]
  (let [experiment {:id experiment-id
                    :name (get config :name (name experiment-id))
                    :description (get config :description "")
                    :hypothesis (get config :hypothesis "")
                    :variants (get config :variants [{:id :control :weight 50}
                                                      {:id :treatment :weight 50}])
                    :traffic-percentage (get config :traffic-percentage 100)
                    :targeting (get config :targeting {})
                    :primary-metric (get config :primary-metric :conversion)
                    :secondary-metrics (get config :secondary-metrics [])
                    :minimum-sample-size (get config :minimum-sample-size 1000)
                    :confidence-level (get config :confidence-level 0.95)
                    :status :draft
                    :start-date nil
                    :end-date nil
                    :created-at (System/currentTimeMillis)
                    :created-by (get config :created-by "system")}]
    (swap! state assoc-in [:experiments experiment-id] experiment)
    (swap! state assoc-in [:metrics experiment-id]
           (into {} (map (fn [v] [(:id v) {:impressions 0 :conversions 0}])
                         (:variants experiment))))
    (logging/log :info "Created experiment" {:experiment-id experiment-id})
    (events/emit! :experiment-created {:experiment-id experiment-id})
    experiment-id))

(defn get-experiment
  "Get an experiment."
  [experiment-id]
  (get-in @state [:experiments experiment-id]))

(defn list-experiments
  "List all experiments."
  [& {:keys [status]}]
  (let [experiments (vals (:experiments @state))
        filtered (if status
                   (filter #(= (:status %) status) experiments)
                   experiments)]
    (mapv #(select-keys % [:id :name :status :start-date :end-date]) filtered)))

(defn update-experiment!
  "Update an experiment."
  [experiment-id updates]
  (swap! state update-in [:experiments experiment-id] merge updates)
  (logging/log :info "Updated experiment" {:experiment-id experiment-id}))

;; ============================================================================
;; Experiment Lifecycle
;; ============================================================================

(defn start-experiment!
  "Start an experiment."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (when (= (:status experiment) :draft)
      (update-experiment! experiment-id
                          {:status :running
                           :start-date (System/currentTimeMillis)})
      (logging/log :info "Started experiment" {:experiment-id experiment-id})
      (events/emit! :experiment-started {:experiment-id experiment-id}))))

(defn pause-experiment!
  "Pause an experiment."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (when (= (:status experiment) :running)
      (update-experiment! experiment-id {:status :paused})
      (logging/log :info "Paused experiment" {:experiment-id experiment-id})
      (events/emit! :experiment-paused {:experiment-id experiment-id}))))

(defn resume-experiment!
  "Resume a paused experiment."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (when (= (:status experiment) :paused)
      (update-experiment! experiment-id {:status :running})
      (logging/log :info "Resumed experiment" {:experiment-id experiment-id})
      (events/emit! :experiment-resumed {:experiment-id experiment-id}))))

(defn stop-experiment!
  "Stop an experiment."
  [experiment-id & {:keys [winner]}]
  (when-let [experiment (get-experiment experiment-id)]
    (when (#{:running :paused} (:status experiment))
      (update-experiment! experiment-id
                          {:status :completed
                           :end-date (System/currentTimeMillis)
                           :winner winner})
      (logging/log :info "Stopped experiment" {:experiment-id experiment-id :winner winner})
      (events/emit! :experiment-completed {:experiment-id experiment-id :winner winner}))))

;; ============================================================================
;; Variant Assignment
;; ============================================================================

(defn- weighted-random-selection
  "Select a variant based on weights."
  [variants]
  (let [total-weight (reduce + (map :weight variants))
        rand-val (* (.nextDouble random) total-weight)]
    (loop [remaining variants
           cumulative 0]
      (if (empty? remaining)
        (:id (last variants))
        (let [variant (first remaining)
              new-cumulative (+ cumulative (:weight variant))]
          (if (< rand-val new-cumulative)
            (:id variant)
            (recur (rest remaining) new-cumulative)))))))

(defn- check-targeting
  "Check if user matches targeting criteria."
  [user-context targeting]
  (if (empty? targeting)
    true
    (every? (fn [[k v]]
              (let [user-val (get user-context k)]
                (cond
                  (set? v) (contains? v user-val)
                  (fn? v) (v user-val)
                  :else (= user-val v))))
            targeting)))

(defn- should-include-in-experiment?
  "Check if user should be included in experiment."
  [experiment user-id]
  (let [traffic-pct (:traffic-percentage experiment)
        hash-val (mod (Math/abs (hash (str (:id experiment) user-id))) 100)]
    (< hash-val traffic-pct)))

(defn assign-variant
  "Assign a variant to a user for an experiment."
  [experiment-id user-id & {:keys [user-context]}]
  (when (flags/enabled? :ab-testing)
    (let [experiment (get-experiment experiment-id)]
      (when (and experiment (= (:status experiment) :running))
        ;; Check if already assigned
        (if-let [existing (get-in @state [:assignments user-id experiment-id])]
          existing
          ;; Check targeting and traffic
          (when (and (check-targeting (or user-context {}) (:targeting experiment))
                     (should-include-in-experiment? experiment user-id))
            (let [variant (weighted-random-selection (:variants experiment))]
              (swap! state assoc-in [:assignments user-id experiment-id] variant)
              (swap! state update-in [:metrics experiment-id variant :impressions] inc)
              (logging/log :debug "Assigned variant" {:experiment-id experiment-id :user-id user-id :variant variant})
              (metrics/increment :ab-assignments {:experiment-id experiment-id :variant variant})
              variant)))))))

(defn get-assignment
  "Get a user's variant assignment for an experiment."
  [experiment-id user-id]
  (get-in @state [:assignments user-id experiment-id]))

(defn get-user-assignments
  "Get all experiment assignments for a user."
  [user-id]
  (get-in @state [:assignments user-id] {}))

;; ============================================================================
;; Conversion Tracking
;; ============================================================================

(defn track-conversion!
  "Track a conversion event."
  [experiment-id user-id & {:keys [value metadata]}]
  (when (flags/enabled? :ab-testing)
    (when-let [variant (get-assignment experiment-id user-id)]
      (let [conversion {:user-id user-id
                        :variant variant
                        :value (or value 1)
                        :metadata metadata
                        :timestamp (System/currentTimeMillis)}]
        (swap! state update-in [:conversions experiment-id] conj conversion)
        (swap! state update-in [:metrics experiment-id variant :conversions] inc)
        (when value
          (swap! state update-in [:metrics experiment-id variant :total-value]
                 (fn [v] (+ (or v 0) value))))
        (logging/log :debug "Tracked conversion" {:experiment-id experiment-id :user-id user-id :variant variant})
        (metrics/increment :ab-conversions {:experiment-id experiment-id :variant variant})
        conversion))))

(defn get-conversions
  "Get conversions for an experiment."
  [experiment-id & {:keys [variant since limit]}]
  (let [conversions (get-in @state [:conversions experiment-id] [])
        filtered (cond->> conversions
                   variant (filter #(= (:variant %) variant))
                   since (filter #(>= (:timestamp %) since))
                   true (sort-by :timestamp >)
                   limit (take limit))]
    (vec filtered)))

;; ============================================================================
;; Statistical Analysis
;; ============================================================================

(defn- calculate-conversion-rate
  "Calculate conversion rate for a variant."
  [metrics]
  (let [impressions (:impressions metrics 0)
        conversions (:conversions metrics 0)]
    (if (pos? impressions)
      (/ conversions (double impressions))
      0.0)))

(defn- calculate-standard-error
  "Calculate standard error for a proportion."
  [p n]
  (if (pos? n)
    (Math/sqrt (/ (* p (- 1 p)) n))
    0.0))

(defn- calculate-z-score
  "Calculate z-score for two proportions."
  [p1 n1 p2 n2]
  (let [p-pooled (/ (+ (* p1 n1) (* p2 n2)) (+ n1 n2))
        se (Math/sqrt (* p-pooled (- 1 p-pooled) (+ (/ 1 n1) (/ 1 n2))))]
    (if (pos? se)
      (/ (- p1 p2) se)
      0.0)))

(defn- z-to-p-value
  "Convert z-score to p-value (two-tailed)."
  [z]
  ;; Approximation using error function
  (let [abs-z (Math/abs z)
        t (/ 1.0 (+ 1.0 (* 0.2316419 abs-z)))
        d (/ (Math/exp (- (/ (* abs-z abs-z) 2))) (Math/sqrt (* 2 Math/PI)))
        p (* d t (+ 0.319381530
                    (* t (+ -0.356563782
                            (* t (+ 1.781477937
                                    (* t (+ -1.821255978
                                            (* t 1.330274429)))))))))]
    (* 2 p)))

(defn analyze-experiment
  "Analyze experiment results."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (let [metrics (:metrics @state)
          exp-metrics (get metrics experiment-id {})
          variants (:variants experiment)
          
          ;; Calculate stats for each variant
          variant-stats (into {}
                              (map (fn [v]
                                     (let [m (get exp-metrics (:id v) {:impressions 0 :conversions 0})
                                           rate (calculate-conversion-rate m)
                                           se (calculate-standard-error rate (:impressions m 0))]
                                       [(:id v)
                                        {:impressions (:impressions m 0)
                                         :conversions (:conversions m 0)
                                         :conversion-rate rate
                                         :standard-error se
                                         :ci-lower (max 0 (- rate (* 1.96 se)))
                                         :ci-upper (min 1 (+ rate (* 1.96 se)))}]))
                                   variants))
          
          ;; Find control and treatment
          control-id (or (first (filter #(= (:id %) :control) variants))
                         (first variants))
          control-stats (get variant-stats (:id control-id))
          
          ;; Calculate significance for each treatment vs control
          comparisons (for [v variants
                           :when (not= (:id v) (:id control-id))]
                        (let [treatment-stats (get variant-stats (:id v))
                              z (calculate-z-score
                                 (:conversion-rate treatment-stats)
                                 (:impressions treatment-stats)
                                 (:conversion-rate control-stats)
                                 (:impressions control-stats))
                              p-value (z-to-p-value z)
                              lift (if (pos? (:conversion-rate control-stats))
                                     (/ (- (:conversion-rate treatment-stats)
                                           (:conversion-rate control-stats))
                                        (:conversion-rate control-stats))
                                     0.0)]
                          {:variant (:id v)
                           :z-score z
                           :p-value p-value
                           :significant? (< p-value (- 1 (:confidence-level experiment)))
                           :lift lift
                           :lift-percent (* 100 lift)}))
          
          ;; Determine winner
          total-samples (reduce + (map :impressions (vals variant-stats)))
          has-enough-samples? (>= total-samples (:minimum-sample-size experiment))
          significant-winners (filter :significant? comparisons)
          winner (when (and has-enough-samples? (seq significant-winners))
                   (:variant (first (sort-by :lift > significant-winners))))]
      
      {:experiment-id experiment-id
       :status (:status experiment)
       :variant-stats variant-stats
       :comparisons (vec comparisons)
       :total-samples total-samples
       :has-enough-samples? has-enough-samples?
       :winner winner
       :recommendation (cond
                         (not has-enough-samples?) :continue-collecting
                         winner :implement-winner
                         :else :no-significant-difference)
       :analyzed-at (System/currentTimeMillis)})))

;; ============================================================================
;; Multi-Armed Bandit
;; ============================================================================

(defn- thompson-sampling-select
  "Select variant using Thompson Sampling."
  [variant-stats]
  (let [samples (map (fn [[variant stats]]
                       (let [alpha (+ 1 (:conversions stats 0))
                             beta (+ 1 (- (:impressions stats 0) (:conversions stats 0)))
                             ;; Beta distribution approximation
                             sample (/ alpha (+ alpha beta))]
                         {:variant variant :sample sample}))
                     variant-stats)]
    (:variant (first (sort-by :sample > samples)))))

(defn assign-variant-bandit
  "Assign variant using multi-armed bandit algorithm."
  [experiment-id user-id & {:keys [algorithm] :or {algorithm :thompson}}]
  (when (flags/enabled? :ab-testing)
    (let [experiment (get-experiment experiment-id)]
      (when (and experiment (= (:status experiment) :running))
        (if-let [existing (get-assignment experiment-id user-id)]
          existing
          (let [exp-metrics (get-in @state [:metrics experiment-id] {})
                variant (case algorithm
                          :thompson (thompson-sampling-select exp-metrics)
                          :epsilon-greedy (if (< (.nextDouble random) 0.1)
                                            (weighted-random-selection (:variants experiment))
                                            (let [best (first (sort-by #(calculate-conversion-rate (val %)) > exp-metrics))]
                                              (key best)))
                          (thompson-sampling-select exp-metrics))]
            (swap! state assoc-in [:assignments user-id experiment-id] variant)
            (swap! state update-in [:metrics experiment-id variant :impressions] inc)
            variant))))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-testing-stats
  "Get A/B testing statistics."
  []
  (let [experiments (vals (:experiments @state))
        by-status (group-by :status experiments)]
    {:total-experiments (count experiments)
     :by-status (into {} (map (fn [[k v]] [k (count v)]) by-status))
     :total-assignments (reduce + (map count (vals (:assignments @state))))
     :total-conversions (reduce + (map count (vals (:conversions @state))))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-ab-testing!
  "Initialize A/B testing framework."
  []
  (when-not (:initialized? @state)
    ;; Create sample experiment
    (create-experiment! :model-detection-algorithm
                        {:name "Model Detection Algorithm Test"
                         :description "Test new vs old mental model detection algorithm"
                         :hypothesis "New algorithm will increase detection accuracy by 10%"
                         :variants [{:id :control :weight 50}
                                    {:id :new-algorithm :weight 50}]
                         :primary-metric :detection-accuracy
                         :minimum-sample-size 500})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "A/B testing framework initialized")
    (events/emit! :ab-testing-initialized {})
    true))
