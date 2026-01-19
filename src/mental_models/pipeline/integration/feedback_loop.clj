(ns mental-models.pipeline.integration.feedback-loop
  "Feedback Loop Module
   
   Continuous improvement through feedback:
   - User feedback collection
   - Model accuracy tracking
   - Automatic calibration
   - A/B testing support
   - Performance optimization"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong AtomicReference]))

;; =============================================================================
;; FEEDBACK LOOP STATE
;; =============================================================================

(defonce loop-state (atom {:feedback (ConcurrentHashMap.)
                           :accuracy-records (ConcurrentHashMap.)
                           :calibrations (ConcurrentHashMap.)
                           :experiments (ConcurrentHashMap.)
                           :optimizations (ConcurrentHashMap.)
                           :feedback-count (AtomicLong. 0)
                           :config {:min-samples-for-calibration 10
                                    :calibration-threshold 0.1
                                    :experiment-min-samples 100
                                    :confidence-level 0.95}}))

;; =============================================================================
;; FEEDBACK COLLECTION
;; =============================================================================

(defn record-feedback!
  "Record user feedback on a prediction/detection."
  [feedback-id {:keys [prediction-id prediction-type predicted-value actual-value
                       user-id confidence context timestamp]}]
  (.incrementAndGet ^AtomicLong (:feedback-count @loop-state))
  (metrics/inc-counter! :feedbackloop/feedback)
  (let [is-correct (= predicted-value actual-value)
        feedback {:id feedback-id
                  :prediction-id prediction-id
                  :prediction-type prediction-type
                  :predicted-value predicted-value
                  :actual-value actual-value
                  :is-correct is-correct
                  :user-id user-id
                  :confidence confidence
                  :context context
                  :timestamp (or timestamp (System/currentTimeMillis))}]
    (.put ^ConcurrentHashMap (:feedback @loop-state) feedback-id feedback)
    (log/info "Feedback recorded" {:id feedback-id :correct is-correct})
    (events/publish! :feedback/recorded {:feedback-id feedback-id :correct is-correct})
    feedback))

(defn get-feedback
  "Get feedback by ID."
  [feedback-id]
  (.get ^ConcurrentHashMap (:feedback @loop-state) feedback-id))

(defn list-feedback
  "List feedback records."
  [& {:keys [prediction-type user-id correct-only incorrect-only limit since]}]
  (let [feedback (vals (:feedback @loop-state))]
    (cond->> feedback
      prediction-type (filter #(= (:prediction-type %) prediction-type))
      user-id (filter #(= (:user-id %) user-id))
      correct-only (filter :is-correct)
      incorrect-only (filter (complement :is-correct))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

;; =============================================================================
;; ACCURACY TRACKING
;; =============================================================================

(defn calculate-accuracy
  "Calculate accuracy for a prediction type."
  [prediction-type & {:keys [since]}]
  (let [feedback (list-feedback :prediction-type prediction-type :since since)
        total (count feedback)
        correct (count (filter :is-correct feedback))]
    (when (pos? total)
      {:prediction-type prediction-type
       :total-samples total
       :correct-count correct
       :accuracy (/ correct total)
       :calculated-at (System/currentTimeMillis)})))

(defn record-accuracy!
  "Record accuracy measurement."
  [record-id accuracy-data]
  (.put ^ConcurrentHashMap (:accuracy-records @loop-state) record-id
        (assoc accuracy-data :id record-id :recorded-at (System/currentTimeMillis))))

(defn get-accuracy-history
  "Get accuracy history for a prediction type."
  [prediction-type & {:keys [limit]}]
  (let [records (vals (:accuracy-records @loop-state))]
    (cond->> records
      true (filter #(= (:prediction-type %) prediction-type))
      true (sort-by :recorded-at >)
      limit (take limit))))

(defn detect-accuracy-trend
  "Detect trend in accuracy over time."
  [prediction-type]
  (let [history (get-accuracy-history prediction-type :limit 10)
        accuracies (map :accuracy history)]
    (when (>= (count accuracies) 3)
      (let [recent-avg (/ (reduce + (take 3 accuracies)) 3)
            older-avg (/ (reduce + (drop 3 accuracies)) (max 1 (count (drop 3 accuracies))))
            diff (- recent-avg older-avg)]
        {:trend (cond
                  (> diff 0.05) :improving
                  (< diff -0.05) :declining
                  :else :stable)
         :recent-accuracy recent-avg
         :older-accuracy older-avg
         :change diff}))))

;; =============================================================================
;; AUTOMATIC CALIBRATION
;; =============================================================================

(defn calculate-calibration-adjustment
  "Calculate calibration adjustment based on feedback."
  [prediction-type]
  (let [feedback (list-feedback :prediction-type prediction-type)
        min-samples (get-in @loop-state [:config :min-samples-for-calibration])]
    (when (>= (count feedback) min-samples)
      ;; Group by confidence buckets
      (let [buckets (group-by #(int (* 10 (:confidence % 0.5))) feedback)
            adjustments (map (fn [[bucket items]]
                               (let [expected-accuracy (/ bucket 10.0)
                                     actual-accuracy (/ (count (filter :is-correct items))
                                                        (count items))
                                     adjustment (- actual-accuracy expected-accuracy)]
                                 {:bucket bucket
                                  :expected expected-accuracy
                                  :actual actual-accuracy
                                  :adjustment adjustment
                                  :sample-size (count items)}))
                             buckets)]
        {:prediction-type prediction-type
         :adjustments adjustments
         :overall-bias (/ (reduce + (map :adjustment adjustments))
                          (count adjustments))
         :calculated-at (System/currentTimeMillis)}))))

(defn apply-calibration!
  "Apply calibration adjustment."
  [calibration-id {:keys [prediction-type adjustments]}]
  (log/info "Applying calibration" {:id calibration-id :type prediction-type})
  (.put ^ConcurrentHashMap (:calibrations @loop-state) calibration-id
        {:id calibration-id
         :prediction-type prediction-type
         :adjustments adjustments
         :applied-at (System/currentTimeMillis)
         :status :active}))

(defn get-active-calibration
  "Get active calibration for a prediction type."
  [prediction-type]
  (->> (vals (:calibrations @loop-state))
       (filter #(and (= (:prediction-type %) prediction-type)
                     (= (:status %) :active)))
       (sort-by :applied-at >)
       first))

(defn calibrate-confidence
  "Calibrate a confidence score using active calibration."
  [prediction-type raw-confidence]
  (if-let [calibration (get-active-calibration prediction-type)]
    (let [bucket (int (* 10 raw-confidence))
          adjustment (or (some #(when (= (:bucket %) bucket) (:adjustment %))
                               (:adjustments calibration))
                         0)]
      (max 0 (min 1 (+ raw-confidence adjustment))))
    raw-confidence))

;; =============================================================================
;; A/B TESTING
;; =============================================================================

(defn create-experiment!
  "Create an A/B testing experiment."
  [experiment-id {:keys [name description variants allocation hypothesis]}]
  (log/info "Creating experiment" {:id experiment-id :name name})
  (.put ^ConcurrentHashMap (:experiments @loop-state) experiment-id
        {:id experiment-id
         :name name
         :description description
         :variants (or variants [:control :treatment])
         :allocation (or allocation {})
         :hypothesis hypothesis
         :status :running
         :results (ConcurrentHashMap.)
         :created-at (System/currentTimeMillis)}))

(defn get-experiment
  "Get an experiment by ID."
  [experiment-id]
  (.get ^ConcurrentHashMap (:experiments @loop-state) experiment-id))

(defn list-experiments
  "List experiments."
  [& {:keys [status]}]
  (let [experiments (vals (:experiments @loop-state))]
    (if status
      (filter #(= (:status %) status) experiments)
      experiments)))

(defn assign-variant
  "Assign a user to an experiment variant."
  [experiment-id user-id]
  (when-let [experiment (get-experiment experiment-id)]
    (when (= (:status experiment) :running)
      (let [variants (:variants experiment)
            ;; Simple hash-based assignment
            hash-val (Math/abs (hash (str experiment-id user-id)))
            variant-idx (mod hash-val (count variants))
            variant (nth variants variant-idx)]
        variant))))

(defn record-experiment-result!
  "Record a result for an experiment."
  [experiment-id {:keys [user-id variant metric-name metric-value]}]
  (when-let [experiment (get-experiment experiment-id)]
    (let [results ^ConcurrentHashMap (:results experiment)
          key (str variant "-" metric-name)
          current (or (.get results key) {:values [] :count 0 :sum 0})]
      (.put results key
            {:values (conj (:values current) metric-value)
             :count (inc (:count current))
             :sum (+ (:sum current) metric-value)}))))

(defn analyze-experiment
  "Analyze experiment results."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (let [results (:results experiment)
          min-samples (get-in @loop-state [:config :experiment-min-samples])
          variant-stats (into {}
                              (map (fn [variant]
                                     (let [key (str variant "-conversion")
                                           data (.get ^ConcurrentHashMap results key)]
                                       [variant (when data
                                                  {:count (:count data)
                                                   :mean (if (pos? (:count data))
                                                           (/ (:sum data) (:count data))
                                                           0)})]))
                                   (:variants experiment)))
          has-enough-data (every? #(>= (get-in % [1 :count] 0) min-samples)
                                  variant-stats)]
      {:experiment-id experiment-id
       :variant-stats variant-stats
       :has-enough-data has-enough-data
       :winner (when has-enough-data
                 (key (apply max-key #(get-in % [1 :mean] 0) variant-stats)))
       :analyzed-at (System/currentTimeMillis)})))

(defn conclude-experiment!
  "Conclude an experiment."
  [experiment-id {:keys [winner conclusion]}]
  (when-let [experiment (get-experiment experiment-id)]
    (let [updated (assoc experiment
                         :status :concluded
                         :winner winner
                         :conclusion conclusion
                         :concluded-at (System/currentTimeMillis))]
      (.put ^ConcurrentHashMap (:experiments @loop-state) experiment-id updated)
      (log/info "Experiment concluded" {:id experiment-id :winner winner})
      (events/publish! :experiment/concluded {:experiment-id experiment-id :winner winner})
      updated)))

;; =============================================================================
;; PERFORMANCE OPTIMIZATION
;; =============================================================================

(defn register-optimization!
  "Register a performance optimization."
  [optimization-id {:keys [name description target-metric current-value target-value strategy]}]
  (log/info "Registering optimization" {:id optimization-id :name name})
  (.put ^ConcurrentHashMap (:optimizations @loop-state) optimization-id
        {:id optimization-id
         :name name
         :description description
         :target-metric target-metric
         :current-value current-value
         :target-value target-value
         :strategy strategy
         :status :pending
         :progress []
         :created-at (System/currentTimeMillis)}))

(defn get-optimization
  "Get an optimization by ID."
  [optimization-id]
  (.get ^ConcurrentHashMap (:optimizations @loop-state) optimization-id))

(defn update-optimization-progress!
  "Update optimization progress."
  [optimization-id {:keys [metric-value notes]}]
  (when-let [optimization (get-optimization optimization-id)]
    (let [progress-entry {:value metric-value
                          :notes notes
                          :recorded-at (System/currentTimeMillis)}
          updated (update optimization :progress conj progress-entry)]
      ;; Check if target reached
      (when (>= metric-value (:target-value optimization))
        (assoc updated :status :achieved :achieved-at (System/currentTimeMillis)))
      (.put ^ConcurrentHashMap (:optimizations @loop-state) optimization-id updated)
      updated)))

(defn list-optimizations
  "List optimizations."
  [& {:keys [status target-metric]}]
  (let [optimizations (vals (:optimizations @loop-state))]
    (cond->> optimizations
      status (filter #(= (:status %) status))
      target-metric (filter #(= (:target-metric %) target-metric)))))

;; =============================================================================
;; FEEDBACK ANALYSIS
;; =============================================================================

(defn analyze-feedback-patterns
  "Analyze patterns in feedback."
  [prediction-type]
  (let [feedback (list-feedback :prediction-type prediction-type)
        incorrect (filter (complement :is-correct) feedback)]
    (when (seq incorrect)
      {:prediction-type prediction-type
       :total-feedback (count feedback)
       :incorrect-count (count incorrect)
       :error-rate (/ (count incorrect) (count feedback))
       :common-contexts (->> incorrect
                             (map :context)
                             (filter some?)
                             frequencies
                             (sort-by val >)
                             (take 5))
       :confidence-distribution (->> incorrect
                                     (map :confidence)
                                     (filter some?)
                                     frequencies)
       :analyzed-at (System/currentTimeMillis)})))

(defn generate-improvement-suggestions
  "Generate suggestions for improvement based on feedback."
  [prediction-type]
  (let [analysis (analyze-feedback-patterns prediction-type)
        accuracy (calculate-accuracy prediction-type)
        trend (detect-accuracy-trend prediction-type)]
    (cond-> []
      (and accuracy (< (:accuracy accuracy) 0.7))
      (conj {:type :low-accuracy
             :message "Accuracy below 70%. Consider retraining or adjusting thresholds."
             :priority :high})
      
      (and trend (= (:trend trend) :declining))
      (conj {:type :declining-trend
             :message "Accuracy is declining. Investigate recent changes."
             :priority :high})
      
      (and analysis (> (:error-rate analysis) 0.3))
      (conj {:type :high-error-rate
             :message "Error rate above 30%. Review common error contexts."
             :priority :medium})
      
      (seq (:common-contexts analysis))
      (conj {:type :context-pattern
             :message (str "Errors concentrated in specific contexts: "
                           (str/join ", " (map first (:common-contexts analysis))))
             :priority :medium}))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-loop-stats
  "Get feedback loop statistics."
  []
  {:feedback-count (.get ^AtomicLong (:feedback-count @loop-state))
   :feedback-records (.size ^ConcurrentHashMap (:feedback @loop-state))
   :accuracy-records (.size ^ConcurrentHashMap (:accuracy-records @loop-state))
   :calibrations (.size ^ConcurrentHashMap (:calibrations @loop-state))
   :experiments (.size ^ConcurrentHashMap (:experiments @loop-state))
   :optimizations (.size ^ConcurrentHashMap (:optimizations @loop-state))
   :running-experiments (count (list-experiments :status :running))
   :pending-optimizations (count (list-optimizations :status :pending))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-feedback-loop!
  "Initialize feedback loop."
  []
  (log/info "Initializing feedback loop")
  ;; Register feature flag
  (flags/register-flag! "feedback-loop" "Enable feedback loop" true)
  ;; Create metrics
  (metrics/create-counter! :feedbackloop/feedback "Feedback records collected")
  (metrics/create-gauge! :feedbackloop/experiments "Running experiments"
                         #(count (list-experiments :status :running)))
  (log/info "Feedback loop initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-feedback-loop-status []
  {:enabled (flags/is-enabled? "feedback-loop")
   :feedback-records (.size ^ConcurrentHashMap (:feedback @loop-state))
   :accuracy-records (.size ^ConcurrentHashMap (:accuracy-records @loop-state))
   :running-experiments (count (list-experiments :status :running))
   :stats (get-loop-stats)
   :config (:config @loop-state)})
