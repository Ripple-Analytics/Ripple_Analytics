(ns mental-models.pipeline.integration.model-interaction
  "Model Interaction Analyzer
   
   Analyzes interactions between detected mental models:
   - Synergy detection (models that reinforce each other)
   - Conflict detection (models that contradict each other)
   - Cascade analysis (models that trigger other models)
   - Lollapalooza pattern recognition"
  (:require
   [mental-models.registry.models :as registry]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.set :as set]))

;; =============================================================================
;; INTERACTION TYPES
;; =============================================================================

(def interaction-types
  {:synergy {:description "Models reinforce each other"
             :weight 1.2}
   :conflict {:description "Models contradict each other"
              :weight 0.8}
   :cascade {:description "One model triggers another"
             :weight 1.1}
   :neutral {:description "Models are independent"
             :weight 1.0}})

;; =============================================================================
;; KNOWN INTERACTIONS
;; =============================================================================

(def model-interactions
  "Known interactions between mental models."
  {;; Synergies
   [:social-proof :authority] :synergy
   [:commitment-consistency :sunk-cost] :synergy
   [:scarcity :loss-aversion] :synergy
   [:reciprocity :liking] :synergy
   [:anchoring :contrast] :synergy
   [:availability :recency] :synergy
   [:confirmation-bias :belief-perseverance] :synergy
   [:overconfidence :hindsight] :synergy
   [:incentive-caused-bias :self-interest] :synergy
   [:envy-jealousy :social-comparison] :synergy
   
   ;; Conflicts
   [:optimism :pessimism] :conflict
   [:risk-seeking :loss-aversion] :conflict
   [:overconfidence :doubt] :conflict
   [:action-bias :status-quo] :conflict
   
   ;; Cascades
   [:stress :tunnel-vision] :cascade
   [:fear :fight-or-flight] :cascade
   [:curiosity :exploration] :cascade
   [:reward :dopamine-seeking] :cascade
   [:pain-avoidance :denial] :cascade})

;; =============================================================================
;; INTERACTION ANALYSIS
;; =============================================================================

(defn get-interaction-type
  "Get the interaction type between two models."
  [model-a model-b]
  (or (get model-interactions [model-a model-b])
      (get model-interactions [model-b model-a])
      :neutral))

(defn analyze-pair
  "Analyze interaction between a pair of detected models."
  [detection-a detection-b]
  (let [model-a (:model-id detection-a)
        model-b (:model-id detection-b)
        interaction-type (get-interaction-type model-a model-b)
        type-info (get interaction-types interaction-type)]
    {:models [model-a model-b]
     :model-names [(:model-name detection-a) (:model-name detection-b)]
     :interaction-type interaction-type
     :description (:description type-info)
     :weight (:weight type-info)
     :combined-confidence (* (:confidence detection-a)
                             (:confidence detection-b)
                             (:weight type-info))}))

(defn analyze-interactions
  "Analyze all interactions between detected models."
  [detections]
  (when (flags/is-enabled? "interaction-analysis")
    (log/debug "Analyzing model interactions" {:model-count (count detections)})
    (let [pairs (for [i (range (count detections))
                      j (range (inc i) (count detections))]
                  [(nth detections i) (nth detections j)])
          interactions (map #(apply analyze-pair %) pairs)]
      {:total-pairs (count pairs)
       :interactions interactions
       :synergies (filter #(= :synergy (:interaction-type %)) interactions)
       :conflicts (filter #(= :conflict (:interaction-type %)) interactions)
       :cascades (filter #(= :cascade (:interaction-type %)) interactions)})))

;; =============================================================================
;; SYNERGY ANALYSIS
;; =============================================================================

(defn find-synergy-clusters
  "Find clusters of models that synergize together."
  [detections]
  (let [analysis (analyze-interactions detections)
        synergies (:synergies analysis)]
    (when (seq synergies)
      (let [model-graph (reduce (fn [g synergy]
                                  (let [[m1 m2] (:models synergy)]
                                    (-> g
                                        (update m1 (fnil conj #{}) m2)
                                        (update m2 (fnil conj #{}) m1))))
                                {}
                                synergies)]
        {:clusters (vals model-graph)
         :strongest-synergy (apply max-key :combined-confidence synergies)
         :synergy-count (count synergies)}))))

;; =============================================================================
;; CONFLICT ANALYSIS
;; =============================================================================

(defn find-conflicts
  "Find conflicting model detections."
  [detections]
  (let [analysis (analyze-interactions detections)
        conflicts (:conflicts analysis)]
    (when (seq conflicts)
      {:conflicts conflicts
       :conflict-count (count conflicts)
       :most-severe (apply max-key :combined-confidence conflicts)})))

;; =============================================================================
;; CASCADE ANALYSIS
;; =============================================================================

(defn trace-cascade
  "Trace a cascade of model activations."
  [detections starting-model]
  (let [analysis (analyze-interactions detections)
        cascades (:cascades analysis)
        cascade-map (reduce (fn [m c]
                              (let [[trigger triggered] (:models c)]
                                (update m trigger (fnil conj []) triggered)))
                            {}
                            cascades)]
    (loop [current starting-model
           chain [starting-model]
           visited #{starting-model}]
      (let [next-models (get cascade-map current [])
            unvisited (remove visited next-models)]
        (if (empty? unvisited)
          chain
          (recur (first unvisited)
                 (conj chain (first unvisited))
                 (conj visited (first unvisited))))))))

;; =============================================================================
;; LOLLAPALOOZA DETECTION
;; =============================================================================

(defn detect-lollapalooza-pattern
  "Detect Lollapalooza effect from model interactions."
  [detections & {:keys [min-models min-confidence]
                 :or {min-models 3 min-confidence 0.7}}]
  (when (flags/is-enabled? "lollapalooza-detection")
    (let [high-confidence (filter #(>= (:confidence %) min-confidence) detections)
          analysis (analyze-interactions high-confidence)
          synergies (:synergies analysis)]
      (when (and (>= (count high-confidence) min-models)
                 (seq synergies))
        (let [synergy-models (set (mapcat :models synergies))
              participating (filter #(contains? synergy-models (:model-id %)) high-confidence)
              combined-confidence (reduce * 1.0 (map :confidence participating))]
          (when (>= (count participating) min-models)
            (let [result {:detected true
                          :models participating
                          :model-count (count participating)
                          :synergy-count (count synergies)
                          :combined-confidence (Math/pow combined-confidence (/ 1.0 (count participating)))
                          :description (str "Lollapalooza effect: " (count participating)
                                            " synergistic models detected with "
                                            (count synergies) " reinforcing interactions")}]
              ;; Record metrics
              (metrics/inc-counter! :model-interaction/lollapaloozas-detected)
              ;; Publish event
              (events/publish! :lollapalooza/detected result)
              ;; Audit
              (audit/log-operation! {:operation :lollapalooza-detected
                                     :model-count (count participating)
                                     :synergy-count (count synergies)})
              result)))))))

;; =============================================================================
;; COMPREHENSIVE ANALYSIS
;; =============================================================================

(defn analyze-model-interactions
  "Perform comprehensive model interaction analysis."
  [detections]
  (log/info "Performing model interaction analysis" {:model-count (count detections)})
  (let [start-time (System/currentTimeMillis)
        interactions (analyze-interactions detections)
        synergy-clusters (find-synergy-clusters detections)
        conflicts (find-conflicts detections)
        lollapalooza (detect-lollapalooza-pattern detections)
        duration (- (System/currentTimeMillis) start-time)]
    ;; Record metrics
    (metrics/observe-histogram! :model-interaction/analysis-time duration)
    {:interactions interactions
     :synergy-clusters synergy-clusters
     :conflicts conflicts
     :lollapalooza lollapalooza
     :analysis-time-ms duration}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-interaction-analyzer!
  "Initialize model interaction analyzer."
  []
  (log/info "Initializing model interaction analyzer")
  ;; Register feature flags
  (flags/register-flag! "interaction-analysis" "Enable interaction analysis" true)
  (flags/register-flag! "lollapalooza-detection" "Enable Lollapalooza detection" true)
  ;; Create metrics
  (metrics/create-counter! :model-interaction/lollapaloozas-detected "Lollapaloozas detected")
  (metrics/create-histogram! :model-interaction/analysis-time "Analysis time" [10 50 100 500 1000])
  (log/info "Model interaction analyzer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-analyzer-status []
  {:enabled (flags/is-enabled? "interaction-analysis")
   :lollapalooza-enabled (flags/is-enabled? "lollapalooza-detection")
   :known-interactions (count model-interactions)})
