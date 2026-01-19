(ns mental-models.pipeline.integration.bias-detector
  "Bias Detector Module
   
   Cognitive bias detection:
   - Multi-bias detection
   - Bias scoring and confidence
   - Context-aware analysis
   - Debiasing suggestions
   - Bias pattern learning"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.util.regex Pattern]))

;; =============================================================================
;; BIAS DETECTOR STATE
;; =============================================================================

(defonce detector-state (atom {:biases (ConcurrentHashMap.)
                               :indicators (ConcurrentHashMap.)
                               :detections (ConcurrentHashMap.)
                               :debiasing-strategies (ConcurrentHashMap.)
                               :detection-count (AtomicLong. 0)
                               :config {:min-confidence 0.3
                                        :max-biases-per-text 10
                                        :context-window 100}}))

;; =============================================================================
;; BIAS DEFINITIONS
;; =============================================================================

(defn register-bias!
  "Register a cognitive bias."
  [bias-id {:keys [name description category severity indicators debiasing-tips]}]
  (log/info "Registering bias" {:id bias-id :name name})
  (.put ^ConcurrentHashMap (:biases @detector-state) bias-id
        {:id bias-id
         :name name
         :description description
         :category (or category :cognitive)
         :severity (or severity :medium)
         :indicators (or indicators [])
         :debiasing-tips (or debiasing-tips [])
         :detection-count (AtomicLong. 0)
         :enabled true
         :created-at (System/currentTimeMillis)}))

(defn get-bias
  "Get a bias by ID."
  [bias-id]
  (.get ^ConcurrentHashMap (:biases @detector-state) bias-id))

(defn list-biases
  "List all biases."
  [& {:keys [category severity enabled-only]}]
  (let [biases (vals (:biases @detector-state))]
    (cond->> biases
      enabled-only (filter :enabled)
      category (filter #(= (:category %) category))
      severity (filter #(= (:severity %) severity)))))

(defn enable-bias!
  "Enable a bias for detection."
  [bias-id]
  (when-let [bias (get-bias bias-id)]
    (.put ^ConcurrentHashMap (:biases @detector-state) bias-id
          (assoc bias :enabled true))))

(defn disable-bias!
  "Disable a bias for detection."
  [bias-id]
  (when-let [bias (get-bias bias-id)]
    (.put ^ConcurrentHashMap (:biases @detector-state) bias-id
          (assoc bias :enabled false))))

;; =============================================================================
;; INDICATORS
;; =============================================================================

(defn register-indicator!
  "Register a bias indicator."
  [indicator-id {:keys [bias-id pattern keywords weight context-required]}]
  (log/debug "Registering indicator" {:id indicator-id :bias bias-id})
  (let [compiled (when pattern
                   (try
                     (Pattern/compile pattern Pattern/CASE_INSENSITIVE)
                     (catch Exception _ nil)))]
    (.put ^ConcurrentHashMap (:indicators @detector-state) indicator-id
          {:id indicator-id
           :bias-id bias-id
           :pattern pattern
           :compiled compiled
           :keywords (or keywords [])
           :weight (or weight 1.0)
           :context-required context-required
           :match-count (AtomicLong. 0)})))

(defn get-indicator
  "Get an indicator by ID."
  [indicator-id]
  (.get ^ConcurrentHashMap (:indicators @detector-state) indicator-id))

(defn get-bias-indicators
  "Get all indicators for a bias."
  [bias-id]
  (filter #(= (:bias-id %) bias-id)
          (vals (:indicators @detector-state))))

;; =============================================================================
;; DETECTION LOGIC
;; =============================================================================

(defn match-pattern
  "Match a pattern against text."
  [indicator text]
  (when-let [compiled (:compiled indicator)]
    (let [matcher (.matcher ^Pattern compiled text)]
      (loop [matches []]
        (if (.find matcher)
          (recur (conj matches {:match (.group matcher)
                                :start (.start matcher)
                                :end (.end matcher)}))
          (when (seq matches)
            (.incrementAndGet ^AtomicLong (:match-count indicator))
            matches))))))

(defn match-keywords
  "Match keywords against text."
  [indicator text]
  (let [text-lower (str/lower-case text)
        keywords (:keywords indicator)]
    (keep (fn [kw]
            (let [kw-lower (str/lower-case kw)
                  idx (str/index-of text-lower kw-lower)]
              (when idx
                {:match kw
                 :start idx
                 :end (+ idx (count kw))})))
          keywords)))

(defn extract-context
  "Extract context around a match."
  [text match]
  (let [window (get-in @detector-state [:config :context-window])
        start (max 0 (- (:start match) window))
        end (min (count text) (+ (:end match) window))]
    (subs text start end)))

(defn calculate-indicator-score
  "Calculate score for an indicator match."
  [indicator matches text]
  (let [match-count (count matches)
        text-length (count text)
        coverage (/ (reduce + (map #(- (:end %) (:start %)) matches)) text-length)
        weight (:weight indicator 1.0)]
    (* weight (min 1.0 (+ (* 0.4 (Math/log (inc match-count)))
                          (* 0.6 coverage))))))

(defn detect-indicator
  "Detect a single indicator in text."
  [indicator text]
  (let [pattern-matches (match-pattern indicator text)
        keyword-matches (match-keywords indicator text)
        all-matches (concat (or pattern-matches []) (or keyword-matches []))]
    (when (seq all-matches)
      {:indicator-id (:id indicator)
       :bias-id (:bias-id indicator)
       :matches all-matches
       :match-count (count all-matches)
       :score (calculate-indicator-score indicator all-matches text)
       :contexts (map #(extract-context text %) (take 3 all-matches))})))

(defn detect-bias
  "Detect a single bias in text."
  [bias text]
  (when (:enabled bias)
    (let [indicators (get-bias-indicators (:id bias))
          indicator-results (keep #(detect-indicator % text) indicators)
          total-score (if (seq indicator-results)
                        (/ (reduce + (map :score indicator-results))
                           (count indicators))
                        0)]
      (when (and (seq indicator-results)
                 (>= total-score (get-in @detector-state [:config :min-confidence])))
        (.incrementAndGet ^AtomicLong (:detection-count bias))
        {:bias-id (:id bias)
         :bias-name (:name bias)
         :category (:category bias)
         :severity (:severity bias)
         :confidence total-score
         :indicators indicator-results
         :debiasing-tips (:debiasing-tips bias)}))))

;; =============================================================================
;; MULTI-BIAS DETECTION
;; =============================================================================

(defn detect-all-biases
  "Detect all biases in text."
  [text & {:keys [biases min-confidence]}]
  (.incrementAndGet ^AtomicLong (:detection-count @detector-state))
  (metrics/inc-counter! :biasdetector/detections)
  (let [min-conf (or min-confidence (get-in @detector-state [:config :min-confidence]))
        max-biases (get-in @detector-state [:config :max-biases-per-text])
        biases-to-check (or biases (list-biases :enabled-only true))
        detections (keep #(detect-bias % text) biases-to-check)
        filtered (filter #(>= (:confidence %) min-conf) detections)
        sorted (sort-by :confidence > filtered)]
    (take max-biases sorted)))

(defn detect-bias-category
  "Detect biases in a specific category."
  [category text]
  (let [biases (list-biases :category category :enabled-only true)]
    (detect-all-biases text :biases biases)))

;; =============================================================================
;; DEBIASING STRATEGIES
;; =============================================================================

(defn register-debiasing-strategy!
  "Register a debiasing strategy."
  [strategy-id {:keys [name description bias-ids steps effectiveness]}]
  (log/info "Registering debiasing strategy" {:id strategy-id :name name})
  (.put ^ConcurrentHashMap (:debiasing-strategies @detector-state) strategy-id
        {:id strategy-id
         :name name
         :description description
         :bias-ids (or bias-ids [])
         :steps (or steps [])
         :effectiveness (or effectiveness 0.7)
         :usage-count (AtomicLong. 0)}))

(defn get-debiasing-strategies
  "Get debiasing strategies for a bias."
  [bias-id]
  (filter #(some #{bias-id} (:bias-ids %))
          (vals (:debiasing-strategies @detector-state))))

(defn suggest-debiasing
  "Suggest debiasing strategies for detected biases."
  [detections]
  (let [bias-ids (set (map :bias-id detections))
        strategies (mapcat get-debiasing-strategies bias-ids)
        unique-strategies (distinct strategies)]
    (sort-by :effectiveness > unique-strategies)))

;; =============================================================================
;; BATCH DETECTION
;; =============================================================================

(defn detect-batch
  "Detect biases in multiple texts."
  [texts & {:keys [parallel?] :or {parallel? true}}]
  (let [detect-fn (fn [text]
                    {:text-preview (subs text 0 (min 100 (count text)))
                     :detections (detect-all-biases text)})]
    (if parallel?
      (pmap detect-fn texts)
      (map detect-fn texts))))

;; =============================================================================
;; DETECTION STORAGE
;; =============================================================================

(defn store-detection!
  "Store a detection result."
  [detection-id {:keys [text detections source context]}]
  (.put ^ConcurrentHashMap (:detections @detector-state) detection-id
        {:id detection-id
         :text-preview (subs text 0 (min 200 (count text)))
         :detections detections
         :debiasing-suggestions (suggest-debiasing detections)
         :source source
         :context context
         :timestamp (System/currentTimeMillis)}))

(defn get-detection
  "Get a stored detection."
  [detection-id]
  (.get ^ConcurrentHashMap (:detections @detector-state) detection-id))

(defn list-detections
  "List stored detections."
  [& {:keys [bias-id limit since]}]
  (let [detections (vals (:detections @detector-state))]
    (cond->> detections
      bias-id (filter #(some (fn [d] (= (:bias-id d) bias-id)) (:detections %)))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-bias-stats
  "Get statistics for a bias."
  [bias-id]
  (when-let [bias (get-bias bias-id)]
    {:id bias-id
     :name (:name bias)
     :detection-count (.get ^AtomicLong (:detection-count bias))
     :indicators (count (get-bias-indicators bias-id))}))

(defn get-detector-stats
  "Get overall detector statistics."
  []
  {:biases (.size ^ConcurrentHashMap (:biases @detector-state))
   :indicators (.size ^ConcurrentHashMap (:indicators @detector-state))
   :detections (.size ^ConcurrentHashMap (:detections @detector-state))
   :strategies (.size ^ConcurrentHashMap (:debiasing-strategies @detector-state))
   :detection-count (.get ^AtomicLong (:detection-count @detector-state))
   :top-biases (->> (list-biases)
                    (map (fn [b] {:id (:id b)
                                  :name (:name b)
                                  :detections (.get ^AtomicLong (:detection-count b))}))
                    (sort-by :detections >)
                    (take 10))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-cognitive-biases!
  "Initialize cognitive biases from Munger's list."
  []
  (log/info "Initializing cognitive biases")
  ;; Confirmation Bias
  (register-bias! "confirmation-bias"
                  {:name "Confirmation Bias"
                   :description "Tendency to search for information that confirms existing beliefs"
                   :category :cognitive
                   :severity :high
                   :debiasing-tips ["Actively seek disconfirming evidence"
                                    "Consider the opposite viewpoint"
                                    "Ask: What would change my mind?"]})
  (register-indicator! "confirmation-1"
                       {:bias-id "confirmation-bias"
                        :pattern "(?:confirms?|supports?|proves?)\\s+(?:my|our|the)\\s+(?:belief|view|theory)"
                        :keywords ["confirms my belief" "proves my point" "as I expected"]
                        :weight 1.0})
  ;; Availability Heuristic
  (register-bias! "availability-heuristic"
                  {:name "Availability Heuristic"
                   :description "Overweighting easily recalled information"
                   :category :cognitive
                   :severity :medium
                   :debiasing-tips ["Look for base rate statistics"
                                    "Consider what you might be missing"
                                    "Seek systematic data"]})
  (register-indicator! "availability-1"
                       {:bias-id "availability-heuristic"
                        :pattern "(?:recently|just|lately)\\s+(?:saw|heard|read)"
                        :keywords ["I remember when" "just the other day" "everyone knows"]
                        :weight 0.9})
  ;; Anchoring
  (register-bias! "anchoring"
                  {:name "Anchoring Bias"
                   :description "Over-reliance on first piece of information"
                   :category :cognitive
                   :severity :medium
                   :debiasing-tips ["Generate your own estimate first"
                                    "Consider multiple anchors"
                                    "Question the source of initial numbers"]})
  (register-indicator! "anchoring-1"
                       {:bias-id "anchoring"
                        :pattern "(?:starting|initial|first|original)\\s+(?:price|value|offer)"
                        :keywords ["starting at" "originally" "first impression"]
                        :weight 0.8})
  ;; Loss Aversion
  (register-bias! "loss-aversion"
                  {:name "Loss Aversion"
                   :description "Losses feel worse than equivalent gains feel good"
                   :category :emotional
                   :severity :high
                   :debiasing-tips ["Reframe as opportunity cost"
                                    "Consider the decision in isolation"
                                    "Think about long-term outcomes"]})
  (register-indicator! "loss-aversion-1"
                       {:bias-id "loss-aversion"
                        :pattern "(?:can't|cannot|won't)\\s+(?:afford|risk|lose)"
                        :keywords ["afraid of losing" "can't risk" "too much to lose"]
                        :weight 1.0})
  ;; Sunk Cost Fallacy
  (register-bias! "sunk-cost"
                  {:name "Sunk Cost Fallacy"
                   :description "Continuing due to past investment rather than future value"
                   :category :cognitive
                   :severity :high
                   :debiasing-tips ["Focus only on future costs and benefits"
                                    "Ask: Would I start this today?"
                                    "Ignore past investments in decisions"]})
  (register-indicator! "sunk-cost-1"
                       {:bias-id "sunk-cost"
                        :pattern "(?:already|have)\\s+(?:invested|spent|put in)"
                        :keywords ["already invested" "come this far" "too late to stop"]
                        :weight 1.0})
  ;; Hindsight Bias
  (register-bias! "hindsight-bias"
                  {:name "Hindsight Bias"
                   :description "Believing past events were predictable"
                   :category :cognitive
                   :severity :medium
                   :debiasing-tips ["Document predictions before outcomes"
                                    "Consider alternative outcomes"
                                    "Review your actual uncertainty"]})
  (register-indicator! "hindsight-1"
                       {:bias-id "hindsight-bias"
                        :pattern "(?:knew|obvious|clearly)\\s+(?:it|this|that)\\s+(?:would|was going)"
                        :keywords ["I knew it" "obviously" "should have seen"]
                        :weight 0.9})
  (log/info "Cognitive biases initialized" {:count 6}))

(defn init-debiasing-strategies!
  "Initialize debiasing strategies."
  []
  (log/info "Initializing debiasing strategies")
  (register-debiasing-strategy! "consider-opposite"
                                {:name "Consider the Opposite"
                                 :description "Actively argue against your position"
                                 :bias-ids ["confirmation-bias" "anchoring"]
                                 :steps ["List reasons your belief might be wrong"
                                         "Seek out opposing viewpoints"
                                         "Steelman the counter-argument"]
                                 :effectiveness 0.8})
  (register-debiasing-strategy! "base-rate-check"
                                {:name "Base Rate Check"
                                 :description "Compare to statistical base rates"
                                 :bias-ids ["availability-heuristic" "hindsight-bias"]
                                 :steps ["Find relevant statistics"
                                         "Compare your estimate to base rates"
                                         "Adjust based on specific factors"]
                                 :effectiveness 0.75})
  (register-debiasing-strategy! "zero-based-thinking"
                                {:name "Zero-Based Thinking"
                                 :description "Decide as if starting fresh"
                                 :bias-ids ["sunk-cost" "loss-aversion"]
                                 :steps ["Ignore past investments"
                                         "Ask: Would I start this today?"
                                         "Focus only on future value"]
                                 :effectiveness 0.85})
  (log/info "Debiasing strategies initialized" {:count 3}))

(defn init-bias-detector!
  "Initialize bias detector."
  []
  (log/info "Initializing bias detector")
  ;; Register feature flag
  (flags/register-flag! "bias-detector" "Enable bias detector" true)
  ;; Create metrics
  (metrics/create-counter! :biasdetector/detections "Bias detections performed")
  (metrics/create-gauge! :biasdetector/biases "Total biases"
                         #(.size ^ConcurrentHashMap (:biases @detector-state)))
  ;; Initialize biases and strategies
  (init-cognitive-biases!)
  (init-debiasing-strategies!)
  (log/info "Bias detector initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-bias-detector-status []
  {:enabled (flags/is-enabled? "bias-detector")
   :biases (.size ^ConcurrentHashMap (:biases @detector-state))
   :indicators (.size ^ConcurrentHashMap (:indicators @detector-state))
   :detections (.size ^ConcurrentHashMap (:detections @detector-state))
   :stats (get-detector-stats)
   :config (:config @detector-state)})
