(ns mental-models.pipeline.integration.pattern-detector
  "Pattern Detector Module
   
   Mental model pattern detection:
   - Regex-based pattern matching
   - Keyword extraction
   - N-gram analysis
   - Pattern scoring
   - Multi-pattern detection"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.util.regex Pattern]))

;; =============================================================================
;; PATTERN DETECTOR STATE
;; =============================================================================

(defonce detector-state (atom {:patterns (ConcurrentHashMap.)
                               :pattern-groups (ConcurrentHashMap.)
                               :detections (ConcurrentHashMap.)
                               :detection-count (AtomicLong. 0)
                               :config {:min-confidence 0.3
                                        :max-patterns-per-text 50
                                        :ngram-sizes [1 2 3]
                                        :case-sensitive false}}))

;; =============================================================================
;; PATTERN MANAGEMENT
;; =============================================================================

(defn compile-pattern
  "Compile a regex pattern with options."
  [pattern-str & {:keys [case-sensitive] :or {case-sensitive false}}]
  (try
    (if case-sensitive
      (Pattern/compile pattern-str)
      (Pattern/compile pattern-str Pattern/CASE_INSENSITIVE))
    (catch Exception e
      (log/warn "Failed to compile pattern" {:pattern pattern-str :error (.getMessage e)})
      nil)))

(defn register-pattern!
  "Register a detection pattern."
  [pattern-id {:keys [name regex keywords weight category mental-model]}]
  (let [compiled (when regex (compile-pattern regex))]
    (log/info "Registering pattern" {:id pattern-id :name name})
    (.put ^ConcurrentHashMap (:patterns @detector-state) pattern-id
          {:id pattern-id
           :name name
           :regex regex
           :compiled compiled
           :keywords (or keywords [])
           :weight (or weight 1.0)
           :category category
           :mental-model mental-model
           :enabled true
           :match-count (AtomicLong. 0)
           :created-at (System/currentTimeMillis)})))

(defn unregister-pattern!
  "Unregister a pattern."
  [pattern-id]
  (.remove ^ConcurrentHashMap (:patterns @detector-state) pattern-id))

(defn get-pattern
  "Get a pattern by ID."
  [pattern-id]
  (.get ^ConcurrentHashMap (:patterns @detector-state) pattern-id))

(defn list-patterns
  "List all patterns."
  [& {:keys [category mental-model enabled-only]}]
  (let [patterns (vals (:patterns @detector-state))]
    (cond->> patterns
      enabled-only (filter :enabled)
      category (filter #(= (:category %) category))
      mental-model (filter #(= (:mental-model %) mental-model)))))

(defn enable-pattern!
  "Enable a pattern."
  [pattern-id]
  (when-let [pattern (get-pattern pattern-id)]
    (.put ^ConcurrentHashMap (:patterns @detector-state) pattern-id
          (assoc pattern :enabled true))))

(defn disable-pattern!
  "Disable a pattern."
  [pattern-id]
  (when-let [pattern (get-pattern pattern-id)]
    (.put ^ConcurrentHashMap (:patterns @detector-state) pattern-id
          (assoc pattern :enabled false))))

;; =============================================================================
;; PATTERN GROUPS
;; =============================================================================

(defn create-pattern-group!
  "Create a group of related patterns."
  [group-id {:keys [name description patterns combine-mode]}]
  (log/info "Creating pattern group" {:id group-id :name name})
  (.put ^ConcurrentHashMap (:pattern-groups @detector-state) group-id
        {:id group-id
         :name name
         :description description
         :patterns (or patterns [])
         :combine-mode (or combine-mode :any)
         :created-at (System/currentTimeMillis)}))

(defn add-pattern-to-group!
  "Add a pattern to a group."
  [group-id pattern-id]
  (when-let [group (.get ^ConcurrentHashMap (:pattern-groups @detector-state) group-id)]
    (.put ^ConcurrentHashMap (:pattern-groups @detector-state) group-id
          (update group :patterns conj pattern-id))))

(defn get-pattern-group
  "Get a pattern group."
  [group-id]
  (.get ^ConcurrentHashMap (:pattern-groups @detector-state) group-id))

;; =============================================================================
;; TEXT PROCESSING
;; =============================================================================

(defn tokenize
  "Tokenize text into words."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" " ")
        (str/split #"\s+")
        (->> (filter #(> (count %) 1))))))

(defn generate-ngrams
  "Generate n-grams from tokens."
  [tokens n]
  (when (>= (count tokens) n)
    (map #(str/join " " %) (partition n 1 tokens))))

(defn extract-all-ngrams
  "Extract all n-grams for configured sizes."
  [text]
  (let [tokens (tokenize text)
        sizes (get-in @detector-state [:config :ngram-sizes])]
    (mapcat #(generate-ngrams tokens %) sizes)))

;; =============================================================================
;; PATTERN MATCHING
;; =============================================================================

(defn match-regex
  "Match a regex pattern against text."
  [pattern text]
  (when-let [compiled (:compiled pattern)]
    (let [matcher (.matcher ^Pattern compiled text)
          matches (loop [results []]
                    (if (.find matcher)
                      (recur (conj results {:match (.group matcher)
                                            :start (.start matcher)
                                            :end (.end matcher)}))
                      results))]
      (when (seq matches)
        (.incrementAndGet ^AtomicLong (:match-count pattern))
        matches))))

(defn match-keywords
  "Match keywords against text."
  [pattern text]
  (let [text-lower (str/lower-case text)
        keywords (:keywords pattern)
        matches (filter #(str/includes? text-lower (str/lower-case %)) keywords)]
    (when (seq matches)
      (map (fn [kw]
             (let [idx (str/index-of text-lower (str/lower-case kw))]
               {:match kw
                :start idx
                :end (+ idx (count kw))}))
           matches))))

(defn calculate-pattern-score
  "Calculate confidence score for a pattern match."
  [pattern matches text]
  (let [match-count (count matches)
        text-length (count text)
        coverage (/ (reduce + (map #(- (:end %) (:start %)) matches)) text-length)
        weight (:weight pattern 1.0)]
    (* weight (min 1.0 (+ (* 0.3 (Math/log (inc match-count)))
                          (* 0.7 coverage))))))

(defn detect-pattern
  "Detect a single pattern in text."
  [pattern text]
  (when (:enabled pattern)
    (let [regex-matches (match-regex pattern text)
          keyword-matches (match-keywords pattern text)
          all-matches (concat (or regex-matches []) (or keyword-matches []))]
      (when (seq all-matches)
        {:pattern-id (:id pattern)
         :pattern-name (:name pattern)
         :mental-model (:mental-model pattern)
         :category (:category pattern)
         :matches all-matches
         :match-count (count all-matches)
         :score (calculate-pattern-score pattern all-matches text)}))))

;; =============================================================================
;; MULTI-PATTERN DETECTION
;; =============================================================================

(defn detect-all-patterns
  "Detect all patterns in text."
  [text & {:keys [patterns min-confidence]}]
  (.incrementAndGet ^AtomicLong (:detection-count @detector-state))
  (metrics/inc-counter! :patterndetector/detections)
  (let [min-conf (or min-confidence (get-in @detector-state [:config :min-confidence]))
        max-patterns (get-in @detector-state [:config :max-patterns-per-text])
        patterns-to-check (or patterns (list-patterns :enabled-only true))
        detections (keep #(detect-pattern % text) patterns-to-check)
        filtered (filter #(>= (:score %) min-conf) detections)
        sorted (sort-by :score > filtered)]
    (take max-patterns sorted)))

(defn detect-pattern-group
  "Detect patterns from a group."
  [group-id text]
  (when-let [group (get-pattern-group group-id)]
    (let [patterns (keep get-pattern (:patterns group))
          detections (detect-all-patterns text :patterns patterns)
          combine-mode (:combine-mode group)]
      (case combine-mode
        :any (when (seq detections) detections)
        :all (when (= (count detections) (count patterns)) detections)
        :majority (when (>= (count detections) (/ (count patterns) 2)) detections)
        detections))))

(defn detect-mental-model-patterns
  "Detect patterns associated with a specific mental model."
  [mental-model text]
  (let [patterns (list-patterns :mental-model mental-model :enabled-only true)]
    (detect-all-patterns text :patterns patterns)))

;; =============================================================================
;; BATCH DETECTION
;; =============================================================================

(defn detect-batch
  "Detect patterns in multiple texts."
  [texts & {:keys [parallel?] :or {parallel? true}}]
  (let [detect-fn (fn [text]
                    {:text (subs text 0 (min 100 (count text)))
                     :detections (detect-all-patterns text)})]
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
         :source source
         :context context
         :timestamp (System/currentTimeMillis)}))

(defn get-detection
  "Get a stored detection."
  [detection-id]
  (.get ^ConcurrentHashMap (:detections @detector-state) detection-id))

(defn list-detections
  "List stored detections."
  [& {:keys [limit since]}]
  (let [detections (vals (:detections @detector-state))]
    (cond->> detections
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-pattern-stats
  "Get statistics for a pattern."
  [pattern-id]
  (when-let [pattern (get-pattern pattern-id)]
    {:id pattern-id
     :name (:name pattern)
     :match-count (.get ^AtomicLong (:match-count pattern))
     :enabled (:enabled pattern)}))

(defn get-detector-stats
  "Get overall detector statistics."
  []
  {:patterns (.size ^ConcurrentHashMap (:patterns @detector-state))
   :pattern-groups (.size ^ConcurrentHashMap (:pattern-groups @detector-state))
   :detections (.size ^ConcurrentHashMap (:detections @detector-state))
   :detection-count (.get ^AtomicLong (:detection-count @detector-state))
   :top-patterns (->> (list-patterns)
                      (map (fn [p] {:id (:id p)
                                    :name (:name p)
                                    :matches (.get ^AtomicLong (:match-count p))}))
                      (sort-by :matches >)
                      (take 10))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-mental-model-patterns!
  "Initialize patterns for mental model detection."
  []
  (log/info "Initializing mental model patterns")
  ;; Confirmation Bias patterns
  (register-pattern! "confirmation-bias-1"
                     {:name "Confirmation Bias - Seeking Agreement"
                      :regex "(?:confirms?|supports?|validates?|proves?)\\s+(?:my|our|the)\\s+(?:belief|view|opinion|theory)"
                      :keywords ["confirms my belief" "supports my view" "validates my opinion"]
                      :weight 1.0
                      :category "cognitive-bias"
                      :mental-model "confirmation-bias"})
  (register-pattern! "confirmation-bias-2"
                     {:name "Confirmation Bias - Ignoring Counter-evidence"
                      :regex "(?:ignore|dismiss|overlook)\\s+(?:evidence|data|facts?)\\s+(?:that|which)"
                      :keywords ["ignore evidence" "dismiss data" "overlook facts"]
                      :weight 0.9
                      :category "cognitive-bias"
                      :mental-model "confirmation-bias"})
  ;; Availability Heuristic patterns
  (register-pattern! "availability-1"
                     {:name "Availability Heuristic - Recent Events"
                      :regex "(?:recently|just|lately)\\s+(?:saw|heard|read|experienced)"
                      :keywords ["recently saw" "just heard" "lately experienced"]
                      :weight 0.8
                      :category "cognitive-bias"
                      :mental-model "availability-heuristic"})
  ;; Anchoring patterns
  (register-pattern! "anchoring-1"
                     {:name "Anchoring - Initial Reference"
                      :regex "(?:starting|initial|first|original)\\s+(?:price|value|estimate|number)"
                      :keywords ["starting price" "initial value" "first estimate"]
                      :weight 0.9
                      :category "cognitive-bias"
                      :mental-model "anchoring"})
  ;; Loss Aversion patterns
  (register-pattern! "loss-aversion-1"
                     {:name "Loss Aversion - Fear of Loss"
                      :regex "(?:afraid|fear|worried)\\s+(?:of|about)\\s+(?:losing|loss)"
                      :keywords ["afraid of losing" "fear of loss" "worried about losing"]
                      :weight 1.0
                      :category "cognitive-bias"
                      :mental-model "loss-aversion"})
  ;; Sunk Cost patterns
  (register-pattern! "sunk-cost-1"
                     {:name "Sunk Cost - Already Invested"
                      :regex "(?:already|have)\\s+(?:invested|spent|put in)\\s+(?:so much|too much)"
                      :keywords ["already invested" "already spent" "put in so much"]
                      :weight 1.0
                      :category "cognitive-bias"
                      :mental-model "sunk-cost"})
  ;; Second-Order Thinking patterns
  (register-pattern! "second-order-1"
                     {:name "Second-Order Thinking"
                      :regex "(?:and then|what happens|consequence|ripple effect|downstream)"
                      :keywords ["and then what" "what happens next" "downstream effects" "ripple effect"]
                      :weight 0.8
                      :category "framework"
                      :mental-model "second-order-thinking"})
  ;; Inversion patterns
  (register-pattern! "inversion-1"
                     {:name "Inversion - Avoiding Failure"
                      :regex "(?:avoid|prevent|what not to|how to fail)"
                      :keywords ["avoid failure" "prevent mistakes" "what not to do"]
                      :weight 0.8
                      :category "framework"
                      :mental-model "inversion"})
  ;; Circle of Competence patterns
  (register-pattern! "circle-competence-1"
                     {:name "Circle of Competence"
                      :regex "(?:within|outside)\\s+(?:my|our)\\s+(?:expertise|competence|knowledge)"
                      :keywords ["within my expertise" "outside my competence" "know what I don't know"]
                      :weight 0.9
                      :category "framework"
                      :mental-model "circle-of-competence"})
  ;; Incentives patterns
  (register-pattern! "incentives-1"
                     {:name "Incentives"
                      :regex "(?:incentive|motivation|reward|punish)"
                      :keywords ["show me the incentive" "follow the money" "what's their motivation"]
                      :weight 0.9
                      :category "framework"
                      :mental-model "incentives"})
  (log/info "Mental model patterns initialized" {:count 10}))

(defn init-pattern-detector!
  "Initialize pattern detector."
  []
  (log/info "Initializing pattern detector")
  ;; Register feature flag
  (flags/register-flag! "pattern-detector" "Enable pattern detector" true)
  ;; Create metrics
  (metrics/create-counter! :patterndetector/detections "Pattern detections performed")
  (metrics/create-gauge! :patterndetector/patterns "Total patterns"
                         #(.size ^ConcurrentHashMap (:patterns @detector-state)))
  ;; Initialize mental model patterns
  (init-mental-model-patterns!)
  (log/info "Pattern detector initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-pattern-detector-status []
  {:enabled (flags/is-enabled? "pattern-detector")
   :patterns (.size ^ConcurrentHashMap (:patterns @detector-state))
   :pattern-groups (.size ^ConcurrentHashMap (:pattern-groups @detector-state))
   :detections (.size ^ConcurrentHashMap (:detections @detector-state))
   :stats (get-detector-stats)
   :config (:config @detector-state)})
