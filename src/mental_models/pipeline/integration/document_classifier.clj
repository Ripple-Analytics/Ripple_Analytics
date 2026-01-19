(ns mental-models.pipeline.integration.document-classifier
  "Document Classifier Module
   
   Document classification and categorization:
   - Multi-label classification
   - Hierarchical categories
   - Confidence scoring
   - Training and model updates
   - Classification rules"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; DOCUMENT CLASSIFIER STATE
;; =============================================================================

(defonce classifier-state (atom {:categories (ConcurrentHashMap.)
                                  :rules (ConcurrentHashMap.)
                                  :models (ConcurrentHashMap.)
                                  :classifications (ConcurrentHashMap.)
                                  :training-data (ConcurrentHashMap.)
                                  :classification-count (AtomicLong. 0)
                                  :config {:min-confidence 0.5
                                           :max-labels 5
                                           :use-hierarchy true}}))

;; =============================================================================
;; CATEGORY MANAGEMENT
;; =============================================================================

(defn create-category!
  "Create a classification category."
  [category-id {:keys [name description parent keywords patterns]}]
  (log/info "Creating category" {:id category-id})
  (.put ^ConcurrentHashMap (:categories @classifier-state) category-id
        {:id category-id
         :name name
         :description description
         :parent parent
         :keywords (or keywords [])
         :patterns (or patterns [])
         :created-at (System/currentTimeMillis)}))

(defn delete-category!
  "Delete a category."
  [category-id]
  (.remove ^ConcurrentHashMap (:categories @classifier-state) category-id))

(defn get-category
  "Get a category by ID."
  [category-id]
  (.get ^ConcurrentHashMap (:categories @classifier-state) category-id))

(defn list-categories
  "List all categories."
  [& {:keys [parent]}]
  (let [categories (vals (:categories @classifier-state))]
    (if parent
      (filter #(= (:parent %) parent) categories)
      categories)))

(defn get-category-hierarchy
  "Get the full hierarchy for a category."
  [category-id]
  (loop [current category-id
         hierarchy []]
    (if-let [category (get-category current)]
      (if (:parent category)
        (recur (:parent category) (conj hierarchy category))
        (conj hierarchy category))
      hierarchy)))

(defn get-subcategories
  "Get all subcategories of a category."
  [category-id]
  (filter #(= (:parent %) category-id) (vals (:categories @classifier-state))))

;; =============================================================================
;; CLASSIFICATION RULES
;; =============================================================================

(defn create-rule!
  "Create a classification rule."
  [rule-id {:keys [name category-id condition priority]}]
  (log/info "Creating classification rule" {:id rule-id})
  (.put ^ConcurrentHashMap (:rules @classifier-state) rule-id
        {:id rule-id
         :name name
         :category-id category-id
         :condition condition
         :priority (or priority 0)
         :enabled true
         :created-at (System/currentTimeMillis)}))

(defn delete-rule!
  "Delete a rule."
  [rule-id]
  (.remove ^ConcurrentHashMap (:rules @classifier-state) rule-id))

(defn get-rule
  "Get a rule by ID."
  [rule-id]
  (.get ^ConcurrentHashMap (:rules @classifier-state) rule-id))

(defn list-rules
  "List all rules."
  [& {:keys [category-id enabled]}]
  (let [rules (vals (:rules @classifier-state))]
    (cond->> rules
      category-id (filter #(= (:category-id %) category-id))
      (some? enabled) (filter #(= (:enabled %) enabled)))))

(defn enable-rule!
  "Enable a rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (.put ^ConcurrentHashMap (:rules @classifier-state) rule-id
          (assoc rule :enabled true))))

(defn disable-rule!
  "Disable a rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (.put ^ConcurrentHashMap (:rules @classifier-state) rule-id
          (assoc rule :enabled false))))

;; =============================================================================
;; KEYWORD-BASED CLASSIFICATION
;; =============================================================================

(defn calculate-keyword-score
  "Calculate classification score based on keywords."
  [text category]
  (let [text-lower (str/lower-case text)
        keywords (:keywords category [])
        matches (filter #(str/includes? text-lower (str/lower-case %)) keywords)]
    (if (empty? keywords)
      0.0
      (/ (count matches) (count keywords)))))

(defn calculate-pattern-score
  "Calculate classification score based on patterns."
  [text category]
  (let [patterns (:patterns category [])
        matches (filter #(re-find (re-pattern %) text) patterns)]
    (if (empty? patterns)
      0.0
      (/ (count matches) (count patterns)))))

(defn calculate-category-score
  "Calculate overall score for a category."
  [text category]
  (let [keyword-score (calculate-keyword-score text category)
        pattern-score (calculate-pattern-score text category)
        weights {:keyword 0.6 :pattern 0.4}]
    (+ (* (:keyword weights) keyword-score)
       (* (:pattern weights) pattern-score))))

;; =============================================================================
;; RULE-BASED CLASSIFICATION
;; =============================================================================

(defn evaluate-rule
  "Evaluate a classification rule."
  [rule document]
  (try
    (when ((:condition rule) document)
      {:category-id (:category-id rule)
       :rule-id (:id rule)
       :confidence 1.0})
    (catch Exception _
      nil)))

(defn apply-rules
  "Apply all rules to a document."
  [document]
  (let [rules (sort-by :priority > (filter :enabled (vals (:rules @classifier-state))))]
    (keep #(evaluate-rule % document) rules)))

;; =============================================================================
;; CLASSIFICATION
;; =============================================================================

(defn classify-document
  "Classify a document."
  [document & {:keys [text-field max-labels min-confidence]}]
  (.incrementAndGet ^AtomicLong (:classification-count @classifier-state))
  (metrics/inc-counter! :documentclassifier/classifications)
  (let [text (if text-field (get document text-field) (str document))
        max-labels (or max-labels (get-in @classifier-state [:config :max-labels]))
        min-confidence (or min-confidence (get-in @classifier-state [:config :min-confidence]))
        ;; Apply rules first
        rule-results (apply-rules document)
        ;; Calculate scores for all categories
        category-scores (map (fn [category]
                               {:category-id (:id category)
                                :category-name (:name category)
                                :confidence (calculate-category-score text category)})
                             (vals (:categories @classifier-state)))
        ;; Combine and filter results
        all-results (concat rule-results category-scores)
        filtered (filter #(>= (:confidence %) min-confidence) all-results)
        sorted (sort-by :confidence > filtered)
        top-results (take max-labels sorted)]
    (log/debug "Document classified" {:labels (count top-results)})
    {:document-id (or (:id document) (hash document))
     :labels top-results
     :classified-at (System/currentTimeMillis)}))

(defn classify-batch
  "Classify multiple documents."
  [documents & {:keys [text-field max-labels min-confidence parallel]}]
  (let [classify-fn #(classify-document % :text-field text-field
                                        :max-labels max-labels
                                        :min-confidence min-confidence)]
    (if parallel
      (pmap classify-fn documents)
      (map classify-fn documents))))

;; =============================================================================
;; TRAINING DATA
;; =============================================================================

(defn add-training-example!
  "Add a training example."
  [example-id {:keys [text category-id metadata]}]
  (.put ^ConcurrentHashMap (:training-data @classifier-state) example-id
        {:id example-id
         :text text
         :category-id category-id
         :metadata metadata
         :added-at (System/currentTimeMillis)}))

(defn remove-training-example!
  "Remove a training example."
  [example-id]
  (.remove ^ConcurrentHashMap (:training-data @classifier-state) example-id))

(defn get-training-examples
  "Get training examples."
  [& {:keys [category-id limit]}]
  (let [examples (vals (:training-data @classifier-state))]
    (cond->> examples
      category-id (filter #(= (:category-id %) category-id))
      limit (take limit))))

(defn extract-keywords-from-training
  "Extract keywords from training examples for a category."
  [category-id & {:keys [min-frequency]}]
  (let [examples (get-training-examples :category-id category-id)
        texts (map :text examples)
        words (mapcat #(str/split (str/lower-case %) #"\s+") texts)
        word-freq (frequencies words)
        min-freq (or min-frequency 2)]
    (->> word-freq
         (filter #(>= (val %) min-freq))
         (sort-by val >)
         (map key)
         (take 50))))

(defn update-category-keywords!
  "Update category keywords from training data."
  [category-id]
  (when-let [category (get-category category-id)]
    (let [keywords (extract-keywords-from-training category-id)]
      (.put ^ConcurrentHashMap (:categories @classifier-state) category-id
            (assoc category :keywords keywords :updated-at (System/currentTimeMillis)))
      (log/info "Category keywords updated" {:category category-id :keywords (count keywords)}))))

;; =============================================================================
;; CLASSIFICATION HISTORY
;; =============================================================================

(defn record-classification!
  "Record a classification result."
  [classification]
  (let [record-id (str (System/currentTimeMillis) "-" (rand-int 10000))]
    (.put ^ConcurrentHashMap (:classifications @classifier-state) record-id
          (assoc classification :record-id record-id))))

(defn get-classification-history
  "Get classification history."
  [& {:keys [category-id limit since]}]
  (let [classifications (vals (:classifications @classifier-state))]
    (cond->> classifications
      category-id (filter #(some (fn [l] (= (:category-id l) category-id)) (:labels %)))
      since (filter #(>= (:classified-at %) since))
      true (sort-by :classified-at >)
      limit (take limit))))

;; =============================================================================
;; BUILT-IN CATEGORIES
;; =============================================================================

(defn init-built-in-categories!
  "Initialize built-in document categories."
  []
  ;; Mental model categories
  (create-category! :psychology
                    {:name "Psychology"
                     :description "Psychological biases and tendencies"
                     :keywords ["bias" "tendency" "psychology" "behavior" "cognitive"]})
  (create-category! :economics
                    {:name "Economics"
                     :description "Economic principles and models"
                     :keywords ["market" "price" "supply" "demand" "economics" "incentive"]})
  (create-category! :systems
                    {:name "Systems Thinking"
                     :description "Systems and complexity"
                     :keywords ["system" "feedback" "loop" "complexity" "emergence"]})
  (create-category! :decision-making
                    {:name "Decision Making"
                     :description "Decision frameworks and heuristics"
                     :keywords ["decision" "choice" "option" "tradeoff" "risk"]})
  (create-category! :investing
                    {:name "Investing"
                     :description "Investment principles"
                     :keywords ["invest" "return" "risk" "portfolio" "value" "margin"]})
  ;; Document type categories
  (create-category! :analysis
                    {:name "Analysis"
                     :description "Analytical documents"
                     :keywords ["analysis" "examine" "evaluate" "assess" "review"]})
  (create-category! :report
                    {:name "Report"
                     :description "Reports and summaries"
                     :keywords ["report" "summary" "findings" "conclusion" "recommendation"]})
  (create-category! :research
                    {:name "Research"
                     :description "Research documents"
                     :keywords ["research" "study" "data" "methodology" "results"]}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-classifier-stats
  "Get classifier statistics."
  []
  {:categories (.size ^ConcurrentHashMap (:categories @classifier-state))
   :rules (.size ^ConcurrentHashMap (:rules @classifier-state))
   :training-examples (.size ^ConcurrentHashMap (:training-data @classifier-state))
   :classifications (.size ^ConcurrentHashMap (:classifications @classifier-state))
   :classification-count (.get ^AtomicLong (:classification-count @classifier-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-document-classifier!
  "Initialize document classifier."
  []
  (log/info "Initializing document classifier")
  ;; Register feature flag
  (flags/register-flag! "document-classifier" "Enable document classifier" true)
  ;; Create metrics
  (metrics/create-counter! :documentclassifier/classifications "Documents classified")
  (metrics/create-gauge! :documentclassifier/categories "Total categories"
                         #(.size ^ConcurrentHashMap (:categories @classifier-state)))
  ;; Initialize built-in categories
  (init-built-in-categories!)
  (log/info "Document classifier initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-document-classifier-status []
  {:enabled (flags/is-enabled? "document-classifier")
   :categories (.size ^ConcurrentHashMap (:categories @classifier-state))
   :rules (.size ^ConcurrentHashMap (:rules @classifier-state))
   :training-examples (.size ^ConcurrentHashMap (:training-data @classifier-state))
   :stats (get-classifier-stats)
   :config (:config @classifier-state)})
