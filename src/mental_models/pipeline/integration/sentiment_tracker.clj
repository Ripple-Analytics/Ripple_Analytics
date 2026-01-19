(ns mental-models.pipeline.integration.sentiment-tracker
  "Sentiment Tracker Module
   
   Sentiment analysis and tracking:
   - Real-time sentiment scoring
   - Sentiment trends over time
   - Entity-level sentiment
   - Sentiment alerts
   - Historical analysis"
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
;; SENTIMENT TRACKER STATE
;; =============================================================================

(defonce tracker-state (atom {:analyses (ConcurrentHashMap.)
                              :trends (ConcurrentHashMap.)
                              :entities (ConcurrentHashMap.)
                              :alerts (ConcurrentHashMap.)
                              :lexicon {:positive #{} :negative #{}}
                              :analysis-count (AtomicLong. 0)
                              :config {:window-size-ms 3600000
                                       :alert-threshold 0.3
                                       :trend-periods 24}}))

;; =============================================================================
;; SENTIMENT LEXICON
;; =============================================================================

(def default-positive-words
  #{"good" "great" "excellent" "amazing" "wonderful" "fantastic" "positive"
    "happy" "joy" "love" "best" "better" "success" "successful" "win" "winning"
    "profit" "gain" "growth" "improve" "improvement" "opportunity" "strong"
    "strength" "advantage" "benefit" "valuable" "value" "quality" "efficient"
    "effective" "innovative" "innovation" "breakthrough" "achievement"})

(def default-negative-words
  #{"bad" "terrible" "awful" "horrible" "negative" "sad" "hate" "worst"
    "worse" "failure" "fail" "loss" "lose" "losing" "decline" "decrease"
    "problem" "issue" "risk" "danger" "threat" "weakness" "weak" "poor"
    "inefficient" "ineffective" "mistake" "error" "bug" "crash" "crisis"
    "concern" "worry" "fear" "doubt" "uncertain" "volatile" "unstable"})

(defn init-lexicon!
  "Initialize the sentiment lexicon."
  []
  (swap! tracker-state assoc :lexicon
         {:positive default-positive-words
          :negative default-negative-words}))

(defn add-positive-word!
  "Add a word to the positive lexicon."
  [word]
  (swap! tracker-state update-in [:lexicon :positive] conj (str/lower-case word)))

(defn add-negative-word!
  "Add a word to the negative lexicon."
  [word]
  (swap! tracker-state update-in [:lexicon :negative] conj (str/lower-case word)))

(defn remove-word!
  "Remove a word from the lexicon."
  [word]
  (swap! tracker-state update-in [:lexicon :positive] disj (str/lower-case word))
  (swap! tracker-state update-in [:lexicon :negative] disj (str/lower-case word)))

;; =============================================================================
;; SENTIMENT ANALYSIS
;; =============================================================================

(defn tokenize
  "Tokenize text into words."
  [text]
  (-> text
      str/lower-case
      (str/replace #"[^\w\s]" "")
      (str/split #"\s+")
      (->> (filter #(> (count %) 1)))))

(defn count-sentiment-words
  "Count positive and negative words in tokens."
  [tokens]
  (let [lexicon (:lexicon @tracker-state)
        positive-set (:positive lexicon)
        negative-set (:negative lexicon)]
    {:positive (count (filter #(contains? positive-set %) tokens))
     :negative (count (filter #(contains? negative-set %) tokens))
     :total (count tokens)}))

(defn calculate-sentiment-score
  "Calculate sentiment score from word counts."
  [{:keys [positive negative total]}]
  (if (zero? total)
    0.0
    (/ (- positive negative) (double total))))

(defn classify-sentiment
  "Classify sentiment as positive, negative, or neutral."
  [score]
  (cond
    (> score 0.1) :positive
    (< score -0.1) :negative
    :else :neutral))

(defn analyze-sentiment
  "Analyze sentiment of text."
  [text & {:keys [entity-id source]}]
  (.incrementAndGet ^AtomicLong (:analysis-count @tracker-state))
  (metrics/inc-counter! :sentimenttracker/analyses)
  (let [tokens (tokenize text)
        counts (count-sentiment-words tokens)
        score (calculate-sentiment-score counts)
        classification (classify-sentiment score)
        analysis {:id (str (java.util.UUID/randomUUID))
                  :text (subs text 0 (min 200 (count text)))
                  :tokens-count (count tokens)
                  :positive-count (:positive counts)
                  :negative-count (:negative counts)
                  :score score
                  :classification classification
                  :entity-id entity-id
                  :source source
                  :analyzed-at (System/currentTimeMillis)}]
    ;; Store analysis
    (.put ^ConcurrentHashMap (:analyses @tracker-state) (:id analysis) analysis)
    ;; Update entity sentiment if provided
    (when entity-id
      (update-entity-sentiment! entity-id analysis))
    ;; Update trends
    (update-trend! analysis)
    ;; Check for alerts
    (check-sentiment-alerts! analysis)
    (log/debug "Sentiment analyzed" {:score score :classification classification})
    analysis))

(defn analyze-batch
  "Analyze sentiment of multiple texts."
  [texts & {:keys [parallel]}]
  (if parallel
    (pmap #(analyze-sentiment %) texts)
    (map #(analyze-sentiment %) texts)))

;; =============================================================================
;; ENTITY SENTIMENT
;; =============================================================================

(defn update-entity-sentiment!
  "Update sentiment tracking for an entity."
  [entity-id analysis]
  (let [existing (or (.get ^ConcurrentHashMap (:entities @tracker-state) entity-id)
                     {:entity-id entity-id
                      :analyses []
                      :total-score 0.0
                      :count 0
                      :first-seen (System/currentTimeMillis)})
        updated (-> existing
                    (update :analyses #(take 100 (conj % (:id analysis))))
                    (update :total-score + (:score analysis))
                    (update :count inc)
                    (assoc :last-seen (System/currentTimeMillis)
                           :avg-score (/ (+ (:total-score existing) (:score analysis))
                                         (inc (:count existing)))))]
    (.put ^ConcurrentHashMap (:entities @tracker-state) entity-id updated)))

(defn get-entity-sentiment
  "Get sentiment data for an entity."
  [entity-id]
  (.get ^ConcurrentHashMap (:entities @tracker-state) entity-id))

(defn list-entities
  "List all tracked entities."
  [& {:keys [min-count sort-by-field limit]}]
  (let [entities (vals (:entities @tracker-state))]
    (cond->> entities
      min-count (filter #(>= (:count %) min-count))
      sort-by-field (sort-by sort-by-field >)
      limit (take limit))))

(defn get-entity-history
  "Get sentiment history for an entity."
  [entity-id & {:keys [limit since]}]
  (let [entity (get-entity-sentiment entity-id)
        analysis-ids (or (:analyses entity) [])
        analyses (keep #(.get ^ConcurrentHashMap (:analyses @tracker-state) %) analysis-ids)]
    (cond->> analyses
      since (filter #(>= (:analyzed-at %) since))
      true (sort-by :analyzed-at >)
      limit (take limit))))

;; =============================================================================
;; TREND TRACKING
;; =============================================================================

(defn get-time-bucket
  "Get the time bucket for a timestamp."
  [timestamp window-ms]
  (* (quot timestamp window-ms) window-ms))

(defn update-trend!
  "Update sentiment trend data."
  [analysis]
  (let [window-ms (get-in @tracker-state [:config :window-size-ms])
        bucket (get-time-bucket (:analyzed-at analysis) window-ms)
        trend-key (str bucket)
        existing (or (.get ^ConcurrentHashMap (:trends @tracker-state) trend-key)
                     {:bucket bucket
                      :total-score 0.0
                      :count 0
                      :positive 0
                      :negative 0
                      :neutral 0})
        updated (-> existing
                    (update :total-score + (:score analysis))
                    (update :count inc)
                    (update (:classification analysis) inc)
                    (assoc :avg-score (/ (+ (:total-score existing) (:score analysis))
                                         (inc (:count existing)))))]
    (.put ^ConcurrentHashMap (:trends @tracker-state) trend-key updated)))

(defn get-trends
  "Get sentiment trends."
  [& {:keys [periods since]}]
  (let [periods (or periods (get-in @tracker-state [:config :trend-periods]))
        window-ms (get-in @tracker-state [:config :window-size-ms])
        now (System/currentTimeMillis)
        since (or since (- now (* periods window-ms)))
        trends (vals (:trends @tracker-state))]
    (->> trends
         (filter #(>= (:bucket %) since))
         (sort-by :bucket))))

(defn calculate-trend-direction
  "Calculate the direction of sentiment trend."
  [& {:keys [periods]}]
  (let [trends (get-trends :periods (or periods 3))]
    (if (< (count trends) 2)
      :stable
      (let [scores (map :avg-score trends)
            first-half (take (/ (count scores) 2) scores)
            second-half (drop (/ (count scores) 2) scores)
            first-avg (if (seq first-half) (/ (reduce + first-half) (count first-half)) 0)
            second-avg (if (seq second-half) (/ (reduce + second-half) (count second-half)) 0)
            diff (- second-avg first-avg)]
        (cond
          (> diff 0.05) :improving
          (< diff -0.05) :declining
          :else :stable)))))

;; =============================================================================
;; ALERTS
;; =============================================================================

(defn create-alert!
  "Create a sentiment alert."
  [alert-id {:keys [name condition threshold action]}]
  (log/info "Creating sentiment alert" {:id alert-id})
  (.put ^ConcurrentHashMap (:alerts @tracker-state) alert-id
        {:id alert-id
         :name name
         :condition condition
         :threshold threshold
         :action action
         :enabled true
         :triggered-count 0
         :last-triggered nil
         :created-at (System/currentTimeMillis)}))

(defn delete-alert!
  "Delete an alert."
  [alert-id]
  (.remove ^ConcurrentHashMap (:alerts @tracker-state) alert-id))

(defn get-alert
  "Get an alert by ID."
  [alert-id]
  (.get ^ConcurrentHashMap (:alerts @tracker-state) alert-id))

(defn check-sentiment-alerts!
  "Check and trigger sentiment alerts."
  [analysis]
  (doseq [[alert-id alert] (:alerts @tracker-state)]
    (when (and (:enabled alert)
               ((:condition alert) analysis))
      (try
        ((:action alert) analysis)
        (.put ^ConcurrentHashMap (:alerts @tracker-state) alert-id
              (-> alert
                  (update :triggered-count inc)
                  (assoc :last-triggered (System/currentTimeMillis))))
        (events/publish! :sentimenttracker/alert-triggered
                         {:alert-id alert-id :analysis-id (:id analysis)})
        (log/info "Sentiment alert triggered" {:alert alert-id})
        (catch Exception e
          (log/error "Alert action failed" {:alert alert-id :error (.getMessage e)}))))))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-tracker-stats
  "Get sentiment tracker statistics."
  []
  (let [analyses (vals (:analyses @tracker-state))
        scores (map :score analyses)]
    {:analyses (.size ^ConcurrentHashMap (:analyses @tracker-state))
     :entities (.size ^ConcurrentHashMap (:entities @tracker-state))
     :trends (.size ^ConcurrentHashMap (:trends @tracker-state))
     :alerts (.size ^ConcurrentHashMap (:alerts @tracker-state))
     :analysis-count (.get ^AtomicLong (:analysis-count @tracker-state))
     :avg-score (if (seq scores) (/ (reduce + scores) (count scores)) 0.0)
     :trend-direction (calculate-trend-direction)}))

(defn get-sentiment-summary
  "Get a summary of recent sentiment."
  [& {:keys [since]}]
  (let [since (or since (- (System/currentTimeMillis) 86400000))
        analyses (filter #(>= (:analyzed-at %) since) (vals (:analyses @tracker-state)))
        by-classification (group-by :classification analyses)]
    {:total (count analyses)
     :positive (count (:positive by-classification))
     :negative (count (:negative by-classification))
     :neutral (count (:neutral by-classification))
     :avg-score (if (seq analyses)
                  (/ (reduce + (map :score analyses)) (count analyses))
                  0.0)
     :period-start since
     :period-end (System/currentTimeMillis)}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-sentiment-tracker!
  "Initialize sentiment tracker."
  []
  (log/info "Initializing sentiment tracker")
  ;; Register feature flag
  (flags/register-flag! "sentiment-tracker" "Enable sentiment tracker" true)
  ;; Create metrics
  (metrics/create-counter! :sentimenttracker/analyses "Sentiment analyses")
  (metrics/create-gauge! :sentimenttracker/avg-score "Average sentiment score"
                         #(:avg-score (get-tracker-stats)))
  ;; Initialize lexicon
  (init-lexicon!)
  ;; Create default alert for negative sentiment spike
  (create-alert! :negative-spike
                 {:name "Negative Sentiment Spike"
                  :condition (fn [a] (< (:score a) -0.3))
                  :threshold -0.3
                  :action (fn [a] (log/warn "Negative sentiment detected" {:score (:score a)}))})
  (log/info "Sentiment tracker initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-sentiment-tracker-status []
  {:enabled (flags/is-enabled? "sentiment-tracker")
   :analyses (.size ^ConcurrentHashMap (:analyses @tracker-state))
   :entities (.size ^ConcurrentHashMap (:entities @tracker-state))
   :trends (.size ^ConcurrentHashMap (:trends @tracker-state))
   :alerts (.size ^ConcurrentHashMap (:alerts @tracker-state))
   :lexicon-size {:positive (count (get-in @tracker-state [:lexicon :positive]))
                  :negative (count (get-in @tracker-state [:lexicon :negative]))}
   :stats (get-tracker-stats)
   :config (:config @tracker-state)})
