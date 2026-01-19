(ns mental-models.pipeline.integration.content-analyzer
  "Content Analyzer Module
   
   Text content analysis:
   - Sentiment analysis
   - Keyword extraction
   - Entity recognition
   - Topic modeling
   - Readability scoring"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; ANALYZER STATE
;; =============================================================================

(defonce analyzer-state (atom {:analyzers {}
                               :stop-words #{"the" "a" "an" "is" "are" "was" "were" "be" "been"
                                             "being" "have" "has" "had" "do" "does" "did" "will"
                                             "would" "could" "should" "may" "might" "must" "shall"
                                             "can" "need" "dare" "ought" "used" "to" "of" "in"
                                             "for" "on" "with" "at" "by" "from" "as" "into"
                                             "through" "during" "before" "after" "above" "below"
                                             "between" "under" "again" "further" "then" "once"
                                             "and" "but" "or" "nor" "so" "yet" "both" "either"
                                             "neither" "not" "only" "own" "same" "than" "too"
                                             "very" "just" "also" "now" "here" "there" "when"
                                             "where" "why" "how" "all" "each" "every" "both"
                                             "few" "more" "most" "other" "some" "such" "no"
                                             "any" "this" "that" "these" "those" "i" "me" "my"
                                             "we" "our" "you" "your" "he" "him" "his" "she"
                                             "her" "it" "its" "they" "them" "their" "what"
                                             "which" "who" "whom" "whose"}
                               :sentiment-words {:positive #{"good" "great" "excellent" "amazing" "wonderful"
                                                             "fantastic" "terrific" "outstanding" "superb"
                                                             "brilliant" "awesome" "perfect" "best" "love"
                                                             "happy" "joy" "success" "win" "positive"
                                                             "benefit" "advantage" "improve" "growth"}
                                                 :negative #{"bad" "terrible" "awful" "horrible" "poor"
                                                             "worst" "hate" "sad" "fail" "failure"
                                                             "negative" "problem" "issue" "wrong" "error"
                                                             "mistake" "loss" "decline" "decrease" "risk"
                                                             "danger" "threat" "concern" "worry"}}}))

;; =============================================================================
;; TEXT PREPROCESSING
;; =============================================================================

(defn tokenize
  "Tokenize text into words."
  [text]
  (-> text
      str/lower-case
      (str/replace #"[^a-z0-9\s]" " ")
      (str/split #"\s+")
      (->> (filter #(> (count %) 1)))))

(defn remove-stop-words
  "Remove stop words from tokens."
  [tokens]
  (let [stop-words (:stop-words @analyzer-state)]
    (remove stop-words tokens)))

(defn stem-word
  "Simple stemming (Porter-like)."
  [word]
  (-> word
      (str/replace #"ing$" "")
      (str/replace #"ed$" "")
      (str/replace #"ly$" "")
      (str/replace #"ies$" "y")
      (str/replace #"es$" "")
      (str/replace #"s$" "")))

(defn preprocess
  "Full preprocessing pipeline."
  [text]
  (->> text
       tokenize
       remove-stop-words
       (map stem-word)))

;; =============================================================================
;; SENTIMENT ANALYSIS
;; =============================================================================

(defn analyze-sentiment
  "Analyze sentiment of text."
  [text]
  (when (flags/is-enabled? "content-analyzer")
    (let [tokens (tokenize text)
          {:keys [positive negative]} (:sentiment-words @analyzer-state)
          pos-count (count (filter positive tokens))
          neg-count (count (filter negative tokens))
          total (count tokens)
          score (if (pos? total)
                  (/ (- pos-count neg-count) (double total))
                  0.0)]
      (metrics/inc-counter! :analyzer/sentiment-analyses)
      {:score score
       :label (cond
                (> score 0.1) :positive
                (< score -0.1) :negative
                :else :neutral)
       :positive-words pos-count
       :negative-words neg-count
       :total-words total})))

;; =============================================================================
;; KEYWORD EXTRACTION
;; =============================================================================

(defn calculate-tf
  "Calculate term frequency."
  [tokens]
  (let [total (count tokens)
        freqs (frequencies tokens)]
    (into {} (map (fn [[term count]]
                    [term (/ (double count) total)])
                  freqs))))

(defn extract-keywords
  "Extract keywords from text using TF."
  [text & {:keys [limit] :or {limit 10}}]
  (when (flags/is-enabled? "content-analyzer")
    (let [tokens (preprocess text)
          tf (calculate-tf tokens)
          sorted (sort-by val > tf)]
      (metrics/inc-counter! :analyzer/keyword-extractions)
      (->> sorted
           (take limit)
           (map (fn [[term score]]
                  {:term term :score score}))))))

;; =============================================================================
;; N-GRAM EXTRACTION
;; =============================================================================

(defn extract-ngrams
  "Extract n-grams from text."
  [text n & {:keys [limit] :or {limit 10}}]
  (let [tokens (tokenize text)
        ngrams (partition n 1 tokens)
        ngram-strs (map #(str/join " " %) ngrams)
        freqs (frequencies ngram-strs)
        sorted (sort-by val > freqs)]
    (->> sorted
         (take limit)
         (map (fn [[ngram count]]
                {:ngram ngram :count count})))))

(defn extract-bigrams
  "Extract bigrams from text."
  [text & opts]
  (apply extract-ngrams text 2 opts))

(defn extract-trigrams
  "Extract trigrams from text."
  [text & opts]
  (apply extract-ngrams text 3 opts))

;; =============================================================================
;; ENTITY RECOGNITION
;; =============================================================================

(def entity-patterns
  {:email #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
   :url #"https?://[^\s]+"
   :phone #"\+?[0-9]{1,3}[-.\s]?[0-9]{3}[-.\s]?[0-9]{3}[-.\s]?[0-9]{4}"
   :date #"\d{1,2}[/-]\d{1,2}[/-]\d{2,4}"
   :money #"\$[0-9,]+(?:\.[0-9]{2})?"
   :percentage #"[0-9]+(?:\.[0-9]+)?%"
   :number #"\b[0-9]+(?:\.[0-9]+)?\b"})

(defn extract-entities
  "Extract named entities from text."
  [text]
  (when (flags/is-enabled? "content-analyzer")
    (metrics/inc-counter! :analyzer/entity-extractions)
    (into {}
          (for [[entity-type pattern] entity-patterns]
            [entity-type (vec (re-seq pattern text))]))))

;; =============================================================================
;; READABILITY SCORING
;; =============================================================================

(defn count-syllables
  "Estimate syllable count in a word."
  [word]
  (let [word (str/lower-case word)
        vowels "aeiouy"
        count (count (filter #(str/includes? vowels (str %)) word))]
    (max 1 count)))

(defn calculate-readability
  "Calculate readability scores."
  [text]
  (when (flags/is-enabled? "content-analyzer")
    (let [sentences (str/split text #"[.!?]+")
          words (tokenize text)
          sentence-count (count (filter #(> (count (str/trim %)) 0) sentences))
          word-count (count words)
          syllable-count (reduce + (map count-syllables words))
          ;; Flesch Reading Ease
          flesch-ease (if (and (pos? sentence-count) (pos? word-count))
                        (- 206.835
                           (* 1.015 (/ word-count sentence-count))
                           (* 84.6 (/ syllable-count word-count)))
                        0)
          ;; Flesch-Kincaid Grade Level
          fk-grade (if (and (pos? sentence-count) (pos? word-count))
                     (+ (* 0.39 (/ word-count sentence-count))
                        (* 11.8 (/ syllable-count word-count))
                        -15.59)
                     0)]
      (metrics/inc-counter! :analyzer/readability-analyses)
      {:flesch-ease flesch-ease
       :flesch-kincaid-grade fk-grade
       :word-count word-count
       :sentence-count sentence-count
       :avg-words-per-sentence (if (pos? sentence-count)
                                 (/ word-count (double sentence-count))
                                 0)
       :avg-syllables-per-word (if (pos? word-count)
                                 (/ syllable-count (double word-count))
                                 0)})))

;; =============================================================================
;; TOPIC DETECTION
;; =============================================================================

(def topic-keywords
  {:finance #{"money" "invest" "stock" "market" "profit" "loss" "revenue" "cost"
              "price" "value" "capital" "fund" "asset" "debt" "equity" "return"}
   :technology #{"software" "hardware" "computer" "data" "algorithm" "code"
                 "program" "system" "network" "digital" "cloud" "ai" "machine"}
   :business #{"company" "business" "market" "customer" "product" "service"
               "strategy" "management" "growth" "competition" "industry"}
   :psychology #{"behavior" "mind" "cognitive" "bias" "decision" "emotion"
                 "motivation" "perception" "thinking" "mental" "psychology"}
   :economics #{"economy" "inflation" "gdp" "trade" "supply" "demand"
                "market" "price" "growth" "recession" "policy" "fiscal"}})

(defn detect-topics
  "Detect topics in text."
  [text]
  (when (flags/is-enabled? "content-analyzer")
    (let [tokens (set (tokenize text))
          topic-scores (for [[topic keywords] topic-keywords]
                         (let [matches (count (clojure.set/intersection tokens keywords))]
                           [topic matches]))
          sorted (sort-by second > topic-scores)
          top-topics (filter #(pos? (second %)) sorted)]
      (metrics/inc-counter! :analyzer/topic-detections)
      (map (fn [[topic score]]
             {:topic topic :score score})
           top-topics))))

;; =============================================================================
;; COMPREHENSIVE ANALYSIS
;; =============================================================================

(defn analyze
  "Perform comprehensive content analysis."
  [text]
  (when (flags/is-enabled? "content-analyzer")
    (log/info "Analyzing content" {:length (count text)})
    (let [start-time (System/currentTimeMillis)
          result {:sentiment (analyze-sentiment text)
                  :keywords (extract-keywords text)
                  :entities (extract-entities text)
                  :readability (calculate-readability text)
                  :topics (detect-topics text)
                  :bigrams (extract-bigrams text :limit 5)
                  :word-count (count (tokenize text))
                  :char-count (count text)}
          duration (- (System/currentTimeMillis) start-time)]
      (metrics/observe-histogram! :analyzer/analysis-time duration)
      (events/publish! :analyzer/analysis-complete {:duration duration})
      result)))

;; =============================================================================
;; CUSTOM ANALYZERS
;; =============================================================================

(defn register-analyzer!
  "Register a custom analyzer."
  [analyzer-id analyzer-fn]
  (log/info "Registering analyzer" {:id analyzer-id})
  (swap! analyzer-state assoc-in [:analyzers analyzer-id] analyzer-fn))

(defn run-analyzer
  "Run a custom analyzer."
  [analyzer-id text]
  (when-let [analyzer-fn (get-in @analyzer-state [:analyzers analyzer-id])]
    (analyzer-fn text)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-content-analyzer!
  "Initialize content analyzer."
  []
  (log/info "Initializing content analyzer")
  ;; Register feature flag
  (flags/register-flag! "content-analyzer" "Enable content analysis" true)
  ;; Create metrics
  (metrics/create-counter! :analyzer/sentiment-analyses "Sentiment analyses")
  (metrics/create-counter! :analyzer/keyword-extractions "Keyword extractions")
  (metrics/create-counter! :analyzer/entity-extractions "Entity extractions")
  (metrics/create-counter! :analyzer/readability-analyses "Readability analyses")
  (metrics/create-counter! :analyzer/topic-detections "Topic detections")
  (metrics/create-histogram! :analyzer/analysis-time "Analysis time" [10 50 100 500 1000])
  (log/info "Content analyzer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-analyzer-status []
  {:enabled (flags/is-enabled? "content-analyzer")
   :custom-analyzers (count (:analyzers @analyzer-state))
   :stop-words (count (:stop-words @analyzer-state))
   :topic-categories (count topic-keywords)})
