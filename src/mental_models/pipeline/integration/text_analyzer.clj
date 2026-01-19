(ns mental-models.pipeline.integration.text-analyzer
  "Text Analyzer Module
   
   Text analysis and NLP processing:
   - Tokenization and stemming
   - Sentiment analysis
   - Keyword extraction
   - Entity recognition
   - Text statistics"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]
   [java.text BreakIterator]
   [java.util Locale]))

;; =============================================================================
;; TEXT ANALYZER STATE
;; =============================================================================

(defonce analyzer-state (atom {:analyzers (ConcurrentHashMap.)
                               :stop-words #{"a" "an" "the" "and" "or" "but" "in" "on" "at" "to" "for"
                                            "of" "with" "by" "from" "as" "is" "was" "are" "were" "been"
                                            "be" "have" "has" "had" "do" "does" "did" "will" "would"
                                            "could" "should" "may" "might" "must" "shall" "can" "need"
                                            "this" "that" "these" "those" "i" "you" "he" "she" "it"
                                            "we" "they" "what" "which" "who" "whom" "whose" "where"
                                            "when" "why" "how" "all" "each" "every" "both" "few"
                                            "more" "most" "other" "some" "such" "no" "nor" "not"
                                            "only" "own" "same" "so" "than" "too" "very" "just"}
                               :sentiment-words {:positive #{"good" "great" "excellent" "amazing" "wonderful"
                                                             "fantastic" "awesome" "best" "love" "happy"
                                                             "positive" "success" "successful" "win" "winning"
                                                             "beautiful" "perfect" "brilliant" "outstanding"}
                                                 :negative #{"bad" "terrible" "awful" "horrible" "worst"
                                                             "hate" "sad" "negative" "fail" "failure"
                                                             "poor" "wrong" "ugly" "stupid" "boring"
                                                             "disappointing" "frustrating" "annoying"}}
                               :analysis-count (AtomicLong. 0)
                               :config {:min-word-length 2
                                        :max-keywords 20
                                        :ngram-sizes [1 2 3]}}))

;; =============================================================================
;; TOKENIZATION
;; =============================================================================

(defn tokenize
  "Tokenize text into words."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" " ")
        (str/split #"\s+")
        (->> (remove str/blank?)))))

(defn tokenize-sentences
  "Tokenize text into sentences."
  [text]
  (when text
    (let [iterator (BreakIterator/getSentenceInstance Locale/ENGLISH)]
      (.setText iterator text)
      (loop [start (.first iterator)
             sentences []]
        (let [end (.next iterator)]
          (if (= end BreakIterator/DONE)
            sentences
            (recur end (conj sentences (str/trim (subs text start end))))))))))

(defn remove-stop-words
  "Remove stop words from tokens."
  [tokens]
  (let [stop-words (:stop-words @analyzer-state)]
    (remove #(contains? stop-words (str/lower-case %)) tokens)))

(defn stem-word
  "Simple Porter-like stemming."
  [word]
  (let [word (str/lower-case word)]
    (cond
      (str/ends-with? word "ies") (str (subs word 0 (- (count word) 3)) "y")
      (str/ends-with? word "es") (subs word 0 (- (count word) 2))
      (str/ends-with? word "s") (subs word 0 (- (count word) 1))
      (str/ends-with? word "ing") (subs word 0 (- (count word) 3))
      (str/ends-with? word "ed") (subs word 0 (- (count word) 2))
      (str/ends-with? word "ly") (subs word 0 (- (count word) 2))
      :else word)))

(defn stem-tokens
  "Stem all tokens."
  [tokens]
  (map stem-word tokens))

;; =============================================================================
;; N-GRAMS
;; =============================================================================

(defn generate-ngrams
  "Generate n-grams from tokens."
  [tokens n]
  (when (>= (count tokens) n)
    (map #(str/join " " %) (partition n 1 tokens))))

(defn generate-all-ngrams
  "Generate n-grams of multiple sizes."
  [tokens & {:keys [sizes]}]
  (let [sizes (or sizes (get-in @analyzer-state [:config :ngram-sizes]))]
    (mapcat #(generate-ngrams tokens %) sizes)))

;; =============================================================================
;; WORD FREQUENCY
;; =============================================================================

(defn word-frequency
  "Calculate word frequency."
  [tokens]
  (frequencies tokens))

(defn top-words
  "Get top N most frequent words."
  [tokens n]
  (->> tokens
       word-frequency
       (sort-by val >)
       (take n)
       (into {})))

(defn tf
  "Calculate term frequency."
  [term tokens]
  (let [freq (word-frequency tokens)
        total (count tokens)]
    (if (pos? total)
      (double (/ (get freq term 0) total))
      0.0)))

(defn idf
  "Calculate inverse document frequency."
  [term documents]
  (let [doc-count (count documents)
        docs-with-term (count (filter #(contains? (set %) term) documents))]
    (if (pos? docs-with-term)
      (Math/log (/ doc-count docs-with-term))
      0.0)))

(defn tf-idf
  "Calculate TF-IDF score."
  [term tokens documents]
  (* (tf term tokens) (idf term documents)))

;; =============================================================================
;; KEYWORD EXTRACTION
;; =============================================================================

(defn extract-keywords
  "Extract keywords from text."
  [text & {:keys [max-keywords min-length]}]
  (let [max-keywords (or max-keywords (get-in @analyzer-state [:config :max-keywords]))
        min-length (or min-length (get-in @analyzer-state [:config :min-word-length]))
        tokens (-> text tokenize remove-stop-words)
        filtered (filter #(>= (count %) min-length) tokens)
        freq (word-frequency filtered)]
    (->> freq
         (sort-by val >)
         (take max-keywords)
         (map first)
         vec)))

(defn extract-keyphrases
  "Extract key phrases (n-grams) from text."
  [text & {:keys [max-phrases ngram-size]}]
  (let [max-phrases (or max-phrases 10)
        ngram-size (or ngram-size 2)
        tokens (-> text tokenize remove-stop-words)
        ngrams (generate-ngrams tokens ngram-size)
        freq (frequencies ngrams)]
    (->> freq
         (sort-by val >)
         (take max-phrases)
         (map first)
         vec)))

;; =============================================================================
;; SENTIMENT ANALYSIS
;; =============================================================================

(defn analyze-sentiment
  "Analyze sentiment of text."
  [text]
  (let [tokens (tokenize text)
        positive-words (get-in @analyzer-state [:sentiment-words :positive])
        negative-words (get-in @analyzer-state [:sentiment-words :negative])
        positive-count (count (filter #(contains? positive-words %) tokens))
        negative-count (count (filter #(contains? negative-words %) tokens))
        total-sentiment-words (+ positive-count negative-count)
        score (if (pos? total-sentiment-words)
                (double (/ (- positive-count negative-count) total-sentiment-words))
                0.0)]
    {:score score
     :label (cond
              (> score 0.2) :positive
              (< score -0.2) :negative
              :else :neutral)
     :positive-count positive-count
     :negative-count negative-count
     :confidence (if (pos? total-sentiment-words)
                   (double (/ (max positive-count negative-count) total-sentiment-words))
                   0.0)}))

(defn add-sentiment-words!
  "Add custom sentiment words."
  [polarity words]
  (swap! analyzer-state update-in [:sentiment-words polarity] into words))

;; =============================================================================
;; TEXT STATISTICS
;; =============================================================================

(defn text-statistics
  "Calculate text statistics."
  [text]
  (let [chars (count text)
        chars-no-spaces (count (str/replace text #"\s" ""))
        words (tokenize text)
        word-count (count words)
        sentences (tokenize-sentences text)
        sentence-count (count sentences)
        avg-word-length (if (pos? word-count)
                          (double (/ chars-no-spaces word-count))
                          0.0)
        avg-sentence-length (if (pos? sentence-count)
                              (double (/ word-count sentence-count))
                              0.0)]
    {:characters chars
     :characters-no-spaces chars-no-spaces
     :words word-count
     :sentences sentence-count
     :paragraphs (count (str/split text #"\n\n+"))
     :avg-word-length avg-word-length
     :avg-sentence-length avg-sentence-length
     :unique-words (count (set words))
     :lexical-diversity (if (pos? word-count)
                          (double (/ (count (set words)) word-count))
                          0.0)}))

(defn readability-score
  "Calculate Flesch-Kincaid readability score."
  [text]
  (let [stats (text-statistics text)
        words (:words stats)
        sentences (:sentences stats)
        syllables (reduce + (map #(max 1 (count (re-seq #"[aeiouy]+" (str/lower-case %)))) (tokenize text)))]
    (if (and (pos? words) (pos? sentences))
      (let [fk-grade (- (+ (* 0.39 (/ words sentences))
                           (* 11.8 (/ syllables words)))
                        15.59)
            fk-ease (- 206.835
                       (* 1.015 (/ words sentences))
                       (* 84.6 (/ syllables words)))]
        {:flesch-kincaid-grade fk-grade
         :flesch-reading-ease fk-ease
         :difficulty (cond
                       (>= fk-ease 90) :very-easy
                       (>= fk-ease 70) :easy
                       (>= fk-ease 50) :moderate
                       (>= fk-ease 30) :difficult
                       :else :very-difficult)})
      {:flesch-kincaid-grade 0
       :flesch-reading-ease 0
       :difficulty :unknown})))

;; =============================================================================
;; ENTITY RECOGNITION
;; =============================================================================

(def entity-patterns
  {:email #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
   :url #"https?://[^\s]+"
   :phone #"\+?[\d\s\-\(\)]{10,}"
   :date #"\d{1,2}[/\-]\d{1,2}[/\-]\d{2,4}"
   :time #"\d{1,2}:\d{2}(?::\d{2})?\s*(?:AM|PM|am|pm)?"
   :money #"\$[\d,]+(?:\.\d{2})?"
   :percentage #"\d+(?:\.\d+)?%"
   :number #"\b\d+(?:\.\d+)?\b"})

(defn extract-entities
  "Extract entities from text."
  [text & {:keys [types]}]
  (let [types (or types (keys entity-patterns))]
    (into {} (for [type types
                   :let [pattern (get entity-patterns type)]
                   :when pattern]
               [type (vec (re-seq pattern text))]))))

(defn register-entity-pattern!
  "Register a custom entity pattern."
  [entity-type pattern]
  (alter-var-root #'entity-patterns assoc entity-type (re-pattern pattern)))

;; =============================================================================
;; TEXT SIMILARITY
;; =============================================================================

(defn jaccard-similarity
  "Calculate Jaccard similarity between two texts."
  [text1 text2]
  (let [tokens1 (set (tokenize text1))
        tokens2 (set (tokenize text2))
        intersection (count (clojure.set/intersection tokens1 tokens2))
        union (count (clojure.set/union tokens1 tokens2))]
    (if (pos? union)
      (double (/ intersection union))
      0.0)))

(defn cosine-similarity
  "Calculate cosine similarity between two texts."
  [text1 text2]
  (let [tokens1 (tokenize text1)
        tokens2 (tokenize text2)
        all-words (set (concat tokens1 tokens2))
        vec1 (map #(get (word-frequency tokens1) % 0) all-words)
        vec2 (map #(get (word-frequency tokens2) % 0) all-words)
        dot-product (reduce + (map * vec1 vec2))
        magnitude1 (Math/sqrt (reduce + (map #(* % %) vec1)))
        magnitude2 (Math/sqrt (reduce + (map #(* % %) vec2)))]
    (if (and (pos? magnitude1) (pos? magnitude2))
      (/ dot-product (* magnitude1 magnitude2))
      0.0)))

(defn levenshtein-distance
  "Calculate Levenshtein edit distance."
  [s1 s2]
  (let [len1 (count s1)
        len2 (count s2)]
    (cond
      (zero? len1) len2
      (zero? len2) len1
      :else
      (let [matrix (vec (for [i (range (inc len1))]
                          (vec (for [j (range (inc len2))]
                                 (cond
                                   (zero? i) j
                                   (zero? j) i
                                   :else 0)))))]
        (loop [i 1 m matrix]
          (if (> i len1)
            (get-in m [len1 len2])
            (recur (inc i)
                   (loop [j 1 m m]
                     (if (> j len2)
                       m
                       (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)
                             val (min (inc (get-in m [(dec i) j]))
                                      (inc (get-in m [i (dec j)]))
                                      (+ (get-in m [(dec i) (dec j)]) cost))]
                         (recur (inc j) (assoc-in m [i j] val))))))))))))

;; =============================================================================
;; COMPREHENSIVE ANALYSIS
;; =============================================================================

(defn analyze-text
  "Perform comprehensive text analysis."
  [text]
  (.incrementAndGet ^AtomicLong (:analysis-count @analyzer-state))
  (metrics/inc-counter! :textanalyzer/analyses-performed)
  {:statistics (text-statistics text)
   :readability (readability-score text)
   :sentiment (analyze-sentiment text)
   :keywords (extract-keywords text)
   :keyphrases (extract-keyphrases text)
   :entities (extract-entities text)
   :analyzed-at (System/currentTimeMillis)})

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-analyzer-stats
  "Get text analyzer statistics."
  []
  {:analyzers (.size ^ConcurrentHashMap (:analyzers @analyzer-state))
   :stop-words (count (:stop-words @analyzer-state))
   :positive-words (count (get-in @analyzer-state [:sentiment-words :positive]))
   :negative-words (count (get-in @analyzer-state [:sentiment-words :negative]))
   :analysis-count (.get ^AtomicLong (:analysis-count @analyzer-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-text-analyzer!
  "Initialize text analyzer."
  []
  (log/info "Initializing text analyzer")
  ;; Register feature flag
  (flags/register-flag! "text-analyzer" "Enable text analyzer" true)
  ;; Create metrics
  (metrics/create-counter! :textanalyzer/analyses-performed "Analyses performed")
  (metrics/create-gauge! :textanalyzer/total-analyses "Total analyses"
                         #(.get ^AtomicLong (:analysis-count @analyzer-state)))
  (log/info "Text analyzer initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-text-analyzer-status []
  {:enabled (flags/is-enabled? "text-analyzer")
   :stop-words (count (:stop-words @analyzer-state))
   :sentiment-words {:positive (count (get-in @analyzer-state [:sentiment-words :positive]))
                     :negative (count (get-in @analyzer-state [:sentiment-words :negative]))}
   :analysis-count (.get ^AtomicLong (:analysis-count @analyzer-state))
   :stats (get-analyzer-stats)
   :config (:config @analyzer-state)})
