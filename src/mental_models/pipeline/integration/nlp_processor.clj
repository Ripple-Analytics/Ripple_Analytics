(ns mental-models.pipeline.integration.nlp-processor
  "Natural Language Processing for mental model text analysis.
   
   Features:
   - Tokenization and sentence splitting
   - Part-of-speech tagging
   - Named entity recognition
   - Dependency parsing
   - Sentiment analysis
   - Keyword extraction
   - Text summarization
   - Language detection"
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
  (atom {:pipelines {}        ;; pipeline-id -> pipeline-config
         :models {}           ;; model-id -> model-config
         :cache {}            ;; text-hash -> processed-result
         :stats {:processed 0 :cache-hits 0}
         :initialized? false}))

;; ============================================================================
;; Tokenization
;; ============================================================================

(defn- word-tokenize
  "Tokenize text into words."
  [text]
  (let [cleaned (-> text
                    (str/lower-case)
                    (str/replace #"[^\w\s'-]" " ")
                    (str/replace #"\s+" " ")
                    str/trim)]
    (str/split cleaned #"\s+")))

(defn- sentence-tokenize
  "Split text into sentences."
  [text]
  (let [;; Simple sentence boundary detection
        sentences (str/split text #"(?<=[.!?])\s+(?=[A-Z])")]
    (mapv str/trim (filter #(not (str/blank? %)) sentences))))

(defn tokenize
  "Tokenize text into words and sentences."
  [text & {:keys [level] :or {level :word}}]
  (case level
    :word (word-tokenize text)
    :sentence (sentence-tokenize text)
    :both {:words (word-tokenize text)
           :sentences (sentence-tokenize text)}
    (word-tokenize text)))

;; ============================================================================
;; Part-of-Speech Tagging
;; ============================================================================

(def ^:private pos-patterns
  "Simple POS patterns based on word endings and common words."
  {:noun-endings #{"tion" "ment" "ness" "ity" "ance" "ence" "er" "or" "ist"}
   :verb-endings #{"ing" "ed" "ize" "ify" "ate"}
   :adj-endings #{"ful" "less" "ous" "ive" "able" "ible" "al" "ical"}
   :adv-endings #{"ly"}
   :determiners #{"the" "a" "an" "this" "that" "these" "those" "my" "your" "his" "her" "its" "our" "their"}
   :prepositions #{"in" "on" "at" "to" "for" "with" "by" "from" "of" "about" "into" "through" "during" "before" "after" "above" "below" "between" "under" "over"}
   :conjunctions #{"and" "but" "or" "nor" "for" "yet" "so" "because" "although" "while" "if" "when" "unless"}
   :pronouns #{"i" "you" "he" "she" "it" "we" "they" "me" "him" "her" "us" "them" "who" "whom" "whose" "which" "that"}})

(defn- guess-pos
  "Guess part-of-speech for a word."
  [word]
  (let [lower (str/lower-case word)]
    (cond
      (contains? (:determiners pos-patterns) lower) :DET
      (contains? (:prepositions pos-patterns) lower) :ADP
      (contains? (:conjunctions pos-patterns) lower) :CONJ
      (contains? (:pronouns pos-patterns) lower) :PRON
      (some #(str/ends-with? lower %) (:adv-endings pos-patterns)) :ADV
      (some #(str/ends-with? lower %) (:adj-endings pos-patterns)) :ADJ
      (some #(str/ends-with? lower %) (:verb-endings pos-patterns)) :VERB
      (some #(str/ends-with? lower %) (:noun-endings pos-patterns)) :NOUN
      (re-matches #"\d+" word) :NUM
      (re-matches #"[A-Z][a-z]*" word) :PROPN
      :else :NOUN)))

(defn pos-tag
  "Tag words with part-of-speech."
  [text]
  (let [tokens (word-tokenize text)]
    (mapv (fn [token]
            {:token token
             :pos (guess-pos token)})
          tokens)))

;; ============================================================================
;; Named Entity Recognition
;; ============================================================================

(def ^:private entity-patterns
  "Patterns for named entity recognition."
  {:person #{"mr" "mrs" "ms" "dr" "prof" "sir" "madam"}
   :organization #{"inc" "corp" "ltd" "llc" "company" "corporation" "institute" "university" "bank" "group"}
   :location #{"street" "avenue" "road" "city" "state" "country" "river" "mountain" "ocean" "sea"}
   :date #{"january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december" "monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"}
   :money #{"$" "€" "£" "¥" "dollar" "euro" "pound" "yen"}})

(defn- is-capitalized?
  "Check if word is capitalized."
  [word]
  (and (not (str/blank? word))
       (Character/isUpperCase (first word))))

(defn- detect-entity-type
  "Detect entity type for a word or phrase."
  [text context]
  (let [lower (str/lower-case text)
        words (str/split lower #"\s+")]
    (cond
      (re-matches #"\d{1,2}/\d{1,2}/\d{2,4}" text) :DATE
      (re-matches #"\d{4}-\d{2}-\d{2}" text) :DATE
      (re-matches #"\$[\d,]+\.?\d*" text) :MONEY
      (some #(contains? (:date entity-patterns) %) words) :DATE
      (some #(contains? (:money entity-patterns) %) words) :MONEY
      (some #(contains? (:person entity-patterns) %) words) :PERSON
      (some #(contains? (:organization entity-patterns) %) words) :ORG
      (some #(contains? (:location entity-patterns) %) words) :LOC
      (and (is-capitalized? text) (> (count words) 1)) :ORG
      (is-capitalized? text) :ENTITY
      :else nil)))

(defn extract-entities
  "Extract named entities from text."
  [text]
  (let [sentences (sentence-tokenize text)
        entities (atom [])]
    (doseq [sentence sentences]
      (let [words (str/split sentence #"\s+")
            ;; Find sequences of capitalized words
            current-entity (atom [])
            start-idx (atom nil)]
        (doseq [[idx word] (map-indexed vector words)]
          (if (and (is-capitalized? word)
                   (not (zero? idx))) ;; Skip first word of sentence
            (do
              (when (nil? @start-idx)
                (reset! start-idx idx))
              (swap! current-entity conj word))
            (when (seq @current-entity)
              (let [entity-text (str/join " " @current-entity)
                    entity-type (detect-entity-type entity-text {})]
                (when entity-type
                  (swap! entities conj
                         {:text entity-text
                          :type entity-type
                          :start @start-idx
                          :end idx})))
              (reset! current-entity [])
              (reset! start-idx nil))))
        ;; Handle entity at end of sentence
        (when (seq @current-entity)
          (let [entity-text (str/join " " @current-entity)
                entity-type (detect-entity-type entity-text {})]
            (when entity-type
              (swap! entities conj
                     {:text entity-text
                      :type entity-type
                      :start @start-idx
                      :end (count words)}))))))
    @entities))

;; ============================================================================
;; Sentiment Analysis
;; ============================================================================

(def ^:private sentiment-lexicon
  "Simple sentiment lexicon."
  {:positive #{"good" "great" "excellent" "amazing" "wonderful" "fantastic" "positive" "happy" "joy" "love" "like" "best" "better" "success" "successful" "win" "winning" "benefit" "beneficial" "advantage" "improve" "improvement" "gain" "profit" "growth" "opportunity" "optimistic" "confident" "strong" "strength"}
   :negative #{"bad" "terrible" "awful" "horrible" "negative" "sad" "hate" "worst" "worse" "fail" "failure" "lose" "losing" "loss" "problem" "issue" "risk" "danger" "threat" "decline" "decrease" "weak" "weakness" "pessimistic" "fear" "worry" "concern" "difficult" "challenge" "crisis"}
   :intensifiers #{"very" "extremely" "highly" "incredibly" "absolutely" "completely" "totally" "really" "quite" "rather"}
   :negators #{"not" "no" "never" "neither" "nobody" "nothing" "nowhere" "hardly" "barely" "scarcely" "don't" "doesn't" "didn't" "won't" "wouldn't" "couldn't" "shouldn't"}})

(defn analyze-sentiment
  "Analyze sentiment of text."
  [text]
  (let [tokens (word-tokenize text)
        positive-count (count (filter #(contains? (:positive sentiment-lexicon) %) tokens))
        negative-count (count (filter #(contains? (:negative sentiment-lexicon) %) tokens))
        intensifier-count (count (filter #(contains? (:intensifiers sentiment-lexicon) %) tokens))
        negator-count (count (filter #(contains? (:negators sentiment-lexicon) %) tokens))
        total-sentiment-words (+ positive-count negative-count)
        
        ;; Calculate sentiment score
        raw-score (if (zero? total-sentiment-words)
                    0.0
                    (/ (- positive-count negative-count) (double total-sentiment-words)))
        
        ;; Adjust for negators (flip sentiment)
        adjusted-score (if (odd? negator-count)
                         (- raw-score)
                         raw-score)
        
        ;; Intensify if intensifiers present
        final-score (* adjusted-score (+ 1.0 (* 0.1 intensifier-count)))]
    
    {:score (max -1.0 (min 1.0 final-score))
     :label (cond
              (> final-score 0.3) :positive
              (< final-score -0.3) :negative
              :else :neutral)
     :positive-words positive-count
     :negative-words negative-count
     :intensifiers intensifier-count
     :negators negator-count
     :confidence (if (zero? total-sentiment-words)
                   0.0
                   (min 1.0 (/ total-sentiment-words 10.0)))}))

;; ============================================================================
;; Keyword Extraction
;; ============================================================================

(def ^:private stopwords
  "Common English stopwords."
  #{"a" "an" "the" "and" "or" "but" "in" "on" "at" "to" "for" "of" "with" "by" "from" "as" "is" "was" "are" "were" "been" "be" "have" "has" "had" "do" "does" "did" "will" "would" "could" "should" "may" "might" "must" "shall" "can" "need" "dare" "ought" "used" "this" "that" "these" "those" "i" "you" "he" "she" "it" "we" "they" "what" "which" "who" "whom" "whose" "where" "when" "why" "how" "all" "each" "every" "both" "few" "more" "most" "other" "some" "such" "no" "nor" "not" "only" "own" "same" "so" "than" "too" "very" "just" "also" "now" "here" "there" "then" "once" "if" "because" "until" "while" "about" "into" "through" "during" "before" "after" "above" "below" "between" "under" "again" "further" "any" "being" "having" "doing" "their" "its" "his" "her" "my" "your" "our"})

(defn- calculate-tf
  "Calculate term frequency."
  [tokens]
  (let [total (count tokens)
        freqs (frequencies tokens)]
    (into {} (map (fn [[term freq]]
                    [term (/ freq (double total))])
                  freqs))))

(defn extract-keywords
  "Extract keywords from text using TF-based scoring."
  [text & {:keys [top-n min-length] :or {top-n 10 min-length 3}}]
  (let [tokens (word-tokenize text)
        ;; Filter stopwords and short words
        filtered (filter #(and (not (contains? stopwords %))
                               (>= (count %) min-length))
                         tokens)
        tf-scores (calculate-tf filtered)
        ;; Sort by score
        sorted (sort-by val > tf-scores)]
    (mapv (fn [[term score]]
            {:term term
             :score score
             :frequency (get (frequencies filtered) term)})
          (take top-n sorted))))

;; ============================================================================
;; Text Summarization
;; ============================================================================

(defn- sentence-score
  "Score a sentence for summarization."
  [sentence keywords]
  (let [tokens (set (word-tokenize sentence))
        keyword-set (set (map :term keywords))
        overlap (count (clojure.set/intersection tokens keyword-set))
        length-penalty (if (< (count tokens) 5) 0.5 1.0)]
    (* overlap length-penalty)))

(defn summarize
  "Generate extractive summary of text."
  [text & {:keys [num-sentences ratio] :or {num-sentences 3 ratio nil}}]
  (let [sentences (sentence-tokenize text)
        n (if ratio
            (max 1 (int (* ratio (count sentences))))
            (min num-sentences (count sentences)))
        keywords (extract-keywords text :top-n 20)
        scored (map (fn [s]
                      {:sentence s
                       :score (sentence-score s keywords)})
                    sentences)
        ;; Sort by score but maintain some original order
        top-sentences (->> scored
                           (sort-by :score >)
                           (take n)
                           (sort-by #(.indexOf sentences (:sentence %))))]
    {:summary (str/join " " (map :sentence top-sentences))
     :sentences (mapv :sentence top-sentences)
     :compression-ratio (/ n (double (count sentences)))
     :original-sentences (count sentences)}))

;; ============================================================================
;; Language Detection
;; ============================================================================

(def ^:private language-indicators
  "Common words for language detection."
  {:english #{"the" "is" "are" "was" "were" "have" "has" "had" "will" "would" "could" "should" "and" "but" "or" "for" "with" "this" "that"}
   :spanish #{"el" "la" "los" "las" "es" "son" "fue" "fueron" "tiene" "tienen" "y" "pero" "o" "para" "con" "este" "esta"}
   :french #{"le" "la" "les" "est" "sont" "était" "étaient" "a" "ont" "et" "mais" "ou" "pour" "avec" "ce" "cette"}
   :german #{"der" "die" "das" "ist" "sind" "war" "waren" "hat" "haben" "und" "aber" "oder" "für" "mit" "dieser" "diese"}})

(defn detect-language
  "Detect the language of text."
  [text]
  (let [tokens (set (word-tokenize text))
        scores (for [[lang indicators] language-indicators]
                 [lang (count (clojure.set/intersection tokens indicators))])
        sorted (sort-by second > scores)
        [best-lang best-score] (first sorted)
        total-matches (reduce + (map second scores))]
    {:language best-lang
     :confidence (if (zero? total-matches)
                   0.0
                   (/ best-score (double total-matches)))
     :scores (into {} scores)}))

;; ============================================================================
;; Pipeline Processing
;; ============================================================================

(defn create-pipeline!
  "Create an NLP processing pipeline."
  [pipeline-id config]
  (let [pipeline {:id pipeline-id
                  :name (get config :name (name pipeline-id))
                  :steps (get config :steps [:tokenize :pos-tag :entities :sentiment :keywords])
                  :options (get config :options {})
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:pipelines pipeline-id] pipeline)
    (logging/log :info "Created NLP pipeline" {:pipeline-id pipeline-id})
    pipeline-id))

(defn process-text
  "Process text through an NLP pipeline."
  [pipeline-id text]
  (when (flags/enabled? :nlp-processor)
    (let [pipeline (get-in @state [:pipelines pipeline-id])
          steps (or (:steps pipeline) [:tokenize])
          options (or (:options pipeline) {})
          start-time (System/currentTimeMillis)
          
          result (reduce
                  (fn [acc step]
                    (assoc acc step
                           (case step
                             :tokenize (tokenize text :level :both)
                             :pos-tag (pos-tag text)
                             :entities (extract-entities text)
                             :sentiment (analyze-sentiment text)
                             :keywords (extract-keywords text
                                                         :top-n (get options :keywords-top-n 10))
                             :summarize (summarize text
                                                   :num-sentences (get options :summary-sentences 3))
                             :language (detect-language text)
                             nil)))
                  {:text text
                   :pipeline-id pipeline-id}
                  steps)
          
          processing-time (- (System/currentTimeMillis) start-time)]
      
      (swap! state update-in [:stats :processed] inc)
      (metrics/histogram :nlp-processing-time {} processing-time)
      
      (assoc result
             :processing-time-ms processing-time
             :timestamp (System/currentTimeMillis)))))

(defn process-batch
  "Process multiple texts through a pipeline."
  [pipeline-id texts]
  (mapv #(process-text pipeline-id %) texts))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-processor-stats
  "Get NLP processor statistics."
  []
  (let [stats (:stats @state)]
    {:total-pipelines (count (:pipelines @state))
     :total-processed (:processed stats)
     :cache-hits (:cache-hits stats)
     :cache-size (count (:cache @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-nlp-processor!
  "Initialize the NLP processor."
  []
  (when-not (:initialized? @state)
    ;; Create default pipeline
    (create-pipeline! :default
                      {:name "Default NLP Pipeline"
                       :steps [:tokenize :pos-tag :entities :sentiment :keywords]
                       :options {:keywords-top-n 10}})
    
    ;; Create mental model analysis pipeline
    (create-pipeline! :mental-model-analysis
                      {:name "Mental Model Analysis Pipeline"
                       :steps [:tokenize :sentiment :keywords :entities :summarize]
                       :options {:keywords-top-n 20
                                 :summary-sentences 5}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "NLP processor initialized")
    (events/emit! :nlp-processor-initialized {})
    true))
