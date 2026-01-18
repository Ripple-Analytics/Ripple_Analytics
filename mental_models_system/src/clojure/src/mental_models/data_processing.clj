(ns mental-models.data-processing
  "Data Processing Engine in Clojure
   Text processing, chunking, and document analysis
   
   This module provides:
   - Text chunking for LLM processing
   - Document analysis
   - Entity extraction
   - Pattern matching
   - Readability analysis"
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; ============================================
;; Text Chunking
;; ============================================

(defn chunk-text
  "Split text into overlapping chunks for processing.
   
   chunk-size: maximum characters per chunk
   overlap: characters to overlap between chunks
   
   Overlapping ensures context is preserved across chunk boundaries."
  [text chunk-size overlap]
  (if (or (nil? text) (empty? text))
    []
    (let [text-len (count text)]
      (if (<= text-len chunk-size)
        [{:text text
          :start 0
          :end text-len
          :index 0}]
        (loop [chunks []
               start 0
               idx 0]
          (if (>= start text-len)
            chunks
            (let [end (min (+ start chunk-size) text-len)
                  chunk-text (subs text start end)
                  chunk {:text chunk-text
                         :start start
                         :end end
                         :index idx}
                  next-start (- end overlap)]
              (recur (conj chunks chunk)
                     (if (>= end text-len) text-len next-start)
                     (inc idx)))))))))

(defn smart-chunk-text
  "Chunk text at sentence boundaries when possible."
  [text chunk-size overlap]
  (let [sentences (str/split text #"(?<=[.!?])\s+")
        chunks (atom [])
        current-chunk (atom "")
        current-start (atom 0)
        idx (atom 0)]
    (doseq [sentence sentences]
      (if (> (+ (count @current-chunk) (count sentence) 1) chunk-size)
        (do
          (when (not (empty? @current-chunk))
            (swap! chunks conj {:text @current-chunk
                                :start @current-start
                                :end (+ @current-start (count @current-chunk))
                                :index @idx})
            (swap! idx inc))
          (reset! current-start (- (+ @current-start (count @current-chunk)) overlap))
          (reset! current-chunk sentence))
        (reset! current-chunk (if (empty? @current-chunk)
                                sentence
                                (str @current-chunk " " sentence)))))
    (when (not (empty? @current-chunk))
      (swap! chunks conj {:text @current-chunk
                          :start @current-start
                          :end (+ @current-start (count @current-chunk))
                          :index @idx}))
    @chunks))

;; ============================================
;; Document Analysis
;; ============================================

(defn count-words
  "Count words in text."
  [text]
  (if (or (nil? text) (empty? text))
    0
    (count (str/split (str/trim text) #"\s+"))))

(defn count-sentences
  "Count sentences in text."
  [text]
  (if (or (nil? text) (empty? text))
    0
    (count (re-seq #"[.!?]+" text))))

(defn count-paragraphs
  "Count paragraphs in text."
  [text]
  (if (or (nil? text) (empty? text))
    0
    (count (filter #(not (empty? (str/trim %)))
                   (str/split text #"\n\n+")))))

(defn analyze-document
  "Analyze document structure and content."
  [text]
  (if (or (nil? text) (empty? text))
    {:error "No text provided"}
    (let [words (count-words text)
          sentences (count-sentences text)
          paragraphs (count-paragraphs text)
          chars (count text)
          avg-word-length (if (> words 0)
                            (/ (count (str/replace text #"\s+" "")) words)
                            0)
          avg-sentence-length (if (> sentences 0)
                                (/ words sentences)
                                0)]
      {:character-count chars
       :word-count words
       :sentence-count sentences
       :paragraph-count paragraphs
       :average-word-length (double avg-word-length)
       :average-sentence-length (double avg-sentence-length)
       :estimated-reading-time-minutes (/ words 200.0)
       :complexity-estimate (cond
                              (> avg-sentence-length 25) "complex"
                              (> avg-sentence-length 15) "moderate"
                              :else "simple")})))

;; ============================================
;; Readability Analysis
;; ============================================

(defn count-syllables
  "Estimate syllable count in a word."
  [word]
  (let [word-lower (str/lower-case word)
        vowels #{\a \e \i \o \u}
        chars (seq word-lower)]
    (loop [count 0
           prev-vowel false
           remaining chars]
      (if (empty? remaining)
        (max 1 count)
        (let [c (first remaining)
              is-vowel (contains? vowels c)
              new-count (if (and is-vowel (not prev-vowel))
                          (inc count)
                          count)]
          (recur new-count is-vowel (rest remaining)))))))

(defn flesch-reading-ease
  "Calculate Flesch Reading Ease score.
   
   Higher scores = easier to read
   90-100: Very easy (5th grade)
   80-90: Easy (6th grade)
   70-80: Fairly easy (7th grade)
   60-70: Standard (8th-9th grade)
   50-60: Fairly difficult (10th-12th grade)
   30-50: Difficult (College)
   0-30: Very difficult (College graduate)"
  [text]
  (let [words (str/split (str/trim text) #"\s+")
        word-count (count words)
        sentence-count (max 1 (count-sentences text))
        syllable-count (reduce + (map count-syllables words))
        score (- 206.835
                 (* 1.015 (/ word-count sentence-count))
                 (* 84.6 (/ syllable-count word-count)))]
    {:score (max 0 (min 100 score))
     :interpretation (cond
                       (>= score 90) "Very easy - 5th grade"
                       (>= score 80) "Easy - 6th grade"
                       (>= score 70) "Fairly easy - 7th grade"
                       (>= score 60) "Standard - 8th-9th grade"
                       (>= score 50) "Fairly difficult - 10th-12th grade"
                       (>= score 30) "Difficult - College"
                       :else "Very difficult - College graduate")
     :word-count word-count
     :sentence-count sentence-count
     :syllable-count syllable-count}))

;; ============================================
;; Entity Extraction
;; ============================================

(def entity-patterns
  {:email #"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
   :url #"https?://[^\s]+"
   :phone #"\+?[\d\s\-\(\)]{10,}"
   :date #"\d{1,2}[/\-]\d{1,2}[/\-]\d{2,4}"
   :money #"\$[\d,]+\.?\d*"
   :percentage #"\d+\.?\d*%"
   :number #"\b\d+\.?\d*\b"})

(defn extract-entities
  "Extract named entities from text."
  [text]
  (let [entities (into {}
                       (map (fn [[entity-type pattern]]
                              [entity-type (vec (re-seq pattern text))])
                            entity-patterns))]
    {:entities entities
     :counts (into {}
                   (map (fn [[k v]] [k (count v)])
                        entities))
     :total (reduce + (map count (vals entities)))}))

;; ============================================
;; Mental Model Pattern Matching
;; ============================================

(def mental-model-patterns
  {:circle-of-competence
   ["competence" "expertise" "know" "understand" "capable" "skill"]
   
   :margin-of-safety
   ["buffer" "safety" "margin" "cushion" "conservative" "risk"]
   
   :second-order-thinking
   ["consequence" "effect" "result" "then what" "downstream" "ripple"]
   
   :inversion
   ["avoid" "prevent" "fail" "wrong" "mistake" "not do"]
   
   :incentives
   ["incentive" "motivate" "reward" "punish" "benefit" "interest"]
   
   :social-proof
   ["everyone" "popular" "trend" "follow" "crowd" "consensus"]
   
   :commitment-consistency
   ["commit" "consistent" "promise" "stick" "follow through"]
   
   :availability-heuristic
   ["recent" "remember" "vivid" "news" "example" "recall"]
   
   :loss-aversion
   ["lose" "loss" "risk" "protect" "preserve" "downside"]
   
   :feedback-loops
   ["feedback" "loop" "cycle" "reinforce" "compound" "snowball"]
   
   :network-effects
   ["network" "platform" "users" "viral" "adoption" "critical mass"]
   
   :compound-interest
   ["compound" "exponential" "growth" "accumulate" "time"]})

(defn match-mental-model-patterns
  "Find mental model patterns in text."
  [text]
  (let [text-lower (str/lower-case text)]
    (into {}
          (map (fn [[model patterns]]
                 (let [matches (filter #(str/includes? text-lower %) patterns)]
                   [model {:count (count matches)
                           :matches (vec matches)}]))
               mental-model-patterns))))

(defn classify-by-mental-models
  "Classify text by applicable mental models."
  [text]
  (let [patterns (match-mental-model-patterns text)
        classifications (->> patterns
                             (filter (fn [[_ v]] (> (:count v) 0)))
                             (map (fn [[model matches]]
                                    {:model (name model)
                                     :confidence (min 1.0 (* 0.2 (:count matches)))
                                     :evidence (:matches matches)}))
                             (sort-by :confidence >))]
    classifications))

;; ============================================
;; Text Transformation
;; ============================================

(defn normalize-text
  "Normalize text for processing."
  [text]
  (-> text
      str/lower-case
      (str/replace #"\s+" " ")
      str/trim))

(defn tokenize
  "Tokenize text into words."
  [text]
  (-> text
      (str/replace #"[^\w\s]" "")
      str/lower-case
      (str/split #"\s+")
      (->> (filter #(not (empty? %))))))

(defn remove-stopwords
  "Remove common stopwords from token list."
  [tokens]
  (let [stopwords #{"the" "a" "an" "and" "or" "but" "in" "on" "at" "to" "for"
                    "of" "with" "by" "from" "as" "is" "was" "are" "were" "been"
                    "be" "have" "has" "had" "do" "does" "did" "will" "would"
                    "could" "should" "may" "might" "must" "shall" "can" "need"
                    "this" "that" "these" "those" "i" "you" "he" "she" "it"
                    "we" "they" "what" "which" "who" "whom" "whose" "where"
                    "when" "why" "how" "all" "each" "every" "both" "few" "more"
                    "most" "other" "some" "such" "no" "nor" "not" "only" "own"
                    "same" "so" "than" "too" "very" "just" "also" "now"}]
    (filter #(not (contains? stopwords %)) tokens)))

(defn bag-of-words
  "Create bag of words representation."
  [text]
  (frequencies (tokenize text)))

;; ============================================
;; Document Hashing
;; ============================================

(defn create-document-hash
  "Create hash for document deduplication."
  [text]
  (let [normalized (normalize-text text)
        md (java.security.MessageDigest/getInstance "MD5")
        bytes (.digest md (.getBytes normalized "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

;; ============================================
;; Embedding Preparation
;; ============================================

(defn prepare-for-embedding
  "Prepare text for embedding generation."
  [text & {:keys [max-length] :or {max-length 512}}]
  (let [cleaned (normalize-text text)
        truncated (if (> (count cleaned) max-length)
                    (subs cleaned 0 max-length)
                    cleaned)]
    {:original-length (count text)
     :processed-length (count truncated)
     :text truncated
     :truncated (> (count text) max-length)}))

(defn batch-prepare-embeddings
  "Prepare multiple texts for batch embedding."
  [texts & {:keys [max-length] :or {max-length 512}}]
  (mapv #(prepare-for-embedding % :max-length max-length) texts))

;; ============================================
;; Insight Extraction
;; ============================================

(defn extract-insights
  "Extract insights from text using mental models."
  [text]
  (let [classifications (classify-by-mental-models text)
        top-models (take 5 classifications)
        insights (map (fn [c]
                        {:model (:model c)
                         :insight (str "Text shows signals of " (:model c) " thinking")
                         :confidence (:confidence c)})
                      top-models)]
    {:text-preview (subs text 0 (min 200 (count text)))
     :top-models top-models
     :insights insights
     :timestamp (java.time.Instant/now)}))

;; ============================================
;; Complete Processing Pipeline
;; ============================================

(defn process-and-analyze
  "Complete text processing and analysis pipeline."
  [text & {:keys [chunk-size overlap] :or {chunk-size 1000 overlap 100}}]
  {:analysis (analyze-document text)
   :readability (flesch-reading-ease text)
   :chunks (chunk-text text chunk-size overlap)
   :entities (extract-entities text)
   :model-signals (match-mental-model-patterns text)
   :embedding-prep (prepare-for-embedding text)
   :hash (create-document-hash text)
   :timestamp (java.time.Instant/now)})
