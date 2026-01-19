(ns mental-models.data-processing
  "Data Processing Engine - Electric Clojure
   
   Text processing, chunking, and document analysis.
   Runs on both client and server for reactive processing."
  (:require [clojure.string :as str]))

;; ============================================
;; Text Chunking
;; ============================================

(defn chunk-text
  "Split text into overlapping chunks for processing."
  [text chunk-size overlap]
  (if (or (nil? text) (empty? text))
    []
    (let [text-len (count text)]
      (if (<= text-len chunk-size)
        [{:text text :start 0 :end text-len :index 0}]
        (loop [chunks []
               start 0
               idx 0]
          (if (>= start text-len)
            chunks
            (let [end (min (+ start chunk-size) text-len)
                  chunk-text (subs text start end)
                  chunk {:text chunk-text :start start :end end :index idx}
                  next-start (- end overlap)]
              (recur (conj chunks chunk)
                     (if (>= end text-len) text-len next-start)
                     (inc idx)))))))))

;; ============================================
;; Document Analysis
;; ============================================

(defn count-words [text]
  (if (or (nil? text) (empty? text))
    0
    (count (str/split (str/trim text) #"\s+"))))

(defn count-sentences [text]
  (if (or (nil? text) (empty? text))
    0
    (count (re-seq #"[.!?]+" text))))

(defn count-paragraphs [text]
  (if (or (nil? text) (empty? text))
    0
    (count (filter #(not (empty? (str/trim %)))
                   (str/split text #"\n\n+")))))

(defn analyze-document [text]
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

(defn count-syllables [word]
  (let [word-lower (str/lower-case word)
        vowels #{\a \e \i \o \u}
        chars (seq word-lower)]
    (loop [syllable-count 0
           prev-vowel false
           remaining chars]
      (if (empty? remaining)
        (max 1 syllable-count)
        (let [c (first remaining)
              is-vowel (contains? vowels c)
              new-count (if (and is-vowel (not prev-vowel))
                          (inc syllable-count)
                          syllable-count)]
          (recur new-count is-vowel (rest remaining)))))))

(defn flesch-reading-ease [text]
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

(defn extract-entities [text]
  (let [entities (into {}
                       (map (fn [[entity-type pattern]]
                              [entity-type (vec (re-seq pattern text))])
                            entity-patterns))]
    {:entities entities
     :counts (into {} (map (fn [[k v]] [k (count v)]) entities))
     :total (reduce + (map count (vals entities)))}))

;; ============================================
;; Mental Model Pattern Matching
;; ============================================

(def mental-model-patterns
  {:circle-of-competence ["competence" "expertise" "know" "understand" "capable"]
   :margin-of-safety ["buffer" "safety" "margin" "cushion" "conservative"]
   :second-order-thinking ["consequence" "effect" "result" "then what" "downstream"]
   :inversion ["avoid" "prevent" "fail" "wrong" "mistake"]
   :incentives ["incentive" "motivate" "reward" "punish" "benefit"]
   :social-proof ["everyone" "popular" "trend" "follow" "crowd"]
   :commitment-consistency ["commit" "consistent" "promise" "stick"]
   :availability-heuristic ["recent" "remember" "vivid" "news" "example"]
   :loss-aversion ["lose" "loss" "risk" "protect" "preserve"]
   :feedback-loops ["feedback" "loop" "cycle" "reinforce" "compound"]
   :network-effects ["network" "platform" "users" "viral" "adoption"]
   :compound-interest ["compound" "exponential" "growth" "accumulate"]})

(defn match-mental-model-patterns [text]
  (let [text-lower (str/lower-case text)]
    (into {}
          (map (fn [[model patterns]]
                 (let [matches (filter #(str/includes? text-lower %) patterns)]
                   [model {:count (count matches) :matches (vec matches)}]))
               mental-model-patterns))))

(defn classify-by-mental-models [text]
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

(defn normalize-text [text]
  (-> text
      str/lower-case
      (str/replace #"\s+" " ")
      str/trim))

(defn tokenize [text]
  (-> text
      (str/replace #"[^\w\s]" "")
      str/lower-case
      (str/split #"\s+")
      (->> (filter #(not (empty? %))))))

(def stopwords
  #{"the" "a" "an" "and" "or" "but" "in" "on" "at" "to" "for"
    "of" "with" "by" "from" "as" "is" "was" "are" "were" "been"
    "be" "have" "has" "had" "do" "does" "did" "will" "would"
    "could" "should" "may" "might" "must" "shall" "can" "need"
    "this" "that" "these" "those" "i" "you" "he" "she" "it"
    "we" "they" "what" "which" "who" "whom" "whose" "where"
    "when" "why" "how" "all" "each" "every" "both" "few" "more"
    "most" "other" "some" "such" "no" "nor" "not" "only" "own"
    "same" "so" "than" "too" "very" "just" "also" "now"})

(defn remove-stopwords [tokens]
  (filter #(not (contains? stopwords %)) tokens))

(defn bag-of-words [text]
  (frequencies (tokenize text)))

;; ============================================
;; Embedding Preparation
;; ============================================

(defn prepare-for-embedding [text & {:keys [max-length] :or {max-length 512}}]
  (let [cleaned (normalize-text text)
        truncated (if (> (count cleaned) max-length)
                    (subs cleaned 0 max-length)
                    cleaned)]
    {:original-length (count text)
     :processed-length (count truncated)
     :text truncated
     :truncated (> (count text) max-length)}))

;; ============================================
;; Complete Processing Pipeline
;; ============================================

(defn process-and-analyze [text & {:keys [chunk-size overlap] :or {chunk-size 1000 overlap 100}}]
  {:analysis (analyze-document text)
   :readability (flesch-reading-ease text)
   :chunks (chunk-text text chunk-size overlap)
   :entities (extract-entities text)
   :model-signals (match-mental-model-patterns text)
   :embedding-prep (prepare-for-embedding text)})
