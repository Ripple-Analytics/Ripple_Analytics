;; Data Processing Engine in Hy (Lisp)
;; Comprehensive data processing functions
;; 
;; This module provides:
;; - Text chunking and processing
;; - Pattern matching
;; - Document analysis
;; - Data transformation
;; - Embedding preparation

(import [typing [Dict List Any Optional Tuple]])
(import [datetime [datetime]])
(import [functools [reduce]])
(import [collections [Counter defaultdict]])
(import [re])
(import [hashlib [md5]])
(import os)

;; ============================================
;; Text Processing
;; ============================================

(defn chunk-text [text chunk-size overlap]
  "Split text into overlapping chunks for processing.
   
   chunk-size: target size of each chunk in characters
   overlap: number of characters to overlap between chunks"
  (let [text-len (len text)
        chunks []
        start 0]
    (while (< start text-len)
      (let [end (min (+ start chunk-size) text-len)
            chunk (cut text start end)]
        (.append chunks {"text" chunk
                        "start" start
                        "end" end
                        "index" (len chunks)})
        (setv start (- end overlap))))
    chunks))

(defn chunk-by-sentences [text max-chunk-size]
  "Split text into chunks at sentence boundaries."
  (let [sentences (split-sentences text)
        chunks []
        current-chunk ""
        current-start 0]
    (for [sentence sentences]
      (if (> (+ (len current-chunk) (len sentence)) max-chunk-size)
          (do
            (when current-chunk
              (.append chunks {"text" (.strip current-chunk)
                              "start" current-start
                              "index" (len chunks)}))
            (setv current-chunk sentence)
            (setv current-start (- (len text) (len sentence))))
          (setv current-chunk (+ current-chunk " " sentence))))
    (when current-chunk
      (.append chunks {"text" (.strip current-chunk)
                      "start" current-start
                      "index" (len chunks)}))
    chunks))

(defn split-sentences [text]
  "Split text into sentences."
  (let [pattern r"(?<=[.!?])\s+(?=[A-Z])"
        sentences (re.split pattern text)]
    (lfor s sentences :if (.strip s) (.strip s))))

(defn chunk-by-paragraphs [text]
  "Split text into paragraph chunks."
  (let [paragraphs (re.split r"\n\n+" text)
        chunks []]
    (for [[i para] (enumerate paragraphs)]
      (let [stripped (.strip para)]
        (when stripped
          (.append chunks {"text" stripped
                          "index" i
                          "type" "paragraph"}))))
    chunks))

;; ============================================
;; Pattern Matching
;; ============================================

(defn find-patterns [text patterns]
  "Find all occurrences of patterns in text.
   
   patterns: dict of {pattern_name: regex_pattern}"
  (let [results {}]
    (for [[name pattern] (.items patterns)]
      (let [matches (re.findall pattern text :flags re.IGNORECASE)]
        (when matches
          (setv (get results name) {"matches" matches
                                    "count" (len matches)}))))
    results))

(defn extract-entities [text]
  "Extract named entities from text (simplified)."
  (let [patterns
        {"emails" r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"
         "urls" r"https?://[^\s]+"
         "numbers" r"\b\d+(?:\.\d+)?\b"
         "dates" r"\b\d{1,2}[/-]\d{1,2}[/-]\d{2,4}\b"
         "percentages" r"\b\d+(?:\.\d+)?%"
         "money" r"\$\d+(?:,\d{3})*(?:\.\d{2})?"
         "capitalized" r"\b[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*\b"}]
    (find-patterns text patterns)))

(defn match-mental-model-patterns [text]
  "Find patterns that suggest mental model applicability."
  (let [model-patterns
        {"circle_of_competence" 
         r"(?:expertise|competence|know(?:ledge)?|understand|familiar)"
         
         "margin_of_safety"
         r"(?:buffer|margin|safety|cushion|reserve|backup)"
         
         "second_order"
         r"(?:consequence|effect|result|impact|then what|next)"
         
         "incentives"
         r"(?:incentive|motivat|reward|punish|benefit|gain)"
         
         "inversion"
         r"(?:avoid|prevent|fail|wrong|mistake|error)"
         
         "opportunity_cost"
         r"(?:alternative|instead|trade-?off|give up|sacrifice)"
         
         "sunk_cost"
         r"(?:already|invested|spent|committed|can't stop)"
         
         "confirmation_bias"
         r"(?:confirm|prove|evidence|support|validate)"
         
         "social_proof"
         r"(?:everyone|popular|trending|most people|they all)"}]
    (find-patterns text model-patterns)))

;; ============================================
;; Document Analysis
;; ============================================

(defn analyze-document [text]
  "Comprehensive document analysis."
  (let [word-count (len (.split text))
        char-count (len text)
        sentences (split-sentences text)
        paragraphs (chunk-by-paragraphs text)
        entities (extract-entities text)
        model-patterns (match-mental-model-patterns text)]
    {"statistics" {"word_count" word-count
                   "char_count" char-count
                   "sentence_count" (len sentences)
                   "paragraph_count" (len paragraphs)
                   "avg_sentence_length" (if sentences
                                           (/ word-count (len sentences))
                                           0)}
     "entities" entities
     "mental_model_signals" model-patterns
     "readability" (calculate-readability text)
     "timestamp" (str (datetime.now))}))

(defn calculate-readability [text]
  "Calculate readability metrics."
  (let [words (.split text)
        sentences (split-sentences text)
        word-count (len words)
        sentence-count (max 1 (len sentences))
        syllable-count (sum (lfor w words (count-syllables w)))
        avg-words-per-sentence (/ word-count sentence-count)
        avg-syllables-per-word (/ syllable-count (max 1 word-count))
        flesch-score (- 206.835
                       (* 1.015 avg-words-per-sentence)
                       (* 84.6 avg-syllables-per-word))]
    {"flesch_reading_ease" (max 0 (min 100 flesch-score))
     "avg_words_per_sentence" avg-words-per-sentence
     "avg_syllables_per_word" avg-syllables-per-word
     "interpretation" (interpret-flesch flesch-score)}))

(defn count-syllables [word]
  "Estimate syllable count in a word."
  (let [word-lower (str.lower word)
        vowels "aeiouy"
        count 0
        prev-vowel False]
    (for [char word-lower]
      (let [is-vowel (in char vowels)]
        (when (and is-vowel (not prev-vowel))
          (setv count (+ count 1)))
        (setv prev-vowel is-vowel)))
    (max 1 count)))

(defn interpret-flesch [score]
  "Interpret Flesch reading ease score."
  (cond
    [(>= score 90) "Very easy - 5th grade"]
    [(>= score 80) "Easy - 6th grade"]
    [(>= score 70) "Fairly easy - 7th grade"]
    [(>= score 60) "Standard - 8th-9th grade"]
    [(>= score 50) "Fairly difficult - 10th-12th grade"]
    [(>= score 30) "Difficult - College"]
    [True "Very difficult - College graduate"]))

;; ============================================
;; Data Transformation
;; ============================================

(defn normalize-text [text]
  "Normalize text for processing."
  (-> text
      (str.lower)
      (re.sub r"\s+" " ")
      (str.strip)))

(defn tokenize [text]
  "Tokenize text into words."
  (let [cleaned (re.sub r"[^\w\s]" "" text)]
    (.split (str.lower cleaned))))

(defn remove-stopwords [tokens stopwords]
  "Remove stopwords from token list."
  (lfor t tokens :if (not (in t stopwords)) t))

(defn stem-word [word]
  "Simple suffix-stripping stemmer."
  (let [suffixes ["ing" "ed" "ly" "es" "s" "ment" "ness" "tion" "ation"]]
    (for [suffix suffixes]
      (when (and (.endswith word suffix) (> (len word) (+ (len suffix) 2)))
        (return (cut word 0 (- (len suffix))))))
    word))

(defn bag-of-words [text]
  "Create bag of words representation."
  (let [tokens (tokenize text)]
    (dict (Counter tokens))))

(defn tfidf-simple [documents]
  "Calculate simple TF-IDF scores.
   
   documents: list of text strings"
  (let [n-docs (len documents)
        doc-tokens (lfor doc documents (set (tokenize doc)))
        all-terms (reduce (fn [a b] (.union a b)) doc-tokens (set))
        idf {}
        tfidf-scores []]
    
    (for [term all-terms]
      (let [doc-freq (sum (lfor dt doc-tokens (if (in term dt) 1 0)))]
        (setv (get idf term) (math.log (/ n-docs (max 1 doc-freq))))))
    
    (for [doc documents]
      (let [bow (bag-of-words doc)
            total-terms (sum (.values bow))
            doc-tfidf {}]
        (for [[term count] (.items bow)]
          (let [tf (/ count total-terms)
                term-idf (get idf term 0)]
            (setv (get doc-tfidf term) (* tf term-idf))))
        (.append tfidf-scores doc-tfidf)))
    
    {"documents" (len documents)
     "vocabulary_size" (len all-terms)
     "idf" idf
     "tfidf" tfidf-scores}))

(import math)

;; ============================================
;; Embedding Preparation
;; ============================================

(defn prepare-for-embedding [text #** kwargs]
  "Prepare text for embedding generation."
  (let [max-length (get kwargs "max_length" 512)
        cleaned (normalize-text text)
        truncated (if (> (len cleaned) max-length)
                    (cut cleaned 0 max-length)
                    cleaned)]
    {"original_length" (len text)
     "processed_length" (len truncated)
     "text" truncated
     "truncated" (> (len text) max-length)}))

(defn batch-prepare-embeddings [texts #** kwargs]
  "Prepare multiple texts for batch embedding."
  (lfor text texts (prepare-for-embedding text #** kwargs)))

(defn create-document-hash [text]
  "Create hash for document deduplication."
  (let [normalized (normalize-text text)
        hash-obj (md5 (.encode normalized "utf-8"))]
    (.hexdigest hash-obj)))

;; ============================================
;; File Processing
;; ============================================

(defn process-text-file [filepath]
  "Process a text file and return analysis."
  (try
    (with [f (open filepath "r" :encoding "utf-8")]
      (let [content (.read f)
            analysis (analyze-document content)
            chunks (chunk-text content 1000 100)]
        {"filepath" filepath
         "analysis" analysis
         "chunks" chunks
         "hash" (create-document-hash content)
         "status" "success"}))
    (except [e Exception]
      {"filepath" filepath
       "status" "error"
       "error" (str e)})))

(defn process-directory [dirpath #** kwargs]
  "Process all text files in a directory."
  (let [extensions (get kwargs "extensions" [".txt" ".md"])
        results []
        processed 0
        errors 0]
    (for [root dirs files] (os.walk dirpath)
      (for [filename files]
        (when (any (lfor ext extensions (.endswith filename ext)))
          (let [filepath (os.path.join root filename)
                result (process-text-file filepath)]
            (.append results result)
            (if (= (get result "status") "success")
                (setv processed (+ processed 1))
                (setv errors (+ errors 1)))))))
    {"directory" dirpath
     "files_processed" processed
     "errors" errors
     "results" results}))

;; ============================================
;; Mental Model Document Classification
;; ============================================

(defn classify-by-mental-models [text models]
  "Classify text by applicable mental models."
  (let [patterns (match-mental-model-patterns text)
        classifications []]
    (for [[model-key matches] (.items patterns)]
      (when (> (get matches "count" 0) 0)
        (.append classifications
                 {"model" model-key
                  "confidence" (min 1.0 (* 0.2 (get matches "count")))
                  "evidence" (get matches "matches")})))
    (sorted classifications :key (fn [x] (get x "confidence")) :reverse True)))

(defn extract-insights [text models]
  "Extract insights from text using mental models."
  (let [classifications (classify-by-mental-models text models)
        top-models (cut classifications 0 5)
        insights []]
    (for [c top-models]
      (.append insights
               {"model" (get c "model")
                "insight" (+ "Text shows signals of " (get c "model") " thinking")
                "confidence" (get c "confidence")}))
    {"text_preview" (cut text 0 200)
     "top_models" top-models
     "insights" insights
     "timestamp" (str (datetime.now))}))

;; ============================================
;; Export Functions
;; ============================================

(defn process-and-analyze [text #** kwargs]
  "Complete text processing and analysis pipeline."
  (let [chunk-size (get kwargs "chunk_size" 1000)
        overlap (get kwargs "overlap" 100)]
    {"analysis" (analyze-document text)
     "chunks" (chunk-text text chunk-size overlap)
     "entities" (extract-entities text)
     "model_signals" (match-mental-model-patterns text)
     "embedding_prep" (prepare-for-embedding text)
     "hash" (create-document-hash text)
     "timestamp" (str (datetime.now))}))
