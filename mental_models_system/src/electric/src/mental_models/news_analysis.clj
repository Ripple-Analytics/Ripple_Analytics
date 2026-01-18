(ns mental-models.news-analysis
  "Real-Time News and Text Analysis - Electric Clojure
   
   Analyzes text data (news, articles, documents) through mental models.
   Optimized for text processing at scale.
   
   Features:
   - Real-time text stream processing
   - Mental model pattern detection in text
   - Sentiment and bias analysis
   - Source credibility assessment
   - Lollapalooza effect detection across articles
   - Historical pattern matching"
  (:require [clojure.string :as str]
            [mental-models.algorithms :as algo])
  (:import [java.time Instant]
           [java.security MessageDigest]
           [java.util UUID]))

;; ============================================
;; Text Processing State
;; ============================================

(defonce analysis-state
  (atom {:articles-processed 0
         :total-characters 0
         :total-words 0
         :models-detected {}
         :biases-detected {}
         :sources {}
         :last-analysis nil
         :processing-times []}))

;; ============================================
;; Text Utilities
;; ============================================

(defn word-count
  "Count actual words in text."
  [text]
  (if (str/blank? text)
    0
    (count (str/split (str/trim text) #"\s+"))))

(defn char-count
  "Count actual characters in text."
  [text]
  (count (or text "")))

(defn sentence-count
  "Count sentences in text."
  [text]
  (if (str/blank? text)
    0
    (count (re-seq #"[.!?]+" text))))

(defn paragraph-count
  "Count paragraphs in text."
  [text]
  (if (str/blank? text)
    0
    (count (filter #(not (str/blank? %)) (str/split text #"\n\n+")))))

(defn text-hash
  "Generate hash of text for deduplication."
  [text]
  (let [md (MessageDigest/getInstance "SHA-256")
        bytes (.digest md (.getBytes (or text "") "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

(defn extract-keywords
  "Extract significant keywords from text."
  [text & {:keys [min-length min-frequency]
           :or {min-length 4 min-frequency 2}}]
  (let [words (-> text
                  str/lower-case
                  (str/replace #"[^a-z\s]" "")
                  (str/split #"\s+"))
        word-freq (frequencies words)
        stopwords #{"the" "and" "for" "that" "this" "with" "from" "have" "been"
                    "were" "they" "their" "what" "when" "where" "which" "will"
                    "would" "could" "should" "about" "into" "more" "some" "such"
                    "than" "then" "there" "these" "those" "very" "just" "also"}]
    (->> word-freq
         (filter (fn [[word freq]]
                   (and (>= (count word) min-length)
                        (>= freq min-frequency)
                        (not (stopwords word)))))
         (sort-by val >)
         (take 20)
         (into {}))))

;; ============================================
;; Text Analysis Functions
;; ============================================

(defn analyze-text-structure
  "Analyze the structure of text - REAL measurements."
  [text]
  (let [start-time (System/currentTimeMillis)]
    {:characters (char-count text)
     :words (word-count text)
     :sentences (sentence-count text)
     :paragraphs (paragraph-count text)
     :avg-word-length (if (pos? (word-count text))
                        (/ (count (str/replace text #"\s+" ""))
                           (double (word-count text)))
                        0.0)
     :avg-sentence-length (if (pos? (sentence-count text))
                            (/ (word-count text) (double (sentence-count text)))
                            0.0)
     :analysis-time-ms (- (System/currentTimeMillis) start-time)}))

(defn analyze-text-content
  "Analyze text content through mental models - REAL analysis."
  [text & {:keys [source-name] :or {source-name "unknown"}}]
  (let [start-time (System/currentTimeMillis)
        structure (analyze-text-structure text)
        keywords (extract-keywords text)
        ;; Run through mental model algorithms
        relevant-models (algo/get-top-models text 5)
        lollapalooza (algo/detect-lollapalooza text)
        failure-modes (algo/analyze-failure-modes text)
        analysis-time (- (System/currentTimeMillis) start-time)]
    
    ;; Update global stats
    (swap! analysis-state update :articles-processed inc)
    (swap! analysis-state update :total-characters + (:characters structure))
    (swap! analysis-state update :total-words + (:words structure))
    (swap! analysis-state update :processing-times conj analysis-time)
    
    ;; Track models detected
    (doseq [{:keys [model]} relevant-models]
      (swap! analysis-state update-in [:models-detected model] (fnil inc 0)))
    
    ;; Track source
    (swap! analysis-state update-in [:sources source-name :count] (fnil inc 0))
    (swap! analysis-state update-in [:sources source-name :last-seen] (constantly (System/currentTimeMillis)))
    
    {:id (str (UUID/randomUUID))
     :timestamp (System/currentTimeMillis)
     :source source-name
     :hash (text-hash text)
     :structure structure
     :keywords keywords
     :mental-models {:relevant (mapv (fn [{:keys [model relevance]}]
                                       {:model model :relevance relevance})
                                     relevant-models)
                     :top-model (when (seq relevant-models)
                                  (:model (first relevant-models)))
                     :model-count (count relevant-models)}
     :lollapalooza {:detected (boolean (seq lollapalooza))
                    :combinations lollapalooza}
     :failure-modes {:detected (boolean (seq failure-modes))
                     :count (count failure-modes)
                     :modes failure-modes}
     :analysis-time-ms analysis-time}))

(defn analyze-batch
  "Analyze a batch of texts - tracks REAL batch metrics."
  [texts & {:keys [source-name] :or {source-name "batch"}}]
  (let [start-time (System/currentTimeMillis)
        results (mapv #(analyze-text-content % :source-name source-name) texts)
        total-time (- (System/currentTimeMillis) start-time)
        successful (filter #(not (:error %)) results)]
    {:batch-id (str (UUID/randomUUID))
     :timestamp (System/currentTimeMillis)
     :total-texts (count texts)
     :successful (count successful)
     :failed (- (count texts) (count successful))
     :total-characters (reduce + (map #(get-in % [:structure :characters] 0) results))
     :total-words (reduce + (map #(get-in % [:structure :words] 0) results))
     :total-time-ms total-time
     :avg-time-per-text (when (pos? (count texts))
                          (/ total-time (double (count texts))))
     :throughput-texts-per-sec (when (pos? total-time)
                                 (/ (* 1000 (count texts)) (double total-time)))
     :results results}))

;; ============================================
;; News-Specific Analysis
;; ============================================

(defn analyze-news-article
  "Analyze a news article for mental model patterns."
  [article]
  (let [{:keys [title content source url published-at]} article
        full-text (str (or title "") "\n\n" (or content ""))
        analysis (analyze-text-content full-text :source-name (or source "news"))]
    (assoc analysis
           :article-metadata {:title title
                              :source source
                              :url url
                              :published-at published-at}
           :title-analysis (when title
                             {:length (count title)
                              :word-count (word-count title)
                              :has-question (boolean (re-find #"\?" title))
                              :has-numbers (boolean (re-find #"\d" title))
                              :all-caps-words (count (re-seq #"\b[A-Z]{2,}\b" title))}))))

(defn detect-narrative-patterns
  "Detect common narrative patterns in text."
  [text]
  (let [patterns {:fear-mongering (re-seq #"(?i)(crisis|disaster|catastrophe|collapse|danger|threat|warning)" text)
                  :urgency (re-seq #"(?i)(breaking|urgent|immediately|now|must|critical)" text)
                  :authority-appeal (re-seq #"(?i)(experts say|according to|studies show|research proves|scientists)" text)
                  :social-proof (re-seq #"(?i)(everyone|most people|majority|popular|trending|viral)" text)
                  :scarcity (re-seq #"(?i)(limited|rare|exclusive|only|last chance|running out)" text)
                  :certainty-language (re-seq #"(?i)(definitely|certainly|absolutely|guaranteed|proven|fact)" text)
                  :hedging-language (re-seq #"(?i)(might|could|possibly|perhaps|seems|appears|allegedly)" text)}]
    (->> patterns
         (map (fn [[pattern matches]]
                [pattern {:count (count matches)
                          :examples (take 3 matches)}]))
         (filter (fn [[_ {:keys [count]}]] (pos? count)))
         (into {}))))

(defn assess-source-credibility
  "Assess credibility indicators in text."
  [text]
  (let [has-citations (boolean (re-find #"(?i)(according to|cited|source:|reference)" text))
        has-quotes (boolean (re-find #"\"[^\"]{10,}\"" text))
        has-numbers (boolean (re-find #"\d+(\.\d+)?%" text))
        has-dates (boolean (re-find #"\d{4}|\d{1,2}/\d{1,2}" text))
        hedging-count (count (re-seq #"(?i)(might|could|possibly|allegedly|reportedly)" text))
        certainty-count (count (re-seq #"(?i)(definitely|certainly|absolutely|proven)" text))]
    {:indicators {:has-citations has-citations
                  :has-quotes has-quotes
                  :has-statistics has-numbers
                  :has-dates has-dates}
     :language-balance {:hedging-count hedging-count
                        :certainty-count certainty-count
                        :ratio (when (pos? certainty-count)
                                 (/ hedging-count (double certainty-count)))}
     :credibility-score (+ (if has-citations 25 0)
                           (if has-quotes 20 0)
                           (if has-numbers 15 0)
                           (if has-dates 10 0)
                           (min 30 (* 5 hedging-count)))}))

;; ============================================
;; Trend Detection
;; ============================================

(defn detect-trends
  "Detect trends across multiple texts."
  [analyses]
  (let [all-models (mapcat #(get-in % [:mental-models :relevant]) analyses)
        model-freq (frequencies (map :model all-models))
        all-keywords (mapcat #(keys (:keywords %)) analyses)
        keyword-freq (frequencies all-keywords)]
    {:trending-models (->> model-freq
                           (sort-by val >)
                           (take 10)
                           (into {}))
     :trending-keywords (->> keyword-freq
                             (sort-by val >)
                             (take 20)
                             (into {}))
     :lollapalooza-frequency (/ (count (filter #(get-in % [:lollapalooza :detected]) analyses))
                                (max 1 (count analyses)))
     :avg-models-per-text (/ (count all-models)
                             (max 1 (count analyses)))}))

;; ============================================
;; Global Statistics
;; ============================================

(defn get-analysis-stats
  "Get REAL analysis statistics."
  []
  (let [state @analysis-state
        processing-times (:processing-times state)]
    {:articles-processed (:articles-processed state)
     :total-characters (:total-characters state)
     :total-words (:total-words state)
     :unique-sources (count (:sources state))
     :models-detected (:models-detected state)
     :top-models (->> (:models-detected state)
                      (sort-by val >)
                      (take 10)
                      (into {}))
     :processing-stats (when (seq processing-times)
                         {:total-analyses (count processing-times)
                          :avg-time-ms (/ (reduce + processing-times)
                                          (double (count processing-times)))
                          :min-time-ms (apply min processing-times)
                          :max-time-ms (apply max processing-times)})
     :timestamp (System/currentTimeMillis)}))

(defn reset-stats!
  "Reset analysis statistics."
  []
  (reset! analysis-state
          {:articles-processed 0
           :total-characters 0
           :total-words 0
           :models-detected {}
           :biases-detected {}
           :sources {}
           :last-analysis nil
           :processing-times []})
  {:reset true :timestamp (System/currentTimeMillis)})

;; ============================================
;; API Functions
;; ============================================

(defn quick-analyze
  "Quick analysis for UI - returns essential metrics."
  [text]
  (let [start-time (System/currentTimeMillis)
        words (word-count text)
        chars (char-count text)
        top-models (algo/get-top-models text 3)]
    {:words words
     :characters chars
     :top-model (when (seq top-models) (:model (first top-models)))
     :model-count (count top-models)
     :analysis-time-ms (- (System/currentTimeMillis) start-time)}))

(defn comprehensive-analyze
  "Full comprehensive analysis of text."
  [text & {:keys [source-name include-patterns include-credibility]
           :or {source-name "manual" include-patterns true include-credibility true}}]
  (let [base-analysis (analyze-text-content text :source-name source-name)]
    (cond-> base-analysis
      include-patterns (assoc :narrative-patterns (detect-narrative-patterns text))
      include-credibility (assoc :credibility (assess-source-credibility text)))))
