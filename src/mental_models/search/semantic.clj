(ns mental-models.search.semantic
  "Semantic search for finding related mental models.
   Lightweight implementation without external dependencies."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Vector Operations
;; ============================================================================

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn magnitude [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn cosine-similarity [v1 v2]
  (let [dot (dot-product v1 v2)
        mag1 (magnitude v1)
        mag2 (magnitude v2)]
    (if (or (zero? mag1) (zero? mag2))
      0.0
      (/ dot (* mag1 mag2)))))

(defn normalize-vector [v]
  (let [mag (magnitude v)]
    (if (zero? mag)
      v
      (mapv #(/ % mag) v))))

;; ============================================================================
;; Text to Vector (Simple TF-IDF style)
;; ============================================================================

(def ^:private stop-words
  #{"the" "a" "an" "is" "are" "was" "were" "be" "been" "being" "have" "has" "had"
    "do" "does" "did" "will" "would" "could" "should" "may" "might" "must" "shall"
    "can" "need" "dare" "ought" "used" "to" "of" "in" "for" "on" "with" "at" "by"
    "from" "as" "into" "through" "during" "before" "after" "above" "below" "between"
    "under" "again" "further" "then" "once" "here" "there" "when" "where" "why"
    "how" "all" "each" "few" "more" "most" "other" "some" "such" "no" "nor" "not"
    "only" "own" "same" "so" "than" "too" "very" "just" "and" "but" "if" "or"
    "because" "until" "while" "although" "though" "this" "that" "these" "those"
    "i" "me" "my" "myself" "we" "our" "ours" "ourselves" "you" "your" "yours"
    "yourself" "yourselves" "he" "him" "his" "himself" "she" "her" "hers" "herself"
    "it" "its" "itself" "they" "them" "their" "theirs" "themselves" "what" "which"
    "who" "whom" "whose" "tend" "tendency"})

(defn tokenize [text]
  "Tokenize text into words"
  (->> (str/lower-case (or text ""))
       (re-seq #"[a-z]+")
       (remove stop-words)
       (remove #(< (count %) 3))))

(defn term-frequency [tokens]
  "Calculate term frequency map"
  (let [total (count tokens)]
    (if (zero? total)
      {}
      (->> tokens
           frequencies
           (map (fn [[term count]] [term (/ count (double total))]))
           (into {})))))

;; Build vocabulary from mental models
(def ^:private vocabulary (atom []))
(def ^:private vocab-index (atom {}))

(defn build-vocabulary! [mental-models]
  "Build vocabulary from mental models for consistent vector dimensions"
  (let [all-terms (->> mental-models
                       (mapcat (fn [m]
                                (concat (tokenize (:name m))
                                        (tokenize (:description m))
                                        (mapcat tokenize (:keywords m)))))
                       distinct
                       sort
                       vec)]
    (reset! vocabulary all-terms)
    (reset! vocab-index (into {} (map-indexed (fn [i t] [t i]) all-terms)))
    (count all-terms)))

(defn text-to-vector [text]
  "Convert text to vector using vocabulary"
  (let [tokens (tokenize text)
        tf (term-frequency tokens)
        vec-size (count @vocabulary)]
    (if (zero? vec-size)
      []
      (let [v (vec (repeat vec-size 0.0))]
        (reduce (fn [v [term freq]]
                  (if-let [idx (get @vocab-index term)]
                    (assoc v idx freq)
                    v))
                v
                tf)))))

;; ============================================================================
;; Mental Model Index
;; ============================================================================

(def ^:private model-vectors (atom {}))

(defn index-models! [mental-models]
  "Index all mental models for search"
  (build-vocabulary! mental-models)
  (doseq [model mental-models]
    (let [text (str (:name model) " " (:description model) " " 
                   (str/join " " (:keywords model)))
          vec (normalize-vector (text-to-vector text))]
      (swap! model-vectors assoc (:id model) {:model model :vector vec})))
  (count @model-vectors))

(defn search-models [query & {:keys [limit] :or {limit 10}}]
  "Search for mental models similar to query"
  (let [query-vec (normalize-vector (text-to-vector query))
        scored (->> @model-vectors
                    (map (fn [[id {:keys [model vector]}]]
                           {:model model
                            :score (cosine-similarity query-vec vector)}))
                    (filter #(> (:score %) 0.01))
                    (sort-by :score >)
                    (take limit))]
    scored))

(defn find-related-models [model-id & {:keys [limit] :or {limit 5}}]
  "Find models related to a given model"
  (when-let [{:keys [vector]} (get @model-vectors model-id)]
    (let [scored (->> @model-vectors
                      (filter (fn [[id _]] (not= id model-id)))
                      (map (fn [[id {:keys [model vector]}]]
                             {:model model
                              :score (cosine-similarity vector vector)}))
                      (sort-by :score >)
                      (take limit))]
      scored)))

;; ============================================================================
;; Document Search
;; ============================================================================

(def ^:private document-vectors (atom {}))

(defn index-document! [doc-id text metadata]
  "Index a document for search"
  (let [vec (normalize-vector (text-to-vector text))]
    (swap! document-vectors assoc doc-id {:text text :vector vec :metadata metadata})
    doc-id))

(defn search-documents [query & {:keys [limit] :or {limit 10}}]
  "Search indexed documents"
  (let [query-vec (normalize-vector (text-to-vector query))
        scored (->> @document-vectors
                    (map (fn [[id {:keys [text vector metadata]}]]
                           {:doc-id id
                            :text text
                            :metadata metadata
                            :score (cosine-similarity query-vec vector)}))
                    (filter #(> (:score %) 0.01))
                    (sort-by :score >)
                    (take limit))]
    scored))

(defn find-models-for-document [doc-id]
  "Find mental models relevant to a document"
  (when-let [{:keys [vector]} (get @document-vectors doc-id)]
    (->> @model-vectors
         (map (fn [[id {:keys [model vector]}]]
                {:model model
                 :score (cosine-similarity vector vector)}))
         (filter #(> (:score %) 0.1))
         (sort-by :score >)
         (take 10))))

;; ============================================================================
;; Hybrid Search (Keyword + Semantic)
;; ============================================================================

(defn hybrid-search [query mental-models & {:keys [limit alpha] :or {limit 10 alpha 0.5}}]
  "Combine keyword matching and semantic search"
  (let [query-lower (str/lower-case query)
        ;; Keyword matches
        keyword-matches (->> mental-models
                             (filter (fn [m]
                                      (or (str/includes? (str/lower-case (:name m)) query-lower)
                                          (some #(str/includes? (str/lower-case %) query-lower)
                                                (:keywords m)))))
                             (map (fn [m] {:model m :keyword-score 1.0})))
        keyword-ids (set (map #(get-in % [:model :id]) keyword-matches))
        
        ;; Semantic matches
        semantic-matches (search-models query :limit (* 2 limit))
        
        ;; Combine scores
        combined (->> (concat 
                       (map (fn [{:keys [model keyword-score]}]
                              (let [sem-score (or (:score (first (filter #(= (get-in % [:model :id]) (:id model))
                                                                         semantic-matches)))
                                                  0)]
                                {:model model
                                 :score (+ (* alpha keyword-score) (* (- 1 alpha) sem-score))}))
                            keyword-matches)
                       (map (fn [{:keys [model score]}]
                              (if (keyword-ids (:id model))
                                nil  ;; Already included
                                {:model model
                                 :score (* (- 1 alpha) score)}))
                            semantic-matches))
                      (remove nil?)
                      (sort-by :score >)
                      (take limit))]
    combined))

;; ============================================================================
;; Cluster Analysis
;; ============================================================================

(defn cluster-models [mental-models & {:keys [k] :or {k 5}}]
  "Simple k-means clustering of mental models"
  ;; Initialize centroids randomly
  (let [model-vecs (map (fn [m]
                          (let [text (str (:name m) " " (:description m))
                                vec (normalize-vector (text-to-vector text))]
                            {:model m :vector vec}))
                        mental-models)
        vec-size (count @vocabulary)]
    (when (pos? vec-size)
      (let [initial-centroids (take k (shuffle (map :vector model-vecs)))
            ;; Simple assignment (one iteration)
            assignments (map (fn [{:keys [model vector]}]
                              (let [distances (map-indexed (fn [i c]
                                                            [i (- 1 (cosine-similarity vector c))])
                                                          initial-centroids)
                                    [cluster _] (apply min-key second distances)]
                                {:model model :cluster cluster}))
                            model-vecs)]
        (group-by :cluster assignments)))))
