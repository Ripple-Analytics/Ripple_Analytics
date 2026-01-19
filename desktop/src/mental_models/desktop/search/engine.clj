(ns mental-models.desktop.search.engine
  "Global search functionality for Mental Models desktop app.
   Provides unified search across scan history, decisions, case studies,
   models, and signals with filtering and semantic search support."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [mental-models.desktop.db :as db]
            [mental-models.desktop.api.web-client :as api])
  (:import [java.time Instant Duration]
           [java.util.regex Pattern]))

;; =============================================================================
;; Search State
;; =============================================================================

(def search-state (atom {:recent-searches []
                         :saved-searches []
                         :last-search nil
                         :search-history-limit 50}))

(def search-dir (str (System/getProperty "user.home") "/.mental-models"))
(def search-file (str search-dir "/search-history.json"))

(defn ensure-search-dir! []
  (let [dir (io/file search-dir)]
    (when-not (.exists dir)
      (.mkdirs dir))))

(defn load-search-history!
  "Load search history from disk"
  []
  (ensure-search-dir!)
  (try
    (when (.exists (io/file search-file))
      (let [data (json/read-str (slurp search-file) :key-fn keyword)]
        (swap! search-state merge data)))
    (catch Exception e
      (println "Error loading search history:" (.getMessage e)))))

(defn save-search-history!
  "Save search history to disk"
  []
  (ensure-search-dir!)
  (try
    (spit search-file (json/write-str @search-state))
    (catch Exception e
      (println "Error saving search history:" (.getMessage e)))))

;; =============================================================================
;; Search Filters
;; =============================================================================

(defrecord SearchFilters [date-from date-to types risk-levels sources categories])

(def default-filters
  (->SearchFilters nil nil #{:all} #{:all} #{:all} #{:all}))

(defn create-filters
  "Create search filters"
  [& {:keys [date-from date-to types risk-levels sources categories]
      :or {types #{:all} risk-levels #{:all} sources #{:all} categories #{:all}}}]
  (->SearchFilters date-from date-to types risk-levels sources categories))

(defn matches-date-filter?
  "Check if an item matches date filters"
  [item filters]
  (let [item-date (or (:created-at item) (:analyzed-at item) (:date item))
        from (:date-from filters)
        to (:date-to filters)]
    (cond
      (nil? item-date) true
      (and from to) (and (>= (compare item-date from) 0)
                         (<= (compare item-date to) 0))
      from (>= (compare item-date from) 0)
      to (<= (compare item-date to) 0)
      :else true)))

(defn matches-type-filter?
  "Check if an item matches type filter"
  [item-type filters]
  (or (contains? (:types filters) :all)
      (contains? (:types filters) item-type)))

(defn matches-risk-filter?
  "Check if an item matches risk level filter"
  [item filters]
  (or (contains? (:risk-levels filters) :all)
      (contains? (:risk-levels filters) (:risk-level item))
      (contains? (:risk-levels filters) (:severity item))))

;; =============================================================================
;; Text Matching
;; =============================================================================

(defn normalize-text
  "Normalize text for searching"
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" " ")
        (str/replace #"\s+" " ")
        str/trim)))

(defn text-contains?
  "Check if text contains query (case-insensitive)"
  [text query]
  (when (and text query)
    (str/includes? (normalize-text text) (normalize-text query))))

(defn text-matches-pattern?
  "Check if text matches a regex pattern"
  [text pattern]
  (when (and text pattern)
    (try
      (boolean (re-find (re-pattern (str "(?i)" pattern)) text))
      (catch Exception _ false))))

(defn calculate-relevance-score
  "Calculate relevance score for a search result"
  [item query]
  (let [query-lower (str/lower-case (or query ""))
        title (str/lower-case (or (:title item) (:name item) (:file-name item) ""))
        description (str/lower-case (or (:description item) (:content item) ""))
        
        ;; Exact title match = highest score
        title-exact (if (= title query-lower) 100 0)
        ;; Title contains query
        title-contains (if (str/includes? title query-lower) 50 0)
        ;; Title starts with query
        title-starts (if (str/starts-with? title query-lower) 25 0)
        ;; Description contains query
        desc-contains (if (str/includes? description query-lower) 10 0)
        ;; Word match bonus
        query-words (str/split query-lower #"\s+")
        word-matches (count (filter #(or (str/includes? title %)
                                         (str/includes? description %))
                                    query-words))
        word-bonus (* 5 word-matches)]
    
    (+ title-exact title-contains title-starts desc-contains word-bonus)))

;; =============================================================================
;; Search Functions by Type
;; =============================================================================

(defn search-scan-history
  "Search through scan history"
  [query & {:keys [filters limit] :or {filters default-filters limit 50}}]
  (let [results (db/get-recent-analyses 1000)]
    (->> results
         (filter (fn [r]
                   (and (matches-date-filter? r filters)
                        (or (text-contains? (:file-path r) query)
                            (text-contains? (:file-name r) query)
                            (some #(text-contains? (:name %) query) (:mental-models r))
                            (some #(text-contains? (:explanation %) query) (:mental-models r))))))
         (map #(assoc % :type :scan-result :relevance (calculate-relevance-score % query)))
         (sort-by :relevance >)
         (take limit)
         vec)))

(defn search-decisions
  "Search through decisions"
  [query & {:keys [filters limit] :or {filters default-filters limit 50}}]
  (let [decisions (or (api/get-decisions) [])]
    (->> decisions
         (filter (fn [d]
                   (and (matches-date-filter? d filters)
                        (or (text-contains? (:title d) query)
                            (text-contains? (:description d) query)
                            (text-contains? (:notes d) query)
                            (some #(text-contains? (:name %) query) (:models d))))))
         (map #(assoc % :type :decision :relevance (calculate-relevance-score % query)))
         (sort-by :relevance >)
         (take limit)
         vec)))

(defn search-case-studies
  "Search through case studies"
  [query & {:keys [filters limit] :or {filters default-filters limit 50}}]
  (let [case-studies (or (api/get-case-studies) [])]
    (->> case-studies
         (filter (fn [cs]
                   (and (matches-date-filter? cs filters)
                        (or (text-contains? (:title cs) query)
                            (text-contains? (:description cs) query)
                            (text-contains? (:summary cs) query)
                            (some #(text-contains? (:name %) query) (:models cs))))))
         (map #(assoc % :type :case-study :relevance (calculate-relevance-score % query)))
         (sort-by :relevance >)
         (take limit)
         vec)))

(defn search-models
  "Search through mental models"
  [query & {:keys [filters limit] :or {filters default-filters limit 50}}]
  (let [models (or (api/get-models) [])]
    (->> models
         (filter (fn [m]
                   (and (or (contains? (:categories filters) :all)
                            (contains? (:categories filters) (:category m)))
                        (or (text-contains? (:name m) query)
                            (text-contains? (:description m) query)
                            (text-contains? (:category m) query)
                            (some #(text-contains? % query) (:keywords m))))))
         (map #(assoc % :type :model :relevance (calculate-relevance-score % query)))
         (sort-by :relevance >)
         (take limit)
         vec)))

(defn search-signals
  "Search through signals"
  [query & {:keys [filters limit] :or {filters default-filters limit 50}}]
  (let [signals (or (api/get-signals) [])]
    (->> signals
         (filter (fn [s]
                   (and (matches-date-filter? s filters)
                        (matches-risk-filter? s filters)
                        (or (text-contains? (:title s) query)
                            (text-contains? (:description s) query)
                            (text-contains? (:source s) query)
                            (some #(text-contains? (:name %) query) (:models s))))))
         (map #(assoc % :type :signal :relevance (calculate-relevance-score % query)))
         (sort-by :relevance >)
         (take limit)
         vec)))

;; =============================================================================
;; Global Search
;; =============================================================================

(defn global-search
  "Search across all content types"
  [query & {:keys [filters limit types]
            :or {filters default-filters limit 100 types #{:all}}}]
  (let [search-types (if (contains? types :all)
                       #{:scan-result :decision :case-study :model :signal}
                       types)
        results (concat
                 (when (contains? search-types :scan-result)
                   (search-scan-history query :filters filters :limit limit))
                 (when (contains? search-types :decision)
                   (search-decisions query :filters filters :limit limit))
                 (when (contains? search-types :case-study)
                   (search-case-studies query :filters filters :limit limit))
                 (when (contains? search-types :model)
                   (search-models query :filters filters :limit limit))
                 (when (contains? search-types :signal)
                   (search-signals query :filters filters :limit limit)))]
    
    ;; Record search
    (add-to-recent-searches! query)
    
    ;; Sort by relevance and return
    (->> results
         (sort-by :relevance >)
         (take limit)
         vec)))

;; =============================================================================
;; Recent and Saved Searches
;; =============================================================================

(defn add-to-recent-searches!
  "Add a query to recent searches"
  [query]
  (when (and query (not (str/blank? query)))
    (swap! search-state
           (fn [state]
             (let [recent (:recent-searches state)
                   ;; Remove duplicates and add to front
                   updated (cons {:query query :searched-at (str (Instant/now))}
                                 (remove #(= (:query %) query) recent))
                   ;; Limit size
                   trimmed (take (:search-history-limit state) updated)]
               (assoc state
                      :recent-searches (vec trimmed)
                      :last-search query))))
    (save-search-history!)))

(defn get-recent-searches
  "Get recent search queries"
  [& {:keys [limit] :or {limit 10}}]
  (take limit (:recent-searches @search-state)))

(defn clear-recent-searches!
  "Clear recent search history"
  []
  (swap! search-state assoc :recent-searches [])
  (save-search-history!))

(defn save-search!
  "Save a search query for later use"
  [query & {:keys [name filters]}]
  (let [saved-search {:id (str (java.util.UUID/randomUUID))
                      :query query
                      :name (or name query)
                      :filters filters
                      :created-at (str (Instant/now))}]
    (swap! search-state update :saved-searches conj saved-search)
    (save-search-history!)
    saved-search))

(defn get-saved-searches
  "Get saved searches"
  []
  (:saved-searches @search-state))

(defn delete-saved-search!
  "Delete a saved search"
  [search-id]
  (swap! search-state update :saved-searches
         #(vec (remove (fn [s] (= (:id s) search-id)) %)))
  (save-search-history!))

(defn run-saved-search
  "Run a saved search"
  [search-id]
  (when-let [saved (first (filter #(= (:id %) search-id) (:saved-searches @search-state)))]
    (global-search (:query saved) :filters (:filters saved))))

;; =============================================================================
;; Semantic Search (Placeholder for embeddings-based search)
;; =============================================================================

(defn semantic-search
  "Semantic search using embeddings (placeholder - requires LM Studio or similar)"
  [query & {:keys [limit] :or {limit 20}}]
  ;; For now, fall back to keyword search
  ;; In the future, this could use embeddings from LM Studio
  (global-search query :limit limit))

;; =============================================================================
;; Search Suggestions
;; =============================================================================

(defn get-search-suggestions
  "Get search suggestions based on partial query"
  [partial-query & {:keys [limit] :or {limit 5}}]
  (let [query-lower (str/lower-case (or partial-query ""))
        recent (->> (:recent-searches @search-state)
                    (filter #(str/starts-with? (str/lower-case (:query %)) query-lower))
                    (map :query)
                    (take limit))
        ;; Add model names as suggestions
        models (or (api/get-models) [])
        model-suggestions (->> models
                               (filter #(str/starts-with? (str/lower-case (:name %)) query-lower))
                               (map :name)
                               (take limit))]
    (distinct (concat recent model-suggestions))))

;; =============================================================================
;; Quick Search
;; =============================================================================

(defn quick-search
  "Quick search with minimal processing for autocomplete"
  [query & {:keys [limit] :or {limit 10}}]
  (when (and query (>= (count query) 2))
    (let [scan-results (take 3 (search-scan-history query :limit 3))
          models (take 3 (search-models query :limit 3))
          decisions (take 2 (search-decisions query :limit 2))
          signals (take 2 (search-signals query :limit 2))]
      {:scan-results scan-results
       :models models
       :decisions decisions
       :signals signals
       :total (+ (count scan-results) (count models) (count decisions) (count signals))})))

;; =============================================================================
;; Advanced Search
;; =============================================================================

(defn advanced-search
  "Advanced search with complex query syntax"
  [query-string & {:keys [limit] :or {limit 100}}]
  ;; Parse query string for operators
  ;; Supported: AND, OR, NOT, "exact phrase", field:value
  (let [;; Extract quoted phrases
        phrases (re-seq #"\"([^\"]+)\"" query-string)
        ;; Extract field:value pairs
        field-values (re-seq #"(\w+):(\S+)" query-string)
        ;; Get remaining terms
        remaining (-> query-string
                      (str/replace #"\"[^\"]+\"" "")
                      (str/replace #"\w+:\S+" "")
                      str/trim)
        terms (when (not (str/blank? remaining))
                (str/split remaining #"\s+"))
        
        ;; Build filters from field:value pairs
        filters (reduce (fn [f [_ field value]]
                          (case field
                            "type" (update f :types conj (keyword value))
                            "risk" (update f :risk-levels conj (keyword value))
                            "category" (update f :categories conj value)
                            "from" (assoc f :date-from value)
                            "to" (assoc f :date-to value)
                            f))
                        default-filters
                        field-values)
        
        ;; Combine search terms
        search-query (str/join " " (concat (map second phrases) terms))]
    
    (global-search search-query :filters filters :limit limit)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize the search engine"
  []
  (load-search-history!))
