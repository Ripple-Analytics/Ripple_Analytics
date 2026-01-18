(ns mental-models.services.academic
  "Academic Source Integration - Auto-update mental models from research
   Monitors arXiv, SSRN, and other academic sources for new insights"
  (:require [clj-http.client :as http]
            [clojure.xml :as xml]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [mental-models.services.llm :as llm]))

;; -- arXiv API ---------------------------------------------------------------

(def arxiv-base-url "http://export.arxiv.org/api/query")

(def relevant-categories
  ["cs.AI"      ; Artificial Intelligence
   "cs.CL"      ; Computation and Language
   "cs.LG"      ; Machine Learning
   "econ.GN"    ; General Economics
   "q-fin.GN"   ; General Finance
   "stat.ML"    ; Machine Learning (Statistics)
   "physics.soc-ph" ; Physics and Society
   "q-bio.NC"]) ; Neurons and Cognition

(defn search-arxiv
  "Search arXiv for papers related to mental models and decision-making"
  [query & {:keys [max-results start] :or {max-results 20 start 0}}]
  (try
    (let [params {"search_query" (str "all:" query)
                  "start" (str start)
                  "max_results" (str max-results)
                  "sortBy" "submittedDate"
                  "sortOrder" "descending"}
          response (http/get arxiv-base-url {:query-params params :as :stream})
          parsed (xml/parse (:body response))
          entries (filter #(= :entry (:tag %)) (:content parsed))]
      (map (fn [entry]
             (let [content (:content entry)
                   get-text (fn [tag]
                              (->> content
                                   (filter #(= tag (:tag %)))
                                   first
                                   :content
                                   first))]
               {:id (get-text :id)
                :title (get-text :title)
                :summary (get-text :summary)
                :published (get-text :published)
                :updated (get-text :updated)
                :authors (->> content
                              (filter #(= :author (:tag %)))
                              (map #(-> % :content first :content first)))
                :categories (->> content
                                 (filter #(= :category (:tag %)))
                                 (map #(get-in % [:attrs :term])))}))
           entries))
    (catch Exception e
      (log/error e "Failed to search arXiv")
      [])))

;; -- Paper Analysis ----------------------------------------------------------

(def model-relevance-prompt
  "You are an expert in mental models, cognitive science, and decision-making.
   
   Analyze this academic paper abstract and determine:
   1. Which mental models it relates to (from the standard list)
   2. What new insights it provides
   3. How it might update our understanding of existing models
   4. The strength of evidence (weak/moderate/strong)
   
   Return as JSON with structure:
   {\"relevant_models\": [...], \"insights\": [...], \"evidence_strength\": \"...\", 
    \"update_recommendations\": [...], \"relevance_score\": 0.0-1.0}")

(defn analyze-paper-relevance
  "Analyze a paper for relevance to mental models"
  [{:keys [title summary]}]
  (when (and title summary)
    (try
      (let [response (llm/invoke-llm
                      {:messages [{:role "system" :content model-relevance-prompt}
                                  {:role "user" :content (str "Title: " title "\n\nAbstract: " summary)}]
                       :temperature 0.3
                       :response-format {:type "json_object"}})
            content (get-in response [:choices 0 :message :content])]
        (when content
          (read-string content)))
      (catch Exception e
        (log/warn "Failed to analyze paper:" title (.getMessage e))
        nil))))

;; -- Monitoring Jobs ---------------------------------------------------------

(def monitoring-queries
  ["cognitive bias decision making"
   "mental models reasoning"
   "behavioral economics heuristics"
   "system thinking complexity"
   "probability judgment uncertainty"
   "second order effects"
   "feedback loops systems"
   "incentive design behavior"
   "risk assessment psychology"
   "confirmation bias evidence"])

(defn run-monitoring-scan
  "Run a scan for new papers across all monitoring queries"
  []
  (let [results (atom [])]
    (doseq [query monitoring-queries]
      (log/info "Scanning arXiv for:" query)
      (let [papers (search-arxiv query :max-results 10)]
        (doseq [paper papers]
          (when-let [analysis (analyze-paper-relevance paper)]
            (when (> (:relevance_score analysis 0) 0.5)
              (swap! results conj (merge paper analysis)))))))
    @results))

;; -- Model Update Recommendations --------------------------------------------

(defn generate-update-recommendations
  "Generate recommendations for updating mental models based on new research"
  [analyzed-papers]
  (let [by-model (group-by :relevant_models analyzed-papers)
        recommendations (for [[models papers] by-model
                              :when (seq models)]
                          {:models models
                           :paper-count (count papers)
                           :avg-relevance (/ (reduce + (map :relevance_score papers)) (count papers))
                           :key-insights (distinct (mapcat :insights papers))
                           :evidence-strength (if (> (count papers) 3) :strong :moderate)})]
    (sort-by :avg-relevance > recommendations)))

;; -- Scheduled Monitoring ----------------------------------------------------

(def last-scan-time (atom nil))
(def scan-results (atom []))

(defn schedule-daily-scan
  "Schedule a daily scan for new papers"
  []
  (future
    (loop []
      (let [now (System/currentTimeMillis)
            last-scan @last-scan-time
            should-scan (or (nil? last-scan)
                            (> (- now last-scan) (* 24 60 60 1000)))]
        (when should-scan
          (log/info "Starting scheduled academic scan...")
          (let [results (run-monitoring-scan)]
            (reset! scan-results results)
            (reset! last-scan-time now)
            (log/info "Scan complete. Found" (count results) "relevant papers")))
        (Thread/sleep (* 60 60 1000)) ; Check every hour
        (recur)))))

;; -- API Functions -----------------------------------------------------------

(defn get-recent-updates
  "Get recent model updates from academic sources"
  []
  {:papers @scan-results
   :last-scan @last-scan-time
   :recommendations (generate-update-recommendations @scan-results)})

(defn search-for-model
  "Search for papers related to a specific mental model"
  [model-name]
  (let [papers (search-arxiv model-name :max-results 20)]
    (map (fn [paper]
           (merge paper (analyze-paper-relevance paper)))
         papers)))
