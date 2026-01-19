(ns mental-models.predict.news-scanner
  "News Story Scanner - Applies mental models to news and shows overlap %"
  (:require [clojure.string :as str]
            [mental-models.predict.engine :as engine]))

;; ============================================================================
;; News Source Patterns
;; ============================================================================

(def news-categories
  "Categories of news that often contain mental model patterns"
  {:corporate-fraud ["fraud" "scandal" "SEC" "investigation" "accounting" "misleading" "whistleblower"]
   :market-bubble ["bubble" "mania" "all-time high" "FOMO" "retail investors" "meme stock" "crypto"]
   :disruption ["disruption" "obsolete" "declining" "market share loss" "pivot" "transformation"]
   :acquisition ["merger" "acquisition" "buyout" "takeover" "deal" "synergies"]
   :earnings ["earnings" "revenue" "profit" "guidance" "beat" "miss" "outlook"]
   :management ["CEO" "executive" "board" "resignation" "appointed" "leadership"]
   :regulatory ["regulation" "antitrust" "lawsuit" "fine" "compliance" "investigation"]})

;; ============================================================================
;; Text Analysis for Model Detection
;; ============================================================================

(def model-indicators
  "Keywords that indicate specific mental models in text"
  {
   ;; PSYCHOLOGY BIASES
   "Social Proof" ["everyone is" "popular" "trending" "viral" "mainstream" "consensus"
                   "investors are piling" "retail investors" "FOMO" "fear of missing"]
   "Incentive-Caused Bias" ["bonus" "compensation" "incentive" "commission" "stock options"
                            "performance targets" "quota" "conflict of interest"]
   "Authority Bias" ["expert says" "analyst" "CEO claims" "according to" "prestigious"
                     "Harvard" "Goldman" "McKinsey" "former executive"]
   "Denial" ["dismissed" "denied" "rejected criticism" "nothing to worry" "temporary"
             "one-time" "non-recurring" "isolated incident"]
   "Overconfidence" ["confident" "certain" "guaranteed" "definitely" "no doubt"
                     "sure thing" "can't fail" "bulletproof"]
   "Commitment & Consistency" ["doubled down" "staying the course" "committed to"
                               "reaffirmed" "maintaining strategy" "not changing"]
   "Availability Bias" ["recent" "just happened" "this week" "breaking" "latest"
                        "everyone's talking about"]
   "FOMO" ["fear of missing" "don't want to miss" "getting in before" "last chance"
           "opportunity" "before it's too late"]
   "Sunk Cost Fallacy" ["already invested" "too far in" "can't stop now" "after all we've done"
                        "throwing good money"]
   
   ;; ECONOMIC MODELS
   "Network Effects" ["network" "platform" "users" "marketplace" "ecosystem" "viral growth"
                      "more users" "flywheel"]
   "Switching Costs" ["lock-in" "switching" "migration" "integration" "embedded"
                      "sticky" "retention"]
   "Scale Economics" ["scale" "volume" "unit economics" "cost per" "leverage"
                      "operating leverage" "fixed costs"]
   "Moat" ["competitive advantage" "barrier" "moat" "defensible" "sustainable advantage"
           "pricing power"]
   "Margin of Safety" ["undervalued" "discount" "margin of safety" "cheap" "below intrinsic"
                       "value investment"]
   
   ;; DANGER SIGNALS
   "Complexity as Feature" ["sophisticated" "complex" "proprietary" "black box"
                            "algorithm" "AI-powered" "revolutionary"]
   "Greater Fool Theory" ["find a buyer" "flip" "momentum" "price appreciation"
                          "someone will pay more"]
   "Leverage Risk" ["leverage" "debt" "borrowed" "margin" "loan" "credit"
                    "highly leveraged"]})

(defn detect-models-in-text [text]
  "Detect mental models mentioned or implied in text"
  (let [text-lower (str/lower-case (or text ""))]
    (->> model-indicators
         (filter (fn [[model keywords]]
                  (some #(str/includes? text-lower (str/lower-case %)) keywords)))
         (map (fn [[model _]] model))
         distinct
         vec)))

(defn categorize-news [text]
  "Categorize news story based on content"
  (let [text-lower (str/lower-case (or text ""))]
    (->> news-categories
         (filter (fn [[category keywords]]
                  (some #(str/includes? text-lower %) keywords)))
         (map first)
         vec)))

;; ============================================================================
;; Overlap Scoring
;; ============================================================================

(defn calculate-overlap-score [detected-models]
  "Calculate overlap with known dangerous/successful patterns"
  (let [danger-models #{"Social Proof" "Incentive-Caused Bias" "Denial" "Overconfidence"
                        "FOMO" "Greater Fool Theory" "Leverage Risk" "Complexity as Feature"}
        success-models #{"Network Effects" "Switching Costs" "Moat" "Margin of Safety"
                         "Scale Economics"}
        
        danger-overlap (count (filter danger-models detected-models))
        success-overlap (count (filter success-models detected-models))
        
        danger-pct (/ (* 100 danger-overlap) (max 1 (count danger-models)))
        success-pct (/ (* 100 success-overlap) (max 1 (count success-models)))]
    
    {:danger-overlap-pct danger-pct
     :success-overlap-pct success-pct
     :net-score (- success-pct danger-pct)
     :danger-models-found (filter danger-models detected-models)
     :success-models-found (filter success-models detected-models)}))

;; ============================================================================
;; Full News Analysis
;; ============================================================================

(defn analyze-story [headline content]
  "Full analysis of a news story"
  (let [full-text (str headline " " content)
        detected-models (detect-models-in-text full-text)
        categories (categorize-news full-text)
        overlap (calculate-overlap-score detected-models)
        prediction (engine/analyze-news-story headline content detected-models)]
    
    {:headline headline
     :categories categories
     :detected-models detected-models
     :model-count (count detected-models)
     
     ;; Overlap scores
     :danger-overlap-pct (:danger-overlap-pct overlap)
     :success-overlap-pct (:success-overlap-pct overlap)
     :net-score (:net-score overlap)
     
     ;; Pattern matches
     :pattern-overlap-pct (:pattern-overlap-pct prediction)
     :case-overlap-pct (:case-overlap-pct prediction)
     
     ;; Predictions
     :danger-score (:danger-score prediction)
     :success-score (:success-score prediction)
     :overall-risk (:overall-risk prediction)
     :recommendation (:primary-recommendation prediction)
     
     ;; Similar historical cases
     :similar-cases (take 3 (:similar-cases prediction))
     :key-lessons (:key-lessons prediction)
     :warning-signs (take 5 (:warning-signs prediction))
     
     ;; Matching patterns
     :matching-patterns (take 3 (:matching-patterns prediction))}))

(defn format-analysis-report [analysis]
  "Format analysis into readable report"
  (let [{:keys [headline categories detected-models model-count
                danger-overlap-pct success-overlap-pct net-score
                danger-score success-score overall-risk recommendation
                similar-cases key-lessons warning-signs matching-patterns]} analysis]
    (str
     "═══════════════════════════════════════════════════════════════\n"
     "MENTAL MODELS ANALYSIS REPORT\n"
     "═══════════════════════════════════════════════════════════════\n\n"
     
     "HEADLINE: " headline "\n\n"
     
     "CATEGORIES: " (str/join ", " (map name categories)) "\n\n"
     
     "DETECTED MODELS (" model-count "):\n"
     (str/join "\n" (map #(str "  • " %) detected-models)) "\n\n"
     
     "═══════════════════════════════════════════════════════════════\n"
     "OVERLAP SCORES\n"
     "═══════════════════════════════════════════════════════════════\n"
     (format "  Danger Pattern Overlap:  %5.1f%%\n" (double danger-overlap-pct))
     (format "  Success Pattern Overlap: %5.1f%%\n" (double success-overlap-pct))
     (format "  Net Score:               %+5.1f\n" (double net-score))
     "\n"
     
     "═══════════════════════════════════════════════════════════════\n"
     "PREDICTION\n"
     "═══════════════════════════════════════════════════════════════\n"
     (format "  Danger Score:  %.0f%%\n" (* 100 danger-score))
     (format "  Success Score: %.0f%%\n" (* 100 success-score))
     "  Overall Risk:  " (name overall-risk) "\n\n"
     "  RECOMMENDATION: " recommendation "\n\n"
     
     (when (seq matching-patterns)
       (str
        "═══════════════════════════════════════════════════════════════\n"
        "MATCHING PATTERNS\n"
        "═══════════════════════════════════════════════════════════════\n"
        (str/join "\n" (map (fn [p]
                             (format "  • %s (%.0f%% confidence)\n    %s\n    Action: %s"
                                    (:pattern-name p)
                                    (* 100 (:confidence p))
                                    (:description p)
                                    (:action p)))
                           matching-patterns))
        "\n\n"))
     
     (when (seq similar-cases)
       (str
        "═══════════════════════════════════════════════════════════════\n"
        "SIMILAR HISTORICAL CASES\n"
        "═══════════════════════════════════════════════════════════════\n"
        (str/join "\n" (map (fn [c]
                             (format "  • %s (%.0f%% similar)\n    Outcome: %s\n    %s"
                                    (:case-name c)
                                    (* 100 (:similarity-score c))
                                    (name (:outcome c))
                                    (:outcome-description c)))
                           similar-cases))
        "\n\n"))
     
     (when (seq warning-signs)
       (str
        "═══════════════════════════════════════════════════════════════\n"
        "WARNING SIGNS TO WATCH\n"
        "═══════════════════════════════════════════════════════════════\n"
        (str/join "\n" (map #(str "  ⚠ " %) warning-signs))
        "\n\n"))
     
     (when (seq key-lessons)
       (str
        "═══════════════════════════════════════════════════════════════\n"
        "KEY LESSONS FROM HISTORY\n"
        "═══════════════════════════════════════════════════════════════\n"
        (str/join "\n" (map #(str "  → " %) key-lessons))
        "\n"))
     
     "═══════════════════════════════════════════════════════════════\n")))

;; ============================================================================
;; Batch News Processing
;; ============================================================================

(defn analyze-news-batch [stories]
  "Analyze multiple news stories"
  (map (fn [{:keys [headline content]}]
         (analyze-story headline content))
       stories))

(defn rank-stories-by-risk [analyses]
  "Rank analyzed stories by risk level"
  (sort-by :danger-score > analyses))

(defn rank-stories-by-opportunity [analyses]
  "Rank analyzed stories by opportunity"
  (sort-by :success-score > analyses))

;; ============================================================================
;; RSS/News Feed Integration Helpers
;; ============================================================================

(defn extract-text-from-html [html]
  "Simple HTML text extraction"
  (-> html
      (str/replace #"<script[^>]*>.*?</script>" "")
      (str/replace #"<style[^>]*>.*?</style>" "")
      (str/replace #"<[^>]+>" " ")
      (str/replace #"&nbsp;" " ")
      (str/replace #"&amp;" "&")
      (str/replace #"&lt;" "<")
      (str/replace #"&gt;" ">")
      (str/replace #"\s+" " ")
      str/trim))

(defn parse-rss-item [item-xml]
  "Parse RSS item (simplified)"
  (let [title-match (re-find #"<title>([^<]+)</title>" item-xml)
        desc-match (re-find #"<description>([^<]+)</description>" item-xml)]
    {:headline (or (second title-match) "")
     :content (extract-text-from-html (or (second desc-match) ""))}))
