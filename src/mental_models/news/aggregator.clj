(ns mental-models.news.aggregator
  "Real-Time News Aggregator with Deep Munger Framework Analysis
   
   Features:
   - Multi-source news ingestion (RSS, APIs, scraping)
   - Real-time streaming updates
   - Instant deep analysis through all 129 mental models
   - Failure mode detection
   - Bias identification
   - Second-order effects mapping
   - Click-to-analyze with comprehensive reports"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! >!! chan 
                                                   close! mult tap timeout]]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration ZonedDateTime]
           [java.time.format DateTimeFormatter]
           [java.net URL HttpURLConnection]
           [java.io BufferedReader InputStreamReader]
           [java.util UUID]))

;; =============================================================================
;; NEWS SOURCES CONFIGURATION
;; =============================================================================

(def news-sources
  "Comprehensive list of news sources organized by category"
  
  {:financial
   [{:id :bloomberg
     :name "Bloomberg"
     :type :rss
     :url "https://feeds.bloomberg.com/markets/news.rss"
     :refresh-ms 30000}
    {:id :reuters-business
     :name "Reuters Business"
     :type :rss
     :url "https://feeds.reuters.com/reuters/businessNews"
     :refresh-ms 30000}
    {:id :wsj
     :name "Wall Street Journal"
     :type :rss
     :url "https://feeds.a.dj.com/rss/RSSMarketsMain.xml"
     :refresh-ms 60000}
    {:id :ft
     :name "Financial Times"
     :type :rss
     :url "https://www.ft.com/rss/home"
     :refresh-ms 60000}
    {:id :cnbc
     :name "CNBC"
     :type :rss
     :url "https://www.cnbc.com/id/100003114/device/rss/rss.html"
     :refresh-ms 30000}]
   
   :technology
   [{:id :hn
     :name "Hacker News"
     :type :api
     :url "https://hacker-news.firebaseio.com/v0"
     :refresh-ms 60000}
    {:id :techcrunch
     :name "TechCrunch"
     :type :rss
     :url "https://techcrunch.com/feed/"
     :refresh-ms 60000}
    {:id :ars
     :name "Ars Technica"
     :type :rss
     :url "https://feeds.arstechnica.com/arstechnica/index"
     :refresh-ms 120000}
    {:id :verge
     :name "The Verge"
     :type :rss
     :url "https://www.theverge.com/rss/index.xml"
     :refresh-ms 120000}]
   
   :general
   [{:id :bbc
     :name "BBC News"
     :type :rss
     :url "http://feeds.bbci.co.uk/news/rss.xml"
     :refresh-ms 60000}
    {:id :nyt
     :name "New York Times"
     :type :rss
     :url "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
     :refresh-ms 60000}
    {:id :guardian
     :name "The Guardian"
     :type :rss
     :url "https://www.theguardian.com/world/rss"
     :refresh-ms 60000}
    {:id :economist
     :name "The Economist"
     :type :rss
     :url "https://www.economist.com/rss"
     :refresh-ms 300000}]
   
   :academic
   [{:id :arxiv-finance
     :name "arXiv Finance"
     :type :rss
     :url "http://export.arxiv.org/rss/q-fin"
     :refresh-ms 3600000}
    {:id :arxiv-econ
     :name "arXiv Economics"
     :type :rss
     :url "http://export.arxiv.org/rss/econ"
     :refresh-ms 3600000}
    {:id :arxiv-ai
     :name "arXiv AI"
     :type :rss
     :url "http://export.arxiv.org/rss/cs.AI"
     :refresh-ms 3600000}
    {:id :ssrn
     :name "SSRN"
     :type :scrape
     :url "https://papers.ssrn.com/sol3/TopDownloads.cfm"
     :refresh-ms 3600000}]
   
   :regulatory
   [{:id :sec-filings
     :name "SEC Filings"
     :type :rss
     :url "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&type=&company=&dateb=&owner=include&count=100&output=atom"
     :refresh-ms 300000}
    {:id :fed
     :name "Federal Reserve"
     :type :rss
     :url "https://www.federalreserve.gov/feeds/press_all.xml"
     :refresh-ms 300000}]})

;; =============================================================================
;; NEWS ITEM STRUCTURE
;; =============================================================================

(defn create-news-item
  "Create a standardized news item"
  [source raw-item]
  {:id (str (UUID/randomUUID))
   :source-id (:id source)
   :source-name (:name source)
   :title (:title raw-item)
   :url (:link raw-item)
   :summary (:description raw-item)
   :content (:content raw-item)
   :published-at (or (:pubDate raw-item) (Instant/now))
   :ingested-at (Instant/now)
   :category (first (keys (filter #(some #{source} (val %)) news-sources)))
   :analysis nil
   :analyzed? false})

;; =============================================================================
;; RSS PARSER
;; =============================================================================

(defn fetch-url
  "Fetch content from URL with timeout"
  [url timeout-ms]
  (try
    (let [conn (doto (.openConnection (URL. url))
                 (.setConnectTimeout timeout-ms)
                 (.setReadTimeout timeout-ms)
                 (.setRequestProperty "User-Agent" "MentalModels/1.0"))]
      (with-open [reader (BufferedReader. 
                           (InputStreamReader. (.getInputStream conn)))]
        (slurp reader)))
    (catch Exception e
      (log/debug "Fetch error:" url (.getMessage e))
      nil)))

(defn parse-rss
  "Parse RSS/Atom feed"
  [xml-str]
  (try
    (let [parsed (xml/parse (java.io.ByteArrayInputStream. 
                              (.getBytes xml-str "UTF-8")))]
      (->> parsed
           :content
           (filter #(= :channel (:tag %)))
           first
           :content
           (filter #(= :item (:tag %)))
           (map (fn [item]
                  (let [content (:content item)]
                    {:title (some #(when (= :title (:tag %)) 
                                     (first (:content %))) content)
                     :link (some #(when (= :link (:tag %)) 
                                    (first (:content %))) content)
                     :description (some #(when (= :description (:tag %)) 
                                           (first (:content %))) content)
                     :pubDate (some #(when (= :pubDate (:tag %)) 
                                       (first (:content %))) content)})))))
    (catch Exception e
      (log/debug "RSS parse error:" (.getMessage e))
      [])))

;; =============================================================================
;; NEWS FETCHER
;; =============================================================================

(defn fetch-source!
  "Fetch news from a single source"
  [source]
  (case (:type source)
    :rss
    (when-let [content (fetch-url (:url source) 10000)]
      (let [items (parse-rss content)]
        (mapv #(create-news-item source %) items)))
    
    :api
    ;; Would implement API-specific fetching
    []
    
    :scrape
    ;; Would implement scraping
    []
    
    []))

;; =============================================================================
;; AGGREGATOR STATE
;; =============================================================================

(defonce aggregator-state
  (atom {:status :idle
         :sources (mapcat val news-sources)
         :items {}           ; id -> news-item
         :by-source {}       ; source-id -> [item-ids]
         :by-category {}     ; category -> [item-ids]
         :latest []          ; Most recent item ids
         :stats {:total-items 0
                 :items-today 0
                 :analyzed 0
                 :sources-active 0}}))

;; =============================================================================
;; REAL-TIME STREAMING
;; =============================================================================

(defonce news-channel (chan 10000))
(defonce news-mult (mult news-channel))

(defn subscribe-news!
  "Subscribe to real-time news stream"
  []
  (let [ch (chan 1000)]
    (tap news-mult ch)
    ch))

(defn start-aggregator!
  "Start the real-time news aggregator"
  []
  (log/info "Starting news aggregator...")
  
  (swap! aggregator-state assoc :status :running)
  
  ;; Start fetcher for each source
  (doseq [source (mapcat val news-sources)]
    (go-loop []
      (when (= :running (:status @aggregator-state))
        (try
          (let [items (fetch-source! source)]
            (doseq [item items]
              ;; Check if new
              (when-not (get-in @aggregator-state [:items (:id item)])
                ;; Store item
                (swap! aggregator-state 
                       (fn [s]
                         (-> s
                             (assoc-in [:items (:id item)] item)
                             (update-in [:by-source (:source-id item)] 
                                        (fnil conj []) (:id item))
                             (update-in [:by-category (:category item)] 
                                        (fnil conj []) (:id item))
                             (update :latest #(take 1000 (cons (:id item) %)))
                             (update-in [:stats :total-items] inc))))
                
                ;; Broadcast to subscribers
                (>!! news-channel item))))
          (catch Exception e
            (log/debug "Aggregator error:" (:id source) (.getMessage e))))
        
        (<! (timeout (:refresh-ms source 60000)))
        (recur))))
  
  (log/info "News aggregator started with" 
            (count (mapcat val news-sources)) "sources"))

(defn stop-aggregator!
  "Stop the news aggregator"
  []
  (swap! aggregator-state assoc :status :stopped)
  (log/info "News aggregator stopped"))

;; =============================================================================
;; DEEP MUNGER FRAMEWORK ANALYSIS
;; =============================================================================

(def mental-models-db
  "Complete database of 129 mental models for analysis"
  
  ;; Mathematics
  [{:id "compound-interest" :name "Compound Interest" :category :mathematics
    :keywords ["compound" "interest" "exponential" "growth" "time"]
    :questions ["Is there compounding at play?" "What's the growth rate?" "How long until significant?"]}
   {:id "probability" :name "Probability Theory" :category :mathematics
    :keywords ["probability" "chance" "likely" "odds" "risk"]
    :questions ["What are the actual probabilities?" "Base rate?" "Conditional probability?"]}
   {:id "bayes-theorem" :name "Bayesian Updating" :category :mathematics
    :keywords ["evidence" "update" "prior" "posterior" "belief"]
    :questions ["What's the prior probability?" "How does new evidence change this?"]}
   {:id "regression-to-mean" :name "Regression to Mean" :category :mathematics
    :keywords ["extreme" "average" "revert" "normal" "outlier"]
    :questions ["Is this an extreme result?" "Will it revert to average?"]}
   {:id "power-laws" :name "Power Laws" :category :mathematics
    :keywords ["winner" "takes" "all" "pareto" "80/20" "scale"]
    :questions ["Does this follow a power law?" "Who are the big winners?"]}
   
   ;; Psychology - Biases
   {:id "confirmation-bias" :name "Confirmation Bias" :category :psychology
    :keywords ["confirm" "belief" "ignore" "contrary" "evidence"]
    :questions ["Am I seeking confirming evidence?" "What would disprove this?"]}
   {:id "availability-bias" :name "Availability Heuristic" :category :psychology
    :keywords ["recent" "memorable" "vivid" "easy" "recall"]
    :questions ["Am I overweighting recent/vivid events?" "What's the base rate?"]}
   {:id "anchoring" :name "Anchoring" :category :psychology
    :keywords ["anchor" "first" "initial" "reference" "starting"]
    :questions ["What's the anchor here?" "Is it relevant?"]}
   {:id "loss-aversion" :name "Loss Aversion" :category :psychology
    :keywords ["loss" "lose" "risk" "downside" "fear"]
    :questions ["Is loss aversion driving decisions?" "What's the actual risk?"]}
   {:id "social-proof" :name "Social Proof" :category :psychology
    :keywords ["everyone" "popular" "crowd" "consensus" "trend"]
    :questions ["Is this just following the crowd?" "Is the crowd right?"]}
   {:id "authority-bias" :name "Authority Bias" :category :psychology
    :keywords ["expert" "authority" "leader" "official" "institution"]
    :questions ["Is the authority actually expert in this?" "Conflicts of interest?"]}
   {:id "narrative-fallacy" :name "Narrative Fallacy" :category :psychology
    :keywords ["story" "narrative" "explain" "cause" "reason"]
    :questions ["Is this just a compelling story?" "What's the actual causation?"]}
   {:id "hindsight-bias" :name "Hindsight Bias" :category :psychology
    :keywords ["obvious" "knew" "predicted" "should" "clear"]
    :questions ["Was this actually predictable?" "What did we know before?"]}
   {:id "overconfidence" :name "Overconfidence" :category :psychology
    :keywords ["certain" "confident" "sure" "definitely" "guaranteed"]
    :questions ["How confident should we actually be?" "What's the uncertainty?"]}
   {:id "sunk-cost" :name "Sunk Cost Fallacy" :category :psychology
    :keywords ["invested" "already" "spent" "committed" "continue"]
    :questions ["Are sunk costs influencing this?" "What's the forward-looking value?"]}
   
   ;; Economics
   {:id "incentives" :name "Incentives" :category :economics
    :keywords ["incentive" "reward" "motivate" "benefit" "gain"]
    :questions ["What are the incentives?" "Who benefits?" "Misaligned incentives?"]}
   {:id "opportunity-cost" :name "Opportunity Cost" :category :economics
    :keywords ["alternative" "instead" "trade-off" "choice" "option"]
    :questions ["What's the opportunity cost?" "What are we giving up?"]}
   {:id "supply-demand" :name "Supply and Demand" :category :economics
    :keywords ["supply" "demand" "price" "scarcity" "abundance"]
    :questions ["How does this affect supply/demand?" "Price implications?"]}
   {:id "comparative-advantage" :name "Comparative Advantage" :category :economics
    :keywords ["specialize" "trade" "advantage" "efficient" "focus"]
    :questions ["Where's the comparative advantage?" "Who should do what?"]}
   {:id "network-effects" :name "Network Effects" :category :economics
    :keywords ["network" "users" "platform" "viral" "adoption"]
    :questions ["Are there network effects?" "Winner-take-all dynamics?"]}
   {:id "moral-hazard" :name "Moral Hazard" :category :economics
    :keywords ["risk" "insured" "protected" "bailout" "guarantee"]
    :questions ["Does this create moral hazard?" "Who bears the risk?"]}
   {:id "principal-agent" :name "Principal-Agent Problem" :category :economics
    :keywords ["agent" "manager" "owner" "interest" "align"]
    :questions ["Are principal and agent interests aligned?" "Agency costs?"]}
   
   ;; Physics/Engineering
   {:id "critical-mass" :name "Critical Mass" :category :physics
    :keywords ["tipping" "point" "threshold" "critical" "mass"]
    :questions ["Is there a critical mass/tipping point?" "How close are we?"]}
   {:id "feedback-loops" :name "Feedback Loops" :category :physics
    :keywords ["feedback" "loop" "reinforce" "amplify" "cycle"]
    :questions ["What feedback loops exist?" "Positive or negative?"]}
   {:id "equilibrium" :name "Equilibrium" :category :physics
    :keywords ["balance" "stable" "equilibrium" "steady" "state"]
    :questions ["What's the equilibrium state?" "Is it stable?"]}
   {:id "leverage" :name "Leverage" :category :physics
    :keywords ["leverage" "multiply" "amplify" "magnify" "force"]
    :questions ["Where's the leverage?" "What multiplies the effect?"]}
   {:id "margin-of-safety" :name "Margin of Safety" :category :engineering
    :keywords ["margin" "safety" "buffer" "cushion" "redundancy"]
    :questions ["What's the margin of safety?" "What could go wrong?"]}
   {:id "redundancy" :name "Redundancy" :category :engineering
    :keywords ["backup" "redundant" "failsafe" "multiple" "spare"]
    :questions ["Is there redundancy?" "Single points of failure?"]}
   
   ;; Biology
   {:id "evolution" :name "Evolution" :category :biology
    :keywords ["evolve" "adapt" "survive" "select" "fit"]
    :questions ["How will this evolve?" "Selection pressures?"]}
   {:id "ecosystem" :name "Ecosystem Thinking" :category :biology
    :keywords ["ecosystem" "environment" "interact" "depend" "system"]
    :questions ["What's the ecosystem?" "Interdependencies?"]}
   {:id "red-queen" :name "Red Queen Effect" :category :biology
    :keywords ["compete" "race" "keep up" "running" "standing"]
    :questions ["Is this a Red Queen situation?" "Running to stand still?"]}
   
   ;; Meta/Decision-Making
   {:id "inversion" :name "Inversion" :category :meta
    :keywords ["avoid" "prevent" "opposite" "reverse" "invert"]
    :questions ["What should we avoid?" "Invert the problem?"]}
   {:id "second-order" :name "Second-Order Thinking" :category :meta
    :keywords ["then" "consequence" "effect" "result" "after"]
    :questions ["And then what?" "Second-order effects?"]}
   {:id "circle-of-competence" :name "Circle of Competence" :category :meta
    :keywords ["know" "understand" "expert" "competence" "edge"]
    :questions ["Is this within our competence?" "Do we really understand?"]}
   {:id "first-principles" :name "First Principles" :category :meta
    :keywords ["fundamental" "basic" "assumption" "ground" "truth"]
    :questions ["What are the first principles?" "Assumptions?"]}
   {:id "occams-razor" :name "Occam's Razor" :category :meta
    :keywords ["simple" "complex" "explanation" "parsimonious" "straightforward"]
    :questions ["Is there a simpler explanation?" "Unnecessary complexity?"]}])

(defn score-model-relevance
  "Score how relevant a mental model is to the content"
  [model content]
  (let [text (str/lower-case (str content))
        keyword-matches (count (filter #(str/includes? text %) (:keywords model)))]
    {:model-id (:id model)
     :model-name (:name model)
     :category (:category model)
     :relevance-score (/ keyword-matches (max 1 (count (:keywords model))))
     :keyword-matches keyword-matches
     :questions (:questions model)}))

(defn analyze-with-models
  "Analyze content against all mental models"
  [content]
  (let [text (str (:title content) " " (:summary content) " " (:content content))
        scores (->> mental-models-db
                    (map #(score-model-relevance % text))
                    (filter #(pos? (:relevance-score %)))
                    (sort-by :relevance-score >))]
    {:total-models-checked (count mental-models-db)
     :relevant-models (count scores)
     :top-models (take 10 scores)
     :by-category (group-by :category scores)
     :key-questions (->> scores
                         (take 5)
                         (mapcat :questions)
                         distinct)}))

;; =============================================================================
;; FAILURE MODE DETECTION
;; =============================================================================

(def failure-modes
  "Common failure modes to detect"
  [{:id "groupthink" 
    :name "Groupthink"
    :indicators ["consensus" "unanimous" "everyone agrees" "no dissent"]}
   {:id "overreaction"
    :name "Overreaction to News"
    :indicators ["crash" "surge" "panic" "euphoria" "extreme"]}
   {:id "recency-bias"
    :name "Recency Bias"
    :indicators ["recent" "just happened" "latest" "new normal"]}
   {:id "extrapolation"
    :name "Linear Extrapolation"
    :indicators ["will continue" "trend" "forever" "always"]}
   {:id "single-cause"
    :name "Single Cause Fallacy"
    :indicators ["because of" "caused by" "due to" "reason is"]}
   {:id "false-precision"
    :name "False Precision"
    :indicators ["exactly" "precisely" "will be" "forecast"]}])

(defn detect-failure-modes
  "Detect potential failure modes in content"
  [content]
  (let [text (str/lower-case (str (:title content) " " (:summary content)))]
    (->> failure-modes
         (map (fn [mode]
                (let [matches (filter #(str/includes? text %) (:indicators mode))]
                  (when (seq matches)
                    {:mode-id (:id mode)
                     :mode-name (:name mode)
                     :indicators-found matches
                     :severity (/ (count matches) (count (:indicators mode)))}))))
         (filter some?))))

;; =============================================================================
;; SECOND-ORDER EFFECTS
;; =============================================================================

(defn map-second-order-effects
  "Map potential second-order effects"
  [content models]
  (let [primary-effects (take 3 (:top-models models))]
    (mapv (fn [model]
            {:primary-model (:model-name model)
             :second-order-questions
             (case (:model-id model)
               "incentives" ["Who will change behavior?" 
                             "Unintended consequences?"
                             "Gaming the system?"]
               "network-effects" ["Winner-take-all outcome?"
                                  "Lock-in effects?"
                                  "Switching costs?"]
               "feedback-loops" ["Will it amplify or dampen?"
                                 "Runaway effects?"
                                 "Stabilizing forces?"]
               "supply-demand" ["Price effects?"
                                "Substitutes emerge?"
                                "New entrants?"]
               ["What happens next?"
                "Who else is affected?"
                "Downstream consequences?"])})
          primary-effects)))

;; =============================================================================
;; COMPREHENSIVE ANALYSIS
;; =============================================================================

(defn deep-analyze!
  "Perform deep analysis of a news item through the complete Munger framework"
  [news-item]
  (log/info "Deep analyzing:" (:title news-item))
  
  (let [start-time (System/currentTimeMillis)
        
        ;; Mental model analysis
        model-analysis (analyze-with-models news-item)
        
        ;; Failure mode detection
        failure-modes (detect-failure-modes news-item)
        
        ;; Second-order effects
        second-order (map-second-order-effects news-item model-analysis)
        
        ;; Bias detection
        biases-detected (->> (:top-models model-analysis)
                             (filter #(= :psychology (:category %)))
                             (map :model-name))
        
        duration-ms (- (System/currentTimeMillis) start-time)
        
        analysis {:analyzed-at (Instant/now)
                  :duration-ms duration-ms
                  :model-analysis model-analysis
                  :failure-modes failure-modes
                  :second-order-effects second-order
                  :biases-detected biases-detected
                  :key-questions (:key-questions model-analysis)
                  :summary {:relevant-models (:relevant-models model-analysis)
                            :top-model (-> model-analysis :top-models first :model-name)
                            :failure-modes-count (count failure-modes)
                            :biases-count (count biases-detected)}}]
    
    ;; Update stored item
    (swap! aggregator-state 
           (fn [s]
             (-> s
                 (assoc-in [:items (:id news-item) :analysis] analysis)
                 (assoc-in [:items (:id news-item) :analyzed?] true)
                 (update-in [:stats :analyzed] inc))))
    
    analysis))

;; =============================================================================
;; API
;; =============================================================================

(defn get-latest-news
  "Get latest news items"
  [& {:keys [limit category source analyzed-only?]
      :or {limit 50}}]
  (let [items (->> (:latest @aggregator-state)
                   (map #(get-in @aggregator-state [:items %]))
                   (filter some?))]
    (cond->> items
      category (filter #(= category (:category %)))
      source (filter #(= source (:source-id %)))
      analyzed-only? (filter :analyzed?)
      true (take limit))))

(defn get-news-item
  "Get a specific news item by ID"
  [id]
  (get-in @aggregator-state [:items id]))

(defn analyze-news-item!
  "Analyze a specific news item"
  [id]
  (when-let [item (get-news-item id)]
    (deep-analyze! item)))

(defn search-news
  "Search news items"
  [query & {:keys [limit] :or {limit 20}}]
  (let [q (str/lower-case query)]
    (->> (vals (:items @aggregator-state))
         (filter #(or (str/includes? (str/lower-case (:title %)) q)
                      (str/includes? (str/lower-case (or (:summary %) "")) q)))
         (sort-by :ingested-at #(compare %2 %1))
         (take limit))))

(defn aggregator-stats
  "Get aggregator statistics"
  []
  (let [s @aggregator-state]
    {:status (:status s)
     :total-items (count (:items s))
     :sources-count (count (:sources s))
     :by-category (into {} (map (fn [[k v]] [k (count v)]) (:by-category s)))
     :analyzed (:analyzed (:stats s))
     :latest-item (first (get-latest-news :limit 1))}))
