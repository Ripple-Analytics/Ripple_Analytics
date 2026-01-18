(ns mental-models.ingestion.social
  "Social media connectors for real-time sentiment and signal detection.
   Monitors Twitter/X, Reddit, and other social platforms for mental model patterns."
  (:require [hyperfiddle.electric :as e]
            [clojure.string :as str]
            #?(:clj [clj-http.client :as http])
            #?(:clj [clojure.data.json :as json])
            [mental-models.models.unified-detector :as detector]))

;; =============================================================================
;; REACTIVE STATE
;; =============================================================================

(e/def !social-queue (atom []))
(e/def !social-stats (atom {:twitter 0 :reddit 0 :classified 0}))

;; =============================================================================
;; TWITTER/X API CONNECTOR
;; =============================================================================

#?(:clj
   (defn twitter-bearer-token
     "Get Twitter API bearer token from environment"
     []
     (System/getenv "TWITTER_BEARER_TOKEN")))

#?(:clj
   (defn search-twitter
     "Search Twitter/X for relevant tweets"
     [query & {:keys [max-results start-time]}]
     (try
       (let [url "https://api.twitter.com/2/tweets/search/recent"
             params {:query query
                     :max_results (or max-results 100)
                     :tweet.fields "created_at,author_id,public_metrics,context_annotations"
                     :expansions "author_id"
                     :user.fields "name,username,verified"}
             response (http/get url
                               {:headers {"Authorization" (str "Bearer " (twitter-bearer-token))}
                                :query-params params
                                :socket-timeout 15000})
             data (json/read-str (:body response) :key-fn keyword)]
         {:success true
          :tweets (map parse-tweet (:data data))
          :users (into {} (map (fn [u] [(:id u) u]) (get-in data [:includes :users])))})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn parse-tweet
     "Parse a single tweet"
     [tweet]
     {:id (:id tweet)
      :text (:text tweet)
      :created-at (:created_at tweet)
      :author-id (:author_id tweet)
      :metrics (:public_metrics tweet)
      :annotations (:context_annotations tweet)
      :source :twitter}))

#?(:clj
   (defn stream-twitter
     "Connect to Twitter filtered stream (requires elevated access)"
     [rules callback]
     ;; Note: Requires Twitter API v2 elevated access
     ;; This is a simplified version - production would use async streaming
     (let [url "https://api.twitter.com/2/tweets/search/stream"]
       (try
         ;; First, set up rules
         (http/post "https://api.twitter.com/2/tweets/search/stream/rules"
                    {:headers {"Authorization" (str "Bearer " (twitter-bearer-token))
                               "Content-Type" "application/json"}
                     :body (json/write-str {:add (map (fn [r] {:value r}) rules)})})
         ;; Then connect to stream
         (let [response (http/get url
                                  {:headers {"Authorization" (str "Bearer " (twitter-bearer-token))}
                                   :as :stream})]
           ;; Process stream - in production this would be async
           {:success true
            :stream (:body response)})
         (catch Exception e
           {:success false
            :error (.getMessage e)})))))

;; =============================================================================
;; REDDIT API CONNECTOR
;; =============================================================================

#?(:clj
   (defn reddit-auth-token
     "Get Reddit OAuth token"
     []
     (try
       (let [client-id (System/getenv "REDDIT_CLIENT_ID")
             client-secret (System/getenv "REDDIT_CLIENT_SECRET")
             response (http/post "https://www.reddit.com/api/v1/access_token"
                                {:basic-auth [client-id client-secret]
                                 :form-params {:grant_type "client_credentials"}
                                 :headers {"User-Agent" "MentalModels/1.0"}})
             data (json/read-str (:body response) :key-fn keyword)]
         (:access_token data))
       (catch Exception e
         (println "Reddit auth error:" (.getMessage e))
         nil))))

#?(:clj
   (defn fetch-subreddit
     "Fetch posts from a subreddit"
     [subreddit & {:keys [sort limit time-filter]}]
     (try
       (let [token (reddit-auth-token)
             url (str "https://oauth.reddit.com/r/" subreddit "/" (or sort "hot"))
             response (http/get url
                               {:headers {"Authorization" (str "Bearer " token)
                                          "User-Agent" "MentalModels/1.0"}
                                :query-params {:limit (or limit 100)
                                               :t (or time-filter "day")}
                                :socket-timeout 15000})
             data (json/read-str (:body response) :key-fn keyword)]
         {:success true
          :posts (map parse-reddit-post (get-in data [:data :children]))})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn parse-reddit-post
     "Parse a single Reddit post"
     [post]
     (let [data (:data post)]
       {:id (:id data)
        :title (:title data)
        :text (or (:selftext data) "")
        :url (:url data)
        :subreddit (:subreddit data)
        :author (:author data)
        :score (:score data)
        :upvote-ratio (:upvote_ratio data)
        :num-comments (:num_comments data)
        :created-utc (:created_utc data)
        :flair (:link_flair_text data)
        :source :reddit})))

#?(:clj
   (defn fetch-reddit-comments
     "Fetch comments for a Reddit post"
     [subreddit post-id & {:keys [sort limit]}]
     (try
       (let [token (reddit-auth-token)
             url (str "https://oauth.reddit.com/r/" subreddit "/comments/" post-id)
             response (http/get url
                               {:headers {"Authorization" (str "Bearer " token)
                                          "User-Agent" "MentalModels/1.0"}
                                :query-params {:sort (or sort "best")
                                               :limit (or limit 200)}
                                :socket-timeout 15000})
             data (json/read-str (:body response) :key-fn keyword)]
         {:success true
          :comments (flatten-comments (second data))})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn flatten-comments
     "Recursively flatten nested Reddit comments"
     [comment-tree]
     (let [children (get-in comment-tree [:data :children])]
       (mapcat (fn [child]
                 (let [data (:data child)]
                   (if (= (:kind child) "t1")
                     (cons {:id (:id data)
                            :body (:body data)
                            :author (:author data)
                            :score (:score data)
                            :created-utc (:created_utc data)}
                           (when-let [replies (:replies data)]
                             (when (map? replies)
                               (flatten-comments replies))))
                     [])))
               children))))

;; =============================================================================
;; SUBREDDITS TO MONITOR
;; =============================================================================

(def monitored-subreddits
  "Subreddits relevant for mental model detection"
  {:investing {:name "investing" :relevance :high}
   :stocks {:name "stocks" :relevance :high}
   :wallstreetbets {:name "wallstreetbets" :relevance :medium}
   :economics {:name "economics" :relevance :high}
   :business {:name "business" :relevance :high}
   :entrepreneur {:name "entrepreneur" :relevance :medium}
   :startups {:name "startups" :relevance :medium}
   :technology {:name "technology" :relevance :medium}
   :science {:name "science" :relevance :medium}
   :worldnews {:name "worldnews" :relevance :medium}
   :news {:name "news" :relevance :medium}
   :personalfinance {:name "personalfinance" :relevance :medium}
   :financialindependence {:name "financialindependence" :relevance :medium}
   :valueinvesting {:name "ValueInvesting" :relevance :high}
   :securityanalysis {:name "SecurityAnalysis" :relevance :high}})

;; =============================================================================
;; TWITTER SEARCH QUERIES
;; =============================================================================

(def twitter-queries
  "Twitter search queries for mental model detection"
  [;; Investment/Business
   "earnings report"
   "quarterly results"
   "market crash"
   "bubble burst"
   "IPO"
   "merger acquisition"
   "bankruptcy"
   "SEC investigation"
   
   ;; Cognitive biases in action
   "everyone is buying"
   "can't lose"
   "guaranteed returns"
   "this time is different"
   "too big to fail"
   "FOMO"
   
   ;; Decision-making
   "sunk cost"
   "opportunity cost"
   "risk reward"
   "margin of safety"
   
   ;; Specific companies/events
   "Tesla stock"
   "Bitcoin crash"
   "Fed rate decision"
   "inflation data"])

;; =============================================================================
;; CLASSIFICATION PIPELINE
;; =============================================================================

#?(:clj
   (defn classify-social-item
     "Classify a social media item using mental models"
     [item]
     (let [text (str (:title item "") "\n\n" (:text item "") "\n\n" (:body item ""))
           result (detector/analyze-text-comprehensive text)
           top-models (->> (:model-scores result)
                           (sort-by :confidence >)
                           (take 10))
           lollapalooza? (>= (count (filter #(> (:confidence %) 0.7) top-models)) 3)]
       (merge item
              {:classified-at (System/currentTimeMillis)
               :top-models top-models
               :lollapalooza? lollapalooza?
               :sentiment (calculate-sentiment text)
               :urgency (calculate-urgency item)}))))

#?(:clj
   (defn calculate-sentiment
     "Calculate sentiment score for text"
     [text]
     (let [positive-words #{"good" "great" "excellent" "profit" "growth" "bullish" 
                            "opportunity" "success" "win" "gain" "up" "rise"}
           negative-words #{"bad" "terrible" "loss" "crash" "bearish" "risk" 
                            "fail" "down" "fall" "crisis" "panic" "fear"}
           words (-> text str/lower-case (str/split #"\s+") set)
           pos-count (count (clojure.set/intersection words positive-words))
           neg-count (count (clojure.set/intersection words negative-words))
           total (+ pos-count neg-count)]
       (if (zero? total)
         0.0
         (/ (- pos-count neg-count) (float total))))))

#?(:clj
   (defn calculate-urgency
     "Calculate urgency score based on engagement metrics"
     [item]
     (let [source (:source item)]
       (case source
         :twitter
         (let [metrics (:metrics item)
               retweets (get metrics :retweet_count 0)
               likes (get metrics :like_count 0)
               replies (get metrics :reply_count 0)]
           (min 1.0 (/ (+ retweets (* 0.5 likes) (* 2 replies)) 1000.0)))
         
         :reddit
         (let [score (:score item 0)
               comments (:num-comments item 0)
               ratio (:upvote-ratio item 0.5)]
           (min 1.0 (* ratio (/ (+ score comments) 1000.0))))
         
         0.5))))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

(e/defn SocialMonitor []
  "Main social media monitoring service"
  (e/server
    (let [queue (e/watch !social-queue)
          stats (e/watch !social-stats)]
      
      ;; Process queue
      (when (seq queue)
        (let [item (first queue)]
          (swap! !social-queue rest)
          (let [classified (e/offload #(classify-social-item item))]
            (swap! !social-stats update :classified inc)
            classified)))
      
      stats)))

(e/defn TwitterMonitor [queries]
  "Monitor Twitter for specific queries"
  (e/server
    (doseq [query queries]
      (let [result (e/offload #(search-twitter query :max-results 50))]
        (when (:success result)
          (doseq [tweet (:tweets result)]
            (swap! !social-queue conj tweet)
            (swap! !social-stats update :twitter inc)))))))

(e/defn RedditMonitor [subreddits]
  "Monitor Reddit subreddits"
  (e/server
    (doseq [[_ sub-info] subreddits]
      (let [result (e/offload #(fetch-subreddit (:name sub-info) :sort "hot" :limit 25))]
        (when (:success result)
          (doseq [post (:posts result)]
            (swap! !social-queue conj post)
            (swap! !social-stats update :reddit inc)))))))

(e/defn SocialFeedView []
  "Reactive view of social media feed with classifications"
  (e/client
    (let [stats (e/server (e/watch !social-stats))]
      (dom/div
        (dom/props {:class "social-feed"})
        (dom/div
          (dom/props {:class "social-stats"})
          (dom/span (dom/text (str "Twitter: " (:twitter stats))))
          (dom/span (dom/text (str "Reddit: " (:reddit stats))))
          (dom/span (dom/text (str "Classified: " (:classified stats)))))))))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

#?(:clj
   (defn start-social-scheduler
     "Start background social media monitoring"
     []
     (let [executor (java.util.concurrent.Executors/newScheduledThreadPool 2)]
       ;; Twitter every 5 minutes
       (.scheduleAtFixedRate executor
                             (fn []
                               (try
                                 (doseq [query twitter-queries]
                                   (let [result (search-twitter query :max-results 20)]
                                     (when (:success result)
                                       (doseq [tweet (:tweets result)]
                                         (let [classified (classify-social-item tweet)]
                                           ;; Store and alert if needed
                                           (when (:lollapalooza? classified)
                                             (println "LOLLAPALOOZA on Twitter:" (:text tweet))))))))
                                 (catch Exception e
                                   (println "Twitter error:" (.getMessage e)))))
                             0 5 java.util.concurrent.TimeUnit/MINUTES)
       
       ;; Reddit every 10 minutes
       (.scheduleAtFixedRate executor
                             (fn []
                               (try
                                 (doseq [[_ sub-info] monitored-subreddits]
                                   (let [result (fetch-subreddit (:name sub-info) :sort "hot" :limit 10)]
                                     (when (:success result)
                                       (doseq [post (:posts result)]
                                         (let [classified (classify-social-item post)]
                                           (when (:lollapalooza? classified)
                                             (println "LOLLAPALOOZA on Reddit:" (:title post))))))))
                                 (catch Exception e
                                   (println "Reddit error:" (.getMessage e)))))
                             0 10 java.util.concurrent.TimeUnit/MINUTES)
       
       executor)))
