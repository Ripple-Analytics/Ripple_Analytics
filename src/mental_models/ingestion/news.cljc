(ns mental-models.ingestion.news
  "News and data ingestion system for mental model classification.
   Gathers real-world reports from multiple sources and classifies
   each situation using the full latticework of mental models."
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [clojure.string :as str]
            #?(:clj [clj-http.client :as http])
            #?(:clj [clojure.data.xml :as xml])
            #?(:clj [clojure.data.json :as json])
            [mental-models.models.unified-detector :as detector]
            [mental-models.db.core :as db]))

;; =============================================================================
;; REACTIVE STATE
;; =============================================================================

(e/def !news-queue (atom []))           ; Pending items to classify
(e/def !classified-items (atom []))     ; Classified results
(e/def !active-sources (atom #{}))      ; Currently active data sources
(e/def !ingestion-stats (atom {:total 0 :classified 0 :lollapalooza 0}))

;; =============================================================================
;; DATA SOURCE DEFINITIONS
;; =============================================================================

(def news-sources
  "Major news sources with RSS/Atom feeds"
  {:reuters {:name "Reuters"
             :feeds [{:url "https://feeds.reuters.com/reuters/businessNews"
                      :category :business}
                     {:url "https://feeds.reuters.com/reuters/topNews"
                      :category :general}]}
   :wsj {:name "Wall Street Journal"
         :feeds [{:url "https://feeds.a]wsj.com/rss/RSSMarketsMain.xml"
                  :category :markets}
                 {:url "https://feeds.a.wsj.com/rss/RSSWorldNews.xml"
                  :category :world}]}
   :ft {:name "Financial Times"
        :feeds [{:url "https://www.ft.com/rss/home"
                 :category :general}]}
   :bloomberg {:name "Bloomberg"
               :feeds [{:url "https://feeds.bloomberg.com/markets/news.rss"
                        :category :markets}]}
   :economist {:name "The Economist"
               :feeds [{:url "https://www.economist.com/finance-and-economics/rss.xml"
                        :category :economics}]}
   :hn {:name "Hacker News"
        :feeds [{:url "https://news.ycombinator.com/rss"
                 :category :tech}]}
   :sec {:name "SEC EDGAR"
         :type :api
         :endpoint "https://www.sec.gov/cgi-bin/browse-edgar"}})

(def sec-filing-types
  "SEC filing types to monitor"
  {:10-K "Annual report"
   :10-Q "Quarterly report"
   :8-K  "Current report (material events)"
   :DEF-14A "Proxy statement"
   :13-F "Institutional holdings"
   :4    "Insider trading"
   :S-1  "IPO registration"})

;; =============================================================================
;; RSS/ATOM FEED PARSING
;; =============================================================================

#?(:clj
   (defn fetch-feed
     "Fetch and parse an RSS/Atom feed"
     [url]
     (try
       (let [response (http/get url {:headers {"User-Agent" "MentalModels/1.0"}
                                     :socket-timeout 10000
                                     :connection-timeout 10000})
             body (:body response)
             parsed (xml/parse-str body)]
         {:success true
          :items (parse-feed-items parsed)})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn parse-feed-items
     "Extract items from parsed RSS/Atom XML"
     [parsed-xml]
     (let [tag-name (name (:tag parsed-xml))]
       (cond
         ;; RSS 2.0
         (= tag-name "rss")
         (let [channel (first (filter #(= :channel (:tag %)) (:content parsed-xml)))
               items (filter #(= :item (:tag %)) (:content channel))]
           (map parse-rss-item items))
         
         ;; Atom
         (= tag-name "feed")
         (let [entries (filter #(= :entry (:tag %)) (:content parsed-xml))]
           (map parse-atom-entry entries))
         
         :else []))))

#?(:clj
   (defn parse-rss-item
     "Parse a single RSS item"
     [item]
     (let [content (:content item)
           get-text (fn [tag]
                      (-> (filter #(= tag (:tag %)) content)
                          first :content first))]
       {:title (get-text :title)
        :link (get-text :link)
        :description (get-text :description)
        :pub-date (get-text :pubDate)
        :guid (get-text :guid)
        :type :rss})))

#?(:clj
   (defn parse-atom-entry
     "Parse a single Atom entry"
     [entry]
     (let [content (:content entry)
           get-text (fn [tag]
                      (-> (filter #(= tag (:tag %)) content)
                          first :content first))
           get-attr (fn [tag attr]
                      (-> (filter #(= tag (:tag %)) content)
                          first :attrs attr))]
       {:title (get-text :title)
        :link (get-attr :link :href)
        :summary (get-text :summary)
        :published (get-text :published)
        :id (get-text :id)
        :type :atom})))

;; =============================================================================
;; SEC EDGAR CONNECTOR
;; =============================================================================

#?(:clj
   (defn fetch-sec-filings
     "Fetch recent SEC filings for a company or all companies"
     [{:keys [cik ticker filing-type start-date]}]
     (try
       (let [base-url "https://data.sec.gov/submissions/"
             ;; If CIK provided, fetch company filings
             url (if cik
                   (str base-url "CIK" (format "%010d" (Long/parseLong cik)) ".json")
                   ;; Otherwise fetch recent filings feed
                   "https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&type=&company=&dateb=&owner=include&count=100&output=atom")
             response (http/get url {:headers {"User-Agent" "MentalModels contact@example.com"
                                               "Accept" "application/json"}
                                     :socket-timeout 15000})
             data (json/read-str (:body response) :key-fn keyword)]
         {:success true
          :filings (parse-sec-response data filing-type)})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn parse-sec-response
     "Parse SEC EDGAR JSON response"
     [data filing-type]
     (let [filings (get-in data [:filings :recent])
           forms (get filings :form [])
           dates (get filings :filingDate [])
           accessions (get filings :accessionNumber [])
           descriptions (get filings :primaryDocument [])]
       (->> (map vector forms dates accessions descriptions)
            (filter (fn [[form _ _ _]]
                      (or (nil? filing-type)
                          (= form (name filing-type)))))
            (take 50)
            (map (fn [[form date accession doc]]
                   {:form form
                    :filing-date date
                    :accession-number accession
                    :document doc
                    :url (str "https://www.sec.gov/Archives/edgar/data/"
                              (:cik data) "/" 
                              (str/replace accession "-" "") "/"
                              doc)}))))))

#?(:clj
   (defn fetch-filing-text
     "Fetch the full text of an SEC filing"
     [filing-url]
     (try
       (let [response (http/get filing-url 
                                {:headers {"User-Agent" "MentalModels contact@example.com"}
                                 :socket-timeout 30000})]
         {:success true
          :text (extract-text-from-filing (:body response))})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn extract-text-from-filing
     "Extract readable text from SEC filing HTML"
     [html]
     (-> html
         (str/replace #"<script[^>]*>.*?</script>" "")
         (str/replace #"<style[^>]*>.*?</style>" "")
         (str/replace #"<[^>]+>" " ")
         (str/replace #"&nbsp;" " ")
         (str/replace #"&amp;" "&")
         (str/replace #"&lt;" "<")
         (str/replace #"&gt;" ">")
         (str/replace #"\s+" " ")
         str/trim)))

;; =============================================================================
;; FINANCIAL NEWS API CONNECTORS
;; =============================================================================

#?(:clj
   (defn fetch-alpha-vantage-news
     "Fetch news from Alpha Vantage API (free tier available)"
     [api-key & {:keys [tickers topics]}]
     (try
       (let [url (str "https://www.alphavantage.co/query"
                      "?function=NEWS_SENTIMENT"
                      "&apikey=" api-key
                      (when tickers (str "&tickers=" (str/join "," tickers)))
                      (when topics (str "&topics=" (str/join "," topics))))
             response (http/get url {:socket-timeout 15000})
             data (json/read-str (:body response) :key-fn keyword)]
         {:success true
          :items (map parse-av-news-item (:feed data))})
       (catch Exception e
         {:success false
          :error (.getMessage e)}))))

#?(:clj
   (defn parse-av-news-item
     "Parse Alpha Vantage news item"
     [item]
     {:title (:title item)
      :url (:url item)
      :summary (:summary item)
      :source (:source item)
      :published (:time_published item)
      :sentiment (:overall_sentiment_score item)
      :tickers (map :ticker (:ticker_sentiment item))
      :topics (map :topic (:topics item))}))

;; =============================================================================
;; CLASSIFICATION PIPELINE
;; =============================================================================

(defn prepare-for-classification
  "Prepare a news item for mental model classification"
  [item source-info]
  {:id (str (java.util.UUID/randomUUID))
   :source (:name source-info)
   :source-type (:type source-info :rss)
   :title (:title item)
   :text (or (:description item) 
             (:summary item) 
             (:text item)
             "")
   :url (:link item)
   :published (or (:pub-date item)
                  (:published item)
                  (:filing-date item))
   :metadata (dissoc item :title :description :summary :text :link)
   :ingested-at (System/currentTimeMillis)
   :status :pending})

#?(:clj
   (defn classify-item
     "Run mental model classification on a single item"
     [item]
     (let [text (str (:title item) "\n\n" (:text item))
           ;; Run through unified detector
           result (detector/analyze-text-comprehensive text)
           ;; Extract key findings
           top-models (->> (:model-scores result)
                           (sort-by :confidence >)
                           (take 10))
           lollapalooza? (>= (count (filter #(> (:confidence %) 0.7) top-models)) 3)
           failure-modes (:failure-modes result)]
       (merge item
              {:status :classified
               :classified-at (System/currentTimeMillis)
               :top-models top-models
               :all-scores (:model-scores result)
               :lollapalooza? lollapalooza?
               :lollapalooza-models (when lollapalooza?
                                      (filter #(> (:confidence %) 0.7) top-models))
               :failure-modes failure-modes
               :summary (:summary result)}))))

;; =============================================================================
;; ELECTRIC REACTIVE COMPONENTS
;; =============================================================================

(e/defn NewsIngestionService []
  "Main reactive news ingestion service"
  (e/server
    (let [queue (e/watch !news-queue)
          stats (e/watch !ingestion-stats)]
      
      ;; Process queue items
      (when (seq queue)
        (let [item (first queue)]
          (swap! !news-queue rest)
          (let [classified (e/offload #(classify-item item))]
            (swap! !classified-items conj classified)
            (swap! !ingestion-stats update :classified inc)
            (when (:lollapalooza? classified)
              (swap! !ingestion-stats update :lollapalooza inc)
              ;; Trigger alert
              (e/offload #(alert-lollapalooza classified))))))
      
      ;; Return current stats
      stats)))

(e/defn FeedMonitor [source-key]
  "Monitor a single news source"
  (e/server
    (let [source (get news-sources source-key)
          feeds (:feeds source)]
      (doseq [feed feeds]
        (let [result (e/offload #(fetch-feed (:url feed)))]
          (when (:success result)
            (doseq [item (:items result)]
              (let [prepared (prepare-for-classification item source)]
                (swap! !news-queue conj prepared)
                (swap! !ingestion-stats update :total inc)))))))))

(e/defn SECMonitor [companies]
  "Monitor SEC EDGAR filings for specific companies"
  (e/server
    (doseq [company companies]
      (let [result (e/offload #(fetch-sec-filings {:cik (:cik company)
                                                    :filing-type :8-K}))]
        (when (:success result)
          (doseq [filing (:filings result)]
            (let [text-result (e/offload #(fetch-filing-text (:url filing)))]
              (when (:success text-result)
                (let [prepared (prepare-for-classification
                                 (assoc filing :text (:text text-result))
                                 {:name "SEC EDGAR" :type :sec})]
                  (swap! !news-queue conj prepared)
                  (swap! !ingestion-stats update :total inc))))))))))

(e/defn ClassifiedItemsView []
  "Reactive view of classified items"
  (e/client
    (let [items (e/server (e/watch !classified-items))
          stats (e/server (e/watch !ingestion-stats))]
      (dom/div
        (dom/props {:class "classified-items"})
        
        ;; Stats header
        (dom/div
          (dom/props {:class "stats-bar"})
          (dom/span (dom/text (str "Total: " (:total stats))))
          (dom/span (dom/text (str "Classified: " (:classified stats))))
          (dom/span (dom/text (str "Lollapalooza: " (:lollapalooza stats)))))
        
        ;; Items list
        (e/for [item (take 50 (reverse items))]
          (dom/div
            (dom/props {:class (str "item " (when (:lollapalooza? item) "lollapalooza"))})
            (dom/div
              (dom/props {:class "item-header"})
              (dom/span (dom/props {:class "source"}) (dom/text (:source item)))
              (dom/span (dom/props {:class "title"}) (dom/text (:title item))))
            (dom/div
              (dom/props {:class "models"})
              (e/for [model (take 5 (:top-models item))]
                (dom/span
                  (dom/props {:class "model-badge"
                              :style {:opacity (:confidence model)}})
                  (dom/text (str (:model-name model) 
                                 " (" (int (* 100 (:confidence model))) "%)")))))))))))

;; =============================================================================
;; ALERTING
;; =============================================================================

#?(:clj
   (defn alert-lollapalooza
     "Send alert when Lollapalooza detected"
     [item]
     (let [models (map :model-name (:lollapalooza-models item))
           message (str "🚨 LOLLAPALOOZA DETECTED\n\n"
                        "Source: " (:source item) "\n"
                        "Title: " (:title item) "\n"
                        "URL: " (:url item) "\n\n"
                        "Converging Models:\n"
                        (str/join "\n" (map #(str "• " (:model-name %) 
                                                   " (" (int (* 100 (:confidence %))) "%)")
                                            (:lollapalooza-models item))))]
       ;; Log to database
       (db/insert-alert {:type :lollapalooza
                         :item-id (:id item)
                         :message message
                         :models models
                         :created-at (System/currentTimeMillis)})
       ;; Could also send to Slack, email, etc.
       (println message))))

;; =============================================================================
;; STORAGE
;; =============================================================================

#?(:clj
   (defn store-classified-item
     "Store a classified item in the database"
     [item]
     (db/insert-classified-item
       {:id (:id item)
        :source (:source item)
        :source-type (name (:source-type item))
        :title (:title item)
        :text (:text item)
        :url (:url item)
        :published (:published item)
        :ingested-at (:ingested-at item)
        :classified-at (:classified-at item)
        :top-models (json/write-str (:top-models item))
        :lollapalooza (:lollapalooza? item)
        :lollapalooza-models (when (:lollapalooza? item)
                               (json/write-str (:lollapalooza-models item)))})))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(def feed-schedule
  "How often to check each source (in minutes)"
  {:reuters 15
   :wsj 15
   :ft 30
   :bloomberg 15
   :economist 60
   :hn 10
   :sec 60})

#?(:clj
   (defn start-ingestion-scheduler
     "Start the background ingestion scheduler"
     []
     (let [executor (java.util.concurrent.Executors/newScheduledThreadPool 4)]
       (doseq [[source-key interval] feed-schedule]
         (.scheduleAtFixedRate executor
                               (fn [] 
                                 (try
                                   (println (str "Fetching " (name source-key) "..."))
                                   (let [source (get news-sources source-key)
                                         feeds (:feeds source)]
                                     (doseq [feed feeds]
                                       (let [result (fetch-feed (:url feed))]
                                         (when (:success result)
                                           (doseq [item (:items result)]
                                             (let [prepared (prepare-for-classification item source)
                                                   classified (classify-item prepared)]
                                               (store-classified-item classified)
                                               (when (:lollapalooza? classified)
                                                 (alert-lollapalooza classified))))))))
                                   (catch Exception e
                                     (println (str "Error fetching " (name source-key) ": " (.getMessage e))))))
                               0
                               interval
                               java.util.concurrent.TimeUnit/MINUTES))
       executor)))
