(ns mental-models.pipeline.integration.data-connector
  "Data Pipeline Connector Module
   
   Connects to external data sources for analysis:
   - Database connections (PostgreSQL, MySQL, SQLite)
   - File system sources
   - API endpoints
   - Message queues
   - Cloud storage (S3, GCS)"
  (:require
   [mental-models.pipeline.integration.api-client :as api]
   [mental-models.features.flags :as flags]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [clojure.java.io :as io]
   [clojure.data.json :as json]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]))

;; =============================================================================
;; CONNECTOR TYPES
;; =============================================================================

(def connector-types
  #{:postgresql :mysql :sqlite :filesystem :api :s3 :gcs :kafka :rabbitmq})

;; =============================================================================
;; CONNECTOR STATE
;; =============================================================================

(defonce connectors (atom {}))

;; =============================================================================
;; CONNECTOR PROTOCOL
;; =============================================================================

(defprotocol DataConnector
  (connect! [this] "Establish connection")
  (disconnect! [this] "Close connection")
  (fetch-data [this query] "Fetch data from source")
  (stream-data [this query callback] "Stream data from source")
  (health-check [this] "Check connector health"))

;; =============================================================================
;; DATABASE CONNECTOR
;; =============================================================================

(defrecord DatabaseConnector [id type config datasource]
  DataConnector
  (connect! [this]
    (log/info "Connecting to database" {:id id :type type})
    (let [ds (jdbc/get-datasource config)]
      (assoc this :datasource ds)))
  
  (disconnect! [this]
    (log/info "Disconnecting from database" {:id id})
    (when datasource
      (.close datasource))
    (assoc this :datasource nil))
  
  (fetch-data [this query]
    (when datasource
      (log/debug "Fetching data" {:id id :query query})
      (let [start-time (System/currentTimeMillis)
            results (jdbc/execute! datasource [query]
                                   {:builder-fn rs/as-unqualified-maps})
            duration (- (System/currentTimeMillis) start-time)]
        (metrics/observe-histogram! :data-connector/query-time duration)
        (metrics/inc-counter! :data-connector/queries)
        results)))
  
  (stream-data [this query callback]
    (when datasource
      (log/debug "Streaming data" {:id id :query query})
      (jdbc/execute! datasource [query]
                     {:builder-fn rs/as-unqualified-maps
                      :fetch-size 1000
                      :result-set-fn (fn [rs]
                                       (doseq [row rs]
                                         (callback row)))})))
  
  (health-check [this]
    (try
      (when datasource
        (jdbc/execute-one! datasource ["SELECT 1"]))
      {:healthy true :id id :type type}
      (catch Exception e
        {:healthy false :id id :type type :error (.getMessage e)}))))

;; =============================================================================
;; FILESYSTEM CONNECTOR
;; =============================================================================

(defrecord FilesystemConnector [id config]
  DataConnector
  (connect! [this]
    (log/info "Initializing filesystem connector" {:id id :path (:path config)})
    this)
  
  (disconnect! [this]
    (log/info "Closing filesystem connector" {:id id})
    this)
  
  (fetch-data [this query]
    (let [path (or (:path query) (:path config))
          pattern (or (:pattern query) "*")]
      (log/debug "Fetching files" {:path path :pattern pattern})
      (let [dir (io/file path)
            files (filter #(and (.isFile %)
                                (re-matches (re-pattern (clojure.string/replace pattern "*" ".*"))
                                            (.getName %)))
                          (file-seq dir))]
        (metrics/inc-counter! :data-connector/file-fetches)
        (map (fn [f]
               {:path (.getAbsolutePath f)
                :name (.getName f)
                :size (.length f)
                :modified (.lastModified f)})
             files))))
  
  (stream-data [this query callback]
    (doseq [file (fetch-data this query)]
      (callback file)))
  
  (health-check [this]
    (let [path (:path config)
          dir (io/file path)]
      {:healthy (.exists dir)
       :id id
       :type :filesystem
       :path path})))

;; =============================================================================
;; API CONNECTOR
;; =============================================================================

(defrecord APIConnector [id config]
  DataConnector
  (connect! [this]
    (log/info "Initializing API connector" {:id id :base-url (:base-url config)})
    this)
  
  (disconnect! [this]
    (log/info "Closing API connector" {:id id})
    this)
  
  (fetch-data [this query]
    (let [url (str (:base-url config) (:endpoint query))
          method (or (:method query) :get)
          headers (merge (:headers config) (:headers query))]
      (log/debug "Fetching from API" {:url url :method method})
      (let [response (case method
                       :get (api/get-request url :headers headers)
                       :post (api/post-request url (:body query) :headers headers))]
        (metrics/inc-counter! :data-connector/api-calls)
        (when (< (:status response) 400)
          (:body response)))))
  
  (stream-data [this query callback]
    (let [data (fetch-data this query)]
      (if (sequential? data)
        (doseq [item data]
          (callback item))
        (callback data))))
  
  (health-check [this]
    (try
      (let [health-endpoint (or (:health-endpoint config) "/health")
            response (api/get-request (str (:base-url config) health-endpoint))]
        {:healthy (< (:status response) 400)
         :id id
         :type :api
         :status (:status response)})
      (catch Exception e
        {:healthy false :id id :type :api :error (.getMessage e)}))))

;; =============================================================================
;; CONNECTOR FACTORY
;; =============================================================================

(defn create-connector
  "Create a connector of the specified type."
  [id type config]
  (case type
    (:postgresql :mysql :sqlite)
    (->DatabaseConnector id type config nil)
    
    :filesystem
    (->FilesystemConnector id config)
    
    :api
    (->APIConnector id config)
    
    (throw (ex-info "Unknown connector type" {:type type}))))

;; =============================================================================
;; CONNECTOR MANAGEMENT
;; =============================================================================

(defn register-connector!
  "Register and connect a new data connector."
  [id type config]
  (log/info "Registering connector" {:id id :type type})
  (let [connector (create-connector id type config)
        connected (connect! connector)]
    (swap! connectors assoc id connected)
    (metrics/inc-counter! :data-connector/registered)
    (audit/log-operation! {:operation :connector-registered :id id :type type})
    (events/publish! :connector/registered {:id id :type type})
    connected))

(defn get-connector [id]
  (get @connectors id))

(defn unregister-connector!
  "Disconnect and remove a connector."
  [id]
  (when-let [connector (get-connector id)]
    (disconnect! connector)
    (swap! connectors dissoc id)
    (log/info "Connector unregistered" {:id id})))

(defn list-connectors []
  (map (fn [[id connector]]
         {:id id
          :type (:type connector)
          :healthy (:healthy (health-check connector))})
       @connectors))

;; =============================================================================
;; DATA FETCHING
;; =============================================================================

(defn fetch-from
  "Fetch data from a registered connector."
  [connector-id query]
  (when-let [connector (get-connector connector-id)]
    (fetch-data connector query)))

(defn stream-from
  "Stream data from a registered connector."
  [connector-id query callback]
  (when-let [connector (get-connector connector-id)]
    (stream-data connector query callback)))

;; =============================================================================
;; HEALTH CHECKS
;; =============================================================================

(defn check-all-connectors []
  (map (fn [[id connector]]
         (health-check connector))
       @connectors))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-data-connectors!
  "Initialize data connector system."
  []
  (log/info "Initializing data connectors")
  ;; Register feature flag
  (flags/register-flag! "data-connectors" "Enable data connectors" true)
  ;; Create metrics
  (metrics/create-counter! :data-connector/registered "Connectors registered")
  (metrics/create-counter! :data-connector/queries "Database queries")
  (metrics/create-counter! :data-connector/file-fetches "File fetches")
  (metrics/create-counter! :data-connector/api-calls "API calls")
  (metrics/create-histogram! :data-connector/query-time "Query time" [10 50 100 500 1000 5000])
  (log/info "Data connectors initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-connector-status []
  {:enabled (flags/is-enabled? "data-connectors")
   :connector-count (count @connectors)
   :connectors (list-connectors)})
