(ns mental-models.db
  "Database persistence layer for Mental Models System.
   
   Uses next.jdbc with PostgreSQL for storing:
   - Analysis results
   - User sessions
   - Model usage statistics
   - Learning history
   
   Supports connection pooling via HikariCP."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [honey.sql :as hsql]
            [honey.sql.helpers :as h]
            [clojure.tools.logging :as log])
  (:import [com.zaxxer.hikari HikariConfig HikariDataSource]))

;; ============================================
;; Configuration
;; ============================================

(def db-config
  "Database configuration from environment variables."
  {:dbtype "postgresql"
   :host (or (System/getenv "DB_HOST") "localhost")
   :port (Integer/parseInt (or (System/getenv "DB_PORT") "5432"))
   :dbname (or (System/getenv "DB_NAME") "mental_models")
   :user (or (System/getenv "DB_USER") "postgres")
   :password (or (System/getenv "DB_PASSWORD") "postgres")})

(def pool-config
  "Connection pool configuration."
  {:minimum-idle 2
   :maximum-pool-size 10
   :idle-timeout 300000
   :max-lifetime 1800000
   :connection-timeout 30000})

;; ============================================
;; Connection Pool
;; ============================================

(defonce ^:private datasource (atom nil))

(defn- create-hikari-config
  "Create HikariCP configuration."
  [db-config pool-config]
  (doto (HikariConfig.)
    (.setJdbcUrl (str "jdbc:postgresql://" 
                      (:host db-config) ":" (:port db-config) 
                      "/" (:dbname db-config)))
    (.setUsername (:user db-config))
    (.setPassword (:password db-config))
    (.setMinimumIdle (:minimum-idle pool-config))
    (.setMaximumPoolSize (:maximum-pool-size pool-config))
    (.setIdleTimeout (:idle-timeout pool-config))
    (.setMaxLifetime (:max-lifetime pool-config))
    (.setConnectionTimeout (:connection-timeout pool-config))))

(defn init-pool!
  "Initialize the connection pool."
  []
  (when-not @datasource
    (try
      (let [config (create-hikari-config db-config pool-config)
            ds (HikariDataSource. config)]
        (reset! datasource ds)
        (log/info "Database connection pool initialized"))
      (catch Exception e
        (log/warn "Could not initialize database pool:" (.getMessage e))
        nil))))

(defn close-pool!
  "Close the connection pool."
  []
  (when-let [ds @datasource]
    (.close ds)
    (reset! datasource nil)
    (log/info "Database connection pool closed")))

(defn get-datasource
  "Get the datasource, initializing if needed."
  []
  (or @datasource
      (do (init-pool!)
          @datasource)))

;; ============================================
;; Schema Management
;; ============================================

(def schema-sql
  "SQL statements to create the database schema."
  ["-- Analyses table
    CREATE TABLE IF NOT EXISTS analyses (
      id SERIAL PRIMARY KEY,
      analysis_type VARCHAR(50) NOT NULL,
      input_text TEXT,
      models_used TEXT[],
      result JSONB NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      session_id VARCHAR(100)
    )"
   
   "-- Model usage statistics
    CREATE TABLE IF NOT EXISTS model_usage (
      id SERIAL PRIMARY KEY,
      model_name VARCHAR(100) NOT NULL,
      usage_count INTEGER DEFAULT 1,
      last_used TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      context VARCHAR(255)
    )"
   
   "-- Learning history
    CREATE TABLE IF NOT EXISTS learning_history (
      id SERIAL PRIMARY KEY,
      source_type VARCHAR(50) NOT NULL,
      source_path TEXT,
      content_hash VARCHAR(64),
      models_detected TEXT[],
      insights JSONB,
      processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"
   
   "-- User sessions
    CREATE TABLE IF NOT EXISTS sessions (
      id VARCHAR(100) PRIMARY KEY,
      user_id VARCHAR(100),
      started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_activity TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      metadata JSONB
    )"
   
   "-- Feedback and ratings
    CREATE TABLE IF NOT EXISTS feedback (
      id SERIAL PRIMARY KEY,
      analysis_id INTEGER REFERENCES analyses(id),
      rating INTEGER CHECK (rating >= 1 AND rating <= 5),
      comment TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"
   
   "-- Tech debt tracking
    CREATE TABLE IF NOT EXISTS tech_debt_analyses (
      id SERIAL PRIMARY KEY,
      codebase_name VARCHAR(255),
      dag_snapshot JSONB,
      tangles_detected INTEGER,
      refactoring_plan JSONB,
      analyzed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"
   
   "-- Indexes
    CREATE INDEX IF NOT EXISTS idx_analyses_type ON analyses(analysis_type)"
   "CREATE INDEX IF NOT EXISTS idx_analyses_created ON analyses(created_at)"
   "CREATE INDEX IF NOT EXISTS idx_model_usage_name ON model_usage(model_name)"
   "CREATE INDEX IF NOT EXISTS idx_learning_source ON learning_history(source_type)"])

(defn init-schema!
  "Initialize the database schema."
  []
  (when-let [ds (get-datasource)]
    (try
      (doseq [sql-stmt schema-sql]
        (jdbc/execute! ds [sql-stmt]))
      (log/info "Database schema initialized")
      true
      (catch Exception e
        (log/error "Failed to initialize schema:" (.getMessage e))
        false))))

;; ============================================
;; Analysis Operations
;; ============================================

(defn save-analysis!
  "Save an analysis result to the database."
  [{:keys [analysis-type input-text models-used result session-id]}]
  (when-let [ds (get-datasource)]
    (try
      (sql/insert! ds :analyses
                   {:analysis_type analysis-type
                    :input_text input-text
                    :models_used (into-array String models-used)
                    :result (cheshire.core/generate-string result)
                    :session_id session-id})
      (catch Exception e
        (log/error "Failed to save analysis:" (.getMessage e))
        nil))))

(defn get-analyses
  "Get analyses with optional filters."
  [& {:keys [analysis-type limit offset session-id]
      :or {limit 100 offset 0}}]
  (when-let [ds (get-datasource)]
    (try
      (let [query (cond-> (-> (h/select :*)
                              (h/from :analyses)
                              (h/order-by [:created_at :desc])
                              (h/limit limit)
                              (h/offset offset))
                    analysis-type (h/where [:= :analysis_type analysis-type])
                    session-id (h/where [:= :session_id session-id]))]
        (jdbc/execute! ds (hsql/format query)
                       {:builder-fn rs/as-unqualified-maps}))
      (catch Exception e
        (log/error "Failed to get analyses:" (.getMessage e))
        []))))

(defn get-analysis-by-id
  "Get a specific analysis by ID."
  [id]
  (when-let [ds (get-datasource)]
    (try
      (sql/get-by-id ds :analyses id)
      (catch Exception e
        (log/error "Failed to get analysis:" (.getMessage e))
        nil))))

;; ============================================
;; Model Usage Operations
;; ============================================

(defn record-model-usage!
  "Record usage of a mental model."
  [model-name & {:keys [context]}]
  (when-let [ds (get-datasource)]
    (try
      (jdbc/execute! ds
                     ["INSERT INTO model_usage (model_name, context, last_used)
                       VALUES (?, ?, CURRENT_TIMESTAMP)
                       ON CONFLICT (model_name) DO UPDATE
                       SET usage_count = model_usage.usage_count + 1,
                           last_used = CURRENT_TIMESTAMP"
                      model-name context])
      (catch Exception e
        (log/error "Failed to record model usage:" (.getMessage e))
        nil))))

(defn get-model-usage-stats
  "Get usage statistics for models."
  [& {:keys [limit] :or {limit 50}}]
  (when-let [ds (get-datasource)]
    (try
      (jdbc/execute! ds
                     ["SELECT model_name, usage_count, last_used
                       FROM model_usage
                       ORDER BY usage_count DESC
                       LIMIT ?" limit]
                     {:builder-fn rs/as-unqualified-maps})
      (catch Exception e
        (log/error "Failed to get model usage stats:" (.getMessage e))
        []))))

(defn get-most-used-models
  "Get the most frequently used models."
  [n]
  (take n (get-model-usage-stats :limit n)))

;; ============================================
;; Learning History Operations
;; ============================================

(defn save-learning!
  "Save a learning history entry."
  [{:keys [source-type source-path content-hash models-detected insights]}]
  (when-let [ds (get-datasource)]
    (try
      (sql/insert! ds :learning_history
                   {:source_type source-type
                    :source_path source-path
                    :content_hash content-hash
                    :models_detected (into-array String models-detected)
                    :insights (cheshire.core/generate-string insights)})
      (catch Exception e
        (log/error "Failed to save learning:" (.getMessage e))
        nil))))

(defn get-learning-history
  "Get learning history entries."
  [& {:keys [source-type limit] :or {limit 100}}]
  (when-let [ds (get-datasource)]
    (try
      (let [query (cond-> (-> (h/select :*)
                              (h/from :learning_history)
                              (h/order-by [:processed_at :desc])
                              (h/limit limit))
                    source-type (h/where [:= :source_type source-type]))]
        (jdbc/execute! ds (hsql/format query)
                       {:builder-fn rs/as-unqualified-maps}))
      (catch Exception e
        (log/error "Failed to get learning history:" (.getMessage e))
        []))))

(defn content-already-processed?
  "Check if content has already been processed."
  [content-hash]
  (when-let [ds (get-datasource)]
    (try
      (let [result (jdbc/execute-one! ds
                                      ["SELECT id FROM learning_history WHERE content_hash = ?" content-hash])]
        (some? result))
      (catch Exception e
        (log/error "Failed to check content hash:" (.getMessage e))
        false))))

;; ============================================
;; Session Operations
;; ============================================

(defn create-session!
  "Create a new session."
  [session-id & {:keys [user-id metadata]}]
  (when-let [ds (get-datasource)]
    (try
      (sql/insert! ds :sessions
                   {:id session-id
                    :user_id user-id
                    :metadata (when metadata (cheshire.core/generate-string metadata))})
      (catch Exception e
        (log/error "Failed to create session:" (.getMessage e))
        nil))))

(defn update-session-activity!
  "Update the last activity timestamp for a session."
  [session-id]
  (when-let [ds (get-datasource)]
    (try
      (jdbc/execute! ds
                     ["UPDATE sessions SET last_activity = CURRENT_TIMESTAMP WHERE id = ?" session-id])
      (catch Exception e
        (log/error "Failed to update session:" (.getMessage e))
        nil))))

(defn get-session
  "Get a session by ID."
  [session-id]
  (when-let [ds (get-datasource)]
    (try
      (sql/get-by-id ds :sessions session-id)
      (catch Exception e
        (log/error "Failed to get session:" (.getMessage e))
        nil))))

;; ============================================
;; Feedback Operations
;; ============================================

(defn save-feedback!
  "Save feedback for an analysis."
  [{:keys [analysis-id rating comment]}]
  (when-let [ds (get-datasource)]
    (try
      (sql/insert! ds :feedback
                   {:analysis_id analysis-id
                    :rating rating
                    :comment comment})
      (catch Exception e
        (log/error "Failed to save feedback:" (.getMessage e))
        nil))))

(defn get-feedback-stats
  "Get feedback statistics."
  []
  (when-let [ds (get-datasource)]
    (try
      (jdbc/execute-one! ds
                         ["SELECT 
                            COUNT(*) as total_feedback,
                            AVG(rating) as avg_rating,
                            COUNT(CASE WHEN rating >= 4 THEN 1 END) as positive_count,
                            COUNT(CASE WHEN rating <= 2 THEN 1 END) as negative_count
                          FROM feedback"]
                         {:builder-fn rs/as-unqualified-maps})
      (catch Exception e
        (log/error "Failed to get feedback stats:" (.getMessage e))
        nil))))

;; ============================================
;; Tech Debt Operations
;; ============================================

(defn save-tech-debt-analysis!
  "Save a tech debt analysis."
  [{:keys [codebase-name dag-snapshot tangles-detected refactoring-plan]}]
  (when-let [ds (get-datasource)]
    (try
      (sql/insert! ds :tech_debt_analyses
                   {:codebase_name codebase-name
                    :dag_snapshot (cheshire.core/generate-string dag-snapshot)
                    :tangles_detected tangles-detected
                    :refactoring_plan (cheshire.core/generate-string refactoring-plan)})
      (catch Exception e
        (log/error "Failed to save tech debt analysis:" (.getMessage e))
        nil))))

(defn get-tech-debt-history
  "Get tech debt analysis history for a codebase."
  [codebase-name & {:keys [limit] :or {limit 10}}]
  (when-let [ds (get-datasource)]
    (try
      (jdbc/execute! ds
                     ["SELECT * FROM tech_debt_analyses
                       WHERE codebase_name = ?
                       ORDER BY analyzed_at DESC
                       LIMIT ?" codebase-name limit]
                     {:builder-fn rs/as-unqualified-maps})
      (catch Exception e
        (log/error "Failed to get tech debt history:" (.getMessage e))
        []))))

;; ============================================
;; Utility Functions
;; ============================================

(defn health-check
  "Check database connectivity."
  []
  (if-let [ds (get-datasource)]
    (try
      (jdbc/execute-one! ds ["SELECT 1"])
      {:status "healthy" :connected true}
      (catch Exception e
        {:status "unhealthy" :connected false :error (.getMessage e)}))
    {:status "unhealthy" :connected false :error "No datasource"}))

(defn get-stats
  "Get database statistics."
  []
  (when-let [ds (get-datasource)]
    (try
      {:analyses (jdbc/execute-one! ds ["SELECT COUNT(*) as count FROM analyses"]
                                    {:builder-fn rs/as-unqualified-maps})
       :model-usage (jdbc/execute-one! ds ["SELECT COUNT(*) as count FROM model_usage"]
                                       {:builder-fn rs/as-unqualified-maps})
       :learning-history (jdbc/execute-one! ds ["SELECT COUNT(*) as count FROM learning_history"]
                                            {:builder-fn rs/as-unqualified-maps})
       :sessions (jdbc/execute-one! ds ["SELECT COUNT(*) as count FROM sessions"]
                                    {:builder-fn rs/as-unqualified-maps})
       :feedback (get-feedback-stats)}
      (catch Exception e
        (log/error "Failed to get stats:" (.getMessage e))
        nil))))

;; ============================================
;; Initialization
;; ============================================

(defn init!
  "Initialize the database connection and schema."
  []
  (when (init-pool!)
    (init-schema!)
    (log/info "Database initialized successfully")
    true))

(defn shutdown!
  "Shutdown the database connection."
  []
  (close-pool!)
  (log/info "Database shutdown complete"))
