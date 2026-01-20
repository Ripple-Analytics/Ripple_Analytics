(ns mental-models.db.postgres
  "PostgreSQL Database Persistence with next.jdbc
   High-performance connection pooling and query execution"
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [honey.sql :as hsql]
            [honey.sql.helpers :as h]
            [taoensso.timbre :as log])
  (:import [com.zaxxer.hikari HikariConfig HikariDataSource]))

;; -- Connection Pool ---------------------------------------------------------

(def ^:private db-config
  {:jdbcUrl (or (System/getenv "DATABASE_URL")
                "jdbc:postgresql://localhost:5432/mental_models")
   :username (or (System/getenv "DB_USER") "postgres")
   :password (or (System/getenv "DB_PASSWORD") "postgres")
   :maximumPoolSize 10
   :minimumIdle 2
   :connectionTimeout 30000
   :idleTimeout 600000
   :maxLifetime 1800000})

(defonce datasource (atom nil))

(defn init-pool!
  "Initialize HikariCP connection pool"
  []
  (when-not @datasource
    (let [config (doto (HikariConfig.)
                   (.setJdbcUrl (:jdbcUrl db-config))
                   (.setUsername (:username db-config))
                   (.setPassword (:password db-config))
                   (.setMaximumPoolSize (:maximumPoolSize db-config))
                   (.setMinimumIdle (:minimumIdle db-config))
                   (.setConnectionTimeout (:connectionTimeout db-config))
                   (.setIdleTimeout (:idleTimeout db-config))
                   (.setMaxLifetime (:maxLifetime db-config)))]
      (reset! datasource (HikariDataSource. config))
      (log/info "Database connection pool initialized"))))

(defn get-datasource []
  (when-not @datasource (init-pool!))
  @datasource)

(defn close-pool!
  "Close the connection pool"
  []
  (when @datasource
    (.close ^HikariDataSource @datasource)
    (reset! datasource nil)
    (log/info "Database connection pool closed")))

;; -- Query Execution ---------------------------------------------------------

(def ^:private jdbc-opts
  {:builder-fn rs/as-unqualified-kebab-maps})

(defn execute!
  "Execute a SQL statement"
  [sql-params]
  (jdbc/execute! (get-datasource) sql-params jdbc-opts))

(def execute execute!)

(defn execute-one!
  "Execute a SQL statement and return first result"
  [sql-params]
  (jdbc/execute-one! (get-datasource) sql-params jdbc-opts))

(defn query
  "Execute a HoneySQL query or raw SQL with params"
  ([hsql-map]
   (execute! (hsql/format hsql-map)))
  ([sql params]
   (execute! (into [sql] params))))

(defn query-one
  "Execute a HoneySQL query and return first result, or raw SQL with params"
  ([hsql-map]
   (execute-one! (hsql/format hsql-map)))
  ([sql params]
   (execute-one! (into [sql] params))))

;; -- Schema Management -------------------------------------------------------

(def schema-ddl
  "CREATE TABLE IF NOT EXISTS categories (
     id SERIAL PRIMARY KEY,
     name VARCHAR(100) NOT NULL,
     slug VARCHAR(100) UNIQUE NOT NULL,
     color VARCHAR(20) DEFAULT '#525252',
     icon VARCHAR(50),
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS mental_models (
     id SERIAL PRIMARY KEY,
     name VARCHAR(200) NOT NULL,
     slug VARCHAR(200) UNIQUE NOT NULL,
     category_id INTEGER REFERENCES categories(id),
     description TEXT,
     example TEXT,
     thinker VARCHAR(200),
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
     updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS failure_modes (
     id SERIAL PRIMARY KEY,
     model_id INTEGER REFERENCES mental_models(id) ON DELETE CASCADE,
     name VARCHAR(200) NOT NULL,
     description TEXT,
     risk_level VARCHAR(20) DEFAULT 'medium',
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS users (
     id SERIAL PRIMARY KEY,
     open_id VARCHAR(100) UNIQUE NOT NULL,
     name VARCHAR(200),
     email VARCHAR(200),
     role VARCHAR(20) DEFAULT 'user',
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
     last_login TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS decisions (
     id SERIAL PRIMARY KEY,
     user_id INTEGER REFERENCES users(id),
     title VARCHAR(500) NOT NULL,
     context TEXT,
     options TEXT,
     outcome TEXT,
     outcome_rating INTEGER CHECK (outcome_rating >= 1 AND outcome_rating <= 5),
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
     updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS decision_models (
     id SERIAL PRIMARY KEY,
     decision_id INTEGER REFERENCES decisions(id) ON DELETE CASCADE,
     model_id INTEGER REFERENCES mental_models(id),
     effectiveness_rating INTEGER CHECK (effectiveness_rating >= 1 AND effectiveness_rating <= 5),
     notes TEXT,
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS model_usage (
     id SERIAL PRIMARY KEY,
     user_id INTEGER REFERENCES users(id),
     model_id INTEGER REFERENCES mental_models(id),
     decision_id INTEGER REFERENCES decisions(id),
     outcome_rating INTEGER,
     context VARCHAR(100),
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS signals (
     id SERIAL PRIMARY KEY,
     user_id INTEGER REFERENCES users(id),
     source VARCHAR(100) NOT NULL,
     content TEXT NOT NULL,
     models_detected TEXT,
     confidence DECIMAL(3,2),
     processed BOOLEAN DEFAULT FALSE,
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE TABLE IF NOT EXISTS documents (
     id SERIAL PRIMARY KEY,
     user_id INTEGER REFERENCES users(id),
     filename VARCHAR(500) NOT NULL,
     file_type VARCHAR(50),
     content TEXT,
     analysis TEXT,
     models_found TEXT,
     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   
   CREATE INDEX IF NOT EXISTS idx_mental_models_category ON mental_models(category_id);
   CREATE INDEX IF NOT EXISTS idx_mental_models_slug ON mental_models(slug);
   CREATE INDEX IF NOT EXISTS idx_decisions_user ON decisions(user_id);
   CREATE INDEX IF NOT EXISTS idx_decision_models_decision ON decision_models(decision_id);
   CREATE INDEX IF NOT EXISTS idx_model_usage_user ON model_usage(user_id);
   CREATE INDEX IF NOT EXISTS idx_model_usage_model ON model_usage(model_id);
   CREATE INDEX IF NOT EXISTS idx_signals_user ON signals(user_id);")

(defn init-schema!
  "Initialize database schema"
  []
  (try
    (execute! [schema-ddl])
    (log/info "Database schema initialized")
    true
    (catch Exception e
      (log/error e "Failed to initialize schema")
      false)))

;; -- Category Operations -----------------------------------------------------

(defn get-all-categories []
  (query (-> (h/select :*)
             (h/from :categories)
             (h/order-by [:name :asc]))))

(defn get-category-by-id [id]
  (query-one (-> (h/select :*)
                 (h/from :categories)
                 (h/where [:= :id id]))))

(defn upsert-category! [{:keys [name slug color icon]}]
  (execute-one!
   ["INSERT INTO categories (name, slug, color, icon) 
     VALUES (?, ?, ?, ?)
     ON CONFLICT (slug) DO UPDATE SET name = ?, color = ?, icon = ?
     RETURNING *"
    name slug color icon name color icon]))

;; -- Mental Model Operations -------------------------------------------------

(defn get-all-models []
  (query (-> (h/select :*)
             (h/from :mental_models)
             (h/order-by [:name :asc]))))

(defn get-model-by-id [id]
  (query-one (-> (h/select :*)
                 (h/from :mental_models)
                 (h/where [:= :id id]))))

(defn get-model-by-slug [slug]
  (query-one (-> (h/select :*)
                 (h/from :mental_models)
                 (h/where [:= :slug slug]))))

(defn get-models-by-category [category-id]
  (query (-> (h/select :*)
             (h/from :mental_models)
             (h/where [:= :category_id category-id])
             (h/order-by [:name :asc]))))

(defn search-models [q]
  (query (-> (h/select :*)
             (h/from :mental_models)
             (h/where [:or
                       [:ilike :name (str "%" q "%")]
                       [:ilike :description (str "%" q "%")]])
             (h/order-by [:name :asc]))))

(defn upsert-model! [{:keys [name slug category-id description example thinker]}]
  (execute-one!
   ["INSERT INTO mental_models (name, slug, category_id, description, example, thinker)
     VALUES (?, ?, ?, ?, ?, ?)
     ON CONFLICT (slug) DO UPDATE SET 
       name = ?, category_id = ?, description = ?, example = ?, thinker = ?, updated_at = CURRENT_TIMESTAMP
     RETURNING *"
    name slug category-id description example thinker
    name category-id description example thinker]))

;; -- Failure Mode Operations -------------------------------------------------

(defn get-failure-modes [model-id]
  (query (-> (h/select :*)
             (h/from :failure_modes)
             (h/where [:= :model_id model-id]))))

(defn add-failure-mode! [{:keys [model-id name description risk-level]}]
  (execute-one!
   ["INSERT INTO failure_modes (model_id, name, description, risk_level)
     VALUES (?, ?, ?, ?)
     RETURNING *"
    model-id name description risk-level]))

;; -- Decision Operations -----------------------------------------------------

(defn get-user-decisions [user-id & {:keys [limit offset] :or {limit 50 offset 0}}]
  (query (-> (h/select :*)
             (h/from :decisions)
             (h/where [:= :user_id user-id])
             (h/order-by [[:created_at :desc]])
             (h/limit limit)
             (h/offset offset))))

(defn get-decision-by-id [id]
  (query-one (-> (h/select :*)
                 (h/from :decisions)
                 (h/where [:= :id id]))))

(defn create-decision! [{:keys [user-id title context options]}]
  (execute-one!
   ["INSERT INTO decisions (user_id, title, context, options)
     VALUES (?, ?, ?, ?)
     RETURNING *"
    user-id title context options]))

(defn update-decision-outcome! [id {:keys [outcome outcome-rating]}]
  (execute-one!
   ["UPDATE decisions SET outcome = ?, outcome_rating = ?, updated_at = CURRENT_TIMESTAMP
     WHERE id = ?
     RETURNING *"
    outcome outcome-rating id]))

;; -- Model Usage Tracking ----------------------------------------------------

(defn record-model-usage! [{:keys [user-id model-id decision-id outcome-rating context]}]
  (execute-one!
   ["INSERT INTO model_usage (user_id, model_id, decision_id, outcome_rating, context)
     VALUES (?, ?, ?, ?, ?)
     RETURNING *"
    user-id model-id decision-id outcome-rating context]))

(defn get-model-effectiveness [model-id]
  (query-one
   ["SELECT 
       COUNT(*) as usage_count,
       AVG(outcome_rating) as avg_rating,
       COUNT(CASE WHEN outcome_rating >= 4 THEN 1 END) as success_count
     FROM model_usage
     WHERE model_id = ?"
    model-id]))

(defn get-user-model-stats [user-id]
  (query
   ["SELECT 
       m.id, m.name, m.slug,
       COUNT(mu.id) as usage_count,
       AVG(mu.outcome_rating) as avg_rating
     FROM mental_models m
     LEFT JOIN model_usage mu ON m.id = mu.model_id AND mu.user_id = ?
     GROUP BY m.id, m.name, m.slug
     ORDER BY usage_count DESC"
    user-id]))

;; -- Signal Operations -------------------------------------------------------

(defn create-signal! [{:keys [user-id source content models-detected confidence]}]
  (execute-one!
   ["INSERT INTO signals (user_id, source, content, models_detected, confidence)
     VALUES (?, ?, ?, ?, ?)
     RETURNING *"
    user-id source content models-detected confidence]))

(defn get-unprocessed-signals [user-id]
  (query (-> (h/select :*)
             (h/from :signals)
             (h/where [:and
                       [:= :user_id user-id]
                       [:= :processed false]])
             (h/order-by [[:created_at :desc]]))))

(defn mark-signal-processed! [id]
  (execute-one!
   ["UPDATE signals SET processed = true WHERE id = ? RETURNING *" id]))

;; -- Document Operations -----------------------------------------------------

(defn create-document! [{:keys [user-id filename file-type content]}]
  (execute-one!
   ["INSERT INTO documents (user_id, filename, file_type, content)
     VALUES (?, ?, ?, ?)
     RETURNING *"
    user-id filename file-type content]))

(defn update-document-analysis! [id {:keys [analysis models-found]}]
  (execute-one!
   ["UPDATE documents SET analysis = ?, models_found = ? WHERE id = ? RETURNING *"
    analysis models-found id]))

;; -- Analytics ---------------------------------------------------------------

(defn get-dashboard-stats [user-id]
  {:total-decisions (-> (query-one
                         ["SELECT COUNT(*) as count FROM decisions WHERE user_id = ?" user-id])
                        :count)
   :total-models-used (-> (query-one
                           ["SELECT COUNT(DISTINCT model_id) as count FROM model_usage WHERE user_id = ?" user-id])
                          :count)
   :avg-outcome (-> (query-one
                     ["SELECT AVG(outcome_rating) as avg FROM decisions WHERE user_id = ? AND outcome_rating IS NOT NULL" user-id])
                    :avg)
   :recent-activity (query
                     ["SELECT 'decision' as type, title as description, created_at
                       FROM decisions WHERE user_id = ?
                       UNION ALL
                       SELECT 'signal' as type, LEFT(content, 100) as description, created_at
                       FROM signals WHERE user_id = ?
                       ORDER BY created_at DESC LIMIT 10"
                      user-id user-id])})

;; -- Seed Data ---------------------------------------------------------------

(defn seed-from-models!
  "Seed database from the in-memory models data"
  [models-ns]
  (log/info "Seeding database from models data...")
  
  ;; Seed categories
  (doseq [cat (models-ns/get-all-categories)]
    (upsert-category! cat))
  
  ;; Seed models and failure modes
  (doseq [model (models-ns/get-all-models)]
    (let [saved-model (upsert-model! model)]
      (doseq [fm (:failure-modes model)]
        (add-failure-mode! (assoc fm :model-id (:id saved-model))))))
  
  (log/info "Database seeded successfully"))
