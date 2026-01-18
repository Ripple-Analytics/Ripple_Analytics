(ns mental-models.desktop.db
  "SQLite database for local storage of analysis results"
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [cheshire.core :as json])
  (:import [com.zaxxer.hikari HikariConfig HikariDataSource]))

;; =============================================================================
;; Database Connection
;; =============================================================================

(def db-path (str (System/getProperty "user.home") "/.mental-models/data.db"))

(defonce datasource (atom nil))

(defn get-datasource []
  (when-not @datasource
    (let [config (doto (HikariConfig.)
                   (.setJdbcUrl (str "jdbc:sqlite:" db-path))
                   (.setMaximumPoolSize 5))]
      (reset! datasource (HikariDataSource. config))))
  @datasource)

(defn init! []
  (log/info "Initializing database at:" db-path)
  
  ;; Ensure directory exists
  (io/make-parents db-path)
  
  (let [ds (get-datasource)]
    ;; Create tables
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS analyses (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        file_path TEXT NOT NULL,
        file_name TEXT NOT NULL,
        analyzed_at TEXT NOT NULL,
        mental_models TEXT,
        convergence_score REAL,
        is_lollapalooza INTEGER DEFAULT 0,
        raw_result TEXT,
        created_at TEXT DEFAULT CURRENT_TIMESTAMP
      )"])
    
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS watched_folders (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        path TEXT NOT NULL UNIQUE,
        enabled INTEGER DEFAULT 1,
        last_scan TEXT,
        created_at TEXT DEFAULT CURRENT_TIMESTAMP
      )"])
    
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS config (
        key TEXT PRIMARY KEY,
        value TEXT
      )"])
    
    (jdbc/execute! ds ["
      CREATE INDEX IF NOT EXISTS idx_analyses_file_path ON analyses(file_path)"])
    
    (jdbc/execute! ds ["
      CREATE INDEX IF NOT EXISTS idx_analyses_lollapalooza ON analyses(is_lollapalooza)"])
    
    (log/info "Database initialized successfully")))

;; =============================================================================
;; Analysis Storage
;; =============================================================================

(defn save-analysis! [result]
  (let [ds (get-datasource)
        models (:mental-models result)
        is-lollapalooza (if (>= (count models) 3) 1 0)
        convergence (or (:convergence-score result) 
                        (if (seq models) 
                          (/ (reduce + (map :confidence models)) (count models))
                          0))]
    (jdbc/execute! ds ["
      INSERT INTO analyses (file_path, file_name, analyzed_at, mental_models, 
                           convergence_score, is_lollapalooza, raw_result)
      VALUES (?, ?, ?, ?, ?, ?, ?)"
      (:file-path result)
      (:file-name result)
      (:analyzed-at result)
      (json/generate-string models)
      convergence
      is-lollapalooza
      (json/generate-string result)])))

(defn get-analysis [file-path]
  (let [ds (get-datasource)]
    (jdbc/execute-one! ds ["SELECT * FROM analyses WHERE file_path = ? ORDER BY id DESC LIMIT 1" file-path]
                       {:builder-fn rs/as-unqualified-maps})))

(defn get-recent-analyses [limit]
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["SELECT * FROM analyses ORDER BY id DESC LIMIT ?" limit]
                   {:builder-fn rs/as-unqualified-maps})))

(defn get-lollapaloozas [limit]
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["SELECT * FROM analyses WHERE is_lollapalooza = 1 ORDER BY id DESC LIMIT ?" limit]
                   {:builder-fn rs/as-unqualified-maps})))

;; =============================================================================
;; Statistics
;; =============================================================================

(defn get-stats []
  (let [ds (get-datasource)
        total (-> (jdbc/execute-one! ds ["SELECT COUNT(*) as cnt FROM analyses"])
                  :cnt)
        successful (-> (jdbc/execute-one! ds ["SELECT COUNT(*) as cnt FROM analyses WHERE raw_result NOT LIKE '%error%'"])
                       :cnt)
        lollapaloozas (-> (jdbc/execute-one! ds ["SELECT COUNT(*) as cnt FROM analyses WHERE is_lollapalooza = 1"])
                          :cnt)]
    {:total total
     :successful successful
     :failed (- total successful)
     :lollapaloozas lollapaloozas}))

;; =============================================================================
;; Watched Folders
;; =============================================================================

(defn add-watched-folder! [path]
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["INSERT OR IGNORE INTO watched_folders (path) VALUES (?)" path])))

(defn remove-watched-folder! [id]
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["DELETE FROM watched_folders WHERE id = ?" id])))

(defn get-watched-folders []
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["SELECT * FROM watched_folders ORDER BY id"]
                   {:builder-fn rs/as-unqualified-maps})))

;; =============================================================================
;; Configuration
;; =============================================================================

(defn set-config! [key value]
  (let [ds (get-datasource)]
    (jdbc/execute! ds ["INSERT OR REPLACE INTO config (key, value) VALUES (?, ?)"
                       (name key) (str value)])))

(defn get-config [key]
  (let [ds (get-datasource)]
    (-> (jdbc/execute-one! ds ["SELECT value FROM config WHERE key = ?" (name key)])
        :value)))

(defn get-all-config []
  (let [ds (get-datasource)]
    (->> (jdbc/execute! ds ["SELECT * FROM config"]
                        {:builder-fn rs/as-unqualified-maps})
         (map (fn [{:keys [key value]}] [(keyword key) value]))
         (into {}))))
