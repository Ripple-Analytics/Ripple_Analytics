(ns mental-models.db.core
  "Database layer with connection pooling and query helpers"
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [honey.sql :as hsql]
            [environ.core :refer [env]]
            [taoensso.timbre :as log]))

;; -- Connection Pool ---------------------------------------------------------

(defonce datasource (atom nil))

(defn init! []
  (let [db-url (or (env :database-url)
                   "jdbc:mysql://localhost:3306/mental_models")]
    (reset! datasource
            (jdbc/get-datasource
             {:jdbcUrl db-url
              :maximumPoolSize 10}))
    (log/info "Database connection pool initialized")))

(defn get-connection []
  (when-not @datasource (init!))
  @datasource)

;; -- Query Helpers -----------------------------------------------------------

(defn query
  "Execute a query and return results as maps"
  [sql-vec]
  (jdbc/execute! (get-connection) sql-vec
                 {:builder-fn rs/as-unqualified-maps}))

(defn query-one
  "Execute a query and return first result"
  [sql-vec]
  (first (query sql-vec)))

(defn execute!
  "Execute a statement (insert, update, delete)"
  [sql-vec]
  (jdbc/execute! (get-connection) sql-vec))

(defn insert!
  "Insert a row and return generated keys"
  [table row]
  (sql/insert! (get-connection) table row
               {:return-keys true}))

(defn update!
  "Update rows matching condition"
  [table set-map where-map]
  (sql/update! (get-connection) table set-map where-map))

(defn delete!
  "Delete rows matching condition"
  [table where-map]
  (sql/delete! (get-connection) table where-map))

;; -- HoneySQL Integration ----------------------------------------------------

(defn q
  "Execute HoneySQL query map"
  [query-map]
  (query (hsql/format query-map)))

(defn q-one
  "Execute HoneySQL query map, return first result"
  [query-map]
  (first (q query-map)))

;; -- Mental Models Queries ---------------------------------------------------

(defn get-all-models []
  (q {:select [:m.* :c.name :c.slug :c.color]
      :from [[:mental_models :m]]
      :join [[:categories :c] [:= :m.category_id :c.id]]
      :order-by [:m.name]}))

(defn get-model-by-slug [slug]
  (q-one {:select [:m.* :c.name :c.slug :c.color]
          :from [[:mental_models :m]]
          :join [[:categories :c] [:= :m.category_id :c.id]]
          :where [:= :m.slug slug]}))

(defn get-model-by-id [id]
  (q-one {:select [:m.* :c.name :c.slug :c.color]
          :from [[:mental_models :m]]
          :join [[:categories :c] [:= :m.category_id :c.id]]
          :where [:= :m.id id]}))

(defn get-categories []
  (q {:select [:*]
      :from [:categories]
      :order-by [:display_order]}))

(defn get-models-by-category [category-id]
  (q {:select [:*]
      :from [:mental_models]
      :where [:= :category_id category-id]
      :order-by [:name]}))

(defn search-models [query-str]
  (q {:select [:m.* :c.name :c.color]
      :from [[:mental_models :m]]
      :join [[:categories :c] [:= :m.category_id :c.id]]
      :where [:or
              [:like :m.name (str "%" query-str "%")]
              [:like :m.description (str "%" query-str "%")]
              [:like :m.thinker (str "%" query-str "%")]]
      :order-by [:m.name]}))

;; -- Decisions ---------------------------------------------------------------

(defn get-user-decisions [user-id]
  (q {:select [:*]
      :from [:decisions]
      :where [:= :user_id user-id]
      :order-by [[:created_at :desc]]}))

(defn create-decision! [decision]
  (insert! :decisions decision))

(defn update-decision! [id updates]
  (update! :decisions updates {:id id}))

(defn get-decision-models [decision-id]
  (q {:select [:m.*]
      :from [[:decision_models :dm]]
      :join [[:mental_models :m] [:= :dm.model_id :m.id]]
      :where [:= :dm.decision_id decision-id]}))

;; -- Model Usage & Effectiveness ---------------------------------------------

(defn record-model-usage! [usage]
  (insert! :model_usage usage))

(defn get-model-effectiveness [model-id]
  (q {:select [[:%count.* :total]
               [[:sum [:case [:> :outcome_rating 3] 1 :else 0]] :successes]]
      :from [:model_usage]
      :where [:= :model_id model-id]}))

(defn get-user-model-stats [user-id]
  (q {:select [:model_id
               [:%count.* :usage_count]
               [[:avg :outcome_rating] :avg_outcome]]
      :from [:model_usage]
      :where [:= :user_id user-id]
      :group-by [:model_id]
      :order-by [[:usage_count :desc]]}))

;; -- Failure Modes -----------------------------------------------------------

(defn get-failure-modes [model-id]
  (q {:select [:*]
      :from [:failure_modes]
      :where [:= :model_id model-id]
      :order-by [:risk_level :desc]}))

(defn get-all-failure-modes []
  (q {:select [:fm.* :m.name :m.slug]
      :from [[:failure_modes :fm]]
      :join [[:mental_models :m] [:= :fm.model_id :m.id]]
      :order-by [[:fm.risk_level :desc]]}))

;; -- Signals -----------------------------------------------------------------

(defn get-recent-signals [limit]
  (q {:select [:*]
      :from [:signals]
      :order-by [[:created_at :desc]]
      :limit limit}))

(defn create-signal! [signal]
  (insert! :signals signal))

;; -- Metrics -----------------------------------------------------------------

(defn get-system-metrics []
  {:total-models (-> (q-one {:select [[:%count.* :count]] :from [:mental_models]}) :count)
   :total-decisions (-> (q-one {:select [[:%count.* :count]] :from [:decisions]}) :count)
   :total-users (-> (q-one {:select [[:%count.* :count]] :from [:users]}) :count)
   :total-analyses (-> (q-one {:select [[:%count.* :count]] :from [:analyses]}) :count)})
