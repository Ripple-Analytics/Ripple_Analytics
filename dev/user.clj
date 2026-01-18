(ns user
  "Development namespace with hot reload and REPL utilities"
  (:require [clojure.tools.namespace.repl :as repl]
            [clojure.java.io :as io]))

;; -- Hot Reload Setup --------------------------------------------------------

(repl/set-refresh-dirs "src")

(defn reset
  "Reset the system - reload all changed namespaces"
  []
  (repl/refresh))

(defn reset-all
  "Full reset - reload all namespaces"
  []
  (repl/refresh-all))

;; -- Development Utilities ---------------------------------------------------

(defn start
  "Start the development server"
  []
  (require 'mental-models.server)
  ((resolve 'mental-models.server/start!) {:port 8080 :dev true})
  (println "Server started on http://localhost:8080"))

(defn stop
  "Stop the development server"
  []
  (when-let [stop-fn (resolve 'mental-models.server/stop!)]
    (stop-fn)
    (println "Server stopped")))

(defn restart
  "Restart the server with fresh code"
  []
  (stop)
  (reset)
  (start))

(defn go
  "Start fresh development session"
  []
  (reset)
  (start))

;; -- REPL Helpers ------------------------------------------------------------

(defn find-ns-by-prefix
  "Find all namespaces matching a prefix"
  [prefix]
  (->> (all-ns)
       (map ns-name)
       (filter #(clojure.string/starts-with? (str %) prefix))
       (sort)))

(defn list-public-fns
  "List all public functions in a namespace"
  [ns-sym]
  (->> (ns-publics ns-sym)
       (keys)
       (sort)))

(defn doc-all
  "Print docs for all public functions in a namespace"
  [ns-sym]
  (doseq [sym (list-public-fns ns-sym)]
    (println "---" sym "---")
    (println (:doc (meta (ns-resolve ns-sym sym))))
    (println)))

;; -- Database Utilities ------------------------------------------------------

(defn db-status
  "Check database connection status"
  []
  (require 'mental-models.db.postgres)
  (let [check-fn (resolve 'mental-models.db.postgres/health-check)]
    (if check-fn
      (check-fn)
      {:status :unknown :message "health-check not found"})))

(defn db-migrate!
  "Run database migrations"
  []
  (require 'mental-models.db.postgres)
  (when-let [migrate-fn (resolve 'mental-models.db.postgres/migrate!)]
    (migrate-fn)))

(defn db-seed!
  "Seed database with sample data"
  []
  (require 'mental-models.db.postgres)
  (when-let [seed-fn (resolve 'mental-models.db.postgres/seed!)]
    (seed-fn)))

;; -- Analysis Utilities ------------------------------------------------------

(defn analyze-codebase
  "Run the autonomous refactoring analysis"
  []
  (load-file "scripts/run_refactor.clj"))

(defn count-loc
  "Count lines of code in src directory"
  []
  (let [files (->> (file-seq (io/file "src"))
                   (filter #(.isFile %))
                   (filter #(re-matches #".*\.clj[cs]?$" (.getName %))))]
    {:files (count files)
     :loc (reduce + (map #(count (line-seq (io/reader %))) files))}))

;; -- Electric Clojure Development --------------------------------------------

(defn electric-dev
  "Start Electric Clojure in development mode"
  []
  (println "Starting Electric Clojure development server...")
  (require 'mental-models.main)
  ((resolve 'mental-models.main/start-dev!)))

;; -- Startup Message ---------------------------------------------------------

(println "")
(println "╔══════════════════════════════════════════════════════════════╗")
(println "║     Mental Models System - Development REPL                  ║")
(println "╠══════════════════════════════════════════════════════════════╣")
(println "║  Commands:                                                   ║")
(println "║    (go)           - Start fresh development session          ║")
(println "║    (reset)        - Reload changed namespaces                ║")
(println "║    (restart)      - Restart server with fresh code           ║")
(println "║    (stop)         - Stop the server                          ║")
(println "║    (db-status)    - Check database connection                ║")
(println "║    (db-migrate!)  - Run database migrations                  ║")
(println "║    (analyze-codebase) - Run autonomous refactoring           ║")
(println "║    (count-loc)    - Count lines of code                      ║")
(println "╚══════════════════════════════════════════════════════════════╝")
(println "")
