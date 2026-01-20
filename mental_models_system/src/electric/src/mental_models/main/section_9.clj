(ns mental-models.main.section-9
  "Main Module - Section 9"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

;; ============================================

(defn init-distributed-systems!
  "Initialize distributed processing and continuous learning systems.
   Called on server startup if enabled via environment variables."
  []
  (let [enable-distributed (= "true" (System/getenv "ENABLE_DISTRIBUTED"))
        enable-continuous (= "true" (System/getenv "ENABLE_CONTINUOUS"))
        enable-db (= "true" (System/getenv "ENABLE_DATABASE"))
        db-url (System/getenv "DATABASE_URL")]
    
    ;; Initialize database if enabled and URL provided
    (when (and enable-db db-url)
      (println "Initializing database connection...")
      (try
        (db/init-pool! {:jdbcUrl db-url})
        (db/init-schema!)
        (println "Database initialized successfully")
        (catch Exception e
          (println "Warning: Database initialization failed:" (.getMessage e)))))
    
    ;; Initialize distributed processing if enabled
    (when enable-distributed
      (println "Initializing distributed processing system...")
      (distributed/init!)
      (let [num-workers (Integer/parseInt (or (System/getenv "NUM_WORKERS") "4"))]
        (distributed/start-workers! num-workers)
        (println (str "Started " num-workers " distributed workers"))))
    
    ;; Initialize continuous learning if enabled
    (when enable-continuous
      (println "Initializing continuous learning system...")
      (continuous/start-all-systems!)
      (println "Continuous learning system started"))))

(defn start-server [& {:keys [port] :or {port 8000}}]
  (println "")
  (println "========================================")
  (println "  Mental Models System - Electric Clojure")
  (println "  Petabyte-Scale Distributed Processing")
  (println "========================================")
  (println "")
  (println (str "Starting server on port " port "..."))
  (println (str "Models loaded: " (count @models/!models)))
  (println (str "Failure modes: " (count @models/!failure-modes)))
  (println (str "Categories: " (count @models/!categories)))
  (println "")
  
  ;; Initialize distributed systems
  (init-distributed-systems!)
  
  (println "")
  (println "API Endpoints:")
  (println "  /api/distributed/status    - Get cluster metrics")
  (println "  /api/distributed/submit    - Submit work to cluster")
  (println "  /api/continuous/status     - Get continuous learning status")
  (println "  /api/continuous/start      - Start all continuous systems")
  (println "  /api/db/health             - Check database health")
  (println "")
  (println (str "Open http://localhost:" port " in your browser"))
  (println "")
  (jetty/run-jetty (wrap-defaults app site-defaults)
                   {:port port :join? false}))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8000"))]
    (start-server :port port)))

