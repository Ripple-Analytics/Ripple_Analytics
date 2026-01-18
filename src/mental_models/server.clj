(ns mental-models.server
  "Electric Clojure server setup with Ring/Jetty"
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.resource :refer [wrap-resource]]
            [hyperfiddle.electric-ring-adapter :as electric-ring]
            [mental-models.db.core :as db]
            [taoensso.timbre :as log]
            [environ.core :refer [env]]))

;; -- Configuration -----------------------------------------------------------

(def config
  {:port (Integer/parseInt (or (env :port) "8080"))
   :host (or (env :host) "0.0.0.0")
   :db-url (env :database-url)})

;; -- Electric Handler --------------------------------------------------------

(defn electric-handler [ring-request]
  (electric-ring/handle-electric-request
   ring-request
   {:entrypoint 'mental-models.main/App}))

;; -- Ring Application --------------------------------------------------------

(defn wrap-electric [handler]
  (fn [request]
    (if (= "/electric" (:uri request))
      (electric-handler request)
      (handler request))))

(def app
  (-> (fn [_] {:status 200 
               :headers {"Content-Type" "text/html"}
               :body (slurp (clojure.java.io/resource "public/index.html"))})
      (wrap-electric)
      (wrap-resource "public")
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))))

;; -- Server Lifecycle --------------------------------------------------------

(defonce server (atom nil))

(defn start! []
  (log/info "Initializing database...")
  (db/init!)
  
  (log/info "Starting Electric server on port" (:port config))
  (reset! server
          (jetty/run-jetty app
                           {:port (:port config)
                            :host (:host config)
                            :join? false}))
  (log/info "Server running at http://localhost:" (:port config)))

(defn stop! []
  (when @server
    (.stop @server)
    (reset! server nil)
    (log/info "Server stopped")))

(defn restart! []
  (stop!)
  (start!))
