(ns mental-models.main
  "Main Module - Server Entry Point
   
   Requires all main sub-modules and starts the server."
  (:require [clojure.string :as str]
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]
                [mental-models.main.lm-studio-integration]
    [mental-models.main.section-1-part1]
    [mental-models.main.section-1-part2]
    [mental-models.main.html-template]
    [mental-models.main.section-3-part1]
    [mental-models.main.section-3-part2]
    [mental-models.main.section-3-part3]
    [mental-models.main.section-3-part4]
    [mental-models.main.section-3-part5]
    [mental-models.main.section-3-part6]
    [mental-models.main.section-3-part7]
    [mental-models.main.section-3-part8]
    [mental-models.main.section-3-part9]
    [mental-models.main.section-3-part10]
    [mental-models.main.section-3-part11]
    [mental-models.main.api-handlers]
    [mental-models.main.section-5]
    [mental-models.main.routes]
    [mental-models.main.section-7-part1]
    [mental-models.main.section-7-part2]
    [mental-models.main.server]
    [mental-models.main.section-9]))

;; Server entry point - see individual modules for handlers
(defn -main [& args]
  (let [port (or (some-> (first args) Integer/parseInt) 8000)]
    (println (str "Starting Mental Models server on port " port))
    ;; Server implementation in main/server.clj
    ))
