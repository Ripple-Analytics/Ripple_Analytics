(ns mental-models.main.section-7-part2
  "Main Module - Section 7 Part2"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

            data-source (get body "source")
            parallel-factor (get body "parallel" 1000)]
        (json-response (continuous/process-petabyte-dataset! data-source :parallel-factor parallel-factor)))
      
      ;; Database API
      (and (= method :get) (= uri "/api/db/health"))
      (json-response (db/health-check))
      
      (and (= method :get) (= uri "/api/db/stats"))
      (json-response (db/get-stats))
      
      (and (= method :post) (= uri "/api/db/analysis"))
      (let [body (parse-json-body request)]
        (db/save-analysis! body)
        (json-response {:status "saved"}))
      
      (and (= method :get) (= uri "/api/db/analyses"))
      (let [params (:query-params request)
            limit (Integer/parseInt (get params "limit" "100"))]
        (json-response {:analyses (db/get-analyses :limit limit)}))
      
      ;; 404
      :else
      (-> (response/response "Not found")
          (response/status 404)))))

