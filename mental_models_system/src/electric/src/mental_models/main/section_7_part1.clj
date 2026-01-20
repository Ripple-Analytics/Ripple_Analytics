(ns mental-models.main.section-7-part1
  "Main Module - Section 7 Part1"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

;; ============================================

(defn app [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      ;; Index
      (and (= method :get) (= uri "/"))
      (-> (response/response index-html)
          (response/content-type "text/html"))
      
      ;; Health
      (and (= method :get) (= uri "/health"))
      (json-response {:status "healthy" :service "mental-models-electric"})
      
      ;; Models API
      (and (= method :get) (= uri "/api/models"))
      (handle-get-models request)
      
      ;; Analysis API
      (and (= method :post) (= uri "/api/analysis/latticework"))
      (handle-latticework request)
      
      (and (= method :post) (= uri "/api/analysis/lollapalooza"))
      (handle-lollapalooza request)
      
      (and (= method :post) (= uri "/api/analysis/inversion"))
      (handle-inversion request)
      
      (and (= method :post) (= uri "/api/analysis/two-track"))
      (handle-two-track request)
      
      (and (= method :post) (= uri "/api/analysis/bias-detection"))
      (handle-bias-detection request)
      
      ;; Statistics API
      (and (= method :post) (= uri "/api/statistics/correlation"))
      (handle-correlation request)
      
      ;; Data API
      (and (= method :post) (= uri "/api/data/analyze"))
      (handle-document-analysis request)
      
      ;; LLM API (LM Studio integration)
      (and (= method :get) (= uri "/api/llm/status"))
      (handle-llm-status request)
      
      (and (= method :post) (= uri "/api/llm/analyze"))
      (handle-llm-analyze request)
      
      (and (= method :post) (= uri "/api/llm/biases"))
      (handle-llm-biases request)
      
      (and (= method :post) (= uri "/api/llm/checklist"))
      (handle-llm-checklist request)
      
      (and (= method :post) (= uri "/api/llm/classify"))
      (handle-llm-classify request)
      
      ;; Tech Debt API
      (and (= method :post) (= uri "/api/techdebt/analyze"))
      (handle-analyze-dag request)
      
      (and (= method :post) (= uri "/api/techdebt/tangles"))
      (handle-detect-tangles request)
      
      (and (= method :post) (= uri "/api/techdebt/plan"))
      (handle-refactoring-plan request)
      
      (and (= method :post) (= uri "/api/techdebt/llm-refactor"))
      (handle-llm-refactor request)
      
      (and (= method :post) (= uri "/api/techdebt/visualize"))
      (handle-dag-visualization request)
      
      ;; Distributed Processing API
      (and (= method :get) (= uri "/api/distributed/status"))
      (json-response (distributed/get-cluster-metrics))
      
      (and (= method :get) (= uri "/api/distributed/throughput"))
      (json-response {:throughput (distributed/get-throughput)})
      
      (and (= method :post) (= uri "/api/distributed/submit"))
      (let [body (parse-json-body request)
            work-type (keyword (get body "type" "analyze"))
            data (get body "data")
            priority (keyword (get body "priority" "normal"))]
        (json-response (distributed/submit-work work-type data :priority priority)))
      
      (and (= method :post) (= uri "/api/distributed/submit-bulk"))
      (let [body (parse-json-body request)
            work-items (get body "items" [])]
        (json-response (distributed/submit-bulk-work work-items)))
      
      (and (= method :post) (= uri "/api/distributed/start-workers"))
      (let [body (parse-json-body request)
            num-workers (get body "workers" 4)]
        (distributed/start-workers! num-workers)
        (json-response {:status "started" :workers num-workers}))
      
      (and (= method :post) (= uri "/api/distributed/stop"))
      (do
        (distributed/shutdown!)
        (json-response {:status "stopped"}))
      
      (and (= method :post) (= uri "/api/distributed/scale"))
      (let [body (parse-json-body request)
            target-workers (get body "target" 8)]
        (distributed/scale-workers! target-workers)
        (json-response {:status "scaling" :target target-workers}))
      
      ;; Continuous Learning API
      (and (= method :get) (= uri "/api/continuous/status"))
      (json-response (continuous/get-system-status))
      
      (and (= method :post) (= uri "/api/continuous/start"))
      (do
        (continuous/start-all-systems!)
        (json-response {:status "started"}))
      
      (and (= method :post) (= uri "/api/continuous/stop"))
      (do
        (continuous/stop-all-systems!)
        (json-response {:status "stopped"}))
      
      (and (= method :post) (= uri "/api/continuous/scraper/start"))
      (let [body (parse-json-body request)
            scraper-id (keyword (get body "id" "default"))
            urls (get body "urls" [])
            depth (get body "depth" 2)
            interval (get body "interval" 3600000)]
        (continuous/start-scraper! scraper-id urls :depth depth :interval interval)
        (json-response {:status "started" :scraper-id scraper-id}))
      
      (and (= method :post) (= uri "/api/continuous/file-watcher/start"))
      (let [body (parse-json-body request)
            watcher-id (keyword (get body "id" "default"))
            directories (get body "directories" [])]
        (continuous/start-file-watcher! watcher-id directories)
        (json-response {:status "started" :watcher-id watcher-id}))
      
      (and (= method :post) (= uri "/api/continuous/sensor/start"))
      (let [body (parse-json-body request)
            device-id (get body "device-id")
            sensor-types (map keyword (get body "sensors" ["accelerometer"]))]
        (continuous/start-sensor-collector! device-id sensor-types)
        (json-response {:status "started" :device-id device-id}))
      
      (and (= method :post) (= uri "/api/continuous/petabyte"))
      (let [body (parse-json-body request)
