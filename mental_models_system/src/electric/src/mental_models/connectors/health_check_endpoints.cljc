(ns mental-models.connectors.health-check-endpoints
  "Connectors Module - Health Check Endpoints"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================
;; Health Check Endpoints
;; ============================================

#?(:clj
   (defn health-check-connector
     "Perform a health check on a specific connector.
      Returns {:healthy true/false :latency-ms N :error error-msg}."
     [connector-type connector]
     (let [start-time (System/currentTimeMillis)]
       (try
         (case connector-type
           :lm-studio
           (let [result (lm-studio-list-models connector)]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:models-count (count (:models result))}
                         {:error (:error result)})})
           
           :github
           (let [result (github-get-repo connector "octocat" "Hello-World")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:repo-found true}
                         {:error (:error result)})})
           
           :slack
           (let [result (slack-list-channels connector)]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:channels-count (:total-count result)}
                         {:error (:error result)})})
           
           :huggingface
           (let [result (huggingface-inference connector "gpt2" "Hello")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:inference-success true}
                         {:error (:error result)})})
           
           :web-scraper
           (let [result (scrape-url connector "https://example.com")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:content-length (count (:content result))}
                         {:error (:error result)})})
           
           :file
           (let [result (file-list connector "")]
             {:healthy (= :success (:status result))
              :latency-ms (- (System/currentTimeMillis) start-time)
              :connector-type connector-type
              :details (if (= :success (:status result))
                         {:files-count (count (:files result))}
                         {:error (:error result)})})
           
           ;; Default for unknown connector types
           {:healthy false
            :latency-ms (- (System/currentTimeMillis) start-time)
            :connector-type connector-type
            :details {:error "Unknown connector type"}})
         (catch Exception e
           {:healthy false
            :latency-ms (- (System/currentTimeMillis) start-time)
            :connector-type connector-type
            :details {:error (.getMessage e)}})))))

#?(:clj
   (defn get-system-health
     "Get overall system health status including all connectors."
     []
     {:timestamp (java.time.Instant/now)
      :metrics-summary (get-metrics-summary)
      :circuit-breakers (get-circuit-status)
      :rate-limiters (into {} (map (fn [[k v]] [k {:requests-in-window (count (:requests v))}]) @rate-limiters))
      :cache-size (count @response-cache)}))

;; ============================================
;; Bulk Operations
