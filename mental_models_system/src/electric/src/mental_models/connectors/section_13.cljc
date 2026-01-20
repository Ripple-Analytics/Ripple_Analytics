(ns mental-models.connectors.section-13
  "Connectors Module - Section 13"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def metrics
  "Atom storing connector metrics."
  (atom {:requests {}
         :errors {}
         :latencies {}
         :cache-hits {}
         :cache-misses {}
         :rate-limit-waits {}
         :retries {}}))

#?(:clj
   (defn record-request
     "Record a request metric for a connector type."
     [connector-type]
     (swap! metrics update-in [:requests connector-type] (fnil inc 0))))

#?(:clj
   (defn record-error
     "Record an error metric for a connector type."
     [connector-type error-type]
     (swap! metrics update-in [:errors connector-type error-type] (fnil inc 0))))

#?(:clj
   (defn record-latency
     "Record latency for a request (in milliseconds)."
     [connector-type latency-ms]
     (swap! metrics update-in [:latencies connector-type]
            (fn [latencies]
              (let [current (or latencies {:count 0 :total 0 :min Long/MAX_VALUE :max 0})]
                {:count (inc (:count current))
                 :total (+ (:total current) latency-ms)
                 :min (min (:min current) latency-ms)
                 :max (max (:max current) latency-ms)})))))

#?(:clj
   (defn record-cache-hit
     "Record a cache hit for a connector type."
     [connector-type]
     (swap! metrics update-in [:cache-hits connector-type] (fnil inc 0))))

#?(:clj
   (defn record-cache-miss
     "Record a cache miss for a connector type."
     [connector-type]
     (swap! metrics update-in [:cache-misses connector-type] (fnil inc 0))))

#?(:clj
   (defn record-rate-limit-wait
     "Record a rate limit wait for a connector type."
     [connector-type]
     (swap! metrics update-in [:rate-limit-waits connector-type] (fnil inc 0))))

#?(:clj
   (defn record-retry
     "Record a retry attempt for a connector type."
     [connector-type attempt]
     (swap! metrics update-in [:retries connector-type attempt] (fnil inc 0))))

#?(:clj
   (defn get-metrics
     "Get all metrics or metrics for a specific connector type."
     ([] @metrics)
     ([connector-type]
      {:requests (get-in @metrics [:requests connector-type] 0)
       :errors (get-in @metrics [:errors connector-type] {})
       :latencies (get-in @metrics [:latencies connector-type] {:count 0 :total 0 :min 0 :max 0})
       :cache-hits (get-in @metrics [:cache-hits connector-type] 0)
       :cache-misses (get-in @metrics [:cache-misses connector-type] 0)
       :rate-limit-waits (get-in @metrics [:rate-limit-waits connector-type] 0)
       :retries (get-in @metrics [:retries connector-type] {})})))

#?(:clj
   (defn get-average-latency
     "Get average latency for a connector type (in milliseconds)."
     [connector-type]
     (let [latencies (get-in @metrics [:latencies connector-type])]
       (if (and latencies (> (:count latencies) 0))
         (/ (:total latencies) (:count latencies))
         0))))

#?(:clj
   (defn get-error-rate
     "Get error rate for a connector type (errors / total requests)."
     [connector-type]
     (let [requests (get-in @metrics [:requests connector-type] 0)
           errors (reduce + (vals (get-in @metrics [:errors connector-type] {})))]
       (if (> requests 0)
         (double (/ errors requests))
         0.0))))

#?(:clj
   (defn get-cache-hit-rate
     "Get cache hit rate for a connector type."
     [connector-type]
     (let [hits (get-in @metrics [:cache-hits connector-type] 0)
           misses (get-in @metrics [:cache-misses connector-type] 0)
           total (+ hits misses)]
       (if (> total 0)
         (double (/ hits total))
         0.0))))

#?(:clj
   (defn reset-metrics
     "Reset all metrics or metrics for a specific connector type."
     ([] (reset! metrics {:requests {}
                          :errors {}
                          :latencies {}
                          :cache-hits {}
                          :cache-misses {}
                          :rate-limit-waits {}
                          :retries {}}))
     ([connector-type]
      (swap! metrics (fn [m]
                       (-> m
                           (update :requests dissoc connector-type)
                           (update :errors dissoc connector-type)
                           (update :latencies dissoc connector-type)
                           (update :cache-hits dissoc connector-type)
                           (update :cache-misses dissoc connector-type)
                           (update :rate-limit-waits dissoc connector-type)
                           (update :retries dissoc connector-type)))))))

#?(:clj
   (defn get-metrics-summary
     "Get a summary of all connector metrics."
     []
     (let [connector-types (keys (:requests @metrics))]
       (into {}
             (map (fn [ct]
                    [ct {:requests (get-in @metrics [:requests ct] 0)
                         :error-rate (get-error-rate ct)
                         :avg-latency-ms (get-average-latency ct)
                         :cache-hit-rate (get-cache-hit-rate ct)
                         :rate-limit-waits (get-in @metrics [:rate-limit-waits ct] 0)}])
                  connector-types)))))

;; ============================================
;; Instrumented HTTP Wrapper
