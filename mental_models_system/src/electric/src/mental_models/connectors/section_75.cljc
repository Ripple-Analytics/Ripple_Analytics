(ns mental-models.connectors.section-75
  "Connectors Module - Section 75"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def health-scores
  "Health scores for each connector."
  (atom {}))

(def health-score-config
  "Configuration for health scoring."
  (atom {:enabled true
         :success-weight 1.0
         :failure-weight -2.0
         :timeout-weight -1.5
         :latency-weight -0.01
         :decay-factor 0.95
         :min-score 0.0
         :max-score 100.0
         :initial-score 50.0}))

(defn get-health-score-config
  "Get current health score configuration."
  []
  @health-score-config)

(defn set-health-score-config
  "Update health score configuration."
  [config]
  (swap! health-score-config merge config))

(defn get-connector-health-score
  "Get the health score for a connector."
  [connector-type]
  (let [config @health-score-config]
    (get @health-scores connector-type (:initial-score config))))

(defn update-health-score
  "Update the health score for a connector based on an event."
  [connector-type event-type & {:keys [latency-ms] :or {latency-ms 0}}]
  (let [config @health-score-config
        current-score (get-connector-health-score connector-type)
        decayed-score (* current-score (:decay-factor config))
        delta (case event-type
                :success (:success-weight config)
                :failure (:failure-weight config)
                :timeout (:timeout-weight config)
                0)
        latency-penalty (if (pos? latency-ms)
                         (* (:latency-weight config) (/ latency-ms 1000.0))
                         0)
        new-score (-> (+ decayed-score delta latency-penalty)
                      (max (:min-score config))
                      (min (:max-score config)))]
    (swap! health-scores assoc connector-type new-score)
    new-score))

(defn get-healthiest-connector
  "Get the connector type with the highest health score from a list."
  [connector-types]
  (let [scores (map (fn [t] {:type t :score (get-connector-health-score t)}) connector-types)]
    (:type (apply max-key :score scores))))

(defn get-all-health-scores
  "Get health scores for all connectors."
  []
  (let [config @health-score-config]
    {:config config
     :scores @health-scores
     :summary (into {}
                    (map (fn [[k v]]
                           [k {:score v
                               :status (cond
                                        (>= v 80) :healthy
                                        (>= v 50) :degraded
                                        (>= v 20) :unhealthy
                                        :else :critical)}])
                         @health-scores))}))

;; ============================================
;; Request Tracing / Correlation IDs
