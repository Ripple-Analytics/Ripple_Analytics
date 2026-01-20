(ns mental-models.connectors.section-73
  "Connectors Module - Section 73"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def timeout-escalation-config
  "Configuration for timeout escalation."
  (atom {:enabled true
         :initial-timeout-ms 5000
         :max-timeout-ms 60000
         :escalation-factor 1.5
         :max-escalations 3}))

(def timeout-history
  "History of timeout escalations per connector."
  (atom {}))

(defn get-timeout-escalation-config
  "Get current timeout escalation configuration."
  []
  @timeout-escalation-config)

(defn set-timeout-escalation-config
  "Update timeout escalation configuration."
  [config]
  (swap! timeout-escalation-config merge config))

(defn calculate-escalated-timeout
  "Calculate the escalated timeout based on history."
  [connector-type]
  (let [config @timeout-escalation-config
        history (get @timeout-history connector-type {:escalation-count 0})
        escalation-count (min (:escalation-count history) (:max-escalations config))
        factor (Math/pow (:escalation-factor config) escalation-count)
        timeout (* (:initial-timeout-ms config) factor)]
    (min timeout (:max-timeout-ms config))))

(defn record-timeout
  "Record a timeout event for a connector."
  [connector-type]
  (swap! timeout-history update connector-type
         (fn [h]
           (let [current (or h {:escalation-count 0 :timeouts []})]
             {:escalation-count (inc (:escalation-count current))
              :timeouts (conj (:timeouts current)
                             {:timestamp (System/currentTimeMillis)})}))))

(defn record-success-timeout
  "Record a successful request (resets escalation)."
  [connector-type]
  (swap! timeout-history update connector-type
         (fn [h]
           (let [current (or h {:escalation-count 0 :timeouts []})]
             {:escalation-count (max 0 (dec (:escalation-count current)))
              :timeouts (:timeouts current)}))))

(defn with-timeout-escalation
  "Execute a function with timeout escalation support."
  [connector-type f]
  (let [timeout-ms (calculate-escalated-timeout connector-type)
        future-result (future (f))
        result (deref future-result timeout-ms {:timeout true})]
    (if (:timeout result)
      (do
        (record-timeout connector-type)
        (future-cancel future-result)
        {:error "Request timed out"
         :timeout-ms timeout-ms
         :escalation-count (get-in @timeout-history [connector-type :escalation-count])})
      (do
        (record-success-timeout connector-type)
        result))))

(defn get-timeout-stats
  "Get timeout statistics for all connectors."
  []
  {:config @timeout-escalation-config
   :history (into {}
                  (map (fn [[k v]]
                         [k {:escalation-count (:escalation-count v)
                             :total-timeouts (count (:timeouts v))
                             :current-timeout-ms (calculate-escalated-timeout k)}])
                       @timeout-history))})

;; ============================================
;; Connector Health Scoring
