(ns mental-models.connectors.section-87
  "Connectors Module - Section 87"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def throttle-config
  "Configuration for request throttling."
  (atom {:enabled true
         :default-max-concurrent 10
         :default-requests-per-second 100
         :burst-allowance 1.5}))

(def throttle-state
  "State for request throttling per connector."
  (atom {}))

(defn get-throttle-config
  "Get current throttle configuration."
  []
  @throttle-config)

(defn set-throttle-config
  "Update throttle configuration."
  [config]
  (swap! throttle-config merge config))

(defn set-connector-throttle
  "Set throttle limits for a specific connector."
  [connector-type & {:keys [max-concurrent requests-per-second]}]
  (swap! throttle-state assoc connector-type
         {:max-concurrent (or max-concurrent (:default-max-concurrent @throttle-config))
          :requests-per-second (or requests-per-second (:default-requests-per-second @throttle-config))
          :current-concurrent (atom 0)
          :last-request-time (atom 0)
          :request-count (atom 0)}))

(defn get-connector-throttle
  "Get throttle state for a connector."
  [connector-type]
  (get @throttle-state connector-type))

(defn can-make-request?
  "Check if a request can be made within throttle limits."
  [connector-type]
  (if-not (:enabled @throttle-config)
    true
    (if-let [state (get-connector-throttle connector-type)]
      (let [current @(:current-concurrent state)
            max-concurrent (:max-concurrent state)]
        (< current max-concurrent))
      true)))

(defn acquire-throttle
  "Acquire a throttle slot for a request."
  [connector-type]
  (when-let [state (get-connector-throttle connector-type)]
    (swap! (:current-concurrent state) inc)
    (reset! (:last-request-time state) (System/currentTimeMillis))
    (swap! (:request-count state) inc)))

(defn release-throttle
  "Release a throttle slot after request completion."
  [connector-type]
  (when-let [state (get-connector-throttle connector-type)]
    (swap! (:current-concurrent state) dec)))

(defn with-throttling
  "Execute a function with throttling."
  [connector-type f]
  (if-not (:enabled @throttle-config)
    (f)
    (if (can-make-request? connector-type)
      (do
        (acquire-throttle connector-type)
        (try
          (f)
          (finally
            (release-throttle connector-type))))
      {:error "Throttle limit exceeded"
       :connector connector-type})))

(defn get-throttle-stats
  "Get throttling statistics."
  []
  {:config @throttle-config
   :connectors (into {}
                     (map (fn [[k v]]
                            [k {:max-concurrent (:max-concurrent v)
                                :current-concurrent @(:current-concurrent v)
                                :total-requests @(:request-count v)}])
                          @throttle-state))})

;; ============================================
;; Connector Aliasing
