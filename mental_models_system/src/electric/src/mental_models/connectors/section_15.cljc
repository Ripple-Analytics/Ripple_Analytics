(ns mental-models.connectors.section-15
  "Connectors Module - Section 15"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

#?(:clj
   (defmacro with-metrics
     "Execute a connector operation with metrics collection."
     [connector-type & body]
     `(let [start-time# (System/currentTimeMillis)]
        (record-request ~connector-type)
        (try
          (let [result# (do ~@body)]
            (record-latency ~connector-type (- (System/currentTimeMillis) start-time#))
            result#)
          (catch Exception e#
            (record-latency ~connector-type (- (System/currentTimeMillis) start-time#))
            (record-error ~connector-type (type e#))
            (throw e#))))))

;; ============================================
;; Circuit Breaker Pattern
