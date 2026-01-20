(ns mental-models.connectors.section-11
  "Connectors Module - Section 11"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def response-cache
  "Atom storing cached responses with TTL."
  (atom {}))

(def cache-config
  "Cache TTL per connector type (milliseconds)."
  {:github 300000      ;; 5 minutes
   :huggingface 60000  ;; 1 minute
   :web-scraper 600000 ;; 10 minutes
   :lm-studio 0})      ;; No caching for LLM responses

#?(:clj
   (defn cache-key
     "Generate a cache key from connector type and request params."
     [connector-type & params]
     (str connector-type "-" (hash params))))

#?(:clj
   (defn get-cached
     "Get a cached response if valid, nil otherwise."
     [connector-type & params]
     (let [key (apply cache-key connector-type params)
           entry (get @response-cache key)
           ttl (get cache-config connector-type 0)]
       (when (and entry 
                  (> ttl 0)
                  (< (- (System/currentTimeMillis) (:timestamp entry)) ttl))
         (:value entry)))))

#?(:clj
   (defn set-cached
     "Cache a response value."
     [connector-type value & params]
     (let [key (apply cache-key connector-type params)
           ttl (get cache-config connector-type 0)]
       (when (> ttl 0)
         (swap! response-cache assoc key {:value value 
                                          :timestamp (System/currentTimeMillis)}))
       value)))

#?(:clj
   (defn clear-cache
     "Clear all cached responses or for a specific connector type."
     ([] (reset! response-cache {}))
     ([connector-type]
      (swap! response-cache 
             (fn [cache]
               (into {} (remove #(str/starts-with? (key %) (str connector-type "-")) cache)))))))

;; ============================================
;; Metrics & Telemetry
