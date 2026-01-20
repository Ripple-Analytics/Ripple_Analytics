(ns mental-models.connectors.section-35
  "Connectors Module - Section 35"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def request-dedup-cache
  "Cache for deduplicating identical concurrent requests."
  (atom {}))

(def dedup-config
  "Configuration for request deduplication."
  (atom {:enabled true
         :ttl-ms 5000
         :max-entries 1000}))

#?(:clj
   (defn get-dedup-config
     "Get current deduplication configuration."
     []
     @dedup-config))

#?(:clj
   (defn set-dedup-config
     "Update deduplication configuration."
     [config]
     (swap! dedup-config merge config)))

#?(:clj
   (defn request-key
     "Generate a unique key for a request based on its parameters."
     [connector-type operation params]
     (str connector-type ":" operation ":" (hash params))))

#?(:clj
   (defn cleanup-expired-dedup-entries
     "Remove expired entries from the deduplication cache."
     []
     (let [now (System/currentTimeMillis)
           ttl (:ttl-ms @dedup-config)]
       (swap! request-dedup-cache
              (fn [cache]
                (into {}
                      (filter (fn [[_ v]]
                                (< (- now (:timestamp v)) ttl))
                              cache)))))))

#?(:clj
   (defn with-deduplication
     "Execute a request with deduplication.
      If an identical request is already in flight, wait for its result.
      If an identical request completed recently, return cached result."
     [connector-type operation params request-fn]
     (if-not (:enabled @dedup-config)
       (request-fn)
       (let [key (request-key connector-type operation params)
             now (System/currentTimeMillis)]
         ;; Check for existing entry
         (if-let [existing (get @request-dedup-cache key)]
           (cond
             ;; In-flight request - wait for it
             (:in-flight existing)
             (do
               (while (and (get-in @request-dedup-cache [key :in-flight])
                           (< (- (System/currentTimeMillis) now) 30000))
                 (Thread/sleep 50))
               (or (:result (get @request-dedup-cache key))
                   (request-fn)))
             
             ;; Cached result still valid
             (< (- now (:timestamp existing)) (:ttl-ms @dedup-config))
             (:result existing)
             
             ;; Expired - make new request
             :else
             (with-deduplication connector-type operation params request-fn))
           
           ;; No existing entry - make request
           (do
             ;; Mark as in-flight
             (swap! request-dedup-cache assoc key {:in-flight true :timestamp now})
             (try
               (let [result (request-fn)]
                 ;; Store result
                 (swap! request-dedup-cache assoc key {:result result
                                                       :timestamp (System/currentTimeMillis)
                                                       :in-flight false})
                 ;; Cleanup if cache is too large
                 (when (> (count @request-dedup-cache) (:max-entries @dedup-config))
                   (cleanup-expired-dedup-entries))
                 result)
               (catch Exception e
                 ;; Remove failed entry
                 (swap! request-dedup-cache dissoc key)
                 (throw e)))))))))

#?(:clj
   (defn clear-dedup-cache
     "Clear the deduplication cache."
     []
     (reset! request-dedup-cache {})))

#?(:clj
   (defn get-dedup-stats
     "Get deduplication cache statistics."
     []
     (let [cache @request-dedup-cache
           now (System/currentTimeMillis)
           ttl (:ttl-ms @dedup-config)]
       {:total-entries (count cache)
        :in-flight (count (filter #(:in-flight (val %)) cache))
        :cached (count (filter #(and (not (:in-flight (val %)))
                                     (< (- now (:timestamp (val %))) ttl))
                               cache))
        :expired (count (filter #(and (not (:in-flight (val %)))
                                      (>= (- now (:timestamp (val %))) ttl))
                                cache))})))

;; ============================================
;; Request Batching
