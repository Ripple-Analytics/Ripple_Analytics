(ns mental-models.connectors.section-31
  "Connectors Module - Section 31"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def connector-pool
  "Pool of reusable connector instances.
   Manages connector lifecycle and provides connection reuse."
  (atom {:pools {}
         :config {:max-pool-size 10
                  :min-pool-size 1
                  :max-idle-time-ms 300000
                  :validation-interval-ms 60000}}))

#?(:clj
   (defn get-pool-config
     "Get the pool configuration."
     []
     (:config @connector-pool)))

#?(:clj
   (defn set-pool-config
     "Set pool configuration options."
     [config]
     (swap! connector-pool update :config merge config)))

#?(:clj
   (defn- create-pool-entry
     "Create a new pool entry for a connector."
     [connector]
     {:connector connector
      :created-at (java.time.Instant/now)
      :last-used-at (atom (java.time.Instant/now))
      :use-count (atom 0)
      :in-use (atom false)}))

#?(:clj
   (defn- pool-key
     "Generate a pool key for a connector type and config."
     [connector-type config]
     (let [safe-config (redact-sensitive-config connector-type config)]
       [connector-type (hash safe-config)])))

#?(:clj
   (defn acquire-from-pool
     "Acquire a connector from the pool, creating one if necessary.
      Returns {:connector ... :pool-entry ...} or {:error ...}"
     [connector-type config]
     (let [key (pool-key connector-type config)
           pool-config (:config @connector-pool)]
       (loop [attempts 0]
         (if (>= attempts 3)
           {:error "Could not acquire connector from pool"}
           (let [pool (get-in @connector-pool [:pools key] [])
                 available (first (filter #(compare-and-set! (:in-use %) false true) pool))]
             (if available
               (do
                 (reset! (:last-used-at available) (java.time.Instant/now))
                 (swap! (:use-count available) inc)
                 {:connector (:connector available)
                  :pool-entry available
                  :from-pool true})
               ;; Create new if pool not full
               (if (< (count pool) (:max-pool-size pool-config))
                 (let [new-connector (create-connector connector-type config)]
                   (if (:error new-connector)
                     new-connector
                     (let [entry (create-pool-entry new-connector)]
                       (reset! (:in-use entry) true)
                       (swap! connector-pool update-in [:pools key] (fnil conj []) entry)
                       {:connector new-connector
                        :pool-entry entry
                        :from-pool false})))
                 (do
                   (Thread/sleep 100)
                   (recur (inc attempts)))))))))))

#?(:clj
   (defn release-to-pool
     "Release a connector back to the pool."
     [pool-entry]
     (when pool-entry
       (reset! (:in-use pool-entry) false)
       (reset! (:last-used-at pool-entry) (java.time.Instant/now)))))

#?(:clj
   (defn with-pooled-connector
     "Execute a function with a pooled connector.
      Automatically acquires and releases the connector."
     [connector-type config f]
     (let [result (acquire-from-pool connector-type config)]
       (if (:error result)
         result
         (try
           (f (:connector result))
           (finally
             (release-to-pool (:pool-entry result))))))))

#?(:clj
   (defn get-pool-stats
     "Get statistics about the connector pool."
     []
     (let [pools (:pools @connector-pool)]
       {:total-pools (count pools)
        :total-connectors (reduce + (map count (vals pools)))
        :pools (into {}
                     (map (fn [[k v]]
                            [k {:size (count v)
                                :in-use (count (filter #(deref (:in-use %)) v))
                                :available (count (filter #(not (deref (:in-use %))) v))}])
                          pools))})))

#?(:clj
   (defn cleanup-idle-connectors
     "Remove connectors that have been idle longer than max-idle-time."
     []
     (let [max-idle (:max-idle-time-ms (:config @connector-pool))
           cutoff (java.time.Instant/ofEpochMilli (- (System/currentTimeMillis) max-idle))
           min-size (:min-pool-size (:config @connector-pool))]
       (swap! connector-pool update :pools
              (fn [pools]
                (into {}
                      (map (fn [[k v]]
                             [k (let [active (filter #(deref (:in-use %)) v)
                                      idle (filter #(not (deref (:in-use %))) v)
                                      keep-idle (take (max 0 (- min-size (count active)))
                                                      (sort-by #(deref (:last-used-at %)) > idle))
                                      recent-idle (filter #(.isAfter (deref (:last-used-at %)) cutoff)
                                                          (drop (count keep-idle) idle))]
                                  (vec (concat active keep-idle recent-idle)))])
                           pools)))))))

#?(:clj
   (defn drain-pool
     "Drain all connectors from the pool for a specific type, or all pools if no type specified."
     ([] (swap! connector-pool assoc :pools {}))
     ([connector-type config]
      (let [key (pool-key connector-type config)]
        (swap! connector-pool update :pools dissoc key)))))

;; ============================================
;; Graceful Shutdown Support
