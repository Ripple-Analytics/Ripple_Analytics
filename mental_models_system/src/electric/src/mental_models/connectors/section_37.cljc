(ns mental-models.connectors.section-37
  "Connectors Module - Section 37"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def batch-queue
  "Queue for batching requests by connector type."
  (atom {}))

(def batch-config
  "Configuration for request batching."
  (atom {:enabled true
         :max-batch-size 10
         :max-wait-ms 100
         :flush-interval-ms 50}))

#?(:clj
   (defn get-batch-config
     "Get current batch configuration."
     []
     @batch-config))

#?(:clj
   (defn set-batch-config
     "Update batch configuration."
     [config]
     (swap! batch-config merge config)))

#?(:clj
   (defn add-to-batch
     "Add a request to the batch queue.
      Returns a promise that will be delivered when the batch is processed."
     [connector-type operation params request-fn]
     (let [result-promise (promise)
           batch-key (str connector-type ":" operation)
           entry {:params params
                  :request-fn request-fn
                  :promise result-promise
                  :timestamp (System/currentTimeMillis)}]
       (swap! batch-queue update batch-key (fnil conj []) entry)
       result-promise)))

#?(:clj
   (defn process-batch
     "Process a batch of requests.
      Executes all requests and delivers results to their promises."
     [batch-key entries]
     (doseq [{:keys [request-fn promise]} entries]
       (try
         (let [result (request-fn)]
           (deliver promise {:success true :result result}))
         (catch Exception e
           (deliver promise {:success false :error (.getMessage e)}))))))

#?(:clj
   (defn flush-batch
     "Flush a specific batch, processing all pending requests."
     [batch-key]
     (let [entries (get @batch-queue batch-key)]
       (when (seq entries)
         (swap! batch-queue dissoc batch-key)
         (process-batch batch-key entries)))))

#?(:clj
   (defn flush-all-batches
     "Flush all pending batches."
     []
     (doseq [batch-key (keys @batch-queue)]
       (flush-batch batch-key))))

#?(:clj
   (defn should-flush-batch?
     "Check if a batch should be flushed based on size or time."
     [batch-key]
     (let [entries (get @batch-queue batch-key [])
           config @batch-config]
       (or (>= (count entries) (:max-batch-size config))
           (and (seq entries)
                (let [oldest (apply min (map :timestamp entries))]
                  (>= (- (System/currentTimeMillis) oldest) (:max-wait-ms config))))))))

#?(:clj
   (defn with-batching
     "Execute a request with batching support.
      Requests are queued and processed in batches for efficiency."
     [connector-type operation params request-fn]
     (if-not (:enabled @batch-config)
       (request-fn)
       (let [batch-key (str connector-type ":" operation)
             result-promise (add-to-batch connector-type operation params request-fn)]
         ;; Check if batch should be flushed
         (when (should-flush-batch? batch-key)
           (flush-batch batch-key))
         ;; Wait for result with timeout
         (let [result (deref result-promise (:max-wait-ms @batch-config) {:timeout true})]
           (if (:timeout result)
             ;; Timeout - flush and wait again
             (do
               (flush-batch batch-key)
               (let [final-result (deref result-promise 5000 {:error "Batch timeout"})]
                 (if (:success final-result)
                   (:result final-result)
                   {:error (:error final-result)})))
             ;; Got result
             (if (:success result)
               (:result result)
               {:error (:error result)})))))))

#?(:clj
   (defn get-batch-stats
     "Get statistics about pending batches."
     []
     (let [queue @batch-queue]
       {:total-batches (count queue)
        :total-pending (reduce + (map count (vals queue)))
        :batches (into {}
                       (map (fn [[k v]]
                              [k {:count (count v)
                                  :oldest-ms (when (seq v)
                                               (- (System/currentTimeMillis)
                                                  (apply min (map :timestamp v))))}])
                            queue))})))

#?(:clj
   (defn clear-batch-queue
     "Clear all pending batches without processing them."
     []
     (let [queue @batch-queue]
       ;; Deliver errors to all pending promises
       (doseq [[_ entries] queue]
         (doseq [{:keys [promise]} entries]
           (deliver promise {:success false :error "Batch cleared"})))
       (reset! batch-queue {}))))

;; ============================================
;; Adaptive Retry Strategy
