(ns mental-models.connectors.section-7
  "Connectors Module - Section 7"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def retry-config
  "Default retry configuration."
  {:max-retries 3
   :initial-delay-ms 1000
   :max-delay-ms 30000
   :backoff-multiplier 2.0
   :retryable-status-codes #{408 429 500 502 503 504}})

#?(:clj
   (defn calculate-backoff-delay
     "Calculate delay for retry attempt using exponential backoff with jitter."
     [attempt {:keys [initial-delay-ms max-delay-ms backoff-multiplier]}]
     (let [base-delay (* initial-delay-ms (Math/pow backoff-multiplier attempt))
           jitter (* base-delay (rand 0.3))
           delay-with-jitter (+ base-delay jitter)]
       (min delay-with-jitter max-delay-ms))))

#?(:clj
   (defn retryable-error?
     "Check if an error or response is retryable."
     [response-or-error config]
     (cond
       (instance? Exception response-or-error)
       (let [msg (.getMessage response-or-error)]
         (or (str/includes? (str msg) "timeout")
             (str/includes? (str msg) "connection")
             (str/includes? (str msg) "reset")))
       
       (map? response-or-error)
       (contains? (:retryable-status-codes config) (:status response-or-error))
       
       :else false)))

#?(:clj
   (defn with-retry
     "Execute a function with retry logic and exponential backoff.
      Returns the result of f or the last error after max retries."
     ([f] (with-retry f retry-config))
     ([f config]
      (loop [attempt 0]
        (let [result (try
                       {:success true :value (f)}
                       (catch Exception e
                         {:success false :error e}))]
          (if (:success result)
            (:value result)
            (if (and (< attempt (:max-retries config))
                     (retryable-error? (:error result) config))
              (do
                (Thread/sleep (long (calculate-backoff-delay attempt config)))
                (recur (inc attempt)))
              (throw (:error result)))))))))

;; ============================================
;; Rate Limiting
;; ============================================

(def rate-limiters
  "Atom storing rate limiter state per connector type."
  (atom {}))

(def rate-limit-config
  "Rate limits per connector type (requests per minute)."
  {:github 60
   :slack 60
   :huggingface 30
   :zapier 100
   :web-scraper 10})

#?(:clj
   (defn check-rate-limit
     "Check if a request is within rate limits. Returns true if allowed."
     [connector-type]
     (let [limit (get rate-limit-config connector-type 60)
           now (System/currentTimeMillis)
           window-ms 60000
           state (get @rate-limiters connector-type {:requests [] :last-reset now})]
       (if (> (- now (:last-reset state)) window-ms)
         ;; Reset window
         (do
           (swap! rate-limiters assoc connector-type {:requests [now] :last-reset now})
           true)
         ;; Check if under limit
         (let [recent-requests (filter #(> % (- now window-ms)) (:requests state))]
           (if (< (count recent-requests) limit)
             (do
               (swap! rate-limiters assoc connector-type 
                      {:requests (conj (vec recent-requests) now) :last-reset (:last-reset state)})
               true)
             false))))))

#?(:clj
   (defn wait-for-rate-limit
     "Wait until rate limit allows a request. Returns after waiting."
     [connector-type]
     (loop [attempts 0]
       (if (check-rate-limit connector-type)
         true
         (when (< attempts 60)
           (Thread/sleep 1000)
           (recur (inc attempts)))))))

;; ============================================
;; Response Caching
