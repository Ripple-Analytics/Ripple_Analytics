(ns mental-models.connectors.section-45
  "Connectors Module - Section 45"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def request-log
  "Atom storing recent request/response logs for debugging."
  (atom []))

(def log-config
  "Configuration for request logging."
  {:max-entries 1000
   :log-request-body true
   :log-response-body true
   :redact-headers #{"authorization" "x-api-key" "api-key" "token"}})

#?(:clj
   (defn redact-sensitive
     "Redact sensitive information from headers."
     [headers]
     (reduce (fn [h key]
               (if (contains? h key)
                 (assoc h key "[REDACTED]")
                 h))
             headers
             (:redact-headers log-config))))

#?(:clj
   (defn log-request
     "Log a request for debugging and auditing."
     [connector-type method url & {:keys [headers body]}]
     (let [entry {:id (java.util.UUID/randomUUID)
                  :timestamp (java.time.Instant/now)
                  :connector-type connector-type
                  :method method
                  :url url
                  :headers (redact-sensitive headers)
                  :body (when (:log-request-body log-config) body)
                  :type :request}]
       (swap! request-log
              (fn [log]
                (let [new-log (conj log entry)]
                  (if (> (count new-log) (:max-entries log-config))
                    (vec (drop (- (count new-log) (:max-entries log-config)) new-log))
                    new-log))))
       (:id entry))))

#?(:clj
   (defn log-response
     "Log a response for debugging and auditing."
     [request-id status & {:keys [headers body latency-ms error]}]
     (let [entry {:id (java.util.UUID/randomUUID)
                  :request-id request-id
                  :timestamp (java.time.Instant/now)
                  :status status
                  :headers (redact-sensitive headers)
                  :body (when (:log-response-body log-config) body)
                  :latency-ms latency-ms
                  :error error
                  :type :response}]
       (swap! request-log
              (fn [log]
                (let [new-log (conj log entry)]
                  (if (> (count new-log) (:max-entries log-config))
                    (vec (drop (- (count new-log) (:max-entries log-config)) new-log))
                    new-log))))
       (:id entry))))

#?(:clj
   (defn get-request-log
     "Get recent request/response logs.
      Options: :connector-type, :limit, :since (timestamp)"
     [& {:keys [connector-type limit since]}]
     (cond->> @request-log
       connector-type (filter #(= connector-type (:connector-type %)))
       since (filter #(.isAfter (:timestamp %) since))
       limit (take-last limit)
       true vec)))

#?(:clj
   (defn get-request-by-id
     "Get a specific request and its response by request ID."
     [request-id]
     (let [request (first (filter #(and (= :request (:type %))
                                        (= request-id (:id %)))
                                  @request-log))
           response (first (filter #(and (= :response (:type %))
                                         (= request-id (:request-id %)))
                                   @request-log))]
       {:request request :response response})))

#?(:clj
   (defn clear-request-log
     "Clear all request/response logs."
     []
     (reset! request-log [])))

#?(:clj
   (defn get-log-stats
     "Get statistics about the request log."
     []
     (let [logs @request-log
           requests (filter #(= :request (:type %)) logs)
           responses (filter #(= :response (:type %)) logs)
           by-connector (group-by :connector-type requests)
           error-responses (filter :error responses)]
       {:total-entries (count logs)
        :total-requests (count requests)
        :total-responses (count responses)
        :error-count (count error-responses)
        :by-connector (into {} (map (fn [[k v]] [k (count v)]) by-connector))
        :avg-latency-ms (when (seq responses)
                          (let [latencies (keep :latency-ms responses)]
                            (when (seq latencies)
                              (/ (reduce + latencies) (count latencies)))))})))

;; ============================================
;; Logged HTTP Request Wrapper
