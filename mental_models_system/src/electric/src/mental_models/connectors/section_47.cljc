(ns mental-models.connectors.section-47
  "Connectors Module - Section 47"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

#?(:clj
   (defn logged-http-request
     "Execute an HTTP request with logging, metrics, and circuit breaker protection."
     [connector-type method url & {:keys [headers body query-params timeout-ms]
                                   :or {timeout-ms 30000}}]
     (let [request-id (log-request connector-type method url :headers headers :body body)
           start-time (System/currentTimeMillis)]
       (try
         (wait-for-rate-limit connector-type)
         (record-request connector-type)
         (let [result (with-circuit-breaker connector-type
                        #(let [response (case method
                                          :get (http/get url (merge default-http-opts
                                                                    {:headers headers
                                                                     :query-params query-params
                                                                     :socket-timeout timeout-ms}))
                                          :post (http/post url (merge default-http-opts
                                                                      {:headers headers
                                                                       :body body
                                                                       :content-type :json
                                                                       :socket-timeout timeout-ms}))
                                          :put (http/put url (merge default-http-opts
                                                                    {:headers headers
                                                                     :body body
                                                                     :content-type :json
                                                                     :socket-timeout timeout-ms}))
                                          :delete (http/delete url (merge default-http-opts
                                                                          {:headers headers
                                                                           :socket-timeout timeout-ms})))]
                            response))
               latency (- (System/currentTimeMillis) start-time)]
           (record-latency connector-type latency)
           (if (:success result)
             (do
               (log-response request-id (:status (:value result))
                             :headers (:headers (:value result))
                             :body (:body (:value result))
                             :latency-ms latency)
               {:success true
                :status (:status (:value result))
                :body (:body (:value result))
                :headers (:headers (:value result))
                :latency-ms latency
                :request-id request-id})
             (do
               (record-error connector-type :circuit-breaker)
               (log-response request-id nil :error (:error result) :latency-ms latency)
               {:success false
                :error (:error result)
                :latency-ms latency
                :request-id request-id})))
         (catch Exception e
           (let [latency (- (System/currentTimeMillis) start-time)]
             (record-error connector-type :exception)
             (log-response request-id nil :error (.getMessage e) :latency-ms latency)
             {:success false
              :error (.getMessage e)
              :latency-ms latency
              :request-id request-id}))))))

;; ============================================
;; Base Connector Protocol
;; ============================================

(defprotocol Connector
  "Base protocol for all connectors."
  (connect [this] "Establish connection")
  (disconnect [this] "Close connection")
  (health-check [this] "Check if connection is healthy")
  (get-status [this] "Get connector status"))

;; ============================================
;; Zapier Connector
;; ============================================

(def zapier-config
  {:base-url "https://hooks.zapier.com"
   :timeout-ms 30000
   :retry-count 3})

(defn create-zapier-connector
  "Create a Zapier webhook connector."
  [webhook-url]
  {:type :zapier
   :webhook-url webhook-url
   :status :disconnected
   :last-triggered nil})

(defn trigger-zapier-webhook
  "Trigger a Zapier webhook with payload."
  [connector payload]
  #?(:clj
     (try
       (let [response (http/post (:webhook-url connector)
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 30000
                                  :connection-timeout 10000})]
         {:success (< (:status response) 400)
          :connector-type :zapier
          :payload payload
          :timestamp (java.time.Instant/now)
          :status-code (:status response)
          :response-body (:body response)})
       (catch Exception e
         {:success false
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:success false
      :error "Zapier webhooks must be triggered from server"}))

;; ============================================
;; Huggingface Connector
