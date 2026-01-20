(ns mental-models.connectors.section-91
  "Connectors Module - Section 91"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def replay-buffer
  "Buffer for storing requests for replay."
  (atom []))

(def replay-config
  "Configuration for request replay."
  (atom {:enabled true
         :max-buffer-size 1000
         :retention-ms 3600000
         :capture-responses true}))

(defn get-replay-config
  "Get current replay configuration."
  []
  @replay-config)

(defn set-replay-config
  "Update replay configuration."
  [config]
  (swap! replay-config merge config))

(defn capture-request
  "Capture a request for potential replay."
  [connector-type operation params & {:keys [response]}]
  (when (:enabled @replay-config)
    (let [entry {:id (str (random-uuid))
                 :connector-type connector-type
                 :operation operation
                 :params params
                 :response (when (:capture-responses @replay-config) response)
                 :captured-at (System/currentTimeMillis)}]
      (swap! replay-buffer
             (fn [buf]
               (let [new-buf (conj buf entry)]
                 (if (> (count new-buf) (:max-buffer-size @replay-config))
                   (vec (rest new-buf))
                   new-buf))))
      (:id entry))))

(defn get-captured-requests
  "Get captured requests, optionally filtered."
  [& {:keys [connector-type operation limit]}]
  (let [requests @replay-buffer
        filtered (cond->> requests
                   connector-type (filter #(= (:connector-type %) connector-type))
                   operation (filter #(= (:operation %) operation))
                   limit (take limit))]
    (vec filtered)))

(defn get-request-by-id
  "Get a specific captured request by ID."
  [request-id]
  (first (filter #(= (:id %) request-id) @replay-buffer)))

(defn replay-request
  "Replay a captured request."
  [request-id request-fn]
  (if-let [request (get-request-by-id request-id)]
    (let [result (request-fn (:connector-type request) (:operation request) (:params request))]
      {:replayed true
       :original-id request-id
       :result result
       :replayed-at (System/currentTimeMillis)})
    {:error "Request not found" :request-id request-id}))

(defn clear-replay-buffer
  "Clear the replay buffer."
  []
  (reset! replay-buffer []))

(defn cleanup-old-requests
  "Remove requests older than retention period."
  []
  (let [cutoff (- (System/currentTimeMillis) (:retention-ms @replay-config))]
    (swap! replay-buffer
           (fn [buf]
             (vec (filter #(> (:captured-at %) cutoff) buf))))))

(defn get-replay-stats
  "Get statistics about request replay."
  []
  {:buffer-size (count @replay-buffer)
   :config @replay-config
   :oldest-request (when (seq @replay-buffer)
                     (:captured-at (first @replay-buffer)))
   :newest-request (when (seq @replay-buffer)
                     (:captured-at (last @replay-buffer)))})

;; ============================================
;; Connector Metrics Export
