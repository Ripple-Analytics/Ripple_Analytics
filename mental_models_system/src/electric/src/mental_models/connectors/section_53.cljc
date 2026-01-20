(ns mental-models.connectors.section-53
  "Connectors Module - Section 53"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def huggingface-config
  {:base-url "https://api-inference.huggingface.co"
   :models {:text-generation "gpt2"
            :sentiment "distilbert-base-uncased-finetuned-sst-2-english"
            :embeddings "sentence-transformers/all-MiniLM-L6-v2"
            :summarization "facebook/bart-large-cnn"}})

(defn create-huggingface-connector
  "Create a Huggingface API connector."
  [api-key]
  {:type :huggingface
   :api-key api-key
   :status :disconnected
   :available-models (:models huggingface-config)})

(defn huggingface-inference
  "Run inference on a Huggingface model."
  [connector model-id input]
  #?(:clj
     (try
       (let [url (str (:base-url huggingface-config) "/models/" model-id)
             response (http/post url
                                 {:body (json/generate-string {:inputs input})
                                  :headers {"Authorization" (str "Bearer " (:api-key connector))}
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 60000
                                  :connection-timeout 10000
                                  :as :json})]
         {:model model-id
          :input input
          :output (:body response)
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:model model-id
          :input input
          :output "NA"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:model model-id
      :input input
      :output "NA"
      :status :error
      :error "Huggingface inference must be called from server"}))

(defn huggingface-embed
  "Generate embeddings using Huggingface."
  [connector text]
  #?(:clj
     (try
       (let [url (str (:base-url huggingface-config) "/models/sentence-transformers/all-MiniLM-L6-v2")
             response (http/post url
                                 {:body (json/generate-string {:inputs text})
                                  :headers {"Authorization" (str "Bearer " (:api-key connector))}
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 60000
                                  :connection-timeout 10000
                                  :as :json})]
         {:text text
          :embedding (:body response)
          :model "sentence-transformers/all-MiniLM-L6-v2"
          :dimensions 384
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:text text
          :embedding nil
          :model "sentence-transformers/all-MiniLM-L6-v2"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:text text
      :embedding nil
      :status :error
      :error "Huggingface embeddings must be called from server"}))

;; ============================================
;; GitHub Connector
