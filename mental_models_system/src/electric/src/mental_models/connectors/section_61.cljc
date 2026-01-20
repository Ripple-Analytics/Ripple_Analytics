(ns mental-models.connectors.section-61
  "Connectors Module - Section 61"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def lm-studio-config
  {:default-host "localhost"
   :default-port 1234
   :api-path "/v1"})

(defn create-lm-studio-connector
  "Create an LM Studio connector for local LLM inference."
  [& {:keys [host port] :or {host "localhost" port 1234}}]
  {:type :lm-studio
   :host host
   :port port
   :base-url (str "http://" host ":" port "/v1")
   :status :disconnected
   :model nil})

(defn lm-studio-chat
  "Send a chat completion request to LM Studio."
  [connector messages & {:keys [temperature max-tokens model]
                         :or {temperature 0.7 max-tokens 1000}}]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/chat/completions")
             payload (cond-> {:messages messages
                              :temperature temperature
                              :max_tokens max-tokens}
                       model (assoc :model model))
             response (http/post url
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 120000
                                  :connection-timeout 10000
                                  :as :json})]
         {:messages messages
          :temperature temperature
          :max-tokens max-tokens
          :response (get-in response [:body :choices 0 :message])
          :usage (get-in response [:body :usage])
          :model (get-in response [:body :model])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:messages messages
          :temperature temperature
          :max-tokens max-tokens
          :response {:role "assistant" :content "NA"}
          :usage {:prompt-tokens 0 :completion-tokens 0 :total-tokens 0}
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:messages messages
      :response {:role "assistant" :content "NA"}
      :status :error
      :error "LM Studio must be called from server"}))

(defn lm-studio-complete
  "Send a completion request to LM Studio."
  [connector prompt & {:keys [temperature max-tokens model]
                       :or {temperature 0.7 max-tokens 500}}]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/completions")
             payload (cond-> {:prompt prompt
                              :temperature temperature
                              :max_tokens max-tokens}
                       model (assoc :model model))
             response (http/post url
                                 {:body (json/generate-string payload)
                                  :content-type :json
                                  :accept :json
                                  :socket-timeout 120000
                                  :connection-timeout 10000
                                  :as :json})]
         {:prompt prompt
          :temperature temperature
          :max-tokens max-tokens
          :completion (get-in response [:body :choices 0 :text])
          :usage (get-in response [:body :usage])
          :model (get-in response [:body :model])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:prompt prompt
          :temperature temperature
          :max-tokens max-tokens
          :completion "NA"
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:prompt prompt
      :completion "NA"
      :status :error
      :error "LM Studio must be called from server"}))

(defn lm-studio-list-models
  "List available models in LM Studio."
  [connector]
  #?(:clj
     (try
       (let [url (str (:base-url connector) "/models")
             response (http/get url
                                {:accept :json
                                 :socket-timeout 30000
                                 :connection-timeout 10000
                                 :as :json})]
         {:models (get-in response [:body :data])
          :status :success
          :timestamp (java.time.Instant/now)})
       (catch Exception e
         {:models []
          :status :error
          :error (.getMessage e)
          :timestamp (java.time.Instant/now)}))
     :cljs
     {:models []
      :status :error
      :error "LM Studio must be called from server"}))

;; ============================================
;; Database Connector
