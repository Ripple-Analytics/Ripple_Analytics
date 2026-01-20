(ns mental-models.connectors.section-39
  "Connectors Module - Section 39"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def retry-history
  "History of retry attempts for adaptive strategy."
  (atom {}))

(def adaptive-retry-config
  "Configuration for adaptive retry strategy."
  (atom {:enabled true
         :history-window-ms 300000
         :min-delay-ms 100
         :max-delay-ms 30000
         :success-threshold 0.8
         :failure-threshold 0.2}))

#?(:clj
   (defn get-adaptive-retry-config
     "Get current adaptive retry configuration."
     []
     @adaptive-retry-config))

#?(:clj
   (defn set-adaptive-retry-config
     "Update adaptive retry configuration."
     [config]
     (swap! adaptive-retry-config merge config)))

#?(:clj
   (defn record-retry-result
     "Record the result of a retry attempt for adaptive learning."
     [connector-type success? delay-ms]
     (let [now (System/currentTimeMillis)
           entry {:timestamp now
                  :success success?
                  :delay-ms delay-ms}]
       (swap! retry-history update connector-type (fnil conj []) entry))))

#?(:clj
   (defn cleanup-retry-history
     "Remove old entries from retry history."
     []
     (let [window (:history-window-ms @adaptive-retry-config)
           cutoff (- (System/currentTimeMillis) window)]
       (swap! retry-history
              (fn [history]
                (into {}
                      (map (fn [[k v]]
                             [k (vec (filter #(> (:timestamp %) cutoff) v))])
                           history)))))))

#?(:clj
   (defn calculate-success-rate
     "Calculate the success rate for a connector type."
     [connector-type]
     (let [entries (get @retry-history connector-type [])
           total (count entries)]
       (if (zero? total)
         1.0
         (/ (count (filter :success entries)) total)))))

#?(:clj
   (defn calculate-adaptive-delay
     "Calculate the adaptive delay based on recent success/failure rates."
     [connector-type base-delay-ms]
     (let [config @adaptive-retry-config
           success-rate (calculate-success-rate connector-type)]
       (cond
         ;; High success rate - use shorter delays
         (>= success-rate (:success-threshold config))
         (max (:min-delay-ms config) (/ base-delay-ms 2))
         
         ;; Low success rate - use longer delays
         (<= success-rate (:failure-threshold config))
         (min (:max-delay-ms config) (* base-delay-ms 2))
         
         ;; Normal - use base delay
         :else
         base-delay-ms))))

#?(:clj
   (defn with-adaptive-retry
     "Execute a function with adaptive retry strategy.
      Adjusts retry delays based on historical success/failure rates."
     [connector-type f & {:keys [max-retries base-delay-ms]
                          :or {max-retries 3 base-delay-ms 1000}}]
     (if-not (:enabled @adaptive-retry-config)
       (f)
       (loop [attempt 0]
         (let [result (try
                        {:success true :value (f)}
                        (catch Exception e
                          {:success false :error e}))]
           (if (:success result)
             (do
               (record-retry-result connector-type true 0)
               (:value result))
             (if (>= attempt max-retries)
               (do
                 (record-retry-result connector-type false 0)
                 (throw (:error result)))
               (let [delay-ms (calculate-adaptive-delay connector-type base-delay-ms)]
                 (record-retry-result connector-type false delay-ms)
                 (Thread/sleep delay-ms)
                 (recur (inc attempt))))))))))

#?(:clj
   (defn get-retry-stats
     "Get statistics about retry history."
     []
     (cleanup-retry-history)
     (let [history @retry-history]
       {:connector-stats
        (into {}
              (map (fn [[k v]]
                     [k {:total-attempts (count v)
                         :success-rate (calculate-success-rate k)
                         :avg-delay-ms (if (seq v)
                                         (/ (reduce + (map :delay-ms v)) (count v))
                                         0)}])
                   history))})))

;; Register built-in connector types
#?(:clj
   (do
     (register-connector-type :github
       (fn [{:keys [token]}] (create-github-connector token))
       :description "GitHub API connector"
       :required-config [:token])
     
     (register-connector-type :slack
       (fn [{:keys [token]}] (create-slack-connector token))
       :description "Slack API connector"
       :required-config [:token])
     
     (register-connector-type :huggingface
       (fn [{:keys [api-key]}] (create-huggingface-connector api-key))
       :description "Huggingface inference API connector"
       :required-config [:api-key])
     
     (register-connector-type :lm-studio
       (fn [{:keys [base-url] :or {base-url "http://localhost:1234"}}]
         (create-lm-studio-connector base-url))
       :description "LM Studio local LLM connector"
       :required-config [])
     
     (register-connector-type :zapier
       (fn [{:keys [webhook-url]}] (create-zapier-connector webhook-url))
       :description "Zapier webhook connector"
       :required-config [:webhook-url])
     
     (register-connector-type :web-scraper
       (fn [{:keys [user-agent] :or {user-agent "Mozilla/5.0"}}]
         (create-web-scraper-connector user-agent))
       :description "Web scraper connector"
       :required-config [])
     
     (register-connector-type :file
       (fn [{:keys [base-path]}] (create-file-connector base-path))
       :description "Local file system connector"
       :required-config [:base-path])))

