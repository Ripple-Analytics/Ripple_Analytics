(ns mental-models.connectors.section-21
  "Connectors Module - Section 21"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

#?(:clj
   (defn bulk-request
     "Execute multiple requests in parallel with rate limiting and circuit breaker protection.
      Returns a vector of results in the same order as requests.
      
      requests: vector of {:connector-type :type :connector connector :fn function}
      options: {:max-parallel N :timeout-ms N}"
     [requests & {:keys [max-parallel timeout-ms] :or {max-parallel 4 timeout-ms 30000}}]
     (let [futures (doall
                     (map-indexed
                       (fn [idx {:keys [connector-type connector fn]}]
                         [idx
                          (future
                            (try
                              (wait-for-rate-limit connector-type)
                              (with-circuit-breaker connector-type
                                #(fn connector))
                              (catch Exception e
                                {:success false :error (.getMessage e) :index idx})))])
                       requests))
           results (into {}
                         (map (fn [[idx fut]]
                                [idx (deref fut timeout-ms {:success false :error "Timeout" :index idx})])
                              futures))]
       (mapv #(get results %) (range (count requests))))))

#?(:clj
   (defn bulk-github-repos
     "Fetch multiple GitHub repositories in parallel."
     [connector repos]
     (bulk-request
       (mapv (fn [[owner repo]]
               {:connector-type :github
                :connector connector
                :fn #(github-get-repo % owner repo)})
             repos))))

#?(:clj
   (defn bulk-scrape-urls
     "Scrape multiple URLs in parallel with rate limiting."
     [connector urls & {:keys [max-parallel] :or {max-parallel 2}}]
     (bulk-request
       (mapv (fn [url]
               {:connector-type :web-scraper
                :connector connector
                :fn #(scrape-url % url)})
             urls)
       :max-parallel max-parallel)))

#?(:clj
   (defn bulk-huggingface-inference
     "Run inference on multiple inputs in parallel."
     [connector model-id inputs]
     (bulk-request
       (mapv (fn [input]
               {:connector-type :huggingface
                :connector connector
                :fn #(huggingface-inference % model-id input)})
             inputs))))

#?(:clj
   (defn bulk-lm-studio-chat
     "Send multiple chat requests to LM Studio in parallel."
     [connector messages-list & {:keys [temperature max-tokens] :or {temperature 0.7 max-tokens 1000}}]
     (bulk-request
       (mapv (fn [messages]
               {:connector-type :lm-studio
                :connector connector
                :fn #(lm-studio-chat % messages :temperature temperature :max-tokens max-tokens)})
             messages-list))))

;; ============================================
;; Connector Lifecycle Management
;; ============================================

(def active-connectors
  "Atom storing active connector instances."
  (atom {}))

#?(:clj
   (defn register-connector
     "Register a connector instance for lifecycle management."
     [name connector-type connector]
     (swap! active-connectors assoc name
            {:type connector-type
             :connector connector
             :registered-at (java.time.Instant/now)
             :status :active})))

#?(:clj
   (defn unregister-connector
     "Unregister a connector instance."
     [name]
     (swap! active-connectors dissoc name)))

#?(:clj
   (defn get-connector
     "Get a registered connector by name."
     [name]
     (get-in @active-connectors [name :connector])))

#?(:clj
   (defn list-active-connectors
     "List all active registered connectors."
     []
     (mapv (fn [[name info]]
             {:name name
              :type (:type info)
              :registered-at (:registered-at info)
              :status (:status info)})
           @active-connectors)))

#?(:clj
   (defn health-check-all
     "Run health checks on all registered connectors."
     []
     (into {}
           (map (fn [[name info]]
                  [name (health-check-connector (:type info) (:connector info))])
                @active-connectors))))

#?(:clj
   (defn shutdown-all-connectors
     "Shutdown all registered connectors and clean up resources."
     []
     (reset! active-connectors {})
     (reset-all-circuits)
     (reset-metrics)
     (clear-cache)
     {:status :shutdown :timestamp (java.time.Instant/now)}))

;; ============================================
;; Connector Factory Pattern
