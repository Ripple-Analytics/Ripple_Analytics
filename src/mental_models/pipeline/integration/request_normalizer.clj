(ns mental-models.pipeline.integration.request-normalizer
  "Request normalizer for mental model analysis system.
   
   Features:
   - URL normalization
   - Header normalization
   - Query parameter normalization
   - Body normalization
   - Path normalization
   - Case normalization
   - Whitespace normalization
   - Normalization metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.net URLDecoder URLEncoder]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:rules {}            ;; rule-id -> rule
         :config {:normalize-headers? true
                  :normalize-query? true
                  :normalize-path? true
                  :normalize-body? false
                  :lowercase-headers? true
                  :sort-query-params? true
                  :remove-trailing-slashes? true
                  :decode-url? true}
         :stats {:requests-normalized 0
                 :headers-normalized 0
                 :paths-normalized 0
                 :queries-normalized 0}
         :initialized? false}))

;; ============================================================================
;; URL Encoding/Decoding
;; ============================================================================

(defn- url-decode
  "URL decode a string."
  [s]
  (when s
    (try
      (URLDecoder/decode s "UTF-8")
      (catch Exception _ s))))

(defn- url-encode
  "URL encode a string."
  [s]
  (when s
    (try
      (URLEncoder/encode s "UTF-8")
      (catch Exception _ s))))

;; ============================================================================
;; Header Normalization
;; ============================================================================

(defn normalize-header-name
  "Normalize a header name."
  [name]
  (when name
    (if (get-in @state [:config :lowercase-headers?])
      (str/lower-case name)
      name)))

(defn normalize-header-value
  "Normalize a header value."
  [value]
  (when value
    (-> value
        str/trim
        (str/replace #"\s+" " "))))

(defn normalize-headers
  "Normalize all headers."
  [headers]
  (when (and headers (get-in @state [:config :normalize-headers?]))
    (swap! state update-in [:stats :headers-normalized] inc)
    (into {}
          (map (fn [[k v]]
                 [(normalize-header-name k) (normalize-header-value v)])
               headers))))

;; ============================================================================
;; Path Normalization
;; ============================================================================

(defn- remove-dot-segments
  "Remove . and .. segments from a path."
  [path]
  (loop [segments (str/split path #"/")
         result []]
    (if (empty? segments)
      (str "/" (str/join "/" result))
      (let [segment (first segments)]
        (cond
          (= segment ".") (recur (rest segments) result)
          (= segment "..") (recur (rest segments) (if (empty? result) [] (pop result)))
          (empty? segment) (recur (rest segments) result)
          :else (recur (rest segments) (conj result segment)))))))

(defn- remove-trailing-slash
  "Remove trailing slash from path."
  [path]
  (if (and (get-in @state [:config :remove-trailing-slashes?])
           (> (count path) 1)
           (str/ends-with? path "/"))
    (subs path 0 (dec (count path)))
    path))

(defn normalize-path
  "Normalize a URL path."
  [path]
  (when (and path (get-in @state [:config :normalize-path?]))
    (swap! state update-in [:stats :paths-normalized] inc)
    (let [decoded (if (get-in @state [:config :decode-url?])
                    (url-decode path)
                    path)]
      (-> decoded
          remove-dot-segments
          remove-trailing-slash
          (str/replace #"/+" "/")))))

;; ============================================================================
;; Query Parameter Normalization
;; ============================================================================

(defn- parse-query-string
  "Parse a query string into a map."
  [query-string]
  (when (and query-string (not (empty? query-string)))
    (let [pairs (str/split query-string #"&")]
      (reduce (fn [m pair]
                (let [[k v] (str/split pair #"=" 2)]
                  (assoc m (url-decode k) (url-decode (or v "")))))
              {}
              pairs))))

(defn- build-query-string
  "Build a query string from a map."
  [params]
  (when (and params (not (empty? params)))
    (let [sorted-params (if (get-in @state [:config :sort-query-params?])
                          (sort-by first params)
                          params)]
      (str/join "&"
                (map (fn [[k v]]
                       (str (url-encode k) "=" (url-encode v)))
                     sorted-params)))))

(defn normalize-query-string
  "Normalize a query string."
  [query-string]
  (when (and query-string (get-in @state [:config :normalize-query?]))
    (swap! state update-in [:stats :queries-normalized] inc)
    (-> query-string
        parse-query-string
        build-query-string)))

;; ============================================================================
;; Body Normalization
;; ============================================================================

(defn- normalize-json-body
  "Normalize a JSON body."
  [body]
  (when body
    (try
      (let [parsed (edn/read-string body)]
        (pr-str parsed))
      (catch Exception _ body))))

(defn- normalize-whitespace
  "Normalize whitespace in a string."
  [s]
  (when s
    (-> s
        str/trim
        (str/replace #"\s+" " "))))

(defn normalize-body
  "Normalize a request body."
  [body content-type]
  (when (and body (get-in @state [:config :normalize-body?]))
    (cond
      (str/includes? (or content-type "") "application/json")
      (normalize-json-body body)
      
      (str/includes? (or content-type "") "text/")
      (normalize-whitespace body)
      
      :else body)))

;; ============================================================================
;; Custom Rules
;; ============================================================================

(defn register-rule!
  "Register a normalization rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :type (get config :type :header)  ;; :header, :path, :query, :body
              :pattern (get config :pattern)
              :replacement (get config :replacement)
              :transform-fn (get config :transform-fn)
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Registered normalization rule" {:rule-id rule-id})
    rule-id))

(defn get-rule
  "Get a normalization rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all normalization rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :type (:type r)
           :enabled? @(:enabled? r)})
        (:rules @state)))

(defn delete-rule!
  "Delete a normalization rule."
  [rule-id]
  (swap! state update :rules dissoc rule-id))

(defn- apply-rules
  "Apply custom rules to a value."
  [value type]
  (reduce (fn [v [_ rule]]
            (if (and @(:enabled? rule) (= (:type rule) type))
              (cond
                (:transform-fn rule) ((:transform-fn rule) v)
                (:pattern rule) (str/replace v (:pattern rule) (:replacement rule))
                :else v)
              v))
          value
          (:rules @state)))

;; ============================================================================
;; Full Request Normalization
;; ============================================================================

(defn normalize-request
  "Normalize a complete request."
  [request]
  (swap! state update-in [:stats :requests-normalized] inc)
  
  (let [headers (normalize-headers (:headers request))
        path (normalize-path (:uri request))
        query (normalize-query-string (:query-string request))
        content-type (get headers "content-type")
        body (normalize-body (:body request) content-type)]
    
    (cond-> request
      headers (assoc :headers headers)
      path (assoc :uri path)
      query (assoc :query-string query)
      body (assoc :body body)
      true (assoc :normalized? true))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-normalize-request
  "Ring middleware to normalize requests."
  [handler]
  (fn [request]
    (handler (normalize-request request))))

(defn wrap-normalize-headers
  "Ring middleware to normalize headers only."
  [handler]
  (fn [request]
    (handler (update request :headers normalize-headers))))

(defn wrap-normalize-path
  "Ring middleware to normalize path only."
  [handler]
  (fn [request]
    (handler (update request :uri normalize-path))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-normalize-headers!
  "Enable/disable header normalization."
  [enabled?]
  (swap! state assoc-in [:config :normalize-headers?] enabled?))

(defn set-normalize-query!
  "Enable/disable query normalization."
  [enabled?]
  (swap! state assoc-in [:config :normalize-query?] enabled?))

(defn set-normalize-path!
  "Enable/disable path normalization."
  [enabled?]
  (swap! state assoc-in [:config :normalize-path?] enabled?))

(defn set-normalize-body!
  "Enable/disable body normalization."
  [enabled?]
  (swap! state assoc-in [:config :normalize-body?] enabled?))

(defn set-lowercase-headers!
  "Enable/disable lowercase headers."
  [enabled?]
  (swap! state assoc-in [:config :lowercase-headers?] enabled?))

(defn set-sort-query-params!
  "Enable/disable query parameter sorting."
  [enabled?]
  (swap! state assoc-in [:config :sort-query-params?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-normalizer-metrics
  "Get normalizer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-normalized (:requests-normalized stats)
     :headers-normalized (:headers-normalized stats)
     :paths-normalized (:paths-normalized stats)
     :queries-normalized (:queries-normalized stats)
     :rules-count (count (:rules @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-normalizer-stats
  "Get normalizer statistics."
  []
  (merge (get-normalizer-metrics)
         {:config (:config @state)}))

(defn reset-stats!
  "Reset normalizer statistics."
  []
  (swap! state assoc :stats {:requests-normalized 0
                             :headers-normalized 0
                             :paths-normalized 0
                             :queries-normalized 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-normalizer!
  "Initialize the request normalizer."
  []
  (when-not (:initialized? @state)
    ;; Register default rules
    (register-rule! :remove-utm
                    {:name "Remove UTM Parameters"
                     :type :query
                     :transform-fn (fn [q]
                                     (when q
                                       (let [params (parse-query-string q)
                                             filtered (into {} (remove (fn [[k _]]
                                                                         (str/starts-with? k "utm_"))
                                                                       params))]
                                         (build-query-string filtered))))})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request normalizer initialized")
    (events/emit! :request-normalizer-initialized {})
    true))
