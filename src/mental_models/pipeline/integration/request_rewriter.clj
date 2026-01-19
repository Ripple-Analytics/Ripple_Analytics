(ns mental-models.pipeline.integration.request-rewriter
  "Request rewriter for mental model analysis system.
   
   Features:
   - URL rewriting
   - Header rewriting
   - Body rewriting
   - Query parameter rewriting
   - Path rewriting
   - Rewrite rules
   - Conditional rewriting
   - Rewriting metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:rules []            ;; rewrite rules
         :config {:enabled? true
                  :max-rewrites 10
                  :log-rewrites? true}
         :stats {:requests-rewritten 0
                 :urls-rewritten 0
                 :headers-rewritten 0
                 :bodies-rewritten 0
                 :rules-applied 0}
         :initialized? false}))

;; ============================================================================
;; Rewrite Rules
;; ============================================================================

(defn register-rule!
  "Register a rewrite rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :type (get config :type :url)  ;; :url, :header, :body, :query, :path
              :pattern (when-let [p (get config :pattern)]
                         (if (string? p) (re-pattern p) p))
              :replacement (get config :replacement)
              :transform-fn (get config :transform-fn)
              :condition-fn (get config :condition-fn (constantly true))
              :priority (get config :priority 100)
              :enabled? (atom true)
              :metrics {:applications (atom 0)}
              :created-at (System/currentTimeMillis)}]
    
    (swap! state update :rules
           (fn [rules]
             (->> (conj rules rule)
                  (sort-by :priority))))
    (logging/log :info "Registered rewrite rule" {:rule-id rule-id :type (:type rule)})
    rule-id))

(defn get-rule
  "Get a rewrite rule."
  [rule-id]
  (first (filter #(= (:id %) rule-id) (:rules @state))))

(defn list-rules
  "List all rewrite rules."
  []
  (mapv (fn [r]
          {:id (:id r)
           :name (:name r)
           :type (:type r)
           :priority (:priority r)
           :enabled? @(:enabled? r)
           :applications @(get-in r [:metrics :applications])})
        (:rules @state)))

(defn delete-rule!
  "Delete a rewrite rule."
  [rule-id]
  (swap! state update :rules
         (fn [rules]
           (vec (remove #(= (:id %) rule-id) rules)))))

(defn enable-rule!
  "Enable a rewrite rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) true)))

(defn disable-rule!
  "Disable a rewrite rule."
  [rule-id]
  (when-let [rule (get-rule rule-id)]
    (reset! (:enabled? rule) false)))

;; ============================================================================
;; URL Rewriting
;; ============================================================================

(defn rewrite-url
  "Rewrite a URL using pattern matching."
  [url pattern replacement]
  (when (and url pattern replacement)
    (str/replace url pattern replacement)))

(defn- apply-url-rules
  "Apply URL rewrite rules to a request."
  [request]
  (let [url-rules (filter #(and (= (:type %) :url) @(:enabled? %)) (:rules @state))]
    (reduce (fn [req rule]
              (if ((:condition-fn rule) req)
                (let [uri (:uri req)
                      new-uri (if (:transform-fn rule)
                                ((:transform-fn rule) uri)
                                (rewrite-url uri (:pattern rule) (:replacement rule)))]
                  (when (not= uri new-uri)
                    (swap! (get-in rule [:metrics :applications]) inc)
                    (swap! state update-in [:stats :urls-rewritten] inc)
                    (swap! state update-in [:stats :rules-applied] inc))
                  (assoc req :uri (or new-uri uri)))
                req))
            request
            url-rules)))

;; ============================================================================
;; Header Rewriting
;; ============================================================================

(defn rewrite-header
  "Rewrite a header value."
  [headers header-name pattern replacement]
  (if-let [value (get headers header-name)]
    (assoc headers header-name (str/replace value pattern replacement))
    headers))

(defn- apply-header-rules
  "Apply header rewrite rules to a request."
  [request]
  (let [header-rules (filter #(and (= (:type %) :header) @(:enabled? %)) (:rules @state))]
    (reduce (fn [req rule]
              (if ((:condition-fn rule) req)
                (let [headers (:headers req)
                      new-headers (if (:transform-fn rule)
                                    ((:transform-fn rule) headers)
                                    (reduce (fn [h [k v]]
                                              (if (and (:pattern rule) (re-find (:pattern rule) (str v)))
                                                (assoc h k (str/replace (str v) (:pattern rule) (:replacement rule)))
                                                h))
                                            headers
                                            headers))]
                  (when (not= headers new-headers)
                    (swap! (get-in rule [:metrics :applications]) inc)
                    (swap! state update-in [:stats :headers-rewritten] inc)
                    (swap! state update-in [:stats :rules-applied] inc))
                  (assoc req :headers new-headers))
                req))
            request
            header-rules)))

;; ============================================================================
;; Body Rewriting
;; ============================================================================

(defn rewrite-body
  "Rewrite a request body."
  [body pattern replacement]
  (cond
    (string? body)
    (str/replace body pattern replacement)
    
    (map? body)
    (walk/postwalk
     (fn [x]
       (if (string? x)
         (str/replace x pattern replacement)
         x))
     body)
    
    :else body))

(defn- apply-body-rules
  "Apply body rewrite rules to a request."
  [request]
  (let [body-rules (filter #(and (= (:type %) :body) @(:enabled? %)) (:rules @state))]
    (reduce (fn [req rule]
              (if (and (:body req) ((:condition-fn rule) req))
                (let [body (:body req)
                      new-body (if (:transform-fn rule)
                                 ((:transform-fn rule) body)
                                 (rewrite-body body (:pattern rule) (:replacement rule)))]
                  (when (not= body new-body)
                    (swap! (get-in rule [:metrics :applications]) inc)
                    (swap! state update-in [:stats :bodies-rewritten] inc)
                    (swap! state update-in [:stats :rules-applied] inc))
                  (assoc req :body new-body))
                req))
            request
            body-rules)))

;; ============================================================================
;; Query Parameter Rewriting
;; ============================================================================

(defn rewrite-query-params
  "Rewrite query parameters."
  [params rules]
  (reduce (fn [p [param-name transform]]
            (if-let [value (get p param-name)]
              (assoc p param-name (transform value))
              p))
          params
          rules))

(defn- apply-query-rules
  "Apply query parameter rewrite rules to a request."
  [request]
  (let [query-rules (filter #(and (= (:type %) :query) @(:enabled? %)) (:rules @state))]
    (reduce (fn [req rule]
              (if (and (:query-params req) ((:condition-fn rule) req))
                (let [params (:query-params req)
                      new-params (if (:transform-fn rule)
                                   ((:transform-fn rule) params)
                                   params)]
                  (when (not= params new-params)
                    (swap! (get-in rule [:metrics :applications]) inc)
                    (swap! state update-in [:stats :rules-applied] inc))
                  (assoc req :query-params new-params))
                req))
            request
            query-rules)))

;; ============================================================================
;; Path Rewriting
;; ============================================================================

(defn rewrite-path
  "Rewrite a path segment."
  [path segment-index transform-fn]
  (let [segments (str/split path #"/")
        new-segments (update segments segment-index transform-fn)]
    (str/join "/" new-segments)))

(defn- apply-path-rules
  "Apply path rewrite rules to a request."
  [request]
  (let [path-rules (filter #(and (= (:type %) :path) @(:enabled? %)) (:rules @state))]
    (reduce (fn [req rule]
              (if ((:condition-fn rule) req)
                (let [uri (:uri req)
                      new-uri (if (:transform-fn rule)
                                ((:transform-fn rule) uri)
                                uri)]
                  (when (not= uri new-uri)
                    (swap! (get-in rule [:metrics :applications]) inc)
                    (swap! state update-in [:stats :rules-applied] inc))
                  (assoc req :uri new-uri))
                req))
            request
            path-rules)))

;; ============================================================================
;; Full Request Rewriting
;; ============================================================================

(defn rewrite-request
  "Apply all rewrite rules to a request."
  [request]
  (when (get-in @state [:config :enabled?])
    (swap! state update-in [:stats :requests-rewritten] inc)
    
    (-> request
        apply-url-rules
        apply-header-rules
        apply-body-rules
        apply-query-rules
        apply-path-rules)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-rewrite
  "Ring middleware to rewrite requests."
  [handler]
  (fn [request]
    (let [rewritten (rewrite-request request)]
      (handler (or rewritten request)))))

(defn wrap-rewrite-response
  "Ring middleware to rewrite responses."
  [handler rewrite-fn]
  (fn [request]
    (let [response (handler request)]
      (rewrite-fn response))))

;; ============================================================================
;; Common Rewrite Patterns
;; ============================================================================

(defn add-api-version-prefix!
  "Add API version prefix to URLs."
  [version]
  (register-rule! :api-version-prefix
                  {:name "API Version Prefix"
                   :type :url
                   :transform-fn (fn [uri]
                                   (if (str/starts-with? uri "/api/")
                                     (str "/api/" version (subs uri 4))
                                     uri))}))

(defn strip-trailing-slash!
  "Strip trailing slashes from URLs."
  []
  (register-rule! :strip-trailing-slash
                  {:name "Strip Trailing Slash"
                   :type :url
                   :pattern #"/$"
                   :replacement ""
                   :condition-fn (fn [req]
                                   (and (:uri req)
                                        (> (count (:uri req)) 1)))}))

(defn lowercase-headers!
  "Lowercase all header names."
  []
  (register-rule! :lowercase-headers
                  {:name "Lowercase Headers"
                   :type :header
                   :transform-fn (fn [headers]
                                   (into {}
                                         (map (fn [[k v]]
                                                [(str/lower-case (name k)) v])
                                              headers)))}))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable rewriting."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-max-rewrites!
  "Set maximum rewrites per request."
  [max-rewrites]
  (swap! state assoc-in [:config :max-rewrites] max-rewrites))

(defn set-log-rewrites!
  "Enable/disable rewrite logging."
  [enabled?]
  (swap! state assoc-in [:config :log-rewrites?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-rewriter-metrics
  "Get rewriter metrics."
  []
  (let [stats (:stats @state)]
    {:requests-rewritten (:requests-rewritten stats)
     :urls-rewritten (:urls-rewritten stats)
     :headers-rewritten (:headers-rewritten stats)
     :bodies-rewritten (:bodies-rewritten stats)
     :rules-applied (:rules-applied stats)
     :rules-count (count (:rules @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-rewriter-stats
  "Get rewriter statistics."
  []
  (merge (get-rewriter-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :max-rewrites (get-in @state [:config :max-rewrites])
          :log-rewrites? (get-in @state [:config :log-rewrites?])}))

(defn reset-stats!
  "Reset rewriter statistics."
  []
  (swap! state assoc :stats {:requests-rewritten 0
                             :urls-rewritten 0
                             :headers-rewritten 0
                             :bodies-rewritten 0
                             :rules-applied 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-rewriter!
  "Initialize the request rewriter."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request rewriter initialized")
    (events/emit! :request-rewriter-initialized {})
    true))
