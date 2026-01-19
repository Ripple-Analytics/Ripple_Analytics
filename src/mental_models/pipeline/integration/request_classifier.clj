(ns mental-models.pipeline.integration.request-classifier
  "Request classifier for mental model analysis system.
   
   Features:
   - Request classification by type
   - Pattern-based classification
   - ML-based classification
   - Classification rules
   - Classification confidence
   - Classification caching
   - Classification metrics
   - Classification training"
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
           [java.util.regex Pattern]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:classifiers {}      ;; classifier-id -> classifier
         :rules {}            ;; rule-id -> rule
         :cache {}            ;; cache-key -> classification
         :config {:default-class :unknown
                  :cache-enabled? true
                  :cache-ttl-ms 60000
                  :min-confidence 0.5}
         :stats {:classifications 0
                 :cache-hits 0
                 :cache-misses 0
                 :by-class {}}
         :initialized? false}))

;; ============================================================================
;; Classification Classes
;; ============================================================================

(def request-classes
  {:api "API request"
   :web "Web page request"
   :static "Static resource request"
   :health "Health check request"
   :admin "Admin request"
   :auth "Authentication request"
   :webhook "Webhook request"
   :graphql "GraphQL request"
   :websocket "WebSocket request"
   :upload "File upload request"
   :download "File download request"
   :search "Search request"
   :analytics "Analytics request"
   :unknown "Unknown request type"})

;; ============================================================================
;; Classification Rules
;; ============================================================================

(defn register-rule!
  "Register a classification rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :class (get config :class :unknown)
              :condition-fn (get config :condition-fn)
              :pattern (get config :pattern)
              :headers (get config :headers {})
              :priority (get config :priority 100)
              :confidence (get config :confidence 1.0)
              :enabled? (atom true)
              :metrics {:matches (atom 0)}
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    (logging/log :info "Registered classification rule" {:rule-id rule-id :class (:class rule)})
    rule-id))

(defn get-rule
  "Get a classification rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn list-rules
  "List all classification rules."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :class (:class r)
           :priority (:priority r)
           :enabled? @(:enabled? r)
           :matches @(get-in r [:metrics :matches])})
        (:rules @state)))

(defn delete-rule!
  "Delete a classification rule."
  [rule-id]
  (swap! state update :rules dissoc rule-id))

;; ============================================================================
;; Rule Matching
;; ============================================================================

(defn- match-pattern?
  "Check if request matches a pattern."
  [request pattern]
  (when-let [uri (:uri request)]
    (re-matches (if (string? pattern) (re-pattern pattern) pattern) uri)))

(defn- match-headers?
  "Check if request matches header conditions."
  [request headers]
  (every? (fn [[header-name expected-value]]
            (let [actual-value (get-in request [:headers (str/lower-case header-name)])]
              (if (fn? expected-value)
                (expected-value actual-value)
                (= actual-value expected-value))))
          headers))

(defn- match-rule?
  "Check if a request matches a rule."
  [request rule]
  (when @(:enabled? rule)
    (cond
      ;; Custom condition function
      (:condition-fn rule)
      ((:condition-fn rule) request)
      
      ;; Pattern and headers
      (and (:pattern rule) (seq (:headers rule)))
      (and (match-pattern? request (:pattern rule))
           (match-headers? request (:headers rule)))
      
      ;; Pattern only
      (:pattern rule)
      (match-pattern? request (:pattern rule))
      
      ;; Headers only
      (seq (:headers rule))
      (match-headers? request (:headers rule))
      
      :else false)))

;; ============================================================================
;; Classification
;; ============================================================================

(defn- generate-cache-key
  "Generate a cache key for a request."
  [request]
  (str (:method request) "|" (:uri request) "|"
       (get-in request [:headers "content-type"])))

(defn- get-cached-classification
  "Get a cached classification."
  [cache-key]
  (when (get-in @state [:config :cache-enabled?])
    (when-let [cached (get-in @state [:cache cache-key])]
      (when (> (:expires-at cached) (System/currentTimeMillis))
        (swap! state update-in [:stats :cache-hits] inc)
        (:classification cached)))))

(defn- cache-classification!
  "Cache a classification."
  [cache-key classification]
  (when (get-in @state [:config :cache-enabled?])
    (let [ttl (get-in @state [:config :cache-ttl-ms])]
      (swap! state assoc-in [:cache cache-key]
             {:classification classification
              :expires-at (+ (System/currentTimeMillis) ttl)}))))

(defn classify-request
  "Classify a request."
  [request]
  (swap! state update-in [:stats :classifications] inc)
  
  (let [cache-key (generate-cache-key request)]
    (if-let [cached (get-cached-classification cache-key)]
      cached
      (do
        (swap! state update-in [:stats :cache-misses] inc)
        (let [rules (->> (vals (:rules @state))
                         (filter #@(:enabled? %))
                         (sort-by :priority))
              matching-rules (filter #(match-rule? request %) rules)
              best-match (first matching-rules)
              classification (if best-match
                               (do
                                 (swap! (get-in best-match [:metrics :matches]) inc)
                                 {:class (:class best-match)
                                  :confidence (:confidence best-match)
                                  :rule-id (:id best-match)
                                  :rule-name (:name best-match)})
                               {:class (get-in @state [:config :default-class])
                                :confidence 0.0
                                :rule-id nil
                                :rule-name nil})]
          
          (swap! state update-in [:stats :by-class (:class classification)] (fnil inc 0))
          (cache-classification! cache-key classification)
          classification)))))

(defn classify-with-all-matches
  "Classify a request and return all matching rules."
  [request]
  (let [rules (->> (vals (:rules @state))
                   (filter #@(:enabled? %))
                   (sort-by :priority))
        matching-rules (filter #(match-rule? request %) rules)]
    {:primary (first matching-rules)
     :all-matches (mapv (fn [r]
                          {:class (:class r)
                           :confidence (:confidence r)
                           :rule-id (:id r)
                           :rule-name (:name r)})
                        matching-rules)}))

;; ============================================================================
;; Custom Classifiers
;; ============================================================================

(defn register-classifier!
  "Register a custom classifier."
  [classifier-id config]
  (let [classifier {:id classifier-id
                    :name (get config :name (name classifier-id))
                    :classify-fn (get config :classify-fn)
                    :classes (get config :classes [])
                    :enabled? (atom true)
                    :metrics {:invocations (atom 0)}
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:classifiers classifier-id] classifier)
    (logging/log :info "Registered classifier" {:classifier-id classifier-id})
    classifier-id))

(defn classify-with-classifier
  "Classify using a specific classifier."
  [classifier-id request]
  (when-let [classifier (get-in @state [:classifiers classifier-id])]
    (when @(:enabled? classifier)
      (swap! (get-in classifier [:metrics :invocations]) inc)
      ((:classify-fn classifier) request))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-classify
  "Ring middleware to classify requests."
  [handler]
  (fn [request]
    (let [classification (classify-request request)]
      (handler (assoc request
                      :classification classification
                      :request-class (:class classification))))))

(defn wrap-route-by-class
  "Ring middleware to route requests by classification."
  [handlers]
  (fn [request]
    (let [classification (classify-request request)
          request-class (:class classification)
          handler (get handlers request-class (get handlers :default))]
      (if handler
        (handler (assoc request :classification classification))
        {:status 404 :body "No handler for request class"}))))

;; ============================================================================
;; Built-in Rules
;; ============================================================================

(defn register-default-rules!
  "Register default classification rules."
  []
  ;; API requests
  (register-rule! :api-json
                  {:name "JSON API Request"
                   :class :api
                   :pattern #"/api/.*"
                   :headers {"content-type" #(and % (str/includes? % "application/json"))}
                   :priority 10
                   :confidence 0.95})
  
  ;; Health checks
  (register-rule! :health-check
                  {:name "Health Check"
                   :class :health
                   :pattern #"/(health|healthz|ready|readyz|live|livez).*"
                   :priority 5
                   :confidence 1.0})
  
  ;; Static resources
  (register-rule! :static-assets
                  {:name "Static Assets"
                   :class :static
                   :pattern #".*\.(css|js|png|jpg|jpeg|gif|svg|ico|woff|woff2|ttf|eot)$"
                   :priority 20
                   :confidence 0.9})
  
  ;; GraphQL
  (register-rule! :graphql
                  {:name "GraphQL Request"
                   :class :graphql
                   :pattern #"/graphql.*"
                   :priority 15
                   :confidence 0.95})
  
  ;; Authentication
  (register-rule! :auth
                  {:name "Authentication Request"
                   :class :auth
                   :pattern #"/(auth|login|logout|oauth|token).*"
                   :priority 10
                   :confidence 0.9})
  
  ;; Webhooks
  (register-rule! :webhook
                  {:name "Webhook Request"
                   :class :webhook
                   :pattern #"/(webhook|hooks|callback).*"
                   :priority 15
                   :confidence 0.85})
  
  ;; Admin
  (register-rule! :admin
                  {:name "Admin Request"
                   :class :admin
                   :pattern #"/(admin|dashboard|manage).*"
                   :priority 10
                   :confidence 0.9}))

;; ============================================================================
;; Cache Management
;; ============================================================================

(defn clear-cache!
  "Clear the classification cache."
  []
  (swap! state assoc :cache {}))

(defn cleanup-expired-cache!
  "Remove expired cache entries."
  []
  (let [now (System/currentTimeMillis)]
    (swap! state update :cache
           (fn [cache]
             (into {} (filter (fn [[_ v]] (> (:expires-at v) now)) cache))))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-classifier-metrics
  "Get classifier metrics."
  []
  (let [stats (:stats @state)]
    {:classifications (:classifications stats)
     :cache-hits (:cache-hits stats)
     :cache-misses (:cache-misses stats)
     :cache-hit-rate (let [total (+ (:cache-hits stats) (:cache-misses stats))]
                       (if (pos? total)
                         (/ (:cache-hits stats) total)
                         0))
     :by-class (:by-class stats)
     :rules-count (count (:rules @state))
     :classifiers-count (count (:classifiers @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-classifier-stats
  "Get classifier statistics."
  []
  (merge (get-classifier-metrics)
         {:cache-enabled? (get-in @state [:config :cache-enabled?])
          :cache-size (count (:cache @state))
          :default-class (get-in @state [:config :default-class])}))

(defn reset-stats!
  "Reset classifier statistics."
  []
  (swap! state assoc :stats {:classifications 0
                             :cache-hits 0
                             :cache-misses 0
                             :by-class {}}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-classifier!
  "Initialize the request classifier."
  []
  (when-not (:initialized? @state)
    (register-default-rules!)
    
    ;; Start cache cleanup task
    (go-loop []
      (when (:initialized? @state)
        (<! (timeout 60000))
        (cleanup-expired-cache!)
        (recur)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request classifier initialized")
    (events/emit! :request-classifier-initialized {})
    true))
