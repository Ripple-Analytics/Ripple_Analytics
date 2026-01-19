(ns mental-models.pipeline.integration.response-merger
  "Response merger for mental model analysis system.
   
   Features:
   - Response merging
   - Deep merging
   - Conflict resolution
   - Merge strategies
   - Array merging
   - Merge validation
   - Merge history
   - Merging metrics"
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
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:strategies {}       ;; strategy-id -> strategy
         :history []          ;; merge history
         :config {:default-strategy :deep-merge
                  :array-strategy :concat
                  :conflict-strategy :prefer-new
                  :max-history 1000}
         :stats {:merges 0
                 :conflicts-resolved 0
                 :arrays-merged 0
                 :deep-merges 0}
         :initialized? false}))

;; ============================================================================
;; Merge Strategies
;; ============================================================================

(def built-in-strategies
  {:prefer-old (fn [old new] old)
   :prefer-new (fn [old new] new)
   :prefer-non-nil (fn [old new] (if (some? new) new old))
   :prefer-truthy (fn [old new] (if new new old))
   :combine-strings (fn [old new]
                      (if (and (string? old) (string? new))
                        (str old new)
                        new))
   :sum-numbers (fn [old new]
                  (if (and (number? old) (number? new))
                    (+ old new)
                    new))
   :max-numbers (fn [old new]
                  (if (and (number? old) (number? new))
                    (max old new)
                    new))
   :min-numbers (fn [old new]
                  (if (and (number? old) (number? new))
                    (min old new)
                    new))})

(def array-strategies
  {:concat (fn [old new] (vec (concat old new)))
   :union (fn [old new] (vec (distinct (concat old new))))
   :replace (fn [old new] new)
   :prefer-old (fn [old new] old)
   :interleave (fn [old new] (vec (interleave old new)))
   :merge-by-index (fn [old new]
                     (vec (map-indexed
                           (fn [i _]
                             (or (get new i) (get old i)))
                           (range (max (count old) (count new))))))})

;; ============================================================================
;; Custom Strategies
;; ============================================================================

(defn register-strategy!
  "Register a custom merge strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :merge-fn (get config :merge-fn)
                  :condition-fn (get config :condition-fn (constantly true))
                  :enabled? (atom true)
                  :metrics {:invocations (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered merge strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get a merge strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id])
      (when-let [built-in (get built-in-strategies strategy-id)]
        {:id strategy-id :merge-fn built-in})))

(defn list-strategies
  "List all strategies."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-strategies)
   (mapv (fn [[id s]]
           {:id id
            :name (:name s)
            :type :custom
            :enabled? @(:enabled? s)})
         (:strategies @state))))

;; ============================================================================
;; Deep Merging
;; ============================================================================

(defn- resolve-conflict
  "Resolve a merge conflict."
  [old-value new-value strategy]
  (swap! state update-in [:stats :conflicts-resolved] inc)
  (let [strategy-fn (cond
                      (fn? strategy) strategy
                      (keyword? strategy) (or (:merge-fn (get-strategy strategy))
                                              (get built-in-strategies strategy)
                                              (fn [_ n] n))
                      :else (fn [_ n] n))]
    (strategy-fn old-value new-value)))

(defn- merge-arrays
  "Merge two arrays."
  [old-arr new-arr strategy]
  (swap! state update-in [:stats :arrays-merged] inc)
  (let [strategy-fn (get array-strategies strategy
                         (get array-strategies :concat))]
    (strategy-fn old-arr new-arr)))

(defn deep-merge
  "Deep merge two maps."
  [old-map new-map & {:keys [conflict-strategy array-strategy]
                      :or {conflict-strategy (get-in @state [:config :conflict-strategy])
                           array-strategy (get-in @state [:config :array-strategy])}}]
  (swap! state update-in [:stats :deep-merges] inc)
  
  (letfn [(merge-fn [old new]
            (cond
              (and (map? old) (map? new))
              (merge-with merge-fn old new)
              
              (and (sequential? old) (sequential? new))
              (merge-arrays old new array-strategy)
              
              (and (some? old) (some? new))
              (resolve-conflict old new conflict-strategy)
              
              :else (if (some? new) new old)))]
    (merge-with merge-fn old-map new-map)))

;; ============================================================================
;; Selective Merging
;; ============================================================================

(defn merge-fields
  "Merge specific fields from source to target."
  [target source fields]
  (reduce (fn [t field]
            (if-let [value (get source field)]
              (assoc t field value)
              t))
          target
          fields))

(defn merge-nested
  "Merge a nested path from source to target."
  [target source path]
  (if-let [value (get-in source path)]
    (assoc-in target path value)
    target))

(defn merge-if
  "Merge if condition is met."
  [target source condition-fn]
  (if (condition-fn source)
    (deep-merge target source)
    target))

;; ============================================================================
;; Multi-Source Merging
;; ============================================================================

(defn merge-all
  "Merge multiple maps."
  [maps & {:keys [conflict-strategy array-strategy]}]
  (reduce (fn [result m]
            (deep-merge result m
                        :conflict-strategy conflict-strategy
                        :array-strategy array-strategy))
          {}
          maps))

(defn merge-with-priority
  "Merge maps with priority (higher priority wins)."
  [maps-with-priority]
  (let [sorted (sort-by :priority maps-with-priority)]
    (reduce (fn [result {:keys [data]}]
              (deep-merge result data))
            {}
            sorted)))

;; ============================================================================
;; Merge Validation
;; ============================================================================

(defn validate-merge
  "Validate a merge result."
  [result & {:keys [required-fields schema]}]
  (let [errors (atom [])]
    ;; Check required fields
    (when required-fields
      (doseq [field required-fields]
        (when-not (get result field)
          (swap! errors conj {:type :missing-field :field field}))))
    
    ;; Check schema (basic type checking)
    (when schema
      (doseq [[field expected-type] schema]
        (let [value (get result field)]
          (when (and value (not (instance? expected-type value)))
            (swap! errors conj {:type :type-mismatch
                                :field field
                                :expected expected-type
                                :actual (type value)})))))
    
    {:valid? (empty? @errors)
     :errors @errors
     :result result}))

;; ============================================================================
;; Merge History
;; ============================================================================

(defn- record-merge!
  "Record a merge operation."
  [sources result]
  (let [max-history (get-in @state [:config :max-history])
        record {:id (str (UUID/randomUUID))
                :sources-count (count sources)
                :result-keys (keys result)
                :timestamp (System/currentTimeMillis)}]
    (swap! state update :history
           (fn [h]
             (let [new-history (conj h record)]
               (if (> (count new-history) max-history)
                 (vec (drop 1 new-history))
                 new-history))))))

(defn get-merge-history
  "Get merge history."
  [& {:keys [limit] :or {limit 100}}]
  (take-last limit (:history @state)))

;; ============================================================================
;; Response Merging
;; ============================================================================

(defn merge-responses
  "Merge multiple API responses."
  [responses & {:keys [conflict-strategy array-strategy record?]
                :or {record? true}}]
  (swap! state update-in [:stats :merges] inc)
  
  (let [bodies (map :body responses)
        merged-body (merge-all bodies
                               :conflict-strategy conflict-strategy
                               :array-strategy array-strategy)
        merged-headers (apply merge (map :headers responses))
        result {:status (or (some #(when (< (:status %) 400) (:status %)) responses)
                            (:status (first responses)))
                :headers merged-headers
                :body merged-body}]
    
    (when record?
      (record-merge! bodies merged-body))
    
    result))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-merge-responses
  "Ring middleware to merge responses from multiple handlers."
  [handlers & {:keys [conflict-strategy array-strategy]}]
  (fn [request]
    (let [responses (mapv #(% request) handlers)]
      (merge-responses responses
                       :conflict-strategy conflict-strategy
                       :array-strategy array-strategy))))

(defn wrap-merge-with-default
  "Ring middleware to merge response with default values."
  [handler default-values]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (deep-merge default-values body))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set the default merge strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-array-strategy!
  "Set the default array merge strategy."
  [strategy]
  (swap! state assoc-in [:config :array-strategy] strategy))

(defn set-conflict-strategy!
  "Set the default conflict resolution strategy."
  [strategy]
  (swap! state assoc-in [:config :conflict-strategy] strategy))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-merger-metrics
  "Get merger metrics."
  []
  (let [stats (:stats @state)]
    {:merges (:merges stats)
     :conflicts-resolved (:conflicts-resolved stats)
     :arrays-merged (:arrays-merged stats)
     :deep-merges (:deep-merges stats)
     :strategies-count (+ (count built-in-strategies)
                          (count (:strategies @state)))
     :history-size (count (:history @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-merger-stats
  "Get merger statistics."
  []
  (merge (get-merger-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :array-strategy (get-in @state [:config :array-strategy])
          :conflict-strategy (get-in @state [:config :conflict-strategy])}))

(defn reset-stats!
  "Reset merger statistics."
  []
  (swap! state assoc :stats {:merges 0
                             :conflicts-resolved 0
                             :arrays-merged 0
                             :deep-merges 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-merger!
  "Initialize the response merger."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response merger initialized")
    (events/emit! :response-merger-initialized {})
    true))
