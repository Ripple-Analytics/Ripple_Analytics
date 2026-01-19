(ns mental-models.pipeline.integration.response-aggregator
  "Response aggregator for mental model analysis system.
   
   Features:
   - Response aggregation
   - Statistical aggregation
   - Time-based aggregation
   - Group aggregation
   - Custom aggregators
   - Aggregation windows
   - Aggregation pipelines
   - Aggregation metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZoneId]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:aggregators {}      ;; aggregator-id -> aggregator
         :windows {}          ;; window-id -> window
         :pipelines {}        ;; pipeline-id -> pipeline
         :config {:default-window-ms 60000
                  :max-window-items 10000}
         :stats {:aggregations 0
                 :items-aggregated 0
                 :windows-created 0
                 :pipelines-run 0}
         :initialized? false}))

;; ============================================================================
;; Built-in Aggregators
;; ============================================================================

(def built-in-aggregators
  {:count (fn [items _] (count items))
   
   :sum (fn [items field]
          (reduce + 0 (map #(get % field 0) items)))
   
   :avg (fn [items field]
          (let [values (map #(get % field 0) items)]
            (if (seq values)
              (/ (reduce + values) (count values))
              0)))
   
   :min (fn [items field]
          (when (seq items)
            (apply min (map #(get % field) items))))
   
   :max (fn [items field]
          (when (seq items)
            (apply max (map #(get % field) items))))
   
   :first (fn [items _]
            (first items))
   
   :last (fn [items _]
           (last items))
   
   :distinct (fn [items field]
               (distinct (map #(get % field) items)))
   
   :distinct-count (fn [items field]
                     (count (distinct (map #(get % field) items))))
   
   :percentile (fn [items field percentile]
                 (let [sorted (sort (map #(get % field 0) items))
                       idx (int (* percentile (count sorted)))]
                   (nth sorted (min idx (dec (count sorted))))))
   
   :median (fn [items field]
             (let [sorted (sort (map #(get % field 0) items))
                   n (count sorted)
                   mid (quot n 2)]
               (if (odd? n)
                 (nth sorted mid)
                 (/ (+ (nth sorted mid) (nth sorted (dec mid))) 2))))
   
   :std-dev (fn [items field]
              (let [values (map #(get % field 0) items)
                    n (count values)
                    mean (/ (reduce + values) n)
                    variance (/ (reduce + (map #(Math/pow (- % mean) 2) values)) n)]
                (Math/sqrt variance)))
   
   :collect (fn [items field]
              (mapv #(get % field) items))
   
   :group-by (fn [items field]
               (group-by #(get % field) items))})

;; ============================================================================
;; Custom Aggregators
;; ============================================================================

(defn register-aggregator!
  "Register a custom aggregator."
  [aggregator-id config]
  (let [aggregator {:id aggregator-id
                    :name (get config :name (name aggregator-id))
                    :aggregate-fn (get config :aggregate-fn)
                    :enabled? (atom true)
                    :metrics {:invocations (atom 0)}
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:aggregators aggregator-id] aggregator)
    (logging/log :info "Registered aggregator" {:aggregator-id aggregator-id})
    aggregator-id))

(defn get-aggregator
  "Get an aggregator."
  [aggregator-id]
  (or (get-in @state [:aggregators aggregator-id])
      (when-let [built-in (get built-in-aggregators aggregator-id)]
        {:id aggregator-id :aggregate-fn built-in})))

(defn list-aggregators
  "List all aggregators."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-aggregators)
   (mapv (fn [[id a]]
           {:id id
            :name (:name a)
            :type :custom
            :enabled? @(:enabled? a)})
         (:aggregators @state))))

;; ============================================================================
;; Basic Aggregation
;; ============================================================================

(defn aggregate
  "Aggregate items using specified aggregators."
  [items aggregations]
  (swap! state update-in [:stats :aggregations] inc)
  (swap! state update-in [:stats :items-aggregated] + (count items))
  
  (into {}
        (for [[result-key [agg-type field & args]] aggregations]
          (let [aggregator (or (:aggregate-fn (get-aggregator agg-type))
                               (get built-in-aggregators agg-type)
                               (constantly nil))]
            [result-key (apply aggregator items field args)]))))

(defn aggregate-responses
  "Aggregate multiple API responses."
  [responses aggregations]
  (let [bodies (map :body responses)
        all-items (mapcat (fn [b]
                            (cond
                              (sequential? b) b
                              (map? b) (or (:items b) (:data b) [b])
                              :else [b]))
                          bodies)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (aggregate (vec all-items) aggregations)}))

;; ============================================================================
;; Group Aggregation
;; ============================================================================

(defn aggregate-by-group
  "Aggregate items grouped by a field."
  [items group-field aggregations]
  (let [groups (group-by #(get % group-field) items)]
    (into {}
          (for [[group-key group-items] groups]
            [group-key (aggregate group-items aggregations)]))))

(defn aggregate-by-multiple-groups
  "Aggregate items grouped by multiple fields."
  [items group-fields aggregations]
  (let [groups (group-by (fn [item]
                           (mapv #(get item %) group-fields))
                         items)]
    (into {}
          (for [[group-key group-items] groups]
            [group-key (aggregate group-items aggregations)]))))

;; ============================================================================
;; Time-Based Aggregation
;; ============================================================================

(defn aggregate-by-time
  "Aggregate items by time intervals."
  [items timestamp-field interval-ms aggregations]
  (let [groups (group-by (fn [item]
                           (let [ts (get item timestamp-field 0)]
                             (* (quot ts interval-ms) interval-ms)))
                         items)]
    (into (sorted-map)
          (for [[interval-start group-items] groups]
            [interval-start (aggregate group-items aggregations)]))))

(defn aggregate-by-hour
  "Aggregate items by hour."
  [items timestamp-field aggregations]
  (aggregate-by-time items timestamp-field 3600000 aggregations))

(defn aggregate-by-day
  "Aggregate items by day."
  [items timestamp-field aggregations]
  (aggregate-by-time items timestamp-field 86400000 aggregations))

;; ============================================================================
;; Aggregation Windows
;; ============================================================================

(defn create-window!
  "Create an aggregation window."
  [window-id config]
  (let [window {:id window-id
                :name (get config :name (name window-id))
                :duration-ms (get config :duration-ms
                                  (get-in @state [:config :default-window-ms]))
                :items (atom [])
                :aggregations (get config :aggregations {})
                :on-complete (get config :on-complete)
                :started-at (atom nil)
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:windows window-id] window)
    (swap! state update-in [:stats :windows-created] inc)
    window-id))

(defn add-to-window!
  "Add an item to a window."
  [window-id item]
  (when-let [window (get-in @state [:windows window-id])]
    (when @(:enabled? window)
      ;; Start window if not started
      (when-not @(:started-at window)
        (reset! (:started-at window) (System/currentTimeMillis)))
      
      ;; Add item
      (let [max-items (get-in @state [:config :max-window-items])]
        (swap! (:items window)
               (fn [items]
                 (let [new-items (conj items item)]
                   (if (> (count new-items) max-items)
                     (vec (drop 1 new-items))
                     new-items)))))
      
      ;; Check if window should close
      (let [started @(:started-at window)
            duration (:duration-ms window)
            now (System/currentTimeMillis)]
        (when (and started (> (- now started) duration))
          (close-window! window-id))))))

(defn close-window!
  "Close a window and aggregate results."
  [window-id]
  (when-let [window (get-in @state [:windows window-id])]
    (let [items @(:items window)
          result (aggregate items (:aggregations window))]
      
      ;; Call on-complete callback
      (when-let [callback (:on-complete window)]
        (callback result))
      
      ;; Reset window
      (reset! (:items window) [])
      (reset! (:started-at window) nil)
      
      result)))

(defn get-window-state
  "Get the current state of a window."
  [window-id]
  (when-let [window (get-in @state [:windows window-id])]
    {:id window-id
     :items-count (count @(:items window))
     :started-at @(:started-at window)
     :duration-ms (:duration-ms window)
     :enabled? @(:enabled? window)}))

;; ============================================================================
;; Aggregation Pipelines
;; ============================================================================

(defn create-pipeline!
  "Create an aggregation pipeline."
  [pipeline-id config]
  (let [pipeline {:id pipeline-id
                  :name (get config :name (name pipeline-id))
                  :stages (get config :stages [])
                  :enabled? (atom true)
                  :metrics {:runs (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:pipelines pipeline-id] pipeline)
    pipeline-id))

(defn run-pipeline
  "Run an aggregation pipeline."
  [pipeline-id items]
  (when-let [pipeline (get-in @state [:pipelines pipeline-id])]
    (when @(:enabled? pipeline)
      (swap! (get-in pipeline [:metrics :runs]) inc)
      (swap! state update-in [:stats :pipelines-run] inc)
      
      (reduce (fn [data stage]
                (case (:type stage)
                  :filter (filter (:predicate stage) data)
                  :map (map (:transform stage) data)
                  :aggregate (aggregate (vec data) (:aggregations stage))
                  :group (aggregate-by-group (vec data)
                                             (:field stage)
                                             (:aggregations stage))
                  :sort (sort-by (:field stage) (:comparator stage identity) data)
                  :take (take (:n stage) data)
                  :drop (drop (:n stage) data)
                  data))
              items
              (:stages pipeline)))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-aggregate
  "Ring middleware to aggregate response data."
  [handler aggregations]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (or (sequential? body) (and (map? body) (or (:items body) (:data body))))
        (let [items (cond
                      (sequential? body) body
                      (map? body) (or (:items body) (:data body)))]
          (assoc response :body (aggregate (vec items) aggregations)))
        response))))

(defn wrap-aggregate-by-group
  "Ring middleware to aggregate by group."
  [handler group-field aggregations]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (or (sequential? body) (and (map? body) (or (:items body) (:data body))))
        (let [items (cond
                      (sequential? body) body
                      (map? body) (or (:items body) (:data body)))]
          (assoc response :body (aggregate-by-group (vec items) group-field aggregations)))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-window-ms!
  "Set the default window duration."
  [duration-ms]
  (swap! state assoc-in [:config :default-window-ms] duration-ms))

(defn set-max-window-items!
  "Set the maximum items per window."
  [max-items]
  (swap! state assoc-in [:config :max-window-items] max-items))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-aggregator-metrics
  "Get aggregator metrics."
  []
  (let [stats (:stats @state)]
    {:aggregations (:aggregations stats)
     :items-aggregated (:items-aggregated stats)
     :windows-created (:windows-created stats)
     :pipelines-run (:pipelines-run stats)
     :aggregators-count (+ (count built-in-aggregators)
                           (count (:aggregators @state)))
     :windows-count (count (:windows @state))
     :pipelines-count (count (:pipelines @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-aggregator-stats
  "Get aggregator statistics."
  []
  (merge (get-aggregator-metrics)
         {:default-window-ms (get-in @state [:config :default-window-ms])
          :max-window-items (get-in @state [:config :max-window-items])}))

(defn reset-stats!
  "Reset aggregator statistics."
  []
  (swap! state assoc :stats {:aggregations 0
                             :items-aggregated 0
                             :windows-created 0
                             :pipelines-run 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-aggregator!
  "Initialize the response aggregator."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response aggregator initialized")
    (events/emit! :response-aggregator-initialized {})
    true))
