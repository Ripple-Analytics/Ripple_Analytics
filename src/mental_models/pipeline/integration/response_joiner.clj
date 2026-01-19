(ns mental-models.pipeline.integration.response-joiner
  "Response joiner for mental model analysis system.
   
   Features:
   - Response joining
   - Multi-source joining
   - Join strategies
   - Key-based joining
   - Outer/inner joins
   - Join validation
   - Join history
   - Joining metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
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
         :history []          ;; join history
         :config {:default-strategy :inner
                  :null-value nil
                  :max-history 1000}
         :stats {:joins 0
                 :inner-joins 0
                 :outer-joins 0
                 :left-joins 0
                 :right-joins 0
                 :records-joined 0}
         :initialized? false}))

;; ============================================================================
;; Join Types
;; ============================================================================

(defn inner-join
  "Perform an inner join on two collections."
  [left right left-key right-key]
  (swap! state update-in [:stats :inner-joins] inc)
  
  (let [right-index (group-by right-key right)]
    (for [l left
          :let [key-val (left-key l)
                matches (get right-index key-val)]
          :when (seq matches)
          r matches]
      (merge l r))))

(defn left-join
  "Perform a left join on two collections."
  [left right left-key right-key]
  (swap! state update-in [:stats :left-joins] inc)
  
  (let [right-index (group-by right-key right)
        null-value (get-in @state [:config :null-value])]
    (for [l left
          :let [key-val (left-key l)
                matches (get right-index key-val)]]
      (if (seq matches)
        (map #(merge l %) matches)
        [(merge l null-value)]))))

(defn right-join
  "Perform a right join on two collections."
  [left right left-key right-key]
  (swap! state update-in [:stats :right-joins] inc)
  
  (let [left-index (group-by left-key left)
        null-value (get-in @state [:config :null-value])]
    (for [r right
          :let [key-val (right-key r)
                matches (get left-index key-val)]]
      (if (seq matches)
        (map #(merge % r) matches)
        [(merge null-value r)]))))

(defn full-outer-join
  "Perform a full outer join on two collections."
  [left right left-key right-key]
  (swap! state update-in [:stats :outer-joins] inc)
  
  (let [left-index (group-by left-key left)
        right-index (group-by right-key right)
        left-keys (set (keys left-index))
        right-keys (set (keys right-index))
        all-keys (set/union left-keys right-keys)
        null-value (get-in @state [:config :null-value])]
    (for [k all-keys
          :let [left-matches (get left-index k)
                right-matches (get right-index k)]]
      (cond
        (and (seq left-matches) (seq right-matches))
        (for [l left-matches r right-matches]
          (merge l r))
        
        (seq left-matches)
        (map #(merge % null-value) left-matches)
        
        :else
        (map #(merge null-value %) right-matches)))))

;; ============================================================================
;; Multi-Source Joining
;; ============================================================================

(defn join-multiple
  "Join multiple collections."
  [collections key-fns & {:keys [strategy] :or {strategy :inner}}]
  (swap! state update-in [:stats :joins] inc)
  
  (reduce (fn [result [coll key-fn]]
            (let [join-fn (case strategy
                           :inner inner-join
                           :left left-join
                           :right right-join
                           :outer full-outer-join
                           inner-join)]
              (vec (flatten (join-fn result coll :id key-fn)))))
          (first collections)
          (map vector (rest collections) (rest key-fns))))

(defn join-responses
  "Join multiple API responses."
  [responses key-fn & {:keys [strategy] :or {strategy :inner}}]
  (let [bodies (map :body responses)
        data-lists (map (fn [b]
                          (cond
                            (sequential? b) b
                            (map? b) (or (:items b) (:data b) [b])
                            :else [b]))
                        bodies)
        key-fns (repeat (count data-lists) key-fn)
        joined (join-multiple (vec data-lists) (vec key-fns) :strategy strategy)]
    
    (swap! state update-in [:stats :records-joined] + (count joined))
    
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body {:items (vec (flatten joined))
            :count (count (flatten joined))
            :sources (count responses)}}))

;; ============================================================================
;; Custom Join Strategies
;; ============================================================================

(defn register-strategy!
  "Register a custom join strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :join-fn (get config :join-fn)
                  :enabled? (atom true)
                  :metrics {:invocations (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered join strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get a join strategy."
  [strategy-id]
  (get-in @state [:strategies strategy-id]))

(defn apply-strategy
  "Apply a custom join strategy."
  [strategy-id left right & args]
  (when-let [strategy (get-strategy strategy-id)]
    (when @(:enabled? strategy)
      (swap! (get-in strategy [:metrics :invocations]) inc)
      (apply (:join-fn strategy) left right args))))

;; ============================================================================
;; Conditional Joining
;; ============================================================================

(defn join-where
  "Join with a condition function."
  [left right condition-fn]
  (for [l left
        r right
        :when (condition-fn l r)]
    (merge l r)))

(defn join-on-multiple-keys
  "Join on multiple keys."
  [left right left-keys right-keys]
  (let [left-key-fn (fn [item] (mapv #(get item %) left-keys))
        right-key-fn (fn [item] (mapv #(get item %) right-keys))]
    (inner-join left right left-key-fn right-key-fn)))

;; ============================================================================
;; Join Validation
;; ============================================================================

(defn validate-join
  "Validate a join result."
  [result & {:keys [min-count max-count required-fields]}]
  (let [errors (atom [])]
    
    (when (and min-count (< (count result) min-count))
      (swap! errors conj {:type :min-count
                          :expected min-count
                          :actual (count result)}))
    
    (when (and max-count (> (count result) max-count))
      (swap! errors conj {:type :max-count
                          :expected max-count
                          :actual (count result)}))
    
    (when required-fields
      (doseq [item result]
        (doseq [field required-fields]
          (when-not (contains? item field)
            (swap! errors conj {:type :missing-field
                                :field field
                                :item item})))))
    
    {:valid? (empty? @errors)
     :errors @errors
     :count (count result)}))

;; ============================================================================
;; Join History
;; ============================================================================

(defn- record-join!
  "Record a join operation."
  [join-type left-count right-count result-count]
  (let [max-history (get-in @state [:config :max-history])
        record {:id (str (UUID/randomUUID))
                :type join-type
                :left-count left-count
                :right-count right-count
                :result-count result-count
                :timestamp (System/currentTimeMillis)}]
    (swap! state update :history
           (fn [h]
             (let [new-history (conj h record)]
               (if (> (count new-history) max-history)
                 (vec (drop 1 new-history))
                 new-history))))))

(defn get-join-history
  "Get join history."
  [& {:keys [limit type] :or {limit 100}}]
  (cond->> (:history @state)
    type (filter #(= (:type %) type))
    true (take-last limit)
    true vec))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn cross-join
  "Perform a cross join (cartesian product)."
  [left right]
  (for [l left r right]
    (merge l r)))

(defn anti-join
  "Return items from left that don't match right."
  [left right left-key right-key]
  (let [right-keys (set (map right-key right))]
    (filter #(not (contains? right-keys (left-key %))) left)))

(defn semi-join
  "Return items from left that have a match in right."
  [left right left-key right-key]
  (let [right-keys (set (map right-key right))]
    (filter #(contains? right-keys (left-key %)) left)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-join-responses
  "Ring middleware to join responses from multiple handlers."
  [handlers key-fn & {:keys [strategy] :or {strategy :inner}}]
  (fn [request]
    (let [responses (mapv #(% request) handlers)]
      (join-responses responses key-fn :strategy strategy))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set the default join strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-null-value!
  "Set the null value for outer joins."
  [value]
  (swap! state assoc-in [:config :null-value] value))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-joiner-metrics
  "Get joiner metrics."
  []
  (let [stats (:stats @state)]
    {:joins (:joins stats)
     :inner-joins (:inner-joins stats)
     :outer-joins (:outer-joins stats)
     :left-joins (:left-joins stats)
     :right-joins (:right-joins stats)
     :records-joined (:records-joined stats)
     :strategies-count (count (:strategies @state))
     :history-size (count (:history @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-joiner-stats
  "Get joiner statistics."
  []
  (merge (get-joiner-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :null-value (get-in @state [:config :null-value])}))

(defn reset-stats!
  "Reset joiner statistics."
  []
  (swap! state assoc :stats {:joins 0
                             :inner-joins 0
                             :outer-joins 0
                             :left-joins 0
                             :right-joins 0
                             :records-joined 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-joiner!
  "Initialize the response joiner."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response joiner initialized")
    (events/emit! :response-joiner-initialized {})
    true))
