(ns mental-models.pipeline.integration.response-composer
  "Response composer for mental model analysis system.
   
   Features:
   - Response composition
   - Multi-source composition
   - Composition strategies
   - Field mapping
   - Response assembly
   - Composition templates
   - Composition validation
   - Composition metrics"
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
  (atom {:templates {}        ;; template-id -> composition template
         :strategies {}       ;; strategy-id -> composition strategy
         :config {:default-strategy :merge
                  :null-handling :omit
                  :conflict-resolution :last-wins}
         :stats {:compositions 0
                 :sources-composed 0
                 :fields-mapped 0
                 :validations 0}
         :initialized? false}))

;; ============================================================================
;; Composition Strategies
;; ============================================================================

(def built-in-strategies
  {:merge (fn [responses]
            (reduce (fn [result response]
                      (merge result (:body response)))
                    {}
                    responses))
   
   :deep-merge (fn [responses]
                 (reduce (fn [result response]
                           (merge-with (fn [a b]
                                         (if (and (map? a) (map? b))
                                           (merge a b)
                                           b))
                                       result
                                       (:body response)))
                         {}
                         responses))
   
   :concat (fn [responses]
             {:items (vec (mapcat (fn [r]
                                    (let [body (:body r)]
                                      (cond
                                        (sequential? body) body
                                        (map? body) (or (:items body) (:data body) [body])
                                        :else [body])))
                                  responses))})
   
   :first-non-null (fn [responses]
                     (first (filter (fn [r]
                                      (some? (:body r)))
                                    responses)))
   
   :aggregate (fn [responses]
                {:sources (count responses)
                 :data (mapv :body responses)
                 :statuses (mapv :status responses)})
   
   :zip (fn [responses]
          (let [bodies (map :body responses)
                keys (distinct (mapcat keys bodies))]
            (into {}
                  (for [k keys]
                    [k (mapv #(get % k) bodies)]))))})

;; ============================================================================
;; Custom Strategies
;; ============================================================================

(defn register-strategy!
  "Register a custom composition strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :compose-fn (get config :compose-fn)
                  :enabled? (atom true)
                  :metrics {:invocations (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered composition strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get a composition strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id])
      (when-let [built-in (get built-in-strategies strategy-id)]
        {:id strategy-id :compose-fn built-in})))

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
;; Field Mapping
;; ============================================================================

(defn map-fields
  "Map fields from source to target structure."
  [data field-mapping]
  (swap! state update-in [:stats :fields-mapped] + (count field-mapping))
  
  (reduce (fn [result [target-path source-path]]
            (let [value (get-in data (if (sequential? source-path)
                                       source-path
                                       [source-path]))]
              (if (some? value)
                (assoc-in result (if (sequential? target-path)
                                   target-path
                                   [target-path])
                          value)
                result)))
          {}
          field-mapping))

(defn rename-fields
  "Rename fields in data."
  [data rename-map]
  (reduce (fn [result [old-key new-key]]
            (if (contains? result old-key)
              (-> result
                  (assoc new-key (get result old-key))
                  (dissoc old-key))
              result))
          data
          rename-map))

(defn select-fields
  "Select specific fields from data."
  [data fields]
  (select-keys data fields))

(defn exclude-fields
  "Exclude specific fields from data."
  [data fields]
  (apply dissoc data fields))

;; ============================================================================
;; Composition Templates
;; ============================================================================

(defn register-template!
  "Register a composition template."
  [template-id config]
  (let [template {:id template-id
                  :name (get config :name (name template-id))
                  :structure (get config :structure)
                  :sources (get config :sources [])
                  :mappings (get config :mappings {})
                  :defaults (get config :defaults {})
                  :enabled? (atom true)
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:templates template-id] template)
    template-id))

(defn get-template
  "Get a composition template."
  [template-id]
  (get-in @state [:templates template-id]))

(defn list-templates
  "List all templates."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :sources-count (count (:sources t))
           :enabled? @(:enabled? t)})
        (:templates @state)))

(defn apply-template
  "Apply a composition template."
  [template-id source-data]
  (when-let [template (get-template template-id)]
    (when @(:enabled? template)
      (let [structure (:structure template)
            mappings (:mappings template)
            defaults (:defaults template)]
        (walk/postwalk
         (fn [x]
           (if (and (keyword? x) (str/starts-with? (name x) "$"))
             (let [source-key (keyword (subs (name x) 1))]
               (or (get-in source-data [source-key])
                   (get mappings source-key)
                   (get defaults source-key)))
             x))
         structure)))))

;; ============================================================================
;; Response Composition
;; ============================================================================

(defn compose
  "Compose multiple responses."
  [responses & {:keys [strategy] :or {strategy :merge}}]
  (swap! state update-in [:stats :compositions] inc)
  (swap! state update-in [:stats :sources-composed] + (count responses))
  
  (let [compose-fn (or (:compose-fn (get-strategy strategy))
                       (get built-in-strategies strategy)
                       (get built-in-strategies :merge))]
    (compose-fn responses)))

(defn compose-with-mapping
  "Compose responses with field mapping."
  [responses mappings & {:keys [strategy] :or {strategy :merge}}]
  (let [mapped-responses (mapv (fn [[response mapping]]
                                 (if mapping
                                   (update response :body #(map-fields % mapping))
                                   response))
                               (map vector responses (concat mappings (repeat nil))))]
    (compose mapped-responses :strategy strategy)))

(defn compose-response
  "Compose a complete HTTP response."
  [responses & {:keys [strategy status headers]
                :or {strategy :merge
                     status 200
                     headers {"Content-Type" "application/json"}}}]
  {:status status
   :headers headers
   :body (compose responses :strategy strategy)})

;; ============================================================================
;; Assembly
;; ============================================================================

(defn assemble
  "Assemble a response from parts."
  [parts]
  (reduce (fn [result [key value]]
            (assoc-in result (if (sequential? key) key [key]) value))
          {}
          parts))

(defn assemble-from-sources
  "Assemble response from multiple sources."
  [sources handlers]
  (let [results (into {}
                      (for [[source-id handler] (map vector sources handlers)]
                        [source-id (handler)]))]
    results))

;; ============================================================================
;; Validation
;; ============================================================================

(defn validate-composition
  "Validate a composed response."
  [data schema]
  (swap! state update-in [:stats :validations] inc)
  
  (let [errors (atom [])]
    (doseq [[field spec] schema]
      (let [value (get data field)]
        (when (and (:required spec) (nil? value))
          (swap! errors conj {:field field :error :required}))
        
        (when (and value (:type spec))
          (let [type-valid? (case (:type spec)
                              :string (string? value)
                              :number (number? value)
                              :boolean (boolean? value)
                              :array (sequential? value)
                              :object (map? value)
                              true)]
            (when-not type-valid?
              (swap! errors conj {:field field :error :type-mismatch}))))))
    
    {:valid? (empty? @errors)
     :errors @errors}))

;; ============================================================================
;; Null Handling
;; ============================================================================

(defn remove-nulls
  "Remove null values from data."
  [data]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {} (filter (fn [[_ v]] (some? v)) x))
       x))
   data))

(defn replace-nulls
  "Replace null values with defaults."
  [data defaults]
  (walk/postwalk
   (fn [x]
     (if (and (map-entry? x) (nil? (val x)))
       [(key x) (get defaults (key x))]
       x))
   data))

;; ============================================================================
;; Conflict Resolution
;; ============================================================================

(defn resolve-conflicts
  "Resolve conflicts in composed data."
  [data-list & {:keys [strategy] :or {strategy :last-wins}}]
  (case strategy
    :last-wins (reduce merge data-list)
    :first-wins (reduce (fn [a b] (merge b a)) data-list)
    :array-merge (reduce (fn [a b]
                           (merge-with (fn [x y]
                                         (if (and (sequential? x) (sequential? y))
                                           (vec (concat x y))
                                           y))
                                       a b))
                         data-list)
    :deep-merge (reduce (fn [a b]
                          (merge-with (fn [x y]
                                        (if (and (map? x) (map? y))
                                          (merge x y)
                                          y))
                                      a b))
                        data-list)
    (reduce merge data-list)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-compose
  "Ring middleware to compose responses from multiple handlers."
  [handlers & {:keys [strategy] :or {strategy :merge}}]
  (fn [request]
    (let [responses (mapv #(% request) handlers)]
      (compose-response responses :strategy strategy))))

(defn wrap-compose-with-template
  "Ring middleware to compose using a template."
  [handlers template-id]
  (fn [request]
    (let [responses (into {}
                          (map-indexed (fn [i handler]
                                         [(keyword (str "source" i)) (:body (handler request))])
                                       handlers))
          composed (apply-template template-id responses)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body composed})))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set default composition strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-null-handling!
  "Set null handling mode."
  [mode]
  (swap! state assoc-in [:config :null-handling] mode))

(defn set-conflict-resolution!
  "Set conflict resolution strategy."
  [strategy]
  (swap! state assoc-in [:config :conflict-resolution] strategy))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-composer-metrics
  "Get composer metrics."
  []
  (let [stats (:stats @state)]
    {:compositions (:compositions stats)
     :sources-composed (:sources-composed stats)
     :fields-mapped (:fields-mapped stats)
     :validations (:validations stats)
     :strategies-count (+ (count built-in-strategies)
                          (count (:strategies @state)))
     :templates-count (count (:templates @state))
     :avg-sources (if (pos? (:compositions stats))
                    (/ (:sources-composed stats) (:compositions stats))
                    0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-composer-stats
  "Get composer statistics."
  []
  (merge (get-composer-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :null-handling (get-in @state [:config :null-handling])
          :conflict-resolution (get-in @state [:config :conflict-resolution])}))

(defn reset-stats!
  "Reset composer statistics."
  []
  (swap! state assoc :stats {:compositions 0
                             :sources-composed 0
                             :fields-mapped 0
                             :validations 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-composer!
  "Initialize the response composer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response composer initialized")
    (events/emit! :response-composer-initialized {})
    true))
