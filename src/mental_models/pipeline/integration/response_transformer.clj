(ns mental-models.pipeline.integration.response-transformer
  "Response transformer for mental model analysis system.
   
   Features:
   - Response transformation
   - Format conversion
   - Field mapping
   - Data enrichment
   - Response filtering
   - Response aggregation
   - Transformer chains
   - Transformer metrics"
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
           [java.util Base64]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:transformers {}     ;; transformer-id -> transformer
         :chains {}           ;; chain-id -> [transformer-ids]
         :config {:max-chain-length 10}
         :stats {:transformations 0
                 :transform-errors 0
                 :bytes-processed 0}
         :initialized? false}))

;; ============================================================================
;; Transformer Creation
;; ============================================================================

(defn register-transformer!
  "Register a response transformer."
  [transformer-id config]
  (let [transformer {:id transformer-id
                     :name (get config :name (name transformer-id))
                     :type (get config :type :map)  ;; :map, :filter, :aggregate, :enrich
                     :transform-fn (get config :transform-fn)
                     :condition-fn (get config :condition-fn (constantly true))
                     :enabled? (atom true)
                     :metrics {:invocations (atom 0)
                               :errors (atom 0)}
                     :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:transformers transformer-id] transformer)
    (logging/log :info "Registered transformer" {:transformer-id transformer-id :type (:type transformer)})
    (events/emit! :transformer-registered {:transformer-id transformer-id})
    transformer-id))

(defn get-transformer
  "Get a transformer."
  [transformer-id]
  (get-in @state [:transformers transformer-id]))

(defn list-transformers
  "List all transformers."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :type (:type t)
           :enabled? @(:enabled? t)})
        (:transformers @state)))

(defn unregister-transformer!
  "Unregister a transformer."
  [transformer-id]
  (swap! state update :transformers dissoc transformer-id)
  (logging/log :info "Unregistered transformer" {:transformer-id transformer-id}))

;; ============================================================================
;; Transformer Chains
;; ============================================================================

(defn create-chain!
  "Create a transformer chain."
  [chain-id transformer-ids]
  (let [max-length (get-in @state [:config :max-chain-length])]
    (when (<= (count transformer-ids) max-length)
      (swap! state assoc-in [:chains chain-id] (vec transformer-ids))
      (logging/log :info "Created transformer chain" {:chain-id chain-id})
      chain-id)))

(defn get-chain
  "Get a transformer chain."
  [chain-id]
  (get-in @state [:chains chain-id]))

(defn delete-chain!
  "Delete a transformer chain."
  [chain-id]
  (swap! state update :chains dissoc chain-id))

;; ============================================================================
;; Transformation Execution
;; ============================================================================

(defn- execute-transformer
  "Execute a single transformer."
  [transformer response context]
  (when (and @(:enabled? transformer)
             ((:condition-fn transformer) response context))
    (try
      (swap! (get-in transformer [:metrics :invocations]) inc)
      ((:transform-fn transformer) response context)
      (catch Exception e
        (swap! (get-in transformer [:metrics :errors]) inc)
        (swap! state update-in [:stats :transform-errors] inc)
        (logging/log :error "Transform error" {:transformer-id (:id transformer)
                                                :error (.getMessage e)})
        response))))

(defn transform
  "Transform a response using a transformer."
  [transformer-id response & {:keys [context] :or {context {}}}]
  (if-let [transformer (get-transformer transformer-id)]
    (do
      (swap! state update-in [:stats :transformations] inc)
      (or (execute-transformer transformer response context) response))
    response))

(defn transform-chain
  "Transform a response using a chain of transformers."
  [chain-id response & {:keys [context] :or {context {}}}]
  (if-let [transformer-ids (get-chain chain-id)]
    (reduce (fn [resp tid]
              (transform tid resp :context context))
            response
            transformer-ids)
    response))

;; ============================================================================
;; Built-in Transformers
;; ============================================================================

;; Field Mapping
(defn field-mapper
  "Create a field mapping transformer."
  [field-map]
  (fn [response _context]
    (reduce-kv (fn [m old-key new-key]
                 (if (contains? m old-key)
                   (-> m
                       (assoc new-key (get m old-key))
                       (dissoc old-key))
                   m))
               response
               field-map)))

;; Field Selection
(defn field-selector
  "Create a field selection transformer."
  [fields]
  (fn [response _context]
    (select-keys response fields)))

;; Field Removal
(defn field-remover
  "Create a field removal transformer."
  [fields]
  (fn [response _context]
    (apply dissoc response fields)))

;; Value Transformation
(defn value-transformer
  "Create a value transformation transformer."
  [field transform-fn]
  (fn [response _context]
    (if (contains? response field)
      (update response field transform-fn)
      response)))

;; Nested Field Extraction
(defn nested-extractor
  "Create a nested field extraction transformer."
  [path]
  (fn [response _context]
    (get-in response path)))

;; Data Enrichment
(defn enricher
  "Create a data enrichment transformer."
  [enrich-fn]
  (fn [response context]
    (merge response (enrich-fn response context))))

;; Response Wrapping
(defn wrapper
  "Create a response wrapping transformer."
  [wrapper-key]
  (fn [response _context]
    {wrapper-key response}))

;; Response Unwrapping
(defn unwrapper
  "Create a response unwrapping transformer."
  [wrapper-key]
  (fn [response _context]
    (get response wrapper-key response)))

;; Collection Mapping
(defn collection-mapper
  "Create a collection mapping transformer."
  [item-transform-fn]
  (fn [response _context]
    (if (sequential? response)
      (mapv item-transform-fn response)
      response)))

;; Collection Filtering
(defn collection-filter
  "Create a collection filtering transformer."
  [predicate-fn]
  (fn [response _context]
    (if (sequential? response)
      (filterv predicate-fn response)
      response)))

;; Pagination Wrapper
(defn pagination-wrapper
  "Create a pagination wrapper transformer."
  [page-size]
  (fn [response context]
    (let [page (get context :page 1)
          items (if (sequential? response) response [response])
          total (count items)
          start (* (dec page) page-size)
          end (min (+ start page-size) total)]
      {:items (subvec (vec items) start end)
       :page page
       :page-size page-size
       :total total
       :total-pages (int (Math/ceil (/ total page-size)))})))

;; Format Conversion
(defn- to-camel-case
  "Convert keyword to camelCase."
  [k]
  (let [s (name k)
        parts (str/split s #"-")]
    (keyword (str (first parts)
                  (str/join (map str/capitalize (rest parts)))))))

(defn camel-case-keys
  "Transform all keys to camelCase."
  [response _context]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (into {} (map (fn [[k v]] [(to-camel-case k) v]) x))
                     x))
                 response))

(defn- to-snake-case
  "Convert keyword to snake_case."
  [k]
  (keyword (str/replace (name k) #"-" "_")))

(defn snake-case-keys
  "Transform all keys to snake_case."
  [response _context]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (into {} (map (fn [[k v]] [(to-snake-case k) v]) x))
                     x))
                 response))

;; ============================================================================
;; Transformer Control
;; ============================================================================

(defn enable-transformer!
  "Enable a transformer."
  [transformer-id]
  (when-let [transformer (get-transformer transformer-id)]
    (reset! (:enabled? transformer) true)
    (logging/log :info "Enabled transformer" {:transformer-id transformer-id})))

(defn disable-transformer!
  "Disable a transformer."
  [transformer-id]
  (when-let [transformer (get-transformer transformer-id)]
    (reset! (:enabled? transformer) false)
    (logging/log :info "Disabled transformer" {:transformer-id transformer-id})))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-transformer-metrics
  "Get metrics for a transformer."
  [transformer-id]
  (when-let [transformer (get-transformer transformer-id)]
    {:transformer-id transformer-id
     :name (:name transformer)
     :type (:type transformer)
     :invocations @(get-in transformer [:metrics :invocations])
     :errors @(get-in transformer [:metrics :errors])
     :enabled? @(:enabled? transformer)}))

(defn get-all-transformer-metrics
  "Get metrics for all transformers."
  []
  (mapv (fn [[id _]] (get-transformer-metrics id)) (:transformers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-transformer-stats
  "Get transformer statistics."
  []
  (let [stats (:stats @state)]
    {:transformers-count (count (:transformers @state))
     :chains-count (count (:chains @state))
     :transformations (:transformations stats)
     :transform-errors (:transform-errors stats)
     :bytes-processed (:bytes-processed stats)}))

(defn reset-stats!
  "Reset transformer statistics."
  []
  (swap! state assoc :stats {:transformations 0
                             :transform-errors 0
                             :bytes-processed 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-transformer!
  "Initialize the response transformer system."
  []
  (when-not (:initialized? @state)
    ;; Register built-in transformers
    (register-transformer! :camel-case
                           {:name "Camel Case Keys"
                            :type :map
                            :transform-fn camel-case-keys})
    
    (register-transformer! :snake-case
                           {:name "Snake Case Keys"
                            :type :map
                            :transform-fn snake-case-keys})
    
    (register-transformer! :wrap-data
                           {:name "Wrap in Data"
                            :type :map
                            :transform-fn (wrapper :data)})
    
    (register-transformer! :unwrap-data
                           {:name "Unwrap Data"
                            :type :map
                            :transform-fn (unwrapper :data)})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response transformer initialized")
    (events/emit! :response-transformer-initialized {})
    true))
