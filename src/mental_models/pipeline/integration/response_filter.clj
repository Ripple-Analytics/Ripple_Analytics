(ns mental-models.pipeline.integration.response-filter
  "Response filter for mental model analysis system.
   
   Features:
   - Field filtering
   - Sparse fieldsets
   - Field projection
   - Nested field filtering
   - Conditional filtering
   - Filter profiles
   - Filter rules
   - Filtering metrics"
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
  (atom {:profiles {}         ;; profile-id -> profile
         :rules {}            ;; rule-id -> rule
         :config {:default-fields nil
                  :always-include #{}
                  :always-exclude #{}
                  :max-depth 10
                  :fields-param "fields"
                  :exclude-param "exclude"}
         :stats {:filters-applied 0
                 :fields-removed 0
                 :fields-included 0
                 :nested-filters 0}
         :initialized? false}))

;; ============================================================================
;; Field Path Parsing
;; ============================================================================

(defn- parse-field-path
  "Parse a field path like 'user.name' into [:user :name]."
  [path]
  (if (keyword? path)
    [path]
    (mapv keyword (str/split (name path) #"\."))))

(defn- parse-fields
  "Parse a fields specification."
  [fields]
  (cond
    (nil? fields) nil
    (string? fields) (mapv parse-field-path (str/split fields #","))
    (sequential? fields) (mapv parse-field-path fields)
    :else nil))

;; ============================================================================
;; Field Selection
;; ============================================================================

(defn- get-nested
  "Get a nested value from a map."
  [m path]
  (get-in m path))

(defn- set-nested
  "Set a nested value in a map."
  [m path value]
  (assoc-in m path value))

(defn- select-fields
  "Select specific fields from a map."
  [m fields]
  (reduce (fn [result path]
            (let [value (get-nested m path)]
              (if (some? value)
                (set-nested result path value)
                result)))
          {}
          fields))

(defn- exclude-fields
  "Exclude specific fields from a map."
  [m fields]
  (reduce (fn [result path]
            (if (= (count path) 1)
              (dissoc result (first path))
              (update-in result (butlast path) dissoc (last path))))
          m
          fields))

;; ============================================================================
;; Recursive Filtering
;; ============================================================================

(defn- filter-map-recursive
  "Recursively filter a map based on field specifications."
  [m include-fields exclude-fields depth max-depth]
  (when (and (map? m) (< depth max-depth))
    (let [filtered (cond
                     (seq include-fields)
                     (select-fields m include-fields)
                     
                     (seq exclude-fields)
                     (exclude-fields m exclude-fields)
                     
                     :else m)]
      (walk/postwalk
       (fn [x]
         (if (map? x)
           (into {} (for [[k v] x]
                      [k (if (map? v)
                           (filter-map-recursive v nil nil (inc depth) max-depth)
                           v)]))
           x))
       filtered))))

;; ============================================================================
;; Filter Profiles
;; ============================================================================

(defn register-profile!
  "Register a filter profile."
  [profile-id config]
  (let [profile {:id profile-id
                 :name (get config :name (name profile-id))
                 :include-fields (parse-fields (get config :include-fields))
                 :exclude-fields (parse-fields (get config :exclude-fields))
                 :condition-fn (get config :condition-fn (constantly true))
                 :enabled? (atom true)
                 :metrics {:applications (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:profiles profile-id] profile)
    (logging/log :info "Registered filter profile" {:profile-id profile-id})
    profile-id))

(defn get-profile
  "Get a filter profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn list-profiles
  "List all filter profiles."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :include-fields-count (count (:include-fields p))
           :exclude-fields-count (count (:exclude-fields p))
           :enabled? @(:enabled? p)})
        (:profiles @state)))

(defn delete-profile!
  "Delete a filter profile."
  [profile-id]
  (swap! state update :profiles dissoc profile-id))

;; ============================================================================
;; Filter Rules
;; ============================================================================

(defn register-rule!
  "Register a filter rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :condition-fn (get config :condition-fn (constantly true))
              :filter-fn (get config :filter-fn identity)
              :priority (get config :priority 100)
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:rules rule-id] rule)
    rule-id))

(defn get-rule
  "Get a filter rule."
  [rule-id]
  (get-in @state [:rules rule-id]))

(defn apply-rules
  "Apply all matching filter rules to data."
  [data request]
  (let [rules (->> (vals (:rules @state))
                   (filter #@(:enabled? %))
                   (filter #((:condition-fn %) request))
                   (sort-by :priority))]
    (reduce (fn [d rule]
              ((:filter-fn rule) d))
            data
            rules)))

;; ============================================================================
;; Request-Based Filtering
;; ============================================================================

(defn- get-fields-from-request
  "Get field specifications from request parameters."
  [request]
  (let [fields-param (get-in @state [:config :fields-param])
        exclude-param (get-in @state [:config :exclude-param])
        params (:params request)]
    {:include-fields (parse-fields (get params (keyword fields-param)))
     :exclude-fields (parse-fields (get params (keyword exclude-param)))}))

(defn filter-by-request
  "Filter data based on request parameters."
  [data request]
  (let [{:keys [include-fields exclude-fields]} (get-fields-from-request request)
        always-include (get-in @state [:config :always-include])
        always-exclude (get-in @state [:config :always-exclude])
        max-depth (get-in @state [:config :max-depth])
        
        ;; Merge always-include with requested includes
        final-includes (if (seq include-fields)
                         (concat include-fields (map vector always-include))
                         nil)
        
        ;; Merge always-exclude with requested excludes
        final-excludes (concat (or exclude-fields [])
                               (map vector always-exclude))]
    
    (swap! state update-in [:stats :filters-applied] inc)
    
    (if (or (seq final-includes) (seq final-excludes))
      (do
        (swap! state update-in [:stats :nested-filters] inc)
        (filter-map-recursive data final-includes final-excludes 0 max-depth))
      data)))

;; ============================================================================
;; Profile-Based Filtering
;; ============================================================================

(defn filter-by-profile
  "Filter data using a profile."
  [data profile-id request]
  (if-let [profile (get-profile profile-id)]
    (when (and @(:enabled? profile)
               ((:condition-fn profile) request))
      (swap! (get-in profile [:metrics :applications]) inc)
      (let [max-depth (get-in @state [:config :max-depth])]
        (filter-map-recursive data
                              (:include-fields profile)
                              (:exclude-fields profile)
                              0
                              max-depth)))
    data))

;; ============================================================================
;; Sparse Fieldsets (JSON:API style)
;; ============================================================================

(defn- parse-sparse-fieldsets
  "Parse sparse fieldsets from request (JSON:API style)."
  [request]
  (let [params (:params request)]
    (into {}
          (for [[k v] params
                :let [key-str (name k)]
                :when (str/starts-with? key-str "fields[")]
            (let [type (subs key-str 7 (dec (count key-str)))
                  fields (str/split v #",")]
              [(keyword type) (mapv keyword fields)])))))

(defn filter-sparse-fieldsets
  "Filter data using sparse fieldsets."
  [data request]
  (let [fieldsets (parse-sparse-fieldsets request)]
    (if (seq fieldsets)
      (walk/postwalk
       (fn [x]
         (if (and (map? x) (:type x))
           (let [type-key (keyword (:type x))
                 allowed-fields (get fieldsets type-key)]
             (if allowed-fields
               (select-keys x (conj allowed-fields :id :type))
               x))
           x))
       data)
      data)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-filter-response
  "Ring middleware to filter responses."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (filter-by-request body request))
        response))))

(defn wrap-filter-with-profile
  "Ring middleware to filter with a profile."
  [handler profile-id]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (filter-by-profile body profile-id request))
        response))))

(defn wrap-sparse-fieldsets
  "Ring middleware for sparse fieldsets."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (filter-sparse-fieldsets body request))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-fields!
  "Set default fields to include."
  [fields]
  (swap! state assoc-in [:config :default-fields] (parse-fields fields)))

(defn set-always-include!
  "Set fields to always include."
  [fields]
  (swap! state assoc-in [:config :always-include] (set fields)))

(defn set-always-exclude!
  "Set fields to always exclude."
  [fields]
  (swap! state assoc-in [:config :always-exclude] (set fields)))

(defn set-max-depth!
  "Set maximum nesting depth for filtering."
  [depth]
  (swap! state assoc-in [:config :max-depth] depth))

(defn set-fields-param!
  "Set the query parameter name for fields."
  [param]
  (swap! state assoc-in [:config :fields-param] param))

(defn set-exclude-param!
  "Set the query parameter name for exclude."
  [param]
  (swap! state assoc-in [:config :exclude-param] param))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-filter-metrics
  "Get filter metrics."
  []
  (let [stats (:stats @state)]
    {:filters-applied (:filters-applied stats)
     :fields-removed (:fields-removed stats)
     :fields-included (:fields-included stats)
     :nested-filters (:nested-filters stats)
     :profiles-count (count (:profiles @state))
     :rules-count (count (:rules @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-filter-stats
  "Get filter statistics."
  []
  (merge (get-filter-metrics)
         {:max-depth (get-in @state [:config :max-depth])
          :fields-param (get-in @state [:config :fields-param])
          :exclude-param (get-in @state [:config :exclude-param])
          :always-include (get-in @state [:config :always-include])
          :always-exclude (get-in @state [:config :always-exclude])}))

(defn reset-stats!
  "Reset filter statistics."
  []
  (swap! state assoc :stats {:filters-applied 0
                             :fields-removed 0
                             :fields-included 0
                             :nested-filters 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-filter!
  "Initialize the response filter."
  []
  (when-not (:initialized? @state)
    ;; Register default profiles
    (register-profile! :minimal
                       {:name "Minimal Response"
                        :include-fields [:id :name :type]})
    
    (register-profile! :no-metadata
                       {:name "No Metadata"
                        :exclude-fields [:_metadata :_links :_embedded]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response filter initialized")
    (events/emit! :response-filter-initialized {})
    true))
