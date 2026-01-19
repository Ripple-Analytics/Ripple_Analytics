(ns mental-models.pipeline.integration.user-preferences
  "User preferences management for mental model analysis.
   
   Features:
   - Preference storage
   - Default values
   - Preference categories
   - Inheritance/cascading
   - Preference validation
   - Change history
   - Import/export
   - Preference sync"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:preferences {}      ;; user-id -> preferences
         :defaults {}         ;; category -> default-preferences
         :schemas {}          ;; preference-key -> schema
         :history {}          ;; user-id -> [changes]
         :sync-status {}      ;; user-id -> sync-status
         :stats {:preferences-set 0 :preferences-reset 0}
         :initialized? false}))

;; ============================================================================
;; Schema Management
;; ============================================================================

(defn define-preference!
  "Define a preference schema."
  [pref-key config]
  (let [schema {:key pref-key
                :name (get config :name (name pref-key))
                :description (get config :description "")
                :category (get config :category :general)
                :type (get config :type :string) ;; :string, :number, :boolean, :enum, :array, :object
                :default (get config :default nil)
                :options (get config :options nil) ;; For enum type
                :min (get config :min nil)
                :max (get config :max nil)
                :required? (get config :required? false)
                :sensitive? (get config :sensitive? false)
                :validator (get config :validator nil)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schemas pref-key] schema)
    (logging/log :info "Defined preference" {:pref-key pref-key})
    pref-key))

(defn get-schema
  "Get a preference schema."
  [pref-key]
  (get-in @state [:schemas pref-key]))

(defn list-schemas
  "List all preference schemas."
  [& {:keys [category]}]
  (let [schemas (vals (:schemas @state))
        filtered (if category
                   (filter #(= (:category %) category) schemas)
                   schemas)]
    (mapv #(select-keys % [:key :name :category :type :default]) filtered)))

;; ============================================================================
;; Validation
;; ============================================================================

(defn- validate-preference
  "Validate a preference value against its schema."
  [pref-key value]
  (if-let [schema (get-schema pref-key)]
    (let [type-valid? (case (:type schema)
                        :string (string? value)
                        :number (number? value)
                        :boolean (boolean? value)
                        :enum (contains? (set (:options schema)) value)
                        :array (vector? value)
                        :object (map? value)
                        true)
          range-valid? (if (and (number? value) (or (:min schema) (:max schema)))
                         (and (or (nil? (:min schema)) (>= value (:min schema)))
                              (or (nil? (:max schema)) (<= value (:max schema))))
                         true)
          custom-valid? (if-let [validator (:validator schema)]
                          (validator value)
                          true)]
      {:valid? (and type-valid? range-valid? custom-valid?)
       :errors (cond-> []
                 (not type-valid?) (conj (str "Invalid type, expected " (:type schema)))
                 (not range-valid?) (conj (str "Value out of range"))
                 (not custom-valid?) (conj "Custom validation failed"))})
    {:valid? true :errors []})) ;; No schema = accept any value

;; ============================================================================
;; Default Values
;; ============================================================================

(defn set-defaults!
  "Set default preferences for a category."
  [category defaults]
  (swap! state assoc-in [:defaults category] defaults)
  (logging/log :info "Set defaults" {:category category}))

(defn get-defaults
  "Get default preferences for a category."
  [category]
  (get-in @state [:defaults category] {}))

(defn get-all-defaults
  "Get all default preferences."
  []
  (reduce (fn [acc [category defaults]]
            (merge acc defaults))
          {}
          (:defaults @state)))

;; ============================================================================
;; Preference Operations
;; ============================================================================

(defn get-preference
  "Get a single preference value."
  [user-id pref-key]
  (let [user-prefs (get-in @state [:preferences user-id] {})
        schema (get-schema pref-key)
        default (or (:default schema)
                    (get (get-all-defaults) pref-key))]
    (get user-prefs pref-key default)))

(defn get-preferences
  "Get all preferences for a user."
  [user-id & {:keys [category include-defaults?] :or {include-defaults? true}}]
  (let [user-prefs (get-in @state [:preferences user-id] {})
        defaults (if include-defaults? (get-all-defaults) {})
        merged (merge defaults user-prefs)]
    (if category
      (let [category-keys (map :key (filter #(= (:category %) category) (vals (:schemas @state))))]
        (select-keys merged category-keys))
      merged)))

(defn set-preference!
  "Set a single preference value."
  [user-id pref-key value]
  (when (flags/enabled? :user-preferences)
    (let [validation (validate-preference pref-key value)]
      (if (:valid? validation)
        (do
          ;; Store preference
          (swap! state assoc-in [:preferences user-id pref-key] value)
          
          ;; Record history
          (swap! state update-in [:history user-id]
                 (fn [h]
                   (take-last 100
                              (conj (or h [])
                                    {:pref-key pref-key
                                     :old-value (get-in @state [:preferences user-id pref-key])
                                     :new-value value
                                     :changed-at (System/currentTimeMillis)}))))
          
          (swap! state update-in [:stats :preferences-set] inc)
          (logging/log :debug "Set preference" {:user-id user-id :pref-key pref-key})
          (events/emit! :preference-changed {:user-id user-id :pref-key pref-key})
          
          {:success true})
        {:success false :errors (:errors validation)}))))

(defn set-preferences!
  "Set multiple preferences at once."
  [user-id preferences]
  (let [results (map (fn [[k v]]
                       {:key k :result (set-preference! user-id k v)})
                     preferences)
        all-success? (every? #(get-in % [:result :success]) results)]
    {:success all-success?
     :results results}))

(defn reset-preference!
  "Reset a preference to its default value."
  [user-id pref-key]
  (swap! state update-in [:preferences user-id] dissoc pref-key)
  (swap! state update-in [:stats :preferences-reset] inc)
  (logging/log :debug "Reset preference" {:user-id user-id :pref-key pref-key})
  (events/emit! :preference-reset {:user-id user-id :pref-key pref-key}))

(defn reset-all-preferences!
  "Reset all preferences for a user."
  [user-id]
  (let [count (count (get-in @state [:preferences user-id] {}))]
    (swap! state update :preferences dissoc user-id)
    (swap! state update-in [:stats :preferences-reset] + count)
    (logging/log :info "Reset all preferences" {:user-id user-id :count count})
    count))

;; ============================================================================
;; History
;; ============================================================================

(defn get-preference-history
  "Get preference change history for a user."
  [user-id & {:keys [pref-key limit] :or {limit 50}}]
  (let [history (get-in @state [:history user-id] [])
        filtered (if pref-key
                   (filter #(= (:pref-key %) pref-key) history)
                   history)]
    (take-last limit filtered)))

(defn clear-history!
  "Clear preference history for a user."
  [user-id]
  (swap! state update :history dissoc user-id))

;; ============================================================================
;; Import/Export
;; ============================================================================

(defn export-preferences
  "Export preferences for a user."
  [user-id & {:keys [include-sensitive?] :or {include-sensitive? false}}]
  (let [prefs (get-preferences user-id)
        filtered (if include-sensitive?
                   prefs
                   (into {}
                         (filter (fn [[k _]]
                                   (let [schema (get-schema k)]
                                     (not (:sensitive? schema))))
                                 prefs)))]
    {:user-id user-id
     :preferences filtered
     :exported-at (System/currentTimeMillis)
     :version "1.0"}))

(defn import-preferences!
  "Import preferences for a user."
  [user-id data & {:keys [merge?] :or {merge? true}}]
  (let [prefs (:preferences data)]
    (if merge?
      (set-preferences! user-id prefs)
      (do
        (reset-all-preferences! user-id)
        (set-preferences! user-id prefs)))))

;; ============================================================================
;; Sync
;; ============================================================================

(defn get-sync-status
  "Get sync status for a user."
  [user-id]
  (get-in @state [:sync-status user-id]
          {:synced? false :last-sync nil}))

(defn mark-synced!
  "Mark preferences as synced."
  [user-id]
  (swap! state assoc-in [:sync-status user-id]
         {:synced? true
          :last-sync (System/currentTimeMillis)}))

(defn get-changes-since
  "Get preference changes since a timestamp."
  [user-id since]
  (let [history (get-in @state [:history user-id] [])]
    (filter #(> (:changed-at %) since) history)))

;; ============================================================================
;; Preference Categories
;; ============================================================================

(defn list-categories
  "List all preference categories."
  []
  (distinct (map :category (vals (:schemas @state)))))

(defn get-category-preferences
  "Get all preferences in a category."
  [user-id category]
  (get-preferences user-id :category category))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-preference-stats
  "Get preference statistics."
  []
  (let [stats (:stats @state)
        prefs-by-user (map (fn [[user-id prefs]]
                             {:user-id user-id :count (count prefs)})
                           (:preferences @state))]
    {:total-users (count (:preferences @state))
     :total-schemas (count (:schemas @state))
     :total-categories (count (list-categories))
     :preferences-set (:preferences-set stats)
     :preferences-reset (:preferences-reset stats)
     :avg-preferences-per-user (if (seq prefs-by-user)
                                 (/ (reduce + (map :count prefs-by-user))
                                    (count prefs-by-user))
                                 0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-user-preferences!
  "Initialize user preferences."
  []
  (when-not (:initialized? @state)
    ;; Define core preferences
    (define-preference! :theme
                        {:name "Theme"
                         :category :appearance
                         :type :enum
                         :options [:light :dark :system]
                         :default :system})
    
    (define-preference! :language
                        {:name "Language"
                         :category :appearance
                         :type :enum
                         :options [:en :es :fr :de :zh :ja]
                         :default :en})
    
    (define-preference! :notifications-enabled
                        {:name "Enable Notifications"
                         :category :notifications
                         :type :boolean
                         :default true})
    
    (define-preference! :email-notifications
                        {:name "Email Notifications"
                         :category :notifications
                         :type :boolean
                         :default true})
    
    (define-preference! :analysis-auto-run
                        {:name "Auto-run Analysis"
                         :category :analysis
                         :type :boolean
                         :default false})
    
    (define-preference! :default-model-confidence
                        {:name "Default Model Confidence Threshold"
                         :category :analysis
                         :type :number
                         :min 0.0
                         :max 1.0
                         :default 0.7})
    
    (define-preference! :lollapalooza-threshold
                        {:name "Lollapalooza Detection Threshold"
                         :category :analysis
                         :type :number
                         :min 2
                         :max 10
                         :default 3})
    
    (define-preference! :favorite-models
                        {:name "Favorite Mental Models"
                         :category :analysis
                         :type :array
                         :default []})
    
    (define-preference! :dashboard-layout
                        {:name "Dashboard Layout"
                         :category :appearance
                         :type :object
                         :default {:columns 3 :compact false}})
    
    ;; Set category defaults
    (set-defaults! :appearance
                   {:theme :system
                    :language :en
                    :dashboard-layout {:columns 3 :compact false}})
    
    (set-defaults! :notifications
                   {:notifications-enabled true
                    :email-notifications true})
    
    (set-defaults! :analysis
                   {:analysis-auto-run false
                    :default-model-confidence 0.7
                    :lollapalooza-threshold 3
                    :favorite-models []})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "User preferences initialized")
    (events/emit! :user-preferences-initialized {})
    true))
