(ns mental-models.features.flags
  "Feature Flags Module for Mental Models Pipeline
   
   Provides feature flag management with:
   - Boolean and percentage-based flags
   - User targeting
   - A/B testing support
   - Flag overrides
   - Flag persistence"
  (:require
   [clojure.string :as str])
  (:import
   [java.util UUID]
   [java.time Instant]
   [java.util.concurrent ConcurrentHashMap]))

;; =============================================================================
;; FLAG DEFINITION
;; =============================================================================

(defrecord FeatureFlag [id name description enabled default-value rules created-at updated-at])

(defn create-flag [name description default-value]
  (->FeatureFlag (str (UUID/randomUUID))
                 name
                 description
                 true
                 default-value
                 []
                 (Instant/now)
                 (Instant/now)))

;; =============================================================================
;; FLAG STORAGE
;; =============================================================================

(def ^:private flags (ConcurrentHashMap.))
(def ^:private overrides (ConcurrentHashMap.))

(defn register-flag!
  "Register a new feature flag."
  [name description default-value]
  (let [flag (create-flag name description default-value)]
    (.put flags name flag)
    flag))

(defn unregister-flag!
  "Remove a feature flag."
  [name]
  (.remove flags name))

(defn get-flag [name]
  (.get flags name))

(defn get-all-flags []
  (vals flags))

;; =============================================================================
;; FLAG EVALUATION
;; =============================================================================

(defn evaluate-rule [rule context]
  (let [{:keys [type field operator value]} rule]
    (case type
      :user-id (let [user-id (:user-id context)]
                 (case operator
                   := (= user-id value)
                   :in (contains? (set value) user-id)
                   :not-in (not (contains? (set value) user-id))
                   false))
      :percentage (let [user-id (:user-id context "anonymous")
                        hash-val (Math/abs (hash user-id))]
                    (< (mod hash-val 100) value))
      :attribute (let [attr-val (get context field)]
                   (case operator
                     := (= attr-val value)
                     :!= (not= attr-val value)
                     :> (> attr-val value)
                     :< (< attr-val value)
                     :in (contains? (set value) attr-val)
                     false))
      :time (let [now (Instant/now)]
              (case operator
                :after (.isAfter now (Instant/parse value))
                :before (.isBefore now (Instant/parse value))
                false))
      false)))

(defn evaluate-rules [rules context]
  (if (empty? rules)
    nil
    (some #(when (evaluate-rule % context) (:value %)) rules)))

(defn is-enabled?
  "Check if a feature flag is enabled for a context."
  ([name] (is-enabled? name {}))
  ([name context]
   ;; Check overrides first
   (if-let [override (.get overrides name)]
     override
     ;; Then check flag rules
     (if-let [flag (get-flag name)]
       (if (:enabled flag)
         (if-let [rule-value (evaluate-rules (:rules flag) context)]
           rule-value
           (:default-value flag))
         false)
       false))))

(defn get-value
  "Get the value of a feature flag."
  ([name] (get-value name {} nil))
  ([name context] (get-value name context nil))
  ([name context default]
   (if-let [flag (get-flag name)]
     (if (:enabled flag)
       (or (evaluate-rules (:rules flag) context)
           (:default-value flag))
       default)
     default)))

;; =============================================================================
;; FLAG MANAGEMENT
;; =============================================================================

(defn enable-flag! [name]
  (when-let [flag (get-flag name)]
    (.put flags name (assoc flag :enabled true :updated-at (Instant/now)))
    true))

(defn disable-flag! [name]
  (when-let [flag (get-flag name)]
    (.put flags name (assoc flag :enabled false :updated-at (Instant/now)))
    true))

(defn set-default-value! [name value]
  (when-let [flag (get-flag name)]
    (.put flags name (assoc flag :default-value value :updated-at (Instant/now)))
    true))

(defn add-rule! [name rule]
  (when-let [flag (get-flag name)]
    (.put flags name (update flag :rules conj rule))
    true))

(defn clear-rules! [name]
  (when-let [flag (get-flag name)]
    (.put flags name (assoc flag :rules []))
    true))

;; =============================================================================
;; OVERRIDES
;; =============================================================================

(defn set-override!
  "Set a global override for a flag."
  [name value]
  (.put overrides name value))

(defn clear-override!
  "Clear a global override for a flag."
  [name]
  (.remove overrides name))

(defn clear-all-overrides!
  "Clear all global overrides."
  []
  (.clear overrides))

(defn get-overrides []
  (into {} overrides))

;; =============================================================================
;; A/B TESTING
;; =============================================================================

(defn create-ab-test
  "Create an A/B test with variants."
  [name description variants]
  (let [flag (create-flag name description nil)
        total-weight (reduce + (map :weight variants))
        rules (loop [remaining variants
                     cumulative 0
                     rules []]
                (if (empty? remaining)
                  rules
                  (let [{:keys [name weight value]} (first remaining)
                        percentage (int (* 100 (/ weight total-weight)))
                        new-cumulative (+ cumulative percentage)]
                    (recur (rest remaining)
                           new-cumulative
                           (conj rules {:type :percentage
                                        :value new-cumulative
                                        :result value})))))]
    (assoc flag :rules rules)))

(defn get-variant
  "Get the variant for a user in an A/B test."
  [name user-id]
  (get-value name {:user-id user-id}))

;; =============================================================================
;; BUILT-IN FLAGS
;; =============================================================================

(def default-flags
  [{:name "lollapalooza-alerts"
    :description "Enable Lollapalooza alerts when 3+ biases converge"
    :default-value true}
   {:name "streaming-analysis"
    :description "Enable streaming analysis responses"
    :default-value true}
   {:name "parallel-processing"
    :description "Enable parallel document processing"
    :default-value true}
   {:name "circuit-breaker"
    :description "Enable circuit breaker for LM Studio"
    :default-value true}
   {:name "metrics-collection"
    :description "Enable metrics collection"
    :default-value true}
   {:name "audit-logging"
    :description "Enable audit logging"
    :default-value true}
   {:name "cache-enabled"
    :description "Enable caching layer"
    :default-value true}
   {:name "rate-limiting"
    :description "Enable rate limiting"
    :default-value true}])

(defn init-default-flags! []
  (doseq [{:keys [name description default-value]} default-flags]
    (register-flag! name description default-value)))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  {:total-flags (.size flags)
   :enabled-flags (count (filter :enabled (get-all-flags)))
   :disabled-flags (count (filter (complement :enabled) (get-all-flags)))
   :overrides-count (.size overrides)})

;; =============================================================================
;; MACROS
;; =============================================================================

(defmacro when-feature
  "Execute body only when feature is enabled."
  [flag-name & body]
  `(when (is-enabled? ~flag-name)
     ~@body))

(defmacro if-feature
  "Execute then-branch when feature is enabled, else-branch otherwise."
  [flag-name then-branch else-branch]
  `(if (is-enabled? ~flag-name)
     ~then-branch
     ~else-branch))

;; =============================================================================
;; PERSISTENCE
;; =============================================================================

(defn export-flags []
  (map (fn [flag]
         {:name (:name flag)
          :description (:description flag)
          :enabled (:enabled flag)
          :default-value (:default-value flag)
          :rules (:rules flag)})
       (get-all-flags)))

(defn import-flags! [flag-data]
  (doseq [{:keys [name description default-value enabled rules]} flag-data]
    (let [flag (create-flag name description default-value)]
      (.put flags name (assoc flag :enabled enabled :rules (or rules [])))))
  (count flag-data))
