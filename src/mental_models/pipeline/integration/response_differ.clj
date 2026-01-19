(ns mental-models.pipeline.integration.response-differ
  "Response differ for mental model analysis system.
   
   Features:
   - Response comparison
   - Diff generation
   - Change detection
   - Semantic diffing
   - Diff formatting
   - Diff storage
   - Diff analysis
   - Diffing metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data :as data]
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
  (atom {:snapshots {}        ;; snapshot-id -> snapshot
         :diffs []            ;; stored diffs
         :config {:max-snapshots 1000
                  :max-diffs 10000
                  :ignore-fields #{:timestamp :updated-at :_metadata}
                  :semantic-diff? true}
         :stats {:comparisons 0
                 :diffs-generated 0
                 :changes-detected 0
                 :snapshots-stored 0}
         :initialized? false}))

;; ============================================================================
;; Snapshot Management
;; ============================================================================

(defn store-snapshot!
  "Store a response snapshot."
  [snapshot-id data & {:keys [metadata]}]
  (let [snapshot {:id snapshot-id
                  :data data
                  :metadata (or metadata {})
                  :hash (hash data)
                  :created-at (System/currentTimeMillis)}
        max-snapshots (get-in @state [:config :max-snapshots])]
    
    (swap! state update :snapshots
           (fn [s]
             (let [new-snapshots (assoc s snapshot-id snapshot)]
               (if (> (count new-snapshots) max-snapshots)
                 (into {} (take-last max-snapshots (sort-by (comp :created-at val) new-snapshots)))
                 new-snapshots))))
    (swap! state update-in [:stats :snapshots-stored] inc)
    snapshot-id))

(defn get-snapshot
  "Get a stored snapshot."
  [snapshot-id]
  (get-in @state [:snapshots snapshot-id]))

(defn list-snapshots
  "List all snapshots."
  [& {:keys [limit] :or {limit 100}}]
  (->> (vals (:snapshots @state))
       (sort-by :created-at)
       (take-last limit)
       (mapv (fn [s]
               {:id (:id s)
                :hash (:hash s)
                :created-at (:created-at s)
                :metadata (:metadata s)}))))

(defn delete-snapshot!
  "Delete a snapshot."
  [snapshot-id]
  (swap! state update :snapshots dissoc snapshot-id))

;; ============================================================================
;; Basic Diffing
;; ============================================================================

(defn- filter-ignored-fields
  "Remove ignored fields from data."
  [data]
  (let [ignore-fields (get-in @state [:config :ignore-fields])]
    (walk/postwalk
     (fn [x]
       (if (map? x)
         (apply dissoc x ignore-fields)
         x))
     data)))

(defn diff
  "Generate a diff between two values."
  [old-value new-value]
  (swap! state update-in [:stats :comparisons] inc)
  
  (let [filtered-old (filter-ignored-fields old-value)
        filtered-new (filter-ignored-fields new-value)
        [only-old only-new both] (data/diff filtered-old filtered-new)]
    
    (when (or only-old only-new)
      (swap! state update-in [:stats :diffs-generated] inc))
    
    {:old-only only-old
     :new-only only-new
     :unchanged both
     :changed? (or (some? only-old) (some? only-new))}))

(defn diff-snapshots
  "Generate a diff between two snapshots."
  [old-snapshot-id new-snapshot-id]
  (let [old-snapshot (get-snapshot old-snapshot-id)
        new-snapshot (get-snapshot new-snapshot-id)]
    (when (and old-snapshot new-snapshot)
      (let [result (diff (:data old-snapshot) (:data new-snapshot))]
        (assoc result
               :old-snapshot-id old-snapshot-id
               :new-snapshot-id new-snapshot-id
               :old-created-at (:created-at old-snapshot)
               :new-created-at (:created-at new-snapshot))))))

;; ============================================================================
;; Semantic Diffing
;; ============================================================================

(defn- classify-change
  "Classify a change type."
  [old-value new-value]
  (cond
    (and (nil? old-value) (some? new-value)) :added
    (and (some? old-value) (nil? new-value)) :removed
    (not= (type old-value) (type new-value)) :type-changed
    (and (number? old-value) (number? new-value))
    (cond
      (> new-value old-value) :increased
      (< new-value old-value) :decreased
      :else :unchanged)
    :else :modified))

(defn- collect-changes
  "Collect all changes between two maps."
  [old-map new-map path]
  (let [all-keys (set (concat (keys old-map) (keys new-map)))]
    (mapcat (fn [k]
              (let [old-val (get old-map k)
                    new-val (get new-map k)
                    current-path (conj path k)]
                (cond
                  (= old-val new-val) []
                  (and (map? old-val) (map? new-val))
                  (collect-changes old-val new-val current-path)
                  :else [{:path current-path
                          :old-value old-val
                          :new-value new-val
                          :change-type (classify-change old-val new-val)}])))
            all-keys)))

(defn semantic-diff
  "Generate a semantic diff with change classification."
  [old-value new-value]
  (swap! state update-in [:stats :comparisons] inc)
  
  (let [filtered-old (filter-ignored-fields old-value)
        filtered-new (filter-ignored-fields new-value)
        changes (if (and (map? filtered-old) (map? filtered-new))
                  (collect-changes filtered-old filtered-new [])
                  (when (not= filtered-old filtered-new)
                    [{:path []
                      :old-value filtered-old
                      :new-value filtered-new
                      :change-type (classify-change filtered-old filtered-new)}]))]
    
    (when (seq changes)
      (swap! state update-in [:stats :changes-detected] + (count changes)))
    
    {:changes (vec changes)
     :change-count (count changes)
     :by-type (frequencies (map :change-type changes))
     :changed? (boolean (seq changes))}))

;; ============================================================================
;; Diff Formatting
;; ============================================================================

(defn format-diff-text
  "Format a diff as text."
  [diff-result]
  (let [changes (:changes diff-result)]
    (str/join "\n"
              (for [{:keys [path old-value new-value change-type]} changes]
                (let [path-str (str/join "." (map name path))]
                  (case change-type
                    :added (str "+ " path-str ": " new-value)
                    :removed (str "- " path-str ": " old-value)
                    :modified (str "~ " path-str ": " old-value " -> " new-value)
                    :increased (str "^ " path-str ": " old-value " -> " new-value)
                    :decreased (str "v " path-str ": " old-value " -> " new-value)
                    :type-changed (str "! " path-str ": " (type old-value) " -> " (type new-value))
                    (str "? " path-str)))))))

(defn format-diff-json
  "Format a diff as JSON-compatible structure."
  [diff-result]
  {:changes (mapv (fn [{:keys [path old-value new-value change-type]}]
                    {:path (mapv name path)
                     :pathString (str/join "." (map name path))
                     :oldValue old-value
                     :newValue new-value
                     :changeType (name change-type)})
                  (:changes diff-result))
   :summary {:total (:change-count diff-result)
             :byType (into {} (map (fn [[k v]] [(name k) v]) (:by-type diff-result)))}})

(defn format-diff-html
  "Format a diff as HTML."
  [diff-result]
  (let [changes (:changes diff-result)]
    (str "<div class=\"diff\">\n"
         (str/join "\n"
                   (for [{:keys [path old-value new-value change-type]} changes]
                     (let [path-str (str/join "." (map name path))
                           class (name change-type)]
                       (str "<div class=\"change " class "\">"
                            "<span class=\"path\">" path-str "</span>: "
                            (when old-value
                              (str "<span class=\"old\">" old-value "</span>"))
                            (when (and old-value new-value) " → ")
                            (when new-value
                              (str "<span class=\"new\">" new-value "</span>"))
                            "</div>"))))
         "\n</div>")))

;; ============================================================================
;; Diff Storage
;; ============================================================================

(defn store-diff!
  "Store a diff result."
  [diff-result & {:keys [metadata]}]
  (let [diff-id (str (UUID/randomUUID))
        stored-diff {:id diff-id
                     :diff diff-result
                     :metadata (or metadata {})
                     :created-at (System/currentTimeMillis)}
        max-diffs (get-in @state [:config :max-diffs])]
    
    (swap! state update :diffs
           (fn [d]
             (let [new-diffs (conj d stored-diff)]
               (if (> (count new-diffs) max-diffs)
                 (vec (drop 1 new-diffs))
                 new-diffs))))
    diff-id))

(defn get-diff
  "Get a stored diff."
  [diff-id]
  (first (filter #(= (:id %) diff-id) (:diffs @state))))

(defn list-diffs
  "List stored diffs."
  [& {:keys [limit since] :or {limit 100}}]
  (cond->> (:diffs @state)
    since (filter #(> (:created-at %) since))
    true (take-last limit)
    true vec))

;; ============================================================================
;; Change Detection
;; ============================================================================

(defn has-changes?
  "Check if there are any changes between two values."
  [old-value new-value]
  (let [filtered-old (filter-ignored-fields old-value)
        filtered-new (filter-ignored-fields new-value)]
    (not= filtered-old filtered-new)))

(defn get-changed-fields
  "Get a list of changed field paths."
  [old-value new-value]
  (let [diff-result (semantic-diff old-value new-value)]
    (mapv :path (:changes diff-result))))

(defn get-change-summary
  "Get a summary of changes."
  [old-value new-value]
  (let [diff-result (semantic-diff old-value new-value)]
    {:changed? (:changed? diff-result)
     :change-count (:change-count diff-result)
     :added (count (filter #(= (:change-type %) :added) (:changes diff-result)))
     :removed (count (filter #(= (:change-type %) :removed) (:changes diff-result)))
     :modified (count (filter #(#{:modified :increased :decreased :type-changed}
                                 (:change-type %))
                              (:changes diff-result)))}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-diff-response
  "Ring middleware to diff responses against previous versions."
  [handler snapshot-key-fn]
  (fn [request]
    (let [response (handler request)
          body (:body response)
          snapshot-key (snapshot-key-fn request)]
      (when (and (map? body) snapshot-key)
        (let [previous (get-snapshot snapshot-key)]
          (when previous
            (let [diff-result (semantic-diff (:data previous) body)]
              (when (:changed? diff-result)
                (events/emit! :response-changed
                              {:key snapshot-key
                               :changes (:change-count diff-result)}))))
          (store-snapshot! snapshot-key body)))
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-ignore-fields!
  "Set fields to ignore in diffs."
  [fields]
  (swap! state assoc-in [:config :ignore-fields] (set fields)))

(defn add-ignore-field!
  "Add a field to ignore in diffs."
  [field]
  (swap! state update-in [:config :ignore-fields] conj field))

(defn set-semantic-diff!
  "Enable/disable semantic diffing."
  [enabled?]
  (swap! state assoc-in [:config :semantic-diff?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-differ-metrics
  "Get differ metrics."
  []
  (let [stats (:stats @state)]
    {:comparisons (:comparisons stats)
     :diffs-generated (:diffs-generated stats)
     :changes-detected (:changes-detected stats)
     :snapshots-stored (:snapshots-stored stats)
     :snapshots-count (count (:snapshots @state))
     :diffs-count (count (:diffs @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-differ-stats
  "Get differ statistics."
  []
  (merge (get-differ-metrics)
         {:ignore-fields (get-in @state [:config :ignore-fields])
          :semantic-diff? (get-in @state [:config :semantic-diff?])
          :max-snapshots (get-in @state [:config :max-snapshots])
          :max-diffs (get-in @state [:config :max-diffs])}))

(defn reset-stats!
  "Reset differ statistics."
  []
  (swap! state assoc :stats {:comparisons 0
                             :diffs-generated 0
                             :changes-detected 0
                             :snapshots-stored 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-differ!
  "Initialize the response differ."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response differ initialized")
    (events/emit! :response-differ-initialized {})
    true))
