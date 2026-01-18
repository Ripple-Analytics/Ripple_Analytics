(ns mental-models.plugins.core
  "Plugin System for Mental Models Pipeline
   
   Provides extensibility with:
   - Plugin registration
   - Plugin lifecycle management
   - Hook system
   - Plugin dependencies
   - Plugin configuration"
  (:require
   [clojure.string :as str])
  (:import
   [java.util UUID]
   [java.time Instant]
   [java.util.concurrent ConcurrentHashMap]))

;; =============================================================================
;; PLUGIN DEFINITION
;; =============================================================================

(defrecord Plugin [id name version description author hooks config enabled? loaded-at])

(defn create-plugin [name version description author hooks config]
  (->Plugin (str (UUID/randomUUID))
            name
            version
            description
            author
            hooks
            config
            true
            (Instant/now)))

;; =============================================================================
;; PLUGIN REGISTRY
;; =============================================================================

(def ^:private plugins (ConcurrentHashMap.))
(def ^:private hooks (atom {}))
(def ^:private plugin-order (atom []))

(defn register-plugin!
  "Register a plugin with the system."
  [plugin]
  (.put plugins (:id plugin) plugin)
  (swap! plugin-order conj (:id plugin))
  (doseq [[hook-name hook-fn] (:hooks plugin)]
    (swap! hooks update hook-name (fnil conj []) {:plugin-id (:id plugin) :fn hook-fn}))
  plugin)

(defn unregister-plugin!
  "Unregister a plugin from the system."
  [plugin-id]
  (when-let [plugin (.remove plugins plugin-id)]
    (swap! plugin-order #(vec (remove #{plugin-id} %)))
    (doseq [[hook-name _] (:hooks plugin)]
      (swap! hooks update hook-name #(vec (remove (fn [h] (= (:plugin-id h) plugin-id)) %))))
    plugin))

(defn get-plugin [plugin-id]
  (.get plugins plugin-id))

(defn get-all-plugins []
  (vals plugins))

(defn get-enabled-plugins []
  (filter :enabled? (get-all-plugins)))

;; =============================================================================
;; HOOK SYSTEM
;; =============================================================================

(defn run-hook
  "Run all registered handlers for a hook."
  [hook-name & args]
  (let [handlers (get @hooks hook-name [])]
    (reduce (fn [result {:keys [plugin-id fn]}]
              (let [plugin (get-plugin plugin-id)]
                (if (:enabled? plugin)
                  (try
                    (apply fn result args)
                    (catch Exception e
                      (println "Hook error in plugin" (:name plugin) ":" (.getMessage e))
                      result))
                  result)))
            (first args)
            handlers)))

(defn run-hook-async
  "Run all registered handlers for a hook asynchronously."
  [hook-name & args]
  (future
    (apply run-hook hook-name args)))

;; =============================================================================
;; BUILT-IN HOOKS
;; =============================================================================

(def hook-points
  {:pre-analysis "Called before analysis starts"
   :post-analysis "Called after analysis completes"
   :model-detected "Called when a model is detected"
   :lollapalooza-detected "Called when Lollapalooza is detected"
   :document-ingested "Called when a document is ingested"
   :config-changed "Called when configuration changes"
   :error "Called when an error occurs"
   :startup "Called on system startup"
   :shutdown "Called on system shutdown"})

(defn get-hook-points []
  hook-points)

(defn get-hooks-for-point [hook-name]
  (get @hooks hook-name []))

;; =============================================================================
;; PLUGIN LIFECYCLE
;; =============================================================================

(defn enable-plugin! [plugin-id]
  (when-let [plugin (get-plugin plugin-id)]
    (.put plugins plugin-id (assoc plugin :enabled? true))
    true))

(defn disable-plugin! [plugin-id]
  (when-let [plugin (get-plugin plugin-id)]
    (.put plugins plugin-id (assoc plugin :enabled? false))
    true))

(defn reload-plugin! [plugin-id]
  (when-let [plugin (get-plugin plugin-id)]
    (unregister-plugin! plugin-id)
    (register-plugin! plugin)))

;; =============================================================================
;; PLUGIN CONFIGURATION
;; =============================================================================

(defn get-plugin-config [plugin-id]
  (:config (get-plugin plugin-id)))

(defn update-plugin-config! [plugin-id config-updates]
  (when-let [plugin (get-plugin plugin-id)]
    (let [new-config (merge (:config plugin) config-updates)]
      (.put plugins plugin-id (assoc plugin :config new-config))
      new-config)))

;; =============================================================================
;; PLUGIN DEPENDENCIES
;; =============================================================================

(def ^:private dependencies (atom {}))

(defn add-dependency! [plugin-id depends-on-id]
  (swap! dependencies update plugin-id (fnil conj #{}) depends-on-id))

(defn get-dependencies [plugin-id]
  (get @dependencies plugin-id #{}))

(defn dependencies-satisfied? [plugin-id]
  (let [deps (get-dependencies plugin-id)]
    (every? #(and (get-plugin %) (:enabled? (get-plugin %))) deps)))

;; =============================================================================
;; PLUGIN DISCOVERY
;; =============================================================================

(defn discover-plugins
  "Discover plugins from a directory."
  [dir]
  ;; Placeholder for plugin discovery logic
  ;; Would scan directory for plugin.edn files
  [])

(defn load-plugin-from-file
  "Load a plugin from a file."
  [filepath]
  ;; Placeholder for file-based plugin loading
  nil)

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-stats []
  {:total-plugins (.size plugins)
   :enabled-plugins (count (get-enabled-plugins))
   :hooks (into {} (map (fn [[k v]] [k (count v)]) @hooks))
   :plugin-order @plugin-order})

;; =============================================================================
;; BUILT-IN PLUGINS
;; =============================================================================

(defn create-logging-plugin []
  (create-plugin
   "logging"
   "1.0.0"
   "Logs all hook events"
   "system"
   {:pre-analysis (fn [data] (println "Pre-analysis:" data) data)
    :post-analysis (fn [data] (println "Post-analysis:" data) data)
    :model-detected (fn [data] (println "Model detected:" data) data)}
   {}))

(defn create-metrics-plugin []
  (create-plugin
   "metrics"
   "1.0.0"
   "Collects metrics for all operations"
   "system"
   {:pre-analysis (fn [data] (assoc data :start-time (System/currentTimeMillis)))
    :post-analysis (fn [data]
                     (when-let [start (:start-time data)]
                       (println "Analysis took" (- (System/currentTimeMillis) start) "ms"))
                     data)}
   {}))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-plugin-system! []
  (register-plugin! (create-logging-plugin))
  (register-plugin! (create-metrics-plugin))
  {:status :initialized :plugins (count (get-all-plugins))})

(defn shutdown-plugin-system! []
  (run-hook :shutdown)
  (doseq [plugin-id @plugin-order]
    (unregister-plugin! plugin-id))
  {:status :shutdown})
