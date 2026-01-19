(ns mental-models.pipeline.integration.api-versioning
  "API versioning and backward compatibility management.
   
   Features:
   - Semantic versioning support
   - Version negotiation
   - Deprecation management
   - Migration paths between versions
   - Version-specific routing
   - Backward compatibility checks
   - API changelog tracking
   - Client version tracking"
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
  (atom {:versions {}         ;; version-string -> version-config
         :current-version nil ;; current active version
         :deprecations {}     ;; endpoint -> deprecation-info
         :migrations {}       ;; [from-version to-version] -> migration-fn
         :changelog []        ;; list of changes
         :client-versions {}  ;; client-id -> version-info
         :initialized? false}))

;; ============================================================================
;; Version Parsing
;; ============================================================================

(defn parse-version
  "Parse a semantic version string."
  [version-str]
  (when version-str
    (let [cleaned (str/replace version-str #"^v" "")
          parts (str/split cleaned #"\.")
          [major minor patch] (map #(try (Integer/parseInt %)
                                         (catch Exception _ 0))
                                   (concat parts [0 0 0]))]
      {:major major
       :minor minor
       :patch patch
       :string (str major "." minor "." patch)
       :original version-str})))

(defn compare-versions
  "Compare two version strings. Returns -1, 0, or 1."
  [v1 v2]
  (let [p1 (parse-version v1)
        p2 (parse-version v2)]
    (cond
      (< (:major p1) (:major p2)) -1
      (> (:major p1) (:major p2)) 1
      (< (:minor p1) (:minor p2)) -1
      (> (:minor p1) (:minor p2)) 1
      (< (:patch p1) (:patch p2)) -1
      (> (:patch p1) (:patch p2)) 1
      :else 0)))

(defn version-satisfies?
  "Check if a version satisfies a requirement (e.g., >=1.0.0, ~1.2.0)."
  [version requirement]
  (let [req-str (str/trim requirement)
        [_ operator req-version] (re-matches #"([<>=~^]+)?(.+)" req-str)
        comparison (compare-versions version req-version)]
    (case operator
      ">=" (>= comparison 0)
      ">" (> comparison 0)
      "<=" (<= comparison 0)
      "<" (< comparison 0)
      "=" (= comparison 0)
      "~" (let [pv (parse-version version)
                pr (parse-version req-version)]
            (and (= (:major pv) (:major pr))
                 (= (:minor pv) (:minor pr))))
      "^" (let [pv (parse-version version)
                pr (parse-version req-version)]
            (= (:major pv) (:major pr)))
      (= comparison 0))))

;; ============================================================================
;; Version Registration
;; ============================================================================

(defn register-version!
  "Register an API version."
  [version-str config]
  (let [parsed (parse-version version-str)
        version {:version (:string parsed)
                 :parsed parsed
                 :name (get config :name (str "Version " (:string parsed)))
                 :description (get config :description "")
                 :released-at (get config :released-at (System/currentTimeMillis))
                 :deprecated? (get config :deprecated? false)
                 :sunset-date (get config :sunset-date nil)
                 :endpoints (get config :endpoints {})
                 :features (get config :features [])
                 :breaking-changes (get config :breaking-changes [])
                 :status (get config :status :active)}]
    (swap! state assoc-in [:versions (:string parsed)] version)
    (logging/log :info "Registered API version" {:version (:string parsed)})
    (events/emit! :version-registered {:version (:string parsed)})
    (:string parsed)))

(defn get-version
  "Get version configuration."
  [version-str]
  (let [parsed (parse-version version-str)]
    (get-in @state [:versions (:string parsed)])))

(defn list-versions
  "List all registered versions."
  []
  (let [versions (vals (:versions @state))]
    (->> versions
         (sort-by #(get-in % [:parsed :string]) compare-versions)
         (mapv #(select-keys % [:version :name :status :deprecated? :released-at])))))

(defn set-current-version!
  "Set the current active API version."
  [version-str]
  (let [parsed (parse-version version-str)]
    (swap! state assoc :current-version (:string parsed))
    (logging/log :info "Set current API version" {:version (:string parsed)})))

(defn get-current-version
  "Get the current active API version."
  []
  (:current-version @state))

;; ============================================================================
;; Version Negotiation
;; ============================================================================

(defn negotiate-version
  "Negotiate the best version based on client request."
  [requested-version & {:keys [accept-header min-version max-version]}]
  (let [versions (keys (:versions @state))
        current (get-current-version)
        
        ;; Parse requested version
        requested (when requested-version
                    (:string (parse-version requested-version)))
        
        ;; Filter available versions
        available (cond->> versions
                    min-version (filter #(>= (compare-versions % min-version) 0))
                    max-version (filter #(<= (compare-versions % max-version) 0))
                    true (filter #(not (:deprecated? (get-version %)))))
        
        ;; Find best match
        negotiated (cond
                     ;; Exact match requested
                     (and requested (contains? (set versions) requested))
                     requested
                     
                     ;; Use current if available
                     (contains? (set available) current)
                     current
                     
                     ;; Use latest available
                     (seq available)
                     (last (sort compare-versions available))
                     
                     ;; Fallback to current
                     :else current)]
    
    {:negotiated negotiated
     :requested requested
     :available (vec available)
     :exact-match? (= negotiated requested)}))

(defn version-middleware
  "Create middleware for version negotiation."
  [handler]
  (fn [request]
    (let [;; Extract version from various sources
          header-version (get-in request [:headers "api-version"])
          query-version (get-in request [:query-params "version"])
          path-version (second (re-find #"/v(\d+(?:\.\d+)*)" (:uri request "")))
          
          requested (or header-version query-version path-version)
          negotiation (negotiate-version requested)
          
          ;; Add version info to request
          versioned-request (assoc request
                                   :api-version (:negotiated negotiation)
                                   :version-negotiation negotiation)]
      
      (metrics/increment :api-requests {:version (:negotiated negotiation)})
      (handler versioned-request))))

;; ============================================================================
;; Deprecation Management
;; ============================================================================

(defn deprecate-endpoint!
  "Mark an endpoint as deprecated."
  [endpoint config]
  (let [deprecation {:endpoint endpoint
                     :deprecated-in (get config :deprecated-in)
                     :removed-in (get config :removed-in)
                     :replacement (get config :replacement)
                     :reason (get config :reason "")
                     :sunset-date (get config :sunset-date)
                     :deprecated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:deprecations endpoint] deprecation)
    (logging/log :warn "Deprecated endpoint" {:endpoint endpoint})
    (events/emit! :endpoint-deprecated {:endpoint endpoint})
    endpoint))

(defn get-deprecation
  "Get deprecation info for an endpoint."
  [endpoint]
  (get-in @state [:deprecations endpoint]))

(defn list-deprecations
  "List all deprecated endpoints."
  []
  (vals (:deprecations @state)))

(defn is-deprecated?
  "Check if an endpoint is deprecated in a version."
  [endpoint version]
  (when-let [dep (get-deprecation endpoint)]
    (and (:deprecated-in dep)
         (>= (compare-versions version (:deprecated-in dep)) 0))))

(defn is-removed?
  "Check if an endpoint is removed in a version."
  [endpoint version]
  (when-let [dep (get-deprecation endpoint)]
    (and (:removed-in dep)
         (>= (compare-versions version (:removed-in dep)) 0))))

(defn deprecation-warning
  "Generate deprecation warning for an endpoint."
  [endpoint version]
  (when-let [dep (get-deprecation endpoint)]
    (when (is-deprecated? endpoint version)
      {:warning (str "Endpoint " endpoint " is deprecated"
                     (when (:removed-in dep)
                       (str " and will be removed in version " (:removed-in dep)))
                     (when (:replacement dep)
                       (str ". Use " (:replacement dep) " instead")))
       :replacement (:replacement dep)
       :sunset-date (:sunset-date dep)})))

;; ============================================================================
;; Migration Management
;; ============================================================================

(defn register-migration!
  "Register a migration between versions."
  [from-version to-version migration-fn]
  (let [from (parse-version from-version)
        to (parse-version to-version)
        key [(:string from) (:string to)]]
    (swap! state assoc-in [:migrations key]
           {:from (:string from)
            :to (:string to)
            :fn migration-fn
            :registered-at (System/currentTimeMillis)})
    (logging/log :info "Registered migration" {:from (:string from) :to (:string to)})
    key))

(defn get-migration
  "Get migration function between versions."
  [from-version to-version]
  (let [from (parse-version from-version)
        to (parse-version to-version)
        key [(:string from) (:string to)]]
    (get-in @state [:migrations key])))

(defn find-migration-path
  "Find a path of migrations between versions."
  [from-version to-version]
  (let [from (:string (parse-version from-version))
        to (:string (parse-version to-version))
        migrations (:migrations @state)
        
        ;; Simple BFS to find path
        find-path (fn [start end]
                    (loop [queue [[start]]
                           visited #{start}]
                      (when (seq queue)
                        (let [path (first queue)
                              current (last path)]
                          (if (= current end)
                            path
                            (let [next-versions (->> migrations
                                                     (filter (fn [[[f _] _]] (= f current)))
                                                     (map (fn [[[_ t] _]] t))
                                                     (remove visited))
                                  new-paths (map #(conj path %) next-versions)]
                              (recur (concat (rest queue) new-paths)
                                     (into visited next-versions))))))))]
    (when-let [path (find-path from to)]
      {:path path
       :steps (dec (count path))
       :migrations (for [[f t] (partition 2 1 path)]
                     (get-migration f t))})))

(defn migrate-data
  "Migrate data between versions."
  [data from-version to-version]
  (let [path (find-migration-path from-version to-version)]
    (if path
      (reduce (fn [d migration]
                ((:fn migration) d))
              data
              (:migrations path))
      data)))

;; ============================================================================
;; Changelog
;; ============================================================================

(defn add-changelog-entry!
  "Add an entry to the API changelog."
  [entry]
  (let [changelog-entry {:id (str (UUID/randomUUID))
                         :version (get entry :version)
                         :type (get entry :type :change)
                         :category (get entry :category :general)
                         :title (get entry :title "")
                         :description (get entry :description "")
                         :breaking? (get entry :breaking? false)
                         :endpoints (get entry :endpoints [])
                         :added-at (System/currentTimeMillis)}]
    (swap! state update :changelog conj changelog-entry)
    (logging/log :info "Added changelog entry" {:version (:version entry) :type (:type entry)})
    (:id changelog-entry)))

(defn get-changelog
  "Get changelog entries."
  [& {:keys [version type since limit]}]
  (let [entries (:changelog @state)
        filtered (cond->> entries
                   version (filter #(= (:version %) version))
                   type (filter #(= (:type %) type))
                   since (filter #(>= (:added-at %) since))
                   true (sort-by :added-at >)
                   limit (take limit))]
    (vec filtered)))

(defn get-breaking-changes
  "Get breaking changes between versions."
  [from-version to-version]
  (let [from (:string (parse-version from-version))
        to (:string (parse-version to-version))
        entries (:changelog @state)]
    (->> entries
         (filter :breaking?)
         (filter #(and (>= (compare-versions (:version %) from) 0)
                       (<= (compare-versions (:version %) to) 0)))
         (sort-by :added-at)
         vec)))

;; ============================================================================
;; Client Version Tracking
;; ============================================================================

(defn track-client-version!
  "Track a client's API version usage."
  [client-id version & {:keys [user-agent platform]}]
  (let [tracking {:client-id client-id
                  :version version
                  :user-agent user-agent
                  :platform platform
                  :first-seen (get-in @state [:client-versions client-id :first-seen]
                                      (System/currentTimeMillis))
                  :last-seen (System/currentTimeMillis)
                  :request-count (inc (get-in @state [:client-versions client-id :request-count] 0))}]
    (swap! state assoc-in [:client-versions client-id] tracking)
    tracking))

(defn get-client-version
  "Get a client's version info."
  [client-id]
  (get-in @state [:client-versions client-id]))

(defn get-version-usage-stats
  "Get statistics on version usage across clients."
  []
  (let [clients (vals (:client-versions @state))
        by-version (group-by :version clients)]
    {:total-clients (count clients)
     :by-version (into {} (map (fn [[v cs]]
                                 [v {:clients (count cs)
                                     :requests (reduce + (map :request-count cs))}])
                               by-version))
     :outdated-clients (count (filter #(< (compare-versions (:version %) (get-current-version)) 0)
                                      clients))}))

;; ============================================================================
;; Compatibility Checks
;; ============================================================================

(defn check-compatibility
  "Check if a request is compatible with the current API."
  [request version]
  (let [endpoint (:uri request)
        method (:request-method request)
        version-config (get-version version)
        deprecation (get-deprecation endpoint)]
    {:compatible? (and version-config
                       (not (is-removed? endpoint version)))
     :deprecated? (is-deprecated? endpoint version)
     :deprecation-warning (deprecation-warning endpoint version)
     :version version
     :endpoint endpoint
     :method method}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-versioning-stats
  "Get API versioning statistics."
  []
  {:total-versions (count (:versions @state))
   :current-version (get-current-version)
   :deprecated-endpoints (count (:deprecations @state))
   :migrations (count (:migrations @state))
   :changelog-entries (count (:changelog @state))
   :tracked-clients (count (:client-versions @state))
   :version-usage (get-version-usage-stats)})

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-api-versioning!
  "Initialize API versioning."
  []
  (when-not (:initialized? @state)
    ;; Register initial versions
    (register-version! "1.0.0"
                       {:name "Initial Release"
                        :description "First stable API version"
                        :features [:mental-model-detection :analysis-pipeline]
                        :status :active})
    
    (register-version! "1.1.0"
                       {:name "Enhanced Analysis"
                        :description "Added advanced analysis features"
                        :features [:batch-processing :streaming-analysis]
                        :status :active})
    
    (register-version! "2.0.0"
                       {:name "Major Update"
                        :description "Breaking changes for improved architecture"
                        :features [:new-api-structure :improved-performance]
                        :breaking-changes ["Changed response format" "Renamed endpoints"]
                        :status :active})
    
    ;; Set current version
    (set-current-version! "2.0.0")
    
    ;; Register sample migration
    (register-migration! "1.0.0" "1.1.0"
                         (fn [data]
                           (assoc data :migrated-to "1.1.0")))
    
    ;; Add changelog entries
    (add-changelog-entry! {:version "1.0.0"
                           :type :release
                           :title "Initial Release"
                           :description "First stable API version"})
    
    (add-changelog-entry! {:version "2.0.0"
                           :type :breaking
                           :title "Response Format Change"
                           :description "Changed response format for consistency"
                           :breaking? true})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "API versioning initialized")
    (events/emit! :api-versioning-initialized {})
    true))
