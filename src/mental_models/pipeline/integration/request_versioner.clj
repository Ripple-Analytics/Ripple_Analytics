(ns mental-models.pipeline.integration.request-versioner
  "Request versioner for mental model analysis system.
   
   Features:
   - API versioning
   - Version negotiation
   - Version routing
   - Deprecation handling
   - Version migration
   - Version compatibility
   - Version headers
   - Versioning metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
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
  (atom {:versions {}         ;; version -> version config
         :handlers {}         ;; version -> handler map
         :migrations {}       ;; [from-version to-version] -> migration fn
         :config {:default-version "v1"
                  :supported-versions #{"v1" "v2" "v3"}
                  :version-header "X-API-Version"
                  :version-param "api_version"
                  :deprecation-warning? true}
         :stats {:requests-versioned 0
                 :version-negotiations 0
                 :migrations-performed 0
                 :deprecation-warnings 0}
         :initialized? false}))

;; ============================================================================
;; Version Management
;; ============================================================================

(defn register-version!
  "Register an API version."
  [version config]
  (let [version-config {:version version
                        :name (get config :name version)
                        :description (get config :description)
                        :released-at (get config :released-at (System/currentTimeMillis))
                        :deprecated? (atom (get config :deprecated? false))
                        :deprecated-at (atom nil)
                        :sunset-at (get config :sunset-at)
                        :enabled? (atom true)
                        :metadata (get config :metadata {})}]
    
    (swap! state assoc-in [:versions version] version-config)
    (swap! state update-in [:config :supported-versions] conj version)
    (logging/log :info "Registered API version" {:version version})
    version))

(defn get-version
  "Get a version configuration."
  [version]
  (get-in @state [:versions version]))

(defn list-versions
  "List all versions."
  []
  (mapv (fn [[v config]]
          {:version v
           :name (:name config)
           :deprecated? @(:deprecated? config)
           :enabled? @(:enabled? config)
           :released-at (:released-at config)
           :sunset-at (:sunset-at config)})
        (:versions @state)))

(defn deprecate-version!
  "Mark a version as deprecated."
  [version & {:keys [sunset-at]}]
  (when-let [config (get-version version)]
    (reset! (:deprecated? config) true)
    (reset! (:deprecated-at config) (System/currentTimeMillis))
    (when sunset-at
      (swap! state assoc-in [:versions version :sunset-at] sunset-at))
    (logging/log :warn "Deprecated API version" {:version version})))

(defn disable-version!
  "Disable a version."
  [version]
  (when-let [config (get-version version)]
    (reset! (:enabled? config) false)))

(defn enable-version!
  "Enable a version."
  [version]
  (when-let [config (get-version version)]
    (reset! (:enabled? config) true)))

;; ============================================================================
;; Version Detection
;; ============================================================================

(defn extract-version-from-header
  "Extract version from request header."
  [request]
  (let [header-name (get-in @state [:config :version-header])]
    (get-in request [:headers (str/lower-case header-name)])))

(defn extract-version-from-param
  "Extract version from query parameter."
  [request]
  (let [param-name (get-in @state [:config :version-param])]
    (get-in request [:query-params param-name])))

(defn extract-version-from-path
  "Extract version from URL path."
  [request]
  (let [uri (:uri request)]
    (when-let [match (re-find #"^/?(v\d+)/" uri)]
      (second match))))

(defn extract-version-from-accept
  "Extract version from Accept header."
  [request]
  (let [accept (get-in request [:headers "accept"])]
    (when accept
      (when-let [match (re-find #"version=(\d+)" accept)]
        (str "v" (second match))))))

(defn detect-version
  "Detect API version from request."
  [request]
  (swap! state update-in [:stats :version-negotiations] inc)
  
  (or (extract-version-from-header request)
      (extract-version-from-param request)
      (extract-version-from-path request)
      (extract-version-from-accept request)
      (get-in @state [:config :default-version])))

;; ============================================================================
;; Version Validation
;; ============================================================================

(defn version-supported?
  "Check if a version is supported."
  [version]
  (contains? (get-in @state [:config :supported-versions]) version))

(defn version-enabled?
  "Check if a version is enabled."
  [version]
  (if-let [config (get-version version)]
    @(:enabled? config)
    false))

(defn version-deprecated?
  "Check if a version is deprecated."
  [version]
  (if-let [config (get-version version)]
    @(:deprecated? config)
    false))

(defn version-sunset?
  "Check if a version has passed its sunset date."
  [version]
  (if-let [config (get-version version)]
    (and (:sunset-at config)
         (> (System/currentTimeMillis) (:sunset-at config)))
    false))

(defn validate-version
  "Validate a version."
  [version]
  (cond
    (not (version-supported? version))
    {:valid? false :error :unsupported-version}
    
    (not (version-enabled? version))
    {:valid? false :error :version-disabled}
    
    (version-sunset? version)
    {:valid? false :error :version-sunset}
    
    :else
    {:valid? true
     :deprecated? (version-deprecated? version)}))

;; ============================================================================
;; Version Handlers
;; ============================================================================

(defn register-handler!
  "Register a handler for a specific version."
  [version route handler]
  (swap! state assoc-in [:handlers version route] handler))

(defn get-handler
  "Get a handler for a version and route."
  [version route]
  (get-in @state [:handlers version route]))

(defn list-handlers
  "List all handlers for a version."
  [version]
  (keys (get-in @state [:handlers version] {})))

;; ============================================================================
;; Version Migration
;; ============================================================================

(defn register-migration!
  "Register a migration between versions."
  [from-version to-version migrate-fn]
  (swap! state assoc-in [:migrations [from-version to-version]] migrate-fn)
  (logging/log :info "Registered version migration" {:from from-version :to to-version}))

(defn get-migration
  "Get a migration function."
  [from-version to-version]
  (get-in @state [:migrations [from-version to-version]]))

(defn migrate-request
  "Migrate a request from one version to another."
  [request from-version to-version]
  (if-let [migrate-fn (get-migration from-version to-version)]
    (do
      (swap! state update-in [:stats :migrations-performed] inc)
      (migrate-fn request))
    request))

(defn migrate-response
  "Migrate a response from one version to another."
  [response from-version to-version]
  (if-let [migrate-fn (get-migration to-version from-version)]
    (do
      (swap! state update-in [:stats :migrations-performed] inc)
      (migrate-fn response))
    response))

;; ============================================================================
;; Version Routing
;; ============================================================================

(defn route-by-version
  "Route a request to the appropriate version handler."
  [request handlers]
  (let [version (detect-version request)
        validation (validate-version version)]
    (if (:valid? validation)
      (if-let [handler (get handlers version)]
        (handler request)
        (if-let [default-handler (get handlers :default)]
          (default-handler request)
          {:status 404
           :body {:error "No handler for version"
                  :version version}}))
      {:status 400
       :body {:error "Invalid version"
              :reason (:error validation)}})))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-version
  "Ring middleware to detect and validate API version."
  [handler]
  (fn [request]
    (swap! state update-in [:stats :requests-versioned] inc)
    
    (let [version (detect-version request)
          validation (validate-version version)]
      (if (:valid? validation)
        (let [response (handler (assoc request :api-version version))]
          ;; Add deprecation warning if needed
          (if (and (:deprecated? validation)
                   (get-in @state [:config :deprecation-warning?]))
            (do
              (swap! state update-in [:stats :deprecation-warnings] inc)
              (let [config (get-version version)]
                (-> response
                    (assoc-in [:headers "Deprecation"] "true")
                    (assoc-in [:headers "Sunset"]
                              (when-let [sunset (:sunset-at config)]
                                (str (Instant/ofEpochMilli sunset)))))))
            response))
        {:status 400
         :headers {"Content-Type" "application/json"}
         :body {:error "Invalid API version"
                :reason (:error validation)
                :supported-versions (vec (get-in @state [:config :supported-versions]))}}))))

(defn wrap-version-routing
  "Ring middleware for version-based routing."
  [handlers]
  (fn [request]
    (route-by-version request handlers)))

(defn wrap-version-header
  "Ring middleware to add version header to response."
  [handler]
  (fn [request]
    (let [version (or (:api-version request)
                      (detect-version request))
          response (handler request)]
      (assoc-in response [:headers "X-API-Version"] version))))

(defn wrap-version-migration
  "Ring middleware to migrate requests/responses between versions."
  [handler target-version]
  (fn [request]
    (let [request-version (detect-version request)
          migrated-request (if (not= request-version target-version)
                             (migrate-request request request-version target-version)
                             request)
          response (handler (assoc migrated-request :api-version target-version))]
      (if (not= request-version target-version)
        (migrate-response response target-version request-version)
        response))))

;; ============================================================================
;; Version Comparison
;; ============================================================================

(defn parse-version
  "Parse a version string to a number."
  [version]
  (when version
    (if-let [match (re-find #"v?(\d+)" version)]
      (Integer/parseInt (second match))
      0)))

(defn compare-versions
  "Compare two versions."
  [v1 v2]
  (compare (parse-version v1) (parse-version v2)))

(defn version-gte?
  "Check if v1 >= v2."
  [v1 v2]
  (>= (compare-versions v1 v2) 0))

(defn version-lte?
  "Check if v1 <= v2."
  [v1 v2]
  (<= (compare-versions v1 v2) 0))

(defn version-between?
  "Check if version is between min and max (inclusive)."
  [version min-version max-version]
  (and (version-gte? version min-version)
       (version-lte? version max-version)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-version!
  "Set default API version."
  [version]
  (swap! state assoc-in [:config :default-version] version))

(defn set-version-header!
  "Set version header name."
  [header]
  (swap! state assoc-in [:config :version-header] header))

(defn set-version-param!
  "Set version query parameter name."
  [param]
  (swap! state assoc-in [:config :version-param] param))

(defn set-deprecation-warning!
  "Enable/disable deprecation warnings."
  [enabled?]
  (swap! state assoc-in [:config :deprecation-warning?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-versioner-metrics
  "Get versioner metrics."
  []
  (let [stats (:stats @state)]
    {:requests-versioned (:requests-versioned stats)
     :version-negotiations (:version-negotiations stats)
     :migrations-performed (:migrations-performed stats)
     :deprecation-warnings (:deprecation-warnings stats)
     :versions-count (count (:versions @state))
     :supported-versions (count (get-in @state [:config :supported-versions]))
     :deprecated-versions (count (filter (fn [[_ v]] @(:deprecated? v))
                                         (:versions @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-versioner-stats
  "Get versioner statistics."
  []
  (merge (get-versioner-metrics)
         {:default-version (get-in @state [:config :default-version])
          :version-header (get-in @state [:config :version-header])
          :deprecation-warning? (get-in @state [:config :deprecation-warning?])}))

(defn reset-stats!
  "Reset versioner statistics."
  []
  (swap! state assoc :stats {:requests-versioned 0
                             :version-negotiations 0
                             :migrations-performed 0
                             :deprecation-warnings 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-versioner!
  "Initialize the request versioner."
  []
  (when-not (:initialized? @state)
    ;; Register default versions
    (register-version! "v1" {:name "Version 1" :description "Initial API version"})
    (register-version! "v2" {:name "Version 2" :description "Enhanced API version"})
    (register-version! "v3" {:name "Version 3" :description "Latest API version"})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request versioner initialized")
    (events/emit! :request-versioner-initialized {})
    true))
