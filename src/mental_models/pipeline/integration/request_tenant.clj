(ns mental-models.pipeline.integration.request-tenant
  "Request tenant handler for mental model analysis system.
   
   Features:
   - Multi-tenancy support
   - Tenant isolation
   - Tenant detection
   - Tenant configuration
   - Tenant quotas
   - Tenant routing
   - Tenant metrics
   - Tenant management"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:tenants {}          ;; tenant-id -> tenant config
         :quotas {}           ;; tenant-id -> quota config
         :usage {}            ;; tenant-id -> usage tracking
         :config {:default-tenant "default"
                  :tenant-header "X-Tenant-ID"
                  :tenant-param "tenant_id"
                  :isolation-mode :logical
                  :enforce-quotas? true}
         :stats {:requests-processed 0
                 :tenant-detections 0
                 :quota-checks 0
                 :quota-exceeded 0}
         :initialized? false}))

;; ============================================================================
;; Tenant Management
;; ============================================================================

(defn register-tenant!
  "Register a tenant."
  [tenant-id config]
  (let [tenant {:id tenant-id
                :name (get config :name tenant-id)
                :description (get config :description)
                :plan (get config :plan :free)
                :settings (get config :settings {})
                :metadata (get config :metadata {})
                :enabled? (atom true)
                :suspended? (atom false)
                :created-at (System/currentTimeMillis)
                :request-count (AtomicLong. 0)}]
    
    (swap! state assoc-in [:tenants tenant-id] tenant)
    (swap! state assoc-in [:usage tenant-id]
           {:requests (AtomicLong. 0)
            :bandwidth (AtomicLong. 0)
            :storage (AtomicLong. 0)
            :last-reset (atom (System/currentTimeMillis))})
    
    (logging/log :info "Registered tenant" {:tenant-id tenant-id})
    tenant-id))

(defn get-tenant
  "Get a tenant."
  [tenant-id]
  (get-in @state [:tenants tenant-id]))

(defn list-tenants
  "List all tenants."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :plan (:plan t)
           :enabled? @(:enabled? t)
           :suspended? @(:suspended? t)
           :request-count (.get (:request-count t))})
        (:tenants @state)))

(defn update-tenant!
  "Update tenant settings."
  [tenant-id updates]
  (when-let [tenant (get-tenant tenant-id)]
    (swap! state update-in [:tenants tenant-id]
           merge (select-keys updates [:name :description :plan :settings :metadata]))))

(defn enable-tenant!
  "Enable a tenant."
  [tenant-id]
  (when-let [tenant (get-tenant tenant-id)]
    (reset! (:enabled? tenant) true)))

(defn disable-tenant!
  "Disable a tenant."
  [tenant-id]
  (when-let [tenant (get-tenant tenant-id)]
    (reset! (:enabled? tenant) false)))

(defn suspend-tenant!
  "Suspend a tenant."
  [tenant-id & {:keys [reason]}]
  (when-let [tenant (get-tenant tenant-id)]
    (reset! (:suspended? tenant) true)
    (logging/log :warn "Suspended tenant" {:tenant-id tenant-id :reason reason})))

(defn unsuspend-tenant!
  "Unsuspend a tenant."
  [tenant-id]
  (when-let [tenant (get-tenant tenant-id)]
    (reset! (:suspended? tenant) false)))

(defn delete-tenant!
  "Delete a tenant."
  [tenant-id]
  (swap! state update :tenants dissoc tenant-id)
  (swap! state update :quotas dissoc tenant-id)
  (swap! state update :usage dissoc tenant-id))

;; ============================================================================
;; Tenant Detection
;; ============================================================================

(defn extract-tenant-from-header
  "Extract tenant from request header."
  [request]
  (let [header-name (get-in @state [:config :tenant-header])]
    (get-in request [:headers (str/lower-case header-name)])))

(defn extract-tenant-from-param
  "Extract tenant from query parameter."
  [request]
  (let [param-name (get-in @state [:config :tenant-param])]
    (get-in request [:query-params param-name])))

(defn extract-tenant-from-subdomain
  "Extract tenant from subdomain."
  [request]
  (let [host (get-in request [:headers "host"])]
    (when host
      (let [parts (str/split host #"\.")]
        (when (> (count parts) 2)
          (first parts))))))

(defn extract-tenant-from-path
  "Extract tenant from URL path."
  [request]
  (let [uri (:uri request)]
    (when-let [match (re-find #"^/tenants/([^/]+)/" uri)]
      (second match))))

(defn detect-tenant
  "Detect tenant from request."
  [request]
  (swap! state update-in [:stats :tenant-detections] inc)
  
  (or (extract-tenant-from-header request)
      (extract-tenant-from-param request)
      (extract-tenant-from-subdomain request)
      (extract-tenant-from-path request)
      (get-in request [:auth :tenant-id])
      (get-in @state [:config :default-tenant])))

;; ============================================================================
;; Tenant Validation
;; ============================================================================

(defn validate-tenant
  "Validate a tenant."
  [tenant-id]
  (if-let [tenant (get-tenant tenant-id)]
    (cond
      (not @(:enabled? tenant))
      {:valid? false :error :tenant-disabled}
      
      @(:suspended? tenant)
      {:valid? false :error :tenant-suspended}
      
      :else
      {:valid? true :tenant tenant})
    {:valid? false :error :tenant-not-found}))

;; ============================================================================
;; Quota Management
;; ============================================================================

(defn set-quota!
  "Set quota for a tenant."
  [tenant-id quota-config]
  (let [quota {:tenant-id tenant-id
               :requests-per-day (get quota-config :requests-per-day Long/MAX_VALUE)
               :requests-per-hour (get quota-config :requests-per-hour Long/MAX_VALUE)
               :bandwidth-per-day (get quota-config :bandwidth-per-day Long/MAX_VALUE)
               :storage-limit (get quota-config :storage-limit Long/MAX_VALUE)
               :concurrent-requests (get quota-config :concurrent-requests 100)
               :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:quotas tenant-id] quota)
    (logging/log :info "Set quota for tenant" {:tenant-id tenant-id})))

(defn get-quota
  "Get quota for a tenant."
  [tenant-id]
  (get-in @state [:quotas tenant-id]))

(defn check-quota
  "Check if tenant is within quota."
  [tenant-id]
  (swap! state update-in [:stats :quota-checks] inc)
  
  (if-let [quota (get-quota tenant-id)]
    (let [usage (get-in @state [:usage tenant-id])
          requests (.get (:requests usage))
          bandwidth (.get (:bandwidth usage))]
      (cond
        (>= requests (:requests-per-day quota))
        (do
          (swap! state update-in [:stats :quota-exceeded] inc)
          {:within-quota? false :reason :requests-exceeded})
        
        (>= bandwidth (:bandwidth-per-day quota))
        (do
          (swap! state update-in [:stats :quota-exceeded] inc)
          {:within-quota? false :reason :bandwidth-exceeded})
        
        :else
        {:within-quota? true
         :remaining-requests (- (:requests-per-day quota) requests)
         :remaining-bandwidth (- (:bandwidth-per-day quota) bandwidth)}))
    {:within-quota? true}))

(defn increment-usage!
  "Increment usage for a tenant."
  [tenant-id & {:keys [requests bandwidth storage]
                :or {requests 1 bandwidth 0 storage 0}}]
  (when-let [usage (get-in @state [:usage tenant-id])]
    (when (pos? requests)
      (.addAndGet (:requests usage) requests))
    (when (pos? bandwidth)
      (.addAndGet (:bandwidth usage) bandwidth))
    (when (pos? storage)
      (.addAndGet (:storage usage) storage))))

(defn reset-usage!
  "Reset usage for a tenant."
  [tenant-id]
  (when-let [usage (get-in @state [:usage tenant-id])]
    (.set (:requests usage) 0)
    (.set (:bandwidth usage) 0)
    (reset! (:last-reset usage) (System/currentTimeMillis))))

(defn get-usage
  "Get usage for a tenant."
  [tenant-id]
  (when-let [usage (get-in @state [:usage tenant-id])]
    {:requests (.get (:requests usage))
     :bandwidth (.get (:bandwidth usage))
     :storage (.get (:storage usage))
     :last-reset @(:last-reset usage)}))

;; ============================================================================
;; Tenant Context
;; ============================================================================

(def ^:dynamic *current-tenant* nil)

(defmacro with-tenant
  "Execute code in tenant context."
  [tenant-id & body]
  `(binding [*current-tenant* ~tenant-id]
     ~@body))

(defn current-tenant
  "Get current tenant ID."
  []
  *current-tenant*)

;; ============================================================================
;; Tenant Settings
;; ============================================================================

(defn get-tenant-setting
  "Get a tenant setting."
  [tenant-id key & {:keys [default]}]
  (if-let [tenant (get-tenant tenant-id)]
    (get-in tenant [:settings key] default)
    default))

(defn set-tenant-setting!
  "Set a tenant setting."
  [tenant-id key value]
  (swap! state assoc-in [:tenants tenant-id :settings key] value))

(defn get-tenant-settings
  "Get all tenant settings."
  [tenant-id]
  (when-let [tenant (get-tenant tenant-id)]
    (:settings tenant)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-tenant
  "Ring middleware to detect and validate tenant."
  [handler]
  (fn [request]
    (swap! state update-in [:stats :requests-processed] inc)
    
    (let [tenant-id (detect-tenant request)
          validation (validate-tenant tenant-id)]
      (if (:valid? validation)
        (let [tenant (:tenant validation)]
          (.incrementAndGet (:request-count tenant))
          (increment-usage! tenant-id)
          (handler (assoc request :tenant-id tenant-id :tenant tenant)))
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body {:error "Tenant access denied"
                :reason (:error validation)}}))))

(defn wrap-tenant-quota
  "Ring middleware to enforce tenant quotas."
  [handler]
  (fn [request]
    (let [tenant-id (:tenant-id request)]
      (if (get-in @state [:config :enforce-quotas?])
        (let [quota-check (check-quota tenant-id)]
          (if (:within-quota? quota-check)
            (let [response (handler request)]
              ;; Track bandwidth
              (when-let [body (:body response)]
                (increment-usage! tenant-id :bandwidth (count (str body))))
              response)
            {:status 429
             :headers {"Content-Type" "application/json"
                       "Retry-After" "3600"}
             :body {:error "Quota exceeded"
                    :reason (:reason quota-check)}}))
        (handler request)))))

(defn wrap-tenant-header
  "Ring middleware to add tenant header to response."
  [handler]
  (fn [request]
    (let [tenant-id (:tenant-id request)
          response (handler request)]
      (assoc-in response [:headers "X-Tenant-ID"] tenant-id))))

(defn wrap-tenant-isolation
  "Ring middleware for tenant data isolation."
  [handler]
  (fn [request]
    (let [tenant-id (:tenant-id request)]
      (with-tenant tenant-id
        (handler request)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-tenant!
  "Set default tenant."
  [tenant-id]
  (swap! state assoc-in [:config :default-tenant] tenant-id))

(defn set-tenant-header!
  "Set tenant header name."
  [header]
  (swap! state assoc-in [:config :tenant-header] header))

(defn set-isolation-mode!
  "Set isolation mode (:logical or :physical)."
  [mode]
  (swap! state assoc-in [:config :isolation-mode] mode))

(defn set-enforce-quotas!
  "Enable/disable quota enforcement."
  [enforce?]
  (swap! state assoc-in [:config :enforce-quotas?] enforce?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-tenant-metrics
  "Get tenant metrics."
  []
  (let [stats (:stats @state)]
    {:requests-processed (:requests-processed stats)
     :tenant-detections (:tenant-detections stats)
     :quota-checks (:quota-checks stats)
     :quota-exceeded (:quota-exceeded stats)
     :tenants-count (count (:tenants @state))
     :active-tenants (count (filter (fn [[_ t]] @(:enabled? t)) (:tenants @state)))
     :suspended-tenants (count (filter (fn [[_ t]] @(:suspended? t)) (:tenants @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-tenant-stats
  "Get tenant statistics."
  []
  (merge (get-tenant-metrics)
         {:default-tenant (get-in @state [:config :default-tenant])
          :isolation-mode (get-in @state [:config :isolation-mode])
          :enforce-quotas? (get-in @state [:config :enforce-quotas?])}))

(defn reset-stats!
  "Reset tenant statistics."
  []
  (swap! state assoc :stats {:requests-processed 0
                             :tenant-detections 0
                             :quota-checks 0
                             :quota-exceeded 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-tenant!
  "Initialize the request tenant handler."
  []
  (when-not (:initialized? @state)
    ;; Register default tenant
    (register-tenant! "default" {:name "Default Tenant" :plan :free})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request tenant handler initialized")
    (events/emit! :request-tenant-initialized {})
    true))
