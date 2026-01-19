(ns mental-models.pipeline.integration.request-authorizer
  "Request authorizer for mental model analysis system.
   
   Features:
   - Role-based access control (RBAC)
   - Permission management
   - Resource authorization
   - Policy evaluation
   - Scope validation
   - Authorization caching
   - Audit logging
   - Authorization metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]
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
  (atom {:roles {}            ;; role-id -> role config
         :permissions {}      ;; permission-id -> permission config
         :policies {}         ;; policy-id -> authorization policy
         :user-roles {}       ;; user-id -> set of role-ids
         :cache {}            ;; cache-key -> authorization result
         :config {:cache-enabled? true
                  :cache-ttl-ms 60000
                  :default-deny? true
                  :audit-enabled? true}
         :stats {:authorizations 0
                 :grants 0
                 :denials 0
                 :cache-hits 0}
         :initialized? false}))

;; ============================================================================
;; Permission Management
;; ============================================================================

(defn register-permission!
  "Register a permission."
  [permission-id config]
  (let [permission {:id permission-id
                    :name (get config :name (name permission-id))
                    :description (get config :description)
                    :resource (get config :resource)
                    :actions (get config :actions #{})
                    :metadata (get config :metadata {})
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:permissions permission-id] permission)
    (logging/log :info "Registered permission" {:permission-id permission-id})
    permission-id))

(defn get-permission
  "Get a permission."
  [permission-id]
  (get-in @state [:permissions permission-id]))

(defn list-permissions
  "List all permissions."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :resource (:resource p)
           :actions (:actions p)})
        (:permissions @state)))

(defn delete-permission!
  "Delete a permission."
  [permission-id]
  (swap! state update :permissions dissoc permission-id))

;; ============================================================================
;; Role Management
;; ============================================================================

(defn register-role!
  "Register a role."
  [role-id config]
  (let [role {:id role-id
              :name (get config :name (name role-id))
              :description (get config :description)
              :permissions (atom (set (get config :permissions [])))
              :parent-roles (get config :parent-roles #{})
              :metadata (get config :metadata {})
              :enabled? (atom true)
              :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:roles role-id] role)
    (logging/log :info "Registered role" {:role-id role-id})
    role-id))

(defn get-role
  "Get a role."
  [role-id]
  (get-in @state [:roles role-id]))

(defn list-roles
  "List all roles."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :permissions (count @(:permissions r))
           :parent-roles (:parent-roles r)
           :enabled? @(:enabled? r)})
        (:roles @state)))

(defn add-permission-to-role!
  "Add a permission to a role."
  [role-id permission-id]
  (when-let [role (get-role role-id)]
    (swap! (:permissions role) conj permission-id)))

(defn remove-permission-from-role!
  "Remove a permission from a role."
  [role-id permission-id]
  (when-let [role (get-role role-id)]
    (swap! (:permissions role) disj permission-id)))

(defn get-role-permissions
  "Get all permissions for a role (including inherited)."
  [role-id]
  (when-let [role (get-role role-id)]
    (let [direct-perms @(:permissions role)
          parent-perms (mapcat get-role-permissions (:parent-roles role))]
      (set/union direct-perms (set parent-perms)))))

(defn disable-role!
  "Disable a role."
  [role-id]
  (when-let [role (get-role role-id)]
    (reset! (:enabled? role) false)))

(defn enable-role!
  "Enable a role."
  [role-id]
  (when-let [role (get-role role-id)]
    (reset! (:enabled? role) true)))

;; ============================================================================
;; User-Role Assignment
;; ============================================================================

(defn assign-role!
  "Assign a role to a user."
  [user-id role-id]
  (swap! state update-in [:user-roles user-id]
         (fnil conj #{}) role-id)
  (logging/log :info "Assigned role to user" {:user-id user-id :role-id role-id}))

(defn revoke-role!
  "Revoke a role from a user."
  [user-id role-id]
  (swap! state update-in [:user-roles user-id] disj role-id)
  (logging/log :info "Revoked role from user" {:user-id user-id :role-id role-id}))

(defn get-user-roles
  "Get all roles for a user."
  [user-id]
  (get-in @state [:user-roles user-id] #{}))

(defn get-user-permissions
  "Get all permissions for a user."
  [user-id]
  (let [roles (get-user-roles user-id)]
    (reduce (fn [perms role-id]
              (set/union perms (get-role-permissions role-id)))
            #{}
            roles)))

;; ============================================================================
;; Authorization Policies
;; ============================================================================

(defn register-policy!
  "Register an authorization policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :resource-pattern (get config :resource-pattern)
                :actions (get config :actions #{})
                :condition-fn (get config :condition-fn (constantly true))
                :effect (get config :effect :allow)
                :priority (get config :priority 100)
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:policies policy-id] policy)
    policy-id))

(defn get-policy
  "Get a policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :resource-pattern (:resource-pattern p)
           :actions (:actions p)
           :effect (:effect p)
           :priority (:priority p)
           :enabled? @(:enabled? p)})
        (:policies @state)))

(defn evaluate-policy
  "Evaluate a policy against a request."
  [policy request]
  (let [{:keys [resource action user]} request
        resource-match? (or (nil? (:resource-pattern policy))
                            (= (:resource-pattern policy) resource)
                            (and (string? (:resource-pattern policy))
                                 (str/starts-with? resource (:resource-pattern policy)))
                            (and (instance? java.util.regex.Pattern (:resource-pattern policy))
                                 (re-matches (:resource-pattern policy) resource)))
        action-match? (or (empty? (:actions policy))
                          (contains? (:actions policy) action))
        condition-match? ((:condition-fn policy) request)]
    (when (and resource-match? action-match? condition-match?)
      (:effect policy))))

;; ============================================================================
;; Authorization
;; ============================================================================

(defn- cache-key
  "Generate a cache key."
  [user-id resource action]
  (str user-id ":" resource ":" action))

(defn- get-cached
  "Get cached authorization result."
  [key]
  (when (get-in @state [:config :cache-enabled?])
    (when-let [entry (get-in @state [:cache key])]
      (when (< (- (System/currentTimeMillis) (:timestamp entry))
               (get-in @state [:config :cache-ttl-ms]))
        (swap! state update-in [:stats :cache-hits] inc)
        (:result entry)))))

(defn- put-cached!
  "Put authorization result in cache."
  [key result]
  (when (get-in @state [:config :cache-enabled?])
    (swap! state assoc-in [:cache key]
           {:result result :timestamp (System/currentTimeMillis)})))

(defn authorize
  "Authorize a request."
  [request]
  (swap! state update-in [:stats :authorizations] inc)
  
  (let [{:keys [user-id resource action]} request
        cache-k (cache-key user-id resource action)]
    
    ;; Check cache
    (if-let [cached (get-cached cache-k)]
      cached
      
      (let [;; Check permission-based authorization
            user-perms (get-user-permissions user-id)
            permission-match? (some (fn [perm-id]
                                      (when-let [perm (get-permission perm-id)]
                                        (and (or (nil? (:resource perm))
                                                 (= (:resource perm) resource))
                                             (or (empty? (:actions perm))
                                                 (contains? (:actions perm) action)))))
                                    user-perms)
            
            ;; Check policy-based authorization
            policies (->> (vals (:policies @state))
                          (filter #@(:enabled? %))
                          (sort-by :priority))
            policy-effect (some #(evaluate-policy % request) policies)
            
            ;; Determine final result
            result (cond
                     (= policy-effect :deny) {:authorized? false :reason :policy-denied}
                     (= policy-effect :allow) {:authorized? true :reason :policy-allowed}
                     permission-match? {:authorized? true :reason :permission-granted}
                     (get-in @state [:config :default-deny?]) {:authorized? false :reason :default-deny}
                     :else {:authorized? true :reason :default-allow})]
        
        ;; Update stats
        (if (:authorized? result)
          (swap! state update-in [:stats :grants] inc)
          (swap! state update-in [:stats :denials] inc))
        
        ;; Audit log
        (when (get-in @state [:config :audit-enabled?])
          (logging/log (if (:authorized? result) :info :warn)
                       "Authorization decision"
                       {:user-id user-id
                        :resource resource
                        :action action
                        :authorized? (:authorized? result)
                        :reason (:reason result)}))
        
        ;; Cache result
        (put-cached! cache-k result)
        
        result))))

(defn authorize?
  "Check if a request is authorized (returns boolean)."
  [request]
  (:authorized? (authorize request)))

;; ============================================================================
;; Scope Validation
;; ============================================================================

(defn validate-scopes
  "Validate that user has required scopes."
  [user-scopes required-scopes]
  (let [user-scope-set (set user-scopes)
        required-scope-set (set required-scopes)
        missing (set/difference required-scope-set user-scope-set)]
    {:valid? (empty? missing)
     :missing missing}))

(defn has-scope?
  "Check if user has a specific scope."
  [user-scopes scope]
  (contains? (set user-scopes) scope))

(defn has-any-scope?
  "Check if user has any of the specified scopes."
  [user-scopes scopes]
  (boolean (some (set user-scopes) scopes)))

(defn has-all-scopes?
  "Check if user has all specified scopes."
  [user-scopes scopes]
  (every? (set user-scopes) scopes))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-authorize
  "Ring middleware for authorization."
  [handler & {:keys [resource-fn action-fn]
              :or {resource-fn :uri
                   action-fn :request-method}}]
  (fn [request]
    (let [user-id (get-in request [:auth :user-id])
          resource (resource-fn request)
          action (action-fn request)
          auth-result (authorize {:user-id user-id
                                  :resource resource
                                  :action action})]
      (if (:authorized? auth-result)
        (handler (assoc request :authorization auth-result))
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body {:error "Forbidden"
                :reason (:reason auth-result)}}))))

(defn wrap-require-role
  "Ring middleware to require a specific role."
  [handler role-id]
  (fn [request]
    (let [user-id (get-in request [:auth :user-id])
          user-roles (get-user-roles user-id)]
      (if (contains? user-roles role-id)
        (handler request)
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body {:error "Forbidden"
                :reason :missing-role}}))))

(defn wrap-require-permission
  "Ring middleware to require a specific permission."
  [handler permission-id]
  (fn [request]
    (let [user-id (get-in request [:auth :user-id])
          user-perms (get-user-permissions user-id)]
      (if (contains? user-perms permission-id)
        (handler request)
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body {:error "Forbidden"
                :reason :missing-permission}}))))

(defn wrap-require-scopes
  "Ring middleware to require specific scopes."
  [handler required-scopes]
  (fn [request]
    (let [user-scopes (get-in request [:auth :scopes] [])
          validation (validate-scopes user-scopes required-scopes)]
      (if (:valid? validation)
        (handler request)
        {:status 403
         :headers {"Content-Type" "application/json"}
         :body {:error "Forbidden"
                :reason :missing-scopes
                :missing (:missing validation)}}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-cache-enabled!
  "Enable/disable authorization caching."
  [enabled?]
  (swap! state assoc-in [:config :cache-enabled?] enabled?))

(defn set-default-deny!
  "Set default deny policy."
  [deny?]
  (swap! state assoc-in [:config :default-deny?] deny?))

(defn set-audit-enabled!
  "Enable/disable audit logging."
  [enabled?]
  (swap! state assoc-in [:config :audit-enabled?] enabled?))

(defn clear-cache!
  "Clear the authorization cache."
  []
  (swap! state assoc :cache {}))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-authorizer-metrics
  "Get authorizer metrics."
  []
  (let [stats (:stats @state)]
    {:authorizations (:authorizations stats)
     :grants (:grants stats)
     :denials (:denials stats)
     :cache-hits (:cache-hits stats)
     :roles-count (count (:roles @state))
     :permissions-count (count (:permissions @state))
     :policies-count (count (:policies @state))
     :grant-rate (if (pos? (:authorizations stats))
                   (/ (:grants stats) (:authorizations stats))
                   1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-authorizer-stats
  "Get authorizer statistics."
  []
  (merge (get-authorizer-metrics)
         {:cache-enabled? (get-in @state [:config :cache-enabled?])
          :default-deny? (get-in @state [:config :default-deny?])
          :audit-enabled? (get-in @state [:config :audit-enabled?])}))

(defn reset-stats!
  "Reset authorizer statistics."
  []
  (swap! state assoc :stats {:authorizations 0
                             :grants 0
                             :denials 0
                             :cache-hits 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-authorizer!
  "Initialize the request authorizer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request authorizer initialized")
    (events/emit! :request-authorizer-initialized {})
    true))
