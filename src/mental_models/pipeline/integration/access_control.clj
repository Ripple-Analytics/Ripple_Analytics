(ns mental-models.pipeline.integration.access-control
  "Access Control Module
   
   Permission and authorization management:
   - Role-based access control (RBAC)
   - Permission checking
   - Resource-level permissions
   - User roles management
   - Access audit logging"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.audit.logger :as audit]))

;; =============================================================================
;; ACCESS CONTROL STATE
;; =============================================================================

(defonce acl-state (atom {:roles {}
                          :users {}
                          :permissions {}
                          :resource-permissions {}
                          :config {:default-role :viewer
                                   :super-admin-role :admin}}))

;; =============================================================================
;; PERMISSION DEFINITIONS
;; =============================================================================

(def default-permissions
  "Default system permissions."
  #{:analysis/read
    :analysis/create
    :analysis/update
    :analysis/delete
    :models/read
    :models/create
    :models/update
    :models/delete
    :reports/read
    :reports/create
    :reports/export
    :alerts/read
    :alerts/acknowledge
    :alerts/configure
    :config/read
    :config/update
    :users/read
    :users/create
    :users/update
    :users/delete
    :admin/system
    :admin/audit})

;; =============================================================================
;; ROLE MANAGEMENT
;; =============================================================================

(defn define-role!
  "Define a role with permissions."
  [role-id permissions & {:keys [description inherits]}]
  (log/info "Defining role" {:role role-id :permissions (count permissions)})
  (swap! acl-state assoc-in [:roles role-id]
         {:id role-id
          :permissions (set permissions)
          :description description
          :inherits inherits
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :acl/roles-defined)
  role-id)

(defn get-role
  "Get a role by ID."
  [role-id]
  (get-in @acl-state [:roles role-id]))

(defn list-roles
  "List all defined roles."
  []
  (keys (:roles @acl-state)))

(defn delete-role!
  "Delete a role."
  [role-id]
  (log/info "Deleting role" {:role role-id})
  (swap! acl-state update :roles dissoc role-id))

(defn get-role-permissions
  "Get all permissions for a role, including inherited."
  [role-id]
  (when-let [role (get-role role-id)]
    (let [direct-perms (:permissions role)
          inherited-perms (when-let [parent (:inherits role)]
                            (get-role-permissions parent))]
      (set/union direct-perms (or inherited-perms #{})))))

;; =============================================================================
;; USER ROLE ASSIGNMENT
;; =============================================================================

(defn assign-role!
  "Assign a role to a user."
  [user-id role-id]
  (log/info "Assigning role" {:user user-id :role role-id})
  (swap! acl-state update-in [:users user-id :roles] (fnil conj #{}) role-id)
  (audit/log-event! :acl/role-assigned {:user user-id :role role-id})
  (events/publish! :acl/role-assigned {:user user-id :role role-id}))

(defn revoke-role!
  "Revoke a role from a user."
  [user-id role-id]
  (log/info "Revoking role" {:user user-id :role role-id})
  (swap! acl-state update-in [:users user-id :roles] disj role-id)
  (audit/log-event! :acl/role-revoked {:user user-id :role role-id})
  (events/publish! :acl/role-revoked {:user user-id :role role-id}))

(defn get-user-roles
  "Get all roles for a user."
  [user-id]
  (get-in @acl-state [:users user-id :roles] #{}))

(defn get-user-permissions
  "Get all permissions for a user (from all roles)."
  [user-id]
  (let [roles (get-user-roles user-id)]
    (reduce (fn [perms role-id]
              (set/union perms (get-role-permissions role-id)))
            #{}
            roles)))

;; =============================================================================
;; PERMISSION CHECKING
;; =============================================================================

(defn has-permission?
  "Check if a user has a specific permission."
  [user-id permission]
  (when (flags/is-enabled? "access-control")
    (let [user-perms (get-user-permissions user-id)
          has-perm (contains? user-perms permission)]
      (metrics/inc-counter! :acl/permission-checks)
      (when-not has-perm
        (metrics/inc-counter! :acl/permission-denied))
      has-perm)))

(defn has-any-permission?
  "Check if a user has any of the specified permissions."
  [user-id permissions]
  (some #(has-permission? user-id %) permissions))

(defn has-all-permissions?
  "Check if a user has all of the specified permissions."
  [user-id permissions]
  (every? #(has-permission? user-id %) permissions))

(defn require-permission!
  "Require a permission, throwing if not authorized."
  [user-id permission]
  (when-not (has-permission? user-id permission)
    (log/warn "Permission denied" {:user user-id :permission permission})
    (audit/log-event! :acl/access-denied {:user user-id :permission permission})
    (throw (ex-info "Permission denied"
                    {:type :unauthorized
                     :user user-id
                     :permission permission}))))

;; =============================================================================
;; RESOURCE-LEVEL PERMISSIONS
;; =============================================================================

(defn grant-resource-access!
  "Grant access to a specific resource."
  [user-id resource-type resource-id permissions]
  (log/info "Granting resource access" {:user user-id :resource-type resource-type
                                        :resource-id resource-id})
  (swap! acl-state assoc-in [:resource-permissions [resource-type resource-id] user-id]
         (set permissions))
  (audit/log-event! :acl/resource-access-granted
                    {:user user-id :resource-type resource-type
                     :resource-id resource-id :permissions permissions}))

(defn revoke-resource-access!
  "Revoke access to a specific resource."
  [user-id resource-type resource-id]
  (log/info "Revoking resource access" {:user user-id :resource-type resource-type
                                        :resource-id resource-id})
  (swap! acl-state update-in [:resource-permissions [resource-type resource-id]]
         dissoc user-id)
  (audit/log-event! :acl/resource-access-revoked
                    {:user user-id :resource-type resource-type :resource-id resource-id}))

(defn has-resource-permission?
  "Check if a user has permission on a specific resource."
  [user-id resource-type resource-id permission]
  (let [resource-perms (get-in @acl-state
                               [:resource-permissions [resource-type resource-id] user-id]
                               #{})
        global-perms (get-user-permissions user-id)]
    (or (contains? resource-perms permission)
        (contains? global-perms permission))))

;; =============================================================================
;; DEFAULT ROLES
;; =============================================================================

(def default-roles
  "Default system roles."
  {:viewer {:permissions #{:analysis/read :models/read :reports/read :alerts/read}
            :description "Read-only access to analyses and reports"}
   :analyst {:permissions #{:analysis/read :analysis/create :models/read
                            :reports/read :reports/create :reports/export
                            :alerts/read :alerts/acknowledge}
             :description "Can create and view analyses"
             :inherits :viewer}
   :editor {:permissions #{:analysis/update :models/create :models/update
                           :alerts/configure}
            :description "Can edit analyses and models"
            :inherits :analyst}
   :admin {:permissions #{:analysis/delete :models/delete :config/read :config/update
                          :users/read :users/create :users/update :users/delete
                          :admin/system :admin/audit}
           :description "Full administrative access"
           :inherits :editor}})

;; =============================================================================
;; MIDDLEWARE
;; =============================================================================

(defn wrap-authorization
  "Ring middleware for authorization."
  [handler required-permission]
  (fn [request]
    (let [user-id (get-in request [:session :user-id])]
      (if (has-permission? user-id required-permission)
        (handler request)
        {:status 403
         :body {:error "Forbidden"
                :message "You do not have permission to access this resource"}}))))

(defn wrap-resource-authorization
  "Ring middleware for resource-level authorization."
  [handler resource-type id-param required-permission]
  (fn [request]
    (let [user-id (get-in request [:session :user-id])
          resource-id (get-in request [:params id-param])]
      (if (has-resource-permission? user-id resource-type resource-id required-permission)
        (handler request)
        {:status 403
         :body {:error "Forbidden"
                :message "You do not have permission to access this resource"}}))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-access-control!
  "Initialize access control system."
  []
  (log/info "Initializing access control")
  ;; Register feature flag
  (flags/register-flag! "access-control" "Enable access control" true)
  ;; Create metrics
  (metrics/create-counter! :acl/roles-defined "Roles defined")
  (metrics/create-counter! :acl/permission-checks "Permission checks")
  (metrics/create-counter! :acl/permission-denied "Permission denied")
  (metrics/create-gauge! :acl/total-roles "Total roles"
                         #(count (:roles @acl-state)))
  (metrics/create-gauge! :acl/total-users "Total users with roles"
                         #(count (:users @acl-state)))
  ;; Register default roles
  (doseq [[role-id {:keys [permissions description inherits]}] default-roles]
    (define-role! role-id permissions :description description :inherits inherits))
  (log/info "Access control initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-acl-status []
  {:enabled (flags/is-enabled? "access-control")
   :total-roles (count (:roles @acl-state))
   :total-users (count (:users @acl-state))
   :total-resource-permissions (count (:resource-permissions @acl-state))
   :roles (into {} (map (fn [[k v]]
                          [k {:permissions (count (:permissions v))
                              :inherits (:inherits v)}])
                        (:roles @acl-state)))})
