(ns mental-models.pipeline.integration.permission-manager
  "Permission Manager Module
   
   Fine-grained permission management:
   - Resource-level permissions
   - Action-based access control
   - Permission inheritance
   - Permission caching
   - Audit logging"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; PERMISSION MANAGER STATE
;; =============================================================================

(defonce permission-state (atom {:permissions (ConcurrentHashMap.)
                                  :grants (ConcurrentHashMap.)
                                  :policies (ConcurrentHashMap.)
                                  :cache (ConcurrentHashMap.)
                                  :audit-log (ConcurrentHashMap.)
                                  :check-count (AtomicLong. 0)
                                  :grant-count (AtomicLong. 0)
                                  :config {:cache-ttl-ms 60000
                                           :max-cache-size 10000
                                           :audit-enabled true}}))

;; =============================================================================
;; PERMISSION DEFINITIONS
;; =============================================================================

(defn define-permission!
  "Define a permission."
  [permission-id {:keys [name description resource action implies]}]
  (log/info "Defining permission" {:id permission-id})
  (.put ^ConcurrentHashMap (:permissions @permission-state) permission-id
        {:id permission-id
         :name name
         :description description
         :resource resource
         :action action
         :implies (or implies #{})
         :created-at (System/currentTimeMillis)}))

(defn undefine-permission!
  "Remove a permission definition."
  [permission-id]
  (.remove ^ConcurrentHashMap (:permissions @permission-state) permission-id))

(defn get-permission
  "Get a permission by ID."
  [permission-id]
  (.get ^ConcurrentHashMap (:permissions @permission-state) permission-id))

(defn list-permissions
  "List all permissions."
  [& {:keys [resource action]}]
  (let [perms (vals (:permissions @permission-state))]
    (cond->> perms
      resource (filter #(= (:resource %) resource))
      action (filter #(= (:action %) action)))))

;; =============================================================================
;; PERMISSION GRANTS
;; =============================================================================

(defn grant-key
  "Generate a grant key."
  [subject-type subject-id]
  (str subject-type ":" subject-id))

(defn grant!
  "Grant a permission to a subject."
  [subject-type subject-id permission-id & {:keys [resource-id conditions expires-at]}]
  (.incrementAndGet ^AtomicLong (:grant-count @permission-state))
  (metrics/inc-counter! :permissionmanager/grants)
  (let [key (grant-key subject-type subject-id)
        grants (or (.get ^ConcurrentHashMap (:grants @permission-state) key) #{})
        grant {:permission-id permission-id
               :resource-id resource-id
               :conditions (or conditions {})
               :expires-at expires-at
               :granted-at (System/currentTimeMillis)}]
    (.put ^ConcurrentHashMap (:grants @permission-state) key (conj grants grant))
    ;; Invalidate cache
    (invalidate-cache! subject-type subject-id)
    ;; Audit log
    (when (get-in @permission-state [:config :audit-enabled])
      (audit-log! :grant {:subject-type subject-type
                          :subject-id subject-id
                          :permission-id permission-id
                          :resource-id resource-id}))
    (log/info "Permission granted" {:subject subject-id :permission permission-id})
    grant))

(defn revoke!
  "Revoke a permission from a subject."
  [subject-type subject-id permission-id & {:keys [resource-id]}]
  (let [key (grant-key subject-type subject-id)
        grants (or (.get ^ConcurrentHashMap (:grants @permission-state) key) #{})]
    (.put ^ConcurrentHashMap (:grants @permission-state) key
          (set (remove (fn [g]
                         (and (= (:permission-id g) permission-id)
                              (or (nil? resource-id)
                                  (= (:resource-id g) resource-id))))
                       grants)))
    ;; Invalidate cache
    (invalidate-cache! subject-type subject-id)
    ;; Audit log
    (when (get-in @permission-state [:config :audit-enabled])
      (audit-log! :revoke {:subject-type subject-type
                           :subject-id subject-id
                           :permission-id permission-id
                           :resource-id resource-id}))
    (log/info "Permission revoked" {:subject subject-id :permission permission-id})))

(defn get-grants
  "Get all grants for a subject."
  [subject-type subject-id]
  (let [key (grant-key subject-type subject-id)]
    (or (.get ^ConcurrentHashMap (:grants @permission-state) key) #{})))

(defn clear-grants!
  "Clear all grants for a subject."
  [subject-type subject-id]
  (let [key (grant-key subject-type subject-id)]
    (.remove ^ConcurrentHashMap (:grants @permission-state) key)
    (invalidate-cache! subject-type subject-id)))

;; =============================================================================
;; PERMISSION POLICIES
;; =============================================================================

(defn create-policy!
  "Create a permission policy."
  [policy-id {:keys [name description rules priority]}]
  (log/info "Creating policy" {:id policy-id})
  (.put ^ConcurrentHashMap (:policies @permission-state) policy-id
        {:id policy-id
         :name name
         :description description
         :rules (or rules [])
         :priority (or priority 0)
         :enabled true
         :created-at (System/currentTimeMillis)}))

(defn delete-policy!
  "Delete a policy."
  [policy-id]
  (.remove ^ConcurrentHashMap (:policies @permission-state) policy-id))

(defn get-policy
  "Get a policy by ID."
  [policy-id]
  (.get ^ConcurrentHashMap (:policies @permission-state) policy-id))

(defn enable-policy!
  "Enable a policy."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    (.put ^ConcurrentHashMap (:policies @permission-state) policy-id
          (assoc policy :enabled true))))

(defn disable-policy!
  "Disable a policy."
  [policy-id]
  (when-let [policy (get-policy policy-id)]
    (.put ^ConcurrentHashMap (:policies @permission-state) policy-id
          (assoc policy :enabled false))))

(defn evaluate-policy-rule
  "Evaluate a single policy rule."
  [rule context]
  (let [{:keys [effect condition]} rule]
    (if (or (nil? condition) (condition context))
      effect
      nil)))

(defn evaluate-policies
  "Evaluate all policies for a permission check."
  [permission-id resource-id context]
  (let [policies (sort-by :priority > (filter :enabled (vals (:policies @permission-state))))]
    (loop [policies policies]
      (if (empty? policies)
        nil
        (let [policy (first policies)
              applicable-rules (filter #(and (= (:permission-id %) permission-id)
                                             (or (nil? (:resource-id %))
                                                 (= (:resource-id %) resource-id)))
                                       (:rules policy))
              effects (keep #(evaluate-policy-rule % context) applicable-rules)]
          (cond
            (some #{:deny} effects) :deny
            (some #{:allow} effects) :allow
            :else (recur (rest policies))))))))

;; =============================================================================
;; PERMISSION CACHING
;; =============================================================================

(defn cache-key
  "Generate a cache key."
  [subject-type subject-id permission-id resource-id]
  (str subject-type ":" subject-id ":" permission-id ":" resource-id))

(defn get-cached
  "Get a cached permission check result."
  [subject-type subject-id permission-id resource-id]
  (let [key (cache-key subject-type subject-id permission-id resource-id)
        cached (.get ^ConcurrentHashMap (:cache @permission-state) key)]
    (when (and cached
               (< (- (System/currentTimeMillis) (:cached-at cached))
                  (get-in @permission-state [:config :cache-ttl-ms])))
      (:result cached))))

(defn cache-result!
  "Cache a permission check result."
  [subject-type subject-id permission-id resource-id result]
  (let [key (cache-key subject-type subject-id permission-id resource-id)
        cache ^ConcurrentHashMap (:cache @permission-state)
        max-size (get-in @permission-state [:config :max-cache-size])]
    (when (>= (.size cache) max-size)
      ;; Simple eviction - clear half the cache
      (let [keys (take (/ max-size 2) (.keySet cache))]
        (doseq [k keys]
          (.remove cache k))))
    (.put cache key {:result result :cached-at (System/currentTimeMillis)})))

(defn invalidate-cache!
  "Invalidate cache for a subject."
  [subject-type subject-id]
  (let [prefix (str subject-type ":" subject-id ":")]
    (doseq [k (.keySet ^ConcurrentHashMap (:cache @permission-state))]
      (when (str/starts-with? k prefix)
        (.remove ^ConcurrentHashMap (:cache @permission-state) k)))))

(defn clear-cache!
  "Clear the entire cache."
  []
  (.clear ^ConcurrentHashMap (:cache @permission-state)))

;; =============================================================================
;; PERMISSION CHECKING
;; =============================================================================

(defn expand-permissions
  "Expand permissions to include implied permissions."
  [permission-ids]
  (loop [to-expand permission-ids
         expanded #{}]
    (if (empty? to-expand)
      expanded
      (let [perm-id (first to-expand)
            perm (get-permission perm-id)
            implied (or (:implies perm) #{})]
        (recur (into (rest to-expand) (set/difference implied expanded))
               (conj expanded perm-id))))))

(defn check-grant
  "Check if a grant allows a permission."
  [grant permission-id resource-id]
  (let [now (System/currentTimeMillis)]
    (and (= (:permission-id grant) permission-id)
         (or (nil? (:resource-id grant))
             (= (:resource-id grant) resource-id))
         (or (nil? (:expires-at grant))
             (> (:expires-at grant) now)))))

(defn has-permission?
  "Check if a subject has a permission."
  [subject-type subject-id permission-id & {:keys [resource-id context]}]
  (.incrementAndGet ^AtomicLong (:check-count @permission-state))
  (metrics/inc-counter! :permissionmanager/checks)
  ;; Check cache first
  (if-let [cached (get-cached subject-type subject-id permission-id resource-id)]
    cached
    (let [;; Check policies first
          policy-result (evaluate-policies permission-id resource-id
                                           (merge context {:subject-type subject-type
                                                           :subject-id subject-id}))
          result (cond
                   (= policy-result :deny) false
                   (= policy-result :allow) true
                   :else
                   ;; Check grants
                   (let [grants (get-grants subject-type subject-id)
                         expanded-perms (expand-permissions #{permission-id})]
                     (some (fn [perm-id]
                             (some #(check-grant % perm-id resource-id) grants))
                           expanded-perms)))]
      ;; Cache result
      (cache-result! subject-type subject-id permission-id resource-id result)
      ;; Audit log
      (when (get-in @permission-state [:config :audit-enabled])
        (audit-log! :check {:subject-type subject-type
                            :subject-id subject-id
                            :permission-id permission-id
                            :resource-id resource-id
                            :result result}))
      result)))

(defn require-permission!
  "Require a permission, throwing if not granted."
  [subject-type subject-id permission-id & {:keys [resource-id context]}]
  (when-not (has-permission? subject-type subject-id permission-id
                             :resource-id resource-id
                             :context context)
    (throw (ex-info "Permission denied"
                    {:subject-type subject-type
                     :subject-id subject-id
                     :permission-id permission-id
                     :resource-id resource-id}))))

;; =============================================================================
;; AUDIT LOGGING
;; =============================================================================

(defn audit-log!
  "Record an audit log entry."
  [action details]
  (let [entry-id (str (System/currentTimeMillis) "-" (rand-int 10000))]
    (.put ^ConcurrentHashMap (:audit-log @permission-state) entry-id
          {:id entry-id
           :action action
           :details details
           :timestamp (System/currentTimeMillis)})))

(defn get-audit-log
  "Get audit log entries."
  [& {:keys [action subject-id limit since]}]
  (let [entries (vals (:audit-log @permission-state))]
    (cond->> entries
      action (filter #(= (:action %) action))
      subject-id (filter #(= (get-in % [:details :subject-id]) subject-id))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

(defn clear-audit-log!
  "Clear audit log entries."
  [& {:keys [before]}]
  (if before
    (doseq [[k v] (:audit-log @permission-state)]
      (when (< (:timestamp v) before)
        (.remove ^ConcurrentHashMap (:audit-log @permission-state) k)))
    (.clear ^ConcurrentHashMap (:audit-log @permission-state))))

;; =============================================================================
;; BUILT-IN PERMISSIONS
;; =============================================================================

(defn init-built-in-permissions!
  "Initialize built-in permissions."
  []
  ;; CRUD permissions
  (define-permission! :create {:name "Create" :description "Create resources" :action :create})
  (define-permission! :read {:name "Read" :description "Read resources" :action :read})
  (define-permission! :update {:name "Update" :description "Update resources" :action :update})
  (define-permission! :delete {:name "Delete" :description "Delete resources" :action :delete})
  ;; Admin permission (implies all CRUD)
  (define-permission! :admin {:name "Admin" :description "Full admin access"
                              :action :admin
                              :implies #{:create :read :update :delete}})
  ;; Mental model specific permissions
  (define-permission! :analyze {:name "Analyze" :description "Run analysis" :action :analyze})
  (define-permission! :export {:name "Export" :description "Export data" :action :export})
  (define-permission! :configure {:name "Configure" :description "Configure system" :action :configure}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-permission-stats
  "Get permission manager statistics."
  []
  {:permissions (.size ^ConcurrentHashMap (:permissions @permission-state))
   :grants (.size ^ConcurrentHashMap (:grants @permission-state))
   :policies (.size ^ConcurrentHashMap (:policies @permission-state))
   :cache-size (.size ^ConcurrentHashMap (:cache @permission-state))
   :audit-entries (.size ^ConcurrentHashMap (:audit-log @permission-state))
   :check-count (.get ^AtomicLong (:check-count @permission-state))
   :grant-count (.get ^AtomicLong (:grant-count @permission-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-permission-manager!
  "Initialize permission manager."
  []
  (log/info "Initializing permission manager")
  ;; Register feature flag
  (flags/register-flag! "permission-manager" "Enable permission manager" true)
  ;; Create metrics
  (metrics/create-counter! :permissionmanager/checks "Permission checks")
  (metrics/create-counter! :permissionmanager/grants "Permission grants")
  (metrics/create-gauge! :permissionmanager/cache-size "Cache size"
                         #(.size ^ConcurrentHashMap (:cache @permission-state)))
  ;; Initialize built-in permissions
  (init-built-in-permissions!)
  (log/info "Permission manager initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-permission-manager-status []
  {:enabled (flags/is-enabled? "permission-manager")
   :permissions (.size ^ConcurrentHashMap (:permissions @permission-state))
   :grants (.size ^ConcurrentHashMap (:grants @permission-state))
   :policies (.size ^ConcurrentHashMap (:policies @permission-state))
   :cache-size (.size ^ConcurrentHashMap (:cache @permission-state))
   :stats (get-permission-stats)
   :config (:config @permission-state)})
