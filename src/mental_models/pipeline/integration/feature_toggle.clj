(ns mental-models.pipeline.integration.feature-toggle
  "Feature Toggle Module
   
   Advanced feature management:
   - Toggle types (release, experiment, ops, permission)
   - Gradual rollout
   - User targeting
   - A/B testing
   - Toggle lifecycle"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; TOGGLE STATE
;; =============================================================================

(defonce toggle-state (atom {:toggles {}
                             :overrides {}
                             :experiments {}
                             :user-assignments {}
                             :config {:default-enabled false
                                      :sticky-experiments true}}))

;; =============================================================================
;; TOGGLE TYPES
;; =============================================================================

(def toggle-types
  #{:release      ;; Feature release toggles
    :experiment   ;; A/B testing toggles
    :ops          ;; Operational toggles
    :permission   ;; Permission-based toggles
    :kill-switch  ;; Emergency kill switches
    })

;; =============================================================================
;; TOGGLE REGISTRATION
;; =============================================================================

(defn register-toggle!
  "Register a feature toggle."
  [toggle-id {:keys [type description enabled? rollout-percentage
                     user-groups start-date end-date variants]}]
  (log/info "Registering toggle" {:id toggle-id :type type})
  (swap! toggle-state assoc-in [:toggles toggle-id]
         {:id toggle-id
          :type (or type :release)
          :description description
          :enabled? (if (nil? enabled?) false enabled?)
          :rollout-percentage (or rollout-percentage 100)
          :user-groups (or user-groups #{})
          :start-date start-date
          :end-date end-date
          :variants variants
          :created-at (System/currentTimeMillis)
          :updated-at (System/currentTimeMillis)})
  (metrics/inc-counter! :toggle/toggles-registered)
  toggle-id)

(defn unregister-toggle!
  "Unregister a feature toggle."
  [toggle-id]
  (log/info "Unregistering toggle" {:id toggle-id})
  (swap! toggle-state update :toggles dissoc toggle-id))

(defn get-toggle
  "Get a feature toggle."
  [toggle-id]
  (get-in @toggle-state [:toggles toggle-id]))

(defn list-toggles
  "List all feature toggles."
  [& {:keys [type enabled?]}]
  (let [toggles (vals (:toggles @toggle-state))]
    (cond->> toggles
      type (filter #(= (:type %) type))
      (some? enabled?) (filter #(= (:enabled? %) enabled?)))))

;; =============================================================================
;; TOGGLE OPERATIONS
;; =============================================================================

(defn enable-toggle!
  "Enable a feature toggle."
  [toggle-id]
  (log/info "Enabling toggle" {:id toggle-id})
  (swap! toggle-state update-in [:toggles toggle-id]
         merge {:enabled? true :updated-at (System/currentTimeMillis)})
  (metrics/inc-counter! :toggle/toggles-enabled)
  (events/publish! :toggle/enabled {:toggle-id toggle-id}))

(defn disable-toggle!
  "Disable a feature toggle."
  [toggle-id]
  (log/info "Disabling toggle" {:id toggle-id})
  (swap! toggle-state update-in [:toggles toggle-id]
         merge {:enabled? false :updated-at (System/currentTimeMillis)})
  (metrics/inc-counter! :toggle/toggles-disabled)
  (events/publish! :toggle/disabled {:toggle-id toggle-id}))

(defn set-rollout-percentage!
  "Set the rollout percentage for a toggle."
  [toggle-id percentage]
  (log/info "Setting rollout percentage" {:id toggle-id :percentage percentage})
  (swap! toggle-state update-in [:toggles toggle-id]
         merge {:rollout-percentage (max 0 (min 100 percentage))
                :updated-at (System/currentTimeMillis)}))

(defn add-user-group!
  "Add a user group to a toggle."
  [toggle-id group]
  (swap! toggle-state update-in [:toggles toggle-id :user-groups] conj group))

(defn remove-user-group!
  "Remove a user group from a toggle."
  [toggle-id group]
  (swap! toggle-state update-in [:toggles toggle-id :user-groups] disj group))

;; =============================================================================
;; OVERRIDES
;; =============================================================================

(defn set-override!
  "Set an override for a toggle."
  [toggle-id user-id enabled?]
  (log/debug "Setting override" {:toggle toggle-id :user user-id :enabled enabled?})
  (swap! toggle-state assoc-in [:overrides toggle-id user-id] enabled?))

(defn clear-override!
  "Clear an override for a toggle."
  [toggle-id user-id]
  (swap! toggle-state update-in [:overrides toggle-id] dissoc user-id))

(defn get-override
  "Get an override for a toggle."
  [toggle-id user-id]
  (get-in @toggle-state [:overrides toggle-id user-id]))

(defn clear-all-overrides!
  "Clear all overrides for a toggle."
  [toggle-id]
  (swap! toggle-state update :overrides dissoc toggle-id))

;; =============================================================================
;; TOGGLE EVALUATION
;; =============================================================================

(defn user-hash
  "Generate a consistent hash for a user."
  [user-id toggle-id]
  (mod (Math/abs (hash (str user-id toggle-id))) 100))

(defn in-rollout?
  "Check if a user is in the rollout percentage."
  [user-id toggle-id rollout-percentage]
  (< (user-hash user-id toggle-id) rollout-percentage))

(defn in-date-range?
  "Check if current time is within the toggle's date range."
  [toggle]
  (let [now (System/currentTimeMillis)
        start (:start-date toggle)
        end (:end-date toggle)]
    (and (or (nil? start) (>= now start))
         (or (nil? end) (<= now end)))))

(defn in-user-group?
  "Check if a user is in any of the toggle's user groups."
  [user-groups user-id user-context]
  (or (empty? user-groups)
      (contains? user-groups user-id)
      (some #(contains? user-groups %) (:groups user-context))))

(defn is-enabled?
  "Check if a toggle is enabled for a user."
  [toggle-id & {:keys [user-id user-context]}]
  (when-let [toggle (get-toggle toggle-id)]
    ;; Check override first
    (if-let [override (get-override toggle-id user-id)]
      override
      ;; Check toggle conditions
      (and (:enabled? toggle)
           (in-date-range? toggle)
           (or (nil? user-id)
               (and (in-user-group? (:user-groups toggle) user-id user-context)
                    (in-rollout? user-id toggle-id (:rollout-percentage toggle))))))))

(defn is-disabled?
  "Check if a toggle is disabled for a user."
  [toggle-id & opts]
  (not (apply is-enabled? toggle-id opts)))

;; =============================================================================
;; A/B TESTING
;; =============================================================================

(defn register-experiment!
  "Register an A/B experiment."
  [experiment-id {:keys [toggle-id variants weights]}]
  (log/info "Registering experiment" {:id experiment-id :variants variants})
  (let [total-weight (reduce + weights)
        normalized-weights (map #(/ % total-weight) weights)]
    (swap! toggle-state assoc-in [:experiments experiment-id]
           {:id experiment-id
            :toggle-id toggle-id
            :variants variants
            :weights normalized-weights
            :created-at (System/currentTimeMillis)})))

(defn get-variant
  "Get the variant for a user in an experiment."
  [experiment-id user-id]
  (when-let [experiment (get-in @toggle-state [:experiments experiment-id])]
    ;; Check for sticky assignment
    (or (get-in @toggle-state [:user-assignments experiment-id user-id])
        ;; Assign based on hash
        (let [hash-val (/ (user-hash user-id experiment-id) 100.0)
              weights (:weights experiment)
              variants (:variants experiment)]
          (loop [cumulative 0
                 ws weights
                 vs variants]
            (if (empty? ws)
              (last variants)
              (let [new-cumulative (+ cumulative (first ws))]
                (if (< hash-val new-cumulative)
                  (let [variant (first vs)]
                    ;; Store sticky assignment
                    (when (get-in @toggle-state [:config :sticky-experiments])
                      (swap! toggle-state assoc-in [:user-assignments experiment-id user-id] variant))
                    variant)
                  (recur new-cumulative (rest ws) (rest vs))))))))))

(defn record-experiment-event!
  "Record an event for an experiment."
  [experiment-id user-id event-type & {:keys [value]}]
  (let [variant (get-variant experiment-id user-id)]
    (metrics/inc-counter! (keyword (str "experiment/" experiment-id "/" variant "/" event-type)))
    (events/publish! :toggle/experiment-event {:experiment experiment-id
                                                :user user-id
                                                :variant variant
                                                :event event-type
                                                :value value})))

;; =============================================================================
;; KILL SWITCHES
;; =============================================================================

(defn register-kill-switch!
  "Register a kill switch."
  [switch-id description]
  (register-toggle! switch-id {:type :kill-switch
                               :description description
                               :enabled? false}))

(defn activate-kill-switch!
  "Activate a kill switch (disables the feature)."
  [switch-id]
  (log/warn "Activating kill switch" {:id switch-id})
  (enable-toggle! switch-id)
  (events/publish! :toggle/kill-switch-activated {:switch-id switch-id}))

(defn deactivate-kill-switch!
  "Deactivate a kill switch (enables the feature)."
  [switch-id]
  (log/info "Deactivating kill switch" {:id switch-id})
  (disable-toggle! switch-id)
  (events/publish! :toggle/kill-switch-deactivated {:switch-id switch-id}))

(defn is-killed?
  "Check if a kill switch is active."
  [switch-id]
  (is-enabled? switch-id))

;; =============================================================================
;; TOGGLE MACROS
;; =============================================================================

(defmacro when-enabled
  "Execute body when toggle is enabled."
  [toggle-id & body]
  `(when (is-enabled? ~toggle-id)
     ~@body))

(defmacro when-disabled
  "Execute body when toggle is disabled."
  [toggle-id & body]
  `(when (is-disabled? ~toggle-id)
     ~@body))

(defmacro if-enabled
  "Execute then-body when enabled, else-body when disabled."
  [toggle-id then-body else-body]
  `(if (is-enabled? ~toggle-id)
     ~then-body
     ~else-body))

;; =============================================================================
;; GRADUAL ROLLOUT
;; =============================================================================

(defn start-gradual-rollout!
  "Start a gradual rollout for a toggle."
  [toggle-id {:keys [start-percentage end-percentage duration-ms step-ms]}]
  (log/info "Starting gradual rollout" {:toggle toggle-id})
  (let [steps (/ duration-ms step-ms)
        increment (/ (- end-percentage start-percentage) steps)]
    (future
      (loop [current start-percentage]
        (when (<= current end-percentage)
          (set-rollout-percentage! toggle-id (int current))
          (Thread/sleep step-ms)
          (recur (+ current increment)))))))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-feature-toggle!
  "Initialize feature toggle."
  []
  (log/info "Initializing feature toggle")
  ;; Register feature flag
  (flags/register-flag! "feature-toggle" "Enable feature toggle" true)
  ;; Create metrics
  (metrics/create-counter! :toggle/toggles-registered "Toggles registered")
  (metrics/create-counter! :toggle/toggles-enabled "Toggles enabled")
  (metrics/create-counter! :toggle/toggles-disabled "Toggles disabled")
  (metrics/create-gauge! :toggle/active-toggles "Active toggles"
                         #(count (filter :enabled? (vals (:toggles @toggle-state)))))
  (metrics/create-gauge! :toggle/experiments "Experiments"
                         #(count (:experiments @toggle-state)))
  (log/info "Feature toggle initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-toggle-status []
  {:enabled (flags/is-enabled? "feature-toggle")
   :toggles (count (:toggles @toggle-state))
   :active (count (filter :enabled? (vals (:toggles @toggle-state))))
   :experiments (count (:experiments @toggle-state))
   :overrides (reduce + (map count (vals (:overrides @toggle-state))))
   :user-assignments (reduce + (map count (vals (:user-assignments @toggle-state))))
   :by-type (frequencies (map :type (vals (:toggles @toggle-state))))
   :config (:config @toggle-state)})
