(ns mental-models.pipeline.integration.session-manager
  "Session Manager Module
   
   User session management:
   - Session creation and tracking
   - Session expiration
   - Session data storage
   - Multi-device support
   - Session analytics"
  (:require
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; SESSION STATE
;; =============================================================================

(defonce session-state (atom {:sessions {}
                              :user-sessions {}
                              :config {:default-ttl-ms 3600000
                                       :max-sessions-per-user 5
                                       :cleanup-interval-ms 60000}
                              :stats {:created 0
                                      :expired 0
                                      :destroyed 0}}))

;; =============================================================================
;; SESSION CREATION
;; =============================================================================

(defn generate-session-id []
  (str (java.util.UUID/randomUUID)))

(defn create-session!
  "Create a new session."
  [user-id & {:keys [ttl-ms metadata device-info]
              :or {metadata {} device-info {}}}]
  (when (flags/is-enabled? "session-manager")
    (let [session-id (generate-session-id)
          ttl (or ttl-ms (get-in @session-state [:config :default-ttl-ms]))
          now (System/currentTimeMillis)
          session {:id session-id
                   :user-id user-id
                   :created-at now
                   :last-activity now
                   :expires-at (+ now ttl)
                   :ttl-ms ttl
                   :metadata metadata
                   :device-info device-info
                   :data {}
                   :active true}]
      (log/info "Creating session" {:session-id session-id :user-id user-id})
      ;; Check max sessions per user
      (let [user-sessions (get-in @session-state [:user-sessions user-id] #{})
            max-sessions (get-in @session-state [:config :max-sessions-per-user])]
        (when (>= (count user-sessions) max-sessions)
          ;; Remove oldest session
          (let [oldest (first (sort-by #(:created-at (get-in @session-state [:sessions %])) user-sessions))]
            (destroy-session! oldest))))
      ;; Store session
      (swap! session-state assoc-in [:sessions session-id] session)
      (swap! session-state update-in [:user-sessions user-id] (fnil conj #{}) session-id)
      (swap! session-state update-in [:stats :created] inc)
      ;; Record metrics
      (metrics/inc-counter! :session/created)
      ;; Publish event
      (events/publish! :session/created {:session-id session-id :user-id user-id})
      session)))

;; =============================================================================
;; SESSION ACCESS
;; =============================================================================

(defn get-session
  "Get a session by ID."
  [session-id]
  (get-in @session-state [:sessions session-id]))

(defn get-user-sessions
  "Get all sessions for a user."
  [user-id]
  (let [session-ids (get-in @session-state [:user-sessions user-id] #{})]
    (map get-session session-ids)))

(defn session-valid?
  "Check if a session is valid and not expired."
  [session-id]
  (when-let [session (get-session session-id)]
    (and (:active session)
         (> (:expires-at session) (System/currentTimeMillis)))))

(defn touch-session!
  "Update session last activity and extend expiration."
  [session-id]
  (when-let [session (get-session session-id)]
    (when (session-valid? session-id)
      (let [now (System/currentTimeMillis)
            new-expires (+ now (:ttl-ms session))]
        (swap! session-state update-in [:sessions session-id]
               assoc :last-activity now :expires-at new-expires)
        (metrics/inc-counter! :session/touched)
        true))))

;; =============================================================================
;; SESSION DATA
;; =============================================================================

(defn get-session-data
  "Get data from a session."
  [session-id key]
  (get-in @session-state [:sessions session-id :data key]))

(defn set-session-data!
  "Set data in a session."
  [session-id key value]
  (when (session-valid? session-id)
    (swap! session-state assoc-in [:sessions session-id :data key] value)
    (touch-session! session-id)
    value))

(defn remove-session-data!
  "Remove data from a session."
  [session-id key]
  (when (session-valid? session-id)
    (swap! session-state update-in [:sessions session-id :data] dissoc key)
    (touch-session! session-id)))

(defn get-all-session-data
  "Get all data from a session."
  [session-id]
  (get-in @session-state [:sessions session-id :data]))

(defn clear-session-data!
  "Clear all data from a session."
  [session-id]
  (when (session-valid? session-id)
    (swap! session-state assoc-in [:sessions session-id :data] {})
    (touch-session! session-id)))

;; =============================================================================
;; SESSION DESTRUCTION
;; =============================================================================

(defn destroy-session!
  "Destroy a session."
  [session-id]
  (when-let [session (get-session session-id)]
    (log/info "Destroying session" {:session-id session-id :user-id (:user-id session)})
    ;; Remove from sessions
    (swap! session-state update :sessions dissoc session-id)
    ;; Remove from user sessions
    (swap! session-state update-in [:user-sessions (:user-id session)] disj session-id)
    (swap! session-state update-in [:stats :destroyed] inc)
    ;; Record metrics
    (metrics/inc-counter! :session/destroyed)
    ;; Publish event
    (events/publish! :session/destroyed {:session-id session-id :user-id (:user-id session)})
    true))

(defn destroy-user-sessions!
  "Destroy all sessions for a user."
  [user-id]
  (log/info "Destroying all sessions for user" {:user-id user-id})
  (let [session-ids (get-in @session-state [:user-sessions user-id] #{})]
    (doseq [session-id session-ids]
      (destroy-session! session-id))))

(defn invalidate-session!
  "Invalidate a session without destroying it."
  [session-id]
  (when (get-session session-id)
    (log/info "Invalidating session" {:session-id session-id})
    (swap! session-state assoc-in [:sessions session-id :active] false)
    (events/publish! :session/invalidated {:session-id session-id})
    true))

;; =============================================================================
;; SESSION EXPIRATION
;; =============================================================================

(defn cleanup-expired-sessions!
  "Remove all expired sessions."
  []
  (let [now (System/currentTimeMillis)
        sessions (:sessions @session-state)
        expired (filter (fn [[_ session]]
                          (< (:expires-at session) now))
                        sessions)]
    (when (seq expired)
      (log/info "Cleaning up expired sessions" {:count (count expired)})
      (doseq [[session-id _] expired]
        (destroy-session! session-id)
        (swap! session-state update-in [:stats :expired] inc)
        (metrics/inc-counter! :session/expired)))))

(defonce cleanup-scheduler (atom nil))

(defn start-cleanup-scheduler!
  "Start the session cleanup scheduler."
  []
  (log/info "Starting session cleanup scheduler")
  (let [interval (get-in @session-state [:config :cleanup-interval-ms])
        control-chan (clojure.core.async/chan)]
    (clojure.core.async/go-loop []
      (let [[v ch] (clojure.core.async/alts!
                    [(clojure.core.async/timeout interval) control-chan])]
        (when-not (= ch control-chan)
          (cleanup-expired-sessions!)
          (recur))))
    (reset! cleanup-scheduler control-chan)
    control-chan))

(defn stop-cleanup-scheduler!
  "Stop the session cleanup scheduler."
  []
  (when @cleanup-scheduler
    (log/info "Stopping session cleanup scheduler")
    (clojure.core.async/close! @cleanup-scheduler)
    (reset! cleanup-scheduler nil)))

;; =============================================================================
;; SESSION ANALYTICS
;; =============================================================================

(defn get-active-session-count
  "Get count of active sessions."
  []
  (count (filter (fn [[_ session]]
                   (and (:active session)
                        (> (:expires-at session) (System/currentTimeMillis))))
                 (:sessions @session-state))))

(defn get-sessions-by-device
  "Get sessions grouped by device type."
  []
  (let [sessions (vals (:sessions @session-state))]
    (group-by #(get-in % [:device-info :type] :unknown) sessions)))

(defn get-session-duration
  "Get the duration of a session."
  [session-id]
  (when-let [session (get-session session-id)]
    (- (or (:last-activity session) (System/currentTimeMillis))
       (:created-at session))))

(defn get-average-session-duration
  "Get average session duration."
  []
  (let [sessions (vals (:sessions @session-state))
        durations (map #(- (or (:last-activity %) (System/currentTimeMillis))
                           (:created-at %))
                       sessions)]
    (if (seq durations)
      (/ (reduce + durations) (count durations))
      0)))

(defn get-session-analytics
  "Get comprehensive session analytics."
  []
  {:total-sessions (count (:sessions @session-state))
   :active-sessions (get-active-session-count)
   :unique-users (count (:user-sessions @session-state))
   :sessions-by-device (into {} (map (fn [[k v]] [k (count v)]) (get-sessions-by-device)))
   :average-duration-ms (get-average-session-duration)
   :stats (:stats @session-state)})

;; =============================================================================
;; CONFIGURATION
;; =============================================================================

(defn configure!
  "Configure session manager."
  [config]
  (log/info "Configuring session manager" config)
  (swap! session-state update :config merge config))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-session-manager!
  "Initialize session manager."
  []
  (log/info "Initializing session manager")
  ;; Register feature flag
  (flags/register-flag! "session-manager" "Enable session manager" true)
  ;; Create metrics
  (metrics/create-counter! :session/created "Sessions created")
  (metrics/create-counter! :session/destroyed "Sessions destroyed")
  (metrics/create-counter! :session/expired "Sessions expired")
  (metrics/create-counter! :session/touched "Sessions touched")
  (metrics/create-gauge! :session/active "Active sessions"
                         #(get-active-session-count))
  ;; Start cleanup scheduler
  (start-cleanup-scheduler!)
  (log/info "Session manager initialized"))

;; =============================================================================
;; SHUTDOWN
;; =============================================================================

(defn shutdown!
  "Shutdown session manager."
  []
  (log/info "Shutting down session manager")
  (stop-cleanup-scheduler!))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-manager-status []
  {:enabled (flags/is-enabled? "session-manager")
   :total-sessions (count (:sessions @session-state))
   :active-sessions (get-active-session-count)
   :unique-users (count (:user-sessions @session-state))
   :config (:config @session-state)
   :stats (:stats @session-state)})
