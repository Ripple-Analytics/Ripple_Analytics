(ns mental-models.pipeline.integration.notification-router
  "Notification Router Module
   
   Intelligent notification routing:
   - Multi-channel routing
   - Priority-based delivery
   - User preferences
   - Delivery scheduling
   - Fallback chains"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [java.util.concurrent ConcurrentHashMap ScheduledThreadPoolExecutor TimeUnit]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; NOTIFICATION ROUTER STATE
;; =============================================================================

(defonce router-state (atom {:channels (ConcurrentHashMap.)
                             :routes (ConcurrentHashMap.)
                             :preferences (ConcurrentHashMap.)
                             :queue (ConcurrentHashMap.)
                             :delivery-log (ConcurrentHashMap.)
                             :scheduler nil
                             :notification-count (AtomicLong. 0)
                             :delivery-count (AtomicLong. 0)
                             :config {:max-retries 3
                                      :retry-delay-ms 5000
                                      :batch-size 100
                                      :quiet-hours {:start 22 :end 7}}}))

;; =============================================================================
;; CHANNEL MANAGEMENT
;; =============================================================================

(defn register-channel!
  "Register a notification channel."
  [channel-id {:keys [name type deliver-fn priority enabled]}]
  (log/info "Registering notification channel" {:id channel-id :type type})
  (.put ^ConcurrentHashMap (:channels @router-state) channel-id
        {:id channel-id
         :name name
         :type type
         :deliver-fn deliver-fn
         :priority (or priority 0)
         :enabled (if (nil? enabled) true enabled)
         :registered-at (System/currentTimeMillis)}))

(defn unregister-channel!
  "Unregister a channel."
  [channel-id]
  (.remove ^ConcurrentHashMap (:channels @router-state) channel-id))

(defn get-channel
  "Get a channel by ID."
  [channel-id]
  (.get ^ConcurrentHashMap (:channels @router-state) channel-id))

(defn list-channels
  "List all channels."
  [& {:keys [type enabled]}]
  (let [channels (vals (:channels @router-state))]
    (cond->> channels
      type (filter #(= (:type %) type))
      (some? enabled) (filter #(= (:enabled %) enabled)))))

(defn enable-channel!
  "Enable a channel."
  [channel-id]
  (when-let [channel (get-channel channel-id)]
    (.put ^ConcurrentHashMap (:channels @router-state) channel-id
          (assoc channel :enabled true))))

(defn disable-channel!
  "Disable a channel."
  [channel-id]
  (when-let [channel (get-channel channel-id)]
    (.put ^ConcurrentHashMap (:channels @router-state) channel-id
          (assoc channel :enabled false))))

;; =============================================================================
;; ROUTING RULES
;; =============================================================================

(defn create-route!
  "Create a routing rule."
  [route-id {:keys [name condition channels priority fallback]}]
  (log/info "Creating route" {:id route-id})
  (.put ^ConcurrentHashMap (:routes @router-state) route-id
        {:id route-id
         :name name
         :condition condition
         :channels (or channels [])
         :priority (or priority 0)
         :fallback fallback
         :enabled true
         :created-at (System/currentTimeMillis)}))

(defn delete-route!
  "Delete a route."
  [route-id]
  (.remove ^ConcurrentHashMap (:routes @router-state) route-id))

(defn get-route
  "Get a route by ID."
  [route-id]
  (.get ^ConcurrentHashMap (:routes @router-state) route-id))

(defn find-matching-routes
  "Find routes matching a notification."
  [notification]
  (let [routes (filter :enabled (vals (:routes @router-state)))]
    (filter (fn [route]
              (if-let [condition (:condition route)]
                (condition notification)
                true))
            (sort-by :priority > routes))))

;; =============================================================================
;; USER PREFERENCES
;; =============================================================================

(defn set-preferences!
  "Set user notification preferences."
  [user-id {:keys [channels quiet-hours timezone enabled-types]}]
  (log/info "Setting user preferences" {:user user-id})
  (.put ^ConcurrentHashMap (:preferences @router-state) user-id
        {:user-id user-id
         :channels (or channels #{})
         :quiet-hours quiet-hours
         :timezone (or timezone "UTC")
         :enabled-types (or enabled-types #{})
         :updated-at (System/currentTimeMillis)}))

(defn get-preferences
  "Get user preferences."
  [user-id]
  (.get ^ConcurrentHashMap (:preferences @router-state) user-id))

(defn delete-preferences!
  "Delete user preferences."
  [user-id]
  (.remove ^ConcurrentHashMap (:preferences @router-state) user-id))

(defn is-quiet-hours?
  "Check if current time is within quiet hours for a user."
  [user-id]
  (let [prefs (get-preferences user-id)
        quiet-hours (or (:quiet-hours prefs) (get-in @router-state [:config :quiet-hours]))
        hour (.getHour (java.time.LocalTime/now))]
    (if (< (:start quiet-hours) (:end quiet-hours))
      (and (>= hour (:start quiet-hours)) (< hour (:end quiet-hours)))
      (or (>= hour (:start quiet-hours)) (< hour (:end quiet-hours))))))

(defn filter-by-preferences
  "Filter channels based on user preferences."
  [user-id channels notification]
  (let [prefs (get-preferences user-id)]
    (if prefs
      (let [allowed-channels (:channels prefs)
            enabled-types (:enabled-types prefs)]
        (filter (fn [ch]
                  (and (or (empty? allowed-channels)
                           (contains? allowed-channels (:id ch)))
                       (or (empty? enabled-types)
                           (contains? enabled-types (:type notification)))))
                channels))
      channels)))

;; =============================================================================
;; NOTIFICATION DELIVERY
;; =============================================================================

(defn deliver-to-channel!
  "Deliver a notification to a specific channel."
  [channel notification]
  (try
    (let [result ((:deliver-fn channel) notification)]
      (log/info "Notification delivered" {:channel (:id channel) :notification-id (:id notification)})
      {:success true :channel (:id channel) :result result})
    (catch Exception e
      (log/error "Delivery failed" {:channel (:id channel) :error (.getMessage e)})
      {:success false :channel (:id channel) :error (.getMessage e)})))

(defn route-notification!
  "Route a notification to appropriate channels."
  [notification]
  (.incrementAndGet ^AtomicLong (:notification-count @router-state))
  (metrics/inc-counter! :notificationrouter/notifications)
  (let [routes (find-matching-routes notification)
        user-id (:user-id notification)]
    (if (seq routes)
      (let [channel-ids (distinct (mapcat :channels routes))
            channels (filter :enabled (map get-channel channel-ids))
            filtered-channels (if user-id
                                (filter-by-preferences user-id channels notification)
                                channels)]
        (if (and user-id (is-quiet-hours? user-id) (not (:urgent notification)))
          ;; Queue for later
          (do
            (queue-notification! notification)
            {:status :queued :reason :quiet-hours})
          ;; Deliver now
          (let [results (doall (map #(deliver-to-channel! % notification) filtered-channels))
                successes (filter :success results)
                failures (filter (complement :success) results)]
            (.addAndGet ^AtomicLong (:delivery-count @router-state) (count successes))
            ;; Try fallback for failures
            (doseq [route routes]
              (when (and (:fallback route)
                         (some #(= (:channel %) (first (:channels route))) failures))
                (when-let [fallback-channel (get-channel (:fallback route))]
                  (deliver-to-channel! fallback-channel notification))))
            ;; Log delivery
            (log-delivery! notification results)
            {:status :delivered
             :successes (count successes)
             :failures (count failures)
             :results results})))
      {:status :no-routes :notification notification})))

;; =============================================================================
;; NOTIFICATION QUEUE
;; =============================================================================

(defn queue-notification!
  "Queue a notification for later delivery."
  [notification]
  (let [notification-id (or (:id notification) (str (java.util.UUID/randomUUID)))]
    (.put ^ConcurrentHashMap (:queue @router-state) notification-id
          (assoc notification
                 :id notification-id
                 :queued-at (System/currentTimeMillis)
                 :retries 0))))

(defn get-queued-notifications
  "Get queued notifications."
  [& {:keys [user-id limit]}]
  (let [notifications (vals (:queue @router-state))]
    (cond->> notifications
      user-id (filter #(= (:user-id %) user-id))
      true (sort-by :queued-at)
      limit (take limit))))

(defn process-queue!
  "Process queued notifications."
  []
  (let [now (System/currentTimeMillis)
        max-retries (get-in @router-state [:config :max-retries])]
    (doseq [[id notification] (:queue @router-state)]
      (let [user-id (:user-id notification)]
        (when (or (not user-id) (not (is-quiet-hours? user-id)) (:urgent notification))
          (let [result (route-notification! (dissoc notification :queued-at :retries))]
            (if (= :delivered (:status result))
              (.remove ^ConcurrentHashMap (:queue @router-state) id)
              (when (>= (:retries notification) max-retries)
                (.remove ^ConcurrentHashMap (:queue @router-state) id)
                (log/warn "Notification dropped after max retries" {:id id})))))))))

;; =============================================================================
;; DELIVERY LOGGING
;; =============================================================================

(defn log-delivery!
  "Log a delivery attempt."
  [notification results]
  (let [log-id (str (System/currentTimeMillis) "-" (rand-int 10000))]
    (.put ^ConcurrentHashMap (:delivery-log @router-state) log-id
          {:id log-id
           :notification-id (:id notification)
           :user-id (:user-id notification)
           :type (:type notification)
           :results results
           :timestamp (System/currentTimeMillis)})))

(defn get-delivery-log
  "Get delivery log entries."
  [& {:keys [user-id notification-id limit since]}]
  (let [entries (vals (:delivery-log @router-state))]
    (cond->> entries
      user-id (filter #(= (:user-id %) user-id))
      notification-id (filter #(= (:notification-id %) notification-id))
      since (filter #(>= (:timestamp %) since))
      true (sort-by :timestamp >)
      limit (take limit))))

;; =============================================================================
;; SCHEDULER
;; =============================================================================

(defn start-scheduler!
  "Start the notification scheduler."
  []
  (log/info "Starting notification scheduler")
  (let [scheduler (ScheduledThreadPoolExecutor. 1)]
    (.scheduleAtFixedRate scheduler
                          #(process-queue!)
                          60000
                          60000
                          TimeUnit/MILLISECONDS)
    (swap! router-state assoc :scheduler scheduler)))

(defn stop-scheduler!
  "Stop the notification scheduler."
  []
  (when-let [scheduler (:scheduler @router-state)]
    (log/info "Stopping notification scheduler")
    (.shutdown ^ScheduledThreadPoolExecutor scheduler)
    (swap! router-state assoc :scheduler nil)))

;; =============================================================================
;; BUILT-IN CHANNELS
;; =============================================================================

(defn init-built-in-channels!
  "Initialize built-in notification channels."
  []
  ;; Console channel (for testing)
  (register-channel! :console
                     {:name "Console"
                      :type :console
                      :deliver-fn (fn [notification]
                                    (println "[NOTIFICATION]" (:type notification) "-" (:message notification))
                                    {:delivered true})
                      :priority 0})
  ;; Log channel
  (register-channel! :log
                     {:name "Log"
                      :type :log
                      :deliver-fn (fn [notification]
                                    (log/info "Notification" notification)
                                    {:delivered true})
                      :priority 0}))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-router-stats
  "Get router statistics."
  []
  {:channels (.size ^ConcurrentHashMap (:channels @router-state))
   :routes (.size ^ConcurrentHashMap (:routes @router-state))
   :preferences (.size ^ConcurrentHashMap (:preferences @router-state))
   :queued (.size ^ConcurrentHashMap (:queue @router-state))
   :delivery-log-entries (.size ^ConcurrentHashMap (:delivery-log @router-state))
   :notification-count (.get ^AtomicLong (:notification-count @router-state))
   :delivery-count (.get ^AtomicLong (:delivery-count @router-state))})

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-notification-router!
  "Initialize notification router."
  []
  (log/info "Initializing notification router")
  ;; Register feature flag
  (flags/register-flag! "notification-router" "Enable notification router" true)
  ;; Create metrics
  (metrics/create-counter! :notificationrouter/notifications "Notifications routed")
  (metrics/create-gauge! :notificationrouter/queued "Queued notifications"
                         #(.size ^ConcurrentHashMap (:queue @router-state)))
  ;; Initialize built-in channels
  (init-built-in-channels!)
  ;; Start scheduler
  (start-scheduler!)
  (log/info "Notification router initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-notification-router-status []
  {:enabled (flags/is-enabled? "notification-router")
   :channels (.size ^ConcurrentHashMap (:channels @router-state))
   :routes (.size ^ConcurrentHashMap (:routes @router-state))
   :preferences (.size ^ConcurrentHashMap (:preferences @router-state))
   :queued (.size ^ConcurrentHashMap (:queue @router-state))
   :scheduler-running (some? (:scheduler @router-state))
   :stats (get-router-stats)
   :config (:config @router-state)})
