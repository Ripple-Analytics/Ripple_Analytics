(ns mental-models.pipeline.integration.request-profiler
  "Request profiler for mental model analysis system.
   
   Features:
   - Request timing
   - Memory profiling
   - CPU profiling
   - Database query profiling
   - External call profiling
   - Profiling aggregation
   - Slow request detection
   - Profiling metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.lang.management ManagementFactory ThreadMXBean]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:profiles {}         ;; request-id -> profile
         :aggregates {}       ;; endpoint -> aggregate-stats
         :config {:enabled? true
                  :slow-threshold-ms 1000
                  :sample-rate 1.0
                  :max-profiles 10000
                  :profile-ttl-ms 3600000}  ;; 1 hour
         :stats {:requests-profiled 0
                 :slow-requests 0
                 :total-time-ms 0
                 :db-queries 0
                 :external-calls 0}
         :initialized? false}))

;; ============================================================================
;; Thread MXBean
;; ============================================================================

(def ^:private thread-mx-bean (ManagementFactory/getThreadMXBean))

(defn- get-thread-cpu-time
  "Get CPU time for current thread in nanoseconds."
  []
  (when (.isCurrentThreadCpuTimeSupported thread-mx-bean)
    (.getCurrentThreadCpuTime thread-mx-bean)))

(defn- get-thread-user-time
  "Get user time for current thread in nanoseconds."
  []
  (when (.isCurrentThreadCpuTimeSupported thread-mx-bean)
    (.getCurrentThreadUserTime thread-mx-bean)))

;; ============================================================================
;; Profile Creation
;; ============================================================================

(defn start-profile
  "Start profiling a request."
  [request]
  (when (and (get-in @state [:config :enabled?])
             (< (rand) (get-in @state [:config :sample-rate])))
    (let [profile-id (UUID/randomUUID)
          runtime (Runtime/getRuntime)]
      {:id profile-id
       :request-id (get request :request-id)
       :uri (get request :uri)
       :method (get request :method)
       :start-time (System/currentTimeMillis)
       :start-cpu-time (get-thread-cpu-time)
       :start-user-time (get-thread-user-time)
       :start-memory (.totalMemory runtime)
       :start-free-memory (.freeMemory runtime)
       :spans []
       :db-queries []
       :external-calls []
       :markers []})))

(defn end-profile
  "End profiling a request."
  [profile response]
  (when profile
    (let [runtime (Runtime/getRuntime)
          end-time (System/currentTimeMillis)
          end-cpu-time (get-thread-cpu-time)
          end-user-time (get-thread-user-time)
          duration-ms (- end-time (:start-time profile))
          cpu-time-ns (when (and end-cpu-time (:start-cpu-time profile))
                        (- end-cpu-time (:start-cpu-time profile)))
          user-time-ns (when (and end-user-time (:start-user-time profile))
                         (- end-user-time (:start-user-time profile)))
          memory-used (- (.totalMemory runtime) (.freeMemory runtime))
          memory-delta (- memory-used (- (:start-memory profile) (:start-free-memory profile)))
          slow-threshold (get-in @state [:config :slow-threshold-ms])
          is-slow? (> duration-ms slow-threshold)]
      
      (swap! state update-in [:stats :requests-profiled] inc)
      (swap! state update-in [:stats :total-time-ms] + duration-ms)
      (when is-slow?
        (swap! state update-in [:stats :slow-requests] inc))
      
      (let [completed-profile (assoc profile
                                     :end-time end-time
                                     :duration-ms duration-ms
                                     :cpu-time-ns cpu-time-ns
                                     :user-time-ns user-time-ns
                                     :memory-used memory-used
                                     :memory-delta memory-delta
                                     :is-slow? is-slow?
                                     :status (get response :status))]
        
        ;; Store profile
        (store-profile! completed-profile)
        
        ;; Update aggregates
        (update-aggregates! completed-profile)
        
        ;; Log slow requests
        (when is-slow?
          (logging/log :warn "Slow request detected" {:uri (:uri profile)
                                                       :duration-ms duration-ms}))
        
        completed-profile))))

;; ============================================================================
;; Span Tracking
;; ============================================================================

(defn start-span
  "Start a profiling span."
  [profile span-name]
  (when profile
    {:name span-name
     :start-time (System/currentTimeMillis)
     :start-cpu-time (get-thread-cpu-time)}))

(defn end-span
  "End a profiling span and add to profile."
  [profile span]
  (when (and profile span)
    (let [end-time (System/currentTimeMillis)
          end-cpu-time (get-thread-cpu-time)
          completed-span (assoc span
                                :end-time end-time
                                :duration-ms (- end-time (:start-time span))
                                :cpu-time-ns (when (and end-cpu-time (:start-cpu-time span))
                                               (- end-cpu-time (:start-cpu-time span))))]
      (update profile :spans conj completed-span))))

(defmacro with-span
  "Execute body within a profiling span."
  [profile span-name & body]
  `(let [span# (start-span ~profile ~span-name)]
     (try
       (let [result# (do ~@body)]
         (when span#
           (swap! ~profile update :spans conj
                  (assoc span#
                         :end-time (System/currentTimeMillis)
                         :duration-ms (- (System/currentTimeMillis) (:start-time span#)))))
         result#)
       (catch Exception e#
         (when span#
           (swap! ~profile update :spans conj
                  (assoc span#
                         :end-time (System/currentTimeMillis)
                         :duration-ms (- (System/currentTimeMillis) (:start-time span#))
                         :error (.getMessage e#))))
         (throw e#)))))

;; ============================================================================
;; Database Query Profiling
;; ============================================================================

(defn record-db-query!
  "Record a database query."
  [profile query duration-ms rows-affected]
  (when profile
    (swap! state update-in [:stats :db-queries] inc)
    (update profile :db-queries conj
            {:query (if (> (count query) 200)
                      (str (subs query 0 200) "...")
                      query)
             :duration-ms duration-ms
             :rows-affected rows-affected
             :timestamp (System/currentTimeMillis)})))

(defmacro profile-db-query
  "Profile a database query."
  [profile query-str & body]
  `(let [start# (System/currentTimeMillis)
         result# (do ~@body)
         duration# (- (System/currentTimeMillis) start#)]
     (when ~profile
       (record-db-query! ~profile ~query-str duration# (count result#)))
     result#))

;; ============================================================================
;; External Call Profiling
;; ============================================================================

(defn record-external-call!
  "Record an external API call."
  [profile service url duration-ms status]
  (when profile
    (swap! state update-in [:stats :external-calls] inc)
    (update profile :external-calls conj
            {:service service
             :url url
             :duration-ms duration-ms
             :status status
             :timestamp (System/currentTimeMillis)})))

(defmacro profile-external-call
  "Profile an external API call."
  [profile service url & body]
  `(let [start# (System/currentTimeMillis)
         result# (do ~@body)
         duration# (- (System/currentTimeMillis) start#)]
     (when ~profile
       (record-external-call! ~profile ~service ~url duration# (:status result#)))
     result#))

;; ============================================================================
;; Markers
;; ============================================================================

(defn add-marker!
  "Add a marker to the profile."
  [profile marker-name data]
  (when profile
    (update profile :markers conj
            {:name marker-name
             :data data
             :timestamp (System/currentTimeMillis)})))

;; ============================================================================
;; Profile Storage
;; ============================================================================

(defn- store-profile!
  "Store a completed profile."
  [profile]
  (let [max-profiles (get-in @state [:config :max-profiles])]
    ;; Cleanup if needed
    (when (> (count (:profiles @state)) max-profiles)
      (let [ttl (get-in @state [:config :profile-ttl-ms])
            cutoff (- (System/currentTimeMillis) ttl)]
        (swap! state update :profiles
               (fn [profiles]
                 (into {} (filter (fn [[_ v]] (> (:end-time v) cutoff)) profiles))))))
    
    (swap! state assoc-in [:profiles (:id profile)] profile)))

(defn get-profile
  "Get a stored profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn list-profiles
  "List profiles with optional filters."
  [& {:keys [limit uri slow-only?] :or {limit 100}}]
  (->> (vals (:profiles @state))
       (filter #(or (nil? uri) (= (:uri %) uri)))
       (filter #(or (not slow-only?) (:is-slow? %)))
       (sort-by :end-time >)
       (take limit)
       (vec)))

;; ============================================================================
;; Aggregates
;; ============================================================================

(defn- update-aggregates!
  "Update aggregate statistics."
  [profile]
  (let [key (str (:method profile) " " (:uri profile))]
    (swap! state update-in [:aggregates key]
           (fn [agg]
             (let [current (or agg {:count 0 :total-ms 0 :min-ms Long/MAX_VALUE :max-ms 0})]
               {:count (inc (:count current))
                :total-ms (+ (:total-ms current) (:duration-ms profile))
                :min-ms (min (:min-ms current) (:duration-ms profile))
                :max-ms (max (:max-ms current) (:duration-ms profile))
                :avg-ms (/ (+ (:total-ms current) (:duration-ms profile))
                           (inc (:count current)))
                :last-seen (:end-time profile)})))))

(defn get-aggregates
  "Get aggregate statistics."
  []
  (mapv (fn [[endpoint stats]]
          (assoc stats :endpoint endpoint))
        (:aggregates @state)))

(defn get-slowest-endpoints
  "Get the slowest endpoints by average time."
  [limit]
  (->> (get-aggregates)
       (sort-by :avg-ms >)
       (take limit)
       (vec)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-profiler
  "Ring middleware for request profiling."
  [handler]
  (fn [request]
    (let [profile (start-profile request)]
      (if profile
        (let [response (handler (assoc request :profile profile))
              completed-profile (end-profile profile response)]
          (assoc response :profile completed-profile))
        (handler request)))))

(defn wrap-profiler-header
  "Ring middleware to add profiling info to response headers."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if-let [profile (:profile response)]
        (-> response
            (assoc-in [:headers "X-Request-Duration"] (str (:duration-ms profile)))
            (assoc-in [:headers "X-Request-Profile-Id"] (str (:id profile))))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn enable-profiling!
  "Enable profiling."
  []
  (swap! state assoc-in [:config :enabled?] true))

(defn disable-profiling!
  "Disable profiling."
  []
  (swap! state assoc-in [:config :enabled?] false))

(defn set-slow-threshold!
  "Set the slow request threshold."
  [threshold-ms]
  (swap! state assoc-in [:config :slow-threshold-ms] threshold-ms))

(defn set-sample-rate!
  "Set the sampling rate (0.0 to 1.0)."
  [rate]
  (swap! state assoc-in [:config :sample-rate] (max 0.0 (min 1.0 rate))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-profiler-metrics
  "Get profiler metrics."
  []
  (let [stats (:stats @state)
        total (:requests-profiled stats)]
    {:requests-profiled total
     :slow-requests (:slow-requests stats)
     :slow-request-rate (if (pos? total)
                          (/ (:slow-requests stats) total)
                          0)
     :total-time-ms (:total-time-ms stats)
     :avg-time-ms (if (pos? total)
                    (/ (:total-time-ms stats) total)
                    0)
     :db-queries (:db-queries stats)
     :external-calls (:external-calls stats)
     :stored-profiles (count (:profiles @state))
     :endpoints-tracked (count (:aggregates @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-profiler-stats
  "Get profiler statistics."
  []
  (merge (get-profiler-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :slow-threshold-ms (get-in @state [:config :slow-threshold-ms])
          :sample-rate (get-in @state [:config :sample-rate])}))

(defn reset-stats!
  "Reset profiler statistics."
  []
  (swap! state assoc :stats {:requests-profiled 0
                             :slow-requests 0
                             :total-time-ms 0
                             :db-queries 0
                             :external-calls 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-profiler!
  "Initialize the request profiler."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request profiler initialized")
    (events/emit! :request-profiler-initialized {})
    true))
