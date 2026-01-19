(ns mental-models.pipeline.integration.request-replayer
  "Request replayer for mental model analysis system.
   
   Features:
   - Request recording
   - Request replay
   - Replay scheduling
   - Replay comparison
   - Replay filtering
   - Replay transformation
   - Replay metrics
   - Replay reports"
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
  (atom {:recordings {}       ;; recording-id -> recording
         :replays {}          ;; replay-id -> replay
         :config {:max-recordings 10000
                  :max-request-body-size 1048576  ;; 1MB
                  :recording-ttl-ms 86400000      ;; 24 hours
                  :default-delay-ms 0}
         :stats {:requests-recorded 0
                 :requests-replayed 0
                 :replay-successes 0
                 :replay-failures 0
                 :response-mismatches 0}
         :recording? (atom false)
         :initialized? false}))

;; ============================================================================
;; Request Recording
;; ============================================================================

(defn- sanitize-request
  "Sanitize a request for recording."
  [request]
  (let [max-body-size (get-in @state [:config :max-request-body-size])]
    (-> request
        (select-keys [:method :uri :query-string :headers :body])
        (update :headers dissoc "authorization" "cookie" "x-api-key")
        (update :body (fn [body]
                        (when body
                          (let [body-str (if (string? body) body (slurp body))]
                            (if (> (count body-str) max-body-size)
                              (str (subs body-str 0 max-body-size) "...[truncated]")
                              body-str))))))))

(defn- sanitize-response
  "Sanitize a response for recording."
  [response]
  (let [max-body-size (get-in @state [:config :max-request-body-size])]
    (-> response
        (select-keys [:status :headers :body])
        (update :headers dissoc "set-cookie")
        (update :body (fn [body]
                        (when body
                          (let [body-str (if (string? body) body (str body))]
                            (if (> (count body-str) max-body-size)
                              (str (subs body-str 0 max-body-size) "...[truncated]")
                              body-str))))))))

(defn record-request!
  "Record a request and response."
  [request response]
  (when @(:recording? @state)
    (let [recording-id (UUID/randomUUID)
          recording {:id recording-id
                     :request (sanitize-request request)
                     :response (sanitize-response response)
                     :timestamp (System/currentTimeMillis)
                     :duration-ms (get request :duration-ms)}]
      
      ;; Cleanup old recordings if needed
      (let [max-recordings (get-in @state [:config :max-recordings])]
        (when (> (count (:recordings @state)) max-recordings)
          (let [ttl (get-in @state [:config :recording-ttl-ms])
                cutoff (- (System/currentTimeMillis) ttl)]
            (swap! state update :recordings
                   (fn [recs]
                     (into {} (filter (fn [[_ v]] (> (:timestamp v) cutoff)) recs)))))))
      
      (swap! state assoc-in [:recordings recording-id] recording)
      (swap! state update-in [:stats :requests-recorded] inc)
      
      recording-id)))

(defn start-recording!
  "Start recording requests."
  []
  (reset! (:recording? @state) true)
  (logging/log :info "Started request recording"))

(defn stop-recording!
  "Stop recording requests."
  []
  (reset! (:recording? @state) false)
  (logging/log :info "Stopped request recording"))

(defn is-recording?
  "Check if recording is active."
  []
  @(:recording? @state))

;; ============================================================================
;; Recording Management
;; ============================================================================

(defn get-recording
  "Get a recording by ID."
  [recording-id]
  (get-in @state [:recordings recording-id]))

(defn list-recordings
  "List recordings with optional filters."
  [& {:keys [limit uri method since] :or {limit 100}}]
  (->> (vals (:recordings @state))
       (filter #(or (nil? uri) (= (get-in % [:request :uri]) uri)))
       (filter #(or (nil? method) (= (get-in % [:request :method]) method)))
       (filter #(or (nil? since) (> (:timestamp %) since)))
       (sort-by :timestamp >)
       (take limit)
       (vec)))

(defn delete-recording!
  "Delete a recording."
  [recording-id]
  (swap! state update :recordings dissoc recording-id))

(defn clear-recordings!
  "Clear all recordings."
  []
  (swap! state assoc :recordings {}))

;; ============================================================================
;; Request Replay
;; ============================================================================

(defn- execute-request
  "Execute a recorded request."
  [request handler]
  (let [start-time (System/currentTimeMillis)
        response (handler request)
        end-time (System/currentTimeMillis)]
    {:response response
     :duration-ms (- end-time start-time)}))

(defn replay-request
  "Replay a single recorded request."
  [recording-id handler & {:keys [transform-fn]}]
  (if-let [recording (get-recording recording-id)]
    (let [request (if transform-fn
                    (transform-fn (:request recording))
                    (:request recording))
          result (execute-request request handler)
          original-response (:response recording)
          replay-response (:response result)
          status-match? (= (:status original-response) (:status replay-response))]
      
      (swap! state update-in [:stats :requests-replayed] inc)
      (if status-match?
        (swap! state update-in [:stats :replay-successes] inc)
        (do
          (swap! state update-in [:stats :replay-failures] inc)
          (swap! state update-in [:stats :response-mismatches] inc)))
      
      {:recording-id recording-id
       :original-response original-response
       :replay-response replay-response
       :duration-ms (:duration-ms result)
       :original-duration-ms (:duration-ms recording)
       :status-match? status-match?
       :timestamp (System/currentTimeMillis)})
    {:error "Recording not found" :recording-id recording-id}))

(defn replay-batch
  "Replay a batch of recordings."
  [recording-ids handler & {:keys [delay-ms transform-fn parallel?]
                            :or {delay-ms 0 parallel? false}}]
  (if parallel?
    (let [results (pmap #(replay-request % handler :transform-fn transform-fn) recording-ids)]
      (vec results))
    (loop [ids recording-ids
           results []]
      (if (empty? ids)
        results
        (do
          (when (pos? delay-ms)
            (Thread/sleep delay-ms))
          (recur (rest ids)
                 (conj results (replay-request (first ids) handler :transform-fn transform-fn))))))))

;; ============================================================================
;; Replay Sessions
;; ============================================================================

(defn create-replay-session!
  "Create a replay session."
  [config]
  (let [replay-id (UUID/randomUUID)
        replay {:id replay-id
                :name (get config :name (str "Replay " replay-id))
                :recording-ids (get config :recording-ids [])
                :filter-fn (get config :filter-fn)
                :transform-fn (get config :transform-fn)
                :delay-ms (get config :delay-ms 0)
                :parallel? (get config :parallel? false)
                :status :pending
                :results []
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:replays replay-id] replay)
    (logging/log :info "Created replay session" {:replay-id replay-id})
    replay-id))

(defn run-replay-session!
  "Run a replay session."
  [replay-id handler]
  (if-let [replay (get-in @state [:replays replay-id])]
    (do
      (swap! state assoc-in [:replays replay-id :status] :running)
      (swap! state assoc-in [:replays replay-id :started-at] (System/currentTimeMillis))
      
      (let [recording-ids (if-let [filter-fn (:filter-fn replay)]
                            (filter filter-fn (:recording-ids replay))
                            (:recording-ids replay))
            results (replay-batch recording-ids handler
                                  :delay-ms (:delay-ms replay)
                                  :transform-fn (:transform-fn replay)
                                  :parallel? (:parallel? replay))]
        
        (swap! state assoc-in [:replays replay-id :status] :completed)
        (swap! state assoc-in [:replays replay-id :completed-at] (System/currentTimeMillis))
        (swap! state assoc-in [:replays replay-id :results] results)
        
        (logging/log :info "Completed replay session" {:replay-id replay-id
                                                        :total (count results)
                                                        :successes (count (filter :status-match? results))})
        results))
    {:error "Replay session not found" :replay-id replay-id}))

(defn get-replay-session
  "Get a replay session."
  [replay-id]
  (get-in @state [:replays replay-id]))

(defn list-replay-sessions
  "List replay sessions."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :status (:status r)
           :recording-count (count (:recording-ids r))
           :created-at (:created-at r)})
        (:replays @state)))

;; ============================================================================
;; Replay Comparison
;; ============================================================================

(defn compare-responses
  "Compare original and replayed responses."
  [original replay]
  {:status-match? (= (:status original) (:status replay))
   :headers-match? (= (dissoc (:headers original) "date" "x-request-id")
                      (dissoc (:headers replay) "date" "x-request-id"))
   :body-match? (= (:body original) (:body replay))
   :original-status (:status original)
   :replay-status (:status replay)})

(defn generate-replay-report
  "Generate a report for a replay session."
  [replay-id]
  (if-let [replay (get-replay-session replay-id)]
    (let [results (:results replay)
          total (count results)
          successes (count (filter :status-match? results))
          failures (- total successes)
          avg-duration (if (pos? total)
                         (/ (reduce + (map :duration-ms results)) total)
                         0)
          avg-original-duration (if (pos? total)
                                  (/ (reduce + (map :original-duration-ms results)) total)
                                  0)]
      {:replay-id replay-id
       :name (:name replay)
       :status (:status replay)
       :total-requests total
       :successes successes
       :failures failures
       :success-rate (if (pos? total) (/ successes total) 1.0)
       :avg-duration-ms avg-duration
       :avg-original-duration-ms avg-original-duration
       :duration-change-percent (if (pos? avg-original-duration)
                                  (* 100 (/ (- avg-duration avg-original-duration)
                                            avg-original-duration))
                                  0)
       :started-at (:started-at replay)
       :completed-at (:completed-at replay)})
    {:error "Replay session not found"}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-recorder
  "Ring middleware to record requests."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)]
      (record-request! (assoc request :duration-ms duration-ms) response)
      response)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-replayer-metrics
  "Get replayer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-recorded (:requests-recorded stats)
     :requests-replayed (:requests-replayed stats)
     :replay-successes (:replay-successes stats)
     :replay-failures (:replay-failures stats)
     :response-mismatches (:response-mismatches stats)
     :success-rate (if (pos? (:requests-replayed stats))
                     (/ (:replay-successes stats) (:requests-replayed stats))
                     1.0)
     :stored-recordings (count (:recordings @state))
     :replay-sessions (count (:replays @state))
     :recording? (is-recording?)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-replayer-stats
  "Get replayer statistics."
  []
  (merge (get-replayer-metrics)
         {:max-recordings (get-in @state [:config :max-recordings])
          :recording-ttl-ms (get-in @state [:config :recording-ttl-ms])}))

(defn reset-stats!
  "Reset replayer statistics."
  []
  (swap! state assoc :stats {:requests-recorded 0
                             :requests-replayed 0
                             :replay-successes 0
                             :replay-failures 0
                             :response-mismatches 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-replayer!
  "Initialize the request replayer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request replayer initialized")
    (events/emit! :request-replayer-initialized {})
    true))
