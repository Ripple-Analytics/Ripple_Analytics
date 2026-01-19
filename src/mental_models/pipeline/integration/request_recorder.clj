(ns mental-models.pipeline.integration.request-recorder
  "Request recorder for mental model analysis system.
   
   Features:
   - Request/response recording
   - Recording sessions
   - Playback support
   - Recording filters
   - Recording storage
   - Recording export
   - Recording search
   - Recording metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.io File]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:sessions {}         ;; session-id -> session
         :recordings []       ;; all recordings
         :config {:enabled? true
                  :max-recordings 100000
                  :max-body-size 1048576  ;; 1MB
                  :record-headers? true
                  :record-body? true
                  :sensitive-headers #{"authorization" "cookie" "x-api-key"}
                  :storage-path "/tmp/recordings"}
         :stats {:recordings-made 0
                 :sessions-created 0
                 :bytes-recorded 0
                 :playbacks 0}
         :initialized? false}))

;; ============================================================================
;; Recording Sessions
;; ============================================================================

(defn create-session!
  "Create a recording session."
  [session-id & {:keys [name description filters]}]
  (let [session {:id session-id
                 :name (or name (str "Session " session-id))
                 :description description
                 :filters (or filters {})
                 :recordings (atom [])
                 :active? (atom true)
                 :created-at (System/currentTimeMillis)
                 :ended-at (atom nil)}]
    
    (swap! state assoc-in [:sessions session-id] session)
    (swap! state update-in [:stats :sessions-created] inc)
    (logging/log :info "Created recording session" {:session-id session-id})
    session-id))

(defn get-session
  "Get a recording session."
  [session-id]
  (get-in @state [:sessions session-id]))

(defn list-sessions
  "List all recording sessions."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :active? @(:active? s)
           :recordings-count (count @(:recordings s))
           :created-at (:created-at s)
           :ended-at @(:ended-at s)})
        (:sessions @state)))

(defn end-session!
  "End a recording session."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:active? session) false)
    (reset! (:ended-at session) (System/currentTimeMillis))
    (logging/log :info "Ended recording session" {:session-id session-id})))

(defn delete-session!
  "Delete a recording session."
  [session-id]
  (swap! state update :sessions dissoc session-id))

;; ============================================================================
;; Request Recording
;; ============================================================================

(defn- sanitize-headers
  "Remove sensitive headers."
  [headers]
  (let [sensitive (get-in @state [:config :sensitive-headers])]
    (into {}
          (for [[k v] headers]
            (if (contains? sensitive (str/lower-case (name k)))
              [k "[REDACTED]"]
              [k v])))))

(defn- truncate-body
  "Truncate body if too large."
  [body]
  (let [max-size (get-in @state [:config :max-body-size])]
    (cond
      (nil? body) nil
      (string? body) (if (> (count body) max-size)
                       (str (subs body 0 max-size) "...[TRUNCATED]")
                       body)
      :else body)))

(defn record-request!
  "Record a request."
  [request & {:keys [session-id]}]
  (when (get-in @state [:config :enabled?])
    (let [recording {:id (str (UUID/randomUUID))
                     :type :request
                     :timestamp (System/currentTimeMillis)
                     :method (:request-method request)
                     :uri (:uri request)
                     :query-string (:query-string request)
                     :headers (when (get-in @state [:config :record-headers?])
                                (sanitize-headers (:headers request)))
                     :body (when (get-in @state [:config :record-body?])
                             (truncate-body (:body request)))
                     :remote-addr (:remote-addr request)
                     :session-id session-id}]
      
      ;; Add to session if specified
      (when-let [session (and session-id (get-session session-id))]
        (when @(:active? session)
          (swap! (:recordings session) conj recording)))
      
      ;; Add to global recordings
      (let [max-recordings (get-in @state [:config :max-recordings])]
        (swap! state update :recordings
               (fn [r]
                 (let [new-recordings (conj r recording)]
                   (if (> (count new-recordings) max-recordings)
                     (vec (drop 1 new-recordings))
                     new-recordings)))))
      
      (swap! state update-in [:stats :recordings-made] inc)
      (swap! state update-in [:stats :bytes-recorded] +
             (count (str recording)))
      
      (:id recording))))

(defn record-response!
  "Record a response."
  [response request-id & {:keys [session-id duration-ms]}]
  (when (get-in @state [:config :enabled?])
    (let [recording {:id (str (UUID/randomUUID))
                     :type :response
                     :request-id request-id
                     :timestamp (System/currentTimeMillis)
                     :status (:status response)
                     :headers (when (get-in @state [:config :record-headers?])
                                (sanitize-headers (:headers response)))
                     :body (when (get-in @state [:config :record-body?])
                             (truncate-body (:body response)))
                     :duration-ms duration-ms
                     :session-id session-id}]
      
      ;; Add to session if specified
      (when-let [session (and session-id (get-session session-id))]
        (when @(:active? session)
          (swap! (:recordings session) conj recording)))
      
      ;; Add to global recordings
      (let [max-recordings (get-in @state [:config :max-recordings])]
        (swap! state update :recordings
               (fn [r]
                 (let [new-recordings (conj r recording)]
                   (if (> (count new-recordings) max-recordings)
                     (vec (drop 1 new-recordings))
                     new-recordings)))))
      
      (swap! state update-in [:stats :recordings-made] inc)
      (swap! state update-in [:stats :bytes-recorded] +
             (count (str recording)))
      
      (:id recording))))

(defn record-exchange!
  "Record a complete request/response exchange."
  [request response duration-ms & {:keys [session-id]}]
  (let [request-id (record-request! request :session-id session-id)]
    (record-response! response request-id
                      :session-id session-id
                      :duration-ms duration-ms)
    request-id))

;; ============================================================================
;; Recording Retrieval
;; ============================================================================

(defn get-recordings
  "Get recordings with optional filters."
  [& {:keys [session-id type uri method since until limit]
      :or {limit 100}}]
  (let [recordings (if session-id
                     (when-let [session (get-session session-id)]
                       @(:recordings session))
                     (:recordings @state))]
    (cond->> recordings
      type (filter #(= (:type %) type))
      uri (filter #(str/includes? (or (:uri %) "") uri))
      method (filter #(= (:method %) method))
      since (filter #(> (:timestamp %) since))
      until (filter #(< (:timestamp %) until))
      true (take-last limit)
      true vec)))

(defn get-recording
  "Get a specific recording by ID."
  [recording-id]
  (first (filter #(= (:id %) recording-id) (:recordings @state))))

(defn get-exchange
  "Get a request/response exchange."
  [request-id]
  (let [recordings (:recordings @state)
        request (first (filter #(and (= (:type %) :request)
                                     (= (:id %) request-id))
                               recordings))
        response (first (filter #(and (= (:type %) :response)
                                      (= (:request-id %) request-id))
                                recordings))]
    (when request
      {:request request
       :response response})))

;; ============================================================================
;; Recording Search
;; ============================================================================

(defn search-recordings
  "Search recordings by various criteria."
  [query]
  (let [recordings (:recordings @state)]
    (filter (fn [r]
              (or (and (:uri r) (str/includes? (:uri r) query))
                  (and (:body r) (string? (:body r))
                       (str/includes? (:body r) query))))
            recordings)))

;; ============================================================================
;; Recording Export/Import
;; ============================================================================

(defn export-recordings
  "Export recordings to EDN format."
  [& {:keys [session-id path]}]
  (let [recordings (if session-id
                     (when-let [session (get-session session-id)]
                       @(:recordings session))
                     (:recordings @state))
        export-path (or path
                        (str (get-in @state [:config :storage-path])
                             "/export-" (System/currentTimeMillis) ".edn"))]
    (io/make-parents export-path)
    (spit export-path (pr-str recordings))
    {:path export-path
     :count (count recordings)}))

(defn import-recordings
  "Import recordings from EDN file."
  [path & {:keys [session-id]}]
  (when (.exists (io/file path))
    (let [recordings (edn/read-string (slurp path))]
      (if session-id
        (when-let [session (get-session session-id)]
          (swap! (:recordings session) into recordings))
        (swap! state update :recordings into recordings))
      {:count (count recordings)})))

;; ============================================================================
;; Playback
;; ============================================================================

(defn playback-recording
  "Playback a recorded request."
  [recording-id handler]
  (when-let [recording (get-recording recording-id)]
    (when (= (:type recording) :request)
      (swap! state update-in [:stats :playbacks] inc)
      (let [request {:request-method (:method recording)
                     :uri (:uri recording)
                     :query-string (:query-string recording)
                     :headers (:headers recording)
                     :body (:body recording)}
            start-time (System/currentTimeMillis)
            response (handler request)
            duration-ms (- (System/currentTimeMillis) start-time)]
        {:original-recording recording
         :playback-response response
         :playback-duration-ms duration-ms}))))

(defn playback-session
  "Playback all requests in a session."
  [session-id handler]
  (when-let [session (get-session session-id)]
    (let [requests (filter #(= (:type %) :request) @(:recordings session))]
      (mapv (fn [recording]
              (playback-recording (:id recording) handler))
            requests))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-record
  "Ring middleware to record requests and responses."
  [handler & {:keys [session-id]}]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)]
      (record-exchange! request response duration-ms :session-id session-id)
      response)))

(defn wrap-record-session
  "Ring middleware to record to a specific session."
  [handler session-id]
  (wrap-record handler :session-id session-id))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable recording."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-max-recordings!
  "Set maximum recordings to keep."
  [max-recordings]
  (swap! state assoc-in [:config :max-recordings] max-recordings))

(defn set-max-body-size!
  "Set maximum body size to record."
  [max-size]
  (swap! state assoc-in [:config :max-body-size] max-size))

(defn set-record-headers!
  "Enable/disable header recording."
  [enabled?]
  (swap! state assoc-in [:config :record-headers?] enabled?))

(defn set-record-body!
  "Enable/disable body recording."
  [enabled?]
  (swap! state assoc-in [:config :record-body?] enabled?))

(defn add-sensitive-header!
  "Add a header to the sensitive list."
  [header]
  (swap! state update-in [:config :sensitive-headers] conj (str/lower-case header)))

(defn set-storage-path!
  "Set the storage path for exports."
  [path]
  (swap! state assoc-in [:config :storage-path] path))

;; ============================================================================
;; Cleanup
;; ============================================================================

(defn clear-recordings!
  "Clear all recordings."
  []
  (swap! state assoc :recordings []))

(defn clear-session-recordings!
  "Clear recordings for a session."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:recordings session) [])))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-recorder-metrics
  "Get recorder metrics."
  []
  (let [stats (:stats @state)]
    {:recordings-made (:recordings-made stats)
     :sessions-created (:sessions-created stats)
     :bytes-recorded (:bytes-recorded stats)
     :playbacks (:playbacks stats)
     :recordings-count (count (:recordings @state))
     :sessions-count (count (:sessions @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-recorder-stats
  "Get recorder statistics."
  []
  (merge (get-recorder-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :max-recordings (get-in @state [:config :max-recordings])
          :max-body-size (get-in @state [:config :max-body-size])
          :record-headers? (get-in @state [:config :record-headers?])
          :record-body? (get-in @state [:config :record-body?])}))

(defn reset-stats!
  "Reset recorder statistics."
  []
  (swap! state assoc :stats {:recordings-made 0
                             :sessions-created 0
                             :bytes-recorded 0
                             :playbacks 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-recorder!
  "Initialize the request recorder."
  []
  (when-not (:initialized? @state)
    ;; Ensure storage directory exists
    (let [storage-path (get-in @state [:config :storage-path])]
      (io/make-parents (str storage-path "/dummy")))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request recorder initialized")
    (events/emit! :request-recorder-initialized {})
    true))
