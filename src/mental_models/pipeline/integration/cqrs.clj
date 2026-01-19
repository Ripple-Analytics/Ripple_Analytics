(ns mental-models.pipeline.integration.cqrs
  "CQRS Module
   
   Command Query Responsibility Segregation:
   - Command handling
   - Query handling
   - Read/write model separation
   - Command validation
   - Query optimization"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.pipeline.integration.event-sourcing :as es]))

;; =============================================================================
;; CQRS STATE
;; =============================================================================

(defonce cqrs-state (atom {:command-handlers {}
                           :query-handlers {}
                           :read-models {}
                           :write-models {}
                           :validators {}
                           :config {:async-commands true
                                    :cache-queries true
                                    :query-cache-ttl-ms 60000}}))

;; =============================================================================
;; COMMAND HANDLING
;; =============================================================================

(defn register-command-handler!
  "Register a command handler."
  [command-type handler-fn & {:keys [validator]}]
  (log/info "Registering command handler" {:type command-type})
  (swap! cqrs-state assoc-in [:command-handlers command-type]
         {:type command-type
          :handler handler-fn
          :validator validator
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :cqrs/command-handlers-registered))

(defn unregister-command-handler!
  "Unregister a command handler."
  [command-type]
  (log/info "Unregistering command handler" {:type command-type})
  (swap! cqrs-state update :command-handlers dissoc command-type))

(defn get-command-handler
  "Get a command handler."
  [command-type]
  (get-in @cqrs-state [:command-handlers command-type]))

(defn list-command-handlers
  "List all command handlers."
  []
  (keys (:command-handlers @cqrs-state)))

;; =============================================================================
;; COMMAND VALIDATION
;; =============================================================================

(defn register-validator!
  "Register a command validator."
  [command-type validator-fn]
  (log/info "Registering validator" {:type command-type})
  (swap! cqrs-state assoc-in [:validators command-type] validator-fn))

(defn validate-command
  "Validate a command."
  [command]
  (let [command-type (:type command)
        validator (or (get-in @cqrs-state [:validators command-type])
                      (get-in @cqrs-state [:command-handlers command-type :validator]))]
    (if validator
      (let [result (validator command)]
        (if (or (true? result) (nil? result))
          {:valid? true}
          {:valid? false :errors (if (string? result) [result] result)}))
      {:valid? true})))

;; =============================================================================
;; COMMAND EXECUTION
;; =============================================================================

(defn create-command
  "Create a command."
  [command-type payload & {:keys [metadata]}]
  {:id (str (java.util.UUID/randomUUID))
   :type command-type
   :payload payload
   :metadata (merge {:timestamp (System/currentTimeMillis)
                     :correlation-id (str (java.util.UUID/randomUUID))}
                    metadata)})

(defn execute-command!
  "Execute a command."
  [command]
  (when (flags/is-enabled? "cqrs")
    (let [start-time (System/currentTimeMillis)
          command-type (:type command)]
      (log/info "Executing command" {:type command-type :id (:id command)})
      (metrics/inc-counter! :cqrs/commands-executed)
      ;; Validate command
      (let [validation (validate-command command)]
        (if (:valid? validation)
          ;; Find handler
          (if-let [handler-info (get-command-handler command-type)]
            (try
              (let [result ((:handler handler-info) command)
                    duration (- (System/currentTimeMillis) start-time)]
                (metrics/observe-histogram! :cqrs/command-duration duration)
                (events/publish! :cqrs/command-executed {:type command-type :duration duration})
                {:success true :result result})
              (catch Exception e
                (log/error "Command execution failed" {:type command-type :error (.getMessage e)})
                (metrics/inc-counter! :cqrs/command-failures)
                {:success false :error (.getMessage e)}))
            (do
              (log/warn "No handler for command" {:type command-type})
              {:success false :error "No handler registered"}))
          (do
            (metrics/inc-counter! :cqrs/validation-failures)
            {:success false :error "Validation failed" :details (:errors validation)}))))))

(defn dispatch-command!
  "Dispatch a command (alias for execute-command!)."
  [command-type payload & opts]
  (execute-command! (apply create-command command-type payload opts)))

;; =============================================================================
;; QUERY HANDLING
;; =============================================================================

(defn register-query-handler!
  "Register a query handler."
  [query-type handler-fn & {:keys [cache-ttl-ms]}]
  (log/info "Registering query handler" {:type query-type})
  (swap! cqrs-state assoc-in [:query-handlers query-type]
         {:type query-type
          :handler handler-fn
          :cache-ttl-ms cache-ttl-ms
          :registered-at (System/currentTimeMillis)})
  (metrics/inc-counter! :cqrs/query-handlers-registered))

(defn unregister-query-handler!
  "Unregister a query handler."
  [query-type]
  (log/info "Unregistering query handler" {:type query-type})
  (swap! cqrs-state update :query-handlers dissoc query-type))

(defn get-query-handler
  "Get a query handler."
  [query-type]
  (get-in @cqrs-state [:query-handlers query-type]))

(defn list-query-handlers
  "List all query handlers."
  []
  (keys (:query-handlers @cqrs-state)))

;; =============================================================================
;; QUERY EXECUTION
;; =============================================================================

(defn create-query
  "Create a query."
  [query-type params & {:keys [metadata]}]
  {:id (str (java.util.UUID/randomUUID))
   :type query-type
   :params params
   :metadata (merge {:timestamp (System/currentTimeMillis)}
                    metadata)})

(defonce query-cache (atom {}))

(defn cache-key
  "Generate a cache key for a query."
  [query]
  (str (:type query) ":" (pr-str (:params query))))

(defn get-cached-result
  "Get a cached query result."
  [query ttl-ms]
  (let [key (cache-key query)
        cached (get @query-cache key)]
    (when (and cached
               (< (- (System/currentTimeMillis) (:timestamp cached)) ttl-ms))
      (:result cached))))

(defn cache-result!
  "Cache a query result."
  [query result]
  (swap! query-cache assoc (cache-key query)
         {:result result :timestamp (System/currentTimeMillis)}))

(defn execute-query
  "Execute a query."
  [query]
  (when (flags/is-enabled? "cqrs")
    (let [start-time (System/currentTimeMillis)
          query-type (:type query)]
      (log/debug "Executing query" {:type query-type :id (:id query)})
      (metrics/inc-counter! :cqrs/queries-executed)
      ;; Find handler
      (if-let [handler-info (get-query-handler query-type)]
        (let [cache-ttl (or (:cache-ttl-ms handler-info)
                            (get-in @cqrs-state [:config :query-cache-ttl-ms]))
              ;; Check cache
              cached (when (get-in @cqrs-state [:config :cache-queries])
                       (get-cached-result query cache-ttl))]
          (if cached
            (do
              (metrics/inc-counter! :cqrs/query-cache-hits)
              {:success true :result cached :cached true})
            (try
              (let [result ((:handler handler-info) query)
                    duration (- (System/currentTimeMillis) start-time)]
                ;; Cache result
                (when (get-in @cqrs-state [:config :cache-queries])
                  (cache-result! query result))
                (metrics/observe-histogram! :cqrs/query-duration duration)
                {:success true :result result :cached false})
              (catch Exception e
                (log/error "Query execution failed" {:type query-type :error (.getMessage e)})
                (metrics/inc-counter! :cqrs/query-failures)
                {:success false :error (.getMessage e)}))))
        (do
          (log/warn "No handler for query" {:type query-type})
          {:success false :error "No handler registered"})))))

(defn query
  "Execute a query (convenience function)."
  [query-type params & opts]
  (execute-query (apply create-query query-type params opts)))

;; =============================================================================
;; READ MODELS
;; =============================================================================

(defn register-read-model!
  "Register a read model."
  [model-id {:keys [initial-state event-handlers]}]
  (log/info "Registering read model" {:id model-id})
  (swap! cqrs-state assoc-in [:read-models model-id]
         {:id model-id
          :state initial-state
          :event-handlers event-handlers
          :created-at (System/currentTimeMillis)})
  ;; Register event handlers
  (doseq [[event-type handler] event-handlers]
    (es/register-handler! (str model-id "-" (name event-type)) event-type
                          (fn [event]
                            (swap! cqrs-state update-in [:read-models model-id :state]
                                   handler event)))))

(defn get-read-model-state
  "Get the current state of a read model."
  [model-id]
  (get-in @cqrs-state [:read-models model-id :state]))

(defn rebuild-read-model!
  "Rebuild a read model from events."
  [model-id]
  (log/info "Rebuilding read model" {:id model-id})
  (when-let [model (get-in @cqrs-state [:read-models model-id])]
    (let [initial-state (:initial-state (get-in @cqrs-state [:read-models model-id]))
          events (es/get-events)
          handlers (:event-handlers model)
          new-state (reduce (fn [state event]
                              (if-let [handler (get handlers (:type event))]
                                (handler state event)
                                state))
                            initial-state
                            events)]
      (swap! cqrs-state assoc-in [:read-models model-id :state] new-state)
      new-state)))

;; =============================================================================
;; WRITE MODELS
;; =============================================================================

(defn register-write-model!
  "Register a write model."
  [model-id {:keys [aggregate-type command-handlers]}]
  (log/info "Registering write model" {:id model-id})
  (swap! cqrs-state assoc-in [:write-models model-id]
         {:id model-id
          :aggregate-type aggregate-type
          :command-handlers command-handlers
          :created-at (System/currentTimeMillis)})
  ;; Register command handlers
  (doseq [[command-type handler] command-handlers]
    (register-command-handler! command-type
                               (fn [command]
                                 (let [events (handler command)]
                                   (doseq [event events]
                                     (es/append-event! (es/create-event (:type event)
                                                                        (:aggregate-id command)
                                                                        (:data event))))
                                   events)))))

(defn get-write-model
  "Get a write model."
  [model-id]
  (get-in @cqrs-state [:write-models model-id]))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-cqrs!
  "Initialize CQRS."
  []
  (log/info "Initializing CQRS")
  ;; Register feature flag
  (flags/register-flag! "cqrs" "Enable CQRS" true)
  ;; Create metrics
  (metrics/create-counter! :cqrs/command-handlers-registered "Command handlers registered")
  (metrics/create-counter! :cqrs/query-handlers-registered "Query handlers registered")
  (metrics/create-counter! :cqrs/commands-executed "Commands executed")
  (metrics/create-counter! :cqrs/queries-executed "Queries executed")
  (metrics/create-counter! :cqrs/command-failures "Command failures")
  (metrics/create-counter! :cqrs/query-failures "Query failures")
  (metrics/create-counter! :cqrs/validation-failures "Validation failures")
  (metrics/create-counter! :cqrs/query-cache-hits "Query cache hits")
  (metrics/create-histogram! :cqrs/command-duration "Command duration" [1 5 10 50 100 500])
  (metrics/create-histogram! :cqrs/query-duration "Query duration" [1 5 10 50 100 500])
  (log/info "CQRS initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-cqrs-status []
  {:enabled (flags/is-enabled? "cqrs")
   :command-handlers (count (:command-handlers @cqrs-state))
   :query-handlers (count (:query-handlers @cqrs-state))
   :read-models (count (:read-models @cqrs-state))
   :write-models (count (:write-models @cqrs-state))
   :validators (count (:validators @cqrs-state))
   :query-cache-size (count @query-cache)
   :config (:config @cqrs-state)})
