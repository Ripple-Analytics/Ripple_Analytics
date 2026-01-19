(ns mental-models.pipeline.integration.command-dispatcher
  "Command Dispatcher Module
   
   Command pattern implementation:
   - Command registration
   - Command execution
   - Command queuing
   - Undo/redo support
   - Command history"
  (:require
   [clojure.core.async :as async :refer [go go-loop <! >! chan close!]]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; DISPATCHER STATE
;; =============================================================================

(defonce dispatcher-state (atom {:commands {}
                                 :handlers {}
                                 :queue (chan 1000)
                                 :history []
                                 :undo-stack []
                                 :redo-stack []
                                 :stats {:dispatched 0
                                         :executed 0
                                         :failed 0}}))

;; =============================================================================
;; COMMAND DEFINITION
;; =============================================================================

(defn create-command
  "Create a command."
  [command-type payload & {:keys [metadata undoable]
                           :or {metadata {} undoable false}}]
  {:id (str (java.util.UUID/randomUUID))
   :type command-type
   :payload payload
   :metadata metadata
   :undoable undoable
   :created-at (System/currentTimeMillis)
   :status :pending})

;; =============================================================================
;; HANDLER REGISTRATION
;; =============================================================================

(defn register-handler!
  "Register a command handler."
  [command-type handler & {:keys [undo-handler validator]}]
  (log/info "Registering command handler" {:type command-type})
  (swap! dispatcher-state assoc-in [:handlers command-type]
         {:handler handler
          :undo-handler undo-handler
          :validator validator})
  command-type)

(defn unregister-handler!
  "Unregister a command handler."
  [command-type]
  (log/info "Unregistering command handler" {:type command-type})
  (swap! dispatcher-state update :handlers dissoc command-type))

(defn get-handler
  "Get a handler for a command type."
  [command-type]
  (get-in @dispatcher-state [:handlers command-type]))

(defn list-handlers
  "List all registered handlers."
  []
  (keys (:handlers @dispatcher-state)))

;; =============================================================================
;; COMMAND VALIDATION
;; =============================================================================

(defn validate-command
  "Validate a command before execution."
  [command]
  (let [handler-info (get-handler (:type command))
        validator (:validator handler-info)]
    (if validator
      (try
        (let [result (validator command)]
          (if (or (true? result) (nil? result))
            {:valid true}
            {:valid false :errors result}))
        (catch Exception e
          {:valid false :errors [(.getMessage e)]}))
      {:valid true})))

;; =============================================================================
;; COMMAND EXECUTION
;; =============================================================================

(defn execute-command!
  "Execute a command synchronously."
  [command]
  (when (flags/is-enabled? "command-dispatcher")
    (log/debug "Executing command" {:id (:id command) :type (:type command)})
    (let [handler-info (get-handler (:type command))
          handler (:handler handler-info)]
      (if-not handler
        (do
          (log/error "No handler for command type" {:type (:type command)})
          {:success false :error "No handler registered"})
        (let [validation (validate-command command)]
          (if-not (:valid validation)
            (do
              (log/warn "Command validation failed" {:id (:id command) :errors (:errors validation)})
              {:success false :error "Validation failed" :errors (:errors validation)})
            (try
              (let [start-time (System/currentTimeMillis)
                    result (handler (:payload command))
                    duration (- (System/currentTimeMillis) start-time)
                    executed-command (assoc command
                                            :status :executed
                                            :result result
                                            :executed-at (System/currentTimeMillis)
                                            :duration-ms duration)]
                ;; Update stats
                (swap! dispatcher-state update-in [:stats :executed] inc)
                ;; Add to history
                (swap! dispatcher-state update :history conj executed-command)
                ;; Add to undo stack if undoable
                (when (:undoable command)
                  (swap! dispatcher-state update :undo-stack conj executed-command)
                  (swap! dispatcher-state assoc :redo-stack []))
                ;; Record metrics
                (metrics/inc-counter! :command/executed)
                (metrics/observe-histogram! :command/duration duration)
                ;; Publish event
                (events/publish! :command/executed {:id (:id command) :type (:type command)})
                {:success true :result result :duration-ms duration})
              (catch Exception e
                (log/error "Command execution failed" {:id (:id command) :error (.getMessage e)})
                (swap! dispatcher-state update-in [:stats :failed] inc)
                (metrics/inc-counter! :command/failed)
                {:success false :error (.getMessage e)}))))))))

(defn dispatch!
  "Dispatch a command for execution."
  [command-type payload & opts]
  (let [command (apply create-command command-type payload opts)]
    (swap! dispatcher-state update-in [:stats :dispatched] inc)
    (execute-command! command)))

(defn dispatch-async!
  "Dispatch a command asynchronously."
  [command-type payload & opts]
  (let [command (apply create-command command-type payload opts)]
    (swap! dispatcher-state update-in [:stats :dispatched] inc)
    (go (>! (:queue @dispatcher-state) command))
    (:id command)))

;; =============================================================================
;; COMMAND QUEUE PROCESSING
;; =============================================================================

(defonce queue-processor (atom nil))

(defn start-queue-processor!
  "Start processing the command queue."
  []
  (log/info "Starting command queue processor")
  (let [processor (go-loop []
                    (when-let [command (<! (:queue @dispatcher-state))]
                      (execute-command! command)
                      (recur)))]
    (reset! queue-processor processor)
    processor))

(defn stop-queue-processor!
  "Stop the command queue processor."
  []
  (log/info "Stopping command queue processor")
  (close! (:queue @dispatcher-state))
  (reset! queue-processor nil))

;; =============================================================================
;; UNDO/REDO
;; =============================================================================

(defn can-undo?
  "Check if undo is available."
  []
  (not (empty? (:undo-stack @dispatcher-state))))

(defn can-redo?
  "Check if redo is available."
  []
  (not (empty? (:redo-stack @dispatcher-state))))

(defn undo!
  "Undo the last undoable command."
  []
  (when (can-undo?)
    (let [command (peek (:undo-stack @dispatcher-state))
          handler-info (get-handler (:type command))
          undo-handler (:undo-handler handler-info)]
      (if-not undo-handler
        (do
          (log/warn "No undo handler for command" {:type (:type command)})
          {:success false :error "No undo handler"})
        (try
          (log/info "Undoing command" {:id (:id command) :type (:type command)})
          (let [result (undo-handler (:payload command) (:result command))]
            (swap! dispatcher-state update :undo-stack pop)
            (swap! dispatcher-state update :redo-stack conj command)
            (metrics/inc-counter! :command/undone)
            (events/publish! :command/undone {:id (:id command)})
            {:success true :result result})
          (catch Exception e
            (log/error "Undo failed" {:id (:id command) :error (.getMessage e)})
            {:success false :error (.getMessage e)}))))))

(defn redo!
  "Redo the last undone command."
  []
  (when (can-redo?)
    (let [command (peek (:redo-stack @dispatcher-state))
          handler-info (get-handler (:type command))
          handler (:handler handler-info)]
      (try
        (log/info "Redoing command" {:id (:id command) :type (:type command)})
        (let [result (handler (:payload command))]
          (swap! dispatcher-state update :redo-stack pop)
          (swap! dispatcher-state update :undo-stack conj
                 (assoc command :result result))
          (metrics/inc-counter! :command/redone)
          (events/publish! :command/redone {:id (:id command)})
          {:success true :result result})
        (catch Exception e
          (log/error "Redo failed" {:id (:id command) :error (.getMessage e)})
          {:success false :error (.getMessage e)})))))

;; =============================================================================
;; COMMAND HISTORY
;; =============================================================================

(defn get-history
  "Get command execution history."
  [& {:keys [limit type] :or {limit 100}}]
  (let [history (:history @dispatcher-state)
        filtered (if type
                   (filter #(= (:type %) type) history)
                   history)]
    (take limit (reverse filtered))))

(defn clear-history!
  "Clear command history."
  []
  (log/info "Clearing command history")
  (swap! dispatcher-state assoc :history []))

(defn get-command
  "Get a command by ID from history."
  [command-id]
  (first (filter #(= (:id %) command-id) (:history @dispatcher-state))))

;; =============================================================================
;; BATCH COMMANDS
;; =============================================================================

(defn dispatch-batch!
  "Dispatch multiple commands as a batch."
  [commands]
  (log/info "Dispatching batch" {:count (count commands)})
  (let [results (doall (map (fn [[type payload opts]]
                              (apply dispatch! type payload opts))
                            commands))]
    {:success (every? :success results)
     :results results}))

(defn create-composite-command
  "Create a composite command from multiple commands."
  [name commands]
  (create-command
   :composite
   {:name name :commands commands}
   :undoable true))

;; =============================================================================
;; PREDEFINED HANDLERS
;; =============================================================================

(defn register-analysis-handlers!
  "Register analysis-related command handlers."
  []
  (register-handler! :analyze-document
                     (fn [payload]
                       (log/info "Analyzing document" {:path (:path payload)})
                       {:analyzed true :path (:path payload)}))
  
  (register-handler! :analyze-batch
                     (fn [payload]
                       (log/info "Analyzing batch" {:count (count (:paths payload))})
                       {:analyzed-count (count (:paths payload))}))
  
  (register-handler! :detect-models
                     (fn [payload]
                       (log/info "Detecting models" {:text-length (count (:text payload))})
                       {:models-detected []})))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-command-dispatcher!
  "Initialize command dispatcher."
  []
  (log/info "Initializing command dispatcher")
  ;; Register feature flag
  (flags/register-flag! "command-dispatcher" "Enable command dispatcher" true)
  ;; Create metrics
  (metrics/create-counter! :command/executed "Commands executed")
  (metrics/create-counter! :command/failed "Commands failed")
  (metrics/create-counter! :command/undone "Commands undone")
  (metrics/create-counter! :command/redone "Commands redone")
  (metrics/create-histogram! :command/duration "Command duration" [1 5 10 50 100 500])
  (metrics/create-gauge! :command/queue-size "Command queue size"
                         #(count (:history @dispatcher-state)))
  ;; Register predefined handlers
  (register-analysis-handlers!)
  ;; Start queue processor
  (start-queue-processor!)
  (log/info "Command dispatcher initialized"))

;; =============================================================================
;; SHUTDOWN
;; =============================================================================

(defn shutdown!
  "Shutdown command dispatcher."
  []
  (log/info "Shutting down command dispatcher")
  (stop-queue-processor!))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-dispatcher-status []
  {:enabled (flags/is-enabled? "command-dispatcher")
   :registered-handlers (count (:handlers @dispatcher-state))
   :history-size (count (:history @dispatcher-state))
   :undo-stack-size (count (:undo-stack @dispatcher-state))
   :redo-stack-size (count (:redo-stack @dispatcher-state))
   :stats (:stats @dispatcher-state)})
