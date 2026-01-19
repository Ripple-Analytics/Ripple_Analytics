(ns mental-models.pipeline.integration.request-debugger
  "Request debugger for mental model analysis system.
   
   Features:
   - Request debugging
   - Breakpoints
   - Step-through execution
   - Variable inspection
   - Call stack tracking
   - Debug sessions
   - Debug logging
   - Debugging metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
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
  (atom {:sessions {}         ;; session-id -> debug session
         :breakpoints {}      ;; breakpoint-id -> breakpoint
         :watches {}          ;; watch-id -> watch expression
         :config {:enabled? true
                  :log-level :debug
                  :max-stack-depth 100
                  :capture-locals? true
                  :pretty-print? true}
         :stats {:sessions-created 0
                 :breakpoints-hit 0
                 :steps-executed 0
                 :inspections 0}
         :initialized? false}))

;; ============================================================================
;; Debug Sessions
;; ============================================================================

(defn create-session!
  "Create a debug session."
  [& {:keys [name request]}]
  (let [session-id (str (UUID/randomUUID))
        session {:id session-id
                 :name (or name (str "Debug Session " session-id))
                 :request request
                 :state :created
                 :call-stack (atom [])
                 :locals (atom {})
                 :history (atom [])
                 :current-step (atom 0)
                 :paused? (atom false)
                 :continue-chan (chan)
                 :created-at (System/currentTimeMillis)
                 :ended-at (atom nil)}]
    
    (swap! state assoc-in [:sessions session-id] session)
    (swap! state update-in [:stats :sessions-created] inc)
    (logging/log :debug "Created debug session" {:session-id session-id})
    session-id))

(defn get-session
  "Get a debug session."
  [session-id]
  (get-in @state [:sessions session-id]))

(defn list-sessions
  "List all debug sessions."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :state (:state s)
           :paused? @(:paused? s)
           :steps (count @(:history s))
           :created-at (:created-at s)})
        (:sessions @state)))

(defn end-session!
  "End a debug session."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:ended-at session) (System/currentTimeMillis))
    (close! (:continue-chan session))
    (swap! state assoc-in [:sessions session-id :state] :ended)))

(defn delete-session!
  "Delete a debug session."
  [session-id]
  (when-let [session (get-session session-id)]
    (close! (:continue-chan session)))
  (swap! state update :sessions dissoc session-id))

;; ============================================================================
;; Breakpoints
;; ============================================================================

(defn set-breakpoint!
  "Set a breakpoint."
  [breakpoint-id config]
  (let [breakpoint {:id breakpoint-id
                    :name (get config :name (name breakpoint-id))
                    :condition (get config :condition (constantly true))
                    :action (get config :action :pause)
                    :hit-count (atom 0)
                    :enabled? (atom true)
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:breakpoints breakpoint-id] breakpoint)
    (logging/log :debug "Set breakpoint" {:breakpoint-id breakpoint-id})
    breakpoint-id))

(defn get-breakpoint
  "Get a breakpoint."
  [breakpoint-id]
  (get-in @state [:breakpoints breakpoint-id]))

(defn list-breakpoints
  "List all breakpoints."
  []
  (mapv (fn [[id b]]
          {:id id
           :name (:name b)
           :enabled? @(:enabled? b)
           :hit-count @(:hit-count b)})
        (:breakpoints @state)))

(defn enable-breakpoint!
  "Enable a breakpoint."
  [breakpoint-id]
  (when-let [bp (get-breakpoint breakpoint-id)]
    (reset! (:enabled? bp) true)))

(defn disable-breakpoint!
  "Disable a breakpoint."
  [breakpoint-id]
  (when-let [bp (get-breakpoint breakpoint-id)]
    (reset! (:enabled? bp) false)))

(defn delete-breakpoint!
  "Delete a breakpoint."
  [breakpoint-id]
  (swap! state update :breakpoints dissoc breakpoint-id))

(defn clear-breakpoints!
  "Clear all breakpoints."
  []
  (swap! state assoc :breakpoints {}))

;; ============================================================================
;; Watches
;; ============================================================================

(defn add-watch-expr!
  "Add a watch expression."
  [watch-id expr-fn]
  (let [watch {:id watch-id
               :expr-fn expr-fn
               :last-value (atom nil)
               :enabled? (atom true)
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:watches watch-id] watch)
    watch-id))

(defn get-watch
  "Get a watch."
  [watch-id]
  (get-in @state [:watches watch-id]))

(defn evaluate-watch
  "Evaluate a watch expression."
  [watch-id context]
  (when-let [watch (get-watch watch-id)]
    (when @(:enabled? watch)
      (try
        (let [value ((:expr-fn watch) context)]
          (reset! (:last-value watch) value)
          value)
        (catch Exception e
          {:error (.getMessage e)})))))

(defn evaluate-all-watches
  "Evaluate all watches."
  [context]
  (into {}
        (for [[id watch] (:watches @state)
              :when @(:enabled? watch)]
          [id (evaluate-watch id context)])))

(defn delete-watch!
  "Delete a watch."
  [watch-id]
  (swap! state update :watches dissoc watch-id))

;; ============================================================================
;; Call Stack
;; ============================================================================

(defn push-frame!
  "Push a frame onto the call stack."
  [session-id frame]
  (when-let [session (get-session session-id)]
    (let [max-depth (get-in @state [:config :max-stack-depth])]
      (swap! (:call-stack session)
             (fn [stack]
               (let [new-stack (conj stack frame)]
                 (if (> (count new-stack) max-depth)
                   (vec (drop 1 new-stack))
                   new-stack)))))))

(defn pop-frame!
  "Pop a frame from the call stack."
  [session-id]
  (when-let [session (get-session session-id)]
    (let [frame (peek @(:call-stack session))]
      (swap! (:call-stack session) pop)
      frame)))

(defn get-call-stack
  "Get the call stack for a session."
  [session-id]
  (when-let [session (get-session session-id)]
    @(:call-stack session)))

(defn get-current-frame
  "Get the current frame."
  [session-id]
  (when-let [session (get-session session-id)]
    (peek @(:call-stack session))))

;; ============================================================================
;; Locals and Variables
;; ============================================================================

(defn set-local!
  "Set a local variable."
  [session-id name value]
  (when-let [session (get-session session-id)]
    (swap! (:locals session) assoc name value)))

(defn get-local
  "Get a local variable."
  [session-id name]
  (when-let [session (get-session session-id)]
    (get @(:locals session) name)))

(defn get-all-locals
  "Get all local variables."
  [session-id]
  (when-let [session (get-session session-id)]
    @(:locals session)))

(defn clear-locals!
  "Clear all local variables."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:locals session) {})))

;; ============================================================================
;; Step Execution
;; ============================================================================

(defn record-step!
  "Record a debug step."
  [session-id step-info]
  (when-let [session (get-session session-id)]
    (swap! state update-in [:stats :steps-executed] inc)
    (swap! (:current-step session) inc)
    (swap! (:history session) conj
           (assoc step-info
                  :step-number @(:current-step session)
                  :timestamp (System/currentTimeMillis)
                  :locals (when (get-in @state [:config :capture-locals?])
                            @(:locals session))
                  :call-stack-depth (count @(:call-stack session))))))

(defn get-step-history
  "Get step history for a session."
  [session-id & {:keys [limit] :or {limit 100}}]
  (when-let [session (get-session session-id)]
    (take-last limit @(:history session))))

;; ============================================================================
;; Pause and Continue
;; ============================================================================

(defn pause!
  "Pause execution at a debug point."
  [session-id & {:keys [reason]}]
  (when-let [session (get-session session-id)]
    (reset! (:paused? session) true)
    (swap! state assoc-in [:sessions session-id :state] :paused)
    (logging/log :debug "Paused at debug point" {:session-id session-id :reason reason})
    
    ;; Wait for continue signal
    (async/<!! (:continue-chan session))))

(defn continue!
  "Continue execution."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:paused? session) false)
    (swap! state assoc-in [:sessions session-id :state] :running)
    (async/>!! (:continue-chan session) :continue)))

(defn step-over!
  "Step over to next statement."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:paused? session) false)
    (async/>!! (:continue-chan session) :step-over)))

(defn step-into!
  "Step into function call."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:paused? session) false)
    (async/>!! (:continue-chan session) :step-into)))

(defn step-out!
  "Step out of current function."
  [session-id]
  (when-let [session (get-session session-id)]
    (reset! (:paused? session) false)
    (async/>!! (:continue-chan session) :step-out)))

;; ============================================================================
;; Inspection
;; ============================================================================

(defn inspect
  "Inspect a value."
  [value & {:keys [depth] :or {depth 3}}]
  (swap! state update-in [:stats :inspections] inc)
  
  (letfn [(inspect-value [v d]
            (if (zero? d)
              {:type (type v) :value "<max-depth>"}
              (cond
                (nil? v) {:type :nil :value nil}
                (boolean? v) {:type :boolean :value v}
                (number? v) {:type :number :value v}
                (string? v) {:type :string :value v :length (count v)}
                (keyword? v) {:type :keyword :value v}
                (symbol? v) {:type :symbol :value v}
                (map? v) {:type :map
                          :count (count v)
                          :keys (keys v)
                          :entries (into {}
                                         (for [[k val] (take 10 v)]
                                           [k (inspect-value val (dec d))]))}
                (sequential? v) {:type :sequential
                                 :count (count v)
                                 :items (mapv #(inspect-value % (dec d)) (take 10 v))}
                (set? v) {:type :set
                          :count (count v)
                          :items (mapv #(inspect-value % (dec d)) (take 10 v))}
                :else {:type (type v) :value (str v)})))]
    (inspect-value value depth)))

(defn inspect-request
  "Inspect a request."
  [request]
  {:method (:request-method request)
   :uri (:uri request)
   :headers (inspect (:headers request))
   :body (inspect (:body request))
   :query-params (inspect (:query-params request))})

(defn inspect-response
  "Inspect a response."
  [response]
  {:status (:status response)
   :headers (inspect (:headers response))
   :body (inspect (:body response))})

;; ============================================================================
;; Debug Logging
;; ============================================================================

(defn debug-log
  "Log a debug message."
  [session-id level message & {:keys [data]}]
  (let [log-entry {:level level
                   :message message
                   :data data
                   :timestamp (System/currentTimeMillis)}]
    (record-step! session-id {:type :log :entry log-entry})
    (when (get-in @state [:config :pretty-print?])
      (pprint/pprint log-entry))))

(defn debug-trace
  "Trace function entry/exit."
  [session-id fn-name args]
  (record-step! session-id {:type :trace
                            :fn-name fn-name
                            :args args}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-debug
  "Ring middleware for debugging."
  [handler]
  (fn [request]
    (if (get-in @state [:config :enabled?])
      (let [session-id (create-session! :request request)]
        (try
          (record-step! session-id {:type :request-start :request (inspect-request request)})
          
          ;; Check breakpoints
          (doseq [[bp-id bp] (:breakpoints @state)
                  :when (and @(:enabled? bp)
                             ((:condition bp) request))]
            (swap! (:hit-count bp) inc)
            (swap! state update-in [:stats :breakpoints-hit] inc)
            (when (= (:action bp) :pause)
              (pause! session-id :reason (str "Breakpoint: " bp-id))))
          
          (let [response (handler request)]
            (record-step! session-id {:type :request-end :response (inspect-response response)})
            (end-session! session-id)
            response)
          
          (catch Exception e
            (record-step! session-id {:type :error :error (.getMessage e)})
            (end-session! session-id)
            (throw e))))
      (handler request))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable debugging."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-log-level!
  "Set debug log level."
  [level]
  (swap! state assoc-in [:config :log-level] level))

(defn set-capture-locals!
  "Enable/disable local capture."
  [enabled?]
  (swap! state assoc-in [:config :capture-locals?] enabled?))

(defn set-pretty-print!
  "Enable/disable pretty printing."
  [enabled?]
  (swap! state assoc-in [:config :pretty-print?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-debugger-metrics
  "Get debugger metrics."
  []
  (let [stats (:stats @state)]
    {:sessions-created (:sessions-created stats)
     :breakpoints-hit (:breakpoints-hit stats)
     :steps-executed (:steps-executed stats)
     :inspections (:inspections stats)
     :active-sessions (count (filter (fn [[_ s]] (not= (:state s) :ended))
                                     (:sessions @state)))
     :breakpoints-count (count (:breakpoints @state))
     :watches-count (count (:watches @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-debugger-stats
  "Get debugger statistics."
  []
  (merge (get-debugger-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :log-level (get-in @state [:config :log-level])
          :capture-locals? (get-in @state [:config :capture-locals?])}))

(defn reset-stats!
  "Reset debugger statistics."
  []
  (swap! state assoc :stats {:sessions-created 0
                             :breakpoints-hit 0
                             :steps-executed 0
                             :inspections 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-debugger!
  "Initialize the request debugger."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request debugger initialized")
    (events/emit! :request-debugger-initialized {})
    true))
