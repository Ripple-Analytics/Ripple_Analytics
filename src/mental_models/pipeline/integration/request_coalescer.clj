(ns mental-models.pipeline.integration.request-coalescer
  "Request coalescer for mental model analysis system.
   
   Features:
   - Request batching
   - Coalescing window
   - Key-based grouping
   - Result distribution
   - Timeout handling
   - Batch size limits
   - Coalescing metrics
   - Async coalescing"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent ConcurrentHashMap]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:coalescers {}       ;; coalescer-id -> coalescer
         :config {:default-window-ms 100
                  :default-max-batch-size 100
                  :default-timeout-ms 5000}
         :stats {:requests-received 0
                 :requests-coalesced 0
                 :batches-executed 0
                 :batch-sizes []}
         :initialized? false}))

;; ============================================================================
;; Coalescer Creation
;; ============================================================================

(defn create-coalescer!
  "Create a request coalescer."
  [coalescer-id config]
  (let [window-ms (get config :window-ms
                       (get-in @state [:config :default-window-ms]))
        max-batch-size (get config :max-batch-size
                            (get-in @state [:config :default-max-batch-size]))
        timeout-ms (get config :timeout-ms
                        (get-in @state [:config :default-timeout-ms]))
        
        coalescer {:id coalescer-id
                   :name (get config :name (name coalescer-id))
                   :window-ms window-ms
                   :max-batch-size max-batch-size
                   :timeout-ms timeout-ms
                   :batch-fn (get config :batch-fn)
                   :key-fn (get config :key-fn identity)
                   :pending (ConcurrentHashMap.)  ;; key -> {:requests [] :promises []}
                   :enabled? (atom true)
                   :metrics {:received (atom 0)
                             :coalesced (atom 0)
                             :batches (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:coalescers coalescer-id] coalescer)
    (logging/log :info "Created coalescer" {:coalescer-id coalescer-id :window-ms window-ms})
    (events/emit! :coalescer-created {:coalescer-id coalescer-id})
    coalescer-id))

(defn get-coalescer
  "Get a coalescer."
  [coalescer-id]
  (get-in @state [:coalescers coalescer-id]))

(defn list-coalescers
  "List all coalescers."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :window-ms (:window-ms c)
           :max-batch-size (:max-batch-size c)
           :pending-count (.size (:pending c))
           :enabled? @(:enabled? c)})
        (:coalescers @state)))

(defn delete-coalescer!
  "Delete a coalescer."
  [coalescer-id]
  (when-let [coalescer (get-coalescer coalescer-id)]
    (reset! (:enabled? coalescer) false)
    (swap! state update :coalescers dissoc coalescer-id)
    (logging/log :info "Deleted coalescer" {:coalescer-id coalescer-id})))

;; ============================================================================
;; Batch Execution
;; ============================================================================

(defn- execute-batch!
  "Execute a batch of coalesced requests."
  [coalescer key requests promises]
  (swap! (get-in coalescer [:metrics :batches]) inc)
  (swap! state update-in [:stats :batches-executed] inc)
  (swap! state update-in [:stats :batch-sizes] conj (count requests))
  
  (logging/log :debug "Executing batch" {:coalescer-id (:id coalescer)
                                          :key key
                                          :batch-size (count requests)})
  
  (try
    (let [batch-fn (:batch-fn coalescer)
          results (if batch-fn
                    (batch-fn key requests)
                    requests)]
      ;; Distribute results to waiting promises
      (doseq [[promise result] (map vector promises results)]
        (deliver promise {:status :success :result result}))
      (logging/log :debug "Batch completed" {:coalescer-id (:id coalescer)
                                              :key key
                                              :results-count (count results)}))
    (catch Exception e
      (logging/log :error "Batch failed" {:coalescer-id (:id coalescer)
                                           :key key
                                           :error (.getMessage e)})
      ;; Deliver error to all waiting promises
      (doseq [promise promises]
        (deliver promise {:status :error :error (.getMessage e)})))))

(defn- schedule-batch!
  "Schedule a batch for execution after the coalescing window."
  [coalescer key]
  (go
    (<! (timeout (:window-ms coalescer)))
    (when @(:enabled? coalescer)
      (let [pending (:pending coalescer)
            entry (.remove pending key)]
        (when entry
          (let [{:keys [requests promises]} entry]
            (execute-batch! coalescer key requests promises)))))))

;; ============================================================================
;; Request Coalescing
;; ============================================================================

(defn coalesce
  "Coalesce a request with others."
  [coalescer-id request]
  (swap! state update-in [:stats :requests-received] inc)
  
  (if-let [coalescer (get-coalescer coalescer-id)]
    (if @(:enabled? coalescer)
      (let [key-fn (:key-fn coalescer)
            key (key-fn request)
            pending (:pending coalescer)
            result-promise (promise)
            max-batch-size (:max-batch-size coalescer)]
        
        (swap! (get-in coalescer [:metrics :received]) inc)
        
        ;; Add to pending batch
        (let [entry (.compute pending key
                              (fn [_ existing]
                                (if existing
                                  (do
                                    (swap! (get-in coalescer [:metrics :coalesced]) inc)
                                    (swap! state update-in [:stats :requests-coalesced] inc)
                                    {:requests (conj (:requests existing) request)
                                     :promises (conj (:promises existing) result-promise)
                                     :scheduled? true})
                                  {:requests [request]
                                   :promises [result-promise]
                                   :scheduled? false})))]
          
          ;; Schedule batch if this is the first request for this key
          (when-not (:scheduled? entry)
            (schedule-batch! coalescer key))
          
          ;; Execute immediately if batch is full
          (when (>= (count (:requests entry)) max-batch-size)
            (let [removed (.remove pending key)]
              (when removed
                (execute-batch! coalescer key (:requests removed) (:promises removed)))))
          
          ;; Wait for result with timeout
          (let [timeout-ms (:timeout-ms coalescer)
                result (deref result-promise timeout-ms {:status :timeout})]
            (if (= :success (:status result))
              (:result result)
              (throw (ex-info "Coalescing failed" result))))))
      (throw (ex-info "Coalescer disabled" {:coalescer-id coalescer-id})))
    (throw (ex-info "Coalescer not found" {:coalescer-id coalescer-id}))))

(defn coalesce-async
  "Coalesce a request asynchronously."
  [coalescer-id request callback]
  (go
    (try
      (let [result (coalesce coalescer-id request)]
        (callback {:status :success :result result}))
      (catch Exception e
        (callback {:status :error :error (.getMessage e)})))))

;; ============================================================================
;; Batch Function Helpers
;; ============================================================================

(defn batch-by-key
  "Create a batch function that groups by key."
  [process-fn]
  (fn [key requests]
    (process-fn key requests)))

(defn batch-parallel
  "Create a batch function that processes in parallel."
  [process-fn]
  (fn [key requests]
    (let [futures (mapv #(future (process-fn %)) requests)]
      (mapv deref futures))))

(defn batch-sequential
  "Create a batch function that processes sequentially."
  [process-fn]
  (fn [key requests]
    (mapv process-fn requests)))

;; ============================================================================
;; Coalescer Control
;; ============================================================================

(defn enable-coalescer!
  "Enable a coalescer."
  [coalescer-id]
  (when-let [coalescer (get-coalescer coalescer-id)]
    (reset! (:enabled? coalescer) true)
    (logging/log :info "Enabled coalescer" {:coalescer-id coalescer-id})))

(defn disable-coalescer!
  "Disable a coalescer."
  [coalescer-id]
  (when-let [coalescer (get-coalescer coalescer-id)]
    (reset! (:enabled? coalescer) false)
    (logging/log :info "Disabled coalescer" {:coalescer-id coalescer-id})))

(defn flush-coalescer!
  "Flush pending requests for a coalescer."
  [coalescer-id]
  (when-let [coalescer (get-coalescer coalescer-id)]
    (let [pending (:pending coalescer)
          keys (vec (.keySet pending))]
      (doseq [key keys]
        (when-let [entry (.remove pending key)]
          (execute-batch! coalescer key (:requests entry) (:promises entry))))
      (logging/log :info "Flushed coalescer" {:coalescer-id coalescer-id :keys-flushed (count keys)}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-window!
  "Set the coalescing window for a coalescer."
  [coalescer-id window-ms]
  (swap! state assoc-in [:coalescers coalescer-id :window-ms] window-ms))

(defn set-max-batch-size!
  "Set the maximum batch size for a coalescer."
  [coalescer-id max-batch-size]
  (swap! state assoc-in [:coalescers coalescer-id :max-batch-size] max-batch-size))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-coalescer-metrics
  "Get metrics for a coalescer."
  [coalescer-id]
  (when-let [coalescer (get-coalescer coalescer-id)]
    {:coalescer-id coalescer-id
     :name (:name coalescer)
     :pending-count (.size (:pending coalescer))
     :received @(get-in coalescer [:metrics :received])
     :coalesced @(get-in coalescer [:metrics :coalesced])
     :batches @(get-in coalescer [:metrics :batches])
     :enabled? @(:enabled? coalescer)}))

(defn get-all-coalescer-metrics
  "Get metrics for all coalescers."
  []
  (mapv (fn [[id _]] (get-coalescer-metrics id)) (:coalescers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-coalescer-stats
  "Get coalescer statistics."
  []
  (let [stats (:stats @state)
        batch-sizes (:batch-sizes stats)]
    {:coalescers-count (count (:coalescers @state))
     :requests-received (:requests-received stats)
     :requests-coalesced (:requests-coalesced stats)
     :batches-executed (:batches-executed stats)
     :coalescing-rate (if (pos? (:requests-received stats))
                        (/ (:requests-coalesced stats) (:requests-received stats))
                        0)
     :avg-batch-size (if (seq batch-sizes)
                       (/ (reduce + batch-sizes) (count batch-sizes))
                       0)}))

(defn reset-stats!
  "Reset coalescer statistics."
  []
  (swap! state assoc :stats {:requests-received 0
                             :requests-coalesced 0
                             :batches-executed 0
                             :batch-sizes []}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-coalescer!
  "Initialize the request coalescer system."
  []
  (when-not (:initialized? @state)
    ;; Create default coalescers
    (create-coalescer! :analysis
                       {:name "Analysis"
                        :window-ms 100
                        :max-batch-size 50
                        :key-fn :model-id
                        :batch-fn (fn [key requests]
                                    (logging/log :debug "Processing analysis batch"
                                                 {:model-id key :count (count requests)})
                                    requests)})
    
    (create-coalescer! :lm-studio
                       {:name "LM Studio"
                        :window-ms 50
                        :max-batch-size 10
                        :key-fn :prompt-type
                        :batch-fn (fn [key requests]
                                    (logging/log :debug "Processing LM Studio batch"
                                                 {:prompt-type key :count (count requests)})
                                    requests)})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request coalescer initialized")
    (events/emit! :request-coalescer-initialized {})
    true))
