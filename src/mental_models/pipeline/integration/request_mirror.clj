(ns mental-models.pipeline.integration.request-mirror
  "Request mirror for mental model analysis system.
   
   Features:
   - Request mirroring
   - Shadow traffic
   - A/B testing support
   - Mirror targets
   - Async mirroring
   - Mirror comparison
   - Mirror filtering
   - Mirroring metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data :as data]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.concurrent Executors ExecutorService]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:targets {}          ;; target-id -> target config
         :comparisons []      ;; comparison results
         :config {:enabled? true
                  :async? true
                  :sample-rate 1.0
                  :timeout-ms 5000
                  :compare-responses? true
                  :max-comparisons 10000}
         :stats {:requests-mirrored 0
                 :mirrors-sent 0
                 :mirrors-succeeded 0
                 :mirrors-failed 0
                 :comparisons-made 0
                 :differences-found 0}
         :executor nil
         :initialized? false}))

;; ============================================================================
;; Mirror Targets
;; ============================================================================

(defn register-target!
  "Register a mirror target."
  [target-id config]
  (let [target {:id target-id
                :name (get config :name (name target-id))
                :handler (get config :handler)
                :url (get config :url)
                :transform-fn (get config :transform-fn identity)
                :filter-fn (get config :filter-fn (constantly true))
                :weight (get config :weight 1.0)
                :enabled? (atom true)
                :metrics {:sent (atom 0)
                          :succeeded (atom 0)
                          :failed (atom 0)
                          :avg-latency-ms (atom 0)}
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:targets target-id] target)
    (logging/log :info "Registered mirror target" {:target-id target-id})
    target-id))

(defn get-target
  "Get a mirror target."
  [target-id]
  (get-in @state [:targets target-id]))

(defn list-targets
  "List all mirror targets."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :enabled? @(:enabled? t)
           :weight (:weight t)
           :sent @(get-in t [:metrics :sent])
           :succeeded @(get-in t [:metrics :succeeded])
           :failed @(get-in t [:metrics :failed])})
        (:targets @state)))

(defn enable-target!
  "Enable a mirror target."
  [target-id]
  (when-let [target (get-target target-id)]
    (reset! (:enabled? target) true)))

(defn disable-target!
  "Disable a mirror target."
  [target-id]
  (when-let [target (get-target target-id)]
    (reset! (:enabled? target) false)))

(defn delete-target!
  "Delete a mirror target."
  [target-id]
  (swap! state update :targets dissoc target-id))

;; ============================================================================
;; Request Mirroring
;; ============================================================================

(defn- should-mirror?
  "Check if a request should be mirrored."
  [request]
  (and (get-in @state [:config :enabled?])
       (< (rand) (get-in @state [:config :sample-rate]))))

(defn- send-to-target
  "Send a request to a mirror target."
  [target request]
  (let [start-time (System/currentTimeMillis)
        transformed ((:transform-fn target) request)]
    (try
      (let [response (if (:handler target)
                       ((:handler target) transformed)
                       ;; If URL is provided, would make HTTP call here
                       {:status 200 :body {:mirrored true}})]
        (let [latency (- (System/currentTimeMillis) start-time)]
          (swap! (get-in target [:metrics :sent]) inc)
          (swap! (get-in target [:metrics :succeeded]) inc)
          ;; Update average latency
          (let [current-avg @(get-in target [:metrics :avg-latency-ms])
                succeeded @(get-in target [:metrics :succeeded])]
            (reset! (get-in target [:metrics :avg-latency-ms])
                    (/ (+ (* current-avg (dec succeeded)) latency) succeeded)))
          {:success true
           :target-id (:id target)
           :response response
           :latency-ms latency}))
      (catch Exception e
        (swap! (get-in target [:metrics :sent]) inc)
        (swap! (get-in target [:metrics :failed]) inc)
        {:success false
         :target-id (:id target)
         :error (.getMessage e)
         :latency-ms (- (System/currentTimeMillis) start-time)}))))

(defn mirror-request
  "Mirror a request to all enabled targets."
  [request]
  (when (should-mirror? request)
    (swap! state update-in [:stats :requests-mirrored] inc)
    
    (let [targets (->> (vals (:targets @state))
                       (filter #@(:enabled? %))
                       (filter #((:filter-fn %) request)))]
      
      (swap! state update-in [:stats :mirrors-sent] + (count targets))
      
      (if (get-in @state [:config :async?])
        ;; Async mirroring
        (let [executor (or (:executor @state)
                           (Executors/newCachedThreadPool))]
          (doseq [target targets]
            (.submit executor
                     (fn []
                       (let [result (send-to-target target request)]
                         (if (:success result)
                           (swap! state update-in [:stats :mirrors-succeeded] inc)
                           (swap! state update-in [:stats :mirrors-failed] inc)))))))
        
        ;; Sync mirroring
        (mapv #(send-to-target % request) targets)))))

(defn mirror-request-sync
  "Mirror a request synchronously and return results."
  [request]
  (when (should-mirror? request)
    (swap! state update-in [:stats :requests-mirrored] inc)
    
    (let [targets (->> (vals (:targets @state))
                       (filter #@(:enabled? %))
                       (filter #((:filter-fn %) request)))]
      
      (swap! state update-in [:stats :mirrors-sent] + (count targets))
      
      (mapv (fn [target]
              (let [result (send-to-target target request)]
                (if (:success result)
                  (swap! state update-in [:stats :mirrors-succeeded] inc)
                  (swap! state update-in [:stats :mirrors-failed] inc))
                result))
            targets))))

;; ============================================================================
;; Response Comparison
;; ============================================================================

(defn compare-responses
  "Compare primary response with mirror responses."
  [primary-response mirror-results]
  (swap! state update-in [:stats :comparisons-made] inc)
  
  (let [comparisons (for [mirror mirror-results
                          :when (:success mirror)]
                      (let [mirror-response (:response mirror)
                            [only-primary only-mirror _] (data/diff
                                                          (:body primary-response)
                                                          (:body mirror-response))
                            has-diff? (or (some? only-primary) (some? only-mirror))]
                        (when has-diff?
                          (swap! state update-in [:stats :differences-found] inc))
                        {:target-id (:target-id mirror)
                         :has-differences? has-diff?
                         :primary-only only-primary
                         :mirror-only only-mirror
                         :status-match? (= (:status primary-response)
                                           (:status mirror-response))
                         :latency-diff-ms (- (:latency-ms mirror) 0)}))]
    
    ;; Store comparison
    (let [max-comparisons (get-in @state [:config :max-comparisons])
          comparison {:id (str (UUID/randomUUID))
                      :timestamp (System/currentTimeMillis)
                      :results (vec comparisons)}]
      (swap! state update :comparisons
             (fn [c]
               (let [new-comparisons (conj c comparison)]
                 (if (> (count new-comparisons) max-comparisons)
                   (vec (drop 1 new-comparisons))
                   new-comparisons)))))
    
    comparisons))

(defn get-comparisons
  "Get comparison results."
  [& {:keys [limit with-differences?] :or {limit 100}}]
  (cond->> (:comparisons @state)
    with-differences? (filter (fn [c]
                                (some :has-differences? (:results c))))
    true (take-last limit)
    true vec))

;; ============================================================================
;; A/B Testing Support
;; ============================================================================

(defn create-ab-test!
  "Create an A/B test with mirror targets."
  [test-id config]
  (let [control-target (register-target!
                        (keyword (str (name test-id) "-control"))
                        {:name (str (name test-id) " Control")
                         :handler (get config :control-handler)
                         :weight (get config :control-weight 0.5)})
        variant-target (register-target!
                        (keyword (str (name test-id) "-variant"))
                        {:name (str (name test-id) " Variant")
                         :handler (get config :variant-handler)
                         :weight (get config :variant-weight 0.5)})]
    {:test-id test-id
     :control control-target
     :variant variant-target}))

(defn get-ab-test-results
  "Get A/B test results."
  [test-id]
  (let [control-id (keyword (str (name test-id) "-control"))
        variant-id (keyword (str (name test-id) "-variant"))
        control (get-target control-id)
        variant (get-target variant-id)]
    (when (and control variant)
      {:test-id test-id
       :control {:sent @(get-in control [:metrics :sent])
                 :succeeded @(get-in control [:metrics :succeeded])
                 :failed @(get-in control [:metrics :failed])
                 :avg-latency-ms @(get-in control [:metrics :avg-latency-ms])}
       :variant {:sent @(get-in variant [:metrics :sent])
                 :succeeded @(get-in variant [:metrics :succeeded])
                 :failed @(get-in variant [:metrics :failed])
                 :avg-latency-ms @(get-in variant [:metrics :avg-latency-ms])}})))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-mirror
  "Ring middleware to mirror requests."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (mirror-request request)
      response)))

(defn wrap-mirror-and-compare
  "Ring middleware to mirror and compare responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (when (get-in @state [:config :compare-responses?])
        (let [mirror-results (mirror-request-sync request)]
          (when (seq mirror-results)
            (compare-responses response mirror-results))))
      response)))

;; ============================================================================
;; Shadow Traffic
;; ============================================================================

(defn enable-shadow-traffic!
  "Enable shadow traffic to a target."
  [target-id sample-rate]
  (when-let [target (get-target target-id)]
    (reset! (:enabled? target) true)
    (swap! state assoc-in [:config :sample-rate] sample-rate)))

(defn disable-shadow-traffic!
  "Disable shadow traffic."
  []
  (swap! state assoc-in [:config :enabled?] false))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-enabled!
  "Enable/disable mirroring."
  [enabled?]
  (swap! state assoc-in [:config :enabled?] enabled?))

(defn set-async!
  "Enable/disable async mirroring."
  [async?]
  (swap! state assoc-in [:config :async?] async?))

(defn set-sample-rate!
  "Set the sample rate for mirroring."
  [rate]
  (swap! state assoc-in [:config :sample-rate] (max 0.0 (min 1.0 rate))))

(defn set-timeout!
  "Set the mirror timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :timeout-ms] timeout-ms))

(defn set-compare-responses!
  "Enable/disable response comparison."
  [enabled?]
  (swap! state assoc-in [:config :compare-responses?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-mirror-metrics
  "Get mirror metrics."
  []
  (let [stats (:stats @state)]
    {:requests-mirrored (:requests-mirrored stats)
     :mirrors-sent (:mirrors-sent stats)
     :mirrors-succeeded (:mirrors-succeeded stats)
     :mirrors-failed (:mirrors-failed stats)
     :comparisons-made (:comparisons-made stats)
     :differences-found (:differences-found stats)
     :targets-count (count (:targets @state))
     :success-rate (if (pos? (:mirrors-sent stats))
                     (/ (:mirrors-succeeded stats) (:mirrors-sent stats))
                     1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-mirror-stats
  "Get mirror statistics."
  []
  (merge (get-mirror-metrics)
         {:enabled? (get-in @state [:config :enabled?])
          :async? (get-in @state [:config :async?])
          :sample-rate (get-in @state [:config :sample-rate])
          :compare-responses? (get-in @state [:config :compare-responses?])}))

(defn reset-stats!
  "Reset mirror statistics."
  []
  (swap! state assoc :stats {:requests-mirrored 0
                             :mirrors-sent 0
                             :mirrors-succeeded 0
                             :mirrors-failed 0
                             :comparisons-made 0
                             :differences-found 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-mirror!
  "Initialize the request mirror."
  []
  (when-not (:initialized? @state)
    (let [executor (Executors/newCachedThreadPool)]
      (swap! state assoc :executor executor))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request mirror initialized")
    (events/emit! :request-mirror-initialized {})
    true))

(defn shutdown!
  "Shutdown the request mirror."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor))
  (swap! state assoc :initialized? false))
