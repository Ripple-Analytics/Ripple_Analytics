(ns mental-models.pipeline.integration.request-sampler
  "Request sampler for mental model analysis system.
   
   Features:
   - Random sampling
   - Rate-based sampling
   - Adaptive sampling
   - Stratified sampling
   - Reservoir sampling
   - Sample storage
   - Sample analysis
   - Sampling metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant LocalDate]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:samplers {}         ;; sampler-id -> sampler
         :samples {}          ;; sampler-id -> [samples]
         :config {:default-rate 0.1
                  :max-samples 10000
                  :sample-ttl-ms 3600000}  ;; 1 hour
         :stats {:requests-seen 0
                 :requests-sampled 0
                 :samples-stored 0
                 :samples-discarded 0}
         :initialized? false}))

(def ^:private random (Random.))

;; ============================================================================
;; Sampler Creation
;; ============================================================================

(defn create-sampler!
  "Create a new sampler."
  [sampler-id config]
  (let [sampler {:id sampler-id
                 :name (get config :name (name sampler-id))
                 :strategy (get config :strategy :random)
                 :rate (get config :rate (get-in @state [:config :default-rate]))
                 :condition-fn (get config :condition-fn (constantly true))
                 :transform-fn (get config :transform-fn identity)
                 :max-samples (get config :max-samples (get-in @state [:config :max-samples]))
                 :counter (AtomicLong. 0)
                 :enabled? (atom true)
                 :metrics {:seen (atom 0)
                           :sampled (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:samplers sampler-id] sampler)
    (swap! state assoc-in [:samples sampler-id] [])
    (logging/log :info "Created sampler" {:sampler-id sampler-id :strategy (:strategy sampler)})
    sampler-id))

(defn get-sampler
  "Get a sampler by ID."
  [sampler-id]
  (get-in @state [:samplers sampler-id]))

(defn list-samplers
  "List all samplers."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :strategy (:strategy s)
           :rate (:rate s)
           :enabled? @(:enabled? s)
           :seen @(get-in s [:metrics :seen])
           :sampled @(get-in s [:metrics :sampled])})
        (:samplers @state)))

(defn delete-sampler!
  "Delete a sampler."
  [sampler-id]
  (swap! state update :samplers dissoc sampler-id)
  (swap! state update :samples dissoc sampler-id))

;; ============================================================================
;; Sampling Strategies
;; ============================================================================

(defn- random-sample?
  "Determine if request should be sampled using random sampling."
  [rate]
  (< (.nextDouble random) rate))

(defn- rate-sample?
  "Determine if request should be sampled using rate-based sampling."
  [sampler]
  (let [counter (:counter sampler)
        count (.incrementAndGet counter)
        rate (:rate sampler)
        interval (int (/ 1.0 rate))]
    (zero? (mod count interval))))

(defn- adaptive-sample?
  "Determine if request should be sampled using adaptive sampling."
  [sampler request]
  (let [base-rate (:rate sampler)
        ;; Increase sampling for errors or slow requests
        error? (get request :error?)
        slow? (> (get request :duration-ms 0) 1000)
        adjusted-rate (cond
                        error? (min 1.0 (* base-rate 10))
                        slow? (min 1.0 (* base-rate 5))
                        :else base-rate)]
    (random-sample? adjusted-rate)))

(defn- should-sample?
  "Determine if a request should be sampled."
  [sampler request]
  (when (and @(:enabled? sampler)
             ((:condition-fn sampler) request))
    (case (:strategy sampler)
      :random (random-sample? (:rate sampler))
      :rate (rate-sample? sampler)
      :adaptive (adaptive-sample? sampler request)
      :all true
      (random-sample? (:rate sampler)))))

;; ============================================================================
;; Sample Storage
;; ============================================================================

(defn- store-sample!
  "Store a sample."
  [sampler-id sample]
  (let [sampler (get-sampler sampler-id)
        max-samples (:max-samples sampler)]
    (swap! state update-in [:samples sampler-id]
           (fn [samples]
             (let [new-samples (conj samples sample)]
               (if (> (count new-samples) max-samples)
                 (do
                   (swap! state update-in [:stats :samples-discarded] inc)
                   (vec (drop 1 new-samples)))
                 new-samples))))
    (swap! state update-in [:stats :samples-stored] inc)))

(defn get-samples
  "Get samples for a sampler."
  [sampler-id & {:keys [limit since] :or {limit 100}}]
  (let [samples (get-in @state [:samples sampler-id] [])]
    (cond->> samples
      since (filter #(> (:timestamp %) since))
      true (take-last limit)
      true vec)))

(defn clear-samples!
  "Clear samples for a sampler."
  [sampler-id]
  (swap! state assoc-in [:samples sampler-id] []))

;; ============================================================================
;; Request Sampling
;; ============================================================================

(defn sample-request
  "Sample a request."
  [sampler-id request]
  (if-let [sampler (get-sampler sampler-id)]
    (do
      (swap! (get-in sampler [:metrics :seen]) inc)
      (swap! state update-in [:stats :requests-seen] inc)
      
      (if (should-sample? sampler request)
        (let [sample (-> request
                         ((:transform-fn sampler))
                         (assoc :sampler-id sampler-id)
                         (assoc :timestamp (System/currentTimeMillis))
                         (assoc :sample-id (UUID/randomUUID)))]
          (swap! (get-in sampler [:metrics :sampled]) inc)
          (swap! state update-in [:stats :requests-sampled] inc)
          (store-sample! sampler-id sample)
          {:sampled? true :sample sample})
        {:sampled? false}))
    {:sampled? false :reason :sampler-not-found}))

(defn sample-with-all
  "Sample a request with all matching samplers."
  [request]
  (let [results (for [[sampler-id sampler] (:samplers @state)
                      :when @(:enabled? sampler)]
                  [sampler-id (sample-request sampler-id request)])]
    (into {} results)))

;; ============================================================================
;; Reservoir Sampling
;; ============================================================================

(defn create-reservoir-sampler!
  "Create a reservoir sampler for uniform sampling from a stream."
  [sampler-id config]
  (let [reservoir-size (get config :reservoir-size 100)
        sampler {:id sampler-id
                 :name (get config :name (name sampler-id))
                 :strategy :reservoir
                 :reservoir-size reservoir-size
                 :reservoir (atom [])
                 :count (atom 0)
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:samplers sampler-id] sampler)
    sampler-id))

(defn reservoir-sample!
  "Add an item to a reservoir sampler."
  [sampler-id item]
  (when-let [sampler (get-sampler sampler-id)]
    (when (= (:strategy sampler) :reservoir)
      (let [count (swap! (:count sampler) inc)
            reservoir-size (:reservoir-size sampler)]
        (if (<= count reservoir-size)
          ;; Fill reservoir
          (swap! (:reservoir sampler) conj item)
          ;; Replace with probability reservoir-size/count
          (let [j (.nextInt random count)]
            (when (< j reservoir-size)
              (swap! (:reservoir sampler) assoc j item))))))))

(defn get-reservoir
  "Get the current reservoir."
  [sampler-id]
  (when-let [sampler (get-sampler sampler-id)]
    @(:reservoir sampler)))

;; ============================================================================
;; Stratified Sampling
;; ============================================================================

(defn create-stratified-sampler!
  "Create a stratified sampler."
  [sampler-id config]
  (let [strata (get config :strata {})  ;; stratum-key -> rate
        stratum-fn (get config :stratum-fn identity)
        sampler {:id sampler-id
                 :name (get config :name (name sampler-id))
                 :strategy :stratified
                 :strata strata
                 :stratum-fn stratum-fn
                 :default-rate (get config :default-rate 0.1)
                 :enabled? (atom true)
                 :metrics {:by-stratum (atom {})}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:samplers sampler-id] sampler)
    (swap! state assoc-in [:samples sampler-id] [])
    sampler-id))

(defn stratified-sample
  "Sample using stratified sampling."
  [sampler-id request]
  (when-let [sampler (get-sampler sampler-id)]
    (when (= (:strategy sampler) :stratified)
      (let [stratum ((:stratum-fn sampler) request)
            rate (get (:strata sampler) stratum (:default-rate sampler))]
        (swap! (get-in sampler [:metrics :by-stratum]) update stratum (fnil inc 0))
        (when (random-sample? rate)
          (let [sample (assoc request
                              :stratum stratum
                              :timestamp (System/currentTimeMillis))]
            (store-sample! sampler-id sample)
            sample))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-sampler
  "Ring middleware to sample requests."
  [handler sampler-id]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)
          enriched-request (assoc request
                                  :duration-ms duration-ms
                                  :status (:status response)
                                  :error? (>= (or (:status response) 200) 400))]
      (sample-request sampler-id enriched-request)
      response)))

(defn wrap-all-samplers
  "Ring middleware to sample with all samplers."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)
          enriched-request (assoc request
                                  :duration-ms duration-ms
                                  :status (:status response))]
      (sample-with-all enriched-request)
      response)))

;; ============================================================================
;; Sample Analysis
;; ============================================================================

(defn analyze-samples
  "Analyze samples for a sampler."
  [sampler-id]
  (let [samples (get-samples sampler-id :limit 1000)]
    (when (seq samples)
      {:count (count samples)
       :time-range {:start (:timestamp (first samples))
                    :end (:timestamp (last samples))}
       :by-status (frequencies (map :status samples))
       :by-method (frequencies (map :method samples))
       :avg-duration-ms (when-let [durations (seq (keep :duration-ms samples))]
                          (/ (reduce + durations) (count durations)))
       :error-rate (let [errors (count (filter :error? samples))]
                     (/ errors (count samples)))})))

(defn get-sample-distribution
  "Get the distribution of samples by a key."
  [sampler-id key-fn]
  (let [samples (get-samples sampler-id :limit 1000)]
    (frequencies (map key-fn samples))))

;; ============================================================================
;; Sampler Control
;; ============================================================================

(defn enable-sampler!
  "Enable a sampler."
  [sampler-id]
  (when-let [sampler (get-sampler sampler-id)]
    (reset! (:enabled? sampler) true)))

(defn disable-sampler!
  "Disable a sampler."
  [sampler-id]
  (when-let [sampler (get-sampler sampler-id)]
    (reset! (:enabled? sampler) false)))

(defn set-sampler-rate!
  "Set the sampling rate for a sampler."
  [sampler-id rate]
  (swap! state assoc-in [:samplers sampler-id :rate] rate))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-sampler-metrics
  "Get metrics for a sampler."
  [sampler-id]
  (when-let [sampler (get-sampler sampler-id)]
    {:sampler-id sampler-id
     :name (:name sampler)
     :strategy (:strategy sampler)
     :rate (:rate sampler)
     :seen @(get-in sampler [:metrics :seen])
     :sampled @(get-in sampler [:metrics :sampled])
     :sample-rate (let [seen @(get-in sampler [:metrics :seen])]
                    (if (pos? seen)
                      (/ @(get-in sampler [:metrics :sampled]) seen)
                      0))
     :stored-samples (count (get-in @state [:samples sampler-id] []))}))

(defn get-all-sampler-metrics
  "Get metrics for all samplers."
  []
  (mapv (fn [[id _]] (get-sampler-metrics id)) (:samplers @state)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-sampler-stats
  "Get sampler statistics."
  []
  (let [stats (:stats @state)]
    {:samplers-count (count (:samplers @state))
     :requests-seen (:requests-seen stats)
     :requests-sampled (:requests-sampled stats)
     :samples-stored (:samples-stored stats)
     :samples-discarded (:samples-discarded stats)
     :overall-sample-rate (if (pos? (:requests-seen stats))
                            (/ (:requests-sampled stats) (:requests-seen stats))
                            0)}))

(defn reset-stats!
  "Reset sampler statistics."
  []
  (swap! state assoc :stats {:requests-seen 0
                             :requests-sampled 0
                             :samples-stored 0
                             :samples-discarded 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-sampler!
  "Initialize the request sampler."
  []
  (when-not (:initialized? @state)
    ;; Create default sampler
    (create-sampler! :default
                     {:name "Default Sampler"
                      :strategy :random
                      :rate 0.01})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request sampler initialized")
    (events/emit! :request-sampler-initialized {})
    true))
