(ns mental-models.pipeline.integration.model-serving
  "Model serving infrastructure for mental model analysis.
   
   Features:
   - Model deployment and versioning
   - A/B testing for models
   - Canary deployments
   - Model warm-up
   - Request batching
   - Model caching
   - Inference monitoring
   - Shadow mode testing"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:models {}           ;; model-id -> model-info
         :deployments {}      ;; deployment-id -> deployment
         :endpoints {}        ;; endpoint-id -> endpoint-config
         :traffic-rules {}    ;; endpoint-id -> traffic-rules
         :inference-cache {}  ;; cache-key -> cached-result
         :batch-queues {}     ;; endpoint-id -> batch-queue
         :stats {:inferences 0 :cache-hits 0 :errors 0}
         :initialized? false}))

;; ============================================================================
;; Model Registration
;; ============================================================================

(defn register-model!
  "Register a model for serving."
  [model-id config]
  (let [model {:id model-id
               :name (get config :name (name model-id))
               :version (get config :version "1.0.0")
               :type (get config :type :classification)
               :framework (get config :framework :custom)
               :artifact-path (get config :artifact-path nil)
               :input-schema (get config :input-schema {})
               :output-schema (get config :output-schema {})
               :metadata (get config :metadata {})
               :status :registered
               :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:models model-id] model)
    (logging/log :info "Registered model" {:model-id model-id :version (:version model)})
    (events/emit! :model-registered {:model-id model-id})
    model-id))

(defn get-model
  "Get a model."
  [model-id]
  (get-in @state [:models model-id]))

(defn list-models
  "List all models."
  [& {:keys [type status]}]
  (let [models (vals (:models @state))
        filtered (cond->> models
                   type (filter #(= (:type %) type))
                   status (filter #(= (:status %) status)))]
    (mapv #(select-keys % [:id :name :version :type :status]) filtered)))

(defn update-model-status!
  "Update model status."
  [model-id status]
  (swap! state assoc-in [:models model-id :status] status)
  (logging/log :info "Updated model status" {:model-id model-id :status status}))

;; ============================================================================
;; Model Deployment
;; ============================================================================

(defn create-deployment!
  "Create a model deployment."
  [deployment-id config]
  (let [deployment {:id deployment-id
                    :name (get config :name (name deployment-id))
                    :model-id (get config :model-id)
                    :model-version (get config :model-version)
                    :replicas (get config :replicas 1)
                    :resources (get config :resources {:cpu "100m" :memory "256Mi"})
                    :environment (get config :environment {})
                    :status :pending
                    :created-at (System/currentTimeMillis)
                    :deployed-at nil}]
    (swap! state assoc-in [:deployments deployment-id] deployment)
    (logging/log :info "Created deployment" {:deployment-id deployment-id :model-id (:model-id deployment)})
    (events/emit! :deployment-created {:deployment-id deployment-id})
    deployment-id))

(defn get-deployment
  "Get a deployment."
  [deployment-id]
  (get-in @state [:deployments deployment-id]))

(defn list-deployments
  "List all deployments."
  [& {:keys [model-id status]}]
  (let [deployments (vals (:deployments @state))
        filtered (cond->> deployments
                   model-id (filter #(= (:model-id %) model-id))
                   status (filter #(= (:status %) status)))]
    (mapv #(select-keys % [:id :name :model-id :status :replicas]) filtered)))

(defn start-deployment!
  "Start a deployment."
  [deployment-id]
  (when-let [deployment (get-deployment deployment-id)]
    (swap! state update-in [:deployments deployment-id]
           (fn [d]
             (assoc d
                    :status :running
                    :deployed-at (System/currentTimeMillis))))
    (update-model-status! (:model-id deployment) :deployed)
    (logging/log :info "Started deployment" {:deployment-id deployment-id})
    (events/emit! :deployment-started {:deployment-id deployment-id})))

(defn stop-deployment!
  "Stop a deployment."
  [deployment-id]
  (when-let [deployment (get-deployment deployment-id)]
    (swap! state assoc-in [:deployments deployment-id :status] :stopped)
    (logging/log :info "Stopped deployment" {:deployment-id deployment-id})
    (events/emit! :deployment-stopped {:deployment-id deployment-id})))

(defn scale-deployment!
  "Scale a deployment."
  [deployment-id replicas]
  (swap! state assoc-in [:deployments deployment-id :replicas] replicas)
  (logging/log :info "Scaled deployment" {:deployment-id deployment-id :replicas replicas}))

;; ============================================================================
;; Endpoints
;; ============================================================================

(defn create-endpoint!
  "Create a serving endpoint."
  [endpoint-id config]
  (let [endpoint {:id endpoint-id
                  :name (get config :name (name endpoint-id))
                  :deployment-id (get config :deployment-id)
                  :path (get config :path (str "/v1/models/" (name endpoint-id)))
                  :protocol (get config :protocol :rest)
                  :timeout-ms (get config :timeout-ms 30000)
                  :max-batch-size (get config :max-batch-size 1)
                  :caching-enabled? (get config :caching-enabled? false)
                  :cache-ttl-ms (get config :cache-ttl-ms 3600000)
                  :enabled? (get config :enabled? true)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:endpoints endpoint-id] endpoint)
    (logging/log :info "Created endpoint" {:endpoint-id endpoint-id})
    endpoint-id))

(defn get-endpoint
  "Get an endpoint."
  [endpoint-id]
  (get-in @state [:endpoints endpoint-id]))

(defn list-endpoints
  "List all endpoints."
  []
  (mapv (fn [[id e]]
          {:id id
           :name (:name e)
           :path (:path e)
           :deployment-id (:deployment-id e)
           :enabled? (:enabled? e)})
        (:endpoints @state)))

(defn enable-endpoint!
  "Enable an endpoint."
  [endpoint-id]
  (swap! state assoc-in [:endpoints endpoint-id :enabled?] true))

(defn disable-endpoint!
  "Disable an endpoint."
  [endpoint-id]
  (swap! state assoc-in [:endpoints endpoint-id :enabled?] false))

;; ============================================================================
;; Traffic Management
;; ============================================================================

(defn set-traffic-rules!
  "Set traffic rules for an endpoint (for A/B testing, canary)."
  [endpoint-id rules]
  (let [traffic-config {:endpoint-id endpoint-id
                        :rules rules
                        :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:traffic-rules endpoint-id] traffic-config)
    (logging/log :info "Set traffic rules" {:endpoint-id endpoint-id :rules rules})))

(defn- select-deployment
  "Select a deployment based on traffic rules."
  [endpoint-id]
  (let [endpoint (get-endpoint endpoint-id)
        traffic-rules (get-in @state [:traffic-rules endpoint-id :rules])]
    (if (seq traffic-rules)
      ;; Weighted selection based on traffic rules
      (let [total-weight (reduce + (map :weight traffic-rules))
            rand-val (* (rand) total-weight)]
        (loop [remaining traffic-rules
               cumulative 0]
          (if (empty? remaining)
            (:deployment-id endpoint)
            (let [rule (first remaining)
                  new-cumulative (+ cumulative (:weight rule))]
              (if (< rand-val new-cumulative)
                (:deployment-id rule)
                (recur (rest remaining) new-cumulative))))))
      ;; Default to endpoint's deployment
      (:deployment-id endpoint))))

;; ============================================================================
;; Inference
;; ============================================================================

(defn- compute-cache-key
  "Compute cache key for inference request."
  [endpoint-id input]
  (str endpoint-id "-" (hash input)))

(defn- get-cached-result
  "Get cached inference result."
  [endpoint-id input]
  (let [cache-key (compute-cache-key endpoint-id input)
        cached (get-in @state [:inference-cache cache-key])]
    (when (and cached
               (< (- (System/currentTimeMillis) (:cached-at cached))
                  (get-in @state [:endpoints endpoint-id :cache-ttl-ms] 3600000)))
      (swap! state update-in [:stats :cache-hits] inc)
      (:result cached))))

(defn- cache-result!
  "Cache an inference result."
  [endpoint-id input result]
  (let [cache-key (compute-cache-key endpoint-id input)]
    (swap! state assoc-in [:inference-cache cache-key]
           {:result result
            :cached-at (System/currentTimeMillis)})))

(defn- run-inference
  "Run inference on a model."
  [deployment-id input]
  (let [deployment (get-deployment deployment-id)
        model (get-model (:model-id deployment))]
    ;; Simulate inference - in production would call actual model
    (let [result {:prediction (rand-nth [:positive :negative :neutral])
                  :confidence (+ 0.5 (* 0.5 (rand)))
                  :model-id (:model-id deployment)
                  :model-version (:model-version deployment)
                  :inference-time-ms (+ 10 (rand-int 90))}]
      result)))

(defn predict
  "Make a prediction using an endpoint."
  [endpoint-id input]
  (when (flags/enabled? :model-serving)
    (let [endpoint (get-endpoint endpoint-id)
          start-time (System/currentTimeMillis)]
      (when (and endpoint (:enabled? endpoint))
        ;; Check cache first
        (if-let [cached (and (:caching-enabled? endpoint)
                             (get-cached-result endpoint-id input))]
          (do
            (metrics/increment :inference-cache-hits {:endpoint-id endpoint-id})
            cached)
          ;; Run inference
          (let [deployment-id (select-deployment endpoint-id)
                result (run-inference deployment-id input)
                latency (- (System/currentTimeMillis) start-time)]
            ;; Cache result if enabled
            (when (:caching-enabled? endpoint)
              (cache-result! endpoint-id input result))
            ;; Update stats
            (swap! state update-in [:stats :inferences] inc)
            (metrics/increment :inferences {:endpoint-id endpoint-id})
            (metrics/histogram :inference-latency {:endpoint-id endpoint-id} latency)
            (logging/log :debug "Inference completed" {:endpoint-id endpoint-id :latency-ms latency})
            result))))))

(defn predict-batch
  "Make batch predictions."
  [endpoint-id inputs]
  (mapv #(predict endpoint-id %) inputs))

;; ============================================================================
;; Shadow Mode
;; ============================================================================

(defn enable-shadow-mode!
  "Enable shadow mode for an endpoint (run new model alongside production)."
  [endpoint-id shadow-deployment-id]
  (swap! state assoc-in [:endpoints endpoint-id :shadow-deployment-id] shadow-deployment-id)
  (logging/log :info "Enabled shadow mode" {:endpoint-id endpoint-id :shadow-deployment-id shadow-deployment-id}))

(defn disable-shadow-mode!
  "Disable shadow mode for an endpoint."
  [endpoint-id]
  (swap! state update-in [:endpoints endpoint-id] dissoc :shadow-deployment-id)
  (logging/log :info "Disabled shadow mode" {:endpoint-id endpoint-id}))

(defn predict-with-shadow
  "Make prediction with shadow model comparison."
  [endpoint-id input]
  (let [endpoint (get-endpoint endpoint-id)
        primary-result (predict endpoint-id input)
        shadow-result (when-let [shadow-id (:shadow-deployment-id endpoint)]
                        (run-inference shadow-id input))]
    {:primary primary-result
     :shadow shadow-result
     :comparison (when shadow-result
                   {:predictions-match? (= (:prediction primary-result)
                                           (:prediction shadow-result))
                    :confidence-diff (when (and (:confidence primary-result)
                                                (:confidence shadow-result))
                                       (- (:confidence primary-result)
                                          (:confidence shadow-result)))})}))

;; ============================================================================
;; Model Warm-up
;; ============================================================================

(defn warm-up-model!
  "Warm up a model with sample inputs."
  [endpoint-id sample-inputs]
  (logging/log :info "Warming up model" {:endpoint-id endpoint-id :samples (count sample-inputs)})
  (doseq [input sample-inputs]
    (predict endpoint-id input))
  (logging/log :info "Model warm-up complete" {:endpoint-id endpoint-id}))

;; ============================================================================
;; Canary Deployment
;; ============================================================================

(defn start-canary!
  "Start a canary deployment."
  [endpoint-id canary-deployment-id & {:keys [initial-weight] :or {initial-weight 5}}]
  (let [current-deployment-id (get-in @state [:endpoints endpoint-id :deployment-id])]
    (set-traffic-rules! endpoint-id
                        [{:deployment-id current-deployment-id :weight (- 100 initial-weight)}
                         {:deployment-id canary-deployment-id :weight initial-weight}])
    (logging/log :info "Started canary deployment" {:endpoint-id endpoint-id
                                                     :canary-deployment-id canary-deployment-id
                                                     :weight initial-weight})))

(defn promote-canary!
  "Promote canary to full traffic."
  [endpoint-id canary-deployment-id]
  (set-traffic-rules! endpoint-id [{:deployment-id canary-deployment-id :weight 100}])
  (swap! state assoc-in [:endpoints endpoint-id :deployment-id] canary-deployment-id)
  (logging/log :info "Promoted canary deployment" {:endpoint-id endpoint-id
                                                    :canary-deployment-id canary-deployment-id}))

(defn rollback-canary!
  "Rollback canary deployment."
  [endpoint-id]
  (let [original-deployment-id (get-in @state [:endpoints endpoint-id :deployment-id])]
    (set-traffic-rules! endpoint-id [{:deployment-id original-deployment-id :weight 100}])
    (logging/log :info "Rolled back canary deployment" {:endpoint-id endpoint-id})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-serving-stats
  "Get model serving statistics."
  []
  (let [stats (:stats @state)]
    {:total-models (count (:models @state))
     :total-deployments (count (:deployments @state))
     :running-deployments (count (filter #(= :running (:status (val %))) (:deployments @state)))
     :total-endpoints (count (:endpoints @state))
     :enabled-endpoints (count (filter #(:enabled? (val %)) (:endpoints @state)))
     :total-inferences (:inferences stats)
     :cache-hits (:cache-hits stats)
     :errors (:errors stats)
     :cache-size (count (:inference-cache @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-model-serving!
  "Initialize model serving."
  []
  (when-not (:initialized? @state)
    ;; Register default model
    (register-model! :mental-model-classifier
                     {:name "Mental Model Classifier"
                      :version "1.0.0"
                      :type :classification
                      :framework :custom
                      :input-schema {:text :string}
                      :output-schema {:prediction :keyword :confidence :float}})
    
    ;; Create default deployment
    (create-deployment! :classifier-v1
                        {:name "Classifier v1"
                         :model-id :mental-model-classifier
                         :model-version "1.0.0"
                         :replicas 2})
    
    ;; Start deployment
    (start-deployment! :classifier-v1)
    
    ;; Create default endpoint
    (create-endpoint! :classify
                      {:name "Classify Endpoint"
                       :deployment-id :classifier-v1
                       :path "/v1/classify"
                       :caching-enabled? true
                       :cache-ttl-ms 3600000})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Model serving initialized")
    (events/emit! :model-serving-initialized {})
    true))
