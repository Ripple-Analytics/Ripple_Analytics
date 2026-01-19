(ns mental-models.pipeline.integration.model-versioning
  "Model versioning for mental model analysis ML models.
   
   Features:
   - Model version management
   - Version comparison
   - Rollback support
   - A/B testing integration
   - Model lineage
   - Deployment tracking
   - Performance comparison
   - Version metadata"
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
  (atom {:models {}           ;; model-id -> model
         :versions {}         ;; model-id -> [versions]
         :deployments {}      ;; deployment-id -> deployment
         :experiments {}      ;; experiment-id -> experiment
         :lineage {}          ;; model-id -> lineage
         :stats {:versions-created 0 :deployments 0 :rollbacks 0}
         :initialized? false}))

;; ============================================================================
;; Model Management
;; ============================================================================

(defn register-model!
  "Register a model."
  [model-id config]
  (let [model {:id model-id
               :name (get config :name (name model-id))
               :description (get config :description "")
               :type (get config :type :classification) ;; :classification, :regression, :nlp, :embedding
               :framework (get config :framework :custom) ;; :custom, :pytorch, :tensorflow, :sklearn
               :current-version nil
               :created-at (System/currentTimeMillis)
               :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:models model-id] model)
    (swap! state assoc-in [:versions model-id] [])
    (logging/log :info "Registered model" {:model-id model-id})
    model-id))

(defn get-model
  "Get a model."
  [model-id]
  (get-in @state [:models model-id]))

(defn list-models
  "List all models."
  []
  (mapv (fn [[id m]]
          {:id id
           :name (:name m)
           :type (:type m)
           :current-version (:current-version m)})
        (:models @state)))

;; ============================================================================
;; Version Management
;; ============================================================================

(defn create-version!
  "Create a new model version."
  [model-id config]
  (when (flags/enabled? :model-versioning)
    (when (get-model model-id)
      (let [versions (get-in @state [:versions model-id] [])
            version-number (inc (count versions))
            version {:version version-number
                     :model-id model-id
                     :artifact-path (get config :artifact-path)
                     :parameters (get config :parameters {})
                     :metrics (get config :metrics {})
                     :training-data (get config :training-data)
                     :parent-version (get config :parent-version)
                     :tags (get config :tags #{})
                     :status :created ;; :created, :validated, :deployed, :deprecated, :archived
                     :created-by (get config :created-by)
                     :created-at (System/currentTimeMillis)}]
        
        (swap! state update-in [:versions model-id] conj version)
        (swap! state update-in [:stats :versions-created] inc)
        
        ;; Update lineage
        (when (:parent-version config)
          (swap! state update-in [:lineage model-id]
                 (fnil conj [])
                 {:from (:parent-version config)
                  :to version-number
                  :created-at (System/currentTimeMillis)}))
        
        (logging/log :info "Created model version" {:model-id model-id :version version-number})
        (events/emit! :model-version-created {:model-id model-id :version version-number})
        
        version-number))))

(defn get-version
  "Get a specific version."
  [model-id version-number]
  (let [versions (get-in @state [:versions model-id] [])]
    (first (filter #(= (:version %) version-number) versions))))

(defn get-latest-version
  "Get the latest version."
  [model-id]
  (last (get-in @state [:versions model-id] [])))

(defn list-versions
  "List all versions of a model."
  [model-id & {:keys [status limit] :or {limit 100}}]
  (let [versions (get-in @state [:versions model-id] [])
        filtered (cond->> versions
                   status (filter #(= (:status %) status))
                   limit (take-last limit))]
    (mapv #(select-keys % [:version :status :metrics :created-at]) filtered)))

(defn update-version!
  "Update a version."
  [model-id version-number updates]
  (swap! state update-in [:versions model-id]
         (fn [versions]
           (mapv (fn [v]
                   (if (= (:version v) version-number)
                     (merge v updates {:updated-at (System/currentTimeMillis)})
                     v))
                 versions))))

(defn set-version-status!
  "Set version status."
  [model-id version-number status]
  (update-version! model-id version-number {:status status})
  (logging/log :info "Updated version status" {:model-id model-id :version version-number :status status}))

;; ============================================================================
;; Version Comparison
;; ============================================================================

(defn compare-versions
  "Compare two versions."
  [model-id version-a version-b]
  (let [v-a (get-version model-id version-a)
        v-b (get-version model-id version-b)]
    (when (and v-a v-b)
      {:version-a version-a
       :version-b version-b
       :metrics-diff (reduce (fn [diff [k v]]
                               (let [v-b-val (get-in v-b [:metrics k])]
                                 (if v-b-val
                                   (assoc diff k {:a v :b v-b-val :diff (- v-b-val v)})
                                   diff)))
                             {}
                             (:metrics v-a))
       :parameter-diff {:a-only (clojure.set/difference (set (keys (:parameters v-a)))
                                                         (set (keys (:parameters v-b))))
                        :b-only (clojure.set/difference (set (keys (:parameters v-b)))
                                                         (set (keys (:parameters v-a))))
                        :changed (filter (fn [k]
                                           (and (contains? (:parameters v-a) k)
                                                (contains? (:parameters v-b) k)
                                                (not= (get-in v-a [:parameters k])
                                                      (get-in v-b [:parameters k]))))
                                         (keys (:parameters v-a)))}})))

;; ============================================================================
;; Deployment Management
;; ============================================================================

(defn deploy-version!
  "Deploy a model version."
  [model-id version-number & {:keys [environment traffic-percentage]}]
  (when-let [version (get-version model-id version-number)]
    (let [deployment-id (str (UUID/randomUUID))
          deployment {:id deployment-id
                      :model-id model-id
                      :version version-number
                      :environment (or environment :production)
                      :traffic-percentage (or traffic-percentage 100)
                      :status :active
                      :deployed-at (System/currentTimeMillis)}]
      
      ;; Update version status
      (set-version-status! model-id version-number :deployed)
      
      ;; Update model's current version
      (swap! state assoc-in [:models model-id :current-version] version-number)
      (swap! state assoc-in [:models model-id :updated-at] (System/currentTimeMillis))
      
      ;; Store deployment
      (swap! state assoc-in [:deployments deployment-id] deployment)
      (swap! state update-in [:stats :deployments] inc)
      
      (logging/log :info "Deployed model version" {:model-id model-id :version version-number :deployment-id deployment-id})
      (events/emit! :model-deployed {:model-id model-id :version version-number})
      
      deployment-id)))

(defn get-deployment
  "Get a deployment."
  [deployment-id]
  (get-in @state [:deployments deployment-id]))

(defn list-deployments
  "List deployments."
  [& {:keys [model-id environment status]}]
  (let [deployments (vals (:deployments @state))
        filtered (cond->> deployments
                   model-id (filter #(= (:model-id %) model-id))
                   environment (filter #(= (:environment %) environment))
                   status (filter #(= (:status %) status)))]
    (mapv #(select-keys % [:id :model-id :version :environment :status :deployed-at]) filtered)))

(defn undeploy!
  "Undeploy a deployment."
  [deployment-id]
  (when-let [deployment (get-deployment deployment-id)]
    (swap! state assoc-in [:deployments deployment-id :status] :inactive)
    (swap! state assoc-in [:deployments deployment-id :undeployed-at] (System/currentTimeMillis))
    (logging/log :info "Undeployed" {:deployment-id deployment-id})))

;; ============================================================================
;; Rollback
;; ============================================================================

(defn rollback!
  "Rollback to a previous version."
  [model-id target-version]
  (when-let [version (get-version model-id target-version)]
    ;; Undeploy current deployments
    (let [current-deployments (filter #(and (= (:model-id %) model-id)
                                            (= :active (:status %)))
                                      (vals (:deployments @state)))]
      (doseq [dep current-deployments]
        (undeploy! (:id dep))))
    
    ;; Deploy target version
    (let [deployment-id (deploy-version! model-id target-version)]
      (swap! state update-in [:stats :rollbacks] inc)
      (logging/log :info "Rolled back model" {:model-id model-id :to-version target-version})
      (events/emit! :model-rolled-back {:model-id model-id :version target-version})
      deployment-id)))

;; ============================================================================
;; A/B Testing
;; ============================================================================

(defn create-experiment!
  "Create an A/B testing experiment."
  [experiment-id config]
  (let [experiment {:id experiment-id
                    :name (get config :name (name experiment-id))
                    :model-id (get config :model-id)
                    :control-version (get config :control-version)
                    :treatment-version (get config :treatment-version)
                    :traffic-split (get config :traffic-split 50) ;; % to treatment
                    :metrics-to-track (get config :metrics-to-track [:accuracy :latency])
                    :status :created ;; :created, :running, :completed, :stopped
                    :results {:control {} :treatment {}}
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:experiments experiment-id] experiment)
    (logging/log :info "Created experiment" {:experiment-id experiment-id})
    experiment-id))

(defn get-experiment
  "Get an experiment."
  [experiment-id]
  (get-in @state [:experiments experiment-id]))

(defn list-experiments
  "List experiments."
  [& {:keys [model-id status]}]
  (let [experiments (vals (:experiments @state))
        filtered (cond->> experiments
                   model-id (filter #(= (:model-id %) model-id))
                   status (filter #(= (:status %) status)))]
    (mapv #(select-keys % [:id :name :model-id :status :traffic-split]) filtered)))

(defn start-experiment!
  "Start an experiment."
  [experiment-id]
  (swap! state update-in [:experiments experiment-id]
         (fn [e]
           (assoc e
                  :status :running
                  :started-at (System/currentTimeMillis))))
  (logging/log :info "Started experiment" {:experiment-id experiment-id})
  (events/emit! :experiment-started {:experiment-id experiment-id}))

(defn record-experiment-result!
  "Record a result for an experiment."
  [experiment-id variant metric value]
  (swap! state update-in [:experiments experiment-id :results variant metric]
         (fnil conj [])
         {:value value :recorded-at (System/currentTimeMillis)}))

(defn stop-experiment!
  "Stop an experiment."
  [experiment-id]
  (swap! state update-in [:experiments experiment-id]
         (fn [e]
           (assoc e
                  :status :completed
                  :completed-at (System/currentTimeMillis))))
  (logging/log :info "Stopped experiment" {:experiment-id experiment-id}))

(defn get-experiment-results
  "Get experiment results with statistical analysis."
  [experiment-id]
  (when-let [experiment (get-experiment experiment-id)]
    (let [control-results (get-in experiment [:results :control])
          treatment-results (get-in experiment [:results :treatment])]
      {:experiment-id experiment-id
       :control {:version (:control-version experiment)
                 :metrics (reduce (fn [m [metric values]]
                                    (let [vals (map :value values)]
                                      (assoc m metric {:mean (/ (reduce + vals) (count vals))
                                                       :count (count vals)})))
                                  {}
                                  control-results)}
       :treatment {:version (:treatment-version experiment)
                   :metrics (reduce (fn [m [metric values]]
                                      (let [vals (map :value values)]
                                        (assoc m metric {:mean (/ (reduce + vals) (count vals))
                                                         :count (count vals)})))
                                    {}
                                    treatment-results)}})))

;; ============================================================================
;; Model Lineage
;; ============================================================================

(defn get-lineage
  "Get model lineage."
  [model-id]
  (get-in @state [:lineage model-id] []))

(defn get-ancestors
  "Get all ancestor versions."
  [model-id version-number]
  (let [lineage (get-lineage model-id)]
    (loop [current version-number
           ancestors []]
      (let [parent (first (filter #(= (:to %) current) lineage))]
        (if parent
          (recur (:from parent) (conj ancestors (:from parent)))
          ancestors)))))

(defn get-descendants
  "Get all descendant versions."
  [model-id version-number]
  (let [lineage (get-lineage model-id)]
    (loop [current [version-number]
           descendants []]
      (let [children (filter #(contains? (set current) (:from %)) lineage)
            child-versions (map :to children)]
        (if (seq child-versions)
          (recur child-versions (concat descendants child-versions))
          descendants)))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-versioning-stats
  "Get versioning statistics."
  []
  (let [stats (:stats @state)
        models (vals (:models @state))
        versions (reduce + (map #(count (get-in @state [:versions (:id %)])) models))]
    {:total-models (count models)
     :total-versions versions
     :total-deployments (count (:deployments @state))
     :total-experiments (count (:experiments @state))
     :versions-created (:versions-created stats)
     :deployments (:deployments stats)
     :rollbacks (:rollbacks stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-model-versioning!
  "Initialize model versioning."
  []
  (when-not (:initialized? @state)
    ;; Register mental model classifier
    (register-model! :mental-model-classifier
                     {:name "Mental Model Classifier"
                      :description "Classifies text into mental model categories"
                      :type :classification
                      :framework :custom})
    
    ;; Create initial version
    (create-version! :mental-model-classifier
                     {:parameters {:confidence-threshold 0.7
                                   :max-models 5}
                      :metrics {:accuracy 0.85
                                :f1-score 0.82}
                      :created-by "system"})
    
    ;; Deploy initial version
    (deploy-version! :mental-model-classifier 1)
    
    ;; Register lollapalooza detector
    (register-model! :lollapalooza-detector
                     {:name "Lollapalooza Effect Detector"
                      :description "Detects convergence of multiple mental models"
                      :type :classification
                      :framework :custom})
    
    (create-version! :lollapalooza-detector
                     {:parameters {:min-models 3
                                   :synergy-threshold 0.7}
                      :metrics {:precision 0.88
                                :recall 0.75}
                      :created-by "system"})
    
    (deploy-version! :lollapalooza-detector 1)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Model versioning initialized")
    (events/emit! :model-versioning-initialized {})
    true))
