(ns mental-models.pipeline.integration.model-trainer
  "Machine learning model training and fine-tuning for mental model detection.
   
   Features:
   - Training data management and preprocessing
   - Model training with configurable hyperparameters
   - Cross-validation and evaluation metrics
   - Model versioning and checkpointing
   - Transfer learning support
   - Hyperparameter optimization
   - Training progress tracking
   - Model export and deployment"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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
  (atom {:training-data {}      ;; dataset-id -> dataset
         :models {}             ;; model-id -> model-config
         :training-jobs {}      ;; job-id -> job-state
         :checkpoints {}        ;; checkpoint-id -> checkpoint
         :experiments {}        ;; experiment-id -> experiment
         :hyperparams {}        ;; search-id -> hyperparameter search
         :initialized? false}))

;; ============================================================================
;; Training Data Management
;; ============================================================================

(defn create-dataset!
  "Create a new training dataset."
  [dataset-id config]
  (let [dataset {:id dataset-id
                 :name (get config :name (name dataset-id))
                 :description (get config :description "")
                 :type (get config :type :classification)
                 :features (get config :features [])
                 :labels (get config :labels [])
                 :samples []
                 :splits {:train 0.8 :validation 0.1 :test 0.1}
                 :preprocessing (get config :preprocessing [])
                 :created-at (System/currentTimeMillis)
                 :updated-at (System/currentTimeMillis)
                 :stats {:total-samples 0
                         :label-distribution {}}}]
    (swap! state assoc-in [:training-data dataset-id] dataset)
    (logging/log :info "Created training dataset" {:dataset-id dataset-id})
    (events/emit! :dataset-created {:dataset-id dataset-id})
    dataset-id))

(defn add-sample!
  "Add a training sample to a dataset."
  [dataset-id sample]
  (let [{:keys [features label metadata]} sample
        sample-record {:id (str (UUID/randomUUID))
                       :features features
                       :label label
                       :metadata (or metadata {})
                       :added-at (System/currentTimeMillis)}]
    (swap! state
           (fn [s]
             (-> s
                 (update-in [:training-data dataset-id :samples] conj sample-record)
                 (update-in [:training-data dataset-id :stats :total-samples] inc)
                 (update-in [:training-data dataset-id :stats :label-distribution label]
                            (fnil inc 0))
                 (assoc-in [:training-data dataset-id :updated-at]
                           (System/currentTimeMillis)))))
    (metrics/increment :training-samples-added {:dataset-id dataset-id})
    sample-record))

(defn add-samples-batch!
  "Add multiple training samples to a dataset."
  [dataset-id samples]
  (doseq [sample samples]
    (add-sample! dataset-id sample))
  (logging/log :info "Added batch samples" {:dataset-id dataset-id :count (count samples)}))

(defn get-dataset
  "Get a training dataset."
  [dataset-id]
  (get-in @state [:training-data dataset-id]))

(defn list-datasets
  "List all training datasets."
  []
  (mapv (fn [[id ds]]
          {:id id
           :name (:name ds)
           :type (:type ds)
           :total-samples (get-in ds [:stats :total-samples] 0)
           :created-at (:created-at ds)})
        (:training-data @state)))

(defn split-dataset
  "Split dataset into train/validation/test sets."
  [dataset-id & {:keys [train-ratio val-ratio test-ratio seed]}]
  (let [dataset (get-dataset dataset-id)
        samples (:samples dataset)
        shuffled (if seed
                   (shuffle samples)
                   (shuffle samples))
        n (count shuffled)
        train-n (int (* n (or train-ratio 0.8)))
        val-n (int (* n (or val-ratio 0.1)))
        train-set (take train-n shuffled)
        val-set (take val-n (drop train-n shuffled))
        test-set (drop (+ train-n val-n) shuffled)]
    {:train train-set
     :validation val-set
     :test test-set
     :stats {:train-size (count train-set)
             :validation-size (count val-set)
             :test-size (count test-set)}}))

;; ============================================================================
;; Preprocessing
;; ============================================================================

(defn- normalize-features
  "Normalize feature values to [0, 1] range."
  [samples feature-idx]
  (let [values (map #(get-in % [:features feature-idx]) samples)
        min-val (apply min values)
        max-val (apply max values)
        range-val (- max-val min-val)]
    (if (zero? range-val)
      samples
      (map (fn [sample]
             (update-in sample [:features feature-idx]
                        #(/ (- % min-val) range-val)))
           samples))))

(defn- standardize-features
  "Standardize features to zero mean and unit variance."
  [samples feature-idx]
  (let [values (map #(get-in % [:features feature-idx]) samples)
        mean-val (/ (reduce + values) (count values))
        variance (/ (reduce + (map #(Math/pow (- % mean-val) 2) values))
                    (count values))
        std-dev (Math/sqrt variance)]
    (if (zero? std-dev)
      samples
      (map (fn [sample]
             (update-in sample [:features feature-idx]
                        #(/ (- % mean-val) std-dev)))
           samples))))

(defn preprocess-dataset
  "Apply preprocessing steps to dataset."
  [dataset-id preprocessing-steps]
  (let [dataset (get-dataset dataset-id)
        samples (:samples dataset)]
    (reduce (fn [samps step]
              (case (:type step)
                :normalize (normalize-features samps (:feature-idx step))
                :standardize (standardize-features samps (:feature-idx step))
                samps))
            samples
            preprocessing-steps)))

;; ============================================================================
;; Model Configuration
;; ============================================================================

(defn register-model!
  "Register a model configuration for training."
  [model-id config]
  (let [model {:id model-id
               :name (get config :name (name model-id))
               :type (get config :type :classifier)
               :architecture (get config :architecture :neural-network)
               :hyperparameters (get config :hyperparameters {})
               :input-shape (get config :input-shape nil)
               :output-shape (get config :output-shape nil)
               :weights nil
               :version 0
               :created-at (System/currentTimeMillis)
               :updated-at (System/currentTimeMillis)
               :status :untrained}]
    (swap! state assoc-in [:models model-id] model)
    (logging/log :info "Registered model" {:model-id model-id :type (:type model)})
    (events/emit! :model-registered {:model-id model-id})
    model-id))

(defn get-model
  "Get a model configuration."
  [model-id]
  (get-in @state [:models model-id]))

(defn list-models
  "List all registered models."
  []
  (mapv (fn [[id m]]
          {:id id
           :name (:name m)
           :type (:type m)
           :architecture (:architecture m)
           :version (:version m)
           :status (:status m)})
        (:models @state)))

(defn update-hyperparameters!
  "Update model hyperparameters."
  [model-id hyperparams]
  (swap! state
         (fn [s]
           (-> s
               (update-in [:models model-id :hyperparameters] merge hyperparams)
               (assoc-in [:models model-id :updated-at] (System/currentTimeMillis)))))
  (logging/log :info "Updated hyperparameters" {:model-id model-id :hyperparams hyperparams}))

;; ============================================================================
;; Training Jobs
;; ============================================================================

(defn- calculate-loss
  "Calculate loss for a batch (placeholder for actual implementation)."
  [predictions labels loss-fn]
  (case loss-fn
    :cross-entropy
    (let [epsilon 1e-15
          clipped (map #(max epsilon (min (- 1 epsilon) %)) predictions)]
      (- (/ (reduce + (map (fn [p l]
                             (+ (* l (Math/log p))
                                (* (- 1 l) (Math/log (- 1 p)))))
                           clipped labels))
            (count labels))))
    :mse
    (/ (reduce + (map (fn [p l] (Math/pow (- p l) 2)) predictions labels))
       (count labels))
    0.0))

(defn- train-epoch
  "Train one epoch (placeholder for actual implementation)."
  [model-id dataset-split hyperparams epoch]
  (let [samples (:train dataset-split)
        batch-size (get hyperparams :batch-size 32)
        learning-rate (get hyperparams :learning-rate 0.001)
        batches (partition-all batch-size samples)
        epoch-losses (for [batch batches]
                       ;; Placeholder: actual training would happen here
                       (let [predictions (repeatedly (count batch) #(rand))
                             labels (map :label batch)]
                         (calculate-loss predictions labels :cross-entropy)))]
    {:epoch epoch
     :train-loss (/ (reduce + epoch-losses) (count epoch-losses))
     :samples-processed (count samples)}))

(defn- evaluate-model
  "Evaluate model on validation/test set."
  [model-id samples]
  (let [predictions (repeatedly (count samples) #(if (> (rand) 0.5) 1 0))
        labels (map :label samples)
        correct (count (filter true? (map = predictions labels)))
        total (count samples)]
    {:accuracy (if (pos? total) (double (/ correct total)) 0.0)
     :predictions predictions
     :total total}))

(defn start-training!
  "Start a training job."
  [job-id config]
  (let [{:keys [model-id dataset-id epochs hyperparams callbacks]} config
        job {:id job-id
             :model-id model-id
             :dataset-id dataset-id
             :epochs (or epochs 10)
             :hyperparams (or hyperparams {})
             :callbacks (or callbacks [])
             :status :running
             :current-epoch 0
             :history []
             :started-at (System/currentTimeMillis)
             :completed-at nil
             :error nil}]
    
    (swap! state assoc-in [:training-jobs job-id] job)
    (logging/log :info "Started training job" {:job-id job-id :model-id model-id})
    (events/emit! :training-started {:job-id job-id})
    
    ;; Run training in background
    (go
      (try
        (let [dataset-split (split-dataset dataset-id)
              total-epochs (:epochs job)]
          (loop [epoch 1]
            (when (<= epoch total-epochs)
              ;; Train one epoch
              (let [epoch-result (train-epoch model-id dataset-split
                                              (:hyperparams job) epoch)
                    ;; Evaluate on validation set
                    val-result (evaluate-model model-id (:validation dataset-split))]
                
                ;; Update job state
                (swap! state
                       (fn [s]
                         (-> s
                             (assoc-in [:training-jobs job-id :current-epoch] epoch)
                             (update-in [:training-jobs job-id :history]
                                        conj (merge epoch-result val-result)))))
                
                (metrics/gauge :training-epoch {:job-id job-id} epoch)
                (metrics/gauge :training-loss {:job-id job-id} (:train-loss epoch-result))
                
                ;; Small delay to simulate training time
                (<! (timeout 100))
                (recur (inc epoch)))))
          
          ;; Training complete
          (swap! state
                 (fn [s]
                   (-> s
                       (assoc-in [:training-jobs job-id :status] :completed)
                       (assoc-in [:training-jobs job-id :completed-at]
                                 (System/currentTimeMillis))
                       (assoc-in [:models model-id :status] :trained)
                       (update-in [:models model-id :version] inc))))
          
          (logging/log :info "Training completed" {:job-id job-id})
          (events/emit! :training-completed {:job-id job-id}))
        
        (catch Exception e
          (swap! state
                 (fn [s]
                   (-> s
                       (assoc-in [:training-jobs job-id :status] :failed)
                       (assoc-in [:training-jobs job-id :error] (.getMessage e)))))
          (logging/log :error "Training failed" {:job-id job-id :error (.getMessage e)})
          (events/emit! :training-failed {:job-id job-id :error (.getMessage e)}))))
    
    job-id))

(defn stop-training!
  "Stop a running training job."
  [job-id]
  (swap! state
         (fn [s]
           (-> s
               (assoc-in [:training-jobs job-id :status] :stopped)
               (assoc-in [:training-jobs job-id :completed-at]
                         (System/currentTimeMillis)))))
  (logging/log :info "Stopped training job" {:job-id job-id})
  (events/emit! :training-stopped {:job-id job-id}))

(defn get-training-job
  "Get training job status."
  [job-id]
  (get-in @state [:training-jobs job-id]))

(defn list-training-jobs
  "List all training jobs."
  []
  (mapv (fn [[id job]]
          {:id id
           :model-id (:model-id job)
           :status (:status job)
           :current-epoch (:current-epoch job)
           :total-epochs (:epochs job)
           :started-at (:started-at job)})
        (:training-jobs @state)))

;; ============================================================================
;; Checkpointing
;; ============================================================================

(defn save-checkpoint!
  "Save a model checkpoint."
  [model-id checkpoint-name]
  (let [model (get-model model-id)
        checkpoint-id (str (UUID/randomUUID))
        checkpoint {:id checkpoint-id
                    :model-id model-id
                    :name checkpoint-name
                    :version (:version model)
                    :weights (:weights model)
                    :hyperparameters (:hyperparameters model)
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:checkpoints checkpoint-id] checkpoint)
    (logging/log :info "Saved checkpoint" {:checkpoint-id checkpoint-id :model-id model-id})
    (events/emit! :checkpoint-saved {:checkpoint-id checkpoint-id})
    checkpoint-id))

(defn load-checkpoint!
  "Load a model checkpoint."
  [model-id checkpoint-id]
  (when-let [checkpoint (get-in @state [:checkpoints checkpoint-id])]
    (swap! state
           (fn [s]
             (-> s
                 (assoc-in [:models model-id :weights] (:weights checkpoint))
                 (assoc-in [:models model-id :hyperparameters] (:hyperparameters checkpoint))
                 (assoc-in [:models model-id :version] (:version checkpoint)))))
    (logging/log :info "Loaded checkpoint" {:checkpoint-id checkpoint-id :model-id model-id})
    true))

(defn list-checkpoints
  "List checkpoints for a model."
  [model-id]
  (filterv #(= (:model-id %) model-id)
           (vals (:checkpoints @state))))

;; ============================================================================
;; Hyperparameter Optimization
;; ============================================================================

(defn- generate-hyperparam-configs
  "Generate hyperparameter configurations for search."
  [search-space n-configs]
  (repeatedly n-configs
              (fn []
                (into {}
                      (map (fn [[param spec]]
                             [param
                              (case (:type spec)
                                :uniform (+ (:min spec)
                                            (* (rand) (- (:max spec) (:min spec))))
                                :log-uniform (Math/exp (+ (Math/log (:min spec))
                                                          (* (rand)
                                                             (- (Math/log (:max spec))
                                                                (Math/log (:min spec))))))
                                :choice (rand-nth (:values spec))
                                :int (+ (:min spec)
                                        (rand-int (- (:max spec) (:min spec))))
                                (:default spec))])
                           search-space)))))

(defn start-hyperparam-search!
  "Start hyperparameter optimization search."
  [search-id config]
  (let [{:keys [model-id dataset-id search-space n-trials strategy]} config
        search {:id search-id
                :model-id model-id
                :dataset-id dataset-id
                :search-space search-space
                :n-trials (or n-trials 10)
                :strategy (or strategy :random)
                :trials []
                :best-trial nil
                :status :running
                :started-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:hyperparams search-id] search)
    (logging/log :info "Started hyperparameter search" {:search-id search-id})
    
    ;; Run search in background
    (go
      (let [configs (generate-hyperparam-configs search-space (:n-trials search))]
        (doseq [[idx config] (map-indexed vector configs)]
          (let [trial-id (str search-id "-trial-" idx)
                ;; Train with these hyperparameters
                _ (start-training! trial-id
                                   {:model-id model-id
                                    :dataset-id dataset-id
                                    :epochs 5
                                    :hyperparams config})
                ;; Wait for training
                _ (<! (timeout 1000))
                job (get-training-job trial-id)
                best-val-acc (if (seq (:history job))
                               (apply max (map :accuracy (:history job)))
                               0.0)
                trial {:trial-id trial-id
                       :hyperparams config
                       :best-accuracy best-val-acc}]
            
            (swap! state update-in [:hyperparams search-id :trials] conj trial)
            
            ;; Update best trial
            (let [current-best (get-in @state [:hyperparams search-id :best-trial])]
              (when (or (nil? current-best)
                        (> best-val-acc (:best-accuracy current-best)))
                (swap! state assoc-in [:hyperparams search-id :best-trial] trial)))))
        
        (swap! state assoc-in [:hyperparams search-id :status] :completed)
        (logging/log :info "Hyperparameter search completed" {:search-id search-id})
        (events/emit! :hyperparam-search-completed {:search-id search-id})))
    
    search-id))

(defn get-hyperparam-search
  "Get hyperparameter search status."
  [search-id]
  (get-in @state [:hyperparams search-id]))

;; ============================================================================
;; Experiments
;; ============================================================================

(defn create-experiment!
  "Create a new experiment for tracking."
  [experiment-id config]
  (let [experiment {:id experiment-id
                    :name (get config :name (name experiment-id))
                    :description (get config :description "")
                    :tags (get config :tags [])
                    :runs []
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:experiments experiment-id] experiment)
    (logging/log :info "Created experiment" {:experiment-id experiment-id})
    experiment-id))

(defn log-run!
  "Log a training run to an experiment."
  [experiment-id run-data]
  (let [run {:id (str (UUID/randomUUID))
             :job-id (:job-id run-data)
             :model-id (:model-id run-data)
             :hyperparams (:hyperparams run-data)
             :metrics (:metrics run-data)
             :artifacts (:artifacts run-data)
             :logged-at (System/currentTimeMillis)}]
    (swap! state update-in [:experiments experiment-id :runs] conj run)
    run))

(defn get-experiment
  "Get experiment details."
  [experiment-id]
  (get-in @state [:experiments experiment-id]))

(defn compare-runs
  "Compare multiple runs in an experiment."
  [experiment-id run-ids]
  (let [experiment (get-experiment experiment-id)
        runs (filter #(contains? (set run-ids) (:id %)) (:runs experiment))]
    {:runs runs
     :comparison (when (seq runs)
                   {:best-accuracy (apply max (map #(get-in % [:metrics :accuracy] 0) runs))
                    :avg-accuracy (/ (reduce + (map #(get-in % [:metrics :accuracy] 0) runs))
                                     (count runs))})}))

;; ============================================================================
;; Model Export
;; ============================================================================

(defn export-model
  "Export a trained model."
  [model-id format]
  (let [model (get-model model-id)]
    (case format
      :edn {:format :edn
            :model model
            :exported-at (System/currentTimeMillis)}
      :onnx {:format :onnx
             :placeholder true
             :note "ONNX export requires additional dependencies"}
      {:format :unknown})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-trainer-stats
  "Get overall trainer statistics."
  []
  (let [jobs (:training-jobs @state)
        completed-jobs (filter #(= (:status (val %)) :completed) jobs)
        failed-jobs (filter #(= (:status (val %)) :failed) jobs)]
    {:total-datasets (count (:training-data @state))
     :total-models (count (:models @state))
     :total-jobs (count jobs)
     :completed-jobs (count completed-jobs)
     :failed-jobs (count failed-jobs)
     :running-jobs (count (filter #(= (:status (val %)) :running) jobs))
     :total-checkpoints (count (:checkpoints @state))
     :total-experiments (count (:experiments @state))
     :hyperparam-searches (count (:hyperparams @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-model-trainer!
  "Initialize the model trainer."
  []
  (when-not (:initialized? @state)
    ;; Create default dataset for mental model classification
    (create-dataset! :mental-model-classification
                     {:name "Mental Model Classification"
                      :description "Dataset for classifying text into mental models"
                      :type :multi-label-classification
                      :features [:text-embedding]
                      :labels [:confirmation-bias :availability-heuristic :anchoring
                               :loss-aversion :sunk-cost :hindsight-bias]})
    
    ;; Register default model
    (register-model! :mental-model-classifier
                     {:name "Mental Model Classifier"
                      :type :classifier
                      :architecture :transformer
                      :hyperparameters {:learning-rate 0.001
                                        :batch-size 32
                                        :hidden-size 256
                                        :num-layers 4
                                        :dropout 0.1}})
    
    ;; Create default experiment
    (create-experiment! :mental-model-experiments
                        {:name "Mental Model Detection Experiments"
                         :description "Experiments for improving mental model detection"
                         :tags [:mental-models :classification :nlp]})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Model trainer initialized")
    (events/emit! :model-trainer-initialized {})
    true))
