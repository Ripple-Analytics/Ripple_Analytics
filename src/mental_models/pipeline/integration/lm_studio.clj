(ns mental-models.pipeline.integration.lm-studio
  "LM Studio Analysis Integration
   
   Connects the pipeline integration to the LM Studio client:
   - Wraps LM Studio calls with circuit breaker
   - Adds caching for repeated analyses
   - Integrates with metrics and audit logging
   - Provides streaming analysis with event publishing"
  (:require
   [mental-models.llm.lm-studio :as lm]
   [mental-models.features.flags :as flags]
   [mental-models.resilience.circuit-breaker :as cb]
   [mental-models.cache.core :as cache]
   [mental-models.audit.core :as audit]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.registry.models :as registry]
   [clojure.core.async :as async :refer [go chan <! >! close!]]))

;; =============================================================================
;; ANALYSIS PROMPTS
;; =============================================================================

(def base-system-prompt
  "You are an expert analyst trained in Charlie Munger's mental models and psychological tendencies.
   Analyze the given text and identify which of the 25 psychological tendencies are present.
   For each tendency found, provide:
   1. The tendency name
   2. Confidence score (0.0 to 1.0)
   3. Evidence from the text
   4. Potential implications")

(defn build-analysis-prompt [text model-ids]
  (let [models (map registry/get-model model-ids)
        model-descriptions (map #(str "- " (:name %) ": " (:description %)) models)]
    (str "Analyze the following text for these mental models:\n\n"
         (clojure.string/join "\n" model-descriptions)
         "\n\nText to analyze:\n\n"
         text
         "\n\nProvide your analysis in JSON format with fields: model_id, confidence, evidence, implications")))

;; =============================================================================
;; CACHED ANALYSIS
;; =============================================================================

(defn get-cache-key [text model-ids]
  (str (hash (str text (sort model-ids)))))

(defn analyze-cached
  "Analyze text with caching support."
  [text model-ids & {:keys [skip-cache] :or {skip-cache false}}]
  (let [cache-key (get-cache-key text model-ids)]
    (if (and (not skip-cache)
             (flags/is-enabled? "cache-enabled"))
      (if-let [cached (cache/get-value "analysis" cache-key)]
        (do
          (log/debug "Analysis cache hit" {:key cache-key})
          (metrics/inc-counter! :pipeline/cache-hits)
          cached)
        (let [result (analyze-cached text model-ids :skip-cache true)]
          (cache/put! "analysis" cache-key result)
          (metrics/inc-counter! :pipeline/cache-misses)
          result))
      ;; Perform actual analysis
      (let [start-time (System/currentTimeMillis)
            prompt (build-analysis-prompt text model-ids)
            ;; Use circuit breaker for LM Studio call
            cb-result (cb/execute (cb/get-circuit-breaker "lm-studio")
                                  #(lm/complete prompt))
            duration (- (System/currentTimeMillis) start-time)]
        (metrics/observe-histogram! :pipeline/analysis-latency duration)
        (if (= :success (:status cb-result))
          (do
            (metrics/inc-counter! :pipeline/analysis-requests)
            (audit/log-operation! {:operation :lm-studio-analysis
                                   :text-length (count text)
                                   :model-count (count model-ids)
                                   :duration-ms duration})
            (:result cb-result))
          (do
            (log/error "LM Studio analysis failed" {:status (:status cb-result)})
            (metrics/inc-counter! :pipeline/analysis-failures)
            nil))))))

;; =============================================================================
;; STREAMING ANALYSIS
;; =============================================================================

(defn analyze-streaming
  "Analyze text with streaming response."
  [text model-ids]
  (let [output-chan (chan 100)
        prompt (build-analysis-prompt text model-ids)]
    (go
      (try
        (let [start-time (System/currentTimeMillis)]
          ;; Start streaming
          (events/publish! :analysis/started {:text-length (count text)
                                              :model-count (count model-ids)})
          ;; Get streaming response from LM Studio
          (if (flags/is-enabled? "streaming-analysis")
            (let [stream-chan (lm/complete-streaming prompt)]
              (loop []
                (when-let [token (<! stream-chan)]
                  (>! output-chan {:type :token :content token})
                  (recur)))
              ;; Stream complete
              (let [duration (- (System/currentTimeMillis) start-time)]
                (>! output-chan {:type :complete :duration-ms duration})
                (metrics/observe-histogram! :pipeline/analysis-latency duration)
                (metrics/inc-counter! :pipeline/analysis-requests)
                (events/publish! :analysis/completed {:duration-ms duration})))
            ;; Non-streaming fallback
            (let [result (analyze-cached text model-ids)
                  duration (- (System/currentTimeMillis) start-time)]
              (>! output-chan {:type :result :content result :duration-ms duration}))))
        (catch Exception e
          (log/error "Streaming analysis failed" {:error (.getMessage e)})
          (>! output-chan {:type :error :message (.getMessage e)}))
        (finally
          (close! output-chan))))
    output-chan))

;; =============================================================================
;; MENTAL MODEL DETECTION
;; =============================================================================

(defn detect-models
  "Detect mental models in text using LM Studio."
  [text & {:keys [threshold models] :or {threshold 0.7 models nil}}]
  (let [model-ids (or models (map :model-id (registry/get-all-models)))
        result (analyze-cached text model-ids)]
    (when result
      ;; Parse result and filter by threshold
      (let [detections (try
                         (read-string result)
                         (catch Exception _ []))]
        (->> detections
             (filter #(>= (:confidence %) threshold))
             (map (fn [d]
                    (registry/record-detection! (:model-id d) (:confidence d) :lm-studio)
                    d)))))))

(defn detect-lollapalooza
  "Detect Lollapalooza effect (3+ models with high confidence)."
  [text & {:keys [threshold min-models] :or {threshold 0.7 min-models 3}}]
  (let [detections (detect-models text :threshold threshold)]
    (when (>= (count detections) min-models)
      (let [alert {:type :lollapalooza
                   :models (map :model-id detections)
                   :confidences (map :confidence detections)
                   :avg-confidence (/ (reduce + (map :confidence detections))
                                      (count detections))
                   :timestamp (System/currentTimeMillis)}]
        (metrics/inc-counter! :pipeline/lollapalooza-alerts)
        (audit/log-operation! {:operation :lollapalooza-detected
                               :model-count (count detections)
                               :avg-confidence (:avg-confidence alert)})
        (events/publish! :lollapalooza/detected alert)
        (log/warn "LOLLAPALOOZA DETECTED!" alert)
        alert))))

;; =============================================================================
;; BATCH ANALYSIS
;; =============================================================================

(defn analyze-batch
  "Analyze multiple texts in batch."
  [texts & {:keys [concurrency threshold] :or {concurrency 4 threshold 0.7}}]
  (log/info "Starting batch analysis" {:count (count texts) :concurrency concurrency})
  (let [start-time (System/currentTimeMillis)
        model-ids (map :model-id (registry/get-all-models))
        results (doall
                 (pmap (fn [text]
                         (try
                           {:text text
                            :detections (detect-models text :threshold threshold)
                            :lollapalooza (detect-lollapalooza text :threshold threshold)}
                           (catch Exception e
                             (log/error "Batch item failed" {:error (.getMessage e)})
                             {:text text :error (.getMessage e)})))
                       texts))
        duration (- (System/currentTimeMillis) start-time)
        successful (count (filter #(not (:error %)) results))
        lollapaloozas (count (filter :lollapalooza results))]
    (audit/log-operation! {:operation :batch-analysis
                           :total (count texts)
                           :successful successful
                           :lollapaloozas lollapaloozas
                           :duration-ms duration})
    (events/publish! :batch/completed {:total (count texts)
                                       :successful successful
                                       :lollapaloozas lollapaloozas
                                       :duration-ms duration})
    {:total (count texts)
     :successful successful
     :failed (- (count texts) successful)
     :lollapaloozas lollapaloozas
     :duration-ms duration
     :results results}))

;; =============================================================================
;; HEALTH CHECK
;; =============================================================================

(defn check-lm-studio-health []
  (let [cb (cb/get-circuit-breaker "lm-studio")]
    {:circuit-breaker-state (cb/get-state cb)
     :failure-rate (cb/get-failure-rate cb)
     :metrics (cb/get-metrics cb)
     :healthy (cb/closed? cb)}))
