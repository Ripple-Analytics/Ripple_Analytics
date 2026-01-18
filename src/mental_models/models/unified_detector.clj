(ns mental-models.models.unified-detector
  \"Unified Mental Model Detection Engine - Electric Clojure
   Integrates all 129+ mental model algorithms
   Orchestrates text analysis, scoring, and Lollapalooza detection\"
  (:require [mental-models.models.algorithms :as base-algorithms]
            [mental-models.models.algorithms-extended :as extended-algorithms]
            [mental-models.analysis.statistical :as stats]
            [taoensso.timbre :as log]
            [clojure.string :as str]))

;; -- Complete Model Registry (129+ models) --

(def complete-model-registry
  (merge base-algorithms/all-detectors
         extended-algorithms/extended-detectors))

;; -- Unified Detection Result --

(defrecord UnifiedDetectionResult
  [text-length models-analyzed successful-analyses
   average-score top-10-models all-scores
   lollapalooza-detected convergence-score convergence-count
   lollapalooza-models analysis-timestamp])

;; -- Main Detection Pipeline --

(defn run-all-detections
  \"Run all 129+ model detectors on text\"
  [text]
  (log/info \"Running detection on\" (count text) \"characters\")
  
  (let [;; Run base algorithms
        base-results (base-algorithms/detect-all text)
        
        ;; Run extended algorithms
        extended-results (extended-algorithms/detect-all-extended text)
        
        ;; Combine all results
        all-results (concat base-results extended-results)
        
        ;; Filter out zero-score results
        valid-results (filter #(> (:score %) 0.0) all-results)
        
        ;; Calculate statistics
        scores (map :score valid-results)
        avg-score (if (empty? scores) 0.0 (stats/mean scores))
        
        ;; Sort by score
        sorted-results (sort-by :score > valid-results)
        top-10 (take 10 sorted-results)]
    
    (log/info \"Detected\" (count valid-results) \"models with score > 0.0\")
    (log/info \"Average score:\" (format \"%.3f\" avg-score))
    
    {:all-results all-results
     :valid-results valid-results
     :sorted-results sorted-results
     :top-10 top-10
     :statistics {:total-analyzed (count all-results)
                  :models-detected (count valid-results)
                  :average-score avg-score
                  :min-score (if (empty? scores) 0.0 (apply min scores))
                  :max-score (if (empty? scores) 0.0 (apply max scores))}}))

;; -- Lollapalooza Detection --

(defn detect-lollapalooza
  \"Detect Lollapalooza effect (3+ models converging > 0.7)\"
  [detection-results]
  (let [high-scoring (filter #(> (:score %) 0.7) (:valid-results detection-results))
        convergence-count (count high-scoring)
        convergence-score (if (empty? high-scoring)
                          0.0
                          (stats/mean (map :score high-scoring)))
        
        is-lollapalooza (>= convergence-count 3)]
    
    (log/info \"Lollapalooza check:\" convergence-count \"models > 0.7\")
    
    {:detected is-lollapalooza
     :convergence-count convergence-count
     :convergence-score convergence-score
     :converging-models (map :model high-scoring)
     :model-details high-scoring}))

;; -- Failure Mode Detection --

(defn detect-failure-modes
  \"Detect potential failure modes based on detected models\"
  [detection-results]
  (let [top-models (:top-10 detection-results)
        
        ;; Map models to potential failure modes
        failure-mappings {
          \"Confirmation Bias\" [{:name \"Missed Signals\" :risk \"high\"}
                               {:name \"Echo Chamber\" :risk \"high\"}]
          \"Anchoring Bias\" [{:name \"Suboptimal Decisions\" :risk \"medium\"}
                            {:name \"Missed Opportunities\" :risk \"medium\"}]
          \"Overconfidence Bias\" [{:name \"Excessive Risk Taking\" :risk \"critical\"}
                                 {:name \"Inadequate Planning\" :risk \"high\"}]
          \"Survivorship Bias\" [{:name \"Unrealistic Expectations\" :risk \"high\"}
                               {:name \"Repeated Mistakes\" :risk \"high\"}]
          \"Sunk Cost Fallacy\" [{:name \"Wasted Resources\" :risk \"high\"}
                               {:name \"Compounding Losses\" :risk \"critical\"}]}
        
        detected-failures (mapcat (fn [model]
                                  (let [model-name (:model model)
                                        modes (get failure-mappings model-name [])]
                                    (map #(assoc % :model model-name :score (:score model))
                                        modes)))
                                top-models)]
    
    (sort-by :score > detected-failures)))

;; -- Comprehensive Analysis --

(defn comprehensive-analysis
  \"Run comprehensive mental model analysis on text\"
  [text]
  (let [start-time (System/currentTimeMillis)
        
        ;; Run all detections
        detection-results (run-all-detections text)
        
        ;; Detect Lollapalooza
        lollapalooza (detect-lollapalooza detection-results)
        
        ;; Detect failure modes
        failure-modes (detect-failure-modes detection-results)
        
        ;; Calculate timing
        end-time (System/currentTimeMillis)
        duration-ms (- end-time start-time)]
    
    (log/info \"Analysis completed in\" duration-ms \"ms\")
    
    (->UnifiedDetectionResult
     (count text)
     (:total-analyzed (:statistics detection-results))
     (:models-detected (:statistics detection-results))
     (:average-score (:statistics detection-results))
     (:top-10 detection-results)
     (:valid-results detection-results)
     (:detected lollapalooza)
     (:convergence-score lollapalooza)
     (:convergence-count lollapalooza)
     (:converging-models lollapalooza)
     (java.time.Instant/now))))

;; -- Batch Analysis --

(defn batch-analysis
  \"Analyze multiple texts\"
  [texts]
  (log/info \"Starting batch analysis of\" (count texts) \"texts\")
  
  (mapv comprehensive-analysis texts))

;; -- Export Functions --

(defn export-results
  \"Export analysis results as map\"
  [analysis-result]
  {:text-length (:text-length analysis-result)
   :models-analyzed (:models-analyzed analysis-result)
   :successful-analyses (:successful-analyses analysis-result)
   :average-score (:average-score analysis-result)
   :top-10 (map #(select-keys % [:model :score :confidence]) (:top-10-models analysis-result))
   :lollapalooza {:detected (:lollapalooza-detected analysis-result)
                  :convergence-score (:convergence-score analysis-result)
                  :convergence-count (:convergence-count analysis-result)
                  :models (:lollapalooza-models analysis-result)}
   :timestamp (:analysis-timestamp analysis-result)})

(defn export-json
  \"Export results as JSON string\"
  [analysis-result]
  (let [exportable (export-results analysis-result)]
    (clojure.data.json/write-str exportable)))

;; -- Performance Metrics --

(defn calculate-performance-metrics
  \"Calculate performance metrics for analysis\"
  [analysis-results]
  (let [scores (map :average-score analysis-results)
        lollapalooza-count (count (filter :lollapalooza-detected analysis-results))]
    
    {:total-analyses (count analysis-results)
     :avg-score (stats/mean scores)
     :min-score (apply min scores)
     :max-score (apply max scores)
     :lollapalooza-events lollapalooza-count
     :lollapalooza-rate (/ lollapalooza-count (count analysis-results))}))

;; -- Streaming Analysis (for Electric reactive updates) --

(defn streaming-analysis
  \"Analyze text with streaming progress updates\"
  [text progress-callback]
  (let [all-detectors (seq complete-model-registry)
        total (count all-detectors)
        
        results (loop [detectors all-detectors
                      completed 0
                      accumulated []]
                 (if (empty? detectors)
                   accumulated
                   (let [[slug detector] (first detectors)
                         result (detector text)
                         new-result (assoc result :model-slug slug)]
                     
                     ;; Call progress callback
                     (when progress-callback
                       (progress-callback {:completed (inc completed) :total total}))
                     
                     (recur (rest detectors)
                           (inc completed)
                           (conj accumulated new-result)))))]
    
    (filter #(> (:score %) 0.0) results)))

;; -- Utilities --

(defn model-by-slug
  \"Get model detection by slug\"
  [results slug]
  (first (filter #(= (:model-slug %) slug) results)))

(defn models-by-category
  \"Group models by category\"
  [results category]
  (filter #(= (:category %) category) results))

(defn confidence-distribution
  \"Get distribution of confidence levels\"
  [results]
  (let [high (count (filter #(> (:score %) 0.7) results))
        medium (count (filter #(and (> (:score %) 0.4) (<= (:score %) 0.7)) results))
        low (count (filter #(<= (:score %) 0.4) results))]
    {:high high :medium medium :low low}))

(defn generate-summary
  \"Generate human-readable summary of analysis\"
  [analysis-result]
  (let [top-models (take 5 (:top-10-models analysis-result))
        lollapalooza? (:lollapalooza-detected analysis-result)]
    
    (str
     \"Analysis Summary:\\n\"
     \"================\\n\"
     \"Text Length: \" (:text-length analysis-result) \" characters\\n\"
     \"Models Analyzed: \" (:models-analyzed analysis-result) \"\\n\"
     \"Average Score: \" (format \"%.2f\" (:average-score analysis-result)) \"\\n\"
     \"\\n\"
     \"Top 5 Detected Models:\\n\"
     (str/join \"\\n\" (map-indexed (fn [idx model]
                                   (str (inc idx) \". \" (:model model) 
                                        \" (\" (format \"%.1f%%\" (* (:score model) 100)) \")\"))
                                 top-models))
     \"\\n\"
     (when lollapalooza?
       (str \"\\n🔥 LOLLAPALOOZA EFFECT DETECTED!\\n\"
            \"Convergence Score: \" (format \"%.2f\" (:convergence-score analysis-result)) \"\\n\"
            \"Converging Models: \" (str/join \", \" (:lollapalooza-models analysis-result)))))))
