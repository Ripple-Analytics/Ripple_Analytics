(ns mental-models.pipeline.integration.response-inspector
  "Response inspector for mental model analysis system.
   
   Features:
   - Response inspection
   - Schema inspection
   - Type inspection
   - Structure analysis
   - Validation inspection
   - Performance inspection
   - Inspection reports
   - Inspection metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
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
  (atom {:inspectors {}       ;; inspector-id -> inspector
         :reports []          ;; inspection reports
         :config {:max-depth 10
                  :max-reports 10000
                  :include-samples? true
                  :sample-size 3}
         :stats {:inspections 0
                 :fields-inspected 0
                 :issues-found 0
                 :reports-generated 0}
         :initialized? false}))

;; ============================================================================
;; Type Detection
;; ============================================================================

(defn detect-type
  "Detect the type of a value."
  [value]
  (cond
    (nil? value) :null
    (boolean? value) :boolean
    (integer? value) :integer
    (float? value) :float
    (number? value) :number
    (string? value) (cond
                      (re-matches #"\d{4}-\d{2}-\d{2}.*" value) :date-string
                      (re-matches #"[a-f0-9-]{36}" value) :uuid-string
                      (re-matches #"https?://.*" value) :url-string
                      (re-matches #"[\w.+-]+@[\w.-]+\.\w+" value) :email-string
                      :else :string)
    (keyword? value) :keyword
    (symbol? value) :symbol
    (map? value) :map
    (vector? value) :vector
    (list? value) :list
    (set? value) :set
    (sequential? value) :sequence
    :else :unknown))

(defn detect-types-in-collection
  "Detect types in a collection."
  [coll]
  (let [types (map detect-type coll)]
    {:types (distinct types)
     :homogeneous? (= 1 (count (distinct types)))
     :count (count coll)}))

;; ============================================================================
;; Schema Inference
;; ============================================================================

(defn infer-schema
  "Infer schema from a value."
  [value depth max-depth]
  (if (>= depth max-depth)
    {:type :max-depth-reached}
    (let [value-type (detect-type value)]
      (case value-type
        :map {:type :map
              :fields (into {}
                            (for [[k v] value]
                              [k (infer-schema v (inc depth) max-depth)]))}
        (:vector :list :sequence)
        {:type :array
         :items (when (seq value)
                  (let [item-schemas (map #(infer-schema % (inc depth) max-depth) value)
                        unique-schemas (distinct item-schemas)]
                    (if (= 1 (count unique-schemas))
                      (first unique-schemas)
                      {:type :mixed :schemas (vec unique-schemas)})))
         :count (count value)}
        :set {:type :set
              :items (when (seq value)
                       (infer-schema (first value) (inc depth) max-depth))
              :count (count value)}
        {:type value-type}))))

(defn infer-response-schema
  "Infer schema from a response."
  [response]
  (let [max-depth (get-in @state [:config :max-depth])]
    {:status-type (detect-type (:status response))
     :headers-schema (when (:headers response)
                       (infer-schema (:headers response) 0 max-depth))
     :body-schema (when (:body response)
                    (infer-schema (:body response) 0 max-depth))}))

;; ============================================================================
;; Structure Analysis
;; ============================================================================

(defn analyze-structure
  "Analyze the structure of a value."
  [value]
  (let [max-depth (get-in @state [:config :max-depth])]
    (letfn [(analyze [v depth path]
              (if (>= depth max-depth)
                {:path path :type :max-depth}
                (let [v-type (detect-type v)]
                  (case v-type
                    :map (concat [{:path path :type :map :keys (keys v)}]
                                 (mapcat (fn [[k val]]
                                           (analyze val (inc depth) (conj path k)))
                                         v))
                    (:vector :list :sequence)
                    (concat [{:path path :type :array :count (count v)}]
                            (when (seq v)
                              (analyze (first v) (inc depth) (conj path 0))))
                    [{:path path :type v-type}]))))]
      (vec (analyze value 0 [])))))

(defn get-field-paths
  "Get all field paths in a value."
  [value]
  (let [structure (analyze-structure value)]
    (mapv :path structure)))

(defn get-leaf-values
  "Get all leaf values with their paths."
  [value]
  (let [structure (analyze-structure value)]
    (filter #(not (#{:map :array} (:type %))) structure)))

;; ============================================================================
;; Validation Inspection
;; ============================================================================

(defn inspect-for-issues
  "Inspect a value for common issues."
  [value]
  (let [issues (atom [])]
    (walk/postwalk
     (fn [x]
       (cond
         (nil? x) (swap! issues conj {:type :null-value :value x})
         (and (string? x) (empty? x)) (swap! issues conj {:type :empty-string :value x})
         (and (coll? x) (empty? x)) (swap! issues conj {:type :empty-collection :value x})
         (and (number? x) (Double/isNaN x)) (swap! issues conj {:type :nan-value :value x})
         (and (number? x) (Double/isInfinite x)) (swap! issues conj {:type :infinite-value :value x}))
       x)
     value)
    (swap! state update-in [:stats :issues-found] + (count @issues))
    @issues))

(defn inspect-response-issues
  "Inspect a response for issues."
  [response]
  {:status-issues (when-not (<= 200 (:status response) 299)
                    [{:type :non-success-status :status (:status response)}])
   :header-issues (inspect-for-issues (:headers response))
   :body-issues (inspect-for-issues (:body response))})

;; ============================================================================
;; Performance Inspection
;; ============================================================================

(defn inspect-size
  "Inspect the size of a value."
  [value]
  (let [str-repr (pr-str value)]
    {:bytes (count (.getBytes str-repr "UTF-8"))
     :chars (count str-repr)}))

(defn inspect-response-performance
  "Inspect response performance characteristics."
  [response & {:keys [duration-ms]}]
  {:size (inspect-size response)
   :body-size (when (:body response)
                (inspect-size (:body response)))
   :duration-ms duration-ms
   :headers-count (count (:headers response))
   :body-depth (when (:body response)
                 (count (analyze-structure (:body response))))})

;; ============================================================================
;; Custom Inspectors
;; ============================================================================

(defn register-inspector!
  "Register a custom inspector."
  [inspector-id config]
  (let [inspector {:id inspector-id
                   :name (get config :name (name inspector-id))
                   :inspect-fn (get config :inspect-fn)
                   :condition-fn (get config :condition-fn (constantly true))
                   :enabled? (atom true)
                   :metrics {:invocations (atom 0)}
                   :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:inspectors inspector-id] inspector)
    (logging/log :info "Registered inspector" {:inspector-id inspector-id})
    inspector-id))

(defn get-inspector
  "Get an inspector."
  [inspector-id]
  (get-in @state [:inspectors inspector-id]))

(defn list-inspectors
  "List all inspectors."
  []
  (mapv (fn [[id i]]
          {:id id
           :name (:name i)
           :enabled? @(:enabled? i)
           :invocations @(get-in i [:metrics :invocations])})
        (:inspectors @state)))

(defn run-inspector
  "Run a specific inspector."
  [inspector-id value]
  (when-let [inspector (get-inspector inspector-id)]
    (when (and @(:enabled? inspector)
               ((:condition-fn inspector) value))
      (swap! (get-in inspector [:metrics :invocations]) inc)
      ((:inspect-fn inspector) value))))

(defn run-all-inspectors
  "Run all matching inspectors."
  [value]
  (into {}
        (for [[id inspector] (:inspectors @state)
              :when (and @(:enabled? inspector)
                         ((:condition-fn inspector) value))]
          (do
            (swap! (get-in inspector [:metrics :invocations]) inc)
            [id ((:inspect-fn inspector) value)]))))

;; ============================================================================
;; Inspection Reports
;; ============================================================================

(defn generate-report
  "Generate a comprehensive inspection report."
  [response & {:keys [duration-ms request]}]
  (swap! state update-in [:stats :inspections] inc)
  (swap! state update-in [:stats :reports-generated] inc)
  
  (let [report {:id (str (UUID/randomUUID))
                :timestamp (System/currentTimeMillis)
                :request (when request
                           {:method (:request-method request)
                            :uri (:uri request)})
                :response {:status (:status response)
                           :headers-count (count (:headers response))}
                :schema (infer-response-schema response)
                :structure (analyze-structure (:body response))
                :issues (inspect-response-issues response)
                :performance (inspect-response-performance response
                                                           :duration-ms duration-ms)
                :custom-inspections (run-all-inspectors response)}
        max-reports (get-in @state [:config :max-reports])]
    
    (swap! state update :reports
           (fn [r]
             (let [new-reports (conj r report)]
               (if (> (count new-reports) max-reports)
                 (vec (drop 1 new-reports))
                 new-reports))))
    
    report))

(defn get-reports
  "Get inspection reports."
  [& {:keys [limit since with-issues?] :or {limit 100}}]
  (cond->> (:reports @state)
    since (filter #(> (:timestamp %) since))
    with-issues? (filter (fn [r]
                           (or (seq (get-in r [:issues :status-issues]))
                               (seq (get-in r [:issues :body-issues])))))
    true (take-last limit)
    true vec))

(defn get-report
  "Get a specific report."
  [report-id]
  (first (filter #(= (:id %) report-id) (:reports @state))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-inspect
  "Ring middleware to inspect responses."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)]
      (generate-report response :duration-ms duration-ms :request request)
      response)))

(defn wrap-inspect-and-log
  "Ring middleware to inspect and log responses."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          response (handler request)
          duration-ms (- (System/currentTimeMillis) start-time)
          report (generate-report response :duration-ms duration-ms :request request)]
      (when (seq (get-in report [:issues :body-issues]))
        (logging/log :warn "Response has issues" {:issues (get-in report [:issues :body-issues])}))
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-depth!
  "Set maximum inspection depth."
  [depth]
  (swap! state assoc-in [:config :max-depth] depth))

(defn set-max-reports!
  "Set maximum reports to keep."
  [max-reports]
  (swap! state assoc-in [:config :max-reports] max-reports))

(defn set-include-samples!
  "Enable/disable sample inclusion."
  [enabled?]
  (swap! state assoc-in [:config :include-samples?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-inspector-metrics
  "Get inspector metrics."
  []
  (let [stats (:stats @state)]
    {:inspections (:inspections stats)
     :fields-inspected (:fields-inspected stats)
     :issues-found (:issues-found stats)
     :reports-generated (:reports-generated stats)
     :inspectors-count (count (:inspectors @state))
     :reports-count (count (:reports @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-inspector-stats
  "Get inspector statistics."
  []
  (merge (get-inspector-metrics)
         {:max-depth (get-in @state [:config :max-depth])
          :max-reports (get-in @state [:config :max-reports])
          :include-samples? (get-in @state [:config :include-samples?])}))

(defn reset-stats!
  "Reset inspector statistics."
  []
  (swap! state assoc :stats {:inspections 0
                             :fields-inspected 0
                             :issues-found 0
                             :reports-generated 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-inspector!
  "Initialize the response inspector."
  []
  (when-not (:initialized? @state)
    ;; Register default inspectors
    (register-inspector! :null-check
                         {:name "Null Value Check"
                          :inspect-fn (fn [v]
                                        {:has-nulls? (boolean
                                                      (some nil?
                                                            (tree-seq coll? seq v)))})})
    
    (register-inspector! :size-check
                         {:name "Size Check"
                          :inspect-fn inspect-size})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response inspector initialized")
    (events/emit! :response-inspector-initialized {})
    true))
