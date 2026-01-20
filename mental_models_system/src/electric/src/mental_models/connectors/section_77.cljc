(ns mental-models.connectors.section-77
  "Connectors Module - Section 77"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def trace-store
  "Store for request traces."
  (atom {}))

(def trace-config
  "Configuration for request tracing."
  (atom {:enabled true
         :max-traces 10000
         :retention-ms 3600000
         :include-headers false
         :include-body false}))

(defn get-trace-config
  "Get current trace configuration."
  []
  @trace-config)

(defn set-trace-config
  "Update trace configuration."
  [config]
  (swap! trace-config merge config))

(defn generate-trace-id
  "Generate a unique trace ID."
  []
  (str "trace-" (System/currentTimeMillis) "-" (random-uuid)))

(defn generate-span-id
  "Generate a unique span ID."
  []
  (str "span-" (random-uuid)))

(defn start-trace
  "Start a new trace."
  [& {:keys [trace-id operation metadata] :or {trace-id (generate-trace-id) operation "unknown" metadata {}}}]
  (let [trace {:trace-id trace-id
               :operation operation
               :metadata metadata
               :started-at (System/currentTimeMillis)
               :spans []
               :status :in-progress}]
    (swap! trace-store assoc trace-id trace)
    trace-id))

(defn start-span
  "Start a new span within a trace."
  [trace-id span-name & {:keys [parent-span-id metadata] :or {metadata {}}}]
  (let [span-id (generate-span-id)
        span {:span-id span-id
              :name span-name
              :parent-span-id parent-span-id
              :metadata metadata
              :started-at (System/currentTimeMillis)
              :status :in-progress}]
    (swap! trace-store update-in [trace-id :spans] conj span)
    span-id))

(defn end-span
  "End a span within a trace."
  [trace-id span-id & {:keys [status result error] :or {status :completed}}]
  (swap! trace-store update-in [trace-id :spans]
         (fn [spans]
           (mapv (fn [span]
                   (if (= (:span-id span) span-id)
                     (assoc span
                            :ended-at (System/currentTimeMillis)
                            :duration-ms (- (System/currentTimeMillis) (:started-at span))
                            :status status
                            :result result
                            :error error)
                     span))
                 spans))))

(defn end-trace
  "End a trace."
  [trace-id & {:keys [status result error] :or {status :completed}}]
  (swap! trace-store update trace-id
         (fn [trace]
           (assoc trace
                  :ended-at (System/currentTimeMillis)
                  :duration-ms (- (System/currentTimeMillis) (:started-at trace))
                  :status status
                  :result result
                  :error error))))

(defn get-trace
  "Get a trace by ID."
  [trace-id]
  (get @trace-store trace-id))

(defn with-tracing
  "Execute a function with tracing."
  [operation f & {:keys [metadata] :or {metadata {}}}]
  (if-not (:enabled @trace-config)
    (f)
    (let [trace-id (start-trace :operation operation :metadata metadata)
          span-id (start-span trace-id operation)]
      (try
        (let [result (f)]
          (end-span trace-id span-id :status :completed :result (when (:include-body @trace-config) result))
          (end-trace trace-id :status :completed)
          result)
        (catch Exception e
          (end-span trace-id span-id :status :failed :error (.getMessage e))
          (end-trace trace-id :status :failed :error (.getMessage e))
          (throw e))))))

(defn cleanup-old-traces
  "Remove traces older than retention period."
  []
  (let [config @trace-config
        cutoff (- (System/currentTimeMillis) (:retention-ms config))]
    (swap! trace-store
           (fn [store]
             (into {}
                   (filter (fn [[_ trace]]
                            (> (:started-at trace) cutoff))
                           store))))))

(defn get-trace-stats
  "Get statistics about traces."
  []
  (let [traces (vals @trace-store)
        completed (filter #(= (:status %) :completed) traces)
        failed (filter #(= (:status %) :failed) traces)]
    {:total-traces (count traces)
     :completed-traces (count completed)
     :failed-traces (count failed)
     :in-progress-traces (count (filter #(= (:status %) :in-progress) traces))
     :avg-duration-ms (if (seq completed)
                        (/ (reduce + (map :duration-ms completed)) (count completed))
                        0)}))

;; ============================================
;; Request Compression
