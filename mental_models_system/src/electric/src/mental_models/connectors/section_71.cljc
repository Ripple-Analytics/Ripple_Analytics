(ns mental-models.connectors.section-71
  "Connectors Module - Section 71"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def priority-queue
  "Priority queue for request scheduling."
  (atom {:high []
         :normal []
         :low []
         :background []}))

(def priority-config
  "Configuration for request prioritization."
  (atom {:enabled true
         :high-weight 4
         :normal-weight 2
         :low-weight 1
         :background-weight 0.5
         :max-queue-size 1000}))

(defn get-priority-config
  "Get current priority configuration."
  []
  @priority-config)

(defn set-priority-config
  "Update priority configuration."
  [config]
  (swap! priority-config merge config))

(defn enqueue-request
  "Add a request to the priority queue."
  [priority request-fn & {:keys [id metadata] :or {id (str (random-uuid)) metadata {}}}]
  (let [entry {:id id
               :fn request-fn
               :priority priority
               :metadata metadata
               :enqueued-at (System/currentTimeMillis)}]
    (swap! priority-queue update priority conj entry)
    id))

(defn dequeue-request
  "Get the next request to process based on priority weights."
  []
  (let [queue @priority-queue
        config @priority-config
        weighted-selection (fn []
                            (let [weights {:high (:high-weight config)
                                          :normal (:normal-weight config)
                                          :low (:low-weight config)
                                          :background (:background-weight config)}
                                  available (filter #(seq (get queue %)) (keys weights))
                                  total-weight (reduce + (map #(get weights %) available))]
                              (when (pos? total-weight)
                                (let [r (* (rand) total-weight)]
                                  (loop [remaining r
                                         [p & ps] available]
                                    (if p
                                      (let [w (get weights p)]
                                        (if (< remaining w)
                                          p
                                          (recur (- remaining w) ps)))
                                      (first available)))))))]
    (when-let [priority (weighted-selection)]
      (let [entries (get queue priority)]
        (when (seq entries)
          (let [entry (first entries)]
            (swap! priority-queue update priority rest)
            entry))))))

(defn process-priority-queue
  "Process requests from the priority queue."
  [& {:keys [max-concurrent] :or {max-concurrent 4}}]
  (let [results (atom [])]
    (dotimes [_ max-concurrent]
      (when-let [entry (dequeue-request)]
        (try
          (let [result ((:fn entry))]
            (swap! results conj {:id (:id entry)
                                :success true
                                :result result}))
          (catch Exception e
            (swap! results conj {:id (:id entry)
                                :success false
                                :error (.getMessage e)})))))
    @results))

(defn get-queue-stats
  "Get statistics about the priority queue."
  []
  (let [queue @priority-queue]
    {:high-count (count (:high queue))
     :normal-count (count (:normal queue))
     :low-count (count (:low queue))
     :background-count (count (:background queue))
     :total-count (reduce + (map count (vals queue)))}))

(defn clear-priority-queue
  "Clear all requests from the priority queue."
  []
  (reset! priority-queue {:high [] :normal [] :low [] :background []}))

;; ============================================
;; Request Timeout Escalation
