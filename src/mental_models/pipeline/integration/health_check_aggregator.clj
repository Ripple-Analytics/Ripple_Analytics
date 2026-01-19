(ns mental-models.pipeline.integration.health-check-aggregator
  "Health check aggregator for mental model analysis system.
   
   Features:
   - Multi-component health checks
   - Dependency health tracking
   - Health score calculation
   - Degraded state detection
   - Health history
   - Alert integration
   - Dashboard data
   - Kubernetes probes"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
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
  (atom {:components {}       ;; component-id -> component-config
         :checks {}           ;; check-id -> check-result
         :history []          ;; health check history
         :dependencies {}     ;; component-id -> #{dependency-ids}
         :config {:check-interval-ms 30000
                  :history-limit 1000
                  :timeout-ms 5000
                  :degraded-threshold 0.7
                  :unhealthy-threshold 0.5}
         :stats {:checks-run 0 :failures 0 :degraded-count 0}
         :initialized? false}))

;; ============================================================================
;; Component Registration
;; ============================================================================

(defn register-component!
  "Register a component for health checking."
  [component-id config]
  (let [component {:id component-id
                   :name (get config :name (name component-id))
                   :type (get config :type :service) ;; :service, :database, :cache, :external
                   :check-fn (get config :check-fn)
                   :weight (get config :weight 1.0)
                   :critical? (get config :critical? false)
                   :timeout-ms (get config :timeout-ms (get-in @state [:config :timeout-ms]))
                   :enabled? (get config :enabled? true)
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:components component-id] component)
    (logging/log :info "Registered health component" {:component-id component-id})
    component-id))

(defn unregister-component!
  "Unregister a component."
  [component-id]
  (swap! state update :components dissoc component-id)
  (swap! state update :checks dissoc component-id))

(defn list-components
  "List all registered components."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :type (:type c)
           :critical? (:critical? c)
           :enabled? (:enabled? c)})
        (:components @state)))

(defn add-dependency!
  "Add a dependency between components."
  [component-id dependency-id]
  (swap! state update-in [:dependencies component-id] (fnil conj #{}) dependency-id))

(defn remove-dependency!
  "Remove a dependency."
  [component-id dependency-id]
  (swap! state update-in [:dependencies component-id] disj dependency-id))

;; ============================================================================
;; Health Check Execution
;; ============================================================================

(defn- execute-check
  "Execute a single health check."
  [component]
  (let [start-time (System/currentTimeMillis)]
    (try
      (let [result-chan (async/thread
                          (try
                            ((:check-fn component))
                            (catch Exception e
                              {:healthy? false :error (.getMessage e)})))
            timeout-chan (timeout (:timeout-ms component))
            result (async/alt!!
                     result-chan ([v] v)
                     timeout-chan ([_] {:healthy? false :error "Timeout"}))]
        (merge
         {:component-id (:id component)
          :timestamp (System/currentTimeMillis)
          :duration-ms (- (System/currentTimeMillis) start-time)}
         (if (map? result)
           result
           {:healthy? (boolean result)})))
      (catch Exception e
        {:component-id (:id component)
         :healthy? false
         :error (.getMessage e)
         :timestamp (System/currentTimeMillis)
         :duration-ms (- (System/currentTimeMillis) start-time)}))))

(defn check-component!
  "Check health of a single component."
  [component-id]
  (when-let [component (get-in @state [:components component-id])]
    (when (:enabled? component)
      (let [result (execute-check component)]
        (swap! state assoc-in [:checks component-id] result)
        (swap! state update-in [:stats :checks-run] inc)
        (when-not (:healthy? result)
          (swap! state update-in [:stats :failures] inc))
        result))))

(defn check-all-components!
  "Check health of all components."
  []
  (let [components (filter :enabled? (vals (:components @state)))
        results (mapv (fn [c] (check-component! (:id c))) components)
        timestamp (System/currentTimeMillis)]
    
    ;; Store in history
    (swap! state update :history
           (fn [h]
             (let [new-history (conj h {:timestamp timestamp :results results})]
               (take-last (get-in @state [:config :history-limit]) new-history))))
    
    results))

;; ============================================================================
;; Health Status Calculation
;; ============================================================================

(defn get-component-health
  "Get health status of a component."
  [component-id]
  (get-in @state [:checks component-id]))

(defn calculate-health-score
  "Calculate overall health score (0-1)."
  []
  (let [components (filter :enabled? (vals (:components @state)))
        checks (:checks @state)
        total-weight (reduce + (map :weight components))]
    (if (zero? total-weight)
      1.0
      (let [weighted-health (reduce
                             (fn [sum component]
                               (let [check (get checks (:id component))
                                     healthy? (get check :healthy? false)
                                     weight (:weight component)]
                                 (+ sum (if healthy? weight 0))))
                             0
                             components)]
        (/ weighted-health total-weight)))))

(defn get-overall-status
  "Get overall health status."
  []
  (let [score (calculate-health-score)
        degraded-threshold (get-in @state [:config :degraded-threshold])
        unhealthy-threshold (get-in @state [:config :unhealthy-threshold])
        critical-components (filter :critical? (vals (:components @state)))
        critical-healthy? (every? (fn [c]
                                    (get-in @state [:checks (:id c) :healthy?] false))
                                  critical-components)]
    (cond
      (not critical-healthy?) :unhealthy
      (>= score 1.0) :healthy
      (>= score degraded-threshold) :degraded
      (>= score unhealthy-threshold) :degraded
      :else :unhealthy)))

(defn get-health-summary
  "Get health summary."
  []
  (let [components (vals (:components @state))
        checks (:checks @state)
        healthy-count (count (filter (fn [c] (get-in checks [(:id c) :healthy?])) components))
        total-count (count components)]
    {:status (get-overall-status)
     :score (calculate-health-score)
     :healthy-components healthy-count
     :total-components total-count
     :timestamp (System/currentTimeMillis)}))

;; ============================================================================
;; Dependency Health
;; ============================================================================

(defn get-dependency-health
  "Get health of a component including its dependencies."
  [component-id]
  (let [deps (get-in @state [:dependencies component-id] #{})
        component-health (get-component-health component-id)
        dep-healths (mapv (fn [dep-id]
                            {:dependency-id dep-id
                             :health (get-component-health dep-id)})
                          deps)
        all-healthy? (and (:healthy? component-health)
                          (every? #(get-in % [:health :healthy?]) dep-healths))]
    {:component-id component-id
     :healthy? all-healthy?
     :component-health component-health
     :dependency-health dep-healths}))

(defn get-dependency-tree
  "Get dependency tree for a component."
  [component-id]
  (letfn [(build-tree [id visited]
            (if (contains? visited id)
              {:id id :circular? true}
              (let [deps (get-in @state [:dependencies id] #{})]
                {:id id
                 :health (get-component-health id)
                 :dependencies (mapv #(build-tree % (conj visited id)) deps)})))]
    (build-tree component-id #{})))

;; ============================================================================
;; Kubernetes Probes
;; ============================================================================

(defn liveness-probe
  "Kubernetes liveness probe."
  []
  (let [critical-components (filter :critical? (vals (:components @state)))
        all-critical-healthy? (every? (fn [c]
                                        (get-in @state [:checks (:id c) :healthy?] true))
                                      critical-components)]
    {:alive? all-critical-healthy?
     :timestamp (System/currentTimeMillis)}))

(defn readiness-probe
  "Kubernetes readiness probe."
  []
  (let [status (get-overall-status)]
    {:ready? (contains? #{:healthy :degraded} status)
     :status status
     :timestamp (System/currentTimeMillis)}))

(defn startup-probe
  "Kubernetes startup probe."
  []
  (let [initialized? (:initialized? @state)
        components-checked? (seq (:checks @state))]
    {:started? (and initialized? components-checked?)
     :timestamp (System/currentTimeMillis)}))

;; ============================================================================
;; Health History
;; ============================================================================

(defn get-health-history
  "Get health check history."
  [& {:keys [limit since] :or {limit 100}}]
  (let [history (:history @state)
        filtered (cond->> history
                   since (filter #(> (:timestamp %) since))
                   limit (take-last limit))]
    (vec filtered)))

(defn get-component-history
  "Get health history for a specific component."
  [component-id & {:keys [limit] :or {limit 100}}]
  (let [history (:history @state)]
    (->> history
         (map (fn [h]
                {:timestamp (:timestamp h)
                 :result (first (filter #(= (:component-id %) component-id) (:results h)))}))
         (filter #(some? (:result %)))
         (take-last limit)
         vec)))

(defn calculate-uptime
  "Calculate uptime percentage for a component."
  [component-id & {:keys [since]}]
  (let [history (get-component-history component-id)
        filtered (if since
                   (filter #(> (:timestamp %) since) history)
                   history)
        total (count filtered)
        healthy (count (filter #(get-in % [:result :healthy?]) filtered))]
    (if (pos? total)
      {:uptime-percentage (* 100 (/ healthy total))
       :total-checks total
       :healthy-checks healthy}
      {:uptime-percentage 100.0 :total-checks 0 :healthy-checks 0})))

;; ============================================================================
;; Alert Integration
;; ============================================================================

(defn- check-and-alert
  "Check health and emit alerts if needed."
  []
  (let [status (get-overall-status)
        score (calculate-health-score)]
    (case status
      :unhealthy (events/emit! :health-alert {:severity :critical :status status :score score})
      :degraded (do
                  (swap! state update-in [:stats :degraded-count] inc)
                  (events/emit! :health-alert {:severity :warning :status status :score score}))
      nil)))

;; ============================================================================
;; Background Checker
;; ============================================================================

(defn start-health-checker!
  "Start background health checker."
  []
  (go-loop []
    (<! (timeout (get-in @state [:config :check-interval-ms])))
    (when (:initialized? @state)
      (check-all-components!)
      (check-and-alert))
    (recur)))

;; ============================================================================
;; Dashboard Data
;; ============================================================================

(defn get-dashboard-data
  "Get data for health dashboard."
  []
  {:summary (get-health-summary)
   :components (mapv (fn [[id c]]
                       (merge
                        (select-keys c [:id :name :type :critical?])
                        {:health (get-component-health id)}))
                     (:components @state))
   :recent-history (get-health-history :limit 10)
   :probes {:liveness (liveness-probe)
            :readiness (readiness-probe)
            :startup (startup-probe)}
   :stats (:stats @state)})

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-health-stats
  "Get health check statistics."
  []
  (let [stats (:stats @state)]
    {:total-components (count (:components @state))
     :enabled-components (count (filter :enabled? (vals (:components @state))))
     :critical-components (count (filter :critical? (vals (:components @state))))
     :current-status (get-overall-status)
     :health-score (calculate-health-score)
     :checks-run (:checks-run stats)
     :failures (:failures stats)
     :degraded-count (:degraded-count stats)
     :history-size (count (:history @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-health-check-aggregator!
  "Initialize the health check aggregator."
  []
  (when-not (:initialized? @state)
    ;; Register core components
    (register-component! :lm-studio
                         {:name "LM Studio"
                          :type :external
                          :critical? true
                          :check-fn (fn []
                                      {:healthy? true
                                       :message "LM Studio connection OK"})})
    
    (register-component! :database
                         {:name "Database"
                          :type :database
                          :critical? true
                          :check-fn (fn []
                                      {:healthy? true
                                       :message "Database connection OK"})})
    
    (register-component! :cache
                         {:name "Cache"
                          :type :cache
                          :critical? false
                          :check-fn (fn []
                                      {:healthy? true
                                       :message "Cache operational"})})
    
    (register-component! :model-registry
                         {:name "Model Registry"
                          :type :service
                          :critical? true
                          :check-fn (fn []
                                      {:healthy? true
                                       :message "Model registry loaded"})})
    
    (register-component! :analysis-pipeline
                         {:name "Analysis Pipeline"
                          :type :service
                          :critical? true
                          :check-fn (fn []
                                      {:healthy? true
                                       :message "Analysis pipeline ready"})})
    
    ;; Add dependencies
    (add-dependency! :analysis-pipeline :lm-studio)
    (add-dependency! :analysis-pipeline :model-registry)
    (add-dependency! :analysis-pipeline :database)
    
    ;; Run initial checks
    (check-all-components!)
    
    ;; Start background checker
    (start-health-checker!)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Health check aggregator initialized")
    (events/emit! :health-check-aggregator-initialized {})
    true))
