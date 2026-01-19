(ns mental-models.pipeline.integration.capacity-planner
  "Capacity planner for mental model analysis infrastructure.
   
   Features:
   - Resource forecasting
   - Capacity modeling
   - Scaling recommendations
   - Load prediction
   - Cost projection
   - Bottleneck detection
   - Growth planning
   - SLA compliance"
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
  (atom {:resources {}        ;; resource-id -> resource
         :metrics-history []  ;; historical metrics
         :forecasts {}        ;; forecast-id -> forecast
         :plans {}            ;; plan-id -> capacity-plan
         :slas {}             ;; sla-id -> sla
         :config {:forecast-window-days 30
                  :history-retention-days 90
                  :default-growth-rate 0.1}
         :stats {:forecasts-generated 0 :plans-created 0}
         :initialized? false}))

;; ============================================================================
;; Resource Management
;; ============================================================================

(defn register-resource!
  "Register a resource for capacity planning."
  [resource-id config]
  (let [resource {:id resource-id
                  :name (get config :name (name resource-id))
                  :type (get config :type :compute) ;; :compute, :memory, :storage, :network, :api
                  :unit (get config :unit :count)
                  :current-capacity (get config :current-capacity 0)
                  :max-capacity (get config :max-capacity 100)
                  :cost-per-unit (get config :cost-per-unit 0)
                  :scaling-increment (get config :scaling-increment 1)
                  :min-capacity (get config :min-capacity 0)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:resources resource-id] resource)
    (logging/log :info "Registered resource" {:resource-id resource-id})
    resource-id))

(defn get-resource
  "Get a resource."
  [resource-id]
  (get-in @state [:resources resource-id]))

(defn list-resources
  "List all resources."
  []
  (mapv (fn [[id r]]
          {:id id
           :name (:name r)
           :type (:type r)
           :current-capacity (:current-capacity r)
           :max-capacity (:max-capacity r)
           :utilization (if (pos? (:max-capacity r))
                          (/ (:current-capacity r) (:max-capacity r))
                          0)})
        (:resources @state)))

(defn update-resource-capacity!
  "Update resource capacity."
  [resource-id capacity]
  (swap! state assoc-in [:resources resource-id :current-capacity] capacity)
  (swap! state assoc-in [:resources resource-id :updated-at] (System/currentTimeMillis)))

;; ============================================================================
;; Metrics Collection
;; ============================================================================

(defn record-metrics!
  "Record resource metrics."
  [resource-id metrics]
  (let [record {:resource-id resource-id
                :utilization (get metrics :utilization 0)
                :requests (get metrics :requests 0)
                :latency-ms (get metrics :latency-ms 0)
                :errors (get metrics :errors 0)
                :timestamp (System/currentTimeMillis)
                :date (str (LocalDate/now))}]
    (swap! state update :metrics-history conj record)
    record))

(defn get-metrics-history
  "Get metrics history."
  [resource-id & {:keys [days limit] :or {days 30 limit 1000}}]
  (let [cutoff (- (System/currentTimeMillis) (* days 24 60 60 1000))
        history (:metrics-history @state)
        filtered (cond->> history
                   resource-id (filter #(= (:resource-id %) resource-id))
                   true (filter #(>= (:timestamp %) cutoff))
                   limit (take-last limit))]
    (vec filtered)))

(defn cleanup-old-metrics!
  "Clean up old metrics."
  []
  (let [retention-days (get-in @state [:config :history-retention-days])
        cutoff (- (System/currentTimeMillis) (* retention-days 24 60 60 1000))]
    (swap! state update :metrics-history
           (fn [history]
             (vec (filter #(>= (:timestamp %) cutoff) history))))))

;; ============================================================================
;; Forecasting
;; ============================================================================

(defn- calculate-trend
  "Calculate trend from historical data."
  [data-points]
  (if (< (count data-points) 2)
    {:slope 0 :intercept (or (first data-points) 0)}
    (let [n (count data-points)
          xs (range n)
          ys data-points
          sum-x (reduce + xs)
          sum-y (reduce + ys)
          sum-xy (reduce + (map * xs ys))
          sum-x2 (reduce + (map #(* % %) xs))
          slope (/ (- (* n sum-xy) (* sum-x sum-y))
                   (- (* n sum-x2) (* sum-x sum-x)))
          intercept (/ (- sum-y (* slope sum-x)) n)]
      {:slope slope :intercept intercept})))

(defn- forecast-value
  "Forecast a value using linear regression."
  [trend steps-ahead]
  (let [{:keys [slope intercept]} trend]
    (+ intercept (* slope steps-ahead))))

(defn generate-forecast!
  "Generate a capacity forecast."
  [resource-id & {:keys [days] :or {days 30}}]
  (when (flags/enabled? :capacity-planner)
    (let [history (get-metrics-history resource-id :days 30)
          utilization-data (mapv :utilization history)
          requests-data (mapv :requests history)
          
          util-trend (calculate-trend utilization-data)
          req-trend (calculate-trend requests-data)
          
          forecast-id (str (UUID/randomUUID))
          forecast {:id forecast-id
                    :resource-id resource-id
                    :generated-at (System/currentTimeMillis)
                    :forecast-days days
                    :current-utilization (last utilization-data)
                    :current-requests (last requests-data)
                    :predictions (mapv (fn [day]
                                         {:day day
                                          :utilization (max 0 (min 1 (forecast-value util-trend (+ (count utilization-data) day))))
                                          :requests (max 0 (forecast-value req-trend (+ (count requests-data) day)))})
                                       (range 1 (inc days)))
                    :trend {:utilization util-trend
                            :requests req-trend}}]
      
      (swap! state assoc-in [:forecasts forecast-id] forecast)
      (swap! state update-in [:stats :forecasts-generated] inc)
      
      (logging/log :info "Generated forecast" {:resource-id resource-id :forecast-id forecast-id})
      forecast)))

(defn get-forecast
  "Get a forecast."
  [forecast-id]
  (get-in @state [:forecasts forecast-id]))

(defn get-latest-forecast
  "Get the latest forecast for a resource."
  [resource-id]
  (let [forecasts (filter #(= (:resource-id %) resource-id) (vals (:forecasts @state)))]
    (last (sort-by :generated-at forecasts))))

;; ============================================================================
;; Capacity Planning
;; ============================================================================

(defn create-capacity-plan!
  "Create a capacity plan."
  [plan-id config]
  (let [plan {:id plan-id
              :name (get config :name (name plan-id))
              :resources (get config :resources [])
              :target-utilization (get config :target-utilization 0.7)
              :growth-rate (get config :growth-rate (get-in @state [:config :default-growth-rate]))
              :planning-horizon-days (get config :planning-horizon-days 90)
              :recommendations []
              :status :draft ;; :draft, :approved, :implemented
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:plans plan-id] plan)
    (swap! state update-in [:stats :plans-created] inc)
    (logging/log :info "Created capacity plan" {:plan-id plan-id})
    plan-id))

(defn get-capacity-plan
  "Get a capacity plan."
  [plan-id]
  (get-in @state [:plans plan-id]))

(defn list-capacity-plans
  "List all capacity plans."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :status (:status p)
           :resource-count (count (:resources p))})
        (:plans @state)))

(defn- calculate-required-capacity
  "Calculate required capacity based on forecast."
  [resource forecast target-utilization]
  (let [max-predicted-util (apply max (map :utilization (:predictions forecast)))
        current-capacity (:current-capacity resource)
        required-capacity (if (pos? target-utilization)
                           (/ (* current-capacity max-predicted-util) target-utilization)
                           current-capacity)]
    {:current current-capacity
     :required (Math/ceil required-capacity)
     :max-predicted-utilization max-predicted-util
     :scaling-needed? (> required-capacity current-capacity)}))

(defn generate-recommendations!
  "Generate scaling recommendations for a plan."
  [plan-id]
  (when-let [plan (get-capacity-plan plan-id)]
    (let [recommendations (mapv (fn [resource-id]
                                  (let [resource (get-resource resource-id)
                                        forecast (or (get-latest-forecast resource-id)
                                                     (generate-forecast! resource-id))
                                        capacity-calc (calculate-required-capacity
                                                       resource forecast (:target-utilization plan))
                                        scaling-increment (:scaling-increment resource 1)
                                        units-to-add (if (:scaling-needed? capacity-calc)
                                                       (Math/ceil (/ (- (:required capacity-calc) (:current capacity-calc))
                                                                     scaling-increment))
                                                       0)
                                        cost-increase (* units-to-add (:cost-per-unit resource 0))]
                                    {:resource-id resource-id
                                     :resource-name (:name resource)
                                     :current-capacity (:current capacity-calc)
                                     :recommended-capacity (:required capacity-calc)
                                     :units-to-add units-to-add
                                     :cost-increase cost-increase
                                     :priority (cond
                                                 (> (:max-predicted-utilization capacity-calc) 0.9) :critical
                                                 (> (:max-predicted-utilization capacity-calc) 0.8) :high
                                                 (> (:max-predicted-utilization capacity-calc) 0.7) :medium
                                                 :else :low)}))
                                (:resources plan))]
      (swap! state assoc-in [:plans plan-id :recommendations] recommendations)
      (swap! state assoc-in [:plans plan-id :updated-at] (System/currentTimeMillis))
      recommendations)))

(defn approve-plan!
  "Approve a capacity plan."
  [plan-id]
  (swap! state assoc-in [:plans plan-id :status] :approved)
  (swap! state assoc-in [:plans plan-id :approved-at] (System/currentTimeMillis))
  (logging/log :info "Approved capacity plan" {:plan-id plan-id}))

(defn implement-plan!
  "Mark a plan as implemented."
  [plan-id]
  (swap! state assoc-in [:plans plan-id :status] :implemented)
  (swap! state assoc-in [:plans plan-id :implemented-at] (System/currentTimeMillis))
  (logging/log :info "Implemented capacity plan" {:plan-id plan-id}))

;; ============================================================================
;; SLA Management
;; ============================================================================

(defn create-sla!
  "Create an SLA."
  [sla-id config]
  (let [sla {:id sla-id
             :name (get config :name (name sla-id))
             :resource-id (get config :resource-id)
             :metrics (get config :metrics {}) ;; {:availability 0.999 :latency-p99 100}
             :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:slas sla-id] sla)
    (logging/log :info "Created SLA" {:sla-id sla-id})
    sla-id))

(defn get-sla
  "Get an SLA."
  [sla-id]
  (get-in @state [:slas sla-id]))

(defn list-slas
  "List all SLAs."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :resource-id (:resource-id s)})
        (:slas @state)))

(defn check-sla-compliance
  "Check SLA compliance."
  [sla-id & {:keys [days] :or {days 30}}]
  (when-let [sla (get-sla sla-id)]
    (let [history (get-metrics-history (:resource-id sla) :days days)
          sla-metrics (:metrics sla)
          
          ;; Calculate actual metrics
          total-records (count history)
          availability (if (pos? total-records)
                         (/ (count (filter #(< (:errors %) 1) history)) total-records)
                         1.0)
          avg-latency (if (pos? total-records)
                        (/ (reduce + (map :latency-ms history)) total-records)
                        0)
          
          ;; Check compliance
          compliance {:availability {:target (get sla-metrics :availability 0.99)
                                     :actual availability
                                     :compliant? (>= availability (get sla-metrics :availability 0.99))}
                      :latency {:target (get sla-metrics :latency-p99 100)
                                :actual avg-latency
                                :compliant? (<= avg-latency (get sla-metrics :latency-p99 100))}}]
      {:sla-id sla-id
       :period-days days
       :compliance compliance
       :overall-compliant? (every? :compliant? (vals compliance))})))

;; ============================================================================
;; Bottleneck Detection
;; ============================================================================

(defn detect-bottlenecks
  "Detect resource bottlenecks."
  []
  (let [resources (vals (:resources @state))]
    (mapv (fn [resource]
            (let [history (get-metrics-history (:id resource) :days 7)
                  avg-util (if (seq history)
                             (/ (reduce + (map :utilization history)) (count history))
                             0)
                  max-util (if (seq history)
                             (apply max (map :utilization history))
                             0)]
              {:resource-id (:id resource)
               :resource-name (:name resource)
               :avg-utilization avg-util
               :max-utilization max-util
               :is-bottleneck? (> max-util 0.85)
               :severity (cond
                           (> max-util 0.95) :critical
                           (> max-util 0.85) :high
                           (> max-util 0.75) :medium
                           :else :low)}))
          resources)))

;; ============================================================================
;; Cost Projection
;; ============================================================================

(defn project-costs
  "Project costs based on capacity plans."
  [plan-id]
  (when-let [plan (get-capacity-plan plan-id)]
    (let [recommendations (:recommendations plan)
          total-cost-increase (reduce + (map :cost-increase recommendations))
          current-monthly-cost (reduce + (map (fn [r]
                                                (* (:current-capacity r 0)
                                                   (get-in @state [:resources (:resource-id r) :cost-per-unit] 0)))
                                              recommendations))
          projected-monthly-cost (+ current-monthly-cost total-cost-increase)]
      {:plan-id plan-id
       :current-monthly-cost current-monthly-cost
       :projected-monthly-cost projected-monthly-cost
       :cost-increase total-cost-increase
       :cost-increase-percentage (if (pos? current-monthly-cost)
                                   (* 100 (/ total-cost-increase current-monthly-cost))
                                   0)
       :by-resource (mapv (fn [r]
                            {:resource-id (:resource-id r)
                             :current-cost (* (:current-capacity r 0)
                                              (get-in @state [:resources (:resource-id r) :cost-per-unit] 0))
                             :projected-cost (* (:recommended-capacity r 0)
                                                (get-in @state [:resources (:resource-id r) :cost-per-unit] 0))})
                          recommendations)})))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-capacity-stats
  "Get capacity planning statistics."
  []
  (let [stats (:stats @state)
        resources (vals (:resources @state))
        bottlenecks (detect-bottlenecks)]
    {:total-resources (count resources)
     :total-forecasts (count (:forecasts @state))
     :total-plans (count (:plans @state))
     :total-slas (count (:slas @state))
     :metrics-history-count (count (:metrics-history @state))
     :critical-bottlenecks (count (filter #(= :critical (:severity %)) bottlenecks))
     :forecasts-generated (:forecasts-generated stats)
     :plans-created (:plans-created stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-capacity-planner!
  "Initialize the capacity planner."
  []
  (when-not (:initialized? @state)
    ;; Register resources
    (register-resource! :lm-studio-connections
                        {:name "LM Studio Connections"
                         :type :api
                         :unit :connections
                         :current-capacity 5
                         :max-capacity 20
                         :cost-per-unit 0
                         :scaling-increment 1})
    
    (register-resource! :analysis-workers
                        {:name "Analysis Workers"
                         :type :compute
                         :unit :workers
                         :current-capacity 4
                         :max-capacity 16
                         :cost-per-unit 10
                         :scaling-increment 2})
    
    (register-resource! :document-storage
                        {:name "Document Storage"
                         :type :storage
                         :unit :gb
                         :current-capacity 100
                         :max-capacity 1000
                         :cost-per-unit 0.1
                         :scaling-increment 50})
    
    ;; Create default SLA
    (create-sla! :analysis-sla
                 {:name "Analysis Service SLA"
                  :resource-id :analysis-workers
                  :metrics {:availability 0.999
                            :latency-p99 500}})
    
    ;; Create default capacity plan
    (create-capacity-plan! :default-plan
                           {:name "Default Capacity Plan"
                            :resources [:lm-studio-connections :analysis-workers :document-storage]
                            :target-utilization 0.7
                            :growth-rate 0.1
                            :planning-horizon-days 90})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Capacity planner initialized")
    (events/emit! :capacity-planner-initialized {})
    true))
