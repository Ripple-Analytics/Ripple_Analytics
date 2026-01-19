(ns mental-models.pipeline.integration.cost-tracker
  "Cost tracker for mental model analysis operations.
   
   Features:
   - API cost tracking
   - Token usage monitoring
   - Budget management
   - Cost alerts
   - Usage reports
   - Cost optimization
   - Provider comparison
   - Cost allocation"
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
  (atom {:providers {}        ;; provider-id -> provider-config
         :usage []            ;; usage records
         :budgets {}          ;; budget-id -> budget
         :alerts {}           ;; alert-id -> alert
         :allocations {}      ;; allocation-id -> allocation
         :config {:default-currency :usd
                  :alert-threshold 0.8}
         :stats {:total-cost 0 :total-tokens 0 :total-requests 0}
         :initialized? false}))

;; ============================================================================
;; Provider Management
;; ============================================================================

(defn register-provider!
  "Register a cost provider."
  [provider-id config]
  (let [provider {:id provider-id
                  :name (get config :name (name provider-id))
                  :type (get config :type :llm) ;; :llm, :embedding, :storage, :compute
                  :pricing (get config :pricing {})
                  :currency (get config :currency :usd)
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:providers provider-id] provider)
    (logging/log :info "Registered provider" {:provider-id provider-id})
    provider-id))

(defn get-provider
  "Get a provider."
  [provider-id]
  (get-in @state [:providers provider-id]))

(defn list-providers
  "List all providers."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :type (:type p)})
        (:providers @state)))

(defn update-pricing!
  "Update provider pricing."
  [provider-id pricing]
  (swap! state assoc-in [:providers provider-id :pricing] pricing)
  (swap! state assoc-in [:providers provider-id :updated-at] (System/currentTimeMillis)))

;; ============================================================================
;; Cost Calculation
;; ============================================================================

(defn calculate-cost
  "Calculate cost for usage."
  [provider-id usage-type quantity]
  (when-let [provider (get-provider provider-id)]
    (let [pricing (:pricing provider)
          rate (get pricing usage-type 0)]
      (* rate quantity))))

(defn calculate-token-cost
  "Calculate cost for token usage."
  [provider-id input-tokens output-tokens]
  (when-let [provider (get-provider provider-id)]
    (let [pricing (:pricing provider)
          input-rate (get pricing :input-tokens 0)
          output-rate (get pricing :output-tokens 0)]
      (+ (* input-rate (/ input-tokens 1000))
         (* output-rate (/ output-tokens 1000))))))

;; ============================================================================
;; Usage Tracking
;; ============================================================================

(defn record-usage!
  "Record usage."
  [config]
  (when (flags/enabled? :cost-tracker)
    (let [provider-id (get config :provider-id)
          usage-type (get config :usage-type :tokens)
          quantity (get config :quantity 0)
          cost (or (get config :cost)
                   (calculate-cost provider-id usage-type quantity))
          record {:id (str (UUID/randomUUID))
                  :provider-id provider-id
                  :usage-type usage-type
                  :quantity quantity
                  :cost cost
                  :metadata (get config :metadata {})
                  :allocation-id (get config :allocation-id)
                  :timestamp (System/currentTimeMillis)
                  :date (str (LocalDate/now))}]
      
      (swap! state update :usage conj record)
      (swap! state update-in [:stats :total-cost] + cost)
      (swap! state update-in [:stats :total-requests] inc)
      
      (when (= usage-type :tokens)
        (swap! state update-in [:stats :total-tokens] + quantity))
      
      ;; Check budget alerts
      (check-budget-alerts!)
      
      (logging/log :debug "Recorded usage" {:provider-id provider-id :cost cost})
      record)))

(defn record-llm-usage!
  "Record LLM usage with token details."
  [provider-id input-tokens output-tokens & {:keys [model allocation-id metadata]}]
  (let [cost (calculate-token-cost provider-id input-tokens output-tokens)]
    (record-usage! {:provider-id provider-id
                    :usage-type :tokens
                    :quantity (+ input-tokens output-tokens)
                    :cost cost
                    :allocation-id allocation-id
                    :metadata (merge {:model model
                                      :input-tokens input-tokens
                                      :output-tokens output-tokens}
                                     metadata)})))

(defn get-usage
  "Get usage records."
  [& {:keys [provider-id date-from date-to allocation-id limit] :or {limit 1000}}]
  (let [usage (:usage @state)
        filtered (cond->> usage
                   provider-id (filter #(= (:provider-id %) provider-id))
                   date-from (filter #(>= (:timestamp %) date-from))
                   date-to (filter #(<= (:timestamp %) date-to))
                   allocation-id (filter #(= (:allocation-id %) allocation-id))
                   limit (take-last limit))]
    (vec filtered)))

;; ============================================================================
;; Budget Management
;; ============================================================================

(defn create-budget!
  "Create a budget."
  [budget-id config]
  (let [budget {:id budget-id
                :name (get config :name (name budget-id))
                :amount (get config :amount 0)
                :period (get config :period :monthly) ;; :daily, :weekly, :monthly, :yearly
                :provider-id (get config :provider-id)
                :allocation-id (get config :allocation-id)
                :alert-threshold (get config :alert-threshold 0.8)
                :current-spend 0
                :period-start (System/currentTimeMillis)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:budgets budget-id] budget)
    (logging/log :info "Created budget" {:budget-id budget-id :amount (:amount budget)})
    budget-id))

(defn get-budget
  "Get a budget."
  [budget-id]
  (get-in @state [:budgets budget-id]))

(defn list-budgets
  "List all budgets."
  []
  (mapv (fn [[id b]]
          {:id id
           :name (:name b)
           :amount (:amount b)
           :current-spend (:current-spend b)
           :utilization (if (pos? (:amount b))
                          (/ (:current-spend b) (:amount b))
                          0)})
        (:budgets @state)))

(defn update-budget-spend!
  "Update budget spend."
  [budget-id amount]
  (swap! state update-in [:budgets budget-id :current-spend] + amount))

(defn reset-budget!
  "Reset budget for new period."
  [budget-id]
  (swap! state update-in [:budgets budget-id]
         (fn [b]
           (assoc b
                  :current-spend 0
                  :period-start (System/currentTimeMillis)))))

(defn- check-budget-alerts!
  "Check and trigger budget alerts."
  []
  (doseq [[budget-id budget] (:budgets @state)]
    (let [utilization (if (pos? (:amount budget))
                        (/ (:current-spend budget) (:amount budget))
                        0)
          threshold (:alert-threshold budget)]
      (when (>= utilization threshold)
        (let [alert-id (str budget-id "-" (System/currentTimeMillis))]
          (swap! state assoc-in [:alerts alert-id]
                 {:id alert-id
                  :type :budget-threshold
                  :budget-id budget-id
                  :utilization utilization
                  :threshold threshold
                  :created-at (System/currentTimeMillis)})
          (logging/log :warn "Budget threshold exceeded" {:budget-id budget-id :utilization utilization})
          (events/emit! :budget-alert {:budget-id budget-id :utilization utilization}))))))

;; ============================================================================
;; Cost Allocation
;; ============================================================================

(defn create-allocation!
  "Create a cost allocation."
  [allocation-id config]
  (let [allocation {:id allocation-id
                    :name (get config :name (name allocation-id))
                    :type (get config :type :project) ;; :project, :team, :user, :feature
                    :parent-id (get config :parent-id)
                    :budget-id (get config :budget-id)
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:allocations allocation-id] allocation)
    (logging/log :info "Created allocation" {:allocation-id allocation-id})
    allocation-id))

(defn get-allocation
  "Get an allocation."
  [allocation-id]
  (get-in @state [:allocations allocation-id]))

(defn list-allocations
  "List all allocations."
  []
  (mapv (fn [[id a]]
          {:id id
           :name (:name a)
           :type (:type a)})
        (:allocations @state)))

(defn get-allocation-cost
  "Get total cost for an allocation."
  [allocation-id & {:keys [date-from date-to]}]
  (let [usage (get-usage :allocation-id allocation-id
                         :date-from date-from
                         :date-to date-to)]
    {:allocation-id allocation-id
     :total-cost (reduce + (map :cost usage))
     :total-requests (count usage)
     :by-provider (reduce (fn [m u]
                            (update m (:provider-id u) (fnil + 0) (:cost u)))
                          {}
                          usage)}))

;; ============================================================================
;; Cost Reports
;; ============================================================================

(defn generate-daily-report
  "Generate daily cost report."
  [date]
  (let [date-str (str date)
        usage (filter #(= (:date %) date-str) (:usage @state))]
    {:date date-str
     :total-cost (reduce + (map :cost usage))
     :total-requests (count usage)
     :by-provider (reduce (fn [m u]
                            (update m (:provider-id u) (fnil + 0) (:cost u)))
                          {}
                          usage)
     :by-allocation (reduce (fn [m u]
                              (when (:allocation-id u)
                                (update m (:allocation-id u) (fnil + 0) (:cost u))))
                            {}
                            usage)}))

(defn generate-summary-report
  "Generate summary cost report."
  [& {:keys [date-from date-to]}]
  (let [usage (get-usage :date-from date-from :date-to date-to)
        total-cost (reduce + (map :cost usage))
        by-provider (reduce (fn [m u]
                              (update m (:provider-id u)
                                      (fn [p]
                                        {:cost (+ (get p :cost 0) (:cost u))
                                         :requests (inc (get p :requests 0))})))
                            {}
                            usage)
        by-date (reduce (fn [m u]
                          (update m (:date u) (fnil + 0) (:cost u)))
                        {}
                        usage)]
    {:period {:from date-from :to date-to}
     :total-cost total-cost
     :total-requests (count usage)
     :average-cost-per-request (if (pos? (count usage))
                                 (/ total-cost (count usage))
                                 0)
     :by-provider by-provider
     :by-date (sort-by first by-date)}))

;; ============================================================================
;; Cost Optimization
;; ============================================================================

(defn analyze-cost-efficiency
  "Analyze cost efficiency."
  []
  (let [usage (:usage @state)
        by-provider (group-by :provider-id usage)]
    (mapv (fn [[provider-id records]]
            (let [total-cost (reduce + (map :cost records))
                  total-tokens (reduce + (map #(get-in % [:metadata :input-tokens] 0) records))
                  avg-cost-per-1k (if (pos? total-tokens)
                                    (/ (* total-cost 1000) total-tokens)
                                    0)]
              {:provider-id provider-id
               :total-cost total-cost
               :total-tokens total-tokens
               :avg-cost-per-1k-tokens avg-cost-per-1k
               :request-count (count records)}))
          by-provider)))

(defn get-optimization-suggestions
  "Get cost optimization suggestions."
  []
  (let [efficiency (analyze-cost-efficiency)
        sorted-by-cost (sort-by :avg-cost-per-1k-tokens efficiency)]
    {:suggestions
     (cond-> []
       (> (count sorted-by-cost) 1)
       (conj {:type :provider-switch
              :message (str "Consider switching from "
                            (:provider-id (last sorted-by-cost))
                            " to "
                            (:provider-id (first sorted-by-cost))
                            " for lower costs")
              :potential-savings (* 0.2 (:total-cost (last sorted-by-cost)))})
       
       true
       (conj {:type :caching
              :message "Enable response caching to reduce duplicate API calls"
              :potential-savings (* 0.1 (get-in @state [:stats :total-cost]))})
       
       true
       (conj {:type :batching
              :message "Batch similar requests to reduce overhead"
              :potential-savings (* 0.05 (get-in @state [:stats :total-cost]))}))}))

;; ============================================================================
;; Alerts
;; ============================================================================

(defn get-alerts
  "Get alerts."
  [& {:keys [type acknowledged?]}]
  (let [alerts (vals (:alerts @state))
        filtered (cond->> alerts
                   type (filter #(= (:type %) type))
                   (some? acknowledged?) (filter #(= (:acknowledged? %) acknowledged?)))]
    (vec filtered)))

(defn acknowledge-alert!
  "Acknowledge an alert."
  [alert-id]
  (swap! state assoc-in [:alerts alert-id :acknowledged?] true)
  (swap! state assoc-in [:alerts alert-id :acknowledged-at] (System/currentTimeMillis)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-cost-stats
  "Get cost statistics."
  []
  (let [stats (:stats @state)]
    {:total-providers (count (:providers @state))
     :total-budgets (count (:budgets @state))
     :total-allocations (count (:allocations @state))
     :total-usage-records (count (:usage @state))
     :total-cost (:total-cost stats)
     :total-tokens (:total-tokens stats)
     :total-requests (:total-requests stats)
     :avg-cost-per-request (if (pos? (:total-requests stats))
                             (/ (:total-cost stats) (:total-requests stats))
                             0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-cost-tracker!
  "Initialize the cost tracker."
  []
  (when-not (:initialized? @state)
    ;; Register LM Studio (local, free)
    (register-provider! :lm-studio
                        {:name "LM Studio"
                         :type :llm
                         :pricing {:input-tokens 0
                                   :output-tokens 0}
                         :currency :usd})
    
    ;; Register OpenAI (for comparison)
    (register-provider! :openai
                        {:name "OpenAI"
                         :type :llm
                         :pricing {:input-tokens 0.0015  ;; per 1K tokens
                                   :output-tokens 0.002}
                         :currency :usd})
    
    ;; Register Anthropic (for comparison)
    (register-provider! :anthropic
                        {:name "Anthropic"
                         :type :llm
                         :pricing {:input-tokens 0.008
                                   :output-tokens 0.024}
                         :currency :usd})
    
    ;; Create default budget
    (create-budget! :monthly-budget
                    {:name "Monthly API Budget"
                     :amount 100.0
                     :period :monthly
                     :alert-threshold 0.8})
    
    ;; Create default allocation
    (create-allocation! :mental-model-analysis
                        {:name "Mental Model Analysis"
                         :type :feature
                         :budget-id :monthly-budget})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Cost tracker initialized")
    (events/emit! :cost-tracker-initialized {})
    true))
