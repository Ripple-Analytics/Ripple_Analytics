(ns mental-models.pipeline.integration.request-simulator
  "Request simulator for mental model analysis system.
   
   Features:
   - Request simulation
   - Load testing
   - Scenario simulation
   - Traffic patterns
   - Concurrent simulation
   - Simulation profiles
   - Result analysis
   - Simulation metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close! alts!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant LocalDate]
           [java.util.concurrent Executors ExecutorService CountDownLatch TimeUnit]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:profiles {}         ;; profile-id -> profile
         :scenarios {}        ;; scenario-id -> scenario
         :simulations []      ;; simulation results
         :config {:max-concurrent 100
                  :default-timeout-ms 30000
                  :warmup-requests 10
                  :cooldown-ms 1000}
         :stats {:simulations-run 0
                 :requests-simulated 0
                 :total-duration-ms 0
                 :errors 0}
         :executor nil
         :initialized? false}))

;; ============================================================================
;; Request Generation
;; ============================================================================

(defn generate-request
  "Generate a simulated request."
  [template & {:keys [params]}]
  (let [request {:request-method (or (:method template) :get)
                 :uri (or (:uri template) "/")
                 :headers (merge {"Content-Type" "application/json"
                                  "User-Agent" "RequestSimulator/1.0"}
                                 (:headers template))
                 :body (:body template)
                 :query-params (:query-params template)
                 :simulated? true
                 :simulation-id (str (UUID/randomUUID))
                 :timestamp (System/currentTimeMillis)}]
    (if params
      (reduce (fn [r [k v]]
                (cond
                  (= k :uri) (assoc r :uri v)
                  (= k :body) (assoc r :body v)
                  (= k :headers) (update r :headers merge v)
                  :else (assoc-in r [:params k] v)))
              request
              params)
      request)))

(defn generate-random-request
  "Generate a random request from templates."
  [templates]
  (let [template (rand-nth templates)]
    (generate-request template)))

(defn generate-request-batch
  "Generate a batch of requests."
  [template count]
  (repeatedly count #(generate-request template)))

;; ============================================================================
;; Traffic Patterns
;; ============================================================================

(def traffic-patterns
  {:constant (fn [rate-per-sec duration-sec]
               (let [total (* rate-per-sec duration-sec)
                     interval-ms (/ 1000.0 rate-per-sec)]
                 (repeat total interval-ms)))
   
   :ramp-up (fn [start-rate end-rate duration-sec]
              (let [steps (* duration-sec 10)
                    rate-increment (/ (- end-rate start-rate) steps)]
                (for [i (range steps)
                      :let [rate (+ start-rate (* i rate-increment))
                            interval-ms (/ 1000.0 (max 1 rate))]]
                  interval-ms)))
   
   :spike (fn [base-rate spike-rate spike-duration-sec total-duration-sec]
            (let [spike-start (/ total-duration-sec 2)
                  spike-end (+ spike-start spike-duration-sec)]
              (for [sec (range total-duration-sec)
                    :let [rate (if (and (>= sec spike-start) (< sec spike-end))
                                 spike-rate
                                 base-rate)
                          interval-ms (/ 1000.0 rate)]]
                interval-ms)))
   
   :wave (fn [min-rate max-rate period-sec duration-sec]
           (for [sec (range duration-sec)
                 :let [phase (/ (* 2 Math/PI sec) period-sec)
                       rate (+ min-rate (* (/ (- max-rate min-rate) 2)
                                           (+ 1 (Math/sin phase))))
                       interval-ms (/ 1000.0 rate)]]
             interval-ms))
   
   :random (fn [min-rate max-rate duration-sec]
             (let [random (Random.)]
               (for [_ (range duration-sec)
                     :let [rate (+ min-rate (* (.nextDouble random) (- max-rate min-rate)))
                           interval-ms (/ 1000.0 rate)]]
                 interval-ms)))})

;; ============================================================================
;; Simulation Profiles
;; ============================================================================

(defn create-profile!
  "Create a simulation profile."
  [profile-id config]
  (let [profile {:id profile-id
                 :name (get config :name (name profile-id))
                 :templates (get config :templates [])
                 :pattern (get config :pattern :constant)
                 :pattern-args (get config :pattern-args [10 60])
                 :concurrent (get config :concurrent 10)
                 :timeout-ms (get config :timeout-ms 30000)
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:profiles profile-id] profile)
    (logging/log :info "Created simulation profile" {:profile-id profile-id})
    profile-id))

(defn get-profile
  "Get a simulation profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn list-profiles
  "List all simulation profiles."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :pattern (:pattern p)
           :concurrent (:concurrent p)
           :enabled? @(:enabled? p)})
        (:profiles @state)))

(defn delete-profile!
  "Delete a simulation profile."
  [profile-id]
  (swap! state update :profiles dissoc profile-id))

;; ============================================================================
;; Scenarios
;; ============================================================================

(defn create-scenario!
  "Create a simulation scenario."
  [scenario-id config]
  (let [scenario {:id scenario-id
                  :name (get config :name (name scenario-id))
                  :steps (get config :steps [])
                  :setup-fn (get config :setup-fn)
                  :teardown-fn (get config :teardown-fn)
                  :enabled? (atom true)
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:scenarios scenario-id] scenario)
    scenario-id))

(defn get-scenario
  "Get a scenario."
  [scenario-id]
  (get-in @state [:scenarios scenario-id]))

(defn run-scenario
  "Run a simulation scenario."
  [scenario-id handler]
  (when-let [scenario (get-scenario scenario-id)]
    (when @(:enabled? scenario)
      ;; Setup
      (when-let [setup (:setup-fn scenario)]
        (setup))
      
      ;; Run steps
      (let [results (for [step (:steps scenario)]
                      (let [request (generate-request step)
                            start-time (System/currentTimeMillis)
                            response (try
                                       (handler request)
                                       (catch Exception e
                                         {:error (.getMessage e)}))
                            duration-ms (- (System/currentTimeMillis) start-time)]
                        {:step step
                         :request request
                         :response response
                         :duration-ms duration-ms
                         :success? (and (not (:error response))
                                        (< (:status response 500) 400))}))]
        
        ;; Teardown
        (when-let [teardown (:teardown-fn scenario)]
          (teardown))
        
        {:scenario-id scenario-id
         :results (vec results)
         :success? (every? :success? results)
         :total-duration-ms (reduce + (map :duration-ms results))}))))

;; ============================================================================
;; Load Testing
;; ============================================================================

(defn run-load-test
  "Run a load test."
  [handler & {:keys [templates concurrent duration-sec pattern pattern-args]
              :or {concurrent 10
                   duration-sec 60
                   pattern :constant
                   pattern-args [10 60]}}]
  (swap! state update-in [:stats :simulations-run] inc)
  
  (let [executor (or (:executor @state)
                     (Executors/newFixedThreadPool concurrent))
        pattern-fn (get traffic-patterns pattern)
        intervals (apply pattern-fn pattern-args)
        results (atom [])
        errors (atom 0)
        start-time (System/currentTimeMillis)]
    
    ;; Warmup
    (dotimes [_ (get-in @state [:config :warmup-requests])]
      (let [request (generate-random-request templates)]
        (try
          (handler request)
          (catch Exception _))))
    
    ;; Main test
    (doseq [interval-ms intervals]
      (.submit executor
               (fn []
                 (let [request (generate-random-request templates)
                       req-start (System/currentTimeMillis)]
                   (try
                     (let [response (handler request)
                           duration-ms (- (System/currentTimeMillis) req-start)]
                       (swap! results conj
                              {:request-id (:simulation-id request)
                               :status (:status response)
                               :duration-ms duration-ms
                               :success? (< (:status response 500) 400)})
                       (swap! state update-in [:stats :requests-simulated] inc))
                     (catch Exception e
                       (swap! errors inc)
                       (swap! state update-in [:stats :errors] inc)
                       (swap! results conj
                              {:request-id (:simulation-id request)
                               :error (.getMessage e)
                               :duration-ms (- (System/currentTimeMillis) req-start)
                               :success? false}))))))
      (Thread/sleep (long interval-ms)))
    
    ;; Wait for completion
    (.shutdown executor)
    (.awaitTermination executor duration-sec TimeUnit/SECONDS)
    
    ;; Cooldown
    (Thread/sleep (get-in @state [:config :cooldown-ms]))
    
    (let [total-duration-ms (- (System/currentTimeMillis) start-time)
          all-results @results]
      (swap! state update-in [:stats :total-duration-ms] + total-duration-ms)
      
      {:total-requests (count all-results)
       :successful-requests (count (filter :success? all-results))
       :failed-requests (count (filter (complement :success?) all-results))
       :total-duration-ms total-duration-ms
       :avg-duration-ms (when (seq all-results)
                          (/ (reduce + (map :duration-ms all-results))
                             (count all-results)))
       :min-duration-ms (when (seq all-results)
                          (apply min (map :duration-ms all-results)))
       :max-duration-ms (when (seq all-results)
                          (apply max (map :duration-ms all-results)))
       :requests-per-second (/ (count all-results) (/ total-duration-ms 1000.0))
       :error-rate (/ @errors (max 1 (count all-results)))})))

(defn run-profile-test
  "Run a load test using a profile."
  [profile-id handler]
  (when-let [profile (get-profile profile-id)]
    (when @(:enabled? profile)
      (run-load-test handler
                     :templates (:templates profile)
                     :concurrent (:concurrent profile)
                     :pattern (:pattern profile)
                     :pattern-args (:pattern-args profile)))))

;; ============================================================================
;; Result Analysis
;; ============================================================================

(defn analyze-results
  "Analyze simulation results."
  [results]
  (let [durations (map :duration-ms results)
        sorted-durations (sort durations)
        n (count sorted-durations)]
    {:count n
     :success-rate (/ (count (filter :success? results)) (max 1 n))
     :avg-duration-ms (/ (reduce + durations) (max 1 n))
     :min-duration-ms (first sorted-durations)
     :max-duration-ms (last sorted-durations)
     :p50-duration-ms (nth sorted-durations (int (* 0.5 n)) 0)
     :p90-duration-ms (nth sorted-durations (int (* 0.9 n)) 0)
     :p95-duration-ms (nth sorted-durations (int (* 0.95 n)) 0)
     :p99-duration-ms (nth sorted-durations (int (* 0.99 n)) 0)
     :by-status (frequencies (map :status results))}))

;; ============================================================================
;; Concurrent Simulation
;; ============================================================================

(defn simulate-concurrent
  "Simulate concurrent requests."
  [handler requests & {:keys [max-concurrent timeout-ms]
                       :or {max-concurrent 100
                            timeout-ms 30000}}]
  (let [executor (Executors/newFixedThreadPool max-concurrent)
        latch (CountDownLatch. (count requests))
        results (atom [])]
    
    (doseq [request requests]
      (.submit executor
               (fn []
                 (try
                   (let [start-time (System/currentTimeMillis)
                         response (handler request)
                         duration-ms (- (System/currentTimeMillis) start-time)]
                     (swap! results conj
                            {:request request
                             :response response
                             :duration-ms duration-ms
                             :success? (< (:status response 500) 400)}))
                   (catch Exception e
                     (swap! results conj
                            {:request request
                             :error (.getMessage e)
                             :success? false}))
                   (finally
                     (.countDown latch))))))
    
    (.await latch timeout-ms TimeUnit/MILLISECONDS)
    (.shutdown executor)
    
    @results))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-concurrent!
  "Set maximum concurrent simulations."
  [max-concurrent]
  (swap! state assoc-in [:config :max-concurrent] max-concurrent))

(defn set-default-timeout!
  "Set default timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :default-timeout-ms] timeout-ms))

(defn set-warmup-requests!
  "Set warmup request count."
  [count]
  (swap! state assoc-in [:config :warmup-requests] count))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-simulator-metrics
  "Get simulator metrics."
  []
  (let [stats (:stats @state)]
    {:simulations-run (:simulations-run stats)
     :requests-simulated (:requests-simulated stats)
     :total-duration-ms (:total-duration-ms stats)
     :errors (:errors stats)
     :profiles-count (count (:profiles @state))
     :scenarios-count (count (:scenarios @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-simulator-stats
  "Get simulator statistics."
  []
  (merge (get-simulator-metrics)
         {:max-concurrent (get-in @state [:config :max-concurrent])
          :default-timeout-ms (get-in @state [:config :default-timeout-ms])
          :warmup-requests (get-in @state [:config :warmup-requests])}))

(defn reset-stats!
  "Reset simulator statistics."
  []
  (swap! state assoc :stats {:simulations-run 0
                             :requests-simulated 0
                             :total-duration-ms 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-simulator!
  "Initialize the request simulator."
  []
  (when-not (:initialized? @state)
    (let [executor (Executors/newFixedThreadPool
                    (get-in @state [:config :max-concurrent]))]
      (swap! state assoc :executor executor))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request simulator initialized")
    (events/emit! :request-simulator-initialized {})
    true))

(defn shutdown!
  "Shutdown the request simulator."
  []
  (when-let [executor (:executor @state)]
    (.shutdown executor))
  (swap! state assoc :initialized? false))
