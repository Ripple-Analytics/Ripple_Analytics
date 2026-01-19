(ns chaos.core
  "Chaos Engineering Service
   
   Proactive resilience testing for Mental Models microservices.
   Implements fault injection, circuit breaker testing, load testing,
   and recovery validation following chaos engineering principles."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.tools.logging :as log])
  (:import [java.time Instant Duration]
           [java.util UUID])
  (:gen-class))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Chaos Engineering Service"
  (Integer/parseInt (or (System/getenv "PORT") "8005")))

(def target-services
  "Target microservices for chaos experiments"
  {:api-gateway (or (System/getenv "API_GATEWAY_URL") "http://api-gateway:8000")
   :analysis (or (System/getenv "ANALYSIS_SERVICE_URL") "http://analysis-service:8001")
   :harvester (or (System/getenv "HARVESTER_SERVICE_URL") "http://harvester-service:8002")
   :storage (or (System/getenv "STORAGE_SERVICE_URL") "http://storage-service:8003")})

;; ============================================
;; State Management
;; ============================================

(def experiments
  "Active and completed chaos experiments"
  (atom {}))

(def fault-injections
  "Currently active fault injections"
  (atom {}))

(def metrics
  "Collected metrics from experiments"
  (atom {:total-experiments 0
         :successful-recoveries 0
         :failed-recoveries 0
         :circuit-breaker-trips 0
         :latency-samples []}))

;; ============================================
;; Utility Functions
;; ============================================

(defn generate-id
  "Generate a unique experiment ID"
  []
  (str (UUID/randomUUID)))

(defn now-iso
  "Get current timestamp in ISO format"
  []
  (str (Instant/now)))

(defn duration-ms
  "Calculate duration in milliseconds between two instants"
  [start end]
  (.toMillis (Duration/between (Instant/parse start) (Instant/parse end))))

;; ============================================
;; Health Check Functions
;; ============================================

(defn check-service-health
  "Check health of a single service"
  [service-name url]
  (try
    (let [start (Instant/now)
          response (http/get (str url "/health")
                             {:throw-exceptions false
                              :socket-timeout 5000
                              :connection-timeout 3000})
          end (Instant/now)
          latency (.toMillis (Duration/between start end))]
      {:service service-name
       :status (if (= 200 (:status response)) "healthy" "unhealthy")
       :latency-ms latency
       :http-status (:status response)
       :timestamp (str end)})
    (catch Exception e
      {:service service-name
       :status "unreachable"
       :error (.getMessage e)
       :timestamp (now-iso)})))

(defn check-all-services
  "Check health of all target services"
  []
  (into {} (map (fn [[k v]] [k (check-service-health (name k) v)]) target-services)))

;; ============================================
;; Fault Injection Functions
;; ============================================

(defn inject-latency
  "Inject artificial latency into service calls"
  [service-name latency-ms duration-seconds]
  (let [injection-id (generate-id)
        end-time (+ (System/currentTimeMillis) (* duration-seconds 1000))]
    (swap! fault-injections assoc injection-id
           {:id injection-id
            :type :latency
            :service service-name
            :latency-ms latency-ms
            :started-at (now-iso)
            :ends-at (str (Instant/ofEpochMilli end-time))
            :active true})
    ;; Schedule removal
    (go
      (<! (timeout (* duration-seconds 1000)))
      (swap! fault-injections update injection-id assoc :active false))
    {:injection-id injection-id
     :type :latency
     :service service-name
     :latency-ms latency-ms
     :duration-seconds duration-seconds}))

(defn inject-error
  "Inject error responses from a service"
  [service-name error-rate duration-seconds]
  (let [injection-id (generate-id)
        end-time (+ (System/currentTimeMillis) (* duration-seconds 1000))]
    (swap! fault-injections assoc injection-id
           {:id injection-id
            :type :error
            :service service-name
            :error-rate error-rate
            :started-at (now-iso)
            :ends-at (str (Instant/ofEpochMilli end-time))
            :active true})
    (go
      (<! (timeout (* duration-seconds 1000)))
      (swap! fault-injections update injection-id assoc :active false))
    {:injection-id injection-id
     :type :error
     :service service-name
     :error-rate error-rate
     :duration-seconds duration-seconds}))

(defn inject-service-failure
  "Simulate complete service failure"
  [service-name duration-seconds]
  (let [injection-id (generate-id)
        end-time (+ (System/currentTimeMillis) (* duration-seconds 1000))]
    (swap! fault-injections assoc injection-id
           {:id injection-id
            :type :service-failure
            :service service-name
            :started-at (now-iso)
            :ends-at (str (Instant/ofEpochMilli end-time))
            :active true})
    (go
      (<! (timeout (* duration-seconds 1000)))
      (swap! fault-injections update injection-id assoc :active false))
    {:injection-id injection-id
     :type :service-failure
     :service service-name
     :duration-seconds duration-seconds}))

(defn clear-fault-injection
  "Clear a specific fault injection"
  [injection-id]
  (if (contains? @fault-injections injection-id)
    (do
      (swap! fault-injections update injection-id assoc :active false)
      {:success true :message (str "Cleared injection " injection-id)})
    {:success false :error "Injection not found"}))

(defn clear-all-faults
  "Clear all active fault injections"
  []
  (let [count (count (filter :active (vals @fault-injections)))]
    (swap! fault-injections (fn [m] (into {} (map (fn [[k v]] [k (assoc v :active false)]) m))))
    {:success true :cleared count}))

;; ============================================
;; Circuit Breaker Testing
;; ============================================

(defn test-circuit-breaker
  "Test circuit breaker behavior by sending rapid requests"
  [service-name num-requests]
  (let [experiment-id (generate-id)
        url (get target-services (keyword service-name))
        results (atom {:successful 0 :failed 0 :circuit-open 0 :latencies []})]
    (swap! experiments assoc experiment-id
           {:id experiment-id
            :type :circuit-breaker-test
            :service service-name
            :num-requests num-requests
            :started-at (now-iso)
            :status "running"})
    ;; Run requests
    (doseq [_ (range num-requests)]
      (try
        (let [start (System/currentTimeMillis)
              response (http/get (str url "/health")
                                 {:throw-exceptions false
                                  :socket-timeout 5000
                                  :connection-timeout 2000})
              latency (- (System/currentTimeMillis) start)]
          (swap! results update :latencies conj latency)
          (cond
            (= 200 (:status response)) (swap! results update :successful inc)
            (= 503 (:status response)) (do
                                         (swap! results update :circuit-open inc)
                                         (swap! metrics update :circuit-breaker-trips inc))
            :else (swap! results update :failed inc)))
        (catch Exception _
          (swap! results update :failed inc))))
    ;; Update experiment
    (let [final-results @results
          avg-latency (if (seq (:latencies final-results))
                        (/ (reduce + (:latencies final-results)) (count (:latencies final-results)))
                        0)]
      (swap! experiments update experiment-id merge
             {:status "completed"
              :completed-at (now-iso)
              :results (assoc final-results
                              :avg-latency-ms avg-latency
                              :latencies nil)})
      (swap! metrics update :total-experiments inc)
      {:experiment-id experiment-id
       :results (assoc final-results :avg-latency-ms avg-latency :latencies nil)})))

;; ============================================
;; Load Testing
;; ============================================

(defn run-load-test
  "Run a load test against a service"
  [service-name requests-per-second duration-seconds]
  (let [experiment-id (generate-id)
        url (get target-services (keyword service-name))
        total-requests (* requests-per-second duration-seconds)
        interval-ms (/ 1000 requests-per-second)
        results (atom {:successful 0 :failed 0 :latencies []})
        stop-signal (atom false)]
    (swap! experiments assoc experiment-id
           {:id experiment-id
            :type :load-test
            :service service-name
            :rps requests-per-second
            :duration-seconds duration-seconds
            :started-at (now-iso)
            :status "running"})
    ;; Run load test in background
    (go
      (dotimes [_ total-requests]
        (when-not @stop-signal
          (go
            (try
              (let [start (System/currentTimeMillis)
                    response (http/get (str url "/health")
                                       {:throw-exceptions false
                                        :socket-timeout 10000
                                        :connection-timeout 5000})
                    latency (- (System/currentTimeMillis) start)]
                (swap! results update :latencies conj latency)
                (if (= 200 (:status response))
                  (swap! results update :successful inc)
                  (swap! results update :failed inc)))
              (catch Exception _
                (swap! results update :failed inc))))
          (<! (timeout (int interval-ms)))))
      ;; Wait for completion
      (<! (timeout (* duration-seconds 1000)))
      (reset! stop-signal true)
      ;; Calculate final results
      (let [final-results @results
            latencies (:latencies final-results)
            sorted-latencies (sort latencies)
            p50 (when (seq sorted-latencies) (nth sorted-latencies (int (* 0.5 (count sorted-latencies)))))
            p95 (when (seq sorted-latencies) (nth sorted-latencies (int (* 0.95 (count sorted-latencies)))))
            p99 (when (seq sorted-latencies) (nth sorted-latencies (int (* 0.99 (count sorted-latencies)))))
            avg (when (seq latencies) (/ (reduce + latencies) (count latencies)))]
        (swap! experiments update experiment-id merge
               {:status "completed"
                :completed-at (now-iso)
                :results {:successful (:successful final-results)
                          :failed (:failed final-results)
                          :total-requests (+ (:successful final-results) (:failed final-results))
                          :avg-latency-ms avg
                          :p50-latency-ms p50
                          :p95-latency-ms p95
                          :p99-latency-ms p99}})
        (swap! metrics update :total-experiments inc)))
    {:experiment-id experiment-id
     :message "Load test started"
     :total-requests total-requests}))

;; ============================================
;; Recovery Validation
;; ============================================

(defn validate-recovery
  "Validate service recovery after fault injection"
  [service-name max-wait-seconds]
  (let [experiment-id (generate-id)
        url (get target-services (keyword service-name))
        start-time (now-iso)
        check-interval-ms 1000
        max-checks (/ (* max-wait-seconds 1000) check-interval-ms)]
    (swap! experiments assoc experiment-id
           {:id experiment-id
            :type :recovery-validation
            :service service-name
            :started-at start-time
            :status "running"})
    (go-loop [checks 0]
      (if (>= checks max-checks)
        (do
          (swap! experiments update experiment-id merge
                 {:status "failed"
                  :completed-at (now-iso)
                  :results {:recovered false
                            :checks-performed checks
                            :message "Service did not recover within timeout"}})
          (swap! metrics update :failed-recoveries inc))
        (let [health (check-service-health service-name url)]
          (if (= "healthy" (:status health))
            (do
              (swap! experiments update experiment-id merge
                     {:status "completed"
                      :completed-at (now-iso)
                      :results {:recovered true
                                :recovery-time-ms (* checks check-interval-ms)
                                :checks-performed checks}})
              (swap! metrics update :successful-recoveries inc))
            (do
              (<! (timeout check-interval-ms))
              (recur (inc checks)))))))
    {:experiment-id experiment-id
     :message "Recovery validation started"
     :max-wait-seconds max-wait-seconds}))

;; ============================================
;; Chaos Scenarios (Pre-built Experiments)
;; ============================================

(defn run-scenario-cascade-failure
  "Simulate cascade failure across services"
  []
  (let [scenario-id (generate-id)]
    (swap! experiments assoc scenario-id
           {:id scenario-id
            :type :scenario
            :name "cascade-failure"
            :started-at (now-iso)
            :status "running"
            :steps []})
    (go
      ;; Step 1: Inject latency into storage service
      (let [step1 (inject-latency "storage" 2000 30)]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 1 :action "inject-latency" :target "storage" :result step1}))
      (<! (timeout 5000))
      ;; Step 2: Test if analysis service handles slow storage
      (let [step2 (test-circuit-breaker "analysis" 20)]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 2 :action "test-circuit-breaker" :target "analysis" :result step2}))
      (<! (timeout 10000))
      ;; Step 3: Validate gateway still responds
      (let [health (check-service-health "api-gateway" (:api-gateway target-services))]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 3 :action "health-check" :target "api-gateway" :result health}))
      ;; Complete scenario
      (swap! experiments update scenario-id merge
             {:status "completed"
              :completed-at (now-iso)}))
    {:scenario-id scenario-id
     :name "cascade-failure"
     :message "Cascade failure scenario started"}))

(defn run-scenario-recovery-test
  "Test full system recovery after failures"
  []
  (let [scenario-id (generate-id)]
    (swap! experiments assoc scenario-id
           {:id scenario-id
            :type :scenario
            :name "recovery-test"
            :started-at (now-iso)
            :status "running"
            :steps []})
    (go
      ;; Step 1: Record baseline health
      (let [baseline (check-all-services)]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 1 :action "baseline-health" :result baseline}))
      ;; Step 2: Inject failures
      (let [injection (inject-service-failure "harvester" 10)]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 2 :action "inject-failure" :target "harvester" :result injection}))
      (<! (timeout 5000))
      ;; Step 3: Verify gateway handles missing service
      (let [gateway-health (check-service-health "api-gateway" (:api-gateway target-services))]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 3 :action "gateway-resilience" :result gateway-health}))
      ;; Step 4: Wait for recovery
      (<! (timeout 10000))
      ;; Step 5: Validate full recovery
      (let [final-health (check-all-services)]
        (swap! experiments update-in [scenario-id :steps] conj
               {:step 5 :action "final-health" :result final-health}))
      ;; Complete scenario
      (swap! experiments update scenario-id merge
             {:status "completed"
              :completed-at (now-iso)}))
    {:scenario-id scenario-id
     :name "recovery-test"
     :message "Recovery test scenario started"}))

;; ============================================
;; Route Handlers
;; ============================================

(defn root-handler
  "Root endpoint"
  [_]
  (response/response
   {:service "Mental Models Chaos Engineering Service"
    :version "1.0.0"
    :capabilities ["fault-injection" "circuit-breaker-testing" "load-testing" "recovery-validation" "chaos-scenarios"]
    :active-injections (count (filter :active (vals @fault-injections)))
    :total-experiments (:total-experiments @metrics)}))

(defn health-handler
  "Health check endpoint"
  [_]
  (response/response {:status "healthy" :service "chaos-engineering"}))

(defn services-health-handler
  "Check health of all target services"
  [_]
  (response/response (check-all-services)))

(defn metrics-handler
  "Get chaos engineering metrics"
  [_]
  (response/response @metrics))

;; Fault Injection Handlers
(defn inject-latency-handler
  "Inject latency into a service"
  [request]
  (let [body (:body request)
        service (or (:service body) "api-gateway")
        latency-ms (or (:latency_ms body) (:latency-ms body) 1000)
        duration (or (:duration_seconds body) (:duration-seconds body) 30)]
    (response/response (inject-latency service latency-ms duration))))

(defn inject-error-handler
  "Inject errors into a service"
  [request]
  (let [body (:body request)
        service (or (:service body) "api-gateway")
        error-rate (or (:error_rate body) (:error-rate body) 0.5)
        duration (or (:duration_seconds body) (:duration-seconds body) 30)]
    (response/response (inject-error service error-rate duration))))

(defn inject-failure-handler
  "Inject complete service failure"
  [request]
  (let [body (:body request)
        service (or (:service body) "harvester")
        duration (or (:duration_seconds body) (:duration-seconds body) 30)]
    (response/response (inject-service-failure service duration))))

(defn clear-injection-handler
  "Clear a specific fault injection"
  [request]
  (let [injection-id (get-in request [:params :id])]
    (response/response (clear-fault-injection injection-id))))

(defn clear-all-handler
  "Clear all fault injections"
  [_]
  (response/response (clear-all-faults)))

(defn list-injections-handler
  "List all fault injections"
  [_]
  (response/response
   {:injections (vals @fault-injections)
    :active (count (filter :active (vals @fault-injections)))}))

;; Testing Handlers
(defn circuit-breaker-test-handler
  "Test circuit breaker behavior"
  [request]
  (let [body (:body request)
        service (or (:service body) "api-gateway")
        num-requests (or (:num_requests body) (:num-requests body) 50)]
    (response/response (test-circuit-breaker service num-requests))))

(defn load-test-handler
  "Run load test"
  [request]
  (let [body (:body request)
        service (or (:service body) "api-gateway")
        rps (or (:requests_per_second body) (:requests-per-second body) 10)
        duration (or (:duration_seconds body) (:duration-seconds body) 30)]
    (response/response (run-load-test service rps duration))))

(defn recovery-test-handler
  "Validate service recovery"
  [request]
  (let [body (:body request)
        service (or (:service body) "api-gateway")
        max-wait (or (:max_wait_seconds body) (:max-wait-seconds body) 60)]
    (response/response (validate-recovery service max-wait))))

;; Scenario Handlers
(defn cascade-failure-handler
  "Run cascade failure scenario"
  [_]
  (response/response (run-scenario-cascade-failure)))

(defn recovery-scenario-handler
  "Run recovery test scenario"
  [_]
  (response/response (run-scenario-recovery-test)))

;; Experiment Handlers
(defn list-experiments-handler
  "List all experiments"
  [_]
  (response/response
   {:experiments (vals @experiments)
    :total (count @experiments)}))

(defn get-experiment-handler
  "Get a specific experiment"
  [request]
  (let [experiment-id (get-in request [:params :id])]
    (if-let [experiment (get @experiments experiment-id)]
      (response/response experiment)
      (-> (response/response {:error "Experiment not found"})
          (response/status 404)))))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= uri "/") (root-handler request)
      (= uri "/health") (health-handler request)
      (= uri "/metrics") (metrics-handler request)
      (= uri "/services/health") (services-health-handler request)
      
      ;; Fault Injection
      (and (= uri "/inject/latency") (= method :post)) (inject-latency-handler request)
      (and (= uri "/inject/error") (= method :post)) (inject-error-handler request)
      (and (= uri "/inject/failure") (= method :post)) (inject-failure-handler request)
      (= uri "/inject/list") (list-injections-handler request)
      (= uri "/inject/clear-all") (clear-all-handler request)
      (re-matches #"/inject/clear/([^/]+)" uri)
      (let [id (second (re-matches #"/inject/clear/([^/]+)" uri))]
        (clear-injection-handler (assoc-in request [:params :id] id)))
      
      ;; Testing
      (and (= uri "/test/circuit-breaker") (= method :post)) (circuit-breaker-test-handler request)
      (and (= uri "/test/load") (= method :post)) (load-test-handler request)
      (and (= uri "/test/recovery") (= method :post)) (recovery-test-handler request)
      
      ;; Scenarios
      (and (= uri "/scenario/cascade-failure") (= method :post)) (cascade-failure-handler request)
      (and (= uri "/scenario/recovery-test") (= method :post)) (recovery-scenario-handler request)
      
      ;; Experiments
      (= uri "/experiments") (list-experiments-handler request)
      (re-matches #"/experiments/([^/]+)" uri)
      (let [id (second (re-matches #"/experiments/([^/]+)" uri))]
        (get-experiment-handler (assoc-in request [:params :id] id)))
      
      :else (-> (response/response {:error "Not found"})
                (response/status 404)))))

;; ============================================
;; Middleware
;; ============================================

(defn wrap-cors
  "Add CORS headers"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (response/header "Access-Control-Allow-Origin" "*")
          (response/header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS")
          (response/header "Access-Control-Allow-Headers" "Content-Type, Authorization")))))

(defn wrap-logging
  "Log requests"
  [handler]
  (fn [request]
    (log/info "Chaos Request:" (:request-method request) (:uri request))
    (let [response (handler request)]
      (log/info "Chaos Response:" (:status response))
      response)))

(def app
  "Main application handler"
  (-> router
      wrap-cors
      wrap-logging
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-defaults api-defaults)))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the Chaos Engineering Service server"
  []
  (log/info "Starting Chaos Engineering Service on port" service-port)
  (log/info "Target services:" target-services)
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Chaos Engineering Service starting...")
  (start-server)
  (log/info "Chaos Engineering Service ready on port" service-port))
