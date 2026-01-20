(ns mental-models.connectors.section-17
  "Connectors Module - Section 17"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def circuit-breaker-config
  "Default circuit breaker configuration."
  {:failure-threshold 5        ;; Number of failures before opening circuit
   :success-threshold 3        ;; Number of successes to close circuit
   :timeout-ms 60000           ;; Time to wait before trying half-open (1 minute)
   :half-open-max-calls 3})    ;; Max calls in half-open state

(def circuit-breakers
  "Atom storing circuit breaker state per connector type.
   States: :closed (normal), :open (failing), :half-open (testing)"
  (atom {}))

#?(:clj
   (defn get-circuit-state
     "Get the current circuit breaker state for a connector type."
     [connector-type]
     (get @circuit-breakers connector-type
          {:state :closed
           :failure-count 0
           :success-count 0
           :last-failure-time nil
           :half-open-calls 0})))

#?(:clj
   (defn circuit-open?
     "Check if the circuit is open (blocking requests)."
     [connector-type]
     (let [state (get-circuit-state connector-type)]
       (and (= :open (:state state))
            (let [timeout (:timeout-ms circuit-breaker-config)
                  last-failure (:last-failure-time state)]
              (and last-failure
                   (< (- (System/currentTimeMillis) last-failure) timeout)))))))

#?(:clj
   (defn circuit-half-open?
     "Check if the circuit is in half-open state (testing)."
     [connector-type]
     (let [state (get-circuit-state connector-type)]
       (or (= :half-open (:state state))
           (and (= :open (:state state))
                (let [timeout (:timeout-ms circuit-breaker-config)
                      last-failure (:last-failure-time state)]
                  (and last-failure
                       (>= (- (System/currentTimeMillis) last-failure) timeout))))))))

#?(:clj
   (defn record-circuit-success
     "Record a successful call for circuit breaker."
     [connector-type]
     (swap! circuit-breakers update connector-type
            (fn [state]
              (let [current (or state {:state :closed :failure-count 0 :success-count 0})]
                (case (:state current)
                  :closed current
                  :half-open
                  (let [new-success-count (inc (:success-count current))]
                    (if (>= new-success-count (:success-threshold circuit-breaker-config))
                      {:state :closed :failure-count 0 :success-count 0 :last-failure-time nil :half-open-calls 0}
                      (assoc current :success-count new-success-count)))
                  :open
                  (if (circuit-half-open? connector-type)
                    {:state :half-open :failure-count 0 :success-count 1 :last-failure-time nil :half-open-calls 1}
                    current)))))))

#?(:clj
   (defn record-circuit-failure
     "Record a failed call for circuit breaker."
     [connector-type]
     (swap! circuit-breakers update connector-type
            (fn [state]
              (let [current (or state {:state :closed :failure-count 0 :success-count 0})
                    new-failure-count (inc (:failure-count current))]
                (case (:state current)
                  :closed
                  (if (>= new-failure-count (:failure-threshold circuit-breaker-config))
                    {:state :open :failure-count new-failure-count :success-count 0 
                     :last-failure-time (System/currentTimeMillis) :half-open-calls 0}
                    (assoc current :failure-count new-failure-count))
                  :half-open
                  {:state :open :failure-count 1 :success-count 0 
                   :last-failure-time (System/currentTimeMillis) :half-open-calls 0}
                  :open
                  (assoc current :last-failure-time (System/currentTimeMillis))))))))

#?(:clj
   (defn reset-circuit
     "Reset the circuit breaker for a connector type."
     [connector-type]
     (swap! circuit-breakers dissoc connector-type)))

#?(:clj
   (defn reset-all-circuits
     "Reset all circuit breakers."
     []
     (reset! circuit-breakers {})))

#?(:clj
   (defn get-circuit-status
     "Get a summary of circuit breaker status for all connectors."
     []
     (into {}
           (map (fn [[k v]]
                  [k {:state (:state v)
                      :failure-count (:failure-count v)
                      :success-count (:success-count v)}])
                @circuit-breakers))))

#?(:clj
   (defn with-circuit-breaker
     "Execute a function with circuit breaker protection.
      Returns {:success true :value result} or {:success false :error error-msg}."
     [connector-type f]
     (cond
       ;; Circuit is open - fail fast
       (circuit-open? connector-type)
       {:success false
        :error "Circuit breaker is open"
        :circuit-state :open
        :timestamp (java.time.Instant/now)}
       
       ;; Circuit is half-open - allow limited calls
       (circuit-half-open? connector-type)
       (let [state (get-circuit-state connector-type)]
         (if (>= (:half-open-calls state) (:half-open-max-calls circuit-breaker-config))
           {:success false
            :error "Circuit breaker half-open limit reached"
            :circuit-state :half-open
            :timestamp (java.time.Instant/now)}
           (do
             (swap! circuit-breakers update connector-type
                    #(update % :half-open-calls (fnil inc 0)))
             (try
               (let [result (f)]
                 (record-circuit-success connector-type)
                 {:success true :value result :circuit-state :half-open})
               (catch Exception e
                 (record-circuit-failure connector-type)
                 {:success false :error (.getMessage e) :circuit-state :half-open})))))
       
       ;; Circuit is closed - normal operation
       :else
       (try
         (let [result (f)]
           (record-circuit-success connector-type)
           {:success true :value result :circuit-state :closed})
         (catch Exception e
           (record-circuit-failure connector-type)
           {:success false :error (.getMessage e) :circuit-state :closed})))))

