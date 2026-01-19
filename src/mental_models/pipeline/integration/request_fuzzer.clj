(ns mental-models.pipeline.integration.request-fuzzer
  "Request fuzzer for mental model analysis system.
   
   Features:
   - Request fuzzing
   - Input mutation
   - Boundary testing
   - Format fuzzing
   - Security fuzzing
   - Fuzz strategies
   - Crash detection
   - Fuzzing metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Random]
           [java.time Instant LocalDate]
           [java.util.concurrent Executors]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:strategies {}       ;; strategy-id -> strategy
         :findings []         ;; fuzz findings
         :config {:max-iterations 1000
                  :timeout-ms 5000
                  :detect-crashes? true
                  :detect-errors? true
                  :seed nil}
         :stats {:requests-fuzzed 0
                 :mutations-applied 0
                 :crashes-found 0
                 :errors-found 0
                 :interesting-inputs 0}
         :random nil
         :initialized? false}))

;; ============================================================================
;; Random Generators
;; ============================================================================

(defn- get-random
  "Get the random generator."
  []
  (or (:random @state)
      (Random.)))

(defn random-string
  "Generate a random string."
  [& {:keys [length charset]
      :or {length 10
           charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"}}]
  (let [random (get-random)]
    (apply str (repeatedly length #(nth charset (.nextInt random (count charset)))))))

(defn random-int
  "Generate a random integer."
  [& {:keys [min max] :or {min Integer/MIN_VALUE max Integer/MAX_VALUE}}]
  (let [random (get-random)]
    (+ min (.nextInt random (- max min)))))

(defn random-bytes
  "Generate random bytes."
  [length]
  (let [random (get-random)
        bytes (byte-array length)]
    (.nextBytes random bytes)
    bytes))

;; ============================================================================
;; Mutation Strategies
;; ============================================================================

(def built-in-mutations
  {:bit-flip (fn [value]
               (if (string? value)
                 (let [idx (rand-int (max 1 (count value)))
                       chars (vec value)]
                   (apply str (update chars idx (fn [c] (char (bit-xor (int c) (bit-shift-left 1 (rand-int 8))))))))
                 value))
   
   :byte-flip (fn [value]
                (if (string? value)
                  (let [idx (rand-int (max 1 (count value)))
                        chars (vec value)]
                    (apply str (update chars idx (fn [_] (char (rand-int 256))))))
                  value))
   
   :insert-random (fn [value]
                    (if (string? value)
                      (let [idx (rand-int (inc (count value)))]
                        (str (subs value 0 idx)
                             (random-string :length (inc (rand-int 10)))
                             (subs value idx)))
                      value))
   
   :delete-random (fn [value]
                    (if (and (string? value) (pos? (count value)))
                      (let [start (rand-int (count value))
                            length (inc (rand-int (- (count value) start)))]
                        (str (subs value 0 start)
                             (subs value (+ start length))))
                      value))
   
   :duplicate (fn [value]
                (if (string? value)
                  (str value value)
                  value))
   
   :reverse (fn [value]
              (if (string? value)
                (apply str (reverse value))
                value))
   
   :empty (fn [_] "")
   
   :null (fn [_] nil)
   
   :max-length (fn [value]
                 (if (string? value)
                   (apply str (repeat 10000 "A"))
                   value))
   
   :special-chars (fn [_]
                    (random-string :length 20
                                   :charset "<>\"'&;|`$(){}[]\\!@#%^*"))
   
   :unicode (fn [_]
              (apply str (repeatedly 20 #(char (+ 0x4E00 (rand-int 0x9FFF))))))
   
   :sql-injection (fn [_]
                    (rand-nth ["' OR '1'='1"
                               "'; DROP TABLE users;--"
                               "1; SELECT * FROM users"
                               "' UNION SELECT * FROM passwords--"]))
   
   :xss (fn [_]
          (rand-nth ["<script>alert('XSS')</script>"
                     "<img src=x onerror=alert('XSS')>"
                     "javascript:alert('XSS')"
                     "<svg onload=alert('XSS')>"]))
   
   :path-traversal (fn [_]
                     (rand-nth ["../../../etc/passwd"
                                "..\\..\\..\\windows\\system32"
                                "....//....//....//etc/passwd"
                                "%2e%2e%2f%2e%2e%2f"]))
   
   :format-string (fn [_]
                    (rand-nth ["%s%s%s%s%s"
                               "%n%n%n%n%n"
                               "%x%x%x%x%x"
                               "{0}{1}{2}{3}"]))
   
   :boundary-int (fn [_]
                   (rand-nth [0 -1 1 Integer/MAX_VALUE Integer/MIN_VALUE
                              Long/MAX_VALUE Long/MIN_VALUE
                              (dec Integer/MIN_VALUE) (inc Integer/MAX_VALUE)]))
   
   :boundary-float (fn [_]
                     (rand-nth [0.0 -0.0 1.0 -1.0
                                Double/MAX_VALUE Double/MIN_VALUE
                                Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY
                                Double/NaN]))})

;; ============================================================================
;; Custom Strategies
;; ============================================================================

(defn register-strategy!
  "Register a custom fuzz strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :mutate-fn (get config :mutate-fn)
                  :weight (get config :weight 1.0)
                  :enabled? (atom true)
                  :metrics {:applications (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered fuzz strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get a fuzz strategy."
  [strategy-id]
  (or (get-in @state [:strategies strategy-id])
      (when-let [built-in (get built-in-mutations strategy-id)]
        {:id strategy-id :mutate-fn built-in})))

(defn list-strategies
  "List all strategies."
  []
  (concat
   (mapv (fn [[id _]] {:id id :type :built-in}) built-in-mutations)
   (mapv (fn [[id s]]
           {:id id
            :name (:name s)
            :type :custom
            :weight (:weight s)
            :enabled? @(:enabled? s)})
         (:strategies @state))))

;; ============================================================================
;; Request Fuzzing
;; ============================================================================

(defn fuzz-value
  "Fuzz a single value."
  [value & {:keys [strategy] :or {strategy :bit-flip}}]
  (swap! state update-in [:stats :mutations-applied] inc)
  
  (let [mutate-fn (or (:mutate-fn (get-strategy strategy))
                      (get built-in-mutations strategy)
                      identity)]
    (mutate-fn value)))

(defn fuzz-map
  "Fuzz values in a map."
  [m & {:keys [strategies depth max-depth]
        :or {strategies [:bit-flip :insert-random :delete-random]
             depth 0
             max-depth 5}}]
  (if (>= depth max-depth)
    m
    (walk/postwalk
     (fn [x]
       (cond
         (string? x) (fuzz-value x :strategy (rand-nth strategies))
         (number? x) (if (< (rand) 0.3)
                       (fuzz-value x :strategy :boundary-int)
                       x)
         :else x))
     m)))

(defn fuzz-request
  "Fuzz a request."
  [request & {:keys [fuzz-uri? fuzz-headers? fuzz-body? strategies]
              :or {fuzz-uri? true
                   fuzz-headers? true
                   fuzz-body? true
                   strategies [:bit-flip :insert-random :special-chars]}}]
  (swap! state update-in [:stats :requests-fuzzed] inc)
  
  (cond-> request
    (and fuzz-uri? (:uri request))
    (update :uri #(fuzz-value % :strategy (rand-nth strategies)))
    
    (and fuzz-headers? (:headers request))
    (update :headers #(fuzz-map % :strategies strategies))
    
    (and fuzz-body? (:body request))
    (update :body #(if (map? %)
                     (fuzz-map % :strategies strategies)
                     (fuzz-value % :strategy (rand-nth strategies))))))

;; ============================================================================
;; Fuzz Testing
;; ============================================================================

(defn run-fuzz-test
  "Run a fuzz test against a handler."
  [handler base-request & {:keys [iterations strategies detect-crashes? detect-errors?]
                           :or {iterations (get-in @state [:config :max-iterations])
                                strategies [:bit-flip :insert-random :delete-random :special-chars]
                                detect-crashes? true
                                detect-errors? true}}]
  (let [findings (atom [])
        interesting (atom [])]
    
    (dotimes [i iterations]
      (let [fuzzed-request (fuzz-request base-request :strategies strategies)]
        (try
          (let [response (handler fuzzed-request)]
            ;; Check for interesting responses
            (when (or (>= (:status response 200) 500)
                      (str/includes? (str (:body response)) "error")
                      (str/includes? (str (:body response)) "exception"))
              (swap! interesting conj
                     {:iteration i
                      :request fuzzed-request
                      :response response
                      :type :error-response})
              (swap! state update-in [:stats :errors-found] inc)))
          
          (catch Exception e
            (when detect-crashes?
              (swap! findings conj
                     {:iteration i
                      :request fuzzed-request
                      :error (.getMessage e)
                      :type :crash})
              (swap! state update-in [:stats :crashes-found] inc))))))
    
    (swap! state update-in [:stats :interesting-inputs] + (count @interesting))
    
    {:iterations iterations
     :crashes (vec @findings)
     :interesting (vec @interesting)
     :crashes-count (count @findings)
     :interesting-count (count @interesting)}))

(defn run-security-fuzz
  "Run security-focused fuzzing."
  [handler base-request & {:keys [iterations]
                           :or {iterations 100}}]
  (run-fuzz-test handler base-request
                 :iterations iterations
                 :strategies [:sql-injection :xss :path-traversal :format-string]))

(defn run-boundary-fuzz
  "Run boundary-focused fuzzing."
  [handler base-request & {:keys [iterations]
                           :or {iterations 100}}]
  (run-fuzz-test handler base-request
                 :iterations iterations
                 :strategies [:boundary-int :boundary-float :empty :null :max-length]))

;; ============================================================================
;; Findings Management
;; ============================================================================

(defn record-finding!
  "Record a fuzz finding."
  [finding]
  (swap! state update :findings conj
         (assoc finding
                :id (str (UUID/randomUUID))
                :timestamp (System/currentTimeMillis))))

(defn get-findings
  "Get fuzz findings."
  [& {:keys [type limit] :or {limit 100}}]
  (cond->> (:findings @state)
    type (filter #(= (:type %) type))
    true (take-last limit)
    true vec))

(defn clear-findings!
  "Clear all findings."
  []
  (swap! state assoc :findings []))

;; ============================================================================
;; Corpus Management
;; ============================================================================

(defn create-corpus
  "Create a corpus of interesting inputs."
  [base-inputs & {:keys [mutations-per-input]
                  :or {mutations-per-input 10}}]
  (let [corpus (atom (vec base-inputs))]
    (doseq [input base-inputs]
      (dotimes [_ mutations-per-input]
        (let [mutated (if (map? input)
                        (fuzz-map input)
                        (fuzz-value input))]
          (swap! corpus conj mutated))))
    @corpus))

(defn evolve-corpus
  "Evolve a corpus based on interesting findings."
  [corpus interesting-inputs & {:keys [keep-ratio]
                                :or {keep-ratio 0.5}}]
  (let [keep-count (int (* (count corpus) keep-ratio))
        kept (take keep-count (shuffle corpus))
        evolved (mapcat (fn [input]
                          [(fuzz-map input) (fuzz-map input)])
                        interesting-inputs)]
    (vec (concat kept evolved))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-fuzz-detector
  "Ring middleware to detect fuzzed requests."
  [handler]
  (fn [request]
    (let [suspicious? (or (some #(str/includes? (str %) "<script>") (vals (:headers request)))
                          (str/includes? (str (:body request)) "' OR '1'='1")
                          (str/includes? (str (:uri request)) "../"))]
      (if suspicious?
        (do
          (logging/log :warn "Suspicious request detected" {:uri (:uri request)})
          {:status 400
           :headers {"Content-Type" "application/json"}
           :body {:error "Suspicious request blocked"}})
        (handler request)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-iterations!
  "Set maximum fuzz iterations."
  [iterations]
  (swap! state assoc-in [:config :max-iterations] iterations))

(defn set-timeout!
  "Set fuzz timeout."
  [timeout-ms]
  (swap! state assoc-in [:config :timeout-ms] timeout-ms))

(defn set-seed!
  "Set random seed for reproducibility."
  [seed]
  (swap! state assoc-in [:config :seed] seed)
  (swap! state assoc :random (Random. seed)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-fuzzer-metrics
  "Get fuzzer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-fuzzed (:requests-fuzzed stats)
     :mutations-applied (:mutations-applied stats)
     :crashes-found (:crashes-found stats)
     :errors-found (:errors-found stats)
     :interesting-inputs (:interesting-inputs stats)
     :strategies-count (+ (count built-in-mutations)
                          (count (:strategies @state)))
     :findings-count (count (:findings @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-fuzzer-stats
  "Get fuzzer statistics."
  []
  (merge (get-fuzzer-metrics)
         {:max-iterations (get-in @state [:config :max-iterations])
          :timeout-ms (get-in @state [:config :timeout-ms])
          :seed (get-in @state [:config :seed])}))

(defn reset-stats!
  "Reset fuzzer statistics."
  []
  (swap! state assoc :stats {:requests-fuzzed 0
                             :mutations-applied 0
                             :crashes-found 0
                             :errors-found 0
                             :interesting-inputs 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-fuzzer!
  "Initialize the request fuzzer."
  []
  (when-not (:initialized? @state)
    (let [seed (or (get-in @state [:config :seed])
                   (System/currentTimeMillis))]
      (swap! state assoc :random (Random. seed)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request fuzzer initialized")
    (events/emit! :request-fuzzer-initialized {})
    true))
