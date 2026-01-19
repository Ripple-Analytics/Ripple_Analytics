(ns mental-models.pipeline.integration.fallback-handler
  "Fallback Handler Module
   
   Graceful degradation:
   - Fallback functions
   - Default values
   - Cached fallbacks
   - Fallback chains
   - Degraded mode"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]))

;; =============================================================================
;; FALLBACK STATE
;; =============================================================================

(defonce fallback-state (atom {:fallbacks {}
                               :cache {}
                               :degraded-services #{}
                               :config {:cache-ttl-ms 300000
                                        :max-cache-size 1000}}))

;; =============================================================================
;; FALLBACK REGISTRATION
;; =============================================================================

(defn register-fallback!
  "Register a fallback handler."
  [fallback-id {:keys [primary fallback default-value cache-result? on-fallback]}]
  (log/info "Registering fallback" {:id fallback-id})
  (swap! fallback-state assoc-in [:fallbacks fallback-id]
         {:id fallback-id
          :primary primary
          :fallback fallback
          :default-value default-value
          :cache-result? (if (nil? cache-result?) true cache-result?)
          :on-fallback on-fallback
          :created-at (System/currentTimeMillis)})
  (metrics/inc-counter! :fallback/fallbacks-registered)
  fallback-id)

(defn unregister-fallback!
  "Unregister a fallback handler."
  [fallback-id]
  (log/info "Unregistering fallback" {:id fallback-id})
  (swap! fallback-state update :fallbacks dissoc fallback-id))

(defn get-fallback
  "Get a fallback handler."
  [fallback-id]
  (get-in @fallback-state [:fallbacks fallback-id]))

(defn list-fallbacks
  "List all fallback handlers."
  []
  (keys (:fallbacks @fallback-state)))

;; =============================================================================
;; CACHE MANAGEMENT
;; =============================================================================

(defn cache-key
  "Generate a cache key."
  [fallback-id args]
  (str fallback-id ":" (pr-str args)))

(defn get-cached
  "Get a cached result."
  [fallback-id args]
  (let [key (cache-key fallback-id args)
        cached (get-in @fallback-state [:cache key])
        ttl (get-in @fallback-state [:config :cache-ttl-ms])]
    (when (and cached
               (< (- (System/currentTimeMillis) (:timestamp cached)) ttl))
      (:value cached))))

(defn cache-result!
  "Cache a result."
  [fallback-id args value]
  (let [key (cache-key fallback-id args)
        max-size (get-in @fallback-state [:config :max-cache-size])]
    ;; Evict if cache is full
    (when (>= (count (:cache @fallback-state)) max-size)
      (let [oldest-key (first (sort-by #(:timestamp (get-in @fallback-state [:cache %]))
                                       (keys (:cache @fallback-state))))]
        (swap! fallback-state update :cache dissoc oldest-key)))
    (swap! fallback-state assoc-in [:cache key]
           {:value value :timestamp (System/currentTimeMillis)})))

(defn clear-cache!
  "Clear the fallback cache."
  []
  (swap! fallback-state assoc :cache {})
  (log/info "Fallback cache cleared"))

(defn invalidate-cache!
  "Invalidate cache for a specific fallback."
  [fallback-id]
  (swap! fallback-state update :cache
         (fn [cache]
           (into {} (remove (fn [[k _]] (str/starts-with? k (str fallback-id ":"))) cache)))))

;; =============================================================================
;; DEGRADED MODE
;; =============================================================================

(defn mark-degraded!
  "Mark a service as degraded."
  [service-id]
  (log/warn "Marking service as degraded" {:service service-id})
  (swap! fallback-state update :degraded-services conj service-id)
  (metrics/inc-counter! :fallback/services-degraded)
  (events/publish! :fallback/service-degraded {:service service-id}))

(defn mark-healthy!
  "Mark a service as healthy."
  [service-id]
  (log/info "Marking service as healthy" {:service service-id})
  (swap! fallback-state update :degraded-services disj service-id)
  (events/publish! :fallback/service-healthy {:service service-id}))

(defn is-degraded?
  "Check if a service is degraded."
  [service-id]
  (contains? (:degraded-services @fallback-state) service-id))

(defn degraded-services
  "Get all degraded services."
  []
  (:degraded-services @fallback-state))

;; =============================================================================
;; FALLBACK EXECUTION
;; =============================================================================

(defn execute-with-fallback
  "Execute a function with fallback."
  [fallback-id & args]
  (when (flags/is-enabled? "fallback-handler")
    (if-let [fb (get-fallback fallback-id)]
      (let [;; Check cache first
            cached (when (:cache-result? fb)
                     (get-cached fallback-id args))]
        (if cached
          (do
            (metrics/inc-counter! :fallback/cache-hits)
            {:success true :result cached :source :cache})
          ;; Try primary
          (try
            (let [result (apply (:primary fb) args)]
              ;; Cache successful result
              (when (:cache-result? fb)
                (cache-result! fallback-id args result))
              (metrics/inc-counter! :fallback/primary-success)
              {:success true :result result :source :primary})
            (catch Exception primary-error
              (log/warn "Primary failed, trying fallback" {:id fallback-id :error (.getMessage primary-error)})
              (metrics/inc-counter! :fallback/primary-failures)
              ;; Try fallback function
              (if-let [fallback-fn (:fallback fb)]
                (try
                  (let [result (apply fallback-fn args)]
                    (when (:on-fallback fb)
                      ((:on-fallback fb) primary-error))
                    (metrics/inc-counter! :fallback/fallback-success)
                    (events/publish! :fallback/used {:id fallback-id})
                    {:success true :result result :source :fallback})
                  (catch Exception fallback-error
                    (log/error "Fallback also failed" {:id fallback-id :error (.getMessage fallback-error)})
                    (metrics/inc-counter! :fallback/fallback-failures)
                    ;; Return default value if available
                    (if (contains? fb :default-value)
                      {:success true :result (:default-value fb) :source :default}
                      {:success false :error (.getMessage fallback-error) :source :none})))
                ;; No fallback function, try default value
                (if (contains? fb :default-value)
                  (do
                    (when (:on-fallback fb)
                      ((:on-fallback fb) primary-error))
                    {:success true :result (:default-value fb) :source :default})
                  {:success false :error (.getMessage primary-error) :source :none}))))))
      {:success false :error "Fallback not found"})))

(defn execute-or-default
  "Execute with fallback, return default on any failure."
  [fallback-id default-value & args]
  (let [result (apply execute-with-fallback fallback-id args)]
    (if (:success result)
      (:result result)
      default-value)))

;; =============================================================================
;; FALLBACK CHAINS
;; =============================================================================

(defn register-fallback-chain!
  "Register a chain of fallbacks."
  [chain-id fallback-fns & {:keys [default-value]}]
  (log/info "Registering fallback chain" {:id chain-id :length (count fallback-fns)})
  (swap! fallback-state assoc-in [:fallbacks chain-id]
         {:id chain-id
          :type :chain
          :chain fallback-fns
          :default-value default-value
          :created-at (System/currentTimeMillis)})
  chain-id)

(defn execute-chain
  "Execute a fallback chain."
  [chain-id & args]
  (when (flags/is-enabled? "fallback-handler")
    (if-let [fb (get-fallback chain-id)]
      (if (= (:type fb) :chain)
        (loop [fns (:chain fb)
               errors []]
          (if (empty? fns)
            ;; All fallbacks failed
            (if (contains? fb :default-value)
              {:success true :result (:default-value fb) :source :default}
              {:success false :errors errors :source :none})
            ;; Try next function
            (try
              (let [result (apply (first fns) args)]
                (metrics/inc-counter! :fallback/chain-success)
                {:success true :result result :source (- (count (:chain fb)) (count fns))})
              (catch Exception e
                (recur (rest fns) (conj errors (.getMessage e)))))))
        ;; Not a chain, execute normally
        (apply execute-with-fallback chain-id args))
      {:success false :error "Fallback chain not found"})))

;; =============================================================================
;; FALLBACK MACROS
;; =============================================================================

(defmacro with-fallback
  "Execute body with a fallback value."
  [fallback-value & body]
  `(try
     ~@body
     (catch Exception _#
       ~fallback-value)))

(defmacro with-fallback-fn
  "Execute body with a fallback function."
  [fallback-fn & body]
  `(try
     ~@body
     (catch Exception e#
       (~fallback-fn e#))))

(defmacro try-or
  "Try body, return alternative on failure."
  [alternative & body]
  `(try
     ~@body
     (catch Exception _#
       ~alternative)))

;; =============================================================================
;; CONDITIONAL FALLBACK
;; =============================================================================

(defn fallback-when
  "Execute fallback when condition is true."
  [condition primary-fn fallback-fn & args]
  (if condition
    (apply fallback-fn args)
    (try
      (apply primary-fn args)
      (catch Exception _
        (apply fallback-fn args)))))

(defn fallback-unless
  "Execute fallback unless condition is true."
  [condition primary-fn fallback-fn & args]
  (fallback-when (not condition) primary-fn fallback-fn args))

(defn fallback-if-degraded
  "Use fallback if service is degraded."
  [service-id primary-fn fallback-fn & args]
  (fallback-when (is-degraded? service-id) primary-fn fallback-fn args))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-fallback-handler!
  "Initialize fallback handler."
  []
  (log/info "Initializing fallback handler")
  ;; Register feature flag
  (flags/register-flag! "fallback-handler" "Enable fallback handler" true)
  ;; Create metrics
  (metrics/create-counter! :fallback/fallbacks-registered "Fallbacks registered")
  (metrics/create-counter! :fallback/primary-success "Primary success")
  (metrics/create-counter! :fallback/primary-failures "Primary failures")
  (metrics/create-counter! :fallback/fallback-success "Fallback success")
  (metrics/create-counter! :fallback/fallback-failures "Fallback failures")
  (metrics/create-counter! :fallback/cache-hits "Cache hits")
  (metrics/create-counter! :fallback/chain-success "Chain success")
  (metrics/create-counter! :fallback/services-degraded "Services degraded")
  (metrics/create-gauge! :fallback/degraded-count "Degraded services"
                         #(count (degraded-services)))
  (metrics/create-gauge! :fallback/cache-size "Cache size"
                         #(count (:cache @fallback-state)))
  (log/info "Fallback handler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-fallback-status []
  {:enabled (flags/is-enabled? "fallback-handler")
   :fallbacks (count (:fallbacks @fallback-state))
   :cache-size (count (:cache @fallback-state))
   :degraded-services (count (degraded-services))
   :degraded-list (vec (degraded-services))
   :config (:config @fallback-state)})
