(ns mental-models.pipeline.integration.webhook-handler
  "Webhook Handler Module
   
   Incoming webhook processing:
   - Webhook registration
   - Signature verification
   - Payload parsing
   - Event routing
   - Retry handling"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log]
   [mental-models.audit.logger :as audit])
  (:import
   [javax.crypto Mac]
   [javax.crypto.spec SecretKeySpec]
   [java.util Base64]))

;; =============================================================================
;; WEBHOOK STATE
;; =============================================================================

(defonce webhook-state (atom {:webhooks {}
                              :handlers {}
                              :received []
                              :config {:max-history 1000
                                       :signature-header "X-Webhook-Signature"
                                       :timestamp-header "X-Webhook-Timestamp"
                                       :max-age-seconds 300}}))

;; =============================================================================
;; WEBHOOK REGISTRATION
;; =============================================================================

(defn register-webhook!
  "Register a webhook endpoint."
  [webhook-id {:keys [secret event-types handler description]}]
  (log/info "Registering webhook" {:id webhook-id :event-types event-types})
  (swap! webhook-state assoc-in [:webhooks webhook-id]
         {:id webhook-id
          :secret secret
          :event-types (set event-types)
          :handler handler
          :description description
          :created-at (System/currentTimeMillis)
          :received-count 0
          :last-received nil})
  (metrics/inc-counter! :webhook/registered)
  webhook-id)

(defn unregister-webhook!
  "Unregister a webhook endpoint."
  [webhook-id]
  (log/info "Unregistering webhook" {:id webhook-id})
  (swap! webhook-state update :webhooks dissoc webhook-id))

(defn get-webhook
  "Get a webhook by ID."
  [webhook-id]
  (get-in @webhook-state [:webhooks webhook-id]))

(defn list-webhooks
  "List all registered webhooks."
  []
  (keys (:webhooks @webhook-state)))

;; =============================================================================
;; SIGNATURE VERIFICATION
;; =============================================================================

(defn compute-hmac-sha256
  "Compute HMAC-SHA256 signature."
  [secret payload]
  (let [mac (Mac/getInstance "HmacSHA256")
        secret-key (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac secret-key)
    (let [hash-bytes (.doFinal mac (.getBytes payload "UTF-8"))]
      (.encodeToString (Base64/getEncoder) hash-bytes))))

(defn verify-signature
  "Verify webhook signature."
  [webhook-id payload signature]
  (when-let [webhook (get-webhook webhook-id)]
    (let [expected (compute-hmac-sha256 (:secret webhook) payload)]
      (= signature expected))))

(defn verify-timestamp
  "Verify webhook timestamp is not too old."
  [timestamp-str]
  (try
    (let [timestamp (Long/parseLong timestamp-str)
          now (System/currentTimeMillis)
          max-age-ms (* (get-in @webhook-state [:config :max-age-seconds]) 1000)]
      (< (- now timestamp) max-age-ms))
    (catch Exception _
      false)))

;; =============================================================================
;; PAYLOAD PARSING
;; =============================================================================

(defn parse-json-payload
  "Parse JSON payload."
  [body]
  (try
    (read-string body)
    (catch Exception e
      (log/warn "Failed to parse JSON payload" {:error (.getMessage e)})
      nil)))

(defn parse-form-payload
  "Parse form-encoded payload."
  [body]
  (try
    (into {}
          (for [pair (str/split body #"&")]
            (let [[k v] (str/split pair #"=")]
              [(keyword k) (java.net.URLDecoder/decode v "UTF-8")])))
    (catch Exception e
      (log/warn "Failed to parse form payload" {:error (.getMessage e)})
      nil)))

(defn parse-payload
  "Parse payload based on content type."
  [body content-type]
  (cond
    (str/includes? (or content-type "") "application/json")
    (parse-json-payload body)
    
    (str/includes? (or content-type "") "application/x-www-form-urlencoded")
    (parse-form-payload body)
    
    :else
    {:raw body}))

;; =============================================================================
;; EVENT ROUTING
;; =============================================================================

(defn route-event
  "Route a webhook event to the appropriate handler."
  [webhook-id event-type payload]
  (when-let [webhook (get-webhook webhook-id)]
    (when (or (empty? (:event-types webhook))
              (contains? (:event-types webhook) event-type))
      (when-let [handler (:handler webhook)]
        (try
          (handler {:webhook-id webhook-id
                    :event-type event-type
                    :payload payload
                    :timestamp (System/currentTimeMillis)})
          (catch Exception e
            (log/error "Webhook handler error" {:webhook webhook-id
                                                :event event-type
                                                :error (.getMessage e)})
            (throw e)))))))

;; =============================================================================
;; WEBHOOK PROCESSING
;; =============================================================================

(defn process-webhook
  "Process an incoming webhook request."
  [webhook-id request]
  (when (flags/is-enabled? "webhook-handler")
    (log/info "Processing webhook" {:id webhook-id})
    (metrics/inc-counter! :webhook/received)
    (let [{:keys [headers body]} request
          signature-header (get-in @webhook-state [:config :signature-header])
          timestamp-header (get-in @webhook-state [:config :timestamp-header])
          signature (get headers signature-header)
          timestamp (get headers timestamp-header)
          content-type (get headers "content-type")
          webhook (get-webhook webhook-id)]
      (cond
        ;; Webhook not found
        (nil? webhook)
        (do
          (log/warn "Webhook not found" {:id webhook-id})
          (metrics/inc-counter! :webhook/not-found)
          {:status 404 :body {:error "Webhook not found"}})
        
        ;; Verify signature if secret is configured
        (and (:secret webhook) (not (verify-signature webhook-id body signature)))
        (do
          (log/warn "Invalid webhook signature" {:id webhook-id})
          (metrics/inc-counter! :webhook/invalid-signature)
          (audit/log-event! :webhook/invalid-signature {:webhook webhook-id})
          {:status 401 :body {:error "Invalid signature"}})
        
        ;; Verify timestamp if provided
        (and timestamp (not (verify-timestamp timestamp)))
        (do
          (log/warn "Webhook timestamp too old" {:id webhook-id})
          (metrics/inc-counter! :webhook/expired)
          {:status 400 :body {:error "Request expired"}})
        
        ;; Process the webhook
        :else
        (let [payload (parse-payload body content-type)
              event-type (or (:event_type payload) (:type payload) :unknown)
              result (try
                       (route-event webhook-id event-type payload)
                       {:status 200 :body {:success true}}
                       (catch Exception e
                         (metrics/inc-counter! :webhook/handler-errors)
                         {:status 500 :body {:error (.getMessage e)}}))]
          ;; Update webhook stats
          (swap! webhook-state update-in [:webhooks webhook-id]
                 (fn [w]
                   (-> w
                       (update :received-count inc)
                       (assoc :last-received (System/currentTimeMillis)))))
          ;; Store in history
          (swap! webhook-state update :received
                 (fn [history]
                   (let [entry {:webhook-id webhook-id
                                :event-type event-type
                                :timestamp (System/currentTimeMillis)
                                :status (:status result)}
                         max-history (get-in @webhook-state [:config :max-history])]
                     (take max-history (conj history entry)))))
          ;; Publish event
          (events/publish! :webhook/processed {:webhook webhook-id
                                               :event event-type
                                               :status (:status result)})
          result)))))

;; =============================================================================
;; WEBHOOK HANDLERS
;; =============================================================================

(defn register-handler!
  "Register a named webhook handler."
  [handler-id handler-fn]
  (log/info "Registering webhook handler" {:id handler-id})
  (swap! webhook-state assoc-in [:handlers handler-id] handler-fn))

(defn get-handler
  "Get a named webhook handler."
  [handler-id]
  (get-in @webhook-state [:handlers handler-id]))

;; Default handlers
(def github-handler
  "Handler for GitHub webhooks."
  (fn [{:keys [event-type payload]}]
    (log/info "GitHub webhook received" {:event event-type})
    (events/publish! :github/webhook {:event event-type :payload payload})))

(def slack-handler
  "Handler for Slack webhooks."
  (fn [{:keys [event-type payload]}]
    (log/info "Slack webhook received" {:event event-type})
    (events/publish! :slack/webhook {:event event-type :payload payload})))

(def stripe-handler
  "Handler for Stripe webhooks."
  (fn [{:keys [event-type payload]}]
    (log/info "Stripe webhook received" {:event event-type})
    (events/publish! :stripe/webhook {:event event-type :payload payload})))

;; =============================================================================
;; WEBHOOK HISTORY
;; =============================================================================

(defn get-webhook-history
  "Get webhook history."
  [& {:keys [webhook-id limit]}]
  (let [history (:received @webhook-state)
        filtered (if webhook-id
                   (filter #(= (:webhook-id %) webhook-id) history)
                   history)]
    (if limit
      (take limit filtered)
      filtered)))

(defn clear-history!
  "Clear webhook history."
  []
  (swap! webhook-state assoc :received []))

;; =============================================================================
;; RING HANDLER
;; =============================================================================

(defn make-webhook-handler
  "Create a Ring handler for webhooks."
  [webhook-id]
  (fn [request]
    (process-webhook webhook-id request)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-webhook-handler!
  "Initialize webhook handler."
  []
  (log/info "Initializing webhook handler")
  ;; Register feature flag
  (flags/register-flag! "webhook-handler" "Enable webhook handling" true)
  ;; Create metrics
  (metrics/create-counter! :webhook/registered "Webhooks registered")
  (metrics/create-counter! :webhook/received "Webhooks received")
  (metrics/create-counter! :webhook/not-found "Webhooks not found")
  (metrics/create-counter! :webhook/invalid-signature "Invalid signatures")
  (metrics/create-counter! :webhook/expired "Expired webhooks")
  (metrics/create-counter! :webhook/handler-errors "Handler errors")
  (metrics/create-gauge! :webhook/total "Total webhooks"
                         #(count (:webhooks @webhook-state)))
  ;; Register default handlers
  (register-handler! :github github-handler)
  (register-handler! :slack slack-handler)
  (register-handler! :stripe stripe-handler)
  (log/info "Webhook handler initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-webhook-status []
  {:enabled (flags/is-enabled? "webhook-handler")
   :total-webhooks (count (:webhooks @webhook-state))
   :total-handlers (count (:handlers @webhook-state))
   :history-size (count (:received @webhook-state))
   :webhooks (into {} (map (fn [[k v]]
                             [k {:received-count (:received-count v)
                                 :last-received (:last-received v)
                                 :event-types (:event-types v)}])
                           (:webhooks @webhook-state)))})
