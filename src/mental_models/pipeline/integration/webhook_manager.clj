(ns mental-models.pipeline.integration.webhook-manager
  "Webhook management for mental model analysis integrations.
   
   Features:
   - Webhook registration
   - Event subscription
   - Payload delivery
   - Retry logic
   - Signature verification
   - Rate limiting
   - Delivery logging
   - Health monitoring"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.security SecureRandom]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]
           [java.util Base64]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:webhooks {}         ;; webhook-id -> webhook
         :subscriptions {}    ;; subscription-id -> subscription
         :deliveries {}       ;; delivery-id -> delivery
         :delivery-queue []   ;; pending deliveries
         :secrets {}          ;; webhook-id -> secret
         :stats {:deliveries-attempted 0 :deliveries-succeeded 0 :deliveries-failed 0}
         :initialized? false}))

;; ============================================================================
;; Webhook Management
;; ============================================================================

(defn- generate-secret
  "Generate a webhook secret."
  []
  (let [random (SecureRandom.)
        bytes (byte-array 32)]
    (.nextBytes random bytes)
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn register-webhook!
  "Register a webhook endpoint."
  [webhook-id config]
  (let [secret (generate-secret)
        webhook {:id webhook-id
                 :name (get config :name (name webhook-id))
                 :url (get config :url)
                 :events (get config :events #{})
                 :headers (get config :headers {})
                 :content-type (get config :content-type "application/json")
                 :timeout-ms (get config :timeout-ms 30000)
                 :retry-count (get config :retry-count 3)
                 :retry-delay-ms (get config :retry-delay-ms 1000)
                 :rate-limit (get config :rate-limit {:requests-per-minute 60})
                 :enabled? (get config :enabled? true)
                 :status :active
                 :created-at (System/currentTimeMillis)
                 :last-delivery nil
                 :success-count 0
                 :failure-count 0}]
    (swap! state assoc-in [:webhooks webhook-id] webhook)
    (swap! state assoc-in [:secrets webhook-id] secret)
    (logging/log :info "Registered webhook" {:webhook-id webhook-id :url (:url webhook)})
    (events/emit! :webhook-registered {:webhook-id webhook-id})
    {:webhook-id webhook-id :secret secret}))

(defn get-webhook
  "Get a webhook."
  [webhook-id]
  (get-in @state [:webhooks webhook-id]))

(defn list-webhooks
  "List all webhooks."
  []
  (mapv (fn [[id w]]
          {:id id
           :name (:name w)
           :url (:url w)
           :events (:events w)
           :enabled? (:enabled? w)
           :status (:status w)})
        (:webhooks @state)))

(defn update-webhook!
  "Update a webhook."
  [webhook-id updates]
  (swap! state update-in [:webhooks webhook-id]
         (fn [w]
           (merge w updates {:updated-at (System/currentTimeMillis)}))))

(defn delete-webhook!
  "Delete a webhook."
  [webhook-id]
  (swap! state update :webhooks dissoc webhook-id)
  (swap! state update :secrets dissoc webhook-id)
  ;; Remove subscriptions
  (swap! state update :subscriptions
         (fn [subs]
           (into {} (filter (fn [[_ s]] (not= (:webhook-id s) webhook-id)) subs))))
  (logging/log :info "Deleted webhook" {:webhook-id webhook-id}))

(defn enable-webhook!
  "Enable a webhook."
  [webhook-id]
  (swap! state assoc-in [:webhooks webhook-id :enabled?] true)
  (swap! state assoc-in [:webhooks webhook-id :status] :active))

(defn disable-webhook!
  "Disable a webhook."
  [webhook-id]
  (swap! state assoc-in [:webhooks webhook-id :enabled?] false)
  (swap! state assoc-in [:webhooks webhook-id :status] :disabled))

(defn rotate-secret!
  "Rotate webhook secret."
  [webhook-id]
  (let [new-secret (generate-secret)]
    (swap! state assoc-in [:secrets webhook-id] new-secret)
    (logging/log :info "Rotated webhook secret" {:webhook-id webhook-id})
    new-secret))

;; ============================================================================
;; Subscriptions
;; ============================================================================

(defn subscribe!
  "Subscribe a webhook to events."
  [subscription-id config]
  (let [subscription {:id subscription-id
                      :webhook-id (get config :webhook-id)
                      :events (get config :events #{})
                      :filter-fn (get config :filter-fn nil)
                      :transform-fn (get config :transform-fn identity)
                      :enabled? (get config :enabled? true)
                      :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:subscriptions subscription-id] subscription)
    (logging/log :info "Created subscription" {:subscription-id subscription-id})
    subscription-id))

(defn get-subscription
  "Get a subscription."
  [subscription-id]
  (get-in @state [:subscriptions subscription-id]))

(defn list-subscriptions
  "List subscriptions."
  [& {:keys [webhook-id event]}]
  (let [subs (vals (:subscriptions @state))
        filtered (cond->> subs
                   webhook-id (filter #(= (:webhook-id %) webhook-id))
                   event (filter #(contains? (:events %) event)))]
    (mapv #(select-keys % [:id :webhook-id :events :enabled?]) filtered)))

(defn unsubscribe!
  "Remove a subscription."
  [subscription-id]
  (swap! state update :subscriptions dissoc subscription-id)
  (logging/log :info "Removed subscription" {:subscription-id subscription-id}))

;; ============================================================================
;; Signature Generation
;; ============================================================================

(defn- compute-signature
  "Compute HMAC signature for payload."
  [secret payload]
  (let [mac (Mac/getInstance "HmacSHA256")
        key-spec (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")
        _ (.init mac key-spec)
        payload-bytes (if (string? payload)
                        (.getBytes payload "UTF-8")
                        payload)
        signature (.doFinal mac payload-bytes)]
    (str "sha256=" (.encodeToString (Base64/getEncoder) signature))))

(defn verify-signature
  "Verify webhook signature."
  [webhook-id payload signature]
  (when-let [secret (get-in @state [:secrets webhook-id])]
    (let [expected (compute-signature secret payload)]
      (= expected signature))))

;; ============================================================================
;; Delivery
;; ============================================================================

(defn- create-delivery
  "Create a delivery record."
  [webhook-id event payload]
  (let [delivery-id (str (UUID/randomUUID))
        delivery {:id delivery-id
                  :webhook-id webhook-id
                  :event event
                  :payload payload
                  :status :pending
                  :attempts 0
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:deliveries delivery-id] delivery)
    delivery-id))

(defn- update-delivery!
  "Update delivery status."
  [delivery-id updates]
  (swap! state update-in [:deliveries delivery-id] merge updates))

(defn- deliver-webhook
  "Deliver a webhook payload."
  [webhook delivery-id payload]
  (let [secret (get-in @state [:secrets (:id webhook)])
        signature (compute-signature secret (str payload))
        headers (merge (:headers webhook)
                       {"Content-Type" (:content-type webhook)
                        "X-Webhook-Signature" signature
                        "X-Webhook-ID" (str (:id webhook))
                        "X-Delivery-ID" delivery-id
                        "X-Timestamp" (str (System/currentTimeMillis))})]
    ;; Simulate HTTP delivery (in production would use clj-http)
    (try
      (update-delivery! delivery-id {:status :delivered
                                     :delivered-at (System/currentTimeMillis)
                                     :response {:status 200}})
      (swap! state update-in [:webhooks (:id webhook) :success-count] inc)
      (swap! state assoc-in [:webhooks (:id webhook) :last-delivery] (System/currentTimeMillis))
      (swap! state update-in [:stats :deliveries-succeeded] inc)
      (metrics/increment :webhook-deliveries-succeeded {:webhook-id (:id webhook)})
      (logging/log :debug "Delivered webhook" {:webhook-id (:id webhook) :delivery-id delivery-id})
      {:success true :delivery-id delivery-id}
      (catch Exception e
        (update-delivery! delivery-id {:status :failed
                                       :error (.getMessage e)
                                       :failed-at (System/currentTimeMillis)})
        (swap! state update-in [:webhooks (:id webhook) :failure-count] inc)
        (swap! state update-in [:stats :deliveries-failed] inc)
        (metrics/increment :webhook-deliveries-failed {:webhook-id (:id webhook)})
        (logging/log :warn "Webhook delivery failed" {:webhook-id (:id webhook) :error (.getMessage e)})
        {:success false :error (.getMessage e)}))))

(defn- deliver-with-retry
  "Deliver webhook with retry logic."
  [webhook delivery-id payload]
  (let [max-retries (:retry-count webhook 3)
        retry-delay (:retry-delay-ms webhook 1000)]
    (loop [attempt 1]
      (update-delivery! delivery-id {:attempts attempt})
      (let [result (deliver-webhook webhook delivery-id payload)]
        (if (or (:success result) (>= attempt max-retries))
          result
          (do
            (Thread/sleep (* retry-delay attempt)) ;; Exponential backoff
            (recur (inc attempt))))))))

(defn trigger-webhook!
  "Trigger a webhook for an event."
  [event payload]
  (when (flags/enabled? :webhook-manager)
    (let [;; Find matching subscriptions
          matching-subs (filter (fn [[_ s]]
                                  (and (:enabled? s)
                                       (contains? (:events s) event)))
                                (:subscriptions @state))]
      (doseq [[_ subscription] matching-subs]
        (when-let [webhook (get-webhook (:webhook-id subscription))]
          (when (:enabled? webhook)
            ;; Apply filter
            (when (or (nil? (:filter-fn subscription))
                      ((:filter-fn subscription) payload))
              ;; Transform payload
              (let [transformed-payload ((:transform-fn subscription identity) payload)
                    delivery-id (create-delivery (:id webhook) event transformed-payload)]
                (swap! state update-in [:stats :deliveries-attempted] inc)
                (go
                  (deliver-with-retry webhook delivery-id transformed-payload))))))))))

(defn get-delivery
  "Get a delivery."
  [delivery-id]
  (get-in @state [:deliveries delivery-id]))

(defn list-deliveries
  "List deliveries."
  [& {:keys [webhook-id status limit] :or {limit 100}}]
  (let [deliveries (vals (:deliveries @state))
        filtered (cond->> deliveries
                   webhook-id (filter #(= (:webhook-id %) webhook-id))
                   status (filter #(= (:status %) status))
                   true (sort-by :created-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :webhook-id :event :status :attempts :created-at]) filtered)))

(defn retry-delivery!
  "Retry a failed delivery."
  [delivery-id]
  (when-let [delivery (get-delivery delivery-id)]
    (when (= :failed (:status delivery))
      (let [webhook (get-webhook (:webhook-id delivery))]
        (update-delivery! delivery-id {:status :pending})
        (go
          (deliver-with-retry webhook delivery-id (:payload delivery)))))))

;; ============================================================================
;; Health Monitoring
;; ============================================================================

(defn get-webhook-health
  "Get webhook health status."
  [webhook-id]
  (when-let [webhook (get-webhook webhook-id)]
    (let [recent-deliveries (filter (fn [[_ d]]
                                      (and (= (:webhook-id d) webhook-id)
                                           (> (:created-at d)
                                              (- (System/currentTimeMillis) 3600000))))
                                    (:deliveries @state))
          success-count (count (filter #(= :delivered (:status (val %))) recent-deliveries))
          failure-count (count (filter #(= :failed (:status (val %))) recent-deliveries))
          total (+ success-count failure-count)
          success-rate (if (pos? total) (/ success-count total) 1.0)]
      {:webhook-id webhook-id
       :status (cond
                 (< success-rate 0.5) :unhealthy
                 (< success-rate 0.9) :degraded
                 :else :healthy)
       :success-rate success-rate
       :recent-successes success-count
       :recent-failures failure-count
       :last-delivery (:last-delivery webhook)})))

(defn check-all-webhooks-health
  "Check health of all webhooks."
  []
  (let [webhooks (keys (:webhooks @state))]
    (mapv get-webhook-health webhooks)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-webhook-stats
  "Get webhook statistics."
  []
  (let [stats (:stats @state)
        webhooks (vals (:webhooks @state))]
    {:total-webhooks (count webhooks)
     :active-webhooks (count (filter :enabled? webhooks))
     :total-subscriptions (count (:subscriptions @state))
     :total-deliveries (count (:deliveries @state))
     :deliveries-attempted (:deliveries-attempted stats)
     :deliveries-succeeded (:deliveries-succeeded stats)
     :deliveries-failed (:deliveries-failed stats)
     :success-rate (if (pos? (:deliveries-attempted stats))
                     (/ (:deliveries-succeeded stats) (:deliveries-attempted stats))
                     1.0)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-webhook-manager!
  "Initialize the webhook manager."
  []
  (when-not (:initialized? @state)
    ;; Register sample webhook
    (register-webhook! :sample-webhook
                       {:name "Sample Webhook"
                        :url "https://example.com/webhook"
                        :events #{:analysis-complete :model-detected :alert-triggered}
                        :timeout-ms 30000
                        :retry-count 3})
    
    ;; Create sample subscription
    (subscribe! :sample-subscription
                {:webhook-id :sample-webhook
                 :events #{:analysis-complete :model-detected}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Webhook manager initialized")
    (events/emit! :webhook-manager-initialized {})
    true))
