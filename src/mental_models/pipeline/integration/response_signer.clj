(ns mental-models.pipeline.integration.response-signer
  "Response signer for mental model analysis system.
   
   Features:
   - Response signing
   - Signature verification
   - Multiple algorithms
   - Key management
   - Signature headers
   - Timestamp validation
   - Signature caching
   - Signing metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Base64]
           [java.time Instant]
           [java.security MessageDigest KeyPairGenerator Signature KeyPair PrivateKey PublicKey]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:keys {}             ;; key-id -> signing key
         :config {:default-algorithm :hmac-sha256
                  :timestamp-tolerance-ms 300000
                  :include-timestamp? true
                  :include-nonce? true}
         :stats {:signatures-created 0
                 :signatures-verified 0
                 :verification-failures 0
                 :nonces-used 0}
         :nonces #{}          ;; used nonces for replay protection
         :initialized? false}))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn- bytes->base64
  "Convert bytes to base64 string."
  [bytes]
  (.encodeToString (Base64/getEncoder) bytes))

(defn- base64->bytes
  "Convert base64 string to bytes."
  [s]
  (.decode (Base64/getDecoder) s))

(defn- generate-nonce
  "Generate a unique nonce."
  []
  (str (UUID/randomUUID)))

;; ============================================================================
;; Key Management
;; ============================================================================

(defn generate-hmac-key!
  "Generate an HMAC key."
  [key-id secret]
  (let [key-config {:id key-id
                    :type :hmac
                    :secret secret
                    :algorithm "HmacSHA256"
                    :created-at (System/currentTimeMillis)
                    :enabled? (atom true)}]
    
    (swap! state assoc-in [:keys key-id] key-config)
    (logging/log :info "Generated HMAC key" {:key-id key-id})
    key-id))

(defn generate-rsa-key-pair!
  "Generate an RSA key pair."
  [key-id & {:keys [key-size] :or {key-size 2048}}]
  (let [key-gen (KeyPairGenerator/getInstance "RSA")
        _ (.initialize key-gen key-size)
        key-pair (.generateKeyPair key-gen)
        key-config {:id key-id
                    :type :rsa
                    :private-key (.getPrivate key-pair)
                    :public-key (.getPublic key-pair)
                    :key-size key-size
                    :created-at (System/currentTimeMillis)
                    :enabled? (atom true)}]
    
    (swap! state assoc-in [:keys key-id] key-config)
    (logging/log :info "Generated RSA key pair" {:key-id key-id})
    key-id))

(defn get-key
  "Get a signing key."
  [key-id]
  (get-in @state [:keys key-id]))

(defn list-keys
  "List all signing keys."
  []
  (mapv (fn [[id k]]
          {:id id
           :type (:type k)
           :enabled? @(:enabled? k)
           :created-at (:created-at k)})
        (:keys @state)))

(defn disable-key!
  "Disable a signing key."
  [key-id]
  (when-let [key-config (get-key key-id)]
    (reset! (:enabled? key-config) false)))

(defn delete-key!
  "Delete a signing key."
  [key-id]
  (swap! state update :keys dissoc key-id))

;; ============================================================================
;; HMAC Signing
;; ============================================================================

(defn sign-hmac
  "Sign data using HMAC."
  [data secret & {:keys [algorithm] :or {algorithm "HmacSHA256"}}]
  (let [mac (Mac/getInstance algorithm)
        secret-key (SecretKeySpec. (.getBytes secret "UTF-8") algorithm)]
    (.init mac secret-key)
    (bytes->base64 (.doFinal mac (.getBytes data "UTF-8")))))

(defn verify-hmac
  "Verify an HMAC signature."
  [data signature secret & {:keys [algorithm] :or {algorithm "HmacSHA256"}}]
  (let [expected (sign-hmac data secret :algorithm algorithm)]
    (= signature expected)))

;; ============================================================================
;; RSA Signing
;; ============================================================================

(defn sign-rsa
  "Sign data using RSA."
  [data private-key & {:keys [algorithm] :or {algorithm "SHA256withRSA"}}]
  (let [sig (Signature/getInstance algorithm)]
    (.initSign sig private-key)
    (.update sig (.getBytes data "UTF-8"))
    (bytes->base64 (.sign sig))))

(defn verify-rsa
  "Verify an RSA signature."
  [data signature public-key & {:keys [algorithm] :or {algorithm "SHA256withRSA"}}]
  (let [sig (Signature/getInstance algorithm)]
    (.initVerify sig public-key)
    (.update sig (.getBytes data "UTF-8"))
    (.verify sig (base64->bytes signature))))

;; ============================================================================
;; Response Signing
;; ============================================================================

(defn- create-signing-string
  "Create the string to sign."
  [response & {:keys [include-timestamp? include-nonce?]
               :or {include-timestamp? true include-nonce? true}}]
  (let [body (if (string? (:body response))
               (:body response)
               (pr-str (:body response)))
        timestamp (when include-timestamp? (System/currentTimeMillis))
        nonce (when include-nonce? (generate-nonce))
        parts (cond-> [(str "status:" (:status response))
                       (str "body:" body)]
                timestamp (conj (str "timestamp:" timestamp))
                nonce (conj (str "nonce:" nonce)))]
    {:signing-string (str/join "\n" parts)
     :timestamp timestamp
     :nonce nonce}))

(defn sign-response
  "Sign a response."
  [response key-id]
  (when-let [key-config (get-key key-id)]
    (when @(:enabled? key-config)
      (swap! state update-in [:stats :signatures-created] inc)
      
      (let [{:keys [signing-string timestamp nonce]}
            (create-signing-string response
                                   :include-timestamp? (get-in @state [:config :include-timestamp?])
                                   :include-nonce? (get-in @state [:config :include-nonce?]))
            
            signature (case (:type key-config)
                        :hmac (sign-hmac signing-string (:secret key-config))
                        :rsa (sign-rsa signing-string (:private-key key-config))
                        nil)]
        
        (when nonce
          (swap! state update :nonces conj nonce)
          (swap! state update-in [:stats :nonces-used] inc))
        
        (cond-> response
          signature (assoc-in [:headers "X-Signature"] signature)
          key-id (assoc-in [:headers "X-Signature-Key-Id"] (str key-id))
          timestamp (assoc-in [:headers "X-Signature-Timestamp"] (str timestamp))
          nonce (assoc-in [:headers "X-Signature-Nonce"] nonce))))))

(defn verify-response
  "Verify a response signature."
  [response]
  (let [signature (get-in response [:headers "X-Signature"])
        key-id (keyword (get-in response [:headers "X-Signature-Key-Id"]))
        timestamp (some-> (get-in response [:headers "X-Signature-Timestamp"])
                          Long/parseLong)
        nonce (get-in response [:headers "X-Signature-Nonce"])]
    
    (cond
      (nil? signature)
      {:valid? false :error :missing-signature}
      
      (nil? key-id)
      {:valid? false :error :missing-key-id}
      
      :else
      (if-let [key-config (get-key key-id)]
        (let [;; Check timestamp
              timestamp-valid? (or (nil? timestamp)
                                   (< (Math/abs (- (System/currentTimeMillis) timestamp))
                                      (get-in @state [:config :timestamp-tolerance-ms])))
              
              ;; Check nonce (replay protection)
              nonce-valid? (or (nil? nonce)
                               (not (contains? (:nonces @state) nonce)))
              
              ;; Recreate signing string
              {:keys [signing-string]} (create-signing-string
                                        (assoc response
                                               :headers (dissoc (:headers response)
                                                                "X-Signature"
                                                                "X-Signature-Key-Id"
                                                                "X-Signature-Timestamp"
                                                                "X-Signature-Nonce"))
                                        :include-timestamp? (some? timestamp)
                                        :include-nonce? (some? nonce))
              
              ;; Verify signature
              signature-valid? (case (:type key-config)
                                 :hmac (verify-hmac signing-string signature (:secret key-config))
                                 :rsa (verify-rsa signing-string signature (:public-key key-config))
                                 false)]
          
          (swap! state update-in [:stats :signatures-verified] inc)
          
          (cond
            (not timestamp-valid?)
            (do
              (swap! state update-in [:stats :verification-failures] inc)
              {:valid? false :error :timestamp-expired})
            
            (not nonce-valid?)
            (do
              (swap! state update-in [:stats :verification-failures] inc)
              {:valid? false :error :nonce-reused})
            
            (not signature-valid?)
            (do
              (swap! state update-in [:stats :verification-failures] inc)
              {:valid? false :error :invalid-signature})
            
            :else
            (do
              (when nonce
                (swap! state update :nonces conj nonce))
              {:valid? true})))
        
        (do
          (swap! state update-in [:stats :verification-failures] inc)
          {:valid? false :error :unknown-key})))))

;; ============================================================================
;; Request Signing
;; ============================================================================

(defn sign-request
  "Sign a request."
  [request key-id]
  (when-let [key-config (get-key key-id)]
    (when @(:enabled? key-config)
      (let [body (if (string? (:body request))
                   (:body request)
                   (pr-str (:body request)))
            timestamp (System/currentTimeMillis)
            nonce (generate-nonce)
            signing-string (str/join "\n"
                                     [(str "method:" (:request-method request))
                                      (str "uri:" (:uri request))
                                      (str "body:" body)
                                      (str "timestamp:" timestamp)
                                      (str "nonce:" nonce)])
            signature (case (:type key-config)
                        :hmac (sign-hmac signing-string (:secret key-config))
                        :rsa (sign-rsa signing-string (:private-key key-config))
                        nil)]
        
        (cond-> request
          signature (assoc-in [:headers "X-Signature"] signature)
          key-id (assoc-in [:headers "X-Signature-Key-Id"] (str key-id))
          timestamp (assoc-in [:headers "X-Signature-Timestamp"] (str timestamp))
          nonce (assoc-in [:headers "X-Signature-Nonce"] nonce))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-sign
  "Ring middleware to sign responses."
  [handler key-id]
  (fn [request]
    (let [response (handler request)]
      (sign-response response key-id))))

(defn wrap-verify-request
  "Ring middleware to verify request signatures."
  [handler]
  (fn [request]
    (let [signature (get-in request [:headers "x-signature"])]
      (if signature
        (let [result (verify-response request)]
          (if (:valid? result)
            (handler (assoc request :signature-verified? true))
            {:status 401
             :headers {"Content-Type" "application/json"}
             :body {:error "Invalid signature"
                    :reason (:error result)}}))
        (handler request)))))

(defn wrap-require-signature
  "Ring middleware to require request signatures."
  [handler key-id]
  (fn [request]
    (let [signature (get-in request [:headers "x-signature"])]
      (if signature
        (let [result (verify-response request)]
          (if (:valid? result)
            (handler (assoc request :signature-verified? true))
            {:status 401
             :headers {"Content-Type" "application/json"}
             :body {:error "Invalid signature"
                    :reason (:error result)}}))
        {:status 401
         :headers {"Content-Type" "application/json"}
         :body {:error "Signature required"}}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-algorithm!
  "Set default signing algorithm."
  [algorithm]
  (swap! state assoc-in [:config :default-algorithm] algorithm))

(defn set-timestamp-tolerance!
  "Set timestamp tolerance."
  [tolerance-ms]
  (swap! state assoc-in [:config :timestamp-tolerance-ms] tolerance-ms))

(defn set-include-timestamp!
  "Enable/disable timestamp in signatures."
  [include?]
  (swap! state assoc-in [:config :include-timestamp?] include?))

(defn set-include-nonce!
  "Enable/disable nonce in signatures."
  [include?]
  (swap! state assoc-in [:config :include-nonce?] include?))

(defn clear-nonces!
  "Clear used nonces."
  []
  (swap! state assoc :nonces #{}))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-signer-metrics
  "Get signer metrics."
  []
  (let [stats (:stats @state)]
    {:signatures-created (:signatures-created stats)
     :signatures-verified (:signatures-verified stats)
     :verification-failures (:verification-failures stats)
     :nonces-used (:nonces-used stats)
     :keys-count (count (:keys @state))
     :nonces-count (count (:nonces @state))
     :verification-success-rate (if (pos? (:signatures-verified stats))
                                  (/ (- (:signatures-verified stats)
                                        (:verification-failures stats))
                                     (:signatures-verified stats))
                                  1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-signer-stats
  "Get signer statistics."
  []
  (merge (get-signer-metrics)
         {:default-algorithm (get-in @state [:config :default-algorithm])
          :timestamp-tolerance-ms (get-in @state [:config :timestamp-tolerance-ms])
          :include-timestamp? (get-in @state [:config :include-timestamp?])
          :include-nonce? (get-in @state [:config :include-nonce?])}))

(defn reset-stats!
  "Reset signer statistics."
  []
  (swap! state assoc :stats {:signatures-created 0
                             :signatures-verified 0
                             :verification-failures 0
                             :nonces-used 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-signer!
  "Initialize the response signer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response signer initialized")
    (events/emit! :response-signer-initialized {})
    true))
