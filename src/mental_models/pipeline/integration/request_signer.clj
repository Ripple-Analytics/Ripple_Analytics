(ns mental-models.pipeline.integration.request-signer
  "Request signer for mental model analysis system.
   
   Features:
   - HMAC signing
   - RSA signing
   - Request canonicalization
   - Signature verification
   - Timestamp validation
   - Nonce handling
   - Multiple signing algorithms
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
           [java.time Instant LocalDate]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]
           [java.security MessageDigest Signature KeyFactory]
           [java.security.spec PKCS8EncodedKeySpec X509EncodedKeySpec]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:keys {}             ;; key-id -> key-config
         :nonces #{}          ;; Used nonces
         :config {:default-algorithm :hmac-sha256
                  :timestamp-tolerance-ms 300000  ;; 5 minutes
                  :nonce-expiry-ms 600000         ;; 10 minutes
                  :include-headers [:host :date :content-type]}
         :stats {:requests-signed 0
                 :signatures-verified 0
                 :verification-failures 0
                 :expired-timestamps 0
                 :replay-attacks-blocked 0}
         :initialized? false}))

;; ============================================================================
;; Key Management
;; ============================================================================

(defn register-key!
  "Register a signing key."
  [key-id config]
  (let [key-config {:id key-id
                    :name (get config :name (name key-id))
                    :algorithm (get config :algorithm :hmac-sha256)
                    :secret (get config :secret)
                    :public-key (get config :public-key)
                    :private-key (get config :private-key)
                    :enabled? (atom true)
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:keys key-id] key-config)
    (logging/log :info "Registered signing key" {:key-id key-id :algorithm (:algorithm key-config)})
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
           :name (:name k)
           :algorithm (:algorithm k)
           :enabled? @(:enabled? k)})
        (:keys @state)))

(defn delete-key!
  "Delete a signing key."
  [key-id]
  (swap! state update :keys dissoc key-id)
  (logging/log :info "Deleted signing key" {:key-id key-id}))

;; ============================================================================
;; Canonicalization
;; ============================================================================

(defn- canonicalize-headers
  "Canonicalize headers for signing."
  [headers include-headers]
  (let [normalized (into {} (map (fn [[k v]] [(str/lower-case (name k)) v]) headers))]
    (->> include-headers
         (map (fn [h]
                (let [header-name (str/lower-case (name h))
                      value (get normalized header-name "")]
                  (str header-name ":" (str/trim value)))))
         (str/join "\n"))))

(defn- canonicalize-request
  "Canonicalize a request for signing."
  [request include-headers]
  (let [method (str/upper-case (name (get request :method :get)))
        uri (get request :uri "/")
        query-string (get request :query-string "")
        headers (canonicalize-headers (get request :headers {}) include-headers)
        body-hash (when-let [body (get request :body)]
                    (let [md (MessageDigest/getInstance "SHA-256")
                          body-bytes (if (string? body) (.getBytes body "UTF-8") body)]
                      (.encodeToString (Base64/getEncoder) (.digest md body-bytes))))]
    (str method "\n"
         uri "\n"
         query-string "\n"
         headers "\n"
         (or body-hash ""))))

;; ============================================================================
;; HMAC Signing
;; ============================================================================

(defn- get-hmac-algorithm
  "Get the HMAC algorithm name."
  [algorithm]
  (case algorithm
    :hmac-sha256 "HmacSHA256"
    :hmac-sha384 "HmacSHA384"
    :hmac-sha512 "HmacSHA512"
    "HmacSHA256"))

(defn- hmac-sign
  "Sign data using HMAC."
  [data secret algorithm]
  (let [algo-name (get-hmac-algorithm algorithm)
        mac (Mac/getInstance algo-name)
        secret-key (SecretKeySpec. (.getBytes secret "UTF-8") algo-name)]
    (.init mac secret-key)
    (.encodeToString (Base64/getEncoder) (.doFinal mac (.getBytes data "UTF-8")))))

(defn- hmac-verify
  "Verify an HMAC signature."
  [data secret algorithm signature]
  (let [expected (hmac-sign data secret algorithm)]
    (= expected signature)))

;; ============================================================================
;; RSA Signing
;; ============================================================================

(defn- get-rsa-algorithm
  "Get the RSA algorithm name."
  [algorithm]
  (case algorithm
    :rsa-sha256 "SHA256withRSA"
    :rsa-sha384 "SHA384withRSA"
    :rsa-sha512 "SHA512withRSA"
    "SHA256withRSA"))

(defn- rsa-sign
  "Sign data using RSA."
  [data private-key-bytes algorithm]
  (let [algo-name (get-rsa-algorithm algorithm)
        key-spec (PKCS8EncodedKeySpec. private-key-bytes)
        key-factory (KeyFactory/getInstance "RSA")
        private-key (.generatePrivate key-factory key-spec)
        signature (Signature/getInstance algo-name)]
    (.initSign signature private-key)
    (.update signature (.getBytes data "UTF-8"))
    (.encodeToString (Base64/getEncoder) (.sign signature))))

(defn- rsa-verify
  "Verify an RSA signature."
  [data public-key-bytes algorithm signature]
  (let [algo-name (get-rsa-algorithm algorithm)
        key-spec (X509EncodedKeySpec. public-key-bytes)
        key-factory (KeyFactory/getInstance "RSA")
        public-key (.generatePublic key-factory key-spec)
        sig (Signature/getInstance algo-name)]
    (.initVerify sig public-key)
    (.update sig (.getBytes data "UTF-8"))
    (.verify sig (.decode (Base64/getDecoder) signature))))

;; ============================================================================
;; Request Signing
;; ============================================================================

(defn sign-request
  "Sign a request."
  [request key-id]
  (if-let [key-config (get-key key-id)]
    (if @(:enabled? key-config)
      (let [include-headers (get-in @state [:config :include-headers])
            timestamp (str (System/currentTimeMillis))
            nonce (str (UUID/randomUUID))
            canonical (canonicalize-request request include-headers)
            string-to-sign (str timestamp "\n" nonce "\n" canonical)
            algorithm (:algorithm key-config)
            signature (cond
                        (#{:hmac-sha256 :hmac-sha384 :hmac-sha512} algorithm)
                        (hmac-sign string-to-sign (:secret key-config) algorithm)
                        
                        (#{:rsa-sha256 :rsa-sha384 :rsa-sha512} algorithm)
                        (rsa-sign string-to-sign (:private-key key-config) algorithm)
                        
                        :else
                        (throw (ex-info "Unsupported algorithm" {:algorithm algorithm})))]
        
        (swap! state update-in [:stats :requests-signed] inc)
        
        (-> request
            (assoc-in [:headers "X-Signature"] signature)
            (assoc-in [:headers "X-Timestamp"] timestamp)
            (assoc-in [:headers "X-Nonce"] nonce)
            (assoc-in [:headers "X-Key-Id"] (name key-id))
            (assoc-in [:headers "X-Algorithm"] (name algorithm))))
      (throw (ex-info "Key is disabled" {:key-id key-id})))
    (throw (ex-info "Key not found" {:key-id key-id}))))

;; ============================================================================
;; Signature Verification
;; ============================================================================

(defn- check-timestamp
  "Check if timestamp is within tolerance."
  [timestamp-str]
  (let [timestamp (Long/parseLong timestamp-str)
        now (System/currentTimeMillis)
        tolerance (get-in @state [:config :timestamp-tolerance-ms])]
    (< (Math/abs (- now timestamp)) tolerance)))

(defn- check-nonce
  "Check if nonce has been used (replay attack prevention)."
  [nonce]
  (if (contains? (:nonces @state) nonce)
    false
    (do
      (swap! state update :nonces conj nonce)
      true)))

(defn verify-request
  "Verify a signed request."
  [request]
  (let [signature (get-in request [:headers "X-Signature"])
        timestamp (get-in request [:headers "X-Timestamp"])
        nonce (get-in request [:headers "X-Nonce"])
        key-id (keyword (get-in request [:headers "X-Key-Id"]))
        algorithm (keyword (get-in request [:headers "X-Algorithm"]))]
    
    (cond
      (not (and signature timestamp nonce key-id))
      {:valid? false :reason :missing-headers}
      
      (not (check-timestamp timestamp))
      (do
        (swap! state update-in [:stats :expired-timestamps] inc)
        {:valid? false :reason :expired-timestamp})
      
      (not (check-nonce nonce))
      (do
        (swap! state update-in [:stats :replay-attacks-blocked] inc)
        {:valid? false :reason :replay-attack})
      
      :else
      (if-let [key-config (get-key key-id)]
        (let [include-headers (get-in @state [:config :include-headers])
              canonical (canonicalize-request request include-headers)
              string-to-sign (str timestamp "\n" nonce "\n" canonical)
              valid? (cond
                       (#{:hmac-sha256 :hmac-sha384 :hmac-sha512} algorithm)
                       (hmac-verify string-to-sign (:secret key-config) algorithm signature)
                       
                       (#{:rsa-sha256 :rsa-sha384 :rsa-sha512} algorithm)
                       (rsa-verify string-to-sign (:public-key key-config) algorithm signature)
                       
                       :else false)]
          (if valid?
            (do
              (swap! state update-in [:stats :signatures-verified] inc)
              {:valid? true})
            (do
              (swap! state update-in [:stats :verification-failures] inc)
              {:valid? false :reason :invalid-signature})))
        {:valid? false :reason :unknown-key}))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-sign-request
  "Ring middleware to sign outgoing requests."
  [handler key-id]
  (fn [request]
    (let [signed-request (sign-request request key-id)]
      (handler signed-request))))

(defn wrap-verify-signature
  "Ring middleware to verify incoming request signatures."
  [handler]
  (fn [request]
    (let [result (verify-request request)]
      (if (:valid? result)
        (handler (assoc request :signature-verified? true))
        {:status 401
         :body {:error "Invalid signature"
                :reason (:reason result)}}))))

(defn wrap-verify-signature-optional
  "Ring middleware to verify signatures if present."
  [handler]
  (fn [request]
    (if (get-in request [:headers "X-Signature"])
      (let [result (verify-request request)]
        (if (:valid? result)
          (handler (assoc request :signature-verified? true))
          {:status 401
           :body {:error "Invalid signature"}}))
      (handler request))))

;; ============================================================================
;; Nonce Cleanup
;; ============================================================================

(defn- cleanup-nonces!
  "Clean up expired nonces."
  []
  (let [expiry (get-in @state [:config :nonce-expiry-ms])
        cutoff (- (System/currentTimeMillis) expiry)]
    ;; In a real implementation, nonces would have timestamps
    ;; For simplicity, we just clear old nonces periodically
    (when (> (count (:nonces @state)) 10000)
      (swap! state assoc :nonces #{}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-timestamp-tolerance!
  "Set the timestamp tolerance."
  [tolerance-ms]
  (swap! state assoc-in [:config :timestamp-tolerance-ms] tolerance-ms))

(defn set-include-headers!
  "Set the headers to include in signing."
  [headers]
  (swap! state assoc-in [:config :include-headers] headers))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-signer-metrics
  "Get signer metrics."
  []
  (let [stats (:stats @state)]
    {:requests-signed (:requests-signed stats)
     :signatures-verified (:signatures-verified stats)
     :verification-failures (:verification-failures stats)
     :expired-timestamps (:expired-timestamps stats)
     :replay-attacks-blocked (:replay-attacks-blocked stats)
     :keys-count (count (:keys @state))
     :nonces-count (count (:nonces @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-signer-stats
  "Get signer statistics."
  []
  (merge (get-signer-metrics)
         {:timestamp-tolerance-ms (get-in @state [:config :timestamp-tolerance-ms])
          :include-headers (get-in @state [:config :include-headers])}))

(defn reset-stats!
  "Reset signer statistics."
  []
  (swap! state assoc :stats {:requests-signed 0
                             :signatures-verified 0
                             :verification-failures 0
                             :expired-timestamps 0
                             :replay-attacks-blocked 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-signer!
  "Initialize the request signer."
  []
  (when-not (:initialized? @state)
    ;; Start nonce cleanup task
    (go-loop []
      (when (:initialized? @state)
        (<! (timeout 60000))
        (cleanup-nonces!)
        (recur)))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request signer initialized")
    (events/emit! :request-signer-initialized {})
    true))
