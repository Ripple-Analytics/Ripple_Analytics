(ns mental-models.pipeline.integration.response-encryptor
  "Response encryptor for mental model analysis system.
   
   Features:
   - Response encryption
   - Field-level encryption
   - Key management
   - Encryption algorithms
   - Decryption support
   - Key rotation
   - Encryption policies
   - Encryption metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Base64]
           [java.security SecureRandom KeyPairGenerator]
           [javax.crypto Cipher KeyGenerator SecretKey]
           [javax.crypto.spec SecretKeySpec IvParameterSpec GCMParameterSpec]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:keys {}             ;; key-id -> encryption key
         :policies {}         ;; policy-id -> encryption policy
         :config {:default-algorithm :aes-256-gcm
                  :key-rotation-interval-ms 86400000
                  :iv-length 12
                  :tag-length 128}
         :stats {:encryptions 0
                 :decryptions 0
                 :key-rotations 0
                 :fields-encrypted 0}
         :secure-random nil
         :initialized? false}))

;; ============================================================================
;; Secure Random
;; ============================================================================

(defn- get-secure-random
  "Get the secure random instance."
  []
  (or (:secure-random @state)
      (SecureRandom.)))

(defn generate-random-bytes
  "Generate random bytes."
  [length]
  (let [bytes (byte-array length)]
    (.nextBytes (get-secure-random) bytes)
    bytes))

;; ============================================================================
;; Key Management
;; ============================================================================

(defn generate-key!
  "Generate a new encryption key."
  [key-id & {:keys [algorithm key-size]
             :or {algorithm "AES" key-size 256}}]
  (let [key-gen (KeyGenerator/getInstance algorithm)
        _ (.init key-gen key-size (get-secure-random))
        secret-key (.generateKey key-gen)
        key-bytes (.getEncoded secret-key)
        key-config {:id key-id
                    :algorithm algorithm
                    :key-size key-size
                    :key-bytes key-bytes
                    :created-at (System/currentTimeMillis)
                    :rotated-at (atom (System/currentTimeMillis))
                    :enabled? (atom true)
                    :version (atom 1)}]
    
    (swap! state assoc-in [:keys key-id] key-config)
    (logging/log :info "Generated encryption key" {:key-id key-id})
    key-id))

(defn get-key
  "Get an encryption key."
  [key-id]
  (get-in @state [:keys key-id]))

(defn list-keys
  "List all encryption keys."
  []
  (mapv (fn [[id k]]
          {:id id
           :algorithm (:algorithm k)
           :key-size (:key-size k)
           :version @(:version k)
           :enabled? @(:enabled? k)
           :created-at (:created-at k)})
        (:keys @state)))

(defn rotate-key!
  "Rotate an encryption key."
  [key-id]
  (when-let [key-config (get-key key-id)]
    (let [key-gen (KeyGenerator/getInstance (:algorithm key-config))
          _ (.init key-gen (:key-size key-config) (get-secure-random))
          new-key (.generateKey key-gen)]
      (swap! state assoc-in [:keys key-id :key-bytes] (.getEncoded new-key))
      (reset! (:rotated-at key-config) (System/currentTimeMillis))
      (swap! (:version key-config) inc)
      (swap! state update-in [:stats :key-rotations] inc)
      (logging/log :info "Rotated encryption key" {:key-id key-id})
      true)))

(defn disable-key!
  "Disable an encryption key."
  [key-id]
  (when-let [key-config (get-key key-id)]
    (reset! (:enabled? key-config) false)))

(defn delete-key!
  "Delete an encryption key."
  [key-id]
  (swap! state update :keys dissoc key-id))

;; ============================================================================
;; Encryption Algorithms
;; ============================================================================

(defn- get-cipher
  "Get a cipher instance."
  [algorithm mode key-bytes iv]
  (let [cipher (case algorithm
                 :aes-256-gcm (Cipher/getInstance "AES/GCM/NoPadding")
                 :aes-256-cbc (Cipher/getInstance "AES/CBC/PKCS5Padding")
                 :aes-128-gcm (Cipher/getInstance "AES/GCM/NoPadding")
                 (Cipher/getInstance "AES/GCM/NoPadding"))
        secret-key (SecretKeySpec. key-bytes "AES")
        param-spec (case algorithm
                     (:aes-256-gcm :aes-128-gcm)
                     (GCMParameterSpec. (get-in @state [:config :tag-length]) iv)
                     (IvParameterSpec. iv))]
    (.init cipher mode secret-key param-spec)
    cipher))

(defn encrypt-bytes
  "Encrypt bytes."
  [data key-id]
  (when-let [key-config (get-key key-id)]
    (when @(:enabled? key-config)
      (swap! state update-in [:stats :encryptions] inc)
      
      (let [iv (generate-random-bytes (get-in @state [:config :iv-length]))
            cipher (get-cipher (get-in @state [:config :default-algorithm])
                               Cipher/ENCRYPT_MODE
                               (:key-bytes key-config)
                               iv)
            encrypted (.doFinal cipher data)]
        {:iv (.encodeToString (Base64/getEncoder) iv)
         :data (.encodeToString (Base64/getEncoder) encrypted)
         :key-id key-id
         :version @(:version key-config)
         :algorithm (get-in @state [:config :default-algorithm])}))))

(defn decrypt-bytes
  "Decrypt bytes."
  [{:keys [iv data key-id version algorithm]}]
  (when-let [key-config (get-key key-id)]
    (swap! state update-in [:stats :decryptions] inc)
    
    (let [iv-bytes (.decode (Base64/getDecoder) iv)
          encrypted-bytes (.decode (Base64/getDecoder) data)
          cipher (get-cipher (or algorithm (get-in @state [:config :default-algorithm]))
                             Cipher/DECRYPT_MODE
                             (:key-bytes key-config)
                             iv-bytes)]
      (.doFinal cipher encrypted-bytes))))

(defn encrypt-string
  "Encrypt a string."
  [s key-id]
  (encrypt-bytes (.getBytes s "UTF-8") key-id))

(defn decrypt-string
  "Decrypt to a string."
  [encrypted]
  (String. (decrypt-bytes encrypted) "UTF-8"))

;; ============================================================================
;; Field-Level Encryption
;; ============================================================================

(defn encrypt-field
  "Encrypt a single field value."
  [value key-id]
  (swap! state update-in [:stats :fields-encrypted] inc)
  (let [value-str (if (string? value) value (pr-str value))]
    (encrypt-string value-str key-id)))

(defn decrypt-field
  "Decrypt a single field value."
  [encrypted]
  (let [decrypted (decrypt-string encrypted)]
    (try
      (edn/read-string decrypted)
      (catch Exception _
        decrypted))))

(defn encrypt-fields
  "Encrypt specific fields in a map."
  [data fields key-id]
  (reduce (fn [result field]
            (if (contains? result field)
              (update result field #(encrypt-field % key-id))
              result))
          data
          fields))

(defn decrypt-fields
  "Decrypt specific fields in a map."
  [data fields]
  (reduce (fn [result field]
            (if (and (contains? result field)
                     (map? (get result field))
                     (:iv (get result field)))
              (update result field decrypt-field)
              result))
          data
          fields))

;; ============================================================================
;; Encryption Policies
;; ============================================================================

(defn register-policy!
  "Register an encryption policy."
  [policy-id config]
  (let [policy {:id policy-id
                :name (get config :name (name policy-id))
                :fields (get config :fields [])
                :key-id (get config :key-id)
                :condition-fn (get config :condition-fn (constantly true))
                :enabled? (atom true)
                :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:policies policy-id] policy)
    policy-id))

(defn get-policy
  "Get an encryption policy."
  [policy-id]
  (get-in @state [:policies policy-id]))

(defn list-policies
  "List all policies."
  []
  (mapv (fn [[id p]]
          {:id id
           :name (:name p)
           :fields (:fields p)
           :key-id (:key-id p)
           :enabled? @(:enabled? p)})
        (:policies @state)))

(defn apply-policy
  "Apply an encryption policy to data."
  [data policy-id]
  (when-let [policy (get-policy policy-id)]
    (when (and @(:enabled? policy)
               ((:condition-fn policy) data))
      (encrypt-fields data (:fields policy) (:key-id policy)))))

;; ============================================================================
;; Response Encryption
;; ============================================================================

(defn encrypt-response
  "Encrypt a response body."
  [response key-id]
  (let [body (:body response)]
    (if (map? body)
      (let [encrypted (encrypt-string (pr-str body) key-id)]
        (assoc response
               :body encrypted
               :headers (assoc (:headers response)
                               "X-Encrypted" "true"
                               "X-Encryption-Key-Id" (str key-id))))
      response)))

(defn decrypt-response
  "Decrypt a response body."
  [response]
  (if (= (get-in response [:headers "X-Encrypted"]) "true")
    (let [decrypted (decrypt-string (:body response))]
      (assoc response :body (edn/read-string decrypted)))
    response))

(defn encrypt-response-fields
  "Encrypt specific fields in response."
  [response fields key-id]
  (if (map? (:body response))
    (update response :body #(encrypt-fields % fields key-id))
    response))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-encrypt
  "Ring middleware to encrypt responses."
  [handler key-id]
  (fn [request]
    (let [response (handler request)]
      (encrypt-response response key-id))))

(defn wrap-encrypt-fields
  "Ring middleware to encrypt specific fields."
  [handler fields key-id]
  (fn [request]
    (let [response (handler request)]
      (encrypt-response-fields response fields key-id))))

(defn wrap-decrypt-request
  "Ring middleware to decrypt request body."
  [handler]
  (fn [request]
    (let [decrypted-request (if (and (map? (:body request))
                                     (:iv (:body request)))
                              (update request :body decrypt-field)
                              request)]
      (handler decrypted-request))))

(defn wrap-policy
  "Ring middleware to apply encryption policy."
  [handler policy-id]
  (fn [request]
    (let [response (handler request)]
      (if-let [encrypted-body (apply-policy (:body response) policy-id)]
        (assoc response :body encrypted-body)
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-algorithm!
  "Set default encryption algorithm."
  [algorithm]
  (swap! state assoc-in [:config :default-algorithm] algorithm))

(defn set-key-rotation-interval!
  "Set key rotation interval."
  [interval-ms]
  (swap! state assoc-in [:config :key-rotation-interval-ms] interval-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-encryptor-metrics
  "Get encryptor metrics."
  []
  (let [stats (:stats @state)]
    {:encryptions (:encryptions stats)
     :decryptions (:decryptions stats)
     :key-rotations (:key-rotations stats)
     :fields-encrypted (:fields-encrypted stats)
     :keys-count (count (:keys @state))
     :policies-count (count (:policies @state))
     :active-keys (count (filter (fn [[_ k]] @(:enabled? k)) (:keys @state)))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-encryptor-stats
  "Get encryptor statistics."
  []
  (merge (get-encryptor-metrics)
         {:default-algorithm (get-in @state [:config :default-algorithm])
          :key-rotation-interval-ms (get-in @state [:config :key-rotation-interval-ms])}))

(defn reset-stats!
  "Reset encryptor statistics."
  []
  (swap! state assoc :stats {:encryptions 0
                             :decryptions 0
                             :key-rotations 0
                             :fields-encrypted 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-encryptor!
  "Initialize the response encryptor."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :secure-random (SecureRandom.))
    (swap! state assoc :initialized? true)
    (logging/log :info "Response encryptor initialized")
    (events/emit! :response-encryptor-initialized {})
    true))
