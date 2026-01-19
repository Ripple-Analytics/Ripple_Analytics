(ns mental-models.pipeline.integration.encryption
  "Encryption and security utilities for mental model analysis.
   
   Features:
   - Symmetric encryption (AES)
   - Asymmetric encryption (RSA)
   - Key management
   - Hashing and HMAC
   - Digital signatures
   - Secure random generation
   - Data masking
   - Key rotation"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Base64]
           [java.security SecureRandom KeyPairGenerator KeyPair MessageDigest Signature]
           [javax.crypto Cipher KeyGenerator SecretKey Mac]
           [javax.crypto.spec SecretKeySpec IvParameterSpec GCMParameterSpec]
           [java.nio.charset StandardCharsets]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:keys {}             ;; key-id -> key-data
         :key-pairs {}        ;; key-pair-id -> key-pair
         :rotation-schedule {}  ;; key-id -> rotation-config
         :masking-rules {}    ;; rule-id -> masking-rule
         :stats {:encryptions 0 :decryptions 0 :signatures 0}
         :initialized? false}))

;; ============================================================================
;; Secure Random
;; ============================================================================

(def ^:private secure-random (SecureRandom.))

(defn generate-random-bytes
  "Generate secure random bytes."
  [length]
  (let [bytes (byte-array length)]
    (.nextBytes secure-random bytes)
    bytes))

(defn generate-random-string
  "Generate a secure random string."
  [length]
  (let [bytes (generate-random-bytes length)]
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn generate-uuid
  "Generate a secure UUID."
  []
  (str (UUID/randomUUID)))

;; ============================================================================
;; Key Management
;; ============================================================================

(defn generate-symmetric-key!
  "Generate a symmetric encryption key."
  [key-id & {:keys [algorithm key-size] :or {algorithm "AES" key-size 256}}]
  (let [key-gen (KeyGenerator/getInstance algorithm)
        _ (.init key-gen key-size secure-random)
        secret-key (.generateKey key-gen)
        key-data {:id key-id
                  :type :symmetric
                  :algorithm algorithm
                  :key-size key-size
                  :key secret-key
                  :created-at (System/currentTimeMillis)
                  :status :active}]
    (swap! state assoc-in [:keys key-id] key-data)
    (logging/log :info "Generated symmetric key" {:key-id key-id :algorithm algorithm})
    key-id))

(defn generate-key-pair!
  "Generate an asymmetric key pair."
  [key-pair-id & {:keys [algorithm key-size] :or {algorithm "RSA" key-size 2048}}]
  (let [key-gen (KeyPairGenerator/getInstance algorithm)
        _ (.initialize key-gen key-size secure-random)
        key-pair (.generateKeyPair key-gen)
        key-data {:id key-pair-id
                  :type :asymmetric
                  :algorithm algorithm
                  :key-size key-size
                  :public-key (.getPublic key-pair)
                  :private-key (.getPrivate key-pair)
                  :created-at (System/currentTimeMillis)
                  :status :active}]
    (swap! state assoc-in [:key-pairs key-pair-id] key-data)
    (logging/log :info "Generated key pair" {:key-pair-id key-pair-id :algorithm algorithm})
    key-pair-id))

(defn get-key
  "Get a symmetric key."
  [key-id]
  (get-in @state [:keys key-id]))

(defn get-key-pair
  "Get a key pair."
  [key-pair-id]
  (get-in @state [:key-pairs key-pair-id]))

(defn list-keys
  "List all keys."
  []
  (concat
   (mapv (fn [[id k]]
           {:id id
            :type (:type k)
            :algorithm (:algorithm k)
            :status (:status k)
            :created-at (:created-at k)})
         (:keys @state))
   (mapv (fn [[id k]]
           {:id id
            :type (:type k)
            :algorithm (:algorithm k)
            :status (:status k)
            :created-at (:created-at k)})
         (:key-pairs @state))))

(defn revoke-key!
  "Revoke a key."
  [key-id]
  (if (get-in @state [:keys key-id])
    (swap! state assoc-in [:keys key-id :status] :revoked)
    (swap! state assoc-in [:key-pairs key-id :status] :revoked))
  (logging/log :info "Revoked key" {:key-id key-id}))

(defn import-key!
  "Import a symmetric key from bytes."
  [key-id key-bytes & {:keys [algorithm] :or {algorithm "AES"}}]
  (let [secret-key (SecretKeySpec. key-bytes algorithm)
        key-data {:id key-id
                  :type :symmetric
                  :algorithm algorithm
                  :key-size (* 8 (count key-bytes))
                  :key secret-key
                  :created-at (System/currentTimeMillis)
                  :status :active
                  :imported? true}]
    (swap! state assoc-in [:keys key-id] key-data)
    key-id))

(defn export-key
  "Export a symmetric key as bytes."
  [key-id]
  (when-let [key-data (get-key key-id)]
    (.getEncoded (:key key-data))))

;; ============================================================================
;; Symmetric Encryption
;; ============================================================================

(defn encrypt
  "Encrypt data using AES-GCM."
  [key-id plaintext]
  (when (flags/enabled? :encryption)
    (when-let [key-data (get-key key-id)]
      (let [iv (generate-random-bytes 12)
            cipher (Cipher/getInstance "AES/GCM/NoPadding")
            spec (GCMParameterSpec. 128 iv)
            _ (.init cipher Cipher/ENCRYPT_MODE (:key key-data) spec)
            plaintext-bytes (if (string? plaintext)
                              (.getBytes plaintext StandardCharsets/UTF_8)
                              plaintext)
            ciphertext (.doFinal cipher plaintext-bytes)]
        (swap! state update-in [:stats :encryptions] inc)
        {:iv (.encodeToString (Base64/getEncoder) iv)
         :ciphertext (.encodeToString (Base64/getEncoder) ciphertext)
         :key-id key-id
         :algorithm "AES-GCM"}))))

(defn decrypt
  "Decrypt data using AES-GCM."
  [key-id encrypted-data]
  (when (flags/enabled? :encryption)
    (when-let [key-data (get-key key-id)]
      (let [iv (.decode (Base64/getDecoder) (:iv encrypted-data))
            ciphertext (.decode (Base64/getDecoder) (:ciphertext encrypted-data))
            cipher (Cipher/getInstance "AES/GCM/NoPadding")
            spec (GCMParameterSpec. 128 iv)
            _ (.init cipher Cipher/DECRYPT_MODE (:key key-data) spec)
            plaintext-bytes (.doFinal cipher ciphertext)]
        (swap! state update-in [:stats :decryptions] inc)
        (String. plaintext-bytes StandardCharsets/UTF_8)))))

;; ============================================================================
;; Asymmetric Encryption
;; ============================================================================

(defn encrypt-with-public-key
  "Encrypt data using RSA public key."
  [key-pair-id plaintext]
  (when-let [key-data (get-key-pair key-pair-id)]
    (let [cipher (Cipher/getInstance "RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
          _ (.init cipher Cipher/ENCRYPT_MODE (:public-key key-data))
          plaintext-bytes (if (string? plaintext)
                            (.getBytes plaintext StandardCharsets/UTF_8)
                            plaintext)
          ciphertext (.doFinal cipher plaintext-bytes)]
      (swap! state update-in [:stats :encryptions] inc)
      {:ciphertext (.encodeToString (Base64/getEncoder) ciphertext)
       :key-pair-id key-pair-id
       :algorithm "RSA-OAEP"})))

(defn decrypt-with-private-key
  "Decrypt data using RSA private key."
  [key-pair-id encrypted-data]
  (when-let [key-data (get-key-pair key-pair-id)]
    (let [ciphertext (.decode (Base64/getDecoder) (:ciphertext encrypted-data))
          cipher (Cipher/getInstance "RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
          _ (.init cipher Cipher/DECRYPT_MODE (:private-key key-data))
          plaintext-bytes (.doFinal cipher ciphertext)]
      (swap! state update-in [:stats :decryptions] inc)
      (String. plaintext-bytes StandardCharsets/UTF_8))))

;; ============================================================================
;; Hashing
;; ============================================================================

(defn hash-data
  "Hash data using SHA-256."
  [data & {:keys [algorithm] :or {algorithm "SHA-256"}}]
  (let [digest (MessageDigest/getInstance algorithm)
        data-bytes (if (string? data)
                     (.getBytes data StandardCharsets/UTF_8)
                     data)
        hash-bytes (.digest digest data-bytes)]
    (.encodeToString (Base64/getEncoder) hash-bytes)))

(defn verify-hash
  "Verify a hash."
  [data expected-hash & {:keys [algorithm] :or {algorithm "SHA-256"}}]
  (= (hash-data data :algorithm algorithm) expected-hash))

(defn hmac
  "Generate HMAC."
  [key-id data]
  (when-let [key-data (get-key key-id)]
    (let [mac (Mac/getInstance "HmacSHA256")
          _ (.init mac (:key key-data))
          data-bytes (if (string? data)
                       (.getBytes data StandardCharsets/UTF_8)
                       data)
          hmac-bytes (.doFinal mac data-bytes)]
      (.encodeToString (Base64/getEncoder) hmac-bytes))))

(defn verify-hmac
  "Verify HMAC."
  [key-id data expected-hmac]
  (= (hmac key-id data) expected-hmac))

;; ============================================================================
;; Digital Signatures
;; ============================================================================

(defn sign
  "Sign data using RSA private key."
  [key-pair-id data]
  (when-let [key-data (get-key-pair key-pair-id)]
    (let [signature (Signature/getInstance "SHA256withRSA")
          _ (.initSign signature (:private-key key-data))
          data-bytes (if (string? data)
                       (.getBytes data StandardCharsets/UTF_8)
                       data)
          _ (.update signature data-bytes)
          sig-bytes (.sign signature)]
      (swap! state update-in [:stats :signatures] inc)
      {:signature (.encodeToString (Base64/getEncoder) sig-bytes)
       :key-pair-id key-pair-id
       :algorithm "SHA256withRSA"})))

(defn verify-signature
  "Verify a digital signature."
  [key-pair-id data signature-data]
  (when-let [key-data (get-key-pair key-pair-id)]
    (let [signature (Signature/getInstance "SHA256withRSA")
          _ (.initVerify signature (:public-key key-data))
          data-bytes (if (string? data)
                       (.getBytes data StandardCharsets/UTF_8)
                       data)
          _ (.update signature data-bytes)
          sig-bytes (.decode (Base64/getDecoder) (:signature signature-data))]
      (.verify signature sig-bytes))))

;; ============================================================================
;; Data Masking
;; ============================================================================

(defn create-masking-rule!
  "Create a data masking rule."
  [rule-id config]
  (let [rule {:id rule-id
              :name (get config :name (name rule-id))
              :pattern (get config :pattern)
              :mask-char (get config :mask-char \*)
              :preserve-start (get config :preserve-start 0)
              :preserve-end (get config :preserve-end 0)
              :type (get config :type :partial)}]
    (swap! state assoc-in [:masking-rules rule-id] rule)
    rule-id))

(defn mask-data
  "Mask sensitive data."
  [rule-id data]
  (when-let [rule (get-in @state [:masking-rules rule-id])]
    (let [data-str (str data)
          len (count data-str)
          preserve-start (:preserve-start rule)
          preserve-end (:preserve-end rule)
          mask-char (:mask-char rule)]
      (case (:type rule)
        :full (apply str (repeat len mask-char))
        :partial (str (subs data-str 0 (min preserve-start len))
                      (apply str (repeat (max 0 (- len preserve-start preserve-end)) mask-char))
                      (subs data-str (max 0 (- len preserve-end))))
        :pattern (str/replace data-str (re-pattern (:pattern rule))
                              (apply str (repeat 4 mask-char)))
        data-str))))

(defn mask-fields
  "Mask specific fields in a map."
  [data field-rules]
  (reduce (fn [d [field rule-id]]
            (if (contains? d field)
              (update d field #(mask-data rule-id %))
              d))
          data
          field-rules))

;; ============================================================================
;; Key Rotation
;; ============================================================================

(defn schedule-key-rotation!
  "Schedule key rotation."
  [key-id config]
  (let [schedule {:key-id key-id
                  :interval-ms (get config :interval-ms 2592000000) ;; 30 days
                  :last-rotation (System/currentTimeMillis)
                  :next-rotation (+ (System/currentTimeMillis)
                                    (get config :interval-ms 2592000000))
                  :auto-rotate? (get config :auto-rotate? true)}]
    (swap! state assoc-in [:rotation-schedule key-id] schedule)
    (logging/log :info "Scheduled key rotation" {:key-id key-id})
    key-id))

(defn rotate-key!
  "Rotate a symmetric key."
  [key-id]
  (when-let [old-key (get-key key-id)]
    (let [new-key-id (keyword (str (name key-id) "-" (System/currentTimeMillis)))]
      ;; Generate new key
      (generate-symmetric-key! new-key-id
                               :algorithm (:algorithm old-key)
                               :key-size (:key-size old-key))
      ;; Mark old key as rotated
      (swap! state assoc-in [:keys key-id :status] :rotated)
      (swap! state assoc-in [:keys key-id :rotated-to] new-key-id)
      ;; Update rotation schedule
      (swap! state update-in [:rotation-schedule key-id]
             (fn [s]
               (assoc s
                      :last-rotation (System/currentTimeMillis)
                      :next-rotation (+ (System/currentTimeMillis)
                                        (:interval-ms s 2592000000)))))
      (logging/log :info "Rotated key" {:old-key-id key-id :new-key-id new-key-id})
      (events/emit! :key-rotated {:old-key-id key-id :new-key-id new-key-id})
      new-key-id)))

(defn check-rotation-due
  "Check if any keys are due for rotation."
  []
  (let [now (System/currentTimeMillis)
        due (filter (fn [[_ s]]
                      (and (:auto-rotate? s)
                           (> now (:next-rotation s))))
                    (:rotation-schedule @state))]
    (mapv first due)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-encryption-stats
  "Get encryption statistics."
  []
  (let [stats (:stats @state)]
    {:total-symmetric-keys (count (:keys @state))
     :total-key-pairs (count (:key-pairs @state))
     :total-masking-rules (count (:masking-rules @state))
     :encryptions (:encryptions stats)
     :decryptions (:decryptions stats)
     :signatures (:signatures stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-encryption!
  "Initialize encryption."
  []
  (when-not (:initialized? @state)
    ;; Generate default encryption key
    (generate-symmetric-key! :default-aes
                             :algorithm "AES"
                             :key-size 256)
    
    ;; Generate default signing key pair
    (generate-key-pair! :default-rsa
                        :algorithm "RSA"
                        :key-size 2048)
    
    ;; Create default masking rules
    (create-masking-rule! :email
                          {:name "Email Masking"
                           :type :partial
                           :preserve-start 2
                           :preserve-end 4})
    
    (create-masking-rule! :credit-card
                          {:name "Credit Card Masking"
                           :type :partial
                           :preserve-start 0
                           :preserve-end 4})
    
    (create-masking-rule! :ssn
                          {:name "SSN Masking"
                           :type :partial
                           :preserve-start 0
                           :preserve-end 4})
    
    ;; Schedule key rotation
    (schedule-key-rotation! :default-aes
                            {:interval-ms 2592000000 ;; 30 days
                             :auto-rotate? true})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Encryption initialized")
    (events/emit! :encryption-initialized {})
    true))
