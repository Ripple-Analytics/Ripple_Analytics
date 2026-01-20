(ns mental-models.connectors.section-85
  "Connectors Module - Section 85"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def signing-config
  "Configuration for request signing."
  (atom {:enabled true
         :algorithm :hmac-sha256
         :timestamp-tolerance-ms 300000
         :include-headers [:host :date :content-type]
         :signature-header "X-Signature"}))

(defn get-signing-config
  "Get current signing configuration."
  []
  @signing-config)

(defn set-signing-config
  "Update signing configuration."
  [config]
  (swap! signing-config merge config))

(def signing-keys
  "Registry of signing keys per connector."
  (atom {}))

(defn register-signing-key
  "Register a signing key for a connector."
  [connector-type key-id secret]
  (swap! signing-keys assoc-in [connector-type key-id]
         {:key-id key-id
          :secret secret
          :registered-at (System/currentTimeMillis)}))

(defn get-signing-key
  "Get a signing key for a connector."
  [connector-type key-id]
  (get-in @signing-keys [connector-type key-id :secret]))

#?(:clj
   (defn compute-signature
     "Compute HMAC signature for request."
     [secret string-to-sign algorithm]
     (let [mac (javax.crypto.Mac/getInstance
                (case algorithm
                  :hmac-sha256 "HmacSHA256"
                  :hmac-sha512 "HmacSHA512"
                  "HmacSHA256"))
           secret-key (javax.crypto.spec.SecretKeySpec.
                       (.getBytes secret "UTF-8")
                       (.getAlgorithm mac))]
       (.init mac secret-key)
       (let [signature (.doFinal mac (.getBytes string-to-sign "UTF-8"))]
         (.encodeToString (java.util.Base64/getEncoder) signature)))))

(defn create-string-to-sign
  "Create the canonical string to sign."
  [method path headers timestamp]
  (let [config @signing-config
        header-values (map #(get headers % "") (:include-headers config))]
    (str method "\n"
         path "\n"
         timestamp "\n"
         (clojure.string/join "\n" header-values))))

#?(:clj
   (defn sign-request
     "Sign a request with the connector's signing key."
     [connector-type key-id method path headers body]
     (if-not (:enabled @signing-config)
       {:signed false}
       (if-let [secret (get-signing-key connector-type key-id)]
         (let [timestamp (System/currentTimeMillis)
               string-to-sign (create-string-to-sign method path headers timestamp)
               signature (compute-signature secret string-to-sign (:algorithm @signing-config))]
           {:signed true
            :signature signature
            :timestamp timestamp
            :algorithm (:algorithm @signing-config)
            :key-id key-id})
         {:signed false :error "Signing key not found"}))))

#?(:clj
   (defn verify-signature
     "Verify a request signature."
     [connector-type key-id method path headers timestamp provided-signature]
     (if-not (:enabled @signing-config)
       {:valid true :reason "Signing disabled"}
       (let [config @signing-config
             now (System/currentTimeMillis)]
         (cond
           (> (Math/abs (- now timestamp)) (:timestamp-tolerance-ms config))
           {:valid false :reason "Timestamp outside tolerance"}
           
           :else
           (if-let [secret (get-signing-key connector-type key-id)]
             (let [string-to-sign (create-string-to-sign method path headers timestamp)
                   expected-signature (compute-signature secret string-to-sign (:algorithm config))]
               (if (= expected-signature provided-signature)
                 {:valid true}
                 {:valid false :reason "Signature mismatch"}))
             {:valid false :reason "Signing key not found"}))))))

(defn get-signing-stats
  "Get statistics about request signing."
  []
  {:connectors-with-keys (count @signing-keys)
   :total-keys (reduce + (map count (vals @signing-keys)))
   :config @signing-config})

;; ============================================
;; Request Throttling
