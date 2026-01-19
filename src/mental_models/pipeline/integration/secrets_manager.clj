(ns mental-models.pipeline.integration.secrets-manager
  "Secrets Manager Module
   
   Secure credential management:
   - Secret storage and retrieval
   - Encryption at rest
   - Secret rotation
   - Access control
   - Audit logging"
  (:require
   [clojure.string :as str]
   [mental-models.features.flags :as flags]
   [mental-models.metrics.aggregation :as metrics]
   [mental-models.events.bus :as events]
   [mental-models.logging.structured :as log])
  (:import
   [javax.crypto Cipher KeyGenerator SecretKey]
   [javax.crypto.spec SecretKeySpec IvParameterSpec]
   [java.security SecureRandom MessageDigest]
   [java.util Base64]
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong]))

;; =============================================================================
;; SECRETS MANAGER STATE
;; =============================================================================

(defonce secrets-state (atom {:secrets (ConcurrentHashMap.)
                              :versions (ConcurrentHashMap.)
                              :access-policies {}
                              :audit-log []
                              :encryption-key nil
                              :config {:max-versions 10
                                       :rotation-interval-ms 86400000
                                       :max-audit-entries 10000}}))

;; =============================================================================
;; ENCRYPTION
;; =============================================================================

(defn generate-key
  "Generate a new AES encryption key."
  []
  (let [key-gen (KeyGenerator/getInstance "AES")]
    (.init key-gen 256)
    (.generateKey key-gen)))

(defn key-to-bytes
  "Convert a SecretKey to bytes."
  [^SecretKey key]
  (.getEncoded key))

(defn bytes-to-key
  "Convert bytes to a SecretKey."
  [^bytes key-bytes]
  (SecretKeySpec. key-bytes "AES"))

(defn generate-iv
  "Generate a random initialization vector."
  []
  (let [iv (byte-array 16)
        random (SecureRandom.)]
    (.nextBytes random iv)
    iv))

(defn encrypt
  "Encrypt data using AES-256-CBC."
  [^String plaintext ^SecretKey key]
  (let [cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")
        iv (generate-iv)]
    (.init cipher Cipher/ENCRYPT_MODE key (IvParameterSpec. iv))
    (let [encrypted (.doFinal cipher (.getBytes plaintext "UTF-8"))
          combined (byte-array (+ 16 (alength encrypted)))]
      (System/arraycopy iv 0 combined 0 16)
      (System/arraycopy encrypted 0 combined 16 (alength encrypted))
      (.encodeToString (Base64/getEncoder) combined))))

(defn decrypt
  "Decrypt data using AES-256-CBC."
  [^String ciphertext ^SecretKey key]
  (let [combined (.decode (Base64/getDecoder) ciphertext)
        iv (byte-array 16)
        encrypted (byte-array (- (alength combined) 16))]
    (System/arraycopy combined 0 iv 0 16)
    (System/arraycopy combined 16 encrypted 0 (alength encrypted))
    (let [cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")]
      (.init cipher Cipher/DECRYPT_MODE key (IvParameterSpec. iv))
      (String. (.doFinal cipher encrypted) "UTF-8"))))

(defn hash-value
  "Hash a value using SHA-256."
  [^String value]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest (.getBytes value "UTF-8"))]
    (.encodeToString (Base64/getEncoder) hash-bytes)))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-encryption!
  "Initialize encryption with a key."
  [& {:keys [key]}]
  (let [encryption-key (or key (generate-key))]
    (swap! secrets-state assoc :encryption-key encryption-key)
    (log/info "Encryption initialized")))

(defn get-encryption-key
  "Get the current encryption key."
  []
  (:encryption-key @secrets-state))

;; =============================================================================
;; SECRET STORAGE
;; =============================================================================

(defn create-secret
  "Create a new secret."
  [secret-id {:keys [value description tags rotation-policy]}]
  (let [key (get-encryption-key)
        encrypted-value (when (and key value) (encrypt value key))]
    {:id secret-id
     :encrypted-value encrypted-value
     :value-hash (when value (hash-value value))
     :description description
     :tags (or tags #{})
     :rotation-policy rotation-policy
     :version 1
     :created-at (System/currentTimeMillis)
     :updated-at (System/currentTimeMillis)
     :last-accessed (AtomicLong. 0)
     :access-count (AtomicLong. 0)}))

(defn store-secret!
  "Store a secret."
  [secret-id value & {:keys [description tags rotation-policy]}]
  (log/info "Storing secret" {:id secret-id})
  (let [secret (create-secret secret-id {:value value
                                         :description description
                                         :tags tags
                                         :rotation-policy rotation-policy})]
    (.put ^ConcurrentHashMap (:secrets @secrets-state) secret-id secret)
    ;; Store version
    (.put ^ConcurrentHashMap (:versions @secrets-state) secret-id [{:version 1
                                                                     :encrypted-value (:encrypted-value secret)
                                                                     :created-at (:created-at secret)}])
    (metrics/inc-counter! :secretsmanager/secrets-stored)
    (audit-log! :store {:secret-id secret-id})
    (events/publish! :secretsmanager/secret-stored {:secret-id secret-id})
    secret-id))

(defn get-secret
  "Get a secret value."
  [secret-id & {:keys [version accessor]}]
  (when-let [secret (.get ^ConcurrentHashMap (:secrets @secrets-state) secret-id)]
    ;; Check access policy
    (when (or (nil? accessor) (can-access? secret-id accessor))
      (.set ^AtomicLong (:last-accessed secret) (System/currentTimeMillis))
      (.incrementAndGet ^AtomicLong (:access-count secret))
      (metrics/inc-counter! :secretsmanager/secrets-accessed)
      (audit-log! :access {:secret-id secret-id :accessor accessor})
      (let [key (get-encryption-key)
            encrypted (if version
                        (let [versions (.get ^ConcurrentHashMap (:versions @secrets-state) secret-id)
                              ver (first (filter #(= (:version %) version) versions))]
                          (:encrypted-value ver))
                        (:encrypted-value secret))]
        (when (and key encrypted)
          (decrypt encrypted key))))))

(defn update-secret!
  "Update a secret value."
  [secret-id new-value & {:keys [description]}]
  (when-let [secret (.get ^ConcurrentHashMap (:secrets @secrets-state) secret-id)]
    (log/info "Updating secret" {:id secret-id})
    (let [key (get-encryption-key)
          encrypted-value (when (and key new-value) (encrypt new-value key))
          new-version (inc (:version secret))
          updated-secret (assoc secret
                                :encrypted-value encrypted-value
                                :value-hash (when new-value (hash-value new-value))
                                :version new-version
                                :updated-at (System/currentTimeMillis)
                                :description (or description (:description secret)))]
      (.put ^ConcurrentHashMap (:secrets @secrets-state) secret-id updated-secret)
      ;; Add version
      (let [versions (.get ^ConcurrentHashMap (:versions @secrets-state) secret-id)
            max-versions (get-in @secrets-state [:config :max-versions])
            new-versions (conj versions {:version new-version
                                         :encrypted-value encrypted-value
                                         :created-at (System/currentTimeMillis)})
            trimmed-versions (if (> (count new-versions) max-versions)
                               (vec (drop (- (count new-versions) max-versions) new-versions))
                               new-versions)]
        (.put ^ConcurrentHashMap (:versions @secrets-state) secret-id trimmed-versions))
      (metrics/inc-counter! :secretsmanager/secrets-updated)
      (audit-log! :update {:secret-id secret-id :version new-version})
      (events/publish! :secretsmanager/secret-updated {:secret-id secret-id :version new-version})
      new-version)))

(defn delete-secret!
  "Delete a secret."
  [secret-id]
  (log/info "Deleting secret" {:id secret-id})
  (.remove ^ConcurrentHashMap (:secrets @secrets-state) secret-id)
  (.remove ^ConcurrentHashMap (:versions @secrets-state) secret-id)
  (metrics/inc-counter! :secretsmanager/secrets-deleted)
  (audit-log! :delete {:secret-id secret-id})
  (events/publish! :secretsmanager/secret-deleted {:secret-id secret-id}))

(defn list-secrets
  "List all secrets (metadata only)."
  [& {:keys [tag]}]
  (let [secrets (vals (:secrets @secrets-state))]
    (cond->> secrets
      tag (filter #(contains? (:tags %) tag))
      true (map #(dissoc % :encrypted-value)))))

(defn secret-exists?
  "Check if a secret exists."
  [secret-id]
  (.containsKey ^ConcurrentHashMap (:secrets @secrets-state) secret-id))

;; =============================================================================
;; SECRET ROTATION
;; =============================================================================

(defn rotate-secret!
  "Rotate a secret with a new value."
  [secret-id new-value]
  (log/info "Rotating secret" {:id secret-id})
  (let [version (update-secret! secret-id new-value)]
    (events/publish! :secretsmanager/secret-rotated {:secret-id secret-id :version version})
    version))

(defn get-secret-versions
  "Get all versions of a secret."
  [secret-id]
  (.get ^ConcurrentHashMap (:versions @secrets-state) secret-id))

(defn rollback-secret!
  "Rollback a secret to a previous version."
  [secret-id target-version]
  (when-let [versions (get-secret-versions secret-id)]
    (when-let [target (first (filter #(= (:version %) target-version) versions))]
      (log/info "Rolling back secret" {:id secret-id :version target-version})
      (when-let [secret (.get ^ConcurrentHashMap (:secrets @secrets-state) secret-id)]
        (let [new-version (inc (:version secret))
              updated-secret (assoc secret
                                    :encrypted-value (:encrypted-value target)
                                    :version new-version
                                    :updated-at (System/currentTimeMillis))]
          (.put ^ConcurrentHashMap (:secrets @secrets-state) secret-id updated-secret)
          (audit-log! :rollback {:secret-id secret-id :from-version (:version secret) :to-version target-version})
          new-version)))))

;; =============================================================================
;; ACCESS CONTROL
;; =============================================================================

(defn set-access-policy!
  "Set access policy for a secret."
  [secret-id policy]
  (log/info "Setting access policy" {:secret secret-id})
  (swap! secrets-state assoc-in [:access-policies secret-id] policy))

(defn get-access-policy
  "Get access policy for a secret."
  [secret-id]
  (get-in @secrets-state [:access-policies secret-id]))

(defn can-access?
  "Check if an accessor can access a secret."
  [secret-id accessor]
  (if-let [policy (get-access-policy secret-id)]
    (let [{:keys [allowed-users allowed-roles]} policy]
      (or (contains? (set allowed-users) accessor)
          (some #(contains? (set allowed-roles) %) (:roles accessor))))
    true))

(defn grant-access!
  "Grant access to a secret."
  [secret-id accessor]
  (log/info "Granting access" {:secret secret-id :accessor accessor})
  (swap! secrets-state update-in [:access-policies secret-id :allowed-users]
         (fnil conj #{}) accessor)
  (audit-log! :grant-access {:secret-id secret-id :accessor accessor}))

(defn revoke-access!
  "Revoke access to a secret."
  [secret-id accessor]
  (log/info "Revoking access" {:secret secret-id :accessor accessor})
  (swap! secrets-state update-in [:access-policies secret-id :allowed-users]
         (fnil disj #{}) accessor)
  (audit-log! :revoke-access {:secret-id secret-id :accessor accessor}))

;; =============================================================================
;; AUDIT LOGGING
;; =============================================================================

(defn audit-log!
  "Add an entry to the audit log."
  [action details]
  (let [entry {:timestamp (System/currentTimeMillis)
               :action action
               :details details}
        max-entries (get-in @secrets-state [:config :max-audit-entries])]
    (swap! secrets-state update :audit-log
           (fn [log]
             (let [new-log (conj log entry)]
               (if (> (count new-log) max-entries)
                 (vec (drop (- (count new-log) max-entries) new-log))
                 new-log))))))

(defn get-audit-log
  "Get the audit log."
  [& {:keys [secret-id action limit since]}]
  (let [log (:audit-log @secrets-state)]
    (cond->> log
      secret-id (filter #(= (get-in % [:details :secret-id]) secret-id))
      action (filter #(= (:action %) action))
      since (filter #(>= (:timestamp %) since))
      limit (take-last limit))))

(defn clear-audit-log!
  "Clear the audit log."
  []
  (swap! secrets-state assoc :audit-log []))

;; =============================================================================
;; ENVIRONMENT VARIABLES
;; =============================================================================

(defn load-from-env!
  "Load secrets from environment variables."
  [prefix]
  (log/info "Loading secrets from environment" {:prefix prefix})
  (doseq [[k v] (System/getenv)]
    (when (str/starts-with? k prefix)
      (let [secret-id (keyword (str/lower-case (subs k (count prefix))))]
        (store-secret! secret-id v :description (str "Loaded from env: " k))))))

(defn export-to-env
  "Export secrets as environment variable map."
  [secret-ids & {:keys [prefix]}]
  (into {} (for [secret-id secret-ids
                 :let [value (get-secret secret-id)]
                 :when value]
             [(str (or prefix "") (str/upper-case (name secret-id))) value])))

;; =============================================================================
;; STATISTICS
;; =============================================================================

(defn get-secret-stats
  "Get statistics for a secret."
  [secret-id]
  (when-let [secret (.get ^ConcurrentHashMap (:secrets @secrets-state) secret-id)]
    {:id secret-id
     :description (:description secret)
     :tags (:tags secret)
     :version (:version secret)
     :created-at (:created-at secret)
     :updated-at (:updated-at secret)
     :last-accessed (.get ^AtomicLong (:last-accessed secret))
     :access-count (.get ^AtomicLong (:access-count secret))
     :has-rotation-policy (some? (:rotation-policy secret))}))

(defn get-all-secret-stats
  "Get statistics for all secrets."
  []
  (into {} (for [secret-id (keys (:secrets @secrets-state))]
             [secret-id (get-secret-stats secret-id)])))

;; =============================================================================
;; INITIALIZATION
;; =============================================================================

(defn init-secrets-manager!
  "Initialize secrets manager."
  []
  (log/info "Initializing secrets manager")
  ;; Register feature flag
  (flags/register-flag! "secrets-manager" "Enable secrets manager" true)
  ;; Initialize encryption
  (init-encryption!)
  ;; Create metrics
  (metrics/create-counter! :secretsmanager/secrets-stored "Secrets stored")
  (metrics/create-counter! :secretsmanager/secrets-accessed "Secrets accessed")
  (metrics/create-counter! :secretsmanager/secrets-updated "Secrets updated")
  (metrics/create-counter! :secretsmanager/secrets-deleted "Secrets deleted")
  (metrics/create-gauge! :secretsmanager/total-secrets "Total secrets"
                         #(.size ^ConcurrentHashMap (:secrets @secrets-state)))
  (log/info "Secrets manager initialized"))

;; =============================================================================
;; STATUS
;; =============================================================================

(defn get-secrets-manager-status []
  {:enabled (flags/is-enabled? "secrets-manager")
   :secrets (.size ^ConcurrentHashMap (:secrets @secrets-state))
   :encryption-initialized (some? (:encryption-key @secrets-state))
   :access-policies (count (:access-policies @secrets-state))
   :audit-log-size (count (:audit-log @secrets-state))
   :stats (get-all-secret-stats)
   :config (:config @secrets-state)})
