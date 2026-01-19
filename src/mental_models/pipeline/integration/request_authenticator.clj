(ns mental-models.pipeline.integration.request-authenticator
  "Request authenticator for mental model analysis system.
   
   Features:
   - Authentication strategies
   - Token validation
   - API key authentication
   - OAuth support
   - JWT validation
   - Session management
   - Authentication caching
   - Authentication metrics"
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
           [java.security MessageDigest]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:strategies {}       ;; strategy-id -> authentication strategy
         :sessions {}         ;; session-id -> session data
         :api-keys {}         ;; api-key -> key config
         :tokens {}           ;; token -> token data (cache)
         :config {:default-strategy :api-key
                  :session-ttl-ms 3600000
                  :token-cache-ttl-ms 300000
                  :max-sessions 10000}
         :stats {:authentications 0
                 :successes 0
                 :failures 0
                 :cache-hits 0}
         :initialized? false}))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn- sha256
  "Generate SHA-256 hash."
  [s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hash-bytes (.digest digest (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) hash-bytes))))

(defn- hmac-sha256
  "Generate HMAC-SHA256."
  [secret message]
  (let [mac (Mac/getInstance "HmacSHA256")
        secret-key (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac secret-key)
    (let [hash-bytes (.doFinal mac (.getBytes message "UTF-8"))]
      (.encodeToString (Base64/getEncoder) hash-bytes))))

(defn- base64-decode
  "Decode base64 string."
  [s]
  (String. (.decode (Base64/getDecoder) s) "UTF-8"))

(defn- base64-encode
  "Encode string to base64."
  [s]
  (.encodeToString (Base64/getEncoder) (.getBytes s "UTF-8")))

;; ============================================================================
;; API Key Authentication
;; ============================================================================

(defn register-api-key!
  "Register an API key."
  [api-key config]
  (let [key-hash (sha256 api-key)
        key-config {:hash key-hash
                    :name (get config :name)
                    :scopes (get config :scopes #{})
                    :rate-limit (get config :rate-limit)
                    :expires-at (get config :expires-at)
                    :metadata (get config :metadata {})
                    :enabled? (atom true)
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:api-keys key-hash] key-config)
    (logging/log :info "Registered API key" {:name (:name key-config)})
    key-hash))

(defn validate-api-key
  "Validate an API key."
  [api-key]
  (let [key-hash (sha256 api-key)
        key-config (get-in @state [:api-keys key-hash])]
    (cond
      (nil? key-config)
      {:valid? false :error :invalid-key}
      
      (not @(:enabled? key-config))
      {:valid? false :error :key-disabled}
      
      (and (:expires-at key-config)
           (> (System/currentTimeMillis) (:expires-at key-config)))
      {:valid? false :error :key-expired}
      
      :else
      {:valid? true
       :name (:name key-config)
       :scopes (:scopes key-config)
       :metadata (:metadata key-config)})))

(defn revoke-api-key!
  "Revoke an API key."
  [api-key]
  (let [key-hash (sha256 api-key)]
    (when-let [key-config (get-in @state [:api-keys key-hash])]
      (reset! (:enabled? key-config) false))))

;; ============================================================================
;; JWT Validation
;; ============================================================================

(defn- parse-jwt
  "Parse a JWT token."
  [token]
  (try
    (let [parts (str/split token #"\.")
          header (-> (nth parts 0) base64-decode edn/read-string)
          payload (-> (nth parts 1) base64-decode edn/read-string)
          signature (nth parts 2)]
      {:header header
       :payload payload
       :signature signature
       :raw-parts parts})
    (catch Exception _
      nil)))

(defn validate-jwt
  "Validate a JWT token."
  [token secret]
  (if-let [parsed (parse-jwt token)]
    (let [{:keys [header payload raw-parts]} parsed
          signing-input (str (first raw-parts) "." (second raw-parts))
          expected-sig (hmac-sha256 secret signing-input)
          actual-sig (:signature parsed)]
      (cond
        (not= expected-sig actual-sig)
        {:valid? false :error :invalid-signature}
        
        (and (:exp payload)
             (> (System/currentTimeMillis) (* 1000 (:exp payload))))
        {:valid? false :error :token-expired}
        
        (and (:nbf payload)
             (< (System/currentTimeMillis) (* 1000 (:nbf payload))))
        {:valid? false :error :token-not-yet-valid}
        
        :else
        {:valid? true
         :claims payload
         :header header}))
    {:valid? false :error :invalid-token-format}))

(defn create-jwt
  "Create a JWT token."
  [claims secret & {:keys [expires-in-seconds] :or {expires-in-seconds 3600}}]
  (let [header {:alg "HS256" :typ "JWT"}
        now (quot (System/currentTimeMillis) 1000)
        payload (merge claims
                       {:iat now
                        :exp (+ now expires-in-seconds)})
        header-b64 (base64-encode (pr-str header))
        payload-b64 (base64-encode (pr-str payload))
        signing-input (str header-b64 "." payload-b64)
        signature (hmac-sha256 secret signing-input)]
    (str signing-input "." signature)))

;; ============================================================================
;; Session Management
;; ============================================================================

(defn create-session!
  "Create a new session."
  [user-id & {:keys [metadata]}]
  (let [session-id (str (UUID/randomUUID))
        session {:id session-id
                 :user-id user-id
                 :metadata (or metadata {})
                 :created-at (System/currentTimeMillis)
                 :last-accessed (atom (System/currentTimeMillis))
                 :expires-at (+ (System/currentTimeMillis)
                                (get-in @state [:config :session-ttl-ms]))}]
    
    ;; Enforce max sessions
    (let [max-sessions (get-in @state [:config :max-sessions])]
      (when (>= (count (:sessions @state)) max-sessions)
        ;; Remove oldest sessions
        (let [sorted-sessions (sort-by (fn [[_ s]] @(:last-accessed s))
                                       (:sessions @state))
              to-remove (take (- (count sorted-sessions) max-sessions -1)
                              (map first sorted-sessions))]
          (doseq [sid to-remove]
            (swap! state update :sessions dissoc sid)))))
    
    (swap! state assoc-in [:sessions session-id] session)
    session-id))

(defn get-session
  "Get a session."
  [session-id]
  (when-let [session (get-in @state [:sessions session-id])]
    (reset! (:last-accessed session) (System/currentTimeMillis))
    session))

(defn validate-session
  "Validate a session."
  [session-id]
  (if-let [session (get-in @state [:sessions session-id])]
    (if (> (System/currentTimeMillis) (:expires-at session))
      {:valid? false :error :session-expired}
      (do
        (reset! (:last-accessed session) (System/currentTimeMillis))
        {:valid? true
         :user-id (:user-id session)
         :metadata (:metadata session)}))
    {:valid? false :error :session-not-found}))

(defn invalidate-session!
  "Invalidate a session."
  [session-id]
  (swap! state update :sessions dissoc session-id))

(defn refresh-session!
  "Refresh a session's expiration."
  [session-id]
  (when-let [session (get-in @state [:sessions session-id])]
    (swap! state assoc-in [:sessions session-id :expires-at]
           (+ (System/currentTimeMillis)
              (get-in @state [:config :session-ttl-ms])))))

;; ============================================================================
;; Basic Authentication
;; ============================================================================

(defn parse-basic-auth
  "Parse Basic authentication header."
  [auth-header]
  (when (and auth-header (str/starts-with? auth-header "Basic "))
    (try
      (let [credentials (base64-decode (subs auth-header 6))
            [username password] (str/split credentials #":" 2)]
        {:username username :password password})
      (catch Exception _
        nil))))

(defn validate-basic-auth
  "Validate Basic authentication."
  [auth-header validate-fn]
  (if-let [{:keys [username password]} (parse-basic-auth auth-header)]
    (if (validate-fn username password)
      {:valid? true :username username}
      {:valid? false :error :invalid-credentials})
    {:valid? false :error :invalid-auth-header}))

;; ============================================================================
;; Bearer Token Authentication
;; ============================================================================

(defn parse-bearer-token
  "Parse Bearer token from header."
  [auth-header]
  (when (and auth-header (str/starts-with? auth-header "Bearer "))
    (subs auth-header 7)))

(defn validate-bearer-token
  "Validate a Bearer token."
  [auth-header validate-fn]
  (if-let [token (parse-bearer-token auth-header)]
    (let [;; Check cache first
          cached (get-in @state [:tokens token])]
      (if (and cached
               (< (- (System/currentTimeMillis) (:cached-at cached))
                  (get-in @state [:config :token-cache-ttl-ms])))
        (do
          (swap! state update-in [:stats :cache-hits] inc)
          (:result cached))
        (let [result (validate-fn token)]
          (when (:valid? result)
            (swap! state assoc-in [:tokens token]
                   {:result result :cached-at (System/currentTimeMillis)}))
          result)))
    {:valid? false :error :invalid-auth-header}))

;; ============================================================================
;; Authentication Strategies
;; ============================================================================

(defn register-strategy!
  "Register an authentication strategy."
  [strategy-id config]
  (let [strategy {:id strategy-id
                  :name (get config :name (name strategy-id))
                  :authenticate-fn (get config :authenticate-fn)
                  :extract-fn (get config :extract-fn)
                  :enabled? (atom true)
                  :metrics {:attempts (atom 0)
                            :successes (atom 0)
                            :failures (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:strategies strategy-id] strategy)
    (logging/log :info "Registered auth strategy" {:strategy-id strategy-id})
    strategy-id))

(defn get-strategy
  "Get an authentication strategy."
  [strategy-id]
  (get-in @state [:strategies strategy-id]))

(defn authenticate
  "Authenticate a request using a strategy."
  [request strategy-id]
  (swap! state update-in [:stats :authentications] inc)
  
  (if-let [strategy (get-strategy strategy-id)]
    (when @(:enabled? strategy)
      (swap! (get-in strategy [:metrics :attempts]) inc)
      (let [credentials ((:extract-fn strategy) request)
            result ((:authenticate-fn strategy) credentials)]
        (if (:valid? result)
          (do
            (swap! state update-in [:stats :successes] inc)
            (swap! (get-in strategy [:metrics :successes]) inc))
          (do
            (swap! state update-in [:stats :failures] inc)
            (swap! (get-in strategy [:metrics :failures]) inc)))
        result))
    {:valid? false :error :unknown-strategy}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-authenticate
  "Ring middleware for authentication."
  [handler strategy-id]
  (fn [request]
    (let [result (authenticate request strategy-id)]
      (if (:valid? result)
        (handler (assoc request :auth result))
        {:status 401
         :headers {"Content-Type" "application/json"
                   "WWW-Authenticate" (name strategy-id)}
         :body {:error "Unauthorized"
                :reason (:error result)}}))))

(defn wrap-api-key
  "Ring middleware for API key authentication."
  [handler & {:keys [header-name query-param]
              :or {header-name "X-API-Key"}}]
  (fn [request]
    (let [api-key (or (get-in request [:headers (str/lower-case header-name)])
                      (get-in request [:query-params query-param]))
          result (when api-key (validate-api-key api-key))]
      (if (and result (:valid? result))
        (handler (assoc request :auth result))
        {:status 401
         :headers {"Content-Type" "application/json"}
         :body {:error "Unauthorized"
                :reason (or (:error result) :missing-api-key)}}))))

(defn wrap-jwt
  "Ring middleware for JWT authentication."
  [handler secret]
  (fn [request]
    (let [auth-header (get-in request [:headers "authorization"])
          token (parse-bearer-token auth-header)
          result (when token (validate-jwt token secret))]
      (if (and result (:valid? result))
        (handler (assoc request :auth result))
        {:status 401
         :headers {"Content-Type" "application/json"}
         :body {:error "Unauthorized"
                :reason (or (:error result) :missing-token)}}))))

(defn wrap-session
  "Ring middleware for session authentication."
  [handler & {:keys [cookie-name] :or {cookie-name "session-id"}}]
  (fn [request]
    (let [session-id (get-in request [:cookies cookie-name :value])
          result (when session-id (validate-session session-id))]
      (if (and result (:valid? result))
        (handler (assoc request :auth result :session-id session-id))
        {:status 401
         :headers {"Content-Type" "application/json"}
         :body {:error "Unauthorized"
                :reason (or (:error result) :missing-session)}}))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-strategy!
  "Set default authentication strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-session-ttl!
  "Set session TTL."
  [ttl-ms]
  (swap! state assoc-in [:config :session-ttl-ms] ttl-ms))

(defn set-token-cache-ttl!
  "Set token cache TTL."
  [ttl-ms]
  (swap! state assoc-in [:config :token-cache-ttl-ms] ttl-ms))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-authenticator-metrics
  "Get authenticator metrics."
  []
  (let [stats (:stats @state)]
    {:authentications (:authentications stats)
     :successes (:successes stats)
     :failures (:failures stats)
     :cache-hits (:cache-hits stats)
     :strategies-count (count (:strategies @state))
     :api-keys-count (count (:api-keys @state))
     :sessions-count (count (:sessions @state))
     :success-rate (if (pos? (:authentications stats))
                     (/ (:successes stats) (:authentications stats))
                     1.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-authenticator-stats
  "Get authenticator statistics."
  []
  (merge (get-authenticator-metrics)
         {:default-strategy (get-in @state [:config :default-strategy])
          :session-ttl-ms (get-in @state [:config :session-ttl-ms])
          :token-cache-ttl-ms (get-in @state [:config :token-cache-ttl-ms])}))

(defn reset-stats!
  "Reset authenticator statistics."
  []
  (swap! state assoc :stats {:authentications 0
                             :successes 0
                             :failures 0
                             :cache-hits 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-authenticator!
  "Initialize the request authenticator."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request authenticator initialized")
    (events/emit! :request-authenticator-initialized {})
    true))
