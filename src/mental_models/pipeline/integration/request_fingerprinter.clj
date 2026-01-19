(ns mental-models.pipeline.integration.request-fingerprinter
  "Request fingerprinter for mental model analysis system.
   
   Features:
   - Request fingerprinting
   - Client identification
   - Bot detection
   - Device fingerprinting
   - Browser fingerprinting
   - Fingerprint storage
   - Fingerprint matching
   - Fingerprint metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.security MessageDigest]
           [java.util Base64]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:fingerprints {}     ;; fingerprint-hash -> fingerprint-data
         :known-bots #{}      ;; Known bot user agents
         :config {:hash-algorithm "SHA-256"
                  :include-headers #{:user-agent :accept :accept-language :accept-encoding}
                  :include-ip? true
                  :max-fingerprints 100000
                  :fingerprint-ttl-ms 86400000}  ;; 24 hours
         :stats {:fingerprints-created 0
                 :fingerprints-matched 0
                 :bots-detected 0
                 :unique-clients 0}
         :initialized? false}))

;; ============================================================================
;; Hashing
;; ============================================================================

(defn- hash-string
  "Hash a string using the configured algorithm."
  [s]
  (let [algorithm (get-in @state [:config :hash-algorithm])
        md (MessageDigest/getInstance algorithm)
        bytes (.digest md (.getBytes s "UTF-8"))]
    (.encodeToString (Base64/getUrlEncoder) bytes)))

;; ============================================================================
;; User Agent Parsing
;; ============================================================================

(defn- parse-user-agent
  "Parse user agent string for device/browser info."
  [user-agent]
  (when user-agent
    (let [ua (str/lower-case user-agent)]
      {:browser (cond
                  (str/includes? ua "chrome") :chrome
                  (str/includes? ua "firefox") :firefox
                  (str/includes? ua "safari") :safari
                  (str/includes? ua "edge") :edge
                  (str/includes? ua "opera") :opera
                  (str/includes? ua "msie") :ie
                  (str/includes? ua "trident") :ie
                  :else :unknown)
       :os (cond
             (str/includes? ua "windows") :windows
             (str/includes? ua "mac os") :macos
             (str/includes? ua "linux") :linux
             (str/includes? ua "android") :android
             (str/includes? ua "iphone") :ios
             (str/includes? ua "ipad") :ios
             :else :unknown)
       :device (cond
                 (str/includes? ua "mobile") :mobile
                 (str/includes? ua "tablet") :tablet
                 (str/includes? ua "ipad") :tablet
                 :else :desktop)
       :is-bot? (or (str/includes? ua "bot")
                    (str/includes? ua "crawler")
                    (str/includes? ua "spider")
                    (str/includes? ua "scraper")
                    (str/includes? ua "curl")
                    (str/includes? ua "wget")
                    (str/includes? ua "python")
                    (str/includes? ua "java/")
                    (str/includes? ua "httpclient"))})))

;; ============================================================================
;; Fingerprint Generation
;; ============================================================================

(defn- extract-fingerprint-components
  "Extract components for fingerprinting."
  [request]
  (let [headers (get request :headers {})
        include-headers (get-in @state [:config :include-headers])
        include-ip? (get-in @state [:config :include-ip?])]
    (merge
     ;; Headers
     (into {} (for [h include-headers
                    :let [header-name (name h)
                          value (get headers header-name)]]
                [h value]))
     ;; IP address
     (when include-ip?
       {:ip (or (get headers "x-forwarded-for")
                (get headers "x-real-ip")
                (get request :remote-addr))})
     ;; Additional signals
     {:method (get request :method)
      :scheme (get request :scheme)})))

(defn generate-fingerprint
  "Generate a fingerprint for a request."
  [request]
  (let [components (extract-fingerprint-components request)
        user-agent (get-in request [:headers "user-agent"])
        ua-info (parse-user-agent user-agent)
        fingerprint-string (pr-str (sort-by first components))
        fingerprint-hash (hash-string fingerprint-string)]
    
    {:hash fingerprint-hash
     :components components
     :user-agent-info ua-info
     :is-bot? (:is-bot? ua-info)
     :timestamp (System/currentTimeMillis)}))

;; ============================================================================
;; Fingerprint Storage
;; ============================================================================

(defn store-fingerprint!
  "Store a fingerprint."
  [fingerprint]
  (let [hash (:hash fingerprint)
        max-fingerprints (get-in @state [:config :max-fingerprints])]
    
    ;; Cleanup if needed
    (when (> (count (:fingerprints @state)) max-fingerprints)
      (let [ttl (get-in @state [:config :fingerprint-ttl-ms])
            cutoff (- (System/currentTimeMillis) ttl)]
        (swap! state update :fingerprints
               (fn [fps]
                 (into {} (filter (fn [[_ v]] (> (:timestamp v) cutoff)) fps))))))
    
    (if (get-in @state [:fingerprints hash])
      ;; Update existing
      (swap! state update-in [:fingerprints hash :seen-count] (fnil inc 1))
      ;; Create new
      (do
        (swap! state assoc-in [:fingerprints hash]
               (assoc fingerprint :seen-count 1 :first-seen (System/currentTimeMillis)))
        (swap! state update-in [:stats :fingerprints-created] inc)
        (swap! state update-in [:stats :unique-clients] inc)))
    
    hash))

(defn get-fingerprint
  "Get a stored fingerprint."
  [hash]
  (get-in @state [:fingerprints hash]))

(defn list-fingerprints
  "List all fingerprints."
  []
  (mapv (fn [[hash fp]]
          {:hash hash
           :seen-count (:seen-count fp)
           :first-seen (:first-seen fp)
           :is-bot? (:is-bot? fp)})
        (:fingerprints @state)))

;; ============================================================================
;; Fingerprint Matching
;; ============================================================================

(defn match-fingerprint
  "Match a request against stored fingerprints."
  [request]
  (let [fingerprint (generate-fingerprint request)
        hash (:hash fingerprint)
        existing (get-fingerprint hash)]
    (if existing
      (do
        (swap! state update-in [:stats :fingerprints-matched] inc)
        (swap! state update-in [:fingerprints hash :seen-count] inc)
        (swap! state update-in [:fingerprints hash :last-seen] (constantly (System/currentTimeMillis)))
        {:matched? true
         :fingerprint existing
         :is-returning? true})
      (do
        (store-fingerprint! fingerprint)
        {:matched? false
         :fingerprint fingerprint
         :is-returning? false}))))

;; ============================================================================
;; Bot Detection
;; ============================================================================

(defn- is-known-bot?
  "Check if user agent is a known bot."
  [user-agent]
  (when user-agent
    (let [ua (str/lower-case user-agent)]
      (some #(str/includes? ua %) (:known-bots @state)))))

(defn detect-bot
  "Detect if a request is from a bot."
  [request]
  (let [user-agent (get-in request [:headers "user-agent"])
        ua-info (parse-user-agent user-agent)
        is-bot? (or (:is-bot? ua-info)
                    (is-known-bot? user-agent)
                    ;; Additional heuristics
                    (nil? user-agent)
                    (empty? (get-in request [:headers "accept"])))]
    (when is-bot?
      (swap! state update-in [:stats :bots-detected] inc))
    {:is-bot? is-bot?
     :confidence (cond
                   (nil? user-agent) 0.9
                   (:is-bot? ua-info) 0.95
                   (is-known-bot? user-agent) 0.99
                   :else 0.5)
     :user-agent-info ua-info}))

(defn add-known-bot!
  "Add a known bot pattern."
  [pattern]
  (swap! state update :known-bots conj (str/lower-case pattern)))

(defn remove-known-bot!
  "Remove a known bot pattern."
  [pattern]
  (swap! state update :known-bots disj (str/lower-case pattern)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-fingerprint
  "Ring middleware to fingerprint requests."
  [handler]
  (fn [request]
    (let [match-result (match-fingerprint request)
          enriched-request (assoc request
                                  :fingerprint (:fingerprint match-result)
                                  :is-returning-client? (:is-returning? match-result))]
      (handler enriched-request))))

(defn wrap-bot-detection
  "Ring middleware to detect bots."
  [handler]
  (fn [request]
    (let [bot-result (detect-bot request)
          enriched-request (assoc request
                                  :is-bot? (:is-bot? bot-result)
                                  :bot-confidence (:confidence bot-result))]
      (handler enriched-request))))

(defn wrap-block-bots
  "Ring middleware to block bots."
  [handler]
  (fn [request]
    (let [bot-result (detect-bot request)]
      (if (and (:is-bot? bot-result) (> (:confidence bot-result) 0.8))
        {:status 403
         :body {:error "Bot access not allowed"}}
        (handler request)))))

;; ============================================================================
;; Fingerprint Analysis
;; ============================================================================

(defn get-client-stats
  "Get statistics about clients."
  []
  (let [fingerprints (vals (:fingerprints @state))
        bots (filter :is-bot? fingerprints)
        humans (remove :is-bot? fingerprints)]
    {:total-clients (count fingerprints)
     :bots (count bots)
     :humans (count humans)
     :by-browser (frequencies (map #(get-in % [:user-agent-info :browser]) humans))
     :by-os (frequencies (map #(get-in % [:user-agent-info :os]) humans))
     :by-device (frequencies (map #(get-in % [:user-agent-info :device]) humans))}))

(defn get-returning-clients
  "Get clients that have returned multiple times."
  [min-visits]
  (->> (vals (:fingerprints @state))
       (filter #(>= (:seen-count %) min-visits))
       (sort-by :seen-count >)
       (vec)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-fingerprinter-metrics
  "Get fingerprinter metrics."
  []
  (let [stats (:stats @state)]
    {:fingerprints-created (:fingerprints-created stats)
     :fingerprints-matched (:fingerprints-matched stats)
     :bots-detected (:bots-detected stats)
     :unique-clients (:unique-clients stats)
     :stored-fingerprints (count (:fingerprints @state))
     :known-bot-patterns (count (:known-bots @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-fingerprinter-stats
  "Get fingerprinter statistics."
  []
  (merge (get-fingerprinter-metrics)
         (get-client-stats)))

(defn reset-stats!
  "Reset fingerprinter statistics."
  []
  (swap! state assoc :stats {:fingerprints-created 0
                             :fingerprints-matched 0
                             :bots-detected 0
                             :unique-clients 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-fingerprinter!
  "Initialize the request fingerprinter."
  []
  (when-not (:initialized? @state)
    ;; Add common bot patterns
    (doseq [pattern ["googlebot" "bingbot" "yandexbot" "duckduckbot"
                     "baiduspider" "facebookexternalhit" "twitterbot"
                     "linkedinbot" "slackbot" "telegrambot"
                     "applebot" "semrushbot" "ahrefsbot"]]
      (add-known-bot! pattern))
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Request fingerprinter initialized")
    (events/emit! :request-fingerprinter-initialized {})
    true))
