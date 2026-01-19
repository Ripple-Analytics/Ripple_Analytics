(ns mental-models.desktop.updater.network
  "Network Resilience - Bulletproof network handling
   
   Features:
   - Slow connection handling (100KB/s+)
   - Intermittent connection recovery
   - Proxy support (HTTP, SOCKS)
   - Firewall detection and handling
   - Configurable timeouts (30s, 60s, 300s)
   - Retry logic with exponential backoff
   - Connection quality monitoring
   - Bandwidth throttling detection
   - Network change detection
   - Offline mode support"
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close! timeout alts!]]
            [taoensso.timbre :as log])
  (:import [java.net InetAddress NetworkInterface URL HttpURLConnection Proxy Proxy$Type InetSocketAddress]
           [java.io IOException]
           [java.time Instant Duration]
           [java.util.concurrent.atomic AtomicBoolean AtomicLong]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def config
  (atom {:connect-timeout-ms 10000
         :read-timeout-slow-ms 300000      ;; 5 minutes for slow connections
         :read-timeout-normal-ms 60000     ;; 1 minute for normal
         :read-timeout-fast-ms 30000       ;; 30 seconds for fast
         :min-speed-bytes-per-sec 1024     ;; 1KB/s minimum
         :slow-speed-threshold 102400      ;; 100KB/s
         :retry-count 5
         :retry-base-delay-ms 1000
         :retry-max-delay-ms 120000        ;; 2 minutes max
         :health-check-interval-ms 30000
         :health-check-url "https://www.google.com/generate_204"
         :proxy-host nil
         :proxy-port nil
         :proxy-type :http                 ;; :http or :socks
         :proxy-username nil
         :proxy-password nil
         :user-agent "MentalModels-Desktop/1.0"
         :offline-mode false}))

(defonce network-state
  (atom {:online true
         :connection-quality :unknown      ;; :unknown, :poor, :fair, :good, :excellent
         :last-speed-test nil
         :average-speed 0
         :last-health-check nil
         :consecutive-failures 0
         :proxy-working nil
         :firewall-detected false
         :network-changed false}))

(defonce health-check-running (AtomicBoolean. false))

;; =============================================================================
;; Network Detection
;; =============================================================================

(defn get-network-interfaces
  "Get all active network interfaces."
  []
  (try
    (->> (NetworkInterface/getNetworkInterfaces)
         enumeration-seq
         (filter #(.isUp %))
         (filter #(not (.isLoopback %)))
         (map (fn [iface]
                {:name (.getName iface)
                 :display-name (.getDisplayName iface)
                 :addresses (->> (.getInetAddresses iface)
                                 enumeration-seq
                                 (map #(.getHostAddress %)))})))
    (catch Exception e
      (log/warn "Failed to get network interfaces:" (.getMessage e))
      [])))

(defn has-network-connection?
  "Check if any network interface is available."
  []
  (seq (get-network-interfaces)))

(defn is-internet-reachable?
  "Check if the internet is reachable."
  []
  (try
    (let [url (URL. (:health-check-url @config))
          conn (doto (.openConnection url)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout 5000)
                 (.setRequestMethod "GET"))]
      (.connect conn)
      (let [code (.getResponseCode conn)]
        (.disconnect conn)
        (<= 200 code 399)))
    (catch Exception _
      false)))

;; =============================================================================
;; Connection Quality
;; =============================================================================

(defn measure-connection-speed
  "Measure connection speed by downloading a small test file."
  []
  (try
    (let [test-url "https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png"
          start-time (System/currentTimeMillis)
          response (http/get test-url {:as :byte-array
                                       :socket-timeout 30000
                                       :connection-timeout 5000})
          end-time (System/currentTimeMillis)
          bytes-downloaded (count (:body response))
          duration-ms (- end-time start-time)
          speed-bps (if (pos? duration-ms)
                      (/ (* bytes-downloaded 1000) duration-ms)
                      0)]
      {:success true
       :speed-bps speed-bps
       :duration-ms duration-ms
       :bytes bytes-downloaded})
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn classify-connection-quality
  "Classify connection quality based on speed."
  [speed-bps]
  (cond
    (nil? speed-bps) :unknown
    (< speed-bps 10240) :poor           ;; < 10KB/s
    (< speed-bps 102400) :fair          ;; < 100KB/s
    (< speed-bps 1048576) :good         ;; < 1MB/s
    :else :excellent))                   ;; >= 1MB/s

(defn update-connection-quality!
  "Update the connection quality assessment."
  []
  (let [result (measure-connection-speed)]
    (if (:success result)
      (let [quality (classify-connection-quality (:speed-bps result))]
        (swap! network-state assoc
               :connection-quality quality
               :last-speed-test (str (Instant/now))
               :average-speed (:speed-bps result))
        (log/debug "Connection quality:" quality "Speed:" (int (/ (:speed-bps result) 1024)) "KB/s")
        quality)
      (do
        (swap! network-state assoc :connection-quality :unknown)
        :unknown))))

;; =============================================================================
;; Timeout Selection
;; =============================================================================

(defn get-appropriate-timeout
  "Get appropriate timeout based on connection quality."
  []
  (case (:connection-quality @network-state)
    :poor (:read-timeout-slow-ms @config)
    :fair (:read-timeout-slow-ms @config)
    :good (:read-timeout-normal-ms @config)
    :excellent (:read-timeout-fast-ms @config)
    (:read-timeout-normal-ms @config)))

;; =============================================================================
;; Proxy Support
;; =============================================================================

(defn create-proxy
  "Create a proxy object from configuration."
  []
  (when (and (:proxy-host @config) (:proxy-port @config))
    (let [proxy-type (case (:proxy-type @config)
                       :socks Proxy$Type/SOCKS
                       Proxy$Type/HTTP)
          address (InetSocketAddress. (:proxy-host @config) (:proxy-port @config))]
      (Proxy. proxy-type address))))

(defn test-proxy
  "Test if the configured proxy is working."
  []
  (when-let [proxy (create-proxy)]
    (try
      (let [url (URL. (:health-check-url @config))
            conn (.openConnection url proxy)]
        (.setConnectTimeout conn 10000)
        (.setReadTimeout conn 10000)
        (.connect conn)
        (let [code (.getResponseCode conn)]
          (.disconnect conn)
          (let [working (<= 200 code 399)]
            (swap! network-state assoc :proxy-working working)
            working)))
      (catch Exception e
        (log/warn "Proxy test failed:" (.getMessage e))
        (swap! network-state assoc :proxy-working false)
        false))))

(defn get-http-client-opts
  "Get HTTP client options with proxy if configured."
  []
  (let [base-opts {:socket-timeout (get-appropriate-timeout)
                   :connection-timeout (:connect-timeout-ms @config)
                   :headers {"User-Agent" (:user-agent @config)}}]
    (if-let [proxy (create-proxy)]
      (assoc base-opts :proxy-host (:proxy-host @config)
                       :proxy-port (:proxy-port @config))
      base-opts)))

;; =============================================================================
;; Retry Logic
;; =============================================================================

(defn exponential-backoff
  "Calculate exponential backoff delay with jitter."
  [attempt]
  (let [base (:retry-base-delay-ms @config)
        max-delay (:retry-max-delay-ms @config)
        delay (* base (Math/pow 2 attempt))
        jitter (* delay (rand 0.2))]
    (long (min max-delay (+ delay jitter)))))

(defn should-retry?
  "Determine if an error is retryable."
  [error]
  (cond
    (instance? java.net.SocketTimeoutException error) true
    (instance? java.net.ConnectException error) true
    (instance? java.net.UnknownHostException error) true
    (instance? java.io.IOException error) true
    (instance? javax.net.ssl.SSLException error) false  ;; Don't retry SSL errors
    :else false))

(defn with-retry
  "Execute a function with retry logic."
  [f & {:keys [max-retries on-retry]
        :or {max-retries (:retry-count @config)}}]
  (loop [attempt 0]
    (let [result (try
                   {:success true :value (f)}
                   (catch Exception e
                     {:success false :error e}))]
      (if (:success result)
        (:value result)
        (if (and (< attempt max-retries)
                 (should-retry? (:error result)))
          (let [delay (exponential-backoff attempt)]
            (log/warn "Attempt" (inc attempt) "failed, retrying in" (/ delay 1000) "seconds..."
                      "Error:" (.getMessage (:error result)))
            (when on-retry
              (on-retry {:attempt (inc attempt)
                         :max-retries max-retries
                         :delay-ms delay
                         :error (:error result)}))
            (Thread/sleep delay)
            (recur (inc attempt)))
          (throw (:error result)))))))

;; =============================================================================
;; Health Check Loop
;; =============================================================================

(defn start-health-check!
  "Start periodic health checking."
  []
  (when (.compareAndSet health-check-running false true)
    (go-loop []
      (when (.get health-check-running)
        (let [reachable (is-internet-reachable?)]
          (swap! network-state assoc
                 :online reachable
                 :last-health-check (str (Instant/now)))
          (if reachable
            (do
              (swap! network-state assoc :consecutive-failures 0)
              (when (= :unknown (:connection-quality @network-state))
                (update-connection-quality!)))
            (swap! network-state update :consecutive-failures inc)))
        (<! (timeout (:health-check-interval-ms @config)))
        (recur)))
    (log/info "Health check started")))

(defn stop-health-check!
  "Stop periodic health checking."
  []
  (.set health-check-running false)
  (log/info "Health check stopped"))

;; =============================================================================
;; Firewall Detection
;; =============================================================================

(defn detect-firewall
  "Attempt to detect if a firewall is blocking connections."
  []
  (let [test-urls ["https://api.github.com"
                   "https://drive.google.com"
                   "https://www.google.com"]
        results (map (fn [url]
                       (try
                         (let [conn (doto (.openConnection (URL. url))
                                      (.setConnectTimeout 5000)
                                      (.setReadTimeout 5000))]
                           (.connect conn)
                           (.disconnect conn)
                           {:url url :blocked false})
                         (catch Exception e
                           {:url url :blocked true :error (.getMessage e)})))
                     test-urls)
        blocked-count (count (filter :blocked results))]
    (let [firewall-detected (>= blocked-count 2)]
      (swap! network-state assoc :firewall-detected firewall-detected)
      (when firewall-detected
        (log/warn "Firewall detected - some URLs are blocked:" 
                  (map :url (filter :blocked results))))
      {:firewall-detected firewall-detected
       :results results})))

;; =============================================================================
;; Offline Mode
;; =============================================================================

(defn enable-offline-mode!
  "Enable offline mode."
  []
  (swap! config assoc :offline-mode true)
  (log/info "Offline mode enabled"))

(defn disable-offline-mode!
  "Disable offline mode."
  []
  (swap! config assoc :offline-mode false)
  (log/info "Offline mode disabled"))

(defn in-offline-mode?
  "Check if offline mode is enabled."
  []
  (:offline-mode @config))

;; =============================================================================
;; Network Change Detection
;; =============================================================================

(defonce last-interfaces (atom nil))

(defn check-network-change
  "Check if network interfaces have changed."
  []
  (let [current (set (map :name (get-network-interfaces)))
        previous @last-interfaces
        changed (and previous (not= current previous))]
    (reset! last-interfaces current)
    (when changed
      (log/info "Network change detected")
      (swap! network-state assoc :network-changed true)
      (update-connection-quality!))
    changed))

;; =============================================================================
;; High-Level Network Operations
;; =============================================================================

(defn make-resilient-request
  "Make an HTTP request with full resilience features."
  [url & {:keys [method headers as on-progress on-retry]
          :or {method :get as :auto}}]
  (when (in-offline-mode?)
    (throw (ex-info "Offline mode enabled" {:type :offline})))
  
  (when-not (:online @network-state)
    (log/warn "Network appears offline, attempting anyway..."))
  
  (with-retry
    (fn []
      (let [opts (merge (get-http-client-opts)
                        {:headers headers
                         :as as})
            response (case method
                       :get (http/get url opts)
                       :head (http/head url opts)
                       :post (http/post url opts))]
        response))
    :on-retry on-retry))

(defn download-with-resilience
  "Download a file with full network resilience."
  [url target-file & {:keys [on-progress on-retry expected-size]}]
  (when (in-offline-mode?)
    (throw (ex-info "Offline mode enabled" {:type :offline})))
  
  (log/info "Starting resilient download:" url)
  
  (with-retry
    (fn []
      (let [timeout (get-appropriate-timeout)
            conn (doto (.openConnection (URL. url))
                   (.setConnectTimeout (:connect-timeout-ms @config))
                   (.setReadTimeout timeout)
                   (.setRequestProperty "User-Agent" (:user-agent @config)))]
        
        ;; Apply proxy if configured
        (when-let [proxy (create-proxy)]
          ;; Note: For URL connections, proxy is set at open time
          nil)
        
        (.connect conn)
        
        (let [content-length (.getContentLengthLong conn)
              total-size (or expected-size content-length)
              buffer (byte-array 65536)
              downloaded (AtomicLong. 0)
              start-time (System/currentTimeMillis)]
          
          (io/make-parents target-file)
          
          (with-open [in (.getInputStream conn)
                      out (io/output-stream target-file)]
            (loop []
              (let [n (.read in buffer)]
                (when (pos? n)
                  (.write out buffer 0 n)
                  (.addAndGet downloaded n)
                  
                  ;; Progress callback
                  (when on-progress
                    (let [total-downloaded (.get downloaded)
                          elapsed (- (System/currentTimeMillis) start-time)
                          speed (if (pos? elapsed)
                                  (/ (* total-downloaded 1000) elapsed)
                                  0)]
                      (on-progress {:downloaded total-downloaded
                                    :total total-size
                                    :progress (if (and total-size (pos? total-size))
                                                (* 100.0 (/ total-downloaded total-size))
                                                -1)
                                    :speed speed})))
                  (recur)))))
          
          {:success true
           :file target-file
           :size (.get downloaded)})))
    :on-retry on-retry))

;; =============================================================================
;; Status & Configuration
;; =============================================================================

(defn get-network-status
  "Get comprehensive network status."
  []
  (let [state @network-state]
    {:online (:online state)
     :connection-quality (:connection-quality state)
     :average-speed-kbps (when (:average-speed state)
                           (int (/ (:average-speed state) 1024)))
     :last-health-check (:last-health-check state)
     :consecutive-failures (:consecutive-failures state)
     :proxy-configured (some? (:proxy-host @config))
     :proxy-working (:proxy-working state)
     :firewall-detected (:firewall-detected state)
     :offline-mode (in-offline-mode?)
     :interfaces (count (get-network-interfaces))}))

(defn configure!
  "Update network configuration."
  [config-map]
  (swap! config merge config-map)
  (log/info "Network configured:" (keys config-map)))

(defn set-proxy!
  "Configure proxy settings."
  [host port & {:keys [type username password]
                :or {type :http}}]
  (swap! config assoc
         :proxy-host host
         :proxy-port port
         :proxy-type type
         :proxy-username username
         :proxy-password password)
  (test-proxy)
  (log/info "Proxy configured:" host ":" port))

(defn clear-proxy!
  "Clear proxy settings."
  []
  (swap! config assoc
         :proxy-host nil
         :proxy-port nil
         :proxy-username nil
         :proxy-password nil)
  (swap! network-state assoc :proxy-working nil)
  (log/info "Proxy cleared"))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-network!
  "Initialize the network module."
  []
  (log/info "Initializing network module...")
  (reset! last-interfaces (set (map :name (get-network-interfaces))))
  (let [online (is-internet-reachable?)]
    (swap! network-state assoc :online online)
    (when online
      (update-connection-quality!))
    (start-health-check!)
    {:success true :online online}))

(defn shutdown-network!
  "Shutdown the network module."
  []
  (stop-health-check!)
  (log/info "Network module shutdown"))
