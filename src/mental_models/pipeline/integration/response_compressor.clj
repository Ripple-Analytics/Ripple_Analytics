(ns mental-models.pipeline.integration.response-compressor
  "Response compressor for mental model analysis system.
   
   Features:
   - Response compression
   - Multiple algorithms
   - Content negotiation
   - Compression levels
   - Selective compression
   - Compression metrics
   - Decompression support
   - Streaming compression"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util.zip GZIPOutputStream GZIPInputStream
            DeflaterOutputStream InflaterInputStream
            Deflater Inflater]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:default-algorithm :gzip
                  :min-size 1024
                  :compression-level 6
                  :compressible-types #{"application/json"
                                        "application/edn"
                                        "application/xml"
                                        "text/plain"
                                        "text/html"
                                        "text/css"
                                        "text/javascript"
                                        "application/javascript"}}
         :stats {:compressions (AtomicLong. 0)
                 :decompressions (AtomicLong. 0)
                 :bytes-before (AtomicLong. 0)
                 :bytes-after (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Compression Algorithms
;; ============================================================================

(defn compress-gzip
  "Compress data using GZIP."
  [data & {:keys [level] :or {level 6}}]
  (let [baos (ByteArrayOutputStream.)
        gzos (GZIPOutputStream. baos)]
    (.write gzos (if (string? data) (.getBytes data "UTF-8") data))
    (.close gzos)
    (.toByteArray baos)))

(defn decompress-gzip
  "Decompress GZIP data."
  [data]
  (let [bais (ByteArrayInputStream. data)
        gzis (GZIPInputStream. bais)
        baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [len (.read gzis buffer)]
          (when (pos? len)
            (.write baos buffer 0 len)
            (recur)))))
    (.close gzis)
    (.toByteArray baos)))

(defn compress-deflate
  "Compress data using Deflate."
  [data & {:keys [level] :or {level 6}}]
  (let [deflater (Deflater. level)
        baos (ByteArrayOutputStream.)
        dos (DeflaterOutputStream. baos deflater)]
    (.write dos (if (string? data) (.getBytes data "UTF-8") data))
    (.close dos)
    (.toByteArray baos)))

(defn decompress-deflate
  "Decompress Deflate data."
  [data]
  (let [inflater (Inflater.)
        bais (ByteArrayInputStream. data)
        iis (InflaterInputStream. bais inflater)
        baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [len (.read iis buffer)]
          (when (pos? len)
            (.write baos buffer 0 len)
            (recur)))))
    (.close iis)
    (.toByteArray baos)))

(defn compress-identity
  "No compression (identity)."
  [data]
  (if (string? data) (.getBytes data "UTF-8") data))

(defn decompress-identity
  "No decompression (identity)."
  [data]
  data)

;; ============================================================================
;; Algorithm Selection
;; ============================================================================

(def compression-algorithms
  {:gzip {:compress compress-gzip
          :decompress decompress-gzip
          :content-encoding "gzip"}
   :deflate {:compress compress-deflate
             :decompress decompress-deflate
             :content-encoding "deflate"}
   :identity {:compress compress-identity
              :decompress decompress-identity
              :content-encoding "identity"}})

(defn get-algorithm
  "Get compression algorithm."
  [algorithm]
  (get compression-algorithms algorithm))

(defn list-algorithms
  "List available compression algorithms."
  []
  (keys compression-algorithms))

;; ============================================================================
;; Content Negotiation
;; ============================================================================

(defn parse-accept-encoding
  "Parse Accept-Encoding header."
  [header]
  (when header
    (->> (str/split header #",")
         (map str/trim)
         (map (fn [part]
                (let [[encoding q] (str/split part #";q=")]
                  {:encoding (keyword (str/trim encoding))
                   :q (if q (Double/parseDouble q) 1.0)})))
         (sort-by :q >)
         (map :encoding))))

(defn negotiate-encoding
  "Negotiate compression encoding."
  [request]
  (let [accept-encoding (get-in request [:headers "accept-encoding"])
        preferred (parse-accept-encoding accept-encoding)
        supported (set (keys compression-algorithms))]
    (or (first (filter supported preferred))
        :identity)))

;; ============================================================================
;; Compression
;; ============================================================================

(defn should-compress?
  "Check if response should be compressed."
  [response]
  (let [content-type (get-in response [:headers "Content-Type"] "")
        body (:body response)
        body-size (cond
                    (string? body) (count body)
                    (bytes? body) (count body)
                    :else 0)
        min-size (get-in @state [:config :min-size])
        compressible-types (get-in @state [:config :compressible-types])]
    (and (>= body-size min-size)
         (some #(str/includes? content-type %) compressible-types))))

(defn compress-response
  "Compress a response."
  [response & {:keys [algorithm level]
               :or {algorithm :gzip
                    level (get-in @state [:config :compression-level])}}]
  (if (should-compress? response)
    (let [body (:body response)
          body-bytes (if (string? body) (.getBytes body "UTF-8") body)
          algo (get-algorithm algorithm)
          compressed ((:compress algo) body-bytes :level level)]
      
      (.addAndGet (:compressions (:stats @state)) 1)
      (.addAndGet (:bytes-before (:stats @state)) (count body-bytes))
      (.addAndGet (:bytes-after (:stats @state)) (count compressed))
      
      (-> response
          (assoc :body compressed)
          (assoc-in [:headers "Content-Encoding"] (:content-encoding algo))
          (assoc-in [:headers "Content-Length"] (str (count compressed)))
          (update-in [:headers] dissoc "Content-Length")))
    response))

(defn decompress-response
  "Decompress a response."
  [response]
  (let [encoding (get-in response [:headers "Content-Encoding"])
        algorithm (keyword encoding)]
    (if-let [algo (get-algorithm algorithm)]
      (let [body (:body response)
            decompressed ((:decompress algo) body)]
        (.addAndGet (:decompressions (:stats @state)) 1)
        (-> response
            (assoc :body decompressed)
            (update-in [:headers] dissoc "Content-Encoding")))
      response)))

;; ============================================================================
;; Request Decompression
;; ============================================================================

(defn decompress-request
  "Decompress a request body."
  [request]
  (let [encoding (get-in request [:headers "content-encoding"])
        algorithm (keyword encoding)]
    (if-let [algo (get-algorithm algorithm)]
      (let [body (:body request)
            decompressed ((:decompress algo) body)]
        (.addAndGet (:decompressions (:stats @state)) 1)
        (-> request
            (assoc :body decompressed)
            (update-in [:headers] dissoc "content-encoding")))
      request)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-compress
  "Ring middleware to compress responses."
  [handler]
  (fn [request]
    (let [algorithm (negotiate-encoding request)
          response (handler request)]
      (if (not= algorithm :identity)
        (compress-response response :algorithm algorithm)
        response))))

(defn wrap-compress-with-algorithm
  "Ring middleware to compress with specific algorithm."
  [handler algorithm]
  (fn [request]
    (let [response (handler request)]
      (compress-response response :algorithm algorithm))))

(defn wrap-decompress-request
  "Ring middleware to decompress request bodies."
  [handler]
  (fn [request]
    (let [decompressed-request (decompress-request request)]
      (handler decompressed-request))))

(defn wrap-vary-accept-encoding
  "Ring middleware to add Vary header."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (update-in response [:headers "Vary"]
                 (fn [v]
                   (if v
                     (str v ", Accept-Encoding")
                     "Accept-Encoding"))))))

;; ============================================================================
;; Streaming Compression
;; ============================================================================

(defn create-gzip-output-stream
  "Create a GZIP output stream."
  [output-stream]
  (GZIPOutputStream. output-stream))

(defn create-deflate-output-stream
  "Create a Deflate output stream."
  [output-stream & {:keys [level] :or {level 6}}]
  (DeflaterOutputStream. output-stream (Deflater. level)))

(defn compress-stream
  "Compress data to an output stream."
  [data output-stream & {:keys [algorithm] :or {algorithm :gzip}}]
  (let [compressed-stream (case algorithm
                            :gzip (create-gzip-output-stream output-stream)
                            :deflate (create-deflate-output-stream output-stream)
                            output-stream)]
    (.write compressed-stream (if (string? data) (.getBytes data "UTF-8") data))
    (.close compressed-stream)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-algorithm!
  "Set default compression algorithm."
  [algorithm]
  (swap! state assoc-in [:config :default-algorithm] algorithm))

(defn set-min-size!
  "Set minimum size for compression."
  [size]
  (swap! state assoc-in [:config :min-size] size))

(defn set-compression-level!
  "Set compression level (1-9)."
  [level]
  (swap! state assoc-in [:config :compression-level] (max 1 (min 9 level))))

(defn add-compressible-type!
  "Add a compressible content type."
  [content-type]
  (swap! state update-in [:config :compressible-types] conj content-type))

(defn remove-compressible-type!
  "Remove a compressible content type."
  [content-type]
  (swap! state update-in [:config :compressible-types] disj content-type))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-compressor-metrics
  "Get compressor metrics."
  []
  (let [stats (:stats @state)
        bytes-before (.get (:bytes-before stats))
        bytes-after (.get (:bytes-after stats))]
    {:compressions (.get (:compressions stats))
     :decompressions (.get (:decompressions stats))
     :bytes-before bytes-before
     :bytes-after bytes-after
     :compression-ratio (if (pos? bytes-before)
                          (- 1.0 (/ bytes-after bytes-before))
                          0.0)
     :bytes-saved (- bytes-before bytes-after)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-compressor-stats
  "Get compressor statistics."
  []
  (merge (get-compressor-metrics)
         {:default-algorithm (get-in @state [:config :default-algorithm])
          :min-size (get-in @state [:config :min-size])
          :compression-level (get-in @state [:config :compression-level])
          :compressible-types (get-in @state [:config :compressible-types])}))

(defn reset-stats!
  "Reset compressor statistics."
  []
  (.set (:compressions (:stats @state)) 0)
  (.set (:decompressions (:stats @state)) 0)
  (.set (:bytes-before (:stats @state)) 0)
  (.set (:bytes-after (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-compressor!
  "Initialize the response compressor."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response compressor initialized")
    (events/emit! :response-compressor-initialized {})
    true))
