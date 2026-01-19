(ns mental-models.pipeline.integration.response-compression
  "Response compression for mental model analysis system.
   
   Features:
   - GZIP compression
   - Deflate compression
   - Brotli compression
   - Content negotiation
   - Minimum size threshold
   - Content type filtering
   - Compression statistics
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
           [java.time Instant LocalDate]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.util.zip GZIPOutputStream GZIPInputStream
            DeflaterOutputStream InflaterInputStream
            Deflater Inflater]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:min-size-bytes 1024
                  :compression-level 6
                  :default-encoding :gzip
                  :enabled? true
                  :compressible-types #{"application/json"
                                        "application/edn"
                                        "text/plain"
                                        "text/html"
                                        "text/css"
                                        "text/javascript"
                                        "application/javascript"
                                        "application/xml"
                                        "text/xml"}}
         :stats {:requests-compressed 0
                 :bytes-before 0
                 :bytes-after 0
                 :gzip-count 0
                 :deflate-count 0}
         :initialized? false}))

;; ============================================================================
;; Compression Functions
;; ============================================================================

(defn gzip-compress
  "Compress data using GZIP."
  [^bytes data & {:keys [level] :or {level 6}}]
  (let [baos (ByteArrayOutputStream.)
        gzip (GZIPOutputStream. baos)]
    (.write gzip data)
    (.close gzip)
    (.toByteArray baos)))

(defn gzip-decompress
  "Decompress GZIP data."
  [^bytes data]
  (let [bais (ByteArrayInputStream. data)
        gzip (GZIPInputStream. bais)
        baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [len (.read gzip buffer)]
          (when (pos? len)
            (.write baos buffer 0 len)
            (recur)))))
    (.close gzip)
    (.toByteArray baos)))

(defn deflate-compress
  "Compress data using Deflate."
  [^bytes data & {:keys [level] :or {level 6}}]
  (let [deflater (Deflater. level)
        baos (ByteArrayOutputStream.)
        dos (DeflaterOutputStream. baos deflater)]
    (.write dos data)
    (.close dos)
    (.toByteArray baos)))

(defn deflate-decompress
  "Decompress Deflate data."
  [^bytes data]
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

;; ============================================================================
;; Content Negotiation
;; ============================================================================

(defn parse-accept-encoding
  "Parse Accept-Encoding header."
  [header]
  (when header
    (let [encodings (str/split header #",")]
      (->> encodings
           (map str/trim)
           (map (fn [e]
                  (let [[encoding q] (str/split e #";q=")]
                    {:encoding (keyword (str/trim encoding))
                     :quality (if q (Double/parseDouble q) 1.0)})))
           (sort-by :quality >)
           (map :encoding)))))

(defn select-encoding
  "Select best encoding based on Accept-Encoding header."
  [accept-encoding]
  (let [supported #{:gzip :deflate :identity}
        requested (or (parse-accept-encoding accept-encoding) [:identity])]
    (first (filter supported requested))))

(defn should-compress?
  "Check if response should be compressed."
  [content-type content-length]
  (let [config (:config @state)
        min-size (:min-size-bytes config)
        compressible-types (:compressible-types config)]
    (and (:enabled? config)
         (or (nil? content-length) (>= content-length min-size))
         (some #(str/starts-with? (or content-type "") %) compressible-types))))

;; ============================================================================
;; Compression API
;; ============================================================================

(defn compress
  "Compress data with specified encoding."
  [data encoding & {:keys [level]}]
  (let [bytes (if (string? data) (.getBytes data "UTF-8") data)
        level (or level (get-in @state [:config :compression-level]))]
    (case encoding
      :gzip (do
              (swap! state update-in [:stats :gzip-count] inc)
              (gzip-compress bytes :level level))
      :deflate (do
                 (swap! state update-in [:stats :deflate-count] inc)
                 (deflate-compress bytes :level level))
      :identity bytes
      bytes)))

(defn decompress
  "Decompress data with specified encoding."
  [data encoding]
  (case encoding
    :gzip (gzip-decompress data)
    :deflate (deflate-decompress data)
    :identity data
    data))

(defn compress-string
  "Compress a string and return base64 encoded result."
  [s encoding]
  (let [compressed (compress s encoding)]
    (java.util.Base64/getEncoder)
    (.encodeToString (java.util.Base64/getEncoder) compressed)))

(defn decompress-string
  "Decompress a base64 encoded string."
  [s encoding]
  (let [decoded (.decode (java.util.Base64/getDecoder) s)
        decompressed (decompress decoded encoding)]
    (String. decompressed "UTF-8")))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn compression-middleware
  "Ring middleware for response compression."
  [handler]
  (fn [request]
    (let [accept-encoding (get-in request [:headers "accept-encoding"])
          encoding (select-encoding accept-encoding)
          response (handler request)]
      (if (and (should-compress? (get-in response [:headers "Content-Type"])
                                 (get-in response [:headers "Content-Length"]))
               (not= encoding :identity)
               (:body response))
        (let [body (:body response)
              body-bytes (cond
                           (string? body) (.getBytes body "UTF-8")
                           (bytes? body) body
                           :else (.getBytes (str body) "UTF-8"))
              compressed (compress body-bytes encoding)
              original-size (count body-bytes)
              compressed-size (count compressed)]
          
          ;; Update stats
          (swap! state update-in [:stats :requests-compressed] inc)
          (swap! state update-in [:stats :bytes-before] + original-size)
          (swap! state update-in [:stats :bytes-after] + compressed-size)
          
          (-> response
              (assoc :body compressed)
              (assoc-in [:headers "Content-Encoding"] (name encoding))
              (assoc-in [:headers "Content-Length"] (str compressed-size))
              (update-in [:headers] dissoc "Content-Length")))
        response))))

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
  "Compress data from input stream to output stream."
  [input-stream output-stream encoding]
  (let [compressed-stream (case encoding
                            :gzip (create-gzip-output-stream output-stream)
                            :deflate (create-deflate-output-stream output-stream)
                            output-stream)
        buffer (byte-array 8192)]
    (loop []
      (let [len (.read input-stream buffer)]
        (when (pos? len)
          (.write compressed-stream buffer 0 len)
          (recur))))
    (.close compressed-stream)))

;; ============================================================================
;; Batch Compression
;; ============================================================================

(defn compress-batch
  "Compress multiple items."
  [items encoding]
  (mapv #(compress % encoding) items))

(defn decompress-batch
  "Decompress multiple items."
  [items encoding]
  (mapv #(decompress % encoding) items))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-min-size!
  "Set minimum size for compression."
  [size-bytes]
  (swap! state assoc-in [:config :min-size-bytes] size-bytes))

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

(defn enable-compression!
  "Enable compression."
  []
  (swap! state assoc-in [:config :enabled?] true))

(defn disable-compression!
  "Disable compression."
  []
  (swap! state assoc-in [:config :enabled?] false))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-compression-stats
  "Get compression statistics."
  []
  (let [stats (:stats @state)
        bytes-before (:bytes-before stats)
        bytes-after (:bytes-after stats)]
    {:requests-compressed (:requests-compressed stats)
     :bytes-before bytes-before
     :bytes-after bytes-after
     :bytes-saved (- bytes-before bytes-after)
     :compression-ratio (if (pos? bytes-before)
                          (/ bytes-after bytes-before)
                          1.0)
     :savings-percentage (if (pos? bytes-before)
                           (* 100 (- 1 (/ bytes-after bytes-before)))
                           0)
     :gzip-count (:gzip-count stats)
     :deflate-count (:deflate-count stats)}))

(defn reset-stats!
  "Reset compression statistics."
  []
  (swap! state assoc :stats {:requests-compressed 0
                             :bytes-before 0
                             :bytes-after 0
                             :gzip-count 0
                             :deflate-count 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-compression!
  "Initialize the response compression system."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response compression initialized")
    (events/emit! :response-compression-initialized {})
    true))
