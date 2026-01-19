(ns mental-models.pipeline.integration.request-decompressor
  "Request decompressor for mental model analysis system.
   
   Features:
   - GZIP decompression
   - Deflate decompression
   - Brotli decompression
   - Automatic format detection
   - Streaming decompression
   - Decompression limits
   - Decompression metrics
   - Ring middleware"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]
           [java.util.zip GZIPInputStream InflaterInputStream]
           [java.io ByteArrayInputStream ByteArrayOutputStream InputStream]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:max-decompressed-size (* 100 1024 1024)  ;; 100MB
                  :buffer-size 8192
                  :enabled-formats #{:gzip :deflate}
                  :auto-detect? true}
         :stats {:requests-decompressed 0
                 :bytes-compressed 0
                 :bytes-decompressed 0
                 :decompression-errors 0
                 :size-limit-exceeded 0}
         :initialized? false}))

;; ============================================================================
;; Format Detection
;; ============================================================================

(defn- detect-format
  "Detect compression format from content-encoding header."
  [content-encoding]
  (when content-encoding
    (let [encoding (str/lower-case content-encoding)]
      (cond
        (str/includes? encoding "gzip") :gzip
        (str/includes? encoding "deflate") :deflate
        (str/includes? encoding "br") :brotli
        :else nil))))

(defn- detect-format-from-bytes
  "Detect compression format from magic bytes."
  [bytes]
  (when (and bytes (>= (count bytes) 2))
    (let [b0 (bit-and (aget bytes 0) 0xFF)
          b1 (bit-and (aget bytes 1) 0xFF)]
      (cond
        ;; GZIP magic number: 1f 8b
        (and (= b0 0x1f) (= b1 0x8b)) :gzip
        ;; Deflate: typically starts with 78 (various compression levels)
        (= b0 0x78) :deflate
        :else nil))))

;; ============================================================================
;; Decompression Functions
;; ============================================================================

(defn- decompress-gzip
  "Decompress GZIP data."
  [input-bytes max-size buffer-size]
  (with-open [bais (ByteArrayInputStream. input-bytes)
              gzis (GZIPInputStream. bais)
              baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array buffer-size)]
      (loop [total 0]
        (let [read (.read gzis buffer)]
          (if (neg? read)
            (.toByteArray baos)
            (do
              (when (> (+ total read) max-size)
                (throw (ex-info "Decompressed size exceeds limit" {:limit max-size})))
              (.write baos buffer 0 read)
              (recur (+ total read)))))))))

(defn- decompress-deflate
  "Decompress Deflate data."
  [input-bytes max-size buffer-size]
  (with-open [bais (ByteArrayInputStream. input-bytes)
              iis (InflaterInputStream. bais)
              baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array buffer-size)]
      (loop [total 0]
        (let [read (.read iis buffer)]
          (if (neg? read)
            (.toByteArray baos)
            (do
              (when (> (+ total read) max-size)
                (throw (ex-info "Decompressed size exceeds limit" {:limit max-size})))
              (.write baos buffer 0 read)
              (recur (+ total read)))))))))

(defn decompress
  "Decompress data based on format."
  [input-bytes format]
  (let [max-size (get-in @state [:config :max-decompressed-size])
        buffer-size (get-in @state [:config :buffer-size])
        enabled-formats (get-in @state [:config :enabled-formats])]
    
    (when-not (contains? enabled-formats format)
      (throw (ex-info "Compression format not enabled" {:format format})))
    
    (try
      (let [result (case format
                     :gzip (decompress-gzip input-bytes max-size buffer-size)
                     :deflate (decompress-deflate input-bytes max-size buffer-size)
                     (throw (ex-info "Unsupported format" {:format format})))]
        
        (swap! state update-in [:stats :requests-decompressed] inc)
        (swap! state update-in [:stats :bytes-compressed] + (count input-bytes))
        (swap! state update-in [:stats :bytes-decompressed] + (count result))
        
        result)
      
      (catch Exception e
        (if (str/includes? (.getMessage e) "exceeds limit")
          (do
            (swap! state update-in [:stats :size-limit-exceeded] inc)
            (throw e))
          (do
            (swap! state update-in [:stats :decompression-errors] inc)
            (logging/log :error "Decompression error" {:format format :error (.getMessage e)})
            (throw e)))))))

;; ============================================================================
;; Streaming Decompression
;; ============================================================================

(defn create-decompressing-stream
  "Create a decompressing input stream."
  [input-stream format]
  (case format
    :gzip (GZIPInputStream. input-stream)
    :deflate (InflaterInputStream. input-stream)
    input-stream))

(defn decompress-stream
  "Decompress a stream to bytes."
  [input-stream format]
  (let [max-size (get-in @state [:config :max-decompressed-size])
        buffer-size (get-in @state [:config :buffer-size])]
    (with-open [decompressing-stream (create-decompressing-stream input-stream format)
                baos (ByteArrayOutputStream.)]
      (let [buffer (byte-array buffer-size)]
        (loop [total 0]
          (let [read (.read decompressing-stream buffer)]
            (if (neg? read)
              (.toByteArray baos)
              (do
                (when (> (+ total read) max-size)
                  (throw (ex-info "Decompressed size exceeds limit" {:limit max-size})))
                (.write baos buffer 0 read)
                (recur (+ total read))))))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-decompress-request
  "Ring middleware to decompress request bodies."
  [handler]
  (fn [request]
    (let [content-encoding (get-in request [:headers "content-encoding"])
          format (detect-format content-encoding)]
      (if (and format (get-in @state [:config :enabled-formats format]))
        (try
          (let [body (:body request)
                body-bytes (if (instance? InputStream body)
                             (let [baos (ByteArrayOutputStream.)]
                               (io/copy body baos)
                               (.toByteArray baos))
                             body)
                decompressed (decompress body-bytes format)]
            (handler (-> request
                         (assoc :body (ByteArrayInputStream. decompressed))
                         (assoc :original-body body-bytes)
                         (assoc :decompressed? true)
                         (update :headers dissoc "content-encoding")
                         (assoc-in [:headers "content-length"] (str (count decompressed))))))
          (catch Exception e
            (logging/log :error "Request decompression failed" {:error (.getMessage e)})
            {:status 400
             :body {:error "Failed to decompress request body"}}))
        (handler request)))))

(defn wrap-decompress-with-options
  "Ring middleware with custom options."
  [handler options]
  (let [max-size (get options :max-size (get-in @state [:config :max-decompressed-size]))
        formats (get options :formats (get-in @state [:config :enabled-formats]))]
    (fn [request]
      (let [content-encoding (get-in request [:headers "content-encoding"])
            format (detect-format content-encoding)]
        (if (and format (contains? formats format))
          (try
            (let [body (:body request)
                  body-bytes (if (instance? InputStream body)
                               (let [baos (ByteArrayOutputStream.)]
                                 (io/copy body baos)
                                 (.toByteArray baos))
                               body)]
              (when (> (count body-bytes) (* max-size 10))  ;; Compressed size check
                (throw (ex-info "Compressed size too large" {:size (count body-bytes)})))
              (let [decompressed (decompress body-bytes format)]
                (when (> (count decompressed) max-size)
                  (throw (ex-info "Decompressed size exceeds limit" {:size (count decompressed)})))
                (handler (-> request
                             (assoc :body (ByteArrayInputStream. decompressed))
                             (assoc :decompressed? true)))))
            (catch Exception e
              {:status 400
               :body {:error "Decompression failed" :message (.getMessage e)}}))
          (handler request))))))

;; ============================================================================
;; Auto-Detection
;; ============================================================================

(defn decompress-auto
  "Decompress data with automatic format detection."
  [input-bytes]
  (if (get-in @state [:config :auto-detect?])
    (if-let [format (detect-format-from-bytes input-bytes)]
      (decompress input-bytes format)
      input-bytes)
    input-bytes))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-max-size!
  "Set the maximum decompressed size."
  [max-size]
  (swap! state assoc-in [:config :max-decompressed-size] max-size))

(defn set-buffer-size!
  "Set the buffer size."
  [buffer-size]
  (swap! state assoc-in [:config :buffer-size] buffer-size))

(defn enable-format!
  "Enable a compression format."
  [format]
  (swap! state update-in [:config :enabled-formats] conj format))

(defn disable-format!
  "Disable a compression format."
  [format]
  (swap! state update-in [:config :enabled-formats] disj format))

(defn set-auto-detect!
  "Enable or disable auto-detection."
  [enabled?]
  (swap! state assoc-in [:config :auto-detect?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-decompressor-metrics
  "Get decompressor metrics."
  []
  (let [stats (:stats @state)
        compressed (:bytes-compressed stats)
        decompressed (:bytes-decompressed stats)]
    {:requests-decompressed (:requests-decompressed stats)
     :bytes-compressed compressed
     :bytes-decompressed decompressed
     :compression-ratio (if (pos? compressed)
                          (/ decompressed compressed)
                          1.0)
     :decompression-errors (:decompression-errors stats)
     :size-limit-exceeded (:size-limit-exceeded stats)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-decompressor-stats
  "Get decompressor statistics."
  []
  (let [stats (:stats @state)]
    (merge stats
           {:enabled-formats (get-in @state [:config :enabled-formats])
            :max-size (get-in @state [:config :max-decompressed-size])
            :auto-detect? (get-in @state [:config :auto-detect?])})))

(defn reset-stats!
  "Reset decompressor statistics."
  []
  (swap! state assoc :stats {:requests-decompressed 0
                             :bytes-compressed 0
                             :bytes-decompressed 0
                             :decompression-errors 0
                             :size-limit-exceeded 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-request-decompressor!
  "Initialize the request decompressor."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Request decompressor initialized")
    (events/emit! :request-decompressor-initialized {})
    true))
